!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
MODULE PRMS_TRANSP_TINDEX
    use kinds_mod, only: r4, r8, i4, i8
    implicit none

    ! Local Variables
    integer(i4), save, allocatable :: Transp_check(:), Transp_beg_restart(:), Transp_end_restart(:)
    real(r4), save, allocatable :: Tmax_sum(:), Transp_tmax_f(:), Transp_tmax_restart(:)
    character(len=13), save :: MODNAME

    ! Declared Parameters
    integer(i4), save, allocatable :: Transp_beg(:), Transp_end(:)
    real(r4), save, allocatable :: Transp_tmax(:)

    private
    public :: transp_tindex

    contains
        integer function transp_tindex(dim_data)
            use PRMS_MODULE, only: Process, Nhru, Save_vars_to_file, Init_vars_from_file, &
                    Start_month, Start_day, print_module
            use PRMS_BASIN, only: Active_hrus, Hru_route_order
            use PRMS_CLIMATEVARS, only: Tmaxf, Temp_units, Transp_on, Basin_transp_on
            use PRMS_SET_TIME, only: Nowmonth, Nowday
            use UTILS_PRMS, only: read_error
            use conversions_mod, only: c_to_f
            use parameter_mod, only: declparam, getparam
            ! use PRMS_MMFAPI, only: declparam, getparam
            use dimensions_mod, only: dimension_list
            implicit none

            type(dimension_list), intent(in) :: dim_data

            ! Local Variables
            integer(i4) :: i, j, motmp, new_values
            character(len=80), save :: Version_transp

            !***********************************************************************
            transp_tindex = 0

            if (Process == 'run') then
                !******Set switch for active transpiration period
                Basin_transp_on = 0
                do j = 1, Active_hrus
                    i = Hru_route_order(j)

                    !******check for month to turn check switch on or
                    !******transpiration switch off
                    if (Nowday == 1) then
                        !******check for end of period
                        if (Nowmonth == Transp_end(i)) then
                            Transp_on(i) = 0
                            Transp_check(i) = 0
                            Tmax_sum(i) = 0.0
                        endif
                        !******check for month to turn transpiration switch on or off
                        if (Nowmonth == Transp_beg(i)) then
                            Transp_check(i) = 1
                            Tmax_sum(i) = 0.0
                        endif
                    endif

                    !******If in checking period, then for each day
                    !******sum maximum temperature until greater than temperature index parameter,
                    !******at which time, turn transpiration switch on, check switch off
                    ! freezing temperature assumed to be 32 degrees Fahrenheit
                    if (Transp_check(i) == 1) then
                        if (Tmaxf(i) > 32.0) Tmax_sum(i) = Tmax_sum(i) + Tmaxf(i)
                        if (Tmax_sum(i) > Transp_tmax_f(i)) then
                            Transp_on(i) = 1
                            Transp_check(i) = 0
                            Tmax_sum(i) = 0.0
                        endif
                    endif

                    if (Basin_transp_on == 0) then
                        if (Transp_on(i) == 1) Basin_transp_on = 1
                    endif
                enddo

            elseif (Process == 'declare') then
                Version_transp = 'transp_tindex.f90 2015-01-06 00:09:15Z'
                call print_module(Version_transp, 'Transpiration Distribution  ', 90)
                MODNAME = 'transp_tindex'

                allocate (Tmax_sum(Nhru), Transp_check(Nhru), Transp_tmax_f(Nhru))

                allocate (Transp_beg(Nhru))
                if (declparam(MODNAME, 'transp_beg', 'nhru', 'integer', &
                        &       '1', '1', '12', &
                        &       'Month to begin testing for transpiration', &
                        &       'Month to begin summing the maximum air temperature for each HRU; when sum is greater than or' // &
                                &       ' equal to transp_tmax, transpiration begins', &
                        &       'month', dim_data) /= 0) call read_error(1, 'transp_beg')

                allocate (Transp_end(Nhru))
                if (declparam(MODNAME, 'transp_end', 'nhru', 'integer', &
                        &       '13', '1', '13', &
                        &       'Month to stop transpiration period', &
                        &       'Month to stop transpiration computations; transpiration is computed thru end of previous month', &
                        &       'month', dim_data) /= 0) call read_error(1, 'transp_end')

                allocate (Transp_tmax(Nhru))
                if (declparam(MODNAME, 'transp_tmax', 'nhru', 'real', &
                        &       '1.0', '0.0', '1000.0', &
                        &       'Tmax index to determine start of transpiration', &
                        &       'Temperature index to determine the specific date of the start of the transpiration period;' // &
                                ' the maximum air temperature for each HRU is summed starting with the first day of ' // &
                                'month transp_beg; when the sum exceeds this index, transpiration begins', &
                        &       'temp_units', dim_data) /= 0) call read_error(1, 'transp_tmax')

            elseif (Process == 'init') then

                if (getparam(MODNAME, 'transp_beg', Nhru, 'integer', Transp_beg) /= 0) call read_error(2, 'transp_beg')
                if (getparam(MODNAME, 'transp_end', Nhru, 'integer', Transp_end) /= 0) call read_error(2, 'transp_end')
                if (getparam(MODNAME, 'transp_tmax', Nhru, 'real', Transp_tmax) /= 0) call read_error(2, 'transp_tmax')

                new_values = 0
                if (Init_vars_from_file == 1) then
                    allocate (Transp_beg_restart(Nhru), Transp_end_restart(Nhru), Transp_tmax_restart(Nhru))
                    call transp_tindex_restart(1)
                    do j = 1, Active_hrus
                        i = Hru_route_order(j)
                        if (new_values == 1) EXIT
                        if (Transp_beg(i) /= Transp_beg_restart(i)) new_values = 1
                        if (Transp_end(i) /= Transp_end_restart(i)) new_values = 1
                        if (Transp_tmax(i) /= Transp_tmax_restart(i)) new_values = 1
                    enddo
                    deallocate (Transp_beg_restart, Transp_end_restart, Transp_tmax_restart)
                endif

                if (Temp_units == 0) then
                    Transp_tmax_f = Transp_tmax
                else
                    do i = 1, Nhru
                        Transp_tmax_f(i) = c_to_f(Transp_tmax(i))
                    enddo
                endif
                !deallocate ( Transp_tmax )

                if (Init_vars_from_file == 0 .OR. new_values == 1) then
                    motmp = Start_month + 12
                    Tmax_sum = 0.0
                    Transp_check = 0
                    Basin_transp_on = 0
                    do j = 1, Active_hrus
                        i = Hru_route_order(j)
                        if (Start_month == Transp_beg(i)) then
                            if (Start_day > 10) then ! rsr, why 10? if transp_tmax < 300, should be < 10
                                Transp_on(i) = 1
                            else
                                Transp_check(i) = 1
                            endif
                        elseif (Transp_end(i) > Transp_beg(i)) then
                            if (Start_month > Transp_beg(i) .AND. Start_month < Transp_end(i)) Transp_on(i) = 1
                        else
                            if (Start_month > Transp_beg(i) .OR. motmp < Transp_end(i) + 12) Transp_on(i) = 1
                        endif
                        if (Basin_transp_on == 0) then
                            if (Transp_on(i) == 1) Basin_transp_on = 1
                        endif
                    enddo
                endif

            elseif (Process == 'clean') then
                if (Save_vars_to_file == 1) call transp_tindex_restart(0)

            endif

        end function transp_tindex

        !***********************************************************************
        !     Write to or read from restart file
        !***********************************************************************
        subroutine transp_tindex_restart(In_out)
            use PRMS_MODULE, only: Restart_outunit, Restart_inunit
            use UTILS_PRMS, only: check_restart
            implicit none

            ! Argument
            integer(i4), INTENT(IN) :: In_out

            ! Local Variable
            character(len=13) :: module_name

            !***********************************************************************
            if (In_out == 0) then
                write (Restart_outunit) MODNAME
                write (Restart_outunit) Transp_check
                write (Restart_outunit) Tmax_sum
                write (Restart_outunit) Transp_beg
                write (Restart_outunit) Transp_end
                write (Restart_outunit) Transp_tmax
            else
                read (Restart_inunit) module_name
                call check_restart(MODNAME, module_name)
                read (Restart_inunit) Transp_check
                read (Restart_inunit) Tmax_sum
                read (Restart_inunit) Transp_beg_restart
                read (Restart_inunit) Transp_end_restart
                read (Restart_inunit) Transp_tmax_restart
            endif
        end subroutine transp_tindex_restart

end MODULE PRMS_TRANSP_TINDEX
