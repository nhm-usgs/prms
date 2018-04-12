submodule (PRMS_TRANSP_TINDEX) sm_transp_tindex
contains
  !***********************************************************************
  ! Transp_tindex constructor
  module function constructor_Transp_tindex(ctl_data, param_data, model_basin, climate) result(this)
    ! use Control_class, only: Control
    ! use Parameters_class, only: Parameters
    ! use PRMS_BASIN, only: Basin
    ! use PRMS_CLIMATEVARS, only: Climateflow
    use conversions_mod, only: c_to_f
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Transp_tindex) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(inout) :: climate

    integer(i32) :: ii
      !! counter
    integer(i32) :: chru
      !! Current HRU

    ! ------------------------------------------------------------------------

    ! These are only allocated when initializing from restart file
    ! integer(i32), allocatable :: transp_beg_restart(:)
    ! integer(i32), allocatable :: transp_end_restart(:)
    ! real(r32), allocatable :: transp_tmax_restart(:)

    associate(nhru => ctl_data%nhru%values(1), &
              print_debug => ctl_data%print_debug%value, &
              st_month => ctl_data%start_time%values(MONTH), &
              st_day => ctl_data%start_time%values(DAY), &
              temp_units => param_data%temp_units%values(1), &
              transp_beg => param_data%transp_beg%values, &
              transp_end => param_data%transp_end%values, &
              transp_tmax => param_data%transp_tmax%values)

    if (print_debug > -2) then
      ! Output module and version information
      call print_module_info(MODNAME, MODDESC, MODVERSION)
    endif

    allocate(this%tmax_sum(nhru))
    allocate(this%transp_check(nhru))
    allocate(this%transp_tmax_f(nhru))

    if (temp_units == 0) then
      this%transp_tmax_f = transp_tmax(:)
    else
      do ii=1, ctl_data%nhru%values(1)
        this%transp_tmax_f(ii) = c_to_f(transp_tmax(ii))
      enddo
    endif

    ! TODO: Incorporate the load from restart file stuff
    this%tmax_sum = 0.0
    this%transp_check = 0
    climate%basin_transp_on = 0

    ! associate(st_month => ctl_data%start_time%values(MONTH), &
    !           st_day => ctl_data%start_time%values(DAY), &
    !           transp_beg => param_data%transp_beg%values, &
    !           transp_end => param_data%transp_end%values)

      do ii=1, model_basin%active_hrus
        chru = model_basin%hru_route_order(ii)

        if (st_month == transp_beg(chru)) then
          if (st_day > 10) then
            climate%transp_on(chru) = 1
          else
            this%transp_check(chru) = 1
          endif
        elseif (transp_end(chru) > transp_beg(chru)) then
          if (st_month > transp_beg(chru) .and. st_month < transp_end(chru)) then
            climate%transp_on(chru) = chru
          endif
        else
          if (st_month > transp_beg(chru) .or. (st_month + 12) < (transp_end(chru) + 12)) then
            ! TODO: shouldn't the 2nd line of the conditional just be:
            !       st_month < transp_end(chr)
            climate%transp_on(chru) = 1
          endif
        endif

        if (climate%basin_transp_on == 0) then
          if (climate%transp_on(chru) == 1) then
            climate%basin_transp_on = 1
          endif
        endif
      enddo

    end associate
  end function


  module subroutine run_Transp_tindex(this, ctl_data, param_data, model_time, model_basin, climate)
    use conversions_mod, only: c_to_f
    ! use Control_class, only: Control
    ! use PRMS_SET_TIME, only: Time_t
    ! use PRMS_BASIN, only: Basin
    ! use Parameters_class, only: Parameters
    ! use PRMS_CLIMATEVARS, only: Climateflow

    implicit none

    class(Transp_tindex), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(inout) :: climate

    ! Local Variables
    integer(i32) :: chru
      !! Current HRU
    integer(i32) :: j
      !! Counter

    !***********************************************************************

    !******Set switch for active transpiration period
    climate%basin_transp_on = 0

    associate(curr_month => model_time%Nowmonth, &
              curr_day => model_time%Nowday, &
              transp_beg => param_data%transp_beg%values, &
              transp_end => param_data%transp_end%values)

      do j = 1, model_basin%active_hrus
        chru = model_basin%hru_route_order(j)

        !******check for month to turn transp_check switch on or
        !******transpiration switch off (transp_on)
        if (curr_day == 1) then
          !******check for end of period
          if (curr_month == transp_end(chru)) then
            climate%transp_on(chru) = 0
            this%transp_check(chru) = 0
            this%tmax_sum(chru) = 0.0
          endif

          !******check for month to turn transpiration switch (transp_check) on or off
          if (curr_month == transp_beg(chru)) then
            this%transp_check(chru) = 1
            this%tmax_sum(chru) = 0.0
          endif
        endif

        !****** If in checking period, then for each day sum the maximum
        !****** temperature until it's greater than temperature index parameter,
        !****** at which time, turn transpiration switch on, check switch off
        ! freezing temperature assumed to be 32 degrees Fahrenheit
        if (this%transp_check(chru) == 1) then
          if (climate%tmaxf(chru) > 32.0) then
            this%tmax_sum(chru) = this%tmax_sum(chru) + climate%tmaxf(chru)
          endif

          if (this%tmax_sum(chru) > this%transp_tmax_f(chru)) then
            climate%transp_on(chru) = 1
            this%transp_check(chru) = 0
            this%tmax_sum(chru) = 0.0
          endif
        endif

        if (climate%basin_transp_on == 0) then
          if (climate%transp_on(chru) == 1) climate%basin_transp_on = 1
        endif
      enddo
    end associate
  end subroutine

  module function module_name() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODNAME
  end function

  module function version() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODVERSION
  end function
  !***********************************************************************
  !     Write to or read from restart file
  !***********************************************************************
  ! subroutine transp_tindex_restart(In_out)
  !   use PRMS_MODULE, only: Restart_outunit, Restart_inunit
  !   use UTILS_PRMS, only: check_restart
  !   implicit none
  !
  !   ! Argument
  !   integer(i32), intent(in) :: In_out
  !
  !   ! Local Variable
  !   character(len=13) :: module_name
  !
  !   !***********************************************************************
  !   if (In_out == 0) then
  !     write (Restart_outunit) MODNAME
  !     write (Restart_outunit) Transp_check
  !     write (Restart_outunit) Tmax_sum
  !     write (Restart_outunit) Transp_beg
  !     write (Restart_outunit) Transp_end
  !     write (Restart_outunit) Transp_tmax
  !   else
  !     read (Restart_inunit) module_name
  !     call check_restart(MODNAME, module_name)
  !     read (Restart_inunit) Transp_check
  !     read (Restart_inunit) Tmax_sum
  !     read (Restart_inunit) Transp_beg_restart
  !     read (Restart_inunit) Transp_end_restart
  !     read (Restart_inunit) Transp_tmax_restart
  !   endif
  ! end subroutine transp_tindex_restart
end submodule
