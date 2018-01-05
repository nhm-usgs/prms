!***********************************************************************
! Read and makes available climate data (tmin, tmax, precip, potential
! solar radiation, potential evapotranspieration) and/or transpiration
! on, by HRU from files pre-processed Data Files available for other
! PRMS modules
!***********************************************************************
module PRMS_CLIMATE_HRU
    use kinds_mod, only: r4, r8, i4, i8
    use prms_constants, only: MAXFILE_LENGTH

    implicit none

    ! Local Variables
    integer(i4), save :: Precip_unit, Tmax_unit, Tmin_unit
    character(len=11), save :: MODNAME

!    ! Control Parameters
!    character(len = MAXFILE_LENGTH), save :: Tmin_day, Tmax_day, Precip_day

    ! Declared Parameters
    real(r4), save, allocatable :: Rain_cbh_adj(:, :)
    real(r4), save, allocatable :: Snow_cbh_adj(:, :)
    real(r4), save, allocatable :: Tmax_cbh_adj(:, :)
    real(r4), save, allocatable :: Tmin_cbh_adj(:, :)

    private :: find_header_end, read_cbh_date, check_cbh_value  ! , check_cbh_intvalue
    public :: climate_hru

    contains

        integer function climate_hru(dim_data, ctl_data)
            use prms_constants, only: MM2INCH, MINTEMP, MAXTEMP
            use PRMS_MODULE, only: Process, Nhru, Model, Climate_precip_flag, Climate_temp_flag, &
                    Start_year, Start_month, Start_day, Cbh_check_flag, Cbh_binary_flag, print_module, &
                    Tmin_day, Tmax_day, Precip_day
            use PRMS_BASIN, only: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv
            use PRMS_CLIMATEVARS, only: Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, Basin_tmin, &
                    Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Hru_ppt, Hru_rain, Hru_snow, Prmx, Pptmix, Newsnow, &
                    Precip_units, Tmax_allrain_f, Adjmix_rain, Basin_ppt, Basin_snow, Basin_rain, Basin_obs_ppt, &
                    Tmax_allsnow_f
            use PRMS_SET_TIME, only: Nowmonth, print_date
            use UTILS_PRMS, only: read_error, find_current_time
            ! use PRMS_CONTROL_FILE, only: control_string
            use parameter_mod, only: declparam, getparam
            ! use PRMS_MMFAPI, only: declparam, getparam
            use PRMS_CLIMATEVARS, only: precip_form, temp_set
            use dimensions_mod, only: dimension_list
            use control_ll_mod, only: control_list
            implicit none

            type(dimension_list), intent(in) :: dim_data
            type(control_list), intent(in) :: ctl_data

            ! Functions
            INTRINSIC ABS, DBLE, SNGL

            ! Local Variables
            integer(i4) :: yr, mo, dy, i, hr, mn, sec, jj, ierr, istop, missing, ios
            real(r8) :: sum_obs
            real(r4) :: tmax_hru, tmin_hru, ppt, harea
            character(len=80), save :: Version_climate_hru

            !***********************************************************************
            climate_hru = 0
            ierr = 0
            if (Process == 'run') then
                if (Climate_temp_flag == 1) then
                    if (Cbh_binary_flag == 0) then
                        read (Tmax_unit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (Tmaxf(i), i = 1, Nhru)
                    else
                        read (Tmax_unit, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (Tmaxf(i), i = 1, Nhru)
                    endif

                    if (Cbh_check_flag == 1) call read_cbh_date(yr, mo, dy, 'Tmaxf', ios, ierr)

                    if (Cbh_binary_flag == 0) then
                        read (Tmin_unit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (Tminf(i), i = 1, Nhru)
                    else
                        read (Tmin_unit, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (Tminf(i), i = 1, Nhru)
                    endif

                    if (Cbh_check_flag == 1) call read_cbh_date(yr, mo, dy, 'Tminf', ios, ierr)
                    Basin_tmax = 0.0D0
                    Basin_tmin = 0.0D0
                    Basin_temp = 0.0D0
                endif

                if (Climate_precip_flag == 1) then
                    if (Cbh_binary_flag == 0) then
                        read (Precip_unit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (Hru_ppt(i), i = 1, Nhru)
                    else
                        read (Precip_unit, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (Hru_ppt(i), i = 1, Nhru)
                    endif

                    if (Cbh_check_flag == 1) call read_cbh_date(yr, mo, dy, 'Hru_ppt', ios, ierr)
                    Basin_ppt = 0.0D0
                    Basin_rain = 0.0D0
                    Basin_snow = 0.0D0
                    sum_obs = 0.0D0
                endif

                if (ierr /= 0) STOP

                missing = 0
                do jj = 1, Active_hrus
                    i = Hru_route_order(jj)
                    harea = Hru_area(i)

                    if (Climate_temp_flag == 1) then
                        if (Cbh_check_flag == 1) then
                            call check_cbh_value('Tmaxf', Tmaxf(i), MINTEMP, MAXTEMP, missing)
                            call check_cbh_value('Tminf', Tminf(i), MINTEMP, MAXTEMP, missing)
                        endif
                        tmax_hru = Tmaxf(i) + Tmax_cbh_adj(i, Nowmonth)
                        tmin_hru = Tminf(i) + Tmin_cbh_adj(i, Nowmonth)
                        call temp_set(i, tmax_hru, tmin_hru, Tmaxf(i), Tminf(i), &
                                &                    Tavgf(i), Tmaxc(i), Tminc(i), Tavgc(i), harea)
                    endif

                    if (Climate_precip_flag == 1) then
                        if (Cbh_check_flag == 1) call check_cbh_value('Hru_ppt', Hru_ppt(i), 0.0, 30.0, missing)

                        !******Initialize HRU variables
                        Pptmix(i) = 0
                        Newsnow(i) = 0
                        Prmx(i) = 0.0
                        Hru_rain(i) = 0.0
                        Hru_snow(i) = 0.0

                        if (Hru_ppt(i) > 0.0) then
                            if (Precip_units == 1) Hru_ppt(i) = Hru_ppt(i) * MM2INCH
                            ppt = Hru_ppt(i)
                            call precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), &
                                    &                         Tmaxf(i), Tminf(i), Pptmix(i), Newsnow(i), &
                                    &                         Prmx(i), Tmax_allrain_f(i, Nowmonth), &
                                    &                         Rain_cbh_adj(i, Nowmonth), Snow_cbh_adj(i, Nowmonth), &
                                    &                         Adjmix_rain(i, Nowmonth), harea, sum_obs, Tmax_allsnow_f(i, Nowmonth))
                        elseif (Hru_ppt(i) < 0.0) then
                            print *, 'ERROR, negative precipitation value entered in CBH File, HRU:', i
                            call print_date(0)
                            ierr = 1
                            !              Hru_ppt(i) = 0.0
                        endif
                    endif

                enddo

                if (missing == 1 .OR. ierr == 1) then
                    call print_date(0)
                    STOP
                endif

                if (Climate_temp_flag == 1) then
                    Basin_tmax = Basin_tmax * Basin_area_inv
                    Basin_tmin = Basin_tmin * Basin_area_inv
                    Basin_temp = Basin_temp * Basin_area_inv
                    Solrad_tmax = real(Basin_tmax, r4)
                    Solrad_tmin = real(Basin_tmin, r4)
                endif

                if (Climate_precip_flag == 1) then
                    Basin_ppt = Basin_ppt * Basin_area_inv
                    Basin_obs_ppt = sum_obs * Basin_area_inv
                    Basin_rain = Basin_rain * Basin_area_inv
                    Basin_snow = Basin_snow * Basin_area_inv
                endif

            elseif (Process == 'declare') then
                Version_climate_hru = 'climate_hru.f90 2017-09-29 13:49:00Z'
                MODNAME = 'climate_hru'

                if (Climate_temp_flag == 1 .OR. Model == 99) call print_module(Version_climate_hru, 'Temperature Distribution    ', 90)
                if (Climate_precip_flag == 1 .OR. Model == 99) call print_module(Version_climate_hru, 'Precipitation Distribution  ', 90)

                !   Declared Parameters
                if (Climate_temp_flag == 1 .OR. Model == 99) then
                    allocate (Tmax_cbh_adj(Nhru, 12))
                    if (declparam(MODNAME, 'tmax_cbh_adj', 'nhru,nmonths', 'real', &
                                           '0.0', '-10.0', '10.0', &
                                           'Monthly maximum temperature adjustment factor for each HRU', &
                                           'Monthly (January to December) adjustment factor to maximum air temperature for each HRU,' // &
                                           ' estimated on the basis of slope and aspect', &
                                           'temp_units', dim_data) /= 0) call read_error(1, 'tmax_cbh_adj')

                    allocate (Tmin_cbh_adj(Nhru, 12))
                    if (declparam(MODNAME, 'tmin_cbh_adj', 'nhru,nmonths', 'real', &
                                           '0.0', '-10.0', '10.0', &
                                           'Monthly minimum temperature adjustment factor for each HRU', &
                                           'Monthly (January to December) adjustment factor to minimum air temperature for each HRU,' // &
                                           ' estimated on the basis of slope and aspect', &
                                           'temp_units', dim_data) /= 0) call read_error(1, 'tmin_cbh_adj')
                endif

                if (Climate_precip_flag == 1 .OR. Model == 99) then
                    allocate (Rain_cbh_adj(Nhru, 12))
                    if (declparam(MODNAME, 'rain_cbh_adj', 'nhru,nmonths', 'real', &
                                           '1.0', '0.5', '2.0', &
                                           'Rain adjustment factor, by month for each HRU', &
                                           'Monthly (January to December) adjustment factor to' // &
                                           ' measured precipitation determined to be rain on' // &
                                           ' each HRU to account for differences in elevation, and so forth', &
                                           'decimal fraction', dim_data) /= 0) call read_error(1, 'rain_cbh_adj')

                    allocate (Snow_cbh_adj(Nhru, 12))
                    if (declparam(MODNAME, 'snow_cbh_adj', 'nhru,nmonths', 'real', &
                                           '1.0', '0.5', '2.0', &
                                           'Snow adjustment factor, by month for each HRU', &
                                           'Monthly (January to December) adjustment factor to' // &
                                           ' measured precipitation determined to be snow on' // &
                                           ' each HRU to account for differences in elevation, and so forth', &
                                           'decimal fraction', dim_data) /= 0) call read_error(1, 'snow_cbh_adj')
                endif

            elseif (Process == 'init') then

                istop = 0
                ierr = 0

                if (Climate_precip_flag == 1) then
                    if (getparam(MODNAME, 'rain_cbh_adj', Nhru * 12, 'real', Rain_cbh_adj) /= 0) call read_error(2, 'rain_cbh_adj')
                    if (getparam(MODNAME, 'snow_cbh_adj', Nhru * 12, 'real', Snow_cbh_adj) /= 0) call read_error(2, 'snow_cbh_adj')

                    call ctl_data%get_data('precip_day', Precip_day, missing_stop=.true.)

                    call find_header_end(Precip_unit, Precip_day, 'precip_day', ierr, 1, Cbh_binary_flag)
                    if (ierr == 1) then
                        istop = 1
                    else
                        call find_current_time(Precip_unit, Start_year, Start_month, Start_day, ierr, Cbh_binary_flag)
                        if (ierr == -1) then
                            print *, 'for first time step, CBH File: ', Precip_day
                            istop = 1
                        endif
                    endif
                endif

                if (Climate_temp_flag == 1) then
                    if (getparam(MODNAME, 'tmax_cbh_adj', Nhru * 12, 'real', Tmax_cbh_adj) /= 0) call read_error(2, 'tmax_cbh_adj')
                    if (getparam(MODNAME, 'tmin_cbh_adj', Nhru * 12, 'real', Tmin_cbh_adj) /= 0) call read_error(2, 'tmin_cbh_adj')

                    call ctl_data%get_data('tmax_day', Tmax_day, missing_stop=.true.)
                    call ctl_data%get_data('tmin_day', Tmin_day, missing_stop=.true.)

                    call find_header_end(Tmax_unit, Tmax_day, 'tmax_day', ierr, 1, Cbh_binary_flag)
                    if (ierr == 1) then
                        istop = 1
                    else
                        call find_current_time(Tmax_unit, Start_year, Start_month, Start_day, ierr, Cbh_binary_flag)
                        if (ierr == -1) then
                            print *, 'for first time step, CBH File: ', Tmax_day
                            istop = 1
                        endif
                    endif
                    call find_header_end(Tmin_unit, Tmin_day, 'tmin_day', ierr, 1, Cbh_binary_flag)
                    if (ierr == 1) then
                        istop = 1
                    else
                        call find_current_time(Tmin_unit, Start_year, Start_month, Start_day, ierr, Cbh_binary_flag)
                        if (ierr == -1) then
                            print *, 'for first time step, CBH File: ', Tmin_day
                            istop = 1
                        endif
                    endif
                endif

                if (istop == 1) STOP 'ERROR in climate_hru'

            endif

        end function climate_hru

        !***********************************************************************
        !     Check CBH integer value limits
        !***********************************************************************
        ! subroutine check_cbh_intvalue(Var, Var_value, Lower_val, Upper_val, Missing)
        !     use PRMS_SET_TIME, only: print_date
        !     implicit none
        !
        !     ! Arguments
        !     integer, intent(in) :: Var_value, Lower_val, Upper_val
        !     character(len = *), intent(in) :: Var
        !     integer, intent(inout) :: Missing
        !
        !     !***********************************************************************
        !     if (Var_value < Lower_val .OR. Var_value > Upper_val) then
        !         print *, 'ERROR, bad value, variable: ', Var, ' Value:', Var_value
        !         print *, '       lower bound:', Lower_val, ' upper bound:', Upper_val
        !         Missing = 1
        !         call print_date(0)
        !     endif
        ! end subroutine check_cbh_intvalue

        !***********************************************************************
        !     Check CBH value limits
        !***********************************************************************
        subroutine check_cbh_value(Var, Var_value, Lower_val, Upper_val, Missing)
            use PRMS_SET_TIME, only: print_date
            implicit none

            ! Arguments
            real(r4), intent(in) :: Lower_val, Upper_val
            real(r4), intent(inout) :: Var_value
            character(len=*), intent(in) :: Var
            integer(i4), intent(inout) :: Missing

            ! Functions
            !INTRINSIC ISNAN

            !***********************************************************************
            !if ( ISNAN(Var_value) ) then
            !  print *, 'ERROR, NaN value found for variable: ', Var
            !  Var_value = 0.0
            !  Missing = 1
            !  call print_date(0)
            !elseif ( Var_value<Lower_val .OR. Var_value>Upper_val ) then
            if (Var_value < Lower_val .OR. Var_value > Upper_val) then
                print *, 'ERROR, bad value, variable: ', Var, ' Value:', Var_value
                print *, '       lower bound:', Lower_val, ' upper bound:', Upper_val
                Missing = 1
                call print_date(0)
            endif
        end subroutine check_cbh_value

        !***********************************************************************
        !     Read File to line before data starts in file
        !***********************************************************************
        subroutine find_header_end(Iunit, Fname, Paramname, Iret, Cbh_flag, Cbh_binary_flag)
            use PRMS_MODULE, only: Nhru
            use UTILS_PRMS, only: PRMS_open_input_file
            use data_mod, only: str_arr_type
            implicit none

            ! Argument
            integer(i4), intent(in) :: Cbh_flag, Cbh_binary_flag
            integer(i4), intent(out) :: Iunit, Iret
            character(len=*), intent(in) :: Fname
            character(len=*), intent(in) :: Paramname

            ! Local Variables
            integer(i4) :: i, ios, dim
            character(len=4) :: dum
            character(len=80) :: dum2

            !***********************************************************************
            call PRMS_open_input_file(Iunit, Fname, Paramname, Cbh_binary_flag, Iret)
            if (Iret == 0) then
                ! read to line before data starts in each file
                i = 0
                do WHILE (i == 0)
                    if (Cbh_binary_flag == 0) then
                        read (Iunit, FMT='(A4)', IOSTAT=ios) dum
                    else
                        read (Iunit, IOSTAT=ios) dum2
                        read (dum2, '(A4)') dum
                    endif

                    if (ios /= 0) then
                        WRITE (*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                                'check to be sure the input file is in correct format'
                        Iret = 1
                        EXIT
                    elseif (dum == '####') then
                        if (Cbh_flag == 0) EXIT
                        BACKSPACE Iunit
                        BACKSPACE Iunit

                        if (Cbh_binary_flag == 0) then
                            read (Iunit, *, IOSTAT=ios) dum, dim
                        else
                            read (Iunit, IOSTAT=ios) dum2
                            read (dum2, *) dum, dim
                        endif

                        if (ios /= 0) then
                            WRITE (*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                                    'check to be sure dimension line is in correct format'
                            Iret = 1
                            EXIT
                        endif

                        if (dim /= Nhru) then
                            print '(/,2(A,I7))', '***CBH file dimension incorrect*** nhru=', Nhru, ' CBH dimension=', &
                                    dim, ' File: ' // Fname
                            STOP 'ERROR: update Control File with correct CBH files'
                        endif

                        if (Cbh_binary_flag == 0) then
                            read (Iunit, FMT='(A4)', IOSTAT=ios) dum
                        else
                            read (Iunit, IOSTAT=ios) dum
                        endif
                        i = 1
                    endif
                enddo
            endif
        end subroutine find_header_end

        !***********************************************************************
        !     Read a day in the CBH File
        !***********************************************************************
        subroutine read_cbh_date(Year, Month, Day, Var, Ios, Iret)
            use PRMS_SET_TIME, only: Nowyear, Nowmonth, Nowday, print_date

            ! Argument
            integer(i4), intent(in) :: Year, Month, Day, Ios
            character(len=*), intent(in) :: Var
            integer(i4), intent(inout) :: Iret

            ! Local Variables
            integer(i4) :: right_day

            !***********************************************************************
            right_day = 1
            if (Year /= Nowyear .OR. Month /= Nowmonth .OR. Day /= Nowday) right_day = 0

            if (Ios /= 0 .OR. right_day == 0) then
                print *, 'ERROR, reading CBH File, variable: ', Var, ' IOSTAT=', Ios

                if (Ios == -1) then
                    print *, '       End-of-File found'
                elseif (right_day == 0) then
                    print *, '       Wrong day found'
                else
                    print *, '       Invalid data value found'
                endif

                call print_date(0)
                Iret = 1
            endif
        end subroutine read_cbh_date
end module PRMS_CLIMATE_HRU
