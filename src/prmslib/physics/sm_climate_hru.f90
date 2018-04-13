submodule (PRMS_CLIMATE_HRU) sm_climate_hru

contains
  !***********************************************************************
  ! Climate_HRU constructor
  module function constructor_Climate_HRU(ctl_data, param_data) result(this)
    ! use Control_class, only: Control
    ! use Parameters_class, only: Parameters
    use UTILS_PRMS, only: find_current_time, print_module_info
    implicit none

    type(Climate_HRU) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! ----------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%values(1), &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! TODO: This is only used for file-related procedures
      this%nhru = nhru

      if (ctl_data%precip_module%values(1)%s == 'climate_hru') then
        call this%find_header_end(this%precip_funit, ierr, &
                                  ctl_data%precip_day%values(1)%s, 'precip_day', &
                                  1, cbh_binary_flag)
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%precip_funit, start_time, cbh_binary_flag)
        endif
      endif

      if (ctl_data%temp_module%values(1)%s == 'climate_hru') then
        call this%find_header_end(this%tmax_funit, ierr, ctl_data%tmax_day%values(1)%s, &
                                  'tmax_day', 1, cbh_binary_flag)
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%tmax_funit, start_time, cbh_binary_flag)
        endif

        ! Iunit, Iret, Fname, Paramname, Cbh_flag, Cbh_binary_flag
        call this%find_header_end(this%tmin_funit, ierr, ctl_data%tmin_day%values(1)%s, &
                                  'tmin_day', 1, cbh_binary_flag)
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%tmin_funit, start_time, cbh_binary_flag)
        endif
      endif

      if (istop == 1) STOP 'ERROR in climate_hru'
    end associate
  end function

  module subroutine run_Climate_HRU(this, ctl_data, param_data, model_time, model_basin, climate)
    use prms_constants, only: MM2INCH
    implicit none

    class(Climate_HRU), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(inout) :: climate

    ! Local variables
    integer(i32) :: chru
    integer(i32) :: ierr = 0
    integer(i32) :: ios = 0
    integer(i32) :: jj
    integer(i32) :: yr, mo, dy, hr, mn, sec
      !! junk vars to hold time info from files
    real(r32) :: tmax_hru  ! different from what's defined in climateflow.f90
    real(r32) :: tmin_hru  ! different from what's defined in climateflow.f90
    real(r32) :: ppt
    real(r64) :: sum_obs

    integer(i32) :: idx1D
      !! 1D index from 2D

    ! ----------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth, &
              nhru => ctl_data%nhru%values(1), &
              hru_area => param_data%hru_area%values, &
              tmax_cbh_adj => param_data%tmax_cbh_adj%values, &
              tmin_cbh_adj => param_data%tmin_cbh_adj%values, &
              rain_cbh_adj => param_data%rain_cbh_adj%values, &
              snow_cbh_adj => param_data%snow_cbh_adj%values, &
              adjmix_rain => param_data%adjmix_rain%values)

      if (ctl_data%temp_module%values(1)%s == 'climate_hru') then
        read(this%tmax_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (climate%tmaxf(jj), jj=1, nhru)
        read(this%tmin_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (climate%tminf(jj), jj=1, nhru)
        climate%basin_tmax = 0.0
        climate%basin_tmin = 0.0
        climate%basin_temp = 0.0
      endif

      if (ctl_data%precip_module%values(1)%s == 'climate_hru') then
        read(this%precip_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (climate%hru_ppt(jj), jj=1, nhru)
        climate%basin_ppt = 0.0
        climate%basin_rain = 0.0
        climate%basin_snow = 0.0
        sum_obs = 0.0
      endif

      do jj = 1, model_basin%active_hrus
        chru = model_basin%hru_route_order(jj)
        idx1D = (curr_month - 1) * nhru + chru

        if (ctl_data%temp_module%values(1)%s == 'climate_hru') then
          tmax_hru = climate%tmaxf(chru) + tmax_cbh_adj(idx1D)
          tmin_hru = climate%tminf(chru) + tmin_cbh_adj(idx1D)

          ! param_data, ihru, hru_area, tmax, tmin
          call climate%temp_set(param_data, chru, hru_area(chru), tmax_hru, tmin_hru)
        endif

        if (ctl_data%precip_module%values(1)%s == 'climate_hru') then
          !******Initialize HRU variables
          climate%pptmix(chru) = 0
          climate%newsnow(chru) = 0
          climate%prmx(chru) = 0.0
          climate%hru_rain(chru) = 0.0
          climate%hru_snow(chru) = 0.0

          if (climate%hru_ppt(chru) > 0.0) then
            if (param_data%precip_units%values(1) == 1) then
              climate%hru_ppt(chru) = climate%hru_ppt(chru) * MM2INCH
            endif

            ppt = climate%hru_ppt(chru)

            ! ihru, month, hru_area, adjmix_rain, rain_adj, snow_adj, precip, sum_obs
            call climate%precip_form(chru, curr_month, hru_area(chru), adjmix_rain(idx1D), &
                                     rain_cbh_adj(idx1D), snow_cbh_adj(idx1D), &
                                     ppt, sum_obs)
          elseif (climate%hru_ppt(chru) < 0.0) then
            print *, 'ERROR, negative precipitation value entered in CBH File, HRU:', chru
            ! call print_date(0)
            ierr = 1
          endif
        endif
      enddo

      if (ierr == 1) then
        ! call print_date(0)
        STOP
      endif

      if (ctl_data%temp_module%values(1)%s == 'climate_hru') then
        climate%basin_tmax = climate%basin_tmax * model_basin%basin_area_inv
        climate%basin_tmin = climate%basin_tmin * model_basin%basin_area_inv
        climate%basin_temp = climate%basin_temp * model_basin%basin_area_inv
        climate%solrad_tmax = real(climate%basin_tmax, r32)
        climate%solrad_tmin = real(climate%basin_tmin, r32)
      endif

      if (ctl_data%precip_module%values(1)%s == 'climate_hru') then
        climate%basin_ppt = climate%basin_ppt * model_basin%basin_area_inv
        climate%basin_obs_ppt = sum_obs * model_basin%basin_area_inv
        climate%basin_rain = climate%basin_rain * model_basin%basin_area_inv
        climate%basin_snow = climate%basin_snow * model_basin%basin_area_inv
      endif
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
  !     Read File to line before data starts in file
  !***********************************************************************
  module subroutine find_header_end(this, Iunit, Iret, Fname, Paramname, Cbh_flag, &
                                    Cbh_binary_flag)
    use UTILS_PRMS, only: PRMS_open_input_file
    implicit none

    ! Argument
    class(Climate_HRU), intent(inout) :: this
    integer(i32), intent(out) :: Iunit
    integer(i32), intent(out) :: Iret
    character(len=*), intent(in) :: Fname
    character(len=*), intent(in) :: Paramname
    integer(i32), intent(in) :: Cbh_flag
    integer(i32), intent(in) :: Cbh_binary_flag

    ! Local Variables
    integer(i32) :: i
    integer(i32) :: ios
    integer(i32) :: dim
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

          if (dim /= this%nhru) then
            print '(/,2(A,I7))', '***CBH file dimension incorrect*** nhru=', this%nhru, ' CBH dimension=', &
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
  module subroutine read_cbh_date(this, model_time, Year, Month, Day, Var, Ios, Iret)
    ! use PRMS_SET_TIME, only: Time_t

    ! Argument
    class(Climate_HRU), intent(inout) :: this
    type(Time_t), intent(in) :: model_time
    integer(i32), intent(in) :: Year
    integer(i32), intent(in) :: Month
    integer(i32), intent(in) :: Day
    integer(i32), intent(in) :: Ios
    character(len=*), intent(in) :: Var
    integer(i32), intent(inout) :: Iret

    ! Local Variables
    integer(i32) :: right_day

    !***********************************************************************
    right_day = 1
    if (Year /= model_time%Nowyear .OR. Month /= model_time%Nowmonth .OR. Day /= model_time%Nowday) right_day = 0

    if (Ios /= 0 .OR. right_day == 0) then
      print *, 'ERROR, reading CBH File, variable: ', Var, ' IOSTAT=', Ios

      if (Ios == -1) then
        print *, '       End-of-File found'
      elseif (right_day == 0) then
        print *, '       Wrong day found'
      else
        print *, '       Invalid data value found'
      endif

      call model_time%print_date(0)
      Iret = 1
    endif
  end subroutine read_cbh_date
end submodule
