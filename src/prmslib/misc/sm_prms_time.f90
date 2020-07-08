submodule (PRMS_SET_TIME) sm_prms_time

contains
  !***********************************************************************
  ! Time_t constructor
  module function constructor_Time(ctl_data, hemisphere) result(this)
    use UTILS_TIME, only: gregorian_to_julian
    use prms_constants, only: dp, FT2_PER_ACRE, SECS_PER_DAY, SECS_PER_HOUR, &
                              HOUR_PER_DAY, MIN_PER_HOUR, NORTHERN
    implicit none

    type(Time_t) :: this
    type(Control), intent(in) :: ctl_data
    integer(i32), optional, intent(in) :: hemisphere

    ! Local Variables
    character(len=:), allocatable :: days_since

    ! ------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              model_start => ctl_data%start_time%values, &
              model_end => ctl_data%end_time%values, &
              save_vars_to_file => ctl_data%save_vars_to_file%value)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      this%Timestep_hours = SNGL(TIMESTEP_DELTA)
      this%Timestep_days = this%Timestep_hours / HOUR_PER_DAY
      this%Timestep_minutes = this%Timestep_hours * MIN_PER_HOUR
      this%Timestep_seconds = TIMESTEP_DELTA * SECS_PER_HOUR

      ! Check to see if in a daily or subdaily time step
      if (this%Timestep_hours > HOUR_PER_DAY) then
        print *, 'ERROR, timestep > daily, fix Data File, timestep:', this%Timestep_hours
        STOP
      elseif (this%Timestep_hours < HOUR_PER_DAY) then
        print *, 'ERROR, timestep < daily for daily model, fix Data File', this%Timestep_hours
        STOP
      endif

      this%Cfs_conv = FT2_PER_ACRE / 12.0_dp / this%Timestep_seconds

      this%Nowtime = model_start
      this%Nowyear = this%Nowtime(YEAR)
      this%Nowmonth = this%Nowtime(MONTH)
      this%Nowday = this%Nowtime(DAY)
      this%Nowhour = this%Nowtime(HOUR)
      this%Nowminute = this%Nowtime(MINUTE)

      ! Default to the northern hemisphere if hemisphere was not specified
      if (present(hemisphere)) then
        call this%set_hemisphere(hemisphere)
      else
        call this%set_hemisphere(NORTHERN)
      end if

      ! TODO: If cfs2inches is really needed it should not be in the time object
      !       causing a dependency to the Basin object.
      ! this%Cfs2inches = basin_area_inv * 12.0_dp * this%Timestep_seconds / FT2_PER_ACRE

      this%start_jdn = gregorian_to_julian(model_start(YEAR), model_start(MONTH), model_start(DAY))
      this%end_jdn = gregorian_to_julian(model_end(YEAR), model_end(MONTH), model_end(DAY))
      this%current_jdn = this%start_jdn
      this%days_in_model = this%end_jdn - this%start_jdn + 1

      this%months_in_model = (model_end(YEAR) - model_start(YEAR) - 1) * 12 + &
                             (12 - model_start(MONTH) + 1) + model_end(MONTH)

      this%years_in_model = model_end(YEAR) - model_start(YEAR)
      if (model_start(MONTH) <= model_end(MONTH)) then
        this%years_in_model = this%years_in_model + 1
      end if

      this%days_since_start = 0

      write(this%start_string, 9001) model_start(YEAR), model_start(MONTH), model_start(DAY)
      9001 format(I4, '-', I2.2, '-', I2.2)
      ! write(*, *) 'start_string: ' // this%start_string

      ! NOTE: Number_timesteps and days_in_model are the same
      this%Number_timesteps = this%end_jdn - this%start_jdn + 1
      this%Timestep = 0   ! Pre-run init value

      if (save_vars_to_file == 1) then
        ! Create restart dimensions and variables
        days_since = 'days since ' // this%start_string // ' 00:00:00'
        call ctl_data%add_time_dimension(dim_name='time', dim_size=1, dim_var_name='time', &
                                         units=days_since, calendar='standard')
      end if
    end associate
  end function


  module subroutine cleanup_Time(this, ctl_data)
    implicit none

    class(Time_t), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! ------------------------------------------------------------------------
    ! ------------------------------------------------------------------------
    if (ctl_data%save_vars_to_file%value == 1) then
      ! Write out this module's restart variables
      call ctl_data%write_restart_variable('time', this%days_since_start)
    end if

  end subroutine

  module subroutine update_summer_flag(this)
    class(Time_t), intent(inout) :: this
    ! ------------------------------------------------------------------------
    ! Summer is based on equinox:
    !   Julian days 79 to 265 for Northern hemisphere
    !   Julian day 265 to 79 in Southern hemisphere
    this%Summer_flag = 1 ! 1 = summer, 0 = winter
    if (this%hemisphere == NORTHERN) then
      ! Northern Hemisphere
      if (this%day_of_year < 79 .OR. this%day_of_year > 265) this%Summer_flag = 0 ! Equinox
    else
      ! Southern Hemisphere
      if (this%day_of_year > 79 .AND. this%day_of_year < 265) this%Summer_flag = 0 ! Equinox
    endif
  end subroutine

  module function next(this) result(res)
    use UTILS_TIME, only: gregorian_to_julian, julian_to_gregorian, ordinal_date
    use prms_constants, only: FT2_PER_ACRE, SECS_PER_DAY, SECS_PER_HOUR, &
                              HOUR_PER_DAY, MIN_PER_HOUR, dp

    logical :: res
    class(Time_t), intent(inout) :: this

    ! ------------------------------------------------------------------------
    res = .true.
    this%Timestep = this%Timestep + 1

    if (this%Timestep > this%Number_timesteps) then
      !! End of simulation reached
      res = .false.
      return
    endif

    this%Nowtime = julian_to_gregorian(this%current_jdn)
    this%Nowyear = this%Nowtime(YEAR)
    this%Nowmonth = this%Nowtime(MONTH)
    this%Nowday = this%Nowtime(DAY)
    this%Nowhour = this%Nowtime(HOUR)
    this%Nowminute = this%Nowtime(MINUTE)

    this%days_since_start = gregorian_to_julian(this%Nowtime(YEAR), this%Nowtime(MONTH), this%Nowtime(DAY)) - this%start_jdn

    this%day_of_year = ordinal_date(this%Nowtime, 'calendar', this%hemisphere)
    this%day_of_solar_year = ordinal_date(this%Nowtime, 'solar', this%hemisphere)
    this%day_of_water_year = ordinal_date(this%Nowtime, 'water', this%hemisphere)

    call this%update_summer_flag()

    ! Increment the current_jdn for the next time this function is called
    this%current_jdn = this%current_jdn + 1

    ! TODO: If cfs2inches is really needed it should not be in the time object
    !       causing a dependency to the Basin object.
    ! this%Cfs2inches = basin_area_inv * 12.0D0 * this%Timestep_seconds / FT2_PER_ACRE

    ! print *, this%Timestep, ": ", this%Nowtime(1), this%Nowtime(2), this%Nowtime(3), &
    !          this%current_jdn, this%day_of_year, this%day_of_solar_year, this%day_of_water_year
  end function

  !***********************************************************************
  ! Print date
  ! 2017-10-30 PAN: moved here from utils_prms.f90
  !***********************************************************************
  module subroutine print_date(this, Flag)
    implicit none

    ! Arguments
    class(Time_t), intent(in) :: this
    integer(i32), intent(in) :: Flag

    !***********************************************************************
    if (Flag == 1) then
      print 9001, this%Nowyear, this%Nowmonth, this%Nowday, this%Nowhour, this%Nowminute
    elseif (Flag == 0) then
      print 9001, this%Nowyear, this%Nowmonth, this%Nowday
    else
      WRITE (Flag, 9001) this%Nowyear, this%Nowmonth, this%Nowday
    endif

    9001 FORMAT ('    Date: ', I4, 2('/', I2.2), I3.2, ':', I2.2, /)
  end subroutine

  module subroutine set_hemisphere(this, hemisphere)
    use iso_fortran_env, only: output_unit
    use UTILS_TIME, only: ordinal_date
    implicit none

    class(Time_t), intent(inout) :: this
    integer(i32), intent(in) :: hemisphere
    ! --------------------------------------------------------------------------
    if (hemisphere > 1) then
      write(output_unit, *) 'ERROR: set_hemisphere() - Hemisphere must be either 0 (NORTHERN) or 1 (SOUTHERN)'
      stop
    end if
    this%hemisphere = hemisphere

    this%day_of_year = ordinal_date(this%Nowtime, 'calendar', this%hemisphere)
    this%day_of_solar_year = ordinal_date(this%Nowtime, 'solar', this%hemisphere)
    this%day_of_water_year = ordinal_date(this%Nowtime, 'water', this%hemisphere)

    call this%update_summer_flag()
  end subroutine

  !***********************************************************************
  ! Return the last day of the given month
  !***********************************************************************
  pure module function last_day_of_month(this, mon) result(res)
    use UTILS_TIME, only: leap_day
    implicit none

    integer(i32) :: res
    class(Time_t), intent(in) :: this
    integer(i32), intent(in) :: mon

    ! Local variables
    ! integer(r32) :: yrdays
    ! ===============================
    ! yrdays = 365
    res = DAYPMO(mon)

    if (leap_day(this%Nowyear)) then
    !   yrdays = yrdays + 1

      if (mon == 2) res = res + 1
    endif
  end function

end submodule
