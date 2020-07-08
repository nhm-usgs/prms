submodule (PRMS_SET_TIME) sm_prms_time

contains
  !***********************************************************************
  ! Time_t constructor
  module function constructor_Time(ctl_data, hemisphere) result(this)
    use prms_constants, only: dp, FT2_PER_ACRE, SECS_PER_DAY, SECS_PER_HOUR, &
                              HOUR_PER_DAY, MIN_PER_HOUR, NORTHERN
    implicit none

    type(Time_t) :: this
    type(Control), intent(in) :: ctl_data
    integer(i32), optional, intent(in) :: hemisphere
    ! type(Basin), intent(in) :: model_basin

    ! Functions
    INTRINSIC SNGL

    ! Local Variables
    ! integer(i32) :: startday
      !! Julian day of model start_time
    integer(i32) :: endday
      !! Julian day of model end_time
    ! real(r64) :: dt
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

      this%Nowtime = model_start

      ! Default to the northern hemisphere if hemisphere was not specified
      if (present(hemisphere)) then
        call this%set_hemisphere(hemisphere)
      else
        call this%set_hemisphere(NORTHERN)
      end if

      ! original declare stuff
      ! this%Timestep_seconds = SECS_PER_DAY
      ! this%Cfs_conv = FT2_PER_ACRE / 12.0_dp / this%Timestep_seconds
      ! 2019-08-09 PAN: Added this back
      this%Timestep_seconds = SECS_PER_DAY
      this%Cfs_conv = FT2_PER_ACRE / 12.0_dp / this%Timestep_seconds

      ! TODO: If cfs2inches is really needed it should not be in the time object
      !       causing a dependency to the Basin object.
      ! this%Cfs2inches = basin_area_inv * 12.0_dp * this%Timestep_seconds / FT2_PER_ACRE

      this%start_jdn = compute_julday(model_start(YEAR), model_start(MONTH), model_start(DAY))
      this%end_jdn = compute_julday(model_end(YEAR), model_end(MONTH), model_end(DAY))
      this%days_in_model = this%end_jdn - this%start_jdn + 1

      this%months_in_model = (model_end(YEAR) - model_start(YEAR) - 1) * 12 + &
                             (12 - model_start(MONTH) + 1) + model_end(MONTH)

      this%years_in_model = model_end(YEAR) - model_start(YEAR)
      if (model_start(MONTH) <= model_end(MONTH)) then
        this%years_in_model = this%years_in_model + 1
      end if

      endday = compute_julday(model_end(YEAR), model_end(MONTH), model_end(DAY))
      this%days_since_start = 0

      write(this%start_string, 9001) model_start(YEAR), model_start(MONTH), model_start(DAY)
      9001 format(I4, '-', I2.2, '-', I2.2)
      ! write(*, *) 'start_string: ' // this%start_string

      this%Number_timesteps = endday - this%start_jdn + 1
      this%Timestep = 0
      this%Julian_day_absolute = this%start_jdn

      this%Nowyear = this%Nowtime(YEAR)
      this%Nowmonth = this%Nowtime(MONTH)
      this%Nowday = this%Nowtime(DAY)
      this%Nowhour = this%Nowtime(HOUR)
      this%Nowminute = this%Nowtime(MINUTE)

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

      if (save_vars_to_file == 1) then
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

    this%Nowtime = julian_to_gregorian(this%Julian_day_absolute)
    this%day_of_year = this%ordinal_date(this%Nowtime, 'calendar', this%hemisphere)
    this%day_of_solar_year = this%ordinal_date(this%Nowtime, 'solar', this%hemisphere)
    this%day_of_water_year = this%ordinal_date(this%Nowtime, 'water', this%hemisphere)

    call this%update_summer_flag()

    this%Julian_day_absolute = this%Julian_day_absolute + 1
    this%days_since_start = this%compute_julday(this%Nowtime(1), this%Nowtime(2), this%Nowtime(3)) - this%start_jdn

    this%Nowyear = this%Nowtime(1)
    this%Nowmonth = this%Nowtime(2)
    this%Nowday = this%Nowtime(3)
    this%Nowhour = this%Nowtime(4)
    this%Nowminute = this%Nowtime(5)

    ! NOTE: This stuff shouldn't change once it's initialized
    ! this%Timestep_hours = SNGL(TIMESTEP_DELTA)
    ! this%Timestep_days = this%Timestep_hours / HOUR_PER_DAY
    ! this%Timestep_minutes = this%Timestep_hours * MIN_PER_HOUR
    ! this%Timestep_seconds = TIMESTEP_DELTA * SECS_PER_HOUR
    ! this%Cfs_conv = FT2_PER_ACRE / 12.0_dp / this%Timestep_seconds

    ! TODO: If cfs2inches is really needed it should not be in the time object
    !       causing a dependency to the Basin object.
    ! this%Cfs2inches = basin_area_inv * 12.0D0 * this%Timestep_seconds / FT2_PER_ACRE

    ! print *, this%Timestep, ": ", this%Nowtime(1), this%Nowtime(2), this%Nowtime(3), &
    !          this%Julian_day_absolute, this%day_of_year, this%day_of_solar_year, this%day_of_water_year

    ! Check to see if in a daily or subdaily time step
    ! if (this%Timestep_hours > HOUR_PER_DAY) then
    !   print *, 'ERROR, timestep > daily, fix Data File, timestep:', this%Timestep_hours
    !   STOP
    ! elseif (this%Timestep_hours < HOUR_PER_DAY) then
    !   print *, 'ERROR, timestep < daily for daily model, fix Data File', this%Timestep_hours
    !   STOP
    ! endif
  end function

  !***********************************************************************
  ! ordinal_date
  ! computes the ordinal day of the year given a Gregorian calendar date
  ! (Year, Month, Day) relative to:
  !   calendar (Jan 1),
  !   solar (12/22 in Northern; 6/21 in Southern) and
  !   water year (10/1 in Northern; 4/1 in Southern) start dates.
  ! The day of the year starts at noon of the Gregorian day and
  ! extends to noon the next Gregorian day.
  !
  ! 2017-10-30 PAN: moved here from utils_prms.f90
  !***********************************************************************
  module function ordinal_date(datetime, Year_type, hemisphere) result(res)
    use prms_constants, only: YEAR, MONTH, DAY
    implicit none

    ! Arguments
    integer(i32) :: res
    ! class(Time_t) :: this
    integer(i32), intent(in) :: datetime(6)
    character(len=*), intent(in) :: Year_type
      !! One of: "calendar", "solar", "water", "absolute"
    integer(i32), intent(in) :: hemisphere
      !! Hemisphere (0=North; 1=South)

    ! Local Variables
    integer(i32) :: reftime_year
    integer(i32) :: reftime_month
    integer(i32) :: reftime_day
    ! integer(i32) :: time_array(6)
    integer(i32) :: yr
    integer(i32) :: mo
    integer(i32) :: dy
    integer(i32) :: absolute_julday
    integer(i32) :: relative_julday
    logical :: found

    !***********************************************************************
    yr = datetime(YEAR)
    mo = datetime(MONTH)
    dy = datetime(DAY)

    found = .false.

    ! set reftime depending on type arg
    if (Year_type == 'solar') then
      found = .true.

      if (hemisphere == NORTHERN) then ! Northern
        if (mo == 12 .AND. dy > 21) then
          reftime_year = yr
        else
          reftime_year = yr - 1
        endif

        reftime_month = 12
        reftime_day = 21
      else ! Southern
        if (mo == 6 .AND. dy > 20) then
          reftime_year = yr;
        else
          reftime_year = yr - 1
        endif

        reftime_month = 6
        reftime_day = 20
      endif
    elseif (Year_type == 'water') then
      found = .true.

      if (hemisphere == NORTHERN) then ! Northern
        if (mo > 9) then
          reftime_year = yr
        else
          reftime_year = yr - 1
        endif

        reftime_month = 9
        reftime_day = 30
      else ! Southern
        if (mo > 3) then
          reftime_year = yr
        else
          reftime_year = yr - 1
        endif

        reftime_month = 3
        reftime_day = 31
      endif
    elseif (Year_type == 'spring') then
      found = .true.

      if (hemisphere == NORTHERN) then ! Northern
        if (mo > 3 .OR. (mo == 3 .AND. dy > 20)) then
          reftime_year = yr
        else
          reftime_year = yr - 1
        endif

        reftime_month = 3
        reftime_day = 20
      else ! Southern
        if (mo > 9 .OR. (mo == 9 .AND. dy > 22)) then
          reftime_year = yr
        else
          reftime_year = yr - 1
        endif

        reftime_month = 9
        reftime_day = 22
      endif
    elseif (Year_type == 'calendar') then
      found = .true.
      reftime_year = yr - 1
      reftime_month = 12
      reftime_day = 31
    endif

    if (.not. found) then
      print *, 'ERROR, invalid year type argument to compute Julian Day: ', Year_type
      STOP
    endif

    ! set actual Julian Day
    absolute_julday = compute_julday(yr, mo, dy)

    ! relative_julday = 0

    ! 2018-01-18 PAN: this conditional isn't needed
    ! if (Year_type == 'calendar') then
    !     relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)
    ! else
    !     relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)
    ! endif
    relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)

    res = absolute_julday - relative_julday
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
    implicit none

    class(Time_t), intent(inout) :: this
    integer(i32), intent(in) :: hemisphere
    ! --------------------------------------------------------------------------
    if (hemisphere > 1) then
      write(output_unit, *) 'ERROR: set_hemisphere() - Hemisphere must be either 0 (NORTHERN) or 1 (SOUTHERN)'
      stop
    end if
    this%hemisphere = hemisphere

    this%day_of_year = this%ordinal_date(this%Nowtime, 'calendar', this%hemisphere)
    this%day_of_solar_year = this%ordinal_date(this%Nowtime, 'solar', this%hemisphere)
    this%day_of_water_year = this%ordinal_date(this%Nowtime, 'water', this%hemisphere)

    call this%update_summer_flag()
  end subroutine

  !***********************************************************************
  ! Return the last day of the given month
  !***********************************************************************
  pure module function last_day_of_month(this, mon) result(res)
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


  !***********************************************************************
  ! leap_day - is the year a leap year: (1=yes; 0=no)
  !***********************************************************************
  pure module function leap_day(year) result(res)
    implicit none

    ! Arguments
    logical :: res
    integer(i32), intent(in) :: year

    ! Functions
    INTRINSIC MOD

    !***********************************************************************
    res = .false.

    ! Check if leapyear - Start by identifying all years not divisible by 4
    if (MOD(year, 4) == 0) then
        res = .true.
        if (MOD(year, 100) == 0) then
            if (MOD(year, 400) /= 0) res = .false.
        endif
    endif
  end function

  !***********************************************************************
  ! julian_to_gregorian
  ! computes the Gregorian calendar date given the Julian Day
  !***********************************************************************
  module function julian_to_gregorian(julian_day) result(res)
    implicit none

    ! Arguments
    integer(i32) :: res(6)
      !! Gregorian date ([1]=year, [2]=month, [3]=day)
    integer(i32), intent(in) :: julian_day
    ! integer(i32), intent(out) :: year
    ! integer(i32), intent(out) :: month
    ! integer(i32), intent(out) :: day

    ! Local Variables
    integer(i32) :: m
    integer(i32) :: n

    !***********************************************************************
    ! See: http://aa.usno.navy.mil/faq/docs/JD_Formula.php
    res = 0

    m = julian_day + 68569
    n = 4 * m / 146097
    m = m - (146097 * n + 3) / 4
    res(1) = 4000 * (m + 1) / 1461001

    m = m - 1461 * res(1) / 4 + 31
    res(2) = 80 * m / 2447
    res(3) = m - 2447 * res(2) / 80

    m = res(2) / 11
    res(2) = res(2) + 2 - 12 * m
    res(1) = 100 * (n - 49) + res(1) + m
  end function


  !***********************************************************************
  ! compute_julday
  ! computes the Julian Day given a Gregorian calendar date
  !***********************************************************************
  pure module function compute_julday(year, month, day) result(julian_day)
    implicit none

    ! Arguments
    integer(i32) :: julian_day
    integer(i32), intent(in) :: year
    integer(i32), intent(in) :: month
    integer(i32), intent(in) :: day

    !***********************************************************************
    julian_day = day - 32075 + 1461 * (year + 4800 + (month - 14) / 12) / 4 + &
                     367 * (month - 2 - (month - 14) / 12 * 12) / 12 - &
                     3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
  end function

end submodule
