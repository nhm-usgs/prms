!***********************************************************************
! Sets PRMS time variables
!***********************************************************************
module PRMS_SET_TIME
  use variableKind
  implicit none

  private
  public :: Time

  character(len=*), parameter :: MODNAME = 'prms_time'
  character(len=*), parameter :: VERSION = 'prms_time.f90 2017-07-06 14:16:00Z'

  integer(i32), parameter :: DAYPMO(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  type Time
    integer(i32) :: Yrdays
    integer(i32) :: Summer_flag
    integer(i32) :: Jday
    integer(i32) :: Jsol
    integer(i32) :: Julwater
    integer(i32) :: Julian_day_absolute
    integer(i32) :: Nowtime(6)
    integer(i32) :: Nowday
    integer(i32) :: Nowmonth
    integer(i32) :: Nowyear
    integer(i32) :: Nowhour
    integer(i32) :: Nowminute
    integer(i32) :: Timestep
    real(r32) :: Timestep_hours
    real(r32) :: Timestep_days
    real(r32) :: Timestep_minutes
    real(r64) :: Cfs2inches
    real(r64) :: Cfs_conv
    real(r64) :: Timestep_seconds

    contains
      procedure, public :: advance
      procedure, private :: dattim
      procedure, nopass, private :: deltim
      procedure, private :: julian_day
      procedure, public :: print_date
      procedure, private :: last_day_of_month
      procedure, nopass, public :: julday_in_year
      procedure, nopass, public :: leap_day
      procedure, nopass, public :: compute_gregorian
      procedure, nopass, public :: compute_julday
  end type

  interface Time
    !! Time constructor
    module function constructor_Time(ctl_data, model_basin) result(this)
      use Control_class, only: Control
      use PRMS_BASIN, only: Basin

      type(Time) :: this
        !! Time class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Basin), intent(in) :: model_basin
        !! Model basin
    end function
  end interface

  contains
    !***********************************************************************
    ! Time constructor
    module function constructor_Time(ctl_data, model_basin) result(this)
      use Control_class, only: Control
      use PRMS_BASIN, only: Basin
      use prms_constants, only: FT2_PER_ACRE, SECS_PER_DAY, SECS_PER_HOUR, &
                                HOUR_PER_DAY, MIN_PER_HOUR
      ! use PRMS_DATA_FILE, only: read_data_line
      ! use variables_arr_mod, only: variables_arr_t
      implicit none

      type(Time) :: this
      class(Control), intent(in) :: ctl_data
      class(Basin), intent(in) :: model_basin

      ! Functions
      INTRINSIC SNGL

      ! Local Variables
      integer(i32) :: startday
      real(r64) :: dt

      ! original declare stuff
      this%Timestep_seconds = SECS_PER_DAY
      this%Cfs_conv = FT2_PER_ACRE / 12.0D0 / this%Timestep_seconds
      this%Cfs2inches = model_basin%basin_area_inv * 12.0D0 * this%Timestep_seconds / FT2_PER_ACRE

      ! original init stuff
      this%Nowtime = ctl_data%start_time%values
      this%Jday = this%julian_day(ctl_data, model_basin, 'start', 'calendar', model_basin%hemisphere)
      this%Jsol = this%julian_day(ctl_data, model_basin, 'start', 'solar', model_basin%hemisphere)
      this%Julwater = this%julian_day(ctl_data, model_basin, 'start', 'water', model_basin%hemisphere)
      startday = compute_julday(this%Nowtime(1), this%Nowtime(2), this%Nowtime(3))
      ! startday = compute_julday(Starttime(1), Starttime(2), Starttime(3))
      this%Julian_day_absolute = startday

      this%Nowyear = this%Nowtime(1)
      this%Nowmonth = this%Nowtime(2)
      this%Nowday = this%Nowtime(3)
      this%Nowhour = this%Nowtime(4)
      this%Nowminute = this%Nowtime(5)

      ! Summer is based on equinox:
      !   Julian days 79 to 265 for Northern hemisphere
      !   Julian day 265 to 79 in Southern hemisphere
      this%Summer_flag = 1 ! 1 = summer, 0 = winter
      if (model_basin%hemisphere == 0) then ! Northern Hemisphere
        if (this%Jday < 79 .OR. this%Jday > 265) this%Summer_flag = 0 ! Equinox
      else ! Southern Hemisphere
        if (this%Jday > 79 .AND. this%Jday < 265) this%Summer_flag = 0 ! Equinox
      endif

      dt = deltim()
      this%Timestep_hours = SNGL(dt)
      this%Timestep_days = this%Timestep_hours / HOUR_PER_DAY
      this%Timestep_minutes = this%Timestep_hours * MIN_PER_HOUR
      this%Timestep_seconds = dt * SECS_PER_HOUR
      this%Cfs_conv = FT2_PER_ACRE / 12.0D0 / this%Timestep_seconds
      this%Cfs2inches = model_basin%basin_area_inv * 12.0D0 * this%Timestep_seconds / FT2_PER_ACRE

      ! Check to see if in a daily or subdaily time step
      if (this%Timestep_hours > HOUR_PER_DAY) then
        print *, 'ERROR, timestep > daily, fix Data File, timestep:', this%Timestep_hours
        STOP
      elseif (this%Timestep_hours < HOUR_PER_DAY) then
        print *, 'ERROR, timestep < daily for daily model, fix Data File', this%Timestep_hours
        STOP
      endif
    end function


    subroutine advance(this, ctl_data, model_basin)
      use Control_class, only: Control
      use PRMS_BASIN, only: Basin
      use prms_constants, only: FT2_PER_ACRE, SECS_PER_DAY, SECS_PER_HOUR, &
                                HOUR_PER_DAY, MIN_PER_HOUR

      class(Time), intent(inout) :: this
      class(Control), intent(in) :: ctl_data
      class(Basin), intent(in) :: model_basin

      real(r64) :: dt

      ! ------------------------------------------------------------------------
      this%Timestep = this%Timestep + 1

      call this%dattim(ctl_data, 'now', this%Nowtime)
      this%Jday = this%julian_day(ctl_data, model_basin, 'now', 'calendar', model_basin%hemisphere)
      this%Jsol = this%julian_day(ctl_data, model_basin, 'now', 'solar', model_basin%hemisphere)
      this%Julwater = this%julian_day(ctl_data, model_basin, 'now', 'water', model_basin%hemisphere)
      this%Julian_day_absolute = this%Julian_day_absolute + 1

      ! TODO: ?why? is this here? It shouldn't be.
      ! call read_data_line(this%Nowtime, var_data)

      this%Nowyear = this%Nowtime(1)
      this%Nowmonth = this%Nowtime(2)
      this%Nowday = this%Nowtime(3)
      this%Nowhour = this%Nowtime(4)
      this%Nowminute = this%Nowtime(5)

      ! Summer is based on equinox:
      !   Julian days 79 to 265 for Northern hemisphere
      !   Julian day 265 to 79 in Southern hemisphere
      this%Summer_flag = 1 ! 1 = summer, 0 = winter
      if (model_basin%hemisphere == 0) then ! Northern Hemisphere
        if (this%Jday < 79 .OR. this%Jday > 265) this%Summer_flag = 0 ! Equinox
      else ! Southern Hemisphere
        if (this%Jday > 79 .AND. this%Jday < 265) this%Summer_flag = 0 ! Equinox
      endif

      dt = deltim()
      this%Timestep_hours = SNGL(dt)
      this%Timestep_days = this%Timestep_hours / HOUR_PER_DAY
      this%Timestep_minutes = this%Timestep_hours * MIN_PER_HOUR
      this%Timestep_seconds = dt * SECS_PER_HOUR
      this%Cfs_conv = FT2_PER_ACRE / 12.0D0 / this%Timestep_seconds
      this%Cfs2inches = model_basin%basin_area_inv * 12.0D0 * this%Timestep_seconds / FT2_PER_ACRE

      ! Check to see if in a daily or subdaily time step
      if (this%Timestep_hours > HOUR_PER_DAY) then
        print *, 'ERROR, timestep > daily, fix Data File, timestep:', this%Timestep_hours
        STOP
      elseif (this%Timestep_hours < HOUR_PER_DAY) then
        print *, 'ERROR, timestep < daily for daily model, fix Data File', this%Timestep_hours
        STOP
      endif
    end subroutine


    !***********************************************************************
    ! dattim - get start, end, or current date and time
    ! 2017-11-07 PAN: moved here from mmf_utils.f90
    !***********************************************************************
    subroutine dattim(this, ctl_data, String, Datetime)
      use Control_class, only: Control
      ! use time_mod, only: compute_gregorian
      implicit none

      ! Arguments
      class(Time) :: this
      class(Control), intent(in) :: ctl_data
        !! Control file data
      character(len=*), intent(in) :: String
        !! One of: 'now', 'start', or 'end'
      integer(i32), intent(out) :: Datetime(6)

      !***********************************************************************
      Datetime = 0

      if (String == 'end') then
        Datetime = ctl_data%end_time%values
      elseif (String == 'now') then
        call compute_gregorian(this%Julian_day_absolute, this%Nowyear, this%Nowmonth, this%Nowday)

        Datetime(1) = this%Nowyear
        Datetime(2) = this%Nowmonth
        Datetime(3) = this%Nowday
        ! Datetime = LIS function
      elseif (String == 'start') then
        Datetime = ctl_data%start_time%values
      else
        STOP 'ERROR, invalid call to dattim'
      endif
    end subroutine dattim


    !***********************************************************************
    ! timestep_hours - time step increment in hours
    ! 2017-11-07 PAN: moved here from mmf_utils.f90
    !***********************************************************************
    real(r64) function deltim()
      implicit none

      !***********************************************************************
      !deltim = lisfunction() ! need to make routine to get time step increment
      deltim = 24.0D0
    end function deltim


    !***********************************************************************
    ! julian_day
    ! computes the Julian date given a Gregorian calendar date
    ! (Year, Month, Day) relative to: calendar (Jan 1),
    ! solar (12/22 in Northern; 6/21 in Southern) and
    ! water year (10/1 in Northern; 4/1 in Southern) start dates.
    ! The Julian day starts at noon of the Gregorian day and
    ! extends to noon the next Gregorian day.
    !
    ! 2017-10-30 PAN: moved here from utils_prms.f90
    !***********************************************************************
    integer function julian_day(this, ctl_data, model_basin, Date_type, Year_type, hemisphere)
      use Control_class, only: Control
      use PRMS_BASIN, only: Basin
      implicit none

      ! Arguments
      class(Time) :: this
      class(Control), intent(in) :: ctl_data
        !! Control file data
      class(Basin), intent(in) :: model_basin
        !! Basin class for the model
      character(len=*), intent(in) :: Date_type
        !! One of: "start", "end", "now"
      character(len=*), intent(in) :: Year_type
        !! One of: "calendar", "solar", "water", "absolute"
      integer(i32), intent(in) :: hemisphere
        !! Hemisphere (0=North; 1=South)

      ! Local Variables
      integer(i32) :: reftime_year
      integer(i32) :: reftime_month
      integer(i32) :: reftime_day
      integer(i32) :: time_array(6)
      integer(i32) :: year
      integer(i32) :: month
      integer(i32) :: day
      integer(i32) :: absolute_julday
      integer(i32) :: relative_julday
      logical :: found

      !***********************************************************************
      if (Date_type == 'end') then
        time_array = ctl_data%end_time%values
      elseif (Date_type == 'now') then
        time_array = this%Nowtime
      elseif (Date_type == 'start') then
        time_array = ctl_data%start_time%values
      else
        print *, 'ERROR, invalid argument to compute Julian Day: ', Date_type
        STOP
      endif

      year = time_array(1)
      month = time_array(2)
      day = time_array(3)

      found = .false.

      ! set reftime depending on type arg
      if (Year_type == 'solar') then
        found = .true.

        if (model_basin%hemisphere == 0) then ! Northern
          if (month == 12 .AND. day > 21) then
            reftime_year = year
          else
            reftime_year = year - 1
          endif

          reftime_month = 12
          reftime_day = 21
        else ! Southern
          if (month == 6 .AND. day > 20) then
            reftime_year = year;
          else
            reftime_year = year - 1
          endif

          reftime_month = 6
          reftime_day = 20
        endif
      elseif (Year_type == 'water') then
        found = .true.

        if (model_basin%hemisphere == 0) then ! Northern
          if (month > 9) then
            reftime_year = year
          else
            reftime_year = year - 1
          endif

          reftime_month = 9
          reftime_day = 30
        else ! Southern
          if (month > 3) then
            reftime_year = year
          else
            reftime_year = year - 1
          endif

          reftime_month = 3
          reftime_day = 31
        endif
      elseif (Year_type == 'spring') then
        found = .true.

        if (model_basin%hemisphere == 0) then ! Northern
          if (month > 3 .OR. (month == 3 .AND. day > 20)) then
            reftime_year = year
          else
            reftime_year = year - 1
          endif

          reftime_month = 3
          reftime_day = 20
        else ! Southern
          if (month > 9 .OR. (month == 9 .AND. day > 22)) then
            reftime_year = year
          else
            reftime_year = year - 1
          endif

          reftime_month = 9
          reftime_day = 22
        endif
      elseif (Year_type == 'calendar') then
        found = .true.
        reftime_year = year - 1
        reftime_month = 12
        reftime_day = 31
      endif

      if (.not. found) then
        print *, 'ERROR, invalid year type argument to compute Julian Day: ', Year_type
        STOP
      endif

      ! set actual Julian Day
      absolute_julday = compute_julday(year, month, day)

      ! relative_julday = 0

      ! 2018-01-18 PAN: this conditional isn't needed
      ! if (Year_type == 'calendar') then
      !     relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)
      ! else
      !     relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)
      ! endif
      relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)

      julian_day = absolute_julday - relative_julday
    end function julian_day


    !***********************************************************************
    ! Print date
    ! 2017-10-30 PAN: moved here from utils_prms.f90
    !***********************************************************************
    subroutine print_date(this, Flag)
      implicit none

      ! Arguments
      class(Time), intent(in) :: this
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
    end subroutine print_date

    !***********************************************************************
    ! Return the last day of the given month
    !***********************************************************************
    integer function last_day_of_month(this, mon)
      ! use time_mod, only: DAYPMO, leap_day
      implicit none

      class(Time), intent(inout) :: this
      integer(i32), intent(in) :: mon

      ! ===============================
      this%Yrdays = 365
      last_day_of_month = DAYPMO(mon)

      if (leap_day(this%Nowyear)) then
        this%Yrdays = this%Yrdays + 1

        if (mon == 2) last_day_of_month = last_day_of_month + 1
      endif
    end function last_day_of_month




    ! **************************************************************************
    ! **************************************************************************
    ! time helper functions pulled from time.f90
    ! **************************************************************************

    !***********************************************************************
    ! julday_in_year
    ! computes the Julian Day of a date
    !***********************************************************************
    integer function julday_in_year(Year, Month, Day)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: Year
      integer(i32), intent(in) :: Month
      integer(i32), intent(in) :: Day

      ! Local Variables
      integer(i32) :: i

      !***********************************************************************
      julday_in_year = Day

      do i = 1, Month - 1
          julday_in_year = julday_in_year + daypmo(i)
      enddo

      ! Add additional day if the day is in a leap year after February
      if (leap_day(Year) .and. Month > 2) julday_in_year = julday_in_year + 1
    end function julday_in_year

    !***********************************************************************
    ! leap_day - is the year a leap year: (1=yes; 0=no)
    !***********************************************************************
    function leap_day(Year)
      implicit none

      ! Arguments
      logical :: leap_day
      integer(i32), intent(in) :: Year

      ! Functions
      INTRINSIC MOD

      !***********************************************************************
      leap_day = .false.

      ! Check if leapyear - Start by identifying all years not divisible by 4
      if (MOD(Year, 4) == 0) then
          leap_day = .true.
          if (MOD(Year, 100) == 0) then
              if (MOD(Year, 400) /= 0) leap_day = .false.
          endif
      endif
    end function leap_day

    !***********************************************************************
    ! compute_gregorian
    ! computes the Gregorian calendar date given the Julian Day
    !***********************************************************************
    subroutine compute_gregorian(Julday, Year, Month, Day)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: Julday
      integer(i32), intent(out) :: Year
      integer(i32), intent(out) :: Month
      integer(i32), intent(out) :: Day

      ! Local Variables
      integer(i32) :: m
      integer(i32) :: n

      !***********************************************************************
      m = Julday + 68569
      n = 4 * m / 146097
      m = m - (146097 * n + 3) / 4
      Year = 4000 * (m + 1) / 1461001
      m = m - 1461 * Year / 4 + 31
      Month = 80 * m / 2447
      Day = m - 2447 * Month / 80
      m = Month / 11
      Month = Month + 2 - 12 * m
      Year = 100 * (n - 49) + Year + m
    end subroutine compute_gregorian

    !***********************************************************************
    ! compute_julday
    ! computes the Julian Day given a Gregorian calendar date
    !***********************************************************************
    integer function compute_julday(Year, Month, Day)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: Year
      integer(i32), intent(in) :: Month
      integer(i32), intent(in) :: Day

      !***********************************************************************
      compute_julday = Day - 32075 + 1461 * (Year + 4800 + (Month - 14) / 12) / 4 &
                       + 367 * (Month - 2 - (Month - 14) / 12 * 12) &
                       / 12 - 3 * ((Year + 4900 + (Month - 14) / 12) / 100) / 4
    end function compute_julday
end module
