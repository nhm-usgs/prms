module Datetime_class
  use variableKind
  use prms_constants, only: NORTHERN, SOUTHERN
  implicit none

  integer(i32), parameter :: DAYPMO(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  private
  public :: Datetime

  type Datetime
    integer(i32) :: year
    integer(i32) :: month
    integer(i32) :: day
    integer(i32) :: hour
    integer(i32) :: minute
    integer(i32) :: second
    integer(i32) :: hemisphere = NORTHERN
      !! Hemisphere (0=Northern, 1=Southern)

    contains

      procedure, public :: is_leap
      procedure, public :: is_summer
      procedure, public :: julian_day
      procedure, public :: julian_day_solar
      procedure, public :: julian_day_water
      procedure, public :: month_end
      procedure, public :: print => print_datetime
      procedure, public :: year_length

      procedure, nopass, private :: compute_julian_day
      ! procedure, public :: month_name
      ! procedure, public :: set_hemisphere
  end type

  interface Datetime
    !! Datetime constructor
    module function constructor_Datetime(date, hemisphere) result(this)
      type(Datetime) :: this
        !! Datetime class
      integer(i32), intent(in) :: date(6)
        !! Array containing year, month, day, hour, minute, second
      integer(i32), optional, intent(in) :: hemisphere
    end function
  end interface

  contains
    !***********************************************************************
    !Datetime constructor
    module function constructor_Datetime(date, hemisphere) result(this)
      implicit none

      type(Datetime) :: this
      integer(i32), intent(in) :: date(6)
      integer(i32), optional, intent(in) :: hemisphere

      this%year = date(1)
      this%month = date(2)
      this%day = date(3)
      this%hour = date(4)
      this%minute = date(5)
      this%second = date(6)
      this%hemisphere = NORTHERN  ! default to northern hemisphere

      if (present(hemisphere)) then
        this%hemisphere = hemisphere
      endif
    end function


    function is_summer(this)
      !! Returns true if date is in the summer.
      implicit none

      logical :: is_summer
      class(Datetime), intent(in) :: this

      ! Local variables
      integer(i32) :: jday
        !! Julian day

      ! ------------------------------------------------------------------------
      is_summer = .true.
      ! Summer is based on equinox:
      !   Julian days 79 to 265 for Northern hemisphere
      !   Julian day 265 to 79 in Southern hemisphere

      jday = this%julian_day()

      if (this%hemisphere == NORTHERN) then
        ! Northern Hemisphere
        if (jday < 79 .or. jday > 265) is_summer = .false. ! Equinox
      else
        ! Southern Hemisphere
        if (jday > 79 .and. jday < 265) is_summer = .false. ! Equinox
      endif
    end function


    function is_leap(this)
      !! Returns true if Datetime year is a leap year.
      implicit none

      ! Arguments
      logical :: is_leap
      class(Datetime), intent(in) :: this

      ! Functions
      INTRINSIC MOD

      !***********************************************************************
      is_leap = .false.

      ! Check if leapyear - Start by identifying if year is divisible by 4
      if (MOD(this%year, 4) == 0) then
        is_leap = .true.

        if (MOD(this%year, 100) == 0) then
          if (MOD(this%year, 400) /= 0) is_leap = .false.
        endif
      endif
    end function


    function julian_day(this)
      !! Return julian day based on the calendar year
      implicit none

      integer(i32) :: julian_day
      class(Datetime), intent(in) :: this

      !***********************************************************************
      julian_day = compute_julian_day(this%year, this%month, this%day)
    end function


    function julian_day_solar(this)
      !! Return julian day based on the solar year
      implicit none

      ! Arguments
      integer(i32) :: julian_day_solar
      class(Datetime), intent(in) :: this

      ! Local Variables
      integer(i32) :: reftime_year
      integer(i32) :: reftime_month
      integer(i32) :: reftime_day
      integer(i32) :: absolute_julday
      integer(i32) :: relative_julday

      !***********************************************************************
      ! set reftime depending on type arg
        if (this%hemisphere == NORTHERN) then
          ! Northern hemisphere
          if (this%month == 12 .and. this%day > 21) then
            reftime_year = this%year
          else
            reftime_year = this%year - 1
          endif

          reftime_month = 12
          reftime_day = 21
        else
          ! Southern hemisphere
          if (this%month == 6 .and. this%day > 20) then
            reftime_year = this%year
          else
            reftime_year = this%year - 1
          endif

          reftime_month = 6
          reftime_day = 20
        endif

      ! set actual Julian Day
      absolute_julday = compute_julian_day(this%year, this%month, this%day)

      relative_julday = compute_julian_day(reftime_year, reftime_month, reftime_day)

      julian_day_solar = absolute_julday - relative_julday
    end function


    function julian_day_water(this)
      !! Return julian day based on the water year
      implicit none

      ! Arguments
      integer(i32) :: julian_day_water
      class(Datetime), intent(in) :: this

      ! Local Variables
      integer(i32) :: reftime_year
      integer(i32) :: reftime_month
      integer(i32) :: reftime_day
      integer(i32) :: absolute_julday
      integer(i32) :: relative_julday

      !***********************************************************************
      if (this%hemisphere == NORTHERN) then
        ! Northern hemisphere
        if (this%month > 9) then
          reftime_year = this%year
        else
          reftime_year = this%year - 1
        endif

        reftime_month = 9
        reftime_day = 30
      else
        ! Southern hemisphere
        if (this%month > 3) then
          reftime_year = this%year
        else
          reftime_year = this%year - 1
        endif

        reftime_month = 3
        reftime_day = 31
      endif

      ! set actual Julian Day
      absolute_julday = compute_julian_day(this%year, this%month, this%day)

      relative_julday = compute_julian_day(reftime_year, reftime_month, reftime_day)

      julian_day_water = absolute_julday - relative_julday
    end function


    function month_end(this)
      !! Returns the last day of the Datetime month.
      implicit none

      integer(i32) :: month_end
      class(Datetime), intent(in) :: this

      month_end = DAYPMO(this%month)

      if (this%month == 2 .and. this%is_leap()) month_end = month_end + 1
    end function


    subroutine print_datetime(this, full_datetime)
      !! Print string representation of the Datetime object.
      implicit none

      class(Datetime), intent(in) :: this
      logical, optional, intent(in) :: full_datetime

      ! ------------------------------------------------------------------------
      if (present(full_datetime) .and. full_datetime) then
        ! Print the full datetime object
        print 9001, this%year, this%month, this%day, this%hour, this%minute
      else
        ! Print the date only
        print 9001, this%year, this%month, this%day
      endif

      9001 FORMAT ('    Date: ', I4, 2('/', I2.2), I3.2, ':', I2.2, /)
    end subroutine


    function year_length(this)
      !! Returns the length of the Datetime year.
      implicit none

      integer(i32) :: year_length
      class(Datetime), intent(in) :: this

      ! ------------------------------------------------------------------------
      if (this%is_leap()) then
        year_length = 366
      else
        year_length = 365
      endif
    end function





    function compute_julian_day(year, month, day)
      !! Convert gregorian calendar date to julian day.
      implicit none

      integer(i32) :: compute_julian_day
        !! Julian day
      integer(i32), intent(in) :: year
        !! Gregorian year
      integer(i32), intent(in) :: month
        !! Gregorian month
      integer(i32), intent(in) :: day
        !! Gregorian day

      !***********************************************************************
      compute_julian_day = day - 32075 + &
                           1461 * (year + 4800 + (month - 14) / 12) / 4 + &
                           367 * (month - 2 - (month - 14) / 12 * 12) / 12 - &
                           3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
    end function
end module
