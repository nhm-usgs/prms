module Datetime_class
  use variableKind
  use prms_constants, only: NORTHERN, SOUTHERN, YEAR, MONTH, DAY
  implicit none

  character(len=*), parameter :: MODDESC = 'Datetime_t'
  character(len=*), parameter :: MODNAME = 'Datetime'
  character(len=*), parameter :: MODVERSION = '2018-04-19 00:00:00Z'

  integer(i32), parameter :: DAYPMO(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  private
  public :: Datetime_t

  type Datetime_t
    integer(i32) :: datetime(6)
    integer(i32), private :: year
    integer(i32), private :: month
    integer(i32), private :: day
    integer(i32) :: hour
    integer(i32) :: minute
    integer(i32) :: second
    integer(i32) :: hemisphere = NORTHERN
      !! Hemisphere (0=Northern, 1=Southern)

    contains

      procedure, public :: is_leap
      procedure, public :: is_summer
      procedure, public :: julian_day
      procedure, public :: month_end
      procedure, public :: ordinal_date
      procedure, public :: print => print_datetime
      procedure, public :: year_length

      ! Get functions for date components
      procedure, public :: year
      procedure, public :: month
      procedure, public :: day

      procedure, nopass, private :: compute_julian_day
      ! procedure, public :: month_name
      ! procedure, public :: set_hemisphere
  end type

  interface Datetime_t
    !! Datetime constructor
    module function constructor_Datetime(date, hemisphere) result(this)
      type(Datetime_t) :: this
        !! Datetime_t class
      integer(i32), intent(in) :: date(6)
        !! Array containing year, month, day, hour, minute, second
      integer(i32), optional, intent(in) :: hemisphere
    end function
  end interface

  contains
    !***********************************************************************
    !Datetime_t constructor
    module function constructor_Datetime_t(date, hemisphere) result(this)
      implicit none

      type(Datetime_t) :: this
      integer(i32), intent(in) :: date(6)
      integer(i32), optional, intent(in) :: hemisphere

      this%datetime = date
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


    function is_summer(this) result(res)
      !! Returns true if date is in the summer.
      implicit none

      logical :: res
      class(Datetime_t), intent(in) :: this

      ! Local variables
      integer(i32) :: jday
        !! Julian day

      ! ------------------------------------------------------------------------
      res = .true.
      ! Summer is based on equinox:
      !   Julian days 79 to 265 for Northern hemisphere
      !   Julian day 265 to 79 in Southern hemisphere

      jday = this%julian_day()

      if (this%hemisphere == NORTHERN) then
        ! Northern Hemisphere
        if (jday < 79 .or. jday > 265) res = .false. ! Equinox
      else
        ! Southern Hemisphere
        if (jday > 79 .and. jday < 265) res = .false. ! Equinox
      endif
    end function


    function is_leap(this) result(res)
      !! Returns true if Datetime year is a leap year.
      implicit none

      ! Arguments
      logical :: res
      class(Datetime_t), intent(in) :: this

      ! Functions
      INTRINSIC MOD

      !***********************************************************************
      res = .false.

      ! Check if leapyear - Start by identifying if year is divisible by 4
      if (MOD(this%year, 4) == 0) then
        res = .true.

        if (MOD(this%year, 100) == 0) then
          if (MOD(this%year, 400) /= 0) res = .false.
        endif
      endif
    end function


    pure function julian_day(this) result(res)
      !! Return julian day based on the calendar year
      implicit none

      integer(i32) :: res
      class(Datetime_t), intent(in) :: this

      !***********************************************************************
      res = compute_julian_day(this%datetime)
    end function


    !***********************************************************************
    ! ordinal_date
    ! computes the ordinal day of the year (1..366) for a Gregorian date
    ! (Year, Month, Day) relative to:
    !   calendar (Jan 1),
    !   solar (12/22 in Northern; 6/21 in Southern) and
    !   water year (10/1 in Northern; 4/1 in Southern) start dates.
    ! The day of the year starts at noon of the Gregorian day and
    ! extends to noon the next Gregorian day.
    pure function ordinal_date(this, calendar) result(res)
      implicit none

      ! Arguments
      integer(i32) :: res
      class(Datetime_t), intent(in) :: this
      character(len=*), optional, intent(in) :: calendar
        !! Calendar type; one of: 'calender', 'solar', 'spring', 'water'

      ! Local Variables
      integer(i32) :: ref_date(3)
      integer(i32) :: absolute_julday
      integer(i32) :: relative_julday
      character(len=*) :: calendar_
        !! Used so calendar always has at least a default value

      ! ------------------------------------------------------------------------
      ! NOTE: Kludge to maintain a default value for optional calendar argument
      calendar_ = 'calendar'
      if (present(calendar)) calendar_ = calendar

      select case (calendar_)
        case('solar')
          ! Solar
          if (this%hemisphere == NORTHERN) then
            ref_date = [this%year-1, 12, 21]

            if (this%month == 12 .and. this%day > 21) then
              ref_date(YEAR) = this%year
            endif
          else
            ! Southern hemisphere
            ref_date = [this%year-1, 6, 20]

            if (this%month == 6 .and. this%day > 20) then
              ref_date(YEAR) = this%year
            endif
          endif
        case('water')
          ! Water
          if (this%hemisphere == NORTHERN) then
            ref_date = [this%year-1, 9, 30]

            if (this%month > 9) then
              ref_date(YEAR) = this%year
            endif
          else
            ! Southern hemisphere
            ref_date = [this%year-1, 3, 31]

            if (this%month > 3) then
              ref_date(YEAR) = this%year
            endif
          endif
        case('spring')
          ! Spring
          if (this%hemisphere == NORTHERN) then
            ref_date = [this%year-1, 3, 20]

            if (month > 3 .OR. (month == 3 .AND. day > 20)) then
              ref_date(YEAR) = this%year
            endif
          else
            ! Southern
            ref_date = [this%year-1, 9, 22]

            if (month > 9 .OR. (month == 9 .AND. day > 22)) then
              ref_date(YEAR) = this%year
            endif
          endif
        case('calendar')
          ! Calendar
          ref_date = [this%year-1, 12, 31]
        case default
          write(output_unit, *) 'Invalid calendar selected; use one of: calendar, solar, spring, or water.'
          return
      end select

      ! set actual Julian Day
      absolute_julday = compute_julian_day(this%datetime)
      relative_julday = compute_julian_day(ref_date)

      res = absolute_julday - relative_julday
    end function


    pure function month_end(this) result(res)
      !! Returns the last day of the Datetime month.
      implicit none

      integer(i32) :: res
      class(Datetime_t), intent(in) :: this

      ! ------------------------------------------------------------------------
      res = DAYPMO(this%month)

      if (this%month == 2 .and. this%is_leap()) res = res + 1
    end function


    subroutine print_datetime(this, full_datetime)
      !! Print string representation of the Datetime object.
      implicit none

      class(Datetime_t), intent(in) :: this
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


    pure function year_length(this) result(res)
      !! Returns the length of the Datetime_t year.
      implicit none

      integer(i32) :: res
      class(Datetime_t), intent(in) :: this

      ! ------------------------------------------------------------------------
      if (this%is_leap()) then
        res = 366
      else
        res = 365
      endif
    end function

    pure function year(this) result(res)
      implicit none

      integer(i32) :: res
      class(Datetime_t), intent(in) this

      res = this%year
    end function

    pure function month(this) result(res)
      implicit none

      integer(i32) :: res
      class(Datetime_t), intent(in) this

      res = this%month
    end function

    pure function day(this) result(res)
      implicit none

      integer(i32) :: res
      class(Datetime_t), intent(in) this

      res = this%day
    end function

    
    !***********************************************************************
    ! computes the Julian Day given a Gregorian calendar date
    !***********************************************************************
    pure function compute_julian_day(date) result(res)
      !! Convert gregorian calendar date to julian day.
      implicit none

      integer(i32) :: res
        !! Julian day
      integer(i32), intent(in) :: date(3)
        !! Gregorian date; array of year, month, day.

      !***********************************************************************
      res = date(DAY) - 32075 + &
            1461 * (date(YEAR) + 4800 + (date(MONTH) - 14) / 12) / 4 + &
            367 * (date(MONTH) - 2 - (date(MONTH) - 14) / 12 * 12) / 12 - &
            3 * ((date(YEAR) + 4900 + (date(MONTH) - 14) / 12) / 100) / 4
    end function
end module
