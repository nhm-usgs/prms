!***********************************************************************
! Sets PRMS time variables
!***********************************************************************
module PRMS_SET_TIME
  use variableKind
  use prms_constants, only: NORTHERN, SOUTHERN, YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, dp
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  implicit none

  private
  public :: Time_t

  character(len=*), parameter :: MODDESC = 'Time_t variables'
  character(len=*), parameter :: MODNAME = 'prms_time'
  character(len=*), parameter :: MODVERSION = '2018-08-30 13:36:00Z'

  real(r64), parameter :: TIMESTEP_DELTA = 24.0_dp
    !! Timestep delta in hours
  integer(i32), parameter :: DAYPMO(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    !! Days per month for non-leap years

  type, extends(ModelBase) :: Time_t
    ! integer(i32) :: Yrdays  ! only used by last_day_of_month()
    integer(i32) :: Summer_flag
    integer(i32) :: days_in_model
      !! Total number of days in model given start and end dates
    integer(i32) :: day_of_year ! was Jday
    integer(i32) :: day_of_solar_year
    integer(i32) :: day_of_water_year
    integer(i32) :: days_since_start
      !! Number of days since the start date of the model simulation

    integer(i32), private :: hemisphere
    integer(i32) :: months
    integer(i32) :: months_in_model
      !! Total number of months in the model
    integer(i32) :: years_in_model
      !! Years in model rounded to nearest full year

    integer(i32) :: Nowtime(6)
    integer(i32) :: Nowday
    integer(i32) :: Nowmonth
    integer(i32) :: Nowyear
    integer(i32) :: Nowhour
    integer(i32) :: Nowminute
    integer(i32) :: Number_timesteps
    integer(i32) :: Timestep

    integer(i32) :: current_jdn
      !! Julian day number of current datetime
    integer(i32) :: start_jdn
      !! Julian day number of model start date
    integer(i32) :: end_jdn
      !! Julian day number of model end date
    character(len=10) :: start_string
      !! String representation of model start date

    real(r32) :: Timestep_hours
    real(r32) :: Timestep_days
    real(r32) :: Timestep_minutes

    ! real(r64) :: Cfs2inches
    real(r64) :: Cfs_conv
    real(r64) :: Timestep_seconds

    contains
      procedure, public :: cleanup => cleanup_Time
      procedure, public :: last_day_of_month
      procedure, public :: next
        !! Advance to next timestep
      procedure, public :: print_date
      procedure, public :: set_hemisphere

      ! procedure, nopass, public :: gregorian_to_julian
      ! procedure, nopass, public :: julian_to_gregorian
      ! procedure, nopass, public :: leap_day

      ! procedure, nopass, private :: ordinal_date
      procedure, private :: update_summer_flag
  end type

  interface Time_t
    !! Time_t constructor
    module function constructor_Time(ctl_data, hemisphere) result(this)
      type(Time_t) :: this
        !! Time_t class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      integer(i32), optional, intent(in) :: hemisphere
        !! Which hemisphere is the model in (0=NORTHERN, 1=SOUTHERN)
    end function
  end interface

  interface
    module subroutine cleanup_Time(this, ctl_data)
      class(Time_t), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    pure module function last_day_of_month(this, mon) result(res)
      integer(i32) :: res
      class(Time_t), intent(in) :: this
      integer(i32), intent(in) :: mon
    end function
  end interface

  interface
    module function next(this) result(res)
      logical :: res
      class(Time_t), intent(inout) :: this
      ! type(Control), intent(in) :: ctl_data
    end function
  end interface

  ! interface
  !   module function ordinal_date(datetime, Year_type, hemisphere) result(res)
  !     integer(i32) :: res
  !     ! class(Time_t) :: this
  !     ! type(Control), intent(in) :: ctl_data
  !       !! Control file data
  !     integer(i32), intent(in) :: datetime(6)
  !     ! character(len=*), intent(in) :: Date_type
  !       !! One of: "start", "end", "now"
  !     character(len=*), intent(in) :: Year_type
  !       !! One of: "calendar", "solar", "water", "absolute"
  !     integer(i32), intent(in) :: hemisphere
  !       !! Hemisphere (0=North; 1=South)
  !   end function
  ! end interface

  interface
    module subroutine print_date(this, Flag)
      class(Time_t), intent(in) :: this
      integer(i32), intent(in) :: Flag
    end subroutine
  end interface

  interface
    module subroutine set_hemisphere(this, hemisphere)
      class(Time_t), intent(inout) :: this
      integer(i32), intent(in) :: hemisphere
        !! 0=NORTHERN, 1=SOUTHERN
    end subroutine
  end interface

  interface
    module subroutine update_summer_flag(this)
      class(Time_t), intent(inout) :: this
    end subroutine
  end interface

  ! interface
  !   pure module function leap_day(year) result(res)
  !     logical :: res
  !     integer(i32), intent(in) :: year
  !   end function
  ! end interface

  ! interface
  !   module function julian_to_gregorian(julian_day) result(res)
  !     integer(i32) :: res(6)
  !       !! Gregorian date ([1]=year, [2]=month, [3]=day)
  !     integer(i32), intent(in) :: julian_day
  !   end function
  ! end interface

  ! interface
  !   pure module function gregorian_to_julian(year, month, day) result(julian_day)
  !     integer(i32) :: julian_day
  !     integer(i32), intent(in) :: year
  !     integer(i32), intent(in) :: month
  !     integer(i32), intent(in) :: day
  !   end function
  ! end interface


end module
