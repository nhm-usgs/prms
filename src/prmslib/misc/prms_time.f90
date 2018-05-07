!***********************************************************************
! Sets PRMS time variables
!***********************************************************************
module PRMS_SET_TIME
  use variableKind
  use prms_constants, only: NORTHERN, SOUTHERN, YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  implicit none

  private
  public :: Time_t

  character(len=*), parameter :: MODDESC = 'Time_t variables'
  character(len=*), parameter :: MODNAME = 'prms_time'
  character(len=*), parameter :: MODVERSION = '2017-07-06 14:16:00Z'

  integer(i32), parameter :: DAYPMO(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  type Time_t
    ! integer(i32) :: Yrdays  ! only used by last_day_of_month()
    integer(i32) :: Summer_flag
    integer(i32) :: day_of_year
    integer(i32) :: day_of_solar_year
    integer(i32) :: day_of_water_year
    integer(i32) :: Julian_day_absolute
    integer(i32) :: Nowtime(6)
    integer(i32) :: Nowday
    integer(i32) :: Nowmonth
    integer(i32) :: Nowyear
    integer(i32) :: Nowhour
    integer(i32) :: Nowminute
    integer(i32) :: Number_timesteps
    integer(i32) :: Timestep
    real(r32) :: Timestep_hours
    real(r32) :: Timestep_days
    real(r32) :: Timestep_minutes
    real(r64) :: Cfs2inches
    real(r64) :: Cfs_conv
    real(r64) :: Timestep_seconds

    contains
      procedure, public :: last_day_of_month
      procedure, public :: next
        !! Advance to next timestep
      procedure, public :: print_date

      procedure, nopass, public :: compute_julday
      procedure, nopass, public :: julian_to_gregorian
      procedure, nopass, public :: leap_day
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module

      procedure, private :: dattim
      procedure, nopass, private :: deltim
      procedure, private :: ordinal_date
  end type

  interface Time_t
    !! Time_t constructor
    module function constructor_Time(ctl_data, model_basin) result(this)
      type(Time_t) :: this
        !! Time_t class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Model basin
    end function
  end interface

  interface
    pure module function last_day_of_month(this, mon) result(res)
      integer(i32) :: res
      class(Time_t), intent(in) :: this
      integer(i32), intent(in) :: mon
    end function
  end interface

  interface
    module function next(this, ctl_data, model_basin) result(res)
      logical :: res
      class(Time_t), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end function
  end interface

  interface
    module function module_name() result(res)
      character(:), allocatable :: res
    end function
  end interface

  interface
    module function version() result(res)
      character(:), allocatable :: res
    end function
  end interface

  interface
    module subroutine dattim(this, ctl_data, period, date_time)
      class(Time_t), intent(in) :: this
      type(Control), intent(in) :: ctl_data
        !! Control file data
      character(len=*), intent(in) :: period
        !! One of: 'now', 'start', or 'end'
      integer(i32), intent(inout) :: date_time(6)
    end subroutine
  end interface

  interface
    module function deltim() result(res)
      real(r64) :: res
    end function deltim
  end interface

  interface
    module function ordinal_date(this, ctl_data, model_basin, Date_type, Year_type, hemisphere) result(res)
      integer(i32) :: res
      class(Time_t) :: this
      type(Control), intent(in) :: ctl_data
        !! Control file data
      type(Basin), intent(in) :: model_basin
        !! Basin class for the model
      character(len=*), intent(in) :: Date_type
        !! One of: "start", "end", "now"
      character(len=*), intent(in) :: Year_type
        !! One of: "calendar", "solar", "water", "absolute"
      integer(i32), intent(in) :: hemisphere
        !! Hemisphere (0=North; 1=South)
    end function
  end interface

  interface
    module subroutine print_date(this, Flag)
      class(Time_t), intent(in) :: this
      integer(i32), intent(in) :: Flag
    end subroutine
  end interface

  interface
    pure module function leap_day(year) result(res)
      logical :: res
      integer(i32), intent(in) :: year
    end function
  end interface

  interface
    module function julian_to_gregorian(julian_day) result(res)
      integer(i32) :: res(6)
        !! Gregorian date ([1]=year, [2]=month, [3]=day)
      integer(i32), intent(in) :: julian_day
    end function
  end interface

  interface
    module function compute_julday(year, month, day) result(julian_day)
      integer(i32) :: julian_day
      integer(i32), intent(in) :: year
      integer(i32), intent(in) :: month
      integer(i32), intent(in) :: day
    end function
  end interface


end module
