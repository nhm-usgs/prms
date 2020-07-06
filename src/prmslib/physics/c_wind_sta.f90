module PRMS_WIND_STA
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_WIND, only: Wind
  implicit none

  private
  public :: Wind_sta

  character(len=*), parameter :: MODDESC = 'Wind distribution by station'
  character(len=*), parameter :: MODNAME = 'wind_sta'
  character(len=*), parameter :: MODVERSION = '2020-07-06 18:36:00Z'

  type, extends(Wind) :: Wind_sta
    ! Dimensions
    integer(i32) :: nwind = 0

    ! Parameters
    integer(i32), pointer :: hru_windspeed_sta(:, :)
      !! Index of wind speed measurement station for each HRU

    ! Time-series
    real(r32), pointer :: windspeed_obs(:)
      !! Wind station observations

    ! Local variables
    integer(i32), private :: windspeed_funit
      !! File unit for the wind CBH file

    integer(i32), private :: varid
    integer(i32), private :: idx_offset

    contains
      procedure, public :: init => init_Wind_sta
      procedure, public :: run => run_Wind_sta
      procedure, public :: cleanup => cleanup_Wind_sta
  end type

  interface
    module subroutine init_Wind_sta(this, ctl_data, model_basin)
      class(Wind_sta), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine run_Wind_sta(this, ctl_data, model_basin, model_time)
      class(Wind_sta), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Wind_sta(this, ctl_data)
      class(Wind_sta), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module