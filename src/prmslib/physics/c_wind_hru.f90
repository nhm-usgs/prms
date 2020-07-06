module PRMS_WIND_HRU
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_WIND, only: Wind
  implicit none

  private
  public :: Wind_hru

  character(len=*), parameter :: MODDESC = 'Wind distribution by HRU'
  character(len=*), parameter :: MODNAME = 'wind_hru'
  character(len=*), parameter :: MODVERSION = '2020-07-06 18:33:00Z'

  type, extends(Wind) :: Wind_hru
    ! Parameters

    ! Time-series

    ! Local variables
    integer(i32), private :: windspeed_funit
      !! File unit for the windspeed CBH file

    integer(i32), private :: varid
    integer(i32), private :: idx_offset

    logical, private :: has_netcdf_windspeed

    contains
      procedure, public :: init => init_Wind_hru
      procedure, public :: run => run_Wind_hru
      procedure, public :: cleanup => cleanup_Wind_hru
  end type

  interface
    module subroutine init_Wind_hru(this, ctl_data, model_basin)
      class(Wind_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine run_Wind_hru(this, ctl_data, model_basin, model_time)
      class(Wind_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Wind_hru(this, ctl_data)
      class(Wind_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module