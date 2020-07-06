module PRMS_HUMIDITY_HRU
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_HUMIDITY, only: Humidity
  implicit none

  private
  public :: Humidity_hru

  character(len=*), parameter :: MODDESC = 'Humidity distribution by HRU'
  character(len=*), parameter :: MODNAME = 'humidity_hru'
  character(len=*), parameter :: MODVERSION = '2020-07-02 16:58:00Z'

  type, extends(Humidity) :: Humidity_hru
    ! Parameters

    ! Time-series

    ! Local variables
    integer(i32), private :: humidity_funit
      !! File unit for the humidity CBH file

    integer(i32), private :: varid
    integer(i32), private :: idx_offset

    logical, private :: has_netcdf_humidity

    contains
      procedure, public :: init => init_Humidity_hru
      procedure, public :: run => run_Humidity_hru
      procedure, public :: cleanup => cleanup_Humidity_hru
  end type

  interface
    module subroutine init_Humidity_hru(this, ctl_data, model_basin)
      class(Humidity_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine run_Humidity_hru(this, ctl_data, model_basin, model_time)
      class(Humidity_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Humidity_hru(this, ctl_data)
      class(Humidity_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module