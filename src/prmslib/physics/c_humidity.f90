module PRMS_HUMIDITY
  use variableKind
  use Control_class, only: Control
  use ModelBase_class, only: ModelBase
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  implicit none

  private
  public :: Humidity

  character(len=*), parameter :: MODDESC = 'Humidity distribution'
  character(len=*), parameter :: MODNAME = 'humidity'
  character(len=*), parameter :: MODVERSION = '2020-07-02 15:41:00Z'

  type, extends(ModelBase) :: Humidity
    ! Parameters

    ! Time-series
    real(r32), pointer :: humidity_hru(:)
      !! Relative humidity for each HRU

    contains
      procedure, public :: init => init_Humidity
      procedure, public :: run => run_Humidity
      procedure, public :: cleanup => cleanup_Humidity
  end type

  interface
    module subroutine init_Humidity(this, ctl_data, model_basin)
      class(Humidity), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine run_Humidity(this, ctl_data, model_basin, model_time)
      class(Humidity), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Humidity(this, ctl_data)
      class(Humidity), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module