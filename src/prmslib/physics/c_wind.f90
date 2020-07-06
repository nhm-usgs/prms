module PRMS_WIND
  use variableKind
  use Control_class, only: Control
  use ModelBase_class, only: ModelBase
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  implicit none

  private
  public :: Wind

  character(len=*), parameter :: MODDESC = 'Wind distribution'
  character(len=*), parameter :: MODNAME = 'wind'
  character(len=*), parameter :: MODVERSION = '2020-07-06 18:32:00Z'

  type, extends(ModelBase) :: Wind
    ! Parameters

    ! Time-series
    real(r32), pointer :: windspeed_hru(:)
      !! Wind speed for each HRU

    contains
      procedure, public :: init => init_Wind
      procedure, public :: run => run_Wind
      procedure, public :: cleanup => cleanup_Wind
  end type

  interface
    module subroutine init_Wind(this, ctl_data, model_basin)
      class(Wind), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine run_Wind(this, ctl_data, model_basin, model_time)
      class(Wind), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Wind(this, ctl_data)
      class(Wind), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module