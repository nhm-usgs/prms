module PRMS_HUMIDITY_PER
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_HUMIDITY, only: Humidity
  implicit none

  private
  public :: Humidity_per

  character(len=*), parameter :: MODDESC = 'Humidity distribution by default percentage'
  character(len=*), parameter :: MODNAME = 'humidity_per'
  character(len=*), parameter :: MODVERSION = '2020-07-06 14:57:00Z'

  type, extends(Humidity) :: Humidity_per
    ! Parameters
    integer(i32), pointer :: humidity_percent(:, :)
      !! Monthy humidity for each HRU

    contains
      procedure, public :: init => init_Humidity_per
      procedure, public :: run => run_Humidity_per
      procedure, public :: cleanup => cleanup_Humidity_per
  end type

  interface
    module subroutine init_Humidity_per(this, ctl_data, model_basin)
      class(Humidity_per), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine run_Humidity_per(this, ctl_data, model_basin, model_time)
      class(Humidity_per), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Humidity_per(this, ctl_data)
      class(Humidity_per), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module