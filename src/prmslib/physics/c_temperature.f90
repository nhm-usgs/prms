module PRMS_TEMPERATURE
  use variableKind
  use prms_constants, only: dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  implicit none

  private
  public :: Temperature

  character(len=*), parameter :: MODDESC = 'Temperature distribution'
  character(len=*), parameter :: MODNAME = 'temp'
  character(len=*), parameter :: MODVERSION = '2018-07-05 19:55:00Z'

  type Temperature
    real(r64) :: basin_temp
    real(r64) :: basin_tmax
    real(r64) :: basin_tmin

    real(r32), allocatable :: tavg(:)
    real(r32), allocatable :: tmax(:)
    real(r32), allocatable :: tmin(:)

    contains
      procedure, public :: run => run_Temperature
  end type

  interface Temperature
    !! Temperature constructor
    module function constructor_Temperature(ctl_data) result(this)
      type(Temperature) :: this
        !! Temperature class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine run_Temperature(this, ctl_data, param_data, model_basin, model_time)
      class(Temperature), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
    end subroutine
  end interface
end module
