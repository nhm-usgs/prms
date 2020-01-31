module PRMS_TEMPERATURE
  use variableKind
  use ModelBase_class, only: ModelBase
  use prms_constants, only: dp
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Temperature

  character(len=*), parameter :: MODDESC = 'Temperature distribution'
  character(len=*), parameter :: MODNAME = 'temperature'
  character(len=*), parameter :: MODVERSION = '2018-10-10 15:45:00Z'

  type, extends(ModelBase) :: Temperature
    ! Parameters
    integer(i32) :: temp_units = 0

    logical :: has_hru_summary_vars

    ! NOTE: 2018-07-24 PAN: Changed tavg, tmax, tmin to r64
    !       The additional precision is needed when fahrenheit temperatures are
    !       converted to Celsius.
    real(r32), pointer :: tavg(:)
    real(r32), pointer :: tmax(:)
    real(r32), pointer :: tmin(:)

    ! NOTE: Only used by potet_jh; remove once temperature units are standardized
    real(r32), pointer :: tavg_f(:)
    ! NOTE: Only used by solar_radiation_degday; remove once temperature units
    !       are standardized.
    real(r32), pointer :: tmax_f(:)

    contains
      procedure, public :: init => init_Temperature
      procedure, public :: run => run_Temperature
      procedure, public :: set_nhru_summary_ptrs
  end type

  interface
    !! Temperature constructor
    module subroutine init_Temperature(this, ctl_data, model_basin, model_summary)
      class(Temperature), intent(inout) :: this
        !! Temperature class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Temperature(this, ctl_data, model_basin, model_time, model_summary)
      class(Temperature), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine set_nhru_summary_ptrs(this, ctl_data, model_summary)
      class(Temperature), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface
end module
