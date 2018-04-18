!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
MODULE PRMS_TRANSP_TINDEX
  use variableKind
  use prms_constants, only: YEAR, MONTH, DAY
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_SET_TIME, only: Time_t
  implicit none

  private
  public :: transp_tindex

  character(len=*), PARAMETER :: MODDESC = 'Transpiration Distribution'
  character(len=*), PARAMETER :: MODNAME = 'transp_tindex'
  character(len=*), PARAMETER :: MODVERSION = '2015-01-06 00:09:15Z'

  type Transp_tindex
    ! Local Variables
    integer(i32), allocatable :: transp_check(:)
    integer(i32), allocatable :: transp_beg_restart(:)
    integer(i32), allocatable :: transp_end_restart(:)
    real(r32), allocatable :: tmax_sum(:)
    real(r32), allocatable :: transp_tmax_f(:)
    real(r32), allocatable :: transp_tmax_restart(:)

    contains
      procedure, public :: cleanup => cleanup_Transp_tindex
      procedure, public :: run => run_Transp_tindex
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module

  end type

  interface Transp_tindex
    !! Transp_tindex constructor
    module function constructor_Transp_tindex(ctl_data, param_data, model_basin, climate) result(this)
      type(Transp_tindex) :: this
        !! Transp_tindex class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Model basin information
      type(Climateflow), intent(inout) :: climate
        !! Climate flow class
    end function
  end interface

  interface
    module subroutine cleanup_Transp_tindex(this, ctl_data)
      class(Transp_tindex), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    module subroutine run_Transp_tindex(this, ctl_data, param_data, model_time, model_basin, climate)
      class(Transp_tindex), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(inout) :: climate
    end subroutine
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
end MODULE
