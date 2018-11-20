!***********************************************************************
! Computes volume of intercepted precipitation, evaporation from
! intercepted precipitation, and throughfall that reaches the soil or
! snowpack
!***********************************************************************
module PRMS_INTCP
  use variableKind
  use ModelBase_class, only: ModelBase
  use prms_constants, only: dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_POTET, only: Potential_ET
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_TRANSPIRATION, only: Transpiration
  use PRMS_BASIN_SUMMARY_PTR, only: basin_summary_ptr
  use PRMS_NHRU_SUMMARY_PTR, only: Nhru_summary_ptr
  ! use PRMS_SNOW, only: Snowcomp
  implicit none

  private
  public :: Interception

  character(len=*), parameter :: MODDESC = 'Canopy Interception'
  character(len=*), parameter :: MODNAME = 'intcp'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:48:00Z'

  type, extends(ModelBase) :: Interception
    ! Local Variables
    real(r32), allocatable :: gain_inches(:)
    real(r32), allocatable :: intcp_changeover(:)
    real(r32), allocatable :: intcp_stor_ante(:)

    real(r64) :: last_intcp_stor
      !! Set by intcp, used by water_balance

    ! integer(i32) :: use_transfer_intcp
    logical :: use_transfer_intcp

    ! Declared Variables
    real(r64), pointer :: basin_changeover
    real(r64), pointer :: basin_hru_apply
    real(r64), pointer :: basin_intcp_evap
    real(r64), pointer :: basin_intcp_stor
    real(r64), pointer :: basin_net_apply
    real(r64), pointer :: basin_net_ppt
    real(r64), pointer :: basin_net_rain
    real(r64), pointer :: basin_net_snow

    real(r32), allocatable :: canopy_covden(:)
    real(r64), allocatable :: hru_intcpevap(:)
    real(r64), allocatable :: hru_intcpstor(:)
    real(r32), allocatable :: intcp_evap(:)
    real(r32), allocatable :: intcp_stor(:)
    real(r32), allocatable :: net_apply(:)
    real(r64), allocatable :: net_ppt(:)
    real(r64), allocatable :: net_rain(:)
    real(r64), allocatable :: net_snow(:)

    integer(i32), allocatable, private :: intcp_form(:)

    logical, allocatable, private :: intcp_on(:)
    logical, allocatable, private :: intcp_transp_on(:)

    ! integer(i32), allocatable, private :: intcp_on(:)
    ! integer(i32), allocatable, private :: intcp_transp_on(:)

    contains
      procedure, nopass, private :: intercept
      procedure, public :: run => run_Interception
      procedure, public :: cleanup => cleanup_Interception
  end type

  interface Interception
    !! Intercept constructor
    module function constructor_Interception(ctl_data, model_transp, basin_summary, nhru_summary) result(this)
      type(Interception) :: this
        !! Interception class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Transpiration), intent(in) :: model_transp
      type(Basin_summary_ptr), intent(inout) :: basin_summary
        !! Basin summary
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
        !! Summary by HRU module
    end function
  end interface

  interface
    module subroutine run_Interception(this, ctl_data, param_data, model_basin, &
                                       model_potet, model_precip, model_transp, model_climate, model_time)
      class(Interception) :: this
        !! Interception class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Potential_ET), intent(in) :: model_potet
      class(Precipitation), intent(inout) :: model_precip
      class(Transpiration), intent(in) :: model_transp
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Interception(this)
      class(Interception) :: this
        !! Interception class
    end subroutine
  end interface

  interface
    module subroutine intercept(intcp_on, net_precip, intcp_stor, cov, precip, stor_max)
      ! integer(i32), intent(out) :: intcp_on
      logical, intent(out) :: intcp_on
      real(r32), intent(out) :: net_precip
      real(r32), intent(inout) :: intcp_stor
      real(r32), intent(in) :: cov
      real(r32), intent(in) :: precip
      real(r32), intent(in) :: stor_max
    end subroutine
  end interface

end module
