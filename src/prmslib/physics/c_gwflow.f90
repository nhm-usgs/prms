module PRMS_GWFLOW
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_INTCP, only: Interception
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_BASIN_SUMMARY_PTR, only: basin_summary_ptr
  use PRMS_NHRU_SUMMARY_PTR, only: Nhru_summary_ptr

  implicit none

  private
  public :: Gwflow

  character(len=*), parameter :: MODDESC = 'Groundwater flow'
  character(len=*), parameter :: MODNAME = 'gwflow'
  character(len=*), parameter :: MODVERSION = '2018-10-10 17:53:00Z'

  type, extends(ModelBase) :: Gwflow
    ! Local Variables
    real(r64), allocatable :: gwstor_minarea(:)
    real(r64), allocatable :: gwin_dprst(:)
    real(r64), allocatable :: lake_seepage_max(:)

    logical :: has_gwstor_minarea
    ! integer(i32) :: gwminarea_flag

    ! Declared Variables
    real(r64), pointer :: basin_dnflow
    real(r64), pointer :: basin_gw_upslope
    real(r64), pointer :: basin_gwflow
    real(r64), pointer :: basin_gwin
    real(r64), pointer :: basin_gwsink
    real(r64), pointer :: basin_gwstor
    real(r64), pointer :: basin_gwstor_minarea_wb
    real(r64), pointer :: basin_lake_seep

    real(r32), allocatable :: elevlake(:)
    real(r64), allocatable :: gwres_flow(:)
    real(r64), allocatable :: gwres_sink(:)
    real(r64), allocatable :: hru_gw_cascadeflow(:)

    real(r64), allocatable :: gw_in_soil(:)
    real(r64), allocatable :: gw_in_ssr(:)
    real(r64), allocatable :: gw_seep_lakein(:)
    real(r64), allocatable :: gw_upslope(:)
    real(r64), allocatable :: gwres_in(:)
    real(r64), allocatable :: gwres_stor(:)
      !! (moved from flowvars) Storage in each GWR
    real(r64), allocatable :: gwstor_minarea_wb(:)
    real(r64), allocatable :: hru_lateral_flow(:)
    real(r64), allocatable :: hru_storage(:)
    real(r64), allocatable :: hru_streamflow_out(:)
    real(r64), allocatable :: lake_seepage(:)
    real(r64), allocatable :: lake_seepage_gwr(:)
    real(r64), allocatable :: lake_vol(:)
      !! (moved from flowvars) Storage in each lake using broad-crested weir or gate opening routing

    contains
      procedure, public :: run => run_Gwflow
      procedure, public :: cleanup => cleanup_Gwflow

      ! TODO: Uncomment when cascade module is converted
      ! procedure, private :: rungw_cascade
  end type

  interface Gwflow
    !! Gwflow constructor
    module function constructor_Gwflow(ctl_data, param_data, model_basin, &
                                       model_climate, intcp, soil, runoff, &
                                       basin_summary, nhru_summary) result(this)
      type(Gwflow) :: this
       !! Gwflow class
      type(Control), intent(in) :: ctl_data
       !! Control file parameters
      type(Parameters), intent(in) :: param_data
       !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: model_climate
       !! Climate variables
      type(Interception), intent(in) :: intcp
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Basin_summary_ptr), intent(inout) :: basin_summary
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
        !! Summary by HRU module
    end function
  end interface

  interface
    module subroutine run_Gwflow(this, ctl_data, param_data, model_basin, &
                                   model_climate, intcp, soil, runoff, model_time)
      class(Gwflow), intent(inout) :: this
        !! Gwflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      ! type(Cascade), intent(in) :: model_cascade
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Interception), intent(in) :: intcp
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Gwflow(this)
      class(Gwflow) :: this
        !! Gwflow class
    end subroutine
  end interface

  ! TODO: Uncomment when cascade module is converted
  ! interface
  !   module subroutine rungw_cascade(this, runoff, model_time, igwr, ncascade_gwr, gwres_flow, dnflow)
  !     class(Gwflow), intent(inout) :: this
  !     ! type(Cascade), intent(in) :: model_cascade
  !     type(Srunoff), intent(inout) :: runoff
  !     type(Time_t), intent(in) :: model_time
  !     integer(i32), intent(in) :: igwr
  !     integer(i32), intent(in) :: ncascade_gwr
  !     real(r32), intent(inout) :: gwres_flow
  !       !! in inches
  !     real(r32), intent(out) :: dnflow
  !   end subroutine
  ! end interface
end module
