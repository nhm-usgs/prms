module PRMS_GWFLOW
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_INTCP, only: Interception
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_SUMMARY, only: Summary

  implicit none

  private
  public :: Gwflow

  character(len=*), parameter :: MODDESC = 'Groundwater flow'
  character(len=*), parameter :: MODNAME = 'gwflow'
  character(len=*), parameter :: MODVERSION = '2018-10-10 17:53:00Z'

  type, extends(ModelBase) :: Gwflow
    ! Dimensions
    integer(i32) :: ngw
      !! Number of groundwater reservoirs

    ! Parameters
    real(r32), allocatable :: gwflow_coef(:)
    real(r32), allocatable :: gwsink_coef(:)
    real(r32), allocatable :: gwstor_init(:)
    real(r32), allocatable :: gwstor_min(:)

    ! NOTE: manual says the following is part of muskingum_lake but only gwflow uses
    !       them when weir_gate_flag == 1 (set in Basin module)
    real(r32), allocatable :: elevlake_init(:)
      !! Initial lake surface elevation for each lake using broad-crested weir routing or gate opening routing
    real(r32), allocatable :: gw_seep_coef(:)
      !! Linear coefficient in equation to compute lakebed seepage to the GWR and groundwater discharge to each lake using broad-crested weir routing or gate opening routing
    real(r32), allocatable :: lake_seep_elev(:)
      !! Elevation over which lakebed seepage to the GWR occurs for lake HRUs using broad-crested weir routing or gate opening routing


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
    module function constructor_Gwflow(ctl_data, model_basin, &
                                       model_climate, intcp, soil, runoff, &
                                       model_summary) result(this)
      type(Gwflow) :: this
       !! Gwflow class
      type(Control), intent(in) :: ctl_data
       !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: model_climate
       !! Climate variables
      type(Interception), intent(in) :: intcp
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Summary), intent(inout) :: model_summary
    end function
  end interface

  interface
    module subroutine run_Gwflow(this, ctl_data, model_basin, &
                                   model_climate, intcp, soil, runoff, model_time)
      class(Gwflow), intent(inout) :: this
        !! Gwflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
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
