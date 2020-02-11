module PRMS_GWFLOW
  use variableKind
  use iso_fortran_env, only: output_unit, error_unit
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
    real(r32), pointer :: gwflow_coef(:)
    real(r32), pointer :: gwsink_coef(:)
    real(r32), pointer, private :: gwstor_init(:)
    real(r32), pointer, private :: gwstor_min(:)

    ! NOTE: manual says the following is part of muskingum_lake but only gwflow uses
    !       them when weir_gate_flag == 1 (set in Basin module)
    real(r32), pointer, private :: elevlake_init(:)
      !! Initial lake surface elevation for each lake using broad-crested weir routing or gate opening routing
    real(r32), pointer, private :: gw_seep_coef(:)
      !! Linear coefficient in equation to compute lakebed seepage to the GWR and groundwater discharge to each lake using broad-crested weir routing or gate opening routing
    real(r32), pointer, private :: lake_seep_elev(:)
      !! Elevation over which lakebed seepage to the GWR occurs for lake HRUs using broad-crested weir routing or gate opening routing


    ! Local Variables
    real(r64), allocatable, private :: gwstor_minarea(:)
    real(r64), allocatable :: gwin_dprst(:)
    real(r64), allocatable, private :: lake_seepage_max(:)

    logical :: has_gwstor_minarea

    ! Water-balance
    real(r64), allocatable :: hru_storage_ante(:)
    real(r64), allocatable :: gwres_stor_ante(:)

    ! Output variables
    real(r32), allocatable, private :: elevlake(:)
    real(r32), pointer :: gwres_flow(:)
    real(r32), pointer :: gwres_sink(:)
    real(r32), pointer :: hru_gw_cascadeflow(:)

    real(r64), pointer, private :: gw_in_soil(:)
      ! r64 is correct
    real(r64), pointer, private :: gw_in_ssr(:)
      ! r64 is correct
    real(r64), pointer, private :: gw_seep_lakein(:)
      ! r64 is correct
    real(r64), pointer :: gw_upslope(:)
      ! r64 is correct
    real(r64), pointer :: gwres_in(:)
      ! r64 is correct
    real(r64), pointer :: gwres_stor(:)
      ! Storage in each GWR (r64 is correct)
    real(r64), pointer :: gwstor_minarea_wb(:)
      ! r64 is correct
    real(r64), pointer :: hru_lateral_flow(:)
      ! r64 is correct
    real(r64), pointer :: hru_storage(:)
      ! r64 is correct
    real(r64), pointer :: hru_streamflow_out(:)
      ! r64 is correct
    real(r64), pointer :: lake_seepage(:)
      ! r64 is correct
    real(r64), pointer :: lake_seepage_gwr(:)
      ! r64 is correct
    real(r64), pointer :: lake_vol(:)
      ! Storage in each lake using broad-crested weir or gate opening routing
      ! r64 is correct

    contains
      procedure, public :: init => init_Gwflow
      procedure, public :: run => run_Gwflow
      procedure, public :: cleanup => cleanup_Gwflow

      ! TODO: Uncomment when cascade module is converted
      ! procedure, private :: rungw_cascade
  end type

  interface
    !! Gwflow constructor
    module subroutine init_Gwflow(this, ctl_data, model_basin, model_climate, &
                                  intcp, soil, runoff, model_summary)
      class(Gwflow), intent(inout) :: this
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
    end subroutine
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
    module subroutine cleanup_Gwflow(this, ctl_data)
      class(Gwflow), intent(in) :: this
        !! Gwflow class
      type(Control), intent(in) :: ctl_data
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
