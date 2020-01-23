module PRMS_SOILZONE
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_INTCP, only: Interception
  use PRMS_POTET, only: Potential_ET
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SNOW, only: Snowcomp
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_TRANSPIRATION, only: Transpiration
  use PRMS_SUMMARY, only: Summary

  implicit none

  private
  public :: Soilzone

  character(len=*), parameter :: MODDESC = 'Soilzone'
  character(len=*), parameter :: MODNAME = 'soilzone'
  character(len=*), parameter :: MODVERSION = '2018-10-10 17:46:00Z'

  type, extends(ModelBase) :: Soilzone
    ! Dimensions
    integer(i32) :: nhrucell = 0
      !! (?should be in map_results?)

    ! Parameters
    real(r32), allocatable :: fastcoef_lin(:)
      !! Linear coefficient in equation to route preferential-flow storage down slope for each HRU
    real(r32), allocatable :: fastcoef_sq(:)
      !! Non-linear coefficient in equation to route preferential-flow storage down slope for each HRU
    integer(i32), allocatable, private :: gvr_hru_id(:)
      !! (?should be in map_results?) Index of the HRU associated with each gravity reservoir
    ! real(r32), allocatable :: lake_evap_adj(:)
      !!
    real(r32), allocatable, public :: pref_flow_den(:)
      !! Fraction of the soil zone in which preferential flow occurs for each HRU
    real(r32), allocatable :: sat_threshold(:)
      !! Water holding capacity of the gravity and preferential-flow reservoirs; difference between field capacity and total soil saturation for each HRU
    real(r32), allocatable :: slowcoef_lin(:)
      !! Linear coefficient in equation to route gravity-reservoir storage down slope for each HRU
    real(r32), allocatable :: slowcoef_sq(:)
      !! Non-linear coefficient in equation to route gravity- reservoir storage down slope for each HRU
    integer(i32), allocatable, private :: soil_type(:)
      !! Soil type of each HRU (1=sand; 2=loam; 3=clay)
      ! NOTE: PAN - manual shows this as part of ET, but I only see it used in soilzone.
    real(r32), allocatable :: soil2gw_max(:)
      !! Maximum amount of the capillary reservoir excess that is routed directly to the GWR for each HRU
    real(r32), allocatable :: ssr2gw_exp(:)
      !! Non-linear coefficient in equation used to route water from the gravity reservoirs to the GWR for each HRU
      ! NOTE: ?should be nssr?
    real(r32), allocatable :: ssr2gw_rate(:)
      !! Linear coefficient in equation used to route water from the gravity reservoir to the GWR for each HRU
      ! NOTE: ?should be nssr?
    real(r32), allocatable, private :: ssstor_init_frac(:)
      !! Initial fraction of available water in the gravity plus preferential-flow reservoirs (fraction of sat_threshold) for each HRU
      ! NOTE: ?should be nssr?


    ! Local Variables
    integer(i32), private :: DBGUNT
    integer(i32), private :: et_type
    ! integer(i32) :: pref_flag
    logical, private :: pref_flag
      !! NOTE: PAN - What is this used for?

    ! integer(i32), allocatable :: pref_flow_flag(:)
    logical, allocatable, private :: pref_flow_flag(:)

    ! integer(i32), allocatable :: soil2gw(:)
    logical, allocatable, private :: soil2gw_flag(:)

    real(r32), allocatable, private :: swale_limit(:)
    real(r64), allocatable, public :: grav_dunnian_flow(:)
    real(r64), allocatable, public :: gvr2pfr(:)
    real(r64), allocatable, public :: pfr_dunnian_flow(:)
    real(r32), allocatable, public :: soil_lower_stor_max(:)
    real(r32), allocatable, public :: soil_moist_ante(:)
    real(r32), allocatable, public :: ssres_stor_ante(:)

    real(r64) :: it0_basin_soil_moist
    real(r64) :: it0_basin_ssstor
    real(r64) :: last_soil_moist
    real(r64) :: last_ssstor


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Output variables
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    real(r32), allocatable :: cap_infil_tot(:)
    real(r32), allocatable :: cap_waterin(:)
    real(r32), allocatable :: dunnian_flow(:)
    real(r32), allocatable :: hru_actet(:)
      !! Actual ET for each HRU
    real(r32), allocatable :: hru_sz_cascadeflow(:)
    real(r64), allocatable :: lakein_sz(:)
      !! should be r64
    real(r32), allocatable :: perv_actet(:)
    real(r32), allocatable :: potet_lower(:)
    real(r32), allocatable :: potet_rechr(:)
    real(r32), allocatable :: pref_flow(:)
    real(r32), allocatable :: pref_flow_in(:)
    real(r32), allocatable :: pref_flow_infil(:)
    real(r32), allocatable :: pref_flow_max(:)
    real(r32), allocatable :: pref_flow_stor(:)
    real(r32), allocatable :: pref_flow_thrsh(:)
    real(r32), allocatable :: recharge(:)
    real(r32), allocatable :: slow_flow(:)
      !! Interflow from gravity reservoir storage that flows to the stream network for each HRU
    real(r32), allocatable :: slow_stor(:)
      !! Storage of gravity reservoir for each HRU
    real(r32), allocatable :: snow_free(:)
    real(r32), allocatable :: soil_lower(:)
    real(r32), allocatable :: soil_lower_ratio(:)
    real(r32), allocatable :: soil_moist_tot(:)
    real(r32), allocatable :: soil_to_gw(:)
      !! Portion of excess flow to the capillary reservoir that drains to the associated GWR for each HRU
    real(r32), allocatable :: soil_to_ssr(:)
      !! Portion of excess flow to the capillary reservoir that flows to the gravity reservoir for each HRU
    real(r32), allocatable :: soil_zone_max(:)
    real(r32), allocatable :: ssr_to_gw(:)
      !! Drainage from the gravity-reservoir to the associated GWR for each HRU
    real(r32), allocatable :: ssres_flow(:)
      !! Interflow from gravity and preferential-flow reservoirs
    real(r32), allocatable :: ssres_in(:)
      !! Inflow to the gravity and preferential-flow reservoirs for each HRU
    real(r32), allocatable :: ssres_stor(:)
      !! Storage in the gravity and preferential-flow reservoirs for each HRU
    real(r32), allocatable :: swale_actet(:)
    real(r32), allocatable :: unused_potet(:)
    real(r64), allocatable :: upslope_dunnianflow(:)
      !! should be r64
    real(r64), allocatable :: upslope_interflow(:)
      !! should be r64


    ! Output variables - basin
    real(r64), allocatable :: basin_actet
      !! (moved from flowvars) Basin area-weighted average actual ET
    real(r64), allocatable :: basin_cap_infil_tot
    real(r64), allocatable :: basin_cap_up_max
    real(r64), allocatable :: basin_capwaterin
    real(r64), allocatable :: basin_cpr_stor_frac
    real(r64), allocatable :: basin_dncascadeflow
    real(r64), allocatable :: basin_dndunnianflow
    real(r64), allocatable :: basin_dninterflow
    real(r64), allocatable :: basin_dunnian
    real(r64), allocatable :: basin_dunnian_gvr
    real(r64), allocatable :: basin_dunnian_pfr
    real(r64), allocatable :: basin_gvr2pfr
    real(r64), allocatable :: basin_gvr2sm
    real(r64), allocatable :: basin_gvr_stor_frac
    real(r64), allocatable :: basin_interflow_max
    real(r64), allocatable :: basin_lakeevap
      !! (moved from flowvars) Basin area-weighted average lake evaporation
    real(r64), allocatable :: basin_lakeinsz
    real(r64), allocatable :: basin_lakeprecip
    real(r64), allocatable :: basin_perv_et
      !! (moved from flowvars) Basin area-weighted average ET from capillary reservoirs
    real(r64), allocatable :: basin_pfr_stor_frac
    real(r64), allocatable :: basin_pref_flow_infil
    real(r64), allocatable :: basin_pref_stor
    real(r64), allocatable :: basin_prefflow
    real(r64), allocatable :: basin_recharge
    real(r64), allocatable :: basin_slowflow
    real(r64), allocatable :: basin_slstor
    real(r64), allocatable :: basin_sm2gvr
    real(r64), allocatable :: basin_sm2gvr_max  ! this is the same as basin_sm2gvr
    real(r64), allocatable :: basin_soil_lower_stor_frac
    real(r64), allocatable :: basin_soil_moist
      !! (from flowvars) Basin area-weighted average capillary reservoir storage
    real(r64), allocatable :: basin_soil_moist_tot
    real(r64), allocatable :: basin_soil_rechr
    real(r64), allocatable :: basin_soil_rechr_stor_frac
    real(r64), allocatable :: basin_soil_to_gw
      !! (moved from flowvars) Basin average excess flow to capillary reservoirs that drain to GWRs
    real(r64), allocatable :: basin_ssflow
      !! (moved from flowvars) Basin area-weighted average interflow from gravity and preferential-flow reservoirs to the stream network
    real(r64), allocatable :: basin_ssin
    real(r64), allocatable :: basin_ssstor
      !! (moved from flowvars)
    real(r64), allocatable :: basin_swale_et
      !! (moved from flowvars)
    real(r64), allocatable :: basin_sz_gwin
    real(r64), allocatable :: basin_sz_stor_frac
    real(r64), allocatable :: basin_sz2gw

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Variables for model_mode == 'GSFLOW'
    integer(i32) :: max_gvrs
    integer(i32), allocatable :: hru_gvr_count(:)
    integer(i32), allocatable :: hru_gvr_index(:, :)
    integer(i32), allocatable :: hrucheck(:)

    real(r32), allocatable :: grav_gwin(:)
    real(r32), allocatable :: gvr2sm(:)
    real(r32), allocatable :: gw2sm_grav(:)
    real(r32), allocatable :: it0_gravity_stor_res(:)
    real(r32), allocatable :: it0_potet(:)
    real(r32), allocatable :: it0_pref_flow_stor(:)
    real(r32), allocatable :: it0_slow_stor(:)
    real(r32), allocatable :: it0_soil_moist(:)
    real(r32), allocatable :: it0_soil_rechr(:)
    real(r32), allocatable :: it0_sroff(:)
    real(r32), allocatable :: it0_ssres_stor(:)
    real(r32), allocatable :: replenish_frac(:)
    real(r32), allocatable :: sm2gw_grav(:)
    real(r64), allocatable :: gvr_hru_pct_adjusted(:)
    real(r64), allocatable :: it0_strm_seg_in(:)

    ! Output variables (GSFLOW)
    real(r32), allocatable :: gravity_stor_res(:)
    ! end GSFLOW variables
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ! real(r32), SAVE, allocatable :: Cascade_interflow(:), Cascade_dunnianflow(:), Interflow_max(:)
    ! real(r32), SAVE, allocatable :: Cpr_stor_frac(:), Pfr_stor_frac(:), Gvr_stor_frac(:), Soil_moist_frac(:)
    ! real(r32), SAVE, allocatable :: Soil_rechr_ratio(:), Snowevap_aet_frac(:), Perv_avail_et(:), Cap_upflow_max(:)

    contains
      procedure, public :: init => init_Soilzone
      procedure, public :: run => run_Soilzone
      procedure, public :: cleanup => cleanup_Soilzone
      procedure, private, nopass :: check_gvr_sm
      procedure, private :: compute_cascades
      procedure, private :: compute_gravflow
      procedure, private, nopass :: compute_gwflow
      procedure, private, nopass :: compute_interflow
      procedure, private, nopass :: compute_soilmoist
      procedure, private :: compute_szactet
      procedure, private :: reset_basin_vars

  end type


  interface
    !! Soilzone constructor
    module subroutine init_Soilzone(this, ctl_data, model_basin, model_climate, snow, model_runoff, model_summary)
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(inout) :: model_climate
      type(Snowcomp), intent(in) :: snow
      type(Srunoff), intent(inout) :: model_runoff
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Soilzone(this, ctl_data, model_basin, model_time, &
                                   model_potet, model_precip, model_climate, intcp, snow, model_transp, model_runoff)
      class(Soilzone) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Time_t), intent(in) :: model_time
      class(Potential_ET), intent(inout) :: model_potet
      class(Precipitation), intent(in) :: model_precip
      type(Climateflow), intent(inout) :: model_climate
        !! Climate variables
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      class(Transpiration), intent(in) :: model_transp
      type(Srunoff), intent(inout) :: model_runoff
    end subroutine
  end interface

  interface
    module subroutine cleanup_Soilzone(this)
      class(Soilzone) :: this
        !! Soilzone class
    end subroutine
  end interface

  interface
    module subroutine check_gvr_sm(capacity, depth, frac, gvr2sm, input)
      real(r32), intent(inout) :: capacity
      real(r32), intent(inout) :: depth
      real(r64), intent(in) :: frac
      real(r32), intent(inout) :: gvr2sm
      real(r32), intent(inout) :: input
    end subroutine
  end interface

  interface
    module subroutine compute_cascades(this, runoff, model_time, ihru, ncascade_hru, &
                                slowflow, preflow, dunnian, dnslowflow, &
                                dnpreflow, dndunnflow)
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      type(Srunoff), intent(inout) :: runoff
      type(Time_t), intent(in) :: model_time
      ! type(Cascade), intent(in) :: model_cascade
      integer(i32), intent(in) :: ihru
      integer(i32), intent(in) :: ncascade_hru
      real(r32), intent(inout) :: dunnian
      real(r32), intent(inout) :: slowflow
      real(r32), intent(inout) :: preflow
      real(r32), intent(inout) :: dnslowflow
      real(r32), intent(inout) :: dnpreflow
      real(r32), intent(inout) :: dndunnflow
    end subroutine
  end interface

  interface
    module subroutine compute_gravflow(this, ctl_data, runoff, &
                                       ihru, capacity, slowcoef_lin, &
                                       slowcoef_sq, ssr2gw_rate, ssr2gw_exp, &
                                       gvr_maxin, pref_flow_thrsh, gvr2pfr, &
                                       ssr_to_gw, slow_flow, slow_stor, gvr2sm, &
                                       soil_to_gw, gwin, hru_type)
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Srunoff), intent(in) :: runoff
      integer(i32), intent(in) :: ihru
      real(r32), intent(inout) :: capacity
      real(r32), intent(in) :: slowcoef_lin
      real(r32), intent(in) :: slowcoef_sq
      real(r32), intent(in) :: ssr2gw_rate
      real(r32), intent(in) :: ssr2gw_exp
      real(r32), intent(in) :: gvr_maxin
      real(r32), intent(in) :: pref_flow_thrsh
      real(r32), intent(out) :: gvr2pfr
      real(r32), intent(out) :: ssr_to_gw
      real(r64), intent(out) :: slow_flow
      real(r64), intent(out) :: slow_stor
      real(r32), intent(out) :: gvr2sm
      real(r32), intent(in) :: soil_to_gw
      real(r64), intent(out) :: gwin
      integer(i32), intent(in) :: hru_type
    end subroutine
  end interface

  interface
    module subroutine compute_gwflow(ssr2gw_rate, ssr2gw_exp, ssr_to_gw, slow_stor)
      real(r32), intent(in) :: ssr2gw_rate
      real(r32), intent(in) :: ssr2gw_exp
      real(r32), intent(inout) :: ssr_to_gw
      real(r32), intent(inout) :: slow_stor
    end subroutine
  end interface

  interface
    module subroutine compute_interflow(coef_lin, coef_sq, ssres_in, storage, &
                                        inter_flow)
      real(r32), intent(in) :: coef_lin
      real(r32), intent(in) :: coef_sq
      real(r32), intent(in) :: ssres_in
      real(r32), intent(inout) :: storage
      real(r32), intent(inout) :: inter_flow
    end subroutine
  end interface

  interface
    module subroutine compute_soilmoist(soil2gw_flag, perv_frac, soil_moist_max, &
                                 soil_rechr_max, soil2gw_max, infil, &
                                 soil_moist, soil_rechr, soil_to_gw, soil_to_ssr)
      ! integer(i32), intent(in) :: soil2gw
      logical, intent(in) :: soil2gw_flag
      real(r32), intent(in) :: perv_frac
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(in) :: soil2gw_max
      real(r32), intent(inout) :: infil
      real(r32), intent(inout) :: soil_moist
      real(r32), intent(inout) :: soil_rechr
      real(r32), intent(inout) :: soil_to_gw
      real(r32), intent(inout) :: soil_to_ssr
    end subroutine
  end interface


  interface
    module subroutine compute_szactet(this, model_time, transp_on, cov_type, soil_type, &
                                      soil_moist_max, soil_rechr_max, snow_free, &
                                      soil_moist, soil_rechr, avail_potet, &
                                      potet_rechr, potet_lower, perv_actet)
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      type(Time_t), intent(in) :: model_time
      ! integer(i32), intent(in) :: transp_on
      logical, intent(in) :: transp_on
      integer(i32), intent(in) :: cov_type
      integer(i32), intent(in) :: soil_type
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(in) :: snow_free
      real(r32), intent(inout) :: soil_moist
      real(r32), intent(inout) :: soil_rechr
      real(r64), intent(inout) :: avail_potet
      real(r32), intent(inout) :: potet_rechr
      real(r32), intent(inout) :: potet_lower
      real(r32), intent(out) :: perv_actet
    end subroutine
  end interface

  interface
    module subroutine reset_basin_vars(this)
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
    end subroutine
  end interface


end module
