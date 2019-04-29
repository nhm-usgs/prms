!***********************************************************************
! Computes inflows to and outflows from soil zone of each HRU and
! includes inflows from infiltration, ground-water, and upslope HRUs,
! and outflows to gravity drainage, interflow, and surface runoff to
! down-slope HRUs; merge of smbal_prms and ssflow_prms with enhancements
!
! Daily accounting for soil zone;
!    adds infiltration
!    computes et
!    computes recharge of soil zone
!    computes interflow to stream or cascade
!    adjusts storage in soil zone
!    sends dunnian runoff to stream or cascade by adding to sroff
!    computes drainage to groundwater
!***********************************************************************
      MODULE PRMS_SOILZONE
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: BALUNT, DBGUNT
      DOUBLE PRECISION, PARAMETER :: TOOSMALL = 1.0D-05 
      CHARACTER(LEN=8), SAVE :: MODNAME
      INTEGER, SAVE :: Et_type, Pref_flag
      DOUBLE PRECISION, SAVE :: Last_soil_moist, Last_ssstor
      INTEGER, SAVE, ALLOCATABLE :: Soil2gw(:)
      INTEGER, SAVE, ALLOCATABLE :: Pref_flow_flag(:)
      REAL, SAVE, ALLOCATABLE :: Gvr2pfr(:)
      REAL, SAVE, ALLOCATABLE :: Swale_limit(:)
      INTEGER, SAVE :: First_run
      INTEGER, SAVE, ALLOCATABLE :: Hru_gvr_count(:), Hru_gvr_index(:,:)
      INTEGER, SAVE, ALLOCATABLE :: Hrucheck(:)
      REAL, SAVE, ALLOCATABLE :: Replenish_frac(:)
      REAL, SAVE, ALLOCATABLE :: It0_soil_rechr(:), It0_soil_moist(:)
      REAL, SAVE, ALLOCATABLE:: It0_pref_flow_stor(:), It0_ssres_stor(:)
      REAL, SAVE, ALLOCATABLE :: It0_gravity_stor_res(:), It0_sroff(:)
      REAL, SAVE, ALLOCATABLE :: It0_slow_stor(:), It0_potet(:)
      DOUBLE PRECISION, ALLOCATABLE :: It0_strm_seg_in(:)
      DOUBLE PRECISION, SAVE :: It0_strm_farfield, Basin_gwin
      DOUBLE PRECISION, SAVE :: It0_basin_soil_moist, It0_basin_ssstor
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sz2gw, Basin_ssr2gw
      DOUBLE PRECISION, SAVE :: Basin_interflow_max, Basin_sm2gvr_max
      DOUBLE PRECISION, SAVE :: Basin_soil_rechr, Basin_dunnian_gvr
      DOUBLE PRECISION, SAVE :: Basin_recharge, Basin_pref_flow_in
      DOUBLE PRECISION, SAVE :: Basin_ssin, Basin_dunnian_pfr
      DOUBLE PRECISION, SAVE :: Basin_sm2gvr, Basin_dninterflow
      DOUBLE PRECISION, SAVE :: Basin_dncascadeflow, Basin_dndunnianflow
      DOUBLE PRECISION, SAVE :: Basin_capwaterin, Basin_dunnian
      DOUBLE PRECISION, SAVE :: Basin_gvr2pfr, Basin_slowflow
      DOUBLE PRECISION, SAVE :: Basin_szfarflow, Basin_pref_stor
      DOUBLE PRECISION, SAVE :: Basin_slstor, Basin_prefflow
      DOUBLE PRECISION, SAVE :: Basin_lakeinsz, Basin_lakeprecip
      DOUBLE PRECISION, SAVE :: Basin_cap_infil_max, Basin_cap_up_max
      DOUBLE PRECISION, SAVE :: Basin_capillary_wb, Basin_gravity_wb
      DOUBLE PRECISION, SAVE :: Basin_soilzone_wb, Basin_soil_moist_tot
      REAL, SAVE, ALLOCATABLE :: Perv_actet(:), Pref_flow_thrsh(:) 
      REAL, SAVE, ALLOCATABLE :: Soil_moist_tot(:), Soil_moist_frac(:)
      REAL, SAVE, ALLOCATABLE :: Recharge(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_interflow(:), Dunnian_flow(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_dunnianflow(:), Infil_tot(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_stor(:), Pref_flow(:)
      REAL, SAVE, ALLOCATABLE :: Lakein_sz(:), Pref_flow_infil(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sz_cascadeflow(:), Swale_actet(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_max(:), Snow_free(:)
      REAL, SAVE, ALLOCATABLE :: Cap_waterin(:), Soil_lower(:)
      REAL, SAVE, ALLOCATABLE :: Soil_zone_max(:), Soil_rechr_ratio(:)
      REAL, SAVE, ALLOCATABLE :: Potet_lower(:), Potet_rechr(:)
      REAL, SAVE, ALLOCATABLE :: Soil_lower_ratio(:)
      DOUBLE PRECISION, SAVE :: Basin_gvr2sm
      REAL, SAVE, ALLOCATABLE :: Sm2gw_grav(:), Sm2gw_grav_old(:)
      REAL, SAVE, ALLOCATABLE :: Gravity_stor_res(:)
      REAL, SAVE, ALLOCATABLE :: Gvr2sm(:)
!   Declared Variables from other modules - mf2prms
      REAL, ALLOCATABLE :: Gw2sm_grav(:)
!   Declared Variables from other modules - prms2mf
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gvr_hru_pct_adjusted(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Soil_type(:), Gvr_hru_id(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_init(:), Ssstor_init(:)
      REAL, SAVE, ALLOCATABLE :: Pref_flow_den(:)
      REAL, SAVE, ALLOCATABLE :: Fastcoef_lin(:), Fastcoef_sq(:)
      REAL, SAVE, ALLOCATABLE :: Slowcoef_lin(:), Slowcoef_sq(:)
      REAL, SAVE, ALLOCATABLE :: Ssr2gw_rate(:), Ssr2gw_exp(:)
      REAL, SAVE, ALLOCATABLE :: Soil_rechr_init(:), Soil2gw_max(:)
      REAL, SAVE, ALLOCATABLE :: Lake_evap_adj(:, :)
      END MODULE PRMS_SOILZONE

!***********************************************************************
!     Main soilzone routine
!***********************************************************************
      INTEGER FUNCTION soilzone()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: szdecl, szinit, szrun
!***********************************************************************
      soilzone = 0

      IF ( Process(:3)=='run' ) THEN
        soilzone = szrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        soilzone = szdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        soilzone = szinit()
      ENDIF

      END FUNCTION soilzone

!***********************************************************************
!     szdecl - set up parameters for soil zone computations
!   Declared Parameters
!     sat_threshold, ssstor_init fastcoef_lin, fastcoef_sq
!     ssr2gw_rate, ssr2gw_exp, soil2gw_max, soil_type
!     soil_rechr_max, soil_rechr_init, soil_moist_max, soil_moist_init
!     pref_flow_den, slowcoef_lin, cov_type
!     hru_area, slowcoef_sq, gvr_hru_id
!***********************************************************************
      INTEGER FUNCTION szdecl()
      USE PRMS_SOILZONE
      USE PRMS_MODULE, ONLY: Model, Nhru, Nssr, Nsegment, Nlake,
     +    Nhrucell
      USE PRMS_CASCADE, ONLY: Cascade_flag
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar, getdim
! Local Variables
      INTEGER :: n, nc
      CHARACTER(LEN=80), SAVE :: Version_soilzone
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Soil Zone'
!***********************************************************************
      szdecl = 0

      Version_soilzone =
     +'$Id: soilzone.f 5243 2013-01-15 21:21:49Z rsregan $'
      nc = INDEX( Version_soilzone, 'Z' )
      n = INDEX( Version_soilzone, '.f' ) + 1
      IF ( declmodule(Version_soilzone(6:n), PROCNAME,
     +     Version_soilzone(n+2:nc))/=0 ) STOP
      MODNAME = 'soilzone'

! Declare Variables
      IF ( declvar(MODNAME, 'basin_capillary_wb', 'one', 1, 'double',
     +     'Basin area-weighted average capillary reservoir storage',
     +     'inches', Basin_capillary_wb)/=0 )
     +     CALL read_error(3, 'basin_capillary_wb')

      IF ( declvar(MODNAME, 'basin_gravity_wb', 'one', 1, 'double',
     +     'Basin area-weighted average gravity reservoir storage',
     +     'inches', Basin_gravity_wb)/=0 )
     +     CALL read_error(3, 'basin_gravity_wb')

      IF ( declvar(MODNAME, 'basin_soilzone_wb', 'one', 1, 'double',
     +     'Basin area-weighted average storage in soilzone reservoirs',
     +     'inches', Basin_soilzone_wb)/=0 )
     +     CALL read_error(3, 'basin_soilzone_wb')

      IF ( declvar(MODNAME, 'basin_capwaterin', 'one', 1, 'double',
     +     'Basin area-weighted average infiltration,'//
     +     ' cascading interflow and Dunnian flow added to'//
     +     ' capillary reservoir storage',
     +     'inches', Basin_capwaterin)/=0 )
     +     CALL read_error(3, 'basin_capwaterin')

      IF ( declvar(MODNAME, 'basin_cap_infil_max', 'one', 1, 'double',
     +     'Basin area-weighted average maximum infiltration'//
     +     ' that flows to capillary reservoirs',
     +     'inches', Basin_cap_infil_max)/=0 )
     +     CALL read_error(3, 'basin_cap_infil_max')

      IF ( declvar(MODNAME, 'basin_cap_up_max', 'one', 1, 'double',
     +     'Basin area-weighted average maximum cascade flow'//
     +     ' that flows to the capillary reservoirs',
     +     'inches', Basin_cap_up_max)/=0 )
     +     CALL read_error(3, 'basin_cap_up_max')

      IF ( declvar(MODNAME, 'basin_pref_flow_in', 'one', 1, 'double',
     +     'Basin area-weighted average infiltration to preferential'//
     +     '-flow reservoir storage',
     +     'inches', Basin_pref_flow_in)/=0 )
     +     CALL read_error(3, 'basin_pref_flow_in')

      IF ( declvar(MODNAME, 'basin_dunnian_pfr', 'one', 1, 'double',
     +     'Basin area-weighted average excess infiltration to'//
     +     ' preferential-flow reservoirs from variable infil',
     +     'inches', Basin_dunnian_pfr)/=0 )
     +     CALL read_error(3, 'basin_dunnian_pfr')

      IF ( declvar(MODNAME, 'basin_dunnian_gvr', 'one', 1, 'double',
     +     'Basin area-weighted average excess flow to preferential'//
     +     '-flow reservoirs from gravity reservoirs',
     +     'inches', Basin_dunnian_gvr)/=0 )
     +     CALL read_error(3, 'basin_dunnian_gvr')

      ALLOCATE ( Infil_tot(Nhru) )
      IF ( declvar(MODNAME, 'infil_tot', 'nhru', Nhru, 'real',
     +     'Infiltration and cascading interflow and Dunnian'//
     +     ' flow added to capillary reservoir storage for'//
     +     ' each HRU',
     +     'inches', Infil_tot)/=0 ) CALL read_error(3, 'infil_tot')

      IF ( declvar(MODNAME, 'basin_soil_moist_tot', 'one', 1, 'double',
     +     'Basin area-weighted average Total soil-zone water storage',
     +     'inches', Basin_soil_moist_tot)/=0 )
     +     CALL read_error(3, 'basin_soil_moist_tot')

      ALLOCATE ( Soil_moist_tot(Nhru) )
      IF ( declvar(MODNAME, 'soil_moist_tot', 'nhru', Nhru, 'real',
     +     'Total soil-zone water storage (soil_moist + ssres_stor)',
     +     'inches', Soil_moist_tot)/=0 )
     +     CALL read_error(3, 'soil_moist_tot')

      ALLOCATE ( Soil_moist_frac(Nhru) )
      IF ( declvar(MODNAME, 'soil_moist_frac', 'nhru', Nhru, 'real',
     +     'Fraction of capillary reservoir storage of the'//
     +     ' maximum storage for each HRU',
     +     'decimal fraction', Soil_moist_frac)/=0 )
     +     CALL read_error(3, 'soil_moist_frac')

      IF ( declvar(MODNAME, 'basin_sm2gvr', 'one', 1, 'double',
     +     'Basin area-weighted average excess flow from'//
     +     ' capillary reservoirs to gravity reservoir storage',
     +     'inches', Basin_sm2gvr)/=0 )
     +     CALL read_error(3, 'basin_sm2gvr')

      IF ( declvar(MODNAME, 'basin_gvr2pfr', 'one', 1, 'double',
     +     'Basin area-weighted average excess flow to'//
     +     ' preferential-flow reservoir storage from gravity'//
     +     ' reservoirs',
     +     'inches', Basin_gvr2pfr)/=0 )
     +     CALL read_error(3, 'basin_gvr2pfr')

      IF ( declvar(MODNAME, 'basin_slowflow', 'one', 1, 'double',
     +     'Basin area-weighted average interflow from gravity'//
     +     ' reservoirs to the stream network',
     +     'inches', Basin_slowflow)/=0 )
     +     CALL read_error(3, 'basin_slowflow')

      IF ( declvar(MODNAME, 'basin_prefflow', 'one', 1, 'double',
     +     'Basin area-weighted average interflow from'//
     +     ' preferential-flow  reservoirs to the stream network',
     +     'inches', Basin_prefflow)/=0 )
     +     CALL read_error(3, 'basin_prefflow')

      IF ( declvar(MODNAME, 'basin_slstor', 'one', 1, 'double',
     +     'Basin area-weighted average storage of gravity reservoirs',
     +     'inches', Basin_slstor)/=0 )
     +     CALL read_error(3, 'basin_slstor')

      ALLOCATE ( Dunnian_flow(Nhru) )
      IF ( declvar(MODNAME, 'dunnian_flow', 'nhru', Nhru, 'real',
     +     'Dunnian surface runoff'//
     +     ' that flows to the stream network for each HRU',
     +     'inches', Dunnian_flow)/=0 )
     +     CALL read_error(3, 'dunnian_flow')

      IF ( declvar(MODNAME, 'basin_dunnian', 'one', 1, 'double',
     +     'Basin area-weighted average Dunnian surface runoff that'//
     +     ' flows to the stream network',
     +     'inches', Basin_dunnian)/=0 )
     +     CALL read_error(3, 'basin_dunnian')

      IF ( declvar(MODNAME, 'basin_soil_rechr', 'one', 1, 'double',
     +     'Basin area-weighted average storage for recharge zone;'//
     +     ' upper portion of capillary reservoir where both'//
     +     ' evaportation and transpiration occurs',
     +     'inches', Basin_soil_rechr)/=0 )
     +     CALL read_error(3, 'basin_soil_rechr')

      IF ( declvar(MODNAME, 'basin_sz2gw', 'one', 1, 'double',
     +     'Basin area-weighted average drainage from gravity'//
     +     ' reservoirs to GWRs',
     +     'inches', Basin_sz2gw)/=0 ) CALL read_error(3, 'basin_sz2gw')

      ! kept for downward compatibilty, user's should only use
      ! basin_sz2gw
      IF ( declvar(MODNAME, 'basin_ssr2gw', 'one', 1, 'double',
     +     'Basin area-weighted average drainage from gravity'//
     +     ' reservoirs to GWRs',
     +     'inches', Basin_ssr2gw)/=0 )
     +      CALL read_error(3, 'basin_ssr2gw')

      IF ( declvar(MODNAME, 'basin_sm2gvr_maxin', 'one', 1, 'double',
     +     'Basin area-weighted average maximum excess flow from'//
     +     ' capillary reservoirs that flows to gravity reservoirs',
     +     'inches', Basin_sm2gvr_max)/=0 )
     +     CALL read_error(3, 'basin_sm2gvr_max')

      IF ( declvar(MODNAME, 'basin_interflow_max', 'one', 1, 'double',
     +     'Basin area-weighted average maximum interflow that flows'//
     +     ' from gravity reservoirs',
     +     'inches', Basin_interflow_max)/=0 )
     +     CALL read_error(3, 'basin_interflow_max')

      ALLOCATE ( Perv_actet(Nhru) )
      IF ( declvar(MODNAME, 'perv_actet', 'nhru', Nhru, 'real',
     +     'Actual ET from the capillary reservoir of each HRU',
     +     'inches', Perv_actet)/=0 ) CALL read_error(3, 'perv_actet')

      ! added to be compatible with ssflow_prms
      IF ( declvar(MODNAME, 'basin_ssin', 'one', 1, 'double',
     +     'Basin area-weighted average inflow to gravity'//
     +     ' and preferential-flow reservoir storage',
     +     'inches',
     +     Basin_ssin)/=0 ) CALL read_error(3, 'basin_ssin')

      IF ( Cascade_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'basin_dndunnianflow', 'one', 1,
     +       'double',
     +       'Basin area-weighted average cascading Dunnian flow',
     +       'inches', Basin_dndunnianflow)/=0 )
     +       CALL read_error(3, 'basin_dndunnianflow')

        IF ( declvar(MODNAME, 'basin_dninterflow', 'one', 1,'double',
     +       'Basin area-weighted average cascading interflow',
     +       'inches', Basin_dninterflow)/=0 )
     +       CALL read_error(3, 'basin_dninterflow')

        IF ( declvar(MODNAME, 'basin_dncascadeflow', 'one', 1,
     +       'double',
     +       'Basin area-weighted average cascading'//
     +       ' interflow, Dunnian surface runoff, and farflow',
     +       'inches', Basin_dncascadeflow)/=0 )
     +       CALL read_error(3, 'basin_dncascadeflow')

        ALLOCATE ( Upslope_interflow(Nhru) )
        IF ( declvar(MODNAME, 'upslope_interflow', 'nhru', Nhru,
     +       'real', 'Cascading interflow runoff that flows to'//
     +       ' the capillary reservoir of each down slope HRU'//
     +       ' for each upslope HRU',
     +       'inches', Upslope_interflow)/=0 )
     +       CALL read_error(3, 'upslope_interflow')

        ALLOCATE ( Upslope_dunnianflow(Nhru) )
        IF ( declvar(MODNAME, 'upslope_dunnianflow', 'nhru', Nhru,
     +       'real', 'Cascading Dunnian surface runoff that'//
     +       ' flows to the capillary reservoir of each down'//
     +       ' slope HRU for each upslope HRU',
     +       'inches', Upslope_dunnianflow)/=0 )
     +       CALL read_error(3, 'upslope_dunnianflow')

        ALLOCATE ( Hru_sz_cascadeflow(Nhru) )
        IF ( declvar(MODNAME, 'hru_sz_cascadeflow', 'nhru', Nhru,
     +       'real',
     +       'Cascading interflow, Dunnian surface runoff, and'//
     +       ' farflow from each HRU',
     +       'inches', Hru_sz_cascadeflow)/=0 )
     +       CALL read_error(3, 'hru_sz_cascadeflow')

        ALLOCATE ( Lakein_sz(Nhru) )
        IF ( declvar(MODNAME, 'lakein_sz', 'nhru', Nhru, 'real',
     +       'Cascading interflow and Dunnian surface runoff to'//
     +       ' lake HRUs for each upslope HRU',
     +       'inches',
     +       Lakein_sz)/=0 ) CALL read_error(3, 'lakein_sz')

        IF ( declvar(MODNAME, 'basin_lakeinsz', 'one', 1, 'double',
     +       'Basin area-weighted average lake inflow from land HRUs',
     +       'inches',
     +       Basin_lakeinsz)/=0 ) CALL read_error(3, 'basin_lakeinsz')
      ENDIF

      IF ( declvar(MODNAME, 'basin_pref_stor', 'one', 1, 'double',
     +     'Basin area-weighted average storage in'//
     +     ' preferential-flow reservoirs',
     +     'inches', Basin_pref_stor)/=0 )
     +     CALL read_error(3, 'basin_pref_stor')
 
      ALLOCATE ( Pref_flow_infil(Nhru) )
      IF ( declvar(MODNAME, 'pref_flow_infil', 'nhru', Nhru, 'real',
     +     'Infiltration to the preferential-flow reservoir'//
     +     ' storage for each HRU',
     +     'inches', Pref_flow_infil)/=0 )
     +     CALL read_error(3, 'pref_flow_infil')

      ALLOCATE ( Pref_flow_stor(Nhru) )
      IF ( declvar(MODNAME, 'pref_flow_stor', 'nhru', Nhru, 'real',
     +     'Storage in preferential-flow reservoir for each HRU',
     +     'inches', Pref_flow_stor)/=0 )
     +     CALL read_error(3, 'pref_flow_stor')

      ALLOCATE ( Pref_flow(Nhru) )
      IF ( declvar(MODNAME, 'pref_flow', 'nhru', Nhru, 'real',
     +     'Interflow from the preferential-flow reservoir that'//
     +     ' flows to the stream network for each HRU',
     +     'inches', Pref_flow)/=0 ) CALL read_error(3, 'pref_flow')

      ALLOCATE ( Pref_flow_thrsh(Nhru) )
      IF ( declvar(MODNAME, 'pref_flow_thrsh', 'nhru', Nhru, 'real',
     +     'Soil storage threshold defining storge between field'//
     +     ' capacity and maximum soil saturation minus'//
     +     ' preferential-flow storage',
     +     'inches', Pref_flow_thrsh)/=0 )
     +     CALL read_error(3, 'pref_flow_thrsh')

      ALLOCATE ( Pref_flow_max(Nhru) )
      IF ( declvar(MODNAME, 'pref_flow_max', 'nhru', Nhru, 'real',
     +     'Maximum storage of the preferential-flow reservoir'//
     +     ' for each HRU',
     +     'inches',
     +     Pref_flow_max)/=0 ) CALL read_error(3, 'pref_flow_max')

      ALLOCATE ( Soil_zone_max(Nhru) )
      IF ( declvar(MODNAME, 'soil_zone_max', 'nhru', Nhru, 'real',
     +     'Maximum storage of all soil zone reservoirs',
     +     'inches',
     +     Soil_zone_max)/=0 ) CALL read_error(3, 'soil_zone_max')

      IF ( declvar(MODNAME, 'basin_lakeprecip', 'one', 1, 'double',
     +     'Basin area-weighted average precipitation on lake HRUs',
     +     'inches',
     +     Basin_lakeprecip)/=0 )
     +     CALL read_error(3, 'basin_lakeprecip')

      IF ( declvar(MODNAME, 'basin_szfarflow', 'one', 1, 'double',
     +     'Basin area-weighted average farfield flow from gravity'//
     +     ' and preferential-flow reservoirs',
     +     'inches',
     +     Basin_szfarflow)/=0 ) CALL read_error(3, 'basin_szfarflow')

      ALLOCATE ( Swale_actet(Nhru) )
      IF ( declvar(MODNAME, 'swale_actet', 'nhru', Nhru, 'real',
     +     'Evaporation from the gravity and preferential-flow'//
     +     ' reservoirs that exceeds sat_threshold',
     +     'inches',
     +     Swale_actet)/=0 ) CALL read_error(3, 'swale_actet')

      IF ( declvar(MODNAME, 'basin_recharge', 'one', 1, 'double',
     +     'Basin area-weighted average recharge to GWRs',
     +     'inches',
     +     Basin_recharge)/=0 ) CALL read_error(3, 'basin_recharge')

      ALLOCATE ( Recharge(Nhru) )
      IF ( declvar(MODNAME, 'recharge', 'nhru', Nhru, 'real',
     +     'Recharge to the associated GWR as sum of soil_to_gw and'//
     +     ' ssr_to_gw for each HRU',
     +     'inches',
     +     Recharge)/=0 ) CALL read_error(3, 'recharge')

      ALLOCATE ( Cap_waterin(Nhru) )
      IF ( declvar(MODNAME, 'cap_waterin', 'nhru', Nhru, 'real',
     +     'Infiltration and any cascading interflow and'//
     +     ' Dunnian surface runoff added to capillary'//
     +     ' reservoir storage for each HRU',
     +     'inches',
     +     Cap_waterin)/=0 ) CALL read_error(3, 'cap_waterin')

      ALLOCATE ( Soil_lower(Nhru) )
      IF ( declvar(MODNAME, 'soil_lower', 'nhru', Nhru, 'real',
     +     'Storage in the lower zone of the capillary'//
     +     ' reservoir that is only available for'//
     +     ' transpiration for each HRU',
     +     'inches',
     +     Soil_lower)/=0 ) CALL read_error(3, 'soil_lower')

      ALLOCATE ( Potet_lower(Nhru) )
      IF ( declvar(MODNAME, 'potet_lower', 'nhru', Nhru, 'real',
     +     'Potential ET in the lower zone of the capillary'//
     +     ' reservoir for each HRU',
     +     'inches',
     +     Potet_lower)/=0 ) CALL read_error(3, 'potet_lower')

      ALLOCATE ( Potet_rechr(Nhru) )
      IF ( declvar(MODNAME, 'potet_rechr', 'nhru', Nhru, 'real',
     +     'Potential ET in the recharge zone of the capillary'//
     +     ' reservoir for each HRU',
     +     'inches',
     +     Potet_rechr)/=0 ) CALL read_error(3, 'potet_rechr')

      ALLOCATE ( Soil_lower_ratio(Nhru) )
      IF ( declvar(MODNAME, 'soil_lower_ratio', 'nhru', Nhru, 'real',
     +     'Water content ratio in the lower zone of the capillary'//
     +     ' reservoir for each HRU',
     +     'decimal fraction',
     +     Soil_lower_ratio)/=0 ) CALL read_error(3, 'soil_lower_ratio')

      ALLOCATE ( Soil_rechr_ratio(Nhru) )
      IF ( declvar(MODNAME, 'soil_rechr_ratio', 'nhru', Nhru, 'real',
     +     'Water content ratio in the recharge zone of the capillary'//
     +     ' reservoir for each HRU',
     +     'decimal fraction',
     +     Soil_rechr_ratio)/=0 ) CALL read_error(3, 'soil_rechr_ratio')

      ALLOCATE ( Snow_free(Nhru) )
      IF ( declvar(MODNAME, 'snow_free', 'nhru', Nhru, 'real',
     +     'Fraction of snow-free surface for each HRU',
     +     'decimal fraction',
     +     Snow_free)/=0 ) CALL read_error(3, 'snow_free')

      IF ( Model==0 .OR. Model==99 ) THEN
        IF ( Nhrucell<-1 )
     +       STOP 'ERROR, dimension nhrucell not specified > 0'
        ALLOCATE ( Gravity_stor_res(Nhrucell) )
        IF ( declvar(MODNAME, 'gravity_stor_res', 'nhrucell',
     +       Nhrucell, 'real', 'Storage in each gravity-flow reservoir',
     +       'inches',
     +       Gravity_stor_res)/=0 )
     +       CALL read_error(3, 'gravity_stor_res')

        ALLOCATE ( Sm2gw_grav(Nhrucell) )
        IF ( declvar(MODNAME, 'sm2gw_grav', 'nhrucell', Nhrucell,
     +       'real',
     +      'Drainage from each gravity reservoir to each MODFLOW cell',
     +       'inches',
     +       Sm2gw_grav)/=0 ) CALL read_error(3, 'sm2gw_grav')

        ALLOCATE ( Sm2gw_grav_old(Nhrucell) )
        IF ( declvar(MODNAME, 'sm2gw_grav_old', 'nhrucell', Nhrucell,
     +       'real',
     +       'Drainage from each gravity reservoir to each MODFLOW'//
     +       ' cell from the previous iteration',
     +       'inches',
     +       Sm2gw_grav_old)/=0 ) CALL read_error(3, 'sm2gw_grav_old')

        IF ( declvar(MODNAME, 'basin_gvr2sm', 'one', 1, 'double',
     +       'Basin area-weighted average gravity flow to capillary'//
     +       ' reservoirs',
     +       'inches',
     +       Basin_gvr2sm)/=0 ) CALL read_error(3, 'basin_gvr2sm')

        ALLOCATE ( Gvr2sm(Nhru) )
        IF ( declvar(MODNAME, 'gvr2sm', 'nhru', Nhru, 'real',
     +       'Gravity flow to soil moist replenishment for each HRU',
     +       'inches',
     +       Gvr2sm)/=0 ) CALL read_error(3, 'gvr2sm')

        ALLOCATE ( Hru_gvr_count(Nhru), Hrucheck(Nhru) )
        ALLOCATE ( Gvr_hru_id(Nhrucell) )
        IF ( declparam(MODNAME, 'gvr_hru_id', 'nhrucell', 'integer',
     +       '1', 'bounded', 'nhru',
     +       'Corresponding HRU id of each GVR',
     +       'Index of the HRU associated with each gravity reservoir',
     +       'none')/=0 ) CALL read_error(1, 'gvr_hru_id')

        ALLOCATE ( Gvr_hru_pct_adjusted(Nhrucell) )
        ALLOCATE ( It0_pref_flow_stor(Nhru), It0_ssres_stor(Nhru) )
        ALLOCATE ( It0_soil_rechr(Nhru), It0_soil_moist(Nhru) )
        ALLOCATE ( It0_gravity_stor_res(Nhrucell), It0_sroff(Nhru) )
        ALLOCATE ( It0_slow_stor(Nhru), Gw2sm_grav(Nhrucell) )
        ALLOCATE ( It0_strm_seg_in(Nsegment), It0_potet(Nhru) )
        ALLOCATE ( Replenish_frac(Nhru) )
      ENDIF

! Declare Parameters
      IF ( Nlake>0 .OR. Model==99 ) THEN
        ALLOCATE ( Lake_evap_adj(12,Nlake) )
        IF ( declparam(MODNAME, 'lake_evap_adj', 'nmonths,nlake',
     +       'real', '1.0', '0.005', '1.0',
     +       'Monthly potet factor to adjust potet on lakes',
     +       'Monthly (January to December) adjustment factor for'//
     +       ' potential ET for each lake',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'lake_evap_adj')
      ENDIF

! Allocate arrays for local and variables from other modules
      ALLOCATE ( Soil2gw(Nhru), Gvr2pfr(Nhru), Swale_limit(Nhru) )
      ALLOCATE ( Pref_flow_flag(Nhru) )
      ALLOCATE ( Slowcoef_lin(Nhru), Slowcoef_sq(Nhru) )
      ALLOCATE ( Pref_flow_den(Nhru), Soil2gw_max(Nhru) )
      ALLOCATE ( Soil_type(Nhru), Fastcoef_lin(Nhru), Fastcoef_sq(Nhru))
      ALLOCATE ( Ssr2gw_rate(Nhru), Ssr2gw_exp(Nhru) )

      IF ( declparam(MODNAME, 'slowcoef_lin', 'nhru', 'real',
     +     '0.015', '0.0', '1.0',
     +     'Linear gravity-flow reservoir routing coefficient',
     +     'Linear coefficient in equation to route gravity-reservoir'//
     +     ' storage down slope for each HRU',
     +     '1.0/day')/=0 ) CALL read_error(1, 'slowcoef_lin')

      IF ( declparam(MODNAME, 'slowcoef_sq', 'nhru', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Non-linear gravity-flow reservoir routing coefficient',
     +     'Non-linear coefficient in equation to route'//
     +     ' gravity-reservoir storage down slope for each HRU',
     +     'none')/=0 ) CALL read_error(1, 'slowcoef_sq')

      IF ( declparam(MODNAME, 'pref_flow_den', 'nhru', 'real',
     +     '0.0', '0.0', '1.0',
     +     'Preferential-flow pore density',
     +     'Preferential-flow pore density for each HRU',
     +     'decimal fraction')/=0 ) CALL read_error(1,'pref_flow_den')

      ALLOCATE ( Soil_rechr_init(Nhru) )
      IF ( declparam(MODNAME, 'soil_rechr_init', 'nhru', 'real',
     +     '1.0', '0.0', '10.0',
     +     'Initial storage of water for soil recharge zone',
     +     'Initial storage for soil recharge zone (upper part of'//
     +     ' capillary reservoir where losses occur as both'//
     +     ' evaporation and transpiration) for each HRU; must be'//
     +     ' less than or equal to soil_moist_init',
     +     'inches')/=0 ) CALL read_error(1, 'soil_rechr_init')

      ALLOCATE ( Soil_moist_init(Nhru) )
      IF ( declparam(MODNAME, 'soil_moist_init', 'nhru', 'real',
     +     '3.0', '0.0', '20.0',
     +     'Initial value of available water in capillary reservoir',
     +     'Initial value of available water in capillary reservoir'//
     +     ' for each HRU',
     +     'inches')/=0 ) CALL read_error(1, 'soil_moist_init')

      IF ( declparam(MODNAME, 'soil2gw_max', 'nhru', 'real',
     +     '0.0', '0.0', '5.0',
     +     'Maximum value for capillary reservoir excess to GWR',
     +     'Maximum amount of the capillary reservoir excess that'//
     +     ' is routed directly to the GWR for each HRU',
     +     'inches')/=0 ) CALL read_error(1, 'soil2gw_max')

      IF ( declparam(MODNAME, 'soil_type', 'nhru', 'integer',
     +     '2', '1', '3',
     +     'HRU soil type',
     +     'Soil type of each HRU (1=sand; 2=loam; 3=clay)',
     +     'none')/=0 ) CALL read_error(1, 'soil_type')

      ALLOCATE ( Ssstor_init(Nssr) )
      IF ( declparam(MODNAME, 'ssstor_init', 'nssr', 'real',
     +     '0.0', '0.0', '20.0',
     +     'Initial storage in each GVR and PFR',
     +     'Initial storage of the gravity and preferential-flow'//
     +     ' reservoirs for each HRU',
     +     'inches')/=0 ) CALL read_error(1, 'ssstor_init')

      IF ( declparam(MODNAME, 'fastcoef_lin', 'nhru', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Linear preferential-flow routing coefficient',
     +     'Linear coefficient in equation to route preferential-flow'//
     +     ' storage down slope for each HRU',
     +     '1/day')/=0 ) CALL read_error(1, 'fastcoef_lin')

      IF ( declparam(MODNAME, 'fastcoef_sq', 'nhru', 'real',
     +     '0.8', '0.0', '1.0',
     +     'Non-linear preferential-flow routing coefficient',
     +     'Non-linear coefficient in equation used to route'//
     +     ' preferential-flow storage down slope for each HRU',
     +     'none')/=0 ) CALL read_error(1, 'fastcoef_sq')

      IF ( declparam(MODNAME, 'ssr2gw_rate', 'nssr', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Coefficient to route water from gravity reservoir to GWR',
     +     'Linear coefficient in equation used to route water from'//
     +     ' the gravity reservoir to the GWR for each HRU',
     +     '1/day')/=0 ) CALL read_error(1, 'ssr2gw_rate')

      IF ( declparam(MODNAME, 'ssr2gw_exp', 'nssr', 'real',
     +     '1.0', '0.0', '3.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Non-linear coefficient in equation used to route water'//
     +     ' from the gravity reservoir to the GWR for each HRU',
     +     'none')/=0 ) CALL read_error(1, 'ssr2gw_exp')

      END FUNCTION szdecl

!***********************************************************************
!     szinit - Initialize soilzone module - get parameter values,
!              set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION szinit()
      USE PRMS_SOILZONE
      USE PRMS_MODULE, ONLY: Print_debug, Nhru, Nssr, Nlake, Model,
     +    Nhrucell, Inputerror_flag, Parameter_check_flag
      USE PRMS_CASCADE, ONLY: Cascade_flag
      USE PRMS_BASIN, ONLY: Hru_type, Hru_perv, Hru_route_order,
     +    Basin_area_inv, Hru_area, Timestep, NEARZERO, Hru_frac_perv,
     +    Active_hrus
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_rechr_max,
     +    Ssres_stor, Basin_ssstor, Basin_soil_moist, Slow_stor,
     +    Soil_moist, Sat_threshold, Soil_rechr
      IMPLICIT NONE
! Functions
      EXTERNAL PRMS_open_module_file
      INTEGER, EXTERNAL :: getparam
      INTRINSIC MIN
! Local Variables
      INTEGER :: i, ii, j, max_gvrs, icnt, ierr, ihru
!***********************************************************************
      szinit = 0

      IF ( Print_debug==1 ) THEN
        CALL PRMS_open_module_file(BALUNT, 'soilzone.wbal')
        IF ( Model==0 ) THEN
          WRITE ( BALUNT, 9001 )
        ELSE
          WRITE ( BALUNT, 9009 )
        ENDIF
      ENDIF

      IF ( Print_debug==7 )
     +     CALL PRMS_open_module_file(DBGUNT, 'soilzone.dbg')

      First_run = 1

      IF ( getparam(MODNAME, 'slowcoef_lin', Nhru, 'real',
     +     Slowcoef_lin)/=0 ) CALL read_error(2, 'slowcoef_lin')
      IF ( getparam(MODNAME, 'slowcoef_sq', Nhru, 'real',
     +     Slowcoef_sq)/=0 ) CALL read_error(2, 'slowcoef_sq')
      IF ( getparam(MODNAME, 'pref_flow_den', Nhru, 'real',
     +     Pref_flow_den)/=0 ) CALL read_error(2, 'pref_flow_den')
      IF ( getparam(MODNAME, 'fastcoef_lin', Nhru, 'real',
     +     Fastcoef_lin)/=0 ) CALL read_error(2, 'fastcoef_lin')
      IF ( getparam(MODNAME, 'fastcoef_sq', Nhru, 'real',
     +     Fastcoef_sq)/=0 ) CALL read_error(2, 'fastcoef_sq')
      IF ( getparam(MODNAME, 'ssstor_init', Nssr, 'real',
     +     Ssstor_init)/=0 ) CALL read_error(2, 'ssstor_init')
      IF ( getparam(MODNAME, 'ssr2gw_rate', Nssr, 'real',
     +     Ssr2gw_rate)/=0 ) CALL read_error(2, 'ssr2gw_rate')
      IF ( getparam(MODNAME, 'ssr2gw_exp', Nssr, 'real', Ssr2gw_exp)
     +     /=0 ) CALL read_error(2, 'ssr2gw_exp')
      IF ( getparam(MODNAME, 'soil_moist_init', Nhru, 'real',
     +     Soil_moist_init)/=0 ) CALL read_error(2, 'soil_moist_init')
      IF ( getparam(MODNAME, 'soil_type', Nhru, 'integer', Soil_type)
     +     /=0 ) CALL read_error(2, 'soil_type')
      IF ( getparam(MODNAME, 'soil_rechr_init', Nhru, 'real',
     +     Soil_rechr_init)/=0 ) CALL read_error(2, 'soil_rechr_init')
      IF ( getparam(MODNAME, 'soil2gw_max', Nhru, 'real',
     +     Soil2gw_max)/=0 ) CALL read_error(2, 'soil2gw_max')

      IF ( Nlake>0 ) THEN
        IF ( getparam(MODNAME, 'lake_evap_adj', 12*Nlake, 'real',
     +       Lake_evap_adj)/=0 ) CALL read_error(2, 'lake_evap_adj')
      ENDIF

      IF ( Model==0 ) THEN
        Replenish_frac = 0.0
        IF ( Nhrucell>0 ) THEN
          IF ( getparam(MODNAME, 'gvr_hru_id', Nhrucell, 'integer',
     +         Gvr_hru_id)/=0 ) CALL read_error(2, 'gvr_hru_id')
        ENDIF
      ENDIF

      Swale_limit = 0.0
      Soil2gw = 0
      Pref_flow_flag = 0
      Pref_flag = 0
      Pref_flow_thrsh = 0.0
      Pref_flow_max = 0.0
      ierr = 0
      ! Sanity checks for parameters
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Hru_type(i)==0 .OR. Hru_type(i)==2 ) THEN
          Pref_flow_den(i) = 0.0
          CYCLE
        ENDIF
        IF ( Soil_type(i)<1 .OR. Soil_type(i)>3 ) THEN
          PRINT 9008, i, ' soil_type', Soil_type(i)
          ierr = 1
        ENDIF
        IF ( Ssr2gw_rate(i)<0.0 ) THEN
          PRINT 9006, i, ' ssr2gw_rate', Ssr2gw_rate(i)
          ierr = 1
        ENDIF
        IF ( Soil_rechr_max(i)>Soil_moist_max(i) ) THEN
          IF ( Parameter_check_flag==1 ) THEN
            PRINT 9002, i, Soil_rechr_max(i), Soil_moist_max(i)
            ierr = 1
          ELSE
            IF ( Print_debug>-1 ) PRINT 9012, i,
     +           Soil_rechr_max(i), Soil_moist_max(i)
            Soil_rechr_max(i) = Soil_moist_max(i)
          ENDIF
        ENDIF
        IF ( Soil_moist_init(i)<0.0 ) THEN
          PRINT 9006, i,' soil_moist_init',Soil_moist_init(i)
          ierr = 1
        ENDIF
        IF ( Soil_rechr_init(i)<0.0 ) THEN
          PRINT 9006, i,' soil_rechr_init',Soil_rechr_init(i)
          ierr = 1
        ENDIF
        IF ( Ssstor_init(i)<0.0 ) THEN
          PRINT 9006, i, ' ssstor_init', Ssstor_init(i)
          ierr = 1
        ENDIF
        IF ( Sat_threshold(i)<0.0 ) THEN
          PRINT 9006, i, ' sat_threshold', Sat_threshold(i)
          ierr = 1
        ENDIF
        IF ( Slowcoef_lin(i)<0.0 ) THEN
          PRINT 9006, i, ' slowcoef_lin ', Slowcoef_lin(i)
          ierr = 1
        ENDIF
        IF ( Slowcoef_sq(i)<0.0 ) THEN
          PRINT 9006, i, ' slowcoef_sq', Slowcoef_sq(i)
          ierr = 1
        ENDIF
        IF ( Fastcoef_lin(i)<0.0 ) THEN
          PRINT 9006, i, ' fastcoef_lin', Fastcoef_lin(i)
          ierr = 1
        ENDIF
        IF ( Fastcoef_sq(i)<0.0 ) THEN
          PRINT 9006, i, ' fastcoef_sq', Fastcoef_sq(i)
          ierr = 1
        ENDIF
        IF ( Soil2gw_max(i)>NEARZERO ) Soil2gw(i) = 1
        IF ( Model==0 ) THEN
          IF ( Soil_moist_max(i)>NEARZERO ) Replenish_frac(i) =
     +         Soil_rechr_max(i)/Soil_moist_max(i)
        ENDIF
        IF ( Hru_type(i)==3 ) THEN ! swales
          IF ( Pref_flow_den(i)>0.0 ) THEN
            IF ( Parameter_check_flag==1 ) THEN
              PRINT *, 'ERROR, pref_flow_den must be 0 for swale HRU:',
     +                 i, Pref_flow_den(i)
              ierr = 1
              CYCLE
            ENDIF
            IF ( Print_debug>-1 ) THEN
              PRINT *, 'Warning, pref_flow_den must be 0 for swale HRUs'
              PRINT *, 'reset from:', Pref_flow_den(i),
     +                 ' to 0.0 for HRU:', i
            ENDIF
          ENDIF
          Pref_flow_den(i) = 0.0
          Swale_limit(i) = 3.0*Sat_threshold(i)
        ENDIF
        IF ( Pref_flow_den(i)>NEARZERO ) THEN
          Pref_flow_flag(i) = 1
          Pref_flag = 1
        ENDIF
        Pref_flow_thrsh(i) = Sat_threshold(i)*(1.0-Pref_flow_den(i))
        Pref_flow_max(i) = Sat_threshold(i) - Pref_flow_thrsh(i)
        Soil_zone_max(i) = Sat_threshold(i) + Soil_moist_max(i)
      ENDDO

      IF ( Timestep==0 ) THEN
! initialize arrays (dimensioned Nhru)
        Dunnian_flow = 0.0
        IF ( Cascade_flag==1 ) THEN
          Upslope_interflow = 0.0
          Upslope_dunnianflow = 0.0
          Hru_sz_cascadeflow = 0.0
          IF ( Cascade_flag==1 .AND. Nlake>0 ) Lakein_sz = 0.0
        ENDIF
        Infil_tot = 0.0
        Pref_flow_stor = 0.0
        Pref_flow_infil = 0.0
        Pref_flow = 0.0
        Gvr2pfr = 0.0
        Swale_actet = 0.0
        Soil_moist_tot = 0.0
        Soil_moist_frac = 0.0
        Perv_actet = 0.0
        Recharge = 0.0
        Cap_waterin = 0.0
        Soil_lower = 0.0
        Potet_lower = 0.0
        Potet_rechr = 0.0
        Soil_lower_ratio = 0.0
        Soil_rechr_ratio = 0.0
        Snow_free = 0.0

! initialize arrays (dimensioned Nhrucell)
        IF ( Model==0 ) THEN
          Gvr2sm = 0.0
          Gw2sm_grav = 0.0
          Sm2gw_grav = 0.0
          Sm2gw_grav_old = 0.0
        ENDIF

! initialize scalers
        Basin_capillary_wb = 0.0D0
        Basin_gravity_wb = 0.0D0
        Basin_soilzone_wb = 0.0D0
        Basin_lakeinsz = 0.0D0
        Basin_soil_moist_tot = 0.0D0
        Basin_recharge = 0.0D0
        Basin_gvr2sm = 0.0D0
        Basin_gwin = 0.0D0
        Basin_ssin = 0.0D0
        Basin_sm2gvr = 0.0D0
        Basin_dninterflow = 0.0D0
        Basin_dndunnianflow = 0.0D0
        Basin_dncascadeflow = 0.0D0
        Basin_sz2gw = 0.0D0
        Basin_ssr2gw = 0.0D0
        Basin_sm2gvr_max = 0.0D0
        Basin_interflow_max = 0.0D0
        Basin_dunnian = 0.0D0
        Basin_capwaterin = 0.0D0
        Basin_cap_infil_max = 0.0D0
        Basin_cap_up_max = 0.0D0
        Basin_pref_flow_in = 0.0D0
        Basin_dunnian_pfr = 0.0D0
        Basin_dunnian_gvr = 0.0D0
        Basin_gvr2pfr = 0.0D0
        Basin_slowflow = 0.0D0
        Basin_prefflow = 0.0D0
        Basin_lakeprecip = 0.0D0
        Basin_szfarflow = 0.0D0
        Basin_slstor = 0.0D0
! do only once so restart uses saved values
        Soil_rechr = Soil_rechr_init
        Soil_moist = Soil_moist_init
        Ssres_stor = Ssstor_init
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Soil_rechr_init(i)>Soil_rechr_max(i) ) THEN
            IF ( Parameter_check_flag== 1 ) THEN
              PRINT 9003, i, Soil_rechr(i), Soil_rechr_max(i)
              ierr = 1
            ELSE
              IF ( Print_debug>-1 ) PRINT 9013, i, Soil_rechr(i),
     +                                          Soil_rechr_max(i)
              Soil_rechr(i) = Soil_rechr_max(i)
            ENDIF
          ENDIF
          IF ( Soil_moist_init(i)>Soil_moist_max(i) ) THEN
            IF ( Parameter_check_flag==1 ) THEN
              PRINT 9004, i, Soil_moist(i), Soil_moist_max(i)
              ierr = 1
            ELSE
              IF ( Print_debug>-1 ) PRINT 9014, i, Soil_moist(i),
     +                                          Soil_moist_max(i)
              Soil_moist(i) = Soil_moist_max(i)
            ENDIF
          ENDIF
          IF ( Soil_rechr(i)>Soil_moist(i) ) THEN
            IF ( Parameter_check_flag==1 ) THEN
              PRINT 9005, i, Soil_rechr(i), Soil_moist(i)
              ierr = 1
            ELSE
              IF ( Print_debug>-1 ) PRINT 9015, i, Soil_rechr(i),
     +                                          Soil_moist(i)
              Soil_rechr(i) = Soil_moist(i)
            ENDIF
          ENDIF
          IF ( Ssres_stor(i)>Sat_threshold(i) ) THEN
            IF ( Parameter_check_flag==1 ) THEN
              PRINT *, 'ERROR, HRU:', i, Ssres_stor(i),Sat_threshold(i),
     +                 ' ssres_stor > sat_threshold'
              ierr = 1
            ELSE
              PRINT *, 'Warning, HRU:', i, Ssres_stor(i),
     +                 Sat_threshold(i),
     +              ' ssres_stor > sat_threshold, ssres_stor set to max'
              Ssres_stor(i) = Sat_threshold(i)
            ENDIF
          ENDIF
          Slow_stor(i) = MIN( Ssres_stor(i), Pref_flow_thrsh(i) )
        ENDDO
      ELSE
        IF ( Print_debug>-1 ) THEN
          PRINT *, 'Warning, using restart values, thus parameters:'
          PRINT *, '   soil_rechr_init, soil_moist_init, ssstor_init'
          PRINT *, '   soil_rechr_max, soil_moist_max, & sat_threshold'
          PRINT *, '   values are not compared to corresponding states'
          PRINT *, '   in soilzone initialize procedure'
        ENDIF
      ENDIF

! initialize arrays (dimensioned Nhrucell)
      IF ( Model==0 ) THEN
        Hru_gvr_count = 0
        Hrucheck = 1 !if land or swale
        DO i = 1, Nhru
          IF ( Hru_type(i)==0 .OR. Hru_type(i)==2 ) Hrucheck(i) = 0
        ENDDO
        max_gvrs = 0
        DO i = 1, Nhrucell
          ii = Gvr_hru_id(i)
          IF ( ii>Nhru ) THEN
            PRINT *, 'ERROR, gvr_hru_id greater than number of HRUs'
            PRINT *, 'gvr_hru_id:', ii, ', nhru=',Nhru,', gvr number:',i
            ierr = 1
            CYCLE
          ENDIF
          ! set only for cold start simulations
          IF ( Timestep==0 ) Gravity_stor_res(i) = Ssstor_init(ii)
          Hru_gvr_count(ii) = Hru_gvr_count(ii) + 1
          IF ( Hru_gvr_count(ii)>max_gvrs ) max_gvrs = Hru_gvr_count(ii)
        ENDDO
        ALLOCATE ( Hru_gvr_index(max_gvrs, Nhru) )
        DO i = 1, Nhru
          icnt = 0
          ihru = Gvr_hru_id(i)
          IF ( Hrucheck(ihru)==0 ) CYCLE
          DO ii = 1, Nhrucell
            IF ( Gvr_hru_id(ii)==i ) THEN
              icnt = icnt + 1
              Hru_gvr_index(icnt, i) = ii
              IF ( icnt==Hru_gvr_count(i) ) EXIT
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      IF ( ierr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF

      Basin_soil_moist = 0.0D0
      Basin_slstor = 0.0D0
      Basin_ssstor = 0.0D0
      Basin_pref_stor = 0.0D0
      Basin_soil_rechr = 0.0D0
      Basin_soil_moist_tot = 0.0D0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Hru_type(i)==2 ) CYCLE
        Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*Hru_perv(i)
        IF ( Pref_flow_flag(i)==1 ) THEN
          Pref_flow_stor(i) = Ssres_stor(i) - Slow_stor(i)
          Basin_pref_stor = Basin_pref_stor
     +                      + Pref_flow_stor(i)*Hru_area(i)
        ENDIF
        Soil_moist_tot(i) = Ssres_stor(i)
     +                        + Soil_moist(i)*Hru_frac_perv(i)
        Basin_soil_moist_tot = Basin_soil_moist_tot
     +                        + Soil_moist_tot(i)*Hru_area(i)
        Basin_slstor = Basin_slstor + Slow_stor(i)*Hru_area(i)
        Basin_ssstor = Basin_ssstor + Ssres_stor(i)*Hru_area(i)
        Basin_soil_rechr = Basin_soil_rechr
     +                     + Soil_rechr(i)*Hru_perv(i)
      ENDDO
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      IF ( Pref_flag==1 ) Basin_pref_stor =
     +      Basin_pref_stor*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_soil_moist_tot = Basin_soil_moist_tot*Basin_area_inv

      DEALLOCATE ( Ssstor_init, Soil_moist_init, Soil_rechr_init )

 9001 FORMAT ('    Date     Water Bal     bsmbal    last SM  soilmoist',
     +        '  last stor    SS stor    perv ET      sz2gw  interflow',
     +        '    soil2gw    Dunnian    soil in   lakeinsz   downflow',
     +        '    farflow   swale ET  pref flow   pfr dunn   gvr dunn',
     +        '  lake evap')
 9002 FORMAT (/, 'ERROR, HRU:', I6, F10.4, F8.4,
     +        ' soil_rechr_max > soil_moist_max')
 9003 FORMAT (/, 'ERROR, HRU:', I6, F10.4, F8.4,
     +        ' soil_rechr_init > soil_rechr_max')
 9004 FORMAT (/, 'ERROR, HRU:', I6, F10.4, F8.4,
     +        ' soil_moist_init > soil_moist_max')
 9005 FORMAT (/, 'ERROR, HRU:', I6, F10.4, F8.4,
     +        ' soil_rechr > soil_moist based on init and max values')
 9006 FORMAT (/, 'ERROR, HRU:', I6, A, ' value < 0.0', F10.4)
 9008 FORMAT (/, 'ERROR, HRU:', I6, A, ' value invalid:', I6)
 9012 FORMAT ('Warning, HRU:', I6, F10.4, F8.4,
     +        '  soil_rechr_max > soil_moist_max,', /, 29X,
     +        'soil_rechr_max set to soil_moist_max')
 9013 FORMAT ('Warning, HRU:', I6, F10.4, F8.4,
     +        '  soil_rechr_init > soil_rechr_max,', /, 29X,
     +        'soil_rechr set to soil_rechr_max')
 9014 FORMAT ('Warning, HRU:', I6, F10.4, F8.4,
     +        '  soil_moist_init > soil_moist_max,', /, 29X,
     +        'soil_moist set to soil_moist_max')
 9015 FORMAT ('Warning, HRU:', I6, F10.4, F8.4,
     +        '  soil_rechr_init > soil_moist_init,', /, 29X,
     +      'soil_rechr set to soil_moist based on init and max values')
 9009 FORMAT ('    Date     Water Bal     bsmbal    last SM  soilmoist',
     +        '  last stor    SS stor    perv ET      sz2gw  interflow',
     +        '    soil2gw    Dunnian    soil in   lakeinsz   downflow',
     +        '    farflow   swale ET  pref flow   pfr dunn   gvr dunn',
     +        '  lake evap     gvr2sm       gwin  iteration')

      END FUNCTION szinit

!***********************************************************************
!     szrun - Does soil water balance for each HRU, adds in infiltration
!             then computes actual et and apportions remainder between
!             recharge of soil moisture, soil storage available for
!             interflow, excess routed to stream,
!             and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION szrun()
      USE PRMS_SOILZONE
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Dprst_flag, Print_debug,
     +    Nsegment, Model, Nlake, Nhrucell, Kkiter
      USE PRMS_BASIN, ONLY: Hru_type, Hru_perv, Hru_frac_perv,
     +    Hru_route_order, Active_hrus, Basin_area_inv, Hru_area,
     +    NEARZERO, Lake_hru_id, Cov_type
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Transp_on, Potet, Basin_potet
! WARNING!!! Sroff, Basin_sroff, and Strm_seg_in can be updated
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_actet, Hru_actet,
     +    Ssres_flow, Soil_to_gw, Basin_soil_to_gw, Ssr_to_gw,
     +    Soil_to_ssr, Basin_lakeevap, Basin_perv_et, Basin_swale_et,
     +    Hru_impervevap, Sroff, Basin_sroff, Soil_moist_max, Infil,
     +    Soil_rechr_max, Strm_seg_in, Strm_farfield, Ssres_in,
     +    Basin_soil_moist, Basin_ssstor, Slow_stor, Slow_flow,
     +    Ssres_stor, Soil_moist, Basin_infil, Sat_threshold,
     +    Hru_intcpevap, Soil_rechr, Dprst_evap_hru
      USE PRMS_CASCADE, ONLY: Ncascade_hru, Cascade_flag
      USE PRMS_OBS, ONLY: Nowtime, Nowyear, Nowmonth, Nowday
      USE PRMS_SNOW, ONLY: Snowcov_area, Snow_evap
      IMPLICIT NONE
! Functions
      INTRINSIC MIN, ABS, MAX
      INTEGER, EXTERNAL :: getvar
      EXTERNAL compute_soilmoist, compute_szactet, compute_cascades
      EXTERNAL compute_interflow, comp_inter_gwflow, read_error
      EXTERNAL compute_gravflow
! Local Variables
      INTEGER :: i, k, ncasc, ihru
      DOUBLE PRECISION :: bsmbal, waterin, gwin, basin_dunnian_pfr_only
      DOUBLE PRECISION :: soilbal, basin_bal, test, soil_in, gvrbal
      REAL :: dunnianflw, last_sm, interflow, last_ss, perv_area, harea
      REAL :: dnslowflow, dnpreflow, dndunn, availh2o, avail_potet
      REAL :: farflow_slow, farflow_pref, farflow_dunn, tmp, gvr_maxin
      REAL :: dunnianflw_pfr, dunnianflw_gvr, pref_flow_maxin, frac
      REAL :: hru_cascadeflow, perv_frac, capacity, capwater_maxin
      REAL :: cap_upflow_max, extra_water, pref_flow_in, gvr2sm_val
!***********************************************************************
      szrun = 0

      IF ( Model==0 ) THEN
        ! get gvr pct now, as prms2mf init called after soilzone init
        IF ( First_run==1 ) THEN
          IF ( getvar(MODNAME, 'gvr_hru_pct_adjusted', Nhrucell,
     +        'double', Gvr_hru_pct_adjusted)/=0 )
     +        CALL read_error(4, 'gvr_hru_pct_adjusted')
          First_run = 0
        ENDIF
        IF ( Kkiter==1 ) THEN
! It0 variables used with MODFLOW integration to save iteration states.
          DO k = 1, Active_hrus
            i = Hru_route_order(k)
            It0_soil_rechr(i) = Soil_rechr(i)
            It0_soil_moist(i) = Soil_moist(i)
            It0_pref_flow_stor(i) = Pref_flow_stor(i)
            It0_slow_stor(i) = Slow_stor(i)
            It0_ssres_stor(i) = Ssres_stor(i)
            It0_sroff(i) = Sroff(i)
            It0_potet(i) = Potet(i)
          ENDDO
          DO i = 1, Nhrucell
            ihru = Gvr_hru_id(i)
            IF ( Hrucheck(ihru)==0 ) CYCLE
            It0_gravity_stor_res(i) = Gravity_stor_res(i)
            Sm2gw_grav(i) = 0.0
            Sm2gw_grav_old(i) = 0.0
            Gw2sm_grav(i) = 0.0
          ENDDO
          DO i = 1, Nsegment
            It0_strm_seg_in(i) = Strm_seg_in(i)
          ENDDO
          It0_strm_farfield = Strm_farfield
          It0_basin_soil_moist = Basin_soil_moist
          It0_basin_ssstor = Basin_ssstor
        ELSE
          DO k = 1, Active_hrus
            i = Hru_route_order(k)
            Soil_rechr(i) = It0_soil_rechr(i)
            Soil_moist(i) = It0_soil_moist(i)
            Ssres_stor(i) = It0_ssres_stor(i)
            Pref_flow_stor(i) = It0_pref_flow_stor(i)
            Slow_stor(i) = It0_slow_stor(i)
            Sroff(i) = It0_sroff(i)
            Potet(i) = It0_potet(i)
          ENDDO
          Basin_soil_moist = It0_basin_soil_moist
          Basin_ssstor = It0_basin_ssstor
          DO i = 1, Nhrucell
            IF ( Hrucheck(Gvr_hru_id(i))==0 ) CYCLE
            Gravity_stor_res(i) = It0_gravity_stor_res(i)
            Sm2gw_grav_old(i) = Sm2gw_grav(i)
            Sm2gw_grav(i) = 0.0
          ENDDO
          DO i = 1, Nsegment
            Strm_seg_in(i) = It0_strm_seg_in(i)
          ENDDO
          Strm_farfield = It0_strm_farfield
! use value from last iteration for each time step instead of 0.0
          IF ( getvar(MODNAME, 'gw2sm_grav', Nhrucell, 'real',
     +         Gw2sm_grav)/=0 ) CALL read_error(4, 'gw2sm_grav')
        ENDIF
      ENDIF

      Last_soil_moist = Basin_soil_moist
      Last_ssstor = Basin_ssstor

      IF ( Cascade_flag==1 ) THEN
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Upslope_interflow(i) = 0.0
          Upslope_dunnianflow(i) = 0.0
        ENDDO
        IF ( Nlake>0 ) THEN
          Lakein_sz = 0.0
          Basin_lakeinsz = 0.0D0
        ENDIF
      ENDIF

      Basin_capillary_wb = 0.0D0
      Basin_gravity_wb = 0.0D0
      Basin_soilzone_wb = 0.0D0
      basin_dunnian_pfr_only = 0.0D0
      Basin_actet = 0.0D0
      Basin_soil_moist = 0.0D0
      Basin_soil_rechr = 0.0D0
      Basin_perv_et = 0.0D0
      Basin_swale_et = 0.0D0
      Basin_lakeevap = 0.0D0
      Basin_soil_to_gw = 0.0D0
      Basin_lakeprecip = 0.0D0
      Basin_sz2gw = 0.0D0
      Basin_ssflow = 0.0D0
      Basin_ssstor = 0.0D0
      Basin_ssin = 0.0D0
      Basin_ssr2gw = 0.0D0
      Basin_sm2gvr_max = 0.0D0
      Basin_interflow_max = 0.0D0
      Basin_sm2gvr = 0.0D0
      Basin_pref_stor = 0.0D0
      Basin_sroff = 0.0D0
      Basin_dunnian = 0.0D0
      basin_bal = 0.0D0
      soil_in = 0.0D0
      Basin_capwaterin = 0.0D0
      Basin_cap_infil_max = 0.0D0
      Basin_cap_up_max = 0.0D0
      Basin_pref_flow_in = 0.0D0
      Basin_dunnian_pfr = 0.0D0
      Basin_dunnian_gvr = 0.0D0
      Basin_dninterflow = 0.0D0
      Basin_dndunnianflow = 0.0D0
      Basin_dncascadeflow = 0.0D0
      Basin_gvr2pfr = 0.0D0
      Basin_slowflow = 0.0D0
      Basin_prefflow = 0.0D0
      Basin_szfarflow = 0.0D0
      Basin_recharge = 0.0D0
      hru_cascadeflow = 0.0
      Basin_gvr2sm = 0.0D0
      Basin_gwin = 0.0D0
      Basin_potet = 0.0D0
      Basin_soil_moist_tot = 0.0D0
      gwin = 0.0D0
      ncasc = 0
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        harea = Hru_area(i)
        perv_area = Hru_perv(i)
        perv_frac = Hru_frac_perv(i)

        ! Soil_to_gw for whole HRU
        ! Soil_to_ssr for whole HRU
        Soil_to_gw(i) = 0.0
        Soil_to_ssr(i) = 0.0
        Perv_actet(i) = 0.0
        Ssr_to_gw(i) = 0.0
        Slow_flow(i) = 0.0
        Ssres_flow(i) = 0.0
        Pref_flow(i) = 0.0
        Gvr2pfr(i) = 0.0
        Ssres_in(i) = 0.0
        Recharge(i) = 0.0
        Hru_actet(i) = Hru_impervevap(i) + Hru_intcpevap(i) +
     +                 Snow_evap(i)
        IF ( Dprst_flag==1 ) Hru_actet(i) = Hru_actet(i) +
     +                                      Dprst_evap_hru(i)
        avail_potet = (Potet(i) - Hru_actet(i))/perv_frac
        IF ( avail_potet<0.0 ) avail_potet = 0.0

        !Hru_type can be 1 (land) or 3 (swale)
        IF ( Hru_type(i)/=2 ) THEN

!******Add infiltration to soil and compute excess
          ! note, perv_area has to be > 0.0
          last_sm = Soil_moist(i)
          last_ss = Ssres_stor(i)
          hru_cascadeflow = 0.0
          dnslowflow = 0.0
          dnpreflow = 0.0
          dndunn = 0.0
          farflow_slow = 0.0
          farflow_pref = 0.0
          farflow_dunn = 0.0
          dunnianflw = 0.0
          dunnianflw_pfr = 0.0
          dunnianflw_gvr = 0.0
          interflow = 0.0
          Pref_flow_infil(i) = 0.0
          Cap_waterin(i) = 0.0
          Infil_tot(i) = 0.0
          pref_flow_maxin = 0.0

!******Add infiltration to soil and compute excess
          !infil_tot is the depth in whole HRU
          !infil is for pervious area
          !capillary reservoir for pervious area
          !preferential flow reservoir for whole HRU
          !gravity reservoir for whole HRU
          !upslope flow for whole HRU

!******if cascading flow available from upslope cascades
!****** add soil excess (Dunnian flow) to infiltration
          ! perv_frac has to be > 0.001
          ! infil for pervious portion of HRU
          capwater_maxin = Infil(i)
          ! compute preferential flow and storage, and any dunnian flow
          IF ( capwater_maxin>0.0 ) THEN
            IF ( Pref_flow_flag(i)==1 ) THEN
              ! pref_flow for whole HRU
              pref_flow_maxin = Infil(i)*Pref_flow_den(i)
              capwater_maxin = capwater_maxin - pref_flow_maxin
              pref_flow_maxin = pref_flow_maxin*perv_frac
              ! compute contribution to preferential-flow reservoir storage
              Pref_flow_stor(i) = Pref_flow_stor(i) + pref_flow_maxin
              dunnianflw_pfr = MAX( 0.0,
     +                              Pref_flow_stor(i)-Pref_flow_max(i) )
              IF ( dunnianflw_pfr>0.0 ) THEN
                Basin_dunnian_pfr = Basin_dunnian_pfr +
     +                              dunnianflw_pfr*harea
                Pref_flow_stor(i) = Pref_flow_max(i)
              ENDIF
              Pref_flow_infil(i) = pref_flow_maxin - dunnianflw_pfr
              Basin_pref_flow_in = Basin_pref_flow_in +
     +                             Pref_flow_infil(i)*harea
            ENDIF
            Basin_cap_infil_max = Basin_cap_infil_max
     +                            + capwater_maxin*perv_area
          ENDIF

          IF ( Cascade_flag==1 ) THEN
            cap_upflow_max = Upslope_dunnianflow(i)+Upslope_interflow(i)
            capwater_maxin = capwater_maxin + cap_upflow_max/perv_frac
            Basin_cap_up_max = Basin_cap_up_max + cap_upflow_max*harea
          ENDIF

!******Add infiltration to soil and compute excess
          gvr_maxin = 0.0
          IF ( capwater_maxin>0.0 ) THEN
            Cap_waterin(i) = capwater_maxin
            CALL compute_soilmoist(Cap_waterin(i), Soil_moist_max(i),
     +           Soil_rechr_max(i), Soil2gw_max(i), gvr_maxin,
     +           Soil_moist(i), Soil_rechr(i), Soil_to_gw(i),
     +           Soil2gw(i), perv_frac)
            Infil_tot(i) = Cap_waterin(i)*perv_frac
            Basin_capwaterin = Basin_capwaterin + Infil_tot(i)*harea
            Basin_soil_to_gw = Basin_soil_to_gw + Soil_to_gw(i)*harea
            Basin_sm2gvr_max = Basin_sm2gvr_max + gvr_maxin*harea
          ENDIF

! compute slow_flow and ssr_to_gw
          IF ( Model==0 ) THEN
            capacity = Soil_moist_max(i) - Soil_moist(i)
            CALL compute_gravflow(i, capacity, Slowcoef_lin(i),
     +           Slowcoef_sq(i), Ssr2gw_rate(i), Ssr2gw_exp(i),
     +           gvr_maxin, Pref_flow_thrsh(i), Soil_to_ssr(i),
     +           Gvr2pfr(i), Ssr_to_gw(i), Slow_flow(i), Slow_stor(i),
     +           Gvr2sm(i), Soil_to_gw(i), gwin, perv_frac, Hru_type(i))
            ! adjust soil moisture with replenish amount
            IF ( Gvr2sm(i)>0.0 ) THEN
              Soil_moist(i) = Soil_moist(i) + Gvr2sm(i)/perv_frac
!              IF ( Soil_moist(i)>Soil_moist_max(i) )
!     +             PRINT *, 'sm>max', Soil_moist(i), Soil_moist_max(i),i
              Soil_rechr(i) = Soil_rechr(i) +
     +                        Gvr2sm(i)/perv_frac*Replenish_frac(i)
              Soil_rechr(i) = MIN( Soil_rechr_max(i), Soil_rechr(i) )
              Basin_gvr2sm = Basin_gvr2sm + Gvr2sm(i)*harea
            ELSEIF ( Gvr2sm(i)<0.0 ) THEN
              PRINT *, 'negative gvr2sm, HRU:', i, Gvr2sm(i)
            ENDIF
            Basin_gwin = Basin_gwin + gwin*harea
          ELSE
            Soil_to_ssr(i) = gvr_maxin
            Slow_stor(i) = Slow_stor(i) + gvr_maxin
            IF ( Slow_stor(i)>0.0 )
     +           CALL comp_inter_gwflow(Slowcoef_lin(i), Slowcoef_sq(i),
     +                Ssr2gw_rate(i), Ssr2gw_exp(i), Pref_flow_thrsh(i),
     +                Gvr2pfr(i), Ssr_to_gw(i), Slow_flow(i),
     +                Slow_stor(i), Hru_type(i), Soil_to_ssr(i))
          ENDIF

          ! compute contribution to Dunnian flow from PFR, if any
          Pref_flow_stor(i) = Pref_flow_stor(i) + Gvr2pfr(i)
          extra_water = MAX( 0.0, Pref_flow_stor(i)-Pref_flow_max(i) )
          IF ( extra_water>0.0 ) THEN
            Gvr2pfr(i) = Gvr2pfr(i) - extra_water
            IF ( Gvr2pfr(i)<-NEARZERO ) THEN
!              PRINT *, 'gvr2pfr<0', Gvr2pfr(i), extra_water,
!     +                 pref_flow_max(i), pref_flow_stor(i), gvr_maxin
              Gvr2pfr(i) = 0.0
            ENDIF
            Pref_flow_stor(i) = Pref_flow_max(i)
            dunnianflw_gvr = extra_water
          ENDIF

          ! compute preferential flow (fast interflow), if any
          IF ( Pref_flow_flag(i)==1 ) THEN
            pref_flow_in = Pref_flow_infil(i) + Gvr2pfr(i)
            IF ( Pref_flow_stor(i)>0.0 )
     +           CALL compute_interflow(Fastcoef_lin(i), Fastcoef_sq(i),
     +                pref_flow_in, Pref_flow_stor(i), Pref_flow(i))
          ELSE
            IF ( Gvr2pfr(i)>0.0 ) print *, 'gvr2pfr>0', Gvr2pfr(i)
            Gvr2pfr(i) = 0.0
          ENDIF

          Basin_sm2gvr = Basin_sm2gvr + Soil_to_ssr(i)*harea
          Basin_dunnian_gvr = Basin_dunnian_gvr + dunnianflw_gvr*harea
          Basin_sz2gw = Basin_sz2gw + Ssr_to_gw(i)*harea
          Recharge(i) = Soil_to_gw(i) + Ssr_to_gw(i)
          Basin_recharge = Basin_recharge + Recharge(i)*harea

!******Compute actual evapotranspiration
          Snow_free(i) = 0.0
          IF ( Soil_moist(i)>0.0 ) THEN
            CALL compute_szactet(Soil_moist_max(i), Soil_rechr_max(i),
     +           Snowcov_area(i), Transp_on(i), Cov_type(i),
     +           Soil_type(i), Soil_moist(i), Soil_rechr(i),
     +           Perv_actet(i), avail_potet, Snow_free(i),
     +           Potet_rechr(i), Potet_lower(i), Soil_rechr_ratio(i),
     +           Soil_lower_ratio(i))
            ! sanity check
            IF ( Perv_actet(i)>avail_potet ) THEN
              Soil_moist(i) = Soil_moist(i) + Perv_actet(i) -avail_potet
              Perv_actet(i) = avail_potet
!              PRINT *, 'perv_et problem', Perv_actet, Avail_potet
            ENDIF
          ENDIF

          ! sanity check
          IF ( Soil_moist(i)<0.0 ) THEN
            IF ( Print_debug>-1 ) PRINT *, i, Soil_moist(i), ' negative'
            IF ( Perv_actet(i)>=ABS(Soil_moist(i)) ) THEN
              Perv_actet(i) = Perv_actet(i) + Soil_moist(i)
              Soil_moist(i) = 0.0
            ENDIF
            IF ( Soil_moist(i)<-NEARZERO ) THEN
              IF ( Print_debug>-1 ) PRINT *, 'HRU:', i,
     +             ' soil_moist<0.0', Soil_moist(i)
            ENDIF
            Soil_moist(i) = 0.0
          ENDIF

          Hru_actet(i) = Hru_actet(i) + Perv_actet(i)*perv_frac
          avail_potet = Potet(i) - Hru_actet(i)
          ! sanity check
          IF ( avail_potet<0.0 ) THEN
            IF ( Print_debug>-1 ) THEN
              IF ( avail_potet<-NEARZERO ) PRINT *, 'hru_actet>potet',i,
     +             Nowmonth, Nowday, Hru_actet(i), Potet(i), avail_potet
            ENDIF
            Hru_actet(i) = Potet(i)
            tmp = avail_potet/perv_frac
            Perv_actet(i) = Perv_actet(i) + tmp
            Soil_moist(i) = Soil_moist(i) - tmp
            Soil_rechr(i) = Soil_rechr(i) - tmp
            IF ( Soil_rechr(i)<0.0 ) Soil_rechr(i) = 0.0
          ENDIF
! soil_moist & soil_rechr multiplied by perv_area instead of harea
          Soil_lower(i) = Soil_moist(i) - Soil_rechr(i)
          Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*perv_area
          Basin_soil_rechr = Basin_soil_rechr + Soil_rechr(i)*perv_area
          Basin_perv_et = Basin_perv_et + Perv_actet(i)*perv_area

! if HRU cascades,
! compute interflow and excess flow to each HRU or stream
          IF ( Hru_type(i)==1 ) THEN
            interflow = Slow_flow(i) + Pref_flow(i)
            Basin_interflow_max = Basin_interflow_max + interflow*harea
            dunnianflw = dunnianflw_gvr + dunnianflw_pfr
            frac = 0.0
            IF ( dunnianflw>0.0 ) frac = dunnianflw_pfr/dunnianflw
            Dunnian_flow(i) = dunnianflw
            IF ( Cascade_flag==1 ) THEN
              ncasc = Ncascade_hru(i)
              IF ( ncasc>0 ) THEN
                IF ( interflow+dunnianflw>0.0 ) THEN
                  CALL compute_cascades(i, ncasc, Slow_flow(i),
     +                 Pref_flow(i), Dunnian_flow(i), dnslowflow,
     +                 dnpreflow, dndunn, farflow_slow, farflow_pref,
     +                 farflow_dunn)
                  Hru_sz_cascadeflow(i) = dnslowflow + dnpreflow +
     +                                    dndunn + farflow_slow +
     +                                    farflow_pref + farflow_dunn
                  hru_cascadeflow = Hru_sz_cascadeflow(i)
                  Basin_dninterflow = Basin_dninterflow +
     +                                (dnslowflow+dnpreflow)*harea
                  Basin_dndunnianflow = Basin_dndunnianflow +
     +                                  dndunn*harea
                  Basin_szfarflow = Basin_szfarflow +
     +                    (farflow_slow+farflow_pref+farflow_dunn)*harea
                  Basin_dncascadeflow = Basin_dncascadeflow +
     +                               (dnslowflow+dnpreflow+dndunn)*harea
     +                  + (farflow_slow+farflow_pref+farflow_dunn)*harea
                ELSE
                  Hru_sz_cascadeflow(i) = 0.0
                ENDIF
                basin_dunnian_pfr_only = basin_dunnian_pfr_only +
     +                                   dunnian_flow(i)*frac
              ENDIF
            ENDIF

! note ssres_flow does not include farfield flow as it leaves the stream network
!      and model domain, thus not included in basin_cfs
            Ssres_flow(i) = Slow_flow(i)
            IF ( Pref_flow_flag(i)==1 ) THEN
              Ssres_flow(i) = Ssres_flow(i) + Pref_flow(i)
              Basin_prefflow = Basin_prefflow + Pref_flow(i)*harea
              Basin_gvr2pfr = Basin_gvr2pfr + Gvr2pfr(i)*harea
              Basin_pref_stor = Basin_pref_stor +
     +                          Pref_flow_stor(i)*harea
            ENDIF
            Basin_ssflow = Basin_ssflow + Ssres_flow(i)*harea
            Basin_slowflow = Basin_slowflow + Slow_flow(i)*harea

! treat dunnianflw as surface runoff to streams
! note sroff does not include farfield flow as it leaves the stream network
!      and model domain, thus not included in basin_cfs
            Sroff(i) = Sroff(i) + Dunnian_flow(i)
            Basin_sroff = Basin_sroff + Sroff(i)*harea
            Basin_dunnian = Basin_dunnian + Dunnian_flow(i)*harea
            Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)
          ELSE ! for swales
            availh2o = Slow_stor(i) - Sat_threshold(i)
            Swale_actet(i) = 0.0
            IF ( availh2o>0.0 ) THEN
              Swale_actet(i) = Potet(i) - Hru_actet(i)
              IF ( Swale_actet(i)>0.0 ) THEN
                IF ( Swale_actet(i)>availh2o ) Swale_actet(i) = availh2o
                Hru_actet(i) = Hru_actet(i) + Swale_actet(i)
                Slow_stor(i) = Slow_stor(i) - Swale_actet(i)
              ENDIF
              IF ( Print_debug==7 ) THEN
                IF ( Slow_stor(i)>Swale_limit(i) ) THEN
                  WRITE ( DBGUNT, * ) 'Swale ponding, HRU:', i,
     +                    ' gravity reservoir is 3*sat_threshold',
     +                    Slow_stor(i), Sat_threshold(i), Nowtime
                ENDIF
              ENDIF
            ENDIF
            Basin_swale_et = Basin_swale_et + Swale_actet(i)*harea
            Ssres_stor(i) = Slow_stor(i)
          ENDIF
          Ssres_in(i) = Soil_to_ssr(i) + Pref_flow_infil(i) + gwin
          Basin_ssin = Basin_ssin + Ssres_in(i)*harea
          Basin_ssstor = Basin_ssstor + Ssres_stor(i)*harea
          Basin_slstor = Basin_slstor + Slow_stor(i)*harea
          Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*perv_frac
          Basin_soil_moist_tot = Basin_soil_moist_tot
     +                           + Soil_moist_tot(i)*harea
          Soil_moist_frac(i) = Soil_moist_tot(i)/Soil_zone_max(i)

          IF ( Print_debug==1 ) THEN
            gvr2sm_val = 0.0
            IF ( Model==0 ) gvr2sm_val = Gvr2sm(i)
            soilbal = (capwater_maxin + last_sm-Soil_moist(i)-
     +                Perv_actet(i))*perv_frac - gvr_maxin
     +                - Soil_to_gw(i) + gvr2sm_val
            IF ( ABS(soilbal)>TOOSMALL ) THEN
              WRITE ( BALUNT, * ) 'HRU capillary problem'
              WRITE ( BALUNT, * ) soilbal, Infil_tot(i), last_sm,
     +                Soil_moist(i), Perv_actet(i), Soil_to_ssr(i),
     +                Soil_to_gw(i), i, Infil(i), Pref_flow_infil(i),
     +                perv_frac, capwater_maxin-Infil_tot(i),
     +                capwater_maxin, gvr2sm_val, Soil_moist_max(i),
     +                Cap_waterin(i), gvr_maxin
              IF ( Cascade_flag==1 ) WRITE ( BALUNT, * ) 'UP',
     +             Upslope_interflow(i), Upslope_dunnianflow(i)
            ENDIF
            gvrbal = last_ss - Ssres_stor(i) + gvr_maxin -
     +               Ssr_to_gw(i) - Swale_actet(i) - Dunnian_flow(i) -
     +               hru_cascadeflow - Ssres_flow(i) - gvr2sm_val +
     +               dunnianflw_pfr + gwin + Pref_flow_infil(i)
            test = ABS( gvrbal )
            IF ( test>TOOSMALL ) THEN
              WRITE ( BALUNT, * ) 'Bad GVR balance, HRU:', i,
     +                            ' hru_type:', Hru_type(i)
              WRITE ( BALUNT, * ) gvrbal, last_ss, Ssres_stor(i),
     +                gvr_maxin, Ssr_to_gw(i), Swale_actet(i),
     +                Dunnian_flow(i), hru_cascadeflow, Ssres_flow(i),
     +                gvr2sm_val, dunnianflw_pfr, gwin,
     +                Pref_flow_infil(i), dunnianflw_gvr, dnslowflow,
     +                dnpreflow, farflow_slow, farflow_pref,
     +                farflow_dunn, Slow_flow(i), Pref_flow(i), dndunn,
     +                dunnianflw, Soil_to_ssr(i), interflow, Gvr2pfr(i),
     +                perv_frac, Slow_stor(i), Pref_flow_stor(i),
     +                Infil(i), Pref_flow_max(i), Pref_flow_den(i),
     +                Pref_flow_thrsh(i), Ssres_in(i)
            ENDIF

            waterin = capwater_maxin*perv_frac + Pref_flow_infil(i)
     =                + gwin + dunnianflw_pfr
            soil_in = soil_in + (Infil(i)*perv_frac + gwin)*harea
            soilbal = waterin + last_ss - Ssres_stor(i) +
     +                (last_sm-Soil_moist(i)-Perv_actet(i))*perv_frac -
     +                Ssr_to_gw(i) - interflow - dunnianflw -
     +                Soil_to_gw(i) - Swale_actet(i)
            basin_bal = basin_bal + soilbal*harea
            test = ABS( soilbal )
            IF ( test>1.0D-5 ) THEN
!              IF ( test>Ssres_stor(i)*TOOSMALL ) THEN
                WRITE ( BALUNT, * ) 'HRU:', i, ' Hru_type:', Hru_type(i)
                IF ( test>5.0D-3 ) THEN
                  WRITE ( BALUNT, * ) 'HRU water balance ***ERROR***'
                ELSEIF ( test>5.0D-4 ) THEN
                  WRITE ( BALUNT, * ) 'possible HRU water balance ERROR'
                ELSE
                  WRITE ( BALUNT, * )
     +                    'possible HRU water balance rounding issue'
                ENDIF
                WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, i,
     +                soilbal, Infil(i), last_sm, last_ss,
     +                Soil_moist(i), Ssres_stor(i), Perv_actet(i),
     +                Ssr_to_gw(i), interflow,Slow_flow(i),Pref_flow(i),
     +                dunnianflw, Soil_to_gw(i), Pref_flow_infil(i),
     +                Pref_flow_stor(i), Slow_stor(i), Soil_rechr(i),
     +                Soil_lower(i), Soil_to_ssr(i), Ssres_flow(i),
     +                waterin, Swale_actet(i), gwin, gvr2sm_val
                IF ( Cascade_flag==1 ) WRITE ( BALUNT, * ) 'upslope',
     +               Upslope_dunnianflow(i), Upslope_interflow(i)
                WRITE ( BALUNT, * ) perv_area, perv_frac, ncasc,
     +                              Pref_flow_den(i)
!              ENDIF
            ENDIF
          ENDIF

        ELSE ! else it is a lake or reservoir
          !WARNING, RSR, if hru_actet>water in lake, then budget error
          Hru_actet(i) = Potet(i) - Hru_actet(i)
          IF ( Lake_hru_id(i)>0 ) Hru_actet(i) = Hru_actet(i)
     +         *Lake_evap_adj(Nowmonth,Lake_hru_id(i))
          IF ( Hru_actet(i)>Potet(i) ) THEN
!            PRINT *, 'Warning, lake evap > potet, for HRU:', i,
!     +               ' potential ET increased to adjusted lake ET'
            Potet(i) = Hru_actet(i)! this could be a problem when it happens
          ENDIF
          Basin_lakeevap = Basin_lakeevap + Hru_actet(i)*harea
          Basin_lakeprecip = Basin_lakeprecip + Hru_ppt(i)*harea
          IF ( Cascade_flag==1 ) THEN
            ! if lake HRU doesn't cascade, should we limit ET to
            !  water entering the HRU to this point (no gwflow yet)
            Lakein_sz(i) = Upslope_interflow(i) + Upslope_dunnianflow(i)
            Basin_lakeinsz = Basin_lakeinsz + Lakein_sz(i)*harea
          ENDIF
        ENDIF

        Basin_actet = Basin_actet + Hru_actet(i)*harea
        Basin_potet = Basin_potet + Potet(i)*harea ! need as lakes can change potet

      ENDDO

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_swale_et = Basin_swale_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_potet = Basin_potet*Basin_area_inv
      Basin_soil_moist_tot = Basin_soil_moist_tot*Basin_area_inv
      IF ( Nlake>0 ) THEN
        Basin_lakeevap = Basin_lakeevap*Basin_area_inv
        Basin_lakeprecip = Basin_lakeprecip*Basin_area_inv
        Basin_lakeinsz = Basin_lakeinsz*Basin_area_inv
      ENDIF
      IF ( Pref_flag==1 ) THEN
        Basin_pref_stor = Basin_pref_stor*Basin_area_inv
        Basin_pref_flow_in = Basin_pref_flow_in*Basin_area_inv
        Basin_prefflow = Basin_prefflow*Basin_area_inv
        Basin_dunnian_pfr = Basin_dunnian_pfr*Basin_area_inv
        basin_dunnian_pfr_only = basin_dunnian_pfr_only*basin_area_inv
      ENDIF
      Basin_dunnian_gvr = Basin_dunnian_gvr*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_ssflow = Basin_ssflow*Basin_area_inv
      Basin_interflow_max = Basin_interflow_max*Basin_area_inv
      Basin_sz2gw = Basin_sz2gw*Basin_area_inv
      Basin_ssr2gw = Basin_sz2gw
      Basin_ssin = Basin_ssin*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_dunnian = Basin_dunnian*Basin_area_inv
      Basin_sm2gvr = Basin_sm2gvr*Basin_area_inv
      Basin_sm2gvr_max = Basin_sm2gvr_max*Basin_area_inv
      Basin_capwaterin = Basin_capwaterin*Basin_area_inv
      Basin_cap_infil_max = Basin_cap_infil_max*Basin_area_inv
      Basin_cap_up_max = Basin_cap_up_max*Basin_area_inv
      Basin_dninterflow = Basin_dninterflow*Basin_area_inv
      Basin_dndunnianflow = Basin_dndunnianflow*Basin_area_inv
      Basin_dncascadeflow = Basin_dncascadeflow*Basin_area_inv
      Basin_szfarflow = Basin_szfarflow*Basin_area_inv
      Basin_gvr2pfr = Basin_gvr2pfr*Basin_area_inv
      Basin_slowflow = Basin_slowflow*Basin_area_inv
      Basin_recharge = Basin_recharge*Basin_area_inv
      Basin_gvr2sm = Basin_gvr2sm*Basin_area_inv
      Basin_gwin = Basin_gwin*Basin_area_inv

      Basin_capillary_wb = Last_soil_moist - Basin_soil_moist -
     +     Basin_perv_et - Basin_sm2gvr_max + Basin_cap_infil_max +
     +     Basin_gvr2sm - Basin_soil_to_gw + Basin_cap_up_max
      Basin_gravity_wb = Last_ssstor - Basin_ssstor + Basin_sm2gvr_max -
     +     Basin_interflow_max - Basin_sz2gw - Basin_dunnian_gvr -
     +     Basin_swale_et - Basin_gvr2sm + Basin_pref_flow_in +
     +     Basin_gwin
      Basin_soilzone_wb = Basin_infil + Basin_gwin + Last_ssstor -
     +     Basin_ssstor + Last_soil_moist - Basin_soil_moist -
     +     Basin_perv_et - Basin_swale_et - Basin_sz2gw -
     +     Basin_soil_to_gw - Basin_ssflow - Basin_dunnian -
     +     Basin_szfarflow - Basin_lakeinsz

      IF ( Print_debug==1 ) THEN
        IF ( ABS(Basin_gravity_wb)>2.0D-5 ) WRITE ( BALUNT, * )
     +      'basin gvrbal issue', Basin_gravity_wb,
     +      Last_ssstor, Basin_ssstor, Basin_sm2gvr_max,
     +      Basin_interflow_max, Basin_sz2gw, Basin_dunnian_gvr,
     +      Basin_swale_et, Basin_gvr2sm, Basin_pref_flow_in,
     +      Basin_dninterflow, Basin_pref_stor, Basin_szfarflow,
     +      Basin_dunnian_pfr, Basin_lakeinsz, Basin_dncascadeflow,
     +      Kkiter
        IF ( ABS(Basin_capillary_wb)>TOOSMALL ) WRITE( BALUNT, * )
     +       'possible basin capillary balance issue',
     +       Basin_capillary_wb,
     +       Last_soil_moist, Basin_soil_moist, Basin_perv_et,
     +       Basin_sm2gvr_max, Basin_cap_infil_max, Basin_gvr2sm,
     +       Basin_soil_to_gw, Basin_cap_up_max, Basin_capwaterin,
     +       Basin_soil_to_gw
        IF ( ABS(Basin_soilzone_wb)>0.0005D0 )
     +    WRITE ( BALUNT, * ) 'possible basin soil zone rounding issue',
     +            Basin_soilzone_wb, Basin_capwaterin,
     +            Basin_pref_flow_in, Basin_infil, Basin_gwin,
     +            Last_ssstor, Basin_ssstor, Last_soil_moist,
     +            Basin_soil_moist, Basin_perv_et, Basin_swale_et,
     +            Basin_sz2gw, Basin_soil_to_gw, Basin_ssflow,
     +            Basin_dunnian, Basin_szfarflow, Basin_dncascadeflow,
     +            Basin_gvr2sm, Basin_sm2gvr, Basin_lakeinsz,
     +            Basin_interflow_max, basin_dunnian_pfr_only

        soil_in = soil_in*Basin_area_inv
        basin_bal = basin_bal*Basin_area_inv
        bsmbal = Last_soil_moist - Basin_soil_moist + Last_ssstor -
     +           Basin_ssstor - Basin_perv_et - Basin_sz2gw + soil_in -
     +           Basin_ssflow - Basin_soil_to_gw - Basin_dunnian -
     +           Basin_szfarflow - Basin_swale_et - Basin_lakeinsz

        IF ( Model==1 ) THEN
          WRITE ( BALUNT, 9002 ) Nowyear, Nowmonth, Nowday, basin_bal,
     +          bsmbal, Last_soil_moist, Basin_soil_moist, Last_ssstor,
     +          Basin_ssstor, Basin_perv_et, Basin_sz2gw, Basin_ssflow,
     +          Basin_soil_to_gw, Basin_dunnian, soil_in,Basin_lakeinsz,
     +          Basin_dncascadeflow, Basin_szfarflow, Basin_swale_et,
     +          Basin_prefflow, Basin_dunnian_pfr, Basin_dunnian_gvr,
     +          Basin_lakeevap
        ELSE
          WRITE ( BALUNT, 9002 ) Nowyear, Nowmonth, Nowday, basin_bal,
     +          bsmbal, Last_soil_moist, Basin_soil_moist, Last_ssstor,
     +          Basin_ssstor, Basin_perv_et, Basin_sz2gw, Basin_ssflow,
     +          Basin_soil_to_gw, Basin_dunnian, soil_in,Basin_lakeinsz,
     +          Basin_dncascadeflow, Basin_szfarflow, Basin_swale_et,
     +          Basin_prefflow, Basin_dunnian_pfr, Basin_dunnian_gvr,
     +          Basin_lakeevap, Basin_gvr2sm, Basin_gwin, Kkiter
        ENDIF

        IF ( ABS(bsmbal)>0.05D0 .OR. ABS(basin_bal)>0.001D0 ) THEN
          WRITE ( BALUNT, * ) '*ERROR, basin water balance', bsmbal,
     +            basin_bal, Last_soil_moist, Basin_soil_moist,
     +            Last_ssstor, Basin_ssstor, Basin_perv_et, Basin_sz2gw,
     +            soil_in, Basin_ssflow, Basin_soil_to_gw,Basin_dunnian,
     +            Basin_szfarflow, Basin_swale_et, Basin_lakeinsz
          WRITE ( BALUNT, * ) Basin_pref_stor, Basin_slstor
        ELSEIF ( ABS(bsmbal)>0.005D0 .OR. ABS(basin_bal)>TOOSMALL )
     +           THEN
          WRITE ( BALUNT, * ) 'Possible basin water balance ERROR',
     +            bsmbal, basin_bal, Last_soil_moist, Basin_soil_moist,
     +            Last_ssstor, Basin_ssstor, Basin_perv_et, Basin_sz2gw,
     +            soil_in, Basin_ssflow, Basin_soil_to_gw,Basin_dunnian,
     +            Basin_szfarflow, Basin_swale_et, Basin_lakeinsz
          WRITE ( BALUNT, * ) Basin_pref_stor, Basin_slstor
        ELSEIF ( ABS(bsmbal)>0.0005D0 .OR.
     +           ABS(basin_bal)>TOOSMALL ) THEN
          WRITE ( BALUNT, '(A,2F12.7)' ) 'Basin rounding issue', bsmbal,
     +                                   basin_bal
          WRITE ( BALUNT, * ) Basin_soilzone_wb, Basin_ssin,
     +            Basin_dninterflow, Basin_sm2gvr, Basin_capwaterin,
     +            soil_in, Basin_gvr2pfr, Basin_gvr2sm,
     +            Basin_dndunnianflow,
     +            soil_in - Basin_infil - Basin_gwin
        ENDIF
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), I5, 26F11.5)
 9002 FORMAT (I5, 2('/', I2.2), 22F11.5, I11)

      END FUNCTION szrun

!***********************************************************************
!     Add infiltration to soil and compute excess
!     Soil_to_gw for whole HRU
!     Soil_to_ssr for whole HRU
!***********************************************************************
      SUBROUTINE compute_soilmoist(Infil, Soil_moist_max,
     +           Soil_rechr_max, Soil2gw_max, Soil_to_ssr, Soil_moist,
     +           Soil_rechr, Soil_to_gw, Soil2gw, Perv_frac)
      IMPLICIT NONE
      INTRINSIC MIN
! Arguments
      INTEGER, INTENT(IN) :: Soil2gw
      REAL, INTENT(IN) :: Perv_frac
      REAL, INTENT(IN) :: Soil_moist_max, Soil_rechr_max, Soil2gw_max
      REAL, INTENT(INOUT) :: Infil, Soil_moist, Soil_rechr
      REAL, INTENT(INOUT) :: Soil_to_gw, Soil_to_ssr
! Local Variables
      REAL :: excs
!***********************************************************************
      Soil_rechr = MIN( (Soil_rechr+Infil), Soil_rechr_max )
      ! soil_moist_max from previous time step or soil_moist_max has
      ! changed for a restart simulation
      excs = Soil_moist + Infil
      Soil_moist = MIN( excs, Soil_moist_max )
      excs = (excs - Soil_moist_max)*Perv_frac
      IF ( excs>0.0 ) THEN
        Infil = Infil - excs/Perv_frac
        IF ( Soil2gw==1 ) Soil_to_gw = MIN(Soil2gw_max, excs)
        ! warning, Soil_to_gw set to zero in init, doesn't change if
        ! soil2gw isn't dynamic, if so need to set to zero
        Soil_to_ssr = excs - Soil_to_gw
        IF ( Soil_to_ssr<0.0 ) Soil_to_ssr = 0.0
      ENDIF

      END SUBROUTINE compute_soilmoist

!***********************************************************************
!     Compute actual evapotranspiration
!***********************************************************************
      SUBROUTINE compute_szactet(Soil_moist_max, Soil_rechr_max,
     +           Snowcov_area, Transp_on, Cov_type, Soil_type,
     +           Soil_moist, Soil_rechr, Perv_actet, Avail_potet,
     +           Snow_free, Potet_rechr, Potet_lower, Soil_rechr_ratio,
     +           Soil_lower_ratio)
      USE PRMS_SOILZONE, ONLY: Et_type
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Transp_on, Cov_type, Soil_type
      REAL, INTENT(IN) :: Soil_moist_max, Soil_rechr_max
      REAL, INTENT(IN) :: Snowcov_area
      REAL, INTENT(INOUT) :: Soil_moist, Soil_rechr
      REAL, INTENT(INOUT) :: Avail_potet, Snow_free
      REAL, INTENT(OUT) :: Perv_actet, Potet_rechr, Potet_lower
      REAL, INTENT(OUT) :: Soil_lower_ratio, Soil_rechr_ratio
! Local Variables
      REAL, PARAMETER :: ONETHIRD = 1.0/3.0, TWOTHIRDS = 2.0/3.0
      REAL :: et
!***********************************************************************
      Potet_lower = 0.0
      Potet_rechr = 0.0

      Snow_free = 1.0 - Snowcov_area

!******Determine if evaporation(Et_type = 2) or transpiration plus
!******evaporation(Et_type = 3) are active.  if not, Et_type = 1

      IF ( Avail_potet<NEARZERO ) THEN
        Et_type = 1
        Avail_potet = 0.0
      ELSEIF ( Transp_on==0 ) THEN
        IF ( Snow_free<0.01 ) THEN
          Et_type = 1
        ELSE
          Et_type = 2
        ENDIF
      ELSEIF ( Cov_type>0 ) THEN
        Et_type = 3
      ELSEIF ( Snow_free<0.01 ) THEN
        Et_type = 1
      ELSE
        Et_type = 2
      ENDIF

      Soil_lower_ratio = Soil_moist/Soil_moist_max
      Soil_rechr_ratio = Soil_rechr/Soil_rechr_max
      IF ( Et_type>1 ) THEN
        Potet_lower = Avail_potet
        Potet_rechr = Avail_potet

!******sandy soil
        IF ( Soil_type==1 ) THEN
          IF ( Soil_lower_ratio<0.25 )
     +         Potet_lower = 0.5*Soil_lower_ratio*Avail_potet
          IF ( Soil_rechr_ratio<0.25 )
     +         Potet_rechr = 0.5*Soil_rechr_ratio*Avail_potet
!******loam soil
        ELSEIF ( Soil_type==2 ) THEN
          IF ( Soil_lower_ratio<0.5 )
     +         Potet_lower = Soil_lower_ratio*Avail_potet
          IF ( Soil_rechr_ratio<0.5 )
     +         Potet_rechr = Soil_rechr_ratio*Avail_potet
!******clay soil
        ELSEIF ( Soil_type==3 ) THEN
          IF ( Soil_lower_ratio<TWOTHIRDS .AND.
     +         Soil_lower_ratio>ONETHIRD ) THEN
            Potet_lower = Soil_lower_ratio*Avail_potet
          ELSEIF ( Soil_lower_ratio<=ONETHIRD ) THEN
            Potet_lower = 0.5*Soil_lower_ratio*Avail_potet
          ENDIF
          IF ( Soil_rechr_ratio<TWOTHIRDS .AND.
     +         Soil_rechr_ratio>ONETHIRD ) THEN
            Potet_rechr = Soil_rechr_ratio*Avail_potet
          ELSEIF ( Soil_rechr_ratio<=ONETHIRD ) THEN
            Potet_rechr = 0.5*Soil_rechr_ratio*Avail_potet
          ENDIF
        ENDIF

!******Soil moisture accounting
        IF ( Et_type==2 ) Potet_rechr = Potet_rechr*Snow_free
        IF ( Potet_rechr>Soil_rechr ) THEN
          Potet_rechr = Soil_rechr
          Soil_rechr = 0.0
        ELSE
          Soil_rechr = Soil_rechr - Potet_rechr
        ENDIF
        IF ( Et_type==2 .OR. Potet_rechr>=Potet_lower ) THEN
          IF ( Potet_rechr>Soil_moist ) THEN
            Potet_rechr = Soil_moist
            Soil_moist = 0.0
          ELSE
            Soil_moist = Soil_moist - Potet_rechr
          ENDIF
          et = Potet_rechr
        ELSEIF ( Potet_lower>Soil_moist ) THEN
          et = Soil_moist
          Soil_moist = 0.0
        ELSE
          Soil_moist = Soil_moist - Potet_lower
          et = Potet_lower
        ENDIF
        IF ( Soil_rechr>Soil_moist ) Soil_rechr = Soil_moist
      ELSE
        et = 0.0
      ENDIF
      Perv_actet = et
      ! sanity check
      IF ( Perv_actet>Avail_potet ) THEN
        Soil_moist = Soil_moist + Perv_actet - Avail_potet
        Perv_actet = Avail_potet
       PRINT *, 'perv_et problem', Perv_actet, Avail_potet
      ENDIF

      END SUBROUTINE compute_szactet

!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE comp_inter_gwflow(Slowcoef_lin, Slowcoef_sq,
     +           Ssr2gw_rate, Ssr2gw_exp, Pref_flow_thrsh,
     +           Gvr2pfr, Ssr_to_gw, Slow_flow, Slow_stor, Hru_type,
     +           Soil_to_ssr)
      USE PRMS_BASIN, ONLY: NEARZERO
! Functions
      IMPLICIT NONE
      INTRINSIC MAX
      EXTERNAL check_gravity, compute_interflow
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Slowcoef_lin, Slowcoef_sq
      REAL, INTENT(IN) :: Ssr2gw_rate, Ssr2gw_exp, Pref_flow_thrsh
      REAL, INTENT(INOUT) :: Slow_stor, Gvr2pfr, Ssr_to_gw, Slow_flow
      REAL, INTENT(INOUT) :: Soil_to_ssr
! Local Variables
      REAL :: extra_water
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        extra_water = MAX( 0.0, Slow_stor-Pref_flow_thrsh )
        IF ( extra_water>0.0 ) THEN
          Soil_to_ssr = Soil_to_ssr - extra_water
          ! compute contribution to preferential-flow reservoir storage
          Gvr2pfr = extra_water
          Slow_stor = Pref_flow_thrsh
        ENDIF
        ! compute slow contribution to interflow, if any
        IF ( Slow_stor>0.0 ) CALL compute_interflow(Slowcoef_lin,
     +       Slowcoef_sq, Soil_to_ssr, Slow_stor, Slow_flow)
      ENDIF

!******compute flow to groundwater
      IF ( Slow_stor>0.0 ) THEN
        IF ( Ssr2gw_rate>NEARZERO ) THEN
          Ssr_to_gw = Ssr2gw_rate*(Slow_stor**Ssr2gw_exp)
          IF ( Ssr_to_gw>Slow_stor ) Ssr_to_gw = Slow_stor
          IF ( Ssr_to_gw<0.0 ) Ssr_to_gw = 0.0
          Slow_stor = Slow_stor - Ssr_to_gw
        ENDIF
      ENDIF

      END SUBROUTINE comp_inter_gwflow

!***********************************************************************
!     Compute subsurface lateral flow
!***********************************************************************
      SUBROUTINE compute_interflow(Coef_lin, Coef_sq, Ssres_in, Storage,
     +           Inter_flow)
      USE PRMS_SOILZONE, ONLY: DBGUNT
      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      INTRINSIC EXP, SQRT
! Arguments
      REAL, INTENT(IN) :: Coef_lin, Coef_sq, Ssres_in
      REAL, INTENT(INOUT) :: Storage
      REAL, INTENT(OUT) :: Inter_flow
! Local Variables
      REAL :: c1, c2, c3, sos
!***********************************************************************
! Inter_flow is in inches for the timestep
!******compute interflow
      IF ( Coef_lin<NEARZERO .AND. Ssres_in<=0.0 ) THEN
        c1 = Coef_sq*Storage
        Inter_flow = Storage*(c1/(1.0+c1))
      ELSEIF ( Coef_lin>0.0 .AND. Coef_sq<NEARZERO ) THEN
        c2 = 1.0 - EXP(-Coef_lin)
        Inter_flow = Ssres_in*(1.0-c2/Coef_lin) + Storage*c2
      ELSE
        c3 = SQRT(Coef_lin**2.0+4.0*Coef_sq*Ssres_in)
        sos = Storage - ((c3-Coef_lin)/(2.0*Coef_sq))
        c1 = Coef_sq*sos/c3
        c2 = 1.0 - EXP(-c3)
        IF ( ABS(1.0+c1*c2)>0.0 ) THEN
          Inter_flow = Ssres_in + (sos*(1.0+c1)*c2)/(1.0+c1*c2)
        ELSE
          Inter_flow = Ssres_in
        ENDIF
      ENDIF

! sanity check
      IF ( Inter_flow<0.0 ) THEN
        IF ( Inter_flow<-1.0E-6 )
     +       PRINT *, 'interflow<0', Inter_flow, Ssres_in, Storage
        Storage = Storage - Inter_flow
        Inter_flow = 0.0
      ELSEIF ( Inter_flow>Storage ) THEN
        Inter_flow = Storage
      ENDIF
      Storage = Storage - Inter_flow
      IF ( Storage<0.0 ) THEN
        IF ( Print_debug==7 ) WRITE ( DBGUNT, * )
     +       'Sanity check, ssres_stor<0.0', Storage
!        print *, 'Sanity check, ssres_stor<0.0', Storage
        Storage = 0.0
! if very small storage, add it to interflow
      ELSEIF ( Storage<NEARZERO ) THEN
!       PRINT *, 'small', Storage
        Inter_flow = Inter_flow + Storage
        Storage = 0.0
      ENDIF

      END SUBROUTINE compute_interflow

!***********************************************************************
!     adjust soil moist based on being below field capacity (capacity)
!     and preferential-flow threshold (Pref_flow_thrsh)
!***********************************************************************
      SUBROUTINE check_gravity(Pref_flow_thrsh, Depth, Gvr2pfr, Input,
     +           Availh2o)
      !Note, no replenishment of capillary reservoir in PRMS-only mode
      IMPLICIT NONE
      INTRINSIC MAX
! Arguments
      REAL, INTENT(IN) :: Pref_flow_thrsh
      REAL, INTENT(INOUT) :: Depth, Input, Availh2o
      REAL, INTENT(OUT) :: Gvr2pfr
!***********************************************************************
! compute contribution to preferential-flow reservoir storage, if any
! adjust depth and input, take from input first
      Gvr2pfr = MAX(0.0, Availh2o-Pref_flow_thrsh)
      Input = Input - Gvr2pfr

      IF ( Input<0.0 ) THEN
        Depth = Depth + Input
        IF ( Depth<0.0 ) Depth = 0.0
        Input = 0.0
      ENDIF

      Availh2o = Input + Depth

      END SUBROUTINE check_gravity

!***********************************************************************
!     Compute cascading interflow and excess flow
!***********************************************************************
      SUBROUTINE compute_cascades(Ihru, Ncascade_hru, Slowflow, Preflow,
     +           Dunnian, Dnslowflow, Dnpreflow, Dndunnflow,
     +           Farflow_slow, Farflow_pref, Farflow_dunn)
      USE PRMS_MODULE, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_SOILZONE, ONLY: Upslope_dunnianflow, Upslope_interflow
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt,
     +    Cascade_area
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Ncascade_hru
      REAL, INTENT(INOUT) :: Dunnian, Slowflow, Preflow
      REAL, INTENT(INOUT) :: Dnslowflow, Dnpreflow, Dndunnflow
      REAL, INTENT(INOUT) :: Farflow_slow, Farflow_pref, Farflow_dunn
! Local Variables
      INTEGER :: j, k
      REAL :: frac, fracwt
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
        frac = Hru_down_frac(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j>0 ) THEN
          fracwt = Hru_down_fracwt(k, Ihru)
          Upslope_interflow(j) = Upslope_interflow(j) +
     +                           (Slowflow+Preflow)*fracwt
          Upslope_dunnianflow(j) = Upslope_dunnianflow(j)
     +                             + Dunnian*fracwt
          Dnslowflow = Dnslowflow + Slowflow*frac
          Dnpreflow = Dnpreflow + Preflow*frac
          Dndunnflow = Dndunnflow + Dunnian*frac
! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS(j)
          IF ( j/=Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) + (Slowflow+Preflow+Dunnian)
     +                       *Cascade_area(k, Ihru)*Cfs_conv
          ELSE
            Strm_farfield = Strm_farfield + (Slowflow+Preflow+Dunnian)
     +                      *Cascade_area(k, Ihru)*Cfs_conv
            Farflow_slow = Farflow_slow + Slowflow*frac
            Farflow_pref = Farflow_pref + Preflow*frac
            Farflow_dunn = Farflow_dunn + Dunnian*frac
          ENDIF
        ENDIF
      ENDDO

! reset Slowflow, Preflow, and Dunnian_flow as they accumulate flow to streams
      Slowflow = Slowflow - Dnslowflow - Farflow_slow
      Preflow = Preflow - Dnpreflow - Farflow_pref
      Dunnian = Dunnian - Dndunnflow - Farflow_dunn

      END SUBROUTINE compute_cascades

!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE compute_gravflow(Ihru, Capacity, Slowcoef_lin,
     +           Slowcoef_sq, Ssr2gw_rate, Ssr2gw_exp, Gvr_maxin,
     +           Pref_flow_thrsh, Soil_to_ssr, Gvr2pfr, Ssr_to_gw,
     +           Slow_flow, Slow_stor, Gvr2sm, Soil_to_gw, Gwin,
     +           Perv_frac, Hru_type)
      USE PRMS_SOILZONE, ONLY: Gvr_hru_id, Gvr_hru_pct_adjusted,
     +    Gravity_stor_res, Sm2gw_grav, Sm2gw_grav_old,
     +    Hru_gvr_count, Hru_gvr_index, Gw2sm_grav
      USE PRMS_MODULE, ONLY: Kkiter
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Functions
      INTRINSIC MAX
      EXTERNAL check_gvr_sm, compute_interflow
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Hru_type
      REAL, INTENT(IN) :: Capacity, Slowcoef_lin, Slowcoef_sq, Perv_frac
      REAL, INTENT(IN) :: Ssr2gw_rate, Ssr2gw_exp
      REAL, INTENT(IN) :: Pref_flow_thrsh, Soil_to_gw, Gvr_maxin
      REAL, INTENT(INOUT) :: Soil_to_ssr
      REAL, INTENT(OUT) :: Ssr_to_gw, Slow_stor, Slow_flow, Gvr2pfr
      REAL, INTENT(OUT) :: Gvr2sm
      DOUBLE PRECISION, INTENT(OUT) :: Gwin
! Local Variables
      INTEGER :: j, igvr
      REAL :: perc, slowflow, extra_water, actual_input, depth, input
      REAL :: hru_capacity
      DOUBLE PRECISION :: frac, dunnianflw, topfr, slflow
      DOUBLE PRECISION :: togw, slowstor
!***********************************************************************
      !Capacity is for pervious area
      !Soil_to_gw is for whole HRU
      !TO DO
! use VKS as a function of slope (vector analysis) instead of coef_lin
! coef_lin for pref_flow needs to be VKS lateral times a factor
! change slow to interflow
! in init, set an array dimensioned by nhrucell to vks*mfl_to_inch

      hru_capacity = Capacity*Perv_frac
      Gwin = 0.0D0
      Gvr2sm = 0.0
      dunnianflw = 0.0D0
      topfr = 0.0D0
      slflow = 0.0D0
      togw = 0.0D0
      slowstor = 0.0D0
      DO j = 1, Hru_gvr_count(Ihru)
        igvr = Hru_gvr_index(j, Ihru)
        frac = Gvr_hru_pct_adjusted(igvr)
        Gwin = Gwin + Gw2sm_grav(igvr)*frac
        input = Gw2sm_grav(igvr)
        depth = Gravity_stor_res(igvr) + input
        ! first replenish with additional gw2sm
        IF ( depth>0.0 .AND. hru_capacity>0.0 )
     +       CALL check_gvr_sm(hru_capacity, depth, frac, Gvr2sm, input)

        actual_input = input
        input = Gvr_maxin
        depth = depth + input
        IF ( depth>0.0 .AND. hru_capacity>0.0 )
     +       CALL check_gvr_sm(hru_capacity, depth, frac, Gvr2sm, input)
        Soil_to_ssr = Soil_to_ssr + input*frac
        actual_input = actual_input + input

        IF ( Hru_type==1 ) THEN
          extra_water = MAX( 0.0, depth-Pref_flow_thrsh )
          IF ( extra_water>0.0 ) THEN
            Soil_to_ssr = Soil_to_ssr - MIN(input, extra_water)*frac
            !compute contribution to preferential-flow reservoir storage
            topfr = topfr + extra_water*frac
            depth = Pref_flow_thrsh
          ENDIF
          actual_input = MAX(0.0, actual_input-extra_water)
! compute slow contribution to interflow, if any
          IF ( depth>0.0 ) THEN
            CALL compute_interflow(Slowcoef_lin, Slowcoef_sq,
     +           actual_input, depth, slowflow)
            slflow = slflow + slowflow*frac
          ENDIF
        ENDIF

! compute flow to groundwater, if any
        IF ( depth>0.0 ) THEN
          IF ( Ssr2gw_rate>NEARZERO ) THEN
! use VKS instead of rate  ???????????????
            perc = Ssr2gw_rate*(depth**Ssr2gw_exp)
            IF ( perc<0.0 ) perc = 0.0
! assume best guess is halfway between last iteration and this iteration
!            IF ( Kkiter>2 ) perc = perc - (perc-Sm2gw_grav_old(igvr))*.5
!            IF ( Kkiter>2 ) perc = (perc+Sm2gw_grav_old(igvr))*.5
            IF ( perc<0.0 ) THEN
              perc = 0.0
            ELSEIF ( perc>depth ) THEN
              perc = depth
            ENDIF
            depth = depth - perc
!      IF ( sm2gw_grav(igvr)>0.0 ) print*,'problem',sm2gw_grav(igvr),igvr
            Sm2gw_grav(igvr) = perc
            togw = togw + perc*frac
          ENDIF
          slowstor = slowstor + depth*frac
        ELSE
          depth = 0.0
        ENDIF

        Gravity_stor_res(igvr) = depth

! add any direct recharge from soil infiltration
        Sm2gw_grav(igvr) = Sm2gw_grav(igvr) + Soil_to_gw

      ENDDO ! end loop of GVRs in the HRU
      Gvr2pfr = topfr
      Slow_flow = slflow
      Ssr_to_gw = togw
      Slow_stor = slowstor
      IF ( Slow_stor>Pref_flow_thrsh ) print *, 'slow_stor > thrsh',
     +     Slow_stor, Pref_flow_thrsh

      END SUBROUTINE compute_gravflow

!***********************************************************************
!     adjust soil moist based on being below field capacity (capacity)
!     and preferential-flow threshold (Pref_flow_thrsh)
!***********************************************************************
      SUBROUTINE check_gvr_sm(Capacity, Depth, Frac, Gvr2sm, Input)
      IMPLICIT NONE
! Functions
      INTRINSIC MAX, ABS
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: Frac
      REAL, INTENT(INOUT) :: Capacity, Gvr2sm, Input
      REAL, INTENT(INOUT) :: Depth
! Local Variables
      REAL :: to_sm
!***********************************************************************
! check to see if soil is below capacity, if so add up to field capacity
! Capacity is for pervious area
! to_sm and Gvr2sm are for whole HRU

      ! fill up capillary with part of gravity water
      to_sm = Capacity
      ! take all gravity water and put in capillary
      IF ( to_sm>Depth ) to_sm = Depth
      ! actual input to gvr reduced by replenish amount first
      Input = Input - to_sm
      IF ( Input<0.0 ) Input = 0.0

! compute adjusmtent to soil moist to get to field capacity
      Capacity = Capacity - to_sm*Frac
      IF ( Capacity<0.0 ) THEN
        to_sm = to_sm - Capacity*Frac
        Capacity = 0.0
      ENDIF
      Gvr2sm = Gvr2sm + to_sm*Frac
      Depth = Depth - to_sm
      IF ( Depth<0.0 ) print *, 'depth<0', depth
      IF ( Depth<0.0 ) Depth = 0.0

      END SUBROUTINE check_gvr_sm
