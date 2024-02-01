!***********************************************************************
! Computes inflows to and outflows from soil zone of each HRU and
! includes inflows from infiltration, groundwater, and upslope HRUs,
! and outflows to gravity drainage, interflow, and surface runoff to
! downslope HRUs; merge of smbal_prms and ssflow_prms with enhancements
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
! FORTRAN module for soilzone_ag
!***********************************************************************
      MODULE PRMS_SOILZONE_AG

      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC_AG = 'Soilzone Computations'
      character(len=11), parameter :: MODNAME_AG = 'soilzone_ag'
      character(len=*), parameter :: Version_soilzone_ag = '2024-01-25'
      INTEGER, SAVE :: Soil_iter !, HRU_id
      DOUBLE PRECISION, SAVE :: Basin_ag_soil_to_gw, Basin_ag_up_max, Basin_perv_to_gw
      DOUBLE PRECISION, SAVE :: Basin_ag_actet, Basin_ag_soil_rechr, Basin_ag_gvr2sm, Basin_ag_cap_infil_tot
      REAL, SAVE, ALLOCATABLE :: Ag_replenish_frac(:), Ag_cap_infil_tot(:), Ag_water_in(:)
!      REAL, SAVE, ALLOCATABLE :: Ag_water_maxin(:)
      INTEGER, SAVE :: iter_nonconverge
      REAL, SAVE, ALLOCATABLE :: It0_sroff(:), It0_hru_sroffp(:), It0_hortonian_flow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: It0_strm_seg_in(:)
!   Pervious Declared Variables
      REAL, SAVE, ALLOCATABLE :: perv_soil_to_gw(:), perv_soil_to_gvr(:)
!   Agriculture Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_saturated(:)
      DOUBLE PRECISION, SAVE :: Basin_ag_irrigation_add
      REAL, SAVE, ALLOCATABLE :: Unused_ag_et(:), ag_soil_to_gvr(:), Ag_soilwater_deficit(:)
      REAL, SAVE, ALLOCATABLE :: Ag_actet(:), Ag_irrigation_add(:), Ag_irrigation_add_vol(:)
      REAL, SAVE, ALLOCATABLE :: ag_soil_to_gw(:), hru_ag_actet(:), Ag_hortonian(:), ag_AET_external_vol(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_lower(:), Ag_soil_lower_stor_max(:), Ag_potet_rechr(:), Ag_potet_lower(:)
!      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ag_upslope_dunnian(:)
      REAL, SAVE, ALLOCATABLE :: Gvr2ag(:), Gvr_maxin(:), Gvr_actual_in(:)
      real, save :: unsatisfied_big
      ! parameters
! have covden a monthly, later
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_type(:) !, Ag_crop_type(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soilwater_deficit_min(:), Ag_covden_sum(:), Ag_covden_win(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil2gw_max(:) ! Ag_crop_coef later, will specify PET
      INTEGER, SAVE :: max_soilzone_ag_iter
      REAL, SAVE :: soilzone_aet_converge

      END MODULE PRMS_SOILZONE_AG

!***********************************************************************
!     Main soilzone_ag routine
!***********************************************************************
      INTEGER FUNCTION soilzone_ag()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ACTIVE, OFF, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: szdecl, szinit, szrun_ag, szdecl_ag, szinit_ag
      EXTERNAL :: soilzone_restart_ag
!***********************************************************************
      soilzone_ag = 0

      IF ( Process_flag == RUN ) THEN
        soilzone_ag = szrun_ag()
      ELSEIF ( Process_flag == DECL ) THEN
        soilzone_ag = szdecl()
        soilzone_ag = szdecl_ag()
      ELSEIF ( Process_flag == INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL soilzone_restart_ag(READ_INIT)
        soilzone_ag = szinit()
        soilzone_ag = szinit_ag()
      ELSEIF ( Process_flag == CLEAN ) THEN
        IF ( Save_vars_to_file == ACTIVE ) CALL soilzone_restart_ag(SAVE_INIT)
      ENDIF

      END FUNCTION soilzone_ag

!***********************************************************************
!     szdecl_ag - set up parameters for agriculture soil zone computations
!   Declared Parameters
!     sat_threshold, ssstor_init_frac fastcoef_lin, fastcoef_sq
!     ssr2gw_rate, ssr2gw_exp, soil2gw_max, soil_type, ag_soil_type, ag_soilwater_deficit_min
!     soil_rechr_max_frac, soil_rechr_init_frac, soil_moist_max, soil_moist_init_frac
!     pref_flow_den, slowcoef_lin, cov_type
!     hru_area, slowcoef_sq, gvr_hru_id
!***********************************************************************
      INTEGER FUNCTION szdecl_ag()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, CASCADE_OFF !, OFF
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Iter_aet_flag, Cascade_flag
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL :: read_error, print_module, PRMS_open_module_file, error_stop
!***********************************************************************
      szdecl_ag = 0

      CALL print_module( MODDESC_AG, MODNAME_AG, Version_soilzone_ag )

      ALLOCATE ( perv_soil_to_gw(Nhru) )
      IF ( declvar(MODNAME_AG, 'perv_soil_to_gw', 'nhru', Nhru, 'real', &
     &     'Direct recharge from pervious capillary reservoir to groundwater reservior for each HRU', &
     &     'inches', perv_soil_to_gw) /= 0 ) CALL read_error(3, 'perv_soil_to_gw')

      ALLOCATE ( perv_soil_to_gvr(Nhru) )
      IF ( declvar(MODNAME_AG, 'perv_soil_to_gvr', 'nhru', Nhru, 'real', &
     &     'Excess pervious capillary water that flows to the gravity reservoir of each HRU', &
     &     'inches', perv_soil_to_gvr) /= 0 ) CALL read_error(3, 'perv_soil_to_gvr')

! Agriculture variables and parameters
      ALLOCATE ( ag_soil_to_gw(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_soil_to_gw', 'nhru', Nhru, 'real', &
     &     'Direct recharge from agriculture capillary reservoir to groundwater reservior for each HRU', &
     &     'inches', ag_soil_to_gw) /= 0 ) CALL read_error(3, 'ag_soil_to_gw')

!      IF ( Cascade_flag>OFF ) THEN
!        ALLOCATE ( Ag_upslope_dunnian(Nhru) )
!        IF ( declvar(MODNAME_AG, 'ag_upslope_dunnian', 'nhru', Nhru, 'double ', &
!     &       'Cascading Dunnian surface runoff that'// &
!     &       ' flows to the agriculture capillary reservoir of each downslope HRU for each upslope HRU', &
!     &       'inches', Ag_upslope_dunnian) /= 0 ) CALL read_error(3, 'ag_upslope_dunnian')
!      ENDIF

      ALLOCATE ( Ag_actet(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_actet', 'nhru', Nhru, 'real', &
     &     'Actual ET for agriculture capillary reservoir for each HRU', &
     &     'inches', Ag_actet) /= 0 ) CALL read_error(3, 'ag_actet')

      ALLOCATE ( hru_ag_actet(Nhru) )
      IF ( declvar(MODNAME_AG, 'hru_ag_actet', 'nhru', Nhru, 'real', &
     &     'Actual ET for agriculture capillary reservoir averaged over each HRU', &
     &     'inches', hru_ag_actet) /= 0 ) CALL read_error(3, 'hru_ag_actet')

      ALLOCATE ( Unused_ag_et(Nhru) )
      IF ( declvar(MODNAME_AG, 'unused_ag_et', 'nhru', Nhru, 'real', &
     &     'Actual ET for agriculture capillary reservoir for each HRU', &
     &     'inches', Unused_ag_et) /= 0 ) CALL read_error(3, 'unused_ag_et')

      ALLOCATE ( Ag_hortonian(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_hortonian', 'nhru', Nhru, 'real', &
     &     'Hortonian surface runoff that flows to the stream network from the agriculture fraction of each HRU', &
     &     'inches', Ag_hortonian) /= 0 ) CALL read_error(3, 'ag_hortonian')

      ALLOCATE ( ag_soil_to_gvr(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_soil_to_gvr', 'nhru', Nhru, 'real', &
     &     'Excess capillary water that flows to the agriculture gravity reservoir from the agriculture fraction of each HRU', &
     &     'inches', ag_soil_to_gvr) /= 0 ) CALL read_error(3, 'ag_soil_to_gvr')

      IF ( declvar(MODNAME_AG, 'Basin_ag_irrigation_add', 'one', 1, 'double', &
     &     'Basin area-weighted average irrigation estimate', &
     &     'inches', Basin_ag_irrigation_add) /= 0 ) CALL read_error(3, 'Basin_ag_irrigation_add')

      ALLOCATE ( Ag_irrigation_add(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_irrigation_add', 'nhru', Nhru, 'real', &
     &     'Irrigation water added to agriculture fraction when ag_actet < AET_external for each HRU', &
     &     'inches', Ag_irrigation_add) /= 0 ) CALL read_error(3, 'ag_irrigation_add')

      ALLOCATE ( Ag_soilwater_deficit(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_soilwater_deficit', 'nhru', Nhru, 'real', &
     &     'Soil-water deficit of agriculture fraction for each HRU', &
     &     'inches', Ag_soilwater_deficit) /= 0 ) CALL read_error(3, 'ag_soilwater_deficit')

      ALLOCATE ( Ag_irrigation_add_vol(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_irrigation_add_vol', 'nhru', Nhru, 'real', &
     &     'Irrigation water added to agriculture fraction when ag_actet < AET_external for each HRU', &
     &     'acre-inches', Ag_irrigation_add_vol) /= 0 ) CALL read_error(3, 'ag_irrigation_add_vol')

      ALLOCATE ( ag_AET_external_vol(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_AET_external_vol', 'nhru', Nhru, 'real', &
     &     'OpenET actual evapotranspiration for transpiration days for each HRU', &
     &     'acre-inches', ag_AET_external_vol) /= 0 ) CALL read_error(3, 'ag_AET_external_vol')

      ALLOCATE ( Ag_soil_lower(Nhru), Ag_soil_lower_stor_max(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_soil_lower', 'nhru', Nhru, 'real', &
     &   'Storage in the lower zone of the agriculture'// &
     &   ' reservoir that is only available for transpiration for each HRU', &
     &   'inches', Ag_soil_lower) /= 0 ) CALL read_error(3, 'ag_soil_lower')


      ALLOCATE ( Ag_potet_lower(Nhru) ) !, Ag_water_maxin(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_potet_lower', 'nhru', Nhru, 'real', &
     &     'Potential ET in the lower zone of the agriculture reservoir for each HRU', &
     &     'inches', Ag_potet_lower) /= 0 ) CALL read_error(3, 'ag_potet_lower')

      ALLOCATE ( Ag_potet_rechr(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_potet_rechr', 'nhru', Nhru, 'real', &
     &     'Potential ET in the recharge zone of the agriculture reservoir for each HRU', &
     &     'inches', Ag_potet_rechr) /= 0 ) CALL read_error(3, 'ag_potet_rechr')

      ALLOCATE ( Ag_soil_saturated(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_soil_saturated', 'nhru', Nhru, 'real', &
     &     'Flag set if infiltration saturates capillary reservoir (0=no, 1=yes)', &
     &     'none', Ag_soil_saturated) /= 0 ) CALL read_error(3, 'ag_soil_saturated')

      ALLOCATE ( Ag_cap_infil_tot(Nhru), Ag_water_in(Nhru) )
      IF ( declvar(MODNAME_AG, 'ag_water_in', 'nhru', Nhru, 'real', &
     &     'Total water into the agriculture reservoir for each HRU', &
     &     'inches', Ag_water_in) /= 0 ) CALL read_error(3, 'ag_water_in')

      IF ( Iter_aet_flag == ACTIVE ) THEN
        IF ( declparam(MODNAME_AG, 'max_soilzone_ag_iter', 'one', 'integer', &
     &       '10', '1', '9999', &
     &       'Maximum number of iterations to optimize computed AET and input AET', &
     &       'Maximum number of iterations to optimize computed AET and input AET', &
     &       'none') /= 0) CALL read_error(1, 'max_soilzone_ag_iter')

        IF ( declparam(MODNAME_AG, 'soilzone_aet_converge', 'one', 'real', &
     &       '0.00001', '0.0', '1.0', &
     &       'Convergence criteria to iterate computed AET compared to input AET', &
     &       'Convergence criteria to iterate computed AET compared to input AET', &
     &       'decimal fraction') /= 0) CALL read_error(1, 'soilzone_aet_converge')

        ALLOCATE ( Ag_soilwater_deficit_min(Nhru) )
        IF ( declparam(MODNAME_AG, 'ag_soilwater_deficit_min', 'nhru', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Minimum soil-water deficit to begin agriculture irrigaition', &
     &       'Minimum soil-water deficit fraction to begin agriculture irrigaition', &
     &       'fraction') /= 0) CALL read_error(1, 'ag_soilwater_deficit_min')
      ENDIF

      IF ( Iter_aet_flag==ACTIVE ) THEN
        ALLOCATE ( It0_sroff(Nhru), It0_hru_sroffp(Nhru), It0_hortonian_flow(Nhru) )
        IF ( Cascade_flag>CASCADE_OFF ) ALLOCATE ( It0_strm_seg_in(Nsegment) )
      ENDIF

      ALLOCATE ( Ag_soil_type(Nhru) )
      IF ( declparam(MODNAME_AG, 'ag_soil_type', 'nhru', 'integer', &
     &     '-1', '-1', '3', &
     &     'Agriculture soil type', 'Soil type of agriculture in each HRU (1=sand; 2=loam; 3=clay)', &
     &     'none') /= 0) CALL read_error(1, 'ag_soil_type')

!      ALLOCATE ( Ag_crop_type(Nhru) ) ! find Mastin's code on different crops
!      IF ( declparam(MODNAME_AG, 'ag_crop_type', 'nhru', 'integer', &
!     &     '3', '0', '4', &
!     &     'Agriculture cover type designation for each HRU', &
!     &     'Vegetation cover type for agriculture in each HRU (0=none;'// &
!     &     ' 1=grasses; 2=grain; 3=trees; 4=vegetable)', &
!     &     'none') /= 0) CALL read_error(1, 'ag_crop_type')

      ALLOCATE ( Ag_covden_sum(Nhru) )
      IF ( declparam(MODNAME_AG, 'ag_covden_sum', 'nhru', 'real', &
     &     '-1.0', '-1.0', '1.0', &
     &     'Summer vegetation cover density for agriculture crop type', &
     &     'Summer vegetation cover density for the agriculture crop type in each HRU', &
     &     'decimal fraction') /= 0) CALL read_error(1, 'ag_covden_sum')

      ALLOCATE ( Ag_covden_win(Nhru) )
      IF ( declparam(MODNAME_AG, 'ag_covden_win', 'nhru', 'real', &
     &     '-1.0', '-1.0', '1.0', &
     &     'Winter vegetation cover density for crop type', &
     &     'Winter vegetation cover density for the crop type in each HRU', &
     &     'decimal fraction') /= 0) CALL read_error(1, 'ag_covden_win')

      ALLOCATE ( Ag_soil2gw_max(Nhru) )
      IF ( declparam(MODNAME_AG, 'ag_soil2gw_max', 'nhru', 'real', &
     &     '-1.0', '-1.0', '5.0', &
     &     'Maximum value for agriculture capillary reservoir excess to groundwater storage', &
     &     'Maximum amount of the agriculture capillary reservoir excess that'// &
     &     ' is routed directly to the groundwater storage for each HRU', &
     &     'inches') /= 0) CALL read_error(1, 'ag_soil2gw_max')

      END FUNCTION szdecl_ag

!***********************************************************************
!     szinit_ag - Initialize soilzone module - get parameter values,
!                 set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION szinit_ag()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, LAKE, INACTIVE, GLACIER, Nmonths
      USE PRMS_MODULE, ONLY: Nhru, Init_vars_from_file, Iter_aet_flag
      USE PRMS_SOILZONE, ONLY: Soil2gw_max, Soil_type
      USE PRMS_SOILZONE_AG
      USE PRMS_BASIN, ONLY: Basin_area_inv, Ag_area, Covden_win, Covden_sum, Hru_type
      USE PRMS_FLOWVARS, ONLY: Basin_ag_soil_moist, Ag_soil_moist, Ag_soil_rechr, Ag_soil_moist_max, &
                               Ag_soil_rechr_max
       IMPLICIT NONE
! Functions
      EXTERNAL :: init_basin_vars, read_error
      INTEGER, EXTERNAL :: getparam
      INTRINSIC :: MIN, DBLE
! Local Variables
      INTEGER :: ihru
!***********************************************************************
      szinit_ag = 0

!??? figure out what to save in restart file ???
      IF ( Iter_aet_flag == ACTIVE ) THEN
        IF ( getparam(MODNAME_AG, 'max_soilzone_ag_iter', 1, 'integer', max_soilzone_ag_iter) /= 0) &
     &       CALL read_error(2, 'max_soilzone_ag_iter')
        IF ( getparam(MODNAME_AG, 'soilzone_aet_converge', 1, 'real', soilzone_aet_converge) /= 0) &
     &       CALL read_error(2, 'soilzone_aet_converge')
        IF ( getparam(MODNAME_AG, 'ag_soilwater_deficit_min', Nhru, 'real', Ag_soilwater_deficit_min) /= 0) &
     &       CALL read_error(2, 'ag_soilwater_deficit_min')
        Ag_irrigation_add = 0.0
        Ag_soilwater_deficit = 0.0
      ENDIF
      IF ( getparam(MODNAME_AG, 'ag_soil_type', Nhru, 'integer', Ag_soil_type) /= 0) CALL read_error(2, 'ag_soil_type')
      IF ( Ag_soil_type(1) == -1 ) THEN
        PRINT *, 'WARNING, ag_soil_type not specified, substituting soil_type'
        Ag_soil_type = Soil_type
      ENDIF
!      IF ( getparam(MODNAME_AG, 'ag_crop_type', Nhru, 'integer', Ag_crop_type) /= 0) CALL read_error(2, 'ag_crop_type')
      IF ( getparam(MODNAME_AG, 'ag_covden_sum', Nhru, 'real', Ag_covden_sum) /= 0) CALL read_error(2, 'ag_covden_sum')
      IF ( Ag_covden_sum(1) < 0.0 ) THEN
        PRINT *, 'WARNING, ag_covden_sum not specified, substituting covden_sum'
        Ag_covden_sum = Covden_sum
      ENDIF
      IF ( getparam(MODNAME_AG, 'ag_covden_win', Nhru, 'real', Ag_covden_win) /= 0) CALL read_error(2, 'ag_covden_win')
      IF ( Ag_covden_win(1) < 0.0 ) THEN
        PRINT *, 'WARNING, ag_covden_win not specified, substituting covden_win'
        Ag_covden_win = Covden_win
      ENDIF
      IF ( getparam(MODNAME_AG, 'ag_soil2gw_max', Nhru, 'real', Ag_soil2gw_max) /= 0) CALL read_error(2, 'ag_soil2gw_max')
      IF ( Ag_soil2gw_max(1) < 0.0 ) THEN
        PRINT *, 'WARNING, ag_soil2gw_max not specified, substituting soil2gw_max'
        Ag_soil2gw_max = Soil2gw_max
      ENDIF
      IF ( Init_vars_from_file == 0 .OR. Init_vars_from_file == 2 .OR. Init_vars_from_file == 5 ) Ag_soil_lower = 0.0
      ! dimensioned nhru
      perv_soil_to_gw = 0.0
! initialize all HRU values in case dynamic ag frac
      Ag_actet = 0.0
      hru_ag_actet = 0.0
      perv_soil_to_gw = 0.0
      perv_soil_to_gvr = 0.0
      Basin_ag_soil_moist = 0.0D0
      Basin_ag_soil_rechr = 0.0D0
      DO ihru = 1, Nhru
        ! make sure LAKE, INACTIVE, GLACIER have agriculture values of 0
        IF ( Hru_type(ihru) == LAKE .OR. Hru_type(ihru) == INACTIVE .OR. Hru_type(ihru) == GLACIER ) Ag_area(ihru) = 0.0
        IF ( Ag_area(ihru) > 0.0 ) THEN
          Basin_ag_soil_moist = Basin_ag_soil_moist + DBLE( Ag_soil_moist(ihru)*Ag_area(ihru) )
          Basin_ag_soil_rechr = Basin_ag_soil_rechr + DBLE( Ag_soil_rechr(ihru)*Ag_area(ihru) )
        ELSE
          Ag_soil_moist(ihru) = 0.0
          Ag_soil_rechr(ihru) = 0.0
        ENDIF
        Ag_soil_lower_stor_max(ihru) = Ag_soil_moist_max(ihru) - Ag_soil_rechr_max(ihru)
      ENDDO
      Basin_ag_soil_moist = Basin_ag_soil_moist*Basin_area_inv
      Basin_ag_soil_rechr = Basin_ag_soil_rechr*Basin_area_inv

      iter_nonconverge = 0

      END FUNCTION szinit_ag

!***********************************************************************
!     szrun_ag - Does soil water balance for each HRU, adds in infiltration
!                then computes actual et and apportions remainder between
!                recharge of soil moisture, soil storage available for
!                interflow, excess routed to stream,
!                and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION szrun_ag()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, NEARZERO, LAKE, SWALE, &
     &    DEBUG_less, ERROR_param, CASCADE_OFF, CLOSEZERO
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Print_debug, Dprst_flag, Cascade_flag, &
     &    Frozen_flag, Soilzone_add_water_use, Nowmonth, Nowyear, Nowday, Iter_aet_flag
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      USE PRMS_BASIN, ONLY: Hru_perv, Hru_frac_perv, Hru_storage, &
     &    Hru_route_order, Active_hrus, Basin_area_inv, Hru_area, &
     &    Lake_hru_id, Cov_type, Numlake_hrus, Hru_area_dble, &
     &    Ag_frac, Ag_area, Ag_cov_type, Ag_area_total, Hru_type
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Transp_on, Potet, Basin_potet, Basin_transp_on
! WARNING!!! Sroff, Basin_sroff, and Strm_seg_in can be updated
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_actet, Hru_actet, Pref_flow_stor, &
     &    Ssres_flow, Soil_to_gw, Basin_soil_to_gw, Ssr_to_gw, Pref_flag, &
     &    Soil_to_ssr, Basin_lakeevap, Basin_perv_et, Basin_swale_et, &
     &    Sroff, Soil_moist_max, Infil, Soil_rechr_max, Ssres_in, Basin_soil_rechr, &
     &    Basin_soil_moist, Basin_ssstor, Slow_stor, Slow_flow, Pkwater_equiv, &
     &    Ssres_stor, Soil_moist, Sat_threshold, Soil_rechr, Basin_sroff, Basin_lake_stor, &
     &    Soil_moist_tot, Dprst_stor_hru, Hru_impervstor, Soil_lower_stor_max, Soil_zone_max, &
     &    Ag_soil_rechr, Ag_soil_moist, Ag_soil_rechr_max, Ag_soil_moist_max, Basin_ag_soil_moist, &
     &    Recharge, Basin_recharge
      USE PRMS_IT0_VARS, ONLY: It0_ag_soil_rechr, It0_ag_soil_moist
      USE PRMS_WATER_USE, ONLY: Soilzone_gain, Soilzone_gain_hru
      USE PRMS_CLIMATE_HRU, ONLY: AET_external, PET_external
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_INTCP, ONLY: Hru_intcpevap, Hru_intcpstor
      USE PRMS_SNOW, ONLY: Snowcov_area, Snow_evap
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap, Dprst_evap_hru, Dprst_seep_hru, Frozen, &
     &    Hru_sroffp, Hortonian_flow, Basin_sroffp, Basin_hortonian, Infil_ag, Strm_seg_in
      IMPLICIT NONE
! Functions
      INTRINSIC :: MIN, ABS, MAX, SNGL, DBLE
      EXTERNAL :: compute_soilmoist, compute_szactet, compute_cascades
      EXTERNAL :: compute_interflow, compute_gwflow, init_basin_vars, print_date, error_stop
! Local Variables
      INTEGER :: i, k, update_potet, compute_lateral
      REAL :: dunnianflw, interflow, perv_area, harea
      REAL :: dnslowflow, dnpreflow, dndunn, availh2o, avail_potet, hruactet
      REAL :: topfr !, tmp
      REAL :: dunnianflw_pfr, dunnianflw_gvr, pref_flow_maxin
      REAL :: perv_frac, capwater_maxin, ssresin, potet_sngl
      REAL :: cap_upflow_max, unsatisfied_et, pervactet, prefflow, ag_water_maxin
      REAL :: ag_upflow_max, agfrac, ag_avail_potet, ag_potet, unsatisfied_ag_et
      REAL :: ag_AETtarget, ag_avail_targetAET, agactet, ag_pref_flow_maxin, ag_hruactet
      REAL :: upflow_max, ag_portion, perv_portion, agarea, unsatisfied_max !, max_irrigation
      DOUBLE PRECISION :: gwin
      INTEGER :: cfgi_frozen_hru, adjust_frozen, adjust_dunnian
      INTEGER :: num_hrus_ag_iter, ag_on_flag, keep_iterating, add_estimated_irrigation, perv_on_flag
!***********************************************************************
      szrun_ag = 0

! It0 variables used to save iteration states.
      IF ( Iter_aet_flag == ACTIVE ) THEN
        ! computed in srunoff
        It0_sroff = Sroff
        It0_hru_sroffp = Hru_sroffp
        It0_hortonian_flow = Hortonian_flow
        IF ( Cascade_flag>CASCADE_OFF ) It0_strm_seg_in = Strm_seg_in
      ENDIF

      IF ( Iter_aet_flag == ACTIVE ) THEN
        Basin_ag_irrigation_add = 0.0D0
        Ag_irrigation_add = 0.0
        Ag_irrigation_add_vol = 0.0
        ag_AET_external_vol = 0.0
      ENDIF

      keep_iterating = ACTIVE
      Soil_iter = 1

! ***************************************
      DO WHILE ( keep_iterating == ACTIVE )
! ***************************************

      IF ( Iter_aet_flag == ACTIVE ) THEN
        Sroff = It0_sroff
        Hru_sroffp = It0_hru_sroffp
        Hortonian_flow = It0_hortonian_flow
        IF ( Cascade_flag>CASCADE_OFF ) Strm_seg_in = It0_strm_seg_in
        !max_irrigation = 0.0
      ENDIF

      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Upslope_interflow = 0.0D0
        Upslope_dunnianflow = 0.0D0
        IF ( Numlake_hrus>0 ) THEN
          Lakein_sz = 0.0D0
          Basin_lakeinsz = 0.0D0
        ENDIF
        Basin_dninterflow = 0.0D0
        Basin_dndunnianflow = 0.0D0
        Basin_dncascadeflow = 0.0D0
      ENDIF

      Ag_soil_moist = It0_ag_soil_moist
      Ag_soil_rechr = It0_ag_soil_rechr

      Basin_ag_soil_moist = 0.0D0
      Basin_ag_soil_rechr = 0.0D0
      Basin_ag_soil_to_gw = 0.0D0
      Basin_ag_up_max = 0.0D0
      Basin_ag_actet = 0.0D0
      Basin_ag_gvr2sm = 0.0D0
      Basin_cap_infil_tot = 0.0D0
      Basin_ag_cap_infil_tot = 0.0D0
      Basin_perv_to_gw = 0.D0
      CALL init_basin_vars()
      IF ( Nlake>0 ) THEN
        Basin_lakeprecip = 0.0D0
        Basin_lakeevap = 0.0D0
      ENDIF
      gwin = 0.0D0
      ! Soil_to_gw and Soil_to_ssr for whole HRU
      Soil_to_gw = 0.0
      Soil_to_ssr = 0.0
      ! gravity reservoir variables for whole HRU
      Ssr_to_gw = 0.0
      Slow_flow = 0.0
      Ssres_flow = 0.0
      Cap_waterin = 0.0
      Soil_saturated = OFF
      Dunnian_flow = 0.0
      Potet_rechr = 0.0
      Potet_lower = 0.0
      Cap_infil_tot = 0.0
      Snow_free = 1.0 - Snowcov_area
      IF ( Pref_flag==ACTIVE ) THEN
        Pref_flow_infil = 0.0
        Pref_flow_in = 0.0
        Pref_flow = 0.0
        Pfr_dunnian_flow = 0.0
      ENDIF
      update_potet = OFF
      IF ( Soilzone_add_water_use==ACTIVE ) Soilzone_gain_hru = 0.0
      adjust_frozen = OFF
      adjust_dunnian = OFF
      perv_soil_to_gw = 0.0
! initialize all HRU values in case dynamic ag frac
      Ag_soil_saturated = OFF
      ag_soil_to_gvr = 0.0
      ag_soil_to_gw = 0.0
      Ag_hortonian = 0.0
      Unused_ag_et = 0.0
      IF ( Iter_aet_flag == ACTIVE ) Ag_soilwater_deficit = 0.0
      Ag_cap_infil_tot = 0.0
      Ag_water_in = 0.0
      Ag_potet_lower = 0.0
      Ag_potet_rechr = 0.0
      Ag_soil_lower = 0.0
      unsatisfied_big = 0.0
      add_estimated_irrigation = OFF
      num_hrus_ag_iter = 0

! ***************************************
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        potet_sngl = SNGL( Potet(i) )
! ***************************************
        !HRU_id = i
        hruactet = Hru_impervevap(i) + Hru_intcpevap(i) + Snow_evap(i)
        IF ( Dprst_flag==ACTIVE ) hruactet = hruactet + Dprst_evap_hru(i)
        harea = Hru_area(i)
        agfrac = Ag_frac(i)

        IF ( Hru_type(i)==LAKE ) THEN ! lake or reservoir
          !WARNING, RSR, if hru_actet>water in lake, then budget error
          hruactet = (potet_sngl - hruactet)*Lake_evap_adj(Nowmonth,Lake_hru_id(i))
          IF ( hruactet>Potet(i) ) THEN
            IF ( Print_debug > DEBUG_less ) THEN
              PRINT *, 'WARNING, lake evap > potet, for HRU:', i, ' potential ET increased to adjusted lake ET'
              PRINT *, hruactet, Potet(i), hruactet - Potet(i)
            ENDIF
            Potet(i) = DBLE( hruactet ) ! this could be a problem when it happens
            update_potet = ACTIVE
          ENDIF
          Unused_potet(i) = SNGL( Potet(i) ) - hruactet
          Basin_actet = Basin_actet + DBLE( hruactet*harea )
          Basin_lakeevap = Basin_lakeevap + DBLE( hruactet*harea )
          Basin_lakeprecip = Basin_lakeprecip + Hru_ppt(i)*Hru_area_dble(i)
          IF ( Cascade_flag>CASCADE_OFF ) THEN
            ! if lake HRU doesn't cascade, should we limit ET to
            !  water entering the HRU to this point (no gwflow yet)
            Lakein_sz(i) = Upslope_interflow(i) + Upslope_dunnianflow(i)
            Basin_lakeinsz = Basin_lakeinsz + Lakein_sz(i)*Hru_area_dble(i)
          ENDIF
          Hru_actet(i) = hruactet
          CYCLE
        ENDIF

        !Hru_type can be 1 (land) or 3 (swale) or 4 (glacier)
        compute_lateral = ACTIVE
        IF ( Hru_type(i)==SWALE ) compute_lateral = OFF
        perv_area = Hru_perv(i)
        perv_frac = Hru_frac_perv(i)
        perv_on_flag = OFF
        IF ( perv_area > 0.0 ) perv_on_flag = ACTIVE
        agarea = Ag_area(i)
        ag_on_flag = OFF
        IF ( agarea > 0.0 ) ag_on_flag = ACTIVE

        avail_potet = potet_sngl - hruactet
        IF ( avail_potet<0.0 ) THEN
!          IF ( avail_potet<-CLOSEZERO ) &
!               print *, 'avail_potet<0', i, avail_potet, Potet(i), Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i), hruactet
          avail_potet = 0.0
          hruactet = potet_sngl
        ENDIF

!******Add infiltration to soil and compute excess
        !infil_tot is the depth in whole HRU
        !capillary reservoir for pervious area
        !agriculture reservoir for irrigated area
        !preferential flow reservoir for whole HRU
        !gravity reservoir for whole HRU
        !upslope flow for whole HRU

!******if cascading flow available from upslope cascades
!****** add soil excess (Dunnian flow) to infiltration
        ! infil for pervious portion of HRU
        ! infil_ag for agriculture portion of HRU
        capwater_maxin = Infil(i)
        ag_water_maxin = Infil_ag(i)

        IF ( Iter_aet_flag == ACTIVE ) ag_water_maxin = ag_water_maxin + Ag_irrigation_add(i) ! units of inches over Ag_area
        IF ( Soilzone_add_water_use==ACTIVE ) THEN
          IF ( Soilzone_gain(i)>0.0 ) THEN
            IF ( perv_on_flag == OFF ) CALL error_stop('pervious area must be > 0 for soilzone transfer', ERROR_param)
            Soilzone_gain_hru(i) = (Soilzone_gain(i)/perv_area)/SNGL(Cfs_conv) ! ??? is this harea
            IF ( ag_on_flag == ACTIVE ) THEN
              Soilzone_gain_hru(i) = (Soilzone_gain(i)/agarea)/SNGL(Cfs_conv)
              ag_water_maxin = ag_water_maxin + Soilzone_gain_hru(i)
            ELSE
              Soilzone_gain_hru(i) = (Soilzone_gain(i)/perv_area)/SNGL(Cfs_conv)
              capwater_maxin = capwater_maxin + Soilzone_gain_hru(i)
            ENDIF
          ENDIF
        ENDIF

        cfgi_frozen_hru = OFF
        !Frozen is HRU variable that says if frozen gravity reservoir
        ! For CFGI all inflow is assumed to be Dunnian Flow when frozen
        IF ( Frozen_flag==ACTIVE ) THEN
          IF ( Frozen(i)==ACTIVE ) THEN
!            IF ( compute_lateral==OFF ) THEN
!              PRINT *, 'ERROR, a swale HRU cannot be frozen for CFGI, HRU:', i
!              ERROR STOP ERROR_param
!            ENDIF
            cfgi_frozen_hru = ACTIVE
          ENDIF
        ENDIF

        ! compute preferential flow and storage, and any dunnian flow
        ! pref_flow for whole HRU
! ??? should cascading flow go to preferential flow fraction ???
        prefflow = 0.0
        dunnianflw_pfr = 0.0
        IF ( Pref_flag == ACTIVE ) THEN
          IF ( Pref_flow_infil_frac(i)>0.0 .AND. Pref_flow_thrsh(i)>0.0 ) THEN
            pref_flow_maxin = 0.0
            IF ( ag_water_maxin > 0.0 ) THEN
              ag_pref_flow_maxin = ag_water_maxin*Pref_flow_infil_frac(i)
              ag_water_maxin = ag_water_maxin - ag_pref_flow_maxin
              ag_pref_flow_maxin = ag_pref_flow_maxin*agfrac
              pref_flow_maxin = ag_pref_flow_maxin
            ENDIF
            IF ( capwater_maxin>0.0 ) THEN
              ! pref_flow for whole HRU
              pref_flow_maxin = capwater_maxin*Pref_flow_infil_frac(i)
              capwater_maxin = capwater_maxin - pref_flow_maxin
              pref_flow_maxin = pref_flow_maxin*perv_frac
              IF ( cfgi_frozen_hru==ACTIVE ) THEN
                IF ( compute_lateral==ACTIVE ) THEN
                  dunnianflw_pfr = pref_flow_maxin
                ELSE ! swale
                  Pref_flow_stor(i) = Pref_flow_stor(i) + pref_flow_maxin
                ENDIF
              ELSE
                ! compute contribution to preferential-flow reservoir storage
                Pref_flow_stor(i) = Pref_flow_stor(i) + pref_flow_maxin
                dunnianflw_pfr = MAX( 0.0, Pref_flow_stor(i)-Pref_flow_max(i) )
              ENDIF
              IF ( dunnianflw_pfr>0.0 ) THEN
                Basin_dunnian_pfr = Basin_dunnian_pfr + DBLE( dunnianflw_pfr*harea )
                Pref_flow_stor(i) = Pref_flow_max(i)
              ENDIF
              Pref_flow_infil(i) = pref_flow_maxin - dunnianflw_pfr
              Basin_pref_flow_infil = Basin_pref_flow_infil + DBLE( Pref_flow_infil(i)*harea )
              Pfr_dunnian_flow(i) = dunnianflw_pfr
            ENDIF
          ENDIF
        ENDIF

        IF ( Cascade_flag>CASCADE_OFF ) THEN
          upflow_max = SNGL(Upslope_dunnianflow(i)+Upslope_interflow(i))
          IF ( upflow_max > 0.0 ) THEN
            IF ( perv_on_flag == ACTIVE ) THEN
              perv_portion = perv_frac / (perv_frac + agfrac)
              cap_upflow_max = upflow_max * perv_portion / perv_frac
              capwater_maxin = capwater_maxin + cap_upflow_max
            ENDIF
            IF ( ag_on_flag == ACTIVE ) THEN
              ag_portion = agfrac / (perv_frac + agfrac)
              ag_upflow_max = upflow_max * ag_portion / agfrac
              ag_water_maxin = ag_water_maxin + ag_upflow_max
              Basin_ag_up_max = Basin_ag_up_max + DBLE( ag_upflow_max*agarea )
            ENDIF
          ENDIF
        ENDIF

        IF ( perv_on_flag == ACTIVE ) THEN
          Cap_infil_tot(i) = capwater_maxin*perv_frac
          Basin_cap_infil_tot = Basin_cap_infil_tot + DBLE( Cap_infil_tot(i)*harea )
        ENDIF
        IF ( ag_on_flag == ACTIVE ) THEN
          Ag_cap_infil_tot(i) = ag_water_maxin*agfrac
          Basin_ag_cap_infil_tot = Basin_ag_cap_infil_tot + DBLE( Ag_cap_infil_tot(i)*harea )
        ENDIF

!******Add infiltration to soil and compute excess
        perv_soil_to_gvr(i) = 0.0

        IF ( cfgi_frozen_hru==OFF ) THEN
          IF ( perv_on_flag == ACTIVE ) THEN
            ! call even if capwater_maxin = 0, just in case soil_moist now > Soil_moist_max
            IF ( capwater_maxin+Soil_moist(i)>0.0 ) THEN
              CALL compute_soilmoist(capwater_maxin, Soil_moist_max(i), &
     &             Soil_rechr_max(i), Soil2gw_max(i), perv_soil_to_gvr(i), &
     &             Soil_moist(i), Soil_rechr(i), perv_soil_to_gw(i), perv_frac)
            ENDIF
          ENDIF
          IF ( ag_on_flag == ACTIVE ) THEN
            IF ( ag_water_maxin+Ag_soil_moist(i) > 0.0 ) THEN
!              if ( Ag_soil_moist(i)<Ag_soil_rechr(i) ) print *, 'AG1 soilrechr, before', i, &
!     &             Ag_soil_moist(i)-Ag_soil_rechr(i), Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i),perv_soil_to_gvr(i)
              CALL compute_soilmoist(ag_water_maxin, Ag_soil_moist_max(i), &
     &                               Ag_soil_rechr_max(i), Soil2gw_max(i), ag_soil_to_gvr(i), &
     &                               Ag_soil_moist(i), Ag_soil_rechr(i), ag_soil_to_gw(i), agfrac)
              !if ( Ag_soil_moist(i)< Ag_soil_rechr(i)) print *, 'AG1 soilrechr, after', i, &
              !     Ag_soil_moist(i)-Ag_soil_rechr(i), Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i)
              Ag_water_in(i) = ag_water_maxin
            ENDIF
          ENDIF
          Soil_to_gw(i) = perv_soil_to_gw(i) + ag_soil_to_gw(i)
          Soil_to_ssr(i) = perv_soil_to_gvr(i) + ag_soil_to_gvr(i)
          Basin_soil_to_gw = Basin_soil_to_gw + DBLE( Soil_to_gw(i)*harea )
          Basin_sm2gvr_max = Basin_sm2gvr_max + DBLE( Soil_to_ssr(i)*harea )
          Basin_perv_to_gw = Basin_perv_to_gw + DBLE ( perv_soil_to_gw(i)*harea )
        ELSE
          adjust_frozen = ACTIVE
          IF ( compute_lateral==ACTIVE ) THEN
            IF ( perv_on_flag == ACTIVE ) THEN
              Sroff(i) = Sroff(i) + capwater_maxin
              Hru_sroffp(i) = Hru_sroffp(i) + capwater_maxin * perv_frac
              Hortonian_flow(i) = Hortonian_flow(i) + capwater_maxin * perv_frac
              capwater_maxin = 0.0
            ENDIF
            IF ( ag_on_flag == ACTIVE ) THEN
              Sroff(i) = Sroff(i) + ag_water_maxin
              Hortonian_flow(i) = Hortonian_flow(i) + ag_water_maxin * agfrac
              Ag_water_in(i) = 0.0
            ENDIF
          ELSE
            IF ( perv_on_flag == ACTIVE ) THEN
              Soil_moist(i) = Soil_moist(i) + capwater_maxin
              Soil_rechr(i) = Soil_rechr(i) + capwater_maxin
            ENDIF
             IF ( ag_on_flag == ACTIVE ) THEN
               Ag_soil_moist(i) = Ag_soil_moist(i) + ag_water_maxin
               Ag_soil_rechr(i) = Ag_soil_rechr(i) + ag_water_maxin
             ENDIF
          ENDIF
        ENDIF
        Cap_waterin(i) = capwater_maxin*perv_frac
        Basin_capwaterin = Basin_capwaterin + DBLE( Cap_waterin(i)*harea )

! compute slow interflow and ssr_to_gw
        topfr = 0.0
        availh2o = Slow_stor(i) + Soil_to_ssr(i)
        IF ( compute_lateral==ACTIVE ) THEN
          IF ( Pref_flag == ACTIVE ) topfr = MAX( 0.0, availh2o-Pref_flow_thrsh(i) )
          ssresin = Soil_to_ssr(i) - topfr
          Slow_stor(i) = availh2o - topfr
          ! compute slow contribution to interflow, if any
          IF ( Slow_stor(i)>0.0 ) &
     &         CALL compute_interflow(Slowcoef_lin(i), Slowcoef_sq(i), &
     &                                ssresin, Slow_stor(i), Slow_flow(i))
        ELSEIF ( compute_lateral==OFF ) THEN
          Slow_stor(i) = availh2o
        ENDIF
        IF ( Slow_stor(i)>0.0 .AND. Ssr2gw_rate(i)>0.0 ) &
     &       CALL compute_gwflow(Ssr2gw_rate(i), Ssr2gw_exp(i), Ssr_to_gw(i), Slow_stor(i))

        ! compute contribution to Dunnian flow from PFR, if any; if frozen or swale don't compute Dunnian
        dunnianflw_gvr = 0.0
        IF ( cfgi_frozen_hru==OFF ) THEN
          IF ( Pref_flag == ACTIVE ) THEN
            IF ( Pref_flow_thrsh(i)>0.0 ) THEN
              availh2o = Pref_flow_stor(i) + topfr
              dunnianflw_gvr = MAX( 0.0, availh2o-Pref_flow_max(i) )
              IF ( dunnianflw_gvr>0.0 ) THEN
                topfr = topfr - dunnianflw_gvr
                IF ( topfr<0.0 ) THEN
!                  IF ( topfr<-NEARZERO .AND. Print_debug>DEBUG_less ) PRINT *, 'gvr2pfr<0', topfr, dunnianflw_gvr, &
!     &                 Pref_flow_max(i), Pref_flow_stor(i), Soil_to_ssr(i)
                  topfr = 0.0
                ENDIF
              ENDIF
              Pref_flow_in(i) = Pref_flow_infil(i) + topfr
              Pref_flow_stor(i) = Pref_flow_stor(i) + topfr
              IF ( Pref_flow_stor(i)>0.0 ) &
     &             CALL compute_interflow(Fastcoef_lin(i), Fastcoef_sq(i), &
     &                                    Pref_flow_in(i), Pref_flow_stor(i), prefflow)
            ELSEIF ( compute_lateral==ACTIVE ) THEN
              dunnianflw_gvr = topfr  !?? is this right
            ENDIF
          ELSE ! add water from slow storage to preferential flow storage when frozen, pref_flow_stor can be > pref_flow_max
            Pref_flow_in(i) = Pref_flow_infil(i) + topfr
            Pref_flow_stor(i) = Pref_flow_stor(i) + topfr
          ENDIF
          Gvr2pfr(i) = topfr
        ENDIF

        Basin_sm2gvr = Basin_sm2gvr + DBLE( Soil_to_ssr(i)*harea )
        Basin_dunnian_gvr = Basin_dunnian_gvr + DBLE( dunnianflw_gvr*harea )
        Basin_sz2gw = Basin_sz2gw + DBLE( Ssr_to_gw(i)*harea )

!******Compute actual evapotranspiration
        pervactet = 0.0
        agactet = 0.0
        ag_hruactet = 0.0
        unsatisfied_ag_et = 0.0

        IF ( cfgi_frozen_hru==OFF ) THEN
          IF ( ag_on_flag == ACTIVE ) THEN
            IF ( Iter_aet_flag == ACTIVE ) THEN
              !soilwater_deficit = MAX( sz_deficit_param, ag_soil_moist(i)/ag_soil_moist_max(i) ) irrigation happens at a deficit threshold
              ag_AETtarget = AET_external(i)
            ELSE
              ag_AETtarget = potet_sngl
            ENDIF

            ! assume canopy interception evaporation is accounted for in intcp module, so subtract here
            ! assume impervious, snow, and dprst evap not in ag fraction
            ! assume ag fraction only contains irrigated land with vegetation
            ag_avail_targetAET = ag_AETtarget - Hru_intcpevap(i)
            IF ( ag_avail_targetAET < 0.0 ) ag_avail_targetAET = 0.0

            IF ( ag_avail_targetAET > 0.0 ) THEN
              !if ( Ag_soil_moist(i) < Ag_soil_rechr(i) ) print *, 'AG1 szactet, before', i, Ag_soil_moist(i)-Ag_soil_rechr(i), &
              !     Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i)
              !if (.not.(ag_soil_moist_max(i) > 0) ) then
              !  print *, 'ag soil max', i, ag_soil_moist_max(i), ag_frac(i), ag_soil_moist(i)
              !  print *, nowyear, nowday, nowmonth
              !  ag_soil_moist_max(i) = soil_moist_max(i)
              !endif
              CALL compute_szactet(Ag_soil_moist_max(i), Ag_soil_rechr_max(i), Transp_on(i), Ag_cov_type(i), &
     &                             Ag_soil_type(i), Ag_soil_moist(i), Ag_soil_rechr(i), agactet, ag_avail_targetAET, & !?? instead of ag_avail_potet use AET_external
     &                             Snow_free(i), Ag_potet_rechr(i), Ag_potet_lower(i), &
     &                             ag_AETtarget, agfrac, Ag_soil_saturated(i), i, 1)
              !if ( Ag_soil_moist(i)< Ag_soil_rechr(i)) print *, 'AG1 szactet, after', i, Ag_soil_moist(i)-Ag_soil_rechr(i), &
                    !Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i)
              ! sanity check
              IF ( Iter_aet_flag == ACTIVE ) THEN
                IF ( agactet-ag_AETtarget>NEARZERO ) THEN
                  PRINT *, 'ag_actet issue', agactet, ag_avail_potet, agfrac, AET_external(i), ag_potet, i, hruactet
                  PRINT *, ag_AETtarget-agactet, Ag_soil_moist(i), Ag_soil_rechr(i)
                ENDIF
                IF ( ag_AETtarget<agactet ) THEN
                  PRINT *, 'WARNING, external agriculture available target AET from CBH File < computed AET', i, &
     &                     Nowyear, Nowmonth, Nowday, num_hrus_ag_iter
                  PRINT '(4(A,F0.6))', '         AET_external: ', ag_AETtarget, '; ag_actet: ', &
     &                  agactet, ' PET_external: ', PET_external(i), ' AET_external: ', AET_external(i)
                  print *, Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i)
                ENDIF
              ENDIF
              ag_hruactet = agactet*agfrac
            ENDIF
            unsatisfied_ag_et = ag_avail_targetAET - agactet
            !if ( unsatisfied_ag_et < 0.0 ) print *, 'unused ag et problem', unsatisfied_ag_et, i, soilzone_aet_converge, ag_AETtarget, agactet
            Unused_ag_et(i) = unsatisfied_ag_et
            ! assume canopy interception is accounted for in intcp module, so add here
            Ag_actet(i) = agactet + Hru_intcpevap(i)
          ELSE
            Ag_actet(i) = 0.0
          ENDIF

          avail_potet = potet_sngl - hruactet - ag_hruactet
!          IF ( avail_potet < 0.0 ) THEN
!            IF ( avail_potet<-CLOSEZERO ) &
!                 print *, 'AG avail_potet < 0', i, avail_potet, potet_sngl, Hru_impervevap(i), &
!                          Hru_intcpevap(i), Snow_evap(i), hruactet, ag_hruactet, agactet, agfrac, ag_AETtarget
!            avail_potet = 0.0
!          ENDIF
          IF ( Soil_moist(i)>0.0 .AND. avail_potet>0.0 ) THEN
            CALL compute_szactet(Soil_moist_max(i), Soil_rechr_max(i), Transp_on(i), Cov_type(i), &
     &                           Soil_type(i), Soil_moist(i), Soil_rechr(i), pervactet, avail_potet, &
     &                           Snow_free(i), Potet_rechr(i), Potet_lower(i), &
     &                           potet_sngl, perv_frac, Soil_saturated(i), i, 0)
          ENDIF
        ENDIF

        Hru_actet(i) = hruactet + pervactet*perv_frac + ag_hruactet
        hru_ag_actet(i) = ag_hruactet
        Perv_actet(i) = pervactet
        hru_perv_actet(i) = pervactet * perv_frac

! soil_moist & soil_rechr multiplied by perv_area instead of harea
        Soil_lower(i) = Soil_moist(i) - Soil_rechr(i)
        Basin_soil_moist = Basin_soil_moist + DBLE( Soil_moist(i)*perv_area )
        Basin_soil_rechr = Basin_soil_rechr + DBLE( Soil_rechr(i)*perv_area )
        Basin_perv_et = Basin_perv_et + DBLE( Perv_actet(i)*perv_area )

! if HRU cascades,
! compute interflow and excess flow to each HRU or stream
        dunnianflw = 0.0
        interflow = 0.0
        IF ( compute_lateral==ACTIVE ) THEN
          interflow = Slow_flow(i) + prefflow
          Basin_interflow_max = Basin_interflow_max + interflow*harea
          dunnianflw = dunnianflw_gvr + dunnianflw_pfr
          Dunnian_flow(i) = dunnianflw
          IF ( dunnianflw > 0.0 ) adjust_dunnian = ACTIVE
          IF ( Cascade_flag>CASCADE_OFF ) THEN
            IF ( Ncascade_hru(i)>0 ) THEN
              dnslowflow = 0.0
              dnpreflow = 0.0
              dndunn = 0.0
              IF ( interflow+dunnianflw>CLOSEZERO ) THEN
                CALL compute_cascades(i, Ncascade_hru(i), Slow_flow(i), &
     &                                prefflow, Dunnian_flow(i), dnslowflow, &
     &                                dnpreflow, dndunn)
                Basin_dninterflow = Basin_dninterflow + DBLE( (dnslowflow+dnpreflow)*harea )
                Basin_dndunnianflow = Basin_dndunnianflow + DBLE( dndunn*harea )
              ENDIF
              Hru_sz_cascadeflow(i) = dnslowflow + dnpreflow + dndunn
              Basin_dncascadeflow = Basin_dncascadeflow + DBLE( Hru_sz_cascadeflow(i)*harea )
            ENDIF
          ENDIF

! treat pref_flow as interflow
          Ssres_flow(i) = Slow_flow(i)
          IF ( Pref_flow_thrsh(i)>0.0 ) THEN
            Pref_flow(i) = prefflow
            Ssres_flow(i) = Ssres_flow(i) + prefflow
            Basin_prefflow = Basin_prefflow + DBLE( prefflow*harea )
            Basin_gvr2pfr = Basin_gvr2pfr + DBLE( Gvr2pfr(i)*harea )
          ENDIF
          Basin_ssflow = Basin_ssflow + DBLE( Ssres_flow(i)*harea )
          Basin_slowflow = Basin_slowflow + DBLE( Slow_flow(i)*harea )

! treat dunnianflw as surface runoff to streams
          Sroff(i) = Sroff(i) + Dunnian_flow(i)
          Basin_dunnian = Basin_dunnian + DBLE( Dunnian_flow(i)*harea )
          Ssres_stor(i) = Slow_stor(i)
          IF ( Pref_flag == ACTIVE ) Ssres_stor(i) = Ssres_stor(i) + Pref_flow_stor(i)

        ELSE ! for swales
          availh2o = Slow_stor(i) - Sat_threshold(i)
          Swale_actet(i) = 0.0
          IF ( availh2o>0.0 ) THEN ! if ponding, as storage > sat_threshold
            unsatisfied_et = potet_sngl - Hru_actet(i)
            IF ( unsatisfied_et>0.0 ) THEN
              availh2o = MIN ( availh2o, unsatisfied_et )
              Swale_actet(i) = availh2o
              Hru_actet(i) = Hru_actet(i) + Swale_actet(i)
              Slow_stor(i) = Slow_stor(i) - Swale_actet(i)
              Basin_swale_et = Basin_swale_et + DBLE( Swale_actet(i)*harea )
            ENDIF
            IF ( Print_debug==7 ) THEN
              IF ( Slow_stor(i)>Swale_limit(i) ) THEN
                WRITE ( DBGUNT, * ) 'Swale ponding, HRU:', i, &
     &                  ' gravity reservoir is 3*sat_threshold', Slow_stor(i), Sat_threshold(i)
                CALL print_date(DBGUNT)
              ENDIF
            ENDIF
          ENDIF
          Ssres_stor(i) = Slow_stor(i)
        ENDIF

        IF ( Soil_lower_stor_max(i)>0.0 ) Soil_lower_ratio(i) = Soil_lower(i)/Soil_lower_stor_max(i)
        Ssres_in(i) = Soil_to_ssr(i) + SNGL( gwin )
        IF ( Pref_flag == ACTIVE ) Ssres_in(i) = Ssres_in(i) + Pref_flow_infil(i)
        Basin_ssin = Basin_ssin + DBLE( Ssres_in(i)*harea )
        Basin_ssstor = Basin_ssstor + DBLE( Ssres_stor(i)*harea )
        Basin_slstor = Basin_slstor + DBLE( Slow_stor(i)*harea )
        Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*perv_frac + Ag_soil_moist(i)*agfrac
        Basin_soil_moist_tot = Basin_soil_moist_tot + DBLE( Soil_moist_tot(i)*harea )
        IF ( perv_on_flag == ACTIVE ) THEN
          Basin_cpr_stor_frac = Basin_cpr_stor_frac + DBLE( (Soil_moist(i)/Soil_moist_max(i))*perv_area )
          Basin_sz_stor_frac = Basin_sz_stor_frac + DBLE( (Soil_moist_tot(i)/Soil_zone_max(i))*harea )
          Basin_soil_lower_stor_frac = Basin_soil_lower_stor_frac + DBLE( Soil_lower_ratio(i)*perv_area )
          IF ( Soil_rechr_max(i) > 0.0 ) &
               Basin_soil_rechr_stor_frac = Basin_soil_rechr_stor_frac + DBLE( (Soil_rechr(i)/Soil_rechr_max(i))*perv_area )
        ENDIF
        IF ( Pref_flow_thrsh(i)>0.0 ) THEN
          Basin_gvr_stor_frac = Basin_gvr_stor_frac + DBLE( (Slow_stor(i)/Pref_flow_thrsh(i))*harea )
          Basin_pref_stor = Basin_pref_stor + DBLE( Pref_flow_stor(i)*harea )
          IF ( Pref_flow_max(i)>0.0 ) Basin_pfr_stor_frac = &
               Basin_pfr_stor_frac + DBLE( (Pref_flow_stor(i)/Pref_flow_max(i))*harea )
        ENDIF
        Recharge(i) = Soil_to_gw(i) + Ssr_to_gw(i)
        IF ( Dprst_flag==1 ) Recharge(i) = Recharge(i) + SNGL( Dprst_seep_hru(i) )
        Basin_recharge = Basin_recharge + DBLE( Recharge(i)*harea )
        Grav_dunnian_flow(i) = dunnianflw_gvr
        Unused_potet(i) = potet_sngl - Hru_actet(i)
        ! sanity check
!        IF ( Unused_potet(i)<-CLOSEZERO ) THEN
!          IF ( Print_debug>DEBUG_less ) THEN
!            IF ( avail_potet<-NEARZERO ) THEN
!              PRINT *, 'hru_actet>potet', i, Nowmonth, Nowday, Unused_potet(i)
!              PRINT *, Hru_actet(i), potet_sngl, ag_hruactet, pervactet*perv_frac, perv_frac, agfrac
!              print *, 'potet', potet_sngl, ag_avail_targetAET
!              PRINT *, 'hru_actet', Hru_actet(i), Hru_impervevap(i) + Hru_intcpevap(i) + Snow_evap(i), &
!     &                 Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i)
!            ENDIF
!          ENDIF
!          Hru_actet(i) = potet_sngl
!          IF ( perv_on_flag == ACTIVE ) THEN
!            tmp = Unused_potet(i)/perv_frac
!            pervactet = pervactet + tmp
!            Soil_moist(i) = Soil_moist(i) - tmp
!            Soil_rechr(i) = Soil_rechr(i) - tmp
!            IF ( Soil_rechr(i) < 0.0 ) Soil_rechr(i) = 0.0
!            IF ( Soil_moist(i) < 0.0 ) Soil_moist(i) = 0.0
!          ENDIF
!        ENDIF
!        if (perv_actet(i) < 0.0) then
!            print *, i, 'perv_actet', perv_actet(i), potet_sngl, Unused_potet(i), hru_actet(i), agactet, hruactet
!            print *, soil_moist(i), soil_moist_max(i), soil_rechr(i), soil_rechr_max(i), soil_lower(i), perv_frac
!            print *, grav_gwin(i), gvr2sm(i)
!            endif

        IF ( ag_on_flag == ACTIVE ) THEN
          Ag_soil_lower(i) = Ag_soil_moist(i) - Ag_soil_rechr(i)
          Basin_ag_soil_moist = Basin_ag_soil_moist + DBLE( Ag_soil_moist(i)*agarea )
          Basin_ag_soil_rechr = Basin_ag_soil_rechr + DBLE( Ag_soil_rechr(i)*agarea )
          Basin_ag_actet = Basin_ag_actet + DBLE( Ag_actet(i)*agarea )
          IF ( Iter_aet_flag == ACTIVE .AND. Transp_on(i) == ACTIVE ) THEN
          !IF ( Iter_aet_flag == ACTIVE ) THEN
            !agriculture_external(i)
            IF ( unsatisfied_ag_et>soilzone_aet_converge ) THEN
              Ag_soilwater_deficit(i) = (Ag_soil_moist_max(i) - Ag_soil_moist(i)) / Ag_soil_moist_max(i)
              IF ( Ag_soilwater_deficit(i)>Ag_soilwater_deficit_min(i) ) THEN
                IF ( unsatisfied_ag_et > ag_soil_moist_max(i) ) THEN
                  !print *, 'unsat>max', i, unsatisfied_ag_et, ag_soil_moist_max(i)
                  unsatisfied_max = ag_soil_moist_max(i) ! temporary fix, need better values for ag_soil_moist_max
                ELSE
                  unsatisfied_max = unsatisfied_ag_et
                  IF ( Soil_iter > 20 ) unsatisfied_max = unsatisfied_max + unsatisfied_ag_et ! this doubles small values so solution converges faster
                  keep_iterating = ACTIVE
                  add_estimated_irrigation = ACTIVE
                  num_hrus_ag_iter = num_hrus_ag_iter + 1
                ENDIF
                Ag_irrigation_add(i) = Ag_irrigation_add(i) + unsatisfied_max
                !if ( Ag_irrigation_add(i)>2.5 ) then
                !    print *, 'large irrigaion', i, Ag_irrigation_add(i), ag_AETtarget, unsatisfied_ag_et, ag_soil_moist(i), ag_soil_rechr(i), soil_iter
                !endif
!                IF ( Ag_irrigation_add(i)>max_irrigation ) max_irrigation = Ag_irrigation_add(i)
                IF ( unsatisfied_max>unsatisfied_big ) unsatisfied_big = unsatisfied_max
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        Basin_actet = Basin_actet + DBLE( Hru_actet(i)*harea )
        Hru_storage(i) = DBLE( Soil_moist_tot(i) + Hru_intcpstor(i) + Hru_impervstor(i) ) + Pkwater_equiv(i)
        IF ( Dprst_flag==ACTIVE ) Hru_storage(i) = Hru_storage(i) + Dprst_stor_hru(i)

! ***************************************
      ENDDO ! end HRU loop
! ***************************************

      Soil_iter = Soil_iter + 1
      IF ( Iter_aet_flag == OFF .OR. Basin_transp_on == OFF ) keep_iterating = OFF
      IF ( Soil_iter>max_soilzone_ag_iter .OR. add_estimated_irrigation == OFF ) keep_iterating = OFF
!      if (ag_actet(i)>potet_sngl) print *, 'ag_actet>potet', ag_actet(i), potet_sngl
!      if (unused_potet(i) < 0.00) print *, 'unused', unused_potet(i), potet_sngl
!      if ( hru_actet(i)>potet_sngl) print *, 'hru_actet', hru_actet(i), potet_sngl

! ***************************************
      ENDDO ! end iteration while loop
! ***************************************

      Soil_iter = Soil_iter - 1
      IF ( Iter_aet_flag == ACTIVE ) THEN
        print '(a,i0)', 'AET iterations: ', soil_iter
        !print '(a,F0.5)', '; maximum irrigation:', max_irrigation
        Basin_ag_irrigation_add = 0.0D0
        IF ( Basin_transp_on == ACTIVE ) THEN
          Ag_irrigation_add_vol = Ag_irrigation_add*Ag_area
          ag_AET_external_vol = ag_AET_external_vol*Ag_area
          do i = 1, nhru ! temporary to put mask in nhru_summary file
            if (ag_frac(i) > 0.0 ) then
                Basin_ag_irrigation_add = Basin_ag_irrigation_add + DBLE( Ag_irrigation_add_vol(i) )
                if ( ag_irrigation_add(i) > 0.0)then
                    if (ag_soil_moist_max(i)-ag_soil_moist(i) < 0.0001 ) then
                        print *, 'ag soil full', i, ag_soil_moist_max(i), ag_soil_moist(i), ag_irrigation_add(i), &
                                 Ag_soilwater_deficit(i)
                        print *, ag_actet(i), aet_external(i)
                    endif
                endif
            endif
          enddo
          Basin_ag_irrigation_add = Basin_ag_irrigation_add / Ag_area_total
        ENDIF
        IF ( num_hrus_ag_iter > 0 ) print '(2(A,I0))', 'number of hrus still iterating on AET: ', num_hrus_ag_iter
        if ( Soil_iter == max_soilzone_ag_iter ) then
           iter_nonconverge = iter_nonconverge + 1
           print '(/,A,I0,2("/",I0))', 'WARNING, ag AET did not converge due to max_soilzone_ag_iter: ', Nowyear, Nowmonth, Nowday
           print '(A,F0.4,2(A,I0))', '         largest AET-ag_actet: ', unsatisfied_big, '; iterations: ', Soil_iter, &
                                    '; number of non-convergence: ', iter_nonconverge
           print '(A,F0.6,A,I0,/)', '         convergence criteria: ', soilzone_aet_converge, &
                                  '; maximum iterations: ', max_soilzone_ag_iter
        ENDIF
      ENDIF
      Basin_ag_soil_moist = Basin_ag_soil_moist*Basin_area_inv
      Basin_ag_soil_rechr = Basin_ag_soil_rechr*Basin_area_inv
      Basin_actet = Basin_actet*Basin_area_inv
      Basin_ag_actet = Basin_ag_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_swale_et = Basin_swale_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_soil_moist_tot = Basin_soil_moist_tot*Basin_area_inv
      Basin_perv_to_gw = Basin_perv_to_gw*Basin_area_inv
      Basin_ag_gvr2sm = Basin_ag_gvr2sm*Basin_area_inv
      IF ( Nlake > 0 ) THEN
        Basin_lakeevap = Basin_lakeevap*Basin_area_inv
        Basin_lakeprecip = Basin_lakeprecip*Basin_area_inv
        Basin_lakeinsz = Basin_lakeinsz*Basin_area_inv
        Basin_lake_stor = Basin_lake_stor + Basin_lakeprecip - Basin_lakeevap
      ENDIF
      IF ( Pref_flag==ACTIVE ) THEN
        Basin_pref_stor = Basin_pref_stor*Basin_area_inv
        Basin_pref_flow_infil = Basin_pref_flow_infil*Basin_area_inv
        Basin_prefflow = Basin_prefflow*Basin_area_inv
        Basin_dunnian_pfr = Basin_dunnian_pfr*Basin_area_inv
        Basin_pfr_stor_frac = Basin_pfr_stor_frac*Basin_area_inv
        Basin_gvr2pfr = Basin_gvr2pfr*Basin_area_inv
      ENDIF
      Basin_dunnian_gvr = Basin_dunnian_gvr*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_ssflow = Basin_ssflow*Basin_area_inv
      Basin_interflow_max = Basin_interflow_max*Basin_area_inv
      Basin_sz2gw = Basin_sz2gw*Basin_area_inv
      Basin_ssin = Basin_ssin*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_dunnian = Basin_dunnian*Basin_area_inv
      Basin_sm2gvr = Basin_sm2gvr*Basin_area_inv
      Basin_sm2gvr_max = Basin_sm2gvr_max*Basin_area_inv
      Basin_capwaterin = Basin_capwaterin*Basin_area_inv
      Basin_cap_infil_tot = Basin_cap_infil_tot*Basin_area_inv
      Basin_ag_cap_infil_tot = Basin_ag_cap_infil_tot*Basin_area_inv
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Basin_dninterflow = Basin_dninterflow*Basin_area_inv
        Basin_dndunnianflow = Basin_dndunnianflow*Basin_area_inv
        Basin_dncascadeflow = Basin_dncascadeflow*Basin_area_inv
      ENDIF
      Basin_slowflow = Basin_slowflow*Basin_area_inv
      Basin_recharge = Basin_recharge*Basin_area_inv
      Basin_cpr_stor_frac = Basin_cpr_stor_frac*Basin_area_inv
      Basin_gvr_stor_frac = Basin_gvr_stor_frac*Basin_area_inv
      Basin_sz_stor_frac = Basin_sz_stor_frac*Basin_area_inv
      Basin_soil_lower_stor_frac = Basin_soil_lower_stor_frac*Basin_area_inv
      Basin_soil_rechr_stor_frac = Basin_soil_rechr_stor_frac*Basin_area_inv
      IF ( update_potet==ACTIVE ) THEN ! need when lakes present
        Basin_potet = 0.0D0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Basin_potet = Basin_potet + Potet(i)*Hru_area_dble(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
      ENDIF
      IF ( adjust_frozen==ACTIVE ) THEN
        Basin_hortonian = 0.0D0
        Basin_sroff = 0.0D0
        Basin_sroffp = 0.0D0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Basin_hortonian = Basin_hortonian + DBLE( Hortonian_flow(i)*Hru_area(i) )
          Basin_sroff = Basin_sroff + DBLE( Sroff(i)*Hru_area(i) )
          Basin_sroffp = Basin_sroffp + DBLE( Hru_sroffp(i)*Hru_perv(i) )
        ENDDO
        Basin_hortonian = Basin_hortonian * Basin_area_inv
        Basin_sroff = Basin_sroff * Basin_area_inv
        Basin_sroffp = Basin_sroffp * Basin_area_inv
      ELSEIF ( adjust_dunnian==ACTIVE ) THEN
        Basin_sroff = 0.0D0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Basin_sroff = Basin_sroff + DBLE( Sroff(i)*Hru_area(i) )
        ENDDO
        Basin_sroff = Basin_sroff * Basin_area_inv
      ENDIF

      END FUNCTION szrun_ag

!***********************************************************************
!     soilzone_restart_ag - write or read soilzone_ag restart file
!***********************************************************************
      SUBROUTINE soilzone_restart_ag(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, ACTIVE, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_FLOWVARS, ONLY: Pref_flow_stor, Basin_soil_rechr, Pref_flag
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Function
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=8) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_soil_rechr, Basin_slstor, Basin_soil_moist_tot, Basin_pref_stor
        IF ( Pref_flag == ACTIVE ) WRITE ( Restart_outunit ) Pref_flow_stor
        WRITE ( Restart_outunit ) Ag_soil_lower
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME_AG, module_name)
        READ ( Restart_inunit ) Basin_soil_rechr, Basin_slstor, Basin_soil_moist_tot, Basin_pref_stor
        IF ( Pref_flag == ACTIVE ) READ ( Restart_inunit ) Pref_flow_stor
        READ ( Restart_inunit ) Ag_soil_lower
      ENDIF
      END SUBROUTINE soilzone_restart_ag
