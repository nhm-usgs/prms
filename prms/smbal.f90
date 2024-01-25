!***********************************************************************
! DEPRECATED
! Superceded by soilzone
!
! Computes soil-moisture accounting, including addition of infiltration,
! computation of actual evapotranspiration, and seepage to subsurface
! and ground-water reservoirs
! no cascading flow, no frozen ground, or depression storage
!
! Daily accounting for soil zone;
!    adds infiltration
!    computes et
!    computes recharge of soil zone
!    adjusts storage in soil zone
!    computes drainage to groundwater
!***********************************************************************
      MODULE PRMS_SMBAL
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Soilzone Computations'
      character(len=*), parameter :: MODNAME = 'smbal'
      character(len=*), parameter :: Version_smbal = '2024-01-04'
      INTEGER, SAVE :: DBGUNT
      INTEGER, SAVE, ALLOCATABLE :: Soil2gw(:), Soil_saturated(:)
      REAL, SAVE, ALLOCATABLE :: Snow_free(:), Potet_rechr(:), Potet_lower(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_lakeprecip
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Soil_type(:)
      REAL, SAVE, ALLOCATABLE :: Soil2gw_max(:)
      REAL, SAVE, ALLOCATABLE :: Lake_evap_adj(:, :)
      END MODULE PRMS_SMBAL

!***********************************************************************
!     Main SMBAL routine
!***********************************************************************
      INTEGER FUNCTION smbal()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: smdecl, sminit, smrun
!***********************************************************************
      smbal = 0

      IF ( Process_flag==RUN ) THEN
        smbal = smrun()
      ELSEIF ( Process_flag==DECL ) THEN
        smbal = smdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        smbal = sminit()
      ENDIF

      END FUNCTION smbal

!***********************************************************************
!     smdecl - set up parameters for soil moisture computations
!   Declared Parameters
!     soil_rechr_max, soil_rechr_init_frac, soil_moist_max, soil_moist_init_frac
!     soil2gw_max, soil_type, cov_type, hru_area
!***********************************************************************
      INTEGER FUNCTION smdecl()
      USE PRMS_SMBAL
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Print_debug
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL :: read_error, print_module, PRMS_open_module_file
!***********************************************************************
      smdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_smbal)

! Declare Variables and Parameters
      IF ( Nlake>0 ) THEN

      ALLOCATE ( Potet_lower(Nhru) )
      ALLOCATE ( Potet_rechr(Nhru) )
      ALLOCATE ( Snow_free(Nhru) )
      ALLOCATE ( Soil_saturated(Nhru) )

      IF ( Print_debug==7 ) CALL PRMS_open_module_file(DBGUNT, 'soilzone.dbg')

        IF ( declvar(MODNAME, 'basin_lakeprecip', 'one', 1, 'double', &
     &       'Basin area-weighted average precipitation on lake HRUs', &
     &       'inches', Basin_lakeprecip)/=0 ) CALL read_error(3, 'basin_lakeprecip')
        ALLOCATE ( Lake_evap_adj(12,Nlake) )
        IF ( declparam(MODNAME, 'lake_evap_adj', 'nmonths,nlake', 'real', &
     &       '1.0', '0.5', '1.5', &
     &       'Monthly potet factor to adjust potet on lakes', &
     &       'Monthly (January to December) multiplicative adjustment factor for potential ET for each lake', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'lake_evap_adj')
      ENDIF
      ALLOCATE ( Soil2gw_max(Nhru) )
      IF ( declparam(MODNAME, 'soil2gw_max', 'nhru', 'real', &
     &     '0.0', '0.0', '5.0', &
     &     'Maximum value for capillary reservoir excess to groundwater storage', &
     &     'Maximum amount of the capillary reservoir excess that'// &
     &     ' is routed directly to groundwater storage for each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'soil2gw_max')

      ALLOCATE ( Soil_type(Nhru) )
      IF ( declparam(MODNAME, 'soil_type', 'nhru', 'integer', &
     &     '2', '1', '3', &
     &     'HRU soil type', 'Soil type of each HRU (1=sand; 2=loam; 3=clay)', &
     &     'none')/=0 ) CALL read_error(1, 'soil_type')

! Allocate arrays for local variables
      ALLOCATE ( Soil2gw(Nhru) )

      END FUNCTION smdecl

!***********************************************************************
!     sminit - Initialize smbal module - get parameter values,
!              set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION sminit()
      USE PRMS_CONSTANTS, ONLY: NEARZERO
      USE PRMS_SMBAL
      USE PRMS_MODULE, ONLY: Nhru, Nlake
      IMPLICIT NONE
! Functions
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      sminit = 0

      IF ( getparam(MODNAME, 'soil_type', Nhru, 'integer', Soil_type)/=0 ) CALL read_error(2, 'soil_type')
      IF ( getparam(MODNAME, 'soil2gw_max', Nhru, 'real', Soil2gw_max)/=0 ) CALL read_error(2, 'soil2gw_max')
      IF ( Nlake>0 ) THEN
        IF ( getparam(MODNAME, 'lake_evap_adj', 12*Nlake, 'real', Lake_evap_adj)/=0 ) &
     &       CALL read_error(2, 'lake_evap_adj')
      ENDIF

      END FUNCTION sminit

!***********************************************************************
!     smrun - Does soil water balance for each HRU, adds in infiltration
!             then computes actual et and apportions remainder between
!             subsurface and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION smrun()
      USE PRMS_CONSTANTS, ONLY: LAKE, DEBUG_less, ACTIVE, OFF
      USE PRMS_SMBAL
      USE PRMS_MODULE, ONLY: Nlake, Nowmonth, Nowday, Print_debug
      USE PRMS_BASIN, ONLY: Hru_area_dble, Hru_perv, Hru_frac_perv, Lake_hru_id, &
     &    Active_hrus, Hru_route_order, Basin_area_inv, Hru_type, Cov_type
      USE PRMS_CLIMATEVARS, ONLY: Transp_on, Potet, Hru_ppt, Basin_potet
      USE PRMS_FLOWVARS, ONLY: Basin_actet, Hru_actet, Soil_to_gw, Basin_soil_rechr, Cap_infil_tot, &
     &    Basin_soil_to_gw, Soil_to_ssr, Basin_perv_et, Basin_lakeevap, Perv_actet, Basin_cap_infil_tot, &
     &    Soil_rechr_max, Soil_moist_max, Infil, Basin_soil_moist, Soil_moist, Soil_rechr
      USE PRMS_SOILZONE, ONLY: Basin_sz2gw, Cap_waterin, Basin_capwaterin, Soil_lower, Basin_lake_stor, Basin_sm2gvr
      USE PRMS_INTCP, ONLY: Hru_intcpevap
      USE PRMS_SNOW, ONLY: Snowcov_area, Snow_evap
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap
      IMPLICIT NONE
      INTRINSIC :: MIN, ABS, DBLE
      EXTERNAL :: compute_soilmoist, compute_szactet
! Local Variables
      INTEGER :: i, k, update_potet
      REAL :: avail_potet, gvr_maxin
      REAL :: perv_area, harea, hruactet, perv_frac, capwater_maxin, pervactet !, tmp
!***********************************************************************
      smrun = 0

      Basin_actet = 0.0D0
      Basin_soil_moist = 0.0D0
      Basin_soil_rechr = 0.0D0
      Basin_perv_et = 0.0D0
      Basin_cap_infil_tot = 0.0D0
      Basin_soil_to_gw = 0.0D0
      Basin_sz2gw = 0.0D0
      Basin_capwaterin = 0.0D0
      Basin_sm2gvr = 0.0D0
      IF ( Nlake>0 ) THEN
        Basin_lakeprecip = 0.0D0
        Basin_lakeevap = 0.0D0
      ENDIF
      ! Soil_to_gw and Soil_to_ssr for whole HRU
      Soil_to_gw = 0.0
      Soil_to_ssr = 0.0
      Cap_waterin = 0.0
      Soil_saturated = OFF
      Potet_rechr = 0.0
      Potet_lower = 0.0
      Cap_infil_tot = 0.0
      Snow_free = 1.0 - Snowcov_area
      update_potet = OFF

! ***************************************
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
! ***************************************
        hruactet = Hru_impervevap(i) + Hru_intcpevap(i) + Snow_evap(i)
        harea = Hru_area(i)

        IF ( Hru_type(i)==LAKE ) THEN ! lake or reservoir
          !WARNING, RSR, if hru_actet>water in lake, then budget error
          hruactet = (sngl(Potet(i)) - hruactet)*Lake_evap_adj(Nowmonth,Lake_hru_id(i))
          IF ( hruactet>sngl(Potet(i)) ) THEN
            IF ( Print_debug > DEBUG_less ) THEN
              PRINT *, 'WARNING, lake evap > potet, for HRU:', i, ' potential ET increased to adjusted lake ET'
              PRINT *, hruactet, Potet(i), hruactet - Potet(i)
            ENDIF
            Potet(i) = dble(hruactet) ! this could be a problem when it happens
            update_potet = ACTIVE
          ENDIF
          Basin_actet = Basin_actet + DBLE( hruactet*harea )
          Basin_lakeevap = Basin_lakeevap + DBLE( hruactet*harea )
          Basin_lakeprecip = Basin_lakeprecip + Hru_ppt(i)*Hru_area_dble(i)
          Hru_actet(i) = hruactet
          CYCLE
        ENDIF

        !Hru_type can be 1 (land) or 3 (swale)
        ! perv_frac has to be > 0.00001
        perv_area = Hru_perv(i)
        perv_frac = Hru_frac_perv(i)

        avail_potet = sngl(Potet(i)) - hruactet
        IF ( avail_potet<0.0 ) THEN
!          IF ( avail_potet<-CLOSEZERO ) &
!               print *, 'avail_potet<0', i, avail_potet, Potet(i), Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i), hruactet
          avail_potet = 0.0
          hruactet = sngl(Potet(i))
        ENDIF

!******Add infiltration to soil and compute excess
        !infil_tot is the depth in whole HRU
        !capillary reservoir for pervious area

        ! perv_frac has to be > 0.00001
        ! infil for pervious portion of HRU
        capwater_maxin = Infil(i)
        Cap_infil_tot(i) = capwater_maxin*perv_frac
        Basin_cap_infil_tot = Basin_cap_infil_tot + DBLE( Cap_infil_tot(i)*harea )

!******Add infiltration to soil and compute excess
        gvr_maxin = 0.0

        ! call even if capwater_maxin = 0, just in case soil_moist now > Soil_moist_max
        IF ( capwater_maxin+Soil_moist(i)>0.0 ) THEN
          CALL compute_soilmoist(capwater_maxin, Soil_moist_max(i), &
     &         Soil_rechr_max(i), Soil2gw_max(i), gvr_maxin, &
     &         Soil_moist(i), Soil_rechr(i), Soil_to_gw(i), perv_frac)
          Cap_waterin(i) = capwater_maxin*perv_frac
          Basin_capwaterin = Basin_capwaterin + DBLE( Cap_waterin(i)*harea )
          Basin_soil_to_gw = Basin_soil_to_gw + DBLE( Soil_to_gw(i)*harea )
          Soil_to_ssr(i) = gvr_maxin
        ENDIF

        soil_lower = Soil_moist(i) - Soil_rechr(i)

          IF ( Print_debug==7 ) WRITE (DBGUNT, 9001) Nowmonth, Nowday, &
               i, Infil(i), Soil_rechr(i), Soil_moist(i), Soil_lower(i)

        Basin_sm2gvr = Basin_sm2gvr + DBLE( Soil_to_ssr(i)*harea )

!******Compute actual evapotranspiration
        pervactet = 0.0
          IF ( Soil_moist(i)>0.0 .AND. avail_potet>0.0 ) THEN
            CALL compute_szactet(Soil_moist_max(i), Soil_rechr_max(i), Transp_on(i), Cov_type(i), &
     &                           Soil_type(i), Soil_moist(i), Soil_rechr(i), pervactet, avail_potet, &
     &                           Snow_free(i), Potet_rechr(i), Potet_lower(i), &
     &                           Potet(i), perv_frac, Soil_saturated(i), i)
          ! sanity check
!          IF ( pervactet>avail_potet ) THEN
!            Soil_moist(i) = Soil_moist(i) + pervactet - avail_potet
!            pervactet = avail_potet
!            PRINT *, 'perv_et problem', pervactet, Avail_potet
!          ENDIF
          ENDIF
          IF ( Print_debug==7 ) WRITE (DBGUNT, 9001) Nowmonth, Nowday, &
                i, Potet(i), avail_potet, Perv_actet(i), Soil_rechr(i), Soil_moist(i)

        Hru_actet(i) = hruactet + pervactet*perv_frac
        avail_potet = sngl(Potet(i)) - Hru_actet(i)
        ! sanity check
!        IF ( avail_potet<0.0 ) THEN
!          IF ( Print_debug>DEBUG_less ) THEN
!            IF ( avail_potet<-NEARZERO ) PRINT *, 'hru_actet>potet', i, &
!     &           Nowmonth, Nowday, Hru_actet(i), Potet(i), avail_potet
!          ENDIF
!           Hru_actet(i) = Potet(i)
!          tmp = avail_potet/perv_frac
!          pervactet = pervactet + tmp
!          Soil_moist(i) = Soil_moist(i) - tmp
!          Soil_rechr(i) = Soil_rechr(i) - tmp
!          IF ( Soil_rechr(i)<0.0 ) Soil_rechr(i) = 0.0
!          IF ( Soil_moist(i)<0.0 ) Soil_moist(i) = 0.0
!         ENDIF
        Perv_actet(i) = pervactet

! soil_moist & soil_rechr multiplied by perv_area instead of harea
        Soil_lower(i) = Soil_moist(i) - Soil_rechr(i)
        Basin_soil_moist = Basin_soil_moist + DBLE( Soil_moist(i)*perv_area )
        Basin_soil_rechr = Basin_soil_rechr + DBLE( Soil_rechr(i)*perv_area )
        Basin_perv_et = Basin_perv_et + DBLE( Perv_actet(i)*perv_area )

      Basin_actet = Basin_actet + DBLE( Hru_actet(i)*harea )
 9001 FORMAT (' smbal: ', 3I4, 7F8.4)

! ***************************************
      ENDDO ! end HRU loop
! ***************************************

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      IF ( Nlake>0 ) THEN
        Basin_lakeevap = Basin_lakeevap*Basin_area_inv
        Basin_lakeprecip = Basin_lakeprecip*Basin_area_inv
        Basin_lake_stor = Basin_lake_stor + Basin_lakeprecip - Basin_lakeevap
      ENDIF
      Basin_sm2gvr = Basin_sm2gvr*Basin_area_inv
      Basin_capwaterin = Basin_capwaterin*Basin_area_inv
      Basin_cap_infil_tot = Basin_cap_infil_tot*Basin_area_inv
      IF ( update_potet==ACTIVE ) THEN ! need when lakes present
        Basin_potet = 0.0D0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Basin_potet = Basin_potet + Potet(i)*Hru_area_dble(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
      ENDIF

      END FUNCTION smrun

