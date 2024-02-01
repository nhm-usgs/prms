!***********************************************************************
! DEPRECATED
! Superceded by soilzone
!
! Adds inflow to subsurface reservoirs and computes outflow to
! ground-water reservoirs and to streamflow
!***********************************************************************
      MODULE PRMS_SSFLOW
      IMPLICIT NONE
! Local Variables
      character(len=*), parameter :: MODDESC = 'Subsurface Computations'
      character(len=*), parameter :: MODNAME = 'ssflow'
      character(len=*), parameter :: Version_ssflow = '2024-01-31'
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_ssr2gw, Basin_soil_moist_tot, Basin_slowflow, Basin_slstor, Basin_ssin
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Ssr2gw_rate(:), Ssrmax_coef(:)
      REAL, SAVE, ALLOCATABLE :: Slowcoef_lin(:), Slowcoef_sq(:), Ssr2gw_exp(:)
      END MODULE PRMS_SSFLOW

!***********************************************************************
!     Main ssflow routine
!***********************************************************************
      INTEGER FUNCTION ssflow()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: ssdecl, ssinit, ssrun
!***********************************************************************
      ssflow = 0

      IF ( Process_flag==RUN ) THEN
        ssflow = ssrun()
      ELSEIF ( Process_flag==DECL ) THEN
        ssflow = ssdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        ssflow = ssinit()
      ENDIF

      END FUNCTION ssflow

!***********************************************************************
!     ssdecl - set up parameters for subsurface flow computations
!   Declared Parameters
!     hru_ssres, ssstor_init_frac
!     slowcoef_lin, slowcoef_sq, ssr2gw_rate, ssrmax_coef, ssr2gw_exp
!***********************************************************************
      INTEGER FUNCTION ssdecl()
      USE PRMS_SSFLOW
      USE PRMS_MODULE, ONLY: Nssr, Nhru
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL :: read_error, print_module
!***********************************************************************
      ssdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_ssflow)

! Declare Variables
       IF ( declvar(MODNAME, 'basin_ssr2gw', 'one', 1, 'double', &
     &     'Basin average drainage from soil added to groundwater', &
     &     'inches', Basin_ssr2gw)/=0 ) CALL read_error(3, 'basin_ssr2gw')

      IF ( declvar(MODNAME, 'basin_soil_moist_tot', 'one', 1, 'double', &
     &     'Basin area-weighted average total soil-zone water storage', &
     &     'inches', Basin_soil_moist_tot)/=0 ) CALL read_error(3, 'basin_soil_moist_tot')

      IF ( declvar(MODNAME, 'basin_slowflow', 'one', 1, 'double', &
     &     'Basin area-weighted average interflow from gravity reservoirs to the stream network', &
     &     'inches', Basin_slowflow)/=0 ) CALL read_error(3, 'basin_slowflow')

      IF ( declvar(MODNAME, 'basin_slstor', 'one', 1, 'double', &
     &     'Basin area-weighted average storage of gravity reservoirs', &
     &     'inches', Basin_slstor)/=0 ) CALL read_error(3, 'basin_slstor')

      IF ( declvar(MODNAME, 'basin_ssin', 'one', 1, 'double', &
     &     'Basin area-weighted average inflow to gravity and preferential-flow reservoir storage', &
     &     'inches', Basin_ssin)/=0 ) CALL read_error(3, 'basin_ssin')

! Declare Parameters

      ALLOCATE ( Slowcoef_lin(Nhru) )
      IF ( declparam(MODNAME, 'slowcoef_lin', 'nhru', 'real', &
     &     '0.015', '0.0', '1.0', &
     &     'Linear gravity-flow reservoir routing coefficient', &
     &     'Linear coefficient in equation to route gravity-reservoir storage for each HRU', &
     &     'fraction/day')/=0 ) CALL read_error(1, 'slowcoef_lin')

      ALLOCATE ( Slowcoef_sq(Nhru) )
      IF ( declparam(MODNAME, 'slowcoef_sq', 'nhru', 'real', &
     &     '0.1', '0.0', '1.0', &
     &     'Non-linear gravity-flow reservoir routing coefficient', &
     &     'Non-linear coefficient in equation to route'// &
     &     ' gravity-reservoir storage for each HRU', &
     &     'none')/=0 ) CALL read_error(1, 'slowcoef_sq')

      ALLOCATE ( Ssr2gw_rate(Nhru) )
      IF ( declparam(MODNAME, 'ssr2gw_rate', 'nssr', 'real', &
     &     '0.1', '0.0001', '999.0', &
     &     'Coefficient to route water from gravity reservoir to groundwater storage', &
     &     'Linear coefficient in equation used to route water from'// &
     &     ' the gravity reservoir to groundwater storage for each HRU', &
     &     'inches/day')/=0 ) CALL read_error(1, 'ssr2gw_rate')

      ALLOCATE ( Ssr2gw_exp(Nhru) )
      IF ( declparam(MODNAME, 'ssr2gw_exp', 'nssr', 'real', &
     &     '1.0', '0.0', '3.0', &
     &     'Coefficient to route water from subsurface to groundwater storage', &
     &     'Non-linear coefficient in equation used to route water'// &
     &     ' from the gravity reservoir to groundwater storage for each HRU', &
     &     'none')/=0 ) CALL read_error(1, 'ssr2gw_exp')

      ALLOCATE ( Ssrmax_coef(Nssr) )
      IF ( declparam(MODNAME, 'ssrmax_coef', 'nssr', 'real', &
     &     '1.0', '1.0', '20.0', &
     &     'Coefficient to route water from subsurface to groundwater', &
     &     'Coefficient in equation used to route water from the'// &
     &     ' subsurface reservoirs to the groundwater reservoirs: '// &
     &     ' ssr_to_gw = ssr2gw_rate * ((ssres_stor / ssrmax_coef)**ssr2gw_exp);'// &
     &     ' recommended value is 1.0', &
     &     'inches')/=0 ) CALL read_error(1, 'ssrmax_coef')

      END FUNCTION ssdecl

!***********************************************************************
!     ssinit - Initialize ssflow module - get parameter values,
!              compute initial values
!***********************************************************************
      INTEGER FUNCTION ssinit()
      USE PRMS_SSFLOW
      USE PRMS_MODULE, ONLY: Nssr
      USE PRMS_FLOWVARS, ONLY: Recharge
      IMPLICIT NONE
! Functions
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      ssinit = 0

      IF ( getparam(MODNAME, 'slowcoef_lin', Nssr, 'real', Slowcoef_lin)/=0 ) CALL read_error(2, 'slowcoef_lin')
      IF ( getparam(MODNAME, 'slowcoef_sq', Nssr, 'real', Slowcoef_sq)/=0 ) CALL read_error(2, 'slowcoef_sq')
      IF ( getparam(MODNAME, 'ssr2gw_rate', Nssr, 'real', Ssr2gw_rate)/=0 ) CALL read_error(2, 'ssr2gw_rate')
      IF ( getparam(MODNAME, 'ssr2gw_exp', Nssr, 'real', Ssr2gw_exp)/=0 ) CALL read_error(2, 'ssr2gw_exp')
      IF ( getparam(MODNAME, 'ssrmax_coef', Nssr, 'real', Ssrmax_coef)/=0 ) CALL read_error(2, 'ssrmax_coef')

! initialize arrays
      Recharge = 0.0

      END FUNCTION ssinit

!***********************************************************************
!     ssrun - Computes subsurface flow to streamflow and to groundwater.
!***********************************************************************
      INTEGER FUNCTION ssrun()
      USE PRMS_SSFLOW
      USE PRMS_MODULE, ONLY: Nssr
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, Hru_frac_perv, Hru_storage, &
          Ssres_area, Hru_ssres
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Ssres_flow, Ssr_to_gw, Recharge, Basin_recharge, Soil_to_gw, &
          Soil_to_ssr, Basin_ssstor, Ssres_stor, Ssres_in, Slow_stor, Slow_flow, Soil_moist_tot, Soil_moist, &
          Pkwater_equiv, Hru_impervstor
      USE PRMS_INTCP, ONLY: Hru_intcpstor
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE
      EXTERNAL :: compute_interflow, compute_gwflow
! Local Variables
      INTEGER :: i, j, k
      REAL :: srarea
!***********************************************************************
      ssrun = 0

      Ssres_in = 0.0
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        j = Hru_ssres(i)
        Ssres_in(j) = Ssres_in(j) + Soil_to_ssr(i)*Hru_area(i)
      ENDDO

      Basin_ssflow = 0.0D0
      Basin_ssstor = 0.0D0
      Basin_ssin = 0.0D0
      Basin_ssr2gw = 0.0D0
      Basin_recharge = 0.0D0
      Ssres_flow = 0.0
      Ssr_to_gw = 0.0
      DO j = 1, Nssr
        srarea = Ssres_area(j)
        Ssres_in(j) = Ssres_in(j)/srarea
        ! compute slow contribution to interflow, if any
        IF ( Ssres_stor(i)>0.0 ) &
     &       CALL compute_interflow(Slowcoef_lin(i), Slowcoef_sq(i), &
     &                              Ssres_in(j), Ssres_stor(i), Ssres_flow(i))
        IF ( Ssres_stor(i)>0.0 .AND. Ssr2gw_rate(i)>0.0 ) &
     &       CALL compute_gwflow(Ssr2gw_rate(i), Ssr2gw_exp(i), Ssr_to_gw(i), Ssres_stor(i))
        Basin_ssstor = Basin_ssstor + DBLE( Ssres_stor(i)*srarea )
        Basin_ssflow = Basin_ssflow + DBLE( Ssres_flow(i)*srarea )
        Basin_ssin = Basin_ssin + DBLE( Ssres_in(j)*srarea )
        Basin_ssr2gw = Basin_ssr2gw + DBLE( Ssr_to_gw(j)*srarea )
        Recharge(i) = Soil_to_gw(i) + Ssr_to_gw(i)
        Basin_recharge = Basin_recharge + DBLE( Recharge(i)*srarea )
        Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*Hru_frac_perv(i)
        Basin_soil_moist_tot = Basin_soil_moist_tot + DBLE( Soil_moist_tot(i)*Hru_area(i) )
        Hru_storage(i) = DBLE( Soil_moist_tot(i) + Hru_intcpstor(i) + Hru_impervstor(i) ) + Pkwater_equiv(i)
      ENDDO
      Slow_stor = Ssres_stor
      Slow_flow = Ssres_flow
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_ssflow = Basin_ssflow*Basin_area_inv
      Basin_ssin = Basin_ssin*Basin_area_inv
      Basin_ssr2gw = Basin_ssr2gw*Basin_area_inv
      Basin_slowflow = Basin_ssflow
      Basin_slstor = Basin_ssstor
      Basin_recharge = Basin_recharge*Basin_area_inv
      Basin_soil_moist_tot = Basin_soil_moist_tot*Basin_area_inv

      END FUNCTION ssrun
