!***********************************************************************
! DEPRECATED
! Superceded by soilzone
!
! Adds inflow to subsurface reservoirs and computes outflow to
! ground-water reservoirs and to streamflow
!***********************************************************************
      MODULE PRMS_SSFLOW
      IMPLICIT NONE
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_ssin, Basin_ssr2gw
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Ssr2gw_rate(:), Ssrmax_coef(:)
      REAL, SAVE, ALLOCATABLE :: Ssrcoef_lin(:), Ssrcoef_sq(:)
      REAL, SAVE, ALLOCATABLE :: Ssstor_init(:), Ssr2gw_exp(:)
!   Local Variables
      CHARACTER(LEN=11), PARAMETER :: MODNAME = 'ssflow_prms'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Soil Zone'
      END MODULE PRMS_SSFLOW    

!***********************************************************************
!     Main soilzone routine
!***********************************************************************
      INTEGER FUNCTION ssflow_prms()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: ssdecl, ssinit, ssrun
!***********************************************************************
      ssflow_prms = 0

      IF ( Process(:3)=='run' ) THEN
        ssflow_prms = ssrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        ssflow_prms = ssdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        ssflow_prms = ssinit()
      ENDIF

      END FUNCTION ssflow_prms

!***********************************************************************
!     ssdecl - set up parameters for subsurface flow computations
!   Declared Parameters
!     hru_ssres, ssstor_init
!     ssrcoef_lin, ssrcoef_sq, ssr2gw_rate, ssrmax_coef, ssr2gw_exp
!***********************************************************************
      INTEGER FUNCTION ssdecl()
      USE PRMS_SSFLOW
      USE PRMS_MODULE, ONLY: Model, Nhru, Nssr, Version_ssflow_prms,
     +    Ssflow_prms_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
! Local Variable
      INTEGER:: n
!***********************************************************************
      ssdecl = 1

      Version_ssflow_prms =
     +'$Id: ssflow_prms.f 4773 2012-08-22 23:32:17Z rsregan $'
      Ssflow_prms_nc = INDEX( Version_ssflow_prms, 'Z' )
      n = INDEX( Version_ssflow_prms, '.f' ) + 1
      IF ( declmodule(Version_ssflow_prms(6:n), PROCNAME,
     +     Version_ssflow_prms(n+2:Ssflow_prms_nc))/=0 ) STOP

! Declare Variables
      IF ( declvar(MODNAME, 'basin_ssin', 'one', 1, 'double',
     +     'Basin weighted average for inflow to subsurface reservoirs',
     +     'inches',
     +     Basin_ssin).NE.0 ) RETURN

       IF ( declvar(MODNAME, 'basin_ssr2gw', 'one', 1, 'double',
     +     'Basin average drainage from soil added to groundwater',
     +     'inches',
     +     Basin_ssr2gw).NE.0 ) RETURN

! Declare Parameters
      ALLOCATE (Ssstor_init(Nssr))
      IF ( declparam(MODNAME, 'ssstor_init', 'nssr', 'real',
     +     '0.0', '0.0', '20.0',
     +     'Initial storage in each subsurface reservoir',
     +     'Initial storage in each subsurface reservoir;'//
     +     ' estimated based on measured flow',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Ssrcoef_lin(Nssr))
      IF ( declparam(MODNAME, 'ssrcoef_lin', 'nssr', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Linear subsurface routing coefficient',
     +     'Coefficient to route subsurface storage to streamflow'//
     +     ' using the following equation: '//
     +     ' ssres_flow = ssrcoef_lin * ssres_stor +'//
     +     ' ssrcoef_sq * ssres_stor**2',
     +     '1/day').NE.0 ) RETURN

      ALLOCATE (Ssrcoef_sq(Nssr))
      IF ( declparam(MODNAME, 'ssrcoef_sq', 'nssr', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Non-linear subsurface routing coefficient',
     +     'Coefficient to route subsurface storage to streamflow'//
     +     ' using the following equation: '//
     +     ' ssres_flow = ssrcoef_lin * ssres_stor +'//
     +     ' ssrcoef_sq * ssres_stor**2',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Ssr2gw_rate(Nssr))
      IF ( declparam(MODNAME, 'ssr2gw_rate', 'nssr', 'real',
     +     '0.1', '0.0', '1.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Coefficient in equation used to route water from the'//
     +     ' subsurface reservoirs to the groundwater reservoirs: '//
     +     ' ssr_to_gw = ssr2gw_rate *'//
     +     ' ((ssres_stor / ssrmax_coef)**ssr2gw_exp)',
     +     '1/day').NE.0 ) RETURN

      ALLOCATE (Ssrmax_coef(Nssr))
      IF ( declparam(MODNAME, 'ssrmax_coef', 'nssr', 'real',
     +     '1.0', '1.0', '20.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Coefficient in equation used to route water from the'//
     +     ' subsurface reservoirs to the groundwater reservoirs: '//
     +     ' ssr_to_gw = ssr2gw_rate *'//
     +     ' ((ssres_stor / ssrmax_coef)**ssr2gw_exp);'//
     +     ' recommended value is 1.0',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Ssr2gw_exp(Nssr))
      IF ( declparam(MODNAME, 'ssr2gw_exp', 'nssr', 'real',
     +     '1.0', '0.0', '3.0',
     +     'Coefficient to route water from subsurface to groundwater',
     +     'Coefficient in equation used to route water from the'//
     +     ' subsurface reservoirs to the groundwater reservoirs: '//
     +     ' ssr_to_gw = ssr2gw_rate * '//
     +     ' ((ssres_stor / ssrmax_coef)**ssr2gw_exp);'//
     +     ' recommended value is 1.0',
     +     'none').NE.0 ) RETURN

      ssdecl = 0
      END FUNCTION ssdecl

!***********************************************************************
!     ssinit - Initialize ssflow module - get parameter values,
!              compute initial values
!***********************************************************************
      INTEGER FUNCTION ssinit()
      USE PRMS_SSFLOW
      USE PRMS_MODULE, ONLY: Nhru, Nssr
      USE PRMS_FLOWVARS, ONLY: Basin_ssstor, Ssres_stor
      USE PRMS_BASIN, ONLY: Timestep, Ssres_area, Hru_ssres, Hru_type,
     +    Basin_area_inv, NEARZERO, Active_hrus, Hru_route_order
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, j, k
!***********************************************************************
      ssinit = 1

      IF ( getparam(MODNAME, 'ssrcoef_lin', Nssr, 'real', Ssrcoef_lin)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'ssrcoef_sq', Nssr, 'real', Ssrcoef_sq)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'ssr2gw_rate', Nssr, 'real', Ssr2gw_rate)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'ssr2gw_exp', Nssr, 'real', Ssr2gw_exp)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'ssrmax_coef', Nssr, 'real', Ssrmax_coef)
     +     .NE.0 ) RETURN

! do only once so restart uses saved values
      IF ( Timestep==0 ) THEN
! initialize scalers
        Basin_ssin = 0.0D0
        Basin_ssr2gw = 0.0D0
! initialize arrays (dimensioned Nssr)
        IF ( getparam(MODNAME, 'ssstor_init', Nssr, 'real',
     +       Ssstor_init).NE.0 ) STOP
        Ssres_stor = Ssstor_init
        DEALLOCATE ( Ssstor_init )
      ENDIF

      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        j = Hru_ssres(i)
        IF ( Hru_type(i)==2 ) THEN
          ! assume if hru_type is 2, SSR has same area as HRU
          IF ( Ssres_stor(j).GT.0.0 ) THEN
            PRINT *, 'Warning, ssres_stor>0 for lake HRU:', j,
     +               Ssres_stor(j)
            Ssres_stor(j) = 0.0
          ENDIF
        ELSEIF ( Ssrcoef_lin(j)<NEARZERO ) THEN
          PRINT *, 'HRU:', j, Ssrcoef_lin(j),
     +             ' ssrcoef_lin <= 0.0, it is set to last HRU value'
          IF ( j>1 ) THEN
            Ssrcoef_lin(j) = Ssrcoef_lin(j-1)
          ELSE
            Ssrcoef_lin(j) = 0.1
          ENDIF
        ENDIF
      ENDDO

      Basin_ssstor = 0.0D0
      DO j = 1, Nssr
        Basin_ssstor = Basin_ssstor + Ssres_stor(j)*Ssres_area(j)
      ENDDO
      Basin_ssstor = Basin_ssstor*Basin_area_inv

      ssinit = 0
      END FUNCTION ssinit

!***********************************************************************
!     ssrun - Computes subsurface flow to streamflow and to
!             groundwater.
!***********************************************************************
      INTEGER FUNCTION ssrun()
      USE PRMS_SSFLOW
      USE PRMS_MODULE, ONLY: Nhru, Nssr
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Hru_ssres, Ssres_area, Basin_area_inv
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Ssres_flow, Ssr_to_gw,
     +    Soil_to_ssr, Basin_ssstor, Ssres_stor, Ssres_in
      IMPLICIT NONE
      EXTERNAL inter_gw_flow
! Local Variables
      INTEGER :: i, j, k
      REAL :: srarea
!***********************************************************************
      ssrun = 1

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
      DO j = 1, Nssr
        Ssres_flow(j) = 0.0
        Ssr_to_gw(j) = 0.0
        srarea = Ssres_area(j)

        Ssres_in(j) = Ssres_in(j)/srarea

        IF ( Ssres_stor(j).GT.0.0 .OR. Ssres_in(j).GT.0.0 ) THEN
          CALL inter_gw_flow(Ssres_stor(j), Ssrcoef_lin(j),
     +         Ssrcoef_sq(j), Ssr2gw_rate(j), Ssr2gw_exp(j),
     +         Ssrmax_coef(j), Ssres_in(j), Ssres_flow(j), Ssr_to_gw(j))
          Basin_ssstor = Basin_ssstor + Ssres_stor(j)*srarea
          Basin_ssflow = Basin_ssflow + Ssres_flow(j)*srarea
          Basin_ssin = Basin_ssin + Ssres_in(j)*srarea
          Basin_ssr2gw = Basin_ssr2gw + Ssr_to_gw(j)*srarea
        ENDIF

      ENDDO

      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_ssflow = Basin_ssflow*Basin_area_inv
      Basin_ssin = Basin_ssin*Basin_area_inv
      Basin_ssr2gw = Basin_ssr2gw*Basin_area_inv

      ssrun = 0
      END FUNCTION ssrun

!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE inter_gw_flow(Storage, Coef_lin, Coef_sq, Ssr2gw_rate,
     +                         Ssr2gw_exp, Ssrmax_coef, Input,
     +                         Inter_flow, Ssr_to_gw)
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      INTRINSIC EXP, SQRT
! Arguments
      REAL, INTENT(IN) :: Coef_lin, Coef_sq, Input
      REAL, INTENT(IN) :: Ssr2gw_rate, Ssr2gw_exp, Ssrmax_coef
      REAL, INTENT(INOUT) :: Storage
      REAL, INTENT(OUT) :: Inter_flow, Ssr_to_gw
! Local Variables
      REAL, PARAMETER :: SMALL = 0.000001
      REAL :: c1, c2, c3, sos, availh2o
!***********************************************************************

      availh2o = Storage + Input
      IF ( availh2o.GT.0.0 ) THEN

!******compute interflow
        IF ( Coef_lin.LT.NEARZERO .AND. Input.LE.0.0 ) THEN
          c1 = Coef_sq*Storage
          Inter_flow = Storage*(c1/(1.0+c1))
        ELSEIF ( Coef_sq.LT.NEARZERO ) THEN
          c2 = 1.0 - EXP(-Coef_lin)
          Inter_flow = Input*(1.0-c2/Coef_lin) + Storage*c2
        ELSE
          c3 = SQRT(Coef_lin**2.0+4.0*Coef_sq*Input)
          sos = Storage - ((c3-Coef_lin)/(2.0*Coef_sq))
          c1 = Coef_sq*sos/c3
          c2 = 1.0 - EXP(-c3)
          Inter_flow = Input + (sos*(1.0+c1)*c2)/(1.0+c1*c2)
        ENDIF

        IF ( Inter_flow.LT.0.0 ) THEN
!         PRINT *, 'Sanity check, interflow<0.0', Inter_flow
          Inter_flow = 0.0
        ELSEIF ( Inter_flow.GT.availh2o ) THEN
          Inter_flow = availh2o
        ENDIF
        Storage = availh2o - Inter_flow
        IF ( Storage.LT.0.0 ) THEN
          PRINT *, 'Sanity check, ssres_stor<0.0', Storage
          Storage = 0.0
! rsr, if very small storage, add it to interflow
        ELSEIF ( Storage.LT.SMALL ) THEN
          Inter_flow = Inter_flow + Storage
          Storage = 0.0
        ENDIF
      ENDIF

!******compute flow to groundwater
      IF ( Storage.GT.0.0 .AND. Ssr2gw_rate.GT.NEARZERO ) THEN
        Ssr_to_gw = Ssr2gw_rate*((Storage/Ssrmax_coef)**Ssr2gw_exp)
        IF ( Ssr_to_gw.GT.Storage ) Ssr_to_gw = Storage
        IF ( Ssr_to_gw.LT.0.0 ) Ssr_to_gw = 0.0
        Storage = Storage - Ssr_to_gw
      ELSE
        Ssr_to_gw = 0.0
      ENDIF

      END SUBROUTINE inter_gw_flow

