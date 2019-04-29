!***********************************************************************
! DEPRECATED
! Superceded by soilzone
!
! Computes soil-moisture accounting, including addition of infiltration,
! computation of actual evapotranspiration, and seepage to subsurface
! and ground-water reservoirs
!
! Daily accounting for soil zone;
!    adds infiltration
!    computes et
!    computes recharge of soil zone
!    computes interflow to stream
!    adjusts storage in soil zone
!    computes drainage to groundwater
!***********************************************************************
      MODULE PRMS_SMBAL
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 195, DBGUNT = 895
      INTEGER, SAVE, ALLOCATABLE :: Soil2gw(:)
      DOUBLE PRECISION, SAVE :: Last_soil_moist
      CHARACTER(LEN=10), SAVE :: MODNAME
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Soil Zone'
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_soil_rechr
      REAL, SAVE, ALLOCATABLE :: Perv_actet(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Soil_type(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_init(:)
      REAL, SAVE, ALLOCATABLE :: Soil_rechr_init(:), Soil2gw_max(:)
      END MODULE PRMS_SMBAL

!***********************************************************************
!     Main SMBAL routine
!***********************************************************************
      INTEGER FUNCTION smbal_prms()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: smdecl, sminit, smrun
!***********************************************************************
      smbal_prms = 0

      IF ( Process(:3)=='run' ) THEN
        smbal_prms = smrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        smbal_prms = smdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        smbal_prms = sminit()
      ENDIF

      END FUNCTION smbal_prms

!***********************************************************************
!     smdecl - set up parameters for  soil moisture computations
!   Declared Parameters
!     soil_rechr_max, soil_rechr_init, soil_moist_max, soil_moist_init
!     soil2gw_max, soil_type, cov_type, hru_area
!***********************************************************************
      INTEGER FUNCTION smdecl()
      USE PRMS_SMBAL
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
! Local Variables
      INTEGER :: n, nc
      CHARACTER(LEN=80), SAVE :: Version_smbal_prms
!***********************************************************************
      smdecl = 1

      Version_smbal_prms =
     +'$Id: smbal_prms.f 5169 2012-12-28 23:51:03Z rsregan $'
      nc = INDEX( Version_smbal_prms, 'Z' )
      n = INDEX( Version_smbal_prms, '.f' ) + 1
      IF ( declmodule(Version_smbal_prms(6:n), PROCNAME,
     +     Version_smbal_prms(n+2:nc))/=0 ) STOP
      MODNAME = 'smbal_prms'

! Declare Variables
      IF ( declvar(MODNAME, 'basin_soil_rechr', 'one', 1, 'double',
     +     'Basin area-weighted average for soil_rechr',
     +     'inches',
     +     Basin_soil_rechr).NE.0 ) RETURN

      ALLOCATE (Perv_actet(Nhru))
      IF ( declvar(MODNAME, 'perv_actet', 'nhru', Nhru, 'real',
     +     'Actual evapotranspiration from pervious areas of HRU',
     +     'inches',
     +     Perv_actet).NE.0 ) RETURN

! Declare Parameters
      ALLOCATE (Soil_rechr_init(Nhru))
      IF ( declparam(MODNAME, 'soil_rechr_init', 'nhru', 'real',
     +     '1.0', '0.0', '10.0',
     +     'Initial value of water for soil recharge zone',
     +     'Initial value for soil recharge zone (upper part of'//
     +   ' soil_moist).  Must be less than or equal to soil_moist_init',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Soil_moist_init(Nhru))
      IF ( declparam(MODNAME, 'soil_moist_init', 'nhru', 'real',
     +     '3.0', '0.0', '20.0',
     +     'Initial values of water for soil zone',
     +     'Initial value of available water in soil profile',
     +     'inches').NE.0 ) RETURN

      ALLOCATE (Soil2gw_max(Nhru))
      IF ( declparam(MODNAME, 'soil2gw_max', 'nhru', 'real',
     +     '0.0', '0.0', '5.0',
     +     'Maximum value for soil water excess to groundwater',
     +     'The maximum amount of the soil water excess for an HRU'//
     +     ' that is routed directly to the associated groundwater'//
     +     ' reservoir each day',
     +     ' inches').NE.0 ) RETURN

      ALLOCATE (Soil_type(Nhru))
      IF ( declparam(MODNAME, 'soil_type', 'nhru', 'integer',
     +     '2', '1', '3',
     +     'HRU soil type', 'HRU soil type (1=sand; 2=loam; 3=clay)',
     +     'none').NE.0 ) RETURN

! Allocate arrays for local variables
      ALLOCATE ( Soil2gw(Nhru) )

      smdecl = 0
      END FUNCTION smdecl

!***********************************************************************
!     sminit - Initialize smbal module - get parameter values,
!              set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION sminit()
      USE PRMS_SMBAL
      USE PRMS_MODULE, ONLY: Nhru, Print_debug
      USE PRMS_BASIN, ONLY: Timestep, Hru_perv, Hru_type, NEARZERO,
     +    Basin_area_inv, Active_hrus, Hru_route_order
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_rechr_max,
     +    Basin_soil_moist, Soil_moist, Soil_rechr
      USE PRMS_OBS, ONLY: Nowyear
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, k
!***********************************************************************
      sminit = 1

      IF ( getparam(MODNAME, 'soil2gw_max', Nhru, 'real', Soil2gw_max)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'soil_type', Nhru, 'integer', Soil_type)
     +     .NE.0 ) RETURN

      IF ( Timestep==0 ) THEN
! initialize arrays (dimensioned Nhru)
        DO i = 1, Nhru
          Perv_actet(i) = 0.0
          Soil2gw(i) = 0
! do only once so restart uses saved values
          IF ( getparam(MODNAME, 'soil_moist_init', Nhru, 'real',
     +         Soil_moist_init).NE.0 ) STOP
          IF ( getparam(MODNAME, 'soil_rechr_init', Nhru, 'real',
     +         Soil_rechr_init).NE.0 ) STOP
          Soil_rechr(i) = Soil_rechr_init(i)
          Soil_moist(i) = Soil_moist_init(i)
        ENDDO
        DEALLOCATE ( Soil_rechr_init, Soil_moist_init )
      ENDIF

      IF ( Print_debug.EQ.1 ) THEN
        OPEN (BALUNT, FILE='smbal.wbal')
        WRITE (BALUNT, 9001)
      ELSEIF ( Print_debug==7 ) THEN
        OPEN (DBGUNT, FILE='smbal.dbg')
      ENDIF

      Basin_soil_moist = 0.0D0
      Basin_soil_rechr = 0.0D0

      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        !sanity checks
        IF ( Soil_rechr_max(i).GT.Soil_moist_max(i) ) THEN
          IF ( Print_debug==7 ) WRITE(DBGUNT, 9002) i,
     +         Soil_rechr_max(i), Soil_moist_max(i)
          Soil_rechr_max(i) = Soil_moist_max(i)
        ENDIF
        IF ( Soil_rechr(i)>Soil_rechr_max(i) ) THEN
          IF ( Print_debug.EQ.1 ) PRINT 9003, i, Soil_rechr(i),
     +         Soil_rechr_max(i)
          Soil_rechr(i) = Soil_rechr_max(i)
        ENDIF
        IF ( Soil_moist(i).GT.Soil_moist_max(i)) THEN
          IF ( Print_debug.EQ.1 ) PRINT 9004, i, Soil_moist(i),
     +         Soil_moist_max(i)
          Soil_moist(i) = Soil_moist_max(i)
        ENDIF
        IF ( Soil_rechr(i).GT.Soil_moist(i)) THEN
          IF ( Print_debug.EQ.1 ) PRINT 9005, i, Soil_rechr(i),
     +         Soil_moist(i)
          Soil_rechr(i) = Soil_moist(i)
        ENDIF
        !rsr, hru_perv must be > 0.0
!       IF ( Hru_type(i).EQ.2 .OR. Hru_perv(i).LT.NEARZERO ) THEN
        IF ( Hru_type(i).EQ.2 ) THEN
          Soil_rechr(i) = 0.0
          Soil_moist(i) = 0.0
        ENDIF
        IF ( Soil2gw_max(i)>NEARZERO ) Soil2gw(i) = 1
        Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*Hru_perv(i)
        Basin_soil_rechr = Basin_soil_rechr + Soil_rechr(i)*Hru_perv(i)
      ENDDO
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv

 9001 FORMAT ('    Date     Water Bal    last SM  soilmoist',
     +        '    soil in    perv ET    soil2gw   soil2ssr')
 9002 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_rechr_max > soil_moist_max,', /, 29X,
     +        'soil_rechr_max set to soil_moist_max')
 9003 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_rechr > soil_rechr_max,', /, 29X,
     +        'soil_rechr set to soil_rechr_max')
 9004 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_moist > soil_moist_max,', /, 29X,
     +        'soil_moist set to soil_moist_max')
 9005 FORMAT ('HRU:', I5, F10.4, F8.4,
     +        '  soil_rechr > soil_moist,', /, 29X,
     +        'soil_rechr set to soil_moist')

      sminit = 0
      END FUNCTION sminit

!***********************************************************************
!     smrun - Does soil water balance for each HRU, adds in infiltration
!             then computes actual et and apportions remainder between
!             subsurface and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION smrun()
      USE PRMS_SMBAL
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Dprst_flag
      USE PRMS_BASIN, ONLY: Hru_area, Hru_perv, Hru_frac_perv,
     +    Active_hrus, Hru_route_order, Basin_area_inv, Hru_type,
     +    Cov_type
      USE PRMS_CLIMATEVARS, ONLY: Transp_on, Potet
      USE PRMS_FLOWVARS, ONLY: Basin_actet, Hru_actet, Soil_to_gw,
     +    Basin_soil_to_gw, Soil_to_ssr, Basin_perv_et, Basin_lakeevap,
     +    Soil_rechr_max, Soil_moist_max, Hru_impervevap, Infil,
     +    Basin_soil_moist, Soil_moist, Hru_intcpevap, Soil_rechr
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_SNOW, ONLY: Snowcov_area, Snow_evap
      IMPLICIT NONE
      INTRINSIC MIN, ABS
      EXTERNAL compute_actet
! Local Variables
      INTEGER :: i, k
      DOUBLE PRECISION :: basin_s2ss, bsmbal, basin_infil
      REAL :: avail_potet, soil_lower, ets, etr, excs
      REAL :: perv_area, harea, soilin
!     REAL :: tmp
!***********************************************************************
      smrun = 0

      Last_soil_moist = Basin_soil_moist
      Basin_actet = 0.0D0
      Basin_soil_moist = 0.0D0
      Basin_soil_rechr = 0.0D0
      Basin_perv_et = 0.0D0
      Basin_lakeevap = 0.0D0
      Basin_soil_to_gw = 0.0D0
      basin_s2ss = 0.0D0
      basin_infil = 0.0D0

      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        harea = Hru_area(i)
        perv_area = Hru_perv(i)

        ! Soil_to_gw for whole HRU
        ! Soil_to_ssr for whole HRU
        Soil_to_gw(i) = 0.0
        Soil_to_ssr(i) = 0.0
        Hru_actet(i) = Hru_impervevap(i) + Hru_intcpevap(i) +
     +                 Snow_evap(i)

        !Hru_type can be 1 (land) or 3 (swale)
        IF ( Hru_type(i).NE.2 ) THEN

!******Add infiltration to soil and compute excess
          !rsr, note perv_area has to be > 0.0
          !infil for pervious
          soilin = Infil(i)
          IF ( soilin>0.0 ) THEN
            Soil_rechr(i) = MIN((Soil_rechr(i)+soilin),
     +                           Soil_rechr_max(i))
            excs = Soil_moist(i) + soilin
            Soil_moist(i) = MIN(excs, Soil_moist_max(i))
            excs = (excs - Soil_moist_max(i))*Hru_frac_perv(i)
            IF ( excs>0.0 ) THEN
              IF ( Soil2gw(i)==1 )
     +             Soil_to_gw(i) = MIN(Soil2gw_max(i), excs)
              Soil_to_ssr(i) = excs - Soil_to_gw(i)
            ENDIF
          ENDIF
          soil_lower = Soil_moist(i) - Soil_rechr(i)

          IF ( Print_debug.EQ.7 ) WRITE (DBGUNT, 9001) Nowmonth, Nowday,
     +         i, Infil(i), Soil_rechr(i), Soil_moist(i), soil_lower

!******Compute actual evapotranspiration

          avail_potet = Potet(i) - Hru_intcpevap(i) - Snow_evap(i)
     +                  - Hru_impervevap(i)
          CALL compute_actet(Soil_moist_max(i), Soil_rechr_max(i),
     +         Snowcov_area(i), Transp_on(i), Cov_type(i), Soil_type(i),
     +         Soil_moist(i), Soil_rechr(i), Perv_actet(i), ets, etr,
     +         avail_potet)
          IF ( Print_debug.EQ.7 ) WRITE (DBGUNT, 9001) Nowmonth, Nowday,
     +         i, ets, etr, Potet(i), avail_potet, Perv_actet(i),
     +         Soil_rechr(i), Soil_moist(i)

          Hru_actet(i) = Hru_actet(i) +
     +                   Perv_actet(i)*Hru_frac_perv(i)
!         tmp = Potet(i) - Hru_actet(i)
!         IF ( tmp<0.0 ) THEN
!           PRINT *, 'hru_actet>potet', i, Nowmonth, Nowday,
!     +               Hru_actet(i), Potet(i)
!           Hru_actet(i) = Potet(i)
!         ENDIF


! ghl1299
! soil_moist & soil_rechr multiplied by perv_area instead of harea
          Basin_soil_to_gw = Basin_soil_to_gw + Soil_to_gw(i)*harea
          Basin_soil_rechr = Basin_soil_rechr + Soil_rechr(i)*perv_area
          Basin_perv_et = Basin_perv_et + Perv_actet(i)*perv_area
          Basin_soil_moist = Basin_soil_moist + Soil_moist(i)*perv_area
          basin_s2ss = basin_s2ss + Soil_to_ssr(i)*harea
          basin_infil = basin_infil + soilin*perv_area
        ELSE ! else it is a lake or reservoir
          avail_potet = 0.0
          Hru_actet(i) = Potet(i)
          Basin_lakeevap = Basin_lakeevap + Hru_actet(i)*harea
        ENDIF

        Basin_actet = Basin_actet + Hru_actet(i)*harea

      ENDDO

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_lakeevap = Basin_lakeevap*Basin_area_inv


      IF ( Print_debug.EQ.1 ) THEN
        basin_infil = basin_infil*Basin_area_inv
        basin_s2ss = basin_s2ss*Basin_area_inv
        bsmbal = Last_soil_moist - Basin_soil_moist + basin_infil -
     +           Basin_perv_et - Basin_soil_to_gw - basin_s2ss
        WRITE (BALUNT, 9002) Nowyear, Nowmonth, Nowday, bsmbal,
     +                   Last_soil_moist, Basin_soil_moist, basin_infil,
     +                   Basin_perv_et, Basin_soil_to_gw, basin_s2ss
        IF ( ABS(bsmbal).GT.1.0E-4 ) THEN
          WRITE (BALUNT, *) 'Possible water balance error'
        ELSEIF ( ABS(bsmbal).GT.5.0E-5 ) THEN
          WRITE (BALUNT, *) 'BSM rounding issue', bsmbal
        ENDIF
      ENDIF

 9001 FORMAT (' smbal: ', 3I4, 7F8.4)
 9002 FORMAT (I5, 2('/', I2.2), 8F11.7)

      END FUNCTION smrun

!***********************************************************************
!     Compute actual evapotranspiration
!***********************************************************************
      SUBROUTINE compute_actet(Soil_moist_max, Soil_rechr_max,
     +           Snowcov_area, Transp_on, Cov_type, Soil_type,
     +           Soil_moist, Soil_rechr, Perv_actet, Ets, Etr,
     +           Avail_potet)
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Transp_on, Cov_type, Soil_type
      REAL, INTENT(IN) :: Soil_moist_max, Soil_rechr_max
      REAL, INTENT(IN) :: Snowcov_area
      REAL, INTENT(INOUT) :: Soil_moist, Soil_rechr, Avail_potet
      REAL, INTENT(OUT) :: Perv_actet, Ets, Etr
! Local Variables
      REAL, PARAMETER :: ONETHIRD = 1.0/3.0, TWOTHIRDS = 2.0/3.0
      INTEGER :: et_type
      REAL :: et, open_ground, fracs, fracr
!***********************************************************************
      Ets = 0.0
      Etr = 0.0

      open_ground = 1.0 - Snowcov_area

!******Determine if evaporation(et_type = 2) or transpiration plus
!******evaporation(et_type = 3) are active.  if not, et_type = 1

      IF ( Avail_potet.LT.NEARZERO ) THEN
        et_type = 1
        Avail_potet = 0.0
      ELSEIF ( Transp_on.EQ.0 ) THEN
        IF ( open_ground.LT.0.01 ) THEN
          et_type = 1
        ELSE
          et_type = 2
        ENDIF
      ELSEIF ( Cov_type.GT.0 ) THEN
        et_type = 3
      ELSEIF ( open_ground.LT.0.01 ) THEN
        et_type = 1
      ELSE
        et_type = 2
      ENDIF

      IF ( et_type.GT.1 ) THEN
        fracs = Soil_moist/Soil_moist_max
        fracr = Soil_rechr/Soil_rechr_max
        Ets = Avail_potet
        Etr = Avail_potet

!******sandy soil
        IF ( Soil_type.EQ.1 ) THEN
          IF ( fracs.LT.0.25 ) Ets = 0.5*fracs*Avail_potet
          IF ( fracr.LT.0.25 ) Etr = 0.5*fracr*Avail_potet
!******loam soil
        ELSEIF ( Soil_type.EQ.2 ) THEN
          IF ( fracs.LT.0.5 ) Ets = fracs*Avail_potet
          IF ( fracr.LT.0.5 ) Etr = fracr*Avail_potet
!******clay soil
        ELSEIF ( Soil_type.EQ.3 ) THEN
          IF ( fracs.LT.TWOTHIRDS .AND. fracs.GT.ONETHIRD ) THEN
            Ets = fracs*Avail_potet
          ELSEIF ( fracs.LE.ONETHIRD ) THEN
            Ets = 0.5*fracs*Avail_potet
          ENDIF
          IF ( fracr.LT.TWOTHIRDS .AND. fracr.GT.ONETHIRD ) THEN
            Etr = fracr*Avail_potet
          ELSEIF ( fracr.LE.ONETHIRD ) THEN
            Etr = 0.5*fracr*Avail_potet
          ENDIF
        ENDIF

!******Soil moisture accounting
        IF ( et_type.EQ.2 ) Etr = Etr*open_ground
        IF ( Etr.GT.Soil_rechr ) THEN
          Etr = Soil_rechr
          Soil_rechr = 0.0
        ELSE
          Soil_rechr = Soil_rechr - Etr
        ENDIF
        IF ( et_type.EQ.2 .OR. Etr.GE.Ets ) THEN
          IF ( Etr.GT.Soil_moist ) THEN
            Etr = Soil_moist
            Soil_moist = 0.0
          ELSE
            Soil_moist = Soil_moist - Etr
          ENDIF
          et = Etr
        ELSEIF ( Ets.GE.Soil_moist ) THEN
          et = Soil_moist
          Soil_moist = 0.0
          Soil_rechr = 0.0
        ELSE
          Soil_moist = Soil_moist - Ets
          et = Ets
        ENDIF
        IF ( Soil_rechr.GT.Soil_moist ) Soil_rechr = Soil_moist
      ELSE
        et = 0.0
      ENDIF
      Perv_actet = et

      END SUBROUTINE compute_actet

