!***********************************************************************
! DEPRECATED
! Superceded by potet_hamon
!
! Computes the potential evapotranspiration using the Hamon
! formulation (Hamon, 1961); modification of potet_hamon_prms
!   Declared Parameters: hamon_coef, hru_radpl
!***********************************************************************
      INTEGER FUNCTION potet_hamon_prms()
      USE PRMS_MODULE, ONLY: Process, Nhru
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tavgc, Basin_potet, Potet
      USE PRMS_SOLTAB_RADPL, ONLY: Sunhrs_soltab
      USE PRMS_OBS, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, INDEX
      INTEGER, EXTERNAL :: getparam, declmodule, declparam
      EXTERNAL read_error, print_module
! Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_radpl(:)
      REAL, SAVE :: Hamon_coef(12)
! Local Variables
      INTEGER :: i, ir, j
      REAL :: dyl, vpsat, vdsat, hamoncoef_mo
      CHARACTER(LEN=16), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_potet_hamon_prms
!***********************************************************************
      potet_hamon_prms = 0

      IF ( Process(:3)=='run' ) THEN
!******Compute potential et for each HRU using Hamon formulation
        hamoncoef_mo = Hamon_coef(Nowmonth)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          ir = Hru_radpl(i)
! Convert daylength from hours to 12 hour multiple (equal day and night period)
          dyl = Sunhrs_soltab(Jday, ir)/12.0
          vpsat = 6.108*EXP(17.26939*Tavgc(i)/(Tavgc(i)+237.3))
          vdsat = 216.7*vpsat/(Tavgc(i)+273.3)
          Potet(i) = hamoncoef_mo*dyl*dyl*vdsat
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_hamon_prms =
     +'$Id: potet_hamon_prms.f 5602 2013-04-23 18:42:50Z rsregan $'
        CALL print_module(Version_potet_hamon_prms,
     +                    'Potential ET              ', 77)
        MODNAME = 'potet_hamon_prms'

        ALLOCATE ( Hru_radpl(Nhru) )
        IF ( declparam(MODNAME, 'hru_radpl', 'nhru', 'integer',
     +       '1', 'bounded', 'nradpl',
     +       'Index of radiation plane for HRU',
     +       'Index of radiation plane used to compute solar'//
     +       ' radiation for an HRU',
     +       'none')/=0 ) CALL read_error(1, 'hru_radpl')
        IF ( declparam(MODNAME, 'hamon_coef', 'nmonths', 'real',
     +       '0.0055', '0.004', '0.008',
     +       'Monthly air temperature coefficient - Hamon',
     +       'Monthly (January to December) air temperature'//
     +       ' coefficient used in Hamon potential ET computations',
     +       '????')/=0 ) CALL read_error(1, 'hamon_coef')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'hamon_coef', 12, 'real', Hamon_coef)
     +       /=0 ) CALL read_error(2, 'hamon_coef')
        IF ( getparam(MODNAME, 'hru_radpl', Nhru, 'integer', Hru_radpl)
     +       /=0 ) CALL read_error(2, 'hru_radpl') 
      ENDIF

      END FUNCTION potet_hamon_prms
