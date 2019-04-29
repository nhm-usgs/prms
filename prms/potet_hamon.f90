!***********************************************************************
! Computes the potential evapotranspiration using the Hamon
! formulation (Hamon, 1961)
!***********************************************************************
      MODULE PRMS_POTET_HAMON
        IMPLICIT NONE
        ! Local Variables
        REAL, PARAMETER :: ONE_12TH = 1.0/12.0
        CHARACTER(LEN=11), SAVE :: MODNAME
        ! Declared Parameter
        REAL, SAVE :: Hamon_coef(12)
      END MODULE PRMS_POTET_HAMON

      INTEGER FUNCTION potet_hamon()
      USE PRMS_POTET_HAMON
      USE PRMS_MODULE, ONLY: Process
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order, Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Tavgc, Basin_potet, Potet
      USE PRMS_SOLTAB, ONLY: Soltab_sunhrs
      USE PRMS_SET_TIME, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC EXP
      INTEGER, EXTERNAL :: getparam, declparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i, j
      REAL :: dyl, vpsat, vdsat, hamoncoef_mo
      CHARACTER(LEN=80), SAVE :: Version_potet
!***********************************************************************
      potet_hamon = 0

      IF ( Process(:3)=='run' ) THEN
!******Compute potential et for each HRU using Hamon formulation
        hamoncoef_mo = Hamon_coef(Nowmonth)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
! Convert daylength from hours to 12 hour multiple (equal day and night period)
          dyl = Soltab_sunhrs(Jday, i)*ONE_12TH
          vpsat = 6.108*EXP(17.26939*Tavgc(i)/(Tavgc(i)+237.3))
          vdsat = 216.7*vpsat/(Tavgc(i)+273.3)
          Potet(i) = hamoncoef_mo*dyl*dyl*vdsat
          IF ( Potet(i)<NEARZERO ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet = '$Id: potet_hamon.f90 6862 2014-10-28 21:58:22Z rsregan $'
        CALL print_module(Version_potet, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_hamon'

        IF ( declparam(MODNAME, 'hamon_coef', 'nmonths', 'real', &
     &       '0.0055', '0.004', '0.008', &
     &       'Monthly air temperature coefficient - Hamon', &
     &       'Monthly (January to December) air temperature coefficient used in Hamon potential ET computations', &
     &       'none')/=0 ) CALL read_error(1, 'hamon_coef')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'hamon_coef', 12, 'real', Hamon_coef)/=0 ) CALL read_error(2, 'hamon_coef')
      ENDIF

      END FUNCTION potet_hamon
