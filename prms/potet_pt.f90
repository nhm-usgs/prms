!***********************************************************************
! Computes the potential evapotranspiration using the Priestly-Taylor
! formulation (Jensen, 1990), modification of potet_dpm.f - Mastin
!***********************************************************************
      MODULE PRMS_POTET_PT
        IMPLICIT NONE
        ! Local Variable
        CHARACTER(LEN=8), SAVE :: MODNAME
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Pt_alpha(:, :)
      END MODULE PRMS_POTET_PT

      INTEGER FUNCTION potet_pt()
      USE PRMS_POTET_PT
      USE PRMS_MODULE, ONLY: Process, Nhru
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, Hru_elev_feet, &
     &    NEARZERO    
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Swrad, Tavgf
      USE PRMS_SET_TIME, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC SQRT
      INTEGER, EXTERNAL :: declparam, getparam
      REAL, EXTERNAL :: sat_vapor_press
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i, j
      REAL :: hvap, satvapor, slpvp, prsr, psycnst, ratio, eeq
      CHARACTER(LEN=80), SAVE :: Version_potet
!***********************************************************************
      potet_pt = 0

      IF ( Process(:3)=='run' ) THEN
!***********************************************************************
!******Compute "EQUIVALENT" EVAPOTRANSPIRATION, EEQ (IN./DAY),
!...USING PRIESTLY-TAYLOR METHOD. THE VARIBLES ARE CALCULATED
!...USING FORMULAS GIVEN IN JENSEN, 1990.
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

!...SATURATION VAPOR PRESSURE AT AVG TEMP, KILOPASCALS (KPA):
          satvapor = sat_vapor_press(Tavgc(i))

!...SLOPE OF SATURATION VAPOR PRESSURE CURVE AT AVG TEMP, KPA/DEG(CELSIUS)
          slpvp = 4098.0*satvapor/(Tavgc(i)+237.3)**2

!...ATMOSPHERIC PRESSURE FOR ALTITUDE, KPA:
          prsr = 101.3 - 0.003215*Hru_elev_feet(i)

!...LATENT HEAT OF VAPORIZATION AT AVG TEMPERATURE, CAL/GRAM:
          hvap = 597.3 - 0.5653*Tavgf(i)

!...PSCHOMETRIC CONSTANT AT AVG TEMPERATURE FOR ALTITUDE, KPA:
          psycnst = 1.6286*prsr/hvap

!...RATIO USED IN PRIESTLY-TAYLOR FORMULA:
          ratio = slpvp/(slpvp+psycnst)

!...COMPUTE EEQ, CM/DAY
!...Swrad in units of Langleys/Day (CAL/CM**2/Day)
          eeq = ratio*Swrad(i)/hvap  !rsr??? need net SW, this might be wrong

!...CONVERT TO INCHES/DAY
          Potet(i) = Pt_alpha(i, Nowmonth)*eeq
          IF ( Potet(i)<NEARZERO ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

!******Declare parameters
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet = '$Id: potet_pt.f90 6835 2014-10-10 19:18:29Z rsregan $'
        CALL print_module(Version_potet, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_pt'

        ALLOCATE ( Pt_alpha(Nhru,12) )
        IF ( declparam(MODNAME, 'pt_alpha', 'nhru,nmonths', 'real', &
     &       '1.26', '1.0', '2.0', &
     &       'Potential ET adjustment factor - Priestly-Taylor', &
     &       'Monthly (January to December) adjustment factor used in Priestly-Taylor potential ET computations for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'pt_alpha')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'pt_alpha', Nhru*12, 'real', Pt_alpha)/=0 ) CALL read_error(2, 'pt_alpha')
        ! divide coefficient by 2.54 (inches/cm) so only done once.
        DO i = 1, Nhru
          DO j = 1, 12
            Pt_alpha(i, j) = Pt_alpha(i, j)/2.54
          ENDDO
        ENDDO

      ENDIF

      END FUNCTION potet_pt
