!***********************************************************************
! Computes the potential evapotranspiration using the Priestly-Taylor
! formulation (Jensen, 1990), modification of potet_dpm.f - Mastin
!***********************************************************************
      INTEGER FUNCTION potet_pt()
      USE PRMS_MODULE, ONLY: Process, Nhru, Version_potet_pt, Potet_pt_nc
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order, Hru_elev_feet
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Swrad, Potet_coef_hru_mo
      USE PRMS_OBS, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC SQRT, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      REAL, EXTERNAL :: sat_vapor_press
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, j
      REAL :: hvap, satvapor, slpvp, prsr, psycnst, ratio, potet_tmp, eeq, temp
      CHARACTER(LEN=8), PARAMETER :: MODNAME = 'potet_pt'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Potential ET'
!***********************************************************************
      potet_pt = 1

      IF ( Process(:3)=='run' ) THEN
!***********************************************************************
!******Compute "EQUIVALENT" EVAPOTRANSPIRATION, EEQ (IN./DAY),
!...USING PRIESTLY-TAYLOR METHOD. THE VARIBLES ARE CALCULATED
!...USING FORMULAS GIVEN IN JENSEN, 1990.
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

!...SATURATION VAPOR PRESSURE AT AVG TEMP, KILOPASCALS (KPA):
          temp = Tavgc(i)
          satvapor = sat_vapor_press(temp)

!...SLOPE OF SATURATION VAPOR PRESSURE CURVE AT AVG TEMP, KPA/DEG(CELCIUS)
          slpvp = 4098.0*satvapor/(temp+237.3)**2

!...ATMOSPHERIC PRESSURE FOR ALTITUDE, KPA:
          prsr = 101.3 - 0.003215*Hru_elev_feet(i)

!...LATENT HEAT OF VAPORIZATION AT AVG TEMPERATURE, KILOJOULES/KILOGRAM:
          hvap = 2501.0 - 2.361*temp

!...PSCHOMETRIC CONSTANT AT AVG TEMPERATURE FOR ALTITUDE, KPA:
          psycnst = 1.6286*prsr/hvap

!...RATIO USED IN PRIESTLY-TAYLOR FORMULA:
          ratio = slpvp/(slpvp+psycnst)

!...COMPUTE EEQ, CM/DAY. FIRST CONVERT HVAP TO CAL/GM:
!...Swrad in units of Langleys/Day (CAL/CM**2/Day)
          hvap = hvap*0.2389
          eeq = ratio*Swrad(i)/hvap  !rsr??? need net SW, this might be wrong

!...CONVERT TO INCHES/DAY
          potet_tmp = Potet_coef_hru_mo(i, Nowmonth)*eeq/2.54
          IF ( potet_tmp<0.0 ) potet_tmp = 0.0
          Basin_potet = Basin_potet + potet_tmp*Hru_area(i)
          Potet(i) = potet_tmp
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

!******Declare parameters
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_pt = '$Id: potet_pt.f90 4630 2012-07-16 15:11:21Z rsregan $'
        Potet_pt_nc = INDEX( Version_potet_pt, 'Z' ) + 1
        i = INDEX( Version_potet_pt, '.f90' ) + 3
        IF ( declmodule(Version_potet_pt(6:i), PROCNAME, Version_potet_pt(i+2:Potet_pt_nc))/=0 ) STOP

!      ELSEIF ( Process(:4)=='init' ) THEN

      ENDIF

      potet_pt = 0
      END FUNCTION potet_pt
