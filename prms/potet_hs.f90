!***********************************************************************
! Computes the potential evapotranspiration using the Hargreaves and
! Samani formulation
! Hargreaves, G.H. and Z.A. Samani, 1985. Reference crop
! evapotranspiration from temperature. Transaction of ASAE 1(2):96-99.
!***********************************************************************

      INTEGER FUNCTION potet_hs()
      USE PRMS_MODULE, ONLY: Process, Nhru
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tminc, Tmaxc, Swrad, Potet_coef_hru_mo
      USE PRMS_OBS, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC SQRT, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, j, nc
      REAL :: temp_diff, coef_kt, swrad_inch_day, potet_tmp
      CHARACTER(LEN=8), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_potet_hs
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Potential ET'
!***********************************************************************
      potet_hs = 0

      IF ( Process(:3)=='run' ) THEN
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          temp_diff = Tmaxc(i) - Tminc(i) ! should be mean monthlys???
!          swrad_mm_day = Swrad(i)/23.89/2.45
!          swrad_mm_day = Swrad(i)*0.04184/2.45
          swrad_inch_day = Swrad(i)*0.000673 ! Langleys->in/day
          coef_kt = 0.00185*(temp_diff**2) - 0.0433*temp_diff + 0.4023
          potet_tmp = Potet_coef_hru_mo(i, Nowmonth)*coef_kt &
                      *swrad_inch_day*SQRT(temp_diff)*(Tavgc(i)+17.8)
          IF ( potet_tmp<0.0 ) potet_tmp = 0.0
          Basin_potet = Basin_potet + potet_tmp*Hru_area(i)
          Potet(i) = potet_tmp
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_hs = '$Id: potet_hs.f90 5203 2013-01-09 01:25:02Z rsregan $'
        nc = INDEX( Version_potet_hs, 'Z' ) + 1
        i = INDEX( Version_potet_hs, '.f90' ) + 3
        IF ( declmodule(Version_potet_hs(6:i), PROCNAME, Version_potet_hs(i+2:nc))/=0 ) STOP
        MODNAME = 'potet_hs'

      ELSEIF ( Process(:4)=='init' ) THEN
        DO i = 1, Nhru
          DO j = 1, 12 ! default coefficient = 0.0135, so update calibration coefficient
            Potet_coef_hru_mo(i, j) = Potet_coef_hru_mo(i, j)*0.0135
          ENDDO
        ENDDO
      ENDIF

      END FUNCTION potet_hs
