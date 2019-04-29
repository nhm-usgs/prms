!***********************************************************************
! Computes the potential evapotranspiration for each HRU using
! pan-evaporation data
!   Declared Parameters: hru_pansta, Epan_coef
!***********************************************************************
      MODULE PRMS_POTET_PAN
        IMPLICIT NONE
        ! Local Variables
        REAL, SAVE, ALLOCATABLE :: Last_pan_evap(:)
        CHARACTER(LEN=9), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru_pansta(:)
        REAL, SAVE, ALLOCATABLE :: Epan_coef(:)
      END MODULE PRMS_POTET_PAN

      INTEGER FUNCTION potet_pan()
      USE PRMS_POTET_PAN
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Inputerror_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet
      USE PRMS_OBS, ONLY: Nevap, Nowtime, Nowmonth, Pan_evap
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, k, j, nc
      REAL :: epancoef_mo
      CHARACTER(LEN=80), SAVE :: Version_potet_pan
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Potential ET'
!***********************************************************************
      potet_pan = 1

      IF ( Process(:3)=='run' ) THEN
        epancoef_mo = Epan_coef(Nowmonth)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          k = Hru_pansta(i)
          IF ( Pan_evap(k)<0.0 ) THEN
            IF ( Print_debug>-1 ) PRINT *, 'Pan_evap<0, set to last value', k, Pan_evap(k), Nowtime
            Pan_evap(k) = Last_pan_evap(k)
          ENDIF
          Potet(i) = Pan_evap(k)*epancoef_mo
          IF ( Potet(i)<NEARZERO ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
        Last_pan_evap = Pan_evap

      ELSEIF ( Process(:4)=='decl'  ) THEN
        Version_potet_pan = '$Id: potet_pan.f90 5169 2012-12-28 23:51:03Z rsregan $'
        nc = INDEX( Version_potet_pan, 'Z' )
        i = INDEX( Version_potet_pan, '.f90' ) + 3
        IF ( declmodule(Version_potet_pan(6:i), PROCNAME, Version_potet_pan(i+2:nc))/=0 ) STOP
        MODNAME = 'potet_pan'

        ALLOCATE ( Epan_coef(12), Hru_pansta(Nhru), Last_pan_evap(Nevap) )
        IF ( declparam(MODNAME, 'epan_coef', 'nmonths', 'real', &
             '1.0', '0.2', '3.0', &
             'Evaporation pan coefficient', &
             'Monthly (January to December) evaporation pan coefficient', &
             'none')/=0 ) CALL read_error(1, 'epan_coef')
        IF ( declparam(MODNAME, 'hru_pansta', 'nhru', 'integer', &
             '0', 'bounded', 'nevap', &
             'Index of pan evaporation station for each HRU', &
             'Index of pan evaporation station used to compute HRU potential ET', &
             'none')/=0 ) CALL read_error(1, 'hru_pansta')

      ELSEIF ( Process(:4)=='init'  ) THEN
        IF ( Nevap==0 ) THEN
          PRINT *, 'ERROR, potet_pan module selected, but nevap=0'
          STOP
        ENDIF
        Last_pan_evap = 0.0
        IF ( getparam(MODNAME, 'epan_coef', 12, 'real', Epan_coef)/=0 ) CALL read_error(2, 'epan_coef')
        IF ( getparam(MODNAME, 'hru_pansta', Nhru, 'integer', Hru_pansta)/=0 ) CALL read_error(2, 'hru_pansta')
        DO i = 1, Nhru
          IF ( Hru_pansta(i)<1 .OR. Hru_pansta(i)>Nevap ) THEN
            PRINT *, 'ERROR, hru_pansta = 0 > nevap for HRU:', i, Hru_pansta(i)
            Inputerror_flag = 1
          ENDIF
        ENDDO
      ENDIF

      END FUNCTION potet_pan
