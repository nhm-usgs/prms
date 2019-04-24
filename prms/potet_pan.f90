!***********************************************************************
! Computes the potential evapotranspiration for each HRU using
! pan-evaporation data
!   Declared Parameters: hru_pansta
!***********************************************************************
      INTEGER FUNCTION potet_pan()
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Version_potet_pan, Potet_pan_nc
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet
      USE PRMS_OBS, ONLY: Nevap, Nowtime, Nowmonth, Pan_evap
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_pansta(:)
      REAL, SAVE, ALLOCATABLE :: Epan_coef(:)
! Local Variables
      INTEGER :: i, k, j
      REAL :: epancoef_mo
      REAL, SAVE, ALLOCATABLE :: last_pan_evap(:)
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
            Pan_evap(k) = last_pan_evap(k)
          ENDIF
          Potet(i) = Pan_evap(k)*epancoef_mo
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
        last_pan_evap = Pan_evap

      ELSEIF ( Process(:4)=='decl'  ) THEN
        Version_potet_pan = '$Id: potet_pan.f90 3797 2011-10-25 16:43:33Z rsregan $'
        Potet_pan_nc = INDEX( Version_potet_pan, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_potet_pan(:Potet_pan_nc))/=0 ) STOP
        ENDIF

        ALLOCATE ( Epan_coef(12) )
        IF ( declparam('potet', 'epan_coef', 'nmonths', 'real', &
             '1.0', '0.2', '3.0', &
             'Evaporation pan coefficient', &
             'Monthly (January to December) evaporation pan coefficient', &
             'none')/=0 ) CALL read_error(1, 'epan_coef')
        ALLOCATE ( Hru_pansta(Nhru) )
        IF ( declparam('potet', 'hru_pansta', 'nhru', 'integer', &
             '0', 'bounded', 'nevap', &
             'Index of pan evaporation station for each HRU', &
             'Index of pan evaporation station used to compute HRU potential ET', &
             'none')/=0 ) CALL read_error(1, 'hru_pansta')

      ELSEIF ( Process(:4)=='init'  ) THEN
        IF ( Nevap==0 ) THEN
          PRINT *, 'ERROR, potet_pan module selected, but nevap=0'
          STOP
        ENDIF
        IF ( getparam('potet', 'epan_coef', 12, 'real', Epan_coef)/=0 ) CALL read_error(2, 'epan_coef')
        IF ( getparam('potet', 'hru_pansta', Nhru, 'integer', Hru_pansta)/=0 ) CALL read_error(2, 'hru_pansta')
        DO i = 1, Nhru
          IF ( Hru_pansta(i)<1 .OR. Hru_pansta(i)>Nevap ) THEN
            PRINT *, 'Warning, hru_pansta=0 or hru_pansta>nevap, set to 1 for HRU:', i
            Hru_pansta(i) = 1
          ENDIF
        ENDDO
        ALLOCATE ( last_pan_evap(Nevap) )
        last_pan_evap = 0.0
      ENDIF

      potet_pan = 0
      END FUNCTION potet_pan

