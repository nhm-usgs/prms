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
        REAL, SAVE :: Epan_coef(12)
      END MODULE PRMS_POTET_PAN

      INTEGER FUNCTION potet_pan()
      USE PRMS_POTET_PAN
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Inputerror_flag, Save_vars_to_file, Model
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, NEARZERO, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet
      USE PRMS_OBS, ONLY: Nevap, Nowtime, Nowmonth, Pan_evap
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, potet_pan_restart
! Local Variables
      INTEGER :: i, k, j
      REAL :: epancoef_mo
      CHARACTER(LEN=80), SAVE :: Version_potet_pan
!***********************************************************************
      potet_pan = 0

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
        Version_potet_pan = '$Id: potet_pan.f90 5603 2013-04-23 18:43:36Z rsregan $'
        CALL print_module(Version_potet_pan, 'Potential ET              ', 90)
        MODNAME = 'potet_pan'

        IF ( Nevap==0 ) STOP 'ERROR, potet_pan module selected, but nevap=0'

        ALLOCATE ( Hru_pansta(Nhru), Last_pan_evap(Nevap) )

        IF ( Timestep/=0 ) RETURN

        IF ( declparam(MODNAME, 'epan_coef', 'nmonths', 'real', &
     &       '1.0', '0.2', '3.0', &
     &       'Evaporation pan coefficient', &
     &       'Monthly (January to December) evaporation pan coefficient', &
     &       'none')/=0 ) CALL read_error(1, 'epan_coef')
        IF ( declparam(MODNAME, 'hru_pansta', 'nhru', 'integer', &
     &       '0', 'bounded', 'nevap', &
     &       'Index of pan evaporation station for each HRU', &
     &       'Index of pan evaporation station used to compute HRU potential ET', &
     &       'none')/=0 ) CALL read_error(1, 'hru_pansta')

      ELSEIF ( Process(:4)=='init'  ) THEN
        IF ( Timestep/=0 ) THEN
          CALL potet_pan_restart(1)
          RETURN
        ENDIF

        Last_pan_evap = 0.0
        IF ( getparam(MODNAME, 'epan_coef', 12, 'real', Epan_coef)/=0 ) CALL read_error(2, 'epan_coef')
        IF ( getparam(MODNAME, 'hru_pansta', Nhru, 'integer', Hru_pansta)/=0 ) CALL read_error(2, 'hru_pansta')
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Hru_pansta(i)<1 .OR. Hru_pansta(i)>Nevap ) THEN
            PRINT *, 'ERROR, hru_pansta = 0 > nevap for HRU:', i, Hru_pansta(i)
            Inputerror_flag = 1
          ENDIF
        ENDDO

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL potet_pan_restart(0)
      ENDIF

      END FUNCTION potet_pan

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_pan_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_POTET_PAN
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=9) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Epan_coef
        WRITE ( Restart_outunit ) Hru_pansta
        WRITE ( Restart_outunit ) Last_pan_evap
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Epan_coef
        READ ( Restart_inunit ) Hru_pansta
        READ ( Restart_inunit ) Last_pan_evap
      ENDIF
      END SUBROUTINE potet_pan_restart
