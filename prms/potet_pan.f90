!***********************************************************************
! Computes the potential evapotranspiration for each HRU using
! pan-evaporation data
!   Declared Parameters: hru_pansta, epan_coef
!***********************************************************************
      MODULE PRMS_POTET_PAN
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
        character(len=9), parameter :: MODNAME = 'potet_pan'
        character(len=*), parameter :: Version_potet = '2024-01-29'
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Last_pan_evap(:)
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru_pansta(:)
      END MODULE PRMS_POTET_PAN

      INTEGER FUNCTION potet_pan()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ACTIVE, OFF, DEBUG_less, ERROR_dim, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Nevap, Nhru, Print_debug, Save_vars_to_file, Init_vars_from_file, Nowmonth
      USE PRMS_POTET_PAN
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area_dble, Hru_route_order, Hru_order_flag
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Epan_coef
      USE PRMS_OBS, ONLY: Pan_evap
      IMPLICIT NONE
! Functions
      EXTERNAL :: read_error, print_module, potet_pan_restart, print_date, error_stop
      INTRINSIC :: DBLE
      INTEGER, EXTERNAL :: declparam, getparam
! Local Variables
      INTEGER :: i, j, k
!***********************************************************************
      potet_pan = 0

      IF ( Process_flag==RUN ) THEN
        DO i = 1, Nevap
          IF ( Pan_evap(i)<0.0D0 ) THEN
            IF ( Print_debug>DEBUG_less ) THEN
              PRINT *, 'Pan_evap<0, set to last value, station:', i, '; value:', Pan_evap(i)
              CALL print_date(1)
            ENDIF
            Pan_evap(i) = Last_pan_evap(i)
          ENDIF
        ENDDO

        Basin_potet = 0.0D0
        IF ( Hru_order_flag == ACTIVE ) THEN
          DO i = 1, Nhru
            Potet(i) = Pan_evap(k)*Epan_coef(i, Nowmonth)
            IF ( Potet(i)<0.0D0 ) Potet(i) = 0.0D0
            Basin_potet = Basin_potet + Potet(i)*Hru_area_dble(i)
          ENDDO
        ELSE
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            k = Hru_pansta(i)
            Potet(i) = Pan_evap(k)*Epan_coef(i, Nowmonth)
            IF ( Potet(i)<0.0D0 ) Potet(i) = 0.0D0
            Basin_potet = Basin_potet + Potet(i)*Hru_area_dble(i)
          ENDDO
        ENDIF
        Basin_potet = Basin_potet*Basin_area_inv
        Last_pan_evap = Pan_evap

!******Declare parameters
      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_potet)

        IF ( Nevap==0 ) CALL error_stop('potet_pan module selected, but nevap=0', ERROR_dim)
        ALLOCATE ( Last_pan_evap(Nevap) )

        ALLOCATE ( Hru_pansta(Nhru) )
        IF ( declparam(MODNAME, 'hru_pansta', 'nhru', 'integer', &
     &       '0', 'bounded', 'nevap', &
     &       'Index of pan evaporation station for each HRU', &
     &       'Index of pan evaporation station used to compute HRU potential ET', &
     &       'none') /= 0 ) CALL read_error(1, 'hru_pansta')

      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) THEN
          CALL potet_pan_restart(READ_INIT)
        ELSE
          Last_pan_evap = 0.0D0
        ENDIF
        IF ( getparam(MODNAME, 'hru_pansta', Nhru, 'integer', Hru_pansta) /= 0 ) CALL read_error(2, 'hru_pansta')

      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL potet_pan_restart(SAVE_INIT)

      ENDIF

      END FUNCTION potet_pan

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_pan_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, text_restart_flag
      USE PRMS_POTET_PAN, ONLY: MODNAME, Last_pan_evap
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=9) :: module_name
!***********************************************************************
      IF ( In_out == SAVE_INIT ) THEN
        IF ( text_restart_flag == OFF ) THEN
          WRITE ( Restart_outunit ) MODNAME
          WRITE ( Restart_outunit ) Last_pan_evap
        ELSE
          WRITE ( Restart_outunit, * ) MODNAME
          WRITE ( Restart_outunit, * ) Last_pan_evap
        ENDIF
      ELSE
        IF ( text_restart_flag == OFF ) THEN
          READ ( Restart_inunit ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit ) Last_pan_evap
        ELSE
          READ ( Restart_inunit, * ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit, * ) Last_pan_evap
        ENDIF
      ENDIF
      END SUBROUTINE potet_pan_restart
