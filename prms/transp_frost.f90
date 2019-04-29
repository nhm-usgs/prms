!***********************************************************************
! Determine whether transpiration is occurring. Transpiration is based
! on time between the last spring and the first fall killing frost.
!***********************************************************************
      MODULE PRMS_TRANSP_FROST
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=12), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Fall_frost(:), Spring_frost(:)
      END MODULE PRMS_TRANSP_FROST

      INTEGER FUNCTION transp_frost()
      USE PRMS_TRANSP_FROST
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Transp_on, Basin_transp_on
      USE PRMS_OBS, ONLY: Jsol
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, transp_frost_restart
! Local Variables
      INTEGER :: i, j
      CHARACTER(LEN=80), SAVE :: Version_transp_frost
!***********************************************************************
      transp_frost = 0

      IF ( Process(:3)=='run' ) THEN
!******Set switch for active transpiration period
! If the current solar day is between the last frost of the
! spring and the first frost of the fall, then transpiration
! is on for the HRU. If any HRU is transpiring, then
! Basin_transp_on is set to 1.
        Basin_transp_on = 0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Jsol>=Spring_frost(i) .AND. Jsol<=Fall_frost(i) ) THEN
            Transp_on(i) = 1
            Basin_transp_on = 1
          ELSE
            Transp_on(i) = 0
          ENDIF
        ENDDO

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_transp_frost = '$Id: transp_frost.f90 5594 2013-04-23 18:29:59Z rsregan $'
        CALL print_module(Version_transp_frost, 'Transpiration Period      ', 90)
        MODNAME = 'transp_frost'

        ALLOCATE ( Spring_frost(Nhru), Fall_frost(Nhru) )

        IF ( Timestep/=0 ) RETURN

        IF ( declparam(MODNAME, 'spring_frost', 'nhru', 'integer', &
     &       '111', '1', '366', &
     &       'The solar date (number of days after winter solstice) of the last killing frost of the spring', &
     &       'The solar date (number of days after winter solstice) of the last killing frost of the spring', &
     &       'Solar date')/=0 ) CALL read_error(1, 'spring_frost')
        IF ( declparam(MODNAME, 'fall_frost', 'nhru', 'integer', &
     &       '264', '1', '366', &
     &       'The solar date (number of days after winter solstice) of the first killing frost of the fall', &
     &       'The solar date (number of days after winter solstice) of the first killing frost of the fall', &
     &       'Solar date')/=0 ) CALL read_error(1, 'fall_frost')

      ELSEIF ( Process(:4)=='init' ) THEN

        IF ( Timestep/=0 ) THEN
          CALL transp_frost_restart(1)
          RETURN
        ENDIF

        IF ( getparam(MODNAME, 'spring_frost', Nhru, 'integer', Spring_frost)/=0 ) CALL read_error(2, 'spring_frost')
        IF ( getparam(MODNAME, 'fall_frost', Nhru, 'integer', Fall_frost)/=0 ) CALL read_error(2, 'fall_frost')

        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Jsol>=Spring_frost(i) .AND. Jsol<=Fall_frost(i) ) THEN
            Transp_on(i) = 1
            Basin_transp_on = 1
          ENDIF
        ENDDO

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL transp_frost_restart(0)
      ENDIF

      END FUNCTION transp_frost

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE transp_frost_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_TRANSP_FROST
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=12) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Fall_frost
        WRITE ( Restart_outunit ) Spring_frost
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Fall_frost
        READ ( Restart_inunit ) Spring_frost
      ENDIF
      END SUBROUTINE transp_frost_restart
