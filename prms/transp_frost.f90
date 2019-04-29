!***********************************************************************
! Determine whether transpiration is occurring. Transpiration is based
! on time between the last spring and the first fall killing frost.
!***********************************************************************
      INTEGER FUNCTION transp_frost()
      USE PRMS_MODULE, ONLY: Process, Nhru, Version_transp_frost, Transp_frost_nc
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Transp_on, Basin_transp_on
      USE PRMS_OBS, ONLY: Jsol
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Fall_frost(:), Spring_frost(:)
! Local Variables
      INTEGER :: i, j
      CHARACTER(LEN=12), PARAMETER :: MODNAME = 'transp_frost'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Transpiration Period'
!***********************************************************************
      transp_frost = 1

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
        Version_transp_frost = '$Id: transp_frost.f90 4231 2012-02-29 21:08:30Z rsregan $'
        Transp_frost_nc = INDEX( Version_transp_frost, 'Z' )
        i = INDEX( Version_transp_frost, '.f90' ) + 3
        IF ( declmodule(Version_transp_frost(6:i), PROCNAME, Version_transp_frost(i+2:Transp_frost_nc))/=0 ) STOP

        ALLOCATE ( Spring_frost(Nhru) )
        IF ( declparam(MODNAME, 'spring_frost', 'nhru', 'integer', &
             '111', '1', '366', &
             'The solar date (number of days after winter solstice) of the last killing frost of the spring', &
             'The solar date (number of days after winter solstice) of the last killing frost of the spring', &
             'Solar date')/=0 ) CALL read_error(1, 'spring_frost')
        ALLOCATE ( Fall_frost(Nhru) )
        IF ( declparam(MODNAME, 'fall_frost', 'nhru', 'integer', &
             '264', '1', '366', &
             'The solar date (number of days after winter solstice) of the first killing frost of the fall', &
             'The solar date (number of days after winter solstice) of the first killing frost of the fall', &
             'Solar date')/=0 ) CALL read_error(1, 'fall_frost')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'spring_frost', Nhru, 'integer', Spring_frost)/=0 ) CALL read_error(2, 'spring_frost')
        IF ( getparam(MODNAME, 'fall_frost', Nhru, 'integer', Fall_frost)/=0 ) CALL read_error(2, 'fall_frost')

        DO i = 1, Nhru
          IF ( Jsol>=Spring_frost(i) .AND. Jsol<=Fall_frost(i) ) THEN
            Transp_on(i) = 1    
            Basin_transp_on = 1
          ENDIF
        ENDDO
      ENDIF

      transp_frost = 0
      END FUNCTION transp_frost
 