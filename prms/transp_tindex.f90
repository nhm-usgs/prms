!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
      MODULE PRMS_TRANSP_TINDEX
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE, ALLOCATABLE :: Span_year(:), Transp_check(:), Transp_end_12(:)
        REAL, SAVE :: Freeze_temp
        REAL, SAVE, ALLOCATABLE :: Tmax_sum(:)
        CHARACTER(LEN=13), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Transp_beg(:), Transp_end(:)
        REAL, SAVE, ALLOCATABLE :: Transp_tmax(:)
      END MODULE PRMS_TRANSP_TINDEX

      INTEGER FUNCTION transp_tindex()
      USE PRMS_TRANSP_TINDEX
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Start_month, Start_day, Active_hrus, Hru_route_order, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tmaxc, Temp_units, Transp_on, Basin_transp_on 
      USE PRMS_OBS, ONLY: Nowmonth, Nowday
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, transp_tindex_restart
! Local Variables
      INTEGER :: i, j, motmp
      REAL, SAVE, ALLOCATABLE :: tmax_hru(:)
      CHARACTER(LEN=80), SAVE :: Version_transp_tindex
!***********************************************************************
      transp_tindex = 0

      IF ( Process(:3)=='run' ) THEN
!******Set switch for active transpiration period
        IF ( Temp_units==0 ) THEN
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            tmax_hru(i) = Tmaxf(i)
          ENDDO
        ELSE
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            tmax_hru(i) = Tmaxc(i)
          ENDDO
        ENDIF

        Basin_transp_on = 0
        motmp = Nowmonth + 12
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

!******If in checking period, then for each day
!******sum maximum temperature until greater than temperature index parameter,
!******at which time, turn transpiration switch on, check switch off
          IF ( Transp_check(i)==1 ) THEN
            IF ( tmax_hru(i)>Freeze_temp ) Tmax_sum(i) = Tmax_sum(i) + tmax_hru(i)
            IF ( Tmax_sum(i)>Transp_tmax(i) ) THEN
              Transp_on(i) = 1
              Transp_check(i) = 0
              Tmax_sum(i) = 0.0
            ENDIF

!******Otherwise, check for month to turn check switch on or
!******transpiration switch off

          ELSEIF ( Nowday==1 ) THEN
            IF ( Nowmonth==Transp_beg(i) ) THEN
              Transp_check(i) = 1
              IF ( tmax_hru(i)>Freeze_temp ) Tmax_sum(i) = Tmax_sum(i) + tmax_hru(i)
!******If transpiration switch on, check for end of period
            ELSEIF ( Transp_on(i)==1 ) THEN
              IF ( Span_year(i)==0 ) THEN
                IF ( Nowmonth==Transp_end(i) ) Transp_on(i) = 0
              ELSE
                IF ( motmp==Transp_end_12(i) ) Transp_on(i) = 0
              ENDIF
            ENDIF
          ENDIF
          IF ( Transp_on(i)==1 ) Basin_transp_on = 1
        ENDDO

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_transp_tindex = '$Id: transp_tindex.f90 5594 2013-04-23 18:29:59Z rsregan $'
        CALL print_module(Version_transp_tindex, 'Transpiration Period      ', 90)
        MODNAME = 'transp_tindex'

        ALLOCATE ( Transp_beg(Nhru), Transp_end(Nhru), Transp_tmax(Nhru) )
        ALLOCATE ( Tmax_sum(Nhru), Transp_check(Nhru), Transp_end_12(Nhru), Span_year(Nhru) )

        IF ( Timestep/=0 ) RETURN

        IF ( declparam(MODNAME, 'transp_beg', 'nhru', 'integer', &
     &       '4', '1', '12', &
     &       'Month to begin testing for transpiration', &
     &       'Month to begin summing the maximum air temperature for each HRU; when sum is greater than or'// &
     &       ' equal to transp_tmax, transpiration begins', &
     &       'month')/=0 ) CALL read_error(1, 'transp_beg')
        IF ( declparam(MODNAME, 'transp_end', 'nhru', 'integer', &
     &       '10', '1', '13', &
     &       'Month to stop transpiration period', &
     &       'Month to stop transpiration computations; transpiration is computed thru end of previous month', &
     &       'month')/=0 ) CALL read_error(1, 'transp_end')
        IF ( declparam(MODNAME, 'transp_tmax', 'nhru', 'real', &
     &       '500.0', '0.0', '1000.0', &
     &       'Tmax index to determine start of transpiration', &
     &       'Temperature index to determine the specific date of the start of the transpiration period;'// &
     &       ' the maximum air temperature for each HRU is summed starting with the first day of month transp_beg;'// &
     &       ' when the sum exceeds this index, transpiration begins', &
     &       'degrees')/=0 ) CALL read_error(1, 'transp_tmax')

      ELSEIF ( Process(:4)=='init' ) THEN
        ALLOCATE ( tmax_hru(Nhru) )

        IF ( Timestep/=0 ) THEN
          CALL transp_tindex_restart(1)
          RETURN
        ENDIF

        IF ( getparam(MODNAME, 'transp_beg', Nhru, 'integer', Transp_beg)/=0 ) CALL read_error(2, 'transp_beg')
        IF ( getparam(MODNAME, 'transp_end', Nhru, 'integer', Transp_end)/=0 ) CALL read_error(2, 'transp_end')
        IF ( getparam(MODNAME, 'transp_tmax', Nhru, 'real', Transp_tmax)/=0 ) CALL read_error(2, 'transp_tmax')

        Tmax_sum = 0.0
        Span_year = 0
        IF ( Temp_units==0 ) THEN
          Freeze_temp = 32.0
        ELSE
          Freeze_temp = 0.0
        ENDIF

        motmp = Start_month + 12
        DO i = 1, Nhru
          Transp_end_12 = Transp_end(i) + 12
        ENDDO
        Transp_check = 0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Start_month==Transp_beg(i) ) THEN
            IF ( Start_day>10 ) THEN
              Transp_on(i) = 1
            ELSE
              Transp_check(i) = 1
            ENDIF
          ELSEIF ( (Transp_end(i)-Transp_beg(i))>0 ) THEN
            IF ( Start_month>Transp_beg(i) .AND. Start_month<Transp_end(i) ) Transp_on(i) = 1
          ELSE
            IF ( Transp_end(i)<Transp_beg(i) ) Span_year(i) = 1
            IF ( Start_month>Transp_beg(i) .OR. motmp<Transp_end_12(i) ) Transp_on(i) = 1
          ENDIF
          IF ( Transp_on(i)==1 ) Basin_transp_on = 1
        ENDDO

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL transp_tindex_restart(0)
      ENDIF

      END FUNCTION transp_tindex

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE transp_tindex_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_TRANSP_TINDEX
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Freeze_temp
        WRITE ( Restart_outunit ) Span_year
        WRITE ( Restart_outunit ) Transp_check
        WRITE ( Restart_outunit ) Transp_end_12
        WRITE ( Restart_outunit ) Tmax_sum
        WRITE ( Restart_outunit ) Transp_beg
        WRITE ( Restart_outunit ) Transp_end
        WRITE ( Restart_outunit ) Transp_tmax
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Freeze_temp
        READ ( Restart_inunit ) Span_year
        READ ( Restart_inunit ) Transp_check
        READ ( Restart_inunit ) Transp_end_12
        READ ( Restart_inunit ) Tmax_sum
        READ ( Restart_inunit ) Transp_beg
        READ ( Restart_inunit ) Transp_end
        READ ( Restart_inunit ) Transp_tmax
      ENDIF
      END SUBROUTINE transp_tindex_restart
