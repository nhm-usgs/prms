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
      USE PRMS_MODULE, ONLY: Process, Nhru
      USE PRMS_BASIN, ONLY: Start_month, Start_day, Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tmaxc, Temp_units, Transp_on, Basin_transp_on 
      USE PRMS_OBS, ONLY: Nowmonth, Nowday
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, j, motmp, nc
      REAL, SAVE, ALLOCATABLE :: tmax_hru(:)
      CHARACTER(LEN=80), SAVE :: Version_transp_tindex
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Transpiration Period'
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
        Version_transp_tindex = '$Id: transp_tindex.f90 5169 2012-12-28 23:51:03Z rsregan $'
        nc = INDEX( Version_transp_tindex, 'Z' )
        i = INDEX( Version_transp_tindex, '.f90' ) + 3
        IF ( declmodule(Version_transp_tindex(6:i), PROCNAME, Version_transp_tindex(i+2:nc))/=0 ) STOP
        MODNAME = 'transp_tindex'

        ALLOCATE ( Transp_beg(Nhru), Transp_end(Nhru), Transp_tmax(Nhru) )
        ALLOCATE ( Tmax_sum(Nhru), Transp_check(Nhru), Transp_end_12(Nhru), Span_year(Nhru) )
        IF ( declparam(MODNAME, 'transp_beg', 'nhru', 'integer', &
             '4', '1', '12', &
             'Month to begin testing for transpiration', &
             'Month to begin summing the maximum air temperature for each HRU; when sum is greater than or'// &
             ' equal to transp_tmax, transpiration begins', &
             'month')/=0 ) CALL read_error(1, 'transp_beg')
        IF ( declparam(MODNAME, 'transp_end', 'nhru', 'integer', &
             '10', '1', '12', &
             'Month to stop transpiration period', &
             'Month to stop transpiration computations; transpiration is computed thru end of previous month', &
             'month')/=0 ) CALL read_error(1, 'transp_end')
        IF ( declparam(MODNAME, 'transp_tmax', 'nhru', 'real', &
             '500.0', '0.0', '1000.0', &
             'Tmax index to determine start of transpiration', &
             'Temperature index to determine the specific date of the start of the transpiration period;'// &
             ' the maximum air temperature for each HRU is summed starting with the first day of month transp_beg;'// &
             ' when the sum exceeds this index, transpiration begins', &
             'degrees')/=0 ) CALL read_error(1, 'transp_tmax')

      ELSEIF ( Process(:4)=='init' ) THEN
        ALLOCATE ( tmax_hru(Nhru) )
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
          Transp_check(i) = 0
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
            Transp_end_12(i) = Transp_end(i) + 12
            IF ( Start_month>Transp_beg(i) .OR. motmp<Transp_end_12(i) ) Transp_on(i) = 1
          ENDIF
          IF ( Transp_on(i)==1 ) Basin_transp_on = 1
        ENDDO
      ENDIF

      END FUNCTION transp_tindex
