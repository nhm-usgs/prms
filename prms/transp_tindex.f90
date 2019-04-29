!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
      INTEGER FUNCTION transp_tindex()
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Version_transp_tindex, Transp_tindex_nc
      USE PRMS_BASIN, ONLY: Starttime, Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tmaxc, Temp_units, Transp_on, Basin_transp_on 
      USE PRMS_OBS, ONLY: Nowmonth, Nowday
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Transp_beg(:), Transp_end(:)
      REAL, SAVE, ALLOCATABLE :: Transp_tmax(:)
! Local Variables
      INTEGER :: mo, day, i, j, motmp
      INTEGER, SAVE, ALLOCATABLE :: span_year(:)
      INTEGER, SAVE, ALLOCATABLE :: transp_check(:), transp_end_12(:)
      REAL, SAVE :: freeze_temp
      REAL, SAVE, ALLOCATABLE :: tmax_sum(:), tmax_hru(:)
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='transp_tindex')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Transpiration Period')
!***********************************************************************
      transp_tindex = 1

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
!******sum max temp until greater than temperature index parameter,
!******at which time, turn transpiration switch on, check switch off
          IF ( transp_check(i)==1 ) THEN
            IF ( tmax_hru(i)>freeze_temp ) tmax_sum(i) = tmax_sum(i) + tmax_hru(i)
            IF ( tmax_sum(i)>Transp_tmax(i) ) THEN
              Transp_on(i) = 1
              transp_check(i) = 0
              tmax_sum(i) = 0.0
            ENDIF

!******Otherwise, check for month to turn check switch on or
!******transpiration switch off

          ELSEIF ( Nowday==1 ) THEN
            IF ( Nowmonth==Transp_beg(i) ) THEN
              transp_check(i) = 1
              IF ( tmax_hru(i)>freeze_temp ) tmax_sum(i) = tmax_sum(i) + tmax_hru(i)
!******If transpiration switch on, check for end of period
            ELSEIF ( Transp_on(i)==1 ) THEN
              IF ( span_year(i)==0 ) THEN
                IF ( Nowmonth==Transp_end(i) ) Transp_on(i) = 0
              ELSE
                IF ( motmp==transp_end_12(i) ) Transp_on(i) = 0
              ENDIF
            ENDIF
          ENDIF
          IF ( Transp_on(i)==1 ) Basin_transp_on = 1
        ENDDO

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_transp_tindex = '$Id: transp_tindex.f90 4077 2012-01-05 23:46:06Z rsregan $'
        Transp_tindex_nc = INDEX( Version_transp_tindex, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(MODNAME, PROCNAME, Version_transp_tindex(:Transp_tindex_nc))/=0 ) STOP
        ENDIF

        ALLOCATE ( Transp_beg(Nhru) )
        IF ( declparam(MODNAME, 'transp_beg', 'nhru', 'integer', &
             '4', '1', '12', &
             'Month to begin testing for transpiration', &
             'Month to begin summing the maximum air temperature for'//&
             ' each HRU; when sum is greater than or equal to'//&
             ' transp_tmax, transpiration begins', &
             'month')/=0 ) CALL read_error(1, 'transp_beg')
        ALLOCATE ( Transp_end(Nhru) )
        IF ( declparam(MODNAME, 'transp_end', 'nhru', 'integer', &
             '10', '1', '12', &
             'Month to stop transpiration period', &
             'Month to stop transpiration computations; transpiration is computed thru end of previous month', &
             'month')/=0 ) CALL read_error(1, 'transp_end')
        ALLOCATE ( Transp_tmax(Nhru) )
        IF ( declparam(MODNAME, 'transp_tmax', 'nhru', 'real', &
             '500.0', '0.0', '1000.0', &
             'Tmax index to determine start of transpiration', &
             'Temperature index to determine the specific date of the start of the transpiration period;'// &
             ' the maximum air temperature for each HRU is summed starting with the first day of month transp_beg;'// &
             ' when the sum exceeds this index, transpiration begins', &
             'degrees')/=0 ) CALL read_error(1, 'transp_tmax')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'transp_beg', Nhru, 'integer', Transp_beg)/=0 ) CALL read_error(2, 'transp_beg')
        IF ( getparam(MODNAME, 'transp_end', Nhru, 'integer', Transp_end)/=0 ) CALL read_error(2, 'transp_end')
        IF ( getparam(MODNAME, 'transp_tmax', Nhru, 'real', Transp_tmax)/=0 ) CALL read_error(2, 'transp_tmax')

        ALLOCATE ( tmax_sum(Nhru), tmax_hru(Nhru), transp_check(Nhru) )
        ALLOCATE ( transp_end_12(Nhru), span_year(Nhru) )
        tmax_sum = 0.0
        span_year = 0
        IF ( Temp_units==0 ) THEN
          freeze_temp = 32.0
        ELSE
          freeze_temp = 0.0
        ENDIF

        mo = Starttime(2)
        day = Starttime(3)
        motmp = mo + 12
        DO i = 1, Nhru
          transp_check(i) = 0
          IF ( mo==Transp_beg(i) ) THEN
            IF ( day>10 ) THEN
              Transp_on(i) = 1
            ELSE
              transp_check(i) = 1
            ENDIF
          ELSEIF ( (Transp_end(i)-Transp_beg(i))>0 ) THEN
            IF ( mo>Transp_beg(i) .AND. mo<Transp_end(i) ) Transp_on(i) = 1
          ELSE
            IF ( Transp_end(i)<Transp_beg(i) ) span_year(i) = 1
            transp_end_12(i) = Transp_end(i) + 12
            IF ( mo>Transp_beg(i) .OR. motmp<transp_end_12(i) ) Transp_on(i) = 1
          ENDIF
          IF ( Transp_on(i)==1 ) Basin_transp_on = 1
        ENDDO
      ENDIF

      transp_tindex = 0
      END FUNCTION transp_tindex

