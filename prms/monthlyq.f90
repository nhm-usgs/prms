!***********************************************************************
!    Aggregates values of routed flow at each node into monthly values
!    to be printed to individual files as either mean monthly cfs or
!    monthly accumulated acre-feet
!    Mark Mastin, USGS July,1999
!
!    monthlyq provides the user the option for:
!    1)listing either average monthly cfs or monthly accumulated
!      runoff (sum_flag parameter);
!    2)listing in acre feet, hundreds of acre feet, or thousands of 
!      acre feet (units_flag parameter); and
!    3)selecting which nodes that will have individual files of monthly
!      summaries of discharge or runoff (Segment_flag parameter).
!
!    This program assumes that the streamflow routing is being used.
!***********************************************************************
      MODULE PRMS_MONTHLYQ
        IMPLICIT NONE
! Local Variables
        character(len=*), parameter :: MODDESC = 'Compute Monthly Flow'
        character(len=*), parameter :: MODNAME = 'monthlyq'
        character(len=*), parameter :: Version_monthlyq = '2024-01-25'
        INTEGER, SAVE :: Cnt, Oldyear, Oldmonth, Out_unit
        INTEGER, SAVE, ALLOCATABLE :: Active_segments(:), Counter(:)
        DOUBLE PRECISION, SAVE :: Conv_fac
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Monavg(:, :), Cum(:, :), Maxq(:, :), Minq(:, :), Avg(:, :)
! Declared Parameters
        INTEGER, SAVE :: Sum_flag, Units_flag
        INTEGER, SAVE, ALLOCATABLE :: Segment_flag(:)
      END MODULE PRMS_MONTHLYQ

      SUBROUTINE monthlyq()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN
      USE PRMS_MONTHLYQ
      USE PRMS_MODULE, ONLY: Process_flag, Nsegment, Nsegment, Starttime, Nowmonth, Nowyear
      USE PRMS_FLOWVARS, ONLY: Seg_inflow
      IMPLICIT NONE
! Functions
      INTRINSIC :: INDEX, CHAR
      INTEGER, EXTERNAL :: declparam, getparam, get_ftnunit
      EXTERNAL :: read_error, getdimname, aggFlows, PrintFlows, print_module
	  EXTERNAL :: PRMS_open_output_file, error_stop
! Local Variables
      INTEGER :: i, j, k, len
      CHARACTER(LEN = 30) :: dimname
!***********************************************************************
      IF ( Process_flag == RUN ) THEN
        IF ( Nowmonth /= Oldmonth  ) THEN
          CALL aggFlows( Oldmonth )
!     check for change in water year (Month = October)
          IF ( Nowmonth == 10 ) THEN
            CALL aggFlows( 13 )
            CALL PrintFlows( )
            Oldyear = Nowyear
          ENDIF
          Oldmonth = Nowmonth
        ENDIF
        DO i = 1, Cnt
          j = Active_segments(i)
          Cum(Nowmonth, j) = Cum(Nowmonth, j) + Seg_inflow(j)*Conv_fac
          Cum(13, j) = Cum(13, j) + Seg_inflow(j)*Conv_fac
        ENDDO

      ELSEIF ( Process_flag == DECL ) THEN
        CALL print_module( MODDESC, MODNAME, Version_monthlyq )

        ALLOCATE ( Counter(13), Monavg(13,Nsegment), Cum(13,Nsegment), Avg(13,Nsegment) )
        ALLOCATE ( Maxq(13,Nsegment), Minq(13,Nsegment), Active_segments(Nsegment) )

        IF ( declparam(MODNAME, 'sum_flag', 'one', 'integer', &
             '1', '0', '1', &
             'Flag to select units of mean monthly discharge for each segment (0=cfs; 1=acre-feet)', &
             'Flag to select units of mean monthly discharge for each segment (0=cfs; 1=acre-feet)', &
             'none')/=0 ) CALL read_error(1, 'sum_flag')

        IF ( declparam(MODNAME, 'units_flag', 'one', 'integer', &
             '2', '1', '3', &
             'Flag for units of mean monthly discharge in acre-feet'// &
             ' (1=acre-feet; 2=hundreds of acre feet; 3=thousands of acre feet)', &
             'Flag for units of mean monthly discharge in acre-feet'// &
             ' (1=acre-feet; 2=hundreds of acre feet; 3=thousands of acre feet)', &
             'none') /= 0 ) CALL read_error( 1, 'units_flag' )

        ALLOCATE ( Segment_flag(Nsegment) )
        IF ( declparam(MODNAME, 'segment_flag', 'nsegment', 'integer', &
             '0', '0', '1', &
             'Flag to indicate whether to write or not write results for each segment (0=no; 1=yes)', &
             'Flag to indicate whether to write or not write results for each segment (0=no; 1=yes)', &
             'none') /= 0 ) CALL read_error( 1, 'segment_flag' )

      ELSEIF ( Process_flag == INIT ) THEN
        IF ( getparam(MODNAME, 'segment_flag', Nsegment, 'integer', Segment_flag) /= 0 ) CALL read_error( 2, 'segment_flag' )
        Active_segments = 0
        Cnt = 0
        DO i = 1, Nsegment
          IF ( Segment_flag(i) == 1  ) THEN
            Cnt = Cnt + 1
            Active_segments(Cnt) = i
          ENDIF
        ENDDO

        Out_unit = get_ftnunit( 531 )
        DO i = 1, Cnt
           j = Active_segments(i)

           CALL getdimname('nsegment', j, Out_unit)

           len = INDEX( dimname, CHAR( 0 ) ) - 1
           CALL PRMS_open_output_file( Out_unit, dimname(:len)//'.out', MODNAME, 0, ios )
           IF ( ios /= 0 ) CALL error_stop( 'in '//MODNAME, ERROR_open_out )
           WRITE ( Out_unit+i, '(40X,A,/)' ) dimname(:len)

           IF ( Sum_flag == 0 ) THEN
             WRITE ( Out_unit+i, '(20X,A)' ) 'MONTHLY MEAN DISCHARGE, IN CUBIC FEET PER SECOND'
           ELSEIF ( Units_flag == 1  ) THEN
             WRITE ( Out_unit+i, '(25X,A)' ) 'MONTHLY ACCUMULATED RUNOFF, IN ACRE FEET'
           ELSEIF ( Units_flag == 2  ) THEN
             WRITE ( Out_unit+i, '(17X,A)' ) 'MONTHLY ACCUMULATED RUNOFF, IN HUNDREDS OF ACRE FEET'
           ELSE
             WRITE ( Out_unit+i, '(17X,A)' ) 'MONTHLY ACCUMULATED RUNOFF, IN THOUSANDS OF ACRE FEET'
           ENDIF
           WRITE ( Out_unit+i, '(/,A,/)' ) '   Year     Oct     Nov     Dec     Jan     Feb     Mar     Apr'// &
                                           '     May     Jun     Jul     Aug     Sep   Total'
        ENDDO

        IF ( getparam(MODNAME, 'sum_flag', 1, 'integer', Sum_flag) /= 0 ) CALL read_error( 2, 'sum_flag' )
        IF ( getparam(MODNAME, 'units_flag', 1, 'integer', Units_flag) /= 0 ) CALL read_error( 2, 'units_flag' )

        IF ( Sum_flag == 0 ) THEN
          Conv_fac = 1.0D0
        ELSEIF ( Units_flag == 1 ) THEN
          Conv_fac = 1.9835D0
        ELSEIF ( Units_flag == 2 ) THEN
          Conv_fac = 1.9835D-2
        ELSE
          Conv_fac = 1.9835D-3
        ENDIF

        Counter = 0
        Monavg = 0.0D0
        Cum = 0.0D0
        Avg = 0.0D0

        Maxq = 0.0D0
        Minq = 1.0D+10

        Oldyear = Starttime(1)
        Oldmonth = Starttime(2)


      ELSEIF ( Process_flag == CLEAN ) THEN
        CALL aggFlows( Oldmonth )
        IF ( Oldmonth == 9 ) CALL aggFlows(13)

        CALL PrintFlows( )

        DO i = 1, 13
          DO j = 1, Nsegment
            IF ( Counter(i) > 0 ) Avg(i, j) = Avg(i, j)/Counter(i)
          ENDDO
        ENDDO

        DO i = 1, Cnt
          j = Active_segments(i)
          WRITE ( Out_unit+i, 100 ) (Avg(k,j), k = 10, 12), (Avg(k,j), k = 1, 9), Avg(13, j)
          WRITE ( Out_unit+i, 300 ) (Minq(k,j), k = 10, 12), (Minq(k,j), k = 1, 9), Minq(13, j)
          WRITE ( Out_unit+i, 200 ) (Maxq(k,j), k = 10, 12), (Maxq(k,j), k = 1, 9), Maxq(13, j)
        ENDDO

      ENDIF

 100  FORMAT ( /, '   AVG:', 13F8.1 )
 200  FORMAT ( '   MAX:', 13F8.1 )
 300  FORMAT ( '   MIN:', 13F8.1 )   

      END SUBROUTINE monthlyq

!***********************************************
!     Subroutine aggFlows: compute average, maximum and minimum
!***********************************************
      SUBROUTINE aggFlows( Ia )
      USE PRMS_MONTHLYQ, ONLY: Cnt, Monavg, Cum, Avg, Maxq, Minq, Active_segments, Sum_flag
      USE PRMS_SET_TIME, ONLY: Yrdays, Modays
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ia
! Local Variables
      INTEGER :: i, j, days
!***********************************************
      IF ( Ia==13 ) THEN
        days = Yrdays
      ELSE
        days = Modays(Ia)
      ENDIF

      DO i = 1, Cnt
        j = Active_segments(i)
        Monavg(Ia, j) = Cum(Ia, j)/days
        IF ( Sum_flag==0 ) THEN
          Avg(Ia, j) = Avg(Ia, j) + Monavg(Ia, j)
          IF ( Monavg(Ia, j) > Maxq(Ia, j) ) Maxq(Ia, j) = Monavg(Ia, j)
          IF ( Monavg(Ia, j) < Minq(Ia, j) ) Minq(Ia, j) = Monavg(Ia, j)
        ELSE
          Avg(Ia, j) = Avg(Ia, j) + Cum(Ia, j)
          IF ( Cum(Ia, j) > Maxq(Ia, j) ) Maxq(Ia, j) = Cum(Ia, j)
          IF ( Cum(Ia, j) < Minq(Ia, j) ) Minq(Ia, j) = Cum(Ia, j)
        ENDIF       
      ENDDO

      END SUBROUTINE aggFlows

!***********************************************
!     Subroutine PrintFlows
!***********************************************
      SUBROUTINE PrintFlows( )
	  USE PRMS_CONSTANTS, ONLY: MONTHS_PER_YEAR
      USE PRMS_MONTHLYQ, ONLY: Cum, Monavg, Cnt, Active_segments, Sum_flag, Oldyear, Counter, Out_unit
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, j, k
!***********************************************
      j = Active_segments(1)
      DO i = 1, 13
        IF ( Sum_flag==0 ) THEN
          IF ( Monavg(i,j)>0.0D0 ) Counter(i) = Counter(i) + 1
        ELSE
          IF ( Cum(i,j)>0.0D0 ) Counter(i) = Counter(i) + 1
        ENDIF
      ENDDO

!      PRINT *, 'in subroutine PrintFlows oldyr=', Oldyear
!      PRINT *, 'counter=', Counter, ' cnt=', Cnt

      DO i = 1, Cnt
        j = Active_segments(i)
        IF ( Sum_flag==0 ) THEN
          WRITE ( Out_unit+i, '(I7, 13F8.1)' ) Oldyear + 1, (Monavg(k,j), k = 10, MONTHS_PER_YEAR), (Monavg(k,j), k=1,9), Monavg(13, j)
        ELSE
          WRITE ( Out_unit+i, '(I7, 13F8.1)' ) Oldyear + 1, (Cum(k,j), k = 10, MONTHS_PER_YEAR), (Cum(k,j), k=1,9), Cum(13, j)
        ENDIF

!     clear the arrays
        DO k = 1, 13
          Monavg(k, j) = 0.0D0
          Cum(k, j)    = 0.0D0
        ENDDO
      ENDDO

      END SUBROUTINE PrintFlows
