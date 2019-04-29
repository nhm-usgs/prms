!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
      MODULE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Begin_results, Begyr
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nhru_var_type(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_daily(:, :), Nhru_var_dble(:, :)
      REAL, SAVE, ALLOCATABLE :: Nhru_var(:, :)
      CHARACTER(LEN=40), SAVE :: Output_fmt, Output_fmt2
      CHARACTER(LEN=12), SAVE :: MODNAME
! Declared Parameters
      INTEGER, SAVE :: Prms_warmup
! Control Parameters
      INTEGER, SAVE :: NhruOutVars
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: NhruOutVar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: NhruOutBaseFileName
      END MODULE PRMS_NHRU_SUMMARY

!     ******************************************************************
!     nhru results module
!     ******************************************************************
      SUBROUTINE nhru_summary()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      EXTERNAL :: nhru_summarydecl, nhru_summaryinit, nhru_summaryrun
!***********************************************************************
      IF ( Process(:3)=='run' ) THEN
        CALL nhru_summaryrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        CALL nhru_summarydecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        CALL nhru_summaryinit()
      ENDIF

      END SUBROUTINE nhru_summary

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
      SUBROUTINE nhru_summarydecl()
      USE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: Model, Nhru, NhruOutON_OFF
      IMPLICIT NONE
! Functions
      INTRINSIC CHAR
      INTEGER, EXTERNAL :: declparam, control_string_array, control_integer, control_string
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i
      CHARACTER(LEN=80), SAVE :: Version_nhru_summary
!***********************************************************************
      Version_nhru_summary = '$Id: nhru_summary.f90 7125 2015-01-13 16:54:29Z rsregan $'
      CALL print_module(Version_nhru_summary, 'Output Summary              ', 90)
      MODNAME = 'nhru_summary'

      IF ( control_integer(NhruOutVars, 'nhruOutVars')/=0 ) NhruOutVars = 0

      IF ( NhruOutVars==0 ) THEN
        IF ( Model/=99 ) THEN
          PRINT *, 'WARNING, nhru_summary requested with nhruOutVars equal 0'
          PRINT *, 'no nhru_summary output is produced'
          NhruOutON_OFF = 0
          RETURN
        ENDIF
      ELSE
        ALLOCATE ( NhruOutVar_names(NhruOutVars), Nhru_var_type(NhruOutVars), Nc_vars(NhruOutVars) )
        ALLOCATE ( Nhru_var(Nhru, NhruOutVars), Nhru_var_dble(Nhru, NhruOutVars) )
        NhruOutVar_names = ' '
        DO i = 1, NhruOutVars
          IF ( control_string_array(NhruOutVar_names(i), 'nhruOutVar_names', i)/=0 ) CALL read_error(5, 'nhruOutVar_names')
        ENDDO
        IF ( control_string(NhruOutBaseFileName, 'nhruOutBaseFileName')/=0 ) CALL read_error(5, 'nhruOutBaseFileName')
      ENDIF

! Declared Parameters
      IF ( declparam(MODNAME, 'prms_warmup', 'one', 'integer', &
     &     '1', '0', '12', &
     &     'Number of years to simulate before writing mapped results', &
     &     'Number of years to simulate before writing mapped results', &
     &     'years')/=0 ) CALL read_error(1, 'prms_warmup')

      END SUBROUTINE nhru_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE nhru_summaryinit()
      USE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: Nhru, Inputerror_flag , MAXFILE_LENGTH, Start_year, End_year
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getparam, getvartype, numchars, getvarsize
      EXTERNAL read_error, PRMS_open_output_file
! Local Variables
      INTEGER :: ios, ierr, size, dim, jj, j
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
!***********************************************************************
      Begin_results = 1
      Begyr = Start_year
      IF ( getparam(MODNAME, 'prms_warmup', 1, 'integer', Prms_warmup)/=0 ) CALL read_error(2, 'prms_warmup')
      IF ( Prms_warmup>0 ) Begin_results = 0
      Begyr = Begyr + Prms_warmup
      IF ( Begyr>End_year ) THEN
        PRINT *, 'ERROR, prms_warmup > than simulation time period:', Prms_warmup
        Inputerror_flag = 1
      ENDIF

      WRITE ( Output_fmt, 9001 ) Nhru

      ierr = 0
      DO jj = 1, NhruOutVars
        Nc_vars(jj) = numchars(NhruOutVar_names(jj))
        Nhru_var_type(jj) = getvartype(NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru_var_type(jj) )
        IF ( Nhru_var_type(jj)/=2 .AND. Nhru_var_type(jj)/=3 ) THEN
          PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only real or double variables allowed'
          ierr = 1
        ENDIF
        size = getvarsize(NhruOutVar_names(jj)(:Nc_vars(jj)), dim )
        IF ( size/=Nhru ) THEN
          PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only variables dimensioned by nhru, nssr, or ngw allowed'
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) STOP

      ALLOCATE ( Nhru_var_daily(Nhru, NhruOutVars) )
      Nhru_var_daily = 0.0D0
      ALLOCATE ( Dailyunit(NhruOutVars) ) ! don't need array if all in one file
      DO jj = 1, NhruOutVars
        fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'.csv'
        !print *, fileName
        CALL PRMS_open_output_file(Dailyunit(jj), fileName, 'xxx', 0, ios)
        WRITE ( Output_fmt2, 9002 ) Nhru
        WRITE ( Dailyunit(jj), Output_fmt2 ) (j, j=1,Nhru)
        IF ( ios/=0 ) STOP
      ENDDO

 9001 FORMAT ('(I4, 2(''-'',I2.2),',I6,'('',''ES10.3))')
 9002 FORMAT ('("Date "',I6,'('',''I6))')

      END SUBROUTINE nhru_summaryinit

!***********************************************************************
!     Output set of declared variables in R compatible format
!***********************************************************************
      SUBROUTINE nhru_summaryrun()
      USE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: Nhru, Start_month, Start_day
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: getvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: j, i, jj
!***********************************************************************
      IF ( Begin_results==0 ) THEN
        IF ( Nowyear==Begyr .AND. Nowmonth==Start_month .AND. Nowday==Start_day ) THEN
          Begin_results = 1
        ELSE
          RETURN
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! need getvars for each variable (only can have short string)
      DO jj = 1, NhruOutVars
        IF ( Nhru_var_type(jj)==2 ) THEN
          IF ( getvar(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'real', Nhru_var(1, jj))/=0 ) &
     &         CALL read_error(4, NhruOutVar_names(jj)(:Nc_vars(jj)))
        ELSEIF ( Nhru_var_type(jj)==3 ) THEN  ! probably don't need double
          IF ( getvar(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'double', Nhru_var_dble(1, jj))/=0 ) &
     &         CALL read_error(4, NhruOutVar_names(jj)(:Nc_vars(jj)))
        ENDIF
      ENDDO

      DO jj = 1, NhruOutVars
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Nhru_var_type(jj)==2 ) THEN
            Nhru_var_daily(i, jj) = Nhru_var(i, jj)
          ELSEIF ( Nhru_var_type(jj)==3 ) THEN
            Nhru_var_daily(i, jj) = Nhru_var_dble(i, jj)
          ENDIF
        ENDDO
      ENDDO

      DO jj = 1, NhruOutVars
        WRITE ( Dailyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, (Nhru_var_daily(j,jj), j=1,Nhru)
      ENDDO

      END SUBROUTINE nhru_summaryrun
