!***********************************************************************
!     Output declared variables by HRU as a grid mapped to a specified
!     spatial resolution
!***********************************************************************
      MODULE PRMS_MAP_RESULTS
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Ngwcell, Mapflg, Numcells
      INTEGER, SAVE :: Lastyear, Totdays, Begin_results, Begyr, Nrow
      INTEGER, SAVE :: Yrdays, Yrresults, Totresults, Monresults, Mondays
      INTEGER, SAVE :: Prevyr, Prevmo, Prevday, Weekresults, Weekdays
      INTEGER, SAVE, ALLOCATABLE :: Totunit(:), Yrunit(:), Monunit(:), Weekunit(:)
      INTEGER, SAVE, ALLOCATABLE :: Nc_vars(:), Map_var_type(:)
      DOUBLE PRECISION, SAVE :: Conv_fac
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_tot(:), Basin_var_yr(:), Basin_var_mon(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_week(:), Map_var_id(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_mon(:, :), Map_var_week(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_yr(:, :), Map_var_tot(:, :)
      REAL, SAVE, ALLOCATABLE :: Map_var(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_dble(:, :)
      CHARACTER(LEN=15), SAVE :: Mapfmt
      CHARACTER(LEN=11), SAVE :: MODNAME
! Declared Parameters
      INTEGER, SAVE :: Ncol, Prms_warmup, Mapvars_freq, Mapvars_units
      INTEGER, SAVE, ALLOCATABLE :: Gvr_map_id(:), Gvr_hru_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_map_frac(:)
! Control Parameters
      INTEGER, SAVE :: NmapOutVars
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: MapOutVar_names(:)
      END MODULE PRMS_MAP_RESULTS

!     ******************************************************************
!     Mapped results module
!     ******************************************************************
      INTEGER FUNCTION map_results()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: map_resultsdecl, map_resultsinit, map_resultsclean, map_resultsrun
      EXTERNAL :: map_results_restart
!***********************************************************************
      map_results = 0

      IF ( Process(:3)=='run' ) THEN
        map_results = map_resultsrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        map_results = map_resultsdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        map_results = map_resultsinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL map_results_restart(0)
        map_results = map_resultsclean()
      ENDIF

      END FUNCTION map_results

!***********************************************************************
!     map_resultsdecl - declare parameters and variables
!***********************************************************************
      INTEGER FUNCTION map_resultsdecl()
      USE PRMS_MAP_RESULTS
      USE PRMS_MODULE, ONLY: Model, Nhru, MapOutON_OFF, Nhrucell
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTRINSIC CHAR
      INTEGER, EXTERNAL :: declparam, getdim, control_string_array, control_integer
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i
      CHARACTER(LEN=80), SAVE :: Version_map_results
!***********************************************************************
      map_resultsdecl = 0

      Version_map_results = '$Id: map_results.f90 5599 2013-04-23 18:35:48Z rsregan $'
      CALL print_module(Version_map_results, 'Summary                   ', 90)
      MODNAME = 'map_results'

      IF ( control_integer(NmapOutVars, 'nmapOutVars')/=0 ) CALL read_error(5, 'nmapOutVars')
      IF ( NmapOutVars==0 ) THEN
        IF ( Model/=99 ) THEN
          PRINT *, 'Warning, map_results requested with nmapOutVars equal 0, no map_results output is produced'
          MapOutON_OFF = 0
          RETURN
        ENDIF
      ELSE
        ALLOCATE ( MapOutVar_names(NmapOutVars), Map_var_type(NmapOutVars), Nc_vars(NmapOutVars) )
        ALLOCATE ( Map_var(Nhru, NmapOutVars), Map_var_dble(Nhru, NmapOutVars) )
        MapOutVar_names = ' '
        DO i = 1, NmapOutVars
          IF ( control_string_array(MapOutVar_names(i), 'mapOutVar_names', i)/=0 ) CALL read_error(5, 'mapOutVar_names')
        ENDDO
      ENDIF

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell==-1 ) CALL read_error(6, 'ngwcell')
      IF ( Model==99 .AND. Ngwcell==0 ) Ngwcell = 1
      Mapflg = 1
      IF ( (Nhru/=Ngwcell .AND. Ngwcell/=0) .OR. mapOutON_OFF==2 ) Mapflg = 0

      IF ( Mapflg==0 .OR. Model==99 ) THEN
        IF ( Nhrucell<1 ) THEN
          IF ( Model==99 ) THEN
            Nhrucell = 1
          ELSE
            STOP 'ERROR, in map_results, nhru_cell = 0 and must be > 0'
          ENDIF
        ENDIF
        ALLOCATE ( Gvr_map_id(Nhrucell), Gvr_map_frac(Nhrucell), Gvr_hru_id(Nhrucell), Map_var_id(Ngwcell) )
      ENDIF

      IF ( Timestep/=0 ) RETURN

! Declared Parameters
      IF ( declparam(MODNAME, 'mapvars_freq', 'one', 'integer', &
     &     '0', '0', '5', &
     &     'Mapped results frequency', &
     &     'Flag to specify the output frequency (0=none; 1=monthly; 2=yearly; 3=total; 4=monthly and yearly;'// &
     &     ' 5=monthly, yearly, and total; 6=weekly)', &
     &     'none')/=0 ) CALL read_error(1, 'mapvars_freq')

      IF ( declparam(MODNAME, 'mapvars_units', 'one', 'integer', &
     &     '0', '0', '3', &
     &     'Flag to specify the output units of mapped results', &
     &     'Flag to specify the output units of mapped results (0=units of the variable;'// &
     &     ' 1=inches/day to feet/day; 2=inches/day to cm/day; 3=inches/day to meters/day)', &
     &     'none')/=0 ) CALL read_error(1, 'mapvars_units')

      IF ( declparam(MODNAME, 'prms_warmup', 'one', 'integer', &
     &     '1', '0', '12', &
     &     'Number of years to simulate before writing mapped results', &
     &     'Number of years to simulate before writing mapped results', &
     &     'years')/=0 ) CALL read_error(1, 'prms_warmup')

      IF ( declparam(MODNAME, 'ncol', 'one', 'integer', &
     &     '1', '1', '50000', &
     &     'Number of columns for each row of the mapped results', &
     &     'Number of columns for each row of the mapped results', &
     &     'none')/=0 ) CALL read_error(1, 'ncol')

      IF ( Mapflg==0 .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'gvr_cell_id', 'nhrucell', 'integer', &
     &       '1', 'bounded', 'ngwcell', &
     &       'Corresponding grid cell id associated with each GVR', &
     &       'Index of the grid cell associated with each gravity reservoir', &
     &       'none')/=0 ) CALL read_error(1, 'gvr_cell_id')
        IF ( declparam(MODNAME, 'gvr_cell_pct', 'nhrucell', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Proportion of the grid cell associated with each GVR', &
     &       'Proportion of the grid cell area associated with each gravity reservoir', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'gvr_cell_pct')
        IF ( declparam(MODNAME, 'gvr_hru_id', 'nhrucell', 'integer', &
     &       '1', 'bounded', 'nhru', &
     &       'Corresponding HRU id of each GVR', &
     &       'Index of the HRU associated with each gravity reservoir', &
     &       'none')/=0 ) CALL read_error(1, 'gvr_hru_id')
      ENDIF

      END FUNCTION map_resultsdecl

!***********************************************************************
!     map_resultsinit - Initialize map_results module
!***********************************************************************
      INTEGER FUNCTION map_resultsinit()
      USE PRMS_MAP_RESULTS
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Nhrucell, Inputerror_flag
      USE PRMS_BASIN, ONLY: Hru_area, Start_year, Start_month, Start_day, &
     &    End_year, End_month, End_day, NEARZERO, Timestep
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getparam, getvartype, numchars
      EXTERNAL read_error, PRMS_open_output_file, map_results_restart
! Local Variables
      INTEGER :: i, jj, is, ios, ierr
      REAL, ALLOCATABLE, DIMENSION(:) :: map_frac
!***********************************************************************
      map_resultsinit = 0

      Begin_results = 1
      Begyr = Start_year
      IF ( Timestep==0 ) THEN
        IF ( getparam(MODNAME, 'mapvars_freq', 1, 'integer', Mapvars_freq)/=0 ) CALL read_error(1, 'mapvars_freq')
        IF ( getparam(MODNAME, 'ncol', 1, 'integer', Ncol)/=0 ) CALL read_error(2, 'ncol')
        IF ( getparam(MODNAME, 'prms_warmup', 1, 'integer', Prms_warmup)/=0 ) CALL read_error(2, 'prms_warmup')
        IF ( getparam(MODNAME, 'mapvars_units', 1, 'integer', Mapvars_units)/=0 ) CALL read_error(2, 'Mapvars_units')
        IF ( Prms_warmup>0 ) Begin_results = 0
        Begyr = Begyr + Prms_warmup
        IF ( Begyr>End_year ) THEN
          PRINT *, 'ERROR, prms_warmup > than simulation time period:', Prms_warmup
          Inputerror_flag = 1
        ENDIF
      ELSE
        CALL map_results_restart(1)
      ENDIF

      IF ( Mapvars_freq==0 ) RETURN

      WRITE ( Mapfmt, 9001 ) Ncol

      IF ( Mapvars_units==0 ) THEN
        Conv_fac = 1.0D0
      ELSEIF ( Mapvars_units==1 ) THEN
        Conv_fac = 1.0D0/12.0D0
      ELSEIF ( Mapvars_units==2 ) THEN
        Conv_fac = 2.54D0
      ELSE
        Conv_fac = 0.0254D0
      ENDIF
      Lastyear = Begyr
      Totdays = 0
      Mondays = 0
      Yrdays = 0
      Weekdays = 0
      Prevyr = Begyr
      Prevmo = Start_month
      Prevday = Start_day
      Map_var_type = 2
      DO jj = 1, NmapOutVars
        Nc_vars(jj) = numchars(MapOutVar_names(jj))
        Map_var_type(jj)= getvartype(MapOutVar_names(jj)(:Nc_vars(jj)), Map_var_type(jj) )
      ENDDO

      Weekresults = 0
      Monresults = 0
      Yrresults = 0
      Totresults = 0
      ierr = 0
      IF ( Mapvars_freq==6 ) THEN
        Weekresults = 1
        ALLOCATE ( Map_var_week(Nhru, NmapOutVars), Basin_var_week(NmapOutVars) )
        Map_var_week = 0.0D0
        Basin_var_week = 0.0D0
        ALLOCATE ( Weekunit(NmapOutVars) )
        DO jj = 1, NmapOutVars
          CALL PRMS_open_output_file(Weekunit(jj), MapOutVar_names(jj)(:Nc_vars(jj))//'.weekly', 'xxx', 0, ios)
          IF ( ios/=0 ) ierr = 1
        ENDDO
      ENDIF

      IF ( Mapvars_freq==1 .OR. Mapvars_freq==4 .OR. Mapvars_freq==5 ) THEN
        Monresults = 1
        ALLOCATE ( Map_var_mon(Nhru, NmapOutVars), Basin_var_mon(NmapOutVars) )
        Map_var_mon = 0.0D0
        Basin_var_mon = 0.0D0
        ALLOCATE ( Monunit(NmapOutVars) )
        DO jj = 1, NmapOutVars
          CALL PRMS_open_output_file(Monunit(jj), MapOutVar_names(jj)(:Nc_vars(jj))//'.monthly', 'xxx', 0, ios)
          IF ( ios/=0 ) ierr = 1
        ENDDO
      ENDIF

      IF ( Mapvars_freq==2 .OR. Mapvars_freq==5 ) THEN
        Yrresults = 1
        ALLOCATE ( Map_var_yr(Nhru, NmapOutVars) )
        ALLOCATE ( Basin_var_yr(NmapOutVars) )
        Map_var_yr = 0.0D0
        Basin_var_yr = 0.0D0
        ALLOCATE ( Yrunit(NmapOutVars) )
        DO jj = 1, NmapOutVars
          CALL PRMS_open_output_file(Yrunit(jj), MapOutVar_names(jj)(:Nc_vars(jj))//'.yearly', 'xxx', 0, ios)
          IF ( ios/=0 ) ierr = 1
        ENDDO
      ENDIF

      IF ( Mapvars_freq==3 .OR. Mapvars_freq==5 ) THEN
        Totresults = 1
        ALLOCATE ( Map_var_tot(Nhru, NmapOutVars) )
        ALLOCATE ( Basin_var_tot(NmapOutVars) )
        Map_var_tot = 0.0D0
        Basin_var_tot = 0.0D0
        ALLOCATE ( Totunit(NmapOutVars) )
        DO jj = 1, NmapOutVars
          CALL PRMS_open_output_file(Totunit(jj), MapOutVar_names(jj)(:Nc_vars(jj))//'.total', 'xxx', 0, ios)
          IF ( ios/=0 ) ierr = 1
        ENDDO
      ENDIF
      IF ( ierr==1 ) STOP

      IF ( Timestep/=0 ) RETURN

      IF ( Mapflg==0 ) THEN
        Nrow = Ngwcell/Ncol
        IF ( Ngwcell==0 ) STOP 'ERROR, dimension ngwcell must be specified > 0'
        IF ( Ngwcell/=Nrow*Ncol ) Nrow = Nrow + 1
        Numcells = Ngwcell
        IF ( getparam(MODNAME, 'gvr_cell_id', Nhrucell, 'integer', Gvr_map_id)/=0 ) CALL read_error(2, 'gvr_cell_id')
        IF ( getparam(MODNAME, 'gvr_cell_pct', Nhrucell, 'real', Gvr_map_frac)/=0 ) CALL read_error(2, 'gvr_cell_pct')
        IF ( getparam(MODNAME, 'gvr_hru_id', Nhrucell, 'integer', Gvr_hru_id)/=0 ) CALL read_error(2, 'gvr_hru_id')

        ALLOCATE ( map_frac(Ngwcell) )
        map_frac = 0.0
        DO i = 1, Nhrucell
          IF ( Gvr_map_id(i)>0 .AND. Gvr_hru_id(i)>0 .AND. Gvr_map_frac(i)>1.0E-10 ) THEN
            is = Gvr_map_id(i)
            map_frac(is) = map_frac(is) + Gvr_map_frac(i)
          ELSEIF ( Print_debug>-1 ) THEN
            PRINT *, 'Warning, map intersection:', i, ' ignored as specification is incomplete'
            PRINT *, '         HRU:', Gvr_hru_id(i), ' Map unit:', Gvr_map_id(i), 'Fraction:', Gvr_map_frac(i)
          ENDIF
        ENDDO

        DO i = 1, Ngwcell
          IF ( map_frac(i)<0.0 ) PRINT *, 'ERROR, map_frac<0, map unit:', i
          IF ( map_frac(i)<NEARZERO ) CYCLE
          IF ( ABS(map_frac(i)-1.0)>1.0001 ) THEN
            IF ( Print_debug>-1 ) THEN
              IF ( map_frac(i)>1.0 ) THEN
                PRINT *, 'Warning, excess accounting for area of mapped spatial unit:'
              ELSE
                PRINT *, 'Warning, incomplete accounting for area of mapped spatial unit'
              ENDIF
              PRINT *, '           Map id:', i, ' Fraction:', map_frac(i)
            ENDIF
          ENDIF
        ENDDO
        DEALLOCATE ( map_frac )
      ELSE
        Nrow = Nhru/Ncol
        IF ( Nhru/=Nrow*Ncol ) Nrow = Nrow + 1
        Numcells = Nhru
      ENDIF

 9001 FORMAT ( '(',I8,'E10.3)' )

      END FUNCTION map_resultsinit

!***********************************************************************
!     map_resultsrun - Outputs various declared variables in grid format
!                      mapped to a specified spatial resolution
!***********************************************************************
      INTEGER FUNCTION map_resultsrun()
      USE PRMS_MAP_RESULTS
      USE PRMS_MODULE, ONLY: Nhru, Nhrucell
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order, Basin_area_inv, DNEARZERO, &
     &    Start_year, Start_month, Start_day, End_year, End_month, End_day
      USE PRMS_OBS, ONLY: Nowtime, Modays
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: getvar
      EXTERNAL read_error, write_results
! Local Variables
      INTEGER :: j, i, k, month, day, year, jj, last_day
! Local static variables
      DOUBLE PRECISION factor
!***********************************************************************
      map_resultsrun = 0
      IF ( Mapvars_freq==0 ) RETURN

      year = Nowtime(1)
      month = Nowtime(2)
      day = Nowtime(3)
      IF ( Begin_results==0 ) THEN
        IF ( year==Begyr .AND. month==Start_month .AND. day==Start_day ) THEN
          Begin_results = 1
        ELSE
          RETURN
        ENDIF
      ENDIF

! check for last day of simulation
      last_day = 0
      IF ( year==End_year ) THEN
        IF ( month==End_month ) THEN
          IF ( day==End_day ) THEN
            last_day = 1
            Prevyr = year
            Prevmo = month
            Prevday = day
          ENDIF
        ENDIF
      ENDIF

      IF ( Yrresults==1 ) THEN
! check for first time step of the next year
        IF ( Lastyear/=year .OR. last_day==1 ) THEN
          IF ( (month==Start_month .AND. day==Start_day) .OR. last_day==1 ) THEN
            Lastyear = year
            factor = Conv_fac/Yrdays
            Basin_var_yr = 0.0D0
            DO jj = 1, NmapOutVars
              DO j = 1, Active_hrus
                i = Hru_route_order(j)
                IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
                Map_var_yr(i, jj) = Map_var_yr(i, jj)*factor
                Basin_var_yr(jj) = Basin_var_yr(jj) + Map_var_yr(i, jj)*Hru_area(i)
              ENDDO
              Basin_var_yr(jj) = Basin_var_yr(jj)*Basin_area_inv

              IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
              WRITE ( Yrunit(jj), 9002 ) Prevyr, Prevmo, Prevday, ' Basin yearly mean:', Basin_var_yr(jj)

              IF ( Mapflg==1 ) THEN
                CALL write_results(Yrunit(jj), Map_var_yr(1,jj))
              ELSE
                Map_var_id = 0.0D0
                DO k = 1, Nhrucell
                  IF ( Gvr_map_frac(k)<DNEARZERO ) CYCLE
                  Map_var_id(Gvr_map_id(k)) = Map_var_id(Gvr_map_id(k)) + &
     &                                        Map_var_yr(Gvr_hru_id(k),jj)*Gvr_map_frac(k)
                ENDDO
                CALL write_results(Yrunit(jj), Map_var_id)
              ENDIF
              WRITE ( Yrunit(jj), 9003 )
            ENDDO
            Map_var_yr = 0.0D0
            Yrdays = 0
          ENDIF
        ENDIF
        Yrdays = Yrdays + 1
        Prevyr = year
        Prevmo = month
        Prevday = day
      ENDIF

!-----------------------------------------------------------------------
! need getvars for each variable (only can have short string)
      DO jj = 1, NmapOutVars
        IF ( Map_var_type(jj)==2 ) THEN
          IF ( getvar(MODNAME,MapOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'real', Map_var(1, jj))/=0 ) &
     &         CALL read_error(4,MapOutVar_names(jj)(:Nc_vars(jj)))
        ELSEIF ( Map_var_type(jj)==3 ) THEN
          IF ( getvar(MODNAME,MapOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'double', Map_var_dble(1, jj))/=0 ) &
     &         CALL read_error(4,MapOutVar_names(jj)(:Nc_vars(jj)))
        ENDIF
      ENDDO

      DO jj = 1, NmapOutVars
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Map_var_type(jj)==2 ) THEN
            IF ( Totresults==1 ) Map_var_tot(i, jj) = Map_var_tot(i, jj) + Map_var(i, jj)
            IF ( Yrresults==1 ) Map_var_yr(i,jj) = Map_var_yr(i, jj) + Map_var(i, jj)
            IF ( Monresults==1 ) Map_var_mon(i,jj) = Map_var_mon(i, jj) + Map_var(i, jj)
            IF ( Weekresults==1 ) Map_var_week(i,jj) = Map_var_week(i, jj) + Map_var(i, jj)
          ELSEIF ( Map_var_type(jj)==3 ) THEN
            IF ( Totresults==1 ) Map_var_tot(i, jj) = Map_var_tot(i, jj) + Map_var_dble(i, jj)
            IF ( Yrresults==1 ) Map_var_yr(i,jj) = Map_var_yr(i, jj) + Map_var_dble(i, jj)
            IF ( Monresults==1 ) Map_var_mon(i,jj) = Map_var_mon(i, jj) + Map_var_dble(i, jj)
            IF ( Weekresults==1 ) Map_var_week(i,jj) = Map_var_week(i, jj) + Map_var_dble(i, jj)
          ENDIF
        ENDDO
      ENDDO

      IF ( Weekresults==1 ) THEN
        Weekdays = Weekdays + 1
! check for seventh day
        IF ( Weekdays==7 ) THEN
          factor = Conv_fac/7
          Basin_var_week = 0.0D0
          DO jj = 1, NmapOutVars
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
              Map_var_week(i, jj) = Map_var_week(i, jj)*factor
              Basin_var_week(jj) = Basin_var_week(jj) + Map_var_week(i, jj)*Hru_area(i)
            ENDDO
            Basin_var_week(jj) = Basin_var_week(jj)*Basin_area_inv

            IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
            WRITE ( Weekunit(jj), 9002 ) year, month, day, ' Basin weekly mean:', Basin_var_week(jj)
            IF ( Mapflg==1 ) THEN
              CALL write_results(Weekunit(jj), Map_var_week(1,jj))
            ELSE
              Map_var_id = 0.0D0
              DO k = 1, Nhrucell
                IF ( Gvr_map_frac(k)<DNEARZERO ) CYCLE
                Map_var_id(Gvr_map_id(k)) = Map_var_id(Gvr_map_id(k)) + &
     &                                      Map_var_week(Gvr_hru_id(k),jj)*Gvr_map_frac(k)
              ENDDO
              CALL write_results(Weekunit(jj), Map_var_id)
            ENDIF
            WRITE ( Weekunit(jj), 9003 )
          ENDDO
          Map_var_week = 0.0D0
          Weekdays = 0
        ENDIF
      ENDIF

      IF ( Monresults==1 ) THEN
        Mondays = Mondays + 1
! check for last day of current month
        IF ( day==Modays(month) .OR. last_day==1 ) THEN
          factor = Conv_fac/Mondays
          Basin_var_mon = 0.0D0
          DO jj = 1, NmapOutVars
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
              Map_var_mon(i, jj) = Map_var_mon(i, jj)*factor
              Basin_var_mon(jj) = Basin_var_mon(jj) + Map_var_mon(i, jj)*Hru_area(i)
            ENDDO
            Basin_var_mon(jj) = Basin_var_mon(jj)*Basin_area_inv

            IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
            WRITE ( Monunit(jj), 9002 ) year, month, day, ' Basin monthly mean:', Basin_var_mon(jj)
            IF ( Mapflg==1 ) THEN
              CALL write_results(Monunit(jj), Map_var_mon(1,jj))
            ELSE
              Map_var_id = 0.0D0
              DO k = 1, Nhrucell
                IF ( Gvr_map_frac(k)<DNEARZERO ) CYCLE
                Map_var_id(Gvr_map_id(k)) = Map_var_id(Gvr_map_id(k)) + &
     &                                      Map_var_mon(Gvr_hru_id(k),jj)*Gvr_map_frac(k)
              ENDDO
              CALL write_results(Monunit(jj), Map_var_id)
            ENDIF
            WRITE ( Monunit(jj), 9003 )
          ENDDO
          Map_var_mon = 0.0D0
          Mondays = 0
        ENDIF
      ENDIF

      IF ( Totresults==1 ) THEN
        Totdays = Totdays + 1
! check for last day of simulation
        IF ( last_day==1 ) THEN
          factor = Conv_fac/Totdays
          Basin_var_tot = 0.0D0
          DO jj = 1, NmapOutVars
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
              Map_var_tot(i, jj) = Map_var_tot(i, jj)*factor
              Basin_var_tot(jj) = Basin_var_tot(jj) + Map_var_tot(i, jj)*Hru_area(i)
            ENDDO
            Basin_var_tot(jj) = Basin_var_tot(jj)*Basin_area_inv

            IF ( Map_var_type(jj)/=2 .AND. Map_var_type(jj)/=3 ) CYCLE
            WRITE ( Totunit(jj), 9004 ) 'Time period: ', Begyr, Start_month, Start_day, &
     &              year, month, day, ' Basin simulation mean:', Basin_var_tot(jj)

            IF ( Mapflg==1 ) THEN
              CALL write_results(Totunit(jj), Map_var_tot(1,jj))
            ELSE
              Map_var_id = 0.0D0
              DO k = 1, Nhrucell
                IF ( Gvr_map_frac(k)<DNEARZERO ) CYCLE
                Map_var_id(Gvr_map_id(k)) = Map_var_id(Gvr_map_id(k)) + &
     &                                      Map_var_tot(Gvr_hru_id(k),jj)*Gvr_map_frac(k)
              ENDDO
              CALL write_results(Totunit(jj), Map_var_id)
            ENDIF
          ENDDO
        ENDIF
      ENDIF

 9002 FORMAT ( I5, 2('/',I2.2), A, E12.3 )
 9003 FORMAT ( '######################', / )
 9004 FORMAT ( A, 2(I5, 2('/',I2.2)), A, E12.4 )

      END FUNCTION map_resultsrun

!***********************************************************************
!     map_resultsclean - close files
!***********************************************************************
      INTEGER FUNCTION map_resultsclean()
      USE PRMS_MAP_RESULTS, ONLY: Totresults, Monresults, Yrresults, &
     &    Totunit, Monunit, Yrunit, NmapOutVars, Weekresults, Weekunit
      IMPLICIT NONE
      INTEGER :: jj
!***********************************************************************
      IF ( Totresults==1 ) THEN
        DO jj = 1, NmapOutVars
          CLOSE ( Totunit(jj) )
        ENDDO
      ENDIF
      IF ( Monresults==1 ) THEN
        DO jj = 1, NmapOutVars
          CLOSE ( Monunit(jj) )
        ENDDO
      ENDIF
      IF ( Yrresults==1 ) THEN
        DO jj = 1, NmapOutVars
          CLOSE ( Yrunit(jj) )
        ENDDO
      ENDIF
      IF ( Weekresults==1 ) THEN
        DO jj = 1, NmapOutVars
          CLOSE ( Weekunit(jj) )
        ENDDO
      ENDIF

      map_resultsclean = 0
      END FUNCTION map_resultsclean

!***********************************************************************
!     write_results - write results to map
!***********************************************************************
      SUBROUTINE write_results(Iunit, Values)
      USE PRMS_MAP_RESULTS, ONLY: Nrow, Ncol, Numcells, Mapfmt
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit
      DOUBLE PRECISION, INTENT(IN) :: Values(Numcells)
! Local Variables
      INTEGER :: i, j, n, k
!***********************************************************************
      k = 1
      DO i = 1, Nrow
        n = k - 1 + Ncol
        IF ( n>Numcells ) n = Numcells
        WRITE ( Iunit, Mapfmt ) (Values(j), j=k,n)
        k = k + Ncol
      ENDDO
      END SUBROUTINE write_results

!***********************************************************************
!     map_results_restart - write or read map_results restart file
!***********************************************************************
      SUBROUTINE map_results_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_MAP_RESULTS
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Numcells, Nrow, Ncol, Prms_warmup, Mapvars_freq, Mapvars_units
        IF ( Mapflg==0 ) THEN
          WRITE ( Restart_outunit ) Map_var_id
          WRITE ( Restart_outunit ) Gvr_map_id
          WRITE ( Restart_outunit ) Gvr_hru_id
          WRITE ( Restart_outunit ) Gvr_map_frac
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Numcells, Nrow, Ncol, Prms_warmup, Mapvars_freq, Mapvars_units
        IF ( Mapflg==0 ) THEN
          READ ( Restart_inunit ) Map_var_id
          READ ( Restart_inunit ) Gvr_map_id
          READ ( Restart_inunit ) Gvr_hru_id
          READ ( Restart_inunit ) Gvr_map_frac
        ENDIF
      ENDIF
      END SUBROUTINE map_results_restart
