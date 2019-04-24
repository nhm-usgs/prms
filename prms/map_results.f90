!***********************************************************************
!     Output declared variables by HRU as a grid mapped to a specified
!     spatial resolution
!***********************************************************************
      MODULE PRMS_MAP_RESULTS
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Ngwcell, Nhrucell, Mapflg
      INTEGER, SAVE :: Endyr, Endmo, Endday, Lastyear, Totdays
      INTEGER, SAVE :: Yrdays, Yrresults, Totresults, Monresults, Mondays
      INTEGER, SAVE :: Begin_results, Begyr, Begmo, Begday, Nrow
      INTEGER, SAVE :: Prevyr, Prevmo, Prevday, Weekresults, Weekdays
      INTEGER, SAVE, ALLOCATABLE :: Totunit(:), Yrunit(:), Monunit(:), Weekunit(:)
      INTEGER, SAVE, ALLOCATABLE :: Nc_vars(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_tot(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_yr(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_mon(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_week(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_week(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_mon(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_yr(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_tot(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_id(:)
      REAL, SAVE, ALLOCATABLE :: Map_var(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Map_var_dble(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Var_values(:)
      DOUBLE PRECISION, SAVE :: Conv_fac
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gvr_map_frac_adjusted(:)
      CHARACTER(LEN=15), SAVE :: Mapfmt
      INTEGER, SAVE, ALLOCATABLE :: Map_var_type(:)
! Declared Parameters
      INTEGER, SAVE :: Ncol, Prms_warmup
      INTEGER, SAVE :: Mapvars_freq, Mapvars_units
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
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: map_resultsdecl, map_resultsinit
      INTEGER, EXTERNAL :: map_resultsclean, map_resultsrun
!***********************************************************************
      map_results = 0

      IF ( Process(:3)=='run' ) THEN
        map_results = map_resultsrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        map_results = map_resultsdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        map_results = map_resultsinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        map_results = map_resultsclean()
      ENDIF

      END FUNCTION map_results

!***********************************************************************
!     map_resultsdecl - declare parameters and variables
!***********************************************************************
      INTEGER FUNCTION map_resultsdecl()
      USE PRMS_MAP_RESULTS
      USE PRMS_MODULE, ONLY: Model, Nhru, Print_debug, Version_map_results, Map_results_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX, CHAR
      INTEGER, EXTERNAL :: declmodule, declparam, getdim, get_ftnunit
      INTEGER, EXTERNAL :: control_string_array, control_integer
      EXTERNAL read_error
! Local Variables
      INTEGER :: i
!***********************************************************************
      map_resultsdecl = 1

      Version_map_results = '$Id: map_results.f90 3878 2011-10-28 20:00:09Z rsregan $'
      Map_results_nc = INDEX( Version_map_results, ' $' ) + 1
      IF ( Print_debug>-1 ) THEN
        IF ( declmodule(Version_map_results(:Map_results_nc))/=0 ) STOP
      ENDIF

      IF ( control_integer(NmapOutVars, 'nmapOutVars')/=0 ) CALL read_error(5, 'nmapOutVars')
      ALLOCATE ( MapOutVar_names(NmapOutVars), Map_var_type(NmapOutVars) )
      ALLOCATE ( Nc_vars(NmapOutVars) )
      ALLOCATE ( Map_var(Nhru, NmapOutVars), Map_var_dble(Nhru, NmapOutVars) )

      MapOutVar_names = ' '
      DO i = 1, NmapOutVars
        IF ( control_string_array(MapOutVar_names(i), 'mapOutVar_names', i)/=0 ) CALL read_error(5, 'mapOutVar_names')
      ENDDO

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell==-1 ) CALL read_error(6, 'ngwcell')
      Mapflg = 1
      IF ( Nhru/=Ngwcell .AND. Ngwcell/=0 ) Mapflg = 0

! Declared Parameters
      IF ( declparam('map_results', 'mapvars_freq', 'one', 'integer', &
           '0', '0', '5', &
           'Mapped results frequency', &
           'Flag to specify the output frequency (0=none; 1=monthly; 2=yearly; 3=total; 4=monthly and yearly;'// &
           ' 5=monthly, yearly, and total; 6=weekly)', &
           'none')/=0 ) CALL read_error(1, 'mapvars_freq')

      IF ( declparam('map_results', 'mapvars_units', 'one', 'integer', &
           '0', '0', '3', &
           'Units of mapped results', &
           'Flag to specify the output units of mapped results (0=inches/day; 1=feet/day; 2=cm/day; 3=meters/day)', &
           'none')/=0 ) CALL read_error(1, 'mapvars_units')

      IF ( declparam('map_results', 'prms_warmup', 'one', 'integer', &
           '1', '0', '12', &
           'Number of years to simulate before writing mapped results', &
           'Number of years to simulate before writing mapped results', &
           'years')/=0 ) CALL read_error(1, 'prms_warmup')

      IF ( declparam('map_results', 'ncol', 'one', 'integer', &
           '1', '1', '50000', &
           'Number of columns for each row of the mapped results', &
           'Number of columns for each row of the mapped results', &
           'none')/=0 ) CALL read_error(1, 'ncol')

      IF ( Mapflg==0 .OR. Model==99 ) THEN
        Nhrucell = getdim('nhrucell')
        IF ( Nhrucell==-1 ) CALL read_error(6, 'nhrucell')

        ALLOCATE (Gvr_map_id(Nhrucell))
        IF ( declparam('map_results', 'gvr_cell_id', 'nhrucell', 'integer', &
             '1', 'bounded', 'ngwcell', &
             'Corresponding grid cell id associated with each GVR', &
             'Index of the grid cell associated with each gravity reservoir', &
             'none')/=0 ) CALL read_error(1, 'gvr_cell_id')

        ALLOCATE (Gvr_map_frac(Nhrucell))
        IF ( declparam('map_results', 'gvr_cell_pct', 'nhrucell', 'real', &
             '0.0', '0.0', '1.0', &
             'Proportion of the grid cell associated with each GVR', &
             'Proportion of the grid cell area associated with each gravity reservoir', &
             'decimal fraction')/=0 ) CALL read_error(1, 'gvr_cell_pct')

        ALLOCATE (Gvr_hru_id(Nhrucell))
        IF ( declparam('map_results', 'gvr_hru_id', 'nhrucell', 'integer', &
             '1', 'bounded', 'nhru', &
             'Corresponding HRU id of each GVR', &
             'Index of the HRU associated with each gravity reservoir', &
             'none')/=0 ) CALL read_error(1, 'gvr_hru_id')
      ENDIF

      map_resultsdecl = 0
      END FUNCTION map_resultsdecl

!***********************************************************************
!     map_resultsinit - Initialize map_results module
!***********************************************************************
      INTEGER FUNCTION map_resultsinit()
      USE PRMS_MAP_RESULTS
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_BASIN, ONLY: Hru_area, Starttime, Endtime
      IMPLICIT NONE
      INTRINSIC DBLE
      INTEGER, EXTERNAL :: getparam, getvartype, get_ftnunit
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, jj, is
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: map_frac, temp_frac
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: newfrac
!***********************************************************************
      map_resultsinit = 1

      IF ( getparam('map_results', 'mapvars_freq', 1, 'integer', Mapvars_freq)/=0 ) &
           CALL read_error(1, 'mapvars_freq')
      IF ( Mapvars_freq==0 ) THEN
        map_resultsinit = 0
        RETURN
      ENDIF

      IF ( getparam('map_results', 'ncol', 1, 'integer', Ncol)/=0 ) CALL read_error(2, 'ncol')
      WRITE (Mapfmt, 9001) Ncol

      IF ( Mapflg==0 ) THEN
        Nrow = Ngwcell/Ncol
        ALLOCATE ( Map_var_id(Ngwcell) )
        IF ( getparam('map_results', 'gvr_cell_id', Nhrucell, 'integer', Gvr_map_id)/=0 ) CALL read_error(2, 'gvr_cell_id')
        IF ( getparam('map_results', 'gvr_cell_pct', Nhrucell, 'real', Gvr_map_frac)/=0 ) CALL read_error(2, 'gvr_cell_pct')
        IF ( getparam('map_results', 'gvr_hru_id', Nhrucell, 'integer', Gvr_hru_id)/=0 ) CALL read_error(2, 'gvr_hru_id')

        ALLOCATE ( temp_frac(Nhrucell), map_frac(Ngwcell) )
        map_frac = 0.0D0
        DO i = 1, Nhrucell
          temp_frac(i) = DBLE( Gvr_map_frac(i) )
          is = Gvr_map_id(i)
          map_frac(is) = map_frac(is) + temp_frac(i)
        ENDDO
        ALLOCATE ( Gvr_map_frac_adjusted(Nhrucell), newfrac(Ngwcell) )

        newfrac = 0.0D0
        DO i = 1, Nhrucell
          is = Gvr_map_id(i)
          temp_frac(i) = temp_frac(i) + temp_frac(i)*(1.0D0-map_frac(is))/map_frac(is)
          Gvr_map_frac_adjusted(i) = temp_frac(i)
          newfrac(is) = newfrac(is) + temp_frac(i)
        ENDDO

        DO i = 1, Ngwcell
          IF ( newfrac(i)>0.0D0 ) THEN
            IF ( ABS(newfrac(i)-1.0D0)>0.000005 ) PRINT *, &
                 'Possible issue with GVR to mapped spatial unit fraction, map id:', &
                 i, newfrac(i)
          ENDIF
        ENDDO
        DEALLOCATE ( temp_frac, map_frac, newfrac )
      ELSE
        Nrow = Nhru/Ncol
        IF ( Nrow/=Nhru*Ncol ) Nrow = Nrow + 1
      ENDIF

      Map_var_type = 2
      DO jj = 1, NmapOutVars
        Nc_vars(jj) = INDEX( MapOutVar_names(jj), CHAR(0) ) - 1
        IF ( getvartype(MapOutVar_names(jj)(:Nc_vars(jj)), Map_var_type(jj) )/=0 ) STOP 'ERROR, in map_results'
        IF ( Map_var_type(jj)==-1 ) &
             PRINT *, 'WARNING: ignoring invalid MapOutVar_name:', MapOutVar_names(jj)(:Nc_vars(jj))
      ENDDO

      Weekresults = 0
      Monresults = 0
      Yrresults = 0
      Totresults = 0

      IF ( Mapvars_freq==6 ) THEN
        Weekresults = 1
        ALLOCATE ( Map_var_week(Nhru, NmapOutVars), Basin_var_week(NmapOutVars) )
        Map_var_week = 0.0D0
        Basin_var_week = 0.0D0
        ALLOCATE ( Weekunit(NmapOutVars) )
        DO jj = 1, NmapOutVars
          Weekunit(jj) = get_ftnunit(382+jj)
          OPEN ( Weekunit(jj), FILE=MapOutVar_names(jj)(:Nc_vars(jj))//'.weekly' )
        ENDDO
      ENDIF

      IF ( Mapvars_freq==1 .OR. Mapvars_freq==4 .OR. Mapvars_freq==5 ) THEN
        Monresults = 1
        ALLOCATE ( Map_var_mon(Nhru, NmapOutVars), Basin_var_mon(NmapOutVars) )
        Map_var_mon = 0.0D0
        Basin_var_mon = 0.0D0
        ALLOCATE ( Monunit(NmapOutVars) )
        DO jj = 1, NmapOutVars
          Monunit(jj) = get_ftnunit(383+jj)
          OPEN ( Monunit(jj), FILE=MapOutVar_names(jj)(:Nc_vars(jj))//'.monthly' )
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
          Yrunit(jj) = get_ftnunit(483+jj)
          OPEN ( Yrunit(jj), FILE=MapOutVar_names(jj)(:Nc_vars(jj))//'.yearly' )
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
          Totunit(jj) = get_ftnunit(583+jj)
          OPEN ( Totunit(jj), FILE=MapOutVar_names(jj)(:Nc_vars(jj))//'.total' )
        ENDDO
      ENDIF

      IF ( getparam('map_results', 'mapvars_units', 1, 'integer', &
           Mapvars_units)/=0 ) CALL read_error(2, 'Mapvars_units')
      IF ( Mapvars_units==0 ) THEN
        Conv_fac = 1.0D0
      ELSEIF ( Mapvars_units==1 ) THEN
        Conv_fac = 1.0D0/12.0D0
      ELSEIF ( Mapvars_units==2 ) THEN
        Conv_fac = 2.54D0
      ELSE
        Conv_fac = 0.0254D0
      ENDIF

      IF ( getparam('map_results', 'prms_warmup', 1, 'integer', &
           Prms_warmup)/=0 ) CALL read_error(2, 'prms_warmup')

      IF ( Prms_warmup>0 ) THEN
        Begin_results = 0
      ELSE
        Begin_results = 1
      ENDIF
      Begyr = Starttime(1) + Prms_warmup
      Begmo = Starttime(2)
      Begday = Starttime(3)
      Endyr = Endtime(1)
      IF ( Begyr>Endyr ) STOP 'ERROR, warmup period longer than simulation time period'
      Endmo = Endtime(2)
      Endday = Endtime(3)
      Lastyear = Begyr
      Totdays = 0
      Mondays = 0
      Yrdays = 0
      Weekdays = 0
      Prevyr = Begyr
      Prevmo = Begmo
      Prevday = Begday

 9001 FORMAT ( '(',I8,'E10.3)' )

      map_resultsinit = 0
      END FUNCTION map_resultsinit

!***********************************************************************
!     map_resultsrun - Outputs various declared variables in grid format
!                      mapped to a specified spatial resolution
!***********************************************************************
      INTEGER FUNCTION map_resultsrun()
      USE PRMS_MAP_RESULTS
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order, Basin_area_inv
      USE PRMS_OBS, ONLY: Nowtime, Modays
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: getvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: j, i, k, month, day, year, jj
      INTEGER :: ihru, imap_id, last_day
! Local static variables
      DOUBLE PRECISION factor
!***********************************************************************
      map_resultsrun = 0
      IF ( Mapvars_freq==0 ) RETURN

      year = Nowtime(1)
      month = Nowtime(2)
      day = Nowtime(3)
      IF ( Begin_results==0 ) THEN
        IF ( year==Begyr .AND. month==Begmo .AND. day==Begday ) THEN
          Begin_results = 1
        ELSE
          RETURN
        ENDIF
      ENDIF

      map_resultsrun = 1

! check for last day of simulation
      last_day = 0
      IF ( year==Endyr ) THEN
        IF ( month==Endmo ) THEN
          IF ( day==Endday ) THEN
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
          IF ( (month==Begmo .AND. day==Begday) .OR. last_day==1 ) THEN
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
              WRITE ( Yrunit(jj), 9002 ) Prevyr, Prevmo, Prevday, &
                      ' Basin yearly mean:', Basin_var_yr(jj)

              IF ( Mapflg==1 ) THEN
                k = 1
                DO i = 1, Nrow
                  WRITE ( Yrunit(jj), Mapfmt ) (Map_var_yr(j,jj), j=k,k+Ncol-1)
                  k = k + Ncol
                ENDDO
              ELSE
                Map_var_id = 0.0D0
                DO k = 1, Nhrucell
                  ihru = Gvr_hru_id(k)
                  imap_id = Gvr_map_id(k)
                  Map_var_id(imap_id) = Map_var_id(imap_id) + &
                                        Map_var_yr(ihru,jj)*Gvr_map_frac_adjusted(k)
                ENDDO
                k = 1
                DO i = 1, Nrow
                  WRITE ( Yrunit(jj), Mapfmt ) (Map_var_id(j), j=k,k+Ncol-1)
                  k = k + Ncol
                ENDDO
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
          IF ( getvar('map_results',MapOutVar_names(jj)(:Nc_vars(jj)), &
                      Nhru, 'real', Map_var(1, jj))/=0 ) CALL read_error(4,MapOutVar_names(jj)(:Nc_vars(jj)))
        ELSEIF ( Map_var_type(jj)==3 ) THEN
          IF ( getvar('map_results',MapOutVar_names(jj)(:Nc_vars(jj)), &
                      Nhru, 'double', Map_var_dble(1, jj))/=0 ) CALL read_error(4,MapOutVar_names(jj)(:Nc_vars(jj)))
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
            WRITE ( Weekunit(jj), 9002 ) year, month, day, &
                    ' Basin weekly mean:', Basin_var_week(jj)
            IF ( Mapflg==1 ) THEN
              k = 1
              DO i = 1, Nrow
                WRITE ( Weekunit(jj), Mapfmt ) (Map_var_week(j,jj), j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ELSE
              Map_var_id = 0.0D0
              DO k = 1, Nhrucell
                ihru = Gvr_hru_id(k)
                imap_id = Gvr_map_id(k)
                Map_var_id(imap_id) = Map_var_id(imap_id) + &
                                      Map_var_week(ihru,jj)*Gvr_map_frac_adjusted(k)
              ENDDO
              k = 1
              DO i = 1, Nrow
                WRITE ( Weekunit(jj), Mapfmt ) (Map_var_id(j), j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
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
            WRITE ( Monunit(jj), 9002 ) year, month, day, &
                    ' Basin monthly mean:', Basin_var_mon(jj)
            IF ( Mapflg==1 ) THEN
              k = 1
              DO i = 1, Nrow
                WRITE ( Monunit(jj), Mapfmt ) (Map_var_mon(j,jj), j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ELSE
              Map_var_id = 0.0D0
              DO k = 1, Nhrucell
                ihru = Gvr_hru_id(k)
                imap_id = Gvr_map_id(k)
                Map_var_id(imap_id) = Map_var_id(imap_id) + &
                                      Map_var_mon(ihru,jj)*Gvr_map_frac_adjusted(k)
              ENDDO
              k = 1
              DO i = 1, Nrow
                WRITE ( Monunit(jj), Mapfmt ) (Map_var_id(j), j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
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
            WRITE ( Totunit(jj), 9004 ) 'Time period: ', Begyr, Begmo, Begday, &
                    year, month, day, ' Basin simulation mean:', Basin_var_tot(jj)

            IF ( Mapflg==1 ) THEN
              k = 1
              DO i = 1, Nrow
                WRITE ( Totunit(jj), Mapfmt ) (Map_var_tot(j,jj), j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ELSE
              Map_var_id = 0.0D0
              DO k = 1, Nhrucell
                ihru = Gvr_hru_id(k)
                imap_id = Gvr_map_id(k)
                Map_var_id(imap_id) = Map_var_id(imap_id) + &
                                      Map_var_tot(ihru,jj)*Gvr_map_frac_adjusted(k)
              ENDDO
              k = 1
              DO i = 1, Nrow
                WRITE ( Totunit(jj), Mapfmt ) (Map_var_id(j), j=k,k+Ncol-1)
                k = k + Ncol
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF

 9002 FORMAT ( A, I5, 2('/',I2.2), A, E12.3 )
 9003 FORMAT ( '######################', / )
 9004 FORMAT ( A, 2(I5, 2('/',I2.2)), A, E12.4 )

      map_resultsrun = 0
      END FUNCTION map_resultsrun

!***********************************************************************
!     map_resultsclean - close files
!***********************************************************************
      INTEGER FUNCTION map_resultsclean()
      USE PRMS_MAP_RESULTS, ONLY: Totresults, Monresults, Yrresults, &
          Totunit, Monunit, Yrunit, NmapOutVars, Weekresults, Weekunit
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
