!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
      MODULE PRMS_OBS
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=3), SAVE :: MODNAME
      INTEGER, SAVE :: Nobs, Nevap, Nform, Nsnow, Modays(12), Yrdays
      INTEGER, SAVE :: Nsfelev, Nlakeelev, Nwind, Nhumid
      INTEGER, SAVE :: Nowtime(6), Jday, Jsol, Julwater, Rain_flag
      INTEGER, SAVE :: Nowday, Nowmonth, Nowyear
      DOUBLE PRECISION, SAVE :: Cfs_conv, Timestep_seconds, Timestep_days
!   Declared Variables
      INTEGER, SAVE :: Rain_day
      INTEGER, SAVE, ALLOCATABLE :: Form_data(:)
      REAL, SAVE, ALLOCATABLE :: Pan_evap(:), Runoff(:), Precip(:)
      REAL, SAVE, ALLOCATABLE :: Humidity(:), Wind_speed(:)
      REAL, SAVE, ALLOCATABLE :: Tmax(:), Tmin(:), Solrad(:), Snow(:)
      REAL, SAVE, ALLOCATABLE :: Streamflow_cfs(:), Streamflow_cms(:)
      ! Lake Module Variables
      REAL, SAVE, ALLOCATABLE :: Gate_ht(:), Lake_elev(:)
!   Declared Parameters
      INTEGER, SAVE :: Runoff_units
      INTEGER, SAVE, ALLOCATABLE :: Rain_code(:)
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun, obssetdims
!***********************************************************************
      obs = 0

      IF ( Process(:3)=='run' ) THEN
        obs = obsrun()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        obs = obssetdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        obs = obsdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        obs = obsinit()
      ENDIF

      END FUNCTION obs

!***********************************************************************
!     obssetdims - declares obs module specific dimensions
!***********************************************************************
      INTEGER FUNCTION obssetdims()
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL read_error
! Local Variables
      ! Maximum values are no longer limits
      INTEGER, PARAMETER :: MAXDIM = 500
!***********************************************************************
      obssetdims = 0

      IF ( decldim('nobs', 0, MAXDIM, 'Number of streamflow-measurement stations')/=0 ) CALL read_error(7, 'nobs')
      IF ( decldim('nsnow', 0, MAXDIM, 'Number of snow-measurement stations')/=0 ) CALL read_error(7, 'nsnow')
      IF ( decldim('nform', 0, MAXDIM, &
     &     'Number of precipitation-form data sets (deprecated, now ignored)')/=0 ) CALL read_error(7, 'nform')
      IF ( decldim('nevap', 0, MAXDIM, 'Number of pan-evaporation data sets')/=0 ) CALL read_error(7, 'nevap')
      IF ( decldim('nsfelev', 0, MAXDIM, &
     &     'Maximum number of lake elevations for any rating table data set')/=0 ) CALL read_error(7, 'nsfelev')
      IF ( decldim('nlakeelev', 0, MAXDIM, &
     &     'Maximum number of lake elevations for any rating table data set')/=0 ) CALL read_error(7, 'nlakeelev')
      IF ( decldim('nwind', 0, MAXDIM, 'Number of wind speed measurement stations')/=0 ) CALL read_error(7, 'nwind')
      IF ( decldim('nhumid', 0, MAXDIM, 'Number of relative humidity measurement stations')/=0 ) CALL read_error(7, 'nhumid')

      END FUNCTION obssetdims

!***********************************************************************
!     obsdecl - makes public variable declarations for the obs module
!   Declared Parameters
!     rain_code
!***********************************************************************
      INTEGER FUNCTION obsdecl()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Precip_flag, Model, Nratetbl, Ntemp, Nrain, Nsol
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declvar, getdim, declparam
      EXTERNAL read_error
! Local Variable
      INTEGER :: n, nc
      CHARACTER(LEN=80), SAVE :: Version_obs
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Time Series Data'
!***********************************************************************
      obsdecl = 0

      Version_obs = '$Id: obs.f90 5169 2012-12-28 23:51:03Z rsregan $'
      nc = INDEX( Version_obs, 'Z' )
      n = INDEX ( Version_obs, '.f90' ) + 3
      IF ( declmodule(Version_obs(6:n), PROCNAME, Version_obs(n+2:nc))/=0 ) STOP
      MODNAME = 'obs'

      Nobs = getdim('nobs')
      IF ( Nobs==-1 ) CALL read_error(6, 'nobs')

!   Declared Variables
      IF ( Nobs>0 .OR. Model==99 ) THEN
        ALLOCATE ( Runoff(Nobs) )
        IF ( declvar(MODNAME, 'runoff', 'nobs', Nobs, 'real', &
     &       'Streamflow at each measurement station', &
     &       'runoff_units', Runoff)/=0 ) CALL read_error(8, 'runoff')
        ALLOCATE ( Streamflow_cfs(Nobs) )
        IF ( declvar(MODNAME, 'streamflow_cfs', 'nobs', Nobs, 'real', &
     &       'Streamflow at each measurement station', &
     &       'cfs', Streamflow_cfs)/=0 ) CALL read_error(8, 'streamflow_cfs')
        ALLOCATE ( Streamflow_cms(Nobs) )
        IF ( declvar(MODNAME, 'streamflow_cms', 'nobs', Nobs, 'real', &
     &       'Streamflow at each measurement station', &
     &       'cms', Streamflow_cms)/=0 ) CALL read_error(8, 'streamflow_cms')
        IF ( declparam(MODNAME, 'runoff_units', 'one', 'integer', &
     &       '0', '0', '1', &
     &       'Measured runoff units', 'Measured runoff units (0=cfs; 1=cms)', &
     &       'none')/=0 ) CALL read_error(1, 'runoff_units')
      ENDIF

      IF ( Nrain>0 .OR. Model==99 ) THEN
        ALLOCATE ( Precip(Nrain) )
        IF ( declvar(MODNAME, 'precip', 'nrain', Nrain, 'real', &
     &       'Precipitation at each measurement station', &
     &       'precip_units', Precip)/=0 ) CALL read_error(8, 'precip')
      ENDIF

      IF ( Ntemp>0 .OR. Model==99 ) THEN
        ALLOCATE ( Tmin(Ntemp) )
        IF ( declvar(MODNAME, 'tmin', 'ntemp', Ntemp, 'real', &
     &       'Minimum air temperature at each measurement station', &
     &       'temp_units', Tmin)/=0 ) CALL read_error(8, 'tmin')
        ALLOCATE ( Tmax(Ntemp) )
        IF ( declvar(MODNAME, 'tmax', 'ntemp', Ntemp, 'real', &
     &       'Maximum air temperature at each measurement station', &
     &       'temp_units', Tmax)/=0 ) CALL read_error(8, 'tmax')
      ENDIF

      IF ( Nsol>0 .OR. Model==99 ) THEN
        ALLOCATE (Solrad(Nsol))
        IF ( declvar(MODNAME, 'solrad', 'nsol', Nsol, 'real', &
     &       'Solar radiation at each measurement station', &
     &       'Langleys', Solrad)/=0 ) CALL read_error(8, 'solrad')
      ENDIF

      Nform = getdim('nform')
      IF ( Nform==-1 ) CALL read_error(6, 'nform')
      IF ( Nform>0 .OR. Model==99 ) THEN
        ALLOCATE ( Form_data(Nform) )
        IF ( declvar(MODNAME, 'form_data', 'nform', Nform, 'integer', &
     &       'Form of precipitation (0=not known; 1=snow; 2=rain) – deprecated, now ignored', &
     &       'none', Form_data)/=0 ) CALL read_error(8, 'form_data')
        PRINT *, 'WARNING, nform>0: form_data values are ignored'
      ENDIF

      Nsnow = getdim('nsnow')
      IF ( Nsnow==-1 ) CALL read_error(6, 'nsnow')
      IF ( Nsnow>0 .OR. Model==99 ) THEN
        ALLOCATE ( Snow(Nsnow) )
        IF ( declvar(MODNAME, 'snow', 'nsnow', Nsnow, 'real', &
     &       'Snow depth at each measurement station', &
     &       'inches', Snow)/=0 ) CALL read_error(8, 'snow')
      ENDIF

      Nevap = getdim('nevap')
      IF ( Nevap==-1 ) CALL read_error(6, 'nevap')
      IF ( Nevap>0 .OR. Model==99 ) THEN
        ALLOCATE ( Pan_evap(Nevap) )
        IF ( declvar(MODNAME, 'pan_evap', 'nevap', Nevap, 'real', &
     &       'Pan evaporation at each measurement station', &
     &       'inches', Pan_evap)/=0 ) CALL read_error(8, 'pan_evap')
      ENDIF

      Nhumid = getdim('nhumid')
      IF ( Nhumid==-1 ) CALL read_error(6, 'nhumid')
      IF ( Nhumid>0 .OR. Model==99 ) THEN
        ALLOCATE ( Humidity(Nhumid) )
        IF ( declvar(MODNAME, 'humidity', 'nhumid', Nhumid, 'real', &
     &       'Relative humidity at each measurement station', &
     &       'decimal fraction', Humidity)/=0 ) CALL read_error(8, 'humidity')
      ENDIF

      Nwind = getdim('nwind')
      IF ( Nwind==-1 ) CALL read_error(6, 'nwind')
      IF ( Nwind>0 .OR. Model==99 ) THEN
        ALLOCATE ( Wind_speed(Nwind) )
        IF ( declvar(MODNAME, 'wind_speed', 'nwind', Nwind, 'real', &
     &       'Wind speed at each measurement station', &
     &       'mph', Wind_speed)/=0 ) CALL read_error(8, 'wind_speed')
      ENDIF

!   Declared Parameters
      Rain_flag = 0
      IF ( Precip_flag==6 ) Rain_flag = 1
      IF ( Rain_flag==1 .OR. Model==99 ) THEN
        IF ( declvar (MODNAME, 'rain_day', 'one', 1, 'integer', &
     &       'Flag to set the form of any precipitation to rain (0=determine form; 1=rain)', &
     &       'none', Rain_day)/=0 ) CALL read_error(8, 'rain_day')
        ALLOCATE ( Rain_code(12))
        IF ( declparam(MODNAME, 'rain_code', 'nmonths', 'integer', &
     &     '2', '1', '5', &
     &     'Flag indicating rule for precipitation station use', &
     &     'Monthly (January to December) flag indicating rule for'// &
     &     ' precipitation measurement station use (1=only'// &
     &     ' precipitation if the regression stations have'// &
     &     ' precipitation; 2=only precipitation'// &
     &     ' if any station in the basin has precipitation;'// &
     &     ' 3=precipitation if xyz says so; 4=only'// &
     &     ' precipitation if rain_day variable is set to 1; 5=only'// &
     &     ' precipitation if psta_freq_nuse stations have precipitation)', &
     &     'none')/=0 ) CALL read_error(1, 'rain_code')
      ENDIF

! Lake Variables
      IF ( Nratetbl>0 .OR. Model==99 ) THEN
        IF ( Nratetbl<-1 ) STOP 'ERROR, dimension nratetbl not specified > 0'
        ALLOCATE ( Gate_ht(Nratetbl) )
        IF ( declvar(MODNAME, 'gate_ht', 'nratetbl', Nratetbl, 'real', &
     &       'Height of the gate opening at each dam with a gate', &
     &       'inches', Gate_ht)/=0 ) CALL read_error(8, 'gate_ht')
      ENDIF

      Nsfelev = getdim('nsfelev')
      IF ( Nsfelev==-1 ) CALL read_error(6, 'nsfelev')
      Nlakeelev = getdim('nlakeelev')
      IF ( Nlakeelev==-1 ) CALL read_error(6, 'nlakeelev')
      IF ( Nsfelev>0 .AND. Nlakeelev==0 ) THEN
        PRINT *,'ERROR, dimension nsfelev has been changed to nlakeelev'
        PRINT *, '       All references to nsfelev must be changed to nlakeelev in your Parameter File'
        STOP
      ENDIF
      IF ( Nlakeelev>0 .OR. Model==99 ) THEN
        ALLOCATE ( Lake_elev(Nlakeelev) )
        IF ( declvar(MODNAME, 'lake_elev', 'nlakeelev', Nlakeelev, 'real', &
     &       'Elevation of each simulated lake surface', &
     &       'feet', Lake_elev)/=0 ) CALL read_error(8, 'lake_elev')
      ENDIF

      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Nratetbl, Ntemp, Nrain, Nsol
      USE PRMS_BASIN, ONLY: Timestep, Start_year
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam, isleap, julian
      DOUBLE PRECISION, EXTERNAL :: deltim
      EXTERNAL read_error
! Local Variables
      DOUBLE PRECISION :: dts
!***********************************************************************
      obsinit = 0

      Modays(1) = 31
      Modays(3) = 31
      Modays(4) = 30
      Modays(5) = 31
      Modays(6) = 30
      Modays(7) = 31
      Modays(8) = 31
      Modays(9) = 30
      Modays(10) = 31
      Modays(11) = 30
      Modays(12) = 31
      IF ( isleap(Start_year)==1 ) THEN
        Yrdays = 366
        Modays(2) = 29
      ELSE
        Yrdays = 365
        Modays(2) = 28
      ENDIF

      Jsol = julian('start', 'solar')
      Julwater = julian('start', 'water')

      dts = deltim()*3600.0D0
      Timestep_seconds = dts
      Timestep_days = deltim()/24.0D0
      Cfs_conv = 43560.0D0/12.0D0/dts

      IF ( Nobs>0 ) THEN
        IF ( getparam(MODNAME, 'runoff_units', 1, 'integer', Runoff_units)/=0 ) CALL read_error(2, 'runoff_units')
      ENDIF

      IF ( Rain_flag==1 ) THEN
        IF ( getparam(MODNAME, 'rain_code', 12, 'integer', Rain_code)/=0 ) CALL read_error(2, 'rain_code')
      ENDIF

      IF ( Timestep==0 ) THEN
        IF ( Nobs>0 ) THEN
          Runoff = 0.0
          Streamflow_cfs = 0.0
          Streamflow_cms = 0.0
        ENDIF
        IF ( Nrain>0 ) Precip = 0.0
        IF ( Ntemp>0 ) THEN
          Tmax = 0.0
          Tmin = 0.0
        ENDIF
        IF ( Nsol>0 ) Solrad = 0.0
        IF ( Nevap>0 ) Pan_evap = 0.0
        IF ( Nsnow>0 ) Snow = 0.0
        IF ( Nform>0 ) Form_data = 0
        IF ( Nlakeelev>0 ) Lake_elev = 0.0
        IF ( Nratetbl>0 ) Gate_ht = 0.0
        IF ( Nhumid>0 ) Humidity = 0.0
        IF ( Nwind>0 ) Wind_speed = 0.0
      ENDIF

      END FUNCTION obsinit

! **********************************************************************
!     obsrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsrun()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Nratetbl, Ntemp, Nrain, Nsol
      USE PRMS_BASIN, ONLY: Timestep, CFS2CMS_CONV
      IMPLICIT NONE
! Functions
      INTRINSIC ISNAN
      INTEGER, EXTERNAL :: julian, isleap, readvar, getstep
      DOUBLE PRECISION, EXTERNAL :: deltim
      EXTERNAL :: dattim, read_error
! Local Variables
      INTEGER :: i, runoff_missing, tmax_missing
      INTEGER :: tmin_missing, precip_missing, pan_missing
      INTEGER :: solrad_missing, missing
      DOUBLE PRECISION :: dt, dthr
! **********************************************************************
      obsrun = 0

      Timestep = getstep()

      CALL dattim('now', Nowtime)
      Nowyear = Nowtime(1)
      Nowmonth = Nowtime(2)
      Nowday = Nowtime(3)

      Jday = julian('now', 'calendar')
      Jsol = julian('now', 'solar')
      Julwater = julian('now', 'water')

      dthr = deltim() 
      dt = dthr*3600.0D0
!   Check to see if daily time step
      IF ( dthr>24.0001D0 ) THEN
        PRINT *, 'ERROR, timestep > daily, fix Data File', dthr
        STOP
      ENDIF
      IF ( dthr<23.999D0 ) THEN
        PRINT *, 'ERROR, timestep < daily, fix Data File', dthr
        STOP
      ENDIF
      Timestep_seconds = dt
      Cfs_conv = 43560.0D0/12.0D0/dt
      Timestep_days = dthr/24.0D0

      IF ( isleap(Nowyear)==1 ) THEN
        Yrdays = 366
        Modays(2) = 29
      ELSE
        Yrdays = 365
        Modays(2) = 28
      ENDIF

      missing = 0
      IF ( Nobs>0 ) THEN
        IF ( readvar(MODNAME, 'runoff')/=0 ) CALL read_error(9, 'runoff')
        runoff_missing = 0
        IF ( Runoff_units==1 ) THEN
          DO i = 1, Nobs
            IF ( ISNAN(Runoff(i)) ) THEN
              runoff_missing = runoff_missing + 1
              CYCLE
            ENDIF
            Streamflow_cms(i) = Runoff(i)
            Streamflow_cfs(i) = Runoff(i)/CFS2CMS_CONV
          ENDDO
        ELSE
          DO i = 1, Nobs
            IF ( ISNAN(Runoff(i)) ) THEN
              runoff_missing = runoff_missing + 1
              CYCLE
            ENDIF
            Streamflow_cms(i) = Runoff(i)*CFS2CMS_CONV
            Streamflow_cfs(i) = Runoff(i)
          ENDDO
        ENDIF
        IF ( runoff_missing>0 ) THEN
          PRINT *, 'ERROR,', runoff_missing,' runoff NaN value(s) found'
          missing = 1
        ENDIF
      ENDIF

      IF ( Nrain>0 ) THEN
        IF ( readvar(MODNAME, 'precip')/=0 ) CALL read_error(9, 'precip')
        precip_missing = 0
        DO i = 1, Nrain
          IF ( ISNAN(Precip(i)) ) precip_missing = precip_missing + 1
        ENDDO
        IF ( precip_missing>0 ) THEN
          PRINT *, 'ERROR,', precip_missing,' precip NaN value(s) found'
          missing = 1
        ENDIF
      ENDIF

      IF ( Ntemp>0 ) THEN
        IF ( readvar(MODNAME, 'tmax')/=0 ) CALL read_error(9, 'tmax')
        IF ( readvar(MODNAME, 'tmin')/=0 ) CALL read_error(9, 'tmin')
        tmax_missing = 0
        tmin_missing = 0
        DO i = 1, Ntemp
          IF ( ISNAN(Tmax(i)) ) tmax_missing = tmax_missing + 1
          IF ( ISNAN(Tmin(i)) ) tmin_missing = tmin_missing + 1
        ENDDO
        IF ( tmax_missing>0 ) THEN
          PRINT *, 'ERROR,', tmax_missing, ' tmax NaN value(s) found'
          missing = 1
        ENDIF
        IF ( tmin_missing>0 ) THEN
          PRINT *, 'ERROR,', tmin_missing, ' tmin NaN value(s) found'
          missing = 1
        ENDIF
      ENDIF

      IF ( Nsol>0 ) THEN
        IF ( readvar(MODNAME, 'solrad')/=0 ) CALL read_error(9, 'solrad')
        solrad_missing = 0
        DO i = 1, Nsol
          IF ( ISNAN(Solrad(i)) ) solrad_missing = solrad_missing + 1
        ENDDO
        IF ( solrad_missing>0 ) THEN
          PRINT *, 'ERROR,', solrad_missing,' solrad NaN value(s) found'
          missing = 1
        ENDIF
      ENDIF

      IF ( Nform>0 ) THEN
        IF ( readvar(MODNAME, 'form_data')/=0 ) CALL read_error(8, 'form_data')
      ENDIF

      IF ( Nevap>0 ) THEN
        IF ( readvar(MODNAME, 'pan_evap')/=0 ) CALL read_error(9, 'pan_evap')
        pan_missing = 0
        DO i = 1, Nevap
          IF ( ISNAN(Tmax(i)) ) pan_missing = pan_missing + 1
        ENDDO
        IF ( pan_missing>0 ) THEN
          PRINT *, 'ERROR,', pan_missing, ' pan_evap NaN value(s) found'
          missing = 1
        ENDIF
      ENDIF

      IF ( Nsnow>0 ) THEN
        IF ( readvar(MODNAME, 'snow')/=0 ) CALL read_error(9, 'snow')
      ENDIF

      IF ( Rain_flag==1 ) THEN
        IF ( Rain_code(Nowmonth)==4 ) THEN
          IF ( readvar(MODNAME, 'rain_day')/=0 ) CALL read_error(9, 'rain_day')
        ENDIF
      ENDIF

      IF ( Nlakeelev>0 ) THEN
        IF ( readvar(MODNAME, 'lake_elev')/=0 ) CALL read_error(9, 'lake_elev')
      ENDIF

      IF ( Nratetbl>0 ) THEN
        IF ( readvar(MODNAME, 'gate_ht')/=0 ) CALL read_error(9, 'gate_ht')
      ENDIF

      IF ( Nhumid>0 ) THEN
        IF ( readvar(MODNAME, 'humidity')/=0 ) CALL read_error(9, 'humidity')
      ENDIF

      IF ( Nwind>0 ) THEN
        IF ( readvar(MODNAME, 'wind_speed')/=0 ) CALL read_error(9, 'wind_speed')
      ENDIF

      IF ( missing==1 ) THEN
        PRINT '(A,I5,A,I2.2,A,I2.2)', 'Date:', Nowyear, '/', Nowmonth, '/', Nowday
        STOP
      ENDIF

      END FUNCTION obsrun
