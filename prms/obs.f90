!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
      MODULE PRMS_OBS
      USE PRMS_CONSTANTS, ONLY: MONTHS_PER_YEAR
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Time Series Data'
      character(len=*), parameter :: MODNAME = 'obs'
      character(len=*), parameter :: Version_obs = '2022-01-12'
      INTEGER, SAVE :: Nlakeelev, Nwind, Nhumid, Rain_flag, nstream_temp
      ! Two dimensions for Spring Creek model
      INTEGER, SAVE :: ncalevap, nsolignored
!   Declared Variables
      INTEGER, SAVE :: Rain_day
      REAL, SAVE, ALLOCATABLE :: Pan_evap(:), Runoff(:), Precip(:)
      REAL, SAVE, ALLOCATABLE :: Humidity(:), Wind_speed(:), Stream_temp(:), Temp_sta_avg(:)
      REAL, SAVE, ALLOCATABLE :: Tmax(:), Tmin(:), Solrad(:), Snowdepth(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Streamflow_cfs(:), Streamflow_cms(:)
      ! Lake Module Variables
      REAL, SAVE, ALLOCATABLE :: Gate_ht(:), Lake_elev(:)
      ! Three input variables for Spring Creek model
      REAL, SAVE, ALLOCATABLE :: cal_evap(:), cal_potevap(:), solrad2(:)
!   Declared Parameters
      INTEGER, SAVE :: Runoff_units, Rain_code(MONTHS_PER_YEAR)
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs()
      USE PRMS_CONSTANTS, ONLY: RUN, SETDIMENS, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun, obssetdims
!***********************************************************************
      obs = 0

      IF ( Process_flag==RUN ) THEN
        obs = obsrun()
      ELSEIF ( Process_flag==SETDIMENS ) THEN
        obs = obssetdims()
      ELSEIF ( Process_flag==DECL ) THEN
        obs = obsdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        obs = obsinit()
      ENDIF

      END FUNCTION obs

!***********************************************************************
!     obssetdims - declares obs module specific dimensions
!***********************************************************************
      INTEGER FUNCTION obssetdims()
      USE PRMS_CONSTANTS, ONLY: MAXDIM
      use prms_utils, only: read_error
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
!***********************************************************************
      obssetdims = 0

      IF ( decldim('nlakeelev', 0, MAXDIM, &
     &     'Maximum number of lake elevations for any rating table data set')/=0 ) CALL read_error(7, 'nlakeelev')
      IF ( decldim('nwind', 0, MAXDIM, 'Number of wind-speed measurement stations')/=0 ) CALL read_error(7, 'nwind')
      IF ( decldim('nhumid', 0, MAXDIM, 'Number of relative humidity measurement stations')/=0 ) CALL read_error(7, 'nhumid')
      IF ( decldim('nstream_temp', 0, MAXDIM, 'Number of stream temperature replacement segments')/=0 ) &
     &     CALL read_error(7, 'nstream_temp')
      IF ( decldim('ncalevap', 0, MAXDIM, 'Number of pan and potential evaporation measurement stations')/=0 ) CALL read_error(7, 'npanevap')
      IF ( decldim('nsolignored', 0, MAXDIM, 'Number of solar radiation measurement stations to ignore')/=0 ) CALL read_error(7, 'nsolignored')

      END FUNCTION obssetdims

!***********************************************************************
!     obsdecl - makes public variable declarations for the obs module
!   Declared Parameters
!     rain_code
!***********************************************************************
      INTEGER FUNCTION obsdecl()
      USE PRMS_CONSTANTS, ONLY: DOCUMENTATION, ACTIVE, OFF, xyz_dist_module, mm_dist_module
      USE PRMS_MODULE, ONLY: Model, Nratetbl, Ntemp, Nrain, Nsol, Nobs, Nevap, Nsnow, Precip_flag
      USE PRMS_OBS
      use prms_utils, only: print_module, read_error
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, getdim, declparam
!***********************************************************************
      obsdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_obs)

!   Declared Variables
      IF ( Nobs>0 ) THEN
        ALLOCATE ( Runoff(Nobs) )
        IF ( declvar(MODNAME, 'runoff', 'nobs', Nobs, 'real', &
     &       'Streamflow at each measurement station', &
     &       'runoff_units', Runoff)/=0 ) CALL read_error(8, 'runoff')
        ALLOCATE ( Streamflow_cfs(Nobs) )
        IF ( declvar(MODNAME, 'streamflow_cfs', 'nobs', Nobs, 'double', &
     &       'Streamflow at each measurement station', &
     &       'cfs', Streamflow_cfs)/=0 ) CALL read_error(8, 'streamflow_cfs')
        ALLOCATE ( Streamflow_cms(Nobs) )
        IF ( declvar(MODNAME, 'streamflow_cms', 'nobs', Nobs, 'double', &
     &       'Streamflow at each measurement station', &
     &       'cms', Streamflow_cms)/=0 ) CALL read_error(8, 'streamflow_cms')
        IF ( declparam(MODNAME, 'runoff_units', 'one', 'integer', &
     &       '0', '0', '1', &
     &       'Measured streamflow units', 'Measured streamflow units (0=cfs; 1=cms)', &
     &       'none')/=0 ) CALL read_error(1, 'runoff_units')
      ENDIF

      IF ( Nrain>0 ) THEN
        ALLOCATE ( Precip(Nrain) )
        IF ( declvar(MODNAME, 'precip', 'nrain', Nrain, 'real', &
     &       'Precipitation at each measurement station', &
     &       'precip_units', Precip)/=0 ) CALL read_error(8, 'precip')
      ENDIF

      IF ( Ntemp>0 ) THEN
        ALLOCATE ( Tmin(Ntemp) )
        IF ( declvar(MODNAME, 'tmin', 'ntemp', Ntemp, 'real', &
     &       'Minimum air temperature at each measurement station', &
     &       'temp_units', Tmin)/=0 ) CALL read_error(8, 'tmin')
        ALLOCATE ( Tmax(Ntemp) )
        IF ( declvar(MODNAME, 'tmax', 'ntemp', Ntemp, 'real', &
     &       'Maximum air temperature at each measurement station', &
     &       'temp_units', Tmax)/=0 ) CALL read_error(8, 'tmax')
        ALLOCATE ( Temp_sta_avg(Ntemp) )
        IF ( declvar(MODNAME, 'temp_sta_avg', 'ntemp', Ntemp, 'real', &
     &       'Average air temperature at each measurement station', &
     &       'temp_units', Temp_sta_avg)/=0 ) CALL read_error(8, 'temp_sta_avg')
      ENDIF

      IF ( Nsol>0 ) THEN
        ALLOCATE ( Solrad(Nsol) )
        IF ( declvar(MODNAME, 'solrad', 'nsol', Nsol, 'real', &
     &       'Solar radiation at each measurement station', &
     &       'Langleys', Solrad)/=0 ) CALL read_error(8, 'solrad')
      ENDIF

      Nsnow = getdim('nsnow')
      IF ( Nsnow==-1 ) CALL read_error(6, 'nsnow')
      Nhumid = getdim('nhumid')
      IF ( Nhumid==-1 ) CALL read_error(6, 'nhumid')
      Nwind = getdim('nwind')
      IF ( Nwind==-1 ) CALL read_error(6, 'nwind')
      Nlakeelev = getdim('nlakeelev')
      IF ( Nlakeelev==-1 ) CALL read_error(6, 'nlakeelev')

      IF ( Model==DOCUMENTATION ) THEN
        IF ( Nsnow==0 ) Nsnow = 1
        IF ( Nhumid==0 ) Nhumid = 1
        IF ( Nwind==0 ) Nwind = 1
        IF ( Nlakeelev==0 ) Nlakeelev = 1
      ENDIF

      IF ( Nsnow>0 ) THEN
        ALLOCATE ( Snowdepth(Nsnow) )
        IF ( declvar(MODNAME, 'snowdepth', 'nsnow', Nsnow, 'real', &
     &       'Snow depth at each measurement station', &
     &       'inches', Snowdepth)/=0 ) CALL read_error(8, 'snowdepth')
      ENDIF

      IF ( Nevap>0 ) THEN
        ALLOCATE ( Pan_evap(Nevap) )
        IF ( declvar(MODNAME, 'pan_evap', 'nevap', Nevap, 'real', &
     &       'Pan evaporation at each measurement station', &
     &       'inches', Pan_evap)/=0 ) CALL read_error(8, 'pan_evap')
      ENDIF

      IF ( Nhumid>0 ) THEN
        ALLOCATE ( Humidity(Nhumid) )
        IF ( declvar(MODNAME, 'humidity', 'nhumid', Nhumid, 'real', &
     &       'Relative humidity at each measurement station', &
     &       'percentage', Humidity)/=0 ) CALL read_error(8, 'humidity')
      ENDIF

      IF ( Nwind>0 ) THEN
        ALLOCATE ( Wind_speed(Nwind) )
        IF ( declvar(MODNAME, 'wind_speed', 'nwind', Nwind, 'real', &
     &       'Wind speed at each measurement station', &
     &       'meters per second', Wind_speed)/=0 ) CALL read_error(8, 'wind_speed')
      ENDIF

      IF ( Nstream_temp>0 ) THEN
        ALLOCATE ( Stream_temp(Nwind) )
        CALL declvar(MODNAME, 'stream_temp', 'nstream_temp', nstream_temp, 'real', &
     &       'Stream temperature for segment replacement in stream_temp', &
     &       'degrees Celsius', Stream_temp)/=0 ) CALL read_error(8, 'stream_temp')
      ENDIF

!   Declared Parameters
      Rain_flag = OFF
      IF ( Precip_flag==xyz_dist_module .OR. Precip_flag==mm_dist_module ) Rain_flag = ACTIVE
      IF ( Rain_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        IF ( declvar(MODNAME, 'rain_day', 'one', 1, 'integer', &
     &       'Flag to set the form of any precipitation to rain (0=determine form; 1=rain)', &
     &       'none', Rain_day)/=0 ) CALL read_error(8, 'rain_day')
        IF ( declparam(MODNAME, 'rain_code', 'nmonths', 'integer', &
     &       '2', '1', '5', &
     &       'Flag indicating rule for precipitation station use', &
     &       'Monthly (January to December) flag indicating rule for'// &
     &       ' precipitation measurement station use (1=only'// &
     &       ' precipitation if the regression stations have'// &
     &       ' precipitation; 2=only precipitation'// &
     &       ' if any station in the basin has precipitation;'// &
     &       ' 3=precipitation if xyz says so; 4=only'// &
     &       ' precipitation if rain_day variable is set to 1; 5=only'// &
     &       ' precipitation if psta_freq_nuse stations have precipitation)', &
     &       'none')/=0 ) CALL read_error(1, 'rain_code')
      ENDIF

! Lake Variables
      IF ( Nratetbl>0 ) THEN
        ALLOCATE ( Gate_ht(Nratetbl) )
        IF ( declvar(MODNAME, 'gate_ht', 'nratetbl', Nratetbl, 'real', &
     &       'Height of the gate opening at each dam with a gate', &
     &       'inches', Gate_ht)/=0 ) CALL read_error(8, 'gate_ht')
      ENDIF

      IF ( Nlakeelev>0 ) THEN
        ALLOCATE ( Lake_elev(Nlakeelev) )
        IF ( declvar(MODNAME, 'lake_elev', 'nlakeelev', Nlakeelev, 'real', &
     &       'Elevation of each simulated lake surface', &
     &       'feet', Lake_elev)/=0 ) CALL read_error(8, 'lake_elev')
      ENDIF

!   Declared Variables for Spring Creek, remove at some point
      nsolignored = getdim('nsolignored')
      IF ( nsolignored==-1 ) CALL read_error(6, 'nsolignored')
      IF ( nsolignored>0 .OR. Model==DOCUMENTATION ) THEN
        IF ( Model==DOCUMENTATION .AND. nsolignored==0 ) Nsolignored = 1
        ALLOCATE ( Solrad2(nsolignored) )
        IF ( declvar(MODNAME, 'solrad2', 'nsolignored', nsolignored, 'real', &
     &       'Measured solar radiation at measurement stations not used in simulation', &
     &       'Langleys', Solrad2)/=0 ) CALL read_error(8, 'solrad2')
      ENDIF
      ncalevap = getdim('ncalevap')
      IF ( ncalevap==-1 ) CALL read_error(6, 'ncalevap')
      IF ( ncalevap>0 .OR. Model==DOCUMENTATION ) THEN
        IF ( Model==DOCUMENTATION .AND. Ncalevap==0 ) Ncalevap = 1
        ALLOCATE ( cal_evap(ncalevap) )
        IF ( declvar(MODNAME, 'pan_evap', 'ncalevap', ncalevap, 'real', &
     &       'Measured pan evaporation at each measurement station', &
     &       'calories', cal_evap)/=0 ) CALL read_error(8, 'cal_evap')
        ALLOCATE ( cal_potevap(ncalevap) )
        IF ( declvar(MODNAME, 'cal_potevap', 'ncalevap', ncalevap, 'real', &
     &       'Measured potential evaporation at each measurement station', &
     &       'calories', cal_potevap)/=0 ) CALL read_error(8, 'cal_potevap')
      ENDIF

      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, MONTHS_PER_YEAR, CFS
      USE PRMS_MODULE, ONLY: Nratetbl, Ntemp, Nrain, Nsol, Nobs, Nevap, Nsnow
      USE PRMS_OBS
      use prms_utils, only: read_error
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      obsinit = 0

      Runoff_units = CFS
      IF ( Nobs>0 ) THEN
        IF ( getparam(MODNAME, 'runoff_units', 1, 'integer', Runoff_units)/=0 ) CALL read_error(2, 'runoff_units')
      ENDIF

      IF ( Rain_flag==ACTIVE ) THEN
        IF ( getparam(MODNAME, 'rain_code', MONTHS_PER_YEAR, 'integer', Rain_code)/=0 ) CALL read_error(2, 'rain_code')
      ENDIF

      IF ( Nobs>0 ) THEN
        Runoff = 0.0
        Streamflow_cfs = 0.0D0
        Streamflow_cms = 0.0D0
      ENDIF
      IF ( Nrain>0 ) Precip = 0.0
      Rain_day = OFF
      IF ( Ntemp>0 ) THEN
        Tmax = 0.0
        Tmin = 0.0
      ENDIF
      IF ( Nsol>0 ) Solrad = 0.0
      IF ( Nevap>0 ) Pan_evap = 0.0
      IF ( Nsnow>0 ) Snowdepth = 0.0
      IF ( Nlakeelev>0 ) Lake_elev = 0.0
      IF ( Nratetbl>0 ) Gate_ht = 0.0
      IF ( Nhumid>0 ) Humidity = 0.0
      IF ( Nwind>0 ) Wind_speed = 0.0
      IF ( nstream_temp>0 ) Stream_temp = 0.0

      END FUNCTION obsinit

! **********************************************************************
!     obsrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsrun()
      USE PRMS_CONSTANTS, ONLY: CMS, CFS2CMS_CONV, ACTIVE
      USE PRMS_MODULE, ONLY: Nratetbl, Ntemp, Nrain, Nsol, Nobs, Nevap, Nsnow, &
                             Nowyear, Nowmonth, Nowday, forcing_check_flag, Timestep
      USE PRMS_OBS
      USE PRMS_CLIMATEVARS, ONLY: Ppt_zero_thresh
      IMPLICIT NONE
! Functions
      INTRINSIC DBLE !, ISNAN
      INTEGER, EXTERNAL :: readvar
      EXTERNAL :: read_error !, print_date
! Local Variables
      INTEGER :: i
      !INTEGER :: tmin_missing, precip_missing, pan_missing, solrad_missing, missing, runoff_missing, tmax_missing
      !REAL :: foo
! **********************************************************************
      obsrun = 0

      Timestep = Timestep + 1

      !missing = 0
      IF ( Nobs>0 ) THEN
        IF ( readvar(MODNAME, 'runoff')/=0 ) CALL read_error(9, 'runoff')
        !runoff_missing = 0
        IF ( Runoff_units==1 ) THEN
          DO i = 1, Nobs
            !IF ( ISNAN(Runoff(i)) ) THEN
            !  runoff_missing = runoff_missing + 1
            !  CYCLE
            !ENDIF
            Streamflow_cms(i) = DBLE( Runoff(i) )
            Streamflow_cfs(i) = Streamflow_cms(i)/CFS2CMS_CONV
          ENDDO
        ELSE
          DO i = 1, Nobs
            !IF ( ISNAN(Runoff(i)) ) THEN
            !  runoff_missing = runoff_missing + 1
            !  CYCLE
            !ENDIF
            Streamflow_cfs(i) = DBLE( Runoff(i) )
            Streamflow_cms(i) = Streamflow_cfs(i)*CFS2CMS_CONV
          ENDDO
        ENDIF
        !IF ( runoff_missing>0 ) THEN
        !  PRINT *, 'ERROR,', runoff_missing,' runoff NaN value(s) found'
        !  missing = 1
        !ENDIF
      ENDIF

      IF ( Nrain>0 ) THEN
        IF ( readvar(MODNAME, 'precip')/=0 ) CALL read_error(9, 'precip')
        !precip_missing = 0
        !DO i = 1, Nrain
        !  IF ( ISNAN(Precip(i)) ) THEN
        !    precip_missing = precip_missing + 1
        !    CYCLE
        !  ENDIF
        !  IF ( Precip(i)<IGNOREPPT .AND. Precip(i)>0.0 ) Precip(i) = 0.0
        !ENDDO
        !IF ( precip_missing>0 ) THEN
        !  PRINT *, 'ERROR,', precip_missing,' precip NaN value(s) found'
        !  missing = 1
        !ENDIF
        IF ( Ppt_zero_thresh>0.0 ) THEN
          DO i = 1, Nrain
            IF ( Precip(i)<Ppt_zero_thresh ) Precip(i) = 0.0
          ENDDO
        ENDIF
      ENDIF

      IF ( Ntemp>0 ) THEN
        IF ( readvar(MODNAME, 'tmax')/=0 ) CALL read_error(9, 'tmax')
        IF ( readvar(MODNAME, 'tmin')/=0 ) CALL read_error(9, 'tmin')
        !tmax_missing = 0
        !tmin_missing = 0
        !DO i = 1, Ntemp
        !  IF ( ISNAN(Tmax(i)) ) tmax_missing = tmax_missing + 1
        !  IF ( ISNAN(Tmin(i)) ) tmin_missing = tmin_missing + 1
        !ENDDO
        !IF ( tmax_missing>0 ) THEN
        !  PRINT *, 'ERROR,', tmax_missing, ' tmax NaN value(s) found'
        !  missing = 1
        !ENDIF
        !IF ( tmin_missing>0 ) THEN
        !  PRINT *, 'ERROR,', tmin_missing, ' tmin NaN value(s) found'
        !  missing = 1
        !ENDIF
        IF ( forcing_check_flag == ACTIVE ) THEN
          DO i = 1, Ntemp
            IF ( Tmin(i) < -98 .OR. Tmax(i) < -98 ) CYCLE
            IF ( Tmin(i) > Tmax(i) ) THEN
              PRINT *, 'WARNING, observed tmin > tmax: HRU, date, tmin, tmax:', &
                       i, Nowyear, Nowmonth, Nowday, Tmin(i), Tmax(i)
!              WRITE ( 862, * ) 'WARNING, observed tmin > tmax, swapped fort_866 and fort_867: HRU, date, tmin, tmax:', &
!                               i, Nowyear, Nowmonth, Nowday, Tmin(i), Tmax(i)
!              foo = Tmax(i)
!              Tmax(i) = Tmin(i)
!              Tmin(i) = foo
            ENDIF
          ENDDO
!          WRITE ( 866,  '(I4,2I3,3I2,22F8.2)' ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tmax(i), i=1,Ntemp)
!          WRITE ( 867, '(I4,2I3,3I2,22F8.2)' ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tmin(i), i=1,Ntemp)
        ENDIF
        DO i = 1, Ntemp
          Temp_sta_avg(i) = (Tmax(i)+Tmin(i))*0.5
        ENDDO
      ENDIF

      IF ( Nsol>0 ) THEN
        IF ( readvar(MODNAME, 'solrad')/=0 ) CALL read_error(9, 'solrad')
        !solrad_missing = 0
        !DO i = 1, Nsol
        !  IF ( ISNAN(Solrad(i)) ) solrad_missing = solrad_missing + 1
        !ENDDO
        !IF ( solrad_missing>0 ) THEN
        !  PRINT *, 'ERROR,', solrad_missing,' solrad NaN value(s) found'
        !  missing = 1
        !ENDIF
      ENDIF

      IF ( Nevap>0 ) THEN
        IF ( readvar(MODNAME, 'pan_evap')/=0 ) CALL read_error(9, 'pan_evap')
        !pan_missing = 0
        !DO i = 1, Nevap
        !  IF ( ISNAN(Tmax(i)) ) pan_missing = pan_missing + 1
        !ENDDO
        !IF ( pan_missing>0 ) THEN
        !  PRINT *, 'ERROR,', pan_missing, ' pan_evap NaN value(s) found'
        !  missing = 1
        !ENDIF
      ENDIF

      IF ( Nsnow>0 ) THEN
        IF ( readvar(MODNAME, 'snowdepth')/=0 ) CALL read_error(9, 'snowdepth')
      ENDIF

      IF ( Rain_flag==ACTIVE ) THEN
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

      IF ( nstream_temp>0 ) THEN
        IF ( readvar(MODNAME, 'stream_temp')/=0 ) CALL read_error(9, 'stream_temp')
      ENDIF

      !IF ( missing==1 ) THEN
      !  CALL print_date(0)
      !  STOP
      !ENDIF

! Variables for Spring Creek model
      IF ( ncalevap>0 ) THEN
        IF ( readvar(MODNAME, 'cal_evap')/=0 ) CALL read_error(9, 'cal_evap')
        IF ( readvar(MODNAME, 'cal_potevap')/=0 ) CALL read_error(9, 'cal_potevap')
      ENDIF

      IF ( nsolignored>0 ) THEN
        IF ( readvar(MODNAME, 'solrad2')/=0 ) CALL read_error(9, 'solrad2')
      ENDIF

      END FUNCTION obsrun
