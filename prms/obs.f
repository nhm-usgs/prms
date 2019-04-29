!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
      MODULE PRMS_OBS
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Nobs, Nevap, Nform, Nsnow, Modays(12), Yrdays
      INTEGER, SAVE :: Nratetbl, Nsfelev
      INTEGER, SAVE :: Nowtime(6), Jday, Jsol, Julwater, Rain_flag
      INTEGER, SAVE :: Nowday, Nowmonth, Nowyear
      DOUBLE PRECISION, SAVE :: Cfs_conv, Timestep_seconds,Timestep_days
!   Declared Variables
      INTEGER, SAVE :: Rain_day
      INTEGER, SAVE, ALLOCATABLE :: Form_data(:)
      REAL, SAVE, ALLOCATABLE :: Pan_evap(:), Runoff(:), Precip(:)
      REAL, SAVE, ALLOCATABLE :: Tmax(:), Tmin(:), Solrad(:), Snow(:)
      REAL, SAVE, ALLOCATABLE :: Streamflow_cfs(:), Streamflow_cms(:)
      ! Lake Module Variables
      REAL, SAVE, ALLOCATABLE :: Gate_ht(:), Sfr_elev(:)
!   Declared Parameters
      INTEGER, SAVE :: Runoff_units
      INTEGER, SAVE, ALLOCATABLE :: Rain_code(:)
      
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='obs')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Time Series Data')
      
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun
!***********************************************************************
      obs = 0

      IF ( Process(:3)=='run' ) THEN
        obs = obsrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        obs = obsdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        obs = obsinit()
      ENDIF

      END FUNCTION obs

!***********************************************************************
!     obsdecl - makes public variable declarations for the
!                     obs module
!   Declared Parameters
!     rain_code
!***********************************************************************
      INTEGER FUNCTION obsdecl()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Precip_flag, Model, Nhru, Print_debug,
     +    Version_obs, Obs_nc
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Nrain, Nsol
      IMPLICIT NONE
! Functions
      INTRINSIC MAX, INDEX
      INTEGER, EXTERNAL :: declmodule, declvar, getdim, declparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: n
!***********************************************************************
      obsdecl = 1

      Version_obs =
     +'$Id: obs.f 4078 2012-01-05 23:47:36Z rsregan $'
      Obs_nc = INDEX( Version_obs, ' $' ) + 1
      IF ( Print_debug>-1 ) THEN
        IF ( declmodule(MODNAME, PROCNAME,
     +                  Version_obs(:Obs_nc))/=0 ) STOP
      ENDIF

      Nobs = getdim('nobs')
      IF ( Nobs==-1 ) CALL read_error(6, 'nobs')

      Nsnow = getdim('nsnow')
      IF ( Nsnow==-1 ) CALL read_error(6, 'nsnow')

!   Declared Variables
      n = MAX(Nobs, 1)
      ALLOCATE ( Runoff(n) )
      ALLOCATE ( Streamflow_cfs(n), Streamflow_cms(n) )
!      IF ( Nobs>0 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'runoff', 'nobs', n, 'real',
     +       'Streamflow at each measurement station',
     +       'runoff_units',
     +       Runoff)/=0 ) CALL read_error(8, 'runoff')
        IF ( declvar(MODNAME, 'streamflow_cfs', 'nobs', n, 'real',
     +       'Streamflow at each measurement station',
     +       'cfs',
     +       Streamflow_cfs)/=0 ) CALL read_error(8, 'streamflow_cfs')
        IF ( declvar(MODNAME, 'streamflow_cms', 'nobs', n, 'real',
     +       'Streamflow at each measurement station',
     +       'cms',
     +       Streamflow_cms)/=0 ) CALL read_error(8, 'streamflow_cms')
      IF ( Nobs>0 .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'runoff_units', 'one', 'integer',
     +       '0', '0', '1',
     +       'Measured runoff units',
     +       'Measured runoff units (0=cfs; 1=cms)',
     +       'none')/=0 ) CALL read_error(1, 'runoff_units')
      ENDIF

      n = MAX(Nrain, 1)
      ALLOCATE (Precip(n))
!      IF ( Nrain>0 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'precip', 'nrain', n, 'real',
     +       'Precipitation at each measurement station',
     +       'precip_units',
     +       Precip)/=0 ) CALL read_error(8, 'precip')
!      ENDIF

      n = MAX(Ntemp, 1)
      ALLOCATE (Tmin(n), Tmax(n))
!      IF ( Ntemp>0 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'tmin', 'ntemp', n, 'real',
     +       'Minimum air temperature at each measurement station',
     +       'temp_units',
     +       Tmin)/=0 ) CALL read_error(8, 'tmin')

        IF ( declvar(MODNAME, 'tmax', 'ntemp', n, 'real',
     +       'Maximum air temperature at each measurement station',
     +       'temp_units',
     +       Tmax)/=0 ) CALL read_error(8, 'tmax')
!      ENDIF

      n = MAX(Nsol, 1)
      ALLOCATE (Solrad(n))
!     IF ( Nsol>0 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'solrad', 'nsol', n, 'real',
     +       'Solar radiation at each measurement station',
     +       'langleys',
     +       Solrad)/=0 ) CALL read_error(8, 'solrad')
!     ENDIF

      Nform = getdim('nform')
      IF ( Nform==-1 ) CALL read_error(6, 'nform')
      n = MAX(Nform, 1)
      ALLOCATE ( Form_data(n) )
      IF ( declvar(MODNAME, 'form_data', 'nform', n, 'integer',
     +     'Form of precipitation (0=not known; 1=snow; 2=rain)'//
     +     ' – deprecated, now ignored',
     +     'none',
     +     Form_data)/=0 ) CALL read_error(8, 'form_data')
      IF ( Nform>0 )
     +     PRINT *, 'WARNING, nform>0: form_data values are ignored'

      n = MAX(Nsnow, 1)
      ALLOCATE (Snow(n))
      IF ( Nsnow>0 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'snow', 'nsnow', Nsnow, 'real',
     +       'Snow depth at each measurement station',
     +       'inches',
     +       Snow)/=0 ) CALL read_error(8, 'snow')
      ENDIF

      Nevap = getdim('nevap')
      IF ( Nevap==-1 ) CALL read_error(6, 'nevap')
      n = MAX(Nevap, 1)
      ALLOCATE (Pan_evap(n))
!     IF ( Nevap>0 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'pan_evap', 'nevap', n, 'real',
     +       'Pan evaporation at each measurement station',
     +       'inches',
     +       Pan_evap)/=0 ) CALL read_error(8, 'pan_evap')
!     ENDIF

!   Declared Parameters
      Rain_flag = 0
      IF ( Precip_flag==6 ) Rain_flag = 1
!      IF ( Rain_flag==1 .OR. Model==99 ) THEN
        IF ( declvar (MODNAME, 'rain_day', 'one', 1, 'integer',
     +       'Flag to set the form of any precipitation to rain'//
     +       ' (0=determine form; 1=rain)',
     +       'none',
     +       Rain_day)/=0 ) CALL read_error(8, 'rain_day')

      IF ( Rain_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Rain_code(12))
        IF ( declparam(MODNAME, 'rain_code', 'nmonths', 'integer',
     +       '2', '1', '5',
     +       'Flag indicating rule for precipitation station use',
     +       'Monthly (January to December) flag indicating rule for'//
     +       ' precipitation measurement station use (1=only'//
     +       ' precipitation if the regression stations have'//
     +       ' precipitation; 2=only precipitation'//
     +       ' if any station in the basin has precipitation;'//
     +       ' 3=precipitation if xyz says so; 4=only'//
     +       ' precipitation if rain_day variable is set to 1; 5=only'//
     +       ' precipitation if psta_freq_nuse stations have'//
     +       ' precipitation)',
     +       'none')/=0 ) CALL read_error(1, 'rain_code')
      ENDIF

      Nratetbl = getdim('nratetbl')
      IF ( Nratetbl==-1 ) CALL read_error(6, 'nratetbl')

      Nsfelev = getdim('nsfelev')
      IF ( Nsfelev==-1 ) CALL read_error(6, 'nsfelev')

!   Declared Variables
      IF ( Nsfelev>0 ) THEN
        ALLOCATE ( Sfr_elev(Nsfelev) )
        IF ( declvar(MODNAME, 'sfr_elev', 'nsfelev', Nsfelev, 'real',
     +       'Elevation of each simulated lake surface',
     +       'feet',
     +       Sfr_elev)/=0 ) CALL read_error(8, 'sfr_elev')
      ENDIF

      IF ( Nratetbl>0 ) THEN
        ALLOCATE ( Gate_ht(Nratetbl) )
        IF ( declvar(MODNAME, 'gate_ht', 'nratetbl', Nratetbl, 'real',
     +       'Height of the gate opening at each dam with a gate',
     +       'inches',
     +       Gate_ht)/=0 ) CALL read_error(8, 'gate_ht')
      ENDIF

      obsdecl = 0
      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_BASIN, ONLY: Timestep, Starttime
      USE PRMS_CLIMATEVARS, ONLY: Ntemp
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam, isleap, julian
      DOUBLE PRECISION, EXTERNAL :: deltim
      EXTERNAL read_error
! Local Variables
      DOUBLE PRECISION :: dts
!***********************************************************************
      obsinit = 1

      IF ( Timestep==0 ) THEN
        Runoff = 0.0
        Precip = 0.0
        Tmax = 0.0
        Tmin = 0.0
        Solrad = 0.0
        Pan_evap = 0.0
        Snow = 0.0
        Streamflow_cfs = 0.0
        Streamflow_cms = 0.0
        Form_data = 0
        IF ( Nsfelev>0 ) Sfr_elev = 0.0
        IF ( Nratetbl>0 ) Gate_ht = 0.0
      ENDIF

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
      IF ( isleap(Starttime(1))==1 ) THEN
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
        IF ( getparam(MODNAME, 'runoff_units', 1, 'integer',
     +       Runoff_units)/=0 ) CALL read_error(2, 'runoff_units')
      ENDIF

      IF ( Rain_flag==1 ) THEN
        IF ( getparam(MODNAME, 'rain_code', 12, 'integer',
     +       Rain_code)/=0 ) CALL read_error(2, 'rain_code')
      ENDIF

      obsinit = 0
      END FUNCTION obsinit

! **********************************************************************
!     obsrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsrun()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Strmflow_flag
      USE PRMS_BASIN, ONLY: Timestep, CFS2CMS_CONV
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Nrain, Nsol
      IMPLICIT NONE
      INTEGER, EXTERNAL :: julian, isleap, readvar, getstep
      DOUBLE PRECISION, EXTERNAL :: deltim
      EXTERNAL :: dattim, read_error
! Local Variables
      INTEGER :: i
      DOUBLE PRECISION :: dt, dthr
! **********************************************************************
      obsrun = 1

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

      IF ( Nobs>0 ) THEN
        IF ( readvar(MODNAME, 'runoff')/=0 )
     +       CALL read_error(9, 'runoff')
        IF ( Runoff_units==1 ) THEN
          DO i = 1, Nobs
            IF ( Runoff(i)<0.0 ) Runoff(i) = -11.0
            Streamflow_cms(i) = Runoff(i)
            Streamflow_cfs(i) = Runoff(i)/CFS2CMS_CONV
          ENDDO
        ELSE
          DO i = 1, Nobs
            IF ( Runoff(i)<0.0 ) Runoff(i) = -11.0
            Streamflow_cms(i) = Runoff(i)*CFS2CMS_CONV
            Streamflow_cfs(i) = Runoff(i)
          ENDDO
        ENDIF
      ENDIF

      IF ( Nrain>0 ) THEN
        IF ( readvar(MODNAME, 'precip')/=0 )
     +       CALL read_error(9, 'precip')
      ENDIF

      IF ( Ntemp>0 ) THEN
        IF ( readvar(MODNAME, 'tmax')/=0 ) CALL read_error(9, 'tmax')
        IF ( readvar(MODNAME, 'tmin')/=0 ) CALL read_error(9, 'tmin')
      ENDIF

      IF ( Nsol>0 ) THEN
        IF ( readvar(MODNAME, 'solrad')/=0 )
     +       CALL read_error(9, 'solrad')
      ENDIF

      IF ( Nform>0 ) THEN
        IF ( readvar(MODNAME, 'form_data')/=0 )
     +       CALL read_error(8, 'form_data')
      ENDIF

      IF ( Nevap>0 ) THEN
        IF ( readvar(MODNAME, 'pan_evap')/=0 )
     +       CALL read_error(9, 'pan_evap')
      ENDIF

      IF ( Nsnow>0 ) THEN
        IF ( readvar(MODNAME, 'snow')/=0 ) CALL read_error(9, 'snow')
      ENDIF

      IF ( Rain_flag==1 ) THEN
        IF ( Rain_code(Nowmonth)==4 ) THEN
          IF ( readvar(MODNAME, 'rain_day')/=0 )
     +         CALL read_error(9, 'rain_day')
        ENDIF
      ENDIF

      IF ( Strmflow_flag==2 ) THEN
        IF ( Nsfelev>0 ) THEN
          IF ( readvar(MODNAME, 'sfr_elev')/=0 )
     +         CALL read_error(9, 'sfr_elev')
        ENDIF
        IF ( Nratetbl>0 ) THEN
          IF ( readvar(MODNAME, 'gate_ht')/=0 )
     +         CALL read_error(9, 'gate_ht')
        ENDIF
      ENDIF

      obsrun = 0
      END FUNCTION obsrun
