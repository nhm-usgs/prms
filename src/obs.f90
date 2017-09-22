!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
      MODULE PRMS_OBS
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=3), SAVE :: MODNAME
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Runoff(:), Precip(:)
      REAL, SAVE, ALLOCATABLE :: Tmax(:), Tmin(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Streamflow_cfs(:), Streamflow_cms(:)
!   Declared Parameters
      INTEGER, SAVE :: Runoff_units
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun
      EXTERNAL :: obs_restart
!***********************************************************************
      obs = 0

      IF ( Process(:3)=='run' ) THEN
        obs = obsrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        obs = obsdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL obs_restart(1)
        obs = obsinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL obs_restart(0)
      ENDIF

      END FUNCTION obs

!***********************************************************************
!     obsdecl - makes public variable declarations for the obs module
!   Declared Parameters
!     rain_code
!***********************************************************************
      INTEGER FUNCTION obsdecl()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Precip_flag, Model, Ntemp, Nrain, Nobs
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, getdim, declparam
      EXTERNAL read_error, print_module
! Local Variable
      CHARACTER(LEN=80), SAVE :: Version_obs
!***********************************************************************
      obsdecl = 0

      Version_obs = 'obs.f90 2017-09-22 12:39:00Z'
      CALL print_module(Version_obs, 'Time Series Data            ', 90)
      MODNAME = 'obs'

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
      ENDIF

      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Ntemp, Nrain, Nobs, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
!***********************************************************************
      obsinit = 0

      Runoff_units = 0
      IF ( Nobs>0 ) THEN
        IF ( getparam(MODNAME, 'runoff_units', 1, 'integer', Runoff_units)/=0 ) CALL read_error(2, 'runoff_units')
      ENDIF

      IF ( Init_vars_from_file==0 ) THEN
        IF ( Nobs>0 ) THEN
          Runoff = 0.0
          Streamflow_cfs = 0.0D0
          Streamflow_cms = 0.0D0
        ENDIF
        IF ( Nrain>0 ) Precip = 0.0
        IF ( Ntemp>0 ) THEN
          Tmax = 0.0
          Tmin = 0.0
        ENDIF
      ENDIF

      END FUNCTION obsinit

! **********************************************************************
!     obsrun - runs obs module
! **********************************************************************
      INTEGER FUNCTION obsrun()
      USE PRMS_OBS
      USE PRMS_MODULE, ONLY: Ntemp, Nrain, Nobs
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV
      USE PRMS_SET_TIME, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC DBLE
      INTEGER, EXTERNAL :: readvar
      EXTERNAL :: read_error
! Local Variables
      INTEGER :: i
! **********************************************************************
      obsrun = 0

      IF ( Nobs>0 ) THEN
        IF ( readvar(MODNAME, 'runoff')/=0 ) CALL read_error(9, 'runoff')
        IF ( Runoff_units==1 ) THEN
          DO i = 1, Nobs
            Streamflow_cms(i) = DBLE( Runoff(i) )
            Streamflow_cfs(i) = Streamflow_cms(i)/CFS2CMS_CONV
          ENDDO
        ELSE
          DO i = 1, Nobs
            Streamflow_cfs(i) = DBLE( Runoff(i) )
            Streamflow_cms(i) = Streamflow_cfs(i)*CFS2CMS_CONV
          ENDDO
        ENDIF
      ENDIF

      IF ( Nrain>0 ) THEN
        IF ( readvar(MODNAME, 'precip')/=0 ) CALL read_error(9, 'precip')
      ENDIF

      IF ( Ntemp>0 ) THEN
        IF ( readvar(MODNAME, 'tmax')/=0 ) CALL read_error(9, 'tmax')
        IF ( readvar(MODNAME, 'tmin')/=0 ) CALL read_error(9, 'tmin')
      ENDIF

      END FUNCTION obsrun

!***********************************************************************
!     obs_restart - write or read obs restart file
!***********************************************************************
      SUBROUTINE obs_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Nrain, Ntemp, Nobs
      USE PRMS_OBS
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart, check_restart_dimen
      ! Local Variables
      INTEGER :: ierr, nrain_test, ntemp_test, nobs_test
      CHARACTER(LEN=3) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Nrain, Ntemp, Nobs
        IF ( Nrain>0 ) WRITE ( Restart_outunit ) Precip
        IF ( Ntemp>0 ) THEN
          WRITE ( Restart_outunit ) Tmax
          WRITE ( Restart_outunit ) Tmin
        ENDIF
        IF ( Nobs>0 ) THEN
          WRITE ( Restart_outunit ) Runoff
          WRITE ( Restart_outunit ) Streamflow_cfs
          WRITE ( Restart_outunit ) Streamflow_cms
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) nrain_test, ntemp_test, nobs_test
        ierr = 0
        CALL check_restart_dimen('nrain', nrain_test, Nrain, ierr)
        CALL check_restart_dimen('ntemp', ntemp_test, Ntemp, ierr)
        CALL check_restart_dimen('nobs', nobs_test, Nobs, ierr)
        IF ( ierr==1 ) STOP
        IF ( Nrain>0 ) READ ( Restart_inunit ) Precip
        IF ( Ntemp>0 ) THEN
          READ ( Restart_inunit ) Tmax
          READ ( Restart_inunit ) Tmin
        ENDIF
        IF ( Nobs>0 ) THEN
          READ ( Restart_inunit ) Runoff
          READ ( Restart_inunit ) Streamflow_cfs
          READ ( Restart_inunit ) Streamflow_cms
        ENDIF
      ENDIF
      END SUBROUTINE obs_restart
