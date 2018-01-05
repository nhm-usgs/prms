!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
MODULE PRMS_OBS
    use kinds_mod, only: r4, r8, i4, i8
    IMPLICIT NONE

    !   Local Variables
    CHARACTER(LEN=:), allocatable, SAVE :: MODNAME

    !   Declared Variables
    REAL(r4), SAVE, ALLOCATABLE :: Runoff(:)
    REAL(r4), SAVE, ALLOCATABLE :: Precip(:)
    REAL(r4), SAVE, ALLOCATABLE :: Tmax(:)
    REAL(r4), SAVE, ALLOCATABLE :: Tmin(:)
    real(r8), SAVE, ALLOCATABLE :: Streamflow_cfs(:)
    real(r8), SAVE, ALLOCATABLE :: Streamflow_cms(:)

    !   Declared Parameters
    INTEGER(i4), SAVE :: Runoff_units(1)

    private :: obsdecl, obsinit, obs_restart
    public :: obs

    contains
        !***********************************************************************
        !     main obs routine
        !***********************************************************************
        INTEGER FUNCTION obs(dim_data)
            USE PRMS_MODULE, ONLY:Process, Save_vars_to_file, Init_vars_from_file
            use dimensions_mod, only: dimension_list
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data

            !***********************************************************************
            obs = 0

            !      IF ( Process(:3)=='run' ) THEN
            !        obs = obsrun()
            IF (Process == 'declare') THEN
                obs = obsdecl(dim_data)
            ELSEIF (Process == 'init') THEN
                IF (Init_vars_from_file == 1) CALL obs_restart(1)
                obs = obsinit()
            ELSEIF (Process == 'clean') THEN
                IF (Save_vars_to_file == 1) CALL obs_restart(0)
            ENDIF
        END FUNCTION obs

        !***********************************************************************
        !     obsdecl - makes public variable declarations for the obs module
        !   Declared Parameters
        !     rain_code
        !***********************************************************************
        INTEGER FUNCTION obsdecl(dim_data)
!            USE PRMS_OBS, ONLY:Runoff, Streamflow_cfs, Streamflow_cms, Precip, Tmin, Tmax, MODNAME
            USE PRMS_MODULE, ONLY: Ntemp, Nrain, Nobs, print_module
            use UTILS_PRMS, only: read_error
            use parameter_mod, only: declparam
            use variables_mod, only: declvar_real, declvar_dble
            ! use PRMS_MMFAPI, only: declvar_real, declvar_dble ! , declparam, getdim
            use dimensions_mod, only: dimension_list
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data

            ! Local Variable
            CHARACTER(LEN=:), allocatable, SAVE :: Version_obs

            !***********************************************************************
            obsdecl = 0

            Version_obs = 'obs.f90 2017-09-29 13:50:00Z'
            CALL print_module(Version_obs, 'Time Series Data            ', 90)
            MODNAME = 'obs'

            !   Declared Variables
            IF (Nobs > 0) THEN
                ALLOCATE (Runoff(Nobs))
                CALL declvar_real(MODNAME, 'runoff', 'nobs', Nobs, 'real', &
                        &       'Streamflow at each measurement station', 'runoff_units', Runoff)
                ALLOCATE (Streamflow_cfs(Nobs))
                CALL declvar_dble(MODNAME, 'streamflow_cfs', 'nobs', Nobs, 'double', &
                        &       'Streamflow at each measurement station', 'cfs', Streamflow_cfs)
                ALLOCATE (Streamflow_cms(Nobs))
                CALL declvar_dble(MODNAME, 'streamflow_cms', 'nobs', Nobs, 'double', &
                        &       'Streamflow at each measurement station', 'cms', Streamflow_cms)
                IF (declparam(MODNAME, 'runoff_units', 'one', 'integer', &
                        &       '0', '0', '1', &
                        &       'Measured streamflow units', 'Measured streamflow units (0=cfs; 1=cms)', &
                        &       'none', dim_data) /= 0) CALL read_error(1, 'runoff_units')
            ENDIF

            IF (Nrain > 0) THEN
                ALLOCATE (Precip(Nrain))
                CALL declvar_real(MODNAME, 'precip', 'nrain', Nrain, 'real', &
                        &       'Precipitation at each measurement station', 'precip_units', Precip)
            ENDIF

            IF (Ntemp > 0) THEN
                ALLOCATE (Tmin(Ntemp))
                CALL declvar_real(MODNAME, 'tmin', 'ntemp', Ntemp, 'real', &
                        &       'Minimum air temperature at each measurement station', 'temp_units', Tmin)
                ALLOCATE (Tmax(Ntemp))
                CALL declvar_real(MODNAME, 'tmax', 'ntemp', Ntemp, 'real', &
                        &       'Maximum air temperature at each measurement station', 'temp_units', Tmax)
            ENDIF

        END FUNCTION obsdecl

        !***********************************************************************
        !     obsinit - initializes obs module
        !***********************************************************************
        INTEGER FUNCTION obsinit()
            USE PRMS_MODULE, ONLY: Ntemp, Nrain, Nobs, Init_vars_from_file
            use UTILS_PRMS, only: read_error
            use parameter_mod, only: getparam
            ! use PRMS_MMFAPI, only: getparam
            IMPLICIT NONE

            !***********************************************************************
            obsinit = 0

            Runoff_units = 0
            IF (Nobs > 0) THEN
                IF (getparam(MODNAME, 'runoff_units', 1, 'integer', Runoff_units) /= 0) CALL read_error(2, 'runoff_units')
            ENDIF

            IF (Init_vars_from_file == 0) THEN
                IF (Nobs > 0) THEN
                    Runoff = 0.0
                    Streamflow_cfs = 0.0D0
                    Streamflow_cms = 0.0D0
                ENDIF
                IF (Nrain > 0) Precip = 0.0
                IF (Ntemp > 0) THEN
                    Tmax = 0.0
                    Tmin = 0.0
                ENDIF
            ENDIF
        END FUNCTION obsinit

        !***********************************************************************
        !     obs_restart - write or read obs restart file
        !***********************************************************************
        SUBROUTINE obs_restart(In_out)
            USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Nrain, Ntemp, Nobs
            use UTILS_PRMS, only: check_restart, check_restart_dimen
            IMPLICIT NONE

            ! Argument
            INTEGER(i4), INTENT(IN) :: In_out

            ! Local Variables
            INTEGER(i4) :: ierr, nrain_test, ntemp_test, nobs_test
            CHARACTER(LEN=3) :: module_name

            !***********************************************************************
            IF (In_out == 0) THEN
                WRITE (Restart_outunit) MODNAME
                WRITE (Restart_outunit) Nrain, Ntemp, Nobs
                IF (Nrain > 0) WRITE (Restart_outunit) Precip

                IF (Ntemp > 0) THEN
                    WRITE (Restart_outunit) Tmax
                    WRITE (Restart_outunit) Tmin
                ENDIF

                IF (Nobs > 0) THEN
                    WRITE (Restart_outunit) Runoff
                    WRITE (Restart_outunit) Streamflow_cfs
                    WRITE (Restart_outunit) Streamflow_cms
                ENDIF
            ELSE
                READ (Restart_inunit) module_name
                CALL check_restart(MODNAME, module_name)
                READ (Restart_inunit) nrain_test, ntemp_test, nobs_test
                ierr = 0
                CALL check_restart_dimen('nrain', nrain_test, Nrain, ierr)
                CALL check_restart_dimen('ntemp', ntemp_test, Ntemp, ierr)
                CALL check_restart_dimen('nobs', nobs_test, Nobs, ierr)
                IF (ierr == 1) STOP
                IF (Nrain > 0) READ (Restart_inunit) Precip
                IF (Ntemp > 0) THEN
                    READ (Restart_inunit) Tmax
                    READ (Restart_inunit) Tmin
                ENDIF
                IF (Nobs > 0) THEN
                    READ (Restart_inunit) Runoff
                    READ (Restart_inunit) Streamflow_cfs
                    READ (Restart_inunit) Streamflow_cms
                ENDIF
            ENDIF
        END SUBROUTINE obs_restart

END MODULE PRMS_OBS
