!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
MODULE PRMS_CLIMATEVARS
    use kinds_mod, only: r4, r8, i4, i8
    IMPLICIT NONE

    !   Local Variables
    CHARACTER(LEN=11), SAVE :: MODNAME

    ! Tmax_hru and Tmin_hru are in temp_units
    REAL(r4), SAVE, ALLOCATABLE :: Tmax_hru(:)
    REAL(r4), SAVE, ALLOCATABLE :: Tmin_hru(:)
    REAL(r4), SAVE, ALLOCATABLE :: Tmax_allsnow_f(:, :)
    REAL(r4), SAVE, ALLOCATABLE :: Tmax_allsnow_c(:, :)
    REAL(r4), SAVE, ALLOCATABLE :: Tmax_allrain_f(:, :)
    REAL(r4), SAVE, ALLOCATABLE :: Tmax_allrain(:, :)

    !   Declared Variables - Precip
    INTEGER(i4), SAVE, ALLOCATABLE :: Newsnow(:), Pptmix(:)
    real(r8), SAVE :: Basin_ppt(1), Basin_rain(1), Basin_snow(1), Basin_obs_ppt(1)
    REAL(r4), SAVE, ALLOCATABLE :: Hru_ppt(:)
    REAL(r4), SAVE, ALLOCATABLE :: Hru_rain(:)
    REAL(r4), SAVE, ALLOCATABLE :: Hru_snow(:)
    REAL(r4), SAVE, ALLOCATABLE :: Prmx(:)

    !   Declared Variables - Temp
    real(r8), SAVE :: Basin_temp(1), Basin_tmax(1), Basin_tmin(1)
    REAL(r4), SAVE :: Solrad_tmax(1), Solrad_tmin(1)
    REAL(r4), SAVE, ALLOCATABLE :: Tmaxf(:), Tminf(:), Tavgf(:)
    REAL(r4), SAVE, ALLOCATABLE :: Tmaxc(:), Tminc(:), Tavgc(:)

    !   Declared Variables - Transp
    INTEGER(i4), SAVE :: Basin_transp_on
    INTEGER(i4), SAVE, ALLOCATABLE :: Transp_on(:)

    !   Declared Variables - Potetential ET
    real(r8), SAVE :: Basin_potet(1)
    REAL(r4), SAVE, ALLOCATABLE :: Potet(:)

    !   Declared Parameters and Variables - Solar Radiation
    INTEGER(i4), SAVE :: Basin_solsta
    real(r8), SAVE :: Basin_swrad(1)
    real(r8), save :: Basin_orad(1), Basin_horad(1)
    REAL(r4), SAVE, ALLOCATABLE :: Swrad(:), Orad_hru(:)
    REAL(r4), SAVE, ALLOCATABLE :: Ppt_rad_adj(:, :), Radmax(:, :), Radj_sppt(:), Radj_wppt(:)

    !   Declared Parameters - Temp
    INTEGER(i4), SAVE :: Temp_units
    REAL(r4), SAVE, ALLOCATABLE :: Tmax_aspect_adjust(:, :), Tmin_aspect_adjust(:, :)

    !   Declared Parameters - Precip
    INTEGER(i4), SAVE :: Precip_units
    REAL(r4), SAVE, ALLOCATABLE :: Tmax_allsnow(:, :), Adjmix_rain(:, :), Tmax_allrain_offset(:, :)

    private :: climateflow_decl, climateflow_init, climateflow_restart
    public :: climateflow, precip_form, temp_set

    contains
        !***********************************************************************
        !     Main climateflow routine
        !***********************************************************************
        INTEGER FUNCTION climateflow(dim_data, param_data, var_data)
            USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
            use dimensions_mod, only: dimension_list
            use parameter_arr_mod, only: parameter_arr_t
            use variables_arr_mod, only: variables_arr_t
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data
            type(parameter_arr_t), intent(inout) :: param_data
            type(variables_arr_t), intent(inout) :: var_data

            !***********************************************************************
            climateflow = 0

            IF (Process == 'declare') THEN
                climateflow = climateflow_decl(dim_data, param_data, var_data)
            ELSEIF (Process == 'init') THEN
                IF (Init_vars_from_file == 1) CALL climateflow_restart(1)
                climateflow = climateflow_init(param_data)
            ELSEIF (Process == 'clean') THEN
                IF (Save_vars_to_file == 1) CALL climateflow_restart(0)
            ENDIF
        END FUNCTION climateflow

        !***********************************************************************
        !     climateflow_decl - declare climate and flow variables and parameters
        !***********************************************************************
        INTEGER FUNCTION climateflow_decl(dim_data, param_data, var_data)
            USE PRMS_MODULE, ONLY: Model, Nhru, Temp_module, Precip_module, Transp_module, Et_module, &
                    &    Solrad_flag, Solrad_module, print_module
            use UTILS_PRMS, only: read_error
            ! use parameter_mod, only: declparam
            ! use variables_mod, only: declvar_real, declvar_dble, declvar_int
            ! use PRMS_MMFAPI, only: declvar_real, declvar_dble, declvar_int
            use dimensions_mod, only: dimension_list
            use parameter_arr_mod, only: parameter_arr_t
            use variables_arr_mod, only: variables_arr_t
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data
            type(parameter_arr_t), intent(inout) :: param_data
            type(variables_arr_t), intent(inout) :: var_data

            ! Local Variables
            CHARACTER(LEN=80), SAVE :: Version_climateflow

            !***********************************************************************
            climateflow_decl = 0

            Version_climateflow = 'climateflow.f90 2017-09-29 13:47:00Z'
            CALL print_module(Version_climateflow, 'Common States and Fluxes    ', 90)
            MODNAME = 'climateflow'

            ALLOCATE (Tmaxf(Nhru))
            CALL var_data%declvar_real(Temp_module, 'tmaxf', 'nhru', Nhru, 'real', &
                    &     'Maximum air temperature distributed to each HRU', 'degrees Fahrenheit', Tmaxf)

            ALLOCATE (Tminf(Nhru))
            CALL var_data%declvar_real(Temp_module, 'tminf', 'nhru', Nhru, 'real', &
                    &     'Minimum air temperature distributed to each HRU', 'degrees Fahrenheit', Tminf)

            ALLOCATE (Tavgf(Nhru))
            CALL var_data%declvar_real(Temp_module, 'tavgf', 'nhru', Nhru, 'real', &
                    &     'Average air temperature distributed to each HRU', 'degrees Fahrenheit', Tavgf)

            ALLOCATE (Tmaxc(Nhru))
            CALL var_data%declvar_real(Temp_module, 'tmaxc', 'nhru', Nhru, 'real', &
                    &     'Maximum air temperature distributed to each HRU', 'degrees Celsius', Tmaxc)

            ALLOCATE (Tminc(Nhru))
            CALL var_data%declvar_real(Temp_module, 'tminc', 'nhru', Nhru, 'real', &
                    &     'Minimum air temperature distributed to each HRU', 'degrees Celsius', Tminc)

            ALLOCATE (Tavgc(Nhru))
            CALL var_data%declvar_real(Temp_module, 'tavgc', 'nhru', Nhru, 'real', &
                    &     'Average air temperature distributed to each HRU', 'degrees Celsius', Tavgc)

            CALL var_data%declvar_dble(Temp_module, 'basin_tmax', 'one', 1, 'double', &
                    &     'Basin area-weighted average maximum air temperature', 'temp_units', Basin_tmax)

            CALL var_data%declvar_dble(Temp_module, 'basin_tmin', 'one', 1, 'double', &
                    &     'Basin area-weighted average minimum air temperature', 'temp_units', Basin_tmin)

            CALL var_data%declvar_dble(Temp_module, 'basin_temp', 'one', 1, 'double', &
                    &     'Basin area-weighted average air temperature', 'temp_units', Basin_temp)

            CALL var_data%declvar_real(Temp_module, 'solrad_tmax', 'one', 1, 'real', &
                    &     'Basin daily maximum temperature for use with solar radiation calculations', 'temp_units', Solrad_tmax)

            CALL var_data%declvar_real(Temp_module, 'solrad_tmin', 'one', 1, 'real', &
                    &     'Basin daily minimum temperature for use with solar radiation calculations', 'temp_units', Solrad_tmin)

            ! PRECIPITATION VARIABLES AND PARAMETERS
            ALLOCATE (Pptmix(Nhru))
            CALL var_data%declvar_int(Precip_module, 'pptmix', 'nhru', Nhru, 'integer', &
                    &     'Flag to indicate if precipitation is a mixture of rain and snow for each HRU (0=no; 1=yes)', 'none', Pptmix)

            ALLOCATE (Newsnow(Nhru))
            CALL var_data%declvar_int(Precip_module, 'newsnow', 'nhru', Nhru, 'integer', &
                    &    'Flag to indicate if new snow fell on each HRU (0=no; 1=yes)', 'none', Newsnow)

            ALLOCATE (Prmx(Nhru))
            CALL var_data%declvar_real(Precip_module, 'prmx', 'nhru', Nhru, 'real', &
                    &     'Fraction of rain in a mixed precipitation event for each HRU', 'decimal fraction', Prmx)

            CALL var_data%declvar_dble(Precip_module, 'basin_rain', 'one', 1, 'double', &
                    &     'Basin area-weighted average rainfall', 'inches', Basin_rain)

            CALL var_data%declvar_dble(Precip_module, 'basin_snow', 'one', 1, 'double', &
                    &     'Basin area-weighted average snowfall for basin', 'inches', Basin_snow)

            CALL var_data%declvar_dble(Precip_module, 'basin_ppt', 'one', 1, 'double', &
                    &     'Basin area-weighted average precipitation', 'inches', Basin_ppt)

            ! DANGER - Not sure what to do about this one.  For right now
            !          I'm setting basin_ppt and basin_obs_ppt to the same
            !          variable.  In the precip_1sta module, basin_obs_ppt
            !          seems to be the area weighted precipitation average before
            !          the correction factor is applied.  In other modules,
            !          the correction "error" is applied to the station
            !          precipitation rather than the hru precipitation.
            CALL var_data%declvar_dble(Precip_module, 'basin_obs_ppt', 'one', 1, 'double', &
                    &     'Basin area-weighted average measured precipitation', 'inches', Basin_obs_ppt)

            ALLOCATE (Hru_ppt(Nhru))
            CALL var_data%declvar_real(Precip_module, 'hru_ppt', 'nhru', Nhru, 'real', &
                    &     'Precipitation distributed to each HRU', 'inches', Hru_ppt)

            ALLOCATE (Hru_rain(Nhru))
            CALL var_data%declvar_real(Precip_module, 'hru_rain', 'nhru', Nhru, 'real', &
                    &     'Rain distributed to each HRU', 'inches', Hru_rain)

            ALLOCATE (Hru_snow(Nhru))
            CALL var_data%declvar_real(Precip_module, 'hru_snow', 'nhru', Nhru, 'real', &
                    &     'Snow distributed to each HRU', 'inches', Hru_snow)

            ! Solar Radiation variables
            ALLOCATE (Swrad(Nhru))
            CALL var_data%declvar_real(Solrad_module, 'swrad', 'nhru', Nhru, 'real', &
                    &     'Shortwave radiation distributed to each HRU', 'Langleys', Swrad)

            CALL var_data%declvar_dble(Solrad_module, 'basin_horad', 'one', 1, 'double', &
                    &     'Potential shortwave radiation for the basin centroid', 'Langleys', Basin_horad)

            CALL var_data%declvar_dble(Solrad_module, 'basin_swrad', 'one', 1, 'double', &
                    &     'Basin area-weighted average shortwave radiation', 'Langleys', Basin_swrad)

            IF (Solrad_flag == 1 .OR. Solrad_flag == 2 .OR. Model == 99) THEN
                CALL var_data%declvar_dble(Solrad_module, 'basin_orad', 'one', 1, 'double', &
                        &       'Basin area-weighted average solar radiation on a horizontal surface', 'Langleys', Basin_orad)

                ALLOCATE (Orad_hru(Nhru))
                CALL var_data%declvar_real(Solrad_module, 'orad_hru', 'nhru', Nhru, 'real', &
                        &       'Solar radiation on a horizontal surface for each HRU', 'Langleys', Orad_hru)
            ENDIF

            ! Transpiration Variables
            ALLOCATE (Transp_on(Nhru))
            CALL var_data%declvar_int(Transp_module, 'transp_on', 'nhru', Nhru, 'integer', &
                    &     'Flag indicating whether transpiration is occurring (0=no; 1=yes)', 'none', Transp_on)

            CALL var_data%declvar_int(Transp_module, 'basin_transp_on', 'one', 1, 'integer', &
                    &     'Flag indicating whether transpiration is occurring anywhere in the basin (0=no; 1=yes)', 'none', Basin_transp_on)

            ! Potential ET Variables
            ALLOCATE (Potet(Nhru))
            CALL var_data%declvar_real(Et_module, 'potet', 'nhru', Nhru, 'real', &
                    &     'Potential ET for each HRU', 'inches', Potet)

            CALL var_data%declvar_dble(Et_module, 'basin_potet', 'one', 1, 'double', &
                    &     'Basin area-weighted average potential ET', 'inches', Basin_potet)


            ! Allocate local variables
            ALLOCATE (Tmax_hru(Nhru), Tmin_hru(Nhru))
            ALLOCATE (Tmax_allsnow_f(Nhru, 12), Tmax_allsnow_c(Nhru, 12), Tmax_allrain_f(Nhru, 12))

            ! Declare Parameters
            ALLOCATE (Tmax_allrain_offset(Nhru, 12), Tmax_allrain(Nhru, 12))
            IF (param_data%declparam(Precip_module, 'tmax_allrain_offset', 'nhru,nmonths', 'real', &
                    &     '1.0', '0.0', '50.0', &
                    &     'Precipitation is rain if HRU max temperature >= tmax_allsnow + this value', &
                    &     'Monthly (January to December) maximum air temperature' // &
                            &     ' when precipitation is assumed to be rain; if HRU air' // &
                            &     ' temperature is greater than or equal to tmax_allsnow plus this value, precipitation is rain', &
                    &     'temp_units', dim_data) /= 0) CALL read_error(1, 'tmax_allrain_offset')

            ALLOCATE (Tmax_allsnow(Nhru, 12))
            IF (param_data%declparam(Precip_module, 'tmax_allsnow', 'nhru,nmonths', 'real', &
                    &     '32.0', '-10.0', '40.0', &
                    &     'Maximum temperature when precipitation is all snow', &
                    &     'Maximum air temperature when precipitation is assumed' // &
                            &     ' to be snow; if HRU air temperature is less than or equal to this value, precipitation is snow', &
                    &     'temp_units', dim_data) /= 0) CALL read_error(1, 'tmax_allsnow')

            ALLOCATE (Adjmix_rain(Nhru, 12))
            IF (param_data%declparam(Precip_module, 'adjmix_rain', 'nhru,nmonths', 'real', &
                    &     '1.0', '0.0', '3.0', &
                    &     'Adjustment factor for rain in a rain/snow mix', &
                    &     'Monthly (January to December) factor to adjust rain proportion in a mixed rain/snow event', &
                    &     'decimal fraction', dim_data) /= 0) CALL read_error(1, 'adjmix_rain')

            IF (param_data%declparam(Temp_module, 'temp_units', 'one', 'integer', &
                    &     '0', '0', '1', &
                    &     'Units flag for measured temperature', &
                    &     'Flag to indicate the units of measured air-temperature values (0=Fahrenheit; 1=Celsius)', &
                    &     'none', dim_data) /= 0) CALL read_error(1, 'temp_units')

            IF (param_data%declparam(Precip_module, 'precip_units', 'one', 'integer', &
                    &     '0', '0', '1', &
                    &     'Units for measured precipitation', &
                    &     'Units for measured precipitation (0=inches; 1=mm)', &
                    &     'none', dim_data) /= 0) CALL read_error(1, 'precip_units')

            ALLOCATE (Ppt_rad_adj(Nhru, 12))
            IF (param_data%declparam(Solrad_module, 'ppt_rad_adj', 'nhru,nmonths', 'real', &
                    &     '0.02', '0.0', '0.5', &
                    &     'Radiation reduced if HRU precipitation above this value', &
                    &     'Monthly minimum precipitation, if HRU precipitation exceeds this value, radiation is' // &
                            &     ' multiplied by radj_sppt or radj_wppt adjustment factor', &
                    &     'inches', dim_data) /= 0) CALL read_error(1, 'ppt_rad_adj')
            ALLOCATE (Radj_sppt(Nhru))
            IF (param_data%declparam(Solrad_module, 'radj_sppt', 'nhru', 'real', &
                    &     '0.44', '0.0', '1.0', &
                    &     'Adjustment to solar radiation on precipitation day - summer', &
                    &     'Adjustment factor for computed solar radiation for summer day with greater than' // &
                            &     ' ppt_rad_adj inches of precipitation for each HRU', &
                    &     'decimal fraction', dim_data) /= 0) CALL read_error(1, 'radj_sppt')
            ALLOCATE (Radj_wppt(Nhru))
            IF (param_data%declparam(Solrad_module, 'radj_wppt', 'nhru', 'real', &
                    &     '0.5', '0.0', '1.0', &
                    &     'Adjustment to solar radiation on precipitation day - winter', &
                    &     'Adjustment factor for computed solar radiation for winter day with greater than' // &
                            &     ' ppt_rad_adj inches of precipitation for each HRU', &
                    &     'decimal fraction', dim_data) /= 0) CALL read_error(1, 'radj_wppt')
            ALLOCATE (Radmax(Nhru, 12))
            IF (param_data%declparam(Solrad_module, 'radmax', 'nhru,nmonths', 'real', &
                    &     '0.8', '0.1', '1.0', &
                    &     'Maximum fraction of potential solar radiation', &
                    &     'Monthly (January to December) maximum fraction of the potential solar radiation' // &
                            &     ' that may reach the ground due to haze, dust, smog, and so forth, for each HRU', &
                    &     'decimal fraction', dim_data) /= 0) CALL read_error(1, 'radmax')

        END FUNCTION climateflow_decl

        !***********************************************************************
        !     climateflow_init - Initialize module - get parameter values,
        !                        set initial values and check parameter values
        !***********************************************************************
        INTEGER FUNCTION climateflow_init(param_data)
            ! USE PRMS_CLIMATEVARS
            USE PRMS_MODULE, ONLY: Nhru, Temp_module, Precip_module, Solrad_module, Init_vars_from_file, Solrad_flag
            use UTILS_PRMS, only: read_error
            use conversions_mod, only: c_to_f, f_to_c
            use parameter_arr_mod, only: parameter_arr_t
            IMPLICIT NONE

            type(parameter_arr_t), intent(in) :: param_data

            ! Local variables
            INTEGER(i4) :: i, j

            !***********************************************************************
            climateflow_init = 0

            IF (param_data%getparam(Temp_module, 'temp_units', 1, 'integer', Temp_units) /= 0) CALL read_error(2, 'temp_units')

            IF (param_data%getparam(Precip_module, 'tmax_allsnow', Nhru * 12, 'real', Tmax_allsnow) /= 0) CALL read_error(2, 'tmax_allsnow')

            IF (param_data%getparam(Precip_module, 'tmax_allrain_offset', Nhru * 12, 'real', Tmax_allrain_offset) /= 0) &
                                    CALL read_error(2, 'tmax_allrain_offset')

            ! Set tmax_allrain in units of the input values
            ! tmax_allsnow must be in the units of the input values
            IF (Temp_units == 0) THEN
                Tmax_allsnow_f = Tmax_allsnow
                DO j = 1, 12
                    DO i = 1, Nhru
                        Tmax_allrain_f(i, j) = Tmax_allsnow(i, j) + Tmax_allrain_offset(i, j)
                        Tmax_allsnow_c(i, j) = f_to_c(Tmax_allsnow(i, j))
                    ENDDO
                ENDDO
                Tmax_allrain = Tmax_allrain_f
            ELSE
                Tmax_allsnow_c = Tmax_allsnow
                DO i = 1, 12
                    DO j = 1, Nhru
                        Tmax_allsnow_f(j, i) = c_to_f(Tmax_allsnow(j, i))
                        Tmax_allrain(j, i) = Tmax_allsnow(j, i) + Tmax_allrain_offset(j, i)
                        Tmax_allrain_f(j, i) = c_to_f(Tmax_allrain(j, i))
                    ENDDO
                ENDDO
            ENDIF

            IF (param_data%getparam(Precip_module, 'adjmix_rain', Nhru * 12, 'real', Adjmix_rain) /= 0) CALL read_error(2, 'adjmix_rain')

            IF (param_data%getparam(Precip_module, 'precip_units', 1, 'integer', Precip_units) /= 0) CALL read_error(2, 'precip_units')

            IF (Solrad_flag == 1 .OR. Solrad_flag == 2) THEN
                IF (param_data%getparam(Solrad_module, 'radj_sppt', Nhru, 'real', Radj_sppt) /= 0) CALL read_error(2, 'radj_sppt')
                IF (param_data%getparam(Solrad_module, 'radj_wppt', Nhru, 'real', Radj_wppt) /= 0) CALL read_error(2, 'radj_wppt')
                IF (param_data%getparam(Solrad_module, 'ppt_rad_adj', Nhru * 12, 'real', Ppt_rad_adj) /= 0) CALL read_error(2, 'ppt_rad_adj')
                IF (param_data%getparam(Solrad_module, 'radmax', Nhru * 12, 'real', Radmax) /= 0) CALL read_error(2, 'radmax')
            ENDIF

            ! FLOW VARIABLES AND PARAMETERS
            IF (Init_vars_from_file == 1) RETURN

            Tmaxf = 0.0
            Tminf = 0.0
            Tavgf = 0.0
            Tmaxc = 0.0
            Tminc = 0.0
            Tavgc = 0.0
            Tmax_hru = 0.0
            Tmin_hru = 0.0
            Solrad_tmax = 0.0
            Solrad_tmin = 0.0
            Basin_temp = 0.0D0
            Basin_tmax = 0.0D0
            Basin_tmin = 0.0D0
            Pptmix = 0
            Newsnow = 0
            Prmx = 0.0
            Basin_ppt = 0.0D0
            Basin_obs_ppt = 0.0D0
            Basin_rain = 0.0D0
            Basin_snow = 0.0D0
            Hru_ppt = 0.0
            Hru_rain = 0.0
            Hru_snow = 0.0
            Swrad = 0.0
            Basin_horad = 0.0D0
            Basin_swrad = 0.0D0
            Transp_on = 0
            Basin_transp_on = 0
            Basin_potet = 0.0D0
            Potet = 0.0
            Basin_orad = 0.0D0

            IF (Solrad_flag == 1 .OR. Solrad_flag == 2) Orad_hru = 0.0
        END FUNCTION climateflow_init

        !***********************************************************************
        !     Write or read restart file
        !***********************************************************************
        SUBROUTINE climateflow_restart(In_out)
            USE PRMS_MODULE, ONLY:Restart_outunit, Restart_inunit, Solrad_flag
            use UTILS_PRMS, only: check_restart
            IMPLICIT NONE

            ! Argument
            INTEGER(i4), INTENT(IN) :: In_out

            ! Local Variable
            CHARACTER(LEN=11) :: module_name

            !***********************************************************************
            IF (In_out == 0) THEN
                WRITE (Restart_outunit) MODNAME
                WRITE (Restart_outunit) Basin_ppt, Basin_rain, Basin_snow, Basin_obs_ppt, Basin_temp, Basin_orad, &
                        &          Basin_tmax, Basin_tmin, Solrad_tmax, Solrad_tmin, Basin_transp_on, Basin_potet, Basin_horad, &
                        &          Basin_swrad
                WRITE (Restart_outunit) Tmax_hru
                WRITE (Restart_outunit) Tmin_hru
                WRITE (Restart_outunit) Newsnow
                WRITE (Restart_outunit) Pptmix
                WRITE (Restart_outunit) Hru_ppt
                WRITE (Restart_outunit) Hru_rain
                WRITE (Restart_outunit) Hru_snow
                WRITE (Restart_outunit) Prmx
                WRITE (Restart_outunit) Tmaxf
                WRITE (Restart_outunit) Tminf
                WRITE (Restart_outunit) Tavgf
                WRITE (Restart_outunit) Tmaxc
                WRITE (Restart_outunit) Tminc
                WRITE (Restart_outunit) Tavgc
                WRITE (Restart_outunit) Transp_on
                WRITE (Restart_outunit) Potet
                WRITE (Restart_outunit) Swrad
                IF (Solrad_flag == 1 .OR. Solrad_flag == 2) WRITE (Restart_outunit) Orad_hru
            ELSE
                READ (Restart_inunit) module_name
                CALL check_restart(MODNAME, module_name)
                READ (Restart_inunit) Basin_ppt, Basin_rain, Basin_snow, Basin_obs_ppt, Basin_temp, Basin_orad, &
                        &         Basin_tmax, Basin_tmin, Solrad_tmax, Solrad_tmin, Basin_transp_on, Basin_potet, Basin_horad, &
                        &         Basin_swrad
                READ (Restart_inunit) Tmax_hru
                READ (Restart_inunit) Tmin_hru
                READ (Restart_inunit) Newsnow
                READ (Restart_inunit) Pptmix
                READ (Restart_inunit) Hru_ppt
                READ (Restart_inunit) Hru_rain
                READ (Restart_inunit) Hru_snow
                READ (Restart_inunit) Prmx
                READ (Restart_inunit) Tmaxf
                READ (Restart_inunit) Tminf
                READ (Restart_inunit) Tavgf
                READ (Restart_inunit) Tmaxc
                READ (Restart_inunit) Tminc
                READ (Restart_inunit) Tavgc
                READ (Restart_inunit) Transp_on
                READ (Restart_inunit) Potet
                READ (Restart_inunit) Swrad
                IF (Solrad_flag == 1 .OR. Solrad_flag == 2) READ (Restart_inunit) Orad_hru
            ENDIF
        END SUBROUTINE climateflow_restart

        !***********************************************************************
        !     Computes precipitation form (rain, snow or mix) and depth for each HRU
        !***********************************************************************
        SUBROUTINE precip_form(Precip, Hru_ppt, Hru_rain, Hru_snow, Tmaxf, &
                &           Tminf, Pptmix, Newsnow, Prmx, Tmax_allrain_f, Rain_adj, &
                &           Snow_adj, Adjmix_rain, Hru_area, Sum_obs, Tmax_allsnow_f)
            USE prms_constants, ONLY: NEARZERO
            use PRMS_SET_TIME, only: print_date
            IMPLICIT NONE

            ! Functions
            INTRINSIC ABS, DBLE

            ! Arguments
            REAL(r4), INTENT(IN) :: Tmax_allrain_f, Tmax_allsnow_f, Rain_adj, Snow_adj
            REAL(r4), INTENT(IN) :: Adjmix_rain, Tmaxf, Tminf, Hru_area
            real(r8), INTENT(INOUT) :: Sum_obs
            INTEGER(i4), INTENT(INOUT) :: Pptmix, Newsnow
            REAL(r4), INTENT(INOUT) :: Precip, Hru_rain, Hru_snow, Prmx, Hru_ppt

            ! Local Variables
            REAL(r4) :: tdiff

            !***********************************************************************
            ! basin precipitation before adjustments
            Sum_obs = Sum_obs + DBLE(Precip * Hru_area)

            !******If maximum temperature is below or equal to the base temperature
            !******for snow then precipitation is all snow
            IF (Tmaxf <= Tmax_allsnow_f) THEN
                Hru_ppt = Precip * Snow_adj
                Hru_snow = Hru_ppt
                Newsnow = 1

                !******If minimum temperature is above base temperature for snow or
                !******maximum temperature is above all_rain temperature then
                !******precipitation is all rain
            ELSEIF (Tminf > Tmax_allsnow_f .OR. Tmaxf >= Tmax_allrain_f) THEN
                Hru_ppt = Precip * Rain_adj
                Hru_rain = Hru_ppt
                Prmx = 1.0

                !******Otherwise precipitation is a mixture of rain and snow
            ELSE
                tdiff = Tmaxf - Tminf
                IF (tdiff < 0.0) THEN
                    PRINT *, 'ERROR, tmax < tmin (degrees Fahrenheit), tmax:', Tmaxf, ' tmin:', TminF
                    CALL print_date(1)
                ENDIF
                IF (ABS(tdiff) < NEARZERO) tdiff = 0.0001
                Prmx = ((Tmaxf - Tmax_allsnow_f) / tdiff) * Adjmix_rain
                IF (Prmx < 0.0) Prmx = 0.0

                !******Unless mixture adjustment raises the proportion of rain to
                !******greater than or equal to 1.0 in which case it all rain
                !******If not, it is a rain/snow mixture
                IF (Prmx < 1.0) THEN
                    Pptmix = 1
                    Hru_ppt = Precip * Snow_adj
                    Hru_rain = Prmx * Hru_ppt
                    Hru_snow = Hru_ppt - Hru_rain
                    Newsnow = 1
                ELSE
                    Hru_ppt = Precip * Rain_adj
                    Hru_rain = Hru_ppt
                    Prmx = 1.0
                ENDIF
            ENDIF
            Basin_ppt = Basin_ppt + DBLE(Hru_ppt * Hru_area)
            Basin_rain = Basin_rain + DBLE(Hru_rain * Hru_area)
            Basin_snow = Basin_snow + DBLE(Hru_snow * Hru_area)

        END SUBROUTINE precip_form

        !***********************************************************************
        !     Sets temperatures in both system of units for each HRU
        !***********************************************************************
        SUBROUTINE temp_set(Ihru, Tmax, Tmin, Tmaxf, Tminf, Tavgf, Tmaxc, Tminc, Tavgc, Hru_area)
            USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
            USE prms_constants, ONLY: MINTEMP, MAXTEMP
            use conversions_mod, only: c_to_f, f_to_c
            IMPLICIT NONE

            ! Arguments
            INTEGER(i4), INTENT(IN) :: Ihru
            REAL(r4), INTENT(IN) :: Tmax, Tmin, Hru_area
            REAL(r4), INTENT(OUT) :: Tmaxf, Tminf, Tavgf, Tmaxc, Tminc, Tavgc

            ! Functions
            INTRINSIC DBLE

            !***********************************************************************
            IF (Temp_units == 0) THEN
                !       degrees Fahrenheit
                Tmaxf = Tmax
                Tminf = Tmin
                Tavgf = (Tmax + Tmin) * 0.5
                Tmaxc = f_to_c(Tmax)
                Tminc = f_to_c(Tmin)
                Tavgc = f_to_c(Tavgf)
                Basin_temp = Basin_temp + DBLE(Tavgf * Hru_area)
            ELSE
                !       degrees Celsius
                Tmaxc = Tmax
                Tminc = Tmin
                Tavgc = (Tmax + Tmin) * 0.5
                Tmaxf = c_to_f(Tmax)
                Tminf = c_to_f(Tmin)
                Tavgf = c_to_f(Tavgc)
                Basin_temp = Basin_temp + DBLE(Tavgc * Hru_area)
            ENDIF

            IF (Tminf < MINTEMP .OR. Tmaxf > MAXTEMP) THEN
                PRINT *, 'ERROR, invalid temperature value for HRU:', Ihru, Tminf, Tmaxf, ' Date:', Nowyear, Nowmonth, Nowday
                STOP
            ENDIF
            Tmax_hru(Ihru) = Tmax ! in units temp_units
            Tmin_hru(Ihru) = Tmin ! in units temp_units

            Basin_tmax = Basin_tmax + DBLE(Tmax * Hru_area)
            Basin_tmin = Basin_tmin + DBLE(Tmin * Hru_area)

        END SUBROUTINE temp_set

END MODULE PRMS_CLIMATEVARS
