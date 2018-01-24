



!***********************************************************************
!     Code piece from main subroutine for declare procedure
!***********************************************************************
subroutine PRMS_header()
    use PRMS_MODULE, only: Print_debug, PRMS_output_unit, PRMS_VERSION, PRMS_versn, &
                           Version_read_control_file, Version_read_parameter_file, print_module
    use prms_constants, only: EQULS
    implicit none

    !***********************************************************************
    if (Print_debug > -2) then
        print 10, PRMS_VERSION
        write (PRMS_output_unit, 10) PRMS_VERSION
        print 15
        print 9002
        write (PRMS_output_unit, 15)
        print 16, EQULS
        write (PRMS_output_unit, 16) EQULS
    endif

    10  FORMAT (/, 15X, 'Precipitation-Runoff Modeling System (PRMS)', /, 23X, A)
    15  FORMAT (/, 8X, 'Process', 12X, 'Available Modules', /, 68('-'), /, &
            &        '  Basin Definition: basin', /, &
            &        '  Time Series Data: obs', /, &
            &        '   Potet Solar Rad: soltab', /, &
            &        '  Temperature Dist: climate_hru', /, &
            &        '       Precip Dist: climate_hru', /, &
            &        '    Solar Rad Dist: ddsolrad', /, &
            &        'Transpiration Dist: transp_tindex', /, &
            &        '    Output Summary: nhru_summary, basin_summary', /, 68('-'))
    16  FORMAT (//, 4X, 'Active modules listed in the order in which they are called', //, 8X, 'Process', 19X, &
            &        'Module', 16X, 'Version Date', /, A)

    call print_module(PRMS_versn, 'PRMS6 Computation Order     ', 90)
    call print_module(Version_read_control_file, 'Read Control File           ', 90)
    call print_module(Version_read_parameter_file, 'Read Parameter File         ', 90)

    9002 FORMAT (//, 74('='), /, 'Please give careful consideration to fixing all ERROR and WARNING messages', &
            /, 74('='), /)
end subroutine PRMS_header

!***********************************************************************
!     Code piece from main subroutine for initialize procedure
!***********************************************************************
subroutine PRMS_init()
    use PRMS_MODULE, only : Print_debug, PRMS_output_unit, Param_file, &
                            Model_output_file, Init_vars_from_file, &
                            Var_init_file, Save_vars_to_file, Var_save_file
    ! use UTILS_PRMS, only: numchars
    implicit none

    if (Print_debug > -1) then
        print 9004, 'Using Parameter File: ', Param_file
        print 9004, 'Writing PRMS Water Budget File: ', Model_output_file
    endif

    if (Print_debug > -2) then
        write (PRMS_output_unit, 9004) 'Using Parameter File: ', Param_file
        write (PRMS_output_unit, 9004) 'Writing PRMS Water Budget File: ', Model_output_file
    endif

    if (Init_vars_from_file == 1) then
        if (Print_debug > -1) print 9004, 'Using var_init_file: ', Var_init_file
    endif
    if (Save_vars_to_file == 1) then
        if (Print_debug > -1) print 9004, 'Using var_save_file: ', Var_save_file
    endif

    9004 FORMAT (/, 2A)
end subroutine PRMS_init


subroutine init_control(ctl_data)
    use kinds_mod, only: i4
    use control_ll_mod, only: control_list
    use PRMS_MODULE
    use time_mod, only: compute_julday
    use UTILS_PRMS, only: PRMS_open_output_file, PRMS_open_input_file, module_error  ! ,read_error
    use PRMS_CONTROL_FILE, only: read_control_file, init_control_defaults

    implicit none

    type(control_list), intent(inout) :: ctl_data

    ! Local Variables
    integer(i4) :: iret
    integer(i4) :: startday
    integer(i4) :: endday

    !***********************************************************************
    Inputerror_flag = 0

    ! Set general defaults for control parameters
    call init_control_defaults(ctl_data)

    ! Set default values for some control parameters
    ! These will be overridden if found in the control file
    ! key, value, datatype
    call ctl_data%set('print_debug', [0], 1)
    call ctl_data%set('parameter_check_flag', [1], 1)
    call ctl_data%set('cbh_check_flag', [1], 1)
    call ctl_data%set('cbh_binary_flag', [0], 1)
    call ctl_data%set('init_vars_from_file', [0], 1)
    call ctl_data%set('save_vars_to_file', [0], 1)
    call ctl_data%set('nhruOutON_OFF', [0], 1)
    call ctl_data%set('basinOutON_OFF', [0], 1)
    call ctl_data%set('prms_warmup', [0], 1)

    call read_control_file(ctl_data)

    ! call ctl_data%traverse(print_key)

    ! Set program variables based on control parameters
    call ctl_data%get_data('print_debug', Print_debug)
    call ctl_data%get_data('parameter_check_flag', Parameter_check_flag)
    call ctl_data%get_data('cbh_check_flag', Cbh_check_flag)
    call ctl_data%get_data('cbh_binary_flag', Cbh_binary_flag)
    call ctl_data%get_data('init_vars_from_file', Init_vars_from_file)
    call ctl_data%get_data('save_vars_to_file', Save_vars_to_file)
    call ctl_data%get_data('nhruOutON_OFF', NhruOutON_OFF)
    call ctl_data%get_data('basinOutON_OFF', BasinOutON_OFF)
    call ctl_data%get_data('prms_warmup', Prms_warmup)

    ! 2017-12-05 PAN: what is this used for? overriding control parameters?
    ! call get_control_arguments()

    ! debug print flag:
    ! -1=quiet - reduced screen output
    ! 0=none; 1=water balances; 2=basin;
    ! 4=basin_sum; 5=soltab; 7=soil zone;
    ! 9=snowcomp; 13=cascade; 14=subbasin tree

    call ctl_data%get_data('model_mode', Model_mode, missing_stop=.true.)

    if (Model_mode == 'PRMS' .OR. Model_mode== '    ' .OR. Model_mode == 'DAILY') then
        Model = 1
    elseif (Model_mode == 'WRITE_CLIMATE') then
        Model = 4
    elseif (Model_mode == 'DOCUMENTATION') then
        Model = 99
    else
        print '(/,2A)', 'ERROR, invalid model_mode value: ', Model_mode
        STOP
    endif

    ! get simulation start_time and end_time
    call ctl_data%get_data('start_time', Starttime, missing_stop=.true.)

    Start_year = Starttime(1)
    if (Start_year < 0) STOP 'ERROR, control parameter start_time must be specified'
    Start_month = Starttime(2)
    Start_day = Starttime(3)

    call ctl_data%get_data('end_time', Endtime, missing_stop=.true.)

    End_year = Endtime(1)
    if (End_year < 0) STOP 'ERROR, control parameter start_time must be specified'
    End_month = Endtime(2)
    End_day = Endtime(3)

    startday = compute_julday(Start_year, Start_month, Start_day)
    endday = compute_julday(End_year, End_month, End_day)
    Number_timesteps = endday - startday + 1

    ! Open PRMS module output file
    call ctl_data%get_data('model_output_file', Model_output_file, missing_stop=.true.)

    if (Print_debug > -2) then
        call PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
        if (iret /= 0) STOP
    endif

    call ctl_data%get_data('param_file', Param_file, missing_stop=.true.)

    ! Check for restart files
    if (Init_vars_from_file == 1) then
        call ctl_data%get_data('var_init_file', Var_init_file, missing_stop=.true.)
        call PRMS_open_input_file(Restart_inunit, Var_init_file, 'var_init_file', 1, iret)
        if (iret /= 0) STOP
    endif

    if (Save_vars_to_file == 1) then
        call ctl_data%get_data('var_save_file', Var_save_file, missing_stop=.true.)
    endif

    call ctl_data%get_data('temp_module', Temp_module, missing_stop=.true.)
    call ctl_data%get_data('precip_module', Precip_module, missing_stop=.true.)
    call ctl_data%get_data('transp_module', Transp_module, missing_stop=.true.)
    call ctl_data%get_data('et_module', Et_module, missing_stop=.true.)
    call ctl_data%get_data('solrad_module', Solrad_module, missing_stop=.true.)

    Climate_precip_flag = 0
    Climate_temp_flag = 0

    if (Precip_module == 'climate_hru') then
        Precip_flag = 7
        Climate_precip_flag = 1
    else
        print '(/,2A)', 'ERROR: invalid precip_module value: ', Precip_module
        Inputerror_flag = 1
    endif

    if (Temp_module == 'climate_hru') then
        Temp_flag = 7
        Climate_temp_flag = 1
    else
        print '(/,2A)', 'ERROR, invalid temp_module value: ', Temp_module
        Inputerror_flag = 1
    endif

    if (Transp_module /= 'transp_tindex') then
        print '(/,2A)', 'ERROR, invalid transp_module value: ', Transp_module
        Inputerror_flag = 1
    endif

    if (Et_module == 'potet_jh') then
        Et_flag = 1
    else
        print '(/,2A)', 'ERROR, invalid et_module value: ', Et_module
        Inputerror_flag = 1
    endif

    if (Solrad_module == 'ddsolrad') then
        Solrad_flag = 1
    else
        print '(/,2A)', 'ERROR, invalid solrad_module value: ', Solrad_module
        Inputerror_flag = 1
    endif

    if (NhruOutON_OFF == 1 .OR. BasinOutON_OFF == 1) then
        if (Start_year + Prms_warmup > End_year) then ! change to start full date ???
            print *, 'ERROR, prms_warmup > than simulation time period:', Prms_warmup
            Inputerror_flag = 1
        endif
    endif

    if (Inputerror_flag == 1) then
        print '(//,A,/,A)', '**FIX input errors in your Control File to continue**', &
                &        'NOTE: some errors may be due to use of default values'
        STOP
    endif

    contains
        subroutine print_key(key, value, datatype, done)  ! print the key for this node
            use iso_fortran_env, only: output_unit
            use arr_mod, only: array_1D_t
            implicit none

            character(len=*), intent(in) :: key
            type(array_1D_t), intent(in) :: value
            integer(i4), intent(in) :: datatype
            logical,intent(out) :: done

            print *, ''
            print *, '**** Control variable ****'
            print *, 'Name: ', key
            print *, 'num_elem:', value%length()
            print *, 'datatype:', datatype
            print *, '*values*'
            call value%print()

            done = .false.
        end subroutine print_key
end subroutine init_control

!***********************************************************************
!     declare the dimensions
!***********************************************************************
subroutine setdims(dim_data)
    use dimensions_mod, only: dimension_list
    implicit none

    type(dimension_list), intent(inout) :: dim_data

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Dimension section

    ! Set default values for some dimensions
    ! key, value, description, default, maximum
    call dim_data%set_dimension('nhru', 1, 'Number of HRUs', 1, 1)
    call dim_data%set_dimension('nrain', 0, 'Number of precipitation-measurement stations', 0, 0)
    call dim_data%set_dimension('ntemp', 0, 'Number of air-temperature-measurement stations', 0, 0)
    call dim_data%set_dimension('nobs', 0, 'Number of streamflow-measurement stations', 0, 0)

    ! Fixed dimensions
    call dim_data%set_dimension('ndays', 366, 'Maximum number of days in a year', 366, 366)
    call dim_data%set_dimension('nmonths', 12, 'Number of months in a year', 12, 12)
    call dim_data%set_dimension('one', 1, 'Number of values for scalar array', 1, 1)
    ! --------------------------------------------------------------------------
end subroutine setdims

!***********************************************************************
!     Get dimensions
!***********************************************************************
subroutine get_dims(dim_data)
    use PRMS_MODULE, only : Nhru, Ntemp, Nrain, Nobs, Model
    use dimensions_mod, only: dimension_list
    implicit none

    type(dimension_list), intent(in) :: dim_data

    !***********************************************************************
    call dim_data%get_data('nhru', Nhru, missing_stop = .true.)
    call dim_data%get_data('ntemp', Ntemp, missing_stop = .true.)
    call dim_data%get_data('nrain', Nrain, missing_stop = .true.)
    call dim_data%get_data('nobs', Nobs, missing_stop = .true.)

    if (Model == 99) then
        if (Ntemp == 0) Ntemp = 1
        if (Nrain == 0) Nrain = 1
        if (Nobs == 0) Nobs = 1
    endif
end subroutine get_dims

!**********************************************************************
!     Module documentation
!**********************************************************************
subroutine module_doc(dim_data, ctl_data, param_data, var_data)
    use kinds_mod, only: i4
    use PRMS_BASIN, only: basin, Hemisphere, Basin_area_inv
    use PRMS_OBS, only: obs
    use PRMS_POTET_JH, only: potet_jh
    use PRMS_CLIMATEVARS, only: climateflow
    use PRMS_CLIMATE_HRU, only: climate_hru
    use PRMS_SOLTAB, only: soltab
    use PRMS_DDSOLRAD, only: ddsolrad
    use PRMS_TRANSP_TINDEX, only: transp_tindex
    use PRMS_BASIN_SUMMARY, only: basin_summary
    use PRMS_NHRU_SUMMARY, only: nhru_summary
    use PRMS_SET_TIME, only: prms_time
    use dimensions_mod, only: dimension_list
    use control_ll_mod, only: control_list
    use parameter_arr_mod, only: parameter_arr_t
    use variables_arr_mod, only: variables_arr_t
    implicit none

    type(dimension_list), intent(in) :: dim_data
    type(control_list), intent(in) :: ctl_data
    type(parameter_arr_t), intent(inout) :: param_data
    type(variables_arr_t), intent(inout) :: var_data  ! should be intent(in)

    ! Local variable
    integer(i4) :: test

    !**********************************************************************
    test = basin(dim_data, param_data, var_data)
    test = climateflow(dim_data, param_data, var_data)
    test = soltab(dim_data, param_data, var_data)
    test = prms_time(Hemisphere, Basin_area_inv, var_data)
    test = obs(dim_data, param_data, var_data)
    test = climate_hru(dim_data, ctl_data, param_data)
    test = ddsolrad(dim_data, param_data)
    test = transp_tindex(dim_data, param_data)
    test = potet_jh(dim_data, param_data)
    call nhru_summary(dim_data, ctl_data, param_data, var_data)
    call basin_summary(ctl_data, var_data)

    print 9001
    9001 FORMAT (//, ' All available modules have been called.', /, &
            &        ' All parameters have been declared.', /, &
            &        ' Note, no simulation was computed.', /)

end subroutine module_doc


!***********************************************************************
!     call_modules_restart - write or read restart file
!***********************************************************************
subroutine call_modules_restart(In_out)
    use kinds_mod, only: i4
    use prms_constants, only: MAXCONTROL_LENGTH
    use PRMS_MODULE, only: MODNAME, Model_mode, Timestep, Nhru, Temp_flag, &
                           Restart_inunit, Restart_outunit
    use UTILS_PRMS, only: check_restart, check_restart_dimen
    implicit none

    ! Argument
    integer(i4), intent(in) :: In_out

    ! Functions
    INTRINSIC TRIM

    ! Local Variables
    integer(i4) :: nhru_test
    integer(i4) :: temp_test
    integer(i4) :: ierr
    character(LEN=MAXCONTROL_LENGTH) :: model_test
    character(LEN=5) :: module_name

    !***********************************************************************
    if (In_out == 0) then
        write (Restart_outunit) MODNAME
        write (Restart_outunit) Timestep, Nhru, Temp_flag, Model_mode
    else
        ierr = 0
        read (Restart_inunit) module_name
        call check_restart(MODNAME, module_name)
        read (Restart_inunit) Timestep, nhru_test, temp_test, model_test

        if (Model_mode /= TRIM(model_test)) then
            print *, 'ERROR, Initial Conditions File saved for model_mode=', TRIM(model_test)
            print *, '       Current model has model_mode=', Model_mode, ' they must be equal'
            ierr = 1
        endif

        call check_restart_dimen('nhru', nhru_test, Nhru, ierr)
        if (ierr == 1) STOP
    endif
end subroutine call_modules_restart

!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
subroutine computation_order(Arg, dim_data, ctl_data, param_data, var_data)
    use kinds_mod, only: i4
    use prms_constants, only: EQULS
    use UTILS_PRMS, only: module_error, PRMS_open_output_file  ! , read_error, numchars
    use PRMS_MODULE, only: Process, BasinOutON_OFF, Elapsed_time, Elapsed_time_start, &
                           Elapsed_time_end, Elapsed_time_minutes, End_day, End_month, End_year, &
                           Start_day, Start_month, Start_year, &
                           Execution_time_start, Execution_time_end, &
                           Init_vars_from_file, Inputerror_flag, Parameter_check_flag, &
                           Model, Print_debug, PRMS_output_unit, PRMS_versn, &
                           Restart_inunit, Restart_outunit, Save_vars_to_file,&
                           NhruOutON_OFF, Et_module, Solrad_module, Transp_module, &
                           Var_save_file
    use PRMS_BASIN, only: basin, Hemisphere, Basin_area_inv
    use PRMS_OBS, only: obs
    use PRMS_POTET_JH, only: potet_jh
    use PRMS_SET_TIME, only: prms_time
    use PRMS_DATA_FILE, only: read_prms_data_file
    use PRMS_READ_PARAM_FILE, only: read_parameter_file_dimens, read_parameter_file_params, check_parameters
    use PRMS_CLIMATEVARS, only: climateflow
    use PRMS_CLIMATE_HRU, only: climate_hru
    use PRMS_SOLTAB, only: soltab
    use PRMS_DDSOLRAD, only: ddsolrad
    use PRMS_TRANSP_TINDEX, only: transp_tindex
    use PRMS_BASIN_SUMMARY, only: basin_summary
    use PRMS_NHRU_SUMMARY, only: nhru_summary
    use dimensions_mod, only: dimension_list
    use control_ll_mod, only: control_list
    use parameter_arr_mod, only: parameter_arr_t
    use variables_arr_mod, only: variables_arr_t
    implicit none

    ! Arguments
    character(len=*), intent(in) :: Arg
    type(dimension_list), intent(inout) :: dim_data
    type(control_list), intent(inout) :: ctl_data
    type(parameter_arr_t), intent(inout) :: param_data
    type(variables_arr_t), intent(inout) :: var_data

    ! Functions
    INTRINSIC :: DATE_AND_TIME, INT

    ! Local Variables
    integer(i4) :: i
    integer(i4) :: iret
    ! integer(i4) :: nc
    integer(i4) :: call_modules
    character(:), allocatable :: data_filename

    !***********************************************************************
    call_modules = 1

    Process = Arg

    ! Process_flag (0=run, 1=declare, 2=init, 3=clean, 4=setdims)
    ! if (Process == 'run') then
        ! Process_flag = 0
    ! elseif (Process == 'declare') then
    if (Process == 'declare') then
        ! Process_flag = 1
        PRMS_versn = 'prms6.f90 2017-09-29 13:51:00Z'

        call get_dims(dim_data)
        call PRMS_header()

        call ctl_data%get_data('data_file', data_filename, missing_stop=.true.)
        call read_prms_data_file(data_filename)

        if (Init_vars_from_file == 1) call call_modules_restart(1)
    elseif (Process == 'init') then
        ! Process_flag = 2
        call PRMS_init()
    elseif (Process == 'setdims') then
        ! Process_flag = 4
        call DATE_AND_TIME(VALUES=Elapsed_time_start)
        Execution_time_start = Elapsed_time_start(5) * 3600 + Elapsed_time_start(6) * 60 + &
                &              Elapsed_time_start(7) + Elapsed_time_start(8) * 0.001

        print *, '    ---- init_control()'
        call init_control(ctl_data)

        print *, '    ---- setdims()'
        call setdims(dim_data)

        ! print *, '    ---- setup_params()'
        ! call setup_params()

        print *, '    ---- read_parameter_file_dimens()'
        call read_parameter_file_dimens(dim_data)
    else !if ( Process(:5)=='clean' ) then
        ! Process_flag = 3
        if (Init_vars_from_file == 1) CLOSE (Restart_inunit)

        if (Save_vars_to_file == 1) then
            ! nc = numchars(Var_save_file)
            call PRMS_open_output_file(Restart_outunit, Var_save_file, 'var_save_file', 1, iret)
            if (iret /= 0) STOP
            call call_modules_restart(0)
        endif
    endif

    if (Model == 99) then
        if (Process == 'setdims' .or. Process == 'declare' .or. Process == 'run') then
        ! if (Process_flag == 4 .OR. Process_flag < 2) then
            Init_vars_from_file = 0 ! make sure this is set so all variables and parameters are declared

            call module_doc(dim_data, ctl_data, param_data, var_data)
            call_modules = 0
            RETURN
        else
            STOP
        endif
    endif

    ! All modules must be called for setdims, declare, initialize, and cleanup
    if (Process /= 'run') then
    ! if (Process_flag /= 0) then
        ! All stages but the run stage
        call_modules = basin(dim_data, param_data, var_data)
        if (call_modules /= 0) call module_error('basin', Arg, call_modules)

        call_modules = climateflow(dim_data, param_data, var_data)
        if (call_modules /= 0) call module_error('climateflow', Arg, call_modules)

        call_modules = soltab(dim_data, param_data, var_data)
        if (call_modules /= 0) call module_error('soltab', Arg, call_modules)

        call_modules = obs(dim_data, param_data, var_data) ! functionality of readvar is in read_data_file, check_data_variables routine
        if (call_modules /= 0) call module_error('obs', Arg, call_modules)
    endif

    ! Run model for current timestep - execute for all stages
    call_modules = prms_time(Hemisphere, Basin_area_inv, var_data)
    if (call_modules /= 0) call module_error('prms_time', Arg, call_modules)

    call_modules = climate_hru(dim_data, ctl_data, param_data)
    if (call_modules /= 0) call module_error('climate_hru', Arg, call_modules)

    call_modules = ddsolrad(dim_data, param_data)
    if (call_modules /= 0) call module_error(Solrad_module, Arg, call_modules)

    call_modules = transp_tindex(dim_data, param_data)
    if (call_modules /= 0) call module_error(Transp_module, Arg, call_modules)

    call_modules = potet_jh(dim_data, param_data)
    if (call_modules /= 0) call module_error(Et_module, Arg, call_modules)

    if (NhruOutON_OFF > 0) call nhru_summary(dim_data, ctl_data, param_data, var_data)

    if (BasinOutON_OFF == 1) call basin_summary(ctl_data, var_data)

    if (Process == 'run') return
    ! if (Process_flag == 0) RETURN   ! For run stage only


    if (Print_debug > -1) then
        if (Process == 'clean') then
        ! if (Process_flag == 3) then
            ! For clean stage only
            call DATE_AND_TIME(VALUES = Elapsed_time_end)

            print 9001
            print 9003, 'start', (Elapsed_time_start(i), i = 1, 3), (Elapsed_time_start(i), i = 5, 7)
            print 9003, 'end', (Elapsed_time_end(i), i = 1, 3), (Elapsed_time_end(i), i = 5, 7)

            Execution_time_end = Elapsed_time_end(5) * 3600 + &
                                 Elapsed_time_end(6) * 60 + Elapsed_time_end(7) + &
                                 Elapsed_time_end(8) * 0.001
            Elapsed_time = Execution_time_end - Execution_time_start
            Elapsed_time_minutes = INT(Elapsed_time / 60.0)

            print '(A,I5,A,F6.2,A,/)', 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
                                       Elapsed_time - Elapsed_time_minutes * 60.0, ' seconds'
        elseif (Process == 'init') then
        ! elseif (Process_flag == 2) then
            ! For init stage only
            if (Inputerror_flag == 1) then
                print '(//,A,//,A,/,A,/,A)', '**Fix input errors in your Parameter File to continue**', &
                                             '  Set control parameter parameter_check_flag to 0 after', &
                                             '  all parameter values are valid.'
                print '(/,A,/,A,/,A,/,A,/,A,/)', 'If input errors are related to paramters used for automated', &
                                                 'calibration processes, with CAUTION, set control parameter', &
                                                 'parameter_check_flag to 0. After calibration set the', &
                                                 'parameter_check_flag to 1 to verify that those calibration', &
                                                 'parameters have valid and compatible values.'
                STOP
            endif
        endif
    endif

    if (Process == 'declare') then
        ! For declare stage only
        call read_parameter_file_params(dim_data, ctl_data, param_data)
        if (Print_debug > -2) then
            print '(A)', EQULS
            write (PRMS_output_unit, '(A)') EQULS
        endif
    elseif (Process == 'init') then
        ! For init stage only
        if (Parameter_check_flag == 2) STOP
        if (Print_debug > -1) call check_parameters(param_data)
        print 4, 'Simulation time period:', Start_year, Start_month, Start_day, ' -', End_year, &
                End_month, End_day, EQULS
    elseif (Process == 'clean') then
        ! For clean stage only
        if (Print_debug > -2) then
            write (PRMS_output_unit, '(A,I5,A,F6.2,A,/)') 'Execution elapsed time', &
                    Elapsed_time_minutes, ' minutes', Elapsed_time - Elapsed_time_minutes * 60.0, ' seconds'
        endif
    endif

    4 FORMAT (/, 2(A, I5, 2('/', I2.2)), //, A, /)
    9001 FORMAT (/, 26X, 27('='), /, 26X, 'Normal completion of PRMS', /, 26X, 27('='), /)
    9003 FORMAT ('Execution ', A, ' date and time (yyyy/mm/dd hh:mm:ss)', I5, 2('/', I2.2), I3, 2(':', I2.2), /)
end subroutine computation_order


!***********************************************************************
! Main program
!***********************************************************************
program prms6
    use kinds_mod, only: i4
    use PRMS_MODULE, only : Number_timesteps
    use dimensions_mod
    use parameter_arr_mod, only: parameter_arr_t
    use variables_arr_mod, only: variables_arr_t
    use control_ll_mod, only: control_list
    ! use PRMS_MMFAPI, only: PRMS_parameter
    implicit none

    type(control_list) :: Control_data
    type(dimension_list) :: Dimension_data
    type(parameter_arr_t) :: Param_data
    type(variables_arr_t) :: Var_data

    integer(i4) :: ii

    !***********************************************************************
    Var_data = variables_arr_t()
    Param_data = parameter_arr_t()
    Dimension_data = dimension_list()
    Control_data = control_list()

    print *, '---- setdims'
    call computation_order('setdims', Dimension_data, Control_data, Param_data, Var_data)

    print *, '---- declare'
    call computation_order('declare', Dimension_data, Control_data, Param_data, Var_data)

    print *, '---- init'
    call computation_order('init', Dimension_data, Control_data, Param_data, Var_data)

    print *, '---- run'
    do ii = 1, Number_timesteps
        call computation_order('run', Dimension_data, Control_data, Param_data, Var_data)
    enddo

    print *, '---- clean'
    call computation_order('clean', Dimension_data, Control_data, Param_data, Var_data)
end program prms6
