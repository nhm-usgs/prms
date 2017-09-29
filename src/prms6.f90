!***********************************************************************
! Defines globals
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAXFILE_LENGTH = 256, MAXCONTROL_LENGTH = 32
      INTEGER, PARAMETER :: MAXDIM = 500
      CHARACTER(LEN=68), PARAMETER :: &
     &  EQULS = '===================================================================='
      CHARACTER(LEN=5), PARAMETER :: MODNAME = 'prms6'
      CHARACTER(LEN=24), PARAMETER :: PRMS_VERSION = 'Version 6.0.0 09/29/2017'
      CHARACTER(LEN=8), SAVE :: Process
      CHARACTER(LEN=80), SAVE :: PRMS_versn
      INTEGER, SAVE :: Model, Process_flag, Number_timesteps
      INTEGER, SAVE :: Nhru, Ntemp, Nrain, Nobs
      INTEGER, SAVE :: Starttime(6), Endtime(6)
      INTEGER, SAVE :: Start_year, Start_month, Start_day, End_year, End_month, End_day
      INTEGER, SAVE :: Transp_flag, Solrad_flag, Et_flag, Temp_flag, Precip_flag
      INTEGER, SAVE :: Climate_temp_flag, Climate_precip_flag
      INTEGER, SAVE :: Inputerror_flag, Timestep, Prms_warmup
      INTEGER, SAVE :: PRMS_output_unit, Restart_inunit, Restart_outunit
      INTEGER, SAVE :: Elapsed_time_start(8), Elapsed_time_end(8), Elapsed_time_minutes
      CHARACTER(LEN=80), SAVE :: Version_read_control_file, Version_read_parameter_file
      REAL, SAVE :: Execution_time_start, Execution_time_end, Elapsed_time
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru
! Control parameters
      INTEGER, SAVE :: Print_debug, Parameter_check_flag, Cbh_check_flag, Cbh_binary_flag
      INTEGER, SAVE :: Init_vars_from_file, Save_vars_to_file
      INTEGER, SAVE :: NhruOutON_OFF, BasinOutON_OFF
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Model_output_file, Var_init_file, Var_save_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Param_file, Model_control_file
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Temp_module, Et_module, Transp_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Model_mode, Precip_module, Solrad_module
      END MODULE PRMS_MODULE

!***********************************************************************
! Main program
!***********************************************************************
      PROGRAM prms6
        USE PRMS_MODULE, ONLY: Number_timesteps
        IMPLICIT NONE
        EXTERNAL :: computation_order
        INTEGER :: i
!***********************************************************************
        CALL computation_order('setdims')
        CALL computation_order('declare')
        CALL computation_order('init')
        DO i = 1, Number_timesteps
          CALL computation_order('run')
        ENDDO
        CALL computation_order('clean')
      END PROGRAM prms6

!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
      SUBROUTINE computation_order(Arg)
      USE PRMS_MODULE
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTRINSIC :: DATE_AND_TIME, INT
      INTEGER, EXTERNAL :: basin, climateflow, prms_time
      INTEGER, EXTERNAL :: obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: climate_hru, ddsolrad, potet_jh, numchars
      EXTERNAL :: module_error, PRMS_open_output_file
      EXTERNAL :: call_modules_restart, basin_summary
      EXTERNAL :: nhru_summary, module_doc, read_error
      EXTERNAL :: get_dims, setdims, read_prms_data_file
      EXTERNAL :: check_parameters, read_parameter_file_dimens, PRMS_header, PRMS_init
! Local Variables
      INTEGER :: i, iret, nc, call_modules
!***********************************************************************
      call_modules = 1

      Process = Arg

      ! Process_flag (0=run, 1=declare, 2=init, 3=clean, 4=setdims)
      IF ( Process(:3)=='run' ) THEN
        Process_flag = 0

      ELSEIF ( Process(:4)=='decl' ) THEN
        Process_flag = 1
        PRMS_versn = 'prms6.f90 2017-09-29 13:51:00Z'
        CALL get_dims()
        CALL PRMS_header()
        CALL read_prms_data_file()
        IF ( Init_vars_from_file==1 ) CALL call_modules_restart(1)
      ELSEIF ( Process(:4)=='init' ) THEN
        Process_flag = 2
        CALL PRMS_init()

      ELSEIF ( Process(:7)=='setdims' ) THEN
        Process_flag = 4
        CALL DATE_AND_TIME(VALUES=Elapsed_time_start)
        Execution_time_start = Elapsed_time_start(5)*3600 + Elapsed_time_start(6)*60 + &
     &                         Elapsed_time_start(7) + Elapsed_time_start(8)*0.001
        CALL setdims()
        CALL setup_params()
        CALL read_parameter_file_dimens()

      ELSE !IF ( Process(:5)=='clean' ) THEN
        Process_flag = 3
        IF ( Init_vars_from_file==1 ) CLOSE ( Restart_inunit )
        IF ( Save_vars_to_file==1 ) THEN
          nc = numchars(Var_save_file)
          CALL PRMS_open_output_file(Restart_outunit, Var_save_file(:nc), 'var_save_file', 1, iret)
          IF ( iret/=0 ) STOP
          CALL call_modules_restart(0)
        ENDIF
      ENDIF

      IF ( Model==99 ) THEN
        IF ( Process_flag==4 .OR. Process_flag<2 ) THEN
          Init_vars_from_file = 0 ! make sure this is set so all variables and parameters are declared
          CALL module_doc()
          call_modules = 0
          RETURN
        ELSE
          STOP
        ENDIF
      ENDIF

! All modules must be called for setdims, declare, initialize, and cleanup
      IF ( Process_flag/=0 ) THEN
        call_modules = basin()
        IF ( call_modules/=0 ) CALL module_error('basin', Arg, call_modules)

        call_modules = climateflow()
        IF ( call_modules/=0 ) CALL module_error('climateflow', Arg, call_modules)

        call_modules = soltab()
        IF ( call_modules/=0 ) CALL module_error('soltab', Arg, call_modules)

        call_modules = obs() ! functionality of readvar is in read_data_file, check_data_variables routine
        IF ( call_modules/=0 ) CALL module_error('obs', Arg, call_modules)
      ENDIF

      call_modules = prms_time()
      IF ( call_modules/=0 ) CALL module_error('prms_time', Arg, call_modules)

      call_modules = climate_hru()
      IF ( call_modules/=0 ) CALL module_error('climate_hru', Arg, call_modules)

      call_modules = ddsolrad()
      IF ( call_modules/=0 ) CALL module_error(Solrad_module, Arg, call_modules)

      call_modules = transp_tindex()
      IF ( call_modules/=0 ) CALL module_error(Transp_module, Arg, call_modules)

      call_modules = potet_jh()
      IF ( call_modules/=0 ) CALL module_error(Et_module, Arg, call_modules)

      IF ( NhruOutON_OFF>0 ) CALL nhru_summary()

      IF ( BasinOutON_OFF==1 ) CALL basin_summary()

      IF ( Process_flag==0 ) RETURN

      IF ( Print_debug>-1 ) THEN
        IF ( Process_flag==3 ) THEN
          CALL DATE_AND_TIME(VALUES=Elapsed_time_end)
          PRINT 9001
          PRINT 9003, 'start', (Elapsed_time_start(i),i=1,3), (Elapsed_time_start(i),i=5,7)
          PRINT 9003, 'end', (Elapsed_time_end(i),i=1,3), (Elapsed_time_end(i),i=5,7)
          Execution_time_end = Elapsed_time_end(5)*3600 + Elapsed_time_end(6)*60 + &
     &                         Elapsed_time_end(7) + Elapsed_time_end(8)*0.001
          Elapsed_time = Execution_time_end - Execution_time_start
          Elapsed_time_minutes = INT(Elapsed_time/60.0)
          PRINT '(A,I5,A,F6.2,A,/)', 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                               Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
        ELSEIF ( Process_flag==2 ) THEN
          IF ( Inputerror_flag==1 ) THEN
            PRINT '(//,A,//,A,/,A,/,A)', '**Fix input errors in your Parameter File to continue**', &
     &            '  Set control parameter parameter_check_flag to 0 after', &
     &            '  all parameter values are valid.'
            PRINT '(/,A,/,A,/,A,/,A,/,A,/)', &
     &            'If input errors are related to paramters used for automated', &
     &            'calibration processes, with CAUTION, set control parameter', &
     &            'parameter_check_flag to 0. After calibration set the', &
     &            'parameter_check_flag to 1 to verify that those calibration', &
     &            'parameters have valid and compatible values.'
            STOP
          ENDIF
        ENDIF
      ENDIF
      IF ( Process_flag==1 ) THEN
        CALL read_parameter_file_params()
        IF ( Print_debug>-2 ) THEN
          PRINT '(A)', EQULS
          WRITE ( PRMS_output_unit, '(A)' ) EQULS
        ENDIF
      ELSEIF ( Process_flag==2 ) THEN
        IF ( Parameter_check_flag==2 ) STOP
        IF ( Print_debug>-1 ) CALL check_parameters()
        PRINT 4, 'Simulation time period:', Start_year, Start_month, Start_day, ' -', End_year, End_month, End_day, EQULS
      ELSEIF ( Process_flag==3 ) THEN
        IF ( Print_debug>-2 ) &
     &    WRITE ( PRMS_output_unit,'(A,I5,A,F6.2,A,/)') 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                                                  Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
      ENDIF

    4 FORMAT (/, 2(A, I5, 2('/',I2.2)), //, A, /)
 9001 FORMAT (/, 26X, 27('='), /, 26X, 'Normal completion of PRMS', /, 26X, 27('='), /)
 9003 FORMAT ('Execution ', A, ' date and time (yyyy/mm/dd hh:mm:ss)', I5, 2('/',I2.2), I3, 2(':',I2.2), /)

      END SUBROUTINE computation_order

!***********************************************************************
!     declare the dimensions
!***********************************************************************
      SUBROUTINE setdims()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix, control_integer_array
      INTEGER, EXTERNAL :: control_string, control_integer, compute_julday
      EXTERNAL :: read_error, PRMS_open_output_file, PRMS_open_input_file
      EXTERNAL :: read_control_file, setup_dimens, read_parameter_file_dimens, get_control_arguments, module_error
! Local Variables
      ! Maximum values are no longer limits
! Local Variables
      INTEGER :: iret, j,startday, endday
!***********************************************************************
      Inputerror_flag = 0

      CALL read_control_file()
      CALL get_control_arguments()

      ! debug print flag:
      ! -1=quiet - reduced screen output
      ! 0=none; 1=water balances; 2=basin;
      ! 4=basin_sum; 5=soltab; 7=soil zone;
      ! 9=snowcomp; 13=cascade; 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug')/=0 ) Print_debug = 0

      IF ( control_integer(Parameter_check_flag, 'parameter_check_flag')/=0 ) Parameter_check_flag = 1
      IF ( control_integer(Cbh_check_flag, 'cbh_check_flag')/=0 ) Cbh_check_flag = 1
      IF ( control_integer(Cbh_binary_flag, 'cbh_binary_flag')/=0 ) Cbh_binary_flag = 0

      IF ( control_string(Model_mode, 'model_mode')/=0 ) CALL read_error(5, 'model_mode')
      IF ( Model_mode(:4)=='PRMS' .OR. Model_mode(:4)=='    ' .OR. Model_mode(:5)=='DAILY' ) THEN
        Model = 1
      ELSEIF ( Model_mode(:13)=='WRITE_CLIMATE' ) THEN
        Model = 4
      ELSEIF ( Model_mode(:13)=='DOCUMENTATION' ) THEN
        Model = 99
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid model_mode value: ', Model_mode
        STOP
      ENDIF

      ! get simulation start_time and end_time
      Starttime = -1
      DO j = 1, 6
        IF ( control_integer_array(Starttime(j), j, 'start_time')/=0 ) THEN
          PRINT *, 'ERROR, start_time, index:', j, 'value: ', Starttime(j)
          STOP
        ENDIF
      ENDDO
      Start_year = Starttime(1)
      IF ( Start_year<0 ) STOP 'ERROR, control parameter start_time must be specified'
      Start_month = Starttime(2)
      Start_day = Starttime(3)
      Endtime = -1
      DO j = 1, 6
        IF ( control_integer_array(Endtime(j), j, 'end_time')/=0 ) THEN
          PRINT *, 'ERROR, end_time, index:', j, 'value: ', Endtime(j)
          STOP
        ENDIF
      ENDDO
      End_year = Endtime(1)
      IF ( End_year<0 ) STOP 'ERROR, control parameter start_time must be specified'
      End_month = Endtime(2)
      End_day = Endtime(3)

      IF ( control_integer(Init_vars_from_file, 'init_vars_from_file')/=0 ) Init_vars_from_file = 0
      IF ( control_integer(Save_vars_to_file, 'save_vars_to_file')/=0 ) Save_vars_to_file = 0

      startday = compute_julday(Start_year, Start_month, Start_day)
      endday = compute_julday(End_year, End_month, End_day)
      Number_timesteps = endday - startday + 1

      CALL setup_dimens()

      ! Open PRMS module output file
      IF ( control_string(Model_output_file, 'model_output_file')/=0 ) CALL read_error(5, 'model_output_file')
      IF ( Print_debug>-2 ) THEN
        CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
        IF ( iret/=0 ) STOP
      ENDIF
      IF ( control_string(Param_file, 'param_file')/=0 ) CALL read_error(5, 'param_file')

      ! Check for restart files
      IF ( Init_vars_from_file==1 ) THEN
        IF ( control_string(Var_init_file, 'var_init_file')/=0 ) CALL read_error(5, 'var_init_file')
        CALL PRMS_open_input_file(Restart_inunit, Var_init_file, 'var_init_file', 1, iret)
        IF ( iret/=0 ) STOP
      ENDIF
      IF ( Save_vars_to_file==1 ) THEN
        IF ( control_string(Var_save_file, 'var_save_file')/=0 ) CALL read_error(5, 'var_save_file')
      ENDIF

      Temp_module = ' '
      IF ( control_string(Temp_module, 'temp_module')/=0 ) CALL read_error(5, 'temp_module')
      Precip_module = ' '
      IF ( control_string(Precip_module, 'precip_module')/=0 ) CALL read_error(5, 'precip_module')
      Transp_module = ' '
      IF ( control_string(Transp_module, 'transp_module')/=0 ) CALL read_error(5, 'transp_module')
      Et_module = ' '
      IF ( control_string(Et_module, 'et_module')/=0 ) CALL read_error(5, 'et_module')
      Solrad_module = ' '
      IF ( control_string(Solrad_module, 'solrad_module')/=0 ) CALL read_error(5, 'solrad_module')

      Climate_precip_flag = 0
      Climate_temp_flag = 0

      IF ( Precip_module(:11)=='climate_hru' ) THEN
        Precip_flag = 7
        Climate_precip_flag = 1
      ELSE
        PRINT '(/,2A)', 'ERROR: invalid precip_module value: ', Precip_module
        Inputerror_flag = 1
      ENDIF

      IF ( Temp_module(:11)=='climate_hru' ) THEN
        Temp_flag = 7
        Climate_temp_flag = 1
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid temp_module value: ', Temp_module
        Inputerror_flag = 1
      ENDIF

      IF ( Transp_module(:13)/='transp_tindex' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid transp_module value: ', Transp_module
        Inputerror_flag = 1
      ENDIF

      IF ( Et_module(:8)=='potet_jh' ) THEN
        Et_flag = 1
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid et_module value: ', Et_module
        Inputerror_flag = 1
      ENDIF

      IF ( Solrad_module(:8)=='ddsolrad' ) THEN
        Solrad_flag = 1
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid solrad_module value: ', Solrad_module
        Inputerror_flag = 1
      ENDIF

! nhru_summary
      IF ( control_integer(NhruOutON_OFF, 'nhruOutON_OFF')/=0 ) NhruOutON_OFF = 0

! basin_summary
      IF ( control_integer(BasinOutON_OFF, 'basinOutON_OFF')/=0 ) BasinOutON_OFF = 0

      IF ( control_integer(Prms_warmup, 'prms_warmup')/=0 ) Prms_warmup = 0
      IF ( NhruOutON_OFF==1 .OR. BasinOutON_OFF==1 ) THEN
        IF ( Start_year+Prms_warmup>End_year ) THEN ! change to start full date ???
          PRINT *, 'ERROR, prms_warmup > than simulation time period:', Prms_warmup
          Inputerror_flag = 1
        ENDIF
      ENDIF

! spatial units
      IF ( decldim('nhru', 1, MAXDIM, 'Number of HRUs')/=0 ) CALL read_error(7, 'nhru')

! Time-series data stations, need to know if in Data File
      IF ( decldim('nrain', 0, MAXDIM, 'Number of precipitation-measurement stations')/=0 ) CALL read_error(7, 'nrain')
      IF ( decldim('ntemp', 0, MAXDIM, 'Number of air-temperature-measurement stations')/=0 ) CALL read_error(7, 'ntemp')
      IF ( decldim('nobs', 0, MAXDIM, 'Number of streamflow-measurement stations')/=0 ) CALL read_error(7, 'nobs')

! fixed dimensions
      IF ( declfix('ndays', 366, 366, 'Maximum number of days in a year ')/=0 ) CALL read_error(7, 'ndays')
      IF ( declfix('nmonths', 12, 12, 'Number of months in a year')/=0 ) CALL read_error(7, 'nmonths')
      IF ( declfix('one', 1, 1, 'Number of values for scaler array')/=0 ) CALL read_error(7, 'one')

      IF ( Inputerror_flag==1 ) THEN
        PRINT '(//,A,/,A)', '**FIX input errors in your Control File to continue**', &
     &        'NOTE: some errors may be due to use of defalut values'
        STOP
      ENDIF

      END SUBROUTINE setdims

!***********************************************************************
!     Get dimensions
!***********************************************************************
      SUBROUTINE get_dims()
      USE PRMS_MODULE, ONLY: Nhru, Ntemp, Nrain, Nobs, Model
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getdim
      EXTERNAL :: read_error
!***********************************************************************

      Nhru = getdim('nhru')
      IF ( Nhru==-1 ) CALL read_error(7, 'nhru')

      Ntemp = getdim('ntemp')
      IF ( Ntemp==-1 ) CALL read_error(6, 'ntemp')

      Nrain = getdim('nrain')
      IF ( Nrain==-1 ) CALL read_error(6, 'nrain')

      Nobs = getdim('nobs')
      IF ( Nobs==-1 ) CALL read_error(6, 'nobs')

      IF ( Model==99 ) THEN
        IF ( Ntemp==0 ) Ntemp = 1
        IF ( Nrain==0 ) Nrain = 1
        IF ( Nobs==0 ) Nobs = 1
      ENDIF

      END SUBROUTINE get_dims

!**********************************************************************
!     Module documentation
!**********************************************************************
      SUBROUTINE module_doc()
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basin, climateflow, prms_time
      INTEGER, EXTERNAL :: obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: climate_hru, ddsolrad, potet_jh
      EXTERNAL :: nhru_summary, basin_summary
! Local variable
      INTEGER :: test
!**********************************************************************
      test = basin()
      test = climateflow()
      test = soltab()
      test = prms_time()
      test = obs()
      test = climate_hru()
      test = ddsolrad()
      test = transp_tindex()
      test = potet_jh()
      CALL nhru_summary()
      CALL basin_summary()

      PRINT 9001
 9001 FORMAT (//, ' All available modules have been called.', /, &
     &        ' All parameters have been declared.', /, &
     &        ' Note, no simulation was computed.', /)

      END SUBROUTINE module_doc

!***********************************************************************
!     Code piece from main subroutine for declare procedure
!***********************************************************************
      SUBROUTINE PRMS_header()
      USE PRMS_MODULE, ONLY: Print_debug, PRMS_output_unit, PRMS_VERSION, EQULS, PRMS_versn, &
     &    Version_read_control_file, Version_read_parameter_file
      IMPLICIT NONE
! Functions
      EXTERNAL :: print_module
!***********************************************************************
      IF ( Print_debug>-2 ) THEN
        PRINT 10, PRMS_VERSION
        WRITE ( PRMS_output_unit, 10 ) PRMS_VERSION
        PRINT 15
        PRINT 9002
        WRITE ( PRMS_output_unit, 15 )
        PRINT 16, EQULS
        WRITE ( PRMS_output_unit, 16 ) EQULS
      ENDIF
  10  FORMAT (/, 15X, 'Precipitation-Runoff Modeling System (PRMS)', /, 23X, A)
  15  FORMAT (/, 8X, 'Process',  12X, 'Available Modules', /, 68('-'), /, &
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

      CALL print_module(PRMS_versn, 'PRMS6 Computation Order     ', 90)
      CALL print_module(Version_read_control_file, 'Read Control File           ', 90)
      CALL print_module(Version_read_parameter_file, 'Read Parameter File         ', 90)

 9002 FORMAT (//, 74('='), /, 'Please give careful consideration to fixing all ERROR and WARNING messages', /, 74('='), /)
      END SUBROUTINE PRMS_header

!***********************************************************************
!     Code piece from main subroutine for initialize procedure
!***********************************************************************
      SUBROUTINE PRMS_init()
      USE PRMS_MODULE, ONLY: Print_debug, PRMS_output_unit, Param_file, &
     &    Model_output_file, Init_vars_from_file, Var_init_file, Save_vars_to_file, Var_save_file
      IMPLICIT NONE
! Arguments
! Functions
      INTEGER, EXTERNAL :: numchars
! Local Variables
      INTEGER :: nc, nc2, nc3
!***********************************************************************
      nc2 = numchars(Param_file)
      nc3 = numchars(Model_output_file)
      IF ( Print_debug>-1 ) THEN
        PRINT 9004, 'Using Parameter File: ', Param_file(:nc2)
        PRINT 9004, 'Writing PRMS Water Budget File: ', Model_output_file(:nc3)
      ENDIF

      IF ( Print_debug>-2 ) THEN
        WRITE ( PRMS_output_unit, 9004 ) 'Using Parameter File: ', Param_file(:nc2)
        WRITE ( PRMS_output_unit, 9004 ) 'Writing PRMS Water Budget File: ', Model_output_file(:nc3)
      ENDIF

      IF ( Init_vars_from_file==1 ) THEN
        nc = numchars(Var_init_file)
        IF ( Print_debug>-1 ) PRINT 9004, 'Using var_init_file: ', Var_init_file(:nc)
      ENDIF
      IF ( Save_vars_to_file==1 ) THEN
        nc = numchars(Var_save_file)
        IF ( Print_debug>-1 ) PRINT 9004, 'Using var_save_file: ', Var_save_file(:nc)
      ENDIF

 9004 FORMAT (/, 2A)
      END SUBROUTINE PRMS_init

!***********************************************************************
!     call_modules_restart - write or read restart file
!***********************************************************************
      SUBROUTINE call_modules_restart(In_out)
      USE PRMS_MODULE
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart, check_restart_dimen
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: nhru_test, temp_test, ierr
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: model_test
      CHARACTER(LEN=5) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Timestep, Nhru, Temp_flag, Model_mode
      ELSE
        ierr = 0
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Timestep, nhru_test, temp_test, model_test
        IF ( TRIM(Model_mode)/=TRIM(model_test) ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model_mode=', model_test
          PRINT *, '       Current model has model_mode=', Model_mode, ' they must be equal'
          ierr = 1
        ENDIF
        CALL check_restart_dimen('nhru', nhru_test, Nhru, ierr)
        IF ( ierr==1 ) STOP
      ENDIF
      END SUBROUTINE call_modules_restart
