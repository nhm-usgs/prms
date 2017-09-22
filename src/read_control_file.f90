!***********************************************************************
! Read Control File
!***********************************************************************
      MODULE PRMS_CONTROL_FILE

        USE PRMS_MODULE, ONLY: Print_debug, EQULS, MAXCONTROL_LENGTH, MAXFILE_LENGTH, &
     &      Init_vars_from_file, Save_vars_to_file, Parameter_check_flag, Param_file, Model_output_file, &
     &      Precip_module, Temp_module, Et_module, Solrad_module, Transp_module, Print_debug, &
     &      Model_mode, Endtime, Starttime, Prms_warmup, NhruOutON_OFF, BasinOutON_OFF, Cbh_check_flag, Cbh_binary_flag
        USE PRMS_CLIMATE_HRU, ONLY: Precip_day, Tmax_day, Tmin_day
        USE PRMS_NHRU_SUMMARY, ONLY: NhruOutVars, NhruOut_freq, NhruOutBaseFileName, NhruOutVar_names
        USE PRMS_BASIN_SUMMARY, ONLY: BasinOutVars, BasinOut_freq, BasinOutBaseFileName, BasinOutVar_names

        INTEGER, PARAMETER :: Max_num_control_parameters = 150 ! WARNING, hard coded, DANGER, DANGER
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Data_file, Var_init_file, Stat_var_file, Var_save_file
        CHARACTER(LEN=MAXFILE_LENGTH) :: Control_file, Control_description
        INTEGER, SAVE :: Num_control_parameters
        INTEGER, SAVE :: Param_file_control_parameter_id
        CHARACTER(LEN=MAXCONTROL_LENGTH), ALLOCATABLE, SAVE :: param_file_names(:)
        ! read_flag: 0 = not set, 1 = set from control file, 2 = set to default, 3 = means variably dimension, 4 = variably dimension set from Control File
        TYPE PRMS_control_parameter
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: name
             INTEGER :: numvals, read_flag, data_type, index
             INTEGER, ALLOCATABLE :: values_int(:)
             REAL, ALLOCATABLE :: values_real(:)
             CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: values_character(:)
        END TYPE PRMS_control_parameter
        TYPE ( PRMS_control_parameter ), SAVE, ALLOCATABLE :: Control_parameter_data(:)
      END MODULE PRMS_CONTROL_FILE

      SUBROUTINE read_control_file()
      USE PRMS_CONTROL_FILE
      USE PRMS_MODULE, ONLY: Version_read_control_file, Print_debug, Model_output_file, Model_control_file
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      INTEGER, EXTERNAL :: numchars
      EXTERNAL read_error, set_control_parameter, PRMS_open_input_file, write_outfile, PRMS_open_output_file !, print_module
      ! Local Variables
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: paramname
      CHARACTER(LEN=4) :: string
      INTEGER nchars, ios, numvalues, param_type, control_unit, j
      INTEGER, ALLOCATABLE :: int_parameter_values(:)
      CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: parameter_values(:)
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: paramstring
      REAL, ALLOCATABLE :: real_parameter_values(:)
!***********************************************************************
      Version_read_control_file = 'read_control_file.f90 2017-07-10 11:30:00Z'

      ! control filename cannot include blanks
      CALL get_control_filename(Model_control_file, nchars)
      CALL PRMS_open_input_file(control_unit, Model_control_file, 'model_control_file', 0, ios)
      IF ( ios/=0 ) CALL read_error(10, TRIM(Model_control_file))
      ! read header
      READ (control_unit, '(A)', IOSTAT=ios ) Control_description
      IF ( ios/=0 ) CALL read_error(12, TRIM(Model_control_file))

      CALL setup_cont() ! set default control parameter values

      ! Read all Control Parameters
      DO
        READ ( control_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) EXIT ! found end of Control File
        IF ( ios/=0 ) CALL read_error(12, 'missing #### delimiter')
        IF ( string(:4)/='####' ) CYCLE ! skip until delimiter found, such as blank of // comment lines
        READ ( control_unit, '(A)', IOSTAT=ios ) paramname ! parameter name
        IF ( ios/=0 ) CALL read_error(5, 'missing parameter name')
        READ ( control_unit, *, IOSTAT=ios ) numvalues
        IF ( ios/=0 ) CALL read_error(5, 'invalid number of values: '//TRIM(paramname) )
        READ ( control_unit, *, IOSTAT=ios ) param_type
        IF ( ios/=0 ) CALL read_error(5, 'invalid parameter type: '//TRIM(paramstring) )
        IF ( param_type<1 .OR. param_type>4 .OR. param_type==3 ) CALL read_error(5, 'invalid parameter type: '//TRIM(paramstring) )
        ALLOCATE ( int_parameter_values(numvalues), real_parameter_values(numvalues), parameter_values(numvalues) )
        IF ( param_type==1 ) THEN
          READ ( Control_unit, *, IOSTAT=ios ) (int_parameter_values(j),j=1,numvalues)
          IF ( ios/=0 ) CALL read_error(5, 'invalid integer value: '//TRIM(paramname) )
        ELSEIF ( param_type==4 ) THEN
          DO j = 1, numvalues
            READ ( Control_unit, '(A)', IOSTAT=ios ) parameter_values(j)
            IF ( ios/=0 ) CALL read_error(5, 'invalid character value: '//TRIM(paramname) )
          ENDDO
        ELSE
          READ ( Control_unit, *, IOSTAT=ios ) (real_parameter_values(j),j=1,numvalues)
          IF ( ios/=0 ) CALL read_error(5, 'invalid real value: '//TRIM(paramname) )
        ENDIF
        CALL set_control_parameter(paramname, numvalues, int_parameter_values, real_parameter_values, parameter_values)
        DEALLOCATE ( int_parameter_values, real_parameter_values, parameter_values )
      ENDDO
      ! reset control parameters based on command line
      CLOSE ( control_unit )

      END SUBROUTINE read_control_file

!***********************************************************************
! setup_cont - Set control parameter value defaults
!***********************************************************************
      SUBROUTINE setup_cont()
      USE PRMS_CONTROL_FILE
      IMPLICIT NONE
      ! Local Variables
      INTEGER i, numvalues
!***********************************************************************
      Num_control_parameters = Max_num_control_parameters
      ! allocate and store parameter data
      ALLOCATE ( Control_parameter_data(Num_control_parameters) )
      DO i = 1, Num_control_parameters
        Control_parameter_data(i)%read_flag = 2 ! set to default
        Control_parameter_data(i)%data_type = 1 ! 1 = integer, 2 = real, 4 = string
        Control_parameter_data(i)%numvals = 1
        Control_parameter_data(i)%name = ' '
        ! WARNING, parameter index is set based on order defaults defined
        Control_parameter_data(i)%index = i
        ALLOCATE ( Control_parameter_data(i)%values_int(1) )
        ALLOCATE ( Control_parameter_data(i)%values_real(1) )
        ALLOCATE ( Control_parameter_data(i)%values_character(1) )
        Control_parameter_data(i)%values_int(1) = 0
        Control_parameter_data(i)%values_real(1) = 0.0
        Control_parameter_data(i)%values_character(1) = ' '
      ENDDO

!      DO i = Num_control_parameters+1, Num_control_parameters+20
!        Control_parameter_data(i)%read_flag = 0 ! 0 means not set
!        Control_parameter_data(i)%data_type = 0 ! 1 = integer, 2 = real, 4 = string
!        Control_parameter_data(i)%numvals = 0
!        Control_parameter_data(i)%name = ' '
        ! WARNING, parameter index is set based on order defaults defined
!        Control_parameter_data(i)%index = i
!      ENDDO

      ! assign default value for integer flags
      ! note: default value for all parameters set to 0, only need to reset if other than 0
      numvalues = 1
      i = 1
      Control_parameter_data(i)%name = 'print_debug'
      Print_debug = 0
      i = i + 1
      Control_parameter_data(i)%name = 'parameter_check_flag'
      Parameter_check_flag = 1
      Control_parameter_data(i)%values_int(1) = Parameter_check_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cbh_check_flag'
      Cbh_check_flag = 1
      Control_parameter_data(i)%values_int(1) = Cbh_check_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cbh_binary_flag'
      Cbh_binary_flag = 1
      Control_parameter_data(i)%values_int(1) = Cbh_binary_flag
      i = i + 1
      Control_parameter_data(i)%name = 'save_vars_to_file'
      Save_vars_to_file = 0
      i = i + 1
      Control_parameter_data(i)%name = 'init_vars_from_file'
      Init_vars_from_file = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutON_OFF'
      NhruOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOut_freq'
      NhruOut_freq = 1
      Control_parameter_data(i)%values_int(1) = NhruOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutVars'
      NhruOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'basinOutON_OFF'
      BasinOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'basinOutVars'
      BasinOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'basinOut_freq'
      BasinOut_freq = 1
      Control_parameter_data(i)%values_int(1) = BasinOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'prms_warmup'
      Prms_warmup = 1
      Control_parameter_data(i)%values_int(1) = Prms_warmup
      i = i + 1

      ! parameters that get allocated if in Control File
      Control_parameter_data(i)%name = 'basinOutVar_names'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutVar_names'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1

      ! assign default value for character parameters
      Control_parameter_data(i)%name = 'model_mode'
      Model_mode = 'GSFLOW'
      Control_parameter_data(i)%values_character(1) = Model_mode
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'precip_module'
      Precip_module = 'precip_1sta'
      Control_parameter_data(i)%values_character(1) = Precip_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'temp_module'
      Temp_module = 'temp_1sta'
      Control_parameter_data(i)%values_character(1) = Temp_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'solrad_module'
      Solrad_module = 'ddsolrad'
      Control_parameter_data(i)%values_character(1) = Solrad_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'et_module'
      Et_module = 'potet_jh'
      Control_parameter_data(i)%values_character(1) = Et_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'transp_module'
      Transp_module = 'transp_tindex'
      Control_parameter_data(i)%values_character(1) = Transp_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'data_file'
      Data_file = 'prms.data'
      Control_parameter_data(i)%values_character(1) = Data_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'param_file'
      Param_file = 'prms.params'
      Control_parameter_data(i)%values_character(1) = Param_file
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      Param_file_control_parameter_id = i
      i = i + 1
      Control_parameter_data(i)%name = 'model_output_file'
      Model_output_file = 'prms.out'
      Control_parameter_data(i)%values_character(1) = Model_output_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'var_save_file'
      Var_save_file = 'prms_ic.out'
      Control_parameter_data(i)%values_character(1) = Var_save_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'var_init_file'
      Var_init_file = 'prms_ic.in'
      Control_parameter_data(i)%values_character(1) = Var_init_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutBaseFileName'
      NhruOutBaseFileName = 'nhruout_path'
      Control_parameter_data(i)%values_character(1) = NhruOutBaseFileName
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'basinOutBaseFileName'
      BasinOutBaseFileName = 'basinout_path'
      Control_parameter_data(i)%values_character(1) = BasinOutBaseFileName
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'tmax_day'
      Tmax_day = 'tmax_day'
      Control_parameter_data(i)%values_character(1) = Tmax_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'tmin_day'
      Tmin_day = 'tmin_day'
      Control_parameter_data(i)%values_character(1) = Tmin_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'precip_day'
      Precip_day = 'precip_day'
      Control_parameter_data(i)%values_character(1) = Precip_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1

      ! time arrays
      Control_parameter_data(i)%name = 'start_time'
      DEALLOCATE ( Control_parameter_data(i)%values_int )
      ALLOCATE ( Control_parameter_data(i)%values_int(6) )
      Starttime(1) = 2000
      Starttime(2) = 10
      Starttime(3) = 1
      Starttime(4) = 0
      Starttime(5) = 0
      Starttime(6) = 0
      Control_parameter_data(i)%values_int = Starttime
      Control_parameter_data(i)%numvals = 6
      i = i + 1
      Control_parameter_data(i)%name = 'end_time'
      DEALLOCATE ( Control_parameter_data(i)%values_int )
      ALLOCATE ( Control_parameter_data(i)%values_int(6) )
      Endtime(1) = 2001
      Endtime(2) = 9
      Endtime(3) = 30
      Endtime(4) = 0
      Endtime(5) = 0
      Endtime(6) = 0
      Control_parameter_data(i)%values_int = Endtime
      Control_parameter_data(i)%numvals = 6

      Num_control_parameters = i
      
      END SUBROUTINE setup_cont

!***********************************************************************
! Get Control File from command line or user interaction.
!***********************************************************************
      SUBROUTINE get_control_filename()
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH, Print_debug, EQULS, Model_control_file
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, GET_COMMAND, TRIM
      ! Local Variables
      CHARACTER(LEN=MAXFILE_LENGTH) command_line_arg, command_line
      LOGICAL exists
      INTEGER status, nchars, numargs
!***********************************************************************
! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
! This routine expects the Control File name to be the first argument, if present
      CALL GET_COMMAND(command_line)
!      print *, 'Command line: ', TRIM(command_line)
      numargs = COMMAND_ARGUMENT_COUNT()
      IF ( Print_debug>-1 ) PRINT '(/,A)', EQULS
      CALL GET_COMMAND_ARGUMENT(1, command_line_arg, nchars, status)
      IF ( status/=0 ) THEN
        WRITE ( *,'(/,A)' ) 'Enter the name of the PRMS Control File or quit:'
        READ ( *, '(A)' ) Model_control_file
        IF ( Model_control_file(:4)=='quit' .OR. Model_control_file(:4)=='QUIT' ) STOP
      ELSE
        IF ( TRIM(command_line_arg)=='-C' ) THEN
          CALL GET_COMMAND_ARGUMENT(2, Model_control_file, nchars, status)
          IF ( status/=0 ) STOP 'ERROR, bad argment value after -C argument'
        ELSE
          Model_control_file = TRIM(command_line_arg)
        ENDIF
      ENDIF

      INQUIRE ( FILE=TRIM(Model_control_file), EXIST=exists )
      IF ( .NOT.exists ) THEN
        WRITE ( *,'(/,A)' ) 'Control File does not exist, file name: '//TRIM(Model_control_file)
        PRINT *, 'Note: Control File names cannot include spaces'
        STOP
      ENDIF

      END SUBROUTINE get_control_filename

!***********************************************************************
! Get Control File set arguments from command line.
!***********************************************************************
      SUBROUTINE get_control_arguments()
      USE PRMS_CONTROL_FILE, ONLY: Num_control_parameters, Control_parameter_data
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH, Print_debug, EQULS
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, GET_COMMAND, TRIM
      ! Local Variables
      CHARACTER(LEN=MAXFILE_LENGTH) command_line_arg, command_line
      INTEGER status, i, j, nchars, numargs, index, param_type, num_param_values
!***********************************************************************
! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
! This routine expects the Control File name to be the first argument, if present
      CALL GET_COMMAND(command_line)
      numargs = COMMAND_ARGUMENT_COUNT()
      i = 0
      DO WHILE ( i < numargs )
        i = i + 1
        CALL GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
        IF ( status/=0 ) STOP 'ERROR, setting control parameters from command line'
        IF ( TRIM(command_line_arg)=='-C' ) THEN
          i = i + 2
          CYCLE
        ELSE
          IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
          IF ( i==1 ) CYCLE
          IF ( TRIM(command_line_arg)=='-set' ) THEN
            ! find control file parameter and reset it, need type and number of values
            i = i + 1
            CALL GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
            IF ( status/=0 ) STOP 'ERROR, bad argment value after -set argument'
            IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
            index = 0
            DO j = 1, Num_control_parameters
              IF ( TRIM(command_line_arg)==TRIM(Control_parameter_data(j)%name) ) THEN
                param_type = Control_parameter_data(j)%data_type
                num_param_values = Control_parameter_data(j)%numvals
                index = j
                EXIT
              ENDIF
            ENDDO
            IF ( index==0 ) STOP 'ERROR, control parameter argument not found'
            DO j = 1, num_param_values
              i = i + 1
              CALL GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
              IF ( status/=0 ) STOP 'ERROR, bad value after -set argument'
              IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
              IF ( param_type==1 ) THEN
                READ ( command_line_arg, *, IOSTAT=status ) Control_parameter_data(index)%values_int(j)
                IF ( status/=0 ) STOP 'ERROR, reading integer command line argument'
              ELSEIF ( param_type==4 ) THEN
                Control_parameter_data(index)%values_character(j) = command_line_arg
              ELSEIF ( param_type==2 ) THEN
                READ ( command_line_arg, * ) Control_parameter_data(index)%values_real(j)
              ELSE
                STOP 'ERROR, control parameter type not implemented'
              ENDIF
            ENDDO
          ELSE
            STOP 'ERROR, command line argument invalid'
          ENDIF
        ENDIF
      ENDDO

      IF ( Print_debug>-1 ) PRINT '(A)', EQULS
      END SUBROUTINE get_control_arguments

!***********************************************************************
! check control parameter if in Control File
!***********************************************************************
      SUBROUTINE set_control_parameter(Paramname, Numvalues, Paramval_int, Paramval_real, Paramval_char) ! allow arrays
      USE PRMS_CONTROL_FILE
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=MAXCONTROL_LENGTH), INTENT(IN) :: Paramname
      INTEGER, INTENT(IN) :: Numvalues
      INTEGER, INTENT(IN) :: Paramval_int(Numvalues)
      REAL, INTENT(IN) :: Paramval_real(Numvalues)
      CHARACTER(LEN=MAXFILE_LENGTH), INTENT(IN) :: Paramval_char(Numvalues)
      ! Functions
      INTRINSIC :: TRIM
      ! Local Variables
      INTEGER :: i, j, found, dtype
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          found = i
          dtype = Control_parameter_data(i)%data_type
          Control_parameter_data(i)%numvals = Numvalues
          IF ( Control_parameter_data(i)%read_flag > 2 ) THEN ! one of variably sized parameters
            IF ( dtype==1 ) THEN
              DEALLOCATE ( Control_parameter_data(i)%values_int )
              ALLOCATE ( Control_parameter_data(i)%values_int(Numvalues) )
            ELSEIF ( dtype==4 ) THEN
              DEALLOCATE ( Control_parameter_data(i)%values_character )
              ALLOCATE ( Control_parameter_data(i)%values_character(Numvalues) )
            ELSE
              STOP 'ERROR, allocatable control parameter that is real'
            ENDIF
          ENDIF
          Control_parameter_data(i)%read_flag = 1
          IF ( dtype==1 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_int(j) = Paramval_int(j)
            ENDDO
          ELSEIF ( dtype==4 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_character(j) = Paramval_char(j)
            ENDDO
          ELSE !IF ( dtype==2 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_real(j) = Paramval_real(j)
            ENDDO
          ENDIF
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'WARNING, control parameter not used: ', TRIM(Paramname), ', ignored'
        RETURN
      ENDIF

      END SUBROUTINE set_control_parameter
