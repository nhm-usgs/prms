!***********************************************************************
! Read Control File
!***********************************************************************
module PRMS_CONTROL_FILE
    use iso_fortran_env
    use kinds_mod, only: r4, r8, i4, i8
    use control_mod, only: control_t
    use control_ll_mod, only: control_list

    implicit none

    character(:), allocatable :: Control_description
contains

    !***********************************************************************
    ! Get Control File set arguments from command line.
    !***********************************************************************
    ! subroutine get_control_arguments()
    !     use prms_constants, only: MAXFILE_LENGTH, EQULS
    !     ! use PRMS_CONTROL_FILE, ONLY : Num_control_parameters, Control_parameter_data
    !     use PRMS_MODULE, ONLY : Print_debug
    !     implicit none
    !
    !     ! Functions
    !     INTRINSIC :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, GET_COMMAND, TRIM
    !
    !     ! Local Variables
    !     character(LEN=MAXFILE_LENGTH) command_line_arg, command_line
    !     INTEGER status, i, j, nchars, numargs, index, param_type, num_param_values
    !
    !     !***********************************************************************
    !     ! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
    !     ! This routine expects the Control File name to be the first argument, if present
    !     call GET_COMMAND(command_line)
    !     numargs = COMMAND_ARGUMENT_COUNT()
    !
    !     i = 0
    !     do WHILE (i < numargs)
    !         i = i + 1
    !         call GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
    !         if (status /= 0) STOP 'ERROR, setting control parameters from command line'
    !         if (TRIM(command_line_arg) == '-C') then
    !             i = i + 2
    !             CYCLE
    !         else
    !             if (Print_debug > -1) print *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
    !             if (i == 1) CYCLE
    !
    !             if (TRIM(command_line_arg) == '-set') then
    !                 ! find control file parameter and reset it, need type and number of values
    !                 i = i + 1
    !                 call GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
    !                 if (status /= 0) STOP 'ERROR, bad argment value after -set argument'
    !                 if (Print_debug > -1) print *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
    !
    !                 index = 0
    !                 do j = 1, Num_control_parameters
    !                     if (TRIM(command_line_arg) == Control_parameter_data(j)%name) then
    !                         param_type = Control_parameter_data(j)%data_type
    !                         num_param_values = Control_parameter_data(j)%numvals
    !                         index = j
    !                         EXIT
    !                     endif
    !                 enddo
    !
    !                 if (index == 0) STOP 'ERROR, control parameter argument not found'
    !
    !                 do j = 1, num_param_values
    !                     i = i + 1
    !                     call GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
    !                     if (status /= 0) STOP 'ERROR, bad value after -set argument'
    !                     if (Print_debug > -1) print *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
    !
    !                     if (param_type == 1) then
    !                         read (command_line_arg, *, IOSTAT = status) Control_parameter_data(index)%values_int(j)
    !                         if (status /= 0) STOP 'ERROR, reading integer command line argument'
    !                     elseif (param_type == 4) then
    !                         Control_parameter_data(index)%values_character(j) = command_line_arg
    !                     elseif (param_type == 2) then
    !                         read (command_line_arg, *) Control_parameter_data(index)%values_real(j)
    !                     else
    !                         STOP 'ERROR, control parameter type not implemented'
    !                     endif
    !                 enddo
    !             else
    !                 STOP 'ERROR, command line argument invalid'
    !             endif
    !         endif
    !     enddo
    !
    !     if (Print_debug > -1) print '(A)', EQULS
    ! end subroutine get_control_arguments

    !***********************************************************************
    ! Get Control File from command line or user interaction.
    !***********************************************************************
    subroutine get_control_filename(Model_control_file)
        use prms_constants, only: EQULS
        use PRMS_MODULE, ONLY : Print_debug  ! , Model_control_file
        implicit none

        character(len=:), allocatable, intent(inout) :: Model_control_file

        ! Functions
        intrinsic :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, TRIM   !, GET_COMMAND

        ! Local Variables
        character(len=:), allocatable :: command_line_arg
        character(len=256) :: buffer    ! contains user-supplied input for filename
        logical :: exists
        integer(i4) :: status
        integer(i4) :: nchars
        integer(i4) :: numargs

        !***********************************************************************
        ! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
        ! This routine expects the Control File name to be the first argument, if present
        !            call GET_COMMAND(command_line)
        !      print *, 'Command line: ', TRIM(command_line)
        numargs = COMMAND_ARGUMENT_COUNT()

        if (Print_debug > -1) print '(/,A)', EQULS

        ! Get the length of the first command-line argument and allocate character array
        call GET_COMMAND_ARGUMENT(1, length=nchars)
        allocate(character(nchars) :: command_line_arg)

        call GET_COMMAND_ARGUMENT(1, value=command_line_arg, status=status)

        if (status /= 0) then
            print *, 'status: ', status
            write(*, '(/,A)') 'Enter the name of the PRMS Control File or quit:'
            read(*, '(A)') buffer
            Model_control_file = TRIM(buffer)

            if (Model_control_file(: 4) == 'quit' .OR. Model_control_file(: 4) == 'QUIT') STOP
        else
            if (command_line_arg == '-C') then
                ! Get length of second cmdline parameter and allocate Model_control_file
                call GET_COMMAND_ARGUMENT(2, length=nchars)
                allocate(character(nchars) :: Model_control_file)

                call GET_COMMAND_ARGUMENT(2, value=Model_control_file, status=status)
                if (status /= 0) STOP 'ERROR, bad argment value after -C argument'
            else
                Model_control_file = command_line_arg
            endif
        endif

        INQUIRE (FILE=Model_control_file, EXIST=exists)
        if (.NOT. exists) then
            write (*, '(/,A)') 'Control File does not exist, file name: ' // Model_control_file
            print *, 'Note: Control File names cannot include spaces'
            STOP
        endif
    end subroutine get_control_filename

    subroutine read_control_file(ctl_data)
        use prms_constants, only: ENTRY_DELIMITER, MAXCONTROL_LENGTH
        use PRMS_MODULE, only: Version_read_control_file, Model_control_file
                               ! , Print_debug, Model_output_file
        ! use fileio_mod, only: write_outfile
        use UTILS_PRMS, only: read_error, PRMS_open_input_file, PRMS_open_output_file  ! , numchars
        use control_ll_mod, only: control_list
        use data_mod, only: str_arr_type

        implicit none

        ! Procedure arguments
        type(control_list), intent(inout) :: ctl_data

        ! Functions
        INTRINSIC TRIM

        ! Local Variables
        character(len=:), allocatable :: paramname
        integer(i4) :: numvalues
        integer(i4) :: param_type

        integer(i4) :: ios
        integer(i4) :: control_unit
        integer(i4) :: jj

        character(len=MAXCONTROL_LENGTH) :: buffer  ! fixed-length buffer for reading

        type(str_arr_type), allocatable, dimension(:) :: str_parameter_values
        integer(i4), allocatable :: int_parameter_values(:)
        real(r4), allocatable :: real_parameter_values(:)


        !***********************************************************************
        Version_read_control_file = 'read_control_file.f90 2017-09-29 13:48:00Z'

        ! control filename cannot include blanks
        call get_control_filename(Model_control_file)
        call PRMS_open_input_file(control_unit, Model_control_file, 'model_control_file', 0, ios)
        if (ios /= 0) call read_error(10, Model_control_file)

        ! read header
        read (control_unit, '(A)', IOSTAT = ios) buffer
        if (ios /= 0) call read_error(12, Model_control_file)
        Control_description = TRIM(buffer)

        ! call setup_cont() ! set default control parameter values

        ! Read all Control Parameters
        do
            read (control_unit, '(A)', IOSTAT = ios) buffer
            if (ios == IOSTAT_END) EXIT ! found end of Control File
            if (ios /= 0) call read_error(12, 'missing #### delimiter')
            if (buffer(:4) /= ENTRY_DELIMITER) CYCLE ! skip until delimiter found, such as blank of // comment lines

            ! The line after #### is the parameter name
            read (control_unit, '(A)', IOSTAT = ios) buffer ! parameter name
            if (ios /= 0) call read_error(5, 'missing parameter name')
            paramname = TRIM(buffer)

            ! The next line contains the number of values for this control parameter
            read (control_unit, *, IOSTAT = ios) numvalues
            if (ios /= 0) call read_error(5, 'invalid number of values: ' // paramname)

            ! Next is the parameter type (1=integer, 2=real, 3=double, 4=string)
            read (control_unit, *, IOSTAT = ios) param_type
            if (ios /= 0) call read_error(5, 'invalid parameter type: ' // paramname)
            if (param_type < 1 .OR. param_type > 4 .OR. param_type == 3) &
                    call read_error(5, 'invalid parameter type: ' // paramname)


            if (param_type == 1) then
                allocate(int_parameter_values(numvalues))
                read (Control_unit, *, IOSTAT = ios) (int_parameter_values(jj), jj = 1, numvalues)
                if (ios /= 0) call read_error(5, 'invalid integer value: ' // paramname)

                call ctl_data%set(paramname, int_parameter_values, param_type)

                deallocate(int_parameter_values)
            elseif (param_type == 4) then
                allocate(str_parameter_values(numvalues))

                do jj = 1, numvalues
                    read (Control_unit, '(A)', IOSTAT = ios) buffer
                    str_parameter_values(jj)%str = TRIM(buffer)
                    if (ios /= 0) call read_error(5, 'invalid character value: ' // paramname // TRIM(buffer))
                enddo

                call ctl_data%set(paramname, str_parameter_values, param_type)

                deallocate(str_parameter_values)
            else
                allocate(real_parameter_values(numvalues))
                read (Control_unit, *, IOSTAT = ios) (real_parameter_values(jj), jj = 1, numvalues)
                if (ios /= 0) call read_error(5, 'invalid real value: ' // paramname)

                call ctl_data%set(paramname, real_parameter_values, param_type)

                deallocate(real_parameter_values)
            endif
        enddo

        ! reset control parameters based on command line
        close (control_unit)
    end subroutine read_control_file

    !***********************************************************************
    ! init_control - Set control parameter value defaults
    !***********************************************************************
    subroutine init_control_defaults(ctl_data)
        use PRMS_MODULE, ONLY : Print_debug, &
                Init_vars_from_file, Save_vars_to_file, Parameter_check_flag, Param_file, Model_output_file, &
                Precip_module, Temp_module, Et_module, Solrad_module, Transp_module, Print_debug, &
                Model_mode, Endtime, Starttime, Prms_warmup, NhruOutON_OFF, BasinOutON_OFF, &
                ! Cbh_check_flag, Cbh_binary_flag, &
                BasinOutVars, BasinOut_freq, BasinOutBaseFileName, &
                NhruOutVars, NhruOut_freq, NhruOutBaseFileName, &
                Precip_day, Tmax_day, Tmin_day, &
                Var_init_file, Var_save_file, Data_file
        use control_ll_mod, only: control_list
        use data_mod, only: str_arr_type
        implicit none

        type(control_list), intent(inout) :: ctl_data

        ! Local Variables
        integer(i4) :: zero(1)
        integer(i4) :: one(1)
        type(str_arr_type), allocatable, dimension(:) :: str_values


        !***********************************************************************
        zero = 0
        one = 1

        allocate(str_values(1))

        ! assign default value for integer flags
        ! note: default value for all parameters set to 0, only need to reset if other than 0
        call ctl_data%set('print_debug', zero, 1)
        call ctl_data%get_data('print_debug', Print_debug)

        call ctl_data%set('parameter_check_flag', zero, 1)
        call ctl_data%get_data('parameter_check_flag', Parameter_check_flag)

        call ctl_data%set('parameter_check_flag', zero, 1)
        call ctl_data%get_data('parameter_check_flag', Parameter_check_flag)

        call ctl_data%set('cbh_check_flag', zero, 1)
        ! call ctl_data%get_data('cbh_check_flag', Cbh_check_flag)

        call ctl_data%set('cbh_binary_flag', zero, 1)
        ! call ctl_data%get_data('cbh_binary_flag', Cbh_binary_flag)

        call ctl_data%set('save_vars_to_file', zero, 1)
        call ctl_data%get_data('save_vars_to_file', Save_vars_to_file)

        call ctl_data%set('init_vars_from_file', zero, 1)
        call ctl_data%get_data('init_vars_from_file', Init_vars_from_file)

        call ctl_data%set('nhruOutON_OFF', zero, 1)
        call ctl_data%get_data('nhrOutON_OFF', NhruOutON_OFF)

        call ctl_data%set('nhruOut_freq', one, 1)
        call ctl_data%get_data('nhruOut_freq', NhruOut_freq)

        call ctl_data%set('nhruOutVars', zero, 1)
        call ctl_data%get_data('nhruOutVars', NhruOutVars)

        call ctl_data%set('basinOutON_OFF', zero, 1)
        call ctl_data%get_data('basinOutON_OFF', BasinOutON_OFF)

        call ctl_data%set('basinOutVars', zero, 1)
        call ctl_data%get_data('basinOutVars', BasinOutVars)

        call ctl_data%set('basinOut_freq', one, 1)
        call ctl_data%get_data('basinOut_freq', BasinOut_freq)

        call ctl_data%set('prms_warmup', one, 1)
        call ctl_data%get_data('prms_warmup', Prms_warmup)

        ! Set defaults for string control parameters
        str_values(1)%str = ''
        call ctl_data%set('basinOutVar_names', str_values, 4)

        call ctl_data%set('nhruOutVar_names', str_values, 4)

        str_values(1)%str = 'GSFLOW'
        call ctl_data%set('model_mode', str_values, 4)
        call ctl_data%get_data('model_mode', Model_mode)

        str_values(1)%str = 'precip_1sta'
        call ctl_data%set('precip_module', str_values, 4)
        call ctl_data%get_data('precip_module', Precip_module)

        str_values(1)%str = 'temp_1sta'
        call ctl_data%set('temp_module', str_values, 4)
        call ctl_data%get_data('temp_module', Temp_module)

        str_values(1)%str = 'ddsolrad'
        call ctl_data%set('solrad_module', str_values, 4)
        call ctl_data%get_data('solrad_module', Solrad_module)

        str_values(1)%str = 'potet_jh'
        call ctl_data%set('et_module', str_values, 4)
        call ctl_data%get_data('et_module', Et_module)

        str_values(1)%str = 'transp_tindex'
        call ctl_data%set('transp_module', str_values, 4)
        call ctl_data%get_data('transp_module', Transp_module)

        str_values(1)%str = 'prms.data'
        call ctl_data%set('data_file', str_values, 4)
        call ctl_data%get_data('data_file', Data_file)

        str_values(1)%str = 'prms.params'
        call ctl_data%set('param_file', str_values, 4)
        call ctl_data%get_data('param_file', Param_file)

        str_values(1)%str = 'prms.out'
        call ctl_data%set('model_output_file', str_values, 4)
        call ctl_data%get_data('model_output_file', Model_output_file)

        str_values(1)%str = 'prms_ic.out'
        call ctl_data%set('var_save_file', str_values, 4)
        call ctl_data%get_data('var_save_file', Var_save_file)

        str_values(1)%str = 'prms_ic.in'
        call ctl_data%set('var_init_file', str_values, 4)
        call ctl_data%get_data('var_init_file', Var_init_file)

        str_values(1)%str = 'nhruout_path'
        call ctl_data%set('nhruOutBaseFileName', str_values, 4)
        call ctl_data%get_data('nhruOutBaseFileName', NhruOutBaseFileName)

        str_values(1)%str = 'basinout_path'
        call ctl_data%set('basinOutBaseFileName', str_values, 4)
        call ctl_data%get_data('basinOutBaseFileName', BasinOutBaseFileName)

        str_values(1)%str = 'tmax_day'
        call ctl_data%set('tmax_day', str_values, 4)
        call ctl_data%get_data('tmax_day', Tmax_day)

        str_values(1)%str = 'tmin_day'
        call ctl_data%set('tmin_day', str_values, 4)
        call ctl_data%get_data('tmin_day', Tmin_day)

        str_values(1)%str = 'precip_day'
        call ctl_data%set('precip_day', str_values, 4)
        call ctl_data%get_data('precip_day', Precip_day)

        ! time arrays
        Starttime = [2000, 10, 1, 0, 0, 0]
        call ctl_data%set('start_time', Starttime, 1)

        Endtime = [2001, 9, 30, 0, 0, 0]
        call ctl_data%set('end_time', Endtime, 1)

    end subroutine init_control_defaults

end module PRMS_CONTROL_FILE
