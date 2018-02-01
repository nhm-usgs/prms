submodule (Control_class) sm_control
    implicit none

contains

    module procedure read_control_file!(this)
        ! Procedure arguments
        !type(Control), intent(inout) :: this

        ! Local Variables
        character(len=:), allocatable :: paramname
        integer(i32) :: numvalues
        integer(i32) :: param_type

        integer(i32) :: ios
        integer(i32) :: control_unit
        integer(i32) :: jj

        character(len=MAXCONTROL_LENGTH) :: buffer  ! fixed-length buffer for reading

        ! type(sVariable), allocatable :: str_parameter_values
        integer(i32), allocatable :: int_parameter_values
        real(r32), allocatable :: real_parameter_values


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
    end procedure

end submodule
