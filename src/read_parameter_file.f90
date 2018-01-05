module PRMS_READ_PARAM_FILE
    use iso_fortran_env
    use kinds_mod, only: r4, r8, i4, i8
    use prms_constants, only: EQULS, DIM_HEADER, PARAM_HEADER, ENTRY_DELIMITER
    use PRMS_MODULE, only: Print_debug, Param_file
    use control_ll_mod, only: control_list

    integer(i4), SAVE :: Param_unit, Read_parameters

    private
    public :: check_parameters, read_parameter_file_dimens, read_parameter_file_params

    contains
        !***********************************************************************
        ! Check for parameters declared but not in Parameter File
        !***********************************************************************
        subroutine check_parameters
            ! use PRMS_CONTROL_FILE, only: control_string
            use parameter_mod, only: Num_parameters, Parameter_data
            ! use PRMS_MMFAPI, only: Num_parameters, Parameter_data ! , getdim, setparam
            ! use UTILS_PRMS, only: numchars, read_error, PRMS_open_input_file
            implicit none

            ! Functions
            ! INTRINSIC :: TRIM

            ! Local Variables
            integer(i4) :: i

            !***********************************************************************
            do i = 1, Num_parameters
                if (Parameter_data(i)%decl_flag == 1 .AND. Parameter_data(i)%read_flag == 0) then
                    print *, 'Parameter: ', Parameter_data(i)%param_name, ' is not specified'
                    ! print *, 'Parameter: ', TRIM(Parameter_data(i)%param_name), ' is not specified'

                    if (Parameter_data(i)%data_flag == 1) then
                        print *, '           Set to default value:', Parameter_data(i)%default_int
                    elseif (Parameter_data(i)%data_flag == 2) then
                        print *, '           Set to default value:', Parameter_data(i)%default_real
                    endif
                endif
            enddo
        end subroutine check_parameters

        !***********************************************************************
        ! Read Parameter File Dimensions
        !***********************************************************************
        subroutine read_parameter_file_dimens(dim_data)
            use PRMS_MODULE, only: Version_read_parameter_file
            use fileio_mod, only: write_outfile
            use UTILS_PRMS, only: numchars, read_error, PRMS_open_input_file
            ! use PRMS_MMFAPI, only: setdimension
            use dimensions_mod, only: dimension_list
            implicit none

            type(dimension_list), intent(inout) :: dim_data

            ! Functions
            INTRINSIC TRIM

            ! Local Variables
            character(len=16) :: string, dimname
            character(len=150) :: line
            character(len=24) :: dimstring
            integer(i4) nchars, ios, dimen_value

            ! TODO: 20171208 PAN: add code to check for initialized dim_data

            !***********************************************************************
            Version_read_parameter_file = 'read_parameter_file.f90 2017-07-07 15:18:00Z'
            ! rewind(Param_unit)
            call PRMS_open_input_file(Param_unit, Param_file, 'param_file', 0, ios)
            if (ios /= 0) STOP

            if (Print_debug > -1) then
                call write_outfile(EQULS)
                call write_outfile('Using PRMS Parameter File: ' // Param_file)
            endif

            ! ~~~~~~~~~~~~~~~~~~~~~
            ! Echo Parameter File Header and comment lines
            read (Param_unit, FMT = '(A)', IOSTAT = ios) line
            if (ios /= 0) call read_error(13, 'description')

            if (Print_debug > -1) then
                call write_outfile('Description: ' // TRIM(line))
                call write_outfile(EQULS)
                call write_outfile('Comment lines:')
            endif

            ! Find start of dimensions section
            do
                read (Param_unit, '(A)', IOSTAT = ios) line
                if (ios == IOSTAT_END) call read_error(13, 'end of file found before dimensions')
                if (ios /= 0) call read_error(13, 'comment')
                if (line(:16) == DIM_HEADER) EXIT

                if (Print_debug > -1) call write_outfile(TRIM(line))
            enddo
            if (line(:16) /= DIM_HEADER) call read_error(11, 'missing dimension section: ' // TRIM(line))

            if (Print_debug > -1) then
                call write_outfile(EQULS)
                call write_outfile('Using dimensions    number')
            endif

            ! Read all dimensions
            do
                read(Param_unit, '(A)', IOSTAT=ios) string

                if (ios == IOSTAT_END) call read_error(13, 'end of file found before parameter section')
                if (ios /= 0) call read_error(11, 'missing dimension #### delimiter')

                if (string(:4) == '    ') CYCLE
                if (string(:2) == '//') CYCLE
                if (string == PARAM_HEADER) EXIT ! stop reading if end of dimensions section
                !if ( string(:4)/='####' ) call read_error(11, 'missing dimension #### delimiter '//string)

                if (string(:4) /= ENTRY_DELIMITER) then
                    print *, 'Warning, ignoring dimension line: ', string
                    CYCLE
                endif

                read(Param_unit, *, IOSTAT = ios) dimname
                nchars = numchars(dimname)
                if (ios /= 0) call read_error(11, 'missing dimension name: ' // dimname(:nchars))

                read(Param_unit, *, IOSTAT = ios) dimen_value
                if (ios /= 0) call read_error(11, 'missing dimension value')

                ! key, value, <description>, <default>, <maximum>
                call dim_data%set_dimension(dimname, dimen_value)
                ! call setdimension(dimname, dimen_value)

                if (dimen_value == 0) then
                    if (Print_debug > -1) print *, 'Warning, dimension: ', dimname(:nchars), ' is not needed as value specified as 0'
                endif

                if (Print_debug > -1) then
                    write (dimstring, '(A,I8)') dimname, dimen_value
                    call write_outfile(dimstring)
                endif
            enddo

            if (Print_debug > -1) call write_outfile(EQULS)
        end subroutine read_parameter_file_dimens

        !***********************************************************************
        ! Read Parameter File Dimensions
        !***********************************************************************
        subroutine read_parameter_file_params(dim_data, ctl_data)
            ! use PRMS_CONTROL_FILE, only: Param_file_control_parameter_id  ! , control_string, Control_parameter_data
            use parameter_mod, only: Num_parameters, Parameter_data, setparam
            ! use PRMS_MMFAPI, only:Num_parameters, Parameter_data, setparam !, getdim
            use UTILS_PRMS, only: numchars, read_error, PRMS_open_input_file
            use dimensions_mod, only: dimension_list
            use data_mod, only: str_arr_type
            implicit none

            type(dimension_list), intent(in) :: dim_data
            type(control_list), intent(in) :: ctl_data

            ! Functions
            INTRINSIC :: TRIM

            ! Local Variables
            character(len=16) :: string
            character(len=32) :: paramstring
            character(len=12) :: dim_string(2)
            integer(i4) nchars, ios, num_dims, num_param_values, i, j, k, param_type, num, inum, numfiles, ii, duplicate, found
            integer(i4), allocatable :: idmy(:)
            real(r4), allocatable :: dmy(:)

            type(str_arr_type), allocatable :: param_files(:)

            !***********************************************************************
            ! Find parameter section
            rewind (Param_unit)
            do
                read (Param_unit, '(A)', IOSTAT = ios) string
                if (ios == IOSTAT_END) call read_error(11, 'end of file found before parameter section') ! found end of Parameter File
                if (string(:16) == PARAM_HEADER) EXIT ! stop reading if end of dimensions section
            enddo

            ! Read all parameters and verify
            call ctl_data%get_data('param_file', param_files)
            numfiles = size(param_files)
            ! numfiles = Control_parameter_data(Param_file_control_parameter_id)%numvals
            Read_parameters = 0

            do k = 1, numfiles
                if (k > 1) then
                    close (Param_unit)
                    call PRMS_open_input_file(Param_unit, param_files(k)%str, 'param_file', 0, ios)
                    ! call PRMS_open_input_file(Param_unit, Control_parameter_data(Param_file_control_parameter_id)%values_character(k), &
                    !         &                              'param_file', 0, ios)
                endif
                do
                    read (Param_unit, '(A)', IOSTAT = ios) string
                    if (ios == IOSTAT_END) EXIT ! found end of a Parameter File
                    if (ios /= 0) call read_error(11, 'missing parameter #### delimiter')
                    if (string(:4) == '    ') CYCLE ! skip blank lines
                    if (string(:2) == '//') CYCLE ! skip comment lines
                    !if ( string(:4)/='####' ) call read_error(11, 'missing parameter #### delimiter')
                    if (string(:4) /= ENTRY_DELIMITER) CYCLE

                    read (Param_unit, '(A)', IOSTAT = ios) paramstring ! parameter name
                    if (ios /= 0) call read_error(11, 'missing parameter name')
                    nchars = numchars(paramstring)

                    read (Param_unit, *, IOSTAT = ios) num_dims
                    if (ios /= 0) call read_error(11, 'invalid number of dimensions: ' // paramstring(:nchars))
                    if (num_dims > 2) call read_error(11, 'number of dimensions > 3: ' // paramstring(:nchars))

                    num = 1
                    do i = 1, num_dims
                        read (Param_unit, '(A)', IOSTAT = ios) dim_string(i)
                        if (ios /= 0) call read_error(11, 'invalid dimension for parameter: ' // paramstring(:nchars))

                        ! inum = getdim(dim_string(i))
                        call dim_data%get_data(dim_string(i), inum)
                        if (inum == IOSTAT_END) call read_error(11, TRIM(dim_string(i)))
                        num = num * inum
                    enddo

                    read (Param_unit, *, IOSTAT = ios) num_param_values
                    if (ios /= 0) call read_error(11, 'invalid number of parameter values: ' // paramstring(:nchars))
                    !        if ( num/=num_param_values ) call read_error(11, 'invalid number of parameter values based on specified dimensions '//paramstring(:nchars))

                    read (Param_unit, *, IOSTAT = ios) param_type
                    if (ios /= 0) call read_error(11, 'invalid parameter type ' // paramstring(:nchars))
                    if (param_type < 1 .OR. param_type > 3) call read_error(11, 'invalid parameter type: ' // paramstring(:nchars))

                    ! check to see if parameter already read
                    duplicate = 0
                    found = 0
                    do ii = 1, Num_parameters
                        if (paramstring(:nchars) == TRIM(Parameter_data(ii)%param_name)) then
                            found = ii

                            if (Parameter_data(ii)%read_flag == 1) then
                                print '(/,3A)', 'WARNING, parameter: ', TRIM(paramstring), ' specified more than once'
                                inum = MIN (num_param_values, 5)
                                print *, '        Ignoring previous value(s)'
                                duplicate = ii
                                EXIT
                            endif
                        endif
                    enddo

                    if (found == 0) then
                        print '(/)'
                        print *, 'Values for parameter: ', TRIM(paramstring), ' are ignored as the parameter is not used'
                        CYCLE
                    endif

                    if (param_type == 1) then
                        allocate (idmy(num_param_values), dmy(1))
                        read (Param_unit, *, IOSTAT = ios) (idmy(j), j = 1, num_param_values)
                        if (ios /= 0) call read_error(11, 'incorrect number of parameter values: ' // paramstring(:nchars))
                        if (duplicate > 0) &
                                &         print '(A,5I8)', '         Using (up to 5 values printed):', (idmy(j), j = 1, inum)
                    ELSE
                        allocate (dmy(num_param_values), idmy(1))
                        read (Param_unit, *, IOSTAT = ios) (dmy(j), j = 1, num_param_values)
                        if (ios /= 0) call read_error(11, 'incorrect number of parameter values: ' // paramstring(:nchars))
                        if (duplicate > 0) &
                                &         print '(A,5F8.2)', '         Using (up to 5 values printed): ', (dmy(j), j = 1, inum)
                    endif

                    if (duplicate > 0) print *, ' '
                    call setparam(paramstring(:nchars), num_param_values, param_type, num_dims, dim_string, dmy, idmy)
                    Read_parameters = Read_parameters + 1
                    Parameter_data(found)%read_flag = 1
                    deallocate (dmy, idmy)
                enddo
            enddo

            close (param_unit)
        end subroutine read_parameter_file_params

end module PRMS_READ_PARAM_FILE
