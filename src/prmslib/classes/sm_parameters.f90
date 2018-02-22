submodule (Parameters_class) sm_parameters

contains
    !====================================================================!
    module function constructor_Parameter(Control_data) result(this)
        type(Parameters) :: this
        class(Control), intent(in) :: Control_data

        this%parameter_filenames = Control_data%param_file
    end function

    module subroutine read_parameter_file(this)
        use variableKind
        use m_fileIO, only: openFile
        use prms_constants, only: ENTRY_DELIMITER
        use iso_fortran_env
        use UTILS_PRMS, only: read_error
        ! use dimensions_mod, only: dimension_list
        ! use data_mod, only: str_arr_type
        implicit none

        type(Parameters), intent(inout) :: this

        ! Functions
        INTRINSIC :: TRIM

        ! Local Variables
        character(len=16) :: string
        character(len=32) :: paramstring
        character(len=12) :: dim_string(2)
        integer(i32) :: iunit
            ! File IO unit for opened parameter file

        integer(i32) :: ios
        integer(i32) :: num_dims
        integer(i32) :: num_param_values
        integer(i32) :: i
        integer(i32) :: j
        integer(i32) :: k
        integer(i32) :: param_type
        integer(i32) :: num
        integer(i32) :: inum
        integer(i32) :: numfiles
        integer(i32) :: ii
        integer(i32) :: duplicate
        integer(i32) :: found
        integer(i32), allocatable :: idmy(:)
        real(r32), allocatable :: dmy(:)


        !***********************************************************************

        ! NOTE: Dimensions no longer stored in the parameter file. They
        !       are now stored in the control file.

        ! Read all parameters and verify
        numfiles = size(this%parameter_filenames%values)
        ! Read_parameters = 0

        do k = 1, numfiles
            ! Open parameter file
            call openFile(this%parameter_filenames%values(k)%s, iunit, stat='old', istat=ios)

            do
                read (iunit, '(A)', IOSTAT = ios) string
                if (ios == IOSTAT_END) EXIT ! found end of a Parameter File
                if (ios /= 0) call read_error(11, 'missing parameter #### delimiter')
                if (string(:4) == '    ') CYCLE ! skip blank lines
                if (string(:2) == '//') CYCLE ! skip comment lines
                !if ( string(:4)/='####' ) call read_error(11, 'missing parameter #### delimiter')
                if (string(:4) /= ENTRY_DELIMITER) CYCLE

                ! ~~~~~~~~~~~~~~~~~~
                ! Parameter name
                read (iunit, '(A)', IOSTAT = ios) paramstring ! parameter name
                if (ios /= 0) call read_error(11, 'missing parameter name')
                ! nchars = numchars(paramstring)

                select case(trim(paramstring))
                case('K_coef')
                    this%K_coef%name = trim(paramstring)
                    ! this%K_coef%read(iunit)
                case default
                    write(*,*) 'Parameter name' // trim(paramstring) // 'is not a valid parameter.'
                end select

                ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~
                ! ! Number of parameter values
                ! read (iunit, *, IOSTAT = ios) num_param_values
                ! if (ios /= 0) call read_error(11, 'invalid number of parameter values: ' // trim(paramstring))
                ! !        if ( num/=num_param_values ) call read_error(11, 'invalid number of parameter values based on specified dimensions '//paramstring(:nchars))
                !
                ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ! ! Datatype of parameter values
                ! read (iunit, *, IOSTAT = ios) param_type
                ! if (ios /= 0) call read_error(11, 'invalid parameter type ' // trim(paramstring))
                ! if (param_type < 1 .OR. param_type > 3) call read_error(11, 'invalid parameter type: ' // trim(paramstring))
                !
                ! ! ~~~~~~~~~~~~~~~~~~~~~
                ! ! Read parameter values
                !
                ! ! check to see if parameter already read
                ! duplicate = 0
                ! found = 0
                ! do ii = 1, param_data%Num_parameters
                !     if (trim(paramstring) == param_data%Parameter_data(ii)%param_name) then
                !         found = ii
                !
                !         if (param_data%Parameter_data(ii)%read_flag == 1) then
                !             print '(/,3A)', 'WARNING, parameter: ', TRIM(paramstring), ' specified more than once'
                !             inum = MIN (num_param_values, 5)
                !             print *, '        Ignoring previous value(s)'
                !             duplicate = ii
                !             EXIT
                !         endif
                !     endif
                ! enddo
                !
                ! if (found == 0) then
                !     print '(/)'
                !     print *, 'Values for parameter: ', TRIM(paramstring), ' are ignored as the parameter is not used'
                !     CYCLE
                ! endif
                !
                ! if (param_type == 1) then
                !     allocate (idmy(num_param_values), dmy(1))
                !     read (iunit, *, IOSTAT = ios) (idmy(j), j = 1, num_param_values)
                !     if (ios /= 0) call read_error(11, 'incorrect number of parameter values: ' // trim(paramstring))
                !     if (duplicate > 0) &
                !             &         print '(A,5I8)', '         Using (up to 5 values printed):', (idmy(j), j = 1, inum)
                ! ELSE
                !     allocate (dmy(num_param_values), idmy(1))
                !     read (iunit, *, IOSTAT = ios) (dmy(j), j = 1, num_param_values)
                !     if (ios /= 0) call read_error(11, 'incorrect number of parameter values: ' // trim(paramstring))
                !     if (duplicate > 0) &
                !             &         print '(A,5F8.2)', '         Using (up to 5 values printed): ', (dmy(j), j = 1, inum)
                ! endif
                !
                ! if (duplicate > 0) print *, ' '
                ! call param_data%setparam(trim(paramstring), num_param_values, param_type, num_dims, dim_string, dmy, idmy)
                ! Read_parameters = Read_parameters + 1
                ! param_data%Parameter_data(found)%read_flag = 1
                ! deallocate (dmy, idmy)
            enddo
        enddo

        close (iunit)
    end subroutine
end submodule
