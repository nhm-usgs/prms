!***********************************************************************
! Main program
!***********************************************************************
program prms6
    use m_prms6
    ! use variableKind
    use kinds_mod, only: i4
    use PRMS_MODULE, only : Number_timesteps
    use dimensions_mod
    use parameter_arr_mod, only: parameter_arr_t
    use variables_arr_mod, only: variables_arr_t
    use control_ll_mod, only: control_list
    implicit none

    type(control_list) :: Control_data
    type(dimension_list) :: Dimension_data
    type(parameter_arr_t) :: Param_data
    type(variables_arr_t) :: Var_data

    integer(i4) :: ii

    ! Functions
    intrinsic :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, TRIM   !, GET_COMMAND

    ! Local Variables
    character(len=:), allocatable :: command_line_arg
    character(len=256) :: buffer    ! contains user-supplied input for filename
    integer(i4) :: status
    integer(i4) :: nchars
    integer(i4) :: numargs

    !***********************************************************************
    ! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
    ! This routine expects the Control File name to be the first argument, if present
    !            call GET_COMMAND(command_line)
    !      print *, 'Command line: ', TRIM(command_line)
    write(*,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~'
    numargs = COMMAND_ARGUMENT_COUNT()
    write(*,*) 'numargs: ', numargs

    ! Get the length of the first command-line argument and allocate character array
    call GET_COMMAND_ARGUMENT(1, length=nchars)
    allocate(character(nchars) :: command_line_arg)

    call GET_COMMAND_ARGUMENT(1, value=command_line_arg, status=status)

    write(*,*) 'Command arg'
    write(*,*) command_line_arg
    write(*,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~'

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
