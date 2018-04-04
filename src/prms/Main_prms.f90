!***********************************************************************
! Main program
!***********************************************************************
program prms6
  use variableKind
  !use PRMS_MODULE, only : Number_timesteps
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SIMULATION, only: Simulation

  implicit none

  character(len=:), allocatable :: control_filename
    !! Name of the control file

  type(Control) :: Control_data
    !! Class of control file related parameters
  type(Parameters) :: Parameter_data
    !! Class of input parameters
  type(Simulation) :: model_simulation
    !! PRMS model simulation class

  ! type(control_list) :: Control_data
  ! type(dimension_list) :: Dimension_data
  ! type(parameter_arr_t) :: Param_data
  ! type(variables_arr_t) :: Var_data

  integer(i32) :: ii
  integer(i32) :: ctl_crap
  real(r32) :: param_crap
  real(r32) :: param_crap_1D(14)
  real(r32) :: param_crap_2D(14, 12)

  call get_control_filename(control_filename)

  Control_data = Control(control_filename)

  ! ctl_crap = Control_data%nhru
  ! print *, 'nhru: ', ctl_crap

  ! TODO: How to handle allocation and reading of parameter variables depending
  !       on which physics modules are selected?
  Parameter_data = Parameters(Control_data)

  !param_crap_1D = Parameter_data%tmin_cbh_adj
  !print *, 'tmin_cbh_adj: ', param_crap_1D

  ! TODO: Need routines for setting up output variables

  print *, "Initialize Simulation"
  model_simulation = Simulation(Control_data, Parameter_data)

  print *, "Run the simulation"
  call model_simulation%run(Control_data, Parameter_data)
  ! TODO: Open, position, and read any ancillary data including:
  !       CBH files,

  ! TODO: Possibly have outermost time loop here and then call physics modules
  !       for each timestep


contains

  !***********************************************************************
  ! Get Control File from command line or user interaction.
  !***********************************************************************
  subroutine get_control_filename(control_filename)
    use m_fileIO, only: fileExists
    implicit none

    character(len=:), allocatable, intent(inout) :: control_filename
      !! Name of the control file

    ! Functions
    intrinsic :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, TRIM

    ! Local Variables
    character(len=:), allocatable :: command_line_arg
    character(len=256) :: buffer    ! contains user-supplied input for filename
    integer(i32) :: status
    integer(i32) :: nchars
    integer(i32) :: numargs
    ! logical :: exists

    !***********************************************************************
    ! Subroutine GET_COMMAND_ARGUMENT is a Fortran 2003 routne and may not
    ! be available with all compilerst.
    ! This routine expects the Control File name, if present, to be the first
    ! argument.
    !      call GET_COMMAND(command_line)
    !      print *, 'Command line: ', TRIM(command_line)
    numargs = COMMAND_ARGUMENT_COUNT()

    ! Get the length of the first command-line argument and allocate character array
    call GET_COMMAND_ARGUMENT(1, length=nchars)
    allocate(character(nchars) :: command_line_arg)

    call GET_COMMAND_ARGUMENT(1, value=command_line_arg, status=status)

    if (status == 0) then
      if (command_line_arg == '-C') then
        ! Get length of second cmdline parameter and allocate Model_control_file
        call GET_COMMAND_ARGUMENT(2, length=nchars)
        allocate(character(nchars) :: control_filename)

        call GET_COMMAND_ARGUMENT(2, value=control_filename, status=status)
        if (status /= 0) STOP 'ERROR, bad argment value after -C argument'
      else
        ! PAN: would this occur if the control filename was provided without -C?
        control_filename = command_line_arg
      endif
    else
      ! PAN: No control file specified on command line; for now just use a default control filename
      control_filename = 'control'
    endif

    ! INQUIRE (FILE=control_filename, EXIST=exists)
    if (.not. fileExists(control_filename)) then
      write(*, '(/,A)') 'Control File does not exist, file name: ' // control_filename
      write(*, *) 'Note: Control File names cannot include spaces'
      STOP
    endif
  end subroutine

end program
