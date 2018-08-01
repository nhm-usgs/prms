!***********************************************************************
! Main program
!***********************************************************************
program prms6
  use variableKind
  use, intrinsic :: iso_fortran_env, only: output_unit
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use Simulation_class, only: Simulation
  implicit none

  character(len=:), allocatable :: control_filename
    !! Name of the control file
  type(Control) :: Control_data
    !! Class of control file related parameters
  type(Parameters) :: Parameter_data
    !! Class of input parameters
  type(Simulation) :: model_simulation
    !! PRMS model simulation class

  integer(i64) :: start_rtc
    !! Starting system clock value
  integer(i64) :: end_rtc
    !! Ending system clock value
  integer(i64) :: max_rtc
    !! Maximum system clock ticks per second
  integer(i64) :: rate_rtc
    !! System clock ticks per second
  real(i64) :: delta_rtc_sec
    !! Elapsed system clock in seconds
  real(r64) :: start_ct
    !! Starting cpu time value
  real(r64) :: end_ct
    !! Ending cpu time value

  ! ---------------------------------------------------------------------------
  call system_clock(count=start_rtc, count_rate=rate_rtc, count_max=max_rtc)
  call cpu_time(time=start_ct)

  write(output_unit, fmt='(a)') repeat('=', 72)
  call get_control_filename(control_filename)

  Control_data = Control(control_filename)

  ! TODO: Other stuff to consider
  ! - variable: kkiter; Current iteration in GSFLOW simulation (when model_mode=GSFLOW)
  ! - code behavior when init_vars_from_file==1
  ! * how to handle having different combinations of physics modules


  ! TODO: How to handle allocation and reading of parameter variables depending
  !       on which physics modules are selected?
  Parameter_data = Parameters(Control_data)

  ! TODO: Need routines for setting up output variables


  ! Initialize the simulation object
  model_simulation = Simulation(Control_data, Parameter_data)

  ! Run the simulation
  call model_simulation%run(Control_data, Parameter_data)

  ! TODO: Open, position, and read any ancillary data including:
  !       CBH files,

  ! Cleanup everything
  call model_simulation%cleanup(Control_data)

  call cpu_time(time=end_ct)
  call system_clock(count=end_rtc)

  if (Control_data%print_debug%value > -1) then
    delta_rtc_sec = real(end_rtc - start_rtc, r64) / real(rate_rtc, r64)

    write(output_unit, fmt='(a)') repeat('-', 72)
    write(output_unit, fmt='(a, 1x, f16.4, 1x, a)') 'Elapsed system clock:', delta_rtc_sec, 'seconds.'
    write(output_unit, fmt='(a, 1x, f16.4, 1x, a)') 'Elapsed cpu time:', end_ct - start_ct, 'seconds.'
  endif
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
    ! character(len=256) :: buffer    ! contains user-supplied input for filename
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
