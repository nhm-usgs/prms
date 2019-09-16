!***********************************************************************
! Main program
!***********************************************************************
program prms6
  use variableKind
  use prms_constants
  ! use, intrinsic :: iso_c_binding, only: c_sizeof
  use, intrinsic :: iso_fortran_env, only: output_unit
  use Control_class, only: Control
  use Simulation_class, only: Simulation
  ! use ieee_arithmetic
  ! use ieee_features
  implicit none

  character(len=:), allocatable :: control_filename
    !! Name of the control file
  type(Control) :: Control_data
    !! Class of control file related parameters
  ! type(Parameters) :: Parameter_data
    !! Class of input parameters
  type(Simulation), target :: model_simulation
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
  real(r64) :: dummy_r64
  real(r32) :: dummy_r32
  ! real(r32) :: arr_dummy_r32(109951,13505)


  ! ---------------------------------------------------------------------------
  ! call ieee_set_halting_mode(ieee_overflow, .false.)
  call system_clock(count=start_rtc, count_rate=rate_rtc, count_max=max_rtc)
  call cpu_time(time=start_ct)

  print *, 'CLOSEZERO, NEARZERO, DNEARZERO'
  print *, CLOSEZERO, NEARZERO, DNEARZERO

  dummy_r32 = 0.0
  dummy_r64 = 0.0

  print *, 'Ranges'
  print *, 'r32: ', range(dummy_r32)
  print *, 'smallest r32 value: ', tiny(dummy_r32)
  print *, 'minexponent of r32: ', minexponent(dummy_r32)

  write(output_unit, *) 'r64: ', radix(dummy_r64), exponent(tiny(dummy_r64)), exponent(huge(dummy_r64))
  write(output_unit, *) 'smallest r64 value: ', tiny(dummy_r64)
  write(output_unit, *) 'precision of r64: ', precision(dummy_r64)
  write(output_unit, *) 'range of r64: ', range(dummy_r64)
  ! print *, 'r32 array memory footprint (109951 x 13505):', c_sizeof(arr_dummy_r32)
  write(output_unit, fmt='(a)') repeat('=', 72)
  call get_control_filename(control_filename)

  ! Control_data = Control(control_filename)
  call Control_data%init(control_filename)

  ! write(*, *) 'Traversing output variables'
  ! call Control_data%output_variables%traverse(print_outvar_key)

  ! TODO: Other stuff to consider
  ! - variable: kkiter; Current iteration in GSFLOW simulation (when model_mode=GSFLOW)
  ! - code behavior when init_vars_from_file==1
  ! * how to handle having different combinations of physics modules


  ! TODO: How to handle allocation and reading of parameter variables depending
  !       on which physics modules are selected?
  ! Parameter_data = Parameters(Control_data)

  ! TODO: Need routines for setting up output variables


  ! Initialize the simulation object
  call model_simulation%init(Control_data)
  ! model_simulation = Simulation(Control_data)

  ! 2019-08-08 PAN: This is rather kludgy...
  ! Close the parameter file
  call Control_data%param_file_hdl%close()

  ! Run the simulation
  call model_simulation%run(Control_data)

  ! TODO: Open, position, and read any ancillary data including:
  !       CBH files,

  ! Cleanup everything
  write(output_unit, fmt='(a)') repeat('-', 72)
  write(output_unit, fmt='(a)') 'Cleaning up...'
  write(output_unit, fmt='(a)') repeat('-', 72)
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

  subroutine print_outvar_key(key, datatype, done)
    implicit none

    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: datatype
    logical, intent(out) :: done

    print *, '-------------------------'
    print *, 'Variable: ', key
    print *, 'datatype: ', datatype

    done = .false.
  end subroutine
end program
