submodule (Control_class) sm_control

contains

  !====================================================================!
  module function constructor_Control(control_filename) result(this)
    use iso_fortran_env
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Control) :: this
    character(len=*), intent(in) :: control_filename

    ! Local variables
    integer(i32) :: numfiles
      !! Number of parameter filenames in the control file

    ! --------------------------------------------------------------------------
    ! if (print_debug > -2) then
      ! Output module and version information
      call print_module_info(MODNAME, MODDESC, MODVERSION)
    ! endif

    ! Initialize certain dimensions with default values
    ! this%ndays = iScalar(366)
    ! this%one = iScalar(1)
    ! this%nevap = iScalar(0)
    ! this%nhumid = iScalar(0)
    ! this%nlakeelev = iScalar(0)
    ! this%nobs = iScalar(0)
    ! this%nrain = iScalar(0)
    ! this%nratetbl = iScalar(0)
    ! this%nsol = iScalar(0)
    ! this%nsnow = iScalar(0)
    ! this%ntemp = iScalar(0)
    ! this%nwind = iScalar(0)

    ! Initialize defaults for some control file parameters
    this%prms_warmup = iScalar(0)

    this%control_filename = control_filename

    call this%read()

    this%model_output_unit = this%open_model_output_file()

    if (this%save_vars_to_file%value == 1) then
      this%restart_output_unit = this%open_var_save_file()
    endif

    if (this%model_mode%values(1)%s == 'GSFLOW') then
      this%gsflow_mode = .true.
    endif

    numfiles = size(this%param_file%values)
    if (numfiles > 1) then
      write(*, *) 'Only one parameter filename is currently supported'
    end if

    ! Example for checking file suffix
    write(*, *) this%param_file%values(1)%s
    if (this%param_file%values(1)%s(index(this%param_file%values(1)%s, '.')+1:) == 'nc') then
      write(*, *) 'Parameter netcdf file'
      ! The parameter file is in netCDF format
      ! allocate(Temperature_hru::this%model_temp)
      ! this%model_temp = Temperature_hru(ctl_data, this%summary_by_basin, this%summary_by_hru)
      ! allocate(FileIO_netcdf::this%param_file_hdl)
      this%param_file_hdl = FileIO_netcdf(filename=this%param_file%values(1)%s)
    end if

    ! Load the list of output variables
    call this%load_output_variables()


    ! TODO: add water_use_flag (composite of other flags)

    ! TODO: if print_debug > -2 output control file to model_output_file
    !                           model_output_file to stdout
    !       if print_debug > -1 output control file to stdout
    !                           output var_init_file to stdout (if used)
    !                           output var_save_file to stdout (if used)
  end function
  !====================================================================!

  !====================================================================!
  module subroutine read_Control(this) !, this%control_filename)
    use iso_fortran_env
    use variableKind, only: cLen
    use m_errors, only: eMsg, fErr, IO_OPEN
    use m_strings, only: compact, isString, lowerCase, str
    implicit none

    class(Control), intent(inout) :: this

    integer(i32) :: istat
      !! Contains the IOSTAT result from a read command
    integer(i32) :: iUnit
      !! Unit of the opened control file
    integer(i32) :: line
      !! Tracks the number of the last line read in the file
    character(len=cLen) :: buf
      !! Buffer for reading control file
    character(len=:), allocatable :: last
      !! Previous line read from file
    integer(i32), parameter :: ENTRY_OFFSET = 2
      !! Additional offset for counting entry line numbers

    logical :: go

    go = .true.

    iUnit = 1
    !call openFile(this%control_filename, iUnit, 'old', istat)
    open(unit=iUnit, file=this%control_filename, status='old', iostat=istat)
    call fErr(istat, this%control_filename, IO_OPEN)

    ! Read the Header line
    read(iUnit, 1) buf
    line = 1
    last = 'Header'

    ! Read the next line - should be '####'
    read(iUnit, 1) buf
    call compact(buf)
    line = line + 1

    do while (go)
      ! NOTE: This will break if a line has a comment included
      !       Comments after an entry are denoted with ' //'
      !       (not including ')
      if (isString(buf(1:4), '####', .true.)) then
        read(iUnit, 1) buf
        call compact(buf)
        line = line + 1
        last = trim(buf)

        ! ##########################################
        ! select-case statements from external file
        include 'sm_control_case_block.inc'
        ! ##########################################

      else
        ! Backup the line counter by one which will be where the problem occurred
        line = line - 1

        call eMsg("Could not read from file " // this%control_filename // &
                  " for entry " // last // " at line " // str(line))
      endif

      read(iUnit, 1, IOSTAT=istat) buf
      if (istat == IOSTAT_END) exit
      call compact(buf)
      line = line + 1
    enddo

    call closeFile(this%control_filename, iUnit, '', istat)
    1   format(a)
  end subroutine

  module subroutine load_output_variables(this)
    class(Control), intent(inout) :: this

    ! --------------------------------------------------------------------------
    this%output_variables = outvar_list()

    ! Freakin' long list of all possible output variables
    ! ##########################################
    ! code from external file
    include 'sm_output_variables_code.inc'
    ! ##########################################
  end subroutine


  module function open_model_output_file(this)
    !! Opens the model_output_file, if present, and sets this%model_output_unit
    use m_errors, only: fErr, IO_OPEN
    implicit none

    integer(i32) :: open_model_output_file
    class(Control), intent(inout) :: this

    integer(i32) :: istat
    integer(i32) :: iunit

    ! --------------------------------------------------------------------------
    if (allocated(this%model_output_file%values)) then
      open(newunit=iunit, file=this%model_output_file%values(1)%s, status='replace', iostat=istat)

      call fErr(istat, this%model_output_file%values(1)%s, IO_OPEN)

      open_model_output_file = iunit
    endif
  end function


  module function open_var_save_file(this)
    !! Open the var_save_file (aka restart file)
    use m_errors, only: fErr, IO_OPEN
    implicit none

    integer(i32) :: open_var_save_file
    class(Control), intent(inout) :: this

    integer(i32) :: istat
    integer(i32) :: iunit

    ! --------------------------------------------------------------------------
    if (allocated(this%var_save_file%values)) then
      open(newunit=iunit, file=this%var_save_file%values(1)%s, status='replace', &
           form='unformatted', access='stream', iostat=istat)

      call fErr(istat, this%var_save_file%values(1)%s, IO_OPEN)

      open_var_save_file = iunit
    endif
  end function

end submodule
