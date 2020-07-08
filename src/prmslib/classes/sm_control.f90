submodule (Control_class) sm_control

contains

  !====================================================================!
  module subroutine init_Control(this, control_filename)
    use iso_fortran_env
    use UTILS_PRMS, only: print_module_info
    implicit none

    class(Control), intent(inout) :: this
      !! Control Class
    character(len=*), intent(in) :: control_filename
      !! File name to read the control parameters from.

    ! Local variables
    character(len=2) :: fileext
    integer(i32) :: numfiles, ppos
      !! Number of parameter filenames in the control file

    ! --------------------------------------------------------------------------
    ! if (print_debug > -2) then
      ! Output module and version information
      call print_module_info(MODNAME, MODDESC, MODVERSION)
    ! endif

    ! Initialize defaults for some control file parameters
    this%prms_warmup = iScalar(0)
    this%cascade_flag = iScalar(0)

    this%control_filename = control_filename

    call this%read()

    this%model_output_unit = this%open_model_output_file()

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Restart

    if (this%init_vars_from_file%value == 1) then
      ! Open a prior restart file
      call this%open_restart_netcdf()
    end if

    if (this%save_vars_to_file%value == 1) then
      ! Create restart output file
      call this%create_restart_netcdf()
    endif

    if (this%model_mode%values(1)%s == 'GSFLOW') then
      this%gsflow_mode = .true.
    endif

    numfiles = size(this%param_file%values)
    if (numfiles > 1) then
      write(*, *) 'Only one parameter filename is currently supported'
    end if

    ! Example for checking file suffix
    ! write(*, *) this%param_file%values(1)%s
    ppos = scan(trim(this%param_file%values(1)%s),".", BACK= .true.)
    fileext = this%param_file%values(1)%s(ppos+1:)
    if(this%param_file%values(1)%s(scan(trim(this%param_file%values(1)%s),".", BACK= .true.)+1:) == 'nc') then
    !if (fileext == 'nc') then
      ! write(*, *) 'Parameter netcdf file'
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
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine read_Control(this) !, this%control_filename)
    use iso_fortran_env
    use variableKind, only: cLen
    use m_errors, only: eMsg, fErr, IO_OPEN
    use m_strings, only: compact, isString, lowerCase, str
    implicit none

    class(Control), intent(inout) :: this

    integer(i32) :: istat = 0
      !! Contains the IOSTAT result from a read command
    integer(i32) :: iUnit = 0
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

    ! iUnit = 1
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
    open_model_output_file = -1

    if (allocated(this%model_output_file%values)) then
      open(newunit=iunit, file=this%model_output_file%values(1)%s, status='replace', iostat=istat)

      call fErr(istat, this%model_output_file%values(1)%s, IO_OPEN)

      open_model_output_file = iunit
    endif
  end function

  module subroutine cleanup_control(this)
    use netcdf
    implicit none

    class(Control) :: this

    logical :: is_opened

    inquire(UNIT=this%model_output_unit, OPENED=is_opened)
    if (is_opened) then
      close(this%model_output_unit)
    end if

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Restart files
    if (this%init_vars_from_file%value == 1) then
      call this%err_check(nf90_close(this%restart_in_hdl))
    end if

    if (this%save_vars_to_file%value == 1) then
      call this%err_check(nf90_close(this%restart_out_hdl))
    end if
  end subroutine

  module subroutine write_restart_var_logical_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    logical, intent(in) :: data(:)

    integer(i32) :: var_id
    integer(i32) :: start(1)
    integer(i32) :: rcount(1)

    integer :: on
    integer :: off
    integer, allocatable :: logical_out(:)

    ! ------------------------------------------------------------------------
    ! Write the variable data
    call this%err_check(nf90_inq_varid(this%restart_out_hdl, var_name, var_id))

    on = 1
    off = 0

    allocate(logical_out(size(data)))
    logical_out = merge(on, off, data)

    start = (/ 1 /)
    rcount = (/ size(data) /)

    call this%err_check(nf90_put_var(this%restart_out_hdl, var_id, logical_out, start=start, count=rcount))
    deallocate(logical_out)
  end subroutine

  module subroutine write_restart_var_i32_0d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    integer(i32), intent(in) :: data

    integer(i32) :: var_id
    integer(i32) :: start(1)

    ! ------------------------------------------------------------------------
    ! Write the variable data
    call this%err_check(nf90_inq_varid(this%restart_out_hdl, var_name, var_id))

    start = (/ 1 /)
    call this%err_check(nf90_put_var(this%restart_out_hdl, var_id, data, start=start))
  end subroutine

  module subroutine write_restart_var_i32_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    integer(i32), intent(in) :: data(:)

    integer(i32) :: var_id
    integer(i32) :: start(1)
    integer(i32) :: rcount(1)

    ! ------------------------------------------------------------------------
    ! Write the variable data
    call this%err_check(nf90_inq_varid(this%restart_out_hdl, var_name, var_id))

    start = (/ 1 /)
    rcount = (/ size(data) /)
    call this%err_check(nf90_put_var(this%restart_out_hdl, var_id, data, start=start, count=rcount))
  end subroutine

  module subroutine write_restart_var_r32_0d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    real(r32), intent(in) :: data

    integer(i32) :: var_id
    integer(i32) :: start(1)

    ! ------------------------------------------------------------------------
    ! Write the variable data
    call this%err_check(nf90_inq_varid(this%restart_out_hdl, var_name, var_id))

    start = (/ 1 /)
    call this%err_check(nf90_put_var(this%restart_out_hdl, var_id, data, start=start))
  end subroutine

  module subroutine write_restart_var_r32_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    real(r32), intent(in) :: data(:)

    integer(i32) :: var_id
    integer(i32) :: start(2)
    integer(i32) :: rcount(2)

    ! ------------------------------------------------------------------------
    ! Write the variable data
    call this%err_check(nf90_inq_varid(this%restart_out_hdl, var_name, var_id))

    start = (/ 1, 1 /)
    rcount = (/ size(data), 1 /)
    call this%err_check(nf90_put_var(this%restart_out_hdl, var_id, data, start=start, count=rcount))
  end subroutine

  module subroutine write_restart_var_r64_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    real(r64), intent(in) :: data(:)

    integer(i32) :: var_id
    integer(i32) :: start(2)
    integer(i32) :: rcount(2)

    ! ------------------------------------------------------------------------
    ! Write the variable data
    call this%err_check(nf90_inq_varid(this%restart_out_hdl, var_name, var_id))

    start = (/ 1, 1 /)
    rcount = (/ size(data), 1 /)
    call this%err_check(nf90_put_var(this%restart_out_hdl, var_id, data, start=start, count=rcount))
  end subroutine


  module subroutine add_dimension(this, dim_name, dim_size, dim_var_name, dim_var_longname, datatype, units)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: dim_name
    integer(i32), intent(in) :: dim_size
    character(len=*), intent(in) :: dim_var_name
    character(len=*), intent(in) :: dim_var_longname
    integer(i32), intent(in) :: datatype
    character(len=*), intent(in) :: units

    integer(i32) :: dimid
    integer(i32) :: varid

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%restart_out_hdl))

    call this%err_check(nf90_def_dim(this%restart_out_hdl, dim_name, dim_size, dimid))

    ! Always include nhm_id as a variable
    call this%err_check(nf90_def_var(this%restart_out_hdl, dim_var_name, datatype, dimid, varid))
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'long_name', dim_var_longname))
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'units', units))

    call this%err_check(nf90_enddef(this%restart_out_hdl))
  end subroutine

  module subroutine add_time_dimension(this, dim_name, dim_size, dim_var_name, units, calendar)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: dim_name
    integer(i32), intent(in) :: dim_size
    character(len=*), intent(in) :: dim_var_name
    character(len=*), intent(in) :: units
    character(len=*), intent(in) :: calendar

    integer(i32) :: dimid
    integer(i32) :: varid

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%restart_out_hdl))

    ! Define the time dimension. Restart files have a single timestep.
    call this%err_check(nf90_def_dim(this%restart_out_hdl, dim_name, dim_size, dimid))

    ! Define the variable for the time dimension
    call this%err_check(nf90_def_var(this%restart_out_hdl, dim_var_name, NF90_FLOAT, dimid, varid))
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'long_name', dim_var_name))
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'calendar', calendar))
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'units', units))

    call this%err_check(nf90_enddef(this%restart_out_hdl))
  end subroutine

  module subroutine add_variable_logical_1d(this, var_name, thevar, dim_name, units)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    logical, intent(in) :: thevar(:)
    character(len=*), intent(in) :: dim_name
    character(len=*), intent(in) :: units

    integer(i32) :: dimid
    integer(i32) :: dim_size
    integer(i32) :: dimids(2)
    integer(i32) :: dimid_time
    integer(i32) :: varid

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%restart_out_hdl))

    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, dim_name, dimid))
    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, 'time', dimid_time))
    dimids = (/ dimid, dimid_time /)

    ! Save the array size for this variable to an array for later
    call this%err_check(nf90_inquire_dimension(this%restart_out_hdl, dimid, len=dim_size))

    if (size(thevar) /= dim_size) then
      write(output_unit, *) 'ERROR: Size of variable, ', var_name, ' does not match dimension, ', dim_name
      stop
    end if

    ! Create the variable
    call this%err_check(nf90_def_var(this%restart_out_hdl, var_name, NF90_INT, dimids, varid))

    ! Add attributes for the variable
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'units', units))

    call this%err_check(nf90_enddef(this%restart_out_hdl))
  end subroutine

  module subroutine add_variable_i32_1d(this, var_name, thevar, dim_name, units)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    integer(i32), intent(in) :: thevar(:)
    character(len=*), intent(in) :: dim_name
    character(len=*), intent(in) :: units

    integer(i32) :: dimid
    integer(i32) :: dim_size
    integer(i32) :: dimids(2)
    integer(i32) :: dimid_time
    integer(i32) :: varid

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%restart_out_hdl))

    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, dim_name, dimid))
    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, 'time', dimid_time))
    dimids = (/ dimid, dimid_time /)

    ! Save the array size for this variable to an array for later
    call this%err_check(nf90_inquire_dimension(this%restart_out_hdl, dimid, len=dim_size))

    if (size(thevar) /= dim_size) then
      write(output_unit, *) 'ERROR: Size of variable, ', var_name, ' does not match dimension, ', dim_name
      stop
    end if

    ! Create the variable
    call this%err_check(nf90_def_var(this%restart_out_hdl, var_name, NF90_INT, dimids, varid))

    ! Add attributes for the variable
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'units', units))

    call this%err_check(nf90_enddef(this%restart_out_hdl))
  end subroutine

  module subroutine add_variable_r32_1d(this, var_name, thevar, dim_name, units)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    real(r32), intent(in) :: thevar(:)
    character(len=*), intent(in) :: dim_name
    character(len=*), intent(in) :: units

    integer(i32) :: dimid
    integer(i32) :: dim_size
    integer(i32) :: dimids(2)
    integer(i32) :: dimid_time
    integer(i32) :: varid

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%restart_out_hdl))

    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, dim_name, dimid))
    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, 'time', dimid_time))
    dimids = (/ dimid, dimid_time /)

    ! Save the array size for this variable to an array for later
    call this%err_check(nf90_inquire_dimension(this%restart_out_hdl, dimid, len=dim_size))

    if (size(thevar) /= dim_size) then
      write(output_unit, *) 'ERROR: Size of variable, ', var_name, ' does not match dimension, ', dim_name
      stop
    end if

    ! Create the variable
    call this%err_check(nf90_def_var(this%restart_out_hdl, var_name, NF90_FLOAT, dimids, varid))

    ! Add attributes for the variable
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'units', units))

    call this%err_check(nf90_enddef(this%restart_out_hdl))
  end subroutine

  module subroutine add_variable_r64_1d(this, var_name, thevar, dim_name, units)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    real(r64), intent(in) :: thevar(:)
    character(len=*), intent(in) :: dim_name
    character(len=*), intent(in) :: units

    integer(i32) :: dimid
    integer(i32) :: dim_size
    integer(i32) :: dimids(2)
    integer(i32) :: dimid_time
    integer(i32) :: varid

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%restart_out_hdl))

    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, dim_name, dimid))
    call this%err_check(nf90_inq_dimid(this%restart_out_hdl, 'time', dimid_time))
    dimids = (/ dimid, dimid_time /)

    ! Save the array size for this variable to an array for later
    call this%err_check(nf90_inquire_dimension(this%restart_out_hdl, dimid, len=dim_size))

    if (size(thevar) /= dim_size) then
      write(output_unit, *) 'ERROR: Size of variable, ', var_name, ' does not match dimension, ', dim_name
      stop
    end if

    ! Create the variable
    call this%err_check(nf90_def_var(this%restart_out_hdl, var_name, NF90_DOUBLE, dimids, varid))

    ! Add attributes for the variable
    call this%err_check(nf90_put_att(this%restart_out_hdl, varid, 'units', units))

    call this%err_check(nf90_enddef(this%restart_out_hdl))
  end subroutine

  module subroutine read_restart_var_logical_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    logical, pointer, intent(inout) :: data(:)

    ! Local variables
    integer(i32) :: ii
    integer(i32) :: var_id
    integer, allocatable :: src_int(:)

    ! ------------------------------------------------------------------------
    allocate(src_int(size(data)))

    call this%err_check(nf90_inq_varid(this%restart_in_hdl, var_name, var_id))
    call this%err_check(nf90_get_var(this%restart_in_hdl, var_id, src_int))

    do ii=1, size(src_int)
      if (src_int(ii) == 0) then
        data(ii) = .false.
      else
        data(ii) = .true.
      end if
    end do
  end subroutine

  module subroutine read_restart_var_i32_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    integer(i32), pointer, intent(inout) :: data(:)

    ! Local variables
    integer(i32) :: var_id

    call this%err_check(nf90_inq_varid(this%restart_in_hdl, var_name, var_id))
    call this%err_check(nf90_get_var(this%restart_in_hdl, var_id, data))
  end subroutine


  module subroutine read_restart_var_r32_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    real(r32), pointer, intent(inout) :: data(:)

    ! Local variables
    integer(i32) :: var_id

    call this%err_check(nf90_inq_varid(this%restart_in_hdl, var_name, var_id))
    call this%err_check(nf90_get_var(this%restart_in_hdl, var_id, data))
  end subroutine


  module subroutine read_restart_var_r64_1d(this, var_name, data)
    use netcdf
    implicit none

    class(Control), intent(in) :: this
    character(len=*), intent(in) :: var_name
    real(r64), pointer, intent(inout) :: data(:)

    ! Local variables
    integer(i32) :: var_id

    call this%err_check(nf90_inq_varid(this%restart_in_hdl, var_name, var_id))
    call this%err_check(nf90_get_var(this%restart_in_hdl, var_id, data))
  end subroutine


  module subroutine create_restart_netcdf(this)
    use netcdf
    use prms_constants, only: YEAR, MONTH, DAY
    implicit none

    class(Control), intent(inout) :: this

    ! Local variables
    character(len=:), allocatable :: filename

    filename = timestamped_filename('PRMS_RESTART_', this%end_time%values)
    write(*, *) 'Output restart filename: ', filename

    ! Create the netcdf restart file
    call this%err_check(nf90_create(filename, NF90_64BIT_OFFSET, this%restart_out_hdl))

    ! End define mode. This tells netCDF we are done defining metadata.
    call this%err_check(nf90_enddef(this%restart_out_hdl))
  end subroutine


  module subroutine open_restart_netcdf(this)
    use netcdf
    use prms_constants, only: YEAR, MONTH, DAY
    use UTILS_TIME, only: gregorian_to_julian, julian_to_gregorian
    implicit none

    class(Control), intent(inout) :: this

    ! Local variables
    integer(i32) :: jul_date
    integer(i32) :: adj_ts(6)
    character(len=:), allocatable :: filename

    jul_date = gregorian_to_julian(this%start_time%values(YEAR), this%start_time%values(MONTH), this%start_time%values(DAY))
    adj_ts = julian_to_gregorian(jul_date - 1)
    filename = timestamped_filename('PRMS_RESTART_', adj_ts)
    write(*, *) 'Input restart filename: ', filename

    ! Open netcdf file as read only
    call this%err_check(nf90_open(filename, NF90_NOWRITE, this%restart_in_hdl))
  end subroutine


  module function timestamped_filename(prefix, timestamp) result(res)
    use prms_constants, only: YEAR, MONTH, DAY
    implicit none

    character(len=:), allocatable :: res
    character(len=*), intent(in) :: prefix
      !! Prefix to use for the filename
    integer(i32), intent(in) :: timestamp(6)
      !! Array of timestamp values (YY, MM, DD, hh, mm, ss) to append to filename

    ! Local variables
    character(len=256) :: buf

    write(buf, 9001) prefix, timestamp(YEAR), timestamp(MONTH), timestamp(DAY)
    9001 format(A, I4, '-', I2.2, '-', I2.2, '.nc')

    res = trim(buf)
    return
  end function

  module subroutine err_check(status)
    integer(i32), intent(in) :: status
      !! The status returned by a netcdf call
  end subroutine

end submodule
