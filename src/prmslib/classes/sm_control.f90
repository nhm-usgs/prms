submodule (Control_class) sm_control

contains

  !====================================================================!
  module function constructor_Control(control_filename) result(this)
    implicit none

    type(Control) :: this
    character(len=*), intent(in) :: control_filename

    this%control_filename = control_filename

    call this%read()
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

        select case(buf)
          case('cbh_binary_flag')
            call this%cbh_binary_flag%read(iUnit)
            line = line + this%cbh_binary_flag%size() + 2
            call this%cbh_binary_flag%print()

          case('cbh_check_flag')
            call this%cbh_check_flag%read(iUnit)
            line = line + this%cbh_check_flag%size() + 2
            call this%cbh_check_flag%print()

          case('print_debug')
            call this%print_debug%read(iUnit)
            line = line + this%print_debug%size() + 2
            call this%print_debug%print()

          case('parameter_check_flag')
            call this%parameter_check_flag%read(iUnit)
            line = line + this%parameter_check_flag%size() + 2
            call this%parameter_check_flag%print()

          case('data_file')
            call this%data_file%read(iUnit)
            line = line + this%data_file%size() + 2
            call this%data_file%print()

          case('model_mode')
            call this%model_mode%read(iUnit)
            line = line + this%model_mode%size() + 2
            call this%model_mode%print()

          case('model_output_file')
            call this%model_output_file%read(iUnit)
            line = line + this%model_output_file%size() + 2
            call this%model_output_file%print()

          case('param_file')
            call this%param_file%read(iUnit)
            line = line + this%param_file%size() + 2
            call this%param_file%print()

          case('save_vars_to_file')
            call this%save_vars_to_file%read(iUnit)
            line = line + this%save_vars_to_file%size() + 2
            call this%save_vars_to_file%print()

          case('init_vars_from_file')
            call this%init_vars_from_file%read(iUnit)
            line = line + this%init_vars_from_file%size() + 2
            call this%init_vars_from_file%print()

          case('prms_warmup')
            call this%prms_warmup%read(iUnit)
            line = line + this%prms_warmup%size() + 2
            call this%prms_warmup%print()

          case('start_time')
            call this%start_time%read(iUnit)
            line = line + this%start_time%size() + 2
            call this%start_time%print()

          case('end_time')
            call this%end_time%read(iUnit)
            line = line + this%end_time%size() + 2
            call this%end_time%print()

          case('precip_module')
            call this%precip_module%read(iUnit)
            line = line + this%precip_module%size() + 2
            call this%precip_module%print()

          case('temp_module')
            call this%temp_module%read(iUnit)
            line = line + this%temp_module%size() + 2
            call this%temp_module%print()

          case('solrad_module')
            call this%solrad_module%read(iUnit)
            line = line + this%solrad_module%size() + 2
            call this%solrad_module%print()

          case('et_module')
            call this%et_module%read(iUnit)
            line = line + this%et_module%size() + 2
            call this%et_module%print()

          case('transp_module')
            call this%transp_module%read(iUnit)
            line = line + this%transp_module%size() + 2
            call this%transp_module%print()

          case('var_save_file')
            call this%var_save_file%read(iUnit)
            line = line + this%var_save_file%size() + 2
            call this%var_save_file%print()

          case('var_init_file')
            call this%var_init_file%read(iUnit)
            line = line + this%var_init_file%size() + 2
            call this%var_init_file%print()

          case('tmax_day')
            call this%tmax_day%read(iUnit)
            line = line + this%tmax_day%size() + 2
            call this%tmax_day%print()

          case('tmin_day')
            call this%tmin_day%read(iUnit)
            line = line + this%tmin_day%size() + 2
            call this%tmin_day%print()

          case('precip_day')
            call this%precip_day%read(iUnit)
            line = line + this%precip_day%size() + 2
            call this%precip_day%print()

          case('basinOutON_OFF')
            call this%basinOutON_OFF%read(iUnit)
            line = line + this%basinOutON_OFF%size() + 2
            call this%basinOutON_OFF%print()

          case('basinOutVars')
            call this%basinOutVars%read(iUnit)
            line = line + this%basinOutVars%size() + 2
            call this%basinOutVars%print()

          case('basinOut_freq')
            call this%basinOut_freq%read(iUnit)
            line = line + this%basinOut_freq%size() + 2
            call this%basinOut_freq%print()

          case('basinOutVar_names')
            call this%basinOutVar_names%read(iUnit)
            line = line + this%basinOutVar_names%size() + 2
            call this%basinOutVar_names%print()

          case('basinOutBaseFileName')
            call this%basinOutBaseFileName%read(iUnit)
            line = line + this%basinOutBaseFileName%size() + 2
            call this%basinOutBaseFileName%print()

          case('nhruOutON_OFF')
            call this%nhruOutON_OFF%read(iUnit)
            line = line + this%nhruOutON_OFF%size() + 2
            call this%nhruOutON_OFF%print()

          case('nhruOutVars')
            call this%nhruOutVars%read(iUnit)
            line = line + this%nhruOutVars%size() + 2
            call this%nhruOutVars%print()

          case('nhruOut_freq')
            call this%nhruOut_freq%read(iUnit)
            line = line + this%nhruOut_freq%size() + 2
            call this%nhruOut_freq%print()

          case('nhruOutVar_names')
            call this%nhruOutVar_names%read(iUnit)
            line = line + this%nhruOutVar_names%size() + 2
            call this%nhruOutVar_names%print()

          case('nhruOutBaseFileName')
            call this%nhruOutBaseFileName%read(iUnit)
            line = line + this%nhruOutBaseFileName%size() + 2
            call this%nhruOutBaseFileName%print()

          case default
            ! Skip to the next ####
        end select
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

end submodule
