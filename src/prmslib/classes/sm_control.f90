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
    use m_errors, only: eMsg, fErr
    use m_strings, only: compact, isString, lowerCase, str
    implicit none

    class(Control), intent(inout) :: this
    ! character(len=*), intent(in) :: this%control_filename

    integer(i32) :: istat, iUnit
    integer(i32) :: line
    character(len=cLen) :: buf
    character(len=:), allocatable :: last
    logical :: go

    go = .true.

    iUnit = 1
    !call openFile(this%control_filename, iUnit, 'old', istat)
    open(unit=iUnit, file=this%control_filename, status='old', iostat=istat)
    call fErr(istat, this%control_filename, 1)

    ! Read the Header line
    read(iUnit, 1) buf
    line = 1
    last = 'Header'

    read(iUnit, 1) buf
    call compact(buf)
    line = line + 1

    do while (go)
      if (isString(buf(1:4), '####', .true.)) then
        read(iUnit, 1) buf
        call compact(buf)
        line = line + 1

        ! select case(lowercase(buf))
        select case(buf)
          case('cbh_binary_flag')
            last = buf
            call this%cbh_binary_flag%read(iUnit, this%control_filename)
            line = line + this%cbh_binary_flag%size() + 2
            call this%cbh_binary_flag%print()

          case('cbh_check_flag')
            last = buf
            call this%cbh_check_flag%read(iUnit, this%control_filename)
            line = line + this%cbh_check_flag%size() + 2
            call this%cbh_check_flag%print()

          case('print_debug')
            last = buf
            call this%print_debug%read(iUnit, this%control_filename)
            line = line + this%print_debug%size() + 2
            call this%print_debug%print()

          case('parameter_check_flag')
            last = buf
            call this%parameter_check_flag%read(iUnit, this%control_filename)
            line = line + this%parameter_check_flag%size() + 2
            call this%parameter_check_flag%print()

          case('data_file')
            last = buf
            call this%data_file%read(iUnit, this%control_filename)
            line = line + this%data_file%size() + 2
            call this%data_file%print()

          case('model_mode')
            last = buf
            call this%model_mode%read(iUnit, this%control_filename)
            line = line + this%model_mode%size() + 2
            call this%model_mode%print()

          case('model_output_file')
            last = buf
            call this%model_output_file%read(iUnit, this%control_filename)
            line = line + this%model_output_file%size() + 2
            call this%model_output_file%print()

          case('param_file')
            last = buf
            call this%param_file%read(iUnit, this%control_filename)
            line = line + this%param_file%size() + 2
            call this%param_file%print()

          case('save_vars_to_file')
            last = buf
            call this%save_vars_to_file%read(iUnit, this%control_filename)
            line = line + this%save_vars_to_file%size() + 2
            call this%save_vars_to_file%print()

          case('init_vars_from_file')
            last = buf
            call this%init_vars_from_file%read(iUnit, this%control_filename)
            line = line + this%init_vars_from_file%size() + 2
            call this%init_vars_from_file%print()

          case('prms_warmup')
            last = buf
            call this%prms_warmup%read(iUnit, this%control_filename)
            line = line + this%prms_warmup%size() + 2
            call this%prms_warmup%print()

          case('start_time')
            last = buf
            call this%start_time%read(iUnit, this%control_filename)
            line = line + this%start_time%size() + 2
            call this%start_time%print()

          case('end_time')
            last = buf
            call this%end_time%read(iUnit, this%control_filename)
            line = line + this%end_time%size() + 2
            call this%end_time%print()

          case('precip_module')
            last = buf
            call this%precip_module%read(iUnit, this%control_filename)
            line = line + this%precip_module%size() + 2
            call this%precip_module%print()

          case('temp_module')
            last = buf
            call this%temp_module%read(iUnit, this%control_filename)
            line = line + this%temp_module%size() + 2
            call this%temp_module%print()

          case('solrad_module')
            last = buf
            call this%solrad_module%read(iUnit, this%control_filename)
            line = line + this%solrad_module%size() + 2
            call this%solrad_module%print()

          case('et_module')
            last = buf
            call this%et_module%read(iUnit, this%control_filename)
            line = line + this%et_module%size() + 2
            call this%et_module%print()

          case('transp_module')
            last = buf
            call this%transp_module%read(iUnit, this%control_filename)
            line = line + this%transp_module%size() + 2
            call this%transp_module%print()

          case('var_save_file')
            last = buf
            call this%var_save_file%read(iUnit, this%control_filename)
            line = line + this%var_save_file%size() + 2
            call this%var_save_file%print()

          case('var_init_file')
            last = buf
            call this%var_init_file%read(iUnit, this%control_filename)
            line = line + this%var_init_file%size() + 2
            call this%var_init_file%print()

          case('tmax_day')
            last = buf
            call this%tmax_day%read(iUnit, this%control_filename)
            line = line + this%tmax_day%size() + 2
            call this%tmax_day%print()

          case('tmin_day')
            last = buf
            call this%tmin_day%read(iUnit, this%control_filename)
            line = line + this%tmin_day%size() + 2
            call this%tmin_day%print()

          case('precip_day')
            last = buf
            call this%precip_day%read(iUnit, this%control_filename)
            line = line + this%precip_day%size() + 2
            call this%precip_day%print()

          case('basinOutON_OFF')
            last = buf
            call this%basinOutON_OFF%read(iUnit, this%control_filename)
            line = line + this%basinOutON_OFF%size() + 2
            call this%basinOutON_OFF%print()

          case('basinOutVars')
            last = buf
            call this%basinOutVars%read(iUnit, this%control_filename)
            line = line + this%basinOutVars%size() + 2
            call this%basinOutVars%print()

          case('basinOut_freq')
            last = buf
            call this%basinOut_freq%read(iUnit, this%control_filename)
            line = line + this%basinOut_freq%size() + 2
            call this%basinOut_freq%print()

          case('basinOutVar_names')
            last = buf
            call this%basinOutVar_names%read(iUnit, this%control_filename)
            line = line + this%basinOutVar_names%size() + 2
            call this%basinOutVar_names%print()

          case('basinOutBaseFileName')
            last = buf
            call this%basinOutBaseFileName%read(iUnit, this%control_filename)
            line = line + this%basinOutBaseFileName%size() + 2
            call this%basinOutBaseFileName%print()

          case('nhruOutON_OFF')
            last = buf
            call this%nhruOutON_OFF%read(iUnit, this%control_filename)
            line = line + this%nhruOutON_OFF%size() + 2
            call this%nhruOutON_OFF%print()

          case('nhruOutVars')
            last = buf
            call this%nhruOutVars%read(iUnit, this%control_filename)
            line = line + this%nhruOutVars%size() + 2
            call this%nhruOutVars%print()

          case('nhruOut_freq')
            last = buf
            call this%nhruOut_freq%read(iUnit, this%control_filename)
            line = line + this%nhruOut_freq%size() + 2
            call this%nhruOut_freq%print()

          case('nhruOutVar_names')
            last = buf
            call this%nhruOutVar_names%read(iUnit, this%control_filename)
            line = line + this%nhruOutVar_names%size() + 2
            call this%nhruOutVar_names%print()

          case('nhruOutBaseFileName')
            last = buf
            call this%nhruOutBaseFileName%read(iUnit, this%control_filename)
            line = line + this%nhruOutBaseFileName%size() + 2
            call this%nhruOutBaseFileName%print()

          case default
            last = buf
            ! Skip to the next ####
        end select
      else
        call eMsg("Could not read from file "//trim(this%control_filename)//" for entry "//str(last)//" at line "//str(line))
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
