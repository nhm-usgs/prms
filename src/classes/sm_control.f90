submodule (Control_class) sm_control
    use variableKind, only: cLen
    use m_errors, only: eMsg, fErr
    use m_strings, only: compact, isString, lowerCase, str
    implicit none

contains

    !====================================================================!
    module procedure constructor_Control!(control_filename) result(this)
        !type(Control) :: this
        !character(len=*), intent(in) :: control_filename

        this%control_filename = control_filename

        call this%read(control_filename)
    end procedure
    !====================================================================!
    !====================================================================!
    module procedure read_Control!(this, fName)
        ! class(Control), intent(inout) :: this
        ! character(len=*) :: fName

        integer(i32) :: istat, iUnit
        integer(i32) :: line
        character(len = cLen) :: buf
        character(len=:), allocatable :: last
        logical :: go

        go = .true.

        iUnit = 1
        !call openFile(fName, iUnit, 'old', istat)
        open(unit=iUnit, file=fName, status='old', iostat=istat)
        call fErr(istat, fName, 1)

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

                select case(lowercase(buf))
                    case('cbh_binary_flag')
                        last = buf
                        call this%cbh_binary_flag%read(iUnit, fName)
                        line = line + this%cbh_binary_flag%size() + 2
                        call this%cbh_binary_flag%print()

                    case('cbh_check_flag')
                        last = buf
                        call this%cbh_check_flag%read(iUnit, fName)
                        line = line + this%cbh_check_flag%size() + 2
                        call this%cbh_check_flag%print()

                    case('data_file')
                        last = buf
                        call this%data_file%read(iUnit, fName)
                        line = line + this%data_file%size() + 2
                        call this%data_file%print()
                    
                    case('end_time')
                        last = buf
                        call this%end_time%read(iUnit, fName)
                        line = line + this%end_time%size() + 2
                        call this%end_time%print()

                    case('et_module')
                        last = buf
                        call this%et_module%read(iUnit, fName)
                        line = line + this%et_module%size() + 2
                        call this%et_module%print()

                    case('model_mode')
                        last = buf
                        call this%model_mode%read(iUnit, fName)
                        line = line + this%model_mode%size() + 2
                        call this%model_mode%print()

                    case('model_output_file')
                        last = buf
                        call this%model_output_file%read(iUnit, fName)
                        line = line + this%model_output_file%size() + 2
                        call this%model_output_file%print()

                    case('parameter_check_flag')
                        last = buf
                        call this%parameter_check_flag%read(iUnit, fName)
                        line = line + this%parameter_check_flag%size() + 2
                        call this%parameter_check_flag%print()

                    case('param_file')
                        last = buf
                        call this%param_file%read(iUnit, fName)
                        line = line + this%param_file%size() + 2
                        call this%param_file%print()
                    
                    case('print_debug')
                        last = buf
                        call this%print_debug%read(iUnit, fName)
                        line = line + this%print_debug%size() + 2
                        call this%print_debug%print()

                    case('prms_warmup')
                        last = buf
                        call this%prms_warmup%read(iUnit, fName)
                        line = line + this%prms_warmup%size() + 2
                        call this%prms_warmup%print()

                    case('start_time')
                        last = buf
                        call this%start_time%read(iUnit, fName)
                        line = line + this%start_time%size() + 2
                        call this%start_time%print()

                    

                case default

                last = buf
                ! Skip to the next ####

                end select
            else
                call eMsg("Could not read from file "//trim(fName)//" for entry "//str(last)//" at line "//str(line))
            endif

            read(iUnit, 1) buf
            call compact(buf)
            line = line + 1
        enddo

        call closeFile(fName, iUnit, '', istat)
1 format(a)
    end procedure

end submodule
