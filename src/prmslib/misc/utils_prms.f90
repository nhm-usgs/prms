! utils_prms.f90 2017-09-22 14:37:00Z

module UTILS_PRMS
    use kinds_mod, only: r4, r8, i4, i8
    implicit none

    contains

        !***********************************************************************
        !     Check parameter value against dimension
        !***********************************************************************
        subroutine checkdim_param_limits(Indx, Param, Dimen, Param_value, Lower_val, Upper_val, Iret)
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Indx
            character(len=*), intent(in) :: Param
            character(len=*), intent(in) :: Dimen
            integer(i4), intent(in) :: Param_value
            integer(i4), intent(in) :: Lower_val
            integer(i4), intent(in) :: Upper_val
            integer(i4), intent(inout) :: Iret

            !***********************************************************************
            if (Param_value < Lower_val .OR. Param_value > Upper_val) then
                print *, 'ERROR, out-of-bounds value for bounded parameter: ', Param
                print *, '       value:  ', Param_value, '; array index:', Indx
                print *, '       minimum:', Lower_val, '; maximum is dimension ', Dimen, ' =', Upper_val
                print *, ' '
                Iret = 1
            endif
        end subroutine checkdim_param_limits

        !***********************************************************************
        !     Check parameter value limits
        !***********************************************************************
        subroutine check_param_limits(Indx, Param, Param_value, Lower_val, Upper_val, Iret)
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Indx
            character(len=*), intent(in) :: Param
            real(r4), intent(in) :: Param_value
            real(r4), intent(in) :: Lower_val
            real(r4), intent(in) :: Upper_val
            integer(i4), intent(inout) :: Iret

            !***********************************************************************
            if (Param_value < Lower_val .OR. Param_value > Upper_val) then
                print *, 'ERROR, bad value, parameter: ', Param
                print *, '       value:  ', Param_value, '; array index:', Indx
                print *, '       minimum:', Lower_val, '  ; maximum:', Upper_val
                print *, ' '
                Iret = 1
            endif
        end subroutine check_param_limits

        !     Check parameter value < 0.0
        !***********************************************************************
        subroutine check_param_zero(Indx, Param, Param_value, Iret)
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Indx
            character(len=*), intent(in) :: Param
            real(r4), intent(in) :: Param_value
            integer(i4), intent(inout) :: Iret

            !***********************************************************************
            if (Param_value < 0.0) then
                print *, 'ERROR, value < 0.0 for parameter: ', Param
                print *, '       value:', Param_value, '; HRU:', Indx
                print *, ' '
                Iret = 1
            endif
        end subroutine check_param_zero

        !***********************************************************************
        ! check restart file module order
        !***********************************************************************
        subroutine check_restart(Modname, Restart_module)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname
            character(len=*), intent(in) :: Restart_module

            !***********************************************************************
            if (Restart_module /= Modname) then
                print *, 'ERROR READING RESTART FILE, expecting module: ', Modname, ' found: ', Restart_module
                STOP
            endif
        end subroutine check_restart

        !***********************************************************************
        ! check restart file dimensions order
        !***********************************************************************
        subroutine check_restart_dimen(Dimen, Oldval, Newval, ierr)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Dimen
            integer(i4), intent(in) :: Oldval
            integer(i4), intent(in) :: Newval
            integer(i4), intent(inout) :: ierr

            !***********************************************************************
            if (Oldval /= Newval) then
                print *, 'ERROR READING RESTART FILE, for dimension ', Dimen
                print *, '      restart value=', Oldval, ' new value=', Newval
                ierr = 1
            endif
        end subroutine check_restart_dimen

        !***********************************************************************
        !     Read File dynamic parameter file to current time
        !***********************************************************************
        subroutine find_current_file_time(Iunit, Year, Month, Day, Year_file, Month_file, Day_file)
            implicit none

            ! Argument
            integer(i4), intent(in) :: Iunit
            integer(i4), intent(in) :: Year
            integer(i4), intent(in) :: Month
            integer(i4), intent(in) :: Day
            integer(i4), intent(out) :: Year_file
            integer(i4), intent(out) :: Month_file
            integer(i4), intent(out) :: Day_file

            ! Local Variables
            integer(i4) :: i, ios

            !***********************************************************************
            ! find first value for simulation time period
            read (Iunit, *, IOSTAT = ios) Year_file, Month_file, Day_file
            if (ios /= 0) then
                Year_file = 0
                Month_file = 0
                Day_file = 0
                return
            endif

            if (Year_file < Year) then
                i = 0
                do while (i == 0)
                    read (Iunit, *, IOSTAT = ios) Year_file, Month_file, Day_file
                    if (ios /= 0) then
                        Year_file = 0
                        Month_file = 0
                        Day_file = 0
                        return
                    endif
                    if (Year_file >= Year) i = 1
                enddo
            endif

            if (Year_file == Year) then
                if (Month_file < Month) then
                    i = 0
                    do while (i == 0)
                        read (Iunit, *, IOSTAT = ios) Year_file, Month_file, Day_file
                        if (ios /= 0) then
                            Year_file = 0
                            Month_file = 0
                            Day_file = 0
                            return
                        endif
                        if (Month_file >= Month .OR. Year_file /= Year) i = 1
                    enddo
                endif

                if (Year_file == Year .AND. Month_file == Month) then
                    if (Day_file < Day) then
                        i = 0
                        do while (i == 0)
                            read (Iunit, *, IOSTAT = ios) Year_file, Month_file, Day_file
                            if (ios /= 0) then
                                Year_file = 0
                                Month_file = 0
                                Day_file = 0
                                return
                            endif
                            if (Day_file >= Day) i = 1
                        enddo
                    endif
                endif
            endif
            backspace Iunit
        end subroutine find_current_file_time

        !***********************************************************************
        !     Read CBH File to current time
        !***********************************************************************
        subroutine find_current_time(Iunit, Year, Month, Day, Iret, Cbh_binary_flag)
            implicit none

            ! Argument
            integer(i4), intent(in) :: Iunit
            integer(i4), intent(in) :: Year
            integer(i4), intent(in) :: Month
            integer(i4), intent(in) :: Day
            integer(i4), intent(out) :: Iret
            integer(i4), intent(in) :: Cbh_binary_flag

            ! Local Variables
            integer(i4) :: yr, mo, dy

            !***********************************************************************
            Iret = 0
            do
                if (Cbh_binary_flag == 0) then
                    read (Iunit, *, IOSTAT = Iret) yr, mo, dy
                else
                    read (Iunit, IOSTAT = Iret) yr, mo, dy
                endif
                if (Iret == -1) print *, 'ERROR, end-of-file found reading input file for date:', Year, Month, Day
                if (Iret /= 0) return
                if (yr == Year .AND. mo == Month .AND. dy == Day) EXIT
            enddo
            backspace Iunit
        end subroutine find_current_time

        !**********************
        ! Check for end of file
        !**********************
        subroutine is_eof(Iunit, Next_yr, Next_mo, Next_day)
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Iunit
            integer(i4), intent(out) :: Next_yr
            integer(i4), intent(out) :: Next_mo
            integer(i4), intent(out) :: Next_day

            ! Local Variables
            integer(i4) :: ios, i
            character(len=80) :: dum

            !*******************************************************************************
            Next_yr = 0
            Next_mo = 0
            Next_day = 0
            i = 0

            do while (i == 0)
                read (Iunit, '(A)', iostat = ios) dum
                if (ios /= 0) return
                if (dum(:2) /= '//') i = 1
            enddo

            read (dum, *, iostat = ios) Next_yr, Next_mo, Next_day

            if (ios /= 0) then
                Next_yr = 0
                Next_mo = 0
                Next_day = 0
            else
                backspace Iunit
            endif
        end subroutine is_eof

        !**********************************************************************
        !     Module error
        !**********************************************************************
        subroutine module_error(Modname, Arg, Retcode)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname
            character(len=*), intent(in) :: Arg
            integer(i4), intent(in) :: Retcode

            !**********************************************************************
            print 9001, Modname, Arg, Retcode
            STOP
            9001 format ('ERROR in ', A, ' module, arg = ', A, /, 'Return val =', I4)
        end subroutine module_error

        !***********************************************************************
        !     Open PRMS input File and assign unit number
        !***********************************************************************
        subroutine PRMS_open_input_file(Iunit, Fname, Paramname, Ftype, Iret)
            implicit none

            ! Argument
            integer(i4), intent(out) :: Iunit
            character(len=*), intent(in) :: Fname
            character(len=*), intent(in) :: Paramname
            integer(i4), intent(in) :: Ftype
            integer(i4), intent(out) :: Iret

            ! Local Variables
            integer(i4) :: ios  ! , nchars

            !***********************************************************************
            Iret = 0
            Iunit = get_ftnunit(777)
            ! nchars = numchars(Fname)
            if (Ftype == 0) then
                open (unit=Iunit, FILE=Fname, STATUS='OLD', IOSTAT=ios)
                ! open (Iunit, FILE=Fname(:nchars), STATUS='OLD', IOSTAT=ios)
            else
                open (unit=Iunit, FILE=Fname, STATUS='OLD', FORM='UNFORMATTED', IOSTAT=ios) ! for linux
                ! open (Iunit, FILE=Fname(:nchars), STATUS='OLD', FORM='BINARY', IOSTAT=ios) ! for windows
            endif

            if (ios /= 0) then
                write (*, '(/,2A,/,A,/,2A,/)') 'ERROR opening input file: ', Fname, &
                        & 'check that input file exists', &
                        & 'file specified by control parameter: ', Paramname
                Iret = 1
            endif
        end subroutine PRMS_open_input_file

        !***********************************************************************
        !     Open PRMS module output file and assign unit number
        !***********************************************************************
        subroutine PRMS_open_module_file(Iunit, Fname)
            implicit none

            ! Argument
            integer(i4), intent(out) :: Iunit
            character(len=*), intent(in) :: Fname

            ! Local Variables
            integer(i4) :: ios  ! , nchars

            !***********************************************************************
            ! Iunit = get_ftnunit(888)
            ! nchars = numchars(Fname)
            open (newunit=Iunit, FILE = Fname, STATUS = 'REPLACE', IOSTAT = ios)
            if (ios /= 0) then
                write (*, '(/,A,/,A,/)') 'ERROR opening water balance output file:', Fname, &
                        &                             'check to be sure the pathname is valid and the file is not open'
                STOP
            endif
        end subroutine PRMS_open_module_file

        !***********************************************************************
        !     Open PRMS output file and assign unit number
        !***********************************************************************
        subroutine PRMS_open_output_file(Iunit, Fname, Paramname, Ftype, Iret)
            implicit none

            ! Argument
            integer(i4), intent(out) :: Iunit
            character(len=*), intent(in) :: Fname
            character(len=*), intent(in) :: Paramname
            integer(i4), intent(in) :: Ftype ! 0=text; 1=BINARY
            integer(i4), intent(out) :: Iret


            ! Local Variables
            integer(i4) :: ios

            !***********************************************************************
            Iret = 0
            ! Iunit = get_ftnunit(888)

            if (Ftype == 0) then
                open (newunit=Iunit, FILE=Fname, STATUS='REPLACE', IOSTAT=ios)
            else
                open (newunit=Iunit, FILE=Fname, STATUS='REPLACE', IOSTAT=ios, FORM='UNFORMATTED' ) ! for linux
                ! open (Iunit, FILE = Fname(:nchars), STATUS = 'REPLACE', IOSTAT = ios, FORM = 'BINARY') ! for windows
            endif

            if (ios /= 0) then
                write (*, '(/,A,/,A,/)') 'ERROR opening output file:', Fname, &
                        &                             'check to be sure the pathname is valid and the file is not open'
                write (*, '(2A,/)') 'file specified by control parameter: ', Paramname
                Iret = 1
            endif
        end subroutine PRMS_open_output_file

        !**********************************************************************
        !     Parameter or Variable delcare or read error
        !**********************************************************************
        subroutine read_error(Iflag, Name)
            !! TODO: write a generic error function
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Iflag
            character(len=*), intent(in) :: Name

            !**********************************************************************
            print '(/,A,/)', 'Due to error condition simulation halted'
            if (Iflag == 1) then
                print *, 'Declare error for parameter: ', Name
            elseif (Iflag == 2) then
                print *, 'Get error for parameter: ', Name
            elseif (Iflag == 3) then
                print *, 'Declare error for variable: ', Name
            elseif (Iflag == 4) then
                print *, 'Get error for variable: ', Name
            elseif (Iflag == 5) then
                print *, 'Read error for control parameter: ', Name
            elseif (Iflag == 6) then
                print *, 'Read error for dimension parameter: ', Name
            elseif (Iflag == 7) then
                print *, 'Declare error for dimension parameter: ', Name
            elseif (Iflag == 8) then
                print *, 'Declare error for Data File variable: ', Name
            elseif (Iflag == 9) then
                print *, 'Read error for Data File variable: ', Name
            elseif (Iflag == 10) then
                print *, 'Open error of Control File ', Name
            elseif (Iflag == 11) then
                print *, 'Read error of Parameter File ', Name
            elseif (Iflag == 12) then
                print *, 'Read error of Control File ', Name
            elseif (Iflag == 13) then
                print *, 'Read error of Data File ', Name
            elseif (Iflag == 14) then
                print *, 'Control parameter not found: ', Name
            elseif (Iflag == 15) then
                print *, 'ERROR, control ', Name, ' expected and is not available in PRMS'
            ! elseif (Iflag == 16) then
            !     print *, 'ERROR, declared parameter ', Name
            endif
            STOP
        end subroutine read_error

        !**********************************************************************
        !     Version Check
        !**********************************************************************
        subroutine version_check(Module_version, Length, Param_version)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Module_version
            integer(i4), intent(in) :: Length
            character(len=*), intent(in) :: Param_version

            !**********************************************************************
            if (Module_version(13:Length + 12) /= Param_version(:Length)) then
                print 9001, Module_version(13:Length + 12), Param_version(:Length)
                print *, 'Enter return to continue'
                read (*, *)
            endif
            9001 format ('WARNING, module versions are not identical', /, &
                    &        'Executable version: ', A, /, &
                    &        'Parameter File version: ', A, /)
        end subroutine version_check

        !***********************************************************************
        subroutine write_double_param(Iunit, Parm_name, Dimen_name, Dimen, Values)
            !***********************************************************************
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Iunit
            character(len=*), intent(in) :: Parm_name
            character(len=*), intent(in) :: Dimen_name
            integer(i4), intent(in) :: Dimen
            real(r8), intent(in) :: Values(Dimen)

            ! Local Variables
            integer(i4) :: i
            character(len=40), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, "3")'

            !***********************************************************************
            write (Iunit, fmt1) Parm_name, Dimen_name, Dimen
            do i = 1, Dimen
                write (Iunit, *) Values(i)
            enddo
        end subroutine write_double_param

        !***********************************************************************
        subroutine write_integer_param(Iunit, Parm_name, Dimen_name, Dimen, Values)
            !***********************************************************************
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Iunit
            character(len=*), intent(in) :: Parm_name
            character(len=*), intent(in) :: Dimen_name
            integer(i4), intent(in) :: Dimen
            integer(i4), intent(in) :: Values(Dimen)

            ! Local Variables
            integer(i4) :: i
            character(len=48), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, /, "1")'

            !***********************************************************************
            write (Iunit, fmt1) Parm_name, Dimen_name, Dimen
            do i = 1, Dimen
                write (Iunit, *) Values(i)
            enddo
        end subroutine write_integer_param

        !***********************************************************************
        subroutine write_real_param(Iunit, Parm_name, Dimen_name, Dimen, Values)
            !***********************************************************************
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Iunit
            character(len=*), intent(in) :: Parm_name
            character(len=*), intent(in) :: Dimen_name
            integer(i4), intent(in) :: Dimen
            real(r4), intent(in) :: Values(Dimen)

            ! Local Variables
            integer(i4) :: i
            character(len=48), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, /, "2")'

            !***********************************************************************
            write (Iunit, fmt1) Parm_name, Dimen_name, Dimen
            do i = 1, Dimen
                write (Iunit, *) Values(i)
            enddo
        end subroutine write_real_param

        !***********************************************************************
        subroutine write_2D_double_array_grid(Iunit, Parm_name, Dimen_name1, Dimen1, Dimen_name2, Dimen2, Values)
            !***********************************************************************
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Iunit
            character(len=*), intent(in) :: Parm_name
            character(len=*), intent(in) :: Dimen_name1
            integer(i4), intent(in) :: Dimen1
            character(len=*), intent(in) :: Dimen_name2
            integer(i4), intent(in) :: Dimen2
            real(r8), intent(in) :: Values(Dimen1, Dimen2)

            ! Local Variables
            integer(i4) :: i, j
            character(len=12) :: fmt

            !***********************************************************************
            write (Iunit, 9001) Parm_name, Dimen_name1, Dimen_name2, Dimen1 * Dimen2
            write (fmt, 9002) Dimen2
            do i = 1, Dimen2
                write (Iunit, fmt) (Values(j, i), j = 1, Dimen1)
            enddo

            9001 format ('####', /, A, /, '2', /, A, /, A, /, I8, /, '3')
            9002 format ('(', I5, 'F10.5)')
        end subroutine write_2D_double_array_grid

        !***********************************************************************
        subroutine write_2D_double_param(Iunit, Parm_name, Dimen_name1, Dimen1, Dimen_name2, Dimen2, Values)
            !***********************************************************************
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Iunit
            character(len=*), intent(in) :: Parm_name
            character(len=*), intent(in) :: Dimen_name1
            integer(i4), intent(in) :: Dimen1
            character(len=*), intent(in) :: Dimen_name2
            integer(i4), intent(in) :: Dimen2
            real(r8), intent(in) :: Values(Dimen1, Dimen2)

            ! Local Variables
            integer(i4) :: i, j
            character(len=46), PARAMETER :: fmt1 = '("####", /, A, /, "2", /, A, /, A, /, I8, "3")'

            !***********************************************************************
            write (Iunit, fmt1) Parm_name, Dimen_name1, Dimen_name2, Dimen1 * Dimen2
            do i = 1, Dimen2
                do j = 1, Dimen1
                    write (Iunit, *) Values(j, i)
                enddo
            enddo
        end subroutine write_2D_double_param

        !***********************************************************************
        ! Set data type flag
        !***********************************************************************
        subroutine set_data_type(Data_type, Type_flag)
            use kinds_mod, only: i4
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Data_type
            integer(i4), intent(out) :: Type_flag

            if (trim(Data_type) == 'integer') then
                Type_flag = 1
            elseif (trim(Data_type) == 'real') then
                Type_flag = 2
            elseif (trim(Data_type) == 'double') then
                Type_flag = 3
            elseif (trim(Data_type) == 'string') then
                Type_flag = 4
            else
                print *, 'ERROR, invalid data type: ', Data_type
                print *, '       valid values are real, double, string, integer'
                STOP
            endif

        end subroutine set_data_type

        ! ***********************************************************************
        !     Determine an unopened FORTRAN File Unit
        ! ***********************************************************************
        integer function get_ftnunit(Iunit)
           implicit none

           ! Argument
           integer, intent(in) :: Iunit

           ! Local Variables
           integer :: good_unit
           LOGICAL :: opend

           !***********************************************************************
           good_unit = Iunit
           opend = .TRUE.
           do while (opend)
               good_unit = good_unit + 1
               INQUIRE (UNIT = good_unit, OPENED = opend)
           enddo
           get_ftnunit = good_unit
       end function get_ftnunit
end module UTILS_PRMS
