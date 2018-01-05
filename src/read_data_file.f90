!***********************************************************************
! Read PRMS Data File
!***********************************************************************
module PRMS_DATA_FILE
    use kinds_mod, only: r4, r8, i4, i8
    implicit none

    integer(i4), save :: Num_datafile_types, Num_datafile_columns, Datafile_unit
    character(len=16), allocatable, save :: Data_varname(:)
    integer(i4), allocatable, save :: Data_varnum(:)
    real(r4), allocatable, save :: Data_line_values(:)

    private :: check_data_variables
    public :: read_data_line, read_prms_data_file

    contains

        !***********************************************************************
        ! check_data_variables - Check data variables and dimensions
        !***********************************************************************
        subroutine check_data_variables(Varname, Numvalues, Values, Iflag, Iret)
            use prms_constants, only: CFS2CMS_CONV
            use PRMS_MODULE, only: Ntemp, Nrain, Nobs
            use PRMS_OBS, only: Tmin, Tmax, Precip, Runoff, Streamflow_cfs, Streamflow_cms, Runoff_units
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Varname
            integer(i4), intent(in) :: Numvalues
            integer(i4), intent(in) :: Iflag
            integer(i4), intent(OUT) :: Iret
            real(r4), intent(in) :: Values(Numvalues)

            ! Local Variables
            integer(i4) :: ndim, i

            !***********************************************************************
            Iret = 0

            if (Varname(:4) == 'tmax') then
                if (Iflag == 0) then
                    if (Numvalues /= Ntemp) then
                        Iret = -1
                        ndim = Ntemp
                    endif
                else
                    do i = 1, Numvalues
                        Tmax(i) = Values(i)
                    enddo
                endif
            elseif (Varname(:4) == 'tmin') then
                if (Iflag == 0) then
                    if (Numvalues /= Ntemp) then
                        Iret = -1
                        ndim = Ntemp
                    endif
                else
                    do i = 1, Numvalues
                        Tmin(i) = Values(i)
                    enddo
                endif
            elseif (Varname(:6) == 'precip') then
                if (Iflag == 0) then
                    if (Numvalues /= Nrain) then
                        Iret = -1
                        ndim = Nrain
                    endif
                else
                    do i = 1, Numvalues
                        Precip(i) = Values(i)
                    enddo
                endif
            elseif (Varname(:6) == 'runoff') then
                if (Iflag == 0) then
                    if (Numvalues /= Nobs) then
                        Iret = -1
                        ndim = Nobs
                    endif
                else
                    do i = 1, Numvalues
                        Runoff(i) = Values(i)
                    enddo
                    if (ALL(Runoff_units == 1)) then
                        do i = 1, Nobs
                            Streamflow_cms(i) = DBLE(Runoff(i))
                            Streamflow_cfs(i) = Streamflow_cms(i) / CFS2CMS_CONV
                        enddo
                    else
                        do i = 1, Nobs
                            Streamflow_cfs(i) = DBLE(Runoff(i))
                            Streamflow_cms(i) = Streamflow_cfs(i) * CFS2CMS_CONV
                        enddo
                    endif
                endif
            else
                print *, 'ERROR, data variable: ', Varname, ' is not valid'
                Iret = -2
            endif
            if (Iret == -1) then
                print *, 'ERROR, number of values for data variable: ', Varname
                print *, 'does not equal dimension value; data values:', Numvalues, ' dimension:', ndim
            endif
        end subroutine check_data_variables

        !***********************************************************************
        ! Read PRMS Data File line
        !***********************************************************************
        subroutine read_data_line(curr_time)
            use iso_fortran_env
            use variables_mod, only: Variable_data, getvarnvals, getvar_id
            ! use PRMS_MMFAPI, only: Variable_data, getvarnvals, getvar_id
            use UTILS_PRMS, only: read_error
            implicit none

            ! Arguments
            integer(i4), intent(in) :: curr_time(6)

            ! Functions
            INTRINSIC TRANSFER

            ! Local Variables
            integer(i4) datatime(6), jj, ios, column_end, column, nvals, var_id

            !***********************************************************************
            read (Datafile_unit, *, IOSTAT = ios) datatime, (Data_line_values(jj), jj = 1, Num_datafile_columns)
            if (ios /= 0) then
                print *, 'ERROR on:', curr_time(1), curr_time(2), curr_time(3)
                if (ios /= 0) then
                    if (ios == IOSTAT_END) call read_error(13, 'hit end of file')
                    call read_error(13, 'invalid line')
                endif
            endif

            if (datatime(1) /= curr_time(1) .OR. datatime(2) /= curr_time(2) .OR. datatime(3) /= curr_time(3)) then
                print *, 'ERROR on: ', curr_time(1), curr_time(2), curr_time(3), 'Data File date:', datatime(1), datatime(2), datatime(3)
                call read_error(13, 'data file date does not match time step date')
            endif

            column_end = 0
            column = 1

            do jj = 1, Num_datafile_types
                nvals = getvarnvals(Data_varname(jj))
                var_id = getvar_id(Data_varname(jj))
                column_end = column_end + nvals

                Variable_data(var_id)%values_real = TRANSFER(Data_line_values(column:column_end), Variable_data(var_id)%values_real)

                call check_data_variables(Data_varname(jj), nvals, Data_line_values(column:column_end), 1, ios)
                if (ios /= 0) then
                    print *, 'ERROR, Data File corrupted. Reading variable: ', Data_varname(jj)
                    print *, 'Date:', curr_time(1), curr_time(2), curr_time(3)
                    STOP
                endif
                column = column + nvals
            enddo
        end subroutine read_data_line

        subroutine read_prms_data_file(data_filename)
            use iso_fortran_env
            use prms_constants, only: EQULS, MAXFILE_LENGTH
            use fileio_mod, only: write_outfile
            use PRMS_MODULE, only:PRMS_output_unit, Print_debug, Starttime, Endtime, print_module
            use UTILS_PRMS, only: numchars, read_error, PRMS_open_input_file, find_current_time
            ! use PRMS_CONTROL_FILE, only: control_string
            implicit none

            character(:), allocatable, intent(in) :: data_filename

            ! Functions
            INTRINSIC LEN_TRIM, TRIM

            ! Local Variables

            character(len=MAXFILE_LENGTH) :: data_line, dmy
            ! character(len = MAXFILE_LENGTH) :: data_filename, data_line, dmy
            character(len=80) :: line
            integer(i4) :: n, ierr, ios, numchrs, length
            integer(i4) :: startyr, startmo, startdy, starthr, startmn, startsec
            integer(i4) :: endyr, endmo, enddy, endhr, endmn, endsec, num_vars
            real(r4), allocatable :: var(:)
            character(len=80), save :: Version_read_data_file

            !***********************************************************************
            Version_read_data_file = 'read_data_file.f90 2017-09-29 13:49:00Z'
            call print_module(Version_read_data_file, 'Read Data File              ', 90)

            ! if (control_string(data_filename, 'data_file') /= 0) call read_error(5, 'data_file')

            call PRMS_open_input_file(Datafile_unit, data_filename, 'data_file', 0, ios)
            if (ios /= 0) STOP

            call write_outfile(' ')
            call write_outfile(EQULS)
            call write_outfile('Using PRMS Data File: ' // data_filename)

            ! Echo Data File Header and comment lines
            read (Datafile_unit, FMT = '(A)', IOSTAT = ios) line

            if (ios /= 0) call read_error(13, 'title')
            call write_outfile('Title: ' // TRIM(line))
            call write_outfile(EQULS)
            call write_outfile('Comment lines:')
            num_vars = 0

            do
                read (Datafile_unit, FMT = '(A)', IOSTAT = ios) line
                if (ios == IOSTAT_END) call read_error(13, 'invalid Data File, end of file reached')
                if (ios /= 0) call read_error(13, 'comment')
                if (line(:4) == '    ') CYCLE
                if (line(:2) == '//') call write_outfile(TRIM(line))
                num_vars = num_vars + 1
                if (line(:4) == '####') EXIT
            enddo

            if (line(:4) /= '####') STOP 'ERROR, invalid Data File, data section not found'
            call write_outfile(EQULS)
            call write_outfile('measured variables')

            ! read variables and number
            rewind (Datafile_unit)
            read (Datafile_unit, FMT = '(A)') line ! skip first line
            allocate (Data_varname(num_vars), Data_varnum(num_vars))
            Data_varname = '                '
            Data_varnum = 0
            Num_datafile_columns = 0
            Num_datafile_types = 0
            ierr = 0

            do
                read (Datafile_unit, FMT = '(A)') line
                if (line(:4) == '    ' .OR. line(:2) == '//') CYCLE
                if (line(:4) == '####') EXIT
                length = LEN_TRIM(line)
                call write_outfile(line(:length))
                numchrs = numchars(line(:length))

                read (line(numchrs + 1:length), *) n
                if (n == 0) then
                    if (Print_debug > -1) print *, 'Varaible: ', line(:numchrs), ' ignored as number of values = 0'
                    CYCLE
                endif

                Num_datafile_types = Num_datafile_types + 1
                Data_varname(Num_datafile_types) = line(:numchrs)
                Data_varnum(Num_datafile_types) = n
                Num_datafile_columns = Num_datafile_columns + n
                allocate (var(n))
                call check_data_variables(Data_varname(Num_datafile_types), n, var, 0, ios)
                DEALLOCATE (var)
                if (ios /= 0) ierr = ios
            enddo

            if (ierr == 1) STOP
            call write_outfile(EQULS)
            allocate (Data_line_values(Num_datafile_columns))

            read (Datafile_unit, *, IOSTAT = ios) startyr, startmo, startdy, starthr, startmn, startsec
            if (ios /= 0) call read_error(13, 'first data line')

            do
                read (Datafile_unit, '(A)', IOSTAT = ios) dmy
                if (ios == -1) EXIT ! found end of file
                if (ios /= 0) call read_error(13, 'data line')
                data_line = dmy
            enddo

            read (data_line, *, IOSTAT = ios) endyr, endmo, enddy, endhr, endmn, endsec
            write (PRMS_output_unit, 10) ' Data File', startyr, startmo, startdy, starthr, startmn, startsec, &
                                                       endyr, endmo, enddy, endhr, endmn, endsec
            10   FORMAT (A, ' time period:', I5.4, 2('/', I2.2), I3.2, 2(':', I2.2), ' to', I5.4, 2('/', I2.2), I3.2, 2(':', I2.2))
            write (PRMS_output_unit, 10) 'Simulation', Starttime, Endtime
            call write_outfile(EQULS)

            ! check start and end times, if not valid stop with print
            ierr = 0
            if (Starttime(1) < startyr) then
                ierr = 1
            elseif (Starttime(1) == startyr) then
                if (Starttime(2) < startmo) then
                    ierr = 1
                elseif (Starttime(2) == startmo .AND. Starttime(3) < startdy) then
                    ierr = 1
                endif
            endif

            if (ierr == 1) STOP 'ERROR, simulation time begins before Data File'

            ierr = 0
            if (Endtime(1) > endyr) then
                ierr = 1
            elseif (Endtime(1) == endyr) then
                if (Endtime(2) > endmo) then
                    ierr = 1
                elseif (Endtime(2) == endmo .AND. Endtime(3) > enddy) then
                    ierr = 1
                endif
            endif

            if (ierr == 1) STOP 'ERROR, simulation end time exceeds Data File'

            ! read to start of data
            rewind Datafile_unit

            do
                read (Datafile_unit, FMT = '(A)') data_line
                if (data_line(:4) == '####') EXIT
            enddo

            call find_current_time(Datafile_unit, Starttime(1), Starttime(2), Starttime(3), ios, 0)
            if (ios /= 0) then
                print *, 'End of file or error reading Data File to find the first simulation time step'
                print *, 'Data File: ', data_filename
                STOP
            endif
        end subroutine read_prms_data_file

end module PRMS_DATA_FILE

!***********************************************************************
! read_data_file_line - Read next data line, check increment
!***********************************************************************
!        subroutine read_data_file_line(Iret)
!            use PRMS_MODULE, only: Start_year, Start_month, Start_day
!            use time_mod, only: compute_julday
!            use PRMS_SET_TIME, only: Nowyear, Nowmonth, Nowday
!            use UTILS_PRMS, only: read_error
!            implicit none
!
!            ! Arguments
!            integer, intent(OUT) :: Iret
!
!            ! Local Variables
!            integer last_julday, now_julday, hr, mn, sec, start, i
!            integer, save :: init
!            DATA init/1/
!
!            !***********************************************************************
!            Iret = 0
!
!            if (init == 1) then
!                Nowyear = Start_year
!                Nowmonth = Start_month
!                Nowday = Start_day
!            else
!                last_julday = compute_julday(Nowyear, Nowmonth, Nowday)
!            endif
!
!            read (Datafile_unit, *, IOSTAT = Iret) Nowyear, Nowmonth, Nowday, hr, mn, sec, &
!                    (Data_line_values(i), i = 1, Num_datafile_columns)
!
!            if (Iret == 0) then
!                if (init == 0) then
!                    now_julday = compute_julday(Nowyear, Nowmonth, Nowday)
!                    if (now_julday - last_julday /= 1) then
!                        print *, 'ERROR, Data File timestep not equal to 1 day on:', Nowyear, Nowmonth, Nowday
!                        print *, '       timestep =', now_julday - last_julday
!                        STOP
!                    endif
!                else
!                    init = 0
!                endif
!
!                start = 1
!                do i = 1, Num_datafile_types
!                    call check_data_variables(Data_varname(i), Data_varnum(i), Data_line_values(start), 1, Iret)
!                    start = start + Data_varnum(i)
!                enddo
!            else
!                call read_error(13, 'measured variables')
!            endif
!        end subroutine read_data_file_line
