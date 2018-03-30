! utils_prms.f90 2017-09-22 14:37:00Z

module UTILS_PRMS
  use variableKind
  implicit none

  contains

    !***********************************************************************
    !     Check parameter value against dimension
    !***********************************************************************
    subroutine checkdim_param_limits(Indx, Param, Dimen, Param_value, Lower_val, Upper_val, iret)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: Indx
      character(len=*), intent(in) :: Param
      character(len=*), intent(in) :: Dimen
      integer(i32), intent(in) :: Param_value
      integer(i32), intent(in) :: Lower_val
      integer(i32), intent(in) :: Upper_val
      integer(i32), intent(inout) :: iret

      !***********************************************************************
      if (Param_value < Lower_val .OR. Param_value > Upper_val) then
        print *, 'ERROR, out-of-bounds value for bounded parameter: ', Param
        print *, '       value:  ', Param_value, '; array index:', Indx
        print *, '       minimum:', Lower_val, '; maximum is dimension ', Dimen, ' =', Upper_val
        print *, ' '
        iret = 1
      endif
    end subroutine checkdim_param_limits

    !***********************************************************************
    !     Check parameter value limits
    !***********************************************************************
    subroutine check_param_limits(Indx, Param, Param_value, Lower_val, Upper_val, iret)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: Indx
      character(len=*), intent(in) :: Param
      real(r32), intent(in) :: Param_value
      real(r32), intent(in) :: Lower_val
      real(r32), intent(in) :: Upper_val
      integer(i32), intent(inout) :: iret

      !***********************************************************************
      if (Param_value < Lower_val .OR. Param_value > Upper_val) then
        print *, 'ERROR, bad value, parameter: ', Param
        print *, '       value:  ', Param_value, '; array index:', Indx
        print *, '       minimum:', Lower_val, '  ; maximum:', Upper_val
        print *, ' '
        iret = 1
      endif
    end subroutine check_param_limits

    !     Check parameter value < 0.0
    !***********************************************************************
    subroutine check_param_zero(Indx, Param, Param_value, iret)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: Indx
      character(len=*), intent(in) :: Param
      real(r32), intent(in) :: Param_value
      integer(i32), intent(inout) :: iret

      !***********************************************************************
      if (Param_value < 0.0) then
        print *, 'ERROR, value < 0.0 for parameter: ', Param
        print *, '       value:', Param_value, '; HRU:', Indx
        print *, ' '
        iret = 1
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
      integer(i32), intent(in) :: Oldval
      integer(i32), intent(in) :: Newval
      integer(i32), intent(inout) :: ierr

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
    subroutine find_current_file_time(iunit, Year, Month, Day, Year_file, Month_file, Day_file)
      implicit none

      ! Argument
      integer(i32), intent(in) :: iunit
      integer(i32), intent(in) :: Year
      integer(i32), intent(in) :: Month
      integer(i32), intent(in) :: Day
      integer(i32), intent(out) :: Year_file
      integer(i32), intent(out) :: Month_file
      integer(i32), intent(out) :: Day_file

      ! Local Variables
      integer(i32) :: i, ios

      !***********************************************************************
      ! find first value for simulation time period
      read (iunit, *, IOSTAT=ios) Year_file, Month_file, Day_file

      if (ios /= 0) then
        Year_file = 0
        Month_file = 0
        Day_file = 0
        return
      endif

      if (Year_file < Year) then
        i = 0

        do while (i == 0)
          read (iunit, *, IOSTAT=ios) Year_file, Month_file, Day_file

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
            read (iunit, *, IOSTAT=ios) Year_file, Month_file, Day_file

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
              read (iunit, *, IOSTAT=ios) Year_file, Month_file, Day_file

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

      backspace iunit
    end subroutine find_current_file_time

    !***********************************************************************
    !     Read CBH File to current time
    !***********************************************************************
    ! subroutine find_current_time(iunit, Year, Month, Day, iret, Cbh_binary_flag)
    subroutine find_current_time(iret, iunit, datetime, binary_flag)
      use prms_constants, only: MAXFILE_LENGTH
      implicit none

      ! Argument
      integer(i32), intent(out) :: iret
      integer(i32), intent(in) :: iunit
      integer(i32), intent(in) :: datetime(6)
      integer(i32), optional, intent(in) :: binary_flag
      ! integer(i32), intent(in) :: Year
      ! integer(i32), intent(in) :: Month
      ! integer(i32), intent(in) :: Day

      ! Local Variables
      integer(i32) :: yr
      integer(i32) :: mo
      integer(i32) :: dy
      character(len=MAXFILE_LENGTH) :: filename

      !***********************************************************************
      iret = 0
      do
        if (present(binary_flag) .and. binary_flag == 1) then
          read(iunit, IOSTAT=iret) yr, mo, dy
        else
          read(iunit, *, IOSTAT=iret) yr, mo, dy
        endif

        if (iret == -1) then
          inquire(UNIT=iunit, NAME=filename)
          print *, 'ERROR, end-of-file found reading ', trim(filename), ' for date:', &
                                 datetime(1), datetime(2), datetime(3)
          stop
        endif

        if (iret /= 0) return
        if (yr == datetime(1) .AND. mo == datetime(2) .AND. dy == datetime(3)) EXIT
      enddo

      backspace iunit
    end subroutine find_current_time

    !**********************
    ! Check for end of file
    !**********************
    subroutine is_eof(iunit, Next_yr, Next_mo, Next_day)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: iunit
      integer(i32), intent(out) :: Next_yr
      integer(i32), intent(out) :: Next_mo
      integer(i32), intent(out) :: Next_day

      ! Local Variables
      integer(i32) :: ios, i
      character(len=80) :: dum

      !*******************************************************************************
      Next_yr = 0
      Next_mo = 0
      Next_day = 0
      i = 0

      do while (i == 0)
        read (iunit, '(A)', iostat=ios) dum
        if (ios /= 0) return
        if (dum(:2) /= '//') i = 1
      enddo

      read (dum, *, iostat=ios) Next_yr, Next_mo, Next_day

      if (ios /= 0) then
        Next_yr = 0
        Next_mo = 0
        Next_day = 0
      else
        backspace iunit
      endif
    end subroutine is_eof

    !**********************************************************************
    ! Module error
    !**********************************************************************
    subroutine module_error(Modname, Arg, Retcode)
      implicit none

      ! Arguments
      character(len=*), intent(in) :: Modname
      character(len=*), intent(in) :: Arg
      integer(i32), intent(in) :: Retcode

      !**********************************************************************
      print 9001, Modname, Arg, Retcode
      STOP

      9001 format ('ERROR in ', A, ' module, arg = ', A, /, 'Return val =', I4)
    end subroutine module_error

    !***********************************************************************
    !     Open PRMS input File and assign unit number
    !***********************************************************************
    subroutine PRMS_open_input_file(iunit, Fname, Paramname, Ftype, iret)
      implicit none

      ! Argument
      integer(i32), intent(out) :: iunit
      character(len=*), intent(in) :: Fname
      character(len=*), intent(in) :: Paramname
      integer(i32), intent(in) :: Ftype
      integer(i32), intent(out) :: iret

      ! Local Variables
      integer(i32) :: ios  ! , nchars

      !***********************************************************************
      iret = 0
      iunit = get_ftnunit(777)
      ! nchars = numchars(Fname)

      if (Ftype == 0) then
        open (unit=iunit, FILE=Fname, STATUS='OLD', IOSTAT=ios)
        ! open (iunit, FILE=Fname(:nchars), STATUS='OLD', IOSTAT=ios)
      else
        open (unit=iunit, FILE=Fname, STATUS='OLD', FORM='UNFORMATTED', IOSTAT=ios) ! for linux
        ! open (iunit, FILE=Fname(:nchars), STATUS='OLD', FORM='BINARY', IOSTAT=ios) ! for windows
      endif

      if (ios /= 0) then
        write (*, '(/,2A,/,A,/,2A,/)') 'ERROR opening input file: ', Fname, &
                  'check that input file exists', &
                  'file specified by control parameter: ', Paramname
        iret = 1
      endif
    end subroutine PRMS_open_input_file

    !***********************************************************************
    !     Open PRMS module output file and assign unit number
    !***********************************************************************
    subroutine PRMS_open_module_file(iunit, Fname)
      implicit none

      ! Argument
      integer(i32), intent(out) :: iunit
      character(len=*), intent(in) :: Fname

      ! Local Variables
      integer(i32) :: ios  ! , nchars

      !***********************************************************************
      ! iunit = get_ftnunit(888)
      ! nchars = numchars(Fname)
      open (newunit=iunit, FILE=Fname, STATUS='REPLACE', IOSTAT=ios)

      if (ios /= 0) then
        write (*, '(/,A,/,A,/)') 'ERROR opening water balance output file:', Fname, &
                                 'check to be sure the pathname is valid and the file is not open'
        STOP
      endif
    end subroutine PRMS_open_module_file

    !***********************************************************************
    !     Open PRMS output file and assign unit number
    !***********************************************************************
    subroutine PRMS_open_output_file(iunit, Fname, Paramname, Ftype, iret)
      implicit none

      ! Argument
      integer(i32), intent(out) :: iunit
      character(len=*), intent(in) :: Fname
      character(len=*), intent(in) :: Paramname
      integer(i32), intent(in) :: Ftype ! 0=text; 1=BINARY
      integer(i32), intent(out) :: iret

      ! Local Variables
      integer(i32) :: ios

      !***********************************************************************
      iret = 0
      ! iunit = get_ftnunit(888)

      if (Ftype == 0) then
        open (newunit=iunit, FILE=Fname, STATUS='REPLACE', IOSTAT=ios)
      else
        open (newunit=iunit, FILE=Fname, STATUS='REPLACE', IOSTAT=ios, FORM='UNFORMATTED' ) ! for linux
        ! open (iunit, FILE = Fname(:nchars), STATUS = 'REPLACE', IOSTAT = ios, FORM = 'BINARY') ! for windows
      endif

      if (ios /= 0) then
        write (*, '(/,A,/,A,/)') 'ERROR opening output file:', Fname, &
                                 'check to be sure the pathname is valid and the file is not open'
        write (*, '(2A,/)') 'file specified by control parameter: ', Paramname
        iret = 1
      endif
    end subroutine PRMS_open_output_file

    !**********************************************************************
    !     Parameter or Variable delcare or read error
    !**********************************************************************
    subroutine read_error(Iflag, Name)
      !! TODO: write a generic error function
      implicit none

      ! Arguments
      integer(i32), intent(in) :: Iflag
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
      integer(i32), intent(in) :: Length
      character(len=*), intent(in) :: Param_version

      !**********************************************************************
      if (Module_version(13:Length + 12) /= Param_version(:Length)) then
        print 9001, Module_version(13:Length + 12), Param_version(:Length)
        print *, 'Enter return to continue'
        read (*, *)
      endif

      9001 format ('WARNING, module versions are not identical', /, &
                   'Executable version: ', A, /, &
                   'Parameter File version: ', A, /)
    end subroutine version_check

    !***********************************************************************
    subroutine write_double_param(iunit, Parm_name, Dimen_name, Dimen, Values)
      !***********************************************************************
      implicit none

      ! Arguments
      integer(i32), intent(in) :: iunit
      character(len=*), intent(in) :: Parm_name
      character(len=*), intent(in) :: Dimen_name
      integer(i32), intent(in) :: Dimen
      real(r64), intent(in) :: Values(Dimen)

      ! Local Variables
      integer(i32) :: i
      character(len=*), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, "3")'

      !***********************************************************************
      write (iunit, fmt1) Parm_name, Dimen_name, Dimen

      do i=1, Dimen
        write (iunit, *) Values(i)
      enddo
    end subroutine write_double_param

    !***********************************************************************
    subroutine write_integer_param(iunit, Parm_name, Dimen_name, Dimen, Values)
      !***********************************************************************
      implicit none

      ! Arguments
      integer(i32), intent(in) :: iunit
      character(len=*), intent(in) :: Parm_name
      character(len=*), intent(in) :: Dimen_name
      integer(i32), intent(in) :: Dimen
      integer(i32), intent(in) :: Values(Dimen)

      ! Local Variables
      integer(i32) :: i
      character(len=48), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, /, "1")'

      !***********************************************************************
      write (iunit, fmt1) Parm_name, Dimen_name, Dimen
      do i=1, Dimen
        write (iunit, *) Values(i)
      enddo
    end subroutine write_integer_param

    !***********************************************************************
    subroutine write_real_param(iunit, Parm_name, Dimen_name, Dimen, Values)
      !***********************************************************************
      implicit none

      ! Arguments
      integer(i32), intent(in) :: iunit
      character(len=*), intent(in) :: Parm_name
      character(len=*), intent(in) :: Dimen_name
      integer(i32), intent(in) :: Dimen
      real(r32), intent(in) :: Values(Dimen)

      ! Local Variables
      integer(i32) :: i
      character(len=*), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, /, "2")'

      !***********************************************************************
      write (iunit, fmt1) Parm_name, Dimen_name, Dimen

      do i=1, Dimen
        write (iunit, *) Values(i)
      enddo
    end subroutine write_real_param

    !***********************************************************************
    subroutine write_2D_double_array_grid(iunit, Parm_name, Dimen_name1, Dimen1, &
                                          Dimen_name2, Dimen2, Values)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: iunit
      character(len=*), intent(in) :: Parm_name
      character(len=*), intent(in) :: Dimen_name1
      integer(i32), intent(in) :: Dimen1
      character(len=*), intent(in) :: Dimen_name2
      integer(i32), intent(in) :: Dimen2
      real(r64), intent(in) :: Values(Dimen1, Dimen2)

      ! Local Variables
      integer(i32) :: i, j
      character(len=12) :: fmt

      !***********************************************************************
      write (iunit, 9001) Parm_name, Dimen_name1, Dimen_name2, Dimen1 * Dimen2
      write (fmt, 9002) Dimen2

      do i=1, Dimen2
        write (iunit, fmt) (Values(j, i), j=1, Dimen1)
      enddo

      9001 format ('####', /, A, /, '2', /, A, /, A, /, I8, /, '3')
      9002 format ('(', I5, 'F10.5)')
    end subroutine write_2D_double_array_grid

    !***********************************************************************
    subroutine write_2D_double_param(iunit, Parm_name, Dimen_name1, Dimen1, Dimen_name2, Dimen2, Values)
      !***********************************************************************
      implicit none

      ! Arguments
      integer(i32), intent(in) :: iunit
      character(len=*), intent(in) :: Parm_name
      character(len=*), intent(in) :: Dimen_name1
      integer(i32), intent(in) :: Dimen1
      character(len=*), intent(in) :: Dimen_name2
      integer(i32), intent(in) :: Dimen2
      real(r64), intent(in) :: Values(Dimen1, Dimen2)

      ! Local Variables
      integer(i32) :: i, j
      character(len=*), PARAMETER :: fmt1 = '("####", /, A, /, "2", /, A, /, A, /, I8, "3")'

      !***********************************************************************
      write (iunit, fmt1) Parm_name, Dimen_name1, Dimen_name2, Dimen1 * Dimen2

      do i=1, Dimen2
        do j=1, Dimen1
          write (iunit, *) Values(j, i)
        enddo
      enddo
    end subroutine write_2D_double_param

    !***********************************************************************
    ! Set data type flag
    !***********************************************************************
    subroutine set_data_type(Data_type, Type_flag)
      implicit none

      ! Arguments
      character(len=*), intent(in) :: Data_type
      integer(i32), intent(out) :: Type_flag

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
    integer function get_ftnunit(iunit)
      implicit none

      ! Argument
      integer, intent(in) :: iunit

      ! Local Variables
      integer :: good_unit
      LOGICAL :: opend

      !***********************************************************************
      good_unit = iunit
      opend = .TRUE.

      do while (opend)
        good_unit = good_unit + 1
        INQUIRE (UNIT=good_unit, OPENED=opend)
      enddo

      get_ftnunit = good_unit
   end function get_ftnunit
end module
