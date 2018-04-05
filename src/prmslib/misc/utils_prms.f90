! utils_prms.f90 2017-09-22 14:37:00Z

module UTILS_PRMS
  use variableKind
  implicit none

  contains
    subroutine print_module_info(module_name, module_desc, module_version)
      use iso_fortran_env, only: output_unit
      implicit none

      character(len=*), intent(in) :: module_name
      character(len=*), intent(in) :: module_desc
      character(len=*), intent(in) :: module_version

      ! ------------------------------------------------------------------------
      ! Output module and version information
      write(output_unit, 9008) module_desc//repeat(' ', 30), &
                               module_name//repeat(' ', 20), &
                               module_version//repeat(' ', 20)

      9008 format(a30, 1x, a20, 1x, a20)
    end subroutine


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
