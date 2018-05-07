! utils_prms.f90 2017-09-22 14:37:00Z

module UTILS_PRMS
  use variableKind
  implicit none

  contains
    function get_array(array, shape_) result(aptr)
      use iso_c_binding, only: C_LOC, C_F_POINTER
      ! NOTE: from https://stackoverflow.com/questions/5406016/changing-array-dimensions-in-fortran
      ! Pass in the array as an array of fixed size so that there
      ! is no array descriptor associated with it. This means we
      ! can get a pointer to the location of the data using C_LOC
      real(r32), target :: array(1)
      integer(i32) :: shape_(:)
      real(r32), pointer :: aptr(:,:)

      ! Use C_LOC to get the start location of the array data, and
      ! use C_F_POINTER to turn this into a fortran pointer (aptr).
      ! Note that we need to specify the shape of the pointer using an
      ! integer array.
      call C_F_POINTER(C_LOC(array), aptr, shape_)
    end function

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
    !     Open PRMS input File and assign unit number
    !***********************************************************************
    subroutine PRMS_open_input_file(iunit, iret, Fname, Paramname, use_stream)
      implicit none

      ! Argument
      integer(i32), intent(out) :: iunit
      integer(i32), intent(out) :: iret
      character(len=*), intent(in) :: Fname
      character(len=*), intent(in) :: Paramname
      logical, optional, intent(in) :: use_stream
        !! When .true. a stream (aka binary) file is opened


      ! Local Variables
      integer(i32) :: ios
      logical, parameter :: use_stream_def = .false.
      logical :: use_stream_

      !***********************************************************************
      ! NOTE: Kludge to get around Fortran's lack of default values for args.
      use_stream_ = use_stream_def
      if (present(use_stream)) use_stream_ = use_stream

      iret = 0
      iunit = get_ftnunit(777)

      if (use_stream_) then
        ! Open file for stream access (aka binary)
        open (unit=iunit, FILE=Fname, STATUS='OLD', form='unformatted', access='stream', IOSTAT=ios)
      else
        open (unit=iunit, FILE=Fname, STATUS='OLD', IOSTAT=ios)
      endif

      if (ios /= 0) then
        write (*, '(/,2A,/,A,/,2A,/)') 'ERROR opening input file: ', Fname, &
                  'check that input file exists', &
                  'file specified by control parameter: ', Paramname
        iret = 1
      endif
    end subroutine


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
    function get_ftnunit(iunit) result(res)
      implicit none

      ! Argument
      integer(i32) :: res
      integer, intent(in) :: iunit

      ! Local Variables
      integer :: good_unit
      logical :: opend

      !***********************************************************************
      good_unit = iunit
      opend = .TRUE.

      do while (opend)
        good_unit = good_unit + 1
        INQUIRE (UNIT=good_unit, OPENED=opend)
      enddo

      res = good_unit
   end function
end module
