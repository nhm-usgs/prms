module UTILS_CBH
  use variableKind
  implicit none

  contains
    !***********************************************************************
    ! Read CBH file to current time
    !***********************************************************************
    subroutine find_current_time(iret, iunit, datetime, use_stream)
      use prms_constants, only: MAXFILE_LENGTH, YEAR, MONTH, DAY
      implicit none

      ! Argument
      integer(i32), intent(out) :: iret
      integer(i32), intent(in) :: iunit
      integer(i32), intent(in) :: datetime(6)
      logical, optional, intent(in) :: use_stream
        !! When .true. a stream (aka binary) file is opened

      logical, parameter :: use_stream_def = .false.
        !! Default value to use for use_stream argument

      ! Local Variables
      integer(i32) :: yr
      integer(i32) :: mo
      integer(i32) :: dy
      character(len=MAXFILE_LENGTH) :: filename
      logical :: use_stream_
        !! Internal copy of use_stream to allow for default value.

      !***********************************************************************
      ! NOTE: Kludge to get around Fortran's lack of default values for args.
      use_stream_ = use_stream_def
      if (present(use_stream)) use_stream_ = use_stream

      iret = 0
      do
        if (use_stream_) then
          ! Stream access file (aka binary)
          read(iunit, IOSTAT=iret) yr, mo, dy
        else
          read(iunit, *, IOSTAT=iret) yr, mo, dy
        endif

        if (iret == -1) then
          inquire(UNIT=iunit, NAME=filename)
          print *, 'ERROR, end-of-file found reading ', trim(filename), ' for date:', &
                                 datetime(YEAR), datetime(MONTH), datetime(DAY)
          stop
        endif

        if (iret /= 0) return
        if (yr == datetime(YEAR) .AND. mo == datetime(MONTH) .AND. dy == datetime(DAY)) EXIT
      enddo

      backspace iunit
    end subroutine


    !***********************************************************************
    ! Read file to line before data starts in file
    !***********************************************************************
    subroutine find_header_end(nhru, Iunit, Iret, Fname, Paramname, &
                                      use_stream)
      use iso_fortran_env, only: output_unit
      use UTILS_PRMS, only: PRMS_open_input_file
      implicit none

      ! Argument
      integer(i32), intent(in) :: nhru
        !! Expected number of HRUs in the CBH file
      integer(i32), intent(out) :: Iunit
      integer(i32), intent(out) :: Iret
      character(len=*), intent(in) :: Fname
      character(len=*), intent(in) :: Paramname
      logical, optional, intent(in) :: use_stream
        !! When .true. a stream (aka binary) file is opened

      logical, parameter :: use_stream_def = .false.
        !! Default value to use for use_stream argument

      ! Local Variables
      integer(i32) :: i
      integer(i32) :: ios
      integer(i32) :: dim
      character(len=4) :: dum
      character(len=80) :: dum2

      logical :: use_stream_
        !! Internal copy of use_stream to allow for default value.

      !***********************************************************************
      ! NOTE: Kludge to get around Fortran's lack of default values for args.
      use_stream_ = use_stream_def
      if (present(use_stream)) use_stream_ = use_stream

      call PRMS_open_input_file(Iunit, Iret, Fname, Paramname, use_stream_)

      if (Iret == 0) then
        ! read to line before data starts in each file
        i = 0

        do while (i == 0)
          if (.not. use_stream_) then
            ! ASCII file
            read(Iunit, FMT='(A4)', IOSTAT=ios) dum

            if (ios /= 0) then
              write(*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                       'check to be sure the input file is in correct format'
              Iret = 1
              exit
            elseif (dum == '####') then
              ! Backspace to the <var> <dim> line
              BACKSPACE Iunit
              BACKSPACE Iunit

              ! Read the variable name and number of entries (should match nhru)
              read(Iunit, *, IOSTAT=ios) dum, dim

              if (ios /= 0) then
                write(*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                         'check to be sure dimension line is in correct format'
                Iret = 1
                EXIT
              endif

              ! Make sure the HRU dimension size in the CBH file matches our model.
              if (dim /= nhru) then
                print '(/,2(A,I7))', '***CBH file dimension incorrect*** nhru=', nhru, ' CBH dimension=', &
                                     dim, ' File: ' // Fname
                STOP 'ERROR: update Control File with correct CBH files'
              endif

              read(Iunit, FMT='(A4)', IOSTAT=ios) dum
              i = 1
            endif
          else
            ! Stream I/O (aka binary)
            read(Iunit, IOSTAT=ios) dum2
            read(dum2, '(A4)') dum

            if (ios /= 0) then
              write(*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                       'check to be sure the input file is in correct format'
              Iret = 1
              EXIT
            elseif (dum == '####') then
              ! Backspace to the <var> <dim> line
              BACKSPACE Iunit
              BACKSPACE Iunit

              read(Iunit, IOSTAT=ios) dum2
              read(dum2, *) dum, dim

              if (ios /= 0) then
                write(*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                         'check to be sure dimension line is in correct format'
                Iret = 1
                EXIT
              endif

              if (dim /= nhru) then
                print '(/,2(A,I7))', '***CBH file dimension incorrect*** nhru=', nhru, ' CBH dimension=', &
                                     dim, ' File: ' // Fname
                STOP 'ERROR: update Control File with correct CBH files'
              endif

              read(Iunit, IOSTAT=ios) dum

              i = 1
            endif
          endif
        enddo
      endif
    end subroutine find_header_end
end module
