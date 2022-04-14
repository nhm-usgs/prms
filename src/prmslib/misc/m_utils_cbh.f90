module UTILS_CBH
  use iso_fortran_env, only: output_unit
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
          ! Stream access to file (aka binary)
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
    end subroutine



    subroutine open_netcdf_cbh_file(iunit, varid, idx_offset, filename, param_name, start_time, end_time, model_nhru)
      use netcdf
      use UTILS_TIME, only: gregorian_to_julian
      implicit none

      integer(i32), intent(out) :: iunit
        !! Unit number for the opened netcdf file
      integer(i32), intent(out) :: varid
        !! Variable ID in the CBH netcdf file
      integer(i32), intent(out) :: idx_offset
        !! Offset into netcdf file to start_time for model
      character(len=*), intent(in) :: filename
        !! Name of netcdf file to open
      character(len=*), intent(in) :: param_name
        !! Name of parameter that should exist in the file
      integer(i32), intent(in) :: start_time(6)
        !! Start date specified by model
      integer(i32), intent(in) :: end_time(6)
        !! End date specified by model
      integer(i32), intent(in) :: model_nhru
        !! Number of HRUs specified for the model

      ! Local variables
      integer(i32) :: dimid
        !! Temporary for dimension ids in the netcdf file
      integer(i32) :: ii
        !! Scratch variable
      integer(i32) :: nhru
        !! Number of HRUs as defined by the netcdf file
      integer(i32) :: ntime
        !! Number of timesteps in the netcdf file
      integer(i32) :: units_len

      integer(i32) :: base_dt(3)
        !! Base date as read from the time.units attribute
      real(r32), allocatable :: time_1D(:)
        !! Array of time values

      character(len=5), dimension(2) :: dimnames = (/ 'hruid', 'time ' /)
        !! Dimensions to read from the netcdf file
      character(len=:), allocatable :: units_txt
        !! Temporary for the units attribute

      ! ------------------------------------------------------------------------
      ! Open netcdf file as read only
      call check(nf90_open(filename, NF90_NOWRITE, iunit))

      ! Get the size of the time and nhru dimensions
      do ii = 1, size(dimnames)
        call check(nf90_inq_dimid(iunit, dimnames(ii), dimid))

        select case(dimnames(ii))
          case('hruid')
            call check(nf90_inquire_dimension(iunit, dimid, len=nhru))
          case('time')
            call check(nf90_inquire_dimension(iunit, dimid, len=ntime))
        end select
      end do

      if (nhru /= model_nhru) then
        write(output_unit, *) 'ERROR: Number of HRUs in netcdf file, ', filename, 'do not match number of model defined HRUs'
        stop
      endif

      ! Allocate the time and tmax variable
      allocate(time_1D(ntime))

      ! Get the varid of the time variable
      call check(nf90_inq_varid(iunit, "time", varid))

      ! Read the time data
      call check(nf90_get_var(iunit, varid, time_1D))

      call check(nf90_inquire_attribute(iunit, varid, 'units', len=units_len))
      allocate(character(len=units_len) :: units_txt)
      call check(nf90_get_att(iunit, varid, 'units', units_txt))

      ! Get the base date for time.units='days since ...'
      base_dt = units_to_date(units_txt)

      ! Compute the offset to the location of the starting date for the model
      ! NOTE: In PRMS this should be a zero-based offset so it can be used in
      !       conjunction with the timestep which starts at 1.
      idx_offset = gregorian_to_julian(start_time(1), start_time(2), start_time(3)) - &
                   gregorian_to_julian(base_dt(1), base_dt(2), base_dt(3))

      if (idx_offset < 0) then
        write(output_unit, *) 'ERROR: start time occurs before first available date in file'
        stop
      endif

      ! TODO: Add code to verify data exists through end_time

      ! Get the varid of the data variable, based on its name.
      call check(nf90_inq_varid(iunit, param_name, varid))

      deallocate(time_1D)
      deallocate(units_txt)
    end subroutine


    subroutine read_netcdf_cbh_file(iunit, varid, idx_offset, timestep, nhru, thedata)
      use netcdf
      use UTILS_TIME, only: gregorian_to_julian
      implicit none

      integer(i32), intent(in) :: iunit
        !! Unit number for the opened netcdf file
      integer(i32), intent(in) :: varid
        !! Variable ID in the CBH netcdf file
      integer(i32), intent(in) :: idx_offset
        !! Offset into netcdf file to start_time for model
      integer(i32), intent(in) :: timestep
        !! Current timestep of the model
      integer(i32), intent(in) :: nhru
        !! Number of HRUs in model
      real(r32), pointer, intent(inout) :: thedata(:)

      ! ------------------------------------------------------------------------
      ! Read a single day of data
      call check(nf90_get_var(iunit, varid, thedata, start=[1, timestep+idx_offset], count=[nhru, 1]))

    end subroutine


    subroutine check(status)
      use netcdf
      implicit none

      integer, intent(in) :: status

      if (status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop "Stopped"
      end if
    end subroutine check


    function units_to_date(units_txt) result(res)
      integer(i32) :: res(3)
      character(len=*), intent(in) :: units_txt

      character(len=10) :: date_txt

      ! --------------------------------------------------------------------------
      date_txt = trim(units_txt(verify(units_txt, 'days since'):index(trim(units_txt), ' ', BACK=.true.)))

      ! Convert the datetime text to integers
      read(date_txt(1:index(date_txt, '-')-1), *) res(1)
      read(date_txt(index(date_txt, '-')+1:index(date_txt, '-', BACK=.true.)-1), *) res(2)
      read(date_txt(index(date_txt, '-', BACK=.true.)+1:), *) res(3)
    end function
end module
