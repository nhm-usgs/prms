submodule(UTILS_PRMS) sm_utils_prms
contains
  module subroutine update_parameter_i32(ctl_data, model_time, dyn_output_unit, dyn_values, param_name, param)
    use prms_constants, only: YEAR, MONTH, DAY

    ! Arguments
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    integer(i32), intent(in) :: dyn_output_unit
    integer(i32), intent(in) :: dyn_values(:)
    character(len=*), intent(in) :: param_name
    integer(i32), intent(inout) :: param(:)

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: num
    integer(i32), allocatable :: updated_hrus(:)

    !***********************************************************************
    associate(print_debug => ctl_data%print_debug%value, &
              curr_time => model_time%Nowtime)

      allocate(updated_hrus(size(param)))
      updated_hrus = 0
      num = 0

      do chru=1, size(param)
        if (dyn_values(chru) >= 0.0) then
          if (dyn_values(chru) /= param(chru)) then
          ! if (abs(dyn_values(chru) - param(chru)) > NEARZERO) then
            num = num + 1
            updated_hrus(num) = chru
            param(chru) = dyn_values(chru)
          end if
        end if
      end do

      if (print_debug > -2) then
        write (dyn_output_unit, '(3A,I4,2("/",I2.2))') 'Parameter ', param_name, &
                                                        ' update entry on: ', &
                                                        curr_time(YEAR), &
                                                        curr_time(MONTH), &
                                                        curr_time(DAY)
      end if

      if (print_debug > -1) then
        if (num > 0) then
          write (dyn_output_unit, '(A,I5,2("/",I2.2))') 'List of updated HRUs; Date:', &
                                                          curr_time(YEAR), &
                                                          curr_time(MONTH), &
                                                          curr_time(DAY)
          write (dyn_output_unit, '(20I7)') (updated_hrus(chru), chru=1, num)
        end if
      endif

      deallocate(updated_hrus)
    end associate
  end subroutine

  module subroutine update_parameter_r32(ctl_data, model_time, dyn_output_unit, dyn_values, param_name, param)
    use prms_constants, only: YEAR, MONTH, DAY

    ! Arguments
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    integer(i32), intent(in) :: dyn_output_unit
    real(r32), intent(in) :: dyn_values(:)
    character(len=*), intent(in) :: param_name
    real(r32), intent(inout) :: param(:)

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: num
    integer(i32), allocatable :: updated_hrus(:)

    !***********************************************************************
    associate(print_debug => ctl_data%print_debug%value, &
              curr_time => model_time%Nowtime)

      allocate(updated_hrus(size(param)))
      updated_hrus = 0
      num = 0

      do chru=1, size(param)
        if (dyn_values(chru) >= 0.0) then
          if (dyn_values(chru) /= param(chru)) then
          ! if (abs(dyn_values(chru) - param(chru)) > NEARZERO) then
            num = num + 1
            updated_hrus(num) = chru
            param(chru) = dyn_values(chru)
          end if
        end if
      end do

      if (print_debug > -2) then
        write (dyn_output_unit, '(3A,I4,2("/",I2.2))') 'Parameter ', param_name, &
                                                        ' update entry on: ', &
                                                        curr_time(YEAR), &
                                                        curr_time(MONTH), &
                                                        curr_time(DAY)
      end if

      if (print_debug > -1) then
        if (num > 0) then
          write (dyn_output_unit, '(A,I5,2("/",I2.2))') 'List of updated HRUs; Date:', &
                                                          curr_time(YEAR), &
                                                          curr_time(MONTH), &
                                                          curr_time(DAY)
          write (dyn_output_unit, '(20I7)') (updated_hrus(chru), chru=1, num)
        end if
      endif

      deallocate(updated_hrus)
    end associate
  end subroutine

  module function get_array(array, shape_) result(aptr)
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

  module subroutine print_module_info(module_name, module_desc, module_version)
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

  module subroutine PRMS_open_input_file(iunit, iret, Fname, Paramname, use_stream)
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


  module subroutine PRMS_open_module_file(iunit, Fname)
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

  module function get_ftnunit(iunit) result(res)
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


  pure module function yr_mo_eq_dy_le(lh_date, rh_date) result(res)
    use prms_constants, only: YEAR, MONTH, DAY
    use UTILS_TIME, only: gregorian_to_julian
    implicit none

    logical :: res
    integer(i32), intent(in) :: lh_date(3)
    integer(i32), intent(in) :: rh_date(3)

    res = (gregorian_to_julian(lh_date(YEAR), lh_date(MONTH), lh_date(DAY)) <= gregorian_to_julian(rh_date(YEAR), rh_date(MONTH), rh_date(DAY)))
  end function


  ! module function get_first_time(iunit, datetime) result(res)
  !   use prms_constants, only: YEAR, MONTH, DAY
  !   implicit none

  !   ! Argument
  !   integer(i32) :: res(3)
  !     !! Return a date array (YY, MM, DD)
  !   integer(i32), intent(in) :: iunit
  !   integer(i32), intent(in) :: datetime(3)
  !     !! Datetime to search for

  !   ! Local variables
  !   logical :: found
  !   integer(i32) :: dy
  !     ! Date day read from file
  !   integer(i32) :: iret
  !   integer(i32) :: mo
  !     ! Date month read from file
  !   integer(i32) :: yr
  !     ! Date year read from file

  !   ! ---------------------------------------------------------------------
  !   read(iunit, *, IOSTAT=iret) yr, mo, dy
  !   if (iret /= 0) then
  !     res = 0
  !     return
  !   end if

  !   if (yr < datetime(YEAR)) then
  !     found = .false.

  !     do while (.not. found)
  !       read(iunit, *, IOSTAT=iret) yr, mo, dy
  !       if (iret /= 0) then
  !         res = 0
  !         return
  !       end if

  !       if (yr >= datetime(YEAR)) found = .true.
  !     end do
  !   end if

  !   if (yr == datetime(YEAR)) then
  !     if (mo < datetime(MONTH)) then
  !       found = .false.

  !       do while (.not. found)
  !         read(iunit, *, IOSTAT=iret) yr, mo, dy
  !         if (iret /= 0) then
  !           res = 0
  !           return
  !         end if

  !         if (mo >=datetime(MONTH) .or. yr /= datetime(YEAR)) found = .true.
  !       end do
  !     end if

  !     if (yr == datetime(YEAR) .and. mo == datetime(MONTH)) then
  !       if (dy < datetime(DAY)) then
  !         found = .false.

  !         do while (.not. found)
  !           read(iunit, *, IOSTAT=iret) yr, mo, dy
  !           if (iret /= 0) then
  !             res = 0
  !             return
  !           end if

  !           if (dy >= datetime(DAY)) found = .true.
  !           if (yr > datetime(YEAR) .or. mo > datetime(MONTH)) then
  !             ! This can happen when the starting date is one or more days after the
  !             ! date in the dynamic parameter file AND the dynamic parameter file has
  !             ! non-continuous dates (e.g. monthly or yearly) landing on a day that is
  !             ! before the start day (but in the same year and month).
  !             found = .true.
  !             backspace iunit
  !             backspace iunit
  !             read(iunit, *, IOSTAT=iret) yr, mo, dy
  !           end if
  !         end do
  !       end if
  !     else if (yr > datetime(YEAR)) then
  !       backspace iunit
  !       backspace iunit
  !       read(iunit, *, IOSTAT=iret) yr, mo, dy
  !     end if
  !   else if (yr > datetime(YEAR)) then
  !     ! This can happen when the dynamic param file has multi-year gaps in rows
  !     ! and the start time lands in between two entries.
  !     backspace iunit
  !     backspace iunit
  !     read(iunit, *, IOSTAT=iret) yr, mo, dy
  !   end if

  !   backspace iunit
  !   res = (/yr, mo, dy/)
  ! end function

  module function get_first_time(iunit, datetime) result(res)
    use prms_constants, only: YEAR, MONTH, DAY
    use UTILS_TIME, only: gregorian_to_julian
    implicit none

    ! Argument
    integer(i32) :: res(3)
      !! Return a date array (YY, MM, DD)
    integer(i32), intent(in) :: iunit
    integer(i32), intent(in) :: datetime(3)
      !! Datetime to search for

    ! Local variables
    logical :: found
    integer(i32) :: dy
      ! Date day read from file
    integer(i32) :: iret
    integer(i32) :: mo
      ! Date month read from file
    integer(i32) :: yr
      ! Date year read from file
    integer(i32) :: julday_file
    integer(i32) :: julday_model

    ! ---------------------------------------------------------------------
    read(iunit, *, IOSTAT=iret) yr, mo, dy
    if (iret /= 0) then
      res = 0
      return
    end if

    julday_file = gregorian_to_julian(yr, mo, dy)
    julday_model = gregorian_to_julian(datetime(YEAR), datetime(MONTH), datetime(DAY))

    if (julday_file < julday_model) then
      found = .false.

      do while (.not. found)
        read(iunit, *, IOSTAT=iret) yr, mo, dy
        if (iret /= 0) then
          res = 0
          return
        end if

        julday_file = gregorian_to_julian(yr, mo, dy)
        if (julday_file >= julday_model) then
          found = .true.
        end if
      end do
    end if

    if (julday_file > julday_model) then
      backspace iunit
      backspace iunit
      read(iunit, *, IOSTAT=iret) yr, mo, dy
    end if

    backspace iunit
    res = (/yr, mo, dy/)
  end function


  module function get_next_time(iunit) result(res)
    ! use prms_constants, only: YEAR, MONTH, DAY
    implicit none

    ! Argument
    integer(i32) :: res(3)
      !! Return a date array (YY, MM, DD)
    integer(i32), intent(in) :: iunit

    ! Local variables
    logical :: found
    integer(i32) :: dy
      ! Date day read from file
    integer(i32) :: iret
    integer(i32) :: mo
      ! Date month read from file
    integer(i32) :: yr
      ! Date year read from file
    character(len=80) :: tmp
    ! -----------------------------------------------------------------------
    found = .false.

    ! Skip any comment lines
    do while(.not. found)
      read(iunit, '(A)', IOSTAT=iret) tmp
      if (iret /= 0) return
      if (tmp(:2) /= '//') found = .true.
    end do

    read(tmp, *, IOSTAT=iret) yr, mo, dy
    if (iret /= 0) then
      res = 0
      return
    end if

    res = (/yr, mo, dy/)
    backspace iunit
  end function


  !***********************************************************************
  ! Read file to line before data starts in file
  !***********************************************************************
  module subroutine open_dyn_param_file(Iunit, Iret, Fname, Paramname)
    use iso_fortran_env, only: output_unit
    implicit none

    ! Argument
    ! integer(i32), intent(in) :: nhru
      !! Expected number of HRUs in the CBH file
    integer(i32), intent(out) :: Iunit
    integer(i32), intent(out) :: Iret
    character(len=*), intent(in) :: Fname
    character(len=*), intent(in) :: Paramname

    ! Local Variables
    logical :: found
    ! integer(i32) :: i
    integer(i32) :: ios
    ! integer(i32) :: dim
    character(len=4) :: dum
    ! character(len=80) :: dum2

    !***********************************************************************
    call PRMS_open_input_file(Iunit, Iret, Fname, Paramname)

    if (Iret == 0) then
      ! read to line before data starts in each file
      found = .false.

      do while (.not. found)
        ! ASCII file
        read(Iunit, FMT='(A4)', IOSTAT=ios) dum

        if (ios /= 0) then
          write(*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                    'check to be sure the input file is in correct format'
          Iret = 1
          exit
        elseif (dum == '####') then
          found = .true.
        end if
      enddo
    endif
  end subroutine

end submodule
