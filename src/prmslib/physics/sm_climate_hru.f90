submodule (PRMS_CLIMATE_HRU) sm_climate_hru

contains
  !***********************************************************************
  ! Climate_HRU constructor
  module function constructor_Climate_HRU(ctl_data, param_data) result(this)
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Climate_HRU) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! ----------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%values(1), &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! TODO: This is only used for file-related procedures
      this%nhru = nhru

      if (ctl_data%precip_module%values(1)%s == 'climate_hru') then
        call this%find_header_end(this%precip_funit, ierr, &
                                  ctl_data%precip_day%values(1)%s, 'precip_day', &
                                  (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call this%find_current_time(ierr, this%precip_funit, start_time, (cbh_binary_flag==1))
        endif
      endif

      if (ctl_data%temp_module%values(1)%s == 'climate_hru') then
        call this%find_header_end(this%tmax_funit, ierr, ctl_data%tmax_day%values(1)%s, &
                                  'tmax_day', (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call this%find_current_time(ierr, this%tmax_funit, start_time, (cbh_binary_flag==1))
        endif

        ! Iunit, Iret, Fname, Paramname, Cbh_flag, Cbh_binary_flag
        call this%find_header_end(this%tmin_funit, ierr, ctl_data%tmin_day%values(1)%s, &
                                  'tmin_day', (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call this%find_current_time(ierr, this%tmin_funit, start_time, (cbh_binary_flag==1))
        endif
      endif

      if (istop == 1) STOP 'ERROR in climate_hru'
    end associate
  end function

  module subroutine run_Climate_HRU(this, ctl_data, param_data, model_time, model_basin, climate)
    use UTILS_PRMS, only: get_array
    implicit none

    class(Climate_HRU), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(inout) :: climate

    ! Local variables
    integer(i32) :: chru
    integer(i32) :: ierr
    integer(i32) :: ios
    integer(i32) :: jj
    integer(i32) :: yr, mo, dy, hr, mn, sec
      !! junk vars to hold time info from files

    real(r32), pointer :: tmax_adj_2d(:,:)
    real(r32), pointer :: tmin_adj_2d(:,:)
    real(r32), pointer :: rain_adj_2d(:,:)
    real(r32), pointer :: snow_adj_2d(:,:)
    real(r32), pointer :: adjmix_rain_2d(:,:)

    ! ----------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth, &
              nhru => ctl_data%nhru%values(1), &
              nmonths => ctl_data%nmonths%values(1))
              ! hru_area => param_data%hru_area%values, &
              ! tmax_cbh_adj => param_data%tmax_cbh_adj%values, &
              ! tmin_cbh_adj => param_data%tmin_cbh_adj%values, &
              ! rain_cbh_adj => param_data%rain_cbh_adj%values, &
              ! snow_cbh_adj => param_data%snow_cbh_adj%values, &
              ! adjmix_rain => param_data%adjmix_rain%values)
              ! active_hrus => model_basin%active_hrus, &
              ! hru_route_order => model_basin%hru_route_order, &
              ! basin_area_inv => model_basin%basin_area_inv)

      ierr = 0
      ios = 0

      ! *****************
      ! * Temperature   *
      ! *****************
      if (ctl_data%temp_module%values(1)%s == 'climate_hru') then
        read(this%tmax_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (climate%tmaxf(jj), jj=1, nhru)
        read(this%tmin_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (climate%tminf(jj), jj=1, nhru)

        ! NOTE: This is dangerous because it circumvents the intent for param_data
        ! Get 2D access to 1D array
        tmin_adj_2d => get_array(param_data%tmin_cbh_adj%values, (/nhru, nmonths/))
        tmax_adj_2d => get_array(param_data%tmax_cbh_adj%values, (/nhru, nmonths/))

        call climate%set_temperature(ctl_data, param_data, model_basin, &
                                     tmin_adj_2d(:, curr_month), tmax_adj_2d(:, curr_month))
      endif

      ! *****************
      ! * Precipitation *
      ! *****************
      if (ctl_data%precip_module%values(1)%s == 'climate_hru') then
        read(this%precip_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (climate%hru_ppt(jj), jj=1, nhru)

        ! NOTE: This is dangerous because it circumvents the intent for param_data
        ! Get 2D access to 1D array
        rain_adj_2d => get_array(param_data%rain_cbh_adj%values, (/nhru, nmonths/))
        snow_adj_2d => get_array(param_data%snow_cbh_adj%values, (/nhru, nmonths/))
        adjmix_rain_2d => get_array(param_data%adjmix_rain%values, (/nhru, nmonths/))

        call climate%set_precipitation_form(ctl_data, param_data, model_basin, &
                                            curr_month, rain_adj_2d(:, curr_month), &
                                            snow_adj_2d(:, curr_month), &
                                            adjmix_rain_2d(:, curr_month))
      endif
    end associate
  end subroutine

  module function module_name() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODNAME
  end function

  module function version() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODVERSION
  end function


  !***********************************************************************
  ! Read CBH file to current time
  !***********************************************************************
  module subroutine find_current_time(iret, iunit, datetime, use_stream)
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
  module subroutine find_header_end(this, Iunit, Iret, Fname, Paramname, &
                                    use_stream)
    use iso_fortran_env, only: output_unit
    use UTILS_PRMS, only: PRMS_open_input_file
    implicit none

    ! Argument
    class(Climate_HRU), intent(inout) :: this
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
            if (dim /= this%nhru) then
              print '(/,2(A,I7))', '***CBH file dimension incorrect*** nhru=', this%nhru, ' CBH dimension=', &
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

            if (dim /= this%nhru) then
              print '(/,2(A,I7))', '***CBH file dimension incorrect*** nhru=', this%nhru, ' CBH dimension=', &
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

  !***********************************************************************
  !     Read a day in the CBH File
  !***********************************************************************
  ! module subroutine read_cbh_date(this, model_time, Year, Month, Day, Var, Ios, Iret)
  !   implicit none
  !
  !   class(Climate_HRU), intent(inout) :: this
  !   type(Time_t), intent(in) :: model_time
  !   integer(i32), intent(in) :: Year
  !   integer(i32), intent(in) :: Month
  !   integer(i32), intent(in) :: Day
  !   integer(i32), intent(in) :: Ios
  !   character(len=*), intent(in) :: Var
  !   integer(i32), intent(inout) :: Iret
  !
  !   ! Local Variables
  !   logical :: right_day
  !
  !   !***********************************************************************
  !   associate(curr_year => model_time%Nowyear, &
  !             curr_month => model_time%Nowmonth, &
  !             curr_day => model_time%Nowday)
  !
  !     right_day = .true.
  !     if (Year /= curr_year .OR. Month /= curr_month .OR. Day /= curr_day) right_day = .false.
  !
  !     if (Ios /= 0 .OR. .not. right_day) then
  !       print *, 'ERROR, reading CBH File, variable: ', Var, ' IOSTAT=', Ios
  !
  !       if (Ios == -1) then
  !         print *, '       End-of-File found'
  !       elseif (.not. right_day) then
  !         print *, '       Wrong day found'
  !       else
  !         print *, '       Invalid data value found'
  !       endif
  !
  !       call model_time%print_date(0)
  !       Iret = 1
  !     endif
  !   end associate
  ! end subroutine read_cbh_date
end submodule
