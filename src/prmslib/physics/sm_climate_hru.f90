submodule (PRMS_CLIMATE_HRU) sm_climate_hru

contains
  !***********************************************************************
  ! Climate_HRU constructor
  module function constructor_Climate_HRU(ctl_data, param_data) result(this)
    ! use Control_class, only: Control
    ! use Parameters_class, only: Parameters
    use UTILS_PRMS, only: find_current_time, print_module_info
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
                                  1, cbh_binary_flag)
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%precip_funit, start_time, cbh_binary_flag)
        endif
      endif

      if (ctl_data%temp_module%values(1)%s == 'climate_hru') then
        call this%find_header_end(this%tmax_funit, ierr, ctl_data%tmax_day%values(1)%s, &
                                  'tmax_day', 1, cbh_binary_flag)
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%tmax_funit, start_time, cbh_binary_flag)
        endif

        ! Iunit, Iret, Fname, Paramname, Cbh_flag, Cbh_binary_flag
        call this%find_header_end(this%tmin_funit, ierr, ctl_data%tmin_day%values(1)%s, &
                                  'tmin_day', 1, cbh_binary_flag)
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%tmin_funit, start_time, cbh_binary_flag)
        endif
      endif

      if (istop == 1) STOP 'ERROR in climate_hru'
    end associate
  end function

  module subroutine run_Climate_HRU(this, ctl_data, param_data, model_time, model_basin, climate)
    use prms_constants, only: MM2INCH, NEARZERO
    use conversions_mod, only: f_to_c, c_to_f
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
              nmonths => ctl_data%nmonths%values(1), &
              ! hru_area => param_data%hru_area%values, &
              tmax_cbh_adj => param_data%tmax_cbh_adj%values, &
              tmin_cbh_adj => param_data%tmin_cbh_adj%values, &
              rain_cbh_adj => param_data%rain_cbh_adj%values, &
              snow_cbh_adj => param_data%snow_cbh_adj%values, &
              adjmix_rain => param_data%adjmix_rain%values)
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
        tmin_adj_2d => get_array(tmin_cbh_adj, (/nhru, nmonths/))
        tmax_adj_2d => get_array(tmax_cbh_adj, (/nhru, nmonths/))

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
        rain_adj_2d => get_array(rain_cbh_adj, (/nhru, nmonths/))
        snow_adj_2d => get_array(snow_cbh_adj, (/nhru, nmonths/))
        adjmix_rain_2d => get_array(adjmix_rain, (/nhru, nmonths/))

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
  !     Read File to line before data starts in file
  !***********************************************************************
  module subroutine find_header_end(this, Iunit, Iret, Fname, Paramname, Cbh_flag, &
                                    Cbh_binary_flag)
    use UTILS_PRMS, only: PRMS_open_input_file
    implicit none

    ! Argument
    class(Climate_HRU), intent(inout) :: this
    integer(i32), intent(out) :: Iunit
    integer(i32), intent(out) :: Iret
    character(len=*), intent(in) :: Fname
    character(len=*), intent(in) :: Paramname
    integer(i32), intent(in) :: Cbh_flag
    integer(i32), intent(in) :: Cbh_binary_flag

    ! Local Variables
    integer(i32) :: i
    integer(i32) :: ios
    integer(i32) :: dim
    character(len=4) :: dum
    character(len=80) :: dum2

    !***********************************************************************
    call PRMS_open_input_file(Iunit, Fname, Paramname, Cbh_binary_flag, Iret)

    if (Iret == 0) then
      ! read to line before data starts in each file
      i = 0

      do while (i == 0)
        if (Cbh_binary_flag == 0) then
          read(Iunit, FMT='(A4)', IOSTAT=ios) dum
        else
          read(Iunit, IOSTAT=ios) dum2
          read(dum2, '(A4)') dum
        endif

        if (ios /= 0) then
          write(*, '(/,A,/,A,/,A)') 'ERROR reading file:', Fname, &
                   'check to be sure the input file is in correct format'
          Iret = 1
          EXIT
        elseif (dum == '####') then
          if (Cbh_flag == 0) EXIT
          BACKSPACE Iunit
          BACKSPACE Iunit

          if (Cbh_binary_flag == 0) then
            read(Iunit, *, IOSTAT=ios) dum, dim
          else
            read(Iunit, IOSTAT=ios) dum2
            read(dum2, *) dum, dim
          endif

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

          if (Cbh_binary_flag == 0) then
            read(Iunit, FMT='(A4)', IOSTAT=ios) dum
          else
            read(Iunit, IOSTAT=ios) dum
          endif
          i = 1
        endif
      enddo
    endif
  end subroutine find_header_end

  !***********************************************************************
  !     Read a day in the CBH File
  !***********************************************************************
  module subroutine read_cbh_date(this, model_time, Year, Month, Day, Var, Ios, Iret)
    implicit none

    class(Climate_HRU), intent(inout) :: this
    type(Time_t), intent(in) :: model_time
    integer(i32), intent(in) :: Year
    integer(i32), intent(in) :: Month
    integer(i32), intent(in) :: Day
    integer(i32), intent(in) :: Ios
    character(len=*), intent(in) :: Var
    integer(i32), intent(inout) :: Iret

    ! Local Variables
    logical :: right_day

    !***********************************************************************
    associate(curr_year => model_time%Nowyear, &
              curr_month => model_time%Nowmonth, &
              curr_day => model_time%Nowday)

      right_day = .true.
      if (Year /= curr_year .OR. Month /= curr_month .OR. Day /= curr_day) right_day = .false.

      if (Ios /= 0 .OR. .not. right_day) then
        print *, 'ERROR, reading CBH File, variable: ', Var, ' IOSTAT=', Ios

        if (Ios == -1) then
          print *, '       End-of-File found'
        elseif (.not. right_day) then
          print *, '       Wrong day found'
        else
          print *, '       Invalid data value found'
        endif

        call model_time%print_date(0)
        Iret = 1
      endif
    end associate
  end subroutine read_cbh_date
end submodule
