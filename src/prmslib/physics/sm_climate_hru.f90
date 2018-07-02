submodule (PRMS_CLIMATE_HRU) sm_climate_hru

contains
  !***********************************************************************
  ! Climate_HRU constructor
  module function constructor_Climate_HRU(ctl_data, param_data) result(this)
    use prms_constants, only: dp
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Climate_HRU) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! Control
    ! nhru, cbh_binary_flag, et_module, print_debug, start_time, solrad_module,
    ! stream_temp_flag, strmtemp_humidity_flag, transp_module,

    ! ----------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              et_module => ctl_data%et_module%values(1), &
              print_debug => ctl_data%print_debug%value, &
              ! solrad_module => ctl_data%solrad_module%values(1), &
              start_time => ctl_data%start_time%values, &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value, &
              transp_module => ctl_data%transp_module%values(1))

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! TODO: This is only used for file-related procedures
      this%nhru = nhru

      ! Humidity
      if (et_module%s == 'potet_pt' .or. et_module%s == 'potet_pan' .or. &
          (stream_temp_flag == 1 .and. strmtemp_humidity_flag == 0)) then
        allocate(this%humidity_hru(nhru))
        this%humidity_hru = 0.0

        call this%find_header_end(this%humidity_funit, ierr, &
                                  ctl_data%humidity_day%values(1)%s, 'humidity_day', &
                                  (cbh_binary_flag==1))

        if (ierr == 1) then
          istop = 1
        else
          ! TODO: Original code defaults ierr=2 which causes the humidity_cbh_flag
          !       to be reset and allows climate_flow to use humidity_percent
          !       from the parameter file instead. Need to figure out a good way
          !       to handle this.
          call this%find_current_time(ierr, this%humidity_funit, start_time, (cbh_binary_flag==1))
        endif
      endif

      if (et_module%s == 'climate_hru') then
        call this%find_header_end(this%et_funit, ierr, &
                                  ctl_data%potet_day%values(1)%s, 'potet_day', &
                                  (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call this%find_current_time(ierr, this%et_funit, start_time, (cbh_binary_flag==1))
        endif
      end if

      ! Windspeed
      if (et_module%s == 'potet_pan') then
        allocate(this%windspeed_hru(nhru))
        this%windspeed_hru = 0.0

        call this%find_header_end(this%windspeed_funit, ierr, &
                                  ctl_data%windspeed_day%values(1)%s, 'windspeed_day', &
                                  (cbh_binary_flag==1))

        if (ierr == 1) then
          istop = 1
        else
          call this%find_current_time(ierr, this%windspeed_funit, start_time, (cbh_binary_flag==1))
        endif
      endif

      this%basin_humidity = 0.0_dp
      this%basin_windspeed = 0.0_dp

      ! Transpiration
      if (transp_module%s == 'climate_hru') then
        call this%find_header_end(this%transp_funit, ierr, &
                                  ctl_data%transp_day%values(1)%s, 'transp_day', &
                                  (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call this%find_current_time(ierr, this%transp_funit, start_time, (cbh_binary_flag==1))
        endif
      endif

      ! Solar radiation
      ! if (solrad_module%s == 'climate_hru') then
      !   call this%find_header_end(this%swrad_funit, ierr, &
      !                             ctl_data%swrad_day%values(1)%s, 'swrad_day', &
      !                             (cbh_binary_flag==1))
      !   if (ierr == 1) then
      !     istop = 1
      !   else
      !     call this%find_current_time(ierr, this%swrad_funit, start_time, (cbh_binary_flag==1))
      !   endif
      ! endif

      ! Precipitation
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

      ! Temperature
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
    use prms_constants, only: dp
    use UTILS_PRMS, only: get_array
    implicit none

    class(Climate_HRU), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(inout) :: climate
    ! type(Soltab), intent(in) :: model_soltab

    ! Local variables
    integer(i32) :: chru
    integer(i32) :: ierr
    integer(i32) :: ios
    integer(i32) :: jj
    integer(i32) :: yr, mo, dy, hr, mn, sec
      !! junk vars to hold time info from files

    real(r32), pointer, contiguous :: tmax_adj_2d(:,:)
    real(r32), pointer, contiguous :: tmin_adj_2d(:,:)
    real(r32), pointer, contiguous :: potet_adj_2d(:,:)
    real(r32), pointer, contiguous :: rain_adj_2d(:,:)
    real(r32), pointer, contiguous :: snow_adj_2d(:,:)
    real(r32), pointer, contiguous :: adjmix_rain_2d(:,:)

    ! Control
    ! nhru, nmonths,
    ! et_module, orad_flag, solrad_module, stream_temp_flag,
    ! strmtemp_humidity_flag, transp_module

    ! Parameter
    ! hru_area, potet_cbh_adj,

    ! Basin
    ! basin_area_inv,

    ! Climate
    ! basin_potet, basin_potsw, basin_swrad, basin_transp_on, orad,
    ! potet, swrad, transp_on

    ! Soltab
    ! hru_cossl, soltab_basinpotsw, soltab_potsw

    ! Time_t
    ! curr_month, day_of_year
    ! ----------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &
              nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &
              et_module => ctl_data%et_module%values(1), &
              ! orad_flag => ctl_data%orad_flag%value, &
              ! solrad_module => ctl_data%solrad_module%values(1), &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value, &
              transp_module => ctl_data%transp_module%values(1), &
              basin_area_inv => model_basin%basin_area_inv, &
              ! basin_horad => climate%basin_horad, &
              basin_potet => climate%basin_potet, &
              ! basin_potsw => climate%basin_potsw, &
              ! basin_swrad => climate%basin_swrad, &
              basin_transp_on => climate%basin_transp_on, &
              ! orad => climate%orad, &
              potet => climate%potet, &
              ! swrad => climate%swrad, &
              transp_on => climate%transp_on, &
              ! hru_cossl => model_soltab%hru_cossl, &
              ! soltab_basinpotsw => model_soltab%soltab_basinpotsw, &
              ! soltab_potsw => model_soltab%soltab_potsw, &
              hru_area => param_data%hru_area%values, &
              potet_cbh_adj => param_data%potet_cbh_adj%values)

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

      ! 2D index to 1D
      ! idx1D = (curr_month - 1) * nhru + chru

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

        ! FIXME: This is dangerous because it circumvents the intent for param_data
        ! Get 2D access to 1D array
        rain_adj_2d => get_array(param_data%rain_cbh_adj%values, (/nhru, nmonths/))
        snow_adj_2d => get_array(param_data%snow_cbh_adj%values, (/nhru, nmonths/))
        adjmix_rain_2d => get_array(param_data%adjmix_rain%values, (/nhru, nmonths/))

        call climate%set_precipitation_form(ctl_data, param_data, model_basin, &
                                            curr_month, rain_adj_2d(:, curr_month), &
                                            snow_adj_2d(:, curr_month), &
                                            adjmix_rain_2d(:, curr_month))
      endif

      ! Evapotranspiration
      if (et_module%s == 'climate_hru') then
        read(this%et_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (potet(jj), jj=1, nhru)
        basin_potet = 0.0_dp

        ! FIXME: This is dangerous because it circumvents the intent for param_data
        potet_adj_2d => get_array(param_data%potet_cbh_adj%values, (/nhru, nmonths/))

        ! Potet(i) = Potet(i)*Potet_cbh_adj(i, Nowmonth)
        potet = potet * potet_adj_2d(:, curr_month)
        basin_potet = sum(dble(potet * hru_area)) * basin_area_inv
      endif

      ! Humidity
      if (et_module%s == 'potet_pt' .or. et_module%s == 'potet_pm' .or. &
          (stream_temp_flag == 1 .and. strmtemp_humidity_flag == 0)) then
        read(this%humidity_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%humidity_hru(jj), jj=1, nhru)

        ! NOTE: This doesn't check for missing/bad values
        this%basin_humidity = sum(dble(this%humidity_hru * hru_area)) * basin_area_inv
      endif

      ! Solar radiation
      ! if (solrad_module%s == 'climate_hru') then
      !   ! basin_horad = soltab_basinpotsw(day_of_year)  ! Calculated in cc/ddsolrad
      !
      !   if (orad_flag == 0) then
      !     read(this%swrad_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (swrad(jj), jj=1, nhru)
      !
      !     ! FIXME: Bad assumption using HRU 1
      !     orad = sngl((dble(swrad(1)) * hru_cossl(1) * soltab_basinpotsw(day_of_year)) / soltab_potsw(day_of_year, 1))
      !   else
      !     ! orad is specified as last column in swrad_day CBH file
      !     read(this%swrad_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (swrad(jj), jj=1, nhru), orad
      !   endif
      !
      !   basin_swrad = sum(dble(swrad * hru_area)) * basin_area_inv
      !   basin_potsw = basin_swrad
      ! endif

      ! Transpiration
      if (transp_module%s == 'climate_hru') then
        read(this%transp_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (transp_on(jj), jj=1, nhru)

        if (any(transp_on==1)) then
          basin_transp_on = 1
        else
          basin_transp_on = 0
        endif

      endif

      ! Windspeed
      if (et_module%s == 'potet_pm') then
        read(this%windspeed_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%windspeed_hru(jj), jj=1, nhru)

        ! NOTE: This doesn't check for missing/bad values
        this%basin_windspeed = sum(dble(this%windspeed_hru * hru_area)) * basin_area_inv
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
