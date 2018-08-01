submodule(PRMS_PRECIPITATION_HRU) sm_precipitation_hru
contains
  !! Precipitation_hru constructor
  module function constructor_Precipitation_hru(ctl_data, param_data) result(this)
    use UTILS_CBH, only: find_current_time, find_header_end
    implicit none

    type(Precipitation_hru) :: this
      !! Precipitation_hru class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Parameters), intent(in) :: param_data

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    this%Precipitation = Precipitation(ctl_data, param_data)

    associate(nhru => ctl_data%nhru%value, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              precip_day => ctl_data%precip_day%values(1), &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values)

      ! if (print_debug > -2) then
      !   ! Output module and version information
      !   call print_module_info(MODNAME, MODDESC, MODVERSION)
      ! endif

      ! Open and read the precipitation cbh file
      call find_header_end(nhru, this%precip_funit, ierr, precip_day%s, &
                           'precip_day', (cbh_binary_flag==1))
      if (ierr == 1) then
        istop = 1
      else
        call find_current_time(ierr, this%precip_funit, start_time, (cbh_binary_flag==1))
      endif

      if (istop == 1) STOP 'ERROR in climate_hru'
    end associate
  end function

  module subroutine run_Precipitation_hru(this, ctl_data, param_data, model_basin, model_temp, model_time)
    use UTILS_PRMS, only: get_array
    implicit none

    class(Precipitation_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Time_t), intent(in), optional :: model_time

    ! Local variables
    integer(i32) :: ios
    integer(i32) :: jj
    integer(i32) :: yr, mo, dy, hr, mn, sec
      !! junk vars to hold time info from files

    real(r32), pointer, contiguous :: rain_adj_2d(:,:)
    real(r32), pointer, contiguous :: snow_adj_2d(:,:)
    real(r32), pointer, contiguous :: adjmix_rain_2d(:,:)

    ! --------------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &

              nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &

              ! basin_area_inv => model_basin%basin_area_inv, &

              hru_area => param_data%hru_area%values)

      ios = 0

      read(this%precip_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%hru_ppt(jj), jj=1, nhru)

      ! FIXME: This is dangerous because it circumvents the intent for param_data
      ! Get 2D access to 1D array
      rain_adj_2d => get_array(param_data%rain_cbh_adj%values, (/nhru, nmonths/))
      snow_adj_2d => get_array(param_data%snow_cbh_adj%values, (/nhru, nmonths/))
      adjmix_rain_2d => get_array(param_data%adjmix_rain%values, (/nhru, nmonths/))

      this%pptmix = 0
      this%newsnow = 0
      this%prmx = 0.0
      this%hru_rain = 0.0
      this%hru_snow = 0.0

      call this%set_precipitation_form(ctl_data, param_data, model_basin, model_temp, &
                                          curr_month, rain_adj_2d(:, curr_month), &
                                          snow_adj_2d(:, curr_month), &
                                          adjmix_rain_2d(:, curr_month))

    end associate
  end subroutine


end submodule
