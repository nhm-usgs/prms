submodule(PRMS_PRECIPITATION_HRU) sm_precipitation_hru
contains
  !! Precipitation_hru constructor
  module function constructor_Precipitation_hru(ctl_data, param_data) result(this)
    use UTILS_CBH, only: find_current_time, find_header_end, open_netcdf_cbh_file, read_netcdf_cbh_file
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
              end_time => ctl_data%end_time%values, &
              precip_day => ctl_data%precip_day%values(1), &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      call open_netcdf_cbh_file(this%precip_funit, this%precip_varid, this%precip_idx_offset, &
                                precip_day%s, 'prcp', start_time, end_time, nhru)

      ! ! Open and read the precipitation cbh file
      ! call find_header_end(nhru, this%precip_funit, ierr, precip_day%s, &
      !                      'precip_day', (cbh_binary_flag==1))
      ! if (ierr == 1) then
      !   istop = 1
      ! else
      !   call find_current_time(ierr, this%precip_funit, start_time, (cbh_binary_flag==1))
      ! endif
      !
      ! if (istop == 1) STOP 'ERROR in climate_hru'
    end associate
  end function

  module subroutine run_Precipitation_hru(this, ctl_data, param_data, model_basin, model_temp, model_time)
    ! use UTILS_PRMS, only: get_array
    use UTILS_CBH, only: read_netcdf_cbh_file
    implicit none

    class(Precipitation_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Time_t), intent(in), optional :: model_time

    ! Local variables
    integer(i32) :: ios
    integer(i32) :: datetime(6)

    ! --------------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &
              timestep => model_time%Timestep, &

              nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &

              hru_area => param_data%hru_area%values, &
              rain_cbh_adj => param_data%rain_cbh_adj%values, &
              snow_cbh_adj => param_data%snow_cbh_adj%values, &
              adjmix_rain => param_data%adjmix_rain%values)

      ios = 0

      call read_netcdf_cbh_file(this%precip_funit, this%precip_varid, this%precip_idx_offset, timestep, nhru, this%hru_ppt)

      ! read(this%precip_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%hru_ppt(jj), jj=1, nhru)
      ! read(this%precip_funit, *, IOSTAT=ios) datetime, this%hru_ppt

      this%pptmix = 0
      this%newsnow = 0
      this%prmx = 0.0
      this%hru_rain = 0.0
      this%hru_snow = 0.0

      call this%set_precipitation_form(ctl_data, param_data, model_basin, model_temp, &
                                       curr_month, rain_cbh_adj, snow_cbh_adj, &
                                       adjmix_rain)
      ! call this%set_precipitation_form(ctl_data, param_data, model_basin, model_temp, &
      !                                     curr_month, rain_adj_2d(:, curr_month), &
      !                                     snow_adj_2d(:, curr_month), &
      !                                     adjmix_rain_2d(:, curr_month))

    end associate
  end subroutine


end submodule
