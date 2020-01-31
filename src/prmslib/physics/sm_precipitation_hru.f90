submodule(PRMS_PRECIPITATION_HRU) sm_precipitation_hru
contains
  !! Precipitation_hru constructor
  module subroutine init_Precipitation_hru(this, ctl_data, model_basin, model_temp, model_summary)
    use UTILS_CBH, only: find_current_time, find_header_end, open_netcdf_cbh_file, read_netcdf_cbh_file
    implicit none

    class(Precipitation_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Summary), intent(inout) :: model_summary

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Precipitation%init(ctl_data, model_basin, model_temp, model_summary)
    ! this%Precipitation = Precipitation(ctl_data, model_basin, model_temp, model_summary)

    associate(cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              end_time => ctl_data%end_time%values, &
              precip_day => ctl_data%precip_day%values(1), &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Setup parameters
      allocate(this%rain_cbh_adj(nhru, nmonths))
      call param_hdl%get_variable('rain_cbh_adj', this%rain_cbh_adj)

      allocate(this%snow_cbh_adj(nhru, nmonths))
      call param_hdl%get_variable('snow_cbh_adj', this%snow_cbh_adj)

      allocate(this%adjmix_rain(nhru, nmonths))
      call param_hdl%get_variable('adjmix_rain', this%adjmix_rain)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Open the CBH file
      !scan(trim(precip_day%s),".", BACK= .true.)
      if (precip_day%s(scan(trim(precip_day%s),".", BACK= .true.)+1:) == 'nc') then

      !if (precip_day%s(index(precip_day%s, '.')+1:) == 'nc') then
        call open_netcdf_cbh_file(this%precip_funit, this%precip_varid, this%precip_idx_offset, &
                                  precip_day%s, 'prcp', start_time, end_time, nhru)
        this%has_netcdf_precip = .true.
      else
        ! Open and read the precipitation cbh file
        call find_header_end(nhru, this%precip_funit, ierr, precip_day%s, &
                             'precip_day', (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%precip_funit, start_time, (cbh_binary_flag==1))
        endif

        this%has_netcdf_precip = .false.
      endif

      if (istop == 1) STOP 'ERROR in precipitation_hru'
    end associate
  end subroutine


  module subroutine run_Precipitation_hru(this, ctl_data, model_basin, model_temp, model_time, model_summary)
    use UTILS_CBH, only: read_netcdf_cbh_file
    implicit none

    class(Precipitation_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Time_t), intent(in), optional :: model_time
    type(Summary), intent(inout) :: model_summary

    ! Local variables
    integer(i32) :: ios
    integer(i32) :: datetime(6)

    ! --------------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &
              timestep => model_time%Timestep, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths)

      ios = 0

      if (this%has_netcdf_precip) then
        call read_netcdf_cbh_file(this%precip_funit, this%precip_varid, this%precip_idx_offset, timestep, nhru, this%hru_ppt)
      else
        ! read(this%precip_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%hru_ppt(jj), jj=1, nhru)
        read(this%precip_funit, *, IOSTAT=ios) datetime, this%hru_ppt
      endif

      this%prmx = 0.0
      this%hru_rain = 0.0
      this%hru_snow = 0.0

      call this%set_precipitation_form(ctl_data, model_basin, model_temp, &
                                       curr_month, this%rain_cbh_adj, this%snow_cbh_adj, &
                                       this%adjmix_rain)

      ! If any precipitation output variables are specified then we set pointers
      ! to the arrays for the nhru_summary module.
      if (this%has_hru_summary_vars) then
        call this%set_summary_ptrs(ctl_data, model_summary)
      end if
    end associate
  end subroutine

end submodule
