submodule(PRMS_WIND_HRU) sm_wind_hru
contains
  module subroutine init_Wind_hru(this, ctl_data, model_basin)
    use UTILS_CBH, only: find_current_time, find_header_end, open_netcdf_cbh_file, read_netcdf_cbh_file
    implicit none

    class(Wind_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Wind%init(ctl_data, model_basin)

    associate(cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              end_time => ctl_data%end_time%values, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              windspeed_day => ctl_data%windspeed_day%values(1), &

              nhru => model_basin%nhru)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Open netcdf or ascii-based cbh files
      if (windspeed_day%s(scan(trim(windspeed_day%s),".", BACK= .true.)+1:) == 'nc') then
        ! Read a netcdf file
        call open_netcdf_cbh_file(this%windspeed_funit, this%varid, this%idx_offset, &
                                  windspeed_day%s, 'windspeed', start_time, end_time, nhru)
        this%has_netcdf_windspeed = .true.
      else
        ! Open and read ASCII cbh file
        call find_header_end(nhru, this%windspeed_funit, ierr, windspeed_day%s, &
                             'windspeed_day', (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%windspeed_funit, start_time, (cbh_binary_flag==1))
        endif

        this%has_netcdf_windspeed = .false.
      endif

      if (istop == 1) then
        STOP 'ERROR opening windspeed_hru'
      end if
    end associate
  end subroutine

  module subroutine run_Wind_hru(this, ctl_data, model_basin, model_time)
    use UTILS_CBH, only: read_netcdf_cbh_file
    implicit none

    class(Wind_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time

    ! Local variables
    integer(i32) :: ios
    integer(i32) :: datetime(6)

    ! --------------------------------------------------------------------------
    associate(nhru => model_basin%nhru, &

              curr_month => model_time%Nowmonth, &
              timestep => model_time%Timestep)

      ios = 0

      if (this%has_netcdf_windspeed) then
        call read_netcdf_cbh_file(this%windspeed_funit, this%varid, this%idx_offset, timestep, nhru, this%windspeed_hru)
      else
        read(this%windspeed_funit, *, IOSTAT=ios) datetime, this%windspeed_hru
      endif
    end associate
  end subroutine

  module subroutine cleanup_Wind_hru(this, ctl_data)
    implicit none

    class(Wind_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
  end subroutine
end submodule