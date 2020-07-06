submodule(PRMS_WIND_STA) sm_wind_sta
contains
  module subroutine init_Wind_sta(this, ctl_data, model_basin)
    implicit none

    class(Wind_sta), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Wind%init(ctl_data, model_basin)

    associate(cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              end_time => ctl_data%end_time%values, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              windspeed_day => ctl_data%windspeed_day%values(1), &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Get the number of wind stations
      this%nwind = param_hdl%get_dimension('nwind')

      ! Allocate the wind station by HRU parameter
      allocate(this%hru_windspeed_sta(nhru, this%nwind))
      call param_hdl%get_variable('hru_windspeed_sta', this%hru_windspeed_sta)

      allocate(this%windspeed_obs(nhru))
      ! IF ( Nwind>0 ) THEN
      !   ALLOCATE ( windspeed_obs(Nwind) )
      !   IF ( declvar(MODNAME, 'wind', 'nwind', Nwind, 'real', &
      !       'Windspeed at each measurement station', &
      !       'meters/second', windspeed_obs)/=0 ) CALL read_error(8, 'wind')
      ! ENDIF
    end associate
  end subroutine

  module subroutine run_Wind_sta(this, ctl_data, model_basin, model_time)
    implicit none

    class(Wind_sta), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time

    ! TODO: 2020-07-06 PAN - finish converting code for reading wind station data

    ! General idea of copying station windspeed to HRUs
    ! do j = 1, Active_hrus
    !   i = hru_route_order(j)
    !   this%windspeed_hru(i) = this%windspeed(this%hru_windspeed_sta(i))
    ! end do
  end subroutine

  module subroutine cleanup_Wind_sta(this, ctl_data)
    implicit none

    class(Wind_sta), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
  end subroutine

end submodule