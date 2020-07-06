submodule(PRMS_HUMIDITY_STA) sm_humidity_sta
contains
  module subroutine init_Humidity_sta(this, ctl_data, model_basin)
    implicit none

    class(Humidity_sta), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Humidity%init(ctl_data, model_basin)

    associate(cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              end_time => ctl_data%end_time%values, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              humidity_day => ctl_data%humidity_day%values(1), &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Get the number of humidity stations
      this%nhumid = param_hdl%get_dimension('nhumid')

      ! Allocate the humidity station by HRU parameter
      allocate(this%hru_humidity_sta(nhru, this%nhumid))
      call param_hdl%get_variable('hru_humidity_sta', this%hru_humidity_sta)

      allocate(this%humidity_obs(nhru))
      ! IF ( Nhumid>0 ) THEN
      !   ALLOCATE ( Humidity(Nhumid) )
      !   IF ( declvar(MODNAME, 'humidity', 'nhumid', Nhumid, 'real', &
      !       'Relative humidity at each measurement station', &
      !       'percentage', Humidity)/=0 ) CALL read_error(8, 'humidity')
      ! ENDIF
    end associate
  end subroutine

  module subroutine run_Humidity_sta(this, ctl_data, model_basin, model_time)
    implicit none

    class(Humidity_sta), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time

    ! TODO: 2020-07-06 PAN - finish converting code for reading humidity station data
    ! IF ( Nhumid>0 ) THEN
    !   IF ( readvar(MODNAME, 'humidity')/=0 ) CALL read_error(9, 'humidity')
    ! ENDIF

    ! General idea of copying station humidity to HRUs
    ! do j = 1, Active_hrus
    !   i = hru_route_order(j)
    !   this%humidity_hru(i) = this%humidity(this%hru_humidity_sta(i))
    ! end do
  end subroutine

  module subroutine cleanup_Humidity_sta(this, ctl_data)
    implicit none

    class(Humidity_sta), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
  end subroutine

end submodule