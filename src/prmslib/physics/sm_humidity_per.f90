submodule(PRMS_HUMIDITY_PER) sm_humidity_per
contains
  module subroutine init_Humidity_per(this, ctl_data, model_basin)
    implicit none

    class(Humidity_per), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Humidity%init(ctl_data, model_basin)

    associate(print_debug => ctl_data%print_debug%value, &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%humidity_percent(nhru, nmonths))
      call param_hdl%get_variable('humidity_percent', this%humidity_percent)
    end associate
  end subroutine

  module subroutine run_Humidity_per(this, ctl_data, model_basin, model_time)
    implicit none

    class(Humidity_per), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time

    ! --------------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth)

      this%humidity_hru = this%humidity_percent(:, curr_month)
    end associate
  end subroutine

  module subroutine cleanup_Humidity_per(this, ctl_data)
    implicit none

    class(Humidity_per), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
  end subroutine
end submodule