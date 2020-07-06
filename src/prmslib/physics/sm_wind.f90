submodule(PRMS_WIND) sm_wind
contains
  module subroutine init_Wind(this, ctl_data, model_basin)
    implicit none

    class(Wind), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin

    ! --------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Read the parameters for the wind module

      ! Setup class variables
      allocate(this%windspeed_hru(nhru))
      this%windspeed_hru = 0.0

      ! NOTE: Should windspeed_hru really be available as an output variable?
    end associate
  end subroutine

  module subroutine run_Wind(this, ctl_data, model_basin, model_time)
    implicit none

    class(Wind), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time
  end subroutine

  module subroutine cleanup_Wind(this, ctl_data)
    implicit none

    class(Wind), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
  end subroutine

end submodule