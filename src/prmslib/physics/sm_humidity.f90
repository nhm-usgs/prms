submodule(PRMS_HUMIDITY) sm_humidity
contains
  module subroutine init_Humidity(this, ctl_data, model_basin)
    implicit none

    class(Humidity), intent(inout) :: this
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
      ! Read the parameters for the humidity module

      ! Setup class variables
      allocate(this%humidity_hru(nhru))
      this%humidity_hru = 0.0

      ! NOTE: Should humidity_hru really be available as an output variable?
    end associate
  end subroutine

  module subroutine run_Humidity(this, ctl_data, model_basin, model_time)
    implicit none

    class(Humidity), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time
  end subroutine

  module subroutine cleanup_Humidity(this, ctl_data)
    implicit none

    class(Humidity), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
  end subroutine

end submodule