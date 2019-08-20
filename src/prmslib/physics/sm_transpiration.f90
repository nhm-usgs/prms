submodule(PRMS_TRANSPIRATION) sm_transpiration
contains
  module subroutine init_Transpiration(this, ctl_data, model_basin, model_temp)
    class(Transpiration), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    ! --------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              nhru => model_basin%nhru)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%transp_on(nhru))
      this%transp_on = .false.
    end associate
  end subroutine

  module subroutine run_Transpiration(this, ctl_data, model_time, model_basin, model_temp)
    class(Transpiration), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    ! --------------------------------------------------------------------------
    print *, 'Transpiration%run() stub'
  end subroutine

end submodule
