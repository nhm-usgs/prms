submodule(PRMS_TEMPERATURE) sm_temperature
contains
  module function constructor_Temperature(ctl_data) result(this)
    type(Temperature) :: this
      !! Temperature class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters

    ! Control
    ! nhru,

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value)

      allocate(this%tavg(nhru))
      allocate(this%tmax(nhru))
      allocate(this%tmin(nhru))

      ! NOTE: Only used by potet_jh; remove once temperature units are standardized
      allocate(this%tavg_f(nhru))
      allocate(this%tmax_f(nhru))
    end associate
  end function

  module subroutine run_Temperature(this, ctl_data, param_data, model_basin, model_time)
    class(Temperature), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time

    ! --------------------------------------------------------------------------
  end subroutine

end submodule
