submodule(PRMS_TRANSPIRATION) sm_transpiration
contains
  module function constructor_Transpiration(ctl_data) result(this)
    type(Transpiration) :: this
      !! Transpiration class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters

    ! Control
    ! nhru,
    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value)

      allocate(this%transp_on(nhru))
      this%transp_on = 0
    end associate
  end function

  module subroutine run_Transpiration(this, ctl_data)
    class(Transpiration), intent(inout) :: this
    type(Control), intent(in) :: ctl_data

    ! --------------------------------------------------------------------------


  end subroutine

end submodule
