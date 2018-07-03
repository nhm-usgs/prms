submodule(PRMS_POTET) sm_potet
contains
  module function constructor_Potet(ctl_data) result(this)
    type(Potential_ET) :: this
      !! Potential_ET class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters

    ! Control
    ! nhru

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value)

      allocate(this%potet(nhru))
      this%potet = 0.0

      ! if (ctl_data%et_module%values(1)%s == 'potet_pt' .or. &
      !     ctl_data%et_module%values(1)%s == 'potet_pm' .or. &
      !     ctl_data%et_module%values(1)%s == 'potet_pm_sta') then
      !   allocate(this%tempc_dewpt(nhru))
      !   allocate(this%vp_actual(nhru))
      !   allocate(this%lwrad_net(nhru))
      !   allocate(this%vp_slope(nhru))
      !
      !   this%tempc_dewpt = 0.0
      !   this%vp_actual = 0.0
      !   this%lwrad_net = 0.0
      !   this%vp_slope = 0.0
      ! endif
      !
      ! if (ctl_data%et_module%values(1)%s == 'potet_pm' .or. &
      !     ctl_data%et_module%values(1)%s == 'potet_pm_sta') then
      !   allocate(this%vp_sat(nhru))
      !
      !   this%vp_sat = 0.0
      ! endif


    end associate
  end function

  module subroutine run_Potet(this, ctl_data, param_data)
    class(Potential_ET), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! --------------------------------------------------------------------------

  end subroutine


end submodule
