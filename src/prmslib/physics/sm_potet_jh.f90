submodule (PRMS_POTET_JH) sm_potet_jh
contains
  !***********************************************************************
  ! Potet_jh constructor
  module subroutine init_Potet_jh(this, ctl_data, model_basin, model_summary)
    use UTILS_PRMS, only: print_module_info
    implicit none

    class(Potet_jh), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Potential_ET%init(ctl_data, model_basin, model_summary)
    ! this%Potential_ET = Potential_ET(ctl_data, model_basin, model_summary)

    associate(param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%jh_coef(nhru, nmonths))
      call param_hdl%get_variable('jh_coef', this%jh_coef)

      allocate(this%jh_coef_hru(nhru))
      call param_hdl%get_variable('jh_coef_hru', this%jh_coef_hru)

      ! WARNING: tavg_f will be removed once temp_unit is standardized to Celsius.
      ! allocate(this%tavg_f(nhru))
    end associate
  end subroutine


  module subroutine run_Potet_jh(this, ctl_data, model_basin, model_time, model_solrad, model_temp)
    use conversions_mod, only: c_to_f
    implicit none

    class(Potet_jh), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    class(SolarRadiation), intent(in) :: model_solrad
    class(Temperature), intent(in) :: model_temp

    ! Local Variables
    integer(i32) :: chru
      ! Current HRU
    integer(i32) :: j
      !! Loop variable
    ! integer(i32) :: idx1D
      !! 1D index from 2D
    real(r32) :: elh
      !! Latent heat of vaporization
    ! real(r32), pointer, contiguous :: jh_coef_2d(:,:)

    ! ***********************************************************************
    ! 597.3 cal/gm at 0 C is the energy required to change the state of
    ! water to vapor
    ! elh is the latent heat of vaporization (not including the *2.54)

    ! Control
    !

    ! Time_t
    ! curr_month

    ! Basin
    ! nhru, nmonths, active_mask, basin_area_inv

    ! Parameters
    ! jh_coef_hru, hru_area,

    ! Temperature
    ! tavgc, tavgf

    ! SolarRadiation
    ! swrad

    ! --------------------------------------------------------------------------
    associate(curr_month => model_time%Nowmonth, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nhru, &
              active_hrus => model_basin%active_hrus, &
              active_mask => model_basin%active_mask, &
              basin_area_inv => model_basin%basin_area_inv, &
              hru_area => model_basin%hru_area, &
              hru_route_order => model_basin%hru_route_order, &

              swrad => model_solrad%swrad, &

              tavg => model_temp%tavg, &
              tavg_f => model_temp%tavg_f)

      ! this%tavg_f = c_to_f(model_temp%tavg)

      ! jh_coef_2d => get_array(param_data%jh_coef%values, (/nhru, nmonths/))
      ! climate%basin_potet = 0.0

      ! WARNING: This is confusing because tavgc and tavgf are used
      ! this%potet = jh_coef_2d(:, curr_month) * (tavgf - jh_coef_hru) * &
      !              swrad / ((597.3 - (0.5653 * tavgc)) * 2.54)
      ! where (this%potet < 0.0) this%potet = 0.0

      do j=1, active_hrus
        chru = hru_route_order(j)
        ! idx1D = (curr_month - 1) * nhru + chru

        ! WARNING: Use of tavgf will be removed once temp_unit is standardized
        !          to Celsius and jh_coef, jh_coef_hru are re-computed for Celsius.
        ! elh = (597.3 - (0.5653 * sngl(tavg(chru)))) * 2.54
        elh = (597.3 - (0.5653 * tavg(chru))) * 2.54
        ! this%potet(chru) = jh_coef_2d(chru, curr_month) * (tavg_f(chru) - &
        ! this%potet(chru) = jh_coef(idx1D) * (tavg_f(chru) - jh_coef_hru(chru)) * swrad(chru) / elh
        this%potet(chru) = this%jh_coef(chru, curr_month) * (tavg_f(chru) - this%jh_coef_hru(chru)) * swrad(chru) / elh

        if (this%potet(chru) < 0.0) this%potet(chru) = 0.0
      enddo

      this%basin_potet = sum(dble(this%potet * hru_area), mask=active_mask) * basin_area_inv
    end associate
  end subroutine

end submodule
