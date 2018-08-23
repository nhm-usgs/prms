submodule (PRMS_POTET_JH) sm_potet_jh
contains
  !***********************************************************************
  ! Potet_jh constructor
  module function constructor_Potet_jh(ctl_data) result(this)
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Potet_jh) :: this
    type(Control), intent(in) :: ctl_data

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    this%Potential_ET = Potential_ET(ctl_data)

    associate(nhru => ctl_data%nhru%value, &
              print_debug => ctl_data%print_debug%value)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! WARNING: tavg_f will be removed once temp_unit is standardized to Celsius.
      ! allocate(this%tavg_f(nhru))

    end associate
  end function


  module subroutine run_Potet_jh(this, ctl_data, param_data, model_basin, model_time, model_solrad, model_temp)
    use conversions_mod, only: c_to_f
    use UTILS_PRMS, only: get_array
    implicit none

    class(Potet_jh), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    ! type(Climateflow), intent(in) :: climate
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
    real(r32), pointer, contiguous :: jh_coef_2d(:,:)

    ! ***********************************************************************
    ! 597.3 cal/gm at 0 C is the energy required to change the state of
    ! water to vapor
    ! elh is the latent heat of vaporization (not including the *2.54)

    ! Control
    ! nrhu, nmonths,

    ! Time_t
    ! curr_month

    ! Basin
    ! active_mask, basin_area_inv

    ! Parameters
    ! jh_coef_hru, hru_area,

    ! Temperature
    ! tavgc, tavgf

    ! SolarRadiation
    ! swrad

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &
              curr_month => model_time%Nowmonth, &
              active_mask => model_basin%active_mask, &
              basin_area_inv => model_basin%basin_area_inv, &
              ! jh_coef => param_data%jh_coef%values, &
              jh_coef_hru => param_data%jh_coef_hru%values, &
              hru_area => param_data%hru_area%values, &

              swrad => model_solrad%swrad, &

              tavg => model_temp%tavg, &
              tavg_f => model_temp%tavg_f)

      ! this%tavg_f = c_to_f(model_temp%tavg)

      jh_coef_2d => get_array(param_data%jh_coef%values, (/nhru, nmonths/))
      ! climate%basin_potet = 0.0

      ! WARNING: This confusing because tavgc and tavgf are used
      ! this%potet = jh_coef_2d(:, curr_month) * (tavgf - jh_coef_hru) * &
      !              swrad / ((597.3 - (0.5653 * tavgc)) * 2.54)
      ! where (this%potet < 0.0) this%potet = 0.0

      do j=1, model_basin%active_hrus
        chru = model_basin%hru_route_order(j)
        ! idx1D = (curr_month - 1) * ctl_data%nhru%values(1) + chru

        ! WARNING: Use of tavgf will be removed once temp_unit is standardized
        !          to Celsius and jh_coef, jh_coef_hru are re-computed for Celsius.
        ! elh = (597.3 - (0.5653 * sngl(tavg(chru)))) * 2.54
        elh = (597.3 - (0.5653 * tavg(chru))) * 2.54
        this%potet(chru) = jh_coef_2d(chru, curr_month) * (tavg_f(chru) - &
                              jh_coef_hru(chru)) * swrad(chru) / elh

        if (this%potet(chru) < 0.0) this%potet(chru) = 0.0
      enddo

      this%basin_potet = sum(dble(this%potet * hru_area), mask=active_mask) * basin_area_inv
    end associate
  end subroutine
end submodule
