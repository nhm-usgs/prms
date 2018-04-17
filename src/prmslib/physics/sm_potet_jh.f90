submodule (PRMS_POTET_JH) sm_potet_jh
contains
  !***********************************************************************
  ! Potet_jh constructor
  module function constructor_Potet_jh(ctl_data) result(this)
    ! use Control_class, only: Control
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Potet_jh) :: this
    type(Control), intent(in) :: ctl_data

    ! ------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value)
      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif
    end associate
  end function


  module subroutine run_Potet_jh(this, ctl_data, param_data, model_basin, model_time, climate)
    use UTILS_PRMS, only: get_array
    implicit none

    class(Potet_jh), intent(in) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    type(Climateflow), intent(inout) :: climate

    ! Functions
    INTRINSIC DBLE

    ! Local Variables
    ! integer(i32) :: chru
      !! Current HRU
    ! integer(i32) :: j
      !! Loop variable
    ! integer(i32) :: idx1D
      !! 1D index from 2D
    ! real(r32) :: elh
      !! Latent heat of vaporization
    real(r32), pointer :: jh_coef_2d(:,:)

    !***********************************************************************
    ! 597.3 cal/gm at 0 C is the energy required to change the state of
    ! water to vapor
    ! elh is the latent heat of vaporization (not including the *2.54)

    associate(nhru => ctl_data%nhru%values(1), &
              nmonths => ctl_data%nmonths%values(1), &
              curr_month => model_time%Nowmonth, &
              active_mask => model_basin%active_mask, &
              basin_area_inv => model_basin%basin_area_inv, &
              ! jh_coef => param_data%jh_coef%values, &
              jh_coef_hru => param_data%jh_coef_hru%values, &
              hru_area => param_data%hru_area%values)

      jh_coef_2d => get_array(param_data%jh_coef%values, (/nhru, nmonths/))
      ! climate%basin_potet = 0.0

      climate%potet = jh_coef_2d(:, curr_month) * (climate%tavgf - jh_coef_hru) * climate%swrad / ((597.3 - (0.5653 * climate%tavgc)) * 2.54)
      where (climate%potet < 0.0) climate%potet = 0.0

      climate%basin_potet = sum(dble(climate%potet * hru_area), mask=active_mask) * basin_area_inv
      
      ! do j = 1, model_basin%active_hrus
      !   chru = model_basin%hru_route_order(j)
      !   ! idx1D = (curr_month - 1) * ctl_data%nhru%values(1) + chru
      !
      !   elh = (597.3 - (0.5653 * climate%tavgc(chru))) * 2.54
      !   climate%potet(chru) = jh_coef_2d(chru, curr_month) * (climate%tavgf(chru) - &
      !                         jh_coef_hru(chru)) * climate%swrad(chru) / elh
      !
      !   if (climate%potet(chru) < 0.0) climate%potet(chru) = 0.0
      !
      !   ! climate%basin_potet = climate%basin_potet + DBLE(climate%potet(chru) * hru_area(chru))
      ! enddo

      ! climate%basin_potet = climate%basin_potet * model_basin%basin_area_inv
    end associate
  end subroutine
end submodule
