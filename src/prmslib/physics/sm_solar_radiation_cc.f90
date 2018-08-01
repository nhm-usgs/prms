submodule(SOLAR_RADIATION_CC) sm_solar_radiation_cc
contains
  module function constructor_Solrad_cc(ctl_data, param_data, model_basin, model_temp) result(this)
    use conversions_mod, only: c_to_f
    use prms_constants, only: dp
    implicit none
    type(Solrad_cc) :: this
      !! Solrad_cc class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Parameters), intent(in) :: param_data
      !! Parameters
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    ! Control
    ! nhru, nsol

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    this%SolarRadiation = SolarRadiation(ctl_data, param_data, model_basin)

    associate(nhru => ctl_data%nhru%value, &
              nsol => ctl_data%nsol%value)
      allocate(this%cloud_radadj(nhru))
      allocate(this%cloud_cover_hru(nhru))

      this%cloud_cover_hru = 0.0
      this%cloud_radadj = 0.0

      this%basin_cloud_cover = 0.0_dp
      this%basin_radadj = 0.0_dp

      ! NOTE: Once units are standardized tmax_f and tmin_f can go away
      this%tmax_f = (c_to_f(model_temp%tmax))
      this%tmin_f = (c_to_f(model_temp%tmin))
    end associate
  end function

  module subroutine run_Solrad_cc(this, ctl_data, param_data, model_time, model_obs, climate, model_basin)
    use prms_constants, only: dp
    implicit none

    class(Solrad_cc), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Time_t), intent(in) :: model_time
    type(Obs), intent(in) :: model_obs
    type(Climateflow), intent(in) :: climate
    type(Basin), intent(in) :: model_basin

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: idx1D
    integer(i32) :: jj
    integer(i32) :: k

    real(r32) :: ccov
    real(r32) :: pptadj
    real(r32) :: radadj

    ! Control
    ! nhru, print_debug

    ! Basin
    ! active_hrus, basin_area_inv, hru_route_order,

    ! Climate
    ! hru_ppt

    ! Obs
    ! solrad,

    ! Parameters
    ! basin_solsta(not included in associate), ccov_slope(2D), ccov_intcp(2D),
    ! crad_coef(2D), crad_exp(2D), hru_area,
    ! hru_solsta, ppt_rad_adj(2D), radj_sppt, radj_wppt, radmax(2D),

    ! Temperature
    ! tmax, tmin

    ! Time_t
    ! day_of_year, Nowmonth, Summer_flag,

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              print_debug => ctl_data%print_debug%value, &

              active_hrus => model_basin%active_hrus, &
              basin_area_inv => model_basin%basin_area_inv, &
              hru_route_order => model_basin%hru_route_order, &

              hru_ppt => climate%hru_ppt, &
              ! tmax_hru => climate%tmax_hru, &
              ! tmin_hru => climate%tmin_hru, &

              solrad => model_obs%solrad, &

              ccov_slope => param_data%ccov_slope%values, &
              ccov_intcp => param_data%ccov_intcp%values, &
              crad_coef => param_data%crad_coef%values, &
              crad_exp => param_data%crad_exp%values, &
              hru_area => param_data%hru_area%values, &
              hru_solsta => param_data%hru_solsta%values, &
              ppt_rad_adj => param_data%ppt_rad_adj%values, &
              radj_sppt => param_data%radj_sppt%values, &
              radj_wppt => param_data%radj_wppt%values, &
              radmax => param_data%radmax%values, &

              ! tmax => model_temp%tmax, &
              ! tmin => model_temp%tmin, &

              curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &
              Summer_flag => model_time%Summer_flag)

      ! rsr using julian day as the soltab arrays are filled by julian day
      this%basin_horad = this%soltab_basinpotsw(day_of_year)
      this%basin_swrad = 0.0_dp
      this%basin_orad = 0.0_dp
      this%basin_radadj = 0.0_dp
      this%basin_cloud_cover = 0.0_dp

      do jj=1, active_hrus
        chru = hru_route_order(jj)

        ! 2D index to 1D
        idx1D = (curr_month - 1) * nhru + chru

        ! determine radiation adjustment due to precipitation
        ! if (hru_ppt(chru) > ppt_rad_adj(chru, curr_month)) then
        if (hru_ppt(chru) > ppt_rad_adj(idx1D)) then
          if (Summer_flag == 1) then
            pptadj = radj_sppt(chru)
          else
            pptadj = radj_wppt(chru) ! Winter
          endif
        else
          pptadj = 1.0
        endif

        ! ccov = ccov_slope(chru, curr_month) * (tmax_hru(chru) - tmin_hru(chru)) + ccov_intcp(chru, curr_month)
        ! ccov = ccov_slope(idx1D) * (tmax_hru(chru) - tmin_hru(chru)) + ccov_intcp(idx1D)
        ! WARNING: ccov_slope and/or ccov_intcp will have to be converted if
        !          tmax and tmin are supplied in degree Celsius.
        ccov = ccov_slope(idx1D) * (this%tmax_f(chru) - this%tmin_f(chru)) + ccov_intcp(idx1D)

        if (ccov < 0.0) then
          ccov = 0.0
        elseif (ccov > 1.0) then
          ccov = 1.0
        endif

        this%cloud_cover_hru(chru) = ccov
        this%basin_cloud_cover = this%basin_cloud_cover + dble(ccov * hru_area(chru))

        ! radadj = crad_coef(chru, curr_month) + (1.0 - crad_coef(chru, curr_month)) * &
        !          ((1.0 - this%cloud_cover_hru(chru))**crad_exp(chru, curr_month))
        ! if (radadj > radmax(chru, curr_month)) radadj = radmax(chru, curr_month)
        radadj = crad_coef(idx1D) + (1.0 - crad_coef(idx1D)) * &
                 ((1.0 - this%cloud_cover_hru(chru))**crad_exp(idx1D))
        if (radadj > radmax(idx1D)) radadj = radmax(idx1D)

        this%cloud_radadj(chru) = radadj * pptadj
        this%basin_radadj = this%basin_radadj + dble(this%cloud_radadj(chru) * hru_area(chru))

        this%orad_hru(chru) = this%cloud_radadj(chru) * sngl(this%soltab_horad_potsw(day_of_year, chru))
        this%basin_orad = this%basin_orad + dble(this%orad_hru(chru) * hru_area(chru))

        if (this%has_hru_obs_station) then
          k = hru_solsta(chru)

          if (k > 0) then
            if (solrad(k) < 0.0 .or. solrad(k) > 10000.0) then
              if (print_debug > -1) then
                print *, 'WARNING, measured solar radiation missing, HRU:', chru, '; station:', k, '; value computed'
                ! call print_date(1)
              endif
            else
              this%swrad(chru) = solrad(k) * this%radiation_cv_factor
              this%basin_swrad = this%basin_swrad + dble(this%swrad(chru) * hru_area(chru))
              cycle
            endif
          endif
        endif

        this%swrad(chru) = sngl(this%soltab_potsw(day_of_year, chru) * dble(this%cloud_radadj(chru)) / this%hru_cossl(chru))
        this%basin_swrad = this%basin_swrad + dble(this%swrad(chru) * hru_area(chru))
      enddo

      this%basin_orad = this%basin_orad * basin_area_inv
      this%basin_radadj = this%basin_radadj * basin_area_inv

      if (this%has_basin_obs_station) then
        this%orad = solrad(param_data%basin_solsta%values(1)) * this%radiation_cv_factor
      else
        this%orad = sngl(this%basin_orad)
      endif

      this%basin_swrad = this%basin_swrad * basin_area_inv
      this%basin_potsw = this%basin_swrad
      this%basin_cloud_cover = this%basin_cloud_cover * basin_area_inv
    end associate
  end subroutine

end submodule
