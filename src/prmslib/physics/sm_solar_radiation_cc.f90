submodule(SOLAR_RADIATION_CC) sm_solar_radiation_cc
contains
  module subroutine init_Solrad_cc(this, ctl_data, model_basin, model_temp, model_summary)
    use conversions_mod, only: c_to_f
    use prms_constants, only: dp
    implicit none

    class(Solrad_cc), intent(inout) :: this
      !! Solrad_cc class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%SolarRadiation%init(ctl_data, model_basin, model_summary)

    associate(nhru => ctl_data%nhru%value, &
              nsol => ctl_data%nsol%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              print_debug => ctl_data%print_debug%value)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%cloud_radadj(nhru))
      allocate(this%cloud_cover_hru(nhru))

      this%cloud_cover_hru = 0.0
      this%cloud_radadj = 0.0

      ! NOTE: Once units are standardized tmax_f and tmin_f can go away
      this%tmax_f = (c_to_f(model_temp%tmax))
      this%tmin_f = (c_to_f(model_temp%tmin))

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          select case(outVar_names%values(jj)%s)
            case('cloud_radadj')
              call model_summary%set_summary_var(jj, this%cloud_radadj)
            case('cloud_cover_hru')
              call model_summary%set_summary_var(jj, this%cloud_cover_hru)
            case default
              ! pass
          end select
        enddo
      endif
    end associate
  end subroutine

  module subroutine run_Solrad_cc(this, ctl_data, model_time, model_obs, model_precip, model_basin)
    use prms_constants, only: dp
    implicit none

    class(Solrad_cc), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Obs), intent(in) :: model_obs
    class(Precipitation), intent(in) :: model_precip
    type(Basin), intent(in) :: model_basin

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: idx1D
    integer(i32) :: jj
    integer(i32) :: k

    real(r32) :: ccov
    real(r32) :: pptadj
    real(r32) :: radadj

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              print_debug => ctl_data%print_debug%value, &

              active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order, &

              hru_ppt => model_precip%hru_ppt, &

              solrad => model_obs%solrad, &

              ! The following 4 are specific to solrad_cc
              ccov_slope => param_data%ccov_slope%values, &
              ccov_intcp => param_data%ccov_intcp%values, &
              crad_coef => param_data%crad_coef%values, &
              crad_exp => param_data%crad_exp%values, &

              curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &
              Summer_flag => model_time%Summer_flag)

      ! rsr using julian day as the soltab arrays are filled by julian day

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

        ! radadj = crad_coef(chru, curr_month) + (1.0 - crad_coef(chru, curr_month)) * &
        !          ((1.0 - this%cloud_cover_hru(chru))**crad_exp(chru, curr_month))
        ! if (radadj > radmax(chru, curr_month)) radadj = radmax(chru, curr_month)
        radadj = crad_coef(idx1D) + (1.0 - crad_coef(idx1D)) * &
                 ((1.0 - this%cloud_cover_hru(chru))**crad_exp(idx1D))
        if (radadj > radmax(idx1D)) radadj = radmax(idx1D)

        this%cloud_radadj(chru) = radadj * pptadj
        this%orad_hru(chru) = this%cloud_radadj(chru) * sngl(this%soltab_horad_potsw(day_of_year, chru))

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
              cycle
            endif
          endif
        endif

        this%swrad(chru) = sngl(this%soltab_potsw(day_of_year, chru) * dble(this%cloud_radadj(chru)) / this%hru_cossl(chru))
      enddo

      if (this%has_basin_obs_station) then
        this%orad = solrad(param_data%basin_solsta%values(1)) * this%radiation_cv_factor
      else
        this%orad = sngl(this%basin_orad)
      endif

    end associate
  end subroutine

end submodule
