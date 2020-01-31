submodule(PRMS_POTET_PM) sm_potet_pm
contains
  module subroutine init_Potet_pm(this, ctl_data, model_basin, model_summary)
    use UTILS_CBH, only: find_current_time, find_header_end
    implicit none

    class(Potet_pm), intent(inout) :: this
      !! Poteh_pm class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: ierr
    integer(i32) :: istop
    integer(i32) :: jj

    ! Control
    ! nhru, cbh_binary_flag, start_time, windspeed_day

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Potential_ET%init(ctl_data, model_basin, model_summary)
    ! this%Potential_ET = Potential_ET(ctl_data, model_summary)

    associate(nhru => model_basin%nhru%value, &

              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              windspeed_day => ctl_data%windspeed_day%values(1))

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%tempc_dewpt(nhru))
      allocate(this%vp_actual(nhru))
      allocate(this%vp_sat(nhru))
      allocate(this%vp_slope(nhru))
      allocate(this%lwrad_net(nhru))
      allocate(this%windspeed_hru(nhru))

      this%tempc_dewpt = 0.0
      this%vp_actual = 0.0
      this%lwrad_net = 0.0
      this%vp_slope = 0.0
      this%vp_sat = 0.0
      this%windspeed_hru = 0.0

      call find_header_end(nhru, this%windspeed_funit, ierr, windspeed_day%s, &
                           'windspeed_day', (cbh_binary_flag==1))

      if (ierr == 1) then
        istop = 1
      else
        call find_current_time(ierr, this%windspeed_funit, start_time, (cbh_binary_flag==1))
      endif

      ! allocate(this%basin_windspeed)

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          select case(outVar_names%values(jj)%s)
            ! case('basin_windspeed')
            !   call model_summary%set_summary_var(jj, this%basin_windspeed)
            case('tempc_dewpt')
              call model_summary%set_summary_var(jj, this%tempc_dewpt)
            case('vp_actual')
              call model_summary%set_summary_var(jj, this%vp_actual)
            case('vp_sat')
              call model_summary%set_summary_var(jj, this%vp_sat)
            case('vp_slope')
              call model_summary%set_summary_var(jj, this%vp_slope)
            case('windspeed_hru')
              call model_summary%set_summary_var(jj, this%windspeed_hru)
            case default
              ! pass
          end select
        enddo
      endif
    end associate
  end subroutine

  module subroutine run_Potet_pm(this, ctl_data, model_basin, model_temp, model_time, model_solrad)
    use conversions_mod, only: sat_vapor_press
    implicit none

    class(Potet_pm), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Time_t), intent(in) :: model_time
    class(SolarRadiation), intent(in) :: model_solrad

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: idx1D
      !! 1D index from 2D
    integer(i32) :: ios
    integer(i32) :: j
    integer(i32) :: jj
    integer(i32) :: yr, mo, dy, hr, mn, sec

    real(r32) :: A1
    real(r32) :: B1
    real(r32) :: a
    real(r32) :: b
    real(r32) :: c
    real(r32) :: den
    real(r32) :: elh
    real(r32) :: heat_flux
    real(r32) :: net_rad
    real(r32) :: num
    real(r32) :: prsr
    real(r32) :: psycnst
    real(r32) :: stab
    real(r32) :: sw
    real(r32) :: t1
    real(r32) :: vp_deficit

    ! Control
    ! nhru, stream_temp_flag, strmtemp_humidity_flag,

    ! Basin
    ! active_hrus, active_mask, basin_area_inv, hru_route_order,

    ! Climate
    ! tavgc, tmaxc, tminc,

    ! Parameters
    ! crop_coef, hru_area, hru_elev, pm_d_coef, pm_n_coef,

    ! SolarRadiation
    ! soltab_potsw, swrad,

    ! Time_t
    ! day_of_year (day_of_year), curr_month (curr_month),

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value, &

              active_hrus => model_basin%active_hrus, &
              active_mask => model_basin%active_mask, &
              basin_area_inv => model_basin%basin_area_inv, &
              hru_route_order => model_basin%hru_route_order, &

              tavg => model_temp%tavg, &
              tmax => model_temp%tmax, &
              tmin => model_temp%tmin, &

              ! crop_coef => param_data%crop_coef%values, &
              ! hru_area => param_data%hru_area%values, &
              ! hru_elev => param_data%hru_elev%values, &
              ! pm_d_coef => param_data%pm_d_coef%values, &
              ! pm_n_coef => param_data%pm_n_coef%values, &

              soltab_potsw => model_solrad%soltab_potsw, &
              swrad => model_solrad%swrad, &

              day_of_year => model_time%day_of_year, &
              curr_month => model_time%Nowmonth)

      ! WARNING: By definition in call_modules this can't happen if using potet_pm
      ! if (Humidity_cbh_flag == 0) then
      !   this%humidity_hru = Humidity_percent(1, curr_month)
      ! endif

      read(this%windspeed_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%windspeed_hru(jj), jj=1, nhru)

      do j=1, active_hrus
        chru = hru_route_order(j)
        idx1D = (curr_month - 1) * nhru + chru

        ! ATMOSPHERIC PRESSURE FOR ALTITUDE, KPA:
        ! prsr = 101.3 - 0.003215*Hru_elev_feet(chru)
        prsr = 101.3 * (((293.0 - 0.0065 * hru_elev(chru)) / 293.0)**5.26)

        ! LATENT HEAT OF VAPORIZATION AT AVG TEMPERATURE, CAL/GRAM:
        ! elh = 597.3 - 0.5653*tavgc(chru) ! same as potet_jh

        ! LATENT HEAT OF VAPORIZATION AT AVG TEMPERATURE, JOULES/GRAM:
        ! elh = (597.3 - 0.5653 * sngl(tavg(chru))) * 4.184
        elh = (597.3 - 0.5653 * tavg(chru)) * 4.184
        ! elh = 2501.0 - 2.361*tavgc(chru)
        ! elh = 2500.8 - 2.36*tavgc(chru) + 0.0016*tavgc(chru)**2 - 0.00006*tavgc(chru)**3

        ! PSCHOMETRIC CONSTANT AT AVG TEMPERATURE FOR ALTITUDE, KPA:
        ! psycnst = 1.6286*prsr/(elh*4.184) ! 4.184 converts CAL to JOULES
        ! psychrometric constant, kilopascals per degrees C
        ! atmospheric pressure for altitude, kPa
        ! Cp = 1.005 approximate specific heat capacity of air at 20 degrees C, increases with temp
        ! MW = 0.622 = molecular weight of water
        ! 1.615755627 = Cp / MW
        psycnst = 1.615755627 * prsr / elh

        ! heat flux density to the ground,  MJ / m2 / day
        ! heat_flux = -4.2 * (Tavgc_ante(chru)-tavgc(chru)) ! could use solrad_tmax or running avg instead of Tavgc_ante
        heat_flux = 0.0 ! Irmak and others (2012) says equal to zero for daily time step ! G

        ! Dew point temperature (Lawrence(2005) eqn. 8), degrees C
        ! humidity_hru is input as percent so divided by 100 to be in units of decimal fraction
        A1 = 17.625
        B1 = 243.04
        t1 = A1 * tavg(chru) / (B1 + tavg(chru))
        num = B1 * (log(this%humidity_hru(chru) / 100.0) + t1)
        den = A1 - log(this%humidity_hru(chru) / 100.0) - t1
        this%tempc_dewpt(chru) = num / den

        ! Actual vapor pressure (Irmak eqn. 12), KPA
        ! divide by 10 to convert millibar to kpa
        this%vp_actual(chru) = sat_vapor_press(this%tempc_dewpt(chru)) / 10.0

        ! SATURATION VAPOR PRESSURE AT AVG TEMP, KILOPASCALS (KPA):
        ! divide by 10 to convert millibar to kpa
        this%vp_sat(chru) = sat_vapor_press(tavg(chru)) / 10.0

        ! saturation vapor pressure deficit
        vp_deficit = this%vp_sat(chru) - this%vp_actual(chru)

        ! SLOPE OF SATURATION VAPOR PRESSURE CURVE AT AVG TEMP, KPA/DEG(CELSIUS), Irmak 2012, eqn 18
        this%vp_slope(chru) = 4098.0 * this%vp_sat(chru) / ((tavg(chru) + 237.3)**2) ! delta

        ! The long wave equation uses the soltab values in the denominator. There
        ! are cases when soltab is zero for certain HRUs (depending on slope/aspect)
        ! for certain months. If this value is zero, reset it to a small value so
        ! there is no divide by zero.
        if (soltab_potsw(day_of_year, chru) <= 10.0) then
          stab = 10.0
        else
          stab = sngl(soltab_potsw(day_of_year, chru))
        endif

        if (swrad(chru) <= 10.0) then
          sw = 10.5
        else
          sw = swrad(chru)
        endif

        ! Net long wave rediation (Irmak eqn. 10) MJ / m2/ day
        ! 4.903E-09 = Stefan-Boltzmann constant
        ! this%lwrad_net(chru) = 4.903E-09 * ((sngl(tmax(chru) + 273.16)**4 + sngl(tmin(chru) + 273.16)**4) / 2.0 ) &
        !                * (0.34 - 0.14 * (this%vp_actual(chru)**0.5)) * (((1.35 * sw) / stab) - 0.35)
        this%lwrad_net(chru) = 4.903E-09 * (((tmax(chru) + 273.16)**4 + (tmin(chru) + 273.16)**4) / 2.0 ) &
                               * (0.34 - 0.14 * (this%vp_actual(chru)**0.5)) * (((1.35 * sw) / stab) - 0.35)


        ! Net radiation (Irmak eqn. 8) MJ / m2 / day
        ! 1 Langley = 0.04184 MJ/m2
        net_rad = swrad(chru) * 0.04184 - this%lwrad_net(chru)

        a = this%vp_slope(chru) * (net_rad - heat_flux) / elh * 1000.0
        ! b = psycnst * pm_n_coef(chru, curr_month) * this%windspeed_hru(chru) * vp_deficit / (tavgc(chru) + 273.0)
        ! c = (this%vp_slope(chru) + psycnst * (1.0 + pm_d_coef(chru, curr_month) * this%windspeed_hru(chru)))
        b = psycnst * pm_n_coef(idx1D) * this%windspeed_hru(chru) * vp_deficit / (tavg(chru) + 273.0)
        c = (this%vp_slope(chru) + psycnst * (1.0 + pm_d_coef(idx1D) * this%windspeed_hru(chru)))

        !  PM equation with crop_coef in mm/day
        !          this%potet(chru) = (a + b)/c
        ! this%potet(chru) = crop_coef(chru, curr_month) * (a + b) / c
        this%potet(chru) = crop_coef(idx1D) * (a + b) / c
        this%potet(chru) = this%potet(chru) / 25.4

        ! may be able to use intrinsic ISNAN
        !          if (potet(chru) .ne. potet(chru)) then
        !             print *, "potet NaN", potet(chru)
        !          end if

        if (this%potet(chru) < 0.0) this%potet(chru) = 0.0
        ! Tavgc_ante(chru) = tavgc(chru)
      enddo

      ! this%basin_potet = sum(dble(this%potet * hru_area), mask=active_mask) * basin_area_inv
      ! this%basin_windspeed = sum(dble(this%windspeed_hru * hru_area), mask=active_mask) * basin_area_inv

      ! NOTE: basin_humidity is totaled in Potential_ET parent class
      ! this%basin_humidity = sum(dble(this%humidity_hru * hru_area), mask=active_mask) * basin_area_inv
    end associate
  end subroutine

end submodule
