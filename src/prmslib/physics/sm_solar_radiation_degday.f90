submodule (SOLAR_RADIATION_DEGDAY) sm_solar_radiation_degday
contains
  !***********************************************************************
  ! Solrad_degday constructor
  module subroutine init_Solrad_degday(this, ctl_data, model_basin, model_summary)
    use UTILS_PRMS, only: print_module_info
    implicit none

    class(Solrad_degday), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%SolarRadiation%init(ctl_data, model_basin, model_summary)

    associate(print_debug => ctl_data%print_debug%value, &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths, &
              active_mask => model_basin%active_mask)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Read parameters
      allocate(this%tmax_index(nhru, nmonths))
      call param_hdl%get_variable('tmax_index', this%tmax_index)

      allocate(this%dday_intcp(nhru, nmonths))
      call param_hdl%get_variable('dday_intcp', this%dday_intcp)

      allocate(this%dday_slope(nhru, nmonths))
      call param_hdl%get_variable('dday_slope', this%dday_slope)

      allocate(this%radadj_intcp(nhru, nmonths))
      call param_hdl%get_variable('radadj_intcp', this%radadj_intcp)

      allocate(this%radadj_slope(nhru, nmonths))
      call param_hdl%get_variable('radadj_slope', this%radadj_slope)

      ! NOTE: Once units are standardized this can go away
      ! allocate(this%tmax_f(nhru))

    end associate
  end subroutine


  module subroutine run_Solrad_degday(this, ctl_data, model_time, model_precip, model_basin, model_temp)
    use conversions_mod, only: c_to_f
    use UTILS_PRMS, only: get_array
    implicit none

    class(Solrad_degday), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    class(Precipitation), intent(in) :: model_precip
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: jj
    integer(i32) :: kk
    integer(i32) :: kp
    integer(i32) :: kp1
    real(r32) :: pptadj
    real(r32) :: radadj
    real(r32) :: dday
    real(r32) :: ddayi

    !***********************************************************************

    associate(print_debug => ctl_data%print_debug%value, &

              curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &

              ! solrad => model_obs%solrad, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths, &
              active_hrus => model_basin%active_hrus, &
              active_mask => model_basin%active_mask, &
              hru_route_order => model_basin%hru_route_order, &

              hru_ppt => model_precip%hru_ppt, &
              tmax_allrain => model_precip%tmax_allrain_f, &

              tmax_f => model_temp%tmax_f)

      ! NOTE: Once units are standardized this can go away
      ! this%tmax_f = c_to_f(model_temp%tmax)

      !rsr using julian day as the soltab arrays are filled by julian day

      do jj = 1, active_hrus
        chru = hru_route_order(jj)

        ! set degree day and radiation adjustment limited by radmax
        dday = this%dday_slope(chru, curr_month) * tmax_f(chru) + &
               this%dday_intcp(chru, curr_month) + 1.0

        if (dday < 1.0) dday = 1.0

        if (dday < 26.0) then
          kp = int(dday)
          ddayi = float(kp)
          kp1 = kp + 1
          radadj = SOLF(kp) + ((SOLF(kp1) - SOLF(kp)) * (dday - ddayi))

          if (radadj > this%radmax(chru, curr_month)) then
            radadj = this%radmax(chru, curr_month)
          endif
        else
          radadj = this%radmax(chru, curr_month)
        endif

        ! Set precipitation adjument factor based on temperature
        ! and amount of precipitation
        pptadj = 1.0

        if (hru_ppt(chru) > this%ppt_rad_adj(chru, curr_month)) then
          if (tmax_f(chru) < this%tmax_index(chru, curr_month)) then
            pptadj = this%radj_sppt(chru)

            ! if (tmax_hru(chru) >= tmax_allrain(chru, curr_month)) then
            if (tmax_f(chru) >= tmax_allrain(chru, curr_month)) then
              if (model_time%Summer_flag == 0) then
                ! Winter
                pptadj = this%radj_wppt(chru)
              endif
            else
              pptadj = this%radj_wppt(chru)
            endif
          else
            pptadj = this%radadj_intcp(chru, curr_month) + &
                     this%radadj_slope(chru, curr_month) * &
                     (tmax_f(chru) - this%tmax_index(chru, curr_month))

            if (pptadj > 1.0) pptadj = 1.0
          endif
        endif

        radadj = radadj * pptadj
        if (radadj < 0.2) radadj = 0.2

        this%orad_hru(chru) = radadj * sngl(this%soltab_horad_potsw(day_of_year, chru))

        if (this%has_hru_obs_station) then
          kk = this%hru_solsta(chru)
          if (kk > 0) then
            if (this%solrad(kk) < 0.0 .or. this%solrad(kk) > 10000.0 ) then
              if (print_debug > -1) then
                print *, 'WARNING, measured solar radiation missing, HRU:', chru, '; station:', kk, '; value computed'
                ! call print_date(1)
              endif
            else
              this%orad_hru(chru) = radadj * sngl(this%soltab_horad_potsw(day_of_year, chru))
              cycle
            endif
          endif
        endif

        this%swrad(chru) = sngl(this%soltab_potsw(day_of_year, chru) / this%soltab_horad_potsw(day_of_year, chru) * &
                                   dble(this%orad_hru(chru)) / this%hru_cossl(chru))
      enddo

    end associate
  end subroutine

end submodule
