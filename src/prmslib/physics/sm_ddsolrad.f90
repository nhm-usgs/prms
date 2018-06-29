submodule (PRMS_DDSOLRAD) sm_ddsolrad
contains
  !***********************************************************************
  ! Ddsolrad constructor
  module function constructor_Ddsolrad(ctl_data, param_data, model_basin) result(this)
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Ddsolrad) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin

    ! ------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              active_mask => model_basin%active_mask)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! If no radiation stations are available for the active HRUs
      ! then the sum of hru_solsta is 0
      if (param_data%hru_solsta%exists()) then
        this%has_obs_station = sum(param_data%hru_solsta%values, mask=active_mask) > 0
      else
        this%has_obs_station = .false.
      endif

      this%radiation_cv_factor = 1.0
      if (param_data%rad_conv%exists()) then
        this%radiation_cv_factor = param_data%rad_conv%values(1)
      endif
    end associate
  end function


  module subroutine run_Ddsolrad(this, ctl_data, param_data, model_time, model_obs, solt, climate, model_basin)
    use UTILS_PRMS, only: get_array
    implicit none

    class(Ddsolrad), intent(in) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Time_t), intent(in) :: model_time
    type(Obs), intent(in) :: model_obs
    type(Soltab), intent(in) :: solt
    type(Climateflow), intent(inout) :: climate
    type(Basin), intent(inout) :: model_basin

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

    real(r32), pointer :: dday_slope_2d(:,:)
    real(r32), pointer :: dday_intcp_2d(:,:)
    real(r32), pointer :: ppt_rad_adj_2d(:,:)
    real(r32), pointer :: radmax_2d(:,:)
    real(r32), pointer :: radadj_intcp_2d(:,:)
    real(r32), pointer :: radadj_slope_2d(:,:)
    real(r32), pointer :: tmax_index_2d(:,:)

    ! Obs
    ! solrad,

    ! Soltad
    ! hru_cossl, soltab_basinpotsw, soltab_horad_potsw, soltab_potsw,

    !***********************************************************************

    associate(nhru => ctl_data%nhru%value, &
              nsol => ctl_data%nsol%value, &
              nmonths => ctl_data%nmonths%value, &
              print_debug => ctl_data%print_debug%value, &
              curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &
              solrad => model_obs%solrad, &
              active_hrus => model_basin%active_hrus, &
              active_mask => model_basin%active_mask, &
              basin_area_inv => model_basin%basin_area_inv, &
              hru_route_order => model_basin%hru_route_order, &
              basin_orad => climate%basin_orad, &
              basin_potsw => climate%basin_potsw, &
              basin_swrad => climate%basin_swrad, &
              orad => climate%orad, &
              swrad => climate%swrad, &
              ! basin_solsta => param_data%basin_solsta%values(1), &
              hru_area => param_data%hru_area%values, &
              hru_solsta => param_data%hru_solsta%values, &
              ! rad_conv => param_data%rad_conv%values(1), &
              radj_sppt => param_data%radj_sppt%values, &
              radj_wppt => param_data%radj_wppt%values)

      ! WARNING: Get pointers to 2D-indexed versions of 1D parameter arrays
      dday_intcp_2d => get_array(param_data%dday_intcp%values, (/nhru, nmonths/))
      dday_slope_2d => get_array(param_data%dday_slope%values, (/nhru, nmonths/))
      ppt_rad_adj_2d => get_array(param_data%ppt_rad_adj%values, (/nhru, nmonths/))
      radmax_2d => get_array(param_data%radmax%values, (/nhru, nmonths/))
      radadj_intcp_2d => get_array(param_data%radadj_intcp%values, (/nhru, nmonths/))
      radadj_slope_2d => get_array(param_data%radadj_slope%values, (/nhru, nmonths/))
      tmax_index_2d => get_array(param_data%tmax_index%values, (/nhru, nmonths/))

      !rsr using julian day as the soltab arrays are filled by julian day
      climate%basin_horad = solt%soltab_basinpotsw(day_of_year)

      do jj = 1, active_hrus
        chru = hru_route_order(jj)

        ! set degree day and radiation adjustment limited by radmax
        dday = dday_slope_2d(chru, curr_month) * climate%tmax_hru(chru) + &
               dday_intcp_2d(chru, curr_month) + 1.0

        if (dday < 1.0) dday = 1.0

        if (dday < 26.0) then
          kp = int(dday)
          ddayi = float(kp)
          kp1 = kp + 1
          radadj = SOLF(kp) + ((SOLF(kp1) - SOLF(kp)) * (dday - ddayi))

          if (radadj > radmax_2d(chru, curr_month)) then
            radadj = radmax_2d(chru, curr_month)
          endif
        else
          radadj = radmax_2d(chru, curr_month)
        endif

        ! Set precipitation adjument factor based on temperature
        ! and amount of precipitation
        pptadj = 1.0

        if (climate%hru_ppt(chru) > ppt_rad_adj_2d(chru, curr_month)) then
          if (climate%tmax_hru(chru) < tmax_index_2d(chru, curr_month)) then
            pptadj = radj_sppt(chru)

            if (climate%tmax_hru(chru) >= climate%tmax_allrain(chru, curr_month)) then
              if (model_time%Summer_flag == 0) then
                ! Winter
                pptadj = radj_wppt(chru)
              endif
            else
              pptadj = radj_wppt(chru)
            endif
          else
            pptadj = radadj_intcp_2d(chru, curr_month) + &
                     radadj_slope_2d(chru, curr_month) * (climate%tmax_hru(chru) - tmax_index_2d(chru, curr_month))

            if (pptadj > 1.0) pptadj = 1.0
          endif
        endif

        radadj = radadj * pptadj
        if (radadj < 0.2) radadj = 0.2

        climate%orad_hru(chru) = radadj * sngl(solt%soltab_horad_potsw(day_of_year, chru))

        if (this%has_obs_station) then
          kk = hru_solsta(chru)
          if (kk > 0) then
            if (solrad(kk) < 0.0 .or. solrad(kk) > 10000.0 ) then
              if (print_debug > -1) then
                print *, 'WARNING, measured solar radiation missing, HRU:', chru, '; station:', kk, '; value computed'
                ! call print_date(1)
              endif
            else
              swrad(chru) = solrad(kk) * this%radiation_cv_factor
              ! basin_swrad = basin_swrad + dble(swrad(j) * hru_area(j))
              cycle
            endif
          endif
        endif

        climate%swrad(chru) = sngl(solt%soltab_potsw(day_of_year, chru) / solt%soltab_horad_potsw(day_of_year, chru) * &
                                   dble(climate%orad_hru(chru)) / solt%hru_cossl(chru))
      enddo

      climate%basin_orad = sum(dble(climate%orad_hru * hru_area), mask=active_mask) * basin_area_inv

      if (this%has_obs_station) then
      ! if (nsol > 0 .and. basin_solsta > 0) then
        orad = solrad(param_data%basin_solsta%values(1)) * this%radiation_cv_factor
      else
        orad = sngl(basin_orad)
      endif

      climate%basin_swrad = sum(dble(climate%swrad * hru_area), mask=active_mask) * basin_area_inv
      basin_potsw = basin_swrad
    end associate
  end subroutine

end submodule
