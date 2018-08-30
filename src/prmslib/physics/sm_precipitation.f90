submodule(PRMS_PRECIPITATION) sm_precipitation
contains
  module function constructor_Precipitation(ctl_data, param_data) result(this)
    use prms_constants, only: FAHRENHEIT
    use conversions_mod, only: f_to_c, c_to_f
    implicit none

    type(Precipitation) :: this
      !! Precipitation class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Parameters), intent(in) :: param_data

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &
              nrain => ctl_data%nrain%value, &
              ntemp => ctl_data%ntemp%value, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              rst_unit => ctl_data%restart_output_unit, &
              print_debug => ctl_data%print_debug%value, &

              elev_units => param_data%elev_units%values(1), &
              tmax_allsnow => param_data%tmax_allsnow%values, &
              tmax_allrain_offset => param_data%tmax_allrain_offset%values)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%hru_ppt(nhru))
      allocate(this%hru_rain(nhru))
      allocate(this%hru_snow(nhru))
      allocate(this%prmx(nhru))

      allocate(this%tmax_allrain(nhru, 12))
      allocate(this%tmax_allrain_c(nhru, 12))
      allocate(this%tmax_allrain_f(nhru, 12))
      allocate(this%tmax_allsnow_c(nhru, 12))
      allocate(this%tmax_allsnow_f(nhru, 12))

      allocate(this%newsnow(nhru))
      allocate(this%pptmix(nhru))

      this%hru_ppt = 0.0
      this%hru_rain = 0.0
      this%hru_snow = 0.0
      this%prmx = 0.0
      this%pptmix = 0
      this%newsnow = 0

      ! ------------------------------------------------------------------------
      ! Set tmax_allrain in units of the input values
      ! tmax_allsnow must be in the units of the input values
      if (param_data%temp_units%values(1) == FAHRENHEIT) then
        ! TODO: remove reshape and use a pointer
        this%tmax_allsnow_f = reshape(tmax_allsnow, shape(this%tmax_allsnow_f))

        ! NOTE: 2018-07-24 PAN: changed tmax_allsnow_2d to this%tmax_allsnow_f
        !       This resulted in a minor change in value causing a different
        !       branch to be followed in precip_form for 1996-02-22, hru=12,13
        ! this%tmax_allrain_f = this%tmax_allsnow_f + tmax_allrain_offset_2d
        ! this%tmax_allrain_f = this%tmax_allsnow_f + reshape(tmax_allrain_offset, shape(this%tmax_allrain_f))
        ! this%tmax_allrain_f = reshape(tmax_allsnow + tmax_allrain_offset, shape(this%tmax_allrain_f))

        this%tmax_allrain_f = reshape(tmax_allrain_offset, shape(this%tmax_allrain_f))
        this%tmax_allrain_f = this%tmax_allrain_f + this%tmax_allsnow_f

        this%tmax_allrain_c = f_to_c(this%tmax_allrain_f)
        this%tmax_allsnow_c = f_to_c(this%tmax_allsnow_f)

        this%tmax_allrain = this%tmax_allrain_f
      else
        ! Celsius
        ! TODO: remove reshape and use a pointer
        this%tmax_allsnow_c = reshape(tmax_allsnow, shape(this%tmax_allsnow_c))

        ! this%tmax_allsnow_f = c_to_f(tmax_allsnow_2d)

        ! this%tmax_allrain = tmax_allsnow_2d + tmax_allrain_offset_2d
        this%tmax_allrain_c = this%tmax_allrain
        this%tmax_allrain_f = c_to_f(this%tmax_allrain)
      endif
    end associate
  end function


  module subroutine run_Precipitation(this, ctl_data, param_data, model_basin, model_temp, model_time)
    class(Precipitation), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Time_t), intent(in), optional :: model_time

    ! --------------------------------------------------------------------------
  end subroutine


  module subroutine set_precipitation_form(this, ctl_data, param_data, model_basin, model_temp, &
                                           month, rain_adj, snow_adj, rainmix_adj)
    use prms_constants, only: DNEARZERO, NEARZERO, INCHES, MM, MM2INCH
    implicit none

    class(Precipitation), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    integer(i32), intent(in) :: month
    real(r32), optional, intent(in) :: rain_adj(:)
      !! Array of rain adjustments
    real(r32), optional, intent(in) :: snow_adj(:)
      !! Array of snow adjustments
    real(r32), optional, intent(in) :: rainmix_adj(:)
      !! Array of rain mixture adjustments

    ! Local variables
    ! real(r32), allocatable :: tdiff_arr(:)
      !! Array containing differences b/t tmaxf and tminf
    real(r32) :: tdiff

    integer(i32) :: chru
    integer(i32) :: ii

    ! Control
    ! nhru,

    ! Parameters
    ! hru_area, precip_units,

    ! Basin
    ! active_hrus, basin_area_inv, hru_route_order,

    ! Temperature
    ! tmax, tmin

    ! -------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &

              active_hrus => model_basin%active_hrus, &
              basin_area_inv => model_basin%basin_area_inv, &
              hru_route_order => model_basin%hru_route_order, &

              hru_area => param_data%hru_area%values, &
              precip_units => param_data%precip_units%values(1), &

              tmax => model_temp%tmax, &
              tmin => model_temp%tmin, &

              ! WARNING: This will change when tmax_allsnow and tmax_allrain
              !          are standardized to Celsius.
              tmax_allrain => this%tmax_allrain_c, &
              tmax_allsnow => this%tmax_allsnow_c)

      ! Basin precipitation before any adjustments
      this%basin_obs_ppt = sum(dble(this%hru_ppt * hru_area)) * basin_area_inv

      do ii=1, active_hrus
        chru = hru_route_order(ii)

        if (this%hru_ppt(chru) > 0.0) then

          if (tmax(chru) <= tmax_allsnow(chru, month)) then
            ! All-snow precipitation event
            this%hru_ppt(chru) = this%hru_ppt(chru) * snow_adj(chru)
            this%hru_snow(chru) = this%hru_ppt(chru)
            this%newsnow(chru) = 1
          elseif (tmin(chru) > tmax_allsnow(chru, month) .or. tmax(chru) >= tmax_allrain(chru, month)) then
            ! All-rain precipitation event
            this%hru_ppt(chru) = this%hru_ppt(chru) * rain_adj(chru)
            this%hru_rain(chru) = this%hru_ppt(chru)
            this%prmx(chru) = 1.0
          else
            ! Mixed rain/snow or all-rain precipitation event

            ! WARNING: This would introduce a bias. tdiff values near zero
            !          would become warmer than values just above the near zero
            !          cutoff. Using max(tdiff, 0.0001) will provide more
            !          consistent adjustment for small values.
            ! tdiff = tmax(chru) - tmin(chru)
            ! if (abs(tdiff) < NEARZERO) then
            !   tdiff = 0.0001
            ! endif

            tdiff = max(tmax(chru) - tmin(chru), 0.0001)

            this%prmx(chru) = ((tmax(chru) - tmax_allsnow(chru, month)) / tdiff) * rainmix_adj(chru)
            this%prmx(chru) = max(this%prmx(chru), 0.0)
            ! if (this%prmx(chru) < 0.0) then
            !   this%prmx(chru) = 0.0
            ! endif

            if (this%prmx(chru) < 1.0) then
              ! Mixed precip event
              this%pptmix(chru) = 1
              this%hru_ppt(chru) = this%hru_ppt(chru) * snow_adj(chru)
              this%hru_rain(chru) = this%prmx(chru) * this%hru_ppt(chru)
              this%hru_snow(chru) = this%hru_ppt(chru) - this%hru_rain(chru)
              this%newsnow(chru) = 1
            else
              ! All-rain event
              this%hru_ppt(chru) = this%hru_ppt(chru) * rain_adj(chru)
              this%hru_rain(chru) = this%hru_ppt(chru)
              this%prmx(chru) = 1.0
            endif
          endif
        endif
      enddo

      this%basin_ppt = sum(dble(this%hru_ppt * hru_area)) * basin_area_inv
      this%basin_rain = sum(dble(this%hru_rain * hru_area)) * basin_area_inv
      this%basin_snow = sum(dble(this%hru_snow * hru_area)) * basin_area_inv
    end associate
  end subroutine

end submodule
