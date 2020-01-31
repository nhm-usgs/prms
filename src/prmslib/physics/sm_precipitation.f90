submodule(PRMS_PRECIPITATION) sm_precipitation
contains
  module subroutine init_Precipitation(this, ctl_data, model_basin, model_temp, model_summary)
    use prms_constants, only: FAHRENHEIT
    use conversions_mod, only: f_to_c, c_to_f
    implicit none

    class(Precipitation), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj

    ! --------------------------------------------------------------------------
    associate(init_vars_from_file => ctl_data%init_vars_from_file%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              ! rst_unit => ctl_data%restart_output_unit, &
              print_debug => ctl_data%print_debug%value, &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths, &

              temp_units => model_temp%temp_units)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Setup the parameters first
      allocate(this%tmax_allsnow(nhru, nmonths))
      call param_hdl%get_variable('tmax_allsnow', this%tmax_allsnow)

      allocate(this%tmax_allrain_offset(nhru, nmonths))
      call param_hdl%get_variable('tmax_allrain_offset', this%tmax_allrain_offset)

      call param_hdl%get_variable('precip_units', this%precip_units)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Setup other variables
      allocate(this%hru_ppt(nhru))
      allocate(this%hru_rain(nhru))
      allocate(this%hru_snow(nhru))
      allocate(this%prmx(nhru))

      allocate(this%tmax_allrain(nhru, nmonths))
      allocate(this%tmax_allrain_c(nhru, nmonths))
      allocate(this%tmax_allrain_f(nhru, nmonths))
      allocate(this%tmax_allsnow_c(nhru, nmonths))
      allocate(this%tmax_allsnow_f(nhru, nmonths))

      this%hru_ppt = 0.0
      this%hru_rain = 0.0
      this%hru_snow = 0.0
      this%prmx = 0.0

      ! TODO: PAN - these variables don't appear to be used anymore
      ! if (ctl_data%precip_module%values(1)%s == 'precip_laps' .or. &
      !     ctl_data%precip_module%values(1)%s == 'ide_dist' .or. &
      !     ctl_data%precip_module%values(1)%s == 'xyz_dist') then
      !   allocate(this%psta_elev_feet(nrain))
      !   allocate(this%psta_elev_meters(nrain))
      !
      !   if (elev_units == FEET) then
      !     this%psta_elev_feet = psta_elev
      !     this%psta_elev_meters = psta_elev * FEET2METERS
      !   else
      !     this%psta_elev_meters = psta_elev
      !     this%psta_elev_feet = psta_elev * METERS2FEET
      !   endif
      ! endif

      ! ------------------------------------------------------------------------
      ! Set tmax_allrain in units of the input values
      ! tmax_allsnow must be in the units of the input values
      if (temp_units == FAHRENHEIT) then
        this%tmax_allsnow_f = this%tmax_allsnow

        ! NOTE: 2018-07-24 PAN: changed tmax_allsnow_2d to this%tmax_allsnow_f
        !       This resulted in a minor change in value causing a different
        !       branch to be followed in precip_form for 1996-02-22, hru=12,13
        this%tmax_allrain_f = this%tmax_allrain_offset + this%tmax_allsnow_f
        this%tmax_allrain_c = f_to_c(this%tmax_allrain_f)
        this%tmax_allsnow_c = f_to_c(this%tmax_allsnow_f)
        this%tmax_allrain = this%tmax_allrain_f
      else
        ! Celsius
        this%tmax_allsnow_c = this%tmax_allsnow
        this%tmax_allrain_c = this%tmax_allrain_offset + this%tmax_allsnow_c
        this%tmax_allrain_f = c_to_f(this%tmax_allrain)
      endif

      this%has_hru_summary_vars = .false.

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          select case(outVar_names%values(jj)%s)
            case('hru_ppt')
              this%has_hru_summary_vars = .true.
            case('hru_rain')
              this%has_hru_summary_vars = .true.
            case('hru_snow')
              this%has_hru_summary_vars = .true.
            case('prmx')
              call model_summary%set_summary_var(jj, this%prmx)
            case default
              ! pass
          end select
        end do
      end if
    end associate
  end subroutine

  module subroutine run_Precipitation(this, ctl_data, model_basin, model_temp, model_time, model_summary)
    class(Precipitation), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Time_t), intent(in), optional :: model_time
    type(Summary), intent(inout) :: model_summary

    ! --------------------------------------------------------------------------
  end subroutine


  ! module subroutine set_precipitation_form(this, ctl_data, model_basin, model_temp, &
  !                                          month, rain_adj, snow_adj, rainmix_adj)
  !   use prms_constants, only: DNEARZERO, NEARZERO, INCHES, MM, MM2INCH
  !   implicit none

  !   class(Precipitation), intent(inout) :: this
  !   type(Control), intent(in) :: ctl_data
  !   type(Basin), intent(in) :: model_basin
  !   class(Temperature), intent(in) :: model_temp
  !   integer(i32), intent(in) :: month
  !   real(r32), optional, intent(in) :: rain_adj(:, :)
  !     !! Array of rain adjustments
  !   real(r32), optional, intent(in) :: snow_adj(:, :)
  !     !! Array of snow adjustments
  !   real(r32), optional, intent(in) :: rainmix_adj(:, :)
  !     !! Array of rain mixture adjustments

  !   ! Local variables
  !   real(r32) :: tdiff
  !   integer(i32) :: chru
  !   integer(i32) :: ii

  !   ! -------------------------------------------------------------------------
  !   associate(nhru => model_basin%nhru, &
  !             hru_area => model_basin%hru_area, &
  !             active_hrus => model_basin%active_hrus, &
  !             basin_area_inv => model_basin%basin_area_inv, &
  !             hru_route_order => model_basin%hru_route_order, &

  !             tmax => model_temp%tmax, &
  !             tmin => model_temp%tmin, &

  !             ! WARNING: This will change when tmax_allsnow and tmax_allrain
  !             !          are standardized to Celsius.
  !             tmax_allrain => this%tmax_allrain_c, &
  !             tmax_allsnow => this%tmax_allsnow_c)

  !     ! WARNING: 2019-10-31 PAN: this is not handling the optional parameters correctly

  !     ! Basin precipitation before any adjustments
  !     this%basin_obs_ppt = sum(dble(this%hru_ppt * hru_area)) * basin_area_inv

  !     do ii=1, active_hrus
  !       chru = hru_route_order(ii)

  !       if (this%hru_ppt(chru) > 0.0) then
  !         if (tmax(chru) <= tmax_allsnow(chru, month)) then
  !           ! All-snow precipitation event
  !           this%hru_ppt(chru) = this%hru_ppt(chru) * snow_adj(chru, month)
  !           this%hru_snow(chru) = this%hru_ppt(chru)
  !           this%newsnow(chru) = 1
  !         elseif (tmin(chru) > tmax_allsnow(chru, month) .or. tmax(chru) >= tmax_allrain(chru, month)) then
  !           ! All-rain precipitation event
  !           this%hru_ppt(chru) = this%hru_ppt(chru) * rain_adj(chru, month)
  !           this%hru_rain(chru) = this%hru_ppt(chru)
  !           this%prmx(chru) = 1.0
  !         else
  !           ! Mixed rain/snow or all-rain precipitation event

  !           ! WARNING: This would introduce a bias. tdiff values near zero
  !           !          would become warmer than values just above the near zero
  !           !          cutoff. Using max(tdiff, 0.0001) will provide more
  !           !          consistent adjustment for small values.
  !           ! tdiff = tmax(chru) - tmin(chru)
  !           ! if (abs(tdiff) < NEARZERO) then
  !           !   tdiff = 0.0001
  !           ! endif
  !           tdiff = max(tmax(chru) - tmin(chru), 0.0001)

  !           this%prmx(chru) = ((tmax(chru) - tmax_allsnow(chru, month)) / tdiff) * rainmix_adj(chru, month)

  !           ! Make sure prmx is between 0.0 and 1.0
  !           this%prmx(chru) = min(max(0.0, this%prmx(chru)), 1.0)

  !           if (this%prmx(chru) < 1.0) then
  !             ! Mixed precip event
  !             this%pptmix(chru) = 1
  !             this%hru_ppt(chru) = this%hru_ppt(chru) * snow_adj(chru, month)
  !             this%hru_rain(chru) = this%prmx(chru) * this%hru_ppt(chru)
  !             this%hru_snow(chru) = this%hru_ppt(chru) - this%hru_rain(chru)
  !             this%newsnow(chru) = 1
  !           else
  !             ! All-rain event
  !             this%hru_ppt(chru) = this%hru_ppt(chru) * rain_adj(chru, month)
  !             this%hru_rain(chru) = this%hru_ppt(chru)
  !             ! this%prmx(chru) = 1.0
  !           endif
  !         endif
  !       endif
  !     enddo
  !   end associate
  ! end subroutine

module subroutine set_precipitation_form(this, ctl_data, model_basin, model_temp, &
                                           month, rain_adj, snow_adj, rainmix_adj)
    use prms_constants, only: DNEARZERO, NEARZERO, INCHES, MM, MM2INCH
    implicit none

    class(Precipitation), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    integer(i32), intent(in) :: month
    real(r32), optional, intent(in) :: rain_adj(:, :)
      !! Array of rain adjustments
    real(r32), optional, intent(in) :: snow_adj(:, :)
      !! Array of snow adjustments
    real(r32), optional, intent(in) :: rainmix_adj(:, :)
      !! Array of rain mixture adjustments

    ! Local variables
    ! real(r32) :: tdiff
    integer(i32) :: chru
    integer(i32) :: ii

    ! -------------------------------------------------------------------------
    associate(nhru => model_basin%nhru, &
              hru_area => model_basin%hru_area, &
              active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order, &

              tmax => model_temp%tmax, &
              tmin => model_temp%tmin, &

              ! WARNING: This will change when tmax_allsnow and tmax_allrain
              !          are standardized to Celsius.
              tmax_allrain => this%tmax_allrain_c, &
              tmax_allsnow => this%tmax_allsnow_c)

      ! WARNING: 2019-10-31 PAN: this is not handling the optional parameters correctly

      ! tdiff = max(tmax(chru) - tmin(chru), 0.0001)
      this%prmx = ((tmax - tmax_allsnow(:, month)) / max(tmax - tmin, 0.0001)) * rainmix_adj(:, month)

      ! Make sure prmx is between 0.0 and 1.0
      this%prmx = min(max(0.0, this%prmx), 1.0)

      do ii=1, active_hrus
        chru = hru_route_order(ii)

        if (this%hru_ppt(chru) > 0.0) then
          if (tmax(chru) <= tmax_allsnow(chru, month)) then
            ! All-snow precipitation event
            this%hru_ppt(chru) = this%hru_ppt(chru) * snow_adj(chru, month)
            this%hru_snow(chru) = this%hru_ppt(chru)
            ! this%newsnow(chru) = 1
          elseif (tmin(chru) > tmax_allsnow(chru, month) .or. tmax(chru) >= tmax_allrain(chru, month)) then
            ! All-rain precipitation event
            this%hru_ppt(chru) = this%hru_ppt(chru) * rain_adj(chru, month)
            this%hru_rain(chru) = this%hru_ppt(chru)
          else
            ! Mixed rain/snow or all-rain precipitation event
            if (this%prmx(chru) < 1.0) then
              ! Mixed precip event
              this%hru_ppt(chru) = this%hru_ppt(chru) * snow_adj(chru, month)
              this%hru_rain(chru) = this%prmx(chru) * this%hru_ppt(chru)
              this%hru_snow(chru) = this%hru_ppt(chru) - this%hru_rain(chru)
              ! this%newsnow(chru) = 1
              ! this%pptmix(chru) = 1
            else
              ! All-rain event
              this%hru_ppt(chru) = this%hru_ppt(chru) * rain_adj(chru, month)
              this%hru_rain(chru) = this%hru_ppt(chru)
            endif
          endif
        endif
      enddo
    end associate
  end subroutine



  module subroutine set_summary_ptrs(this, ctl_data, model_summary)
    implicit none

    class(Precipitation), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj

    ! --------------------------------------------------------------------------
    associate(outVar_names => ctl_data%outVar_names)

      ! Connect any nhru_summary variables that need to be output
      do jj=1, outVar_names%size()
        select case(outVar_names%values(jj)%s)
          case('hru_ppt')
            call model_summary%set_summary_var(jj, this%hru_ppt)
          case('hru_rain')
            call model_summary%set_summary_var(jj, this%hru_rain)
          case('hru_snow')
            call model_summary%set_summary_var(jj, this%hru_snow)
          case default
            ! pass
        end select
      enddo
    end associate
  end subroutine
end submodule
