submodule (PRMS_CLIMATEVARS) sm_climateflow

contains
  !***********************************************************************
  ! Climateflow constructor
  module function constructor_Climateflow(ctl_data, param_data) result(this)
    use conversions_mod, only: c_to_f, f_to_c
    use UTILS_PRMS, only: check_restart, get_array, print_module_info
    implicit none

    type(Climateflow) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Local variables
    ! character(LEN=11) :: modname_rst
      !! Used to verify module name when reading from restart file
    ! integer(r32) :: i
    ! integer(r32) :: j
    ! integer(r32) :: idx1D
      !! 1D index from 2D

    ! real(r32), pointer :: tmax_allsnow_2d(:, :)
    ! real(r32), pointer :: tmax_allrain_offset_2d(:, :)

    integer(i32) :: jj
    ! real(r32), allocatable :: crap(:)
    ! ------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &
              nrain => ctl_data%nrain%value, &
              ntemp => ctl_data%ntemp%value, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              rst_unit => ctl_data%restart_output_unit, &
              print_debug => ctl_data%print_debug%value, &
              elev_units => param_data%elev_units%values(1), &
              ! psta_elev => param_data%psta_elev%values, &
              tmax_allsnow => param_data%tmax_allsnow%values, &
              tmax_allrain_offset => param_data%tmax_allrain_offset%values)
              ! tsta_elev => param_data%tsta_elev%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! real(r32), allocatable :: tmax_allrain(:, :)

      ! Get 2d-indexed pointers to 1d arrays of parameters
      ! tmax_allsnow_2d => get_array(tmax_allsnow, (/nhru, nmonths/))
      ! tmax_allrain_offset_2d => get_array(tmax_allrain_offset, (/nhru, nmonths/))

      allocate(this%hru_ppt(nhru))
      allocate(this%hru_rain(nhru))
      allocate(this%hru_snow(nhru))
      allocate(this%prmx(nhru))

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

      ! if (ctl_data%temp_module%values(1)%s /= 'climate_hru' .and. &
      !     ctl_data%temp_module%values(1)%s /= 'temp_sta') then
      !   allocate(this%tsta_elev_feet(ntemp))
      !   allocate(this%tsta_elev_meters(ntemp))
      !
      !   if (elev_units == FEET) then
      !     this%tsta_elev_feet = tsta_elev
      !     this%tsta_elev_meters = tsta_elev * FEET2METERS
      !   else
      !     this%tsta_elev_meters = tsta_elev
      !     this%tsta_elev_feet = tsta_elev * METERS2FEET
      !   endif
      ! endif

      allocate(this%tmax_allrain(nhru, 12))
      allocate(this%tmax_allrain_c(nhru, 12))
      allocate(this%tmax_allrain_f(nhru, 12))
      allocate(this%tmax_allsnow_c(nhru, 12))
      allocate(this%tmax_allsnow_f(nhru, 12))
      ! allocate(this%tmax_aspect_adjust(nhru, 12), this%tmin_aspect_adjust(nhru, 12))

      allocate(this%newsnow(nhru))
      allocate(this%pptmix(nhru))

      ! Snow
      allocate(this%pkwater_equiv(nhru))
      this%pkwater_equiv = 0.0_dp

      ! TODO: Figure this out
      ! if (ctl_data%et_module%values(1)%s /= 'potet_pm' .and. &
      !     ctl_data%et_module%values(1)%s /= 'potet_pt') then
      !   ! ?anything needed?
      ! else
      !   ! This is confusing because humidity_percent appears to only be used
      !   ! by potet_pm and potet_pt. But it's forced to 1.0 in this case which
      !   ! overrides the parameter values.
      !   humidity_percent = 1.0
      ! endif

      ! NOTE: Doesn't appear to be used by stream_temp; computed in ccsolrad
      ! For stream temperature
      ! if (ctl_data%solrad_module%values(1)%s == 'ccsolrad' .or. &
      !     ctl_data%solrad_module%values(1)%s == 'ddsolrad' .or. &
      !     ctl_data%stream_temp_flag%value == 1) then
      !   allocate(this%cloud_cover_hru(nhru))
      ! endif

      if (init_vars_from_file == 1) then
        ! read(rst_unit) modname_rst
        ! call check_restart(MODNAME, modname_rst)
        ! read(rst_unit) this%basin_ppt, this%basin_rain, this%basin_snow, &
        !                this%basin_obs_ppt, this%basin_temp, &
        !                this%basin_orad, this%basin_tmax, this%basin_tmin, &
        !                this%solrad_tmax, this%solrad_tmin, &
        !                this%basin_transp_on, this%basin_potet, &
        !                this%basin_horad, this%basin_swrad
        ! read(rst_unit) this%tmax_hru
        ! read(rst_unit) this%tmin_hru
        ! read(rst_unit) this%newsnow
        ! read(rst_unit) this%pptmix
        ! read(rst_unit) this%hru_ppt
        ! read(rst_unit) this%hru_rain
        ! read(rst_unit) this%hru_snow
        ! read(rst_unit) this%prmx
        ! read(rst_unit) this%tmaxf
        ! read(rst_unit) this%tminf
        ! read(rst_unit) this%tavgf
        ! read(rst_unit) this%tmaxc
        ! read(rst_unit) this%tminc
        ! read(rst_unit) this%tavgc
        ! read(rst_unit) this%transp_on
        ! read(rst_unit) this%potet
        ! read(rst_unit) this%swrad
        !
        ! ! if (ANY(['ddsolrad', 'ccsolrad']==Solrad_module)) read(rst_unit) this%orad_hru
      else
        this%hru_ppt = 0.0
        this%hru_rain = 0.0
        this%hru_snow = 0.0
        this%prmx = 0.0
        this%solrad_tmax = 0.0
        this%solrad_tmin = 0.0
        ! this%tavgc = 0.0
        ! this%tavgf = 0.0
        ! this%tmax_hru = 0.0
        ! this%tmaxc = 0.0
        ! this%tmaxf = 0.0
        ! this%tmin_hru = 0.0
        ! this%tminc = 0.0
        ! this%tminf = 0.0

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
      endif
    end associate
  end function


  module subroutine cleanup_Climateflow(this, ctl_data)
    use Control_class, only: Control
    implicit none

    class(Climateflow), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! ------------------------------------------------------------------------
    ! TODO: Update to reflect the full PRMS codebase
    associate(rst_unit => ctl_data%restart_output_unit, &
              solrad_module => ctl_data%solrad_module%values(1))

      ! write(rst_unit) MODNAME
      ! write(rst_unit) this%basin_ppt, this%basin_rain, this%basin_snow, &
      !                 this%basin_obs_ppt, this%basin_temp, this%basin_orad, &
      !                 this%basin_tmax, this%basin_tmin, this%solrad_tmax, &
      !                 this%solrad_tmin, this%basin_transp_on, this%basin_potet, &
      !                 this%basin_horad, this%basin_swrad
      ! write(rst_unit) this%tmax_hru
      ! write(rst_unit) this%tmin_hru
      ! write(rst_unit) this%newsnow
      ! write(rst_unit) this%pptmix
      ! write(rst_unit) this%hru_ppt
      ! write(rst_unit) this%hru_rain
      ! write(rst_unit) this%hru_snow
      ! write(rst_unit) this%prmx
      ! write(rst_unit) this%tmaxf
      ! write(rst_unit) this%tminf
      ! write(rst_unit) this%tavgf
      ! write(rst_unit) this%tmaxc
      ! write(rst_unit) this%tminc
      ! write(rst_unit) this%tavgc
      ! write(rst_unit) this%transp_on
      ! write(rst_unit) this%potet
      ! write(rst_unit) this%swrad
      !
      ! if (solrad_module%s == 'ddsolrad' .or. solrad_module%s == 'ccsolrad') then
      !   write(rst_unit) this%orad_hru
      ! endif
    end associate
  end subroutine


  module function module_name() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODNAME
  end function


  module function version() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODVERSION
  end function


    module subroutine set_precipitation_form(this, ctl_data, param_data, model_basin, model_temp, &
                                             month, rain_adj, snow_adj, rainmix_adj)
      use prms_constants, only: DNEARZERO, NEARZERO, INCHES, MM, MM2INCH
      implicit none

      class(Climateflow), intent(inout) :: this
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

  ! module subroutine set_precipitation_form(this, param_data, model_basin, model_temp, &
  !                                          month, rain_adj, snow_adj, rainmix_adj)
  !   use prms_constants, only: NEARZERO, INCHES, MM, MM2INCH
  !   implicit none
  !
  !   class(Climateflow), intent(inout) :: this
  !   ! type(Control), intent(in) :: ctl_data
  !   type(Parameters), intent(in) :: param_data
  !   type(Basin), intent(in) :: model_basin
  !   class(Temperature), intent(in) :: model_temp
  !   integer(i32), intent(in) :: month
  !   real(r32), optional, intent(in) :: rain_adj(:)
  !     !! Array of rain adjustments
  !   real(r32), optional, intent(in) :: snow_adj(:)
  !     !! Array of snow adjustments
  !   real(r32), optional, intent(in) :: rainmix_adj(:)
  !     !! Array of rain mixture adjustments
  !
  !   ! Local variables
  !   real(r32), allocatable :: tdiff_arr(:)
  !     !! Array containing differences b/t tmaxf and tminf
  !
  !   ! Control
  !
  !   ! Parameters
  !   ! hru_area, precip_units,
  !
  !   ! Basin
  !   ! basin_area_inv,
  !
  !   ! Temperature
  !   ! tmax, tmin
  !
  !   ! -------------------------------------------------------------------------
  !   associate(basin_area_inv => model_basin%basin_area_inv, &
  !             hru_area => param_data%hru_area%values, &
  !             precip_units => param_data%precip_units%values(1), &
  !
  !             tmax => model_temp%tmax, &
  !             tmin => model_temp%tmin, &
  !
  !             ! WARNING: This will change when tmax_allsnow and tmax_allrain
  !             !          are standardized to Celsius.
  !             tmax_allrain => this%tmax_allrain_c, &
  !             tmax_allsnow => this%tmax_allsnow_c)
  !
  !     ! tdiff_arr = this%tmaxf - this%tminf
  !     tdiff_arr = tmax - tmin
  !     where (abs(tdiff_arr) < NEARZERO)
  !       tdiff_arr = 0.0001
  !     end where
  !
  !     ! Convert precipitation to inches if required
  !     if (precip_units == MM) then
  !       where (this%hru_ppt > 0.0) this%hru_ppt = this%hru_ppt * MM2INCH
  !     endif
  !
  !     ! Basin precipitation before any adjustments
  !     this%basin_obs_ppt = sum(dble(this%hru_ppt * hru_area)) * basin_area_inv
  !
  !     !******Initialize HRU variables
  !     this%pptmix = 0
  !     this%newsnow = 0
  !     this%prmx = 0.0
  !     this%hru_rain = 0.0
  !     this%hru_snow = 0.0
  !
  !     ! TODO: how to handle tmax_allsnow_f??
  !     where (tmax <= tmax_allsnow(:, month))
  !       this%hru_ppt = this%hru_ppt * snow_adj
  !       this%hru_snow = this%hru_ppt
  !       this%newsnow = 1
  !     elsewhere
  !       where (tmin > tmax_allsnow(:, month) .or. &
  !              tmax >= tmax_allrain(:, month))
  !         this%hru_ppt = this%hru_ppt * rain_adj
  !         this%hru_rain = this%hru_ppt
  !         this%prmx = 1.0
  !       elsewhere
  !         this%prmx = ((tmax - tmax_allsnow(:, month)) / tdiff_arr) * rainmix_adj
  !
  !         where (this%prmx < 0.0)
  !           this%prmx = 0.0
  !         end where
  !
  !         where (this%prmx < 1.0)
  !           this%pptmix = 1
  !           this%hru_ppt = this%hru_ppt * snow_adj
  !           this%hru_rain = this%prmx * this%hru_ppt
  !           this%hru_snow = this%hru_ppt - this%hru_rain
  !           this%newsnow = 1
  !         elsewhere
  !           this%hru_ppt = this%hru_ppt * rain_adj
  !           this%hru_rain = this%hru_ppt
  !           this%prmx = 1.0
  !         end where
  !       end where
  !     end where
  !
  !     this%basin_ppt = sum(DBLE(this%hru_ppt * hru_area)) * basin_area_inv
  !     this%basin_rain = sum(DBLE(this%hru_rain * hru_area)) * basin_area_inv
  !     this%basin_snow = sum(DBLE(this%hru_snow * hru_area)) * basin_area_inv
  !   end associate
  ! end subroutine


  ! !***********************************************************************
  ! !     Sets temperatures in both system of units for each HRU
  ! !***********************************************************************
  ! module subroutine set_temperature(this, ctl_data, param_data, model_basin, tmin_adj, tmax_adj)
  !   use prms_constants, only: MINTEMP, MAXTEMP, FAHRENHEIT, CELSIUS
  !   use conversions_mod, only: c_to_f, f_to_c
  !   implicit none
  !
  !   class(Climateflow), intent(inout) :: this
  !   type(Control), intent(in) :: ctl_data
  !   type(Parameters), intent(in) :: param_data
  !   type(Basin), intent(in) :: model_basin
  !   real(r32), optional, intent(in) :: tmin_adj(:)
  !     !! Array of minimum temperature adjustments
  !   real(r32), optional, intent(in) :: tmax_adj(:)
  !     !! Array of maximum temperature adjustments
  !
  !   ! --------------------------------------------------------------------------
  !   associate(basin_area_inv => model_basin%basin_area_inv, &
  !             hru_area => param_data%hru_area%values)
  !
  !     if (param_data%temp_units%values(1) == FAHRENHEIT) then
  !       if (present(tmax_adj)) then
  !         this%tmaxf = this%tmaxf + tmax_adj
  !       endif
  !
  !       if (present(tmin_adj)) then
  !         this%tminf = this%tminf + tmin_adj
  !       endif
  !
  !       this%tavgf = (this%tmaxf + this%tminf) * 0.5
  !
  !       this%tmaxc = f_to_c(this%tmaxf)
  !       this%tminc = f_to_c(this%tminf)
  !       this%tavgc = f_to_c(this%tavgf)
  !
  !       ! NOTE: Used by ddsolrad, ccsolrad and frost_date modules.
  !       this%tmax_hru = this%tmaxf ! in units temp_units
  !       this%tmin_hru = this%tminf ! in units temp_units
  !
  !       this%basin_temp = sum(dble(this%tavgf * hru_area)) * basin_area_inv
  !       this%basin_tmax = sum(dble(this%tmaxf * hru_area)) * basin_area_inv
  !       this%basin_tmin = sum(dble(this%tminf * hru_area)) * basin_area_inv
  !     else
  !       ! degrees Celsius
  !
  !       ! WARNING: This won't work because tmax and tmin from the CBH file
  !       !          are always assumed to be in Fahrenheit.
  !       if (present(tmax_adj)) then
  !         this%tmaxc = this%tmaxc + tmax_adj
  !       endif
  !
  !       if (present(tmin_adj)) then
  !         this%tminc = this%tminc + tmin_adj
  !       endif
  !
  !       this%tavgc = (this%tmaxc + this%tminc) * 0.5
  !
  !       this%tmaxf = c_to_f(this%tmaxc)
  !       this%tminf = c_to_f(this%tminc)
  !       this%tavgf = c_to_f(this%tavgc)
  !       this%tmax_hru = this%tmaxc ! in units temp_units
  !       this%tmin_hru = this%tminc ! in units temp_units
  !
  !       this%basin_temp = sum(dble(this%tavgc * hru_area)) * basin_area_inv
  !       this%basin_tmax = sum(dble(this%tmaxc * hru_area)) * basin_area_inv
  !       this%basin_tmin = sum(dble(this%tminc * hru_area)) * basin_area_inv
  !     endif
  !
  !     ! NOTE: Why are separate variables needed for solrad?
  !     this%solrad_tmax = real(this%basin_tmax, r32)
  !     this%solrad_tmin = real(this%basin_tmin, r32)
  !
  !     ! if (this%tminf(ihru) < MINTEMP .OR. this%tmaxf(ihru) > MAXTEMP) then
  !     !   PRINT *, 'ERROR, invalid temperature value for HRU: ', ihru, this%tminf(ihru), this%tmaxf(ihru) !, ' Date:', Nowyear, Nowmonth, Nowday
  !     !   STOP
  !     ! endif
  !   end associate
  ! end subroutine
end submodule
