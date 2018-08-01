submodule (PRMS_CLIMATEVARS) sm_climateflow

contains
  !***********************************************************************
  ! Climateflow constructor
  module function constructor_Climateflow(ctl_data, param_data) result(this)
    use UTILS_PRMS, only: check_restart, print_module_info
    implicit none

    type(Climateflow) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! ------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              rst_unit => ctl_data%restart_output_unit, &
              print_debug => ctl_data%print_debug%value, &

              soil_rechr_init_frac => param_data%soil_rechr_init_frac%values, &
              soil_rechr_max_frac => param_data%soil_rechr_max_frac%values, &
              soil_moist_init_frac => param_data%soil_moist_init_frac%values, &
              soil_moist_max => param_data%soil_moist_max%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! Soilzone variables
      allocate(this%soil_moist(nhru))
      allocate(this%soil_rechr_max(nhru))
      allocate(this%soil_rechr(nhru))

      this%soil_moist = soil_moist_init_frac * soil_moist_max
      this%soil_rechr_max = soil_rechr_max_frac * soil_moist_max
      this%soil_rechr = soil_rechr_init_frac * this%soil_rechr_max

      ! Snow
      allocate(this%pkwater_equiv(nhru))
      this%pkwater_equiv = 0.0_dp


      ! NOTE: could deallocate soil_moist_init_frac, soil_rechr_init_frac,
      !       and ssstor_init_frac

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

      ! allocate(this%tmax_aspect_adjust(nhru, 12), this%tmin_aspect_adjust(nhru, 12))

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

      ! if (init_vars_from_file == 1) then
      !   ! read(rst_unit) modname_rst
      !   ! call check_restart(MODNAME, modname_rst)
      !   ! read(rst_unit) this%basin_ppt, this%basin_rain, this%basin_snow, &
      !   !                this%basin_obs_ppt, this%basin_temp, &
      !   !                this%basin_orad, this%basin_tmax, this%basin_tmin, &
      !   !                this%solrad_tmax, this%solrad_tmin, &
      !   !                this%basin_transp_on, this%basin_potet, &
      !   !                this%basin_horad, this%basin_swrad
      !   ! read(rst_unit) this%tmax_hru
      !   ! read(rst_unit) this%tmin_hru
      !   ! read(rst_unit) this%newsnow
      !   ! read(rst_unit) this%pptmix
      !   ! read(rst_unit) this%hru_ppt
      !   ! read(rst_unit) this%hru_rain
      !   ! read(rst_unit) this%hru_snow
      !   ! read(rst_unit) this%prmx
      !   ! read(rst_unit) this%tmaxf
      !   ! read(rst_unit) this%tminf
      !   ! read(rst_unit) this%tavgf
      !   ! read(rst_unit) this%tmaxc
      !   ! read(rst_unit) this%tminc
      !   ! read(rst_unit) this%tavgc
      !   ! read(rst_unit) this%transp_on
      !   ! read(rst_unit) this%potet
      !   ! read(rst_unit) this%swrad
      !   !
      !   ! ! if (ANY(['ddsolrad', 'ccsolrad']==Solrad_module)) read(rst_unit) this%orad_hru
      ! else
      !   this%solrad_tmax = 0.0
      !   this%solrad_tmin = 0.0
      ! endif
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

end submodule
