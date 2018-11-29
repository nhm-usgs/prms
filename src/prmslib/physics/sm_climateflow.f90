submodule (PRMS_CLIMATEVARS) sm_climateflow

contains
  !***********************************************************************
  ! Climateflow constructor
  module function constructor_Climateflow(ctl_data, param_data, nhru_summary) result(this)
    use UTILS_PRMS, only: check_restart
    implicit none

    type(Climateflow) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Nhru_summary_ptr), intent(inout) :: nhru_summary

    integer(i32) :: jj

    ! ------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              nhruOutON_OFF => ctl_data%nhruOutON_OFF%value, &
              nhruOutVars => ctl_data%nhruOutVars%value, &
              nhruOutVar_names => ctl_data%nhruOutVar_names%values, &
              rst_unit => ctl_data%restart_output_unit, &
              print_debug => ctl_data%print_debug%value, &

              soil_rechr_init_frac => param_data%soil_rechr_init_frac%values, &
              soil_rechr_max_frac => param_data%soil_rechr_max_frac%values, &
              soil_moist_init_frac => param_data%soil_moist_init_frac%values, &
              soil_moist_max => param_data%soil_moist_max%values)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
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

      ! Connect any nhru_summary variables that need to be output
      if (nhruOutON_OFF == 1) then
        do jj=1, nhruOutVars
          select case(nhruOutVar_names(jj)%s)
            case('pkwater_equiv')
              call nhru_summary%set_nhru_var(jj, this%pkwater_equiv)
            case('soil_moist')
              call nhru_summary%set_nhru_var(jj, this%soil_moist)
            case('soil_rechr')
              call nhru_summary%set_nhru_var(jj, this%soil_rechr)
            case default
              ! pass
          end select
        enddo
      endif

      ! NOTE: could deallocate soil_moist_init_frac, soil_rechr_init_frac,
      !       and ssstor_init_frac

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


end submodule
