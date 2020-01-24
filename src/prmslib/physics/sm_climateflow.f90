submodule (PRMS_CLIMATEVARS) sm_climateflow

contains
  !***********************************************************************
  ! Climateflow constructor
  module subroutine init_Climateflow(this, ctl_data, model_basin, model_summary)
    use iso_fortran_env, only: output_unit, error_unit
    use prms_constants, only: INACTIVE, LAKE
    implicit none

    class(Climateflow), target, intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj

    ! ------------------------------------------------------------------------
    associate(init_vars_from_file => ctl_data%init_vars_from_file%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              ! rst_unit => ctl_data%restart_output_unit, &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &

              nhru => model_basin%nhru, &
              hru_type => model_basin%hru_type)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Parameters
      allocate(this%soil_moist_init_frac(nhru))
      call param_hdl%get_variable('soil_moist_init_frac', this%soil_moist_init_frac)

      allocate(this%soil_moist_max(nhru))
      call param_hdl%get_variable('soil_moist_max', this%soil_moist_max)

      allocate(this%soil_rechr_init_frac(nhru))
      call param_hdl%get_variable('soil_rechr_init_frac', this%soil_rechr_init_frac)

      allocate(this%soil_rechr_max_frac(nhru))
      call param_hdl%get_variable('soil_rechr_max_frac', this%soil_rechr_max_frac)

      ! Other variables

      ! Soilzone variables
      allocate(this%soil_moist(nhru))
      allocate(this%soil_rechr_max(nhru))
      allocate(this%soil_rechr(nhru))

      this%soil_moist = this%soil_moist_init_frac * this%soil_moist_max
      this%soil_rechr_max = this%soil_rechr_max_frac * this%soil_moist_max
      this%soil_rechr = this%soil_rechr_init_frac * this%soil_rechr_max

      do jj=1, nhru
        if (hru_type(jj) == INACTIVE .or. hru_type(jj) == LAKE) cycle

        if (this%soil_rechr_max(jj) > this%soil_moist_max(jj)) then
          write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_rechr_max > soil_moist_max (HRU=', jj, ')'
          this%soil_rechr_max(jj) = this%soil_moist_max(jj)
        end if

        if (this%soil_rechr(jj) > this%soil_rechr_max(jj)) then
          ! NOTE: PRMS5 has 'soil_rechr_init > soil_rechr_max'
          write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_rechr_init_frac > soil_rechr_max (HRU=', jj, ')'
          this%soil_rechr(jj) = this%soil_rechr_max(jj)
        end if

        if (this%soil_moist(jj) > this%soil_moist_max(jj)) then
          ! NOTE: PRMS5 has 'soil_moist_init > soil_moist_max'
          write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_moist_init_frac > soil_moist_max (HRU=', jj, ')'
          this%soil_moist(jj) = this%soil_moist_max(jj)
        end if

        if (this%soil_rechr(jj) > this%soil_moist(jj)) then
          ! NOTE: PRMS5 has 'soil_rechr_init > soil_moist_init'
          ! DEBUG: PAN - re-add output
          ! write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_rechr_init_frac > soil_moist_init_frac (HRU=', jj, ')'
          this%soil_rechr(jj) = this%soil_moist(jj)
        end if
      end do

      9012 format(2X, A, A, I0, A)

      ! Snow
      allocate(this%pkwater_equiv(nhru))
      this%pkwater_equiv = 0.0_dp

      ! Connect any nhru_summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj=1, outVar_names%size()
          select case(outVar_names%values(jj)%s)
            case('pkwater_equiv')
              call model_summary%set_summary_var(jj, this%pkwater_equiv)
            case('soil_moist')
              call model_summary%set_summary_var(jj, this%soil_moist)
            case('soil_rechr')
              call model_summary%set_summary_var(jj, this%soil_rechr)
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
  end subroutine


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
