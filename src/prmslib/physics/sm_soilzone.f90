submodule (PRMS_SOILZONE) sm_soilzone
  contains
    module function constructor_Soilzone(ctl_data, param_data, model_basin, model_climate, snow, basin_summary) result(this)
      use prms_constants, only: dp, INACTIVE, LAND, LAKE, SWALE
      implicit none

      type(Soilzone) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(inout) :: model_climate
      type(Snowcomp), intent(in) :: snow
      type(Basin_summary_ptr), intent(inout) :: basin_summary

      ! Local variables
      ! integer(i32) :: chru
      integer(i32) :: jj

      ! GSFLOW-related variables
      ! TODO: Uncommented next 4 when GSFLOW figured out
      ! integer(i32) :: ii
      ! integer(i32) :: ihru
      ! integer(i32) :: icnt
      ! integer(i32) :: ierr

      ! Control
      ! nhru, nhrucell, nlake, nsegment,
      ! cascade_flag, init_vars_from_file, model_mode, print_debug

      ! Parameters
      ! hru_area, hru_type, gvr_hru_id, pref_flow_den, sat_threshold, soil_moist_max,
      ! soil2gw_max, ssstor_init_frac

      ! Basin
      ! hru_frac_perv, hru_area_perv, basin_area_inv

      ! flowvars
      ! soil_moist, soil_rechr, soil_rechr_max

      ! Snowcomp
      ! snowcov_area

      ! -----------------------------------------------------------------------
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                nhru => ctl_data%nhru%value, &
                nhrucell => ctl_data%nhrucell%value, &
                nlake => ctl_data%nlake%value, &
                nsegment => ctl_data%nsegment%value, &
                basinOutON_OFF => ctl_data%basinOutON_OFF%value, &
                basinOutVars => ctl_data%basinOutVars%value, &
                basinOutVar_names => ctl_data%basinOutVar_names%values, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                gsflow_mode => ctl_data%gsflow_mode, &
                ! model_mode => ctl_data%model_mode%values, &
                print_debug => ctl_data%print_debug%value, &

                hru_area => param_data%hru_area%values, &
                hru_type => param_data%hru_type%values, &
                gvr_hru_id => param_data%gvr_hru_id%values, &
                pref_flow_den => param_data%pref_flow_den%values, &
                sat_threshold => param_data%sat_threshold%values, &
                soil_moist_max => param_data%soil_moist_max%values, &
                soil2gw_max => param_data%soil2gw_max%values, &
                ssstor_init_frac => param_data%ssstor_init_frac%values, &

                basin_area_inv => model_basin%basin_area_inv, &
                hru_frac_perv => model_basin%hru_frac_perv, &
                hru_area_perv => model_basin%hru_area_perv, &

                soil_moist => model_climate%soil_moist, &
                soil_rechr => model_climate%soil_rechr, &
                soil_rechr_max => model_climate%soil_rechr_max, &

                snowcov_area => snow%snowcov_area)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! TODO: 2018-06-21 - Uncomment once cascade module is converted.
        ! if (cascade_flag == 1) then
        !   allocate(this%hru_sz_cascadeflow(nhru))
        !   allocate(this%upslope_dunnianflow(nhru))
        !   allocate(this%upslope_interflow(nhru))
        !
        !   if (nlake > 0) then
        !     allocate(this%lakein_sz(nhru))
        !   endif
        ! end if

        allocate(this%cap_infil_tot(nhru))
        allocate(this%cap_waterin(nhru))
        allocate(this%dunnian_flow(nhru))
        allocate(this%hru_actet(nhru))  ! moved from flowvars
        allocate(this%perv_actet(nhru))
        allocate(this%potet_lower(nhru))
        allocate(this%potet_rechr(nhru))
        allocate(this%pref_flow(nhru))
        allocate(this%pref_flow_in(nhru))
        allocate(this%pref_flow_infil(nhru))
        allocate(this%pref_flow_max(nhru))
        allocate(this%pref_flow_stor(nhru))
        allocate(this%pref_flow_thrsh(nhru))
        allocate(this%recharge(nhru))
        allocate(this%slow_flow(nhru))  ! moved from flowvars
        allocate(this%slow_stor(nhru))  ! moved from flowvars
        allocate(this%snow_free(nhru))
        allocate(this%soil_lower(nhru))
        allocate(this%soil_lower_ratio(nhru))
        allocate(this%soil_lower_stor_max(nhru))
        allocate(this%soil_moist_tot(nhru))
        allocate(this%soil_to_gw(nhru))
        allocate(this%soil_to_ssr(nhru))
        allocate(this%soil_zone_max(nhru))
        allocate(this%ssr_to_gw(nhru))
        allocate(this%ssres_flow(nhru))  ! moved from flowvars
        allocate(this%ssres_in(nhru))
        allocate(this%ssres_stor(nhru))  ! moved from flowvars
        allocate(this%swale_actet(nhru))
        allocate(this%unused_potet(nhru))

        ! Allocate arrays for local and variables from other modules
        allocate(this%grav_dunnian_flow(nhru))
        allocate(this%gvr2pfr(nhru))
        allocate(this%pfr_dunnian_flow(nhru))
        allocate(this%pref_flow_flag(nhru))
        allocate(this%soil2gw_flag(nhru))
        allocate(this%swale_limit(nhru))

        if (print_debug == 1) then
          allocate(this%soil_moist_ante(nhru))
          allocate(this%ssres_stor_ante(nhru))
        endif

        this%soil_to_gw = 0.0
        this%soil_to_ssr = 0.0
        this%ssr_to_gw = 0.0
        this%ssres_in = 0.0

        ! TODO: 2018-06-21 Uncomment when GSFLOW stuff is figured out.
        ! ! if (model_mode(1)%s == 'GSFLOW') then
        ! if (gsflow_mode) then
        ! ! if (model == 0) then
        !   if (nhrucell < -1) STOP 'ERROR, dimension nhrucell not specified > 0'
        !
        !   allocate(this%grav_gwin(nhru) ) ! ??
        !   allocate(this%gravity_stor_res(nhrucell))
        !   allocate(this%gvr2sm(nhru))
        !   allocate(this%gvr_hru_pct_adjusted(nhrucell))
        !   allocate(this%gw2sm_grav(nhrucell))
        !   allocate(this%hru_gvr_count(nhru))
        !   allocate(this%hrucheck(nhru))
        !   allocate(this%it0_gravity_stor_res(nhrucell))
        !   allocate(this%it0_potet(nhru))
        !   allocate(this%it0_pref_flow_stor(nhru))
        !   allocate(this%it0_slow_stor(nhru))
        !   allocate(this%it0_soil_moist(nhru))
        !   allocate(this%it0_soil_rechr(nhru))
        !   allocate(this%it0_sroff(nhru))
        !   allocate(this%it0_ssres_stor(nhru))
        !   allocate(this%it0_strm_seg_in(nsegment))
        !   allocate(this%replenish_frac(nhru))
        !   allocate(this%sm2gw_grav(nhrucell))
        ! endif

        ! if ( print_debug==7 ) call PRMS_open_module_file(DBGUNT, 'soilzone.dbg')

        ! TODO: figure out if this is needed still
        ! if (Model == 0) then
        !   if (nhru /= nhrucell) then
        !     if (getparam(MODNAME, 'gvr_hru_id', nhrucell, 'integer', gvr_hru_id)/=0 ) call read_error(2, 'gvr_hru_id')
        !     if (Parameter_check_flag==1 ) call checkdim_bounded_limits('gvr_hru_id', 'nhru', gvr_hru_id, nhrucell, 1, nhru, ierr)
        !   else
        !     do chru = 1, nhru
        !       gvr_hru_id(chru) = chru
        !     enddo
        !   endif
        !   this%grav_gwin = 0.0 ! dimension nhru
        !   this%gw2sm_grav = 0.0
        ! endif

        allocate(this%basin_actet)
        allocate(this%basin_cap_infil_tot)
        allocate(this%basin_cap_up_max)
        allocate(this%basin_capwaterin)
        allocate(this%basin_cpr_stor_frac)
        allocate(this%basin_dncascadeflow)
        allocate(this%basin_dndunnianflow)
        allocate(this%basin_dninterflow)
        allocate(this%basin_dunnian)
        allocate(this%basin_dunnian_gvr)
        allocate(this%basin_dunnian_pfr)
        allocate(this%basin_gvr2pfr)
        allocate(this%basin_gvr2sm)
        allocate(this%basin_gvr_stor_frac)
        allocate(this%basin_interflow_max)
        allocate(this%basin_lakeevap)
        allocate(this%basin_lakeinsz)
        allocate(this%basin_lakeprecip)
        allocate(this%basin_perv_et)
        allocate(this%basin_pfr_stor_frac)
        allocate(this%basin_pref_flow_infil)
        allocate(this%basin_pref_stor)
        allocate(this%basin_prefflow)
        allocate(this%basin_recharge)
        allocate(this%basin_slowflow)
        allocate(this%basin_slstor)
        allocate(this%basin_sm2gvr)
        allocate(this%basin_sm2gvr_max)
        allocate(this%basin_soil_lower_stor_frac)
        allocate(this%basin_soil_moist)
        allocate(this%basin_soil_moist_tot)
        allocate(this%basin_soil_rechr)
        allocate(this%basin_soil_rechr_stor_frac)
        allocate(this%basin_soil_to_gw)
        allocate(this%basin_ssflow)
        allocate(this%basin_ssin)
        allocate(this%basin_ssstor)
        allocate(this%basin_swale_et)
        allocate(this%basin_sz_gwin)
        allocate(this%basin_sz_stor_frac)
        allocate(this%basin_sz2gw)

        ! Connect any basin summary variables that need to be output
        if (basinOutON_OFF == 1) then
          do jj = 1, basinOutVars
            ! TODO: This is where the daily basin values are linked based on
            !       what was requested in basinOutVar_names.
            select case(basinOutVar_names(jj)%s)
              case('basin_actet')
                call basin_summary%set_basin_var(jj, this%basin_actet)
              case('basin_cap_infil_tot')
                call basin_summary%set_basin_var(jj, this%basin_cap_infil_tot)
              case('basin_cap_up_max')
                call basin_summary%set_basin_var(jj, this%basin_cap_up_max)
              case('basin_capwaterin')
                call basin_summary%set_basin_var(jj, this%basin_capwaterin)
              case('basin_cpr_stor_frac')
                call basin_summary%set_basin_var(jj, this%basin_cpr_stor_frac)
              case('basin_dncascadeflow')
                call basin_summary%set_basin_var(jj, this%basin_dncascadeflow)
              case('basin_dndunnianflow')
                call basin_summary%set_basin_var(jj, this%basin_dndunnianflow)
              case('basin_dninterflow')
                call basin_summary%set_basin_var(jj, this%basin_dninterflow)
              case('basin_dunnian')
                call basin_summary%set_basin_var(jj, this%basin_dunnian)
              case('basin_dunnian_gvr')
                call basin_summary%set_basin_var(jj, this%basin_dunnian_gvr)
              case('basin_dunnian_pfr')
                call basin_summary%set_basin_var(jj, this%basin_dunnian_pfr)
              case('basin_gvr2pfr')
                call basin_summary%set_basin_var(jj, this%basin_gvr2pfr)
              case('basin_gvr2sm')
                call basin_summary%set_basin_var(jj, this%basin_gvr2sm)
              case('basin_gvr_stor_frac')
                call basin_summary%set_basin_var(jj, this%basin_gvr_stor_frac)
              case('basin_interflow_max')
                call basin_summary%set_basin_var(jj, this%basin_interflow_max)
              case('basin_lakeevap')
                call basin_summary%set_basin_var(jj, this%basin_lakeevap)
              case('basin_lakeinsz')
                call basin_summary%set_basin_var(jj, this%basin_lakeinsz)
              case('basin_lakeprecip')
                call basin_summary%set_basin_var(jj, this%basin_lakeprecip)
              case('basin_perv_et')
                call basin_summary%set_basin_var(jj, this%basin_perv_et)
              case('basin_pfr_stor_frac')
                call basin_summary%set_basin_var(jj, this%basin_pfr_stor_frac)
              case('basin_pref_flow_infil')
                call basin_summary%set_basin_var(jj, this%basin_pref_flow_infil)
              case('basin_pref_stor')
                call basin_summary%set_basin_var(jj, this%basin_pref_stor)
              case('basin_prefflow')
                call basin_summary%set_basin_var(jj, this%basin_prefflow)
              case('basin_recharge')
                call basin_summary%set_basin_var(jj, this%basin_recharge)
              case('basin_slowflow')
                call basin_summary%set_basin_var(jj, this%basin_slowflow)
              case('basin_slstor')
                call basin_summary%set_basin_var(jj, this%basin_slstor)
              case('basin_sm2gvr')
                call basin_summary%set_basin_var(jj, this%basin_sm2gvr)
              case('basin_sm2gvr_max')
                call basin_summary%set_basin_var(jj, this%basin_sm2gvr_max)
              case('basin_soil_lower_stor_frac')
                call basin_summary%set_basin_var(jj, this%basin_soil_lower_stor_frac)
              case('basin_soil_moist')
                call basin_summary%set_basin_var(jj, this%basin_soil_moist)
              case('basin_soil_moist_tot')
                call basin_summary%set_basin_var(jj, this%basin_soil_moist_tot)
              case('basin_soil_rechr')
                call basin_summary%set_basin_var(jj, this%basin_soil_rechr)
              case('basin_soil_rechr_stor_frac')
                call basin_summary%set_basin_var(jj, this%basin_soil_rechr_stor_frac)
              case('basin_soil_to_gw')
                call basin_summary%set_basin_var(jj, this%basin_soil_to_gw)
              case('basin_ssflow')
                call basin_summary%set_basin_var(jj, this%basin_ssflow)
              case('basin_ssin')
                call basin_summary%set_basin_var(jj, this%basin_ssin)
              case('basin_ssstor')
                call basin_summary%set_basin_var(jj, this%basin_ssstor)
              case('basin_swale_et')
                call basin_summary%set_basin_var(jj, this%basin_swale_et)
              case('basin_sz_gwin')
                call basin_summary%set_basin_var(jj, this%basin_sz_gwin)
              case('basin_sz_stor_frac')
                call basin_summary%set_basin_var(jj, this%basin_sz_stor_frac)
              case('basin_sz2gw')
                call basin_summary%set_basin_var(jj, this%basin_sz2gw)
              case default
                ! pass
            end select
          enddo
        endif

        ! Reset most of the basin variables used by soilzone
        call this%reset_basin_vars()

        this%hru_actet = 0.0
        this%grav_dunnian_flow = 0.0
        this%pfr_dunnian_flow = 0.0
        this%pref_flag = .false.
        this%pref_flow_flag = .false.
        this%pref_flow_thrsh = 0.0
        this%slow_flow = 0.0
        this%soil2gw_flag = .false.
        this%soil_lower_ratio = 0.0
        this%ssres_flow = 0.0   ! moved from flowvars
        this%ssres_stor = ssstor_init_frac * sat_threshold  ! moved from flowvars
        this%swale_limit = 0.0


        ! Initialize
        this%snow_free = 1.0 - snowcov_area
        this%pref_flow_max = 0.0
        this%pref_flow_stor = 0.0
        this%slow_stor = 0.0
        this%soil_lower = 0.0
        this%soil_lower_stor_max = 0.0
        this%soil_moist_tot = 0.0
        this%soil_zone_max = 0.0
        this%ssres_stor = 0.0
        soil_moist = 0.0 ! WARNING: Overrides init in climateflow
        soil_rechr = 0.0 ! WARNING: Overrides init in climateflow

        where (hru_type == SWALE)
          this%swale_limit = 3.0 * sat_threshold
          this%pref_flow_thrsh = sat_threshold
          ! pref_flow_den = 0.0  ! WARNING: parameters are read-only
        end where

        where (hru_type == LAND)
          this%pref_flow_thrsh = sat_threshold * (1.0 - pref_flow_den)
          this%pref_flow_max = sat_threshold - this%pref_flow_thrsh
        end where

        if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 5) then
          where (hru_type == LAND .or. hru_type == SWALE)
            this%slow_stor = min(this%ssres_stor, this%pref_flow_thrsh)
            this%pref_flow_stor = this%ssres_stor - this%slow_stor
          end where
        endif

        where (soil2gw_max > 0.)
          this%soil2gw_flag = .true.
        end where

        where (hru_type == LAND .and. pref_flow_den > 0.0)
          this%pref_flow_flag = .true.
        end where

        this%pref_flag = any(this%pref_flow_flag)

        this%soil_zone_max = sat_threshold + soil_moist_max * hru_frac_perv
        this%soil_moist_tot = this%ssres_stor + sngl(soil_moist) * hru_frac_perv

        this%soil_lower = soil_moist - soil_rechr
        this%soil_lower_stor_max = soil_moist_max - soil_rechr_max

        where (this%soil_lower_stor_max > 0.0)
          this%soil_lower_ratio = this%soil_lower / this%soil_lower_stor_max
        end where

        ! do chru = 1, nhru
        !   this%snow_free(chru) = 1.0 - snowcov_area(chru)
        !
        !   if (hru_type(chru) == INACTIVE .or. hru_type(chru) == LAKE) then
        !     ! If inactive or lake
        !     ! pref_flow_den(chru) = 0.0  ! WARNING: parameters are read-only
        !     this%pref_flow_max(chru) = 0.0
        !     this%pref_flow_stor(chru) = 0.0
        !     ! sat_threshold(chru) = 0.0  ! WARNING: parameters are read-only
        !     this%slow_stor(chru) = 0.0
        !     this%soil_lower(chru) = 0.0
        !     this%soil_lower_stor_max(chru) = 0.0
        !     this%soil_moist_tot(chru) = 0.0
        !     this%soil_zone_max(chru) = 0.0
        !     this%ssres_stor(chru) = 0.0
        !
        !     soil_moist(chru) = 0.0 ! WARNING: Overrides init in climateflow
        !     soil_rechr(chru) = 0.0 ! WARNING: Overrides init in climateflow
        !     cycle
        !   endif
        !
        !   if (hru_type(chru) == SWALE) then
        !     ! swale
        !     this%swale_limit(chru) = 3.0 * sat_threshold(chru)
        !     ! pref_flow_den(chru) = 0.0  ! WARNING: parameters are read-only
        !     this%pref_flow_thrsh(chru) = sat_threshold(chru)
        !     this%pref_flow_max(chru) = 0.0
        !   else
        !     ! land
        !     this%pref_flow_thrsh(chru) = sat_threshold(chru) * (1.0 - pref_flow_den(chru))
        !     this%pref_flow_max(chru) = sat_threshold(chru) - this%pref_flow_thrsh(chru)
        !   endif
        !
        !   hru_type = LAND or SWALE
        !   if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 5) then
        !     this%slow_stor(chru) = min(this%ssres_stor(chru), this%pref_flow_thrsh(chru))
        !     this%pref_flow_stor(chru) = this%ssres_stor(chru) - this%slow_stor(chru)
        !   endif
        !
        !   if (soil2gw_max(chru) > 0.0) then
        !     this%soil2gw_flag(chru) = .true.
        !   endif
        !
        !   if (hru_type(chru) == LAND) then
        !     ! Interflow coefficient values don't matter except for land HRU
        !     if (pref_flow_den(chru) > 0.0) then
        !       this%pref_flow_flag(chru) = .true.
        !       this%pref_flag = .true.
        !     endif
        !   endif
        !
        !   this%soil_zone_max(chru) = sat_threshold(chru) + soil_moist_max(chru) * hru_frac_perv(chru)
        !   this%soil_moist_tot(chru) = this%ssres_stor(chru) + soil_moist(chru) * hru_frac_perv(chru)
        !
        !   this%soil_lower(chru) = soil_moist(chru) - soil_rechr(chru)
        !   this%soil_lower_stor_max(chru) = soil_moist_max(chru) - soil_rechr_max(chru)
        !
        !   if (this%soil_lower_stor_max(chru) > 0.0) then
        !     this%soil_lower_ratio(chru) = this%soil_lower(chru) / this%soil_lower_stor_max(chru)
        !   endif
        ! enddo

        this%basin_gvr_stor_frac = sum(dble(this%slow_stor / this%pref_flow_thrsh * hru_area), mask=(this%pref_flow_thrsh > 0.0)) * basin_area_inv
        this%basin_pfr_stor_frac = sum(dble(this%pref_flow_stor / this%pref_flow_max * hru_area), mask=(this%pref_flow_flag)) * basin_area_inv
        this%basin_pref_stor = sum(dble(this%pref_flow_stor * hru_area), mask=(this%pref_flow_flag)) * basin_area_inv
        this%basin_slstor = sum(dble(this%slow_stor * hru_area)) * basin_area_inv
        this%basin_soil_moist_tot = sum(dble(this%soil_moist_tot * hru_area)) * basin_area_inv
        this%basin_ssstor = sum(dble(this%ssres_stor * hru_area)) * basin_area_inv
        this%basin_sz_stor_frac = sum(dble(this%soil_moist_tot / this%soil_zone_max * hru_area)) * basin_area_inv

        ! TODO: PAN Should basin_area_inv be used with hru_area_perv???
        this%basin_cpr_stor_frac = sum(dble(soil_moist / soil_moist_max * hru_area_perv)) * basin_area_inv
        this%basin_soil_lower_stor_frac = sum(dble(this%soil_lower_ratio * hru_area_perv)) * basin_area_inv
        this%basin_soil_moist = sum(dble(soil_moist * hru_area_perv)) * basin_area_inv
        this%basin_soil_rechr = sum(dble(soil_rechr * hru_area_perv)) * basin_area_inv
        this%basin_soil_rechr_stor_frac = sum(dble(soil_rechr / soil_rechr_max * hru_area_perv)) * basin_area_inv

        this%last_soil_moist = this%basin_soil_moist
        this%last_ssstor = this%basin_ssstor

        ! initialize arrays (dimensioned nhru)
        ! TODO: 2018-06-21 - Uncomment once cascade module and lakes are ready.
        ! this%dunnian_flow = 0.0
        ! if (cascade_flag == 1) then
        !   this%upslope_interflow = 0.0_dp
        !   this%upslope_dunnianflow = 0.0_dp
        !   this%hru_sz_cascadeflow = 0.0
        !
        !   if (numlake_hrus > 0) then
        !     this%lakein_sz = 0.0_dp
        !   endif
        ! endif

        this%cap_infil_tot = 0.0
        this%cap_waterin = 0.0
        this%gvr2pfr = 0.0
        this%perv_actet = 0.0
        this%potet_lower = 0.0
        this%potet_rechr = 0.0
        this%pref_flow = 0.0
        this%pref_flow_in = 0.0
        this%pref_flow_infil = 0.0
        this%recharge = 0.0
        this%swale_actet = 0.0
        this%unused_potet = 0.0  ! dimension nhru

        ! TODO: Aren't basin variables already initialized?
        ! Initialize scalers
        ! if (init_vars_from_file == 0) call init_basin_vars()

        ! Initialize arrays (dimensioned nhrucell)
        ! TODO: 2018-06-21 - Uncomment when GSFLOW stuff is figured out.
        ! ! if (model_mode(1)%s == 'GSFLOW') then
        ! if (gsflow_mode) then
        !   this%gvr2sm = 0.0  ! dimension nhru
        !   this%sm2gw_grav = 0.0  ! dimension nhrucell

        !   this%max_gvrs = 1
        !   this%hrucheck = 1
        !   this%hru_gvr_count = 0
        !
        !   do chru=1, nhrucell
        !     ihru = gvr_hru_id(chru)
        !
        !     if (hru_type(ihru) == INACTIVE .or. hru_type(ihru) == LAKE) then
        !       this%gravity_stor_res(chru) = 0.0
        !       this%hrucheck(ihru) = 0
        !       this%replenish_frac(ihru) = 0.0
        !     else
        !       ! set only for cold start simulations
        !       if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 5) then
        !         this%gravity_stor_res(chru) = this%ssres_stor(ihru)
        !       endif
        !
        !       this%hru_gvr_count(ihru) = this%hru_gvr_count(ihru) + 1
        !
        !       if (this%hru_gvr_count(ihru) > this%max_gvrs) then
        !         this%max_gvrs = this%hru_gvr_count(ihru)
        !       endif
        !
        !       this%replenish_frac(ihru) = soil_rechr_max(ihru) / soil_moist_max(ihru)
        !     endif
        !   enddo
        !
        !   allocate(this%hru_gvr_index(this%max_gvrs, nhru))
        !
        !   if (nhru == nhrucell) then
        !     if (this%max_gvrs /= 1) then
        !       print *, 'ERROR, nhru=nhrucell, but, gvr_hru_id array specifies more than one GVR for an HRU'
        !       STOP
        !     endif
        !
        !     do chru=1, nhru
        !       this%hru_gvr_index(1, chru) = chru
        !     enddo
        !   else
        !     this%hru_gvr_index = 0
        !
        !     do chru=1, nhru
        !       if (hru_type(chru) == INACTIVE .or. hru_type(chru) == LAKE) cycle  ! if inactive or lake
        !       icnt = 0
        !
        !       do ii=1, nhrucell
        !         if (gvr_hru_id(ii) == chru) then
        !           icnt = icnt + 1
        !           this%hru_gvr_index(icnt, chru) = ii
        !
        !           if (icnt == this%hru_gvr_count(chru)) EXIT
        !         endif
        !       enddo
        !     enddo
        !   endif
        ! endif
      end associate
    end function


    module subroutine run_Soilzone(this, ctl_data, param_data, model_basin, model_time, &
                                   model_potet, model_precip, model_climate, intcp, snow, model_transp, runoff)
      use iso_fortran_env, only: output_unit
      use prms_constants, only: dp, LAKE, LAND, SWALE, NEARZERO, DNEARZERO
      use ieee_arithmetic
      use ieee_exceptions
      use ieee_features
      implicit none

      class(Soilzone) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Time_t), intent(in) :: model_time
      class(Potential_ET), intent(inout) :: model_potet
      class(Precipitation), intent(in) :: model_precip
      type(Climateflow), intent(inout) :: model_climate
        !! Climate variables
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      class(Transpiration), intent(in) :: model_transp
      type(Srunoff), intent(inout) :: runoff

      ! Local Variables
      integer(i32) :: chru
      integer(i32) :: k
      integer(i32) :: update_potet

      real(r64) :: avail_potet
      real(r64) :: availh2o
      real(r64) :: cap_upflow_max
      real(r64) :: capwater_maxin
      real(r64) :: dunnianflw
      real(r64) :: dunnianflw_gvr
      real(r64) :: dunnianflw_pfr
      real(r64) :: gwin
      ! real(r32) :: gvr_maxin
      real(r64) :: interflow
      real(r64) :: pervactet
      real(r64) :: pref_flow_maxin
      real(r64) :: prefflow
      real(r64) :: ssresin
      real(r64) :: topfr
      real(r64) :: unsatisfied_et

      character(len=:), allocatable :: error_txt
      type(ieee_flag_type), parameter :: ieee_custom(4) = [ieee_usual, ieee_underflow]
      type(ieee_status_type) :: status_value
      logical, dimension(4) :: flag_value

      ! TODO: Uncomment when gsflow module(s) is converted
      ! GSFLOW-related variables
      ! real(r32) :: capacity
      ! real(r32) :: dndunn
      ! real(r32) :: dnpreflow
      ! real(r32) :: dnslowflow

      ! Control
      ! nlake, cascade_flag, dprst_flag, model_mode, print_debug,

      ! Basin
      ! active_hrus, hru_route_order, basin_area_inv, hru_area_perv, hru_frac_perv,
      ! numlake_hrus

      ! Time
      ! Nowtime

      ! Parameters
      ! cov_type, fastcoef_lin, fastcoef_sq, hru_area, hru_type,
      ! lake_evap_adj(2D), pref_flow_den, sat_threshold,
      ! soil_moist_max, soil_type,
      ! soil2gw_max, slowcoef_lin, slowcoef_sq, ssr2gw_exp, ssr2gw_rate,

      ! Srunoff
      ! dprst_evap_hru, dprst_seep_hru, hru_impervevap, sroff(RW),
      ! strm_seg_in(RW), infil, basin_sroff(RW)

      ! Precipitation
      ! hru_ppt

      ! Climateflow
      ! soil_rechr(RW), soil_rechr_max, soil_moist(RW),

      ! Interception
      ! hru_intcpevap,

      ! Potential_ET
      ! basin_potet, potet(RW)

      ! Snow
      ! snow_evap, snowcov_area,

      !***********************************************************************
      associate(nlake => ctl_data%nlake%value, &
                cascade_flag => ctl_data%cascade_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                gsflow_mode => ctl_data%gsflow_mode, &
                print_debug => ctl_data%print_debug%value, &

                active_hrus => model_basin%active_hrus, &
                active_mask => model_basin%active_mask, &
                basin_area_inv => model_basin%basin_area_inv, &
                hru_frac_perv => model_basin%hru_frac_perv, &
                hru_area_perv => model_basin%hru_area_perv, &
                hru_route_order => model_basin%hru_route_order, &

                basin_sroff => runoff%basin_sroff, &
                dprst_evap_hru => runoff%dprst_evap_hru, &
                dprst_seep_hru => runoff%dprst_seep_hru, &
                hru_impervevap => runoff%hru_impervevap, &
                infil => runoff%infil, &
                sroff => runoff%sroff, &
                strm_seg_in => runoff%strm_seg_in, &

                basin_potet => model_potet%basin_potet, &
                potet => model_potet%potet, &

                nowtime => model_time%Nowtime, &

                hru_ppt => model_precip%hru_ppt, &

                transp_on => model_transp%transp_on, &

                hru_intcpevap => intcp%hru_intcpevap, &

                snow_evap => snow%snow_evap, &
                snowcov_area => snow%snowcov_area, &

                cov_type => param_data%cov_type%values, &
                fastcoef_lin => param_data%fastcoef_lin%values, &
                fastcoef_sq => param_data%fastcoef_sq%values, &
                hru_area => param_data%hru_area%values, &
                hru_type => param_data%hru_type%values, &
                lake_evap_adj => param_data%lake_evap_adj%values, &
                pref_flow_den => param_data%pref_flow_den%values, &
                sat_threshold => param_data%sat_threshold%values, &
                soil_moist_max => param_data%soil_moist_max%values, &
                soil_type => param_data%soil_type%values, &
                soil2gw_max => param_data%soil2gw_max%values, &
                slowcoef_lin => param_data%slowcoef_lin%values, &
                slowcoef_sq => param_data%slowcoef_sq%values, &
                ssr2gw_exp => param_data%ssr2gw_exp%values, &
                ssr2gw_rate => param_data%ssr2gw_rate%values, &

                soil_rechr => model_climate%soil_rechr, &
                soil_rechr_max => model_climate%soil_rechr_max, &
                soil_moist => model_climate%soil_moist)

        ! call ieee_set_halting_mode(ieee_all, .false.)

        ! TODO: 2018-06-21 - Uncomment when GSFLOW stuff is figured out.
        ! ! if (model_mode(1)%s == 'GSFLOW') then
        ! if (gsflow_mode) then
        !   if (Kkiter == 0) STOP 'ERROR, problem with KKITER, equals 0'
        !
        !   if (Kkiter == 1) then
        !     ! it0_* variables used with MODFLOW integration to save iteration states.
        !     do k=1, active_hrus
        !       chru = hru_route_order(k)
        !       this%it0_soil_rechr(chru) = soil_rechr(chru)
        !       this%it0_soil_moist(chru) = soil_moist(chru)
        !       this%it0_ssres_stor(chru) = this%ssres_stor(chru)
        !       this%it0_pref_flow_stor(chru) = this%pref_flow_stor(chru)
        !       this%it0_slow_stor(chru) = this%slow_stor(chru)
        !       this%it0_sroff(chru) = sroff(chru)
        !       this%it0_potet(chru) = potet(chru)
        !     enddo
        !
        !     this%it0_basin_soil_moist = this%basin_soil_moist
        !     this%it0_basin_ssstor = this%basin_ssstor
        !     this%it0_gravity_stor_res = this%gravity_stor_res
        !     this%it0_strm_seg_in = strm_seg_in
        !     this%gw2sm_grav = 0.0
        !   else
        !     do k=1, active_hrus
        !       chru = hru_route_order(k)
        !       soil_rechr(chru) = this%it0_soil_rechr(chru)
        !       soil_moist(chru) = this%it0_soil_moist(chru)
        !       this%ssres_stor(chru) = this%it0_ssres_stor(chru)
        !       this%pref_flow_stor(chru) = this%it0_pref_flow_stor(chru)
        !       this%slow_stor(chru) = this%it0_slow_stor(chru)
        !       sroff(chru) = this%it0_sroff(chru)
        !       potet(chru) = this%it0_potet(chru)
        !     enddo
        !
        !     this%basin_soil_moist = this%it0_basin_soil_moist
        !     this%basin_ssstor = this%it0_basin_ssstor
        !     this%gravity_stor_res = this%it0_gravity_stor_res
        !     strm_seg_in = this%it0_strm_seg_in
        !   endif
        !
        !   this%sm2gw_grav = 0.0
        ! endif

        ! TODO: Uncomment once cascade model and lakes have been converted.
        ! if (cascade_flag == 1) then
        !   do k=1, active_hrus
        !     chru = hru_route_order(k)
        !     this%upslope_interflow(chru) = 0.0_dp
        !     this%upslope_dunnianflow(chru) = 0.0_dp
        !   enddo
        !
        !   if (numlake_hrus > 0) then
        !     this%lakein_sz = 0.0_dp
        !     this%basin_lakeinsz = 0.0_dp
        !   endif
        ! endif


        call ieee_get_status(status_value)
        call ieee_set_halting_mode(ieee_custom, .false.)
        call ieee_set_flag(ieee_custom, .false.)



        if (print_debug == 1) then
          this%soil_moist_ante = soil_moist
          this%ssres_stor_ante = this%ssres_stor
          this%last_soil_moist = this%basin_soil_moist
          this%last_ssstor = this%basin_ssstor
        endif

        ! TODO: Do basin variables need to be initialized for each timestep?
        basin_sroff = 0.0_dp
        call this%reset_basin_vars()

        gwin = 0.0_dp
        update_potet = 0

        ! soil_to_gw for whole HRU
        this%soil_to_gw = 0.0_dp
        this%soil_to_ssr = 0.0_dp
        this%ssr_to_gw = 0.0_dp
        this%slow_flow = 0.0_dp
        this%ssres_flow = 0.0_dp

        this%potet_rechr = 0.0_dp
        this%potet_lower = 0.0_dp
        this%snow_free = 1.0 - snowcov_area

        this%hru_actet = hru_impervevap + hru_intcpevap + snow_evap

        if (dprst_flag == 1) then
          this%hru_actet = this%hru_actet + dprst_evap_hru
        endif

        do k=1, active_hrus
          chru = hru_route_order(k)

          ! TODO: 2018-06-21 - Uncomment once lakes are working.
          ! if (hru_type(chru) == LAKE) then ! lake or reservoir
          !   ! WARNING: RSR, if hru_actet>water in lake, then budget error
          !   this%hru_actet(chru) = (potet(chru) - this%hru_actet(chru)) * lake_evap_adj(Nowmonth, Lake_hru_id(chru))
          !
          !   if (this%hru_actet(chru) > potet(chru)) then
          !     print *, 'WARNING, lake evap > potet, for HRU:', chru, ' potential ET increased to adjusted lake ET'
          !     print *, this%hru_actet(chru), potet(chru), this%hru_actet(chru) - potet(chru)
          !     basin_potet = basin_potet - dble(potet(chru) * hru_area(chru))
          !     potet(chru) = this%hru_actet(chru) ! WARNING: This could be a problem when it happens
          !     basin_potet = basin_potet + dble(potet(chru) * hru_area(chru))
          !     update_potet = 1
          !   endif
          !
          !   this%unused_potet(chru) = potet(chru) - this%hru_actet(chru)
          !   this%basin_actet = this%basin_actet + dble(this%hru_actet(chru) * hru_area(chru))
          !   Basin_lakeevap = Basin_lakeevap + dble(this%hru_actet(chru) * hru_area(chru))
          !   this%basin_lakeprecip = this%basin_lakeprecip + dble(hru_ppt(chru) * hru_area(chru))
          !
          !   if (cascade_flag == 1) then
          !     ! If lake HRU doesn't cascade, should we limit ET to
          !     ! water entering the HRU to this point (no gwflow yet)
          !     this%lakein_sz(chru) = this%upslope_interflow(chru) + this%upslope_dunnianflow(chru)
          !     this%basin_lakeinsz = this%basin_lakeinsz + this%lakein_sz(chru) * Hru_area_dble(chru)
          !   endif
          !
          !   cycle
          ! endif
          ! --------------------------------

          avail_potet = max(0.0_dp, dble(potet(chru)) - this%hru_actet(chru))

          ! ******Add infiltration to soil and compute excess
          ! NOTE: hru_area_perv(chru) has to be > 0.0
          dunnianflw = 0.0_dp
          dunnianflw_pfr = 0.0_dp
          dunnianflw_gvr = 0.0_dp
          interflow = 0.0_dp

          ! ****** Add infiltration to soil and compute excess
          !   infil_tot is the depth in whole HRU
          !   capillary reservoir for pervious area
          !   preferential flow reservoir for whole HRU
          !   gravity reservoir for whole HRU
          !   upslope flow for whole HRU

          ! ****** If cascading flow available from upslope cascades
          ! ****** add soil excess (Dunnian flow) to infiltration
          !   hru_frac_perv(chru) has to be > 0.001
          !   infil for pervious portion of HRU
          capwater_maxin = infil(chru)

          if (exponent(capwater_maxin) - range(capwater_maxin) >= 0) then
            write(*, 9005) MODNAME, ' WARNING: capwater_maxin less than TINY resetting to zero.', capwater_maxin
            capwater_maxin = 0.0_dp
          endif

          9005 format(A, A, es12.4e2)

          ! Compute preferential flow and storage, and any dunnian flow
          prefflow = 0.0_dp
          if (this%pref_flow_flag(chru)) then
            this%pref_flow_infil(chru) = 0.0_dp
            pref_flow_maxin = 0.0_dp

            if (capwater_maxin > 0.0) then
              ! pref_flow for whole HRU
              pref_flow_maxin = capwater_maxin * dble(pref_flow_den(chru))
              capwater_maxin = capwater_maxin - pref_flow_maxin
              pref_flow_maxin = pref_flow_maxin * hru_frac_perv(chru)

              ! Compute contribution to preferential-flow reservoir storage
              this%pref_flow_stor(chru) = this%pref_flow_stor(chru) + pref_flow_maxin
              dunnianflw_pfr = max(0.0_dp, this%pref_flow_stor(chru) - this%pref_flow_max(chru))

              if (dunnianflw_pfr > 0.0_dp) then
                this%pref_flow_stor(chru) = this%pref_flow_max(chru)
              endif

              this%pref_flow_infil(chru) = pref_flow_maxin - dunnianflw_pfr
            endif

            this%pfr_dunnian_flow(chru) = dunnianflw_pfr
          endif

          if (cascade_flag == 1) then
            cap_upflow_max = (this%upslope_dunnianflow(chru) + this%upslope_interflow(chru)) / hru_frac_perv(chru)
            capwater_maxin = capwater_maxin + cap_upflow_max
            this%basin_cap_up_max = this%basin_cap_up_max + cap_upflow_max * hru_area_perv(chru)
          endif

          this%cap_infil_tot(chru) = capwater_maxin * hru_frac_perv(chru)

          ! ****** Add infiltration to soil and compute excess
          ! gvr_maxin = 0.0
          this%cap_waterin(chru) = capwater_maxin

          ! if (soil_moist(chru) < DNEARZERO) then
          !   write(output_unit, 9008) MODNAME, '%run WARNING: soil_moist less than 10e-16,', soil_moist(chru), ', reset to zero ', nowtime(1:3)
          !   soil_moist(chru) = 0.0
          ! endif
          !
          ! if (soil_rechr(chru) < DNEARZERO) then
          !   write(output_unit, 9008) MODNAME, '%run WARNING: soil_rechr less than 10e-16,', soil_rechr(chru), ', reset to zero ', nowtime(1:3)
          !   soil_rechr(chru) = 0.0
          ! endif

          ! 9008 format(A,A,es12.4e2,A,I4,2('/', I2.2))

          ! Call even if capwater_maxin = 0, just in case soil_moist now > soil_moist_max
          if (capwater_maxin + soil_moist(chru) > 0.0_dp) then
            call this%compute_soilmoist(this%soil2gw_flag(chru), hru_frac_perv(chru), soil_moist_max(chru), &
                                        soil_rechr_max(chru), soil2gw_max(chru), &
                                        this%cap_waterin(chru), soil_moist(chru), &
                                        soil_rechr(chru), this%soil_to_gw(chru), this%soil_to_ssr(chru))

            this%cap_waterin(chru) = this%cap_waterin(chru) * hru_frac_perv(chru)
            this%basin_sm2gvr_max = this%basin_sm2gvr_max + this%soil_to_ssr(chru) * hru_area(chru)
          endif

          ! soil_to_ssr for whole HRU
          ! this%soil_to_ssr(chru) = gvr_maxin

          ! Compute slow interflow and ssr_to_gw
          topfr = 0.0_dp

          ! TODO: 2018-06-21 Uncomment when GSFLOW stuff is figured out.
          ! ! if (model_mode(1)%s == 'GSFLOW') then
          ! if (gsflow_mode) then
          !   ! capacity for whole HRU
          !   capacity = (soil_moist_max(chru) - soil_moist(chru)) * hru_frac_perv(chru)
          !   call this%compute_gravflow(chru, capacity, slowcoef_lin(chru), &
          !                         slowcoef_sq(chru), ssr2gw_rate(chru), ssr2gw_exp(chru), &
          !                         this%soil_to_ssr(chru), this%pref_flow_thrsh(chru), topfr, &
          !                         this%ssr_to_gw(chru), this%slow_flow(chru), this%slow_stor(chru), &
          !                         this%gvr2sm(chru), this%soil_to_gw(chru), gwin, hru_type(chru))
          !
          !   ! Adjust soil moisture with replenish amount
          !   if (this%gvr2sm(chru) > 0.0) then
          !     soil_moist(chru) = soil_moist(chru) + this%gvr2sm(chru) / hru_frac_perv(chru)
          !     ! if (soil_moist(chru)>soil_moist_max(chru)) &
          !     !    print *, 'sm>max', soil_moist(chru), soil_moist_max(chru), chru
          !     soil_rechr(chru) = soil_rechr(chru) + this%gvr2sm(chru) / hru_frac_perv(chru) * this%replenish_frac(chru)
          !     soil_rechr(chru) = min(soil_rechr_max(chru), soil_rechr(chru))
          !     this%basin_gvr2sm = this%basin_gvr2sm + dble(this%gvr2sm(chru) * hru_area(chru))
          !     ! elseif ( this%gvr2sm(chru)<-NEARZERO ) then
          !     !  print *, 'negative gvr2sm, HRU:', chru, this%gvr2sm(chru)
          !     this%gvr2sm(chru) = 0.0
          !   endif
          !
          !   this%grav_gwin(chru) = sngl(gwin)
          !   this%basin_sz_gwin = this%basin_sz_gwin + gwin * dble(hru_area(chru))
          ! else
          ! if (model_mode(1)%s /= 'GSFLOW') then
          if (.not. gsflow_mode) then
            availh2o = this%slow_stor(chru) + this%soil_to_ssr(chru)

            if (hru_type(chru) == LAND) then
              topfr = max(0.0_dp, availh2o - this%pref_flow_thrsh(chru))
              ssresin = this%soil_to_ssr(chru) - topfr
              this%slow_stor(chru) = max(0.0_dp, availh2o - topfr)

              ! Compute slow contribution to interflow, if any
              if (this%slow_stor(chru) > NEARZERO) then
                call this%compute_interflow(slowcoef_lin(chru), slowcoef_sq(chru), &
                                            ssresin, this%slow_stor(chru), this%slow_flow(chru))
              endif
            elseif (hru_type(chru) == SWALE) then
              this%slow_stor(chru) = availh2o
            endif

            if (this%slow_stor(chru) > NEARZERO .and. ssr2gw_rate(chru) > 0.0) then
              call this%compute_gwflow(ssr2gw_rate(chru), ssr2gw_exp(chru), this%ssr_to_gw(chru), this%slow_stor(chru))
            endif
          endif

          ! Compute contribution to Dunnian flow from PFR, if any
          if (this%pref_flow_flag(chru)) then
            availh2o = this%pref_flow_stor(chru) + topfr
            dunnianflw_gvr = max(0.0_dp, availh2o - this%pref_flow_max(chru))

            if (dunnianflw_gvr > 0.0_dp) then
              ! topfr = topfr - dunnianflw_gvr
              topfr = max(0.0_dp, topfr - dunnianflw_gvr)
            endif

            this%pref_flow_in(chru) = this%pref_flow_infil(chru) + topfr
            this%pref_flow_stor(chru) = this%pref_flow_stor(chru) + topfr

            if (this%pref_flow_stor(chru) > 0.0) then
              call this%compute_interflow(fastcoef_lin(chru), fastcoef_sq(chru), &
                                          this%pref_flow_in(chru), this%pref_flow_stor(chru), &
                                          prefflow)
            endif
          elseif (hru_type(chru) == LAND) then
            dunnianflw_gvr = topfr  !?? is this right
          endif

          this%gvr2pfr(chru) = topfr

          ! ****** Compute actual evapotranspiration
          ! this%snow_free(chru) = 1.0 - snowcov_area(chru)
          ! this%potet_rechr(chru) = 0.0
          ! this%potet_lower(chru) = 0.0
          pervactet = 0.0

          if (soil_moist(chru) > 0.0) then
            call this%compute_szactet(model_time, transp_on(chru), cov_type(chru), &
                                      soil_type(chru), soil_moist_max(chru), &
                                      soil_rechr_max(chru), this%snow_free(chru),  &
                                      soil_moist(chru), soil_rechr(chru), &
                                      avail_potet, this%potet_rechr(chru), &
                                      this%potet_lower(chru), pervactet)
          endif



              call ieee_get_flag(ieee_custom, flag_value)
              if (any(flag_value)) then
                error_txt = ''

                ! Only checking for underflow and overflow
                if (flag_value(1)) then
                  error_txt = 'overflow'
                elseif (flag_value(4)) then
                  error_txt = 'underflow'
                else
                  error_txt = 'ieee_divide_by_zero or ieee_invalid'
                endif

                write(output_unit, 9009) MODNAME, '%run WARNING: ', error_txt, ' occurred []'
                write(output_unit, 9011) '    ', nowtime(1:3), soil_moist(chru), soil_rechr(chru), this%potet_rechr(chru), this%potet_lower(chru)

                call ieee_set_flag(ieee_custom, .false.)
              endif

              9009 format(A, A, A, A)
              9011 format(A,I4, 2('/', I2.2), 4es12.4e2)


          this%hru_actet(chru) = this%hru_actet(chru) + pervactet * hru_frac_perv(chru)
          avail_potet = dble(potet(chru)) - this%hru_actet(chru)
          this%perv_actet(chru) = pervactet
          this%soil_lower(chru) = soil_moist(chru) - soil_rechr(chru)

          ! If HRU cascades, compute interflow and excess flow to each HRU or stream
          if (hru_type(chru) == LAND) then
            interflow = this%slow_flow(chru) + prefflow

            this%basin_interflow_max = this%basin_interflow_max + interflow * hru_area(chru)
            dunnianflw = dunnianflw_gvr + dunnianflw_pfr
            this%dunnian_flow(chru) = dunnianflw

            ! TODO: 2018-06-21 - Uncomment once cascade module is converted.
            ! if (cascade_flag == 1) then
            !   if (Ncascade_hru(chru) > 0) then
            !     dnslowflow = 0.0
            !     dnpreflow = 0.0
            !     dndunn = 0.0
            !
            !     if (interflow + dunnianflw > 0.0) then
            !       call this%compute_cascades(chru, Ncascade_hru(chru), this%slow_flow(chru), &
            !                             prefflow, this%dunnian_flow(chru), dnslowflow, &
            !                             dnpreflow, dndunn)
            !       this%basin_dninterflow = this%basin_dninterflow + dble((dnslowflow + dnpreflow) * hru_area(chru))
            !       this%basin_dndunnianflow = this%basin_dndunnianflow + dble(dndunn * hru_area(chru))
            !     endif
            !
            !     this%hru_sz_cascadeflow(chru) = dnslowflow + dnpreflow + dndunn
            !          ! Cascade_interflow(chru) = dnslowflow + dnpreflow
            !          ! Cascade_dunnianflow(chru) = dndunn
            !     this%basin_dncascadeflow = this%basin_dncascadeflow + dble(this%hru_sz_cascadeflow(chru) * hru_area(chru))
            !   endif
            ! endif

            ! Treat pref_flow as interflow
            this%ssres_flow(chru) = this%slow_flow(chru)

            if (this%pref_flow_flag(chru)) then
              this%pref_flow(chru) = prefflow
              this%ssres_flow(chru) = this%ssres_flow(chru) + prefflow
            endif

            ! Treat dunnianflw as surface runoff to streams
            ! WARNING: PAN This is modifying sroff and basin_sroff from the srunoff module
            sroff(chru) = sroff(chru) + this%dunnian_flow(chru)
            basin_sroff = basin_sroff + dble(sroff(chru) * hru_area(chru))
            this%ssres_stor(chru) = this%slow_stor(chru) + this%pref_flow_stor(chru)
          else
            ! For swales
            availh2o = this%slow_stor(chru) - sat_threshold(chru)
            this%swale_actet(chru) = 0.0_dp

            if (availh2o > 0.0_dp) then
              ! If ponding, as storage > sat_threshold
              unsatisfied_et = potet(chru) - this%hru_actet(chru)

              if (unsatisfied_et > 0.0_dp) then
                availh2o = min(availh2o, unsatisfied_et)
                this%swale_actet(chru) = availh2o
                this%hru_actet(chru) = this%hru_actet(chru) + this%swale_actet(chru)
                this%slow_stor(chru) = this%slow_stor(chru) - this%swale_actet(chru)
                this%basin_swale_et = this%basin_swale_et + this%swale_actet(chru) * hru_area(chru)
              endif

              ! TODO: Uncomment once debug output of soilzone is figured out.
              ! if (print_debug == 7) then
              !   if (this%slow_stor(chru) > this%swale_limit(chru)) then
              !     write(DBGUNT, *) 'Swale ponding, HRU:', chru, &
              !             ' gravity reservoir is 3*sat_threshold', this%slow_stor(chru), sat_threshold(chru)
              !     call print_date(DBGUNT)
              !   endif
              ! endif
            endif

            this%ssres_stor(chru) = this%slow_stor(chru)
          endif


          ! if (this%soil_lower_stor_max(chru) > 0.0) then
          !   this%soil_lower_ratio(chru) = this%soil_lower(chru) / this%soil_lower_stor_max(chru)
          ! endif

          this%ssres_in(chru) = this%soil_to_ssr(chru) + this%pref_flow_infil(chru) + gwin
          ! this%soil_moist_tot(chru) = this%ssres_stor(chru) + soil_moist(chru) * hru_frac_perv(chru)

          ! ! DEBUG: This can be removed after debugging
          ! if (.not. ieee_is_normal(this%soil_moist_tot(chru))) then
          !   write(*, *) 'soilzone: soil_moist_tot is denormal. (chru, soil_moist_tot)'
          !   write(*, *) chru, this%soil_moist_tot(chru)
          ! end if
          ! write(*, *) 'SZ: ', chru, this%basin_sz_stor_frac, this%soil_moist_tot(chru), this%soil_zone_max(chru), hru_area(chru)

          ! this%recharge(chru) = this%soil_to_gw(chru) + this%ssr_to_gw(chru)
          !
          ! if (dprst_flag == 1) then
          !   this%recharge(chru) = this%recharge(chru) + sngl(dprst_seep_hru(chru))
          ! endif

          this%grav_dunnian_flow(chru) = dunnianflw_gvr
          this%unused_potet(chru) = potet(chru) - this%hru_actet(chru)
          this%basin_actet = this%basin_actet + this%hru_actet(chru) * hru_area(chru)
        enddo

        where (this%soil_lower_stor_max > 0.0)
          this%soil_lower_ratio = this%soil_lower / this%soil_lower_stor_max
        end where

        this%soil_moist_tot = this%ssres_stor + soil_moist * hru_frac_perv
        this%recharge = this%soil_to_gw + this%ssr_to_gw

        if (dprst_flag == 1) then
          this%recharge = this%recharge + dprst_seep_hru
        endif

        ! Compute basin totals
        this%basin_cap_infil_tot = sum(this%cap_infil_tot * hru_area, mask=active_mask) * basin_area_inv
        this%basin_capwaterin = sum(this%cap_waterin * hru_area, mask=active_mask) * basin_area_inv
        this%basin_dunnian = sum(this%dunnian_flow * hru_area, mask=(hru_type == LAND)) * basin_area_inv
        this%basin_dunnian_gvr = sum(this%grav_dunnian_flow * hru_area, mask=active_mask) * basin_area_inv
        this%basin_dunnian_pfr = sum(this%pfr_dunnian_flow * hru_area, mask=(this%pfr_dunnian_flow > 0.0)) * basin_area_inv
        this%basin_gvr2pfr = sum(this%gvr2pfr * hru_area, mask=(this%pref_flow_flag)) * basin_area_inv
        this%basin_gvr_stor_frac = sum(this%slow_stor / this%pref_flow_thrsh * hru_area, mask=(this%pref_flow_thrsh > 0.0)) * basin_area_inv
        this%basin_pfr_stor_frac = sum(this%pref_flow_stor / this%pref_flow_max * hru_area, mask=(this%pref_flow_flag)) * basin_area_inv
        this%basin_pref_flow_infil = sum(this%pref_flow_infil * hru_area, mask=active_mask) * basin_area_inv
        this%basin_pref_stor = sum(this%pref_flow_stor * hru_area, mask=(this%pref_flow_flag)) * basin_area_inv
        this%basin_prefflow = sum(this%pref_flow * hru_area, mask=(this%pref_flow_flag)) * basin_area_inv
        this%basin_recharge = sum(this%recharge * hru_area, mask=active_mask) * basin_area_inv
        this%basin_slowflow = sum(this%slow_flow * hru_area, mask=active_mask) * basin_area_inv
        this%basin_slstor = sum(this%slow_stor * hru_area, mask=active_mask) * basin_area_inv
        this%basin_sm2gvr = sum(this%soil_to_ssr * hru_area, mask=active_mask) * basin_area_inv
        this%basin_soil_moist_tot = sum(this%soil_moist_tot * hru_area, mask=active_mask) * basin_area_inv
        this%basin_soil_to_gw = sum(this%soil_to_gw * hru_area, mask=active_mask) * basin_area_inv
        this%basin_ssflow = sum(this%ssres_flow * hru_area, mask=active_mask) * basin_area_inv
        this%basin_ssin = sum(this%ssres_in * hru_area, mask=active_mask) * basin_area_inv
        this%basin_ssstor = sum(this%ssres_stor * hru_area, mask=active_mask) * basin_area_inv
        this%basin_sz2gw = sum(this%ssr_to_gw * hru_area, mask=active_mask) * basin_area_inv

        ! NOTE: Each r32 variable must be converted to r64 individually or a
        !       SIGFPE can occur when soil_moist_tot is divided by soil_zone_max
        this%basin_sz_stor_frac = sum(this%soil_moist_tot / dble(this%soil_zone_max) * dble(hru_area)) * basin_area_inv


        ! TODO: PAN Is it correct to use basin_area_inv with hru_area_perv??
        this%basin_cpr_stor_frac = sum(dble(soil_moist / soil_moist_max * hru_area_perv), mask=active_mask) * basin_area_inv
        this%basin_perv_et = sum(this%perv_actet * hru_area_perv, mask=active_mask) * basin_area_inv
        this%basin_soil_lower_stor_frac = sum(this%soil_lower_ratio * hru_area_perv, mask=active_mask) * basin_area_inv
        this%basin_soil_moist = sum(soil_moist * hru_area_perv, mask=active_mask) * basin_area_inv
        this%basin_soil_rechr = sum(soil_rechr * hru_area_perv, mask=active_mask) * basin_area_inv
        this%basin_soil_rechr_stor_frac = sum(soil_rechr / dble(soil_rechr_max * hru_area_perv), mask=active_mask) * basin_area_inv


        this%basin_actet = this%basin_actet * basin_area_inv
        this%basin_cap_up_max = this%basin_cap_up_max * basin_area_inv
        this%basin_dncascadeflow = this%basin_dncascadeflow * basin_area_inv
        this%basin_dndunnianflow = this%basin_dndunnianflow * basin_area_inv
        this%basin_dninterflow = this%basin_dninterflow * basin_area_inv
        this%basin_gvr2sm = this%basin_gvr2sm * basin_area_inv
        this%basin_interflow_max = this%basin_interflow_max * basin_area_inv
        this%basin_sm2gvr_max = this%basin_sm2gvr_max * basin_area_inv
        this%basin_swale_et = this%basin_swale_et * basin_area_inv
        this%basin_sz_gwin = this%basin_sz_gwin * basin_area_inv

        if (update_potet == 1) then
          ! WARNING: This modifies basin_potet from potet module
          basin_potet = basin_potet * basin_area_inv
        endif

        ! WARNING: This replaces basin_sroff from srunoff module
        basin_sroff = basin_sroff * basin_area_inv

        ! TODO: Uncomment once lakes are working.
        ! if (nlake > 0) then
        !   Basin_lakeevap = Basin_lakeevap * basin_area_inv
        !   this%basin_lakeprecip = this%basin_lakeprecip * basin_area_inv
        !   this%basin_lakeinsz = this%basin_lakeinsz * basin_area_inv
        !   Basin_lake_stor = Basin_lake_stor + this%basin_lakeprecip - Basin_lakeevap
        ! endif

      end associate
    end subroutine


    module subroutine cleanup_Soilzone(this)
      class(Soilzone) :: this
        !! Soilzone class
    end subroutine


    !***********************************************************************
    ! Adjust soil moist based on being below field capacity (capacity)
    ! and preferential-flow threshold (pref_flow_thrsh)
    !***********************************************************************
    module subroutine check_gvr_sm(capacity, depth, frac, gvr2sm, input)
      implicit none

      ! Arguments
      real(r32), intent(inout) :: capacity
      real(r64), intent(inout) :: depth
      real(r64), intent(in) :: frac
      real(r32), intent(inout) :: gvr2sm
      real(r32), intent(inout) :: input

      ! Local Variables
      real(r32) :: to_sm
      real(r32) :: frac_sngl

      !************************************************************************
      ! Check to see if soil is below capacity, if so add up to field capacity.
      !    capacity is for whole HRU
      !    to_sm and gvr2sm are for whole HRU
      frac_sngl = sngl(frac)

      ! Fill up capillary with part of gravity water
      to_sm = capacity

      ! Take all gravity water and put in capillary
      if (to_sm > depth) then
        to_sm = sngl(depth)
      endif

      ! Compute adjustment to soil moist to get to field capacity
      capacity = capacity - to_sm * frac_sngl

      if (capacity < 0.0) then
        to_sm = to_sm - capacity * frac_sngl
        capacity = 0.0
      endif

      gvr2sm = gvr2sm + to_sm * frac_sngl
      depth = depth - to_sm
      ! IF ( depth<0.0 ) PRINT *, 'depth<0', depth
      ! IF ( depth<CLOSEZERO ) depth = 0.0
      input = input - to_sm * frac_sngl
    end subroutine



    !***********************************************************************
    ! Compute cascading interflow and excess flow
    !***********************************************************************
    module subroutine compute_cascades(this, runoff, model_time, ihru, ncascade_hru, &
                                slowflow, preflow, dunnian, dnslowflow, &
                                dnpreflow, dndunnflow)
      implicit none

      ! Arguments
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      type(Srunoff), intent(inout) :: runoff
      type(Time_t), intent(in) :: model_time
      ! type(Cascade), intent(in) :: model_cascade
      integer(i32), intent(in) :: ihru
      integer(i32), intent(in) :: ncascade_hru
      real(r32), intent(inout) :: dunnian
      real(r32), intent(inout) :: slowflow
      real(r32), intent(inout) :: preflow
      real(r32), intent(inout) :: dnslowflow
      real(r32), intent(inout) :: dnpreflow
      real(r32), intent(inout) :: dndunnflow

      ! *this
      ! *runoff
      !  model_time
      !  ihru => chru
      !  ncascade_hru => Ncascade_hru(chru)
      ! *slowflow => this%slow_flow(chru)
      ! *preflow => prefflow
      ! *dunnian => this%dunnian_flow(chru)
      ! *dnslowflow => dnslowflow
      ! *dnpreflow => dnpreflow
      ! *dndunnflow => dndunn

      ! Local Variables
      ! TODO: Uncomment when cascade module is converted
      ! integer(i32) :: j

      integer(i32) :: k

      ! TODO: Uncomment next 2 when cascade module is converted
      ! real(r32) :: frac
      ! real(r32) :: fracwt

      ! Cascade
      ! hru_down, hru_down_frac, hru_down_fracwt, cascade_area

      ! Srunoff
      ! strm_seg_in(RW)

      ! Time_t
      ! Cfs_conv

      !***********************************************************************
      associate(cfs_conv => model_time%cfs_conv, &
                ! cascade_area => model_cascade%cascade_area, &
                ! hru_down => model_cascade%hru_down, &
                ! hru_down_frac => model_cascade%hru_down_frac, &
                ! hru_down_fracwt => model_cascade%hru_down_fracwt, &
                strm_seg_in => runoff%strm_seg_in)

        do k=1, ncascade_hru
          ! TODO: Uncomment once cascade module is completed
          ! j = hru_down(k, ihru)
          ! frac = hru_down_frac(k, ihru)
          ! ! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
          !
          ! if (j > 0) then
          !   fracwt = hru_down_fracwt(k, ihru)
          !   this%upslope_interflow(j) = this%upslope_interflow(j) + dble((slowflow + preflow) * fracwt)
          !   this%upslope_dunnianflow(j) = this%upslope_dunnianflow(j) + dble(dunnian * fracwt)
          !   dnslowflow = dnslowflow + slowflow * frac
          !   dnpreflow = dnpreflow + preflow * frac
          !   dndunnflow = dndunnflow + dunnian * frac
          !   ! if hru_down(k, Ihru) < 0, cascade contributes to a stream
          ! elseif (j < 0) then
          !   j = iabs(j)
          !   strm_seg_in(j) = strm_seg_in(j) + dble((slowflow + preflow + dunnian) * cascade_area(k, ihru)) * Cfs_conv
          ! endif
        enddo

        ! reset Slowflow, Preflow, and Dunnian_flow as they accumulate flow to streams
        slowflow = slowflow - dnslowflow
        preflow = preflow - dnpreflow
        dunnian = dunnian - dndunnflow
      end associate
    end subroutine



    !***********************************************************************
    ! Compute interflow and flow to groundwater reservoir
    !***********************************************************************
    module subroutine compute_gravflow(this, ctl_data, param_data, runoff, &
                                       ihru, capacity, slowcoef_lin, &
                                       slowcoef_sq, ssr2gw_rate, ssr2gw_exp, &
                                       gvr_maxin, pref_flow_thrsh, gvr2pfr, &
                                       ssr_to_gw, slow_flow, slow_stor, gvr2sm, &
                                       soil_to_gw, gwin, hru_type)
      use prms_constants, only: dp, NEARZERO
      implicit none


      !  ctl_data
      !  param_data
      !  runoff
      !  ihru => chru
      ! *capacity => capacity
      !  slowcoef_lin => slowcoef_lin(chru)
      !  slowcoef_sq => slowcoef_sq(chru)
      !  ssr2gw_rate => ssr2gw_rate(chru)
      !  ssr2gw_exp => ssr2gw_exp(chru)
      !  gvr_maxin => this%soil_to_ssr(chru)
      !  pref_flow_thrsh => this%pref_flow_thrsh(chru)
      ! ~gvr2pfr => topfr
      ! ~ssr_to_gw => ssr_to_gw(chru)
      ! ~slow_flow => slow_flow(chru)
      ! ~slow_stor => this%slow_stor(chru)
      ! ~gvr2sm => this%gvr2sm(chru)
      !  soil_to_gw => soil_to_gw(chru)
      ! ~gwin => gwin
      !  hru_type => hru_type(chru)


      ! Arguments
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Srunoff), intent(in) :: runoff
      integer(i32), intent(in) :: ihru
      real(r32), intent(inout) :: capacity
      real(r32), intent(in) :: slowcoef_lin
      real(r32), intent(in) :: slowcoef_sq
      real(r32), intent(in) :: ssr2gw_rate
      real(r32), intent(in) :: ssr2gw_exp
      real(r32), intent(in) :: gvr_maxin
      real(r32), intent(in) :: pref_flow_thrsh
      real(r32), intent(out) :: gvr2pfr
      real(r32), intent(out) :: ssr_to_gw
      real(r64), intent(out) :: slow_flow
      real(r64), intent(out) :: slow_stor
      real(r32), intent(out) :: gvr2sm
      real(r32), intent(in) :: soil_to_gw
      real(r64), intent(out) :: gwin
      integer(i32), intent(in) :: hru_type

      ! Local Variables
      integer(i32) :: igvr
      integer(i32) :: j

      real(r64) :: depth
      real(r64) :: extra_water
      real(r64) :: gvrin_actual
      real(r32) :: input
      real(r32) :: perc
      real(r64) :: slowflow

      real(r64) :: frac
      real(r64) :: slflow
      real(r64) :: slowstor
      real(r64) :: togw
      real(r64) :: topfr

      ! Control
      ! dprst_flag, print_debug

      ! Parameter
      ! hru_type, slowcoef_lin, slowcoef_sq, ssr2gw_exp, ssr2gw_rate

      ! Srunoff
      ! dprst_seep_hru

      !***********************************************************************
      associate(dprst_flag => ctl_data%dprst_flag%value, &
                print_debug => ctl_data%print_debug%value, &
                ! hru_type => param_data%hru_type%values, &
                ! slowcoef_lin => param_data%slowcoef_lin%values, &
                ! slowcoef_sq => param_data%slowcoef_sq%values, &
                ! ssr2gw_exp => param_data%ssr2gw_exp%values, &
                ! ssr2gw_rate => param_data%ssr2gw_rate%values, &
                dprst_seep_hru => runoff%dprst_seep_hru)

        ! capacity is for whole HRU
        ! soil_to_gw is for whole HRU
        ! TODO:
        ! Use VKS as a function of slope (vector analysis) instead of coef_lin
        ! coef_lin for pref_flow needs to be VKS lateral times a factor
        ! Change slow to interflow
        ! In init, set an array dimensioned by nhrucell to vks*mfl_to_inch

        gwin = 0.0_dp
        gvr2sm = 0.0
        topfr = 0.0_dp
        slflow = 0.0_dp
        togw = 0.0_dp
        slowstor = 0.0_dp

        do j=1, this%hru_gvr_count(ihru)
          igvr = this%hru_gvr_index(j, ihru)
          frac = this%gvr_hru_pct_adjusted(igvr)
          gwin = gwin + dble(this%gw2sm_grav(igvr)) * frac
          input = gvr_maxin + this%gw2sm_grav(igvr)
          depth = this%gravity_stor_res(igvr) + input

          if (depth > 0.0 .and. capacity > 0.0) then
            call this%check_gvr_sm(capacity, depth, frac, gvr2sm, input)
          endif

          if (hru_type == 1) then
            extra_water = max(0.0_dp, depth - pref_flow_thrsh)

            if (extra_water > 0.0) then
              ! Compute contribution to preferential-flow reservoir storage
              topfr = topfr + dble(extra_water) * frac
              depth = pref_flow_thrsh
            endif
            gvrin_actual = max(0.0, input - sngl(extra_water))

            ! Compute contribution to slow interflow, if any
            if (depth > 0.0) then
              ! coef_lin, coef_sq, ssres_in, storage, inter_flow
              call this%compute_interflow(slowcoef_lin, slowcoef_sq, gvrin_actual, depth, slowflow)
              slflow = slflow + dble(slowflow) * frac
            endif
          endif

          ! Compute flow to groundwater, if any
          if (depth > 0.0) then
            if (ssr2gw_rate > NEARZERO) then
              ! Use VKS instead of rate  ???????????????
              perc = ssr2gw_rate * sngl(depth**ssr2gw_exp)

              if (perc < 0.0) then
                perc = 0.0
              elseif (perc > depth) then
                perc = sngl(depth)
              endif

              depth = depth - perc
              ! if ( this%sm2gw_grav(igvr)>0.0 ) print*,'problem',this%sm2gw_grav(igvr),igvr
              this%sm2gw_grav(igvr) = perc
              togw = togw + dble(perc) * frac
            endif
            ! ELSE ! GVRs can go negative if flux change in MODFLOW final iteration decreases, so don't set to 0
            !   if(depth<0.0) print *, 'depth<0', depth, ihru
            !   depth = 0.0
          endif

          this%gravity_stor_res(igvr) = depth
          slowstor = slowstor + depth * frac

          ! add any direct recharge from soil infiltration
          this%sm2gw_grav(igvr) = this%sm2gw_grav(igvr) + soil_to_gw

          if (dprst_flag == 1) then
            this%sm2gw_grav(igvr) = this%sm2gw_grav(igvr) + sngl(dprst_seep_hru(ihru))
          endif
        enddo ! end loop of GVRs in the HRU

        gvr2pfr = sngl(topfr)
        slow_flow = sngl(slflow)
        ssr_to_gw = sngl(togw)
        slow_stor = sngl(slowstor)

        if (slow_stor > pref_flow_thrsh) then
          if (print_debug > -1 .and. hru_type == 1) then
            print *, 'slow_stor > thrsh', slow_stor, pref_flow_thrsh, ' HRU:', ihru, ' type:', hru_type
          endif
        endif
      end associate
    end subroutine


    !***********************************************************************
    ! Compute interflow and flow to groundwater reservoir
    !***********************************************************************
    module subroutine compute_gwflow(ssr2gw_rate, ssr2gw_exp, ssr_to_gw, slow_stor)
      use prms_constants, only: dp
      implicit none

      ! Arguments
      real(r32), intent(in) :: ssr2gw_rate
      real(r32), intent(in) :: ssr2gw_exp
      real(r64), intent(inout) :: ssr_to_gw
      real(r64), intent(inout) :: slow_stor

      ! ********************************************************************
      ! ****** Compute flow to groundwater
      ssr_to_gw = max(0.0_dp, ssr2gw_rate * slow_stor**ssr2gw_exp)
      ssr_to_gw = min(ssr_to_gw, slow_stor)

      slow_stor = slow_stor - ssr_to_gw
    end subroutine


    !***********************************************************************
    ! Compute subsurface lateral flow
    !***********************************************************************
    module subroutine compute_interflow(coef_lin, coef_sq, ssres_in, storage, &
                                        inter_flow)
      implicit none

      ! Arguments
      real(r32), intent(in) :: coef_lin
      real(r32), intent(in) :: coef_sq
      real(r64), intent(in) :: ssres_in
      real(r64), intent(inout) :: storage
      real(r64), intent(inout) :: inter_flow

      ! Local Variables
      real(r32) :: c1
      real(r32) :: c2
      real(r32) :: c3
      real(r32) :: sos

      ! ***********************************************************************
      ! inter_flow is in inches for the timestep
      ! ****** compute interflow
      if (coef_lin <= 0.0 .and. ssres_in <= 0.0) then
        c1 = coef_sq * sngl(storage)
        inter_flow = storage * dble(c1 / (1.0 + c1))
      elseif (coef_lin > 0.0 .and. coef_sq <= 0.0) then
        c2 = 1.0 - exp(-coef_lin)
        inter_flow = ssres_in * (1.0 - c2 / coef_lin) + storage * c2
      elseif (coef_sq > 0.0) then
        c3 = sqrt(coef_lin**2.0 + 4.0 * coef_sq * sngl(ssres_in))
        sos = sngl(storage) - ((c3 - coef_lin) / (2.0 * coef_sq))

        if (c3 == 0.0) then
          STOP 'ERROR, in compute_interflow sos=0, please contact code developers'
        endif

        c1 = coef_sq * sos / c3
        c2 = 1.0 - exp(-c3)

        if (1.0 + c1 * c2 > 0.0) then
          inter_flow = ssres_in + (sos * (1.0 + c1) * c2) / (1.0 + c1 * c2)
        else
          inter_flow = ssres_in
        endif
      else
        inter_flow = 0.0
      endif

      ! TODO: Check if inter_flow < 0.0; if so, throw warning, reset to zero
      inter_flow = min(inter_flow, storage)
      ! if (inter_flow > storage) then
      !   inter_flow = storage
      ! endif

      storage = storage - inter_flow
    end subroutine


    !***********************************************************************
    ! Add infiltration to soil and compute excess
    ! soil_to_gw and soil_to_ssr for whole HRU
    !***********************************************************************
    module subroutine compute_soilmoist(soil2gw_flag, perv_frac, soil_moist_max, &
                                        soil_rechr_max, soil2gw_max, infil, &
                                        soil_moist, soil_rechr, soil_to_gw, soil_to_ssr)
      use iso_fortran_env, only: output_unit
      use prms_constants, only: dp
      implicit none

      ! Arguments
      ! integer(i32), intent(in) :: soil2gw_flag
      logical, intent(in) :: soil2gw_flag
      real(r32), intent(in) :: perv_frac
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(in) :: soil2gw_max
      real(r64), intent(inout) :: infil
      real(r64), intent(inout) :: soil_moist
      real(r64), intent(inout) :: soil_rechr
      real(r64), intent(inout) :: soil_to_gw
      real(r64), intent(inout) :: soil_to_ssr

      !  Soil2gw => this%soil2gw_flag(chru)
      !  perv_frac => perv_frac               local
      !  soil_moist_max => soil_moist_max(chru)  P
      !  soil_rechr_max => soil_rechr_max(chru)  P
      !  Soil2gw_max => soil2gw_max(chru)        P
      ! *infil => this%cap_waterin(chru)
      ! *soil_moist => soil_moist(chru)          FV
      ! *soil_rechr => soil_rechr(chru)          FV
      ! *soil_to_gw => this%soil_to_gw(chru)
      ! *Soil_to_ssr => gvr_maxin             local

      ! Local Variables
      real(r64) :: excess

      !***********************************************************************
      if (minexponent(soil_moist) - exponent(soil_moist) >= 0) then
        write(output_unit, 9005) MODNAME, '%compute_soilmoist WARNING: soil_moist underflow, reset to zero'
        soil_moist = 0.0
      endif

      if (minexponent(soil_rechr) - exponent(soil_rechr) >= 0) then
        write(output_unit, 9005) MODNAME, '%compute_soilmoist WARNING: soil_rechr underflow, reset to zero'
        soil_rechr = 0.0
      endif

      9005 format(A,A)

      soil_rechr = min(soil_rechr + infil, dble(soil_rechr_max))

      ! soil_moist_max from previous time step or soil_moist_max has
      ! changed for a restart simulation
      excess = sngl(soil_moist + infil)
      soil_moist = min(excess, dble(soil_moist_max))
      excess = (excess - soil_moist_max) * perv_frac

      if (excess > 0.0) then
        if (soil2gw_flag) then
          soil_to_gw = min(dble(soil2gw_max), excess)
          excess = excess - soil_to_gw
        endif

        if (excess > infil * perv_frac) then
          ! Probably dynamic
          infil = 0.0
        else
          !???? what if infil<0 ??? might happen with dynamic and small values,
          !???? maybe ABS < NEARZERO = 0.0
          infil = infil - excess / perv_frac
        endif

        soil_to_ssr = max(0.0_dp, excess)
      endif
    end subroutine


    !***********************************************************************
    ! Compute actual evapotranspiration
    !***********************************************************************
    module subroutine compute_szactet(this, model_time, transp_on, cov_type, soil_type, &
                                      soil_moist_max, soil_rechr_max, snow_free, &
                                      soil_moist, soil_rechr, avail_potet, &
                                      potet_rechr, potet_lower, perv_actet)
      use iso_fortran_env, only: output_unit
      use prms_constants, only: dp, NEARZERO, SAND, LOAM, CLAY, ET_DEFAULT, EVAP_ONLY, EVAP_PLUS_TRANSP
      ! use ieee_arithmetic
      ! use ieee_exceptions
      ! use ieee_features
      implicit none

      ! TODO: 2018-06-21 - Use class variables if possible.
      ! NOTE: Class variables snow_free, potet_rechr, and potet_lower are modified

      ! Arguments
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      type(Time_t), intent(in) :: model_time
      logical, intent(in) :: transp_on
      integer(i32), intent(in) :: cov_type
      integer(i32), intent(in) :: soil_type
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(in) :: snow_free
      real(r64), intent(inout) :: soil_moist
      real(r64), intent(inout) :: soil_rechr
      real(r64), intent(inout) :: avail_potet
      real(r64), intent(inout) :: potet_rechr
      real(r64), intent(inout) :: potet_lower
      real(r64), intent(out) :: perv_actet

      !  soil_moist_max => soil_moist_max(chru)
      !  soil_rechr_max => soil_rechr_max(chru) (Climateflow)
      !  transp_on => transp_on(chru) (Transpiration)
      !  cov_type => cov_type(chru)   (Parameter)
      !  soil_type => soil_type(chru) (Parameter)
      ! *soil_moist => soil_moist(chru) (Climateflow)
      ! *soil_rechr => soil_rechr(chru) (Climateflow)
      ! ~perv_actet => pervactet  (local to caller)
      ! *avail_potet => avail_potet   (local to caller)
      !  snow_free => this%snow_free(chru)
      ! *potet_rechr => this%potet_rechr(chru)
      ! *potet_lower => this%potet_lower(chru)

      ! Local Variables
      real(r64), parameter :: ONETHIRD = 1.0_dp / 3.0_dp
      real(r64), parameter :: TWOTHIRDS = 2.0_dp / 3.0_dp


      real(r64) :: et
      real(r64) :: pctr
      real(r64) :: pcts

      real(r64) :: soil_moist_ante
      real(r64) :: soil_rechr_ante
      real(r64) :: avail_potet_ante

      real(r64) :: potet_rechr_ante
      real(r64) :: potet_lower_ante

      ! character(len=:), allocatable :: error_txt
      ! type(ieee_flag_type), parameter :: ieee_custom(4) = [ieee_usual, ieee_underflow]
      ! type(ieee_status_type) :: status_value
      ! logical, dimension(4) :: flag_value

      ! ***********************************************************************
      ! ****** Determine type of evapotranspiration.
      !          et_type=2    - evaporation only
      !          et_type=3    - transpiration plus evaporation
      !          et_type=1    - default

      ! print *, '****', avail_potet, transp_on, cov_type, snow_free
      ! call ieee_get_status(status_value)
      ! call ieee_set_halting_mode(ieee_custom, .false.)
      ! call ieee_set_flag(ieee_custom, .false.)

      associate(nowtime => model_time%Nowtime)

        soil_moist_ante = soil_moist
        soil_rechr_ante = soil_rechr
        avail_potet_ante = avail_potet

        potet_rechr_ante = potet_rechr
        potet_lower_ante = potet_lower

        if (avail_potet < NEARZERO) then
          this%et_type = ET_DEFAULT   ! 1
          avail_potet = 0.0
        elseif (.not. transp_on) then
          if (snow_free < 0.01) then
            this%et_type = ET_DEFAULT   ! 1
          else
            this%et_type = EVAP_ONLY    ! 2
          endif
        elseif (cov_type > 0) then
          this%et_type = EVAP_PLUS_TRANSP   ! 3
        elseif (snow_free < 0.01) then
          this%et_type = ET_DEFAULT   ! 1
        else
          this%et_type = EVAP_ONLY    ! 2
        endif

        ! if (minexponent(soil_moist) - exponent(soil_moist) >= 0) then
        !   write(output_unit, 9008) MODNAME, '%compute_szactet WARNING: soil_moist underflow,', soil_moist, ', reset to zero ', nowtime(1:3)
        !   soil_moist = 0.0
        ! endif
        !
        ! if (minexponent(soil_rechr) - exponent(soil_rechr) >= 0) then
        !   write(output_unit, 9008) MODNAME, '%compute_szactet WARNING: soil_rechr underflow,', soil_rechr, ', reset to zero ', nowtime(1:3)
        !   soil_rechr = 0.0
        ! endif

        ! if (this%et_type > 1) then
        if (any([EVAP_ONLY, EVAP_PLUS_TRANSP]==this%et_type)) then
          pcts = soil_moist / soil_moist_max
          pctr = soil_rechr / soil_rechr_max
          potet_lower = avail_potet
          potet_rechr = avail_potet

          select case(soil_type)
            case(SAND)
              ! ****** sandy soil
              if (pcts < 0.25) then
                potet_lower = 0.5 * pcts * avail_potet
              endif

              if (pctr < 0.25) then
                potet_rechr = 0.5 * pctr * avail_potet
              endif

            case(LOAM)
              ! ****** loam soil
              if (pcts < 0.5) then
                potet_lower = pcts * avail_potet
              endif

              if (pctr < 0.5) then
                potet_rechr = pctr * avail_potet
              endif

            case(CLAY)
              ! ****** clay soil
              if (pcts < TWOTHIRDS .and. pcts > ONETHIRD) then
                potet_lower = pcts * avail_potet
              elseif (pcts <= ONETHIRD) then
                potet_lower = 0.5 * pcts * avail_potet
              endif

              if (pctr < TWOTHIRDS .and. pctr > ONETHIRD) then
                potet_rechr = pctr * avail_potet
              elseif (pctr <= ONETHIRD) then
                potet_rechr = 0.5 * pctr * avail_potet
              endif
            case default
              ! pass
          end select

          ! if (minexponent(potet_lower) - exponent(potet_lower) >= 0) then
          !   write(output_unit, 9008) MODNAME, '%compute_szactet WARNING: potet_lower underflow,', potet_lower, ', reset to zero ', nowtime(1:3)
          !   potet_lower = 0.0
          ! endif
          !
          ! if (minexponent(potet_rechr) - exponent(potet_rechr) >= 0) then
          !   write(output_unit, 9008) MODNAME, '%compute_szactet WARNING: potet_rechr underflow,', potet_rechr, ', reset to zero ', nowtime(1:3)
          !   potet_rechr = 0.0
          ! endif

          ! ****** Soil moisture accounting
          if (this%et_type == EVAP_ONLY) then
            potet_rechr = potet_rechr * snow_free
          endif

          if (potet_rechr > soil_rechr) then
            potet_rechr = soil_rechr
            soil_rechr = 0.0_dp
          else
            soil_rechr = soil_rechr - potet_rechr
          endif

          if (this%et_type == EVAP_ONLY .or. potet_rechr >= potet_lower) then
            if (potet_rechr > soil_moist) then
              potet_rechr = soil_moist
              soil_moist = 0.0_dp
            else
              soil_moist = soil_moist - potet_rechr
            endif

            et = potet_rechr
          elseif (potet_lower > soil_moist) then
            et = soil_moist
            soil_moist = 0.0_dp
          else
            soil_moist = soil_moist - potet_lower
            et = potet_lower
          endif

          if (soil_rechr > soil_moist) then
             soil_rechr = soil_moist
          endif
        else
          et = 0.0
        endif

        ! 9007 format(A,A)

        ! if (minexponent(et) - exponent(et) >= 1) then
        !   write(output_unit, 9008) MODNAME, '%compute_szactet WARNING: perv_actet underflow,', et, ', reset to 0.0 ', nowtime(1:3)
        !   perv_actet = 0.0
        ! else
        !   perv_actet = et
        ! endif

        ! call ieee_get_flag(ieee_custom, flag_value)
        ! if (any(flag_value)) then
        !   error_txt = ''
        !
        !   ! Only checking for underflow and overflow
        !   if (flag_value(1)) then
        !     error_txt = 'overflow'
        !   elseif (flag_value(4)) then
        !     error_txt = 'underflow'
        !   else
        !     error_txt = 'ieee_divide_by_zero or ieee_invalid'
        !   endif
        !
        !   write(*, 9004) MODNAME, '%compute_szactet WARNING: ', error_txt, ' occurred [et, soil_moist, soil_rechr, potet_rechr, potet_lower, et_type, soil_type]'
        !   write(*, 9005) '    ', nowtime(1:3), et, soil_moist, soil_rechr, potet_rechr, potet_lower, pctr, pcts, this%et_type, soil_type
        !   write(*, *) 'Antecedent'
        !   write(*, *) soil_moist_ante, soil_rechr_ante, potet_rechr_ante, potet_lower_ante, avail_potet_ante
        !   ! write(*, *) '    (soil_moist_ante, soil_rechr_ante)', soil_moist_ante, soil_rechr_ante
        !   ! write(*, *) '    (avail_potet_ante, avail_potet)', avail_potet_ante, avail_potet
        !   ! write(*, *) '    (soil_moist_max, soil_rechr_max)', soil_moist_max, soil_rechr_max
        !   ! write(*, *) '    (exp, range)', exponent(soil_moist), range(soil_moist), epsilon(soil_moist), precision(soil_moist), minexponent(soil_moist)
        !   call ieee_set_flag(ieee_custom, .false.)
        ! endif
        !
        ! 9004 format(A,A,A,A)
        ! 9005 format(A,I4,2('/', I2.2),7es12.4e2, 2i3)

      end associate

      ! 9006 format(A,A,I4,2('/', I2.2))
      ! 9008 format(A,A,es12.4e2,A,I4,2('/', I2.2))
      ! call ieee_set_status(status_value)
    end subroutine


    module subroutine reset_basin_vars(this)
      use prms_constants, only: dp
      implicit none

      class(Soilzone), intent(inout) :: this
        !! Soilzone class

      ! ------------------------------------------------------------------------
      this%basin_actet = 0.0_dp
      this%basin_cap_up_max = 0.0_dp
      this%basin_dncascadeflow = 0.0_dp
      this%basin_dndunnianflow = 0.0_dp
      this%basin_dninterflow = 0.0_dp
      this%basin_gvr2sm = 0.0_dp
      this%basin_interflow_max = 0.0_dp
      this%basin_lakeevap = 0.0_dp
      this%basin_lakeinsz = 0.0_dp
      this%basin_lakeprecip = 0.0_dp
      this%basin_sm2gvr_max = 0.0_dp
      this%basin_swale_et = 0.0_dp
      this%basin_sz_gwin = 0.0_dp
    end subroutine

end submodule
