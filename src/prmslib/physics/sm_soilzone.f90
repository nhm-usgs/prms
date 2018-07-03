submodule (PRMS_SOILZONE) sm_soilzone
  contains
    module function constructor_Soilzone(ctl_data, param_data, model_basin, model_flow, snow) result(this)
      use prms_constants, only: dp, INACTIVE, LAND, LAKE, SWALE
      implicit none

      type(Soilzone) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Flowvars), intent(inout) :: model_flow
      type(Snowcomp), intent(in) :: snow

      ! Local variables
      integer(i32) :: i
      integer(i32) :: ii
      integer(i32) :: ihru
      integer(i32) :: icnt
      integer(i32) :: ierr

      real(r32) :: hruarea
      real(r32) :: hruperv

      ! Control
      ! nhru, nhrucell, nlake, nsegment,
      ! cascade_flag, init_vars_from_file, model_mode, print_debug

      ! Parameters
      ! hru_area, hru_type, gvr_hru_id, pref_flow_den, sat_threshold, soil_moist_max,
      ! soil2gw_max, ssstor_init_frac

      ! Basin
      ! hru_frac_perv, hru_perv, basin_area_inv

      ! flowvars
      ! soil_moist, soil_rechr, soil_rechr_max, slow_stor, ssres_stor, soil_to_gw,

      ! Snowcomp
      ! snowcov_area

      ! -----------------------------------------------------------------------
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                nhru => ctl_data%nhru%value, &
                nhrucell => ctl_data%nhrucell%value, &
                nlake => ctl_data%nlake%value, &
                nsegment => ctl_data%nsegment%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                model_mode => ctl_data%model_mode%values, &
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
                hru_perv => model_basin%hru_perv, &
                soil_moist => model_flow%soil_moist, &
                soil_rechr => model_flow%soil_rechr, &
                soil_rechr_max => model_flow%soil_rechr_max, &
                snowcov_area => snow%snowcov_area)

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
        allocate(this%soil_zone_max(nhru))
        allocate(this%ssres_flow(nhru))  ! moved from flowvars
        allocate(this%ssres_stor(nhru))  ! moved from flowvars
        allocate(this%swale_actet(nhru))
        allocate(this%unused_potet(nhru))

        allocate(this%soil_to_gw(nhru))
        allocate(this%soil_to_ssr(nhru))
        allocate(this%ssr_to_gw(nhru))
        allocate(this%ssres_in(nhru))
        this%soil_to_gw = 0.0
        this%soil_to_ssr = 0.0
        this%ssr_to_gw = 0.0
        this%ssres_in = 0.0

        ! TODO: 2018-06-21 Uncomment when GSFLOW stuff is figured out.
        ! if (model_mode(1)%s == 'GSFLOW') then
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

        ! Allocate arrays for local and variables from other modules
        allocate(this%grav_dunnian_flow(nhru))
        allocate(this%gvr2pfr(nhru))
        allocate(this%pfr_dunnian_flow(nhru))
        allocate(this%pref_flow_flag(nhru))
        allocate(this%soil2gw(nhru))
        allocate(this%swale_limit(nhru))

        if (print_debug == 1) then
          allocate(this%soil_moist_ante(nhru))
          allocate(this%ssres_stor_ante(nhru))
        endif

        ! if ( print_debug==7 ) call PRMS_open_module_file(DBGUNT, 'soilzone.dbg')

        ! TODO: figure out if this is needed still
        ! if (Model == 0) then
        !   if (nhru /= nhrucell) then
        !     if (getparam(MODNAME, 'gvr_hru_id', nhrucell, 'integer', gvr_hru_id)/=0 ) call read_error(2, 'gvr_hru_id')
        !     if (Parameter_check_flag==1 ) call checkdim_bounded_limits('gvr_hru_id', 'nhru', gvr_hru_id, nhrucell, 1, nhru, ierr)
        !   else
        !     do i = 1, nhru
        !       gvr_hru_id(i) = i
        !     enddo
        !   endif
        !   this%grav_gwin = 0.0 ! dimension nhru
        !   this%gw2sm_grav = 0.0
        ! endif

        ! Reset most of the basin variables used by soilzone
        call this%reset_basin_vars()

        this%hru_actet = 0.0
        this%grav_dunnian_flow = 0.0
        this%pfr_dunnian_flow = 0.0
        this%pref_flag = 0
        this%pref_flow_flag = 0
        this%pref_flow_thrsh = 0.0
        this%slow_flow = 0.0
        this%soil2gw = 0
        this%soil_lower_ratio = 0.0
        this%ssres_flow = 0.0   ! moved from flowvars
        this%ssres_stor = ssstor_init_frac * sat_threshold  ! moved from flowvars
        this%swale_limit = 0.0

        do i = 1, nhru
          this%snow_free(i) = 1.0 - snowcov_area(i)

          if (hru_type(i) == INACTIVE .or. hru_type(i) == LAKE) then
            ! If inactive or lake
            ! pref_flow_den(i) = 0.0  ! WARNING: parameters read-only
            this%pref_flow_max(i) = 0.0
            this%pref_flow_stor(i) = 0.0
            ! sat_threshold(i) = 0.0  ! WARNING: parameters read-only
            this%slow_stor(i) = 0.0
            this%soil_lower(i) = 0.0
            this%soil_lower_stor_max(i) = 0.0
            this%soil_moist_tot(i) = 0.0
            this%soil_zone_max(i) = 0.0
            this%ssres_stor(i) = 0.0

            soil_moist(i) = 0.0 ! WARNING: Overrides init in flowvars
            soil_rechr(i) = 0.0 ! WARNING: Overrides init in flowvars
            cycle
          endif

          if (hru_type(i) == SWALE) then
            ! swale
            this%swale_limit(i) = 3.0 * sat_threshold(i)
            ! pref_flow_den(i) = 0.0  ! WARNING: parameters read-only
            this%pref_flow_thrsh(i) = sat_threshold(i)
            this%pref_flow_max(i) = 0.0
          else
            ! land
            this%pref_flow_thrsh(i) = sat_threshold(i) * (1.0 - pref_flow_den(i))
            this%pref_flow_max(i) = sat_threshold(i) - this%pref_flow_thrsh(i)
          endif

          ! hru_type = LAND or SWALE
          if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 5) then
            this%slow_stor(i) = min(this%ssres_stor(i), this%pref_flow_thrsh(i))
            this%pref_flow_stor(i) = this%ssres_stor(i) - this%slow_stor(i)
          endif

          if (soil2gw_max(i) > 0.0) then
            this%soil2gw(i) = 1
          endif

          if (hru_type(i) == LAND) then
            ! Interflow coefficient values don't matter except for land HRU
            if (pref_flow_den(i) > 0.0) then
              this%pref_flow_flag(i) = 1
              this%pref_flag = 1
            endif
          endif

          hruarea = hru_area(i)
          hruperv = hru_perv(i)
          this%soil_zone_max(i) = sat_threshold(i) + soil_moist_max(i) * hru_frac_perv(i)
          this%soil_moist_tot(i) = this%ssres_stor(i) + soil_moist(i) * hru_frac_perv(i)

          this%basin_cpr_stor_frac = this%basin_cpr_stor_frac + &
                                dble(soil_moist(i) / soil_moist_max(i) * hruperv)

          if (this%pref_flow_thrsh(i) > 0.0) then
            this%basin_gvr_stor_frac = this%basin_gvr_stor_frac + &
                                  dble(this%slow_stor(i) / this%pref_flow_thrsh(i) * hruarea)
          endif

          this%soil_lower(i) = soil_moist(i) - soil_rechr(i)
          this%soil_lower_stor_max(i) = soil_moist_max(i) - soil_rechr_max(i)

          if (this%soil_lower_stor_max(i) > 0.0) then
            this%soil_lower_ratio(i) = this%soil_lower(i) / this%soil_lower_stor_max(i)
          endif

          this%basin_sz_stor_frac = this%basin_sz_stor_frac + &
                               dble(this%soil_moist_tot(i) / this%soil_zone_max(i) * hruarea)

          this%basin_soil_lower_stor_frac = this%basin_soil_lower_stor_frac + &
                                       dble(this%soil_lower_ratio(i) * hruperv)

          this%basin_soil_rechr_stor_frac = this%basin_soil_rechr_stor_frac + &
                                       dble(soil_rechr(i) / soil_rechr_max(i) * hruperv)
          this%basin_soil_moist = this%basin_soil_moist + dble(soil_moist(i) * hru_perv(i))
          this%basin_soil_moist_tot = this%basin_soil_moist_tot + dble(this%soil_moist_tot(i) * hruarea)

          ! rsr, 6/12/2014 potential problem for GSFLOW if sum of slow_stor /= gravity_stor_res
          this%basin_slstor = this%basin_slstor + dble(this%slow_stor(i) * hruarea)
          this%basin_ssstor = this%basin_ssstor + dble(this%ssres_stor(i) * hruarea)
          this%basin_soil_rechr = this%basin_soil_rechr + dble(soil_rechr(i) * hruperv)

          if (this%pref_flow_flag(i) == 1) then
            this%basin_pref_stor = this%basin_pref_stor + dble(this%pref_flow_stor(i) * hruarea)
            this%basin_pfr_stor_frac = this%basin_pfr_stor_frac + &
                                  dble(this%pref_flow_stor(i) / this%pref_flow_max(i) * hruarea)
          endif
        enddo

        this%basin_soil_rechr = this%basin_soil_rechr * basin_area_inv
        this%basin_ssstor = this%basin_ssstor * basin_area_inv
        this%basin_slstor = this%basin_slstor * basin_area_inv
        this%basin_soil_moist = this%basin_soil_moist * basin_area_inv
        this%basin_soil_moist_tot = this%basin_soil_moist_tot * basin_area_inv
        this%basin_pref_stor = this%basin_pref_stor * basin_area_inv
        this%last_soil_moist = this%basin_soil_moist
        this%last_ssstor = this%basin_ssstor
        this%basin_cpr_stor_frac = this%basin_cpr_stor_frac * basin_area_inv
        this%basin_gvr_stor_frac = this%basin_gvr_stor_frac * basin_area_inv
        this%basin_pfr_stor_frac = this%basin_pfr_stor_frac * basin_area_inv
        this%basin_sz_stor_frac = this%basin_sz_stor_frac * basin_area_inv
        this%basin_soil_lower_stor_frac = this%basin_soil_lower_stor_frac * basin_area_inv
        this%basin_soil_rechr_stor_frac = this%basin_soil_rechr_stor_frac * basin_area_inv

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
        ! if (model_mode(1)%s == 'GSFLOW') then
        !   this%gvr2sm = 0.0  ! dimension nhru
        !   this%sm2gw_grav = 0.0  ! dimension nhrucell
        ! endif
        !
        ! ! Initialize arrays (dimensioned nhrucell)
        ! if (model_mode(1)%s == 'GSFLOW') then
        ! ! if (Model == 0) then
        !   this%max_gvrs = 1
        !   this%hrucheck = 1
        !   this%hru_gvr_count = 0
        !
        !   do i=1, nhrucell
        !     ihru = gvr_hru_id(i)
        !
        !     if (hru_type(ihru) == INACTIVE .or. hru_type(ihru) == LAKE) then
        !       this%gravity_stor_res(i) = 0.0
        !       this%hrucheck(ihru) = 0
        !       this%replenish_frac(ihru) = 0.0
        !     else
        !       ! set only for cold start simulations
        !       if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 5) then
        !         this%gravity_stor_res(i) = this%ssres_stor(ihru)
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
        !     do i=1, nhru
        !       this%hru_gvr_index(1, i) = i
        !     enddo
        !   else
        !     this%hru_gvr_index = 0
        !
        !     do i=1, nhru
        !       if (hru_type(i) == INACTIVE .or. hru_type(i) == LAKE) cycle  ! if inactive or lake
        !       icnt = 0
        !
        !       do ii=1, nhrucell
        !         if (gvr_hru_id(ii) == i) then
        !           icnt = icnt + 1
        !           this%hru_gvr_index(icnt, i) = ii
        !
        !           if (icnt == this%hru_gvr_count(i)) EXIT
        !         endif
        !       enddo
        !     enddo
        !   endif
        ! endif
      end associate
    end function


    module subroutine run_Soilzone(this, ctl_data, param_data, model_basin, &
                                   model_potet, model_climate, intcp, snow, runoff, model_flow)
      ! USE PRMS_CASCADE, ONLY: Ncascade_hru
      ! USE PRMS_SET_TIME, ONLY: Nowmonth !, Nowday
      use prms_constants, only: dp, LAKE, LAND, SWALE
      implicit none

      class(Soilzone) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Potential_ET), intent(inout) :: model_potet
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      type(Srunoff), intent(inout) :: runoff
      type(Flowvars), intent(inout) :: model_flow

      ! Local Variables
      integer(i32) :: i
      integer(i32) :: k
      integer(i32) :: update_potet

      real(r32) :: avail_potet
      real(r32) :: availh2o
      real(r32) :: cap_upflow_max
      real(r32) :: capacity
      real(r32) :: capwater_maxin
      real(r32) :: dndunn
      real(r32) :: dnpreflow
      real(r32) :: dnslowflow
      real(r32) :: dunnianflw
      real(r32) :: dunnianflw_gvr
      real(r32) :: dunnianflw_pfr
      real(r32) :: gvr_maxin
      real(r32) :: harea
      real(r32) :: interflow
      real(r32) :: perv_area
      real(r32) :: perv_frac
      real(r32) :: pervactet
      real(r32) :: pref_flow_maxin
      real(r32) :: prefflow
      real(r32) :: ssresin
      real(r32) :: topfr
      real(r32) :: unsatisfied_et

      real(r64) :: gwin

      ! Control
      ! nlake, cascade_flag, dprst_flag, model_mode, print_debug,

      ! Basin
      ! active_hrus, hru_route_order, basin_area_inv, hru_perv, hru_frac_perv,
      ! numlake_hrus

      ! Parameters
      ! cov_type, fastcoef_lin, fastcoef_sq, hru_area, hru_type,
      ! lake_evap_adj(2D), pref_flow_den, sat_threshold,
      ! soil_moist_max, soil_type,
      ! soil2gw_max, slowcoef_lin, slowcoef_sq, ssr2gw_exp, ssr2gw_rate,

      ! Srunoff
      ! dprst_evap_hru, dprst_seep_hru, hru_impervevap, sroff(RW),
      ! strm_seg_in(RW), infil, basin_sroff(RW)

      ! Climateflow
      ! potet(RW), transp_on, hru_ppt, basin_potet(RW)

      ! Flowvars
      ! soil_rechr(RW), soil_rechr_max, soil_moist(RW),

      ! Interception
      ! hru_intcpevap,

      ! Snow
      ! snow_evap, snowcov_area,

      !***********************************************************************
      associate(nlake => ctl_data%nlake%value, &
                cascade_flag => ctl_data%cascade_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                model_mode => ctl_data%model_mode%values, &
                print_debug => ctl_data%print_debug%value, &
                active_hrus => model_basin%active_hrus, &
                basin_area_inv => model_basin%basin_area_inv, &
                hru_frac_perv => model_basin%hru_frac_perv, &
                hru_perv => model_basin%hru_perv, &
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

                hru_ppt => model_climate%hru_ppt, &
                transp_on => model_climate%transp_on, &
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
                ! hru_actet => model_flow%hru_actet, &
                soil_rechr => model_flow%soil_rechr, &
                soil_rechr_max => model_flow%soil_rechr_max, &
                soil_moist => model_flow%soil_moist)
                ! soil_to_gw => model_flow%soil_to_gw, &
                ! ssr_to_gw => model_flow%ssr_to_gw, &
                ! slow_flow => model_flow%slow_flow, &
                ! soil_to_ssr => model_flow%soil_to_ssr, &
                ! ssres_in => model_flow%ssres_in)

        ! TODO: 2018-06-21 - Uncomment when GSFLOW stuff is figured out.
        ! if (model_mode(1)%s == 'GSFLOW') then
        ! ! if (Model == 0) then
        !   if (Kkiter == 0) STOP 'ERROR, problem with KKITER, equals 0'
        !
        !   if (Kkiter == 1) then
        !     ! it0_* variables used with MODFLOW integration to save iteration states.
        !     do k=1, active_hrus
        !       i = hru_route_order(k)
        !       this%it0_soil_rechr(i) = soil_rechr(i)
        !       this%it0_soil_moist(i) = soil_moist(i)
        !       this%it0_ssres_stor(i) = this%ssres_stor(i)
        !       this%it0_pref_flow_stor(i) = this%pref_flow_stor(i)
        !       this%it0_slow_stor(i) = this%slow_stor(i)
        !       this%it0_sroff(i) = sroff(i)
        !       this%it0_potet(i) = potet(i)
        !     enddo
        !
        !     this%it0_basin_soil_moist = this%basin_soil_moist
        !     this%it0_basin_ssstor = this%basin_ssstor
        !     this%it0_gravity_stor_res = this%gravity_stor_res
        !     this%it0_strm_seg_in = strm_seg_in
        !     this%gw2sm_grav = 0.0
        !   else
        !     do k=1, active_hrus
        !       i = hru_route_order(k)
        !       soil_rechr(i) = this%it0_soil_rechr(i)
        !       soil_moist(i) = this%it0_soil_moist(i)
        !       this%ssres_stor(i) = this%it0_ssres_stor(i)
        !       this%pref_flow_stor(i) = this%it0_pref_flow_stor(i)
        !       this%slow_stor(i) = this%it0_slow_stor(i)
        !       sroff(i) = this%it0_sroff(i)
        !       potet(i) = this%it0_potet(i)
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
        !     i = hru_route_order(k)
        !     this%upslope_interflow(i) = 0.0_dp
        !     this%upslope_dunnianflow(i) = 0.0_dp
        !   enddo
        !
        !   if (numlake_hrus > 0) then
        !     this%lakein_sz = 0.0_dp
        !     this%basin_lakeinsz = 0.0_dp
        !   endif
        ! endif

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

        do k=1, active_hrus
          i = hru_route_order(k)
          this%hru_actet(i) = hru_impervevap(i) + hru_intcpevap(i) + snow_evap(i)

          if (dprst_flag == 1) then
            this%hru_actet(i) = this%hru_actet(i) + dprst_evap_hru(i)
          endif

          harea = Hru_area(i)

          ! TODO: 2018-06-21 - Uncomment once lakes are working.
          ! if (hru_type(i) == LAKE) then ! lake or reservoir
          !   ! WARNING: RSR, if hru_actet>water in lake, then budget error
          !   this%hru_actet(i) = (potet(i) - this%hru_actet(i)) * lake_evap_adj(Nowmonth, Lake_hru_id(i))
          !
          !   if (this%hru_actet(i) > potet(i)) then
          !     print *, 'WARNING, lake evap > potet, for HRU:', i, ' potential ET increased to adjusted lake ET'
          !     print *, this%hru_actet(i), potet(i), this%hru_actet(i) - potet(i)
          !     basin_potet = basin_potet - dble(potet(i) * harea)
          !     potet(i) = this%hru_actet(i) ! WARNING: This could be a problem when it happens
          !     basin_potet = basin_potet + dble(potet(i) * harea)
          !     update_potet = 1
          !   endif
          !
          !   this%unused_potet(i) = potet(i) - this%hru_actet(i)
          !   this%basin_actet = this%basin_actet + dble(this%hru_actet(i) * harea)
          !   Basin_lakeevap = Basin_lakeevap + dble(this%hru_actet(i) * harea)
          !   this%basin_lakeprecip = this%basin_lakeprecip + dble(hru_ppt(i) * harea)
          !
          !   if (cascade_flag == 1) then
          !     ! If lake HRU doesn't cascade, should we limit ET to
          !     ! water entering the HRU to this point (no gwflow yet)
          !     this%lakein_sz(i) = this%upslope_interflow(i) + this%upslope_dunnianflow(i)
          !     this%basin_lakeinsz = this%basin_lakeinsz + this%lakein_sz(i) * Hru_area_dble(i)
          !   endif
          !
          !   cycle
          ! endif

          perv_area = hru_perv(i)
          perv_frac = hru_frac_perv(i)

          ! soil_to_gw for whole HRU
          this%soil_to_gw(i) = 0.0
          this%ssr_to_gw(i) = 0.0
          this%slow_flow(i) = 0.0
          this%ssres_flow(i) = 0.0
          avail_potet = potet(i) - this%hru_actet(i)

          if (avail_potet < 0.0) avail_potet = 0.0
          ! Snowevap_aet_frac(i) = 0.0

          ! hru_type can be 1 (land) or 3 (swale)

          ! ******Add infiltration to soil and compute excess
          ! NOTE: perv_area has to be > 0.0
          dunnianflw = 0.0
          dunnianflw_pfr = 0.0
          dunnianflw_gvr = 0.0
          interflow = 0.0
          pref_flow_maxin = 0.0

          ! ****** Add infiltration to soil and compute excess
          ! infil_tot is the depth in whole HRU
          ! capillary reservoir for pervious area
          ! preferential flow reservoir for whole HRU
          ! gravity reservoir for whole HRU
          ! upslope flow for whole HRU

          ! ****** If cascading flow available from upslope cascades
          ! ****** add soil excess (Dunnian flow) to infiltration
          ! perv_frac has to be > 0.001
          ! infil for pervious portion of HRU
          capwater_maxin = infil(i)

          ! Compute preferential flow and storage, and any dunnian flow
          prefflow = 0.0
          if (this%pref_flow_flag(i) == 1) then
            this%pref_flow_infil(i) = 0.0

            if (capwater_maxin > 0.0) then
              ! pref_flow for whole HRU
              pref_flow_maxin = capwater_maxin * pref_flow_den(i)
              capwater_maxin = capwater_maxin - pref_flow_maxin
              pref_flow_maxin = pref_flow_maxin * perv_frac

              ! Compute contribution to preferential-flow reservoir storage
              this%pref_flow_stor(i) = this%pref_flow_stor(i) + pref_flow_maxin
              dunnianflw_pfr = max(0.0, this%pref_flow_stor(i) - this%pref_flow_max(i))

              if (dunnianflw_pfr > 0.0) then
                this%basin_dunnian_pfr = this%basin_dunnian_pfr + dunnianflw_pfr * harea
                this%pref_flow_stor(i) = this%pref_flow_max(i)
              endif

              this%pref_flow_infil(i) = pref_flow_maxin - dunnianflw_pfr
              this%basin_pref_flow_infil = this%basin_pref_flow_infil + this%pref_flow_infil(i) * harea
            endif

            this%pfr_dunnian_flow(i) = dunnianflw_pfr
          endif

          if (cascade_flag == 1) then
            ! Cap_upflow_max(i) = sngl(this%upslope_dunnianflow(i)+this%upslope_interflow(i))/perv_frac
            ! capwater_maxin = capwater_maxin + Cap_upflow_max(i)
            ! this%basin_cap_up_max = this%basin_cap_up_max + Cap_upflow_max(i)*perv_area
            cap_upflow_max = sngl(this%upslope_dunnianflow(i) + this%upslope_interflow(i)) / perv_frac
            capwater_maxin = capwater_maxin + cap_upflow_max
            this%basin_cap_up_max = this%basin_cap_up_max + cap_upflow_max * perv_area
          endif

          this%cap_infil_tot(i) = capwater_maxin * perv_frac
          this%basin_cap_infil_tot = this%basin_cap_infil_tot + dble(this%cap_infil_tot(i) * harea)

          ! ****** Add infiltration to soil and compute excess
          gvr_maxin = 0.0
          this%cap_waterin(i) = capwater_maxin

          ! Call even if capwater_maxin = 0, just in case soil_moist now > soil_moist_max
          if (capwater_maxin + soil_moist(i) > 0.0) then
            call this%compute_soilmoist(this%soil2gw(i), perv_frac, soil_moist_max(i), &
                                        soil_rechr_max(i), soil2gw_max(i), &
                                        this%cap_waterin(i), soil_moist(i), &
                                        soil_rechr(i), this%soil_to_gw(i), gvr_maxin)

            this%cap_waterin(i) = this%cap_waterin(i) * perv_frac
            this%basin_capwaterin = this%basin_capwaterin + dble(this%cap_waterin(i) * harea)
            this%basin_soil_to_gw = this%basin_soil_to_gw + dble(this%soil_to_gw(i) * harea)
            this%basin_sm2gvr_max = this%basin_sm2gvr_max + dble(gvr_maxin * harea)
          endif

          ! soil_to_ssr for whole HRU
          this%soil_to_ssr(i) = gvr_maxin

          ! Compute slow interflow and ssr_to_gw
          topfr = 0.0

          ! TODO: 2018-06-21 Uncomment when GSFLOW stuff is figured out.
          ! if (model_mode(1)%s == 'GSFLOW') then
          !   ! capacity for whole HRU
          !   capacity = (soil_moist_max(i) - soil_moist(i)) * perv_frac
          !   call this%compute_gravflow(i, capacity, slowcoef_lin(i), &
          !                         slowcoef_sq(i), ssr2gw_rate(i), ssr2gw_exp(i), &
          !                         gvr_maxin, this%pref_flow_thrsh(i), topfr, &
          !                         this%ssr_to_gw(i), this%slow_flow(i), this%slow_stor(i), &
          !                         this%gvr2sm(i), this%soil_to_gw(i), gwin, hru_type(i))
          !
          !   ! Adjust soil moisture with replenish amount
          !   if (this%gvr2sm(i) > 0.0) then
          !     soil_moist(i) = soil_moist(i) + this%gvr2sm(i) / perv_frac
          !     ! if (soil_moist(i)>soil_moist_max(i)) &
          !     !    print *, 'sm>max', soil_moist(i), soil_moist_max(i), i
          !     soil_rechr(i) = soil_rechr(i) + this%gvr2sm(i) / perv_frac * this%replenish_frac(i)
          !     soil_rechr(i) = min(soil_rechr_max(i), soil_rechr(i))
          !     this%basin_gvr2sm = this%basin_gvr2sm + dble(this%gvr2sm(i) * harea)
          !     ! elseif ( this%gvr2sm(i)<-NEARZERO ) then
          !     !  print *, 'negative gvr2sm, HRU:', i, this%gvr2sm(i)
          !     this%gvr2sm(i) = 0.0
          !   endif
          !
          !   this%grav_gwin(i) = sngl(gwin)
          !   this%basin_sz_gwin = this%basin_sz_gwin + gwin * dble(harea)
          ! else
          if (model_mode(1)%s /= 'GSFLOW') then
            availh2o = this%slow_stor(i) + gvr_maxin

            if (hru_type(i) == LAND) then
              topfr = max(0.0, availh2o - this%pref_flow_thrsh(i))
              ssresin = gvr_maxin - topfr
              this%slow_stor(i) = availh2o - topfr

              ! compute slow contribution to interflow, if any
              if (this%slow_stor(i) > 0.0) then
                call this%compute_interflow(slowcoef_lin(i), slowcoef_sq(i), &
                                       ssresin, this%slow_stor(i), this%slow_flow(i))
              endif
            elseif (hru_type(i) == SWALE) then
              this%slow_stor(i) = availh2o
            endif

            if (this%slow_stor(i) > 0.0 .and. ssr2gw_rate(i) > 0.0) then
              call this%compute_gwflow(ssr2gw_rate(i), ssr2gw_exp(i), this%ssr_to_gw(i), this%slow_stor(i))
            endif
          endif

          ! Compute contribution to Dunnian flow from PFR, if any
          if (this%pref_flow_flag(i) == 1) then
            availh2o = this%pref_flow_stor(i) + topfr
            dunnianflw_gvr = max(0.0, availh2o - this%pref_flow_max(i))

            if (dunnianflw_gvr > 0.0) then
              topfr = topfr - dunnianflw_gvr

              if (topfr < 0.0) then
                ! if ( topfr<-NEARZERO .AND. print_debug>-1 ) print *, 'gvr2pfr<0', topfr, dunnianflw_gvr, &
                !   this%pref_flow_max(i), this%pref_flow_stor(i), gvr_maxin
                topfr = 0.0
              endif
            endif

            this%pref_flow_in(i) = this%pref_flow_infil(i) + topfr
            this%pref_flow_stor(i) = this%pref_flow_stor(i) + topfr

            if (this%pref_flow_stor(i) > 0.0) then
              call this%compute_interflow(fastcoef_lin(i), fastcoef_sq(i), &
                                          this%pref_flow_in(i), this%pref_flow_stor(i), &
                                          prefflow)
            endif

            this%basin_pref_stor = this%basin_pref_stor + dble(this%pref_flow_stor(i) * harea)
            ! Pfr_stor_frac(i) = this%pref_flow_stor(i)/this%pref_flow_max(i)
            ! this%basin_pfr_stor_frac = this%basin_pfr_stor_frac + Pfr_stor_frac(i)*harea
            this%basin_pfr_stor_frac = this%basin_pfr_stor_frac + this%pref_flow_stor(i) / this%pref_flow_max(i) * harea
          elseif (hru_type(i) == 1) then
            dunnianflw_gvr = topfr  !?? is this right
          endif
          this%gvr2pfr(i) = topfr

          this%basin_sm2gvr = this%basin_sm2gvr + dble(this%soil_to_ssr(i) * harea)
          this%basin_dunnian_gvr = this%basin_dunnian_gvr + dble(dunnianflw_gvr * harea)
          this%basin_sz2gw = this%basin_sz2gw + dble(this%ssr_to_gw(i) * harea)

          !******Compute actual evapotranspiration
          this%snow_free(i) = 1.0 - snowcov_area(i)
          this%potet_rechr(i) = 0.0
          this%potet_lower(i) = 0.0
          pervactet = 0.0

          if (soil_moist(i) > 0.0) then
            call this%compute_szactet(soil_moist_max(i), soil_rechr_max(i), transp_on(i), cov_type(i), &
                                 soil_type(i), soil_moist(i), soil_rechr(i), pervactet, &
                                 avail_potet, this%snow_free(i), this%potet_rechr(i), this%potet_lower(i))
          endif

          this%hru_actet(i) = this%hru_actet(i) + pervactet * perv_frac
          avail_potet = potet(i) - this%hru_actet(i)

          this%perv_actet(i) = pervactet

          ! soil_moist & soil_rechr multiplied by perv_area instead of harea
          this%soil_lower(i) = soil_moist(i) - soil_rechr(i)
          this%basin_soil_moist = this%basin_soil_moist + dble(soil_moist(i) * perv_area)
          this%basin_soil_rechr = this%basin_soil_rechr + dble(soil_rechr(i) * perv_area)
          this%basin_perv_et = this%basin_perv_et + dble(this%perv_actet(i) * perv_area)

          ! If HRU cascades,
          ! compute interflow and excess flow to each HRU or stream
          if (hru_type(i) == LAND) then
            interflow = this%slow_flow(i) + prefflow
            ! Interflow_max(i) = interflow
            this%basin_interflow_max = this%basin_interflow_max + interflow * harea
            dunnianflw = dunnianflw_gvr + dunnianflw_pfr
            this%dunnian_flow(i) = dunnianflw

            ! TODO: 2018-06-21 - Uncomment once cascade module is converted.
            ! if (cascade_flag == 1) then
            !   if (Ncascade_hru(i) > 0) then
            !     dnslowflow = 0.0
            !     dnpreflow = 0.0
            !     dndunn = 0.0
            !
            !     if (interflow + dunnianflw > 0.0) then
            !       call this%compute_cascades(i, Ncascade_hru(i), this%slow_flow(i), &
            !                             prefflow, this%dunnian_flow(i), dnslowflow, &
            !                             dnpreflow, dndunn)
            !       this%basin_dninterflow = this%basin_dninterflow + dble((dnslowflow + dnpreflow) * harea)
            !       this%basin_dndunnianflow = this%basin_dndunnianflow + dble(dndunn * harea)
            !     endif
            !
            !     this%hru_sz_cascadeflow(i) = dnslowflow + dnpreflow + dndunn
            !          ! Cascade_interflow(i) = dnslowflow + dnpreflow
            !          ! Cascade_dunnianflow(i) = dndunn
            !     this%basin_dncascadeflow = this%basin_dncascadeflow + dble(this%hru_sz_cascadeflow(i) * harea)
            !   endif
            ! endif

            ! Treat pref_flow as interflow
            this%ssres_flow(i) = this%slow_flow(i)

            if (this%pref_flow_flag(i) == 1) then
              this%pref_flow(i) = prefflow
              this%ssres_flow(i) = this%ssres_flow(i) + prefflow
              this%basin_prefflow = this%basin_prefflow + dble(prefflow * harea)
              this%basin_gvr2pfr = this%basin_gvr2pfr + dble(this%gvr2pfr(i) * harea)
            endif

            this%basin_ssflow = this%basin_ssflow + dble(this%ssres_flow(i) * harea)
            this%basin_slowflow = this%basin_slowflow + dble(this%slow_flow(i) * harea)

            ! Treat dunnianflw as surface runoff to streams
            sroff(i) = sroff(i) + this%dunnian_flow(i)
            basin_sroff = basin_sroff + dble(sroff(i) * harea)
            this%basin_dunnian = this%basin_dunnian + dble(this%dunnian_flow(i) * harea)
            this%ssres_stor(i) = this%slow_stor(i) + this%pref_flow_stor(i)
          else
            ! for swales
            availh2o = this%slow_stor(i) - Sat_threshold(i)
            this%swale_actet(i) = 0.0

            if (availh2o > 0.0) then
              ! Ff ponding, as storage > sat_threshold
              unsatisfied_et = potet(i) - this%hru_actet(i)

              if (unsatisfied_et > 0.0) then
                availh2o = min (availh2o, unsatisfied_et)
                this%swale_actet(i) = availh2o
                this%hru_actet(i) = this%hru_actet(i) + this%swale_actet(i)
                this%slow_stor(i) = this%slow_stor(i) - this%swale_actet(i)
                this%basin_swale_et = this%basin_swale_et + dble(this%swale_actet(i) * harea)
              endif

              ! TODO: Uncomment once debug output of soilzone is figured out.
              ! if (print_debug == 7) then
              !   if (this%slow_stor(i) > this%swale_limit(i)) then
              !     write(DBGUNT, *) 'Swale ponding, HRU:', i, &
              !             ' gravity reservoir is 3*sat_threshold', this%slow_stor(i), Sat_threshold(i)
              !     call print_date(DBGUNT)
              !   endif
              ! endif
            endif
            this%ssres_stor(i) = this%slow_stor(i)
          endif

          if (this%soil_lower_stor_max(i) > 0.0) this%soil_lower_ratio(i) = this%soil_lower(i) / this%soil_lower_stor_max(i)

          ! Soil_rechr_ratio(i) = soil_rechr(i)/soil_rechr_max(i)
          this%ssres_in(i) = this%soil_to_ssr(i) + this%pref_flow_infil(i) + sngl(gwin)
          this%basin_ssin = this%basin_ssin + dble(this%ssres_in(i) * harea)
          this%basin_ssstor = this%basin_ssstor + dble(this%ssres_stor(i) * harea)
          this%basin_slstor = this%basin_slstor + dble(this%slow_stor(i) * harea)
          this%soil_moist_tot(i) = this%ssres_stor(i) + soil_moist(i) * perv_frac
          this%basin_soil_moist_tot = this%basin_soil_moist_tot + dble(this%soil_moist_tot(i) * harea)
          this%basin_cpr_stor_frac = this%basin_cpr_stor_frac + &
                                     soil_moist(i) / soil_moist_max(i) * perv_area

          if (this%pref_flow_thrsh(i) > 0.0) then
            this%basin_gvr_stor_frac = this%basin_gvr_stor_frac + this%slow_stor(i) / this%pref_flow_thrsh(i) * harea
          endif

          this%basin_sz_stor_frac = this%basin_sz_stor_frac + this%soil_moist_tot(i) / this%soil_zone_max(i) * harea
          this%basin_soil_lower_stor_frac = this%basin_soil_lower_stor_frac + this%soil_lower_ratio(i) * perv_area
          ! this%basin_soil_rechr_stor_frac = this%basin_soil_rechr_stor_frac + Soil_rechr_ratio(i) * perv_area
          this%basin_soil_rechr_stor_frac = this%basin_soil_rechr_stor_frac + soil_rechr(i) / soil_rechr_max(i) * perv_area
          this%recharge(i) = this%soil_to_gw(i) + this%ssr_to_gw(i)

          if (dprst_flag == 1) then
            this%recharge(i) = this%recharge(i) + sngl(dprst_seep_hru(i))
          endif

          this%basin_recharge = this%basin_recharge + dble(this%recharge(i) * harea)
          this%grav_dunnian_flow(i) = dunnianflw_gvr
          this%unused_potet(i) = potet(i) - this%hru_actet(i)
          this%basin_actet = this%basin_actet + dble(this%hru_actet(i) * harea)
          ! if ( hru_actet(i)>0.0 ) Snowevap_aet_frac(i) = snow_evap(i)/hru_actet(i)
        enddo

        this%basin_actet = this%basin_actet * basin_area_inv
        this%basin_perv_et = this%basin_perv_et * basin_area_inv
        this%basin_swale_et = this%basin_swale_et * basin_area_inv
        this%basin_soil_rechr = this%basin_soil_rechr * basin_area_inv
        this%basin_soil_to_gw = this%basin_soil_to_gw * basin_area_inv
        this%basin_soil_moist = this%basin_soil_moist * basin_area_inv
        this%basin_soil_moist_tot = this%basin_soil_moist_tot * basin_area_inv

        if (update_potet == 1) then
          basin_potet = basin_potet * basin_area_inv
        endif

        ! TODO: Uncomment once lakes are working.
        ! if (nlake > 0) then
        !   Basin_lakeevap = Basin_lakeevap * basin_area_inv
        !   this%basin_lakeprecip = this%basin_lakeprecip * basin_area_inv
        !   this%basin_lakeinsz = this%basin_lakeinsz * basin_area_inv
        !   Basin_lake_stor = Basin_lake_stor + this%basin_lakeprecip - Basin_lakeevap
        ! endif

        if (this%pref_flag == 1) then
          this%basin_pref_stor = this%basin_pref_stor * basin_area_inv
          this%basin_pref_flow_infil = this%basin_pref_flow_infil * basin_area_inv
          this%basin_prefflow = this%basin_prefflow * basin_area_inv
          this%basin_dunnian_pfr = this%basin_dunnian_pfr * basin_area_inv
          this%basin_pfr_stor_frac = this%basin_pfr_stor_frac * basin_area_inv
        endif

        this%basin_dunnian_gvr = this%basin_dunnian_gvr * basin_area_inv
        this%basin_ssstor = this%basin_ssstor * basin_area_inv
        this%basin_ssflow = this%basin_ssflow * basin_area_inv
        this%basin_interflow_max = this%basin_interflow_max * basin_area_inv
        this%basin_sz2gw = this%basin_sz2gw * basin_area_inv
        this%basin_ssin = this%basin_ssin * basin_area_inv
        this%basin_slstor = this%basin_slstor * basin_area_inv
        basin_sroff = basin_sroff * basin_area_inv
        this%basin_dunnian = this%basin_dunnian * basin_area_inv
        this%basin_sm2gvr = this%basin_sm2gvr * basin_area_inv
        this%basin_sm2gvr_max = this%basin_sm2gvr_max * basin_area_inv
        this%basin_capwaterin = this%basin_capwaterin * basin_area_inv
        this%basin_cap_infil_tot = this%basin_cap_infil_tot * basin_area_inv
        this%basin_cap_up_max = this%basin_cap_up_max * basin_area_inv
        this%basin_dninterflow = this%basin_dninterflow * basin_area_inv
        this%basin_dndunnianflow = this%basin_dndunnianflow * basin_area_inv
        this%basin_dncascadeflow = this%basin_dncascadeflow * basin_area_inv
        this%basin_gvr2pfr = this%basin_gvr2pfr * basin_area_inv
        this%basin_slowflow = this%basin_slowflow * basin_area_inv
        this%basin_recharge = this%basin_recharge * basin_area_inv
        this%basin_gvr2sm = this%basin_gvr2sm * basin_area_inv
        this%basin_sz_gwin = this%basin_sz_gwin * basin_area_inv
        this%basin_cpr_stor_frac = this%basin_cpr_stor_frac * basin_area_inv
        this%basin_gvr_stor_frac = this%basin_gvr_stor_frac * basin_area_inv
        this%basin_sz_stor_frac = this%basin_sz_stor_frac * basin_area_inv
        this%basin_soil_lower_stor_frac = this%basin_soil_lower_stor_frac * basin_area_inv
        this%basin_soil_rechr_stor_frac = this%basin_soil_rechr_stor_frac * basin_area_inv
      end associate
    end subroutine

    module subroutine cleanup_Soilzone(this)
      class(Soilzone) :: this
        !! Soilzone class
    end subroutine


    !***********************************************************************
    !     adjust soil moist based on being below field capacity (capacity)
    !     and preferential-flow threshold (Pref_flow_thrsh)
    !***********************************************************************
    module subroutine check_gvr_sm(capacity, depth, frac, gvr2sm, input)
      ! USE PRMS_BASIN, ONLY: CLOSEZERO
      implicit none

      ! Arguments
      real(r32), intent(inout) :: capacity
      real(r32), intent(inout) :: depth
      real(r64), intent(in) :: frac
      real(r32), intent(inout) :: gvr2sm
      real(r32), intent(inout) :: input

      ! Local Variables
      real(r32) :: to_sm
      real(r32) :: frac_sngl

      !***********************************************************************
      ! check to see if soil is below capacity, if so add up to field capacity
      ! capacity is for whole HRU
      ! to_sm and gvr2sm are for whole HRU

      frac_sngl = sngl(frac)

      ! fill up capillary with part of gravity water
      to_sm = capacity

      ! take all gravity water and put in capillary
      if (to_sm > depth) to_sm = depth

      ! compute adjusmtent to soil moist to get to field capacity
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
      !  ihru => i
      !  ncascade_hru => Ncascade_hru(i)
      ! *slowflow => this%slow_flow(i)
      ! *preflow => prefflow
      ! *dunnian => this%dunnian_flow(i)
      ! *dnslowflow => dnslowflow
      ! *dnpreflow => dnpreflow
      ! *dndunnflow => dndunn

      ! Local Variables
      integer(i32) :: j
      integer(i32) :: k
      real(r32) :: frac
      real(r32) :: fracwt

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
      !  ihru => i
      ! *capacity => capacity
      !  slowcoef_lin => slowcoef_lin(i)
      !  slowcoef_sq => slowcoef_sq(i)
      !  ssr2gw_rate => ssr2gw_rate(i)
      !  ssr2gw_exp => ssr2gw_exp(i)
      !  gvr_maxin => gvr_maxin
      !  pref_flow_thrsh => this%pref_flow_thrsh(i)
      ! ~gvr2pfr => topfr
      ! ~ssr_to_gw => ssr_to_gw(i)
      ! ~slow_flow => slow_flow(i)
      ! ~slow_stor => this%slow_stor(i)
      ! ~gvr2sm => this%gvr2sm(i)
      !  soil_to_gw => soil_to_gw(i)
      ! ~gwin => gwin
      !  hru_type => hru_type(i)


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
      real(r32), intent(out) :: slow_flow
      real(r32), intent(out) :: slow_stor
      real(r32), intent(out) :: gvr2sm
      real(r32), intent(in) :: soil_to_gw
      real(r64), intent(out) :: gwin
      integer(i32), intent(in) :: hru_type

      ! Local Variables
      integer(i32) :: igvr
      integer(i32) :: j

      real(r32) :: depth
      real(r32) :: extra_water
      real(r32) :: gvrin_actual
      real(r32) :: input
      real(r32) :: perc
      real(r32) :: slowflow

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
            extra_water = max(0.0, depth - pref_flow_thrsh)

            if (extra_water > 0.0) then
              ! Compute contribution to preferential-flow reservoir storage
              topfr = topfr + dble(extra_water) * frac
              depth = pref_flow_thrsh
            endif
            gvrin_actual = max(0.0, input - extra_water)

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
              perc = ssr2gw_rate * (depth**ssr2gw_exp)

              if (perc < 0.0) then
                perc = 0.0
              elseif (perc > depth) then
                perc = depth
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
          slowstor = slowstor + dble(depth) * frac

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
      implicit none

      ! Arguments
      real(r32), intent(in) :: ssr2gw_rate
      real(r32), intent(in) :: ssr2gw_exp
      real(r32), intent(inout) :: ssr_to_gw
      real(r32), intent(inout) :: slow_stor

      ! ********************************************************************
      ! ****** Compute flow to groundwater
      ssr_to_gw = ssr2gw_rate * (slow_stor**ssr2gw_exp)

      if (ssr_to_gw < 0.0) then
        ssr_to_gw = 0.0
      elseif (ssr_to_gw > slow_stor) then
        ssr_to_gw = slow_stor
      endif

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
      real(r32), intent(in) :: ssres_in
      real(r32), intent(inout) :: storage
      real(r32), intent(inout) :: inter_flow

      ! Local Variables
      real(r32) :: c1
      real(r32) :: c2
      real(r32) :: c3
      real(r32) :: sos

      !***********************************************************************
      ! inter_flow is in inches for the timestep
      ! ****** compute interflow
      if (coef_lin <= 0.0 .and. ssres_in <= 0.0) then
        c1 = coef_sq * storage
        inter_flow = storage * (c1 / (1.0 + c1))
      elseif (coef_sq <= 0.0) then
        c2 = 1.0 - exp(-coef_lin)
        inter_flow = ssres_in * (1.0 - c2 / coef_lin) + storage * c2
      else
        c3 = sqrt(coef_lin**2.0 + 4.0 * coef_sq * ssres_in)
        sos = storage - ((c3 - coef_lin) / (2.0 * coef_sq))

        if (c3 == 0.0) STOP 'ERROR, in compute_interflow sos=0, please contact code developers'

        c1 = coef_sq * sos / c3
        c2 = 1.0 - exp(-c3)
        if (1.0 + c1 * c2 > 0.0) then
          inter_flow = ssres_in + (sos * (1.0 + c1) * c2) / (1.0 + c1 * c2)
        else
          inter_flow = ssres_in
        endif
      endif

      if (inter_flow > storage) inter_flow = storage
      storage = storage - inter_flow
    end subroutine


    !***********************************************************************
    ! Add infiltration to soil and compute excess
    ! soil_to_gw and soil_to_ssr for whole HRU
    !***********************************************************************
    module subroutine compute_soilmoist(soil2gw, perv_frac, soil_moist_max, &
                                 soil_rechr_max, soil2gw_max, infil, &
                                 soil_moist, soil_rechr, soil_to_gw, soil_to_ssr)
      implicit none

      ! Arguments
      integer(i32), intent(in) :: soil2gw
      real(r32), intent(in) :: perv_frac
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(in) :: soil2gw_max
      real(r32), intent(inout) :: infil
      real(r32), intent(inout) :: soil_moist
      real(r32), intent(inout) :: soil_rechr
      real(r32), intent(inout) :: soil_to_gw
      real(r32), intent(inout) :: soil_to_ssr

      !  Soil2gw => this%soil2gw(i)
      !  perv_frac => perv_frac               local
      !  soil_moist_max => soil_moist_max(i)  P
      !  soil_rechr_max => soil_rechr_max(i)  P
      !  Soil2gw_max => soil2gw_max(i)        P
      ! *infil => this%cap_waterin(i)
      ! *soil_moist => soil_moist(i)          FV
      ! *soil_rechr => soil_rechr(i)          FV
      ! *soil_to_gw => soil_to_gw(i)          FV
      ! *Soil_to_ssr => gvr_maxin             local

      ! Local Variables
      real(r32) :: excs

      !***********************************************************************
      soil_rechr = min((soil_rechr+infil), soil_rechr_max)

      ! soil_moist_max from previous time step or soil_moist_max has
      ! changed for a restart simulation
      excs = soil_moist + infil
      soil_moist = min(excs, soil_moist_max)
      excs = (excs - soil_moist_max) * perv_frac

      if (excs > 0.0) then
        if (soil2gw == 1) then
          soil_to_gw = min(soil2gw_max, excs)
          excs = excs - soil_to_gw
        endif

        if (excs > infil * perv_frac) then !probably dynamic
          infil = 0.0
        else
          infil = infil - excs / perv_frac  !???? what if infil<0 ??? might happen with dynamic and small values, maybe ABS < NEARZERO = 0.0
        endif

        soil_to_ssr = excs
        if (soil_to_ssr < 0.0) soil_to_ssr = 0.0
      endif
    end subroutine


    !***********************************************************************
    ! Compute actual evapotranspiration
    !***********************************************************************
    module subroutine compute_szactet(this, soil_moist_max, soil_rechr_max, &
                                      transp_on, cov_type, soil_type, &
                                      soil_moist, soil_rechr, perv_actet, avail_potet, &
                                      snow_free, potet_rechr, potet_lower)
      use prms_constants, only: NEARZERO
      ! USE PRMS_SOILZONE, ONLY: this%et_type
      implicit none

      ! TODO: 2018-06-21 - Reorder arguments, use class variables if possible.

      ! Arguments
      class(Soilzone), intent(inout) :: this
        !! Soilzone class
      integer(i32), intent(in) :: transp_on
      integer(i32), intent(in) :: cov_type
      integer(i32), intent(in) :: soil_type
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(in) :: snow_free
      real(r32), intent(inout) :: soil_moist
      real(r32), intent(inout) :: soil_rechr
      real(r32), intent(inout) :: avail_potet
      real(r32), intent(inout) :: potet_rechr
      real(r32), intent(inout) :: potet_lower
      real(r32), intent(out) :: perv_actet

      !  soil_moist_max => soil_moist_max(i)
      !  soil_rechr_max => soil_rechr_max(i)
      !  transp_on => transp_on(i)
      !  cov_type => cov_type(i)
      !  soil_type => soil_type(i)
      ! *soil_moist => soil_moist(i)
      ! *soil_rechr => soil_rechr(i)
      ! ~perv_actet => pervactet
      ! *avail_potet => avail_potet
      !  snow_free => this%snow_free(i)
      ! *potet_rechr => this%potet_rechr(i)
      ! *potet_lower => this%potet_lower(i)

      ! Local Variables
      real(r32), parameter :: ONETHIRD = 1.0 / 3.0
      real(r32), parameter :: TWOTHIRDS = 2.0 / 3.0
      real(r32) :: et
      real(r32) :: pctr
      real(r32) :: pcts

      ! ***********************************************************************
      ! ****** Determine if evaporation(this%et_type = 2) or transpiration plus
      ! ****** evaporation(this%et_type = 3) are active. If not, this%et_type = 1
      if (avail_potet < NEARZERO) then
        this%et_type = 1
        avail_potet = 0.0
      elseif (transp_on == 0) then
        if (snow_free < 0.01) then
          this%et_type = 1
        else
          this%et_type = 2
        endif
      elseif (cov_type > 0) then
        this%et_type = 3
      elseif (snow_free < 0.01) then
        this%et_type = 1
      else
        this%et_type = 2
      endif

      if (this%et_type > 1) then
        pcts = soil_moist / soil_moist_max
        pctr = soil_rechr / soil_rechr_max
        potet_lower = avail_potet
        potet_rechr = avail_potet

        if (soil_type == 1) then
          ! ****** sandy soil
          if (pcts < 0.25) potet_lower = 0.5 * pcts * avail_potet
          if (pctr < 0.25) potet_rechr = 0.5 * pctr * avail_potet
        elseif (soil_type == 2) then
          ! ****** loam soil
          if (pcts < 0.5) potet_lower = pcts * avail_potet
          if (pctr < 0.5) potet_rechr = pctr * avail_potet
        elseif (soil_type == 3) then
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
        endif

        ! ****** Soil moisture accounting
        if (this%et_type == 2) potet_rechr = potet_rechr * snow_free

        if (potet_rechr > soil_rechr) then
          potet_rechr = soil_rechr
          soil_rechr = 0.0
        else
          soil_rechr = soil_rechr - potet_rechr
        endif

        if (this%et_type == 2 .or. potet_rechr >= potet_lower) then
          if (potet_rechr > soil_moist) then
            potet_rechr = soil_moist
            soil_moist = 0.0
          else
            soil_moist = soil_moist - potet_rechr
          endif

          et = potet_rechr
        elseif (potet_lower > soil_moist) then
          et = soil_moist
          soil_moist = 0.0
        else
          soil_moist = soil_moist - potet_lower
          et = potet_lower
        endif

        if (soil_rechr > soil_moist) soil_rechr = soil_moist
      else
        et = 0.0
      endif

      perv_actet = et
    end subroutine


    module subroutine reset_basin_vars(this)
      use prms_constants, only: dp
      implicit none

      class(Soilzone), intent(inout) :: this
        !! Soilzone class

      ! ------------------------------------------------------------------------
      this%basin_actet = 0.0_dp
      this%basin_cap_infil_tot = 0.0_dp
      this%basin_cap_up_max = 0.0_dp
      this%basin_capwaterin = 0.0_dp
      this%basin_cpr_stor_frac = 0.0_dp
      this%basin_dncascadeflow = 0.0_dp
      this%basin_dndunnianflow = 0.0_dp
      this%basin_dninterflow = 0.0_dp
      this%basin_dunnian = 0.0_dp
      this%basin_dunnian_gvr = 0.0_dp
      this%basin_dunnian_pfr = 0.0_dp
      this%basin_gvr2pfr = 0.0_dp
      this%basin_gvr2sm = 0.0_dp
      this%basin_gvr_stor_frac = 0.0_dp
      this%basin_interflow_max = 0.0_dp
      this%basin_lakeevap = 0.0_dp
      this%basin_lakeinsz = 0.0_dp
      this%basin_lakeprecip = 0.0_dp
      this%basin_perv_et = 0.0_dp
      this%basin_pfr_stor_frac = 0.0_dp
      this%basin_pref_flow_infil = 0.0_dp
      this%basin_pref_stor = 0.0_dp
      this%basin_prefflow = 0.0_dp
      this%basin_recharge = 0.0_dp
      this%basin_slowflow = 0.0_dp
      this%basin_slstor = 0.0_dp
      this%basin_sm2gvr = 0.0_dp
      this%basin_sm2gvr_max = 0.0_dp
      this%basin_soil_lower_stor_frac = 0.0_dp
      this%basin_soil_moist = 0.0_dp
      this%basin_soil_moist_tot = 0.0_dp
      this%basin_soil_rechr = 0.0_dp
      this%basin_soil_rechr_stor_frac = 0.0_dp
      this%basin_soil_to_gw = 0.0_dp
      ! this%basin_sroff = 0.0_dp
      this%basin_ssflow = 0.0_dp
      this%basin_ssin = 0.0_dp
      this%basin_ssstor = 0.0_dp
      this%basin_swale_et = 0.0_dp
      this%basin_sz2gw = 0.0_dp
      this%basin_sz_gwin = 0.0_dp
      this%basin_sz_stor_frac = 0.0_dp
    end subroutine

end submodule
