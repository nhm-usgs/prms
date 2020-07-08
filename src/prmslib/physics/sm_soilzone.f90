submodule (PRMS_SOILZONE) sm_soilzone
  contains
    module subroutine init_Soilzone(this, ctl_data, model_basin, model_climate, snow, model_runoff, model_summary)
      use prms_constants, only: dp, INACTIVE, LAND, LAKE, SWALE
      implicit none

      class(Soilzone), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: model_climate
      type(Snowcomp), intent(in) :: snow
      type(Srunoff), intent(in) :: model_runoff
      type(Summary), intent(inout) :: model_summary

      ! Local variables
      ! integer(i32) :: chru
      integer(i32) :: jj

      ! GSFLOW-related variables
      ! TODO: Uncommented next 4 when GSFLOW figured out
      ! integer(i32) :: ii
      ! integer(i32) :: ihru
      ! integer(i32) :: icnt
      ! integer(i32) :: ierr

      ! -----------------------------------------------------------------------
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                gsflow_mode => ctl_data%gsflow_mode, &
                ! model_mode => ctl_data%model_mode%values, &
                outVarON_OFF => ctl_data%outVarON_OFF%value, &
                outVar_names => ctl_data%outVar_names, &
                param_hdl => ctl_data%param_file_hdl, &
                print_debug => ctl_data%print_debug%value, &
                save_vars_to_file => ctl_data%save_vars_to_file%value, &

                nhru => model_basin%nhru, &
                nlake => model_basin%nlake, &
                nsegment => model_basin%nsegment, &
                hru_type => model_basin%hru_type, &

                soil_moist => model_climate%soil_moist, &
                soil_moist_max => model_climate%soil_moist_max, &
                soil_rechr => model_climate%soil_rechr, &
                soil_rechr_max => model_climate%soil_rechr_max, &

                snowcov_area => snow%snowcov_area, &

                hru_area_perv => model_runoff%hru_area_perv, &
                hru_frac_perv => model_runoff%hru_frac_perv)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! Dimensions
        this%nhrucell = param_hdl%get_dimension('nhrucell')

        ! Parameters
        if (this%nhrucell > 0) then
          allocate(this%gvr_hru_id(this%nhrucell))
          call param_hdl%get_variable('gvr_hru_id', this%gvr_hru_id)
        end if

        allocate(this%fastcoef_lin(nhru))
        call param_hdl%get_variable('fastcoef_lin', this%fastcoef_lin)

        allocate(this%fastcoef_sq(nhru))
        call param_hdl%get_variable('fastcoef_sq', this%fastcoef_sq)

        ! NOTE: PAN - lake_evap_adj is really part of muskingum_lake (according to the manual)
        !             It is accessed below as a 2D array but the manual shows it
        !             As a 1D array. The only module that uses this parameter
        !             is soilzone.
        ! allocate(this%lake_evap_adj(nhru))
        ! call param_hdl%get_variable('lake_evap_adj', this%lake_evap_adj)

        allocate(this%pref_flow_den(nhru))
        call param_hdl%get_variable('pref_flow_den', this%pref_flow_den)

        allocate(this%sat_threshold(nhru))
        call param_hdl%get_variable('sat_threshold', this%sat_threshold)

        where (hru_type == INACTIVE .or. hru_type == LAKE)
          this%sat_threshold = 0.0  ! WARNING: parameters should be read-only
        end where

        allocate(this%slowcoef_lin(nhru))
        call param_hdl%get_variable('slowcoef_lin', this%slowcoef_lin)

        allocate(this%slowcoef_sq(nhru))
        call param_hdl%get_variable('slowcoef_sq', this%slowcoef_sq)

        allocate(this%soil_type(nhru))
        call param_hdl%get_variable('soil_type', this%soil_type)

        allocate(this%soil2gw_max(nhru))
        call param_hdl%get_variable('soil2gw_max', this%soil2gw_max)

        allocate(this%ssr2gw_exp(nhru))
        call param_hdl%get_variable('ssr2gw_exp', this%ssr2gw_exp)

        allocate(this%ssr2gw_rate(nhru))
        call param_hdl%get_variable('ssr2gw_rate', this%ssr2gw_rate)

        ! NOTE: ssstor_init_frac should have dimensions of nssr
        allocate(this%ssstor_init_frac(nhru))
        call param_hdl%get_variable('ssstor_init_frac', this%ssstor_init_frac)


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
        allocate(this%hru_actet(nhru))
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
        allocate(this%slow_flow(nhru))
        allocate(this%slow_stor(nhru))
        allocate(this%snow_free(nhru))
        allocate(this%soil_lower(nhru))
        allocate(this%soil_lower_ratio(nhru))
        allocate(this%soil_lower_stor_max(nhru))
        allocate(this%soil_moist_tot(nhru))
        allocate(this%soil_to_gw(nhru))
        allocate(this%soil_to_ssr(nhru))
        allocate(this%soil_zone_max(nhru))
        allocate(this%ssr_to_gw(nhru))
        allocate(this%ssres_flow(nhru))
        allocate(this%ssres_in(nhru))
        allocate(this%ssres_stor(nhru))
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


        ! Initialize
        this%grav_dunnian_flow = 0.0_dp
        this%hru_actet = 0.0
        this%pfr_dunnian_flow = 0.0_dp
        this%pref_flow_flag = .false.
        this%pref_flow_max = 0.0
        this%pref_flow_stor = 0.0
        this%pref_flow_thrsh = 0.0
        this%slow_flow = 0.0
        this%slow_stor = 0.0
        this%snow_free = 1.0 - snowcov_area
        this%soil2gw_flag = .false.
        this%soil_lower = 0.0
        this%soil_lower_ratio = 0.0
        this%soil_lower_stor_max = 0.0
        this%soil_moist_tot = 0.0
        this%soil_zone_max = 0.0
        this%ssres_flow = 0.0
        this%swale_limit = 0.0

        if (any([0, 2, 5] == init_vars_from_file)) then
          where (hru_type == INACTIVE .or. hru_type == LAKE)
            this%ssres_stor = 0.0
          else where
            this%ssres_stor = this%ssstor_init_frac * this%sat_threshold
          end where

          ! ssstor_init_frac no longer needed at this point
          deallocate(this%ssstor_init_frac)
        else
          ! ~~~~~~~~~~~~~~~~~~~~~~~~
          ! Initialize from restart
          call ctl_data%read_restart_variable('ssres_stor', this%ssres_stor)
        end if

        where (hru_type /= LAND)
            this%pref_flow_den = 0.0  ! WARNING: parameters should be read-only
        end where

        where (hru_type == SWALE)
          this%swale_limit = 3.0 * this%sat_threshold
          this%pref_flow_thrsh = this%sat_threshold
        end where

        where (hru_type == LAND)
          this%pref_flow_thrsh = this%sat_threshold * (1.0 - this%pref_flow_den)
          this%pref_flow_max = this%sat_threshold - this%pref_flow_thrsh

          where (this%pref_flow_den > 0.0)
            this%pref_flow_flag = .true.
          end where
        end where

        if (any([0, 2, 5] == init_vars_from_file)) then
          where (hru_type == LAND .or. hru_type == SWALE)
            this%slow_stor = min(this%ssres_stor, this%pref_flow_thrsh)
            this%pref_flow_stor = this%ssres_stor - this%slow_stor
          end where
        else
          ! ~~~~~~~~~~~~~~~~~~~~~~~~
          ! Initialize from restart
          call ctl_data%read_restart_variable('pref_flow_stor', this%pref_flow_stor)
          call ctl_data%read_restart_variable('slow_stor', this%slow_stor)
        endif

        where (this%soil2gw_max > 0.0)
          this%soil2gw_flag = .true.
        end where

        this%soil_zone_max = this%sat_threshold + soil_moist_max * hru_frac_perv
        this%soil_moist_tot = this%ssres_stor + soil_moist * hru_frac_perv

        this%soil_lower = soil_moist - soil_rechr
        this%soil_lower_stor_max = soil_moist_max - soil_rechr_max

        where (this%soil_lower_stor_max > 0.0)
          this%soil_lower_ratio = this%soil_lower / this%soil_lower_stor_max
        end where

        this%dunnian_flow = 0.0

        ! initialize arrays (dimensioned nhru)
        ! TODO: 2018-06-21 - Uncomment once cascade module and lakes are ready.
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
        this%gvr2pfr = 0.0_dp
        this%perv_actet = 0.0
        this%potet_lower = 0.0
        this%potet_rechr = 0.0
        this%pref_flow = 0.0
        this%pref_flow_in = 0.0
        this%pref_flow_infil = 0.0
        this%recharge = 0.0
        this%swale_actet = 0.0
        this%unused_potet = 0.0  ! dimension nhru

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

        if (save_vars_to_file == 1) then
          ! Create restart variables
          ! call ctl_data%add_variable('et_type', this%et_type, 'nhru', 'none')
          ! call ctl_data%add_variable('gravity_stor_res', this%gravity_stor_res, 'nhru', 'none')
          call ctl_data%add_variable('pref_flow_stor', this%pref_flow_stor, 'nhru', 'inches')
          call ctl_data%add_variable('slow_stor', this%slow_stor, 'nhru', 'inches')
          call ctl_data%add_variable('ssres_stor', this%ssres_stor, 'nhru', 'inches')
        end if

        ! Connect summary variables that need to be output
        if (outVarON_OFF == 1) then
          do jj = 1, outVar_names%size()
            ! TODO: This is where the daily basin values are linked based on
            !       what was requested in basinOutVar_names.
            select case(outVar_names%values(jj)%s)
              case('cap_waterin')
                call model_summary%set_summary_var(jj, this%cap_waterin)
              case('dunnian_flow')
                call model_summary%set_summary_var(jj, this%dunnian_flow)
              case('hru_actet')
                call model_summary%set_summary_var(jj, this%hru_actet)
              case('perv_actet')
                call model_summary%set_summary_var(jj, this%perv_actet)
              case('pref_flow')
                call model_summary%set_summary_var(jj, this%pref_flow)
              case('pref_flow_in')
                call model_summary%set_summary_var(jj, this%pref_flow_in)
              case('pref_flow_infil')
                call model_summary%set_summary_var(jj, this%pref_flow_infil)
              case('pref_flow_stor')
                call model_summary%set_summary_var(jj, this%pref_flow_stor)
              case('recharge')
                call model_summary%set_summary_var(jj, this%recharge)
              case('slow_flow')
                call model_summary%set_summary_var(jj, this%slow_flow)
              case('slow_stor')
                call model_summary%set_summary_var(jj, this%slow_stor)
              case('snow_free')
                call model_summary%set_summary_var(jj, this%snow_free)
              case('soil_lower')
                call model_summary%set_summary_var(jj, this%soil_lower)
              case('soil_moist_tot')
                call model_summary%set_summary_var(jj, this%soil_moist_tot)
              case('soil_to_gw')
                call model_summary%set_summary_var(jj, this%soil_to_gw)
              case('soil_to_ssr')
                call model_summary%set_summary_var(jj, this%soil_to_ssr)
              case('ssres_flow')
                call model_summary%set_summary_var(jj, this%ssres_flow)
              case('ssres_in')
                call model_summary%set_summary_var(jj, this%ssres_in)
              case('ssres_stor')
                call model_summary%set_summary_var(jj, this%ssres_stor)
              case('ssr_to_gw')
                call model_summary%set_summary_var(jj, this%ssr_to_gw)
              case('unused_potet')
                call model_summary%set_summary_var(jj, this%unused_potet)
              case default
                ! pass
            end select
          enddo
        endif
      end associate
    end subroutine


    module subroutine run_Soilzone(this, ctl_data, model_basin, model_time, &
                                   model_potet, model_precip, model_climate, intcp, snow, model_transp, model_runoff)
      use iso_fortran_env, only: output_unit, error_unit
      use prms_constants, only: dp, sp, LAKE, LAND, SWALE, NEARZERO, DNEARZERO
      ! use ieee_arithmetic
      ! use ieee_exceptions
      ! use ieee_features
      implicit none

      class(Soilzone) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      class(Potential_ET), intent(inout) :: model_potet
      class(Precipitation), intent(in) :: model_precip
      type(Climateflow), intent(inout) :: model_climate
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      class(Transpiration), intent(in) :: model_transp
      type(Srunoff), intent(inout) :: model_runoff

      ! Local Variables
      integer(i32) :: chru
      integer(i32) :: k
      integer(i32) :: update_potet

      real(r64) :: avail_potet
      real(r32) :: availh2o
      real(r32) :: cap_upflow_max
      real(r32) :: capwater_maxin
      real(r32) :: dunnianflw
      real(r32) :: dunnianflw_gvr
      real(r32) :: dunnianflw_pfr
      real(r64) :: gwin
      ! real(r32) :: gvr_maxin
      real(r64) :: interflow
      real(r32) :: pervactet
      real(r32) :: pref_flow_maxin
      real(r32) :: prefflow
      real(r32) :: ssresin
      real(r32) :: topfr
      real(r32) :: unsatisfied_et

      ! character(len=:), allocatable :: error_txt
      ! type(ieee_flag_type), parameter :: ieee_custom(4) = [ieee_usual, ieee_underflow]
      ! type(ieee_status_type) :: status_value
      ! logical, dimension(4) :: flag_value

      ! TODO: Uncomment when gsflow module(s) is converted
      ! GSFLOW-related variables
      ! real(r32) :: capacity
      ! real(r32) :: dndunn
      ! real(r32) :: dnpreflow
      ! real(r32) :: dnslowflow

      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                gsflow_mode => ctl_data%gsflow_mode, &
                print_debug => ctl_data%print_debug%value, &

                nlake => model_basin%nlake, &
                active_hrus => model_basin%active_hrus, &
                active_mask => model_basin%active_mask, &
                cov_type => model_basin%cov_type, &
                hru_route_order => model_basin%hru_route_order, &
                hru_type => model_basin%hru_type, &

                dprst_evap_hru => model_runoff%dprst_evap_hru, &
                dprst_seep_hru => model_runoff%dprst_seep_hru, &
                hru_area_perv => model_runoff%hru_area_perv, &
                hru_frac_perv => model_runoff%hru_frac_perv, &
                hru_impervevap => model_runoff%hru_impervevap, &
                infil => model_runoff%infil, &
                soil_moist_chg => model_runoff%soil_moist_chg, &
                soil_rechr_chg => model_runoff%soil_rechr_chg, &
                sroff => model_runoff%sroff, &
                srunoff_updated_soil => model_runoff%srunoff_updated_soil, &
                strm_seg_in => model_runoff%strm_seg_in, &

                potet => model_potet%potet, &

                nowtime => model_time%Nowtime, &

                hru_ppt => model_precip%hru_ppt, &

                transp_on => model_transp%transp_on, &

                hru_intcpevap => intcp%hru_intcpevap, &

                snow_evap => snow%snow_evap, &
                snowcov_area => snow%snowcov_area, &

                soil_rechr => model_climate%soil_rechr, &
                soil_rechr_max => model_climate%soil_rechr_max, &
                soil_moist => model_climate%soil_moist, &
                soil_moist_max => model_climate%soil_moist_max)

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
        !   endif
        ! endif


        ! call ieee_get_status(status_value)
        ! call ieee_set_halting_mode(ieee_custom, .false.)
        ! call ieee_set_flag(ieee_custom, .false.)

        if (srunoff_updated_soil) then
          ! Dynamic parameters in srunoff resulted in changes to soil_moist
          ! and soil_rechr.
          ! NOTE: Should this occur before or after the GSFLOW-related
          !       code above?
          ! write(output_unit, *) '**********ARRRRRGHHH!!!!!!'
          soil_moist = soil_moist_chg
          soil_rechr = soil_rechr_chg
        end if

        if (print_debug == 1) then
          this%soil_moist_ante = soil_moist
          this%ssres_stor_ante = this%ssres_stor
        endif

        gwin = 0.0_dp
        update_potet = 0

        ! soil_to_gw for whole HRU
        this%soil_to_gw = 0.0
        this%soil_to_ssr = 0.0
        this%ssr_to_gw = 0.0
        this%slow_flow = 0.0
        this%ssres_flow = 0.0

        this%potet_rechr = 0.0
        this%potet_lower = 0.0
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
          !     potet(chru) = this%hru_actet(chru) ! WARNING: This could be a problem when it happens
          !     update_potet = 1
          !   endif
          !
          !   this%unused_potet(chru) = potet(chru) - this%hru_actet(chru)
          !
          !   if (cascade_flag == 1) then
          !     ! If lake HRU doesn't cascade, should we limit ET to
          !     ! water entering the HRU to this point (no gwflow yet)
          !     this%lakein_sz(chru) = this%upslope_interflow(chru) + this%upslope_dunnianflow(chru)
          !   endif
          !
          !   cycle
          ! endif
          ! --------------------------------

          avail_potet = max(0.0_dp, dble(potet(chru)) - this%hru_actet(chru))

          ! ******Add infiltration to soil and compute excess
          ! NOTE: hru_area_perv(chru) has to be > 0.0
          dunnianflw = 0.0
          dunnianflw_pfr = 0.0
          dunnianflw_gvr = 0.0
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

          ! if (exponent(capwater_maxin) - range(capwater_maxin) >= 0) then
          !   write(*, 9005) MODNAME, ' WARNING: capwater_maxin less than TINY resetting to zero.', capwater_maxin
          !   capwater_maxin = 0.0_dp
          ! endif

          ! 9005 format(A, A, es12.4e2)

          ! Compute preferential flow and storage, and any dunnian flow
          prefflow = 0.0
          if (this%pref_flow_flag(chru)) then
            this%pref_flow_infil(chru) = 0.0
            pref_flow_maxin = 0.0

            if (capwater_maxin > 0.0) then
              ! pref_flow for whole HRU
              pref_flow_maxin = capwater_maxin * this%pref_flow_den(chru)
              capwater_maxin = capwater_maxin - pref_flow_maxin
              pref_flow_maxin = pref_flow_maxin * hru_frac_perv(chru)

              ! Compute contribution to preferential-flow reservoir storage
              this%pref_flow_stor(chru) = this%pref_flow_stor(chru) + pref_flow_maxin
              dunnianflw_pfr = max(0.0, this%pref_flow_stor(chru) - this%pref_flow_max(chru))

              if (dunnianflw_pfr > 0.0) then
                this%pref_flow_stor(chru) = this%pref_flow_max(chru)
              endif

              this%pref_flow_infil(chru) = pref_flow_maxin - dunnianflw_pfr
            endif

            this%pfr_dunnian_flow(chru) = dunnianflw_pfr
          endif

          if (cascade_flag == 1) then
            cap_upflow_max = sngl(this%upslope_dunnianflow(chru) + this%upslope_interflow(chru)) / hru_frac_perv(chru)
            capwater_maxin = capwater_maxin + cap_upflow_max
          endif

          this%cap_infil_tot(chru) = capwater_maxin * hru_frac_perv(chru)

          ! ****** Add infiltration to soil and compute excess
          ! gvr_maxin = 0.0
          this%cap_waterin(chru) = capwater_maxin

          ! Call even if capwater_maxin = 0, just in case soil_moist now > soil_moist_max
          if (capwater_maxin + soil_moist(chru) > 0.0) then
            call this%compute_soilmoist(this%soil2gw_flag(chru), hru_frac_perv(chru), soil_moist_max(chru), &
                                        soil_rechr_max(chru), this%soil2gw_max(chru), &
                                        this%cap_waterin(chru), soil_moist(chru), &
                                        soil_rechr(chru), this%soil_to_gw(chru), this%soil_to_ssr(chru))

            this%cap_waterin(chru) = this%cap_waterin(chru) * hru_frac_perv(chru)
          endif


          ! soil_moist and soil_rechr can get quite small leading to instability
          ! later in the code. Reset to zero when they get too small.
          ! TODO: PAN - what should the smallest value be?

          ! if (soil_moist(chru) > 0.0_dp .and. soil_moist(chru) < DNEARZERO) then
          ! if (exponent(soil_moist(chru)) - exponent(tiny(0.0_sp)) <= 2) then
          !   ! write(output_unit, 9008) MODNAME, '%run() WARNING: soil_moist less than 2.2e-16,', chru, soil_moist(chru), ', reset to zero ', nowtime(1:3)
          !   write(output_unit, 9008) MODNAME, '%run() WARNING: soil_moist(', chru, ') =',  soil_moist(chru), ' (<2.2e-16). Reset to zero ', nowtime(1:3)
          !   soil_moist(chru) = 0.0
          ! endif


          ! ! if (soil_rechr(chru) > 0.0_dp .and. soil_rechr(chru) < DNEARZERO) then
          ! if (exponent(soil_rechr(chru)) - exponent(tiny(0.0_sp)) <= 2) then
          !   ! write(output_unit, 9008) MODNAME, '%run() WARNING: soil_rechr less than 2.2e-16,', chru, soil_rechr(chru), ', reset to zero ', nowtime(1:3)
          !   write(output_unit, 9008) MODNAME, '%run() WARNING: soil_rechr(', chru, ') =' , soil_rechr(chru), ' (<2.2e-16). Reset to zero ', nowtime(1:3)
          !   soil_rechr(chru) = 0.0
          ! endif

          ! 9008 format(A, A, I0.1, A, es11.3e3, A, I4, 2('/', I2.2))

          ! soil_to_ssr for whole HRU
          ! this%soil_to_ssr(chru) = gvr_maxin

          ! Compute slow interflow and ssr_to_gw
          topfr = 0.0

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
          !     ! elseif ( this%gvr2sm(chru)<-NEARZERO ) then
          !     !  print *, 'negative gvr2sm, HRU:', chru, this%gvr2sm(chru)
          !     this%gvr2sm(chru) = 0.0
          !   endif
          !
          !   this%grav_gwin(chru) = sngl(gwin)
          ! else
          ! if (model_mode(1)%s /= 'GSFLOW') then
          if (.not. gsflow_mode) then
            availh2o = this%slow_stor(chru) + this%soil_to_ssr(chru)

            if (hru_type(chru) == LAND) then
              topfr = max(0.0, availh2o - this%pref_flow_thrsh(chru))
              ssresin = this%soil_to_ssr(chru) - topfr
              this%slow_stor(chru) = max(0.0, availh2o - topfr)

              ! Compute slow contribution to interflow, if any
              if (this%slow_stor(chru) > NEARZERO) then
                call this%compute_interflow(this%slowcoef_lin(chru), this%slowcoef_sq(chru), &
                                            ssresin, this%slow_stor(chru), this%slow_flow(chru))
              endif
            elseif (hru_type(chru) == SWALE) then
              this%slow_stor(chru) = availh2o
            endif

            if (this%slow_stor(chru) > NEARZERO .and. this%ssr2gw_rate(chru) > 0.0) then
              call this%compute_gwflow(this%ssr2gw_rate(chru), this%ssr2gw_exp(chru), this%ssr_to_gw(chru), this%slow_stor(chru))
            endif
          endif

          ! Compute contribution to Dunnian flow from PFR, if any
          if (this%pref_flow_flag(chru)) then
            availh2o = this%pref_flow_stor(chru) + topfr
            dunnianflw_gvr = max(0.0, availh2o - this%pref_flow_max(chru))

            if (dunnianflw_gvr > 0.0) then
              ! topfr = topfr - dunnianflw_gvr
              topfr = max(0.0, topfr - dunnianflw_gvr)
            endif

            this%pref_flow_in(chru) = this%pref_flow_infil(chru) + topfr
            this%pref_flow_stor(chru) = this%pref_flow_stor(chru) + topfr

            if (this%pref_flow_stor(chru) > 0.0) then
              call this%compute_interflow(this%fastcoef_lin(chru), this%fastcoef_sq(chru), &
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
                                      this%soil_type(chru), soil_moist_max(chru), &
                                      soil_rechr_max(chru), this%snow_free(chru),  &
                                      soil_moist(chru), soil_rechr(chru), &
                                      avail_potet, this%potet_rechr(chru), &
                                      this%potet_lower(chru), pervactet)
          endif


              !
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
              !   write(output_unit, 9009) MODNAME, '%run WARNING: ', error_txt, ' occurred []'
              !   write(output_unit, 9011) '    ', nowtime(1:3), soil_moist(chru), soil_rechr(chru), this%potet_rechr(chru), this%potet_lower(chru)
              !
              !   call ieee_set_flag(ieee_custom, .false.)
              ! endif
              !
              ! 9009 format(A, A, A, A)
              ! 9011 format(A,I4, 2('/', I2.2), 4es12.4e2)

          ! NOTE: 2019-04-02 PAN - pervactet is reset to zero if it gets outside
          !       of the normal range for real32 numbers. Otherwise hru_actet
          !       (which is real32) can end up with denormal numbers.
          ! if (exponent(pervactet) - exponent(tiny(0.0_sp)) <= 1) then
          !   write(output_unit, 9017) MODNAME, '%run() WARNING: pervactet(', chru, ') =', pervactet, ' is too small, resetting to zero. ', nowtime(1:3)
          !   ! 9017 format(A, A, I0.1, A, es11.3e3, A, I4, 2('/', I2.2))
          !   pervactet = 0.0_dp
          ! end if

          ! if (exponent(soil_moist(chru)) - exponent(tiny(0.0_sp)) <= 3) then
          !   write(output_unit, 9008) MODNAME, '%run() WARNING: soil_moist(', chru, ') =',  soil_moist(chru), ' is too small. Reset to zero (after compute_szactet) ', nowtime(1:3)
          !   soil_moist(chru) = 0.0
          ! endif

          ! if (exponent(soil_rechr(chru)) - exponent(tiny(0.0_sp)) <= 3) then
          !   write(output_unit, 9008) MODNAME, '%run() WARNING: soil_rechr(', chru, ') =' , soil_rechr(chru), ' is too small. Reset to zero (after compute_szactet) ', nowtime(1:3)
          !   soil_rechr(chru) = 0.0
          ! endif

          this%hru_actet(chru) = this%hru_actet(chru) + pervactet * hru_frac_perv(chru)
          avail_potet = dble(potet(chru)) - this%hru_actet(chru)
          this%perv_actet(chru) = pervactet
          this%soil_lower(chru) = soil_moist(chru) - soil_rechr(chru)

          ! If HRU cascades, compute interflow and excess flow to each HRU or stream
          if (hru_type(chru) == LAND) then
            interflow = this%slow_flow(chru) + prefflow

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
            !     endif
            !
            !     this%hru_sz_cascadeflow(chru) = dnslowflow + dnpreflow + dndunn
            !          ! Cascade_interflow(chru) = dnslowflow + dnpreflow
            !          ! Cascade_dunnianflow(chru) = dndunn
            !   endif
            ! endif

            ! Treat pref_flow as interflow
            this%ssres_flow(chru) = this%slow_flow(chru)

            if (this%pref_flow_flag(chru)) then
              this%pref_flow(chru) = prefflow
              this%ssres_flow(chru) = this%ssres_flow(chru) + prefflow
            endif

            ! Treat dunnianflw as surface runoff to streams
            ! WARNING: PAN This is modifying sroff from the srunoff module
            sroff(chru) = sroff(chru) + this%dunnian_flow(chru)
            this%ssres_stor(chru) = this%slow_stor(chru) + this%pref_flow_stor(chru)
          else
            ! For swales
            availh2o = this%slow_stor(chru) - this%sat_threshold(chru)
            this%swale_actet(chru) = 0.0

            if (availh2o > 0.0) then
              ! If ponding, as storage > sat_threshold
              unsatisfied_et = potet(chru) - this%hru_actet(chru)

              if (unsatisfied_et > 0.0) then
                availh2o = min(availh2o, unsatisfied_et)
                this%swale_actet(chru) = availh2o
                this%hru_actet(chru) = this%hru_actet(chru) + this%swale_actet(chru)
                this%slow_stor(chru) = this%slow_stor(chru) - this%swale_actet(chru)
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

          this%ssres_in(chru) = this%soil_to_ssr(chru) + this%pref_flow_infil(chru) + sngl(gwin)
          ! this%soil_moist_tot(chru) = this%ssres_stor(chru) + soil_moist(chru) * hru_frac_perv(chru)

          ! ! DEBUG: This can be removed after debugging
          ! if (.not. ieee_is_normal(this%soil_moist_tot(chru))) then
          !   write(*, *) 'soilzone: soil_moist_tot is denormal. (chru, soil_moist_tot)'
          !   write(*, *) chru, this%soil_moist_tot(chru)
          ! end if
          ! write(*, *) 'SZ: ', chru, this%soil_moist_tot(chru), this%soil_zone_max(chru)

          ! this%recharge(chru) = this%soil_to_gw(chru) + this%ssr_to_gw(chru)
          !
          ! if (dprst_flag == 1) then
          !   this%recharge(chru) = this%recharge(chru) + sngl(dprst_seep_hru(chru))
          ! endif

          this%grav_dunnian_flow(chru) = dunnianflw_gvr
          this%unused_potet(chru) = potet(chru) - this%hru_actet(chru)

          ! NOTE: 2019-04-02 PAN - soil_moist is reset to zero if it gets outside
          !       of the normal range for real32 numbers. Otherwise soil_moist_tot
          !       (which is real32) can end up with denormal numbers.
          ! if (exponent(soil_moist(chru)) - exponent(tiny(0.0_sp)) <= 1) then
          !   write(output_unit, 9017) MODNAME, '%run() WARNING: soil_moist(', chru, ') =', soil_moist(chru), ' is too small, resetting to zero. ', nowtime(1:3)
          !   soil_moist(chru) = 0.0
          !   9017 format(A, A, I0.1, A, es11.3e3, A, I4, 2('/', I2.2))
          ! end if
        enddo

        where (this%soil_lower_stor_max > 0.0)
          this%soil_lower_ratio = this%soil_lower / this%soil_lower_stor_max
        end where

        this%soil_moist_tot = sngl(this%ssres_stor + soil_moist * dble(hru_frac_perv))
        this%recharge = this%soil_to_gw + this%ssr_to_gw

        if (dprst_flag == 1) then
          this%recharge = this%recharge + sngl(dprst_seep_hru)
        endif

        ! if (update_potet == 1) then
        ! endif

        ! TODO: Uncomment once lakes are working.
        ! if (nlake > 0) then
        ! endif
      end associate
    end subroutine


    module subroutine cleanup_Soilzone(this, ctl_data)
      class(Soilzone) :: this
        !! Soilzone class
      type(Control), intent(in) :: ctl_data

      ! --------------------------------------------------------------------------
      associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
        if (save_vars_to_file == 1) then
          ! Write out this module's restart variables
          ! call ctl_data%write_restart_variable('et_type', this%et_type)
          ! ! call ctl_data%write_restart_variable('gravity_stor_res', this%gravity_stor_res)
          call ctl_data%write_restart_variable('pref_flow_stor', this%pref_flow_stor)
          call ctl_data%write_restart_variable('slow_stor', this%slow_stor)
          call ctl_data%write_restart_variable('ssres_stor', this%ssres_stor)
        end if
      end associate
    end subroutine


    !***********************************************************************
    ! Adjust soil moist based on being below field capacity (capacity)
    ! and preferential-flow threshold (pref_flow_thrsh)
    !***********************************************************************
    module subroutine check_gvr_sm(capacity, depth, frac, gvr2sm, input)
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

      !************************************************************************
      ! Check to see if soil is below capacity, if so add up to field capacity.
      !    capacity is for whole HRU
      !    to_sm and gvr2sm are for whole HRU
      frac_sngl = sngl(frac)

      ! Fill up capillary with part of gravity water
      to_sm = capacity

      ! Take all gravity water and put in capillary
      if (to_sm > depth) then
        to_sm = depth
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

        ! TODO: test for cascades
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
    module subroutine compute_gravflow(this, ctl_data, runoff, &
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
      type(Control), intent(in) :: ctl_data
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

      real(r32) :: depth
      real(r64) :: extra_water
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
            extra_water = max(0.0_dp, dble(depth - pref_flow_thrsh))

            if (extra_water > 0.0_dp) then
              ! Compute contribution to preferential-flow reservoir storage
              topfr = topfr + extra_water * frac
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
              perc = ssr2gw_rate * depth**ssr2gw_exp

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
      real(r32), intent(inout) :: ssr_to_gw
      real(r32), intent(inout) :: slow_stor

      ! ********************************************************************
      ! ****** Compute flow to groundwater
      ssr_to_gw = max(0.0, ssr2gw_rate * slow_stor**ssr2gw_exp)
      ssr_to_gw = min(ssr_to_gw, slow_stor)

      slow_stor = slow_stor - ssr_to_gw
    end subroutine


    !***********************************************************************
    ! Compute subsurface lateral flow
    !***********************************************************************
    module subroutine compute_interflow(coef_lin, coef_sq, ssres_in, storage, &
                                        inter_flow)
      use prms_constants, only: dp
      implicit none

      ! Arguments
      real(r32), intent(in) :: coef_lin
      real(r32), intent(in) :: coef_sq
      real(r32), intent(in) :: ssres_in
      real(r32), intent(inout) :: storage
      real(r32), intent(inout) :: inter_flow

      ! Local Variables
      real(r64) :: c1
      real(r64) :: c2
      real(r64) :: c3
      real(r64) :: sos

      ! ***********************************************************************
      ! inter_flow is in inches for the timestep
      ! ****** compute interflow
      if (coef_lin <= 0.0 .and. ssres_in <= 0.0) then
        c1 = dble(coef_sq) * storage
        inter_flow = storage * sngl(c1 / (1.0_dp + c1))
      elseif (coef_lin > 0.0 .and. coef_sq <= 0.0) then
        c2 = dble(1.0 - exp(-coef_lin))
        inter_flow = ssres_in * sngl(1.0 - c2 / coef_lin) + storage * sngl(c2)
      elseif (coef_sq > 0.0) then
        c3 = sqrt(dble(coef_lin**2.0 + 4.0 * coef_sq * ssres_in))
        sos = storage - ((c3 - coef_lin) / (2.0 * coef_sq))

        if (c3 == 0.0_dp) then
          STOP 'ERROR, in compute_interflow sos=0, please contact code developers'
        endif

        c1 = dble(coef_sq) * sos / c3
        c2 = 1.0_dp - exp(-c3)

        if (1.0_dp + c1 * c2 > 0.0_dp) then
          inter_flow = ssres_in + sngl((sos * (1.0_dp + c1) * c2) / (1.0_dp + c1 * c2))
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
        !! Pervious fraction
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(in) :: soil2gw_max
      real(r32), intent(inout) :: infil
      real(r32), intent(inout) :: soil_moist
      real(r32), intent(inout) :: soil_rechr
      real(r32), intent(inout) :: soil_to_gw
      real(r32), intent(inout) :: soil_to_ssr

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
      real(r32) :: excess

      !***********************************************************************
      ! if (minexponent(soil_moist) - exponent(soil_moist) >= 0) then
      !   write(output_unit, 9005) MODNAME, '%compute_soilmoist WARNING: soil_moist underflow, reset to zero'
      !   soil_moist = 0.0
      ! endif
      !
      ! if (minexponent(soil_rechr) - exponent(soil_rechr) >= 0) then
      !   write(output_unit, 9005) MODNAME, '%compute_soilmoist WARNING: soil_rechr underflow, reset to zero'
      !   soil_rechr = 0.0
      ! endif
      !
      ! 9005 format(A,A)

      soil_rechr = min(soil_rechr + infil, soil_rechr_max)

      ! soil_moist_max from previous time step or soil_moist_max has
      ! changed for a restart simulation
      excess = soil_moist + infil
      soil_moist = min(excess, soil_moist_max)
      excess = (excess - soil_moist_max) * perv_frac

      if (excess > 0.0) then
        if (soil2gw_flag) then
          soil_to_gw = min(soil2gw_max, excess)
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

        soil_to_ssr = max(0.0, excess)
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
      use prms_constants, only: dp, sp, DNEARZERO, SAND, LOAM, CLAY, ET_DEFAULT, EVAP_ONLY, EVAP_PLUS_TRANSP
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
      real(r32), intent(inout) :: soil_moist
      real(r32), intent(inout) :: soil_rechr
      real(r64), intent(inout) :: avail_potet
      real(r32), intent(inout) :: potet_rechr
      real(r32), intent(inout) :: potet_lower
      real(r32), intent(out) :: perv_actet

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

      real(r32) :: et
      real(r32) :: pctr
      real(r32) :: pcts

      real(r32) :: soil_moist_ante
      real(r32) :: soil_rechr_ante
      real(r64) :: avail_potet_ante

      real(r32) :: potet_rechr_ante
      real(r32) :: potet_lower_ante

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

        if (avail_potet < DNEARZERO) then
          this%et_type = ET_DEFAULT   ! 1
          avail_potet = 0.0_dp
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
          potet_lower = sngl(avail_potet)
          potet_rechr = sngl(avail_potet)

          select case(soil_type)
            case(SAND)
              ! ****** sandy soil
              if (pcts < 0.25) then
                potet_lower = 0.5 * pcts * sngl(avail_potet)
              endif

              if (pctr < 0.25) then
                potet_rechr = 0.5 * pctr * sngl(avail_potet)
              endif

            case(LOAM)
              ! ****** loam soil
              if (pcts < 0.5) then
                potet_lower = pcts * sngl(avail_potet)
              endif

              if (pctr < 0.5) then
                potet_rechr = pctr * sngl(avail_potet)
              endif

            case(CLAY)
              ! ****** clay soil
              if (pcts < TWOTHIRDS .and. pcts > ONETHIRD) then
                potet_lower = pcts * sngl(avail_potet)
              elseif (pcts <= ONETHIRD) then
                potet_lower = 0.5 * pcts * sngl(avail_potet)
              endif

              if (pctr < TWOTHIRDS .and. pctr > ONETHIRD) then
                potet_rechr = pctr * sngl(avail_potet)
              elseif (pctr <= ONETHIRD) then
                potet_rechr = 0.5 * pctr * sngl(avail_potet)
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

          ! if (exponent(potet_rechr) - exponent(tiny(0.0_sp)) <= 2) then
          !   write(output_unit, 9008) MODNAME, '%compute_szactet() WARNING: potet_rechr =',  potet_rechr, ' (<2.2e-16). Reset to zero ', nowtime(1:3)
          !   potet_rechr = 0.0
          ! endif

          ! if (exponent(soil_moist) - exponent(tiny(0.0_sp)) <= 2) then
          !   write(output_unit, 9008) MODNAME, '%compute_szactet() WARNING: soil_moist =',  soil_moist, ' (<2.2e-16). Reset to zero ', nowtime(1:3)
          !   soil_moist = 0.0
          ! endif

          ! if (exponent(soil_rechr) - exponent(tiny(0.0_sp)) <= 2) then
          !   write(output_unit, 9008) MODNAME, '%compute_szactet() WARNING: soil_rechr =' , soil_rechr, ' (<2.2e-16). Reset to zero ', nowtime(1:3)
          !   soil_rechr = 0.0
          ! endif

          ! 9008 format(A, A, es11.3e3, A, I4, 2('/', I2.2))

          if (potet_rechr > soil_rechr) then
            potet_rechr = soil_rechr
            soil_rechr = 0.0
          else
            soil_rechr = soil_rechr - potet_rechr
          endif

          if (this%et_type == EVAP_ONLY .or. potet_rechr >= potet_lower) then
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

          if (soil_rechr > soil_moist) then
             soil_rechr = soil_moist
          endif
        else
          et = 0.0
        endif

        perv_actet = et

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

end submodule
