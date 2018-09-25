submodule (PRMS_SRUNOFF) sm_srunoff

  contains
    module function constructor_Srunoff(ctl_data, param_data, model_basin) result(this)
      use prms_constants, only: dp
      implicit none

      type(Srunoff) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin

      ! Local variables
      ! integer(i32) :: chru
      ! integer(i32) :: jj
      ! integer(i32) :: kk
      ! integer(i32) :: num_hrus
      ! real(r32) :: frac

      ! nconsumed, nwateruse, segment_transferON_OFF, gwr_transferON_OFF,
      ! external_transferON_OFF, dprst_transferON_OFF, lake_transferON_OFF,
      ! init_vars_from_file, active_hrus, hru_route_order, sroff_flag
      !
      ! Parameters
      ! carea_max, carea_min

      ! ------------------------------------------------------------------------
      associate(nconsumed => ctl_data%nconsumed%value, &
                nhru => ctl_data%nhru%value, &
                nlake => ctl_data%nlake%value, &
                nsegment => ctl_data%nsegment%value, &
                nwateruse => ctl_data%nwateruse%value, &
                cascade_flag => ctl_data%cascade_flag%value, &
                cascadegw_flag => ctl_data%cascadegw_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                dprst_transferON_OFF => ctl_data%dprst_transferON_OFF%value, &
                external_transferON_OFF => ctl_data%external_transferON_OFF%value, &
                gwr_transferON_OFF => ctl_data%gwr_transferON_OFF%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                lake_transferON_OFF => ctl_data%lake_transferON_OFF%value, &
                print_debug => ctl_data%print_debug%value, &
                segment_transferON_OFF => ctl_data%segment_transferON_OFF%value, &
                srunoff_module => ctl_data%srunoff_module%values, &

                active_mask => model_basin%active_mask, &

                carea_max => param_data%carea_max%values, &
                carea_min => param_data%carea_min%values)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        allocate(this%contrib_fraction(nhru))
        allocate(this%hru_impervevap(nhru))
        allocate(this%hru_impervstor(nhru))
        allocate(this%imperv_evap(nhru))
        allocate(this%hru_sroffp(nhru))
        allocate(this%hru_sroffi(nhru))
        allocate(this%hortonian_flow(nhru))

        ! NOTE: moved from sm_flowvars.f90
        allocate(this%imperv_stor(nhru))
        allocate(this%infil(nhru))
        allocate(this%sroff(nhru))

        if (cascade_flag == 1) then
          ! Cascading variables
          allocate(this%upslope_hortonian(nhru))
          allocate(this%hru_hortn_cascflow(nhru))
          this%upslope_hortonian = 0.0_dp
          this%hru_hortn_cascflow = 0.0_dp

          if (nlake > 0) then
            allocate(this%hortonian_lakes(nhru))
            this%hortonian_lakes = 0.0_dp
          endif
        endif

        if (cascade_flag == 1 .or. cascadegw_flag > 0) then
          allocate(this%strm_seg_in(nsegment))
          this%strm_seg_in = 0.0_dp
        endif

        if (print_debug == 1) then
          allocate(this%imperv_stor_ante(nhru))
        endif

        ! Now initialize everything
        this%use_sroff_transfer = .false.
        if (nwateruse > 0) then
          if (segment_transferON_OFF == 1 .or. gwr_transferON_OFF == 1 .or. &
              external_transferON_OFF == 1 .or. dprst_transferON_OFF == 1 .or. &
              lake_transferON_OFF == 1 .or. nconsumed > 0 .or. nwateruse > 0) then

            this%use_sroff_transfer = .true.
          endif
        endif

        this%imperv_evap = 0.0
        this%hortonian_flow = 0.0
        this%hru_sroffi = 0.0
        this%hru_sroffp = 0.0
        this%contrib_fraction = 0.0
        this%hru_impervevap = 0.0
        this%hru_impervstor = 0.0

        ! NOTE: moved from sm_flowvars
        this%imperv_stor = 0.0
        this%infil = 0.0
        this%sroff = 0.0

        if (init_vars_from_file == 0) then
          this%basin_contrib_fraction = 0.0_dp
          this%basin_dprst_evap = 0.0_dp
          this%basin_dprst_seep = 0.0_dp
          this%basin_dprst_sroff = 0.0_dp
          this%basin_dprst_volcl = 0.0_dp
          this%basin_dprst_volop = 0.0_dp
          this%basin_hortonian = 0.0_dp
          this%basin_hortonian_lakes = 0.0_dp
          this%basin_imperv_evap = 0.0_dp
          this%basin_imperv_stor = 0.0_dp
          this%basin_infil = 0.0_dp
          this%basin_sroff = 0.0_dp
          this%basin_sroff_down = 0.0_dp
          this%basin_sroff_upslope = 0.0_dp
          this%basin_sroffi = 0.0_dp
          this%basin_sroffp = 0.0_dp
          this%sri = 0.0
          this%srp = 0.0
        endif

        if (srunoff_module(1)%s == 'srunoff_carea') then
          allocate(this%carea_dif(nhru))

          where (model_basin%active_mask)
            this%carea_dif = carea_max - carea_min
          end where
        endif

        ! Depression storage initialization
        if (dprst_flag == 1) then
          call this%dprst_init(ctl_data, param_data, model_basin)
        endif
      end associate
    end function



    !***********************************************************************
    !     srunoffrun - Computes surface runoff using contributing area
    !                  computations using antecedent soil moisture.
    !***********************************************************************
    module subroutine run_Srunoff(this, ctl_data, param_data, model_basin, &
                                  model_climate, model_potet, intcp, snow, model_time)  ! , cascades)
      use prms_constants, only: dp, LAKE, LAND, NEARZERO
      implicit none
       class(Srunoff), intent(inout) :: this
         !! Srunoff class
       type(Control), intent(in) :: ctl_data
         !! Control file parameters
       type(Parameters), intent(in) :: param_data
         !! Parameters
       type(Basin), intent(in) :: model_basin
         !! Basin variables
       type(Climateflow), intent(in) :: model_climate
         !! Climate variables
       class(Potential_ET), intent(in) :: model_potet
       type(Interception), intent(in) :: intcp
       type(Snowcomp), intent(in) :: snow
       type(Time_t), intent(in) :: model_time

      ! Local Variables
      integer(i32) :: dprst_chk
      integer(i32) :: chru
      integer(i32) :: k

      real(r32) :: avail_et
      real(r32) :: sra
      real(r32) :: srunoff

      real(r64) :: apply_sroff
      ! TODO: Uncomment when cascade converted
      ! real(r64) :: hru_sroff_down
      real(r64) :: runoff

      ! Control
      ! print_debug, dprst_flag, cascade_flag

      ! Parameters
      ! hru_area, hru_type, hru_percent_imperv, imperv_stor_max, snowinfil_max,
      ! sro_to_dprst_imperv, sro_to_dprst_perv,

      ! Basin
      ! active_hrus, hru_route_order, hru_frac_perv, hru_area_imperv, hru_area_perv,
      ! dprst_area_max, basin_area_inv

      ! Interception
      ! hru_intcpevap, net_apply, net_rain, net_ppt, net_snow

      ! Snowcomp
      ! snow_evap(chru), snowmelt, snowcov_area,

      ! Climateflow
      ! pkwater_equiv,

      ! Cascade
      ! ncascade_hru,

      ! Potetential_ET
      ! potet,

      ! ?? call_cascade, hru_area_dble

      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                print_debug => ctl_data%print_debug%value, &

                hru_area => param_data%hru_area%values, &
                hru_type => param_data%hru_type%values, &
                hru_percent_imperv => param_data%hru_percent_imperv%values, &
                imperv_stor_max => param_data%imperv_stor_max%values, &
                snowinfil_max => param_data%snowinfil_max%values, &
                sro_to_dprst_imperv => param_data%sro_to_dprst_imperv%values, &
                sro_to_dprst_perv => param_data%sro_to_dprst_perv%values, &

                active_hrus => model_basin%active_hrus, &
                active_mask => model_basin%active_mask, &
                basin_area_inv => model_basin%basin_area_inv, &
                dprst_area_max => model_basin%dprst_area_max, &
                hru_area_dble => model_basin%hru_area_dble, &
                hru_frac_perv => model_basin%hru_frac_perv, &
                hru_area_imperv => model_basin%hru_area_imperv, &
                hru_area_perv => model_basin%hru_area_perv, &
                hru_route_order => model_basin%hru_route_order, &

                hru_intcpevap => intcp%hru_intcpevap, &
                net_apply => intcp%net_apply, &
                net_ppt => intcp%net_ppt, &
                net_rain => intcp%net_rain, &
                net_snow => intcp%net_snow, &

                snowcov_area => snow%snowcov_area, &
                snowmelt => snow%snowmelt, &
                snow_evap => snow%snow_evap, &

                potet => model_potet%potet, &

                pkwater_equiv => model_climate%pkwater_equiv, &

                nowtime => model_time%Nowtime)

        if (print_debug == 1) then
          this%imperv_stor_ante = this%hru_impervstor

          if (dprst_flag == 1) then
            this%dprst_stor_ante = this%dprst_stor_hru
          endif
        endif

        this%basin_sroffi = 0.0_dp
        this%basin_sroffp = 0.0_dp
        this%basin_imperv_evap = 0.0_dp
        this%basin_imperv_stor = 0.0_dp
        this%basin_apply_sroff = 0.0_dp

        ! TODO: Uncomment once cascades are converted
        ! if (call_cascade == 1) this%strm_seg_in = 0.0_dp

        ! Initialize arrays for current timestep
        this%infil = 0.0
        this%hru_sroffp = 0.0
        this%contrib_fraction = 0.0

        dprst_chk = 0

        do k=1, active_hrus
          chru = hru_route_order(k)
          runoff = 0.0_dp

          if (hru_type(chru) == LAKE) then
            ! HRU is a lake
            ! Eventually add code for lake area less than hru_area that includes
            ! soil_moist for fraction of hru_area that is dry bank.
            if (this%infil(chru) + this%sroff(chru) + this%imperv_stor(chru) + this%imperv_evap(chru) > 0.0) then
              print *, 'srunoff lake ERROR', nowtime, chru, this%infil(chru), this%sroff(chru), this%imperv_stor(chru), this%imperv_evap(chru), chru
            endif

            if (cascade_flag == 1) then
              this%hortonian_lakes(chru) = this%upslope_hortonian(chru)
            endif

            cycle
          endif

          ! WARNING: Placing this initialization for infil, hru_sroffp, and
          !          contrib_fraction here would cause the prior values to remain
          !          when a LAKE hru_type is encountered.
          ! this%infil(chru) = 0.0
          ! this%hru_sroffp(chru) = 0.0
          ! this%contrib_fraction(chru) = 0.0

          this%srp = 0.0
          this%sri = 0.0

          if (hru_area_imperv(chru) > 0.0) then
            this%hru_sroffi(chru) = 0.0
            this%imperv_evap(chru) = 0.0
            this%hru_impervevap(chru) = 0.0
          endif

          avail_et = potet(chru) - snow_evap(chru) - hru_intcpevap(chru)

          ! ******
          ! Compute runoff for pervious, impervious, and depression storage area
          ! do IRRIGATION APPLICATION, ONLY DONE HERE, ASSUMES NO SNOW and
          ! only for pervious areas (just like infiltration).
          ! if (this%use_sroff_transfer) then
          !   if (net_apply(chru) > 0.0) then
          !     sra = 0.0
          !     this%infil(chru) = this%infil(chru) + net_apply(chru)
          !
          !     if (hru_type(chru) == LAND) then
          !       call this%perv_comp(ctl_data, param_data, model_climate, chru, &
          !                           net_apply(chru), net_apply(chru), sra)
          !
          !       ! ** ADD in water from irrigation application and water-use
          !       !    transfer for pervious portion - sra (if any)
          !       apply_sroff = dble(sra * hru_area_perv(chru))
          !       this%basin_apply_sroff = this%basin_apply_sroff + apply_sroff
          !       runoff = runoff + apply_sroff
          !     endif
          !   endif
          ! endif

          call this%compute_infil(ctl_data, param_data, model_basin, model_climate, intcp, snow, chru)

          if (dprst_flag == 1) then
            this%dprst_in(chru) = 0.0_dp
            dprst_chk = 0

            if (dprst_area_max(chru) > 0.0) then
              dprst_chk = 1
              ! ****** Compute the depression storage component
              ! Only call if total depression surface area for each HRU is > 0.0
              call this%dprst_comp(ctl_data, param_data, model_basin, model_climate, model_potet, intcp, snow, model_time, chru, avail_et)

              runoff = runoff + this%dprst_sroff_hru(chru) * hru_area_dble(chru)
            endif
          endif
          ! **********************************************************

          srunoff = 0.0
          if (hru_type(chru) == LAND) then
            ! ******Compute runoff for pervious and impervious area, and depression storage area
            runoff = runoff + dble(this%srp * hru_area_perv(chru) + this%sri * hru_area_imperv(chru))
            srunoff = sngl(runoff / hru_area_dble(chru))

            ! TODO: Uncomment when cascade is converted
            ! ******Compute HRU weighted average (to units of inches/dt)
            ! if (cascade_flag == 1) then
            !   hru_sroff_down = 0.0_dp
            !
            !   if (srunoff > 0.0) then
            !     if (ncascade_hru(chru) > 0) then
            !       call this%run_cascade_sroff(ncascade_hru(chru), srunoff, hru_sroff_down)
            !     endif
            !
            !     this%hru_hortn_casc_flow(chru) = hru_sroff_down
            !     !if ( this%hru_hortn_casc_flow(chru)<0.0D0 ) this%hru_hortn_casc_flow(chru) = 0.0D0
            !     !if ( this%upslope_hortonian(chru)<0.0D0 ) this%upslope_hortonian(chru) = 0.0D0
            !     this%basin_sroff_upslope = this%basin_sroff_upslope + this%upslope_hortonian(chru) * hru_area_dble(chru)
            !     this%basin_sroff_down = this%basin_sroff_down + hru_sroff_down * hru_area_dble(chru)
            !   else
            !     this%hru_hortn_casc_flow(chru) = 0.0_dp
            !   endif
            ! endif

            this%hru_sroffp(chru) = this%srp * hru_frac_perv(chru)
            this%basin_sroffp = this%basin_sroffp + this%srp * hru_area_perv(chru)
          endif

          !******Compute evaporation from impervious area
          if (hru_area_imperv(chru) > 0.0) then
            ! NOTE: imperv_stor can get incredibly small (> e-35) which causes
            !       floating point underflow (SIGFPE).
            ! if (this%imperv_stor(chru) > 0.0) then
            if (this%imperv_stor(chru) > NEARZERO) then
              call this%imperv_et(chru, param_data, potet(chru), snowcov_area(chru), avail_et)

              this%hru_impervevap(chru) = this%imperv_evap(chru) * hru_percent_imperv(chru)
              ! if ( this%hru_impervevap(chru)<0.0 ) this%hru_impervevap(chru) = 0.0
              avail_et = avail_et - this%hru_impervevap(chru)

              if (avail_et < 0.0) then
                this%hru_impervevap(chru) = max(0.0, this%hru_impervevap(chru) + avail_et)

                this%imperv_evap(chru) = this%hru_impervevap(chru) / hru_percent_imperv(chru)
                this%imperv_stor(chru) = this%imperv_stor(chru) - avail_et / hru_percent_imperv(chru)
                avail_et = 0.0
              endif

              this%basin_imperv_evap = this%basin_imperv_evap + dble(this%hru_impervevap(chru) * hru_area(chru))
              this%hru_impervstor(chru) = this%imperv_stor(chru) * hru_percent_imperv(chru)
              this%basin_imperv_stor = this%basin_imperv_stor + dble(this%imperv_stor(chru) * hru_area_imperv(chru))
            endif

            this%hru_sroffi(chru) = this%sri * hru_percent_imperv(chru)
            this%basin_sroffi = this%basin_sroffi + dble(this%sri * hru_area_imperv(chru))
          endif

          if (dprst_chk == 1) then
            this%dprst_stor_hru(chru) = (this%dprst_vol_open(chru) + this%dprst_vol_clos(chru)) / hru_area_dble(chru)
          endif

          this%sroff(chru) = srunoff
          this%hortonian_flow(chru) = srunoff
        enddo

        ! ****** Compute basin weighted averages (to units of inches/dt)
        ! rsr, should be land_area???
        this%basin_contrib_fraction = sum(dble(this%contrib_fraction * hru_area_perv)) * basin_area_inv
        this%basin_hortonian = sum(dble(this%hortonian_flow * hru_area)) * basin_area_inv
        this%basin_infil = sum(dble(this%infil * hru_area_perv)) * basin_area_inv
        this%basin_sroff = sum(dble(this%sroff * hru_area)) * basin_area_inv

        if (cascade_flag == 1) then
          ! this%basin_potet = sum(dble(this%potet * hru_area), mask=active_mask) * basin_area_inv
          this%basin_hortonian_lakes = sum(this%hortonian_lakes * dble(hru_area), mask=active_mask) * basin_area_inv
        endif

        this%basin_imperv_evap = this%basin_imperv_evap * basin_area_inv
        this%basin_imperv_stor = this%basin_imperv_stor * basin_area_inv
        this%basin_sroffp = this%basin_sroffp * basin_area_inv
        this%basin_sroffi = this%basin_sroffi * basin_area_inv

        if (cascade_flag == 1) then
          this%basin_sroff_down = this%basin_sroff_down * basin_area_inv
          this%basin_sroff_upslope = this%basin_sroff_upslope * basin_area_inv
        endif

        if (dprst_flag == 1) then
          this%basin_dprst_volop = sum(this%dprst_vol_open) * basin_area_inv
          this%basin_dprst_volcl = sum(this%dprst_vol_clos) * basin_area_inv
          this%basin_dprst_evap = sum(dble(this%dprst_evap_hru * hru_area)) * basin_area_inv
          this%basin_dprst_seep = sum(this%dprst_seep_hru * hru_area_dble) * basin_area_inv
          this%basin_dprst_sroff = sum(this%dprst_sroff_hru * hru_area_dble) * basin_area_inv
        endif

      end associate
    end subroutine


    module subroutine cleanup_Srunoff(this)
      class(Srunoff) :: this
        !! Srunoff class
    end subroutine


    !***********************************************************************
    ! Initialize depression storage area hydrology
    !***********************************************************************
    module subroutine dprst_init(this, ctl_data, param_data, model_basin)
      use prms_constants, only: dp, NEARZERO
      implicit none

      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin

      ! ------------------
      ! Control vars
      ! init_vars_from_file

      ! ------------------
      ! Parameters
      ! sro_to_dprst_perv, sro_to_dprst_imperv, dprst_depth_avg, dprst_frac_init,
      ! op_flow_thres, va_open_exp, hru_area

      ! dprst_et_coef
      ! when init_vars_from_file=[0, 2, 7]: dprst_frac_init
      ! when has_open_dprst: dprst_seep_rate_open, va_open_exp, op_flow_thres
      ! when has_closed_dprst: dprst_seep_rate_close, va_clos_exp

      ! ------------------
      ! Basin
      ! active_hrus, basin_area_inv, hru_route_order, hru_percent_imperv
      ! dprst_area_max,


      ! Local Variables
      integer(i32) :: chru
      integer(i32) :: j

      real(r32) :: frac_op_ar
      real(r32) :: frac_cl_ar
      real(r32) :: open_vol_r
      real(r32) :: clos_vol_r

      ! ***********************************************************************
      associate(nhru => ctl_data%nhru%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                print_debug => ctl_data%print_debug%value, &
                active_hrus => model_basin%active_hrus, &
                basin_area_inv => model_basin%basin_area_inv, &
                dprst_area_max => model_basin%dprst_area_max, &
                hru_route_order => model_basin%hru_route_order, &

                dprst_depth_avg => param_data%dprst_depth_avg%values, &
                dprst_frac => param_data%dprst_frac%values, &
                dprst_frac_init => param_data%dprst_frac_init%values, &
                dprst_frac_open => param_data%dprst_frac_open%values, &
                dprst_seep_rate_clos => param_data%dprst_seep_rate_clos%values, &
                dprst_seep_rate_open => param_data%dprst_seep_rate_open%values, &
                hru_area => param_data%hru_area%values, &
                hru_percent_imperv => param_data%hru_percent_imperv%values, &
                op_flow_thres => param_data%op_flow_thres%values, &
                sro_to_dprst_imperv => param_data%sro_to_dprst_imperv%values, &
                sro_to_dprst_perv => param_data%sro_to_dprst_perv%values, &
                va_clos_exp => param_data%va_clos_exp%values, &
                va_open_exp => param_data%va_open_exp%values)

        allocate(this%dprst_area_clos(nhru))
        allocate(this%dprst_area_clos_max(nhru))
        allocate(this%dprst_area_open(nhru))
        allocate(this%dprst_area_open_max(nhru))
        allocate(this%dprst_evap_hru(nhru))
        allocate(this%dprst_frac_clos(nhru))
        allocate(this%dprst_in(nhru))
        allocate(this%dprst_insroff_hru(nhru))
        allocate(this%dprst_seep_hru(nhru))
        allocate(this%dprst_sroff_hru(nhru))
        allocate(this%dprst_stor_hru(nhru))
        allocate(this%dprst_vol_clos(nhru))
        allocate(this%dprst_vol_clos_frac(nhru))
        allocate(this%dprst_vol_clos_max(nhru))
        allocate(this%dprst_vol_frac(nhru))
        allocate(this%dprst_vol_open(nhru))
        allocate(this%dprst_vol_open_frac(nhru))
        allocate(this%dprst_vol_open_max(nhru))
        allocate(this%dprst_vol_thres_open(nhru))

        if (print_debug == 1) then
          allocate(this%dprst_stor_ante(nhru))
        endif

        this%dprst_vol_open = 0.0_dp
        this%dprst_vol_clos = 0.0_dp

        this%has_closed_dprst = .false.
        this%has_open_dprst = .false.
        this%dprst_evap_hru = 0.0
        this%dprst_seep_hru = 0.0_dp
        this%dprst_sroff_hru = 0.0_dp
        this%dprst_insroff_hru = 0.0

        ! if (init_vars_from_file==0 .OR. init_vars_from_file==2 .OR. init_vars_from_file==7 ) then
        !   if ( getparam(MODNAME, 'dprst_frac_init', Nhru, 'real', Dprst_frac_init)/=0 ) call read_error(2, 'dprst_frac_init')
        ! endif
        !
        ! if ( getparam(MODNAME, 'dprst_flow_coef', Nhru, 'real', Dprst_flow_coef)/=0 ) call read_error(2, 'dprst_flow_coef')
        ! if ( Dprst_open_flag==1 ) then
        !   if (getparam(MODNAME, 'dprst_seep_rate_open', Nhru, 'real', Dprst_seep_rate_open)/=0 ) call read_error(2, 'dprst_seep_rate_open')
        !   if ( getparam(MODNAME, 'va_open_exp', Nhru, 'real', Va_open_exp)/=0 ) call read_error(2, 'va_open_exp')
        !   if ( getparam(MODNAME, 'op_flow_thres', Nhru, 'real', Op_flow_thres)/=0 ) call read_error(2, 'op_flow_thres')
        ! else
        ! WARNING: this modifies parameters
        !   dprst_seep_rate_open = 0.0
        !   va_open_exp = 0.0
        !   op_flow_thres = 0.0
        ! endif
        ! if ( getparam(MODNAME, 'sro_to_dprst_perv', Nhru, 'real', Sro_to_dprst_perv)/=0 ) call read_error(2, 'sro_to_dprst_perv')
        ! if ( getparam(MODNAME, 'sro_to_dprst_imperv', Nhru, 'real', Sro_to_dprst_imperv)/=0 ) call read_error(2, 'sro_to_dprst_imperv')
        ! if ( getparam(MODNAME, 'dprst_depth_avg', Nhru, 'real', Dprst_depth_avg)/=0 ) call read_error(2, 'dprst_depth_avg')
        ! if ( getparam(MODNAME, 'dprst_et_coef', Nhru, 'real', Dprst_et_coef)/=0 ) call read_error(2, 'dprst_et_coef')
        ! if ( Dprst_clos_flag==1 ) then
        !   if ( getparam(MODNAME, 'dprst_seep_rate_clos', Nhru, 'real', Dprst_seep_rate_clos)/=0 ) call read_error(2, 'dprst_seep_rate_clos')
        !   if ( getparam(MODNAME, 'va_clos_exp', Nhru, 'real', Va_clos_exp)/=0 ) call read_error(2, 'va_clos_exp')
        ! else
        !   ! WARNING: modifies parameters
        !   Dprst_seep_rate_clos = 0.0
        !   Va_clos_exp = 0.0
        ! endif

        this%basin_dprst_volcl = 0.0_dp
        this%basin_dprst_volop = 0.0_dp
        this%dprst_area_clos = 0.0
        this%dprst_area_clos_max = 0.0
        this%dprst_area_open = 0.0
        this%dprst_area_open_max = 0.0
        this%dprst_frac_clos = 0.0
        this%dprst_in = 0.0_dp
        this%dprst_stor_hru = 0.0_dp
        this%dprst_vol_clos_frac = 0.0
        this%dprst_vol_clos_max = 0.0_dp
        this%dprst_vol_frac = 0.0
        this%dprst_vol_open_frac = 0.0
        this%dprst_vol_open_max = 0.0_dp
        this%dprst_vol_thres_open = 0.0_dp

        do j=1, active_hrus
          chru = hru_route_order(j)

          if (dprst_area_max(chru) > 0.0) then
            this%dprst_area_open_max(chru) = dprst_area_max(chru) * dprst_frac_open(chru)
            this%dprst_frac_clos(chru) = 1.0 - dprst_frac_open(chru)
            this%dprst_area_clos_max(chru) = dprst_area_max(chru) - this%dprst_area_open_max(chru)

            if (hru_percent_imperv(chru) + dprst_frac(chru) > 0.999) then
               print *, 'ERROR, impervious plus depression fraction > 0.999 for HRU:', chru
               print *, '       value:', hru_percent_imperv(chru) + dprst_frac(chru)
               ! TODO: What should happen when error occurs?
            endif
          endif

          ! NOTE: originally in basin.f90 only; removed for now
          ! basin_dprst = basin_dprst + dble(dprst_area_max(chru))

          if (this%dprst_area_clos_max(chru) > 0.0) this%has_closed_dprst = .true.
          if (this%dprst_area_open_max(chru) > 0.0) this%has_open_dprst = .true.
        enddo

        do j=1, active_hrus
          chru = hru_route_order(j)

          if (dprst_area_max(chru) > 0.0) then
            ! calculate open and closed volumes (acre-inches) of depression storage by HRU
            ! dprst_area_open_max is the maximum open depression area (acres) that can generate surface runoff:
            if (this%has_closed_dprst) then
              this%dprst_vol_clos_max(chru) = dble(this%dprst_area_clos_max(chru) * dprst_depth_avg(chru))
            endif

            if (this%has_open_dprst) then
              this%dprst_vol_open_max(chru) = dble(this%dprst_area_open_max(chru) * dprst_depth_avg(chru))
            endif

            ! calculate the initial open and closed depression storage volume:
            if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 7) then
              if (this%has_open_dprst) this%dprst_vol_open(chru) = dble(dprst_frac_init(chru)) * this%dprst_vol_open_max(chru)
              if (this%has_closed_dprst) this%dprst_vol_clos(chru) = dble(dprst_frac_init(chru)) * this%dprst_vol_clos_max(chru)
            endif

            ! threshold volume is calculated as the % of maximum open
            ! depression storage above which flow occurs *  total open depression storage volume
            this%dprst_vol_thres_open(chru) = dble(op_flow_thres(chru)) * this%dprst_vol_open_max(chru)

            ! Initial open and closed storage volume as fraction of total open and closed storage volume

            ! Open depression surface area for each HRU:
            if (this%dprst_vol_open(chru) > 0.0_dp) then
              open_vol_r = sngl(this%dprst_vol_open(chru) / this%dprst_vol_open_max(chru))

              if (open_vol_r < NEARZERO) then
                frac_op_ar = 0.0
              elseif (open_vol_r > 1.0) then
                frac_op_ar = 1.0
              else
                frac_op_ar = exp(va_open_exp(chru) * log(open_vol_r))
              endif

              this%dprst_area_open(chru) = this%dprst_area_open_max(chru) * frac_op_ar

              if (this%dprst_area_open(chru) > this%dprst_area_open_max(chru)) then
                this%dprst_area_open(chru) = this%dprst_area_open_max(chru)
              endif
              ! if ( this%dprst_area_open(chru)<NEARZERO ) this%dprst_area_open(chru) = 0.0
            endif

            ! Closed depression surface area for each HRU:
            if (this%dprst_vol_clos(chru) > 0.0_dp) then
              clos_vol_r = sngl(this%dprst_vol_clos(chru) / this%dprst_vol_clos_max(chru))

              if (clos_vol_r < NEARZERO) then
                frac_cl_ar = 0.0
              elseif (clos_vol_r > 1.0) then
                frac_cl_ar = 1.0
              else
                frac_cl_ar = exp(va_clos_exp(chru) * log(clos_vol_r))
              endif

              this%dprst_area_clos(chru) = this%dprst_area_clos_max(chru) * frac_cl_ar

              if (this%dprst_area_clos(chru) > this%dprst_area_clos_max(chru)) then
                this%dprst_area_clos(chru) = this%dprst_area_clos_max(chru)
              endif
              ! if ( this%dprst_area_clos(chru)<NEARZERO ) this%dprst_area_clos(chru) = 0.0
            endif

            ! calculate the basin open and closed depression storage volumes
            this%basin_dprst_volop = this%basin_dprst_volop + this%dprst_vol_open(chru)
            this%basin_dprst_volcl = this%basin_dprst_volcl + this%dprst_vol_clos(chru)
            this%dprst_stor_hru(chru) = (this%dprst_vol_open(chru) + this%dprst_vol_clos(chru)) / dble(hru_area(chru))

            if (this%dprst_vol_open_max(chru) > 0.0) then
              this%dprst_vol_open_frac(chru) = sngl(this%dprst_vol_open(chru) / this%dprst_vol_open_max(chru))
            endif

            if (this%dprst_vol_clos_max(chru) > 0.0) then
              this%dprst_vol_clos_frac(chru) = sngl(this%dprst_vol_clos(chru) / this%dprst_vol_clos_max(chru))
            endif

            this%dprst_vol_frac(chru) = sngl((this%dprst_vol_open(chru) + this%dprst_vol_clos(chru)) / &
                                             (this%dprst_vol_open_max(chru) + this%dprst_vol_clos_max(chru)))
          endif
        enddo

        this%basin_dprst_volop = this%basin_dprst_volop * basin_area_inv
        this%basin_dprst_volcl = this%basin_dprst_volcl * basin_area_inv

        ! if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 7) then
        !   deallocate(dprst_frac_init)
        ! endif
      end associate
    end subroutine


    !***********************************************************************
    ! fill soil to soil_moist_max, if more than capacity restrict
    ! infiltration by snowinfil_max, with excess added to runoff
    !***********************************************************************
    module subroutine check_capacity(this, param_data, model_climate, idx)
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      integer(i32), intent(in) :: idx
      ! real(r32), intent(in) :: snowinfil_max
      ! real(r32), intent(inout) :: infil

      ! Local Variables
      real(r32) :: capacity
      real(r32) :: excess

      ! this
      ! srp, infil

      ! Parameters
      ! soil_moist_max, snowinfil_max

      !***********************************************************************
      associate(snowinfil_max => param_data%snowinfil_max%values, &
                soil_moist_max => param_data%soil_moist_max%values, &
                soil_moist => model_climate%soil_moist)

        ! print *, '-- check_capacity()'
        capacity = soil_moist_max(idx) - soil_moist(idx)
        excess = this%infil(idx) - capacity

        if (excess > snowinfil_max(idx)) then
          this%srp = this%srp + excess - snowinfil_max(idx)
          this%infil(idx) = snowinfil_max(idx) + capacity
        endif
      end associate
    end subroutine



    !***********************************************************************
    ! Compute infiltration
    !***********************************************************************
    module subroutine compute_infil(this, ctl_data, param_data, model_basin, model_climate, &
                                    intcp, snow, idx)
      use prms_constants, only: dp, DNEARZERO, NEARZERO, LAND
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: model_climate
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      integer(i32), intent(in) :: idx

      ! integer(i32), intent(in) :: hru_type
      ! real(r32), intent(in) :: net_rain
      ! real(r32), intent(in) :: net_ppt
      ! real(r32), intent(in) :: imperv_stor_max
      ! real(r32), intent(in) :: snowmelt
      ! real(r32), intent(in) :: snowinfil_max
      ! real(r32), intent(in) :: net_snow
      ! real(r64), intent(in) :: pkwater_equiv
      ! real(r32), intent(inout) :: imperv_stor
      ! real(r32), intent(inout) :: infil

      ! Local Variables
      real(r32) :: avail_water

      ! Control
      ! cascade_flag,

      ! Parameter
      ! hru_type, imperv_stor_max, snowinfil_max,

      ! Snowcomp
      ! pptmix_nopack, snowmelt

      ! Climateflow
      ! pkwater_equiv

      ! Interception
      ! net_rain, net_ppt, net_snow

      ! this%imperv_stor is modified
      ! this%infil is modified
      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value, &

                hru_type => param_data%hru_type%values(idx), &
                imperv_stor_max => param_data%imperv_stor_max%values, &
                snowinfil_max => param_data%snowinfil_max%values, &

                hru_area_imperv => model_basin%hru_area_imperv, &

                pptmix_nopack => snow%pptmix_nopack, &
                snowmelt => snow%snowmelt, &

                pkwater_equiv => model_climate%pkwater_equiv, &

                net_ppt => intcp%net_ppt, &
                net_rain => intcp%net_rain, &
                net_snow => intcp%net_snow)

        ! print *, '-- compute_infil()'

        ! compute runoff from cascading Hortonian flow
        if (cascade_flag == 1) then
          avail_water = sngl(this%upslope_hortonian(idx))

          if (avail_water > 0.0) then
            this%infil(idx) = avail_water

            if (hru_type == LAND) then
              call this%perv_comp(ctl_data, param_data, model_climate, idx, &
                                  avail_water, avail_water, this%srp)
            endif
          endif
        else
          avail_water = 0.0
        endif

        ! ***** If rain/snow event with no antecedent snowpack, compute the
        ! ***** runoff from the rain first and then proceed with the
        ! ***** snowmelt computations.
        ! if (pptmix_nopack(idx) == 1) then
        if (pptmix_nopack(idx)) then
          avail_water = avail_water + net_rain(idx)
          this%infil(idx) = this%infil(idx) + net_rain(idx)

          if (hru_type == LAND) then
            call this%perv_comp(ctl_data, param_data, model_climate, &
                                idx, net_rain(idx), net_rain(idx), this%srp)
          endif
        endif

        ! ***** If precipitation on snowpack, all water available to the surface
        ! ***** is considered to be snowmelt, and the snowmelt infiltration
        ! ***** procedure is used.  If there is no snowpack and no precip,
        ! ***** then check for melt from last of snowpack.  If rain/snow mix
        ! ***** with no antecedent snowpack, compute snowmelt portion of runoff.
        if (snowmelt(idx) > 0.0) then
          avail_water = avail_water + snowmelt(idx)
          this%infil(idx) = this%infil(idx) + snowmelt(idx)

          if (hru_type == LAND) then
            if (pkwater_equiv(idx) > 0.0_dp .or. net_ppt(idx) - net_snow(idx) < NEARZERO) then
              ! ****** Pervious area computations
              call this%check_capacity(param_data, model_climate, idx)
            else
              ! ****** Snowmelt occurred and depleted the snowpack
              call this%perv_comp(ctl_data, param_data, model_climate, idx, snowmelt(idx), &
                                  net_ppt(idx), this%srp)
            endif


          endif

        ! ****** There was no snowmelt but a snowpack may exist.  If there is
        ! ****** no snowpack then check for rain on a snowfree HRU.
        elseif (pkwater_equiv(idx) < DNEARZERO) then
          ! If no snowmelt and no snowpack but there was net snow then
          ! snowpack was small and was lost to sublimation.
          if (net_snow(idx) < NEARZERO .and. net_rain(idx) > 0.0) then
            ! no snow, some rain
            avail_water = avail_water + net_rain(idx)
            this%infil(idx) = this%infil(idx) + net_rain(idx)

            if (hru_type == LAND) then
              call this%perv_comp(ctl_data, param_data, model_climate, idx, &
                                  net_rain(idx), net_rain(idx), this%srp)
            endif
          endif
        elseif (this%infil(idx) > 0.0) then
          ! ***** Snowpack exists, check to see if infil exceeds maximum daily
          ! ***** snowmelt infiltration rate. infil results from rain snow mix
          ! ***** on a snowfree surface.
          if (hru_type == LAND) then
            call this%check_capacity(param_data, model_climate, idx)
          endif
        endif

        ! ****** Impervious area computations
        if (hru_area_imperv(idx) > 0.0) then
          this%imperv_stor(idx) = this%imperv_stor(idx) + avail_water

          if (hru_type == LAND) then
            if (this%imperv_stor(idx) > imperv_stor_max(idx)) then
              this%sri = this%imperv_stor(idx) - imperv_stor_max(idx)
              this%imperv_stor(idx) = imperv_stor_max(idx)
            endif
          endif
        endif
      end associate
    end subroutine


    !***********************************************************************
    ! Compute depression storage area hydrology
    !***********************************************************************
    module subroutine dprst_comp(this, ctl_data, param_data, model_basin, model_climate, model_potet, intcp, snow, model_time, idx, avail_et)
      ! subroutine dprst_comp(Dprst_vol_clos, Dprst_area_clos_max, Dprst_area_clos, &
      !                       Dprst_vol_open_max, Dprst_vol_open, Dprst_area_open_max, &
      !                       Dprst_area_open, Dprst_sroff_hru, Dprst_seep_hru, &
      !                       Sro_to_dprst_perv, Sro_to_dprst_imperv, Dprst_evap_hru, &
      !                       Avail_et, Net_rain, Dprst_in)
      use prms_constants, only: dp, DNEARZERO, NEARZERO
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: model_climate
      class(Potential_ET), intent(in) :: model_potet
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      type(Time_t), intent(in) :: model_time
      integer(i32), intent(in) :: idx
      real(r32), intent(inout) :: avail_et

      ! Local Variables
      real(r32) :: clos_vol_r
      real(r32) :: dprst_avail_et
      real(r32) :: dprst_sri
      real(r32) :: dprst_sri_clos
      real(r32) :: dprst_sri_open
      real(r32) :: dprst_srp
      real(r32) :: dprst_srp_clos
      real(r32) :: dprst_srp_open
      real(r32) :: frac_cl_ar
      real(r32) :: frac_op_ar
      real(r32) :: inflow
      real(r32) :: open_vol_r
      real(r32) :: tmp
      real(r32) :: unsatisfied_et

      real(r64) :: dprst_evap_clos
      real(r64) :: dprst_evap_open
      real(r64) :: seep_open
      real(r64) :: seep_clos
      real(r64) :: tmp1

      ! this
      ! upslope_hortonian, sri (RW), srp (RW),

      ! Control
      ! cascade_flag

      ! Parameter
      ! dprst_et_coef, dprst_flow_coef, dprst_frac_open,
      ! dprst_seep_rate_clos, dprst_seep_rate_open,
      ! sro_to_dprst_imperv, sro_to_dprst_perv,
      ! va_clos_exp, va_open_exp

      ! Basin
      ! hru_area,

      ! Snowcomp
      ! pptmix_nopack, snowmelt, snowcov_area,

      ! Climateflow
      ! pkwater_equiv

      ! Interception
      ! net_rain, net_snow

      ! Potential_ET
      ! potet,

      ! *Dprst_vol_clos => this%dprst_vol(chru)
      !  Dprst_area_clos_max => this%dprst_area_clos_max(chru)
      ! *Dprst_area_clos => this%dprst_area_clos(chru)
      !  Dprst_vol_open_max => this%dprst_area_max(chru)
      ! *Dprst_vol_open => this%dprst_vol_open(chru)
      !  Dprst_area_open_max => this%dprst_area_open_max(chru)
      ! *Dprst_area_open => this%dprst_area_open(chru)
      ! *Dprst_sroff_hru => this%dprst_sroff_hru(chru)
      ! *Dprst_seep_hru => this%dprst_seep_hru(chru)
      !  Sro_to_dprst_perv => sro_to_dprst_perv(chru)
      !  Sro_to_dprst_imperv => sro_to_dprst_imperv(chru)
      ! *Dprst_evap_hru => this%dprst_evap_hru(chru)
      ! *Avail_et => avail_et
      !  Net_rain => net_rain(chru)
      ! *Dprst_in => this%dprst_in(chru)


      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value, &

                dprst_et_coef => param_data%dprst_et_coef%values, &
                dprst_flow_coef => param_data%dprst_flow_coef%values, &
                dprst_frac_open => param_data%dprst_frac_open%values, &
                dprst_seep_rate_clos => param_data%dprst_seep_rate_clos%values, &
                dprst_seep_rate_open => param_data%dprst_seep_rate_open%values, &
                hru_area => param_data%hru_area%values, &
                hru_percent_imperv => param_data%hru_percent_imperv%values, &
                sro_to_dprst_imperv => param_data%sro_to_dprst_imperv%values, &
                sro_to_dprst_perv => param_data%sro_to_dprst_perv%values, &
                va_clos_exp => param_data%va_clos_exp%values, &
                va_open_exp => param_data%va_open_exp%values, &

                hru_area_dble => model_basin%hru_area_dble, &
                hru_frac_perv => model_basin%hru_frac_perv, &

                pptmix_nopack => snow%pptmix_nopack, &
                snowmelt => snow%snowmelt, &
                snowcov_area => snow%snowcov_area, &

                pkwater_equiv => model_climate%pkwater_equiv, &

                potet => model_potet%potet, &

                net_rain => intcp%net_rain, &
                net_snow => intcp%net_snow, &

                nowtime => model_time%Nowtime)

        ! print *, '-- dprst_comp()'

        ! Add the hortonian flow to the depression storage volumes:
        if (cascade_flag == 1) then
          inflow = sngl(this%upslope_hortonian(idx))
        else
          inflow = 0.0
        endif

        ! if (pptmix_nopack(idx) == 1) then
        if (pptmix_nopack(idx)) then
          inflow = inflow + net_rain(idx)
        endif

        ! **** If precipitation on snowpack all water available to the surface
        ! **** is considered to be snowmelt. If there is no snowpack and
        ! **** no precip,then check for melt from last of snowpack. If rain/snow
        ! **** mix with no antecedent snowpack, compute snowmelt portion of runoff.
        if (snowmelt(idx) > 0.0) then
          inflow = inflow + snowmelt(idx)
        elseif (pkwater_equiv(idx) < DNEARZERO) then
          ! ****** There was no snowmelt but a snowpack may exist.  If there is
          ! ****** no snowpack then check for rain on a snowfree HRU.
          if (net_snow(idx) < NEARZERO .and. net_rain(idx) > 0.0) then
            ! If no snowmelt and no snowpack but there was net snow then
            ! snowpack was small and was lost to sublimation.
            inflow = inflow + net_rain(idx)
          endif
        endif

        this%dprst_in(idx) = 0.0_dp

            ! if (idx == 11) then
            !   print *, '(S) ', this%dprst_vol_open(idx), pptmix_nopack(idx), inflow, net_rain(idx)
            ! endif

        ! ******* Block 1 *******
        if (this%dprst_area_open_max(idx) > 0.0) then
          this%dprst_in(idx) = dble(inflow * this%dprst_area_open_max(idx))  ! inch-acres
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + this%dprst_in(idx)
        endif

        if (this%dprst_area_clos_max(idx) > 0.0) then
          tmp1 = dble(inflow * this%dprst_area_clos_max(idx))  ! inch-acres
          this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + tmp1
          this%dprst_in(idx) = this%dprst_in(idx) + tmp1
        endif

        this%dprst_in(idx) = this%dprst_in(idx) / hru_area_dble(idx)  ! inches over HRU

        ! Add any pervious surface runoff fraction to depressions
        dprst_srp = 0.0
        dprst_sri = 0.0

        ! Pervious surface runoff
        if (this%srp > 0.0) then
          tmp = this%srp * hru_frac_perv(idx) * sro_to_dprst_perv(idx) * hru_area(idx)

          ! ******* Block 2 *******
          if (this%dprst_area_open_max(idx) > 0.0) then
            dprst_srp_open = tmp * dprst_frac_open(idx)  ! acre-inches
            dprst_srp = dprst_srp_open / hru_area(idx)
            this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + dble(dprst_srp_open)
          endif

          if (this%dprst_area_clos_max(idx) > 0.0) then
            dprst_srp_clos = tmp * this%dprst_frac_clos(idx)
            dprst_srp = dprst_srp + dprst_srp_clos / hru_area(idx)
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + dble(dprst_srp_clos)
          endif

          this%srp = this%srp - dprst_srp / hru_frac_perv(idx)

          if (this%srp < 0.0) then
            if (this%srp < -NEARZERO) then
              write(*, 9004) MODNAME, 'WARNING: ', nowtime(1:3), idx, ' dprst srp < 0.0 ', this%srp, dprst_srp
            endif

            ! May need to adjust dprst_srp and volumes
            this%srp = 0.0
          endif
        endif

        9004 format(A,A,I4,2('/', I2.2),I7,A,2F10.7)
        ! 9005 format(A,A,I4,2('/', I2.2),I7,3es12.4e2)

        ! Impervious surface runoff
        if (this%sri > 0.0) then
          tmp = this%sri * hru_percent_imperv(idx) * sro_to_dprst_imperv(idx) * hru_area(idx)

          ! ******* Block 3 *******
          if (this%dprst_area_open_max(idx) > 0.0) then
            dprst_sri_open = tmp * dprst_frac_open(idx)
            dprst_sri = dprst_sri_open / hru_area(idx)
            this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + dble(dprst_sri_open)
          endif

          if (this%dprst_area_clos_max(idx) > 0.0) then
            dprst_sri_clos = tmp * this%dprst_frac_clos(idx)
            dprst_sri = dprst_sri + dprst_sri_clos / hru_area(idx)
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + dble(dprst_sri_clos)
          endif

          this%sri = this%sri - dprst_sri / hru_percent_imperv(idx)

          if (this%sri < 0.0) then
            if (this%sri < -NEARZERO) then
              write(*, 9004) MODNAME, ' WARNING: ', nowtime(1:3), idx, ' dprst sri < 0.0; (sri, dprst_sri) ', this%sri, dprst_sri
            endif

            ! May need to adjust dprst_sri and volumes
            this%sri = 0.0
          endif
        endif

        this%dprst_insroff_hru(idx) = dprst_srp + dprst_sri

        ! Open depression surface area for each HRU:
        this%dprst_area_open(idx) = 0.0

        if (this%dprst_vol_open(idx) > 0.0_dp) then
          open_vol_r = sngl(this%dprst_vol_open(idx) / this%dprst_vol_open_max(idx))

          if (open_vol_r < NEARZERO) then
            frac_op_ar = 0.0
          elseif (open_vol_r > 1.0) then
            frac_op_ar = 1.0
          else
            frac_op_ar = exp(va_open_exp(idx) * log(open_vol_r))
          endif

          this%dprst_area_open(idx) = min(this%dprst_area_open_max(idx) * frac_op_ar, &
                                          this%dprst_area_open_max(idx))

          ! this%dprst_area_open(idx) = this%dprst_area_open_max(idx) * frac_op_ar
          !
          ! if (this%dprst_area_open(idx) > this%dprst_area_open_max(idx)) then
          !   this%dprst_area_open(idx) = this%dprst_area_open_max(idx)
          ! endif
        endif

        ! Closed depression surface area for each HRU:
        if (this%dprst_area_clos_max(idx) > 0.0) then
          this%dprst_area_clos(idx) = 0.0

          if (this%dprst_vol_clos(idx) > 0.0_dp) then
            clos_vol_r = sngl(this%dprst_vol_clos(idx) / this%dprst_vol_clos_max(idx))

            if (clos_vol_r < NEARZERO) then
              frac_cl_ar = 0.0
            elseif (clos_vol_r > 1.0) then
              frac_cl_ar = 1.0
            else
              frac_cl_ar = exp(va_clos_exp(idx) * log(clos_vol_r))
            endif

            this%dprst_area_clos(idx) = min(this%dprst_area_clos_max(idx) * frac_cl_ar, &
                                            this%dprst_area_clos_max(idx))

            ! this%dprst_area_clos(idx) = this%dprst_area_clos_max(idx) * frac_cl_ar
            !
            ! if (this%dprst_area_clos(idx) > this%dprst_area_clos_max(idx)) then
            !   this%dprst_area_clos(idx) = this%dprst_area_clos_max(idx)
            ! endif
          endif
        endif

        ! Evaporate water from depressions based on snowcov_area
        ! dprst_evap_open & dprst_evap_clos = inches-acres on the HRU
        unsatisfied_et = avail_et
        dprst_avail_et = (potet(idx) * (1.0 - snowcov_area(idx))) * dprst_et_coef(idx)
        this%dprst_evap_hru(idx) = 0.0

        ! ******* Block 4 *******
        if (dprst_avail_et > 0.0) then
          dprst_evap_open = 0.0_dp
          dprst_evap_clos = 0.0_dp

          if (this%dprst_area_open(idx) > 0.0) then
            dprst_evap_open = min(dble(this%dprst_area_open(idx) * dprst_avail_et), &
                                  this%dprst_vol_open(idx), &
                                  dble(unsatisfied_et * hru_area(idx)))

            ! if (dprst_evap_open / hru_area(idx) > unsatisfied_et) then
            !   dprst_evap_open = unsatisfied_et * hru_area(idx)
            ! endif
            !
            ! if (dprst_evap_open > real(this%dprst_vol_open(idx), r32)) then
            !   dprst_evap_open = real(this%dprst_vol_open(idx), r32)
            ! endif

            this%dprst_vol_open(idx) = max(this%dprst_vol_open(idx) - dprst_evap_open, 0.0_dp)
            unsatisfied_et = unsatisfied_et - sngl(dprst_evap_open / hru_area_dble(idx))
          endif

          if (this%dprst_area_clos(idx) > 0.0) then
            dprst_evap_clos = min(dble(this%dprst_area_clos(idx) * dprst_avail_et), this%dprst_vol_clos(idx))

            if (dprst_evap_clos / hru_area_dble(idx) > unsatisfied_et) then
              dprst_evap_clos = unsatisfied_et * hru_area(idx)
            endif

            if (dprst_evap_clos > this%dprst_vol_clos(idx)) then
              dprst_evap_clos = this%dprst_vol_clos(idx)
            endif

            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) - dprst_evap_clos

            ! Added 2018-07-24 PAN
            this%dprst_vol_clos(idx) = max(0.0_dp, this%dprst_vol_clos(idx))
          endif

          this%dprst_evap_hru(idx) = sngl((dprst_evap_open + dprst_evap_clos) / hru_area_dble(idx))
        endif

        ! Compute seepage
        this%dprst_seep_hru(idx) = 0.0_dp

        ! ******* Block 5 *******
        if (this%dprst_vol_open(idx) > 0.0_dp) then
          ! Compute seepage
          seep_open = this%dprst_vol_open(idx) * dble(dprst_seep_rate_open(idx))
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) - seep_open

          if (this%dprst_vol_open(idx) < 0.0_dp) then
            seep_open = seep_open + this%dprst_vol_open(idx)
            this%dprst_vol_open(idx) = 0.0_dp
          endif

          this%dprst_seep_hru(idx) = seep_open / hru_area_dble(idx)
        endif

        ! compute open surface runoff
        this%dprst_sroff_hru(idx) = 0.0_dp

        ! ******* Block 6 *******
        if (this%dprst_vol_open(idx) > 0.0_dp) then
          this%dprst_sroff_hru(idx) = max(0.0_dp, this%dprst_vol_open(idx) - this%dprst_vol_open_max(idx))
          this%dprst_sroff_hru(idx) = this%dprst_sroff_hru(idx) + &
                                      max(0.0_dp, (this%dprst_vol_open(idx) - this%dprst_sroff_hru(idx) - &
                                          this%dprst_vol_thres_open(idx)) * dble(dprst_flow_coef(idx)))
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) - this%dprst_sroff_hru(idx)
          this%dprst_sroff_hru(idx) = this%dprst_sroff_hru(idx) / hru_area_dble(idx)

          this%dprst_vol_open(idx) = max(0.0_dp, this%dprst_vol_open(idx))
          ! if (this%dprst_vol_open(idx) < 0.0_dp) then
          !   this%dprst_vol_open(idx) = 0.0_dp
          ! endif
        endif

        if (this%dprst_area_clos_max(idx) > 0.0) then
          if (this%dprst_area_clos(idx) > NEARZERO) then
            seep_clos = this%dprst_vol_clos(idx) * dble(dprst_seep_rate_clos(idx))
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) - seep_clos

            if (this%dprst_vol_clos(idx) < 0.0_dp) then
              seep_clos = seep_clos + this%dprst_vol_clos(idx)
              this%dprst_vol_clos(idx) = 0.0_dp
            endif

            this%dprst_seep_hru(idx) = this%dprst_seep_hru(idx) + seep_clos / hru_area_dble(idx)
          endif

          this%dprst_vol_clos(idx) = max(0.0_dp, this%dprst_vol_clos(idx))
          ! if (this%dprst_vol_clos(idx) < 0.0_dp) then
          !   this%dprst_vol_clos(idx) = 0.0_dp
          ! endif
        endif

        ! this%basin_dprst_volop = this%basin_dprst_volop + this%dprst_vol_open(idx)
        ! this%basin_dprst_volcl = this%basin_dprst_volcl + this%dprst_vol_clos(idx)
        ! this%basin_dprst_evap = this%basin_dprst_evap + dble(this%dprst_evap_hru(idx) * hru_area(idx))
        ! this%basin_dprst_seep = this%basin_dprst_seep + this%dprst_seep_hru(idx) * hru_area_dble(idx)
        ! this%basin_dprst_sroff = this%basin_dprst_sroff + this%dprst_sroff_hru(idx) * hru_area_dble(idx)
        avail_et = avail_et - this%dprst_evap_hru(idx)

        if (this%dprst_vol_open_max(idx) > 0.0) then
          this%dprst_vol_open_frac(idx) = sngl(this%dprst_vol_open(idx) / this%dprst_vol_open_max(idx))
        endif

        if (this%dprst_vol_clos_max(idx) > 0.0) then
          this%dprst_vol_clos_frac(idx) = sngl(this%dprst_vol_clos(idx) / this%dprst_vol_clos_max(idx))
        endif

        this%dprst_vol_frac(idx) = sngl((this%dprst_vol_open(idx) + this%dprst_vol_clos(idx)) / (this%dprst_vol_open_max(idx) + this%dprst_vol_clos_max(idx)))
        this%dprst_stor_hru(idx) = (this%dprst_vol_open(idx) + this%dprst_vol_clos(idx)) / hru_area_dble(idx)
      end associate
    end subroutine



    !***********************************************************************
    ! Subroutine to compute evaporation from impervious area at
    ! potential ET rate up to available ET
    !***********************************************************************
    ! module subroutine imperv_et(imperv_stor, potet, imperv_evap, sca, avail_et)
    module subroutine imperv_et(this, idx, param_data, potet, sca, avail_et)
      ! this%imperv_stor(chru), potet(chru), this%imperv_evap(chru), snowcov_area(chru), avail_et

      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      integer(i32), intent(in) :: idx
      type(Parameters), intent(in) :: param_data
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: sca
      real(r32), intent(in) :: avail_et
      ! real(r32), intent(inout) :: imperv_stor
      ! real(r32), intent(inout) :: imperv_evap

      ! ********************************************************************
      associate(hru_percent_imperv => param_data%hru_percent_imperv%values)

        ! print *, '-- imperv_et()'
        if (sca < 1.0) then
          if (potet < this%imperv_stor(idx)) then
            this%imperv_evap(idx) = potet * (1.0 - sca)
          else
            this%imperv_evap(idx) = this%imperv_stor(idx) * (1.0 - sca)
          endif

          if (this%imperv_evap(idx) * hru_percent_imperv(idx) > avail_et) then
            this%imperv_evap(idx) = avail_et / hru_percent_imperv(idx)
          endif

          this%imperv_stor(idx) = this%imperv_stor(idx) - this%imperv_evap(idx)
        endif
      end associate
    end subroutine



    module subroutine perv_comp(this, ctl_data, param_data, model_climate, &
                                idx, pptp, ptc, srp)
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      ! type(Flowvars), intent(in) :: model_flow
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: pptp
      real(r32), intent(in) :: ptc
      ! real(r32), intent(inout) :: infil
      real(r32), intent(inout) :: srp

      ! Local Variables
      real(r32) :: smidx
      real(r32) :: srpp
      real(r32) :: ca_fraction

      ! Control
      ! srunoff_module

      ! Parameters
      ! smidx_coef, smidx_exp, carea_max, carea_min, soil_rechr_max

      ! Flowvars
      ! soil_moist, soil_rechr

      !***********************************************************************
      associate(srunoff_module => ctl_data%srunoff_module%values, &

                carea_max => param_data%carea_max%values, &
                carea_min => param_data%carea_min%values, &
                smidx_coef => param_data%smidx_coef%values, &
                smidx_exp => param_data%smidx_exp%values, &
                soil_rechr_max => param_data%soil_rechr_max%values, &

                soil_moist => model_climate%soil_moist, &
                soil_rechr => model_climate%soil_rechr)

        ! print *, '-- perv_comp()'
        !****** Pervious area computations
        ! if (sroff_flag == 1) then
        if (srunoff_module(1)%s == 'srunoff_smidx') then
          ! antecedent soil_moist
          smidx = soil_moist(idx) + (0.5 * ptc)
          ca_fraction = smidx_coef(idx) * 10.0**(smidx_exp(idx) * smidx)
        else
          ! srunoff_module == 'srunoff_carea'
          ! antecedent soil_rechr
          ca_fraction = carea_min(idx) + this%carea_dif(idx) * &
                        (soil_rechr(idx) / soil_rechr_max(idx))
        endif

        if (ca_fraction > carea_max(idx)) then
          ca_fraction = carea_max(idx)
        endif

        srpp = ca_fraction * pptp
        this%contrib_fraction(idx) = ca_fraction

        ! TODO: infil and srp update this%infil and this%srp
        this%infil(idx) = this%infil(idx) - srpp
        srp = srp + srpp
      end associate
    end subroutine

end submodule
