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

          if (nlake > 0) then
            allocate(this%hortonian_lakes(nhru))
          endif
        endif

        if (cascade_flag == 1 .or. cascadegw_flag > 0) then
          allocate(this%strm_seg_in(nsegment))
        endif

        if (print_debug == 1) then
          allocate(this%imperv_stor_ante(nhru))
        endif

        ! Now initialize everything
        this%use_sroff_transfer = 0
        if (nwateruse > 0) then
          if (segment_transferON_OFF == 1 .or. gwr_transferON_OFF == 1 .or. &
              external_transferON_OFF == 1 .or. dprst_transferON_OFF == 1 .or. &
              lake_transferON_OFF == 1 .or. nconsumed > 0 .or. nwateruse > 0) then

              this%use_sroff_transfer = 1
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

        if (cascade_flag == 1 .or. cascadegw_flag > 0) then
          this%strm_seg_in = 0.0_dp
        endif

        if (cascade_flag == 1) then
          this%upslope_hortonian = 0.0_dp
          this%hru_hortn_cascflow = 0.0_dp

          if (nlake > 0) then
            this%hortonian_lakes = 0.0_dp
          endif
        endif

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
        ! if (sroff_flag == 2) then
          where (model_basin%active_mask)
            this%carea_dif = carea_max - carea_min
          end where

          ! do jj=1, active_hrus
          !   chru = hru_route_order(jj)
          !
          !   this%carea_dif(chru) = carea_max(chru) - carea_min(chru)
          ! enddo
        endif

        ! Depression storage initialization
        if (dprst_flag == 1) call this%dprst_init(ctl_data, param_data, model_basin)
      end associate
    end function



    !***********************************************************************
    !     srunoffrun - Computes surface runoff using contributing area
    !                  computations using antecedent soil moisture.
    !***********************************************************************
    module subroutine run_Srunoff(this, ctl_data, param_data, model_basin, &
                                  model_climate, model_flow, intcp, snow)  ! , cascades)
      use prms_constants, only: dp
      implicit none
       class(Srunoff) :: this
         !! Srunoff class
       type(Control), intent(in) :: ctl_data
         !! Control file parameters
       type(Parameters), intent(in) :: param_data
         !! Parameters
       type(Basin), intent(in) :: model_basin
         !! Basin variables
       type(Climateflow), intent(in) :: model_climate
         !! Climate variables
       type(Flowvars), intent(in) :: model_flow
       type(Interception), intent(in) :: intcp
       type(Snowcomp), intent(in) :: snow

      ! Local Variables
      integer(i32) :: dprst_chk
      integer(i32) :: i
      integer(i32) :: k

      real(r32) :: avail_et
      real(r32) :: hperv
      real(r32) :: sra
      real(r32) :: srunoff

      real(r64) :: apply_sroff
      real(r64) :: hru_sroff_down
      real(r64) :: runoff

      ! Control
      ! print_debug, dprst_flag, cascade_flag

      ! Parameters
      ! hru_area, hru_type, hru_percent_imperv, imperv_stor_max, snowinfil_max,
      ! sro_to_dprst_imperv, sro_to_dprst_perv,

      ! Basin
      ! active_hrus, hru_route_order, hru_frac_perv, hru_imperv, hru_perv,
      ! dprst_area_max, basin_area_inv

      ! Interception
      ! hru_intcpevap, net_apply, net_rain, net_ppt, net_snow

      ! Snowcomp
      ! snow_evap(i), snowmelt, snowcov_area,

      ! Climateflow
      ! potet, pkwater_equiv,

      ! Cascade
      ! ncascade_hru,

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
                basin_area_inv => model_basin%basin_area_inv, &
                dprst_area_max => model_basin%dprst_area_max, &
                hru_frac_perv => model_basin%hru_frac_perv, &
                hru_imperv => model_basin%hru_imperv, &
                hru_perv => model_basin%hru_perv, &
                hru_route_order => model_basin%hru_route_order, &
                hru_intcpevap => intcp%hru_intcpevap, &
                net_apply => intcp%net_apply, &
                net_ppt => intcp%net_ppt, &
                net_rain => intcp%net_rain, &
                net_snow => intcp%net_snow, &
                snowcov_area => snow%snowcov_area, &
                snowmelt => snow%snowmelt, &
                snow_evap => snow%snow_evap, &
                potet => model_climate%potet, &
                pkwater_equiv => model_climate%pkwater_equiv)

        if (print_debug == 1) then
          this%imperv_stor_ante = this%hru_impervstor
          if (dprst_flag == 1) this%dprst_stor_ante = this%dprst_stor_hru
        endif

        this%basin_sroffi = 0.0_dp
        this%basin_sroffp = 0.0_dp
        this%basin_sroff = 0.0_dp
        this%basin_infil = 0.0_dp
        this%basin_imperv_evap = 0.0_dp
        this%basin_imperv_stor = 0.0_dp
        this%basin_hortonian = 0.0_dp
        this%basin_contrib_fraction = 0.0_dp
        this%basin_apply_sroff = 0.0_dp

        ! if (call_cascade == 1) this%strm_seg_in = 0.0_dp
        !
        ! if (cascade_flag == 1) then
        !   this%basin_sroff_down = 0.0_dp
        !   this%basin_sroff_upslope = 0.0_dp
        !   this%basin_hortonian_lakes = 0.0_dp
        !   this%upslope_hortonian = 0.0_dp
        ! endif

        if (dprst_flag == 1) then
          this%basin_dprst_sroff = 0.0_dp
          this%basin_dprst_evap = 0.0_dp
          this%basin_dprst_seep = 0.0_dp
          this%basin_dprst_volop = 0.0_dp
          this%basin_dprst_volcl = 0.0_dp
        endif

        dprst_chk = 0
        do k = 1, active_hrus
          i = hru_route_order(k)
          this%hruarea = hru_area(i)
          this%hruarea_dble = dble(hru_area(i))
          ! ihru = i  ! not sure what this is for
          runoff = 0.0_dp

          if (hru_type(i) == 2) then
            ! HRU is a lake
            ! eventually add code for lake area less than hru_area
            ! that includes soil_moist for fraction of hru_area that is dry bank
            ! Sanity check
            if (this%infil(i) + this%sroff(i) + this%imperv_stor(i) + this%imperv_evap(i) > 0.0) then
              print *, 'srunoff lake ERROR', this%infil(i), this%sroff(i), this%imperv_stor(i), this%imperv_evap(i), i
            endif

            if (cascade_flag == 1) then
              this%hortonian_lakes(i) = this%upslope_hortonian(i)
              this%basin_hortonian_lakes = this%basin_hortonian_lakes + this%hortonian_lakes(i) * this%hruarea_dble
            endif

            cycle
          endif

          this%infil(i) = 0.0
          hperv = hru_perv(i)
          this%perv_frac = hru_frac_perv(i)
          this%srp = 0.0
          this%sri = 0.0
          this%hru_sroffp(i) = 0.0
          this%contrib_fraction(i) = 0.0
          this%hruarea_imperv = hru_imperv(i)

          if (this%hruarea_imperv > 0.0) then
            this%imperv_frac = hru_percent_imperv(i)
            this%hru_sroffi(i) = 0.0
            this%imperv_evap(i) = 0.0
            this%hru_impervevap(i) = 0.0
          endif

          avail_et = potet(i) - snow_evap(i) - hru_intcpevap(i)

          ! ******
          ! Compute runoff for pervious, impervious, and depression storage area
          ! do IRRIGATION APPLICATION, ONLY DONE HERE, ASSUMES NO SNOW and
          ! only for pervious areas (just like infiltration).
          if (this%use_sroff_transfer == 1) then
            if (net_apply(i) > 0.0) then
              sra = 0.0
              this%infil(i) = this%infil(i) + net_apply(i)

              if (hru_type(i) == 1) then
                call this%perv_comp(ctl_data, param_data, model_flow, i, &
                                    net_apply(i), net_apply(i), sra)

                ! ** ADD in water from irrigation application and water-use
                !    transfer for pervious portion - sra (if any)
                apply_sroff = dble(sra * hperv)
                this%basin_apply_sroff = this%basin_apply_sroff + apply_sroff
                runoff = runoff + apply_sroff
              endif
            endif
          endif

          call this%compute_infil(ctl_data, param_data, model_climate, model_flow, intcp, snow, i)
          ! call this%compute_infil(net_rain(i), net_ppt(i), this%imperv_stor(i), &
          !                    imperv_stor_max(i), snowmelt(i), snowinfil_max(i), &
          !                    net_snow(i), pkwater_equiv(i), this%infil(i), hru_type(i))

          if (dprst_flag == 1) then
            this%dprst_in(i) = 0.0_dp
            dprst_chk = 0

            if (dprst_area_max(i) > 0.0) then
              dprst_chk = 1
              ! ****** Compute the depression storage component
              ! only call if total depression surface area for each HRU is > 0.0
              call this%dprst_comp(ctl_data, param_data, model_climate, intcp, snow, i, avail_et)
              ! call this%dprst_comp(this%dprst_vol_clos(i), this%dprst_area_clos_max(i), &
              !                 this%dprst_area_clos(i), this%dprst_vol_open_max(i), &
              !                 this%dprst_vol_open(i), this%dprst_area_open_max(i), &
              !                 this%dprst_area_open(i), this%dprst_sroff_hru(i), &
              !                 this%dprst_seep_hru(i), sro_to_dprst_perv(i), &
              !                 sro_to_dprst_imperv(i), this%dprst_evap_hru(i), &
              !                 avail_et, net_rain(i), this%dprst_in(i))
              runoff = runoff + this%dprst_sroff_hru(i) * this%hruarea_dble
            endif
          endif
          ! **********************************************************

          srunoff = 0.0
          if (hru_type(i) == 1) then
            ! ******Compute runoff for pervious and impervious area, and depression storage area
            runoff = runoff + dble(this%srp * hperv + this%sri * this%hruarea_imperv)
            srunoff = sngl(runoff / this%hruarea_dble)

            ! TODO: Uncomment when cascade is converted
            ! ******Compute HRU weighted average (to units of inches/dt)
            ! if (cascade_flag == 1) then
            !   hru_sroff_down = 0.0_dp
            !
            !   if (srunoff > 0.0) then
            !     if (ncascade_hru(i) > 0) then
            !       call this%run_cascade_sroff(ncascade_hru(i), srunoff, hru_sroff_down)
            !     endif
            !
            !     this%hru_hortn_casc_flow(i) = hru_sroff_down
            !     !if ( this%hru_hortn_casc_flow(i)<0.0D0 ) this%hru_hortn_casc_flow(i) = 0.0D0
            !     !if ( this%upslope_hortonian(i)<0.0D0 ) this%upslope_hortonian(i) = 0.0D0
            !     this%basin_sroff_upslope = this%basin_sroff_upslope + this%upslope_hortonian(i) * this%hruarea_dble
            !     this%basin_sroff_down = this%basin_sroff_down + hru_sroff_down * this%hruarea_dble
            !   else
            !     this%hru_hortn_casc_flow(i) = 0.0_dp
            !   endif
            ! endif

            this%hru_sroffp(i) = this%srp * this%perv_frac
            this%basin_sroffp = this%basin_sroffp + this%srp * hperv
          endif

          this%basin_infil = this%basin_infil + dble(this%infil(i) * hperv)
          this%basin_contrib_fraction = this%basin_contrib_fraction + dble(this%contrib_fraction(i) * hperv)

          !******Compute evaporation from impervious area
          if (this%hruarea_imperv > 0.0) then
            if (this%imperv_stor(i) > 0.0) then
              call this%imperv_et(i, potet(i), snowcov_area(i), avail_et)
              ! call this%imperv_et(this%imperv_stor(i), potet(i), this%imperv_evap(i), &
              !                snowcov_area(i), avail_et)

              this%hru_impervevap(i) = this%imperv_evap(i) * this%imperv_frac
              !if ( this%hru_impervevap(i)<0.0 ) this%hru_impervevap(i) = 0.0
              avail_et = avail_et - this%hru_impervevap(i)

              if (avail_et < 0.0) then
                ! sanity check
                ! if ( avail_et<-NEARZERO ) print*, 'avail_et<0 in srunoff imperv', i, Nowmonth, Nowday, avail_et
                this%hru_impervevap(i) = this%hru_impervevap(i) + avail_et

                if (this%hru_impervevap(i) < 0.0) this%hru_impervevap(i) = 0.0

                this%imperv_evap(i) = this%hru_impervevap(i) / this%imperv_frac
                this%imperv_stor(i) = this%imperv_stor(i) - avail_et / this%imperv_frac
                avail_et = 0.0
              endif

              this%basin_imperv_evap = this%basin_imperv_evap + dble(this%hru_impervevap(i) * this%hruarea)
              this%hru_impervstor(i) = this%imperv_stor(i) * this%imperv_frac
              this%basin_imperv_stor = this%basin_imperv_stor + dble(this%imperv_stor(i) * this%hruarea_imperv)
            endif

            this%hru_sroffi(i) = this%sri * this%imperv_frac
            this%basin_sroffi = this%basin_sroffi + dble(this%sri * this%hruarea_imperv)
          endif

          if (dprst_chk == 1) this%dprst_stor_hru(i) = (this%dprst_vol_open(i) + this%dprst_vol_clos(i)) / this%hruarea_dble

          this%sroff(i) = srunoff
          this%hortonian_flow(i) = srunoff
          this%basin_hortonian = this%basin_hortonian + dble(srunoff * this%hruarea)
          this%basin_sroff = this%basin_sroff + dble(srunoff * this%hruarea)
        enddo

        !******Compute basin weighted averages (to units of inches/dt)
        !rsr, should be land_area???
        this%basin_sroff = this%basin_sroff * basin_area_inv
        this%basin_imperv_evap = this%basin_imperv_evap * basin_area_inv
        this%basin_imperv_stor = this%basin_imperv_stor * basin_area_inv
        this%basin_infil = this%basin_infil * basin_area_inv
        this%basin_sroffp = this%basin_sroffp * basin_area_inv
        this%basin_sroffi = this%basin_sroffi * basin_area_inv
        this%basin_hortonian = this%basin_hortonian * basin_area_inv
        this%basin_contrib_fraction = this%basin_contrib_fraction * basin_area_inv

        if (cascade_flag == 1) then
          this%basin_hortonian_lakes = this%basin_hortonian_lakes * basin_area_inv
          this%basin_sroff_down = this%basin_sroff_down * basin_area_inv
          this%basin_sroff_upslope = this%basin_sroff_upslope * basin_area_inv
        endif

        if (dprst_flag == 1) then
          this%basin_dprst_volop = this%basin_dprst_volop * basin_area_inv
          this%basin_dprst_volcl = this%basin_dprst_volcl * basin_area_inv
          this%basin_dprst_evap = this%basin_dprst_evap * basin_area_inv
          this%basin_dprst_seep = this%basin_dprst_seep * basin_area_inv
          this%basin_dprst_sroff = this%basin_dprst_sroff * basin_area_inv
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
          ! basin_dprst = basin_dprst + dble(dprst_area_max(i))

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
    module subroutine check_capacity(this, param_data, model_flow, idx)
      ! SUBROUTINE check_capacity(Snowinfil_max, Infil)
      ! USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_moist
      ! USE PRMS_SRUNOFF, ONLY: Ihru, Srp
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Parameters), intent(in) :: param_data
      type(Flowvars), intent(in) :: model_flow
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

      ! Flowvars
      ! soil_moist

      !***********************************************************************
      associate(snowinfil_max => param_data%snowinfil_max%values, &
                soil_moist_max => param_data%soil_moist_max%values, &
                soil_moist => model_flow%soil_moist)

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
    module subroutine compute_infil(this, ctl_data, param_data, model_climate, &
                                    model_flow, intcp, snow, idx)
      ! subroutine compute_infil(net_rain, net_ppt, imperv_stor, imperv_stor_max, snowmelt, &
      !                          snowinfil_max, net_snow, pkwater_equiv, infil, hru_type)
      ! USE PRMS_SRUNOFF, ONLY: Sri, Hruarea_imperv, Upslope_hortonian, idx, Srp
      ! USE PRMS_SNOW, ONLY: pptmix_nopack
      ! USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      ! USE PRMS_MODULE, ONLY: Cascade_flag
      use prms_constants, only: dp, DNEARZERO, NEARZERO
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      type(Flowvars), intent(in) :: model_flow
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
                pptmix_nopack => snow%pptmix_nopack, &
                snowmelt => snow%snowmelt, &
                pkwater_equiv => model_climate%pkwater_equiv, &
                net_ppt => intcp%net_ppt, &
                net_rain => intcp%net_rain, &
                net_snow => intcp%net_snow)

        ! compute runoff from cascading Hortonian flow
        if (cascade_flag == 1) then
          avail_water = sngl(this%upslope_hortonian(idx))

          if (avail_water > 0.0) then
            this%infil = avail_water

            if (hru_type == 1) then
              call this%perv_comp(ctl_data, param_data, model_flow, idx, &
                                  avail_water, avail_water, this%srp)
            endif
          endif
        else
          avail_water = 0.0
        endif

        ! ****** If rain/snow event with no antecedent snowpack,
        ! ****** compute the runoff from the rain first and then proceed with the
        ! ****** snowmelt computations
        if (pptmix_nopack(idx) == 1) then
          avail_water = avail_water + net_rain(idx)
          this%infil(idx) = this%infil(idx) + net_rain(idx)

          if (hru_type == 1) call this%perv_comp(ctl_data, param_data, model_flow, &
                                                 idx, net_rain(idx), net_rain(idx), this%srp)
        endif

        ! ****** If precipitation on snowpack, all water available to the surface is
        ! ****** considered to be snowmelt, and the snowmelt infiltration
        ! ****** procedure is used.  If there is no snowpack and no precip,
        ! ****** then check for melt from last of snowpack.  If rain/snow mix
        ! ****** with no antecedent snowpack, compute snowmelt portion of runoff.
        if (snowmelt(idx) > 0.0) then
          avail_water = avail_water + snowmelt(idx)
          this%infil(idx) = this%infil(idx) + snowmelt(idx)

          if (hru_type == 1) then
            if (pkwater_equiv(idx) > 0.0_dp .or. net_ppt(idx) - net_snow(idx) < NEARZERO) then
              ! ****** Pervious area computations
              ! this, param_data, model_flow, idx
              call this%check_capacity(param_data, model_flow, idx)
            else
              ! ****** Snowmelt occurred and depleted the snowpack
              call this%perv_comp(ctl_data, param_data, model_flow, idx, snowmelt(idx), net_ppt(idx), this%srp)
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

            if (hru_type == 1) call this%perv_comp(ctl_data, param_data, model_flow, &
                                                   idx, net_rain(idx), net_rain(idx), this%srp)
          endif

        ! ***** Snowpack exists, check to see if infil exceeds maximum daily
        ! ***** snowmelt infiltration rate. infil results from rain snow mix
        ! ***** on a snowfree surface.
      elseif (this%infil(idx) > 0.0) then
          if (hru_type == 1) call this%check_capacity(param_data, model_flow, idx)
        endif

        ! ****** Impervious area computations
        if (this%hruarea_imperv > 0.0) then
          this%imperv_stor = this%imperv_stor + avail_water

          if (hru_type == 1) then
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
    module subroutine dprst_comp(this, ctl_data, param_data, model_climate, intcp, snow, idx, avail_et)
      ! subroutine dprst_comp(Dprst_vol_clos, Dprst_area_clos_max, Dprst_area_clos, &
      !                       Dprst_vol_open_max, Dprst_vol_open, Dprst_area_open_max, &
      !                       Dprst_area_open, Dprst_sroff_hru, Dprst_seep_hru, &
      !                       Sro_to_dprst_perv, Sro_to_dprst_imperv, Dprst_evap_hru, &
      !                       Avail_et, Net_rain, Dprst_in)
      ! USE PRMS_SRUNOFF, ONLY: Srp, Sri, idx, Perv_frac, Imperv_frac, this%hruarea, Dprst_et_coef, &
      !    Dprst_seep_rate_open, Dprst_seep_rate_clos, Va_clos_exp, Va_open_exp, Dprst_flow_coef, &
      !    Dprst_vol_thres_open, Dprst_vol_clos_max, Dprst_insroff_hru, Upslope_hortonian, &
      !    Basin_dprst_volop, Basin_dprst_volcl, Basin_dprst_evap, Basin_dprst_seep, Basin_dprst_sroff, &
      !    Dprst_vol_open_frac, Dprst_vol_clos_frac, Dprst_vol_frac, Dprst_stor_hru, this%hruarea_dble
      ! USE PRMS_MODULE, ONLY: Cascade_flag !, Print_debug
      ! USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO, Dprst_frac_open, Dprst_frac_clos
      ! USE PRMS_INTCP, ONLY: Net_snow
      ! USE PRMS_CLIMATEVARS, ONLY: Potet
      ! USE PRMS_FLOWVARS, ONLY: Pkwater_equiv
      ! USE PRMS_SNOW, ONLY: Snowmelt, Pptmix_nopack, Snowcov_area
      use prms_constants, only: dp, DNEARZERO, NEARZERO
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      integer(i32), intent(in) :: idx
      real(r32), intent(inout) :: avail_et

      ! real(r32), intent(in) :: Dprst_area_open_max
      ! real(r32), intent(in) :: Dprst_area_clos_max
      ! real(r32), intent(in) :: Net_rain
      ! real(r32), intent(in) :: Sro_to_dprst_perv
      ! real(r32), intent(in) :: Sro_to_dprst_imperv
      ! real(r64), intent(in) :: Dprst_vol_open_max
      ! real(r64), intent(inout) :: Dprst_vol_open
      ! real(r64), intent(inout) :: Dprst_vol_clos
      ! real(r64), intent(inout) :: Dprst_in
      ! real(r32), intent(inout) :: Avail_et
      ! real(r32), intent(out) :: Dprst_area_open
      ! real(r32), intent(out) :: Dprst_area_clos
      ! real(r32), intent(out) :: Dprst_evap_hru
      ! real(r64), intent(out) :: Dprst_sroff_hru
      ! real(r64), intent(out) :: Dprst_seep_hru

      ! Local Variables
      real(r32) :: clos_vol_r
      real(r32) :: dprst_avail_et
      real(r32) :: dprst_evap_clos
      real(r32) :: dprst_evap_open
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

      real(r64) :: seep_open
      real(r64) :: seep_clos
      real(r64) :: tmp1

      ! this
      ! upslope_hortonian, srp,

      ! Control
      ! cascade_flag

      ! Parameter
      ! dprst_et_coef, dprst_flow_coef, dprst_frac_open,
      ! dprst_seep_rate_clos, dprst_seep_rate_open,
      ! sro_to_dprst_imperv, sro_to_dprst_perv,
      ! va_clos_exp, va_open_exp

      ! Snowcomp
      ! pptmix_nopack, snowmelt, snowcov_area,

      ! Climateflow
      ! pkwater_equiv, potet

      ! Interception
      ! net_rain, net_snow

      ! *Dprst_vol_clos => this%dprst_vol(i)
      !  Dprst_area_clos_max => this%dprst_area_clos_max(i)
      ! *Dprst_area_clos => this%dprst_area_clos(i)
      !  Dprst_vol_open_max => this%dprst_area_max(i)
      ! *Dprst_vol_open => this%dprst_vol_open(i)
      !  Dprst_area_open_max => this%dprst_area_open_max(i)
      ! *Dprst_area_open => this%dprst_area_open(i)
      ! *Dprst_sroff_hru => this%dprst_sroff_hru(i)
      ! *Dprst_seep_hru => this%dprst_seep_hru(i)
      !  Sro_to_dprst_perv => sro_to_dprst_perv(i)
      !  Sro_to_dprst_imperv => sro_to_dprst_imperv(i)
      ! *Dprst_evap_hru => this%dprst_evap_hru(i)
      ! *Avail_et => avail_et
      !  Net_rain => net_rain(i)
      ! *Dprst_in => this%dprst_in(i)


      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                dprst_et_coef => param_data%dprst_et_coef%values, &
                dprst_flow_coef => param_data%dprst_flow_coef%values, &
                dprst_frac_open => param_data%dprst_frac_open%values, &
                dprst_seep_rate_clos => param_data%dprst_seep_rate_clos%values, &
                dprst_seep_rate_open => param_data%dprst_seep_rate_open%values, &
                sro_to_dprst_imperv => param_data%sro_to_dprst_imperv%values, &
                sro_to_dprst_perv => param_data%sro_to_dprst_perv%values, &
                va_clos_exp => param_data%va_clos_exp%values, &
                va_open_exp => param_data%va_open_exp%values, &
                pptmix_nopack => snow%pptmix_nopack, &
                snowmelt => snow%snowmelt, &
                snowcov_area => snow%snowcov_area, &
                pkwater_equiv => model_climate%pkwater_equiv, &
                potet => model_climate%potet, &
                net_rain => intcp%net_rain, &
                net_snow => intcp%net_snow)

        ! Add the hortonian flow to the depression storage volumes:
        if (cascade_flag == 1) then
          inflow = sngl(this%upslope_hortonian(idx))
        else
          inflow = 0.0
        endif

        if (pptmix_nopack(idx) == 1) inflow = inflow + net_rain(idx)

        ! ****** If precipitation on snowpack all water available to the surface is considered to be snowmelt
        ! ****** If there is no snowpack and no precip,then check for melt from last of snowpack.
        ! ****** If rain/snow mix with no antecedent snowpack, compute snowmelt portion of runoff.
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
        if (this%dprst_area_open_max(idx) > 0.0) then
          this%dprst_in(idx) = dble(inflow * this%dprst_area_open_max(idx))  ! inch-acres
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + this%dprst_in(idx)
        endif

        if (this%dprst_area_clos_max(idx) > 0.0) then
          tmp1 = dble(inflow * this%dprst_area_clos_max(idx))  ! inch-acres
          this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + tmp1
          this%dprst_in(idx) = this%dprst_in(idx) + tmp1
        endif

        this%dprst_in(idx) = this%dprst_in(idx) / this%hruarea_dble  ! inches over HRU

        ! Add any pervious surface runoff fraction to depressions
        dprst_srp = 0.0
        dprst_sri = 0.0

        if (this%srp > 0.0) then
          tmp = this%srp * this%perv_frac * sro_to_dprst_perv(idx) * this%hruarea

          if (this%dprst_area_open_max(idx) > 0.0) then
            dprst_srp_open = tmp * dprst_frac_open(idx)  ! acre-inches
            dprst_srp = dprst_srp_open / this%hruarea
            this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + dble(dprst_srp_open)
          endif

          if (this%dprst_area_clos_max(idx) > 0.0) then
            dprst_srp_clos = tmp * this%dprst_frac_clos(idx)
            dprst_srp = dprst_srp + dprst_srp_clos / this%hruarea
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + dble(dprst_srp_clos)
          endif

          this%srp = this%srp - dprst_srp / this%perv_frac

          if (this%srp < 0.0) then
            if (this%srp < -NEARZERO) print *, 'dprst srp<0.0', this%srp, dprst_srp
            ! May need to adjust dprst_srp and volumes
            this%srp = 0.0
          endif
        endif

        if (this%sri > 0.0) then
          tmp = this%sri * this%imperv_frac * sro_to_dprst_imperv(idx) * this%hruarea

          if (this%dprst_area_open_max(idx) > 0.0) then
            dprst_sri_open = tmp * dprst_frac_open(idx)
            dprst_sri = dprst_sri_open / this%hruarea
            this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + dble(dprst_sri_open)
          endif

          if (this%dprst_area_clos_max(idx) > 0.0) then
            dprst_sri_clos = tmp * this%dprst_frac_clos(idx)
            dprst_sri = dprst_sri + dprst_sri_clos / this%hruarea
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + dble(dprst_sri_clos)
          endif

          this%sri = this%sri - dprst_sri / this%imperv_frac

          if (this%sri < 0.0) then
            if (this%sri < -NEARZERO) print *, 'dprst sri<0.0', this%sri, dprst_sri
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

          this%dprst_area_open(idx) = this%dprst_area_open_max(idx) * frac_op_ar
          if (this%dprst_area_open(idx) > this%dprst_area_open_max(idx)) this%dprst_area_open(idx) = this%dprst_area_open_max(idx)
          ! if ( this%dprst_area_open(idx)<NEARZERO ) this%dprst_area_open(idx) = 0.0
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

            this%dprst_area_clos(idx) = this%dprst_area_clos_max(idx) * frac_cl_ar
            if (this%dprst_area_clos(idx) > this%dprst_area_clos_max(idx)) this%dprst_area_clos(idx) = this%dprst_area_clos_max(idx)
            ! if ( this%dprst_area_clos(idx)<NEARZERO ) this%dprst_area_clos(idx) = 0.0
          endif
        endif

        ! Evaporate water from depressions based on snowcov_area
        ! dprst_evap_open & dprst_evap_clos = inches-acres on the HRU
        unsatisfied_et = avail_et
        dprst_avail_et = (potet(idx) * (1.0 - snowcov_area(idx))) * dprst_et_coef(idx)
        this%dprst_evap_hru(idx) = 0.0

        if (dprst_avail_et > 0.0) then
          dprst_evap_open = 0.0
          dprst_evap_clos = 0.0

          if (this%dprst_area_open(idx) > 0.0) then
            dprst_evap_open = min(this%dprst_area_open(idx) * dprst_avail_et, sngl(this%dprst_vol_open(idx)))

            if (dprst_evap_open / this%hruarea > unsatisfied_et) then
              !if ( Print_debug>-1 ) then
              !  print *, 'Warning, open dprst evaporation > available ET, HRU:, ', idx, &
                       ! unsatisfied_et, dprst_evap_open*dble(Dprst_frac_open(idx))
              !  print *, 'Set to available ET, perhaps dprst_et_coef specified too large'
              !  print *, 'Set print_debug to -1 to turn off message'
              !endif
              dprst_evap_open = unsatisfied_et * this%hruarea
            endif

            !if ( dprst_evap_open>sngl(this%dprst_vol_open(idx)) ) print *, '>', dprst_evap_open, dprst_vol_open
            if (dprst_evap_open > sngl(this%dprst_vol_open(idx))) dprst_evap_open = sngl(this%dprst_vol_open(idx))
            unsatisfied_et = unsatisfied_et - dprst_evap_open / this%hruarea
            this%dprst_vol_open(idx) = this%dprst_vol_open(idx) - dble(dprst_evap_open)
          endif

          if (this%dprst_area_clos(idx) > 0.0) then
            dprst_evap_clos = min(this%dprst_area_clos(idx) * dprst_avail_et, sngl(this%dprst_vol_clos(idx)))
            if (dprst_evap_clos / this%hruarea > unsatisfied_et) then
              !if ( Print_debug>-1 ) then
              !  print *, 'Warning, closed dprst evaporation > available ET, HRU:, ', idx, &
                        ! unsatisfied_et, dprst_evap_clos*Dprst_frac_clos(idx)
              !  print *, 'Set to available ET, perhaps dprst_et_coef specified too large'
              !  print *, 'Set print_debug to -1 to turn off message'
              !endif
              dprst_evap_clos = unsatisfied_et * this%hruarea
            endif

            if (dprst_evap_clos > sngl(this%dprst_vol_clos(idx))) dprst_evap_clos = sngl(this%dprst_vol_clos(idx))
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) - dble(dprst_evap_clos)
          endif

          this%dprst_evap_hru(idx) = (dprst_evap_open + dprst_evap_clos) / this%hruarea
        endif

        ! Compute seepage
        this%dprst_seep_hru(idx) = 0.0_dp

        if (this%dprst_vol_open(idx) > 0.0_dp) then
          seep_open = this%dprst_vol_open(idx) * dble(dprst_seep_rate_open(idx))
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) - seep_open

          if (this%dprst_vol_open(idx) < 0.0_dp) then
           ! if ( this%dprst_vol_open(idx)<-DNEARZERO ) print *, 'negative dprst_vol_open:', this%dprst_vol_open(idx), ' HRU:', idx
            seep_open = seep_open + this%dprst_vol_open(idx)
            this%dprst_vol_open(idx) = 0.0_dp
          endif
          this%dprst_seep_hru(idx) = seep_open / this%hruarea_dble
        endif

        ! compute open surface runoff
        this%dprst_sroff_hru(idx) = 0.0_dp

        if (this%dprst_vol_open(idx) > 0.0_dp) then
          this%dprst_sroff_hru(idx) = max(0.0_dp, this%dprst_vol_open(idx) - this%dprst_vol_open_max(idx))
          this%dprst_sroff_hru(idx) = this%dprst_sroff_hru(idx) + &
                            max(0.0_dp, (this%dprst_vol_open(idx) - this%dprst_sroff_hru(idx) - &
                                this%dprst_vol_thres_open(idx)) * dble(dprst_flow_coef(idx)))
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) - this%dprst_sroff_hru(idx)
          this%dprst_sroff_hru(idx) = this%dprst_sroff_hru(idx) / this%hruarea_dble

          ! Sanity checks
          if (this%dprst_vol_open(idx) < 0.0_dp) then
            ! if ( this%dprst_vol_open(idx)<-DNEARZERO ) print *, 'issue, dprst_vol_open<0.0', this%dprst_vol_open(idx)
            this%dprst_vol_open(idx) = 0.0_dp
          endif
        endif

        if (this%dprst_area_clos_max(idx) > 0.0) then
          if (this%dprst_area_clos(idx) > NEARZERO) then
            seep_clos = this%dprst_vol_clos(idx) * dble(dprst_seep_rate_clos(idx))
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) - seep_clos

            if (this%dprst_vol_clos(idx) < 0.0_dp) then
              ! if ( this%dprst_vol_clos(idx)<-DNEARZERO ) print *, 'issue, dprst_vol_clos<0.0', this%dprst_vol_clos(idx)
              seep_clos = seep_clos + this%dprst_vol_clos(idx)
              this%dprst_vol_clos(idx) = 0.0_dp
            endif
            this%dprst_seep_hru(idx) = this%dprst_seep_hru(idx) + seep_clos / this%hruarea_dble
          endif
          if (this%dprst_vol_clos(idx) < 0.0_dp) then
            ! if ( this%dprst_vol_clos(idx)<-DNEARZERO ) print *, 'issue, dprst_vol_clos<0.0', this%dprst_vol_clos(idx)
            this%dprst_vol_clos(idx) = 0.0_dp
          endif
        endif

        this%basin_dprst_volop = this%basin_dprst_volop + this%dprst_vol_open(idx)
        this%basin_dprst_volcl = this%basin_dprst_volcl + this%dprst_vol_clos(idx)
        this%basin_dprst_evap = this%basin_dprst_evap + dble(this%dprst_evap_hru(idx) * this%hruarea)
        this%basin_dprst_seep = this%basin_dprst_seep + this%dprst_seep_hru(idx) * this%hruarea_dble
        this%basin_dprst_sroff = this%basin_dprst_sroff + this%dprst_sroff_hru(idx) * this%hruarea_dble
        avail_et = avail_et - this%dprst_evap_hru(idx)

        if (this%dprst_vol_open_max(idx) > 0.0) this%dprst_vol_open_frac(idx) = sngl(this%dprst_vol_open(idx) / this%dprst_vol_open_max(idx))
        if (this%dprst_vol_clos_max(idx) > 0.0) this%dprst_vol_clos_frac(idx) = sngl(this%dprst_vol_clos(idx) / this%dprst_vol_clos_max(idx))

        this%dprst_vol_frac(idx) = sngl((this%dprst_vol_open(idx) + this%dprst_vol_clos(idx)) / (this%dprst_vol_open_max(idx) + this%dprst_vol_clos_max(idx)))
        this%dprst_stor_hru(idx) = (this%dprst_vol_open(idx) + this%dprst_vol_clos(idx)) / this%hruarea_dble
      end associate
    end subroutine



    !***********************************************************************
    ! Subroutine to compute evaporation from impervious area at
    ! potential ET rate up to available ET
    !***********************************************************************
    ! module subroutine imperv_et(imperv_stor, potet, imperv_evap, sca, avail_et)
    module subroutine imperv_et(this, idx, potet, sca, avail_et)
      ! this%imperv_stor(i), potet(i), this%imperv_evap(i), snowcov_area(i), avail_et
          ! USE PRMS_SRUNOFF, ONLY: Imperv_frac
          implicit none

          ! Arguments
          class(Srunoff), intent(inout) :: this
          integer(i32), intent(in) :: idx
          real(r32), intent(in) :: potet
          real(r32), intent(in) :: sca
          real(r32), intent(in) :: avail_et
          ! real(r32), intent(inout) :: imperv_stor
          ! real(r32), intent(inout) :: imperv_evap

          ! ***********************************************************************
          if (sca < 1.0) then
            if (potet < this%imperv_stor(idx)) then
              this%imperv_evap(idx) = potet * (1.0 - sca)
            else
              this%imperv_evap(idx) = this%imperv_stor(idx) * (1.0 - sca)
            endif

            if (this%imperv_evap(idx) * this%imperv_frac > avail_et) then
              this%imperv_evap(idx) = avail_et / this%imperv_frac
            endif

            this%imperv_stor(idx) = this%imperv_stor(idx) - this%imperv_evap(idx)
          endif
    end subroutine



    module subroutine perv_comp(this, ctl_data, param_data, model_flow, &
                                idx, pptp, ptc, srp)
      ! USE PRMS_SRUNOFF, ONLY: idx, Smidx_coef, Smidx_exp, &
      !    Carea_max, Carea_min, Carea_dif, Contrib_fraction
      ! USE PRMS_MODULE, ONLY: Sroff_flag
      ! USE PRMS_FLOWVARS, ONLY: Soil_moist, Soil_rechr, Soil_rechr_max
      implicit none

      ! Arguments
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Flowvars), intent(in) :: model_flow
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
                soil_moist => model_flow%soil_moist, &
                soil_rechr => model_flow%soil_rechr)

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

        ! TODO: infil and srp update this%infil and this%sra
        this%infil(idx) = this%infil(idx) - srpp
        srp = srp + srpp
      end associate
    end subroutine

end submodule
