submodule (PRMS_SRUNOFF) sm_srunoff

  contains
    module function constructor_Srunoff(ctl_data, model_basin, model_summary) result(this)
      use prms_constants, only: dp, LAKE, INACTIVE
      use UTILS_PRMS, only: open_dyn_param_file, get_first_time !, get_next_time
      implicit none

      type(Srunoff) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary

      ! Local variables
      integer(i32) :: chru
      integer(i32) :: ierr
      integer(i32) :: jj
      real(r64) :: basin_perv = 0.0
      real(r64) :: basin_imperv = 0.0
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
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                cascadegw_flag => ctl_data%cascadegw_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                dprst_transferON_OFF => ctl_data%dprst_transferON_OFF%value, &
                dyn_dprst_flag => ctl_data%dyn_dprst_flag%value, &
                dyn_imperv_flag => ctl_data%dyn_imperv_flag%value, &
                external_transferON_OFF => ctl_data%external_transferON_OFF%value, &
                gwr_transferON_OFF => ctl_data%gwr_transferON_OFF%value, &
                imperv_frac_dynamic => ctl_data%imperv_frac_dynamic%values(1), &
                imperv_stor_dynamic => ctl_data%imperv_stor_dynamic%values(1), &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                lake_transferON_OFF => ctl_data%lake_transferON_OFF%value, &
                outVarON_OFF => ctl_data%outVarON_OFF%value, &
                outVar_names => ctl_data%outVar_names, &
                param_hdl => ctl_data%param_file_hdl, &
                print_debug => ctl_data%print_debug%value, &
                segment_transferON_OFF => ctl_data%segment_transferON_OFF%value, &
                srunoff_module => ctl_data%srunoff_module%values, &
                start_time => ctl_data%start_time%values, &

                nconsumed => model_basin%nconsumed, &
                nhru => model_basin%nhru, &
                nlake => model_basin%nlake, &
                nsegment => model_basin%nsegment, &
                nwateruse => model_basin%nwateruse, &
                active_mask => model_basin%active_mask, &
                basin_area_inv => model_basin%basin_area_inv, &
                hru_area => model_basin%hru_area, &
                hru_type => model_basin%hru_type)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! Parameters
        allocate(this%carea_max(nhru))
        call param_hdl%get_variable('carea_max', this%carea_max)

        allocate(this%hru_percent_imperv(nhru))
        call param_hdl%get_variable('hru_percent_imperv', this%hru_percent_imperv)

        allocate(this%imperv_stor_max(nhru))
        call param_hdl%get_variable('imperv_stor_max', this%imperv_stor_max)

        allocate(this%smidx_coef(nhru))
        call param_hdl%get_variable('smidx_coef', this%smidx_coef)

        allocate(this%smidx_exp(nhru))
        call param_hdl%get_variable('smidx_exp', this%smidx_exp)

        allocate(this%snowinfil_max(nhru))
        call param_hdl%get_variable('snowinfil_max', this%snowinfil_max)

        if (srunoff_module(1)%s == 'srunoff_carea') then
          allocate(this%carea_min(nhru))
          call param_hdl%get_variable('carea_min', this%carea_min)

          allocate(this%carea_dif(nhru))

          where (active_mask)
            this%carea_dif = this%carea_max - this%carea_min
          end where
        endif

        allocate(this%hru_area_imperv(nhru))
        allocate(this%hru_area_perv(nhru))
        allocate(this%hru_frac_perv(nhru))

        ! where (active_mask)
          this%hru_area_imperv = this%hru_percent_imperv * hru_area
          this%hru_area_perv = hru_area - this%hru_area_imperv
          this%hru_frac_perv = 1.0 - this%hru_percent_imperv
          ! this%hru_frac_perv = this%hru_area_perv / hru_area
        ! end where

        ! Depression storage initialization
        if (dprst_flag == 1) then
          call this%dprst_init(ctl_data, model_basin)
        endif

        ! Lake-related updates to pervious and impervious areas
        where (hru_type == LAKE .or. hru_type == INACTIVE)
          ! Fix up the LAKE HRUs
          this%hru_frac_perv = 1.0
          this%hru_area_imperv = 0.0
          this%hru_area_perv = hru_area
        end where

        if (dprst_flag == 1) then
          where (hru_type == LAKE .or. hru_type == INACTIVE)
            this%dprst_area_max = 0.0
          end where
        end if

        basin_perv = sum(dble(this%hru_area_perv)) * basin_area_inv
        basin_imperv = sum(dble(this%hru_area_imperv)) * basin_area_inv

        ! Other variables
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

        allocate(this%basin_apply_sroff)
        allocate(this%basin_contrib_fraction)
        allocate(this%basin_hortonian)
        allocate(this%basin_imperv_evap)
        allocate(this%basin_imperv_stor)
        allocate(this%basin_infil)
        allocate(this%basin_sroff)
        allocate(this%basin_sroffi)
        allocate(this%basin_sroffp)
        allocate(this%basin_hortonian_lakes)
        allocate(this%basin_sroff_down)
        allocate(this%basin_sroff_upslope)

        ! Connect summary variables that need to be output
        if (outVarON_OFF == 1) then
          do jj = 1, outVar_names%size()
            ! TODO: This is where the daily basin values are linked based on
            !       what was requested in basinOutVar_names.
            select case(outVar_names%values(jj)%s)
              case('basin_apply_sroff')
                call model_summary%set_summary_var(jj, this%basin_apply_sroff)
              case('basin_contrib_fraction')
                call model_summary%set_summary_var(jj, this%basin_contrib_fraction)
              case('basin_hortonian')
                call model_summary%set_summary_var(jj, this%basin_hortonian)
              case('basin_imperv_evap')
                call model_summary%set_summary_var(jj, this%basin_imperv_evap)
              case('basin_imperv_stor')
                call model_summary%set_summary_var(jj, this%basin_imperv_stor)
              case('basin_infil')
                call model_summary%set_summary_var(jj, this%basin_infil)
              case('basin_sroff')
                call model_summary%set_summary_var(jj, this%basin_sroff)
              case('basin_sroffi')
                call model_summary%set_summary_var(jj, this%basin_sroffi)
              case('basin_sroffp')
                call model_summary%set_summary_var(jj, this%basin_sroffp)
              case('basin_hortonian_lakes')
                call model_summary%set_summary_var(jj, this%basin_hortonian_lakes)
              case('basin_sroff_down')
                call model_summary%set_summary_var(jj, this%basin_sroff_down)
              case('basin_sroff_upslope')
                call model_summary%set_summary_var(jj, this%basin_sroff_upslope)
              case('basin_dprst_evap')
                call model_summary%set_summary_var(jj, this%basin_dprst_evap)
              case('basin_dprst_seep')
                call model_summary%set_summary_var(jj, this%basin_dprst_seep)
              case('basin_dprst_sroff')
                call model_summary%set_summary_var(jj, this%basin_dprst_sroff)
              case('basin_dprst_volcl')
                call model_summary%set_summary_var(jj, this%basin_dprst_volcl)
              case('basin_dprst_volop')
                call model_summary%set_summary_var(jj, this%basin_dprst_volop)
              case('dprst_area_open')
                call model_summary%set_summary_var(jj, this%dprst_area_open)
              case('dprst_evap_hru')
                call model_summary%set_summary_var(jj, this%dprst_evap_hru)
              case('dprst_insroff_hru')
                call model_summary%set_summary_var(jj, this%dprst_insroff_hru)
              case('dprst_seep_hru')
                call model_summary%set_summary_var(jj, this%dprst_seep_hru)
              case('dprst_sroff_hru')
                call model_summary%set_summary_var(jj, this%dprst_sroff_hru)
              case('dprst_stor_hru')
                call model_summary%set_summary_var(jj, this%dprst_stor_hru)
              case('dprst_vol_clos')
                call model_summary%set_summary_var(jj, this%dprst_vol_clos)
              case('dprst_vol_open')
                call model_summary%set_summary_var(jj, this%dprst_vol_open)
              case('dprst_vol_open_frac')
                call model_summary%set_summary_var(jj, this%dprst_vol_open_frac)
              case('hru_impervevap')
                call model_summary%set_summary_var(jj, this%hru_impervevap)
              case('hru_impervstor')
                call model_summary%set_summary_var(jj, this%hru_impervstor)
              case('hru_sroffi')
                call model_summary%set_summary_var(jj, this%hru_sroffi)
              case('hru_sroffp')
                call model_summary%set_summary_var(jj, this%hru_sroffp)
              case default
                ! pass
            end select
          enddo
        endif


        if (init_vars_from_file == 0) then
          this%basin_contrib_fraction = 0.0_dp
          ! this%basin_dprst_evap = 0.0_dp
          ! this%basin_dprst_seep = 0.0_dp
          ! this%basin_dprst_sroff = 0.0_dp
          ! this%basin_dprst_volcl = 0.0_dp
          ! this%basin_dprst_volop = 0.0_dp
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
          ! this%sri = 0.0_dp
          ! this%srp = 0.0_dp
        endif

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! If requested, open dynamic parameter file(s)
        this%srunoff_updated_soil = .false.
        this%has_dynamic_params = dyn_imperv_flag > 0 .or. dyn_dprst_flag > 0

        if (any([1, 3]==dyn_imperv_flag) .or. any([1, 3]==dyn_dprst_flag)) then
          allocate(this%soil_moist_chg(nhru))
          allocate(this%soil_rechr_chg(nhru))

            ! Open the output unit for summary information
          open(NEWUNIT=this%dyn_output_unit, STATUS='REPLACE', FILE='dyn_srunoff.out')
        end if

        if (any([1, 3]==dyn_imperv_flag)) then
          ! Open the imperv_frac_dynamic file
          call open_dyn_param_file(nhru, this%imperv_frac_unit, ierr, imperv_frac_dynamic%s, 'imperv_frac_dynamic')
          if (ierr /= 0) then
            write(output_unit, *) MODNAME, ' ERROR opening dynamic imperv_frac parameter file.'
            stop
          end if

          this%next_dyn_imperv_frac_date = get_first_time(this%imperv_frac_unit, start_time(1:3))
          write(output_unit, *) ' Dynamic imperv_frac next avail time: ', this%next_dyn_imperv_frac_date

          allocate(this%imperv_frac_chgs(nhru))
        end if

        if (dyn_imperv_flag > 1) then
          ! Open dynamic imperv_stor file
          call open_dyn_param_file(nhru, this%imperv_stor_unit, ierr, imperv_stor_dynamic%s, 'imperv_stor_dynamic')
          if (ierr /= 0) then
            write(output_unit, *) MODNAME, ' ERROR opening dynamic imperv_stor parameter file.'
            stop
          end if

          this%next_dyn_imperv_stor_date = get_first_time(this%imperv_stor_unit, start_time(1:3))
          write(output_unit, *) ' Dynamic imperv_stor next avail time: ', this%next_dyn_imperv_stor_date

          allocate(this%imperv_stor_chgs(nhru))
        end if

      end associate
    end function



    !***********************************************************************
    !     srunoffrun - Computes surface runoff using contributing area
    !                  computations using antecedent soil moisture.
    !***********************************************************************
    module subroutine run_Srunoff(this, ctl_data, model_basin, model_climate, &
                                  model_potet, intcp, snow, model_time)  ! , cascades)
      use prms_constants, only: dp, LAKE, LAND, DNEARZERO, NEARZERO
      implicit none

      class(Srunoff), intent(inout) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      class(Potential_ET), intent(in) :: model_potet
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      type(Time_t), intent(in) :: model_time

      ! Local Variables
      logical :: check_dprst
      integer(i32) :: chru
      integer(i32) :: k

      real(r32) :: availh2o
      real(r32) :: avail_et
      ! real(r64) :: sra
      real(r64) :: sri
      real(r64) :: srp
      real(r64) :: srunoff

      ! real(r64) :: apply_sroff
      ! TODO: Uncomment when cascade converted
      ! real(r64) :: hru_sroff_down
      real(r64) :: runoff

      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                print_debug => ctl_data%print_debug%value, &

                active_hrus => model_basin%active_hrus, &
                active_mask => model_basin%active_mask, &
                basin_area_inv => model_basin%basin_area_inv, &
                hru_area => model_basin%hru_area, &
                hru_area_dble => model_basin%hru_area_dble, &
                hru_route_order => model_basin%hru_route_order, &
                hru_type => model_basin%hru_type, &

                pkwater_equiv => model_climate%pkwater_equiv, &
                soil_moist => model_climate%soil_moist, &
                soil_moist_max => model_climate%soil_moist_max, &
                soil_rechr => model_climate%soil_rechr, &
                soil_rechr_max => model_climate%soil_rechr_max, &

                hru_intcpevap => intcp%hru_intcpevap, &
                intcp_changeover => intcp%intcp_changeover, &
                ! intcp_stor => intcp%intcp_stor, &
                ! net_apply => intcp%net_apply, &
                net_ppt => intcp%net_ppt, &
                net_rain => intcp%net_rain, &
                net_snow => intcp%net_snow, &

                pptmix_nopack => snow%pptmix_nopack, &
                snowcov_area => snow%snowcov_area, &
                snowmelt => snow%snowmelt, &
                snow_evap => snow%snow_evap, &

                potet => model_potet%potet, &

                nowtime => model_time%Nowtime)

        ! Dynamic parameter read
        if (this%has_dynamic_params) then
          call this%read_dyn_params(ctl_data, model_basin, model_time, model_climate)
        end if

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
        ! this%infil = 0.0
        ! this%imperv_evap = 0.0
        this%contrib_fraction = 0.0
        this%hru_impervevap = 0.0
        this%hru_sroffi = 0.0
        this%hru_sroffp = 0.0
        check_dprst = .false.

        do k=1, active_hrus
          chru = hru_route_order(k)
          runoff = 0.0_dp

          if (hru_type(chru) == LAKE) then
            ! HRU is a lake
            ! Eventually add code for lake area less than hru_area that includes
            ! soil_moist for fraction of hru_area that is dry bank.
            if (this%infil(chru) + this%sroff(chru) + this%imperv_stor(chru) + this%imperv_evap(chru) > 0.0) then
              write(output_unit, *) MODNAME, '%run() lake ERROR', nowtime, chru, this%infil(chru), this%sroff(chru), this%imperv_stor(chru), this%imperv_evap(chru), chru
            endif

            if (cascade_flag == 1) then
              this%hortonian_lakes(chru) = this%upslope_hortonian(chru)
            endif

            cycle
          endif

          avail_et = potet(chru) - snow_evap(chru) - hru_intcpevap(chru)
          sri = 0.0_dp
          srp = 0.0_dp
          srunoff = 0.0_dp

          ! this%hru_sroffi(chru) = 0.0
          this%infil(chru) = 0.0
          this%imperv_evap(chru) = 0.0
          ! this%hru_impervevap(chru) = 0.0

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

          availh2o = intcp_changeover(chru) + net_rain(chru)
          call this%compute_infil(ctl_data, chru, hru_type(chru), net_ppt(chru), &
                                  availh2o, net_snow(chru), pkwater_equiv(chru), &
                                  pptmix_nopack(chru), snowmelt(chru), &
                                  soil_moist(chru), soil_moist_max(chru), soil_rechr(chru), soil_rechr_max(chru), &
                                  this%contrib_fraction(chru), this%imperv_stor(chru), &
                                  this%infil(chru), sri, srp)

          if (dprst_flag == 1) then
            this%dprst_in(chru) = 0.0_dp
            check_dprst = .false.

            if (this%dprst_area_max(chru) > 0.0) then
              check_dprst = .true.
              ! ****** Compute the depression storage component
              ! Only call if total depression surface area for each HRU is > 0.0
              call this%dprst_comp(ctl_data, chru, model_basin, model_time, &
                                   pkwater_equiv(chru), potet(chru), availh2o, &
                                   net_snow(chru), pptmix_nopack(chru), &
                                   snowmelt(chru), snowcov_area(chru), avail_et, &
                                   sri, srp)

              runoff = runoff + this%dprst_sroff_hru(chru) * hru_area_dble(chru)
            endif
          endif
          ! **********************************************************

          if (hru_type(chru) == LAND) then
            ! ******Compute runoff for pervious and impervious area, and depression storage area
            runoff = runoff + srp * dble(this%hru_area_perv(chru)) + sri * dble(this%hru_area_imperv(chru))
            srunoff = runoff / hru_area_dble(chru)

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

            this%hru_sroffp(chru) = sngl(srp) * this%hru_frac_perv(chru)
            this%basin_sroffp = this%basin_sroffp + srp * dble(this%hru_area_perv(chru))
          endif

          !******Compute evaporation from impervious area
          if (this%hru_area_imperv(chru) > 0.0) then
            ! NOTE: imperv_stor can get incredibly small (> e-35) which causes
            !       floating point underflow (SIGFPE).
            ! if (this%imperv_stor(chru) > 0.0) then
            if (this%imperv_stor(chru) > NEARZERO) then
              call this%imperv_et(chru, potet(chru), snowcov_area(chru), avail_et, &
                                  this%imperv_evap(chru), this%imperv_stor(chru))

              this%hru_impervevap(chru) = this%imperv_evap(chru) * this%hru_percent_imperv(chru)
              avail_et = avail_et - this%hru_impervevap(chru)

              if (avail_et < 0.0) then
                this%hru_impervevap(chru) = max(0.0, this%hru_impervevap(chru) + avail_et)

                this%imperv_evap(chru) = this%hru_impervevap(chru) / this%hru_percent_imperv(chru)
                this%imperv_stor(chru) = this%imperv_stor(chru) - avail_et / this%hru_percent_imperv(chru)
                avail_et = 0.0
              endif

              ! this%basin_imperv_evap = this%basin_imperv_evap + this%hru_impervevap(chru) * hru_area(chru)
              this%hru_impervstor(chru) = this%imperv_stor(chru) * this%hru_percent_imperv(chru)
              ! this%basin_imperv_stor = this%basin_imperv_stor + this%imperv_stor(chru) * this%hru_area_imperv(chru)
            endif

            this%hru_sroffi(chru) = sngl(sri) * this%hru_percent_imperv(chru)
            this%basin_sroffi = this%basin_sroffi + sri * dble(this%hru_area_imperv(chru))
          endif

          if (check_dprst) then
            this%dprst_stor_hru(chru) = (this%dprst_vol_open(chru) + this%dprst_vol_clos(chru)) / hru_area_dble(chru)
          endif

          this%sroff(chru) = sngl(srunoff)
          this%hortonian_flow(chru) = sngl(srunoff)
        enddo

        ! ****** Compute basin weighted averages (to units of inches/dt)
        ! rsr, should be land_area???
        this%basin_contrib_fraction = sum(dble(this%contrib_fraction * this%hru_area_perv), mask=active_mask) * basin_area_inv
        this%basin_hortonian = sum(dble(this%hortonian_flow * hru_area), mask=active_mask) * basin_area_inv
        this%basin_infil = sum(dble(this%infil * this%hru_area_perv), mask=active_mask) * basin_area_inv
        ! this%basin_sroff = sum(dble(this%sroff * hru_area), mask=active_mask) * basin_area_inv

        this%basin_imperv_evap = sum(dble(this%hru_impervevap * hru_area), mask=active_mask) * basin_area_inv
        this%basin_imperv_stor = sum(dble(this%imperv_stor * this%hru_area_imperv), mask=active_mask) * basin_area_inv

        if (cascade_flag == 1) then
          ! this%basin_potet = sum(dble(this%potet * hru_area), mask=active_mask) * basin_area_inv
          this%basin_hortonian_lakes = sum(this%hortonian_lakes * hru_area_dble, mask=active_mask) * basin_area_inv
        endif

        ! this%basin_imperv_evap = this%basin_imperv_evap * basin_area_inv
        ! this%basin_imperv_stor = this%basin_imperv_stor * basin_area_inv
        this%basin_sroffp = this%basin_sroffp * basin_area_inv
        this%basin_sroffi = this%basin_sroffi * basin_area_inv
        this%basin_sroff = this%basin_sroffp + this%basin_sroffi

        if (dprst_flag == 1) then
          this%basin_dprst_volop = sum(this%dprst_vol_open, mask=active_mask) * basin_area_inv
          this%basin_dprst_volcl = sum(this%dprst_vol_clos, mask=active_mask) * basin_area_inv
          this%basin_dprst_evap = sum(dble(this%dprst_evap_hru * hru_area), mask=active_mask) * basin_area_inv
          this%basin_dprst_seep = sum(this%dprst_seep_hru * hru_area_dble, mask=active_mask) * basin_area_inv
          this%basin_dprst_sroff = sum(this%dprst_sroff_hru * hru_area_dble, mask=active_mask) * basin_area_inv

          ! Add basin_dprst_sroff to basin_sroff
          this%basin_sroff = this%basin_sroff + this%basin_dprst_sroff
        endif

        if (cascade_flag == 1) then
          this%basin_sroff_down = this%basin_sroff_down * basin_area_inv
          this%basin_sroff_upslope = this%basin_sroff_upslope * basin_area_inv
        endif
      end associate
    end subroutine


    module subroutine cleanup_Srunoff(this)
      class(Srunoff) :: this
        !! Srunoff class

      logical :: is_opened

      call close_if_open(this%dyn_output_unit)
      call close_if_open(this%imperv_frac_unit)
      call close_if_open(this%imperv_stor_unit)
      call close_if_open(this%dprst_frac_unit)
      call close_if_open(this%dprst_depth_unit)
      ! inquire(UNIT=this%dyn_output_unit, OPENED=is_opened)
      ! if (is_opened) then
      !   close(this%dyn_output_unit)
      ! end if

      ! if (any([1, 3]==dyn_imperv_flag) .or. any([1, 3]==dyn_dprst_flag)) then
      !   deallocate(this%soil_moist_chg)
      !   deallocate(this%soil_rechr_chg)

      !   ! TODO: PAN - need routine to close any netcdf files
      !   close(this%dyn_output_unit)
      ! end if

      ! if (dyn_imperv_flag==1) then
      !   close(this%imperv_frac_unit)
      ! else if (dyn_imperv_flag==2) then
      !   close(this%imperv_stor_unit)
      ! else if (dyn_imperv_flag==3) then
      !   close(this%imperv_frac_unit)
      !   close(this%imperv_stor_unit)
      ! end if
    end subroutine

    module subroutine close_if_open(unit)
      implicit none

      integer(i32), intent(in) :: unit

      logical :: is_opened

      inquire(UNIT=unit, OPENED=is_opened)
      if (is_opened) then
        close(unit)
      end if
    end subroutine

    !***********************************************************************
    ! Initialize depression storage area hydrology
    !***********************************************************************
    module subroutine dprst_init(this, ctl_data, model_basin)
      use prms_constants, only: dp, NEARZERO
      use UTILS_PRMS, only: open_dyn_param_file, get_first_time, get_next_time
      implicit none

      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
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
      ! active_hrus, basin_area_inv, hru_route_order

      ! Local Variables
      integer(i32) :: chru
      integer(i32) :: ierr
      integer(i32) :: j

      real(r64) :: basin_dprst = 0.0

      ! ***********************************************************************
      associate(dprst_flag => ctl_data%dprst_flag%value, &
                dprst_depth_dynamic => ctl_data%dprst_depth_dynamic%values(1), &
                dprst_frac_dynamic => ctl_data%dprst_frac_dynamic%values(1), &
                dyn_dprst_flag => ctl_data%dyn_dprst_flag%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                param_hdl => ctl_data%param_file_hdl, &
                print_debug => ctl_data%print_debug%value, &
                start_time => ctl_data%start_time%values, &

                nhru => model_basin%nhru, &
                active_hrus => model_basin%active_hrus, &
                active_mask => model_basin%active_mask, &
                basin_area_inv => model_basin%basin_area_inv, &
                hru_area => model_basin%hru_area, &
                hru_route_order => model_basin%hru_route_order)

        ! Parameters
        allocate(this%dprst_depth_avg(nhru))
        call param_hdl%get_variable('dprst_depth_avg', this%dprst_depth_avg)

        allocate(this%dprst_et_coef(nhru))
        call param_hdl%get_variable('dprst_et_coef', this%dprst_et_coef)

        allocate(this%dprst_flow_coef(nhru))
        call param_hdl%get_variable('dprst_flow_coef', this%dprst_flow_coef)

        allocate(this%dprst_frac(nhru))
        call param_hdl%get_variable('dprst_frac', this%dprst_frac)

        allocate(this%dprst_frac_init(nhru))

        if (any([0, 2, 7]==init_vars_from_file)) then
          call param_hdl%get_variable('dprst_frac_init', this%dprst_frac_init)
        else
          this%dprst_frac_init = 0.0
        end if

        allocate(this%dprst_frac_open(nhru))
        call param_hdl%get_variable('dprst_frac_open', this%dprst_frac_open)

        allocate(this%sro_to_dprst_imperv(nhru))
        call param_hdl%get_variable('sro_to_dprst_imperv', this%sro_to_dprst_imperv)

        allocate(this%sro_to_dprst_perv(nhru))
        call param_hdl%get_variable('sro_to_dprst_perv', this%sro_to_dprst_perv)

        allocate(this%dprst_area_max(nhru))
        this%dprst_area_max = this%dprst_frac * hru_area

        where (active_mask)
          this%hru_area_perv = this%hru_area_perv - this%dprst_area_max

          ! Recompute hru_frac_perv to reflect the depression storage area
          this%hru_frac_perv = this%hru_area_perv / hru_area
        end where

        allocate(this%dprst_area_clos_max(nhru))
        this%dprst_area_clos_max = 0.0

        allocate(this%dprst_area_open_max(nhru))
        this%dprst_area_open_max = 0.0

        allocate(this%dprst_frac_clos(nhru))
        this%dprst_frac_clos = 0.0

        where (active_mask .and. this%dprst_area_max > 0.0)
          this%dprst_area_open_max = this%dprst_area_max * this%dprst_frac_open
          this%dprst_area_clos_max = this%dprst_area_max - this%dprst_area_open_max
          this%dprst_frac_clos = 1.0 - this%dprst_frac_open
        end where

        do j=1, active_hrus
          chru = hru_route_order(j)

          if (this%hru_percent_imperv(chru) + this%dprst_frac(chru) > 0.999) then
            write(output_unit, *) MODNAME, 'dprst_init() ERROR: impervious plus depression fraction > 0.999 for HRU:', chru
            write(output_unit, *) MODNAME, 'dprst_init()        value:', this%hru_percent_imperv(chru) + this%dprst_frac(chru)
            ! TODO: What should happen when error occurs?
          endif
        end do

        ! do j=1, active_hrus
        !   chru = hru_route_order(j)

        !   if (this%dprst_area_max(chru) > 0.0) then
        !     this%dprst_area_open_max(chru) = this%dprst_area_max(chru) * this%dprst_frac_open(chru)
        !     this%dprst_area_clos_max(chru) = this%dprst_area_max(chru) - this%dprst_area_open_max(chru)
        !     this%dprst_frac_clos(chru) = 1.0 - this%dprst_frac_open(chru)

        !     if (this%hru_percent_imperv(chru) + this%dprst_frac(chru) > 0.999) then
        !        print *, 'ERROR, impervious plus depression fraction > 0.999 for HRU:', chru
        !        print *, '       value:', this%hru_percent_imperv(chru) + this%dprst_frac(chru)
        !        ! TODO: What should happen when error occurs?
        !     endif
        !   endif

        !   ! NOTE: originally in basin.f90 only; removed for now
        !   ! basin_dprst = basin_dprst + dble(dprst_area_max(chru))
        ! enddo

        this%has_closed_dprst = any(this%dprst_area_clos_max > 0.0)
        this%has_open_dprst = any(this%dprst_area_open_max > 0.0)

        ! write(output_unit, *) '  dprst_area_open_max: ', this%dprst_area_open_max
        ! write(output_unit, *) '  dprst_area_clos_max: ', this%dprst_area_clos_max
        ! write(output_unit, *) '  dprst_frac_clos: ', this%dprst_frac_clos
        ! write(output_unit, *) '  dprst_frac_open: ', this%dprst_frac_open
        ! write(output_unit, *) '  has_open_dprst: ', this%has_open_dprst
        ! write(output_unit, *) '  has_closed_dprst: ', this%has_closed_dprst

        allocate(this%dprst_seep_rate_open(nhru))
        allocate(this%op_flow_thres(nhru))
        allocate(this%va_open_exp(nhru))

        if (this%has_open_dprst) then
          call param_hdl%get_variable('dprst_seep_rate_open', this%dprst_seep_rate_open)
          call param_hdl%get_variable('op_flow_thres', this%op_flow_thres)
          call param_hdl%get_variable('va_open_exp', this%va_open_exp)
        else
          this%dprst_seep_rate_open = 0.0
          this%op_flow_thres = 0.0
          this%va_open_exp = 0.0
        end if

        allocate(this%dprst_seep_rate_clos(nhru))
        allocate(this%va_clos_exp(nhru))

        if (this%has_closed_dprst) then
          call param_hdl%get_variable('dprst_seep_rate_clos', this%dprst_seep_rate_clos)
          call param_hdl%get_variable('va_clos_exp', this%va_clos_exp)
        else
          this%dprst_seep_rate_clos = 0.0
          this%va_clos_exp = 0.0
        end if

        ! Other variables
        allocate(this%basin_dprst_evap)
        allocate(this%basin_dprst_seep)
        allocate(this%basin_dprst_sroff)
        allocate(this%basin_dprst_volcl)
        allocate(this%basin_dprst_volop)

        allocate(this%dprst_area_clos(nhru))
        allocate(this%dprst_area_open(nhru))
        allocate(this%dprst_evap_hru(nhru))
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

        ! Compute total area of depressions in the model
        basin_dprst = sum(dble(this%dprst_area_max))

        this%basin_dprst_evap = 0.0_dp
        this%basin_dprst_seep = 0.0_dp
        this%basin_dprst_sroff = 0.0_dp

        this%basin_dprst_volcl = 0.0_dp
        this%basin_dprst_volop = 0.0_dp
        this%dprst_area_clos = 0.0_dp
        this%dprst_area_open = 0.0_dp
        this%dprst_evap_hru = 0.0_dp
        this%dprst_in = 0.0_dp
        this%dprst_insroff_hru = 0.0_dp
        this%dprst_seep_hru = 0.0_dp
        this%dprst_sroff_hru = 0.0_dp
        this%dprst_stor_hru = 0.0_dp
        this%dprst_vol_clos = 0.0_dp
        this%dprst_vol_clos_frac = 0.0
        this%dprst_vol_clos_max = 0.0_dp
        this%dprst_vol_frac = 0.0
        this%dprst_vol_open = 0.0_dp
        this%dprst_vol_open_frac = 0.0
        this%dprst_vol_open_max = 0.0_dp
        this%dprst_vol_thres_open = 0.0_dp

        where (this%has_closed_dprst .and. active_mask .and. this%dprst_area_max > 0.0)
          this%dprst_vol_clos_max = dble(this%dprst_area_clos_max * this%dprst_depth_avg)
        end where

        where (this%has_open_dprst .and. active_mask .and. this%dprst_area_max > 0.0)
          this%dprst_vol_open_max = dble(this%dprst_area_open_max * this%dprst_depth_avg)
        end where

        ! Calculate the initial open and closed depression storage volume:
        if (any([0, 2, 7]==init_vars_from_file)) then
          where (this%has_closed_dprst .and. active_mask .and. this%dprst_area_max > 0.0)
            this%dprst_vol_clos = dble(this%dprst_frac_init) * this%dprst_vol_clos_max
          end where

          where (this%has_open_dprst .and. active_mask .and. this%dprst_area_max > 0.0)
            this%dprst_vol_open = dble(this%dprst_frac_init) * this%dprst_vol_open_max
          end where
        endif

        where (active_mask .and. this%dprst_area_max > 0.0)
          this%dprst_vol_thres_open = dble(this%op_flow_thres) * this%dprst_vol_open_max
        end where

        ! Calculate depression surface area
        where (this%dprst_vol_open > 0.0_dp .and. active_mask .and. this%dprst_area_max > 0.0)
          ! Open depression surface area for each HRU:
          this%dprst_area_open = this%depression_surface_area(this%dprst_vol_open, &
                                                              this%dprst_vol_open_max, &
                                                              this%dprst_area_open_max, &
                                                              this%va_open_exp)
        end where

        where (this%dprst_vol_clos > 0.0_dp .and. active_mask .and. this%dprst_area_max > 0.0)
          ! Closed depression surface area for each HRU:
          this%dprst_area_clos = this%depression_surface_area(this%dprst_vol_clos, &
                                                              this%dprst_vol_clos_max, &
                                                              this%dprst_area_clos_max, &
                                                              this%va_clos_exp)
        end where

        where (this%dprst_vol_clos_max > 0.0_dp)
          this%dprst_vol_clos_frac = sngl(this%dprst_vol_clos / this%dprst_vol_clos_max)
        end where

        where (this%dprst_vol_open_max > 0.0_dp)
          this%dprst_vol_open_frac = sngl(this%dprst_vol_open / this%dprst_vol_open_max)
        end where

        where (active_mask .and. this%dprst_area_max > 0.0)
          this%dprst_vol_frac = sngl((this%dprst_vol_open + this%dprst_vol_clos) / &
                                     (this%dprst_vol_open_max + this%dprst_vol_clos_max))
          this%dprst_stor_hru = (this%dprst_vol_open + this%dprst_vol_clos) / dble(hru_area)
        end where

        ! do j=1, active_hrus
        !   chru = hru_route_order(j)

        !   if (this%dprst_area_max(chru) > 0.0) then
        !     ! calculate open and closed volumes (acre-inches) of depression storage by HRU
        !     ! dprst_area_open_max is the maximum open depression area (acres) that can generate surface runoff:
        !     if (this%has_closed_dprst) then
        !       this%dprst_vol_clos_max(chru) = dble(this%dprst_area_clos_max(chru) * this%dprst_depth_avg(chru))
        !     endif

        !     if (this%has_open_dprst) then
        !       this%dprst_vol_open_max(chru) = dble(this%dprst_area_open_max(chru) * this%dprst_depth_avg(chru))
        !     endif

        !     ! calculate the initial open and closed depression storage volume:
        !     ! if (init_vars_from_file == 0 .or. init_vars_from_file == 2 .or. init_vars_from_file == 7) then
        !     if (any([0, 2, 7]==init_vars_from_file)) then
        !       if (this%has_open_dprst) then
        !         this%dprst_vol_open(chru) = dble(this%dprst_frac_init(chru)) * this%dprst_vol_open_max(chru)
        !       end if

        !       if (this%has_closed_dprst) then
        !         this%dprst_vol_clos(chru) = dble(this%dprst_frac_init(chru)) * this%dprst_vol_clos_max(chru)
        !       end if
        !     endif

        !     ! Threshold volume is calculated as the % of maximum open
        !     ! Depression storage above which flow occurs *  total open depression storage volume
        !     this%dprst_vol_thres_open(chru) = dble(this%op_flow_thres(chru)) * this%dprst_vol_open_max(chru)

        !     ! Initial open and closed storage volume as fraction of total open and closed storage volume
        !     if (this%dprst_vol_open(chru) > 0.0_dp) then
        !       ! Open depression surface area for each HRU:
        !       this%dprst_area_open(chru) = this%depression_surface_area(this%dprst_vol_open(chru), &
        !                                                                 this%dprst_vol_open_max(chru), &
        !                                                                 this%dprst_area_open_max(chru), &
        !                                                                 this%va_open_exp(chru))
        !     endif

        !     if (this%dprst_vol_clos(chru) > 0.0_dp) then
        !       ! Closed depression surface area for each HRU:
        !       this%dprst_area_clos(chru) = this%depression_surface_area(this%dprst_vol_clos(chru), &
        !                                                                this%dprst_vol_clos_max(chru), &
        !                                                                this%dprst_area_clos_max(chru), &
        !                                                                this%va_clos_exp(chru))
        !     endif

        !     if (this%dprst_vol_open_max(chru) > 0.0) then
        !       this%dprst_vol_open_frac(chru) = sngl(this%dprst_vol_open(chru) / this%dprst_vol_open_max(chru))
        !     endif

        !     if (this%dprst_vol_clos_max(chru) > 0.0) then
        !       this%dprst_vol_clos_frac(chru) = sngl(this%dprst_vol_clos(chru) / this%dprst_vol_clos_max(chru))
        !     endif

        !     this%dprst_vol_frac(chru) = sngl((this%dprst_vol_open(chru) + this%dprst_vol_clos(chru)) / &
        !                                      (this%dprst_vol_open_max(chru) + this%dprst_vol_clos_max(chru)))
        !     this%dprst_stor_hru(chru) = (this%dprst_vol_open(chru) + this%dprst_vol_clos(chru)) / dble(hru_area(chru))
        !   endif
        ! enddo

        ! this%basin_gvr_stor_frac = sum(dble(this%slow_stor / this%pref_flow_thrsh * hru_area), mask=(this%pref_flow_thrsh > 0.0)) * basin_area_inv
        this%basin_dprst_volop = sum(this%dprst_vol_open, mask=(active_mask)) * basin_area_inv
        this%basin_dprst_volcl = sum(this%dprst_vol_clos, mask=(active_mask)) * basin_area_inv

        ! write(output_unit, *) '  dprst_vol_frac: ', this%dprst_vol_frac
        ! write(output_unit, *) '  basin_dprst_volop: ', this%basin_dprst_volop
        ! write(output_unit, *) '  basin_dprst_volcl: ', this%basin_dprst_volcl

        if (any([0, 2, 7]==init_vars_from_file)) then
          deallocate(this%dprst_frac_init)
        end if

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! If requested, open dynamic parameter file(s)
        if (any([1, 3]==dyn_dprst_flag)) then
          ! Open the dprst_frac_dynamic file
          call open_dyn_param_file(nhru, this%dprst_frac_unit, ierr, dprst_frac_dynamic%s, 'dprst_frac_dynamic')
          if (ierr /= 0) then
            write(output_unit, *) MODNAME, '%dprst_init() ERROR opening dynamic dprst_frac parameter file.'
            stop
          end if

          this%next_dyn_dprst_frac_date = get_first_time(this%dprst_frac_unit, start_time(1:3))
          write(output_unit, *) MODNAME, '%dprst_init() Dynamic dprst_frac next avail time: ', this%next_dyn_dprst_frac_date

          allocate(this%dprst_frac_chgs(nhru))
          ! NOTE: prms5 has dprst_frac_flag = 1
        end if

        if (any([2, 3]==dyn_dprst_flag)) then
          ! Open the dprst_frac_dynamic file
          call open_dyn_param_file(nhru, this%dprst_depth_unit, ierr, dprst_depth_dynamic%s, 'dprst_depth_dynamic')
          if (ierr /= 0) then
            write(output_unit, *) MODNAME, '%dprst_init() ERROR opening dynamic dprst_depth_avg parameter file.'
            stop
          end if

          this%next_dyn_dprst_depth_date = get_first_time(this%dprst_depth_unit, start_time(1:3))
          write(output_unit, *) MODNAME, '%dprst_init() Dynamic dprst_depth_avg next avail time: ', this%next_dyn_dprst_depth_date

          allocate(this%dprst_depth_chgs(nhru))
          ! NOTE: prms5 has dprst_depth_flag = 1
        end if

      end associate
    end subroutine


    !***********************************************************************
    ! fill soil to soil_moist_max, if more than capacity restrict
    ! infiltration by snowinfil_max, with excess added to runoff
    !***********************************************************************
    module subroutine check_capacity(this, idx, soil_moist, soil_moist_max, infil, srp)
      implicit none

      class(Srunoff), intent(in) :: this
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: srp

      ! Local Variables
      real(r32) :: capacity
      real(r32) :: excess

      !***********************************************************************
      ! print *, '-- check_capacity()'
      capacity = soil_moist_max - soil_moist
      excess = infil - capacity

      if (excess > this%snowinfil_max(idx)) then
        srp = srp + dble(excess - this%snowinfil_max(idx))
        infil = this%snowinfil_max(idx) + capacity
      endif
    end subroutine



    !***********************************************************************
    ! Compute infiltration
    !***********************************************************************
    module subroutine compute_infil(this, ctl_data, idx, hru_type, net_ppt, net_rain, &
                                    net_snow, pkwater_equiv, pptmix_nopack, &
                                    snowmelt, soil_moist, soil_moist_max, soil_rechr, soil_rechr_max, &
                                    contrib_frac, imperv_stor, infil, sri, srp)
      use prms_constants, only: dp, DNEARZERO, NEARZERO, LAND
      implicit none

      class(Srunoff), intent(in) :: this
      type(Control), intent(in) :: ctl_data
      integer(i32), intent(in) :: idx
      integer(i32), intent(in) :: hru_type
      real(r32), intent(in) :: net_ppt
        !! Net precipitation
      real(r32), intent(in) :: net_rain
        !! Net liquid rain
      real(r32), intent(in) :: net_snow
        !! Net snow
      real(r64), intent(in) :: pkwater_equiv
        !! Snow packwater equivalent
      logical, intent(in) :: pptmix_nopack
      real(r32), intent(in) :: snowmelt
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(inout) :: contrib_frac
      real(r32), intent(inout) :: imperv_stor
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: sri
      real(r64), intent(inout) :: srp
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

      ! reads: this%upslope_hortonian, this%imperv_stor_max
      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value)
        ! Compute runoff from cascading Hortonian flow
        if (cascade_flag == 1) then
          avail_water = sngl(this%upslope_hortonian(idx))

          if (avail_water > 0.0) then
            infil = avail_water

            if (hru_type == LAND) then
              call this%perv_comp(ctl_data, idx, soil_moist, soil_rechr, soil_rechr_max, &
                                  avail_water, avail_water, contrib_frac, infil, srp)
            endif
          endif
        else
          avail_water = 0.0
        endif

        ! ***** If rain/snow event with no antecedent snowpack, compute the runoff
        ! ***** from the rain first and then proceed with the snowmelt computations.
        if (pptmix_nopack) then
          avail_water = avail_water + net_rain
          infil = infil + net_rain

          if (hru_type == LAND) then
            call this%perv_comp(ctl_data, idx, soil_moist, soil_rechr, soil_rechr_max, &
                                net_rain, net_rain, contrib_frac, infil, srp)
          endif
        endif

        ! ***** If precipitation on snowpack, all water available to the surface
        ! ***** is considered to be snowmelt, and the snowmelt infiltration
        ! ***** procedure is used.  If there is no snowpack and no precip,
        ! ***** then check for melt from last of snowpack.  If rain/snow mix
        ! ***** with no antecedent snowpack, compute snowmelt portion of runoff.
        if (snowmelt > 0.0) then
          avail_water = avail_water + snowmelt
          infil = infil + snowmelt

          if (hru_type == LAND) then
            if (pkwater_equiv > 0.0_dp .or. net_ppt - net_snow < NEARZERO) then
              ! ****** Pervious area computations
              call this%check_capacity(idx, soil_moist, soil_moist_max, infil, srp)
            else
              ! ****** Snowmelt occurred and depleted the snowpack
              call this%perv_comp(ctl_data, idx, soil_moist, soil_rechr, soil_rechr_max, &
                                  snowmelt, net_ppt, contrib_frac, infil, srp)
            endif
          endif

        ! ****** There was no snowmelt but a snowpack may exist.  If there is
        ! ****** no snowpack then check for rain on a snowfree HRU.
        elseif (pkwater_equiv < DNEARZERO) then
          ! If no snowmelt and no snowpack but there was net snow then
          ! snowpack was small and was lost to sublimation.
          if (net_snow < NEARZERO .and. net_rain > 0.0) then
            ! no snow, some rain
            avail_water = avail_water + net_rain
            infil = infil + net_rain

            if (hru_type == LAND) then
              call this%perv_comp(ctl_data, idx, soil_moist, soil_rechr, soil_rechr_max, &
                                  net_rain, net_rain, contrib_frac, infil, srp)
            endif
          endif
        elseif (infil > 0.0) then
          ! ***** Snowpack exists, check to see if infil exceeds maximum daily
          ! ***** snowmelt infiltration rate. infil results from rain snow mix
          ! ***** on a snowfree surface.
          if (hru_type == LAND) then
            call this%check_capacity(idx, soil_moist, soil_moist_max, infil, srp)
          endif
        endif

        ! ****** Impervious area computations
        if (this%hru_area_imperv(idx) > 0.0) then
          imperv_stor = imperv_stor + avail_water

          if (hru_type == LAND) then
            if (imperv_stor > this%imperv_stor_max(idx)) then
              sri = dble(imperv_stor - this%imperv_stor_max(idx))
              imperv_stor = this%imperv_stor_max(idx)
            endif
          endif
        endif
      end associate
    end subroutine


    !***********************************************************************
    ! Compute depression storage area hydrology
    !***********************************************************************
    module subroutine dprst_comp(this, ctl_data, idx, model_basin, model_time, pkwater_equiv, &
                                 potet, net_rain, net_snow, pptmix_nopack, snowmelt, &
                                 snowcov_area, avail_et, sri, srp)
      use prms_constants, only: dp, DNEARZERO, NEARZERO
      implicit none

      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      integer(i32), intent(in) :: idx
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      real(r64), intent(in) :: pkwater_equiv
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: net_rain
      real(r32), intent(in) :: net_snow
      logical, intent(in) :: pptmix_nopack
      real(r32), intent(in) :: snowmelt
      real(r32), intent(in) :: snowcov_area
      real(r32), intent(inout) :: avail_et
      real(r64), intent(inout) :: sri
      real(r64), intent(inout) :: srp

      ! Local Variables
      real(r64) :: dprst_avail_et
      real(r64) :: dprst_sri
      real(r64) :: dprst_sri_clos
      real(r64) :: dprst_sri_open
      real(r64) :: dprst_srp
      real(r64) :: dprst_srp_clos
      real(r64) :: dprst_srp_open
      real(r64) :: inflow
      real(r64) :: tmp
      real(r64) :: unsatisfied_et

      real(r64) :: dprst_evap_clos
      real(r64) :: dprst_evap_open
      real(r64) :: seep_open
      real(r64) :: seep_clos
      real(r64) :: tmp1

      ! this
      ! upslope_hortonian

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

                hru_area => model_basin%hru_area, &
                hru_area_dble => model_basin%hru_area_dble, &

                nowtime => model_time%Nowtime)

        ! print *, '-- dprst_comp()'

        ! Add the hortonian flow to the depression storage volumes:
        if (cascade_flag == 1) then
          inflow = this%upslope_hortonian(idx)
        else
          inflow = 0.0_dp
        endif

        if (pptmix_nopack) then
          inflow = inflow + dble(net_rain)
        endif

        ! **** If precipitation on snowpack all water available to the surface
        ! **** is considered to be snowmelt. If there is no snowpack and
        ! **** no precip,then check for melt from last of snowpack. If rain/snow
        ! **** mix with no antecedent snowpack, compute snowmelt portion of runoff.
        if (snowmelt > 0.0) then
          inflow = inflow + dble(snowmelt)
        elseif (pkwater_equiv < DNEARZERO) then
          ! ****** There was no snowmelt but a snowpack may exist.  If there is
          ! ****** no snowpack then check for rain on a snowfree HRU.
          if (net_snow < NEARZERO .and. net_rain > 0.0) then
            ! If no snowmelt and no snowpack but there was net snow then
            ! snowpack was small and was lost to sublimation.
            inflow = inflow + dble(net_rain)
          endif
        endif

        this%dprst_in(idx) = 0.0_dp

            ! if (idx == 11) then
            !   print *, '(S) ', this%dprst_vol_open(idx), pptmix_nopack(idx), inflow, net_rain(idx)
            ! endif

        ! ******* Block 1 *******
        if (this%dprst_area_open_max(idx) > 0.0) then
          this%dprst_in(idx) = inflow * dble(this%dprst_area_open_max(idx))  ! inch-acres
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + this%dprst_in(idx)
        endif

        if (this%dprst_area_clos_max(idx) > 0.0) then
          tmp1 = inflow * dble(this%dprst_area_clos_max(idx))  ! inch-acres
          this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + tmp1
          this%dprst_in(idx) = this%dprst_in(idx) + tmp1
        endif

        this%dprst_in(idx) = this%dprst_in(idx) / hru_area_dble(idx)  ! inches over HRU

        ! ******* Block 2 *******
        ! Add any pervious surface runoff fraction to depressions
        dprst_srp = 0.0_dp
        dprst_sri = 0.0_dp

        ! Pervious surface runoff
        if (srp > 0.0_dp) then
          tmp = srp * dble(this%sro_to_dprst_perv(idx))

          if (this%dprst_area_open_max(idx) > 0.0) then
            dprst_srp_open = tmp * dble(this%dprst_frac_open(idx))
            dprst_srp = dprst_srp_open
            this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + dprst_srp_open * dble(this%hru_frac_perv(idx)) * hru_area_dble(idx) ! acre-inches
          endif

          if (this%dprst_area_clos_max(idx) > 0.0) then
            dprst_srp_clos = tmp * dble(this%dprst_frac_clos(idx))
            dprst_srp = dprst_srp + dprst_srp_clos
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + dprst_srp_clos * dble(this%hru_frac_perv(idx)) * hru_area_dble(idx)
          endif

          srp = srp - dprst_srp

          if (srp < 0.0_dp) then
            if (srp < -DNEARZERO) then
              write(*, 9004) MODNAME, 'WARNING: ', nowtime(1:3), idx, ' dprst srp < 0.0 ', srp, dprst_srp
            endif

            ! May need to adjust dprst_srp and volumes
            srp = 0.0_dp
          endif
        endif

        9004 format(A,A,I4,2('/', I2.2),I7,A,2F10.7)
        ! 9005 format(A,A,I4,2('/', I2.2),I7,3es12.4e2)

        ! ******* Block 3 *******
        ! Impervious surface runoff
        if (sri > 0.0_dp) then
          tmp = sri * dble(this%sro_to_dprst_imperv(idx))

          if (this%dprst_area_open_max(idx) > 0.0) then
            dprst_sri_open = tmp * dble(this%dprst_frac_open(idx))
            dprst_sri = dprst_sri_open
            this%dprst_vol_open(idx) = this%dprst_vol_open(idx) + dprst_sri_open * dble(this%hru_percent_imperv(idx)) * hru_area_dble(idx)
          endif

          if (this%dprst_area_clos_max(idx) > 0.0) then
            dprst_sri_clos = tmp * dble(this%dprst_frac_clos(idx))
            dprst_sri = dprst_sri + dprst_sri_clos
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) + dprst_sri_clos * dble(this%hru_percent_imperv(idx)) * hru_area_dble(idx)
          endif

          sri = sri - dprst_sri

          if (sri < 0.0_dp) then
            if (sri < -DNEARZERO) then
              write(*, 9004) MODNAME, '%dprst_comp() WARNING: ', nowtime(1:3), idx, ' dprst sri < 0.0; (sri, dprst_sri) ', sri, dprst_sri
            endif

            ! May need to adjust dprst_sri and volumes
            sri = 0.0_dp
          endif
        endif

        this%dprst_insroff_hru(idx) = sngl(dprst_srp) * this%hru_frac_perv(idx) + &
                                      sngl(dprst_sri) * this%hru_percent_imperv(idx)

        ! Open depression surface area for each HRU:
        this%dprst_area_open(idx) = 0.0

        if (this%dprst_vol_open(idx) > 0.0_dp) then
          this%dprst_area_open(idx) = this%depression_surface_area(this%dprst_vol_open(idx), &
                                                                   this%dprst_vol_open_max(idx), &
                                                                   this%dprst_area_open_max(idx), &
                                                                   this%va_open_exp(idx))
        endif

        ! Closed depression surface area for each HRU:
        if (this%dprst_area_clos_max(idx) > 0.0) then
          this%dprst_area_clos(idx) = 0.0

          this%dprst_area_clos(idx) = this%depression_surface_area(this%dprst_vol_clos(idx), &
                                                                   this%dprst_vol_clos_max(idx), &
                                                                   this%dprst_area_clos_max(idx), &
                                                                   this%va_clos_exp(idx))
        endif

        ! ******* Block 4 *******
        ! Evaporate water from depressions based on snowcov_area
        ! dprst_evap_open & dprst_evap_clos = inches-acres on the HRU
        unsatisfied_et = dble(avail_et)
        dprst_avail_et = dble((potet * (1.0 - snowcov_area)) * this%dprst_et_coef(idx))
        this%dprst_evap_hru(idx) = 0.0

        if (dprst_avail_et > 0.0_dp) then
          dprst_evap_open = 0.0_dp
          dprst_evap_clos = 0.0_dp

          if (this%dprst_area_open(idx) > 0.0) then
            dprst_evap_open = min(this%dprst_area_open(idx) * dprst_avail_et, &
                                  this%dprst_vol_open(idx), &
                                  unsatisfied_et * hru_area_dble(idx))

            this%dprst_vol_open(idx) = max(this%dprst_vol_open(idx) - dprst_evap_open, 0.0_dp)
            unsatisfied_et = unsatisfied_et - dprst_evap_open / hru_area_dble(idx)
          endif

          if (this%dprst_area_clos(idx) > 0.0) then
            dprst_evap_clos = min(dble(this%dprst_area_clos(idx)) * dprst_avail_et, this%dprst_vol_clos(idx))

            if (dprst_evap_clos / hru_area_dble(idx) > unsatisfied_et) then
              dprst_evap_clos = unsatisfied_et * hru_area_dble(idx)
            endif

            if (dprst_evap_clos > this%dprst_vol_clos(idx)) then
              dprst_evap_clos = this%dprst_vol_clos(idx)
            endif

            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) - dprst_evap_clos
            this%dprst_vol_clos(idx) = max(0.0_dp, this%dprst_vol_clos(idx))
          endif

          this%dprst_evap_hru(idx) = sngl((dprst_evap_open + dprst_evap_clos) / hru_area_dble(idx))
        endif

        avail_et = avail_et - this%dprst_evap_hru(idx)

        ! ******* Block 5 *******
        ! Compute seepage
        this%dprst_seep_hru(idx) = 0.0_dp

        if (this%dprst_vol_open(idx) > 0.0_dp) then
          ! Compute seepage
          seep_open = this%dprst_vol_open(idx) * dble(this%dprst_seep_rate_open(idx))
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) - seep_open

          if (this%dprst_vol_open(idx) < 0.0_dp) then
            seep_open = seep_open + this%dprst_vol_open(idx)
            this%dprst_vol_open(idx) = 0.0_dp
          endif

          this%dprst_seep_hru(idx) = seep_open / hru_area_dble(idx)
        endif

        ! ******* Block 6 *******
        ! compute open surface runoff
        this%dprst_sroff_hru(idx) = 0.0_dp

        if (this%dprst_vol_open(idx) > 0.0_dp) then
          this%dprst_sroff_hru(idx) = max(0.0_dp, this%dprst_vol_open(idx) - this%dprst_vol_open_max(idx))
          this%dprst_sroff_hru(idx) = this%dprst_sroff_hru(idx) + &
                                      max(0.0_dp, (this%dprst_vol_open(idx) - this%dprst_sroff_hru(idx) - &
                                                   this%dprst_vol_thres_open(idx)) * dble(this%dprst_flow_coef(idx)))
          this%dprst_vol_open(idx) = this%dprst_vol_open(idx) - this%dprst_sroff_hru(idx)
          this%dprst_sroff_hru(idx) = this%dprst_sroff_hru(idx) / hru_area_dble(idx)

          this%dprst_vol_open(idx) = max(0.0_dp, this%dprst_vol_open(idx))
        endif

        if (this%dprst_area_clos_max(idx) > 0.0) then
          if (this%dprst_area_clos(idx) > NEARZERO) then
            seep_clos = this%dprst_vol_clos(idx) * dble(this%dprst_seep_rate_clos(idx))
            this%dprst_vol_clos(idx) = this%dprst_vol_clos(idx) - seep_clos

            if (this%dprst_vol_clos(idx) < 0.0_dp) then
              seep_clos = seep_clos + this%dprst_vol_clos(idx)
              this%dprst_vol_clos(idx) = 0.0_dp
            endif

            this%dprst_seep_hru(idx) = this%dprst_seep_hru(idx) + seep_clos / hru_area_dble(idx)
          endif

          this%dprst_vol_clos(idx) = max(0.0_dp, this%dprst_vol_clos(idx))
        endif

        if (this%dprst_vol_open_max(idx) > 0.0_dp) then
          this%dprst_vol_open_frac(idx) = sngl(this%dprst_vol_open(idx) / this%dprst_vol_open_max(idx))
        endif

        if (this%dprst_vol_clos_max(idx) > 0.0_dp) then
          this%dprst_vol_clos_frac(idx) = sngl(this%dprst_vol_clos(idx) / this%dprst_vol_clos_max(idx))
        endif

        this%dprst_vol_frac(idx) = sngl((this%dprst_vol_open(idx) + this%dprst_vol_clos(idx)) / &
                                        (this%dprst_vol_open_max(idx) + this%dprst_vol_clos_max(idx)))
        this%dprst_stor_hru(idx) = (this%dprst_vol_open(idx) + this%dprst_vol_clos(idx)) / hru_area_dble(idx)
      end associate
    end subroutine


    !***********************************************************************
    ! Subroutine to compute evaporation from impervious area at
    ! potential ET rate up to available ET
    !***********************************************************************
    module subroutine imperv_et(this, idx, potet, sca, avail_et, imperv_evap, imperv_stor)
      implicit none

      class(Srunoff), intent(in) :: this
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: sca
      real(r32), intent(in) :: avail_et
      real(r32), intent(inout) :: imperv_evap
      real(r32), intent(inout) :: imperv_stor
      ! ********************************************************************
      ! print *, '-- imperv_et()'
      if (sca < 1.0) then
        if (potet < imperv_stor) then
          imperv_evap = potet * (1.0 - sca)
        else
          imperv_evap = imperv_stor * (1.0 - sca)
        endif

        if (imperv_evap * this%hru_percent_imperv(idx) > avail_et) then
          imperv_evap = avail_et / this%hru_percent_imperv(idx)
        endif

        imperv_stor = imperv_stor - imperv_evap
      endif
    end subroutine


    module subroutine perv_comp(this, ctl_data, idx, soil_moist, soil_rechr, soil_rechr_max, &
                                pptp, ptc, contrib_frac, infil, srp)
      implicit none

      class(Srunoff), intent(in) :: this
      type(Control), intent(in) :: ctl_data
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: pptp
      real(r32), intent(in) :: ptc
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: soil_rechr
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(inout) :: contrib_frac
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: srp

      ! Local Variables
      real(r32) :: smidx
      real(r32) :: srpp

      ! READS: this%smidx_coef, this%smidx_exp, this%carea_min, this%carea_max,
      !        this%carea_dif
      !***********************************************************************
      associate(srunoff_module => ctl_data%srunoff_module%values)

        ! print *, '-- perv_comp()'
        !****** Pervious area computations
        if (srunoff_module(1)%s == 'srunoff_smidx') then
          ! Antecedent soil_moist
          smidx = soil_moist + (0.5 * ptc)
          contrib_frac = min(this%carea_max(idx), this%smidx_coef(idx) * 10.0**(this%smidx_exp(idx) * smidx))
        else
          ! srunoff_module == 'srunoff_carea'
          ! Antecedent soil_rechr
          contrib_frac = min(this%carea_max(idx), this%carea_min(idx) + this%carea_dif(idx) * (soil_rechr / soil_rechr_max))
        endif

        srpp = contrib_frac * pptp
        infil = infil - srpp
        srp = srp + dble(srpp)
      end associate
    end subroutine


    elemental module function depression_surface_area(volume, volume_max, area_max, va_exp) result(res)
      use prms_constants, only: DNEARZERO, dp
      implicit none

      real(r32) :: res
      real(r64), intent(in) :: volume
      real(r64), intent(in) :: volume_max
      real(r32), intent(in) :: area_max
      real(r32), intent(in) :: va_exp

      ! Local variables
      real(r32) :: frac_area
      real(r64) :: vol_r

      !***********************************************************************
      ! Depression surface area for each HRU:
      ! if (volume > 0.0_dp) then
        vol_r = volume / volume_max

        if (vol_r < DNEARZERO) then
          frac_area = 0.0
        elseif (vol_r > 1.0) then
          frac_area = 1.0
        else
          frac_area = exp(va_exp * sngl(log(vol_r)))
        endif

        res = min(area_max * frac_area, area_max)
      ! endif
    end function


    module subroutine read_dyn_params(this, ctl_data, model_basin, model_time, model_climate)
      use prms_constants, only: dp, LAKE, INACTIVE, NEARZERO
      use UTILS_PRMS, only: get_next_time, update_parameter
      implicit none

      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Climateflow), intent(in) :: model_climate

      logical :: has_imperv_update
      logical :: has_dprst_depth_update
      logical :: has_dprst_frac_update

      integer(i32) :: chru
      integer(i32) :: jj

      real(r32) :: adj
      real(r32) :: tmp
      ! real(r32), allocatable :: dprst_area_max_old(:)
      real(r32), allocatable :: dprst_frac_old(:)
      real(r32), allocatable :: hru_percent_imperv_old(:)
      real(r32), allocatable :: hru_frac_perv_old(:)
      real(r32), allocatable :: hru_area_perv_old(:)

      ! -----------------------------------------------------------------------
      associate(dprst_flag => ctl_data%dprst_flag%value, &
                dyn_dprst_flag => ctl_data%dyn_dprst_flag%value, &
                dyn_imperv_flag => ctl_data%dyn_imperv_flag%value, &

                nhru => model_basin%nhru, &
                active_hrus => model_basin%active_hrus, &
                hru_area => model_basin%hru_area, &
                hru_route_order => model_basin%hru_route_order, &
                hru_type => model_basin%hru_type, &

                curr_time => model_time%Nowtime, &

                soil_moist => model_climate%soil_moist, &
                soil_rechr => model_climate%soil_rechr)

        has_imperv_update = .false.
        has_dprst_depth_update = .false.
        has_dprst_frac_update = .false.
        this%srunoff_updated_soil = .false.

        if (any([1, 3]==dyn_imperv_flag) .or. any([1, 3]==dyn_dprst_flag)) then
          ! Need to keep temporary copy of the old pervious values
          allocate(hru_frac_perv_old(nhru))
          allocate(hru_area_perv_old(nhru))
          hru_frac_perv_old = this%hru_frac_perv
          hru_area_perv_old = this%hru_area_perv

          this%soil_moist_chg = soil_moist
          this%soil_rechr_chg = soil_rechr
        end if

        ! 1) Update imperv_stor_max if necessary
        ! leave current impervious storage amount alone as it will be taken care of later in current timestep
        if (any([2, 3]==dyn_imperv_flag)) then
          if (all(this%next_dyn_imperv_stor_date == curr_time(1:3))) then
            read(this%imperv_stor_unit, *) this%next_dyn_imperv_stor_date, this%imperv_stor_chgs
            ! write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: imperv_stor_max was updated. ', this%next_dyn_imperv_stor_date

            ! Update hru_percent_imperv with new values
            call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%imperv_stor_chgs, 'imperv_stor_max', this%imperv_stor_max)
            this%next_dyn_imperv_stor_date = get_next_time(this%imperv_stor_unit)
          end if
        endif

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 2) Update hru_percent_imperv
        if (any([1, 3]==dyn_imperv_flag)) then
          if (all(this%next_dyn_imperv_frac_date == curr_time(1:3))) then
            read(this%imperv_frac_unit, *) this%next_dyn_imperv_frac_date, this%imperv_frac_chgs
            ! write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: imperv_frac was updated. ', this%next_dyn_imperv_frac_date

            has_imperv_update = .true.

            ! Copy the old values for use below
            allocate(hru_percent_imperv_old(nhru))
            hru_percent_imperv_old = this%hru_percent_imperv

            ! Update hru_percent_imperv with new values
            call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%imperv_frac_chgs, 'hru_percent_imperv', this%hru_percent_imperv)
            this%next_dyn_imperv_frac_date = get_next_time(this%imperv_frac_unit)

            do jj = 1, active_hrus
              chru = hru_route_order(jj)
              if (hru_type(chru) == LAKE .or. hru_type(chru) == INACTIVE) CYCLE ! skip lake HRUs

              if (this%imperv_stor(chru) > 0.0) then
                if (this%hru_percent_imperv(chru) > 0.0) then
                  this%imperv_stor(chru) = this%imperv_stor(chru) * hru_percent_imperv_old(chru) / this%hru_percent_imperv(chru)

                else
                  write(output_unit, 9205) MODNAME, '%read_dyn_params() WARNING: ', curr_time(1:3), ' Dynamic hru_percent_imperv changed to 0 when imperv_stor > 0 for HRU: ', chru
                  adj = this%imperv_stor(chru) * hru_percent_imperv_old(chru) / hru_frac_perv_old(chru)
                  write(output_unit, 9206) this%imperv_stor(chru), hru_percent_imperv_old(chru), hru_frac_perv_old(chru), adj

                  this%soil_moist_chg(chru) = this%soil_moist_chg(chru) + adj
                  this%soil_rechr_chg(chru) = this%soil_rechr_chg(chru) + adj
                  this%srunoff_updated_soil = .true.
                  this%imperv_stor(chru) = 0.0
                end if
                this%hru_impervstor(chru) = this%imperv_stor(chru) * this%hru_percent_imperv(chru)
              end if
            end do

            9205 format(A, A, I4, 2('/', I2.2), A, I0)
            9206 format(10X, 3F9.6, F12.9)
            this%hru_area_imperv = this%hru_percent_imperv * hru_area
            this%hru_area_perv = hru_area - this%hru_area_imperv
            this%hru_frac_perv = 1.0 - this%hru_percent_imperv

            if (dprst_flag == 1) then
              this%hru_area_perv = this%hru_area_perv - this%dprst_frac * hru_area
              this%hru_frac_perv = this%hru_frac_perv - this%dprst_frac
            end if
          end if
        end if

        ! need to update maximum volumes after DPRST area is updated
        if (any([2, 3]==dyn_dprst_flag)) then
          if (all(this%next_dyn_dprst_depth_date == curr_time(1:3))) then
            read(this%dprst_depth_unit, *) this%next_dyn_dprst_depth_date, this%dprst_depth_chgs
            ! write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: dprst_depth_avg was updated. ', this%next_dyn_dprst_depth_date
            ! TODO: some work
            call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%dprst_depth_chgs, 'dprst_depth_avg', this%dprst_depth_avg)
            this%next_dyn_dprst_depth_date = get_next_time(this%dprst_depth_unit)

            has_dprst_depth_update = .true.
          end if
        end if

        if (any([1, 3]==dyn_dprst_flag)) then
          if (all(this%next_dyn_dprst_frac_date == curr_time(1:3))) then
            if (dprst_flag == 1) then
              read(this%dprst_frac_unit, *) this%next_dyn_dprst_frac_date, this%dprst_frac_chgs
              ! write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: dprst_frac was updated. ', this%next_dyn_dprst_frac_date

              ! allocate(dprst_area_max_old(nhru))
              ! dprst_area_max_old = this%dprst_area_max
              allocate(dprst_frac_old(nhru))
              dprst_frac_old = this%dprst_frac

              call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%dprst_frac_chgs, 'dprst_frac', this%dprst_frac)
              this%next_dyn_dprst_frac_date = get_next_time(this%dprst_frac_unit)

              has_dprst_frac_update = .true.

              do jj = 1, active_hrus
                chru = hru_route_order(jj)
                if (hru_type(chru) == LAKE .or. hru_type(chru) == INACTIVE) CYCLE ! skip lake HRUs

                tmp = sngl(this%dprst_vol_open(chru) + this%dprst_vol_clos(chru))

                if (this%dprst_frac(chru) == 0.0 .and. tmp > 0.0) then
                  adj = tmp / (dprst_frac_old(chru) * hru_area(chru)) / hru_frac_perv_old(chru)
                  write(output_unit, *) MODNAME, '%read_dyn_params() WARNING: dprst_frac reduced to 0 with storage > 0'
                  write(output_unit, *) '                            storage added to soil_moist and soil_rechr: ', adj

                  this%soil_moist_chg(chru) = this%soil_moist_chg(chru) + adj
                  this%soil_rechr_chg(chru) = this%soil_rechr_chg(chru) + adj
                  this%srunoff_updated_soil = .true.
                  this%dprst_vol_clos(chru) = 0.0_dp
                  this%dprst_vol_open(chru) = 0.0_dp
                end if
              end do

              ! dprst_frac_old is no longer needed at this point
              deallocate(dprst_frac_old)
              this%dprst_area_max = this%dprst_frac * hru_area

              ! Update pervious variables to reflect depression storage
              this%hru_area_perv = hru_area * (1.0 - this%hru_percent_imperv - this%dprst_frac)
              this%hru_frac_perv = 1.0 - this%hru_percent_imperv - this%dprst_frac
            else
              write(output_unit, *) MODNAME, '%dyn_param_read() WARNING: Attempt to update dprst_frac when drpst_flag = 0'
            end if
          end if
        end if

        9008 format(A, A, I4, 2('/', I2.2))

        if (has_imperv_update .or. has_dprst_frac_update) then
          where (hru_type == LAKE)
            ! Fix up the LAKE HRUs
            this%hru_frac_perv = 1.0
            this%hru_area_imperv = 0.0
            this%hru_area_perv = hru_area
          end where

          if (dprst_flag == 1) then
            where (hru_type == LAKE)
              this%dprst_area_max = 0.0
              end where
          end if

        end if

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! Any or all dynamic parameters have been read. Now update dependent
        ! variables.
        do jj = 1, active_hrus
          chru = hru_route_order(jj)
          if (hru_type(chru) == LAKE .or. hru_type(chru) == INACTIVE) CYCLE ! skip lake HRUs

          if (has_dprst_depth_update) then
            if (this%dprst_depth_avg(chru) == 0.0 .and. this%dprst_frac(chru) > 0.0) then
              write(output_unit, *) MODNAME, '%read_dyn_params() ERROR: dprst_frac > 0 and dprst_depth_avg == 0 for HRU: ', chru, '; dprst_frac: ', this%dprst_frac(chru)
              stop
            end if
          end if

          if (has_dprst_frac_update .or. has_dprst_depth_update) then
            this%dprst_area_open_max(chru) = this%dprst_area_max(chru) * this%dprst_frac_open(chru)
            this%dprst_area_clos_max(chru) = this%dprst_area_max(chru) - this%dprst_area_open_max(chru)

            this%dprst_vol_clos_max(chru) = this%dprst_area_clos_max(chru) * dble(this%dprst_depth_avg(chru))
            this%dprst_vol_open_max(chru) = this%dprst_area_open_max(chru) * dble(this%dprst_depth_avg(chru))
            this%dprst_vol_thres_open(chru) = this%dprst_vol_open_max(chru) * dble(this%op_flow_thres(chru))
          end if

          if (has_imperv_update .or. has_dprst_frac_update) then
            if (dprst_flag == 1) then
              if (this%hru_percent_imperv(chru) + this%dprst_frac(chru) > 0.999) then
                write(output_unit, *) MODNAME, '%read_dyn_params() ERROR: Fraction impervious + fraction depression > 0.999, for HRU: ', chru
                stop
              end if
            end if

            if (hru_area_perv_old(chru) /= this%hru_area_perv(chru)) then
              adj = hru_area_perv_old(chru) / this%hru_area_perv(chru)
              this%soil_moist_chg(chru) = this%soil_moist_chg(chru) * adj
              this%soil_rechr_chg(chru) = this%soil_rechr_chg(chru) * adj
              this%srunoff_updated_soil = .true.
            end if
          end if

          ! if (has_dprst_frac_update .or. has_dprst_depth_update) then
          !   if (this%dprst_area_max(chru) > NEARZERO) then
          !     this%dprst_vol_clos_max(chru) = this%dprst_area_clos_max(chru) * dble(this%dprst_depth_avg(chru))
          !     this%dprst_vol_open_max(chru) = this%dprst_area_open_max(chru) * dble(this%dprst_depth_avg(chru))
          !     this%dprst_vol_thres_open(chru) = this%dprst_vol_open_max(chru) * dble(this%op_flow_thres(chru))
          !     ! if (Dprst_vol_open_max(chru) > 0.0) then
          !     !   this%dprst_vol_open_frac(chru) = SNGL(this%dprst_vol_open(chru) / this%dprst_vol_open_max(chru))
          !     ! end if

          !     ! if (this%dprst_vol_clos_max(chru) > 0.0) then
          !     !   this%dprst_vol_clos_frac(chru) = SNGL(this%dprst_vol_clos(chru) / this%dprst_vol_clos_max(chru))
          !     ! end if

          !     ! this%dprst_vol_frac(chru) = SNGL((this%dprst_vol_open(chru) + this%dprst_vol_clos(chru)) / (this%dprst_vol_open_max(chru) + this%dprst_vol_clos_max(chru)))
          !   end if
          ! end if
        end do

        ! if (has_imperv_update) then
        !   write(output_unit, *) 'hru_percent_imperv (old/new): ', hru_percent_imperv_old(2568), this%hru_percent_imperv(2568)
        ! end if

        if (has_dprst_frac_update .or. has_dprst_depth_update) then
          this%has_closed_dprst = any(this%dprst_area_clos_max > 0.0)
          this%has_open_dprst = any(this%dprst_area_open_max > 0.0)
        end if

        if (has_imperv_update) then
          deallocate(hru_percent_imperv_old)
        end if

        if (any([1, 3]==dyn_imperv_flag) .or. any([1, 3]==dyn_dprst_flag)) then
          deallocate(hru_frac_perv_old)
          deallocate(hru_area_perv_old)
        end if
      end associate
    end subroutine
end submodule
