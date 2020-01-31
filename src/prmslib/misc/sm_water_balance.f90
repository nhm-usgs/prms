submodule (PRMS_WATER_BALANCE) sm_water_balance
  contains
    module function constructor_WaterBalance(ctl_data, model_basin, model_gwflow) result(this)
      use UTILS_PRMS, only: PRMS_open_module_file
      implicit none

      type(WaterBalance) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Gwflow), intent(in) :: model_gwflow

      ! ------------------------------------------------------------------------
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                print_debug => ctl_data%print_debug%value, &

                nhru => model_basin%nhru, &

                ! basin_gwstor => model_gwflow%basin_gwstor, &
                gwres_stor => model_gwflow%gwres_stor, &
                hru_storage => model_gwflow%hru_storage)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        call PRMS_open_module_file(this%intcp_unit, 'intcp.wbal')
        write(this%intcp_unit, 9003)

        call PRMS_open_module_file(this%snow_unit, 'snowcomp.wbal')
        write(this%snow_unit, 9007)

        call PRMS_open_module_file(this%sro_unit, MODNAME//'.wbal')
        if (cascade_flag > 0) then
          write(this%sro_unit, 9006)
        else
          write(this%sro_unit, 9005)
        endif

        call PRMS_open_module_file(this%sz_unit, 'soilzone.wbal')
        write (this%sz_unit, 9001)

        call PRMS_open_module_file(this%gw_unit, 'gwflow.wbal')
        write (this%gw_unit, 9004)

        call PRMS_open_module_file(this%bal_unit, 'wbal.msgs')

        9001 format ('    Date     Water Bal     bsmbal    last SM  soilmoist  last stor    SS stor    perv ET      sz2gw  interflow', &
                     '    soil2gw    dunnian    soil in   lakeinsz   downflow   swale ET  pref flow   pfr dunn   pfr stor', &
                     '  slow stor dunnian gvr lake evap')
        9003 format ('    Date     Water Bal     Precip     Netppt  Intcpevap  Intcpstor  last_stor changeover  net apply     apply')
        9004 format ('    Date     Water Bal last store  GWR store', &
                     '   GW input    GW flow    GW sink GW upslope minarea_in   downflow')
        9005 format ('    Date     Water Bal     Robal      sroff      infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
                     '   Perv Sro Imperv Sro  Dprst Sro')
        9006 format ('    Date     Water Bal     Robal      sroff      infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
                     '   Perv Sro Imperv Sro  Dprst Sro  Sroffdown  Srofflake')
        9007 format ('    Date     Water Bal  Snowpack    snowmelt   Snowevap  Snowcover' )

        this%basin_dprst_wb = 0.0_dp
        ! this%last_basin_gwstor = basin_gwstor
      end associate
    end function


    module subroutine run_WaterBalance(this, ctl_data, model_basin, &
                                       model_climate, model_gwflow, model_intcp, &
                                       model_precip, model_snow, model_soilzone, &
                                       model_srunoff, model_time)
      use prms_constants, only: LAKE, DNEARZERO, NEARZERO
      ! use, intrinsic :: ieee_arithmetic
      ! use, intrinsic :: ieee_exceptions
      ! use, intrinsic :: ieee_features, only: ieee_underflow_flag
      implicit none

      class(WaterBalance), intent(inout) :: this
        !! WaterBalance class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Gwflow), intent(in) :: model_gwflow
        !! Groundwater
      type(Interception), intent(in) :: model_intcp
      class(Precipitation), intent(in) :: model_precip
      type(Snowcomp), intent(in) :: model_snow
      type(Soilzone), intent(in) :: model_soilzone
      type(Srunoff), intent(in) :: model_srunoff
      type(Time_t), intent(in) :: model_time

      ! Local Variables
      ! character(len=:), allocatable :: error_txt

      integer(i32) :: chru
      integer(i32) :: kk

      real(r32) :: delstor
      real(r32) :: hrubal
      real(r64) :: last_sm

      real(r64) :: basin_bal
      real(r64) :: basin_robal
      real(r64) :: bsnobal
      real(r64) :: dprst_hru_wb
      real(r64) :: gvrbal
      real(r64) :: gwup
      real(r64) :: hru_in
      real(r64) :: hru_out
      real(r64) :: last_ss
      ! real(r64) :: robal
      real(r64) :: soil_in
      real(r64) :: soilbal
      real(r64) :: test
      real(r64) :: waterin
      real(r64) :: waterout
      real(r64) :: wbal

      real(r64), allocatable :: robal(:)
      ! type(ieee_flag_type), parameter :: ieee_custom(4) = [ieee_usual, ieee_underflow]
      ! ! type(ieee_flag_type), parameter :: ieee_custom(5) = [ieee_all]
      ! type(ieee_status_type) :: status_value
      ! logical, dimension(4) :: flag_value

      ! ------------------------------------------------------------------------
      ! call ieee_get_status(status_value)
      ! call ieee_set_halting_mode(ieee_custom, .false.)
      ! call ieee_set_flag(ieee_custom, .false.)


      associate(cascade_flag => ctl_data%cascade_flag%value, &
                cascadegw_flag => ctl_data%cascadegw_flag%value, &
                dprst_flag => ctl_data%dprst_flag%value, &

                nhru => model_basin%nhru, &
                active_hrus => model_basin%active_hrus, &
                basin_area_inv => model_basin%basin_area_inv, &
                cov_type => model_basin%cov_type, &
                hru_area_dble => model_basin%hru_area_dble, &
                hru_route_order => model_basin%hru_route_order, &
                hru_type => model_basin%hru_type, &

                pkwater_equiv => model_climate%pkwater_equiv, &
                soil_moist => model_climate%soil_moist, &
                soil_moist_max => model_climate%soil_moist_max, &
                soil_rechr => model_climate%soil_rechr, &

                gwin_dprst => model_gwflow%gwin_dprst, &
                has_gwstor_minarea => model_gwflow%has_gwstor_minarea, &
                gwres_flow => model_gwflow%gwres_flow, &
                gwres_in => model_gwflow%gwres_in, &
                gwres_sink => model_gwflow%gwres_sink, &
                gwres_stor => model_gwflow%gwres_stor, &
                gwres_stor_ante => model_gwflow%gwres_stor_ante, &
                gwstor_minarea_wb => model_gwflow%gwstor_minarea_wb, &
                gw_upslope => model_gwflow%gw_upslope, &
                hru_gw_cascadeflow => model_gwflow%hru_gw_cascadeflow, &
                hru_storage => model_gwflow%hru_storage, &
                hru_storage_ante => model_gwflow%hru_storage_ante, &

                canopy_covden => model_intcp%canopy_covden, &
                gain_inches => model_intcp%gain_inches, &
                hru_intcpevap => model_intcp%hru_intcpevap, &
                hru_intcpstor => model_intcp%hru_intcpstor, &
                intcp_changeover => model_intcp%intcp_changeover, &
                intcp_evap => model_intcp%intcp_evap, &
                intcp_stor => model_intcp%intcp_stor, &
                intcp_stor_ante => model_intcp%intcp_stor_ante, &
                last_intcp_stor => model_intcp%last_intcp_stor, &
                net_apply => model_intcp%net_apply, &
                net_ppt => model_intcp%net_ppt, &
                net_rain => model_intcp%net_rain, &
                net_snow => model_intcp%net_snow, &
                snow_intcp => model_intcp%snow_intcp, &
                srain_intcp => model_intcp%srain_intcp, &
                use_transfer_intcp => model_intcp%use_transfer_intcp, &
                wrain_intcp => model_intcp%wrain_intcp, &

                hru_ppt => model_precip%hru_ppt, &
                hru_rain => model_precip%hru_rain, &
                hru_snow => model_precip%hru_snow, &
                ! newsnow => model_precip%newsnow, &
                ! pptmix => model_precip%pptmix, &

                newsnow => model_snow%newsnow, &
                pkwater_ante => model_snow%pkwater_ante, &
                pptmix => model_snow%pptmix, &
                pptmix_nopack => model_snow%pptmix_nopack, &
                snowmelt => model_snow%snowmelt, &
                snow_evap => model_snow%snow_evap, &
                snowcov_area => model_snow%snowcov_area, &

                cap_infil_tot => model_soilzone%cap_infil_tot, &
                cap_waterin => model_soilzone%cap_waterin, &
                dunnian_flow => model_soilzone%dunnian_flow, &
                grav_dunnian_flow => model_soilzone%grav_dunnian_flow, &
                gvr2pfr => model_soilzone%gvr2pfr, &
                hru_actet => model_soilzone%hru_actet, &
                hru_sz_cascadeflow => model_soilzone%hru_sz_cascadeflow, &
                ! last_soil_moist => model_soilzone%last_soil_moist, &
                ! last_ssstor => model_soilzone%last_ssstor, &
                perv_actet => model_soilzone%perv_actet, &
                pfr_dunnian_flow => model_soilzone%pfr_dunnian_flow, &
                pref_flow => model_soilzone%pref_flow, &
                pref_flow_den => model_soilzone%pref_flow_den, &
                pref_flow_infil => model_soilzone%pref_flow_infil, &
                pref_flow_max => model_soilzone%pref_flow_max, &
                pref_flow_stor => model_soilzone%pref_flow_stor, &
                pref_flow_thrsh => model_soilzone%pref_flow_thrsh, &
                slow_flow => model_soilzone%slow_flow, &
                slow_stor => model_soilzone%slow_stor, &
                soil_lower => model_soilzone%soil_lower, &
                soil_moist_ante => model_soilzone%soil_moist_ante, &
                soil_moist_tot => model_soilzone%soil_moist_tot, &
                soil_to_gw => model_soilzone%soil_to_gw, &
                soil_to_ssr => model_soilzone%soil_to_ssr, &
                ssres_flow => model_soilzone%ssres_flow, &
                ssr_to_gw => model_soilzone%ssr_to_gw, &
                ssres_in => model_soilzone%ssres_in, &
                ssres_stor => model_soilzone%ssres_stor, &
                ssres_stor_ante => model_soilzone%ssres_stor_ante, &
                swale_actet => model_soilzone%swale_actet, &
                upslope_dunnianflow => model_soilzone%upslope_dunnianflow, &
                upslope_interflow => model_soilzone%upslope_interflow, &

                dprst_area_clos => model_srunoff%dprst_area_clos, &
                dprst_area_max => model_srunoff%dprst_area_max, &
                dprst_evap_hru => model_srunoff%dprst_evap_hru, &
                dprst_frac => model_srunoff%dprst_frac, &
                dprst_in => model_srunoff%dprst_in, &
                dprst_insroff_hru => model_srunoff%dprst_insroff_hru, &
                dprst_seep_hru => model_srunoff%dprst_seep_hru, &
                dprst_sroff_hru => model_srunoff%dprst_sroff_hru, &
                dprst_stor_ante => model_srunoff%dprst_stor_ante, &
                dprst_stor_hru => model_srunoff%dprst_stor_hru, &
                dprst_vol_clos => model_srunoff%dprst_vol_clos, &
                dprst_vol_open => model_srunoff%dprst_vol_open, &
                hortonian_flow => model_srunoff%hortonian_flow, &
                hru_area_perv => model_srunoff%hru_area_perv, &
                hru_frac_perv => model_srunoff%hru_frac_perv, &
                hru_hortn_cascflow => model_srunoff%hru_hortn_cascflow, &
                hru_impervevap => model_srunoff%hru_impervevap, &
                hru_impervstor => model_srunoff%hru_impervstor, &
                hru_percent_imperv => model_srunoff%hru_percent_imperv, &
                hru_sroffi => model_srunoff%hru_sroffi, &
                hru_sroffp => model_srunoff%hru_sroffp, &
                imperv_stor_ante => model_srunoff%imperv_stor_ante, &
                infil => model_srunoff%infil, &
                sroff => model_srunoff%sroff, &
                sro_to_dprst_perv => model_srunoff%sro_to_dprst_perv, &
                upslope_hortonian => model_srunoff%upslope_hortonian, &
                use_sroff_transfer => model_srunoff%use_sroff_transfer, &

                nowday => model_time%Nowday, &
                nowmonth => model_time%Nowmonth, &
                nowtime => model_time%Nowtime, &
                nowyear => model_time%Nowyear)

        ! this%basin_capillary_wb = 0.0_dp
        ! this%basin_gravity_wb = 0.0_dp
        ! this%basin_soilzone_wb = 0.0_dp
        basin_bal = 0.0_dp
        soil_in = 0.0_dp
        basin_robal = 0.0_dp
        bsnobal = 0.0_dp

        allocate(robal(nhru))
        robal = dble(snowmelt - hortonian_flow - infil * hru_frac_perv - hru_impervevap + &
                     imperv_stor_ante - hru_impervstor) + intcp_changeover

        if (use_sroff_transfer) then
          robal = robal + dble(net_apply * hru_frac_perv)
        endif

        where (net_ppt > 0.0)
          where (pptmix_nopack)
            robal = robal + dble(net_rain)
          elsewhere (snowmelt < NEARZERO .and. pkwater_equiv < DNEARZERO)
            where (snow_evap < NEARZERO)
              robal = robal + dble(net_ppt)
            elsewhere (net_snow < NEARZERO)
              robal = robal + dble(net_rain)
            end where
          end where
        end where

        ! TODO: Uncomment once cascade module is converted
        ! if (cascade_flag > 0) then
        !   robal = robal + sngl(upslope_hortonian(chru) - hru_hortn_cascflow(chru))
        ! endif

        if (dprst_flag == 1) then
          robal = robal - dble(dprst_evap_hru) + dprst_stor_ante - dprst_stor_hru - dprst_seep_hru
        endif

        do kk=1, active_hrus
          chru = hru_route_order(kk)

          if (hru_type(chru) == LAKE) cycle ! no water balance for lakes

          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! intcp
          delstor = hru_intcpstor(chru) - intcp_stor_ante(chru)
          hrubal = hru_rain(chru) + hru_snow(chru) - net_rain(chru) - &
                   net_snow(chru) - delstor - hru_intcpevap(chru) - intcp_changeover(chru)

          ! if (use_transfer_intcp == 1) then
          if (use_transfer_intcp) then
            hrubal = hrubal + gain_inches(chru) - net_apply(chru)
          endif

          if (abs(hrubal) > TOOSMALL) then
            if (abs(hrubal) > SMALL) then
              write(this%bal_unit, *) 'Possible HRU interception water balance error'
            else
              write(this%bal_unit, *) 'Interception HRU rounding issue'
            endif

            write(this%bal_unit,'(I7,6I5,15F10.5,I5)') chru, nowtime, hrubal, &
                  net_rain(chru), net_snow(chru), hru_rain(chru), hru_snow(chru), &
                  intcp_stor(chru), intcp_stor_ante(chru), intcp_evap(chru), srain_intcp(chru), &
                  wrain_intcp(chru), snow_intcp(chru), canopy_covden(chru), delstor, &
                  hru_intcpstor(chru), intcp_changeover(chru), cov_type(chru)

            ! if (use_transfer_intcp == 1) then
            if (use_transfer_intcp) then
              write(this%bal_unit, *) gain_inches(chru), net_apply(chru)
            endif
          endif

          ! --------------------------------
          ! Snow

          ! Skip the HRU if there is no snowpack and no new snow
          if (pkwater_ante(chru) > DNEARZERO .or. newsnow(chru) == 1) then
            hrubal = sngl(pkwater_ante(chru) - pkwater_equiv(chru)) - snow_evap(chru) - snowmelt(chru)

            ! if (pptmix_nopack(chru) == 1) then
            if (pptmix_nopack(chru)) then
              hrubal = hrubal + net_snow(chru)
            else
              hrubal = hrubal + net_ppt(chru)
            endif

            if (abs(hrubal) > TOOSMALL) then
              if (abs(hrubal) > SMALL) then
                write(this%bal_unit, *) 'Possible snow water balance error'
              else
                write(this%bal_unit, *) 'Possible HRU snow rounding issue'
              endif

              write(this%bal_unit, *) chru, hrubal, nowyear, nowmonth, nowday, &
                               pkwater_ante(chru), pkwater_equiv(chru), &
                               snow_evap(chru), snowmelt(chru), net_ppt(chru), &
                               net_snow(chru), net_rain(chru), newsnow(chru), &
                               pptmix(chru), pptmix_nopack(chru)
            endif

            bsnobal = bsnobal + dble(hrubal) * hru_area_dble(chru)
          endif

          ! Includes dprst runoff, if any
          ! robal = dble(snowmelt(chru) - hortonian_flow(chru) - infil(chru) * hru_frac_perv(chru) - &
          !         hru_impervevap(chru) + imperv_stor_ante(chru) - hru_impervstor(chru)) + &
          !         intcp_changeover(chru)

          ! if (use_sroff_transfer) then
          !   robal = robal + dble(net_apply(chru) * hru_frac_perv(chru))
          ! endif

          ! if (net_ppt(chru) > 0.0) then
          !   ! if (pptmix_nopack(chru) == 1) then
          !   if (pptmix_nopack(chru)) then
          !     robal = robal + dble(net_rain(chru))
          !   elseif (snowmelt(chru) < NEARZERO .and. pkwater_equiv(chru) < DNEARZERO) then
          !     if (snow_evap(chru) < NEARZERO) then
          !       robal = robal + dble(net_ppt(chru))
          !     elseif (net_snow(chru) < NEARZERO) then
          !       robal = robal + dble(net_rain(chru))
          !     endif
          !   endif
          ! endif

          ! ! TODO: Uncomment once cascade module is converted
          ! ! if (cascade_flag > 0) then
          ! !   robal = robal + sngl(upslope_hortonian(chru) - hru_hortn_cascflow(chru))
          ! ! endif

          ! if (dprst_flag == 1) then
          !   robal = robal - dble(dprst_evap_hru(chru)) + dprst_stor_ante(chru) - dprst_stor_hru(chru) - dprst_seep_hru(chru)
          ! endif

          ! basin_robal = basin_robal + robal

          if (abs(robal(chru)) > TOOSMALL) then
            if (dprst_flag == 1) then
              dprst_hru_wb = dprst_stor_ante(chru) - dprst_stor_hru(chru) - &
                             dprst_seep_hru(chru) - dprst_sroff_hru(chru) + dprst_in(chru) - &
                             dble(dprst_evap_hru(chru)) + dble(dprst_insroff_hru(chru))
              this%basin_dprst_wb = this%basin_dprst_wb + dprst_hru_wb * hru_area_dble(chru)

              write(this%bal_unit, 9200) nowtime(1:3), ' dprst ', dprst_hru_wb, dprst_stor_hru(chru), &
                               dprst_stor_hru(chru), dprst_seep_hru(chru), &
                               dprst_evap_hru(chru), dprst_sroff_hru(chru), &
                               snowmelt(chru), net_rain(chru), dprst_insroff_hru(chru)

              write(this%bal_unit, 9201) dprst_vol_open(chru), dprst_vol_clos(chru), &
                               (dprst_vol_open(chru) + dprst_vol_clos(chru)) / hru_area_dble(chru), &
                               dprst_area_max(chru), pkwater_equiv(chru), &
                               dprst_area_clos(chru), snowcov_area(chru), &
                               dprst_in(chru), sro_to_dprst_perv(chru), hru_sroffp(chru), &
                               hru_sroffi(chru)

              write(this%bal_unit, 9202) robal(chru), net_rain(chru), net_ppt(chru), &
                               net_rain(chru) * dprst_frac(chru), dprst_frac(chru), &
                               pptmix_nopack(chru)

              write(this%bal_unit, 9203) infil(chru), hru_frac_perv(chru), hru_impervevap(chru), &
                               imperv_stor_ante(chru), hru_impervstor(chru), &
                               hru_percent_imperv(chru), dprst_sroff_hru(chru)
              9200 format(I4, 2('/', I2.2), A, 9es11.3e3)
              9201 format(2X, 11es11.3e3)
              9202 format(2X, 5es11.3e3, 1X, L1)
              9203 format(2X, 7es11.3e3)
            endif

            if (abs(robal(chru)) > SMALL) then
              write(this%bal_unit, 9117) nowtime(1:3), ' Possible HRU surface runoff water balance ERROR ', chru, &
                               ' hru_type:', hru_type(chru)
            else
              write(this%bal_unit, 9117) nowtime(1:3), ' HRU surface runoff rounding issue ', chru, &
                               ' hru_type:', hru_type(chru)
            endif
            9117 format(I4, 2('/', I2.2), A, I0, A, I0)

            if (cascade_flag > 0) then
              write(this%bal_unit, '(3I3,F10.6,17F10.4)') nowmonth, nowday, pptmix_nopack(chru), &
                                                   robal(chru), snowmelt(chru), upslope_hortonian(chru), &
                                                   imperv_stor_ante(chru), hru_hortn_cascflow(chru), &
                                                   infil(chru), hortonian_flow(chru), &
                                                   hru_impervstor(chru), hru_impervevap(chru), &
                                                   net_ppt(chru), pkwater_equiv(chru), &
                                                   snow_evap(chru), net_snow(chru), &
                                                   net_rain(chru), hru_sroffp(chru), &
                                                   hru_sroffi(chru), hru_area_dble(chru)
            else
              ! write(this%bal_unit,'(2I3, L, F10.6,15F10.5,F10.3)') nowtime(1:3), pptmix_nopack(chru), &
              write(this%bal_unit, 9118) nowtime(1:3), pptmix_nopack(chru), &
                                                        robal(chru), snowmelt(chru), imperv_stor_ante(chru), &
                                                        infil(chru), hortonian_flow(chru), &
                                                        hru_impervstor(chru), hru_impervevap(chru), &
                                                        hru_percent_imperv(chru), net_ppt(chru), &
                                                        pkwater_equiv(chru), snow_evap(chru), &
                                                        net_snow(chru), net_rain(chru), &
                                                        hru_sroffp(chru), hru_sroffi(chru), hru_area_dble(chru)
            endif

            9118 format(I4, 2('/', I2.2), 1X, L1, 1X, 16es11.3e3)
          endif

          last_sm = soil_moist_ante(chru)
          last_ss = ssres_stor_ante(chru)

          ! if (chru == 508) then
          !   write(*, *) chru, last_sm, soil_moist(chru), perv_actet(chru), hru_frac_perv(chru)
          ! endif
          soilbal = (last_sm - soil_moist(chru) - perv_actet(chru)) * hru_frac_perv(chru) - &
                    soil_to_ssr(chru) - soil_to_gw(chru) + cap_infil_tot(chru)


          ! call ieee_get_flag(ieee_custom, flag_value)
          ! if (any(flag_value)) then
          !   error_txt = ''
          !
          !   ! Only checking for underflow and overflow
          !   if (flag_value(1)) then
          !     error_txt = 'overflow'
          !   elseif (flag_value(2)) then
          !     error_txt = 'divide-by-zero'
          !   elseif (flag_value(3)) then
          !     error_txt = 'ieee_invalid'
          !   elseif (flag_value(4)) then
          !     error_txt = 'underflow'
          !   ! elseif (flag_value(5)) then
          !   !   error_txt = 'ieee_inexact'
          !   endif
          !
          !   write(*, 9005) MODNAME, '%run WARNING: ', error_txt, ' occurred in soilbal [hru, soilbal, last_sm, soil_moist, perv_actet] ', nowtime(1:3), chru, soilbal, last_sm, soil_moist(chru), perv_actet(chru)
          !
          !   call ieee_set_flag(ieee_custom, .false.)
          ! endif
          !
          ! ! ieee_inexact occurs alot and can be ignored
          ! call ieee_set_flag(ieee_inexact, .false.)
          ! 9005 format(A,A,A,A,I4,2('/', I2.2),i7,4es12.4e2)

          ! call ieee_set_status(status_value)

          if (abs(soilbal) > TOOSMALL) then
            write(this%bal_unit, *) 'HRU capillary problem'
            write(this%bal_unit, *) soilbal, cap_infil_tot(chru), last_sm, soil_moist(chru), &
                             perv_actet(chru), soil_to_ssr(chru), soil_to_gw(chru), &
                             chru, infil(chru), pref_flow_infil(chru), hru_frac_perv(chru), &
                             soil_moist_max(chru), cap_waterin(chru)

            ! TODO: Uncomment once cascade module is converted
            ! if (cascade_flag > 0) then
            !   write(this%bal_unit, *) 'UP cascade', upslope_interflow(chru), upslope_dunnianflow(chru)
            ! endif
          endif

          gvrbal = last_ss - ssres_stor(chru) + soil_to_ssr(chru) - ssr_to_gw(chru) - &
                   swale_actet(chru) - dunnian_flow(chru) - ssres_flow(chru) + &
                   pfr_dunnian_flow(chru) + pref_flow_infil(chru)

          ! TODO: Uncomment once cascade module is converted
          ! if (cascade_flag > 0) then
          !   gvrbal = gvrbal - hru_sz_cascadeflow(chru)
          ! endif

          test = abs(gvrbal)
          if (test > TOOSMALL) then
            write(this%bal_unit, *) 'Bad GVR balance, HRU:', chru, ' hru_type:', hru_type(chru)
            write(this%bal_unit, *) gvrbal, last_ss, ssres_stor(chru), ssr_to_gw(chru), &
                             swale_actet(chru), dunnian_flow(chru), ssres_flow(chru), &
                             pfr_dunnian_flow(chru), pref_flow_thrsh(chru), ssres_in(chru), &
                             pref_flow_infil(chru), grav_dunnian_flow(chru), &
                             slow_flow(chru), pref_flow(chru), soil_to_ssr(chru), &
                             gvr2pfr(chru), hru_frac_perv(chru), slow_stor(chru), &
                             pref_flow_stor(chru), infil(chru), pref_flow_max(chru), &
                             pref_flow_den(chru)

            ! TODO: Uncomment once cascade module is converted
            ! if (cascade_flag > 0) then
            !   write(this%bal_unit, *) 'sz cascade', hru_sz_cascadeflow(chru)
            ! endif
          endif

          waterin = cap_infil_tot(chru) + pref_flow_infil(chru) + pfr_dunnian_flow(chru)
          waterout = ssr_to_gw(chru) + ssres_flow(chru) + soil_to_gw(chru) + &
                     swale_actet(chru) + perv_actet(chru) * hru_frac_perv(chru) + dunnian_flow(chru)

          ! TODO: Uncomment once cascade module is converted
          ! if (cascade_flag > 0) then
          !   waterout = waterout + hru_sz_cascadeflow(chru)
          ! endif

          soil_in = soil_in + dble(infil(chru) * hru_frac_perv(chru)) * hru_area_dble(chru)
          soilbal = waterin - waterout + last_ss - ssres_stor(chru) + &
                    (last_sm - soil_moist(chru)) * hru_frac_perv(chru)
          basin_bal = basin_bal + dble(soilbal) * hru_area_dble(chru)

          test = abs(soilbal)
          if (test > TOOSMALL) then
            write(this%bal_unit, *) 'HRU:', chru, ' hru_type:', hru_type(chru)

            if (test > BAD) then
              write(this%bal_unit, *) 'HRU soilzone water balance ***ERROR***'
            elseif (test > SMALL) then
              write(this%bal_unit, *) 'Possible soilzone HRU water balance ERROR'
            else
              write(this%bal_unit, *) 'Possible soilzone HRU water balance rounding issue'
            endif

            write(this%bal_unit, 9001) nowyear, nowmonth, nowday, chru, soilbal, infil(chru), &
                                last_sm, last_ss, soil_moist(chru), ssres_stor(chru), &
                                perv_actet(chru), ssr_to_gw(chru), slow_flow(chru), &
                                pref_flow(chru), ssres_flow(chru), soil_to_gw(chru), &
                                pref_flow_infil(chru), pref_flow_stor(chru), &
                                slow_stor(chru), soil_rechr(chru), soil_lower(chru), &
                                soil_to_ssr(chru), ssres_flow(chru), waterin, swale_actet(chru)

            ! TODO: Uncomment once cascade module is converted
            ! if (cascade_flag > 0) then
            !   write(this%bal_unit, *) 'cascade', upslope_dunnianflow(chru), upslope_interflow(chru), &
            !                    hru_sz_cascadeflow(chru), Ncascade_hru(chru)
            ! endif

            write(this%bal_unit, *) hru_area_perv(chru), hru_frac_perv(chru), pref_flow_den(chru), &
                             (infil(chru) * hru_frac_perv(chru)), cap_infil_tot(chru)
            write(this%bal_unit, *) dunnian_flow(chru), pfr_dunnian_flow(chru)
          endif

          hru_out = dble(sroff(chru) + gwres_flow(chru) + ssres_flow(chru) + hru_actet(chru) + gwres_sink(chru))
          hru_in = dble(hru_ppt(chru))

          ! TODO: Uncomment once cascade module is converted
          ! if (cascade_flag > 0) then
          !   hru_out = hru_out + dble(hru_sz_cascadeflow(chru)) + hru_hortn_cascflow(chru)
          !   hru_in = hru_in + upslope_dunnianflow(chru) + upslope_interflow(chru) + upslope_hortonian(chru)
          ! endif

          if (cascadegw_flag > 0) then
            hru_out = hru_out + hru_gw_cascadeflow(chru)
            hru_in = hru_in + gw_upslope(chru) / dble(hru_area_dble(chru))
          endif

          wbal = hru_in - hru_out + hru_storage_ante(chru) - hru_storage(chru)

          if (has_gwstor_minarea) then
            wbal = wbal + gwstor_minarea_wb(chru)
          endif

          if (dabs(wbal) > DTOOSMALL) then
            write(this%bal_unit, 9205) nowtime(1:3), ' Possible HRU water balance issue: ', wbal, &
                             '; HRU: ', chru, ' hru_type: ', hru_type(chru), &
                             '; area: ', hru_area_dble(chru)
            write(this%bal_unit, 9206) sroff(chru), gwres_flow(chru), ssres_flow(chru), &
                             hru_actet(chru), gwres_sink(chru), hru_storage_ante(chru), &
                             hru_storage(chru), pfr_dunnian_flow(chru)

            write(this%bal_unit, 9206) soil_moist_tot(chru), hru_intcpstor(chru), gwres_stor(chru), &
                             pkwater_equiv(chru), hru_impervstor(chru), hru_percent_imperv(chru)

            ! 9202 format (I5, 2('/', I2.2), 23F11.5)
            9206 format (10F11.5)
            9205 format(I4, 2('/', I2.2), A, F0.7, A, I0, A, I0, A, F0.6)
            ! 9205 format(I4, 2('/', I2.2), A, es11.3e3, A, I0, A, I0, A, es11.3e3)
            ! TODO: Uncomment once cascade module is converted
            ! if (cascade_flag > 0) then
            !   write(this%bal_unit, *) hru_sz_cascadeflow(chru), upslope_dunnianflow(chru), &
            !                    upslope_interflow(chru)
            !   write(this%bal_unit, *) upslope_hortonian(chru), hru_hortn_cascflow(chru)
            ! endif

            if (cascadegw_flag > 0) then
              write(this%bal_unit, *) gw_upslope(chru) / dble(hru_area_dble(chru)), hru_gw_cascadeflow(chru)
            endif

            ! write(this%bal_unit, *) nowtime
          endif

          wbal = gwres_stor_ante(chru) + gwres_in(chru) / hru_area_dble(chru) - gwres_stor(chru) - &
                 dble(gwres_sink(chru) + gwres_flow(chru))

          gwup = 0.0_dp
          if (cascadegw_flag > 0) then
            wbal = wbal - hru_gw_cascadeflow(chru)
            gwup = gw_upslope(chru)
          endif

          if (has_gwstor_minarea) then
            wbal = wbal + gwstor_minarea_wb(chru)
          endif

          if (dabs(wbal) > DTOOSMALL) then
            write(this%bal_unit, 9113) nowtime(1:3), ' Possible GWR water balance issue ', &
                             chru, wbal, gwres_stor_ante(chru), gwres_in(chru) / hru_area_dble(chru), &
                             gwres_stor(chru), gwres_flow(chru), gwres_sink(chru), &
                             soil_to_gw(chru), ssr_to_gw(chru), gwup, hru_area_dble(chru)

            if (cascadegw_flag > 0) then
              write(this%bal_unit, *) 'gw cascade', hru_gw_cascadeflow(chru)
            endif

            if (has_gwstor_minarea) then
              write(this%bal_unit, *) 'gwstor_minarea_wb', gwstor_minarea_wb(chru)
            endif

            if (dprst_flag == 1) then
              write(this%bal_unit, *) 'gwin_dprst', gwin_dprst(chru)
            endif
          endif
        enddo

        9113 format(I4, 2('/', I2.2), A, I0, 10es11.3e3)

        this%basin_dprst_wb = this%basin_dprst_wb * basin_area_inv

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! intcp
        ! call this%basin_wb_intcp(model_intcp, model_srunoff, model_precip, model_time)

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! snowcomp
        ! bsnobal = bsnobal * basin_area_inv
        ! call this%basin_wb_snow(model_snow, model_time, bsnobal)

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! srunoff
        ! basin_robal = sum(robal)
        ! call this%basin_wb_srunoff(ctl_data, model_srunoff, model_time, basin_robal)
        deallocate(robal)

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! soilzone
        ! soil_in = soil_in * basin_area_inv
        ! basin_bal = basin_bal * basin_area_inv
        ! call this%basin_wb_soilzone(model_soilzone, model_srunoff, model_time, basin_bal, soil_in)

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! gwflow
        ! call this%basin_wb_gwflow(model_gwflow, model_time)

        9001 format (I5, 2('/', I2.2), I7, 26F11.5)
        ! 9002 format (I5, 2('/', I2.2), 23F11.5)
        ! 9003 format (A, I5, 2('/', I2.2), F12.5)
      end associate
    end subroutine


    ! module subroutine basin_wb_gwflow(this, model_gwflow, model_time)
    !   implicit none

    !   class(WaterBalance), intent(inout) :: this
    !   type(Gwflow), intent(in) :: model_gwflow
    !   type(Time_t), intent(in) :: model_time

    !   ! Local variables
    !   real(r64) :: gwbal

    !   ! Gwflow
    !   ! basin_dnflow, basin_gwflow, basin_gwin, basin_gwsink, basin_gwstor,
    !   ! basin_gwstor_minarea_wb, basin_gw_upslope,

    !   ! Time
    !   ! nowday, nowmonth, nowyear,

    !   ! ------------------------------------------------------------------------
    !   associate(basin_dnflow => model_gwflow%basin_dnflow, &
    !             basin_gwflow => model_gwflow%basin_gwflow, &
    !             basin_gwin => model_gwflow%basin_gwin, &
    !             basin_gwsink => model_gwflow%basin_gwsink, &
    !             basin_gwstor => model_gwflow%basin_gwstor, &
    !             basin_gwstor_minarea_wb => model_gwflow%basin_gwstor_minarea_wb, &
    !             basin_gw_upslope => model_gwflow%basin_gw_upslope, &

    !             nowtime => model_time%nowtime, &
    !             nowday => model_time%Nowday, &
    !             nowmonth => model_time%Nowmonth, &
    !             nowyear => model_time%Nowyear)

    !     ! Not going to balance because gwstor under lakes is computed each
    !     ! time step fix for lakes.
    !     ! basin_gwin includes upslope flow, gwin_dprst, soil_to_gw, ssr_to_gw
    !     gwbal = basin_gwin + this%last_basin_gwstor - basin_gwstor - basin_gwsink - &
    !             basin_gwflow - basin_dnflow + basin_gwstor_minarea_wb

    !     if (dabs(gwbal) > DSMALL) then
    !       write(this%bal_unit, 9003) nowtime(1:3), ' Possible GWR basin water balance issue ', gwbal
    !     endif

    !     write (this%gw_unit, 9002) nowyear, nowmonth, nowday, gwbal, this%last_basin_gwstor, &
    !                          basin_gwstor, basin_gwin, basin_gwflow, basin_gwsink, &
    !                          basin_gw_upslope, basin_gwstor_minarea_wb, basin_dnflow
    !     this%last_basin_gwstor = basin_gwstor
    !   end associate

    !   9002 format (I5, 2('/', I2.2), 23F11.5)
    !   9003 format(I4, 2('/', I2.2), A, es11.3e3)
    !   ! 9003 format (A, I5, 2('/', I2.2), F12.5)
    ! end subroutine


    ! module subroutine basin_wb_intcp(this, model_intcp, model_srunoff, model_precip, model_time)
    !   implicit none

    !   class(WaterBalance), intent(inout) :: this
    !   type(Interception), intent(in) :: model_intcp
    !   type(Srunoff), intent(in) :: model_srunoff
    !   class(Precipitation), intent(in) :: model_precip
    !   type(Time_t), intent(in) :: model_time

    !   ! Local variables
    !   real(r64) :: delta_stor
    !   real(r64) :: pptbal

    !   ! Intcp
    !   ! basin_changeover, basin_hru_apply, basin_intcp_evap, basin_intcp_stor,
    !   ! basin_net_apply, basin_net_ppt, last_intcp_stor,

    !   ! Precipitation
    !   ! basin_ppt

    !   ! Srunoff
    !   ! use_sroff_transfer

    !   ! Time
    !   ! nowday, nowmonth, nowyear,

    !   ! ------------------------------------------------------------------------
    !   associate(basin_changeover => model_intcp%basin_changeover, &
    !             basin_hru_apply => model_intcp%basin_hru_apply, &
    !             basin_intcp_evap => model_intcp%basin_intcp_evap, &
    !             basin_intcp_stor => model_intcp%basin_intcp_stor, &
    !             basin_net_apply => model_intcp%basin_net_apply, &
    !             basin_net_ppt => model_intcp%basin_net_ppt, &
    !             last_intcp_stor => model_intcp%last_intcp_stor, &

    !             basin_ppt => model_precip%basin_ppt, &

    !             use_sroff_transfer => model_srunoff%use_sroff_transfer, &

    !             nowtime => model_time%nowtime, &
    !             nowday => model_time%Nowday, &
    !             nowmonth => model_time%Nowmonth, &
    !             nowyear => model_time%Nowyear)

    !     delta_stor = basin_intcp_stor - last_intcp_stor
    !     pptbal = basin_ppt - basin_net_ppt - delta_stor - basin_intcp_evap

    !     if (use_sroff_transfer) then
    !       pptbal = pptbal + basin_net_apply
    !     endif

    !     if (dabs(pptbal) > DSMALL) then
    !       write(this%bal_unit, 9003) nowtime(1:3), ' Possible basin interception water balance error ', pptbal
    !     elseif (dabs(pptbal) > DTOOSMALL) then
    !       write(this%bal_unit, 9003) nowtime(1:3), ' Interception basin rounding issue ', pptbal
    !     endif

    !     if (use_sroff_transfer) then
    !       write(this%intcp_unit, 9002) nowyear, nowmonth, nowday, pptbal, basin_ppt, &
    !                             basin_net_ppt, basin_intcp_evap, basin_intcp_stor, &
    !                             last_intcp_stor, basin_changeover, basin_net_apply, &
    !                             basin_hru_apply
    !     else
    !       write(this%intcp_unit, 9002) nowyear, nowmonth, nowday, pptbal, basin_ppt, &
    !                             basin_net_ppt, basin_intcp_evap, basin_intcp_stor, &
    !                             last_intcp_stor, basin_changeover
    !     end if
    !   end associate

    !   9002 format (I5, 2('/', I2.2), 23F11.5)
    !   9003 format(I4, 2('/', I2.2), A, es11.3e3)
    !   ! 9003 format (A, I5, 2('/', I2.2), F12.5)
    ! end subroutine


    ! module subroutine basin_wb_snow(this, model_snow, model_time, basin_snowbal)
    !   implicit none

    !   class(WaterBalance), intent(inout) :: this
    !   type(Snowcomp), intent(in) :: model_snow
    !   type(Time_t), intent(in) :: model_time
    !   real(r64), intent(in) :: basin_snowbal

    !   ! Snowcomp
    !   ! basin_pweqv, basin_snowcov, basin_snowevap, basin_snowmelt,

    !   ! Time
    !   ! Nowday, Nowmonth, Nowyear

    !   ! ------------------------------------------------------------------------
    !   associate(basin_pweqv => model_snow%basin_pweqv, &
    !             basin_snowcov => model_snow%basin_snowcov, &
    !             basin_snowevap => model_snow%basin_snowevap, &
    !             basin_snowmelt => model_snow%basin_snowmelt, &

    !             nowtime => model_time%nowtime, &
    !             nowday => model_time%Nowday, &
    !             nowmonth => model_time%Nowmonth, &
    !             nowyear => model_time%Nowyear)

    !     if (dabs(basin_snowbal) > DSMALL) then
    !       write(this%bal_unit, 9003) nowtime(1:3), ' Possible basin snow water balance error ', basin_snowbal
    !     elseif (dabs(basin_snowbal) > DTOOSMALL) then
    !       write(this%bal_unit, 9003) nowtime(1:3), ' Possible basin snow rounding issue ', basin_snowbal
    !     endif

    !     write(this%snow_unit, 9002) nowyear, nowmonth, nowday, basin_snowbal, basin_pweqv, &
    !                           basin_snowmelt, basin_snowevap, basin_snowcov
    !   end associate

    !   9002 format (I5, 2('/', I2.2), 23F11.5)
    !   ! 9003 format (A, I5, 2('/', I2.2), F12.5)
    !   9003 format(I4, 2('/', I2.2), A, es11.3e3)
    ! end subroutine


    ! module subroutine basin_wb_soilzone(this, model_soil, model_srunoff, model_time, basin_bal, soil_in)
    !   implicit none

    !   class(WaterBalance), intent(inout) :: this
    !   type(Soilzone), intent(in) :: model_soil
    !   type(Srunoff), intent(in) :: model_srunoff
    !   type(Time_t), intent(in) :: model_time
    !   real(r64), intent(in) :: basin_bal
    !   real(r64), intent(in) :: soil_in

    !   ! Local variables
    !   real(r64) :: basin_capillary_wb
    !   real(r64) :: basin_gravity_wb
    !   real(r64) :: basin_soilzone_wb
    !   real(r64) :: bsmbal


    !   ! Soilzone
    !   ! basin_capwaterin, basin_cap_infil_tot, basin_dncascadeflow,
    !   ! basin_dndunnianflow, basin_dninterflow, basin_dunnian, basin_dunnian_gvr,
    !   ! basin_dunnian_pfr, basin_gvr2pfr, basin_lakeevap, basin_lakeinsz,
    !   ! basin_perv_et, basin_prefflow, basin_pref_flow_infil, basin_pref_stor,
    !   ! basin_slowflow, basin_slstor, basin_sm2gvr, basin_sm2gvr_max,
    !   ! basin_soil_moist, basin_soil_to_gw, basin_ssflow, basin_ssin,
    !   ! basin_ssstor, basin_swale_et, basin_sz2gw, last_soil_moist, last_ssstor

    !   ! Srunoff
    !   ! basin_infil

    !   ! Time
    !   ! Nowday, Nowmonth, Nowtime, Nowyear
    !   ! ------------------------------------------------------------------------
    !   associate(basin_capwaterin => model_soil%basin_capwaterin, &
    !             basin_cap_infil_tot => model_soil%basin_cap_infil_tot, &
    !             basin_dncascadeflow => model_soil%basin_dncascadeflow, &
    !             basin_dndunnianflow => model_soil%basin_dndunnianflow, &
    !             basin_dninterflow => model_soil%basin_dninterflow, &
    !             basin_dunnian => model_soil%basin_dunnian, &
    !             basin_dunnian_gvr => model_soil%basin_dunnian_gvr, &
    !             basin_dunnian_pfr => model_soil%basin_dunnian_pfr, &
    !             basin_gvr2pfr => model_soil%basin_gvr2pfr, &
    !             basin_lakeevap => model_soil%basin_lakeevap, &
    !             basin_lakeinsz => model_soil%basin_lakeinsz, &
    !             basin_perv_et => model_soil%basin_perv_et, &
    !             basin_prefflow => model_soil%basin_prefflow, &
    !             basin_pref_flow_infil => model_soil%basin_pref_flow_infil, &
    !             basin_pref_stor => model_soil%basin_pref_stor, &
    !             basin_slowflow => model_soil%basin_slowflow, &
    !             basin_slstor => model_soil%basin_slstor, &
    !             basin_sm2gvr => model_soil%basin_sm2gvr, &
    !             basin_sm2gvr_max => model_soil%basin_sm2gvr_max, &
    !             basin_soil_moist => model_soil%basin_soil_moist, &
    !             basin_soil_to_gw => model_soil%basin_soil_to_gw, &
    !             basin_ssflow => model_soil%basin_ssflow, &
    !             basin_ssin => model_soil%basin_ssin, &
    !             basin_ssstor => model_soil%basin_ssstor, &
    !             basin_swale_et => model_soil%basin_swale_et, &
    !             basin_sz2gw => model_soil%basin_sz2gw, &
    !             last_soil_moist => model_soil%last_soil_moist, &
    !             last_ssstor => model_soil%last_ssstor, &

    !             basin_infil => model_srunoff%basin_infil, &

    !             nowday => model_time%Nowday, &
    !             nowmonth => model_time%Nowmonth, &
    !             nowtime => model_time%Nowtime, &
    !             nowyear => model_time%Nowyear)

    !     basin_capillary_wb = last_soil_moist - basin_soil_moist - basin_perv_et - &
    !                          basin_sm2gvr_max + basin_cap_infil_tot - basin_soil_to_gw
    !     basin_gravity_wb = last_ssstor - basin_ssstor + basin_sm2gvr - basin_dncascadeflow - &
    !                        basin_ssflow - basin_sz2gw - basin_dunnian + basin_dunnian_pfr - &
    !                        basin_swale_et + basin_pref_flow_infil
    !     basin_soilzone_wb = basin_infil + last_ssstor - basin_ssstor + last_soil_moist - &
    !                         basin_soil_moist - basin_perv_et - basin_swale_et - &
    !                         basin_sz2gw - basin_soil_to_gw - basin_ssflow - &
    !                         basin_dunnian - basin_lakeinsz

    !     if (dabs(basin_gravity_wb) > DTOOSMALL) then
    !       write(this%bal_unit, 9113) nowtime(1:3), ' basin gvrbal issue', basin_gravity_wb, last_ssstor, &
    !                        basin_ssstor, basin_sm2gvr, basin_ssflow, basin_sz2gw, &
    !                        basin_dunnian, basin_swale_et, basin_pref_flow_infil, &
    !                        basin_dninterflow, basin_pref_stor, basin_dunnian_pfr, &
    !                        basin_lakeinsz, basin_dncascadeflow, basin_dunnian_gvr, &
    !                        basin_slowflow, basin_prefflow, basin_gvr2pfr
    !       9113 format(I4, 2('/', I2.2), A, 18es11.3e3)
    !     endif

    !     9111 format(I4, 2('/', I2.2), A, 9es11.3e3)

    !     if (dabs(basin_capillary_wb) > DTOOSMALL) then
    !       write(this%bal_unit, 9111) nowtime(1:3), ' Possible basin capillary balance issue', &
    !                        basin_capillary_wb, last_soil_moist, basin_soil_moist, &
    !                        basin_perv_et, basin_sm2gvr, basin_cap_infil_tot, &
    !                        basin_soil_to_gw, basin_sm2gvr_max, basin_capwaterin
    !     endif

    !     if (dabs(basin_soilzone_wb) > DTOOSMALL) then
    !       write(this%bal_unit, 9112) nowtime(1:3), ' Possible basin soil zone rounding issue', &
    !                        basin_soilzone_wb, basin_capwaterin, basin_pref_flow_infil, &
    !                        basin_infil, last_ssstor, basin_ssstor, last_soil_moist, &
    !                        basin_soil_moist, basin_perv_et, basin_swale_et, basin_sz2gw, &
    !                        basin_soil_to_gw, basin_ssflow, basin_dunnian, basin_dncascadeflow, &
    !                        basin_sm2gvr, basin_lakeinsz, basin_dunnian_pfr
    !     endif
    !     9112 format(I4, 2('/', I2.2), A, 18es11.3e3)

    !     bsmbal = last_soil_moist - basin_soil_moist + last_ssstor - basin_ssstor - &
    !              basin_perv_et - basin_sz2gw + soil_in - basin_ssflow - &
    !              basin_soil_to_gw - basin_dunnian - basin_swale_et - basin_lakeinsz

    !     write(this%sz_unit, 9002) nowyear, nowmonth, nowday, basin_bal, bsmbal, last_soil_moist, &
    !                         basin_soil_moist, last_ssstor, basin_ssstor, basin_perv_et, &
    !                         basin_sz2gw, basin_ssflow, basin_soil_to_gw, basin_dunnian, &
    !                         soil_in, basin_lakeinsz, basin_dncascadeflow, basin_swale_et, &
    !                         basin_prefflow, basin_dunnian_pfr, basin_pref_stor, &
    !                         basin_slstor, basin_dunnian_gvr, basin_lakeevap

    !     if (dabs(bsmbal) > 0.05_dp .or. dabs(basin_bal) > 0.001_dp) then
    !       write(this%bal_unit, *) '*ERROR, soilzone basin water balance', bsmbal, basin_bal, &
    !                        last_soil_moist, basin_soil_moist, last_ssstor, basin_ssstor, &
    !                        basin_perv_et, basin_sz2gw, soil_in, basin_ssflow, &
    !                        basin_soil_to_gw, basin_dunnian, basin_swale_et, basin_lakeinsz
    !       write(this%bal_unit, *) basin_pref_stor, basin_slstor
    !     elseif (dabs(bsmbal) > 0.005_dp .or. dabs(basin_bal) > DTOOSMALL) then
    !       write(this%bal_unit, *) 'Possible soilzone basin water balance ERROR', bsmbal, &
    !                        basin_bal, last_soil_moist, basin_soil_moist, last_ssstor, &
    !                        basin_ssstor, basin_perv_et, basin_sz2gw, soil_in, &
    !                        basin_ssflow, basin_soil_to_gw, basin_dunnian, &
    !                        basin_swale_et, basin_lakeinsz
    !       write(this%bal_unit, *) basin_pref_stor, basin_slstor
    !     elseif (dabs(bsmbal) > 0.0005_dp .or. dabs(basin_bal) > DTOOSMALL) then
    !       write(this%bal_unit, '(A,2F12.7)') 'Basin soilzone rounding issue', bsmbal, basin_bal
    !       write(this%bal_unit, *) basin_soilzone_wb, basin_ssin, basin_dninterflow, &
    !                        basin_sm2gvr, basin_capwaterin, soil_in, basin_gvr2pfr, &
    !                        basin_dndunnianflow, (soil_in - basin_infil)
    !     endif
    !   end associate

    !   9002 format (I5, 2('/', I2.2), 23F11.5)
    !   ! 9003 format (A, I5, 2('/', I2.2), F12.5)
    ! end subroutine


    ! module subroutine basin_wb_srunoff(this, ctl_data, model_srunoff, model_time, basin_robal)
    !   implicit none

    !   class(WaterBalance), intent(inout) :: this
    !   type(Control), intent(in) :: ctl_data
    !   type(Srunoff), intent(in) :: model_srunoff
    !   type(Time_t), intent(in) :: model_time
    !   real(r64), intent(in) :: basin_robal

    !   ! Local variables
    !   real(r64) :: brobal

    !   ! Control
    !   ! cascade_flag,

    !   ! Srunoff
    !   ! basin_dprst_evap, basin_dprst_seep, basin_dprst_sroff, basin_imperv_evap,
    !   ! basin_imperv_stor, basin_infil, basin_sroff, basin_sroffi, basin_sroffp,
    !   ! basin_sroff_down, basin_hortonian_lakes

    !   ! Time
    !   ! Nowday, Nowmonth, Nowyear

    !   ! ------------------------------------------------------------------------
    !   associate(cascade_flag => ctl_data%cascade_flag%value, &
    !             dprst_flag => ctl_data%dprst_flag%value, &

    !             basin_dprst_evap => model_srunoff%basin_dprst_evap, &
    !             basin_dprst_seep => model_srunoff%basin_dprst_seep, &
    !             basin_dprst_sroff => model_srunoff%basin_dprst_sroff, &
    !             basin_imperv_evap => model_srunoff%basin_imperv_evap, &
    !             basin_imperv_stor => model_srunoff%basin_imperv_stor, &
    !             basin_infil => model_srunoff%basin_infil, &
    !             basin_sroff => model_srunoff%basin_sroff, &
    !             basin_sroffi => model_srunoff%basin_sroffi, &
    !             basin_sroffp => model_srunoff%basin_sroffp, &
    !             basin_sroff_down => model_srunoff%basin_sroff_down, &
    !             basin_hortonian_lakes => model_srunoff%basin_hortonian_lakes, &

    !             nowtime => model_time%nowtime, &
    !             nowday => model_time%Nowday, &
    !             nowmonth => model_time%Nowmonth, &
    !             nowyear => model_time%Nowyear)

    !     brobal = basin_sroff - basin_sroffp - basin_sroffi

    !     if (dprst_flag == 1) then
    !       brobal = brobal - basin_dprst_sroff
    !     end if

    !     if (cascade_flag > 0) then
    !       brobal = brobal + basin_sroff_down
    !       write(this%sro_unit, 9002) nowtime(1:3), basin_robal, brobal, &
    !                            basin_sroff, basin_infil, basin_imperv_evap, &
    !                            basin_imperv_stor, basin_dprst_evap, basin_dprst_seep, &
    !                            basin_sroffp, basin_sroffi, basin_dprst_sroff, &
    !                            basin_sroff_down, basin_hortonian_lakes
    !     else
    !       if (dprst_flag == 1) then
    !         write(this%sro_unit, 9002) nowtime(1:3), basin_robal, brobal, &
    !                             basin_sroff, basin_infil, basin_imperv_evap, &
    !                             basin_imperv_stor, basin_sroffp, basin_sroffi, &
    !                             basin_dprst_evap, basin_dprst_seep, basin_dprst_sroff
    !       else
    !         write(this%sro_unit, 9002) nowtime(1:3), basin_robal, brobal, &
    !                             basin_sroff, basin_infil, basin_imperv_evap, &
    !                             basin_imperv_stor, basin_sroffp, basin_sroffi
    !       end if
    !     endif

    !     if (dabs(basin_robal) > DSMALL) then
    !       write(this%bal_unit, 9003) nowtime(1:3), ' Possible srunoff basin water balance ERROR ', basin_robal
    !     elseif (dabs(basin_robal) > DTOOSMALL) then
    !       write(this%bal_unit, 9003) nowtime(1:3), ' Possible srunoff basin water balance rounding issue ', basin_robal
    !     endif
    !   end associate

    !   9002 format (I4, 2('/', I2.2), 2F11.7, 23F11.5)
    !   9003 format (I4, 2('/', I2.2), A, es11.3e3)
    !   ! 9003 format (A, I5, 2('/', I2.2), F12.5)
    ! end subroutine

    module subroutine cleanup_WaterBalance(this)
      class(WaterBalance) :: this
        !! Srunoff class

      logical :: is_opened

       inquire(UNIT=this%bal_unit, OPENED=is_opened)
       if (is_opened) then
         close(this%bal_unit)
       end if

      inquire(UNIT=this%gw_unit, OPENED=is_opened)
       if (is_opened) then
         close(this%gw_unit)
       end if

      inquire(UNIT=this%intcp_unit, OPENED=is_opened)
       if (is_opened) then
         close(this%intcp_unit)
       end if

      inquire(UNIT=this%snow_unit, OPENED=is_opened)
       if (is_opened) then
         close(this%snow_unit)
       end if

      inquire(UNIT=this%sro_unit, OPENED=is_opened)
       if (is_opened) then
         close(this%sro_unit)
       end if

      inquire(UNIT=this%sz_unit, OPENED=is_opened)
       if (is_opened) then
         close(this%sz_unit)
       end if

    end subroutine

end submodule
