submodule (PRMS_WATER_BALANCE) sm_water_balance
  contains
    module function constructor_Balance(ctl_data, param_data) result(this)
      type(WaterBalance) :: this
        !! WaterBalance class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters

      ! Control
      ! nhru,

      ! gwflow
      ! basin_gwstor, gwres_stor, hru_storage


      ! ------------------------------------------------------------------------
      associate()

        allocate(this%hru_storage_ante(nhru))
        allocate(this%gwstor_ante(nhru))

        ! CALL PRMS_open_module_file(INTCPUNT, 'intcp.wbal')
        ! write ( INTCPUNT, 9003 )
        !
        ! CALL PRMS_open_module_file(SNOWUNIT, 'snowcomp.wbal')
        ! write ( SNOWUNIT, 9007 )
        !
        ! CALL PRMS_open_module_file(SROUNIT, MODNAME//'.wbal')
        ! if ( cascade_flag>0 ) then
        !   write ( SROUNIT, 9006 )
        ! else
        !   write ( SROUNIT, 9005 )
        ! endif
        !
        ! CALL PRMS_open_module_file(SZUNIT, 'soilzone.wbal')
        ! write ( SZUNIT, 9001 )
        !
        ! CALL PRMS_open_module_file(GWUNIT, 'gwflow.wbal')
        ! write ( GWUNIT, 9004 )
        !
        ! CALL PRMS_open_module_file(BALUNT, 'wbal.msgs')

        ! 9001 format ('    Date     Water Bal     bsmbal    last SM  soilmoist  last stor    SS stor    perv ET      sz2gw  interflow', &
        !              '    soil2gw    dunnian    soil in   lakeinsz   downflow   swale ET  pref flow   pfr dunn   pfr stor', &
        !              '  slow stor dunnian gvr lake evap')
        ! 9003 format ('    Date     Water Bal     Precip     Netppt  Intcpevap  Intcpstor  last_stor changeover  net apply     apply')
        ! 9004 format ('    Date     Water Bal last store  GWR store', &
        !              '   GW input    GW flow    GW sink GW upslope minarea_in   downflow')
        ! 9005 format ('    Date     Water Bal     Robal      Sroff      Infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
        !              '   Perv Sro Imperv Sro  Dprst Sro')
        ! 9006 format ('    Date     Water Bal     Robal      Sroff      Infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
        !              '   Perv Sro Imperv Sro  Dprst Sro  Sroffdown  Srofflake')
        ! 9007 format ('    Date     Water Bal  Snowpack    snowmelt   Snowevap  Snowcover' )

        this%basin_capillary_wb = 0.0_dp
        this%basin_gravity_wb = 0.0_dp
        this%basin_soilzone_wb = 0.0_dp
        this%basin_dprst_wb = 0.0_dp
        this%last_basin_gwstor = basin_gwstor
        this%gwstor_ante = gwres_stor
        this%hru_storage_ante = hru_storage
      end associate
    end function

    module subroutine run_WaterBalance(this, ctl_data, param_data, model_basin, &
                                       model_climate, model_potet, intcp, snow)
      use prms_constants, only: LAKE
      implicit none

      class(WaterBalance), intent(inout) :: this
        !! WaterBalance class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      ! type(Flowvars), intent(in) :: model_flow
      class(Potential_ET), intent(in) :: model_potet
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow

      ! Local Variables
      integer(i32) :: chru
      integer(i32) :: kk
      real(r32) :: last_sm
      real(r32) :: last_ss
      real(r32) :: soilbal
      real(r32) :: perv_frac
      real(r32) :: gvrbal
      real(r32) :: test
      real(r32) :: waterin
      real(r32) :: waterout
      real(r32) :: hrubal
      real(r32) :: delstor
      real(r32) :: robal

      real(r64) :: basin_bal
      real(r64) :: bsmbal
      real(r64) :: soil_in
      real(r64) :: gwbal
      real(r64) :: gwup
      real(r64) :: basin_robal
      real(r64) :: bsnobal
      real(r64) :: hru_out
      real(r64) :: hru_in
      real(r64) :: wbal
      real(r64) :: delta_stor
      real(r64) :: pptbal
      real(r64) :: brobal
      real(r64) :: dprst_hru_wb
      real(r64) :: harea

      ! Control
      ! cascade_flag, cascadegw_flag, dprst_flag,

      ! Parameters
      ! cov_type, hru_type, snow_intcp, srain_intcp, wrain_intcp,

      ! Basin
      ! active_hrus, hru_area_dble, hru_frac_perv, hru_route_order,

      ! Climateflow
      ! pkwater_equiv,

      ! Gwflow
      ! gwminarea_flag,

      ! Intcp
      ! canopy_covden, gain_inches, hru_intcpevap, hru_intcpstor, intcp_changeover,
      ! intcp_evap, intcp_stor,
      ! intcp_stor_ante, net_apply,
      ! net_ppt, net_rain, net_snow,
      ! use_transfer_intcp,

      ! Precipitation
      ! hru_rain, hru_snow,

      ! Snowcomp
      ! pkwater_ante, snowmelt, snow_evap,

      ! srunoff
      ! use_sroff_transfer,

      ! ------------------------------------------------------------------------

      Basin_capillary_wb = 0.0_dp
      Basin_gravity_wb = 0.0_dp
      basin_soilzone_wb = 0.0_dp
      basin_bal = 0.0_dp
      soil_in = 0.0_dp
      basin_robal = 0.0_dp
      bsnobal = 0.0_dp

      do kk=1, active_hrus
        chru = hru_route_order(kk)

        if (hru_type(chru) == LAKE) cycle ! no water balance for lakes

        harea = hru_area_dble(chru)
        perv_frac = hru_frac_perv(chru)

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! intcp
        delstor = hru_intcpstor(chru) - intcp_stor_ante(chru)
        hrubal = hru_rain(chru) + hru_snow(chru) - net_rain(chru) - &
                 net_snow(chru) - delstor - hru_intcpevap(chru)

        if (use_transfer_intcp == 1) then
          hrubal = hrubal + gain_inches(chru) - net_apply(chru)
        endif

        if (abs(hrubal) > TOOSMALL) then
          if (abs(hrubal) > SMALL) then
            write(BALUNT, *) 'Possible HRU interception water balance error'
          else
            write(BALUNT, *) 'Interception HRU rounding issue'
          endif

          write(BALUNT,'(I7,6I5,15F10.5,I5)') chru, Nowtime, hrubal, &
                net_rain(chru), net_snow(chru), hru_rain(chru), hru_snow(chru), &
                intcp_stor(chru), intcp_stor_ante(chru), intcp_evap(chru), srain_intcp(chru), &
                wrain_intcp(chru), snow_intcp(chru), canopy_covden(chru), delstor, &
                hru_intcpstor(chru), intcp_changeover(chru), cov_type(chru)

          if (use_transfer_intcp == 1) then
            write(BALUNT, *) gain_inches(chru), net_apply(chru)
          endif
        endif

        ! Skip the HRU if there is no snowpack and no new snow
        if (pkwater_ante(chru) > DNEARZERO .or. Newsnow(chru) == 1) then
          hrubal = sngl(pkwater_ante(chru) - pkwater_equiv(chru)) - snow_evap(chru) - snowmelt(chru)

          if (Pptmix_nopack(chru) == 1) then
            hrubal = hrubal + net_snow(chru)
          else
            hrubal = hrubal + net_ppt(chru)
          endif

          if (abs(hrubal) > TOOSMALL) then
            if (abs(hrubal) > SMALL) then
              write(BALUNT, *) 'Possible snow water balance error'
            else
              write(BALUNT, *) 'Possible HRU snow rounding issue'
            endif

            write(BALUNT, *) chru, hrubal, Nowyear, Nowmonth, Nowday, &
                             pkwater_ante(chru), pkwater_equiv(chru), &
                             snow_evap(chru), snowmelt(chru), net_ppt(chru), &
                             net_snow(chru), net_rain(chru), Newsnow(chru), &
                             Pptmix(chru), Pptmix_nopack(chru)
          endif

          bsnobal = bsnobal + dble(hrubal) * harea
        endif

        ! Includes dprst runoff, if any
        robal = snowmelt(chru) - Hortonian_flow(chru) - Infil(chru) * perv_frac - &
                Hru_impervevap(chru) + Imperv_stor_ante(chru) - Hru_impervstor(chru)

        if (use_sroff_transfer == 1) then
          robal = robal + net_apply(chru) * perv_frac
        endif

        if (net_ppt(chru) > 0.0) then
          if (Pptmix_nopack(chru) == 1) then
            robal = robal + net_rain(chru)
          elseif (snowmelt(chru) < NEARZERO .and. pkwater_equiv(chru) < DNEARZERO) then
            if (snow_evap(chru) < NEARZERO) then
              robal = robal + net_ppt(chru)
            elseif (net_snow(chru) < NEARZERO) then
              robal = robal + net_rain(chru)
            endif
          endif
        endif

        if (cascade_flag > 0) then
          robal = robal + sngl(Upslope_hortonian(chru) - Hru_hortn_cascflow(chru))
        endif

        if (dprst_flag == 1) then
          robal = robal - Dprst_evap_hru(chru) + sngl(Dprst_stor_ante(chru) - Dprst_stor_hru(chru) - Dprst_seep_hru(chru))
        endif

        basin_robal = basin_robal + dble(robal)

        if (abs(robal) > TOOSMALL) then
          if (dprst_flag == 1) then
            dprst_hru_wb = Dprst_stor_ante(chru) - Dprst_stor_hru(chru) - &
                           Dprst_seep_hru(chru) - Dprst_sroff_hru(chru) + Dprst_in(chru) - &
                           dble(Dprst_evap_hru(chru)) + dble(Dprst_insroff_hru(chru))
            Basin_dprst_wb = Basin_dprst_wb + dprst_hru_wb * harea

            write(BALUNT, *) 'dprst', dprst_hru_wb, Dprst_stor_hru(chru), &
                             Dprst_stor_hru(chru), Dprst_seep_hru(chru), &
                             Dprst_evap_hru(chru), Dprst_sroff_hru(chru), &
                             snowmelt(chru), net_rain(chru), Dprst_insroff_hru(chru)

            write(BALUNT, *) Dprst_vol_open(chru), Dprst_vol_clos(chru), &
                             (Dprst_vol_open(chru) + Dprst_vol_clos(chru)) / harea, &
                             Dprst_area_max(chru), pkwater_equiv(chru), &
                             Dprst_area_clos(chru), Snowcov_area(chru), &
                             Dprst_in(chru), Sro_to_dprst_perv(chru), Hru_sroffp(chru), &
                             Hru_sroffi(chru)

            write(BALUNT, *) robal, net_rain(chru), net_ppt(chru), &
                             net_rain(chru) * Dprst_frac(chru), Dprst_frac(chru), &
                             Pptmix_nopack(chru)

            write(BALUNT, *) Infil(chru), perv_frac, Hru_impervevap(chru), &
                             Imperv_stor_ante(chru), Hru_impervstor(chru), &
                             Hru_percent_imperv(chru), Dprst_sroff_hru(chru)
          endif

          if (abs(robal) > SMALL) then
            write(BALUNT, *) 'Possible HRU surface runoff water balance ERROR', chru, &
                             ' hru_type:', hru_type(chru)
          else
            write(BALUNT, *) 'HRU surface runoff rounding issue', chru, &
                             ' hru_type:', hru_type(chru)
          endif

          if (cascade_flag > 0) then
            write(BALUNT, '(3I3,F10.6,17F10.4)') Nowmonth, Nowday, Pptmix_nopack(chru), &
                                                 robal, snowmelt(chru), Upslope_hortonian(chru), &
                                                 Imperv_stor_ante(chru), Hru_hortn_cascflow(chru), &
                                                 Infil(chru), Hortonian_flow(chru), &
                                                 Hru_impervstor(chru), Hru_impervevap(chru), &
                                                 net_ppt(chru), pkwater_equiv(chru), &
                                                 snow_evap(chru), net_snow(chru), &
                                                 net_rain(chru), Hru_sroffp(chru), &
                                                 Hru_sroffi(chru), harea
          else
            write(BALUNT,'(3I3,F10.6,15F10.5,F10.3)') Nowmonth, Nowday, Pptmix_nopack(chru), &
                                                      robal, snowmelt(chru), Imperv_stor_ante(chru), &
                                                      Infil(chru), Hortonian_flow(chru), &
                                                      Hru_impervstor(chru), Hru_impervevap(chru), &
                                                      Hru_percent_imperv(chru), net_ppt(chru), &
                                                      pkwater_equiv(chru), snow_evap(chru), &
                                                      net_snow(chru), net_rain(chru), &
                                                      Hru_sroffp(chru), Hru_sroffi(chru), harea
          endif
        endif

        last_sm = Soil_moist_ante(chru)
        last_ss = Ssres_stor_ante(chru)

        soilbal = (last_sm - Soil_moist(chru) - Perv_actet(chru)) * perv_frac - &
                  Soil_to_ssr(chru) - Soil_to_gw(chru) + Cap_infil_tot(chru)

        if (abs(soilbal) > TOOSMALL) then
          write(BALUNT, *) 'HRU capillary problem'
          write(BALUNT, *) soilbal, Cap_infil_tot(chru), last_sm, Soil_moist(chru), &
                           Perv_actet(chru), Soil_to_ssr(chru), Soil_to_gw(chru), &
                           chru, Infil(chru), Pref_flow_infil(chru), perv_frac, &
                           Soil_moist_max(chru), Cap_waterin(chru)

          if (cascade_flag > 0) then
            write(BALUNT, *) 'UP cascade', Upslope_interflow(chru), Upslope_dunnianflow(chru)
          endif
        endif

        gvrbal = last_ss - Ssres_stor(chru) + Soil_to_ssr(chru) - Ssr_to_gw(chru) - &
                 Swale_actet(chru) - Dunnian_flow(chru) - Ssres_flow(chru) + &
                 Pfr_dunnian_flow(chru) + Pref_flow_infil(chru)

        if (cascade_flag > 0) then
          gvrbal = gvrbal - Hru_sz_cascadeflow(chru)
        endif

        test = abs(gvrbal)
        if (test > TOOSMALL) then
          write(BALUNT, *) 'Bad GVR balance, HRU:', chru, ' hru_type:', hru_type(chru)
          write(BALUNT, *) gvrbal, last_ss, Ssres_stor(chru), Ssr_to_gw(chru), &
                           Swale_actet(chru), Dunnian_flow(chru), Ssres_flow(chru), &
                           Pfr_dunnian_flow(chru), Pref_flow_thrsh(chru), Ssres_in(chru), &
                           Pref_flow_infil(chru), Grav_dunnian_flow(chru), &
                           Slow_flow(chru), Pref_flow(chru), Soil_to_ssr(chru), &
                           Gvr2pfr(chru), perv_frac, Slow_stor(chru), &
                           Pref_flow_stor(chru), Infil(chru), Pref_flow_max(chru), &
                           Pref_flow_den(chru)

          if (cascade_flag > 0) then
            write(BALUNT, *) 'sz cascade', Hru_sz_cascadeflow(chru)
          endif
        endif

        waterin = Cap_infil_tot(chru) + Pref_flow_infil(chru) + Pfr_dunnian_flow(chru)
        waterout = Ssr_to_gw(chru) + Ssres_flow(chru) + Soil_to_gw(chru) + &
                   Swale_actet(chru) + Perv_actet(chru) * perv_frac + Dunnian_flow(chru)

        if (cascade_flag > 0) then
          waterout = waterout + Hru_sz_cascadeflow(chru)
        endif

        soil_in = soil_in + dble(Infil(chru) * perv_frac) * harea
        soilbal = waterin - waterout + last_ss - Ssres_stor(chru) + &
                  (last_sm - Soil_moist(chru)) * perv_frac
        basin_bal = basin_bal + dble(soilbal) * harea

        test = abs(soilbal)
        if (test > TOOSMALL) then
          write(BALUNT, *) 'HRU:', chru, ' hru_type:', hru_type(chru)

          if (test > BAD) then
            write(BALUNT, *) 'HRU soilzone water balance ***ERROR***'
          elseif (test > SMALL) then
            write(BALUNT, *) 'Possible soilzone HRU water balance ERROR'
          else
            write(BALUNT, *) 'Possible soilzone HRU water balance rounding issue'
          endif

          write(BALUNT, 9001) Nowyear, Nowmonth, Nowday, chru, soilbal, Infil(chru), &
                              last_sm, last_ss, Soil_moist(chru), Ssres_stor(chru), &
                              Perv_actet(chru), Ssr_to_gw(chru), Slow_flow(chru), &
                              Pref_flow(chru), Ssres_flow(chru), Soil_to_gw(chru), &
                              Pref_flow_infil(chru), Pref_flow_stor(chru), &
                              Slow_stor(chru), Soil_rechr(chru), Soil_lower(chru), &
                              Soil_to_ssr(chru), Ssres_flow(chru), waterin, Swale_actet(chru)

          if (cascade_flag > 0) then
            write(BALUNT, *) 'cascade', Upslope_dunnianflow(chru), Upslope_interflow(chru), &
                             Hru_sz_cascadeflow(chru), Ncascade_hru(chru)
          endif

          write(BALUNT, *) Hru_perv(chru), perv_frac, Pref_flow_den(chru), &
                           (Infil(chru) * perv_frac), Cap_infil_tot(chru)
          write(BALUNT, *) Dunnian_flow(chru), Pfr_dunnian_flow(chru)
        endif

        hru_out = dble(Sroff(chru) + Gwres_flow(chru) + Ssres_flow(chru) + Hru_actet(chru) + Gwres_sink(chru))
        hru_in = dble(Hru_ppt(chru))

        if (cascade_flag > 0) then
          hru_out = hru_out + dble(Hru_sz_cascadeflow(chru)) + Hru_hortn_cascflow(chru)
          hru_in = hru_in + Upslope_dunnianflow(chru) + Upslope_interflow(chru) + Upslope_hortonian(chru)
        endif

        if (cascadegw_flag > 0) then
          hru_out = hru_out + Hru_gw_cascadeflow(chru)
          hru_in = hru_in + Gw_upslope(chru) / dble(harea)
        endif

        wbal = hru_in - hru_out + Hru_storage_ante(chru) - Hru_storage(chru)

        if (gwminarea_flag == 1) then
          wbal = wbal + Gwstor_minarea_wb(chru)
        endif

        if (dabs(wbal) > DTOOSMALL) then
          write(BALUNT, *) 'Possible HRU water balance issue:', wbal, &
                           '; HRU:', chru, ' hru_type:', hru_type(chru), &
                           '; area:', harea
          write(BALUNT, *) Sroff(chru), Gwres_flow(chru), Ssres_flow(chru), &
                           Hru_actet(chru), Gwres_sink(chru), Hru_storage_ante(chru), &
                           Hru_storage(chru), hru_type(chru),  Pfr_dunnian_flow(chru)

          write(BALUNT, *) Soil_moist_tot(chru), hru_intcpstor(chru), Gwres_stor(chru), &
                           pkwater_equiv(chru), Hru_impervstor(chru)

          if (cascade_flag > 0) then
            write(BALUNT, *) Hru_sz_cascadeflow(chru), Upslope_dunnianflow(chru), &
                             Upslope_interflow(chru)
            write(BALUNT, *) Upslope_hortonian(chru), Hru_hortn_cascflow(chru)
          endif

          if (cascadegw_flag > 0) then
            write(BALUNT, *) Gw_upslope(chru) / dble(harea), Hru_gw_cascadeflow(chru)
          endif

          write(BALUNT, *) Nowtime
        endif

        wbal = Gwstor_ante(chru) + Gwres_in(chru) / harea - Gwres_stor(chru) - &
               dble(Gwres_sink(chru) + Gwres_flow(chru))

        if (cascadegw_flag > 0) then
          wbal = wbal - Hru_gw_cascadeflow(chru)
        endif

        if (gwminarea_flag == 1) then
          wbal = wbal + Gwstor_minarea_wb(chru)
        endif

        gwup = 0.0_dp
        if (cascadegw_flag > 0) then
          gwup = Gw_upslope(chru)
        endif

        if (dabs(wbal) > DTOOSMALL) then
          write(BALUNT, *) 'Possible GWR water balance issue', &
                           chru, wbal, Gwstor_ante(chru), Gwres_in(chru) / harea, &
                           Gwres_stor(chru), Gwres_flow(chru), Gwres_sink(chru), &
                           Soil_to_gw(chru), Ssr_to_gw(chru), gwup, harea

          if (cascadegw_flag > 0) then
            write(BALUNT, *) 'gw cascade', Hru_gw_cascadeflow(chru)
          endif

          if (gwminarea_flag == 1) then
            write(BALUNT, *) 'gwstor_minarea_wb', Gwstor_minarea_wb(chru)
          endif

          if (dprst_flag == 1) then
            write(BALUNT, *) 'gwin_dprst', Gwin_dprst(chru)
          endif
        endif
        Hru_storage_ante(chru) = Hru_storage(chru)
        Gwstor_ante(chru) = Gwres_stor(chru)
      enddo

      Basin_dprst_wb = Basin_dprst_wb * Basin_area_inv

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! intcp
      delta_stor = Basin_intcp_stor - Last_intcp_stor
      pptbal = Basin_ppt - Basin_net_ppt - delta_stor - Basin_intcp_evap

      if (use_sroff_transfer == 1) then
        pptbal = pptbal + Basin_net_apply
      endif

      if (dabs(pptbal) > DSMALL) then
        write(BALUNT, 9003) 'Possible basin interception water balance error', &
                            Nowyear, Nowmonth, Nowday, pptbal
      elseif (dabs(pptbal) > DTOOSMALL) then
        write(BALUNT, 9003) 'Interception basin rounding issue', &
                            Nowyear, Nowmonth, Nowday, pptbal
      endif

      write(INTCPUNT, 9002) Nowyear, Nowmonth, Nowday, pptbal, Basin_ppt, &
                            Basin_net_ppt, Basin_intcp_evap, Basin_intcp_stor, &
                            Last_intcp_stor, Basin_changeover, Basin_net_apply, &
                            Basin_hru_apply

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! snowcomp
      bsnobal = bsnobal * Basin_area_inv
      if (dabs(bsnobal) > DSMALL) then
        write(BALUNT, 9003) 'Possible basin snow water balance error', &
                            Nowyear, Nowmonth, Nowday, bsnobal
      elseif (dabs(bsnobal) > DTOOSMALL) then
        write(BALUNT, 9003) 'Possible basin snow rounding issue', &
                            Nowyear, Nowmonth, Nowday, bsnobal
      endif

      write(SNOWUNIT, 9002) Nowyear, Nowmonth, Nowday, bsnobal, Basin_pweqv, &
                            Basin_snowmelt, Basin_snowevap, Basin_snowcov

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! srunoff
      brobal = Basin_sroff - Basin_sroffp - Basin_sroffi - Basin_dprst_sroff

      if (cascade_flag > 0) then
        brobal = brobal + Basin_sroff_down
        write(SROUNIT, 9002) Nowyear, Nowmonth, Nowday, basin_robal, brobal, &
                             Basin_sroff, Basin_infil, Basin_imperv_evap, &
                             Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
                             Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, &
                             Basin_sroff_down, Basin_hortonian_lakes
      else
        write(SROUNIT, 9002) Nowyear, Nowmonth, Nowday, basin_robal, brobal, &
                             Basin_sroff, Basin_infil, Basin_imperv_evap, &
                             Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
                             Basin_sroffp, Basin_sroffi, Basin_dprst_sroff
      endif

      if (dabs(basin_robal) > DSMALL) then
        write(BALUNT, 9003) 'possible srunoff basin water balance ERROR', &
                            Nowyear, Nowmonth, Nowday, basin_robal
      elseif (dabs(basin_robal) > DTOOSMALL) then
        write(BALUNT, 9003) 'possible srunoff basin water balance rounding issue', &
                            Nowyear, Nowmonth, Nowday, basin_robal
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! soilzone
      Basin_capillary_wb = Last_soil_moist - Basin_soil_moist - Basin_perv_et - &
                           Basin_sm2gvr_max + Basin_cap_infil_tot - Basin_soil_to_gw
      Basin_gravity_wb = Last_ssstor - Basin_ssstor + Basin_sm2gvr - Basin_dncascadeflow - &
                         Basin_ssflow - Basin_sz2gw - Basin_dunnian + Basin_dunnian_pfr - &
                         Basin_swale_et + Basin_pref_flow_infil
      Basin_soilzone_wb = Basin_infil + Last_ssstor - Basin_ssstor + Last_soil_moist - &
                          Basin_soil_moist - Basin_perv_et - Basin_swale_et - &
                          Basin_sz2gw - Basin_soil_to_gw - Basin_ssflow - &
                          Basin_dunnian - Basin_lakeinsz

      if (dabs(Basin_gravity_wb) > DTOOSMALL) then
        write(BALUNT, *) 'basin gvrbal issue', Basin_gravity_wb, Last_ssstor, &
                         Basin_ssstor, Basin_sm2gvr, Basin_ssflow, Basin_sz2gw, &
                         Basin_dunnian, Basin_swale_et, Basin_pref_flow_infil, &
                         Basin_dninterflow, Basin_pref_stor, Basin_dunnian_pfr, &
                         Basin_lakeinsz, Basin_dncascadeflow, Basin_dunnian_gvr, &
                         Basin_slowflow, Basin_prefflow, Basin_gvr2pfr, Nowtime
      endif

      if (dabs(Basin_capillary_wb) > DTOOSMALL) then
        write(BALUNT, *) 'possible basin capillary balance issue', &
                         Basin_capillary_wb, Last_soil_moist, Basin_soil_moist, &
                         Basin_perv_et, Basin_sm2gvr, Basin_cap_infil_tot, &
                         Basin_soil_to_gw, Basin_sm2gvr_max, Basin_capwaterin, &
                         Nowtime
      endif

      if (dabs(Basin_soilzone_wb) > DTOOSMALL) then
        write(BALUNT, *) 'possible basin soil zone rounding issue', &
                         Basin_soilzone_wb, Basin_capwaterin, Basin_pref_flow_infil, &
                         Basin_infil, Last_ssstor, Basin_ssstor, Last_soil_moist, &
                         Basin_soil_moist, Basin_perv_et, Basin_swale_et, Basin_sz2gw, &
                         Basin_soil_to_gw, Basin_ssflow, Basin_dunnian, Basin_dncascadeflow, &
                         Basin_sm2gvr, Basin_lakeinsz, Basin_dunnian_pfr, Nowtime
      endif

      soil_in = soil_in * Basin_area_inv
      basin_bal = basin_bal * Basin_area_inv
      bsmbal = Last_soil_moist - Basin_soil_moist + Last_ssstor - Basin_ssstor - &
               Basin_perv_et - Basin_sz2gw + soil_in - Basin_ssflow - &
               Basin_soil_to_gw - Basin_dunnian - Basin_swale_et - Basin_lakeinsz

      write(SZUNIT, 9002) Nowyear, Nowmonth, Nowday, basin_bal, bsmbal, Last_soil_moist, &
                          Basin_soil_moist, Last_ssstor, Basin_ssstor, Basin_perv_et, &
                          Basin_sz2gw, Basin_ssflow, Basin_soil_to_gw, Basin_dunnian, &
                          soil_in, Basin_lakeinsz, Basin_dncascadeflow, Basin_swale_et, &
                          Basin_prefflow, Basin_dunnian_pfr, Basin_pref_stor, &
                          Basin_slstor, Basin_dunnian_gvr, Basin_lakeevap

      if (dabs(bsmbal) > 0.05_dp .or. dabs(basin_bal) > 0.001_dp) then
        write(BALUNT, *) '*ERROR, soilzone basin water balance', bsmbal, basin_bal, &
                         Last_soil_moist, Basin_soil_moist, Last_ssstor, Basin_ssstor, &
                         Basin_perv_et, Basin_sz2gw, soil_in, Basin_ssflow, &
                         Basin_soil_to_gw, Basin_dunnian, Basin_swale_et, Basin_lakeinsz
        write(BALUNT, *) Basin_pref_stor, Basin_slstor
      elseif (dabs(bsmbal) > 0.005_dp .or. dabs(basin_bal) > DTOOSMALL) then
        write(BALUNT, *) 'Possible soilzone basin water balance ERROR', bsmbal, &
                         basin_bal, Last_soil_moist, Basin_soil_moist, Last_ssstor, &
                         Basin_ssstor, Basin_perv_et, Basin_sz2gw, soil_in, &
                         Basin_ssflow, Basin_soil_to_gw, Basin_dunnian, &
                         Basin_swale_et, Basin_lakeinsz
        write(BALUNT, *) Basin_pref_stor, Basin_slstor
      elseif (dabs(bsmbal) > 0.0005_dp .or. dabs(basin_bal) > DTOOSMALL) then
        write(BALUNT, '(A,2F12.7)') 'Basin soilzone rounding issue', bsmbal, basin_bal
        write(BALUNT, *) Basin_soilzone_wb, Basin_ssin, Basin_dninterflow, &
                         Basin_sm2gvr, Basin_capwaterin, soil_in, Basin_gvr2pfr, &
                         Basin_dndunnianflow, (soil_in - Basin_infil)
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! gwflow

      ! Not going to balance because gwstor under lakes is computed each
      ! time step fix for lakes.
      ! basin_gwin includes upslope flow, gwin_dprst, soil_to_gw, ssr_to_gw
      gwbal = Basin_gwin + Last_basin_gwstor - Basin_gwstor - Basin_gwsink - &
              Basin_gwflow - Basin_dnflow + Basin_gwstor_minarea_wb

      if (dabs(gwbal) > DSMALL) then
        write(BALUNT, 9003) 'Possible GWR basin water balance issue', &
                            Nowyear, Nowmonth, Nowday, gwbal
      endif

      write (GWUNIT, 9002) Nowyear, Nowmonth, Nowday, gwbal, Last_basin_gwstor, &
                           Basin_gwstor, Basin_gwin, Basin_gwflow, Basin_gwsink, &
                           Basin_gw_upslope, Basin_gwstor_minarea_wb, Basin_dnflow
      Last_basin_gwstor = Basin_gwstor

      9001 format (I5, 2('/', I2.2), I7, 26F11.5)
      9002 format (I5, 2('/', I2.2), 23F11.5)
      9003 format (A, I5, 2('/', I2.2), F12.5)

    end subroutine
end submodule
