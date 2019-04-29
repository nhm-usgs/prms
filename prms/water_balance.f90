!***********************************************************************
! Computes water balance for components of a PRMS model
!***********************************************************************
      MODULE PRMS_WATER_BALANCE
        IMPLICIT NONE
!   Local Variables
        INTEGER, SAVE :: BALUNT, SZUNIT, GWUNIT, INTCPUNT, SROUNIT, SNOWUNIT
        REAL, PARAMETER :: TOOSMALL = 3.1E-05, SMALL = 1.0E-04, BAD = 1.0E-03
        DOUBLE PRECISION, PARAMETER :: DSMALL = 1.0D-4, DTOOSMALL = 1.0D-05
        DOUBLE PRECISION, SAVE :: Last_basin_gwstor, Basin_dprst_wb
        DOUBLE PRECISION, ALLOCATABLE, SAVE :: Hru_storage_ante(:), Gwstor_ante(:)
        CHARACTER(LEN=13), SAVE :: MODNAME_WB
!   Declared Variables
        DOUBLE PRECISION, SAVE :: Basin_capillary_wb, Basin_gravity_wb, Basin_soilzone_wb
      END MODULE PRMS_WATER_BALANCE

!***********************************************************************
!***********************************************************************
      SUBROUTINE water_balance()
      USE PRMS_WATER_BALANCE, ONLY: SZUNIT, BALUNT, INTCPUNT, GWUNIT, SROUNIT, SNOWUNIT
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      EXTERNAL :: water_balance_decl, water_balance_init, water_balance_run
!***********************************************************************
      IF ( Process(:3)=='run' ) THEN
        CALL water_balance_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        CALL water_balance_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        CALL water_balance_init()
      ELSEIF ( Process(:5)=='clean' ) THEN
        CLOSE ( SZUNIT )
        CLOSE ( BALUNT )
        CLOSE ( INTCPUNT )
        CLOSE ( GWUNIT )
        CLOSE ( SROUNIT )
        CLOSE ( SNOWUNIT )
      ENDIF

      END SUBROUTINE water_balance

!***********************************************************************
!***********************************************************************
      SUBROUTINE water_balance_decl()
      USE PRMS_WATER_BALANCE
      USE PRMS_MODULE, ONLY: Model, Nhru, Cascade_flag, Dprst_flag, Soilzone_flag
      USE PRMS_SRUNOFF, ONLY: MODNAME
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar
      EXTERNAL :: read_error, print_module, PRMS_open_module_file
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_water_balance
!***********************************************************************
      Version_water_balance = '$Id: water_balance.f90 7224 2015-03-06 16:23:48Z rsregan $'
      CALL print_module(Version_water_balance, 'Water Balance Computations  ', 90 )
      MODNAME_WB = 'water_balance'

! Declare Variables
      IF ( Soilzone_flag==1 ) THEN
      IF ( declvar(MODNAME_WB, 'basin_capillary_wb', 'one', 1, 'double', &
     &     'Basin area-weighted average capillary reservoir storage', &
     &     'inches', Basin_capillary_wb)/=0 ) CALL read_error(3, 'basin_capillary_wb')

      IF ( declvar(MODNAME_WB, 'basin_gravity_wb', 'one', 1, 'double', &
     &     'Basin area-weighted average gravity reservoir storage', &
     &     'inches', Basin_gravity_wb)/=0 ) CALL read_error(3, 'basin_gravity_wb')

      IF ( declvar(MODNAME_WB, 'basin_soilzone_wb', 'one', 1, 'double', &
     &     'Basin area-weighted average storage in soilzone reservoirs', &
     &     'inches', Basin_soilzone_wb)/=0 ) CALL read_error(3, 'basin_soilzone_wb')

      ALLOCATE ( Hru_storage_ante(Nhru) )
      ENDIF

      IF ( Dprst_flag==1 ) THEN
        IF ( declvar(MODNAME_WB, 'basin_dprst_wb', 'one', 1, 'double', &
     &       'Basin area-weighted average surface-depression storage', &
     &       'inches', Basin_dprst_wb)/=0 ) CALL read_error(3, 'basin_dprst_wb')
      ENDIF

      ALLOCATE ( Gwstor_ante(Nhru) )

      IF ( Model/=99 ) THEN
        CALL PRMS_open_module_file(INTCPUNT, 'intcp.wbal')
        WRITE ( INTCPUNT, 9003 )

        CALL PRMS_open_module_file(SNOWUNIT, 'snowcomp.wbal')
        WRITE ( SNOWUNIT, 9007 )

        CALL PRMS_open_module_file(SROUNIT, MODNAME//'.wbal')
        IF ( Cascade_flag==1 ) THEN
          WRITE ( SROUNIT, 9006 )
        ELSE
          WRITE ( SROUNIT, 9005 )
        ENDIF

        IF ( Soilzone_flag==1 ) THEN
        CALL PRMS_open_module_file(SZUNIT, 'soilzone.wbal')
        IF ( Model==1 ) THEN
          WRITE ( SZUNIT, 9001 )
        ELSE
          WRITE ( SZUNIT, 9002 )
        ENDIF
        ENDIF

        IF ( Model/=0 ) THEN
          CALL PRMS_open_module_file(GWUNIT, 'gwflow.wbal')
          WRITE ( GWUNIT, 9004 )
        ENDIF

        CALL PRMS_open_module_file(BALUNT, 'wbal.msgs')
      ENDIF

 9001 FORMAT ('    Date     Water Bal     bsmbal    last SM  soilmoist  last stor    SS stor    perv ET      sz2gw  interflow', &
     &        '    soil2gw    dunnian    soil in   lakeinsz   downflow   swale ET  pref flow   pfr dunn   pfr stor', &
     &        '  slow stor dunnian gvr lake evap')
 9002 FORMAT ('    Date     Water Bal     bsmbal    last SM  soilmoist  last stor    SS stor    perv ET      sz2gw  interflow', &
     &        '    soil2gw    dunnian    soil in   lakeinsz   downflow   swale ET  pref flow   pfr dunn   pfr stor', &
     &        '  slow stor dunnian gvr lake evap     gvr2sm       gwin  iteration')
 9003 FORMAT ('    Date     Water Bal     Precip     Netppt  Intcpevap  Intcpstor  last_stor changeover')
 9004 FORMAT ('    Date     Water Bal last store  GWR store', &
              '   GW input    GW flow    GW sink    farflow GW upslope minarea_in   downflow')
 9005 FORMAT ('    Date     Water Bal     Robal      Sroff      Infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
     &        '   Perv Sro Imperv Sro  Dprst Sro')
 9006 FORMAT ('    Date     Water Bal     Robal      Sroff      Infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
     &        '   Perv Sro Imperv Sro  Dprst Sro  Sroffdown  Srofflake')
 9007 FORMAT ('    Date     Water Bal  Snowpack    Snowmelt   Snowevap  Snowcover' )

      END SUBROUTINE water_balance_decl

!***********************************************************************
!     szinit - Initialize soilzone module - get parameter values,
!              set initial values and check parameter values
!***********************************************************************
      SUBROUTINE water_balance_init()
      USE PRMS_WATER_BALANCE
      USE PRMS_MODULE, ONLY: Model, Soilzone_flag
      USE PRMS_GWFLOW, ONLY: Basin_gwstor, Hru_storage, Gwres_stor
!***********************************************************************
      Basin_capillary_wb = 0.0D0
      Basin_gravity_wb = 0.0D0
      Basin_soilzone_wb = 0.0D0
      Basin_dprst_wb = 0.0D0

      IF ( Model/=0 ) THEN
        Last_basin_gwstor = Basin_gwstor
        Gwstor_ante = Gwres_stor
        IF ( Soilzone_flag==1 ) Hru_storage_ante = Hru_storage
      ENDIF

      END SUBROUTINE water_balance_init

!***********************************************************************
!     water_balance_run - Computes balance for each HRU and model domain
!***********************************************************************
      SUBROUTINE water_balance_run()
      USE PRMS_WATER_BALANCE
      USE PRMS_MODULE, ONLY: Model, Cascade_flag, Cascadegw_flag, Dprst_flag, Kkiter, Soilzone_flag, Gw_rejected, Basin_szreject
      USE PRMS_BASIN, ONLY: Hru_route_order, Active_hrus, Hru_frac_perv, Hru_area, Hru_perv, &
     &    Hru_type, Basin_area_inv, NEARZERO, Dprst_area_max, Hru_frac_imperv, Hru_frac_dprst, Cov_type
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Basin_ppt, Hru_rain, Hru_snow, Newsnow, Pptmix
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Basin_ssstor, Soil_to_gw, Soil_to_ssr, &
     &    Infil, Soil_moist_max, Ssr_to_gw, Ssres_flow, Basin_soil_to_gw, Soil_moist, Ssres_stor, &
     &    Slow_flow, Basin_perv_et, Basin_ssflow, Basin_swale_et, Slow_stor, Ssres_in, Soil_rechr, &
     &    Basin_lakeevap, Sroff, Hru_actet, Pkwater_equiv
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday, Nowtime
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_INTCP, ONLY: Hru_intcpstor, Basin_net_ppt, Basin_intcp_evap, Basin_changeover, &
     &    Basin_intcp_stor, Net_rain, Net_snow, Hru_intcpevap, &
     &    Srain_intcp, Wrain_intcp, Snow_intcp, Intcp_stor, Intcp_evap, &
     &    Canopy_covden, Intcp_changeover, Net_ppt, Intcp_stor_ante, Last_intcp_stor
      USE PRMS_SNOW, ONLY: Snowmelt, Pptmix_nopack, Snow_evap, Snowcov_area, Basin_pweqv, &
     &    Basin_snowmelt, Basin_snowevap, Basin_snowcov, Pkwater_ante
      USE PRMS_SRUNOFF, ONLY: Basin_infil, Hru_hortn_cascflow, Upslope_hortonian, Hru_impervstor, &
     &    Dprst_stor_hru, Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, Basin_sroff_down, &
     &    Basin_sroff_farflow, Basin_hortonian_lakes, Basin_imperv_evap, Basin_imperv_stor, &
     &    Basin_dprst_evap, Basin_dprst_seep, Basin_sroff, Hru_impervevap, Dprst_seep_hru, &
     &    Dprst_evap_hru, Dprst_sroff_hru, Dprst_insroff_hru, Dprst_vol_open, Dprst_vol_clos, &
     &    Sro_to_dprst, Dprst_area_clos, Hortonian_flow, Dprst_in, Hru_sroffp, Hru_sroffi, Imperv_stor_ante, Dprst_stor_ante
      USE PRMS_SOILZONE, ONLY: Gvr2sm, Swale_actet, Dunnian_flow, Basin_sz2gw, &
     &    Perv_actet, Cap_infil_tot, Pref_flow_infil, Cap_waterin, Upslope_interflow, &
     &    Upslope_dunnianflow, Pref_flow, Pref_flow_stor, Soil_lower, Basin_sz_gwin, Gvr2pfr, Basin_ssin, &
     &    Basin_szfarflow, Basin_lakeinsz, Basin_dunnian, Pref_flow_max, Pref_flow_den, Pref_flow_thrsh, &
     &    Basin_sm2gvr_max, Basin_cap_infil_tot, Basin_gvr2sm, Basin_slowflow, &
     &    Basin_dunnian_gvr, Basin_pref_flow_infil, Basin_dninterflow, Basin_pref_stor, Basin_dunnian_pfr, &
     &    Basin_dncascadeflow, Basin_capwaterin, Basin_sm2gvr, Basin_prefflow, Basin_slstor, Basin_gvr2pfr, &
     &    Hru_sz_cascadeflow, Pfr_dunnian_flow, Grav_gwin, Grav_dunnian_flow, Basin_dndunnianflow, &
     &    Soil_moist_tot, Soil_moist_ante, Ssres_stor_ante, Last_soil_moist, Last_ssstor
      USE PRMS_GWFLOW, ONLY: Basin_dnflow, Basin_gwsink, Basin_farflow, Basin_gwstor_minarea_wb, Gwres_flow, &
     &    Basin_gwstor, Basin_gwflow, Basin_gw_upslope, Basin_gwin, Gwres_sink, Hru_gw_cascadeflow, Gw_upslope, &
     &    Gwminarea_flag, Gwstor_minarea_wb, Gwin_dprst, Gw_in_soil, Gw_in_ssr, Gwres_in, Hru_storage, Gwres_stor
      IMPLICIT NONE
! Functions
      INTRINSIC ABS
      EXTERNAL print_date
! Local Variables
      INTEGER :: i, k
      REAL :: last_sm, last_ss, soilbal, perv_frac, gvrbal, test, waterin, waterout, hrubal, dprst_hru_wb
      REAL :: harea, hru_out, hru_in, wbal,  delta_stor, pptbal, delstor, robal, gvr2sm_val, gwin
      DOUBLE PRECISION :: basin_bal, bsmbal, soil_in, gwbal, gwup, basin_robal, bsnobal
!***********************************************************************
      Basin_capillary_wb = 0.0D0
      Basin_gravity_wb = 0.0D0
      basin_soilzone_wb = 0.0D0
      basin_bal = 0.0D0
      soil_in = 0.0D0
      basin_robal = 0.0D0
      bsnobal = 0.0D0
      DO k = 1, Active_hrus
        i = Hru_route_order(k)

        IF ( Hru_type(i)==2 ) CYCLE ! no water balance for lakes

        harea = Hru_area(i)
        perv_frac = Hru_frac_perv(i)

        ! intcp
        delstor = Hru_intcpstor(i) - Intcp_stor_ante(i)
        hrubal = Hru_rain(i) + Hru_snow(i) - Net_rain(i) - Net_snow(i) &
     &           - delstor - Hru_intcpevap(i) ! + Intcp_changeover(i)
        IF ( ABS(hrubal)>TOOSMALL ) THEN
          IF ( ABS(hrubal)>SMALL ) THEN
            WRITE ( BALUNT, * ) 'Possible HRU interception water balance error'
          ELSE
            WRITE ( BALUNT, * ) 'Interception HRU rounding issue'
          ENDIF
          WRITE ( BALUNT,'(I7,6I5,15F10.5,I5)' ) i, Nowtime, hrubal, &
     &            Net_rain(i), Net_snow(i), Hru_rain(i), Hru_snow(i), &
     &            Intcp_stor(i), Intcp_stor_ante(i), Intcp_evap(i), Srain_intcp(i), &
     &            Wrain_intcp(i), Snow_intcp(i), Canopy_covden(i), delstor, &
     &            Hru_intcpstor(i), Intcp_changeover(i), Cov_type(i)
        ENDIF

        ! Skip the HRU if there is no snowpack and no new snow
        IF ( Pkwater_ante(i)>NEARZERO .OR. Newsnow(i)==1 ) THEN
          hrubal = Pkwater_ante(i) - Pkwater_equiv(i) - Snow_evap(i) - Snowmelt(i)
          IF ( Pptmix_nopack(i)==1 ) THEN
            hrubal = hrubal + Net_snow(i)
          ELSE
            hrubal = hrubal + Net_ppt(i)
          ENDIF
          IF ( ABS(hrubal)>TOOSMALL ) THEN
            IF ( ABS(hrubal)>SMALL ) THEN
              WRITE ( BALUNT, * ) 'Possible snow water balance error'
            ELSE
              WRITE ( BALUNT, * ) 'Possible HRU snow rounding issue'
            ENDIF
            WRITE ( BALUNT,* ) i, hrubal, Nowyear, Nowmonth, Nowday, &
     &              Pkwater_ante(i), Pkwater_equiv(i), Snow_evap(i), &
     &              Snowmelt(i), Net_ppt(i), Net_snow(i), Net_rain(i), &
     &              Newsnow(i), Pptmix(i), Pptmix_nopack(i)
          ENDIF
          bsnobal = bsnobal + hrubal*harea
        ENDIF

        robal = Snowmelt(i) - Hortonian_flow(i) & !includes dprst runoff, if any
     &          - Infil(i)*perv_frac - Hru_impervevap(i) + Imperv_stor_ante(i) - Hru_impervstor(i)
        IF ( Net_ppt(i)>0.0 ) THEN
          IF ( Pptmix_nopack(i)==1 ) THEN
            robal = robal + Net_rain(i)
          ELSEIF ( Snowmelt(i)<NEARZERO .AND. Pkwater_equiv(i)<NEARZERO) THEN
            IF ( Snow_evap(i)<NEARZERO ) THEN
              robal = robal + Net_ppt(i)
            ELSEIF ( Net_snow(i)<NEARZERO ) THEN
              robal = robal + Net_rain(i)
            ENDIF
            !IF ( Net_snow(i)<NEARZERO ) robal = robal + Net_rain(i)
          ENDIF
        ENDIF
        IF ( Cascade_flag==1 ) robal = robal + Upslope_hortonian(i) - Hru_hortn_cascflow(i)
        IF ( Dprst_flag==1 ) robal = robal - Dprst_seep_hru(i) - Dprst_evap_hru(i) &
     &                               + Dprst_stor_ante(i) - Dprst_stor_hru(i) !- Dprst_in(i) - Dprst_insroff_hru(i)
        basin_robal = basin_robal + robal
        IF ( ABS(robal)>TOOSMALL ) THEN
          IF ( Dprst_flag==1 ) THEN
            dprst_hru_wb = Dprst_stor_ante(i) - Dprst_stor_hru(i) - Dprst_seep_hru(i) - Dprst_sroff_hru(i) &
     &                     - Dprst_evap_hru(i) + Dprst_insroff_hru(i) + Dprst_in(i)
            Basin_dprst_wb = Basin_dprst_wb + dprst_hru_wb*harea
            WRITE ( BALUNT, * ) 'dprst', dprst_hru_wb, Dprst_stor_hru(i), Dprst_stor_hru(i), &
     &              Dprst_seep_hru(i), Dprst_evap_hru(i), Dprst_sroff_hru(i), Snowmelt(i), Net_rain(i), &
     &              Dprst_insroff_hru(i), Upslope_hortonian(i)
            WRITE ( BALUNT, * ) Dprst_vol_open(i), Dprst_vol_clos(i), &
     &              (Dprst_vol_open(i)+Dprst_vol_clos(i))/harea, Dprst_area_max(i), Pkwater_equiv(i), &
     &              Dprst_area_clos(i), Snowcov_area(i), Dprst_in(i), Sro_to_dprst(i)
            WRITE ( BALUNT, * ) robal, Net_rain(i), Net_ppt(i), Net_rain(i)*Hru_frac_dprst(i), &
     &              Hru_frac_dprst(i), Pptmix_nopack(i)
            WRITE ( BALUNT, * ) Infil(i), perv_frac, Hru_impervevap(i), Imperv_stor_ante(i), Hru_impervstor(i), &
     &                          Hru_frac_imperv(i), Dprst_sroff_hru(i)
          ENDIF
          IF ( ABS(robal)>SMALL ) THEN
            WRITE ( BALUNT, * ) 'Possible HRU surface runoff water balance ERROR', i
          ELSE
            WRITE ( BALUNT, * ) 'HRU surface runoff rounding issue', i
          ENDIF
          IF ( Cascade_flag==1 ) THEN
            WRITE ( BALUNT, '(3I3,F10.6,17F10.5)' ) Nowmonth, Nowday, Pptmix_nopack(i), robal, Snowmelt(i), &
     &              Upslope_hortonian(i), Imperv_stor_ante(i), Hru_hortn_cascflow(i), Infil(i), Hortonian_flow(i), &
     &              Hru_impervstor(i), Hru_impervevap(i), Net_ppt(i), &
     &              Pkwater_equiv(i), Snow_evap(i), Net_snow(i), Net_rain(i), Hru_sroffp, Hru_sroffi, harea
          ELSE
            WRITE ( BALUNT,'(3I3,F10.6,16F10.5)' ) Nowmonth, Nowday, Pptmix_nopack(i), &
     &              robal, Snowmelt(i), Imperv_stor_ante(i), Infil(i), &
     &              Hortonian_flow(i), Hru_impervstor(i), Hru_impervevap(i), Hru_frac_imperv(i), Net_ppt(i), &
     &              Pkwater_equiv(i), Snow_evap(i), Net_snow(i), Net_rain(i), Hru_sroffp(i), Hru_sroffi(i), harea
          ENDIF
        ENDIF

        IF ( Soilzone_flag==1 ) THEN
        last_sm = Soil_moist_ante(i)
        last_ss = Ssres_stor_ante(i)

        gwin = 0.0
        gvr2sm_val = 0.0
        IF ( Model==0 ) THEN
          gwin = Grav_gwin(i)
          gvr2sm_val = Gvr2sm(i)
        ENDIF
        soilbal = (last_sm - Soil_moist(i) - Perv_actet(i))*perv_frac &
     &            - Soil_to_ssr(i) - Soil_to_gw(i) + gvr2sm_val + Cap_infil_tot(i)

        IF ( ABS(soilbal)>TOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'HRU capillary problem'
          WRITE ( BALUNT, * ) soilbal, Cap_infil_tot(i), last_sm, Soil_moist(i), Perv_actet(i), Soil_to_ssr(i), &
     &                        Soil_to_gw(i), i, Infil(i), Pref_flow_infil(i), perv_frac, &
     &                        Soil_moist_max(i), Cap_waterin(i), Soil_to_ssr(i)
          IF ( Cascade_flag==1 ) WRITE ( BALUNT, * ) 'UP cascade', Upslope_interflow(i), Upslope_dunnianflow(i)
          IF ( Model==0 ) WRITE ( BALUNT, * ) 'gsflow', gvr2sm_val
        ENDIF
        gvrbal = last_ss - Ssres_stor(i) + Soil_to_ssr(i) - Ssr_to_gw(i) - Swale_actet(i) - Dunnian_flow(i) &
     &           - Ssres_flow(i) + Pfr_dunnian_flow(i) + Pref_flow_infil(i)
        IF ( Cascade_flag==1 ) gvrbal = gvrbal - Hru_sz_cascadeflow(i)
        IF ( Model==0 ) gvrbal = gvrbal + gwin - gvr2sm_val + Gw_rejected(i)
        test = ABS( gvrbal )
        IF ( test>TOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'Bad GVR balance, HRU:', i, ' hru_type:', Hru_type(i)
          WRITE ( BALUNT, * ) gvrbal, last_ss, Ssres_stor(i), Ssr_to_gw(i), Swale_actet(i), &
     &            Dunnian_flow(i), Ssres_flow(i), Pfr_dunnian_flow(i), Pref_flow_thrsh(i), Ssres_in(i), &
     &            Pref_flow_infil(i), Grav_dunnian_flow(i), Slow_flow(i), Pref_flow(i), Soil_to_ssr(i), Gvr2pfr(i), &
     &            perv_frac, Slow_stor(i), Pref_flow_stor(i), Infil(i), Pref_flow_max(i), Pref_flow_den(i)
          IF ( Cascade_flag==1 ) WRITE ( BALUNT, * ) 'sz cascade', Hru_sz_cascadeflow(i)
          IF ( Model==0 ) WRITE ( BALUNT, * ) 'gsflow', gwin, gvr2sm_val, Gw_rejected(i), Kkiter
        ENDIF

        waterin = Cap_waterin(i) + Pref_flow_infil(i) + Pfr_dunnian_flow(i) + gwin
        waterout = Ssr_to_gw(i) + Ssres_flow(i) + Soil_to_gw(i) + Swale_actet(i) + Perv_actet(i)*perv_frac &
     &             + Dunnian_flow(i)   
        IF ( Cascade_flag==1 ) waterout = waterout + Hru_sz_cascadeflow(i)
        soil_in = soil_in + (Infil(i)*perv_frac + gwin)*harea
        IF ( Model==0 ) THEN
          waterin = waterin + Gw_rejected(i)
          soil_in = soil_in + Gw_rejected(i)*harea
        ENDIF
        soilbal = waterin - waterout + last_ss - Ssres_stor(i) + (last_sm-Soil_moist(i))*perv_frac
        basin_bal = basin_bal + soilbal*harea
        test = ABS( soilbal )
        IF ( test>TOOSMALL ) THEN
!          IF ( test>Ssres_stor(i)*TOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'HRU:', i, ' Hru_type:', Hru_type(i)
          IF ( test>BAD ) THEN
            WRITE ( BALUNT, * ) 'HRU soilzone water balance ***ERROR***'
          ELSEIF ( test>SMALL ) THEN
            WRITE ( BALUNT, * ) 'Possible soilzone HRU water balance ERROR'
          ELSE
            WRITE ( BALUNT, * ) 'Possible soilzone HRU water balance rounding issue'
          ENDIF
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, i, soilbal, Infil(i), last_sm, last_ss, &
     &            Soil_moist(i), Ssres_stor(i), Perv_actet(i), Ssr_to_gw(i), Slow_flow(i), Pref_flow(i), Ssres_flow(i), &
     &            Soil_to_gw(i), Pref_flow_infil(i), Pref_flow_stor(i), Slow_stor(i), Soil_rechr(i), &
     &            Soil_lower(i), Soil_to_ssr(i), Ssres_flow(i), waterin, Swale_actet(i), gwin, gvr2sm_val
          IF ( Cascade_flag==1 ) WRITE ( BALUNT, * ) 'cascade', Upslope_dunnianflow(i), Upslope_interflow(i), &
     &                                               Hru_sz_cascadeflow(i), Ncascade_hru(i)
          WRITE ( BALUNT, * ) Hru_perv(i), perv_frac, Pref_flow_den(i), (Infil(i)*perv_frac + gwin), Cap_infil_tot(i)
          WRITE ( BALUNT, * ) Dunnian_flow(i), Pfr_dunnian_flow(i)
!          ENDIF
        ENDIF

        IF ( Model/=0 ) THEN
          hru_out = Sroff(i) + Gwres_flow(i) + Ssres_flow(i) + Hru_actet(i) + Gwres_sink(i)
          hru_in = Hru_ppt(i)
          IF ( Cascade_flag==1 ) THEN
            hru_out = hru_out + Hru_sz_cascadeflow(i) + Hru_hortn_cascflow(i)
            hru_in = hru_in + Upslope_dunnianflow(i) + Upslope_interflow(i) + Upslope_hortonian(i)
          ENDIF
          IF ( Cascadegw_flag==1 ) THEN
            hru_out = hru_out + Hru_gw_cascadeflow(i)
            hru_in = hru_in + Gw_upslope(i)/harea
          ENDIF
          wbal = hru_in - hru_out + Hru_storage_ante(i) - Hru_storage(i)
          IF ( Gwminarea_flag==1 ) wbal = wbal + Gwstor_minarea_wb(i)
          IF ( ABS(wbal)>TOOSMALL ) THEN
            PRINT *, 'Possible HRU water balance issue:', wbal, '; HRU:', i, '; area:', harea
            PRINT *, Sroff(i), Gwres_flow(i), Ssres_flow(i), Hru_actet(i), Gwres_sink(i), &
     &               Hru_storage_ante(i), Hru_storage(i), Hru_type(i)
            !PRINT *, Gwstor_minarea_wb(i)
            PRINT *, Soil_moist_tot(i), Hru_intcpstor(i), Gwres_stor(i), Pkwater_equiv(i), Hru_impervstor(i)
            IF ( Cascade_flag==1 ) THEN
              PRINT *, Hru_sz_cascadeflow(i), Upslope_dunnianflow(i), Upslope_interflow(i)
              PRINT *, Upslope_hortonian(i), Hru_hortn_cascflow(i)
            ENDIF
            IF ( Cascadegw_flag==1 ) PRINT *, Gw_upslope(i)/harea, Hru_gw_cascadeflow(i)
            CALL print_date(0)
          ENDIF
          ENDIF

          wbal = Gwstor_ante(i) + Gwres_in(i)/harea - Gwres_stor(i) - Gwres_sink(i) - Gwres_flow(i)
          IF ( Cascadegw_flag==1 ) wbal = wbal - Hru_gw_cascadeflow(i)
          IF ( Gwminarea_flag==1 ) wbal = wbal + Gwstor_minarea_wb(i)
          gwup = 0.0D0
          IF ( Cascadegw_flag==1 ) gwup = Gw_upslope(i)
          IF ( ABS(wbal)>TOOSMALL ) THEN
            WRITE ( BALUNT, * ) 'Possible GWR water balance issue', &
     &                          i, wbal, Gwstor_ante(i), Gwres_in(i)/harea, Gwres_stor(i), Gwres_flow(i), &
     &                          Gwres_sink(i), Gw_in_soil(i), Gw_in_ssr(i), gwup, harea
            IF ( Cascadegw_flag==1 ) WRITE ( BALUNT, * ) 'gw cascade', Hru_gw_cascadeflow(i)
            IF ( Gwminarea_flag==1 ) WRITE ( BALUNT, * ) 'gwstor_minarea_wb', Gwstor_minarea_wb(i)
            IF ( Dprst_flag==1 ) WRITE ( BALUNT, * ) 'gwin_dprst', Gwin_dprst(i)
          ENDIF
          IF ( Soilzone_flag==1 ) Hru_storage_ante(i) = Hru_storage(i)
          Gwstor_ante(i) = Gwres_stor(i)
        ENDIF
      ENDDO
      Basin_dprst_wb = Basin_dprst_wb*Basin_area_inv

! intcp
      delta_stor = Basin_intcp_stor - Last_intcp_stor
      pptbal = Basin_ppt - Basin_net_ppt - delta_stor - Basin_intcp_evap
      IF ( ABS(pptbal)>SMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Possible basin interception water balance error', &
     &                         Nowyear, Nowmonth, Nowday, pptbal
      ELSEIF ( ABS( pptbal )>TOOSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Interception basin rounding issue', &
     &                         Nowyear, Nowmonth, Nowday, pptbal
      ENDIF
      WRITE ( INTCPUNT, 9002 ) Nowyear, Nowmonth, Nowday, pptbal, &
     &        Basin_ppt, Basin_net_ppt, Basin_intcp_evap, &
     &        Basin_intcp_stor, Last_intcp_stor, Basin_changeover

! snowcomp
      bsnobal = bsnobal*Basin_area_inv
      IF ( ABS(bsnobal)>SMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Possible basin snow water balance error', &
     &                         Nowyear, Nowmonth, Nowday, bsnobal
      ELSEIF ( ABS(bsnobal)>TOOSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Possible basin snow rounding issue', &
     &                         Nowyear, Nowmonth, Nowday, bsnobal
      ENDIF
      WRITE ( SNOWUNIT, 9002 ) Nowyear, Nowmonth, Nowday, bsnobal, Basin_pweqv, &
     &                         Basin_snowmelt, Basin_snowevap, Basin_snowcov

! srunoff
      robal = Basin_sroff - Basin_sroffp - Basin_sroffi - Basin_dprst_sroff
      IF ( Cascade_flag==1 ) THEN
        robal = robal + Basin_sroff_down + Basin_sroff_farflow
        WRITE ( SROUNIT, 9002 ) Nowyear, Nowmonth, Nowday, basin_robal, &
     &          robal, Basin_sroff, Basin_infil, Basin_imperv_evap, &
     &          Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
     &          Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, &
     &          Basin_sroff_down, Basin_hortonian_lakes
      ELSE
        WRITE ( SROUNIT, 9002 ) Nowyear, Nowmonth, Nowday, basin_robal, &
     &          robal, Basin_sroff, Basin_infil, Basin_imperv_evap, &
     &          Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
     &          Basin_sroffp, Basin_sroffi, Basin_dprst_sroff
      ENDIF
      IF ( ABS(basin_robal)>DSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'possible srunoff basin water balance ERROR', &
     &                         Nowyear, Nowmonth, Nowday, basin_robal
      ELSEIF ( ABS(basin_robal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'possible srunoff basin water balance rounding issue', &
     &                         Nowyear, Nowmonth, Nowday, basin_robal
      ENDIF

! soilzone
      IF ( Soilzone_flag==1 ) THEN
      Basin_capillary_wb = Last_soil_moist - Basin_soil_moist - &
     &                     Basin_perv_et - Basin_sm2gvr_max + Basin_cap_infil_tot + &
     &                     Basin_gvr2sm - Basin_soil_to_gw
      Basin_gravity_wb = Last_ssstor - Basin_ssstor + Basin_sm2gvr - Basin_dncascadeflow - &
     &                   Basin_ssflow - Basin_sz2gw - Basin_dunnian + Basin_dunnian_pfr - &
     &                   Basin_swale_et - Basin_gvr2sm + Basin_pref_flow_infil + Basin_sz_gwin + Basin_szreject
      Basin_soilzone_wb = Basin_infil + Basin_sz_gwin + Last_ssstor - &
     &                    Basin_ssstor + Last_soil_moist - Basin_soil_moist - &
     &                    Basin_perv_et - Basin_swale_et - Basin_sz2gw - &
     &                    Basin_soil_to_gw - Basin_ssflow - Basin_dunnian - &
     &                    Basin_szfarflow - Basin_lakeinsz + Basin_szreject

      IF ( ABS(Basin_gravity_wb)>DTOOSMALL ) WRITE ( BALUNT, * ) 'basin gvrbal issue', Basin_gravity_wb, &
     &     Last_ssstor, Basin_ssstor, Basin_sm2gvr, Basin_ssflow, Basin_sz2gw, Basin_dunnian, &
     &     Basin_swale_et, Basin_gvr2sm, Basin_pref_flow_infil, Basin_dninterflow, Basin_pref_stor, Basin_szfarflow, &
     &     Basin_dunnian_pfr, Basin_lakeinsz, Basin_dncascadeflow, Kkiter, &
     &     Basin_sz_gwin, Basin_szreject, Basin_dunnian_gvr, Basin_slowflow, Basin_prefflow, Nowtime, Basin_sroff_farflow
      IF ( ABS(Basin_capillary_wb)>DTOOSMALL ) WRITE( BALUNT, * ) 'possible basin capillary balance issue', &
     &     Basin_capillary_wb, Last_soil_moist, Basin_soil_moist, Basin_perv_et, &
     &     Basin_sm2gvr, Basin_cap_infil_tot, Basin_gvr2sm, &
     &     Basin_soil_to_gw, Basin_sm2gvr_max, Basin_capwaterin, Nowtime
      IF ( ABS(Basin_soilzone_wb)>DTOOSMALL ) WRITE ( BALUNT, * ) 'possible basin soil zone rounding issue', &
     &     Basin_soilzone_wb, Basin_capwaterin, Basin_pref_flow_infil, Basin_infil, Basin_sz_gwin, &
     &     Last_ssstor, Basin_ssstor, Last_soil_moist, Basin_soil_moist, Basin_perv_et, Basin_swale_et, &
     &     Basin_sz2gw, Basin_soil_to_gw, Basin_ssflow, Basin_dunnian, Basin_szfarflow, Basin_dncascadeflow, &
     &     Basin_gvr2sm, Basin_sm2gvr, Basin_lakeinsz, Basin_dunnian_pfr, Basin_szreject, Nowtime
      IF ( Basin_szreject>0.0 ) Basin_szreject = 0.0

      soil_in = soil_in*Basin_area_inv
      basin_bal = basin_bal*Basin_area_inv
      bsmbal = Last_soil_moist - Basin_soil_moist + Last_ssstor - Basin_ssstor - Basin_perv_et - Basin_sz2gw + soil_in - &
     &         Basin_ssflow - Basin_soil_to_gw - Basin_dunnian - Basin_szfarflow - Basin_swale_et - Basin_lakeinsz

      IF ( Model==1 ) THEN
        WRITE ( SZUNIT, 9002 ) Nowyear, Nowmonth, Nowday, basin_bal, &
     &          bsmbal, Last_soil_moist, Basin_soil_moist, Last_ssstor, &
     &          Basin_ssstor, Basin_perv_et, Basin_sz2gw, Basin_ssflow, &
     &          Basin_soil_to_gw, Basin_dunnian, soil_in, Basin_lakeinsz, &
     &          Basin_dncascadeflow, Basin_szfarflow, Basin_swale_et, &
     &          Basin_prefflow, Basin_dunnian_pfr, &
     &          Basin_pref_stor, Basin_slstor, Basin_dunnian_gvr, Basin_lakeevap
      ELSE
        WRITE ( SZUNIT, 9002 ) Nowyear, Nowmonth, Nowday, basin_bal, &
     &          bsmbal, Last_soil_moist, Basin_soil_moist, Last_ssstor, &
     &          Basin_ssstor, Basin_perv_et, Basin_sz2gw, Basin_ssflow, &
     &          Basin_soil_to_gw, Basin_dunnian, soil_in, Basin_lakeinsz, &
     &          Basin_dncascadeflow, Basin_szfarflow, Basin_swale_et, &
     &          Basin_prefflow, Basin_dunnian_pfr, &
     &          Basin_pref_stor, Basin_slstor, Basin_dunnian_gvr, &
     &          Basin_lakeevap, Basin_gvr2sm, Basin_sz_gwin, Kkiter
      ENDIF

      IF ( ABS(bsmbal)>0.05D0 .OR. ABS(basin_bal)>0.001D0 ) THEN
        WRITE ( BALUNT, * ) '*ERROR, soilzone basin water balance', bsmbal, &
     &          basin_bal, Last_soil_moist, Basin_soil_moist, &
     &          Last_ssstor, Basin_ssstor, Basin_perv_et, Basin_sz2gw, &
     &          soil_in, Basin_ssflow, Basin_soil_to_gw, Basin_dunnian, &
     &          Basin_szfarflow, Basin_swale_et, Basin_lakeinsz
        WRITE ( BALUNT, * ) Basin_pref_stor, Basin_slstor
      ELSEIF ( ABS(bsmbal)>0.005D0 .OR. ABS(basin_bal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, * ) 'Possible soilzone basin water balance ERROR', &
     &          bsmbal, basin_bal, Last_soil_moist, Basin_soil_moist, &
     &          Last_ssstor, Basin_ssstor, Basin_perv_et, Basin_sz2gw, &
     &          soil_in, Basin_ssflow, Basin_soil_to_gw, Basin_dunnian, &
     &          Basin_szfarflow, Basin_swale_et, Basin_lakeinsz
        WRITE ( BALUNT, * ) Basin_pref_stor, Basin_slstor
      ELSEIF ( ABS(bsmbal)>0.0005D0 .OR. ABS(basin_bal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, '(A,2F12.7)' ) 'Basin soilzone rounding issue', bsmbal, basin_bal
        WRITE ( BALUNT, * ) Basin_soilzone_wb, Basin_ssin, &
     &          Basin_dninterflow, Basin_sm2gvr, Basin_capwaterin, &
     &          soil_in, Basin_gvr2pfr, Basin_gvr2sm, Basin_dndunnianflow, &
     &          (soil_in - Basin_infil - Basin_sz_gwin)
      ENDIF
      ENDIF

! gwflow

      ! not going to balance because gwstor under lakes is computed each time step
      ! fix for lakes
      IF ( Model/=0 ) THEN
        ! gwin includes upslope flow, farflow, gwin_dprst, gw_in_soil, gw_in_ssr
        gwbal = Basin_gwin + Last_basin_gwstor - Basin_gwstor - Basin_gwsink &
     &          - Basin_gwflow - Basin_dnflow - Basin_farflow+ Basin_gwstor_minarea_wb
        IF ( DABS(gwbal)>DSMALL ) WRITE ( BALUNT, 9003 ) 'Possible GWR basin water balance issue', &
     &                                                   Nowyear, Nowmonth, Nowday, gwbal
        WRITE ( GWUNIT, 9002 ) Nowyear, Nowmonth, Nowday, &
     &          gwbal, Last_basin_gwstor, Basin_gwstor, Basin_gwin, &
     &          Basin_gwflow, Basin_gwsink, Basin_farflow,&
     &          Basin_gw_upslope, Basin_gwstor_minarea_wb, Basin_dnflow
        Last_basin_gwstor = Basin_gwstor
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), I7, 26F11.5)
 9002 FORMAT (I5, 2('/', I2.2), 24F11.5, I11)
 9003 FORMAT (A, I5, 2('/', I2.2), F12.6)

      END SUBROUTINE water_balance_run
