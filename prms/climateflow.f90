!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
      MODULE PRMS_CLIMATEVARS
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=11), SAVE :: MODNAME
      ! Tmax_hru and Tmin_hru are in temp_units
      REAL, SAVE, ALLOCATABLE :: Tmax_hru(:), Tmin_hru(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_elev_feet(:), Tsta_elev_meters(:)
      REAL, SAVE, ALLOCATABLE :: Psta_elev_feet(:), Psta_elev_meters(:)
      REAL, SAVE, ALLOCATABLE :: Tmax_allsnow_hru_f(:), Tmax_allrain_hru_month_f(:, :)
      REAL, SAVE :: Tmax_allrain_f(12), Tmax_allsnow_f
!   Declared Variables - Precip
      INTEGER, SAVE, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      DOUBLE PRECISION, SAVE :: Basin_ppt, Basin_rain, Basin_snow, Basin_obs_ppt
      REAL, SAVE, ALLOCATABLE :: Hru_ppt(:), Hru_rain(:), Hru_snow(:), Prmx(:)
      REAL, SAVE, ALLOCATABLE :: Tmax_rain_sta(:), Tmin_rain_sta(:)
!   Declared Variables - Temp
      DOUBLE PRECISION, SAVE :: Basin_temp, Basin_tmax, Basin_tmin
      REAL, SAVE :: Solrad_tmax, Solrad_tmin
      REAL, SAVE, ALLOCATABLE :: Tmaxf(:), Tminf(:), Tavgf(:)
      REAL, SAVE, ALLOCATABLE :: Tmaxc(:), Tminc(:), Tavgc(:)
!   Declared Variables - Transp
      INTEGER, SAVE :: Basin_transp_on
      INTEGER, SAVE, ALLOCATABLE :: Transp_on(:)
!   Declared Variables - Potetential ET
      DOUBLE PRECISION, SAVE :: Basin_potet
      REAL, SAVE, ALLOCATABLE :: Potet(:), Potet_coef_hru_mo(:, :)
!   Declared Parameters and Variables - Solar Radiation
      INTEGER, SAVE :: Basin_solsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_solsta(:)
      DOUBLE PRECISION, SAVE :: Basin_horad, Basin_potsw
      REAL, SAVE :: Radj_sppt, Radj_wppt, Radmax, Rad_conv, Orad, Ppt_rad_adj(12)
      REAL, SAVE, ALLOCATABLE :: Swrad(:)
!   Declared Parameters - Temp
      INTEGER, SAVE :: Temp_units, Basin_tsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_tsta(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_elev(:), Tmax_adj(:), Tmin_adj(:)
!   Declared Parameters - Precip
      INTEGER, SAVE :: Precip_units
      REAL, SAVE :: Tmax_allsnow, Adjmix_rain(12), Tmax_allrain(12), Adjust_snow(12), Adjust_rain(12)
      REAL, SAVE, ALLOCATABLE :: Psta_elev(:)
      REAL, SAVE, ALLOCATABLE :: Tmax_allsnow_hru(:), Tmax_allrain_hru_mo(:, :)
      END MODULE PRMS_CLIMATEVARS

!***********************************************************************
! Declares parameters and variables related to flows from soilzone,
! smbal, ssflow, srunoff_carea, srunoff_smidx
!***********************************************************************
      MODULE PRMS_FLOWVARS
      IMPLICIT NONE
!   Local Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open_max(:), Dprst_vol_clos_max(:), Dprst_vol_thres_open(:)
!   Declared Variables
      ! snow
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Pkwater_equiv(:)
      ! soilzone
      DOUBLE PRECISION, SAVE :: Basin_ssflow, Basin_soil_to_gw, Basin_actet, Basin_lakeevap
      DOUBLE PRECISION, SAVE :: Basin_swale_et, Basin_perv_et, Basin_soil_moist, Basin_ssstor
      REAL, SAVE, ALLOCATABLE :: Hru_actet(:), Soil_moist(:), Soil_to_gw(:), Slow_flow(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_ssr(:), Ssres_in(:), Ssr_to_gw(:), Slow_stor(:)
      REAL, SAVE, ALLOCATABLE :: Ssres_stor(:), Ssres_flow(:), Soil_rechr(:)
      ! srunoff
      REAL, SAVE, ALLOCATABLE :: Sroff(:), Imperv_stor(:), Infil(:)
      ! Surface-Depression Storage
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open(:), Dprst_vol_clos(:)
      ! gwflow
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwres_stor(:)
      ! lakes
      DOUBLE PRECISION, SAVE :: Basin_lake_stor, Basin_2ndstflow
      REAL, SAVE, ALLOCATABLE :: Elevlake(:)
      ! streamflow
      DOUBLE PRECISION, SAVE :: Basin_cfs, Basin_cms, Basin_ssflow_cfs, Basin_sroff_cfs
      DOUBLE PRECISION, SAVE :: Basin_stflow_in, Basin_gwflow_cfs, Basin_stflow_out
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_outflow(:), Seg_upstream_inflow(:), Seg_lateral_inflow(:)
      REAL, SAVE, ALLOCATABLE :: Seginc_ssflow(:), Seginc_sroff(:), Seginc_gwflow(:), Seginc_swrad(:)
      REAL, SAVE, ALLOCATABLE :: Seg_outflow(:), Seg_inflow(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Soil_moist_max(:), Soil_rechr_max(:), Sat_threshold(:)
      REAL, SAVE, ALLOCATABLE :: Snowinfil_max(:), Imperv_stor_max(:)
      END MODULE PRMS_FLOWVARS

!***********************************************************************
!     Main climateflow routine
!***********************************************************************
      INTEGER FUNCTION climateflow()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: climateflow_decl, climateflow_init
      EXTERNAL :: climateflow_restart
!***********************************************************************
      climateflow = 0

      IF ( Process(:4)=='decl' ) THEN
        climateflow = climateflow_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL climateflow_restart(1)
        ELSE
          climateflow = climateflow_init()
        ENDIF
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL climateflow_restart(0)
      ENDIF

      END FUNCTION climateflow

!***********************************************************************
!     climateflow_decl - declare climate and flow variables and parameters
!***********************************************************************
      INTEGER FUNCTION climateflow_decl()
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      USE PRMS_MODULE, ONLY: Temp_flag, Precip_flag, Model, Nhru, Nssr, Nsegment, Strmflow_module, Et_flag, &
     &    Dprst_flag, Temp_module, Ngw, Ntemp, Precip_module, Solrad_module, Transp_module, Et_module, &
     &    Soilzone_module, Srunoff_module, Nrain, Nsol, Cascade_flag, Cascadegw_flag, Strmflow_flag, &
     &    Strmflow_module, Nlake, Nratetbl, Soilzone_flag
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, declparam
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_climateflow
!***********************************************************************
      climateflow_decl = 0

      Version_climateflow = '$Id: climateflow.f90 5642 2013-04-25 22:34:21Z rsregan $'
      CALL print_module(Version_climateflow, 'States and Fluxes         ', 90)
      MODNAME = 'climateflow'

      ALLOCATE ( Tmaxf(Nhru) )
      IF ( declvar(Temp_module, 'tmaxf', 'nhru', Nhru, 'real', &
     &     'Maximum air temperature distributed to each HRU', &
     &     'degrees F', Tmaxf)/=0 ) CALL read_error(3, 'tmaxf')

      ALLOCATE ( Tminf(Nhru) )
      IF ( declvar(Temp_module, 'tminf', 'nhru', Nhru, 'real', &
     &     'Minimum air temperature distributed to each HRU', &
     &     'degrees F', Tminf)/=0 ) CALL read_error(3, 'tminf')

      ALLOCATE ( Tavgf(Nhru) )
      IF ( declvar(Temp_module, 'tavgf', 'nhru', Nhru, 'real', &
     &     'Average air temperature distributed to each HRU', &
     &     'degrees F', Tavgf)/=0 ) CALL read_error(3, 'tavgf')

      ALLOCATE ( Tmaxc(Nhru) )
      IF ( declvar(Temp_module, 'tmaxc', 'nhru', Nhru, 'real', &
     &     'Maximum air temperature distributed to each HRU', &
     &     'degrees Celsius', Tmaxc)/=0 ) CALL read_error(3, 'tmaxc')

      ALLOCATE ( Tminc(Nhru) )
      IF ( declvar(Temp_module, 'tminc', 'nhru', Nhru, 'real', &
     &     'Minimum air temperature distributed to each HRU', &
     &     'degrees Celsius', Tminc)/=0 ) CALL read_error(3, 'tminc')

      ALLOCATE ( Tavgc(Nhru) )
      IF ( declvar(Temp_module, 'tavgc', 'nhru', Nhru, 'real', &
     &     'Average air temperature distributed to each HRU', &
     &     'degrees Celsius', Tavgc)/=0 ) CALL read_error(3, 'tavgc')

      IF ( declvar(Temp_module, 'basin_tmax', 'one', 1, 'double', &
     &     'Basin area-weighted average maximum air temperature', &
     &     'temp_units', Basin_tmax)/=0 ) CALL read_error(3, 'basin_tmax')

      IF ( declvar(Temp_module, 'basin_tmin', 'one', 1, 'double', &
     &     'Basin area-weighted average minimum air temperature', &
     &     'temp_units', Basin_tmin)/=0 ) CALL read_error(3, 'basin_tmin')

      IF ( declvar(Temp_module, 'basin_temp', 'one', 1, 'double', &
     &     'Basin area-weighted average air temperature', &
     &     'temp_units', Basin_temp)/=0 ) CALL read_error(3, 'basin_temp')

      IF ( declvar(Temp_module, 'solrad_tmax', 'one', 1, 'real', &
     &     'Basin daily maximum temperature for use with solar radiation calculations', &
     &     'temp_units', Solrad_tmax)/=0 ) CALL read_error(3, 'solrad_tmax')

      IF ( declvar(Temp_module, 'solrad_tmin', 'one', 1, 'real', &
     &     'Basin daily minimum temperature for use with solar radiation calculations', &
     &     'temp_units', Solrad_tmin)/=0 ) CALL read_error(3, 'solrad_tmin')

! PRECIPITATION VARIABLES AND PARAMETERS
      ALLOCATE ( Pptmix(Nhru) )
      IF ( declvar(Precip_module, 'pptmix', 'nhru', Nhru, 'integer', &
     &     'Flag to indicate if precipitation is a mixture of rain and snow for each HRU (0=no; 1=yes)', &
     &     'none', Pptmix)/=0 ) CALL read_error(3, 'pptmix')

      ALLOCATE ( Newsnow(Nhru) )
      IF ( declvar(Precip_module, 'newsnow', 'nhru', Nhru, 'integer', &
     &    'Flag to indicate if new snow fell on each HRU (0=no; 1=yes)', &
     &    'none', Newsnow)/=0 ) CALL read_error(3, 'newsnow')

      ALLOCATE ( Prmx(Nhru) )
      IF ( declvar(Precip_module, 'prmx', 'nhru', Nhru, 'real', &
     &     'Fraction of rain in a mixed precipitation event for each HRU', &
     &     'decimal fraction', Prmx)/=0 ) CALL read_error(3, 'prmx')

      IF ( declvar(Precip_module, 'basin_rain', 'one', 1, 'double', &
     &     'Basin area-weighted average rainfall', &
     &     'inches', Basin_rain)/=0 ) CALL read_error(3, 'basin_rain')

      IF ( declvar(Precip_module, 'basin_snow', 'one', 1, 'double', &
     &     'Basin area-weighted average snowfall for basin', &
     &     'inches', Basin_snow)/=0 ) CALL read_error(3, 'basin_snow')

      IF ( declvar(Precip_module, 'basin_ppt', 'one', 1, 'double', &
     &     'Basin area-weighted average precipitation', &
     &     'inches', Basin_ppt)/=0 ) CALL read_error(3, 'basin_ppt')

! DANGER - Not sure what to do about this one.  For right now
!          I'm setting basin_ppt and basin_obs_ppt to the same
!          variable.  In the precip_1sta module, basin_obs_ppt
!          seems to be the area weighted precipitation average before
!          the correction factor is applied.  In other modules,
!          the correction "error" is applied to the station
!          precipitation rather than the hru precipitation.
      IF ( declvar(Precip_module, 'basin_obs_ppt', 'one', 1, 'double', &
     &     'Basin area-weighted average measured precipitation', &
     &     'inches', Basin_obs_ppt)/=0 ) CALL read_error(3, 'basin_obs_ppt')

      ALLOCATE ( Hru_ppt(Nhru) )
      IF ( declvar(Precip_module, 'hru_ppt', 'nhru', Nhru, 'real', &
     &     'Precipitation distributed to each HRU', &
     &     'inches', Hru_ppt)/=0 ) CALL read_error(3, 'hru_ppt')

      ALLOCATE ( Hru_rain(Nhru) )
      IF ( declvar(Precip_module, 'hru_rain', 'nhru', Nhru, 'real', &
     &     'Rain distributed to each HRU', &
     &     'inches', Hru_rain)/=0 ) CALL read_error(3, 'hru_rain')

      ALLOCATE ( Hru_snow(Nhru) )
      IF ( declvar(Precip_module, 'hru_snow', 'nhru', Nhru, 'real', &
     &     'Snow distributed to each HRU', &
     &     'inches', Hru_snow)/=0 ) CALL read_error(3, 'hru_snow')

      IF ( Precip_flag==5 .OR. Precip_flag==6 .OR. Model==99 ) THEN
        ALLOCATE ( Tmax_rain_sta(Nrain) )
        IF ( declvar(Precip_module, 'tmax_rain_sta', 'nrain', Nrain, 'real', &
     &       'Maximum temperature distributed to the precipitation measurement stations', &
     &       'degrees F', Tmax_rain_sta)/=0 ) CALL read_error(3, 'tmax_rain_sta')
        ALLOCATE ( Tmin_rain_sta(Nrain) )
        IF ( declvar(Precip_module, 'tmin_rain_sta', 'nrain', Nrain, 'real', &
     &       'Minimum temperature distributed to the precipitation measurement stations', &
     &       'degrees F', Tmin_rain_sta)/=0 ) CALL read_error(3, 'tmin_rain_sta')
      ENDIF

! Solar Radiation variables
      ALLOCATE ( Swrad(Nhru) )
      IF ( declvar(Solrad_module, 'swrad', 'nhru', Nhru, 'real', &
     &     'Shortwave radiation distributed to each HRU', &
     &     'Langleys', Swrad)/=0 ) CALL read_error(3, 'swrad')

      IF ( declvar(Solrad_module, 'orad', 'one', 1, 'real', &
     &     'Measured or computed solar radiation on a horizontal surface', &
     &     'Langleys', Orad)/=0 ) CALL read_error(3, 'orad')

      IF ( declvar(Solrad_module, 'basin_horad', 'one', 1, 'double', &
     &     'Potential shortwave radiation for the basin centroid', &
     &     'Langleys', Basin_horad)/=0 ) CALL read_error(3, 'basin_horad')

      IF ( declvar(Solrad_module, 'basin_potsw', 'one', 1, 'double', &
     &     'Basin area-weighted average potential shortwave radiation', &
     &     'Langleys', Basin_potsw)/=0 ) CALL read_error(3, 'basin_potsw')

! Transpiration Variables
      ALLOCATE ( Transp_on(Nhru) )
      IF ( declvar(Transp_module, 'transp_on', 'nhru', Nhru, 'integer', &
     &     'Flag indicating whether transpiration is occurring (0=no; 1=yes)', &
     &     'none', Transp_on)/=0 ) CALL read_error(3, 'transp_on')

      IF ( declvar(Transp_module, 'basin_transp_on', 'one', 1,'integer', &
     &     'Flag indicating whether transpiration is occurring anywhere in the basin (0=no; 1=yes)', &
     &     'none', Basin_transp_on)/=0 ) CALL read_error(3, 'basin_transp_on')

! Potential ET Variables
      ALLOCATE ( Potet(Nhru) )
      IF ( declvar(Et_module, 'potet', 'nhru', Nhru, 'real', &
     &     'Potential ET for each HRU', &
     &     'inches', Potet)/=0 ) CALL read_error(3, 'potet')

      IF ( declvar(Et_module, 'basin_potet', 'one', 1, 'double', &
     &     'Basin area-weighted average potential ET', &
     &     'inches', Basin_potet)/=0 ) CALL read_error(3, 'basin_potet')

      ALLOCATE ( Soil_rechr(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_rechr', 'nhru', Nhru, 'real', &
     &     'Storage for recharge zone (upper portion) of the capillary reservoir that is available for both'// &
     &     ' evaporation and transpiration', &
     &     'inches', Soil_rechr)/=0 ) CALL read_error(3, 'soil_rechr')

      ALLOCATE ( Ssr_to_gw(Nssr) )
      IF ( declvar(Soilzone_module, 'ssr_to_gw', 'nssr', Nssr, 'real', &
     &     'Drainage from the gravity-reservoir to the associated GWR for each HRU', &
     &     'inches', Ssr_to_gw)/=0 ) CALL read_error(3, 'ssr_to_gw')

      ALLOCATE ( Ssres_stor(Nssr) )
      IF ( declvar(Soilzone_module, 'ssres_stor', 'nssr', Nssr, 'real', &
     &     'Storage in the gravity and preferential-flow reservoirs for each HRU', &
     &     'inches', Ssres_stor)/=0 ) CALL read_error(3, 'ssres_stor')

      ALLOCATE ( Slow_flow(Nhru) )
      IF ( declvar(Soilzone_module, 'slow_flow', 'nhru', Nhru, 'real', &
     &     'Interflow from gravity reservoir storage that flows to the stream network for each HRU', &
     &     'inches', Slow_flow)/=0 ) CALL read_error(3, 'slow_flow')

      ALLOCATE ( Ssres_flow(Nssr) )
      IF ( declvar(Soilzone_module, 'ssres_flow', 'nssr', Nssr, 'real', &
     &     'Interflow from gravity and preferential-flow reservoirs to the stream network for each HRU', &
     &     'inches', Ssres_flow)/=0 ) CALL read_error(3, 'ssres_flow')

      IF ( declvar(Soilzone_module, 'basin_ssflow', 'one', 1, 'double', &
     &     'Basin area-weighted average interflow from gravity and preferential-flow reservoirs to the stream network', &
     &     'inches', Basin_ssflow)/=0 ) CALL read_error(3, 'basin_ssflow')

! soilzone
      IF ( declvar(Soilzone_module, 'basin_swale_et', 'one', 1, 'double', &
     &     'Basin area-weighted average ET from swale HRUs', &
     &     'inches', Basin_swale_et)/=0 ) CALL read_error(3, 'basin_swale_et')

      IF ( declvar(Soilzone_module, 'basin_soil_moist', 'one', 1, 'double', &
     &     'Basin area-weighted average capillary reservoir storage', &
     &     'inches', Basin_soil_moist)/=0 ) CALL read_error(3, 'basin_soil_moist')

      IF ( declvar(Soilzone_module, 'basin_ssstor', 'one', 1, 'double', &
     &     'Basin weighted average gravity and preferential-flow reservoir storage', &
     &     'inches', Basin_ssstor)/=0 ) CALL read_error(3, 'basin_ssstor')

      ALLOCATE ( Slow_stor(Nhru) )
      IF ( declvar(Soilzone_module, 'slow_stor', 'nhru', Nhru, 'real', &
     &     'Storage of gravity reservoir for each HRU', &
     &     'inches', Slow_stor)/=0 ) CALL read_error(3, 'slow_stor')

      ALLOCATE ( Soil_moist(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_moist', 'nhru', Nhru, 'real', &
     &     'Storage of capillary reservoir for each HRU', &
     &     'inches', Soil_moist)/=0 ) CALL read_error(3, 'soil_moist')

      ALLOCATE ( Hru_actet(Nhru) )
      IF ( declvar(Soilzone_module, 'hru_actet', 'nhru', Nhru, 'real', &
     &     'Actual ET for each HRU', &
     &     'inches', Hru_actet)/=0 ) CALL read_error(3, 'hru_actet')

      IF ( declvar(Soilzone_module, 'basin_actet', 'one', 1, 'double', &
     &     'Basin area-weighted average actual ET', &
     &     'inches', Basin_actet)/=0 ) CALL read_error(3, 'basin_actet')

      IF ( declvar(Soilzone_module, 'basin_perv_et', 'one', 1, 'double', &
     &     'Basin area-weighted average ET from capillary reservoirs', &
     &     'inches', Basin_perv_et)/=0 ) CALL read_error(3, 'basin_perv_et')

      IF ( declvar(Soilzone_module, 'basin_lakeevap', 'one', 1, 'double', &
     &     'Basin area-weighted average lake evaporation', &
     &     'inches', Basin_lakeevap)/=0 ) CALL read_error(3, 'basin_lakeevap')

      ALLOCATE ( Ssres_in(Nssr) )
      IF ( declvar(Soilzone_module, 'ssres_in', 'nssr', Nssr, 'real', &
     &     'Inflow to the gravity and preferential-flow reservoirs for each HRU', &
     &     'inches', Ssres_in)/=0 ) CALL read_error(3, 'ssres_in')

      ALLOCATE ( Soil_to_gw(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_to_gw', 'nhru', Nhru, 'real', &
     &     'Portion of excess flow to the capillary reservoir that drains to the associated GWR for each HRU', &
     &     'inches', Soil_to_gw)/=0 ) CALL read_error(3, 'soil_to_gw')

      ALLOCATE ( Soil_to_ssr(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_to_ssr', 'nhru', Nhru, 'real', &
     &     'Portion of excess flow to the capillary reservoir that flows to the gravity reservoir for each HRU', &
     &     'inches', Soil_to_ssr)/=0 ) CALL read_error(3, 'soil_to_ssr')

      IF ( declvar(Soilzone_module, 'basin_soil_to_gw', 'one', 1, 'double', &
     &     'Basin average excess flow to capillary reservoirs that drains to GWRs', &
     &     'inches', Basin_soil_to_gw)/=0 ) CALL read_error(3, 'basin_soil_to_gw')

! gwflow
      ALLOCATE ( Gwres_stor(Ngw) )
      IF ( declvar('gwflow', 'gwres_stor', 'ngw', Ngw, 'double', &
     &     'Storage in each GWR', &
     &     'inches', Gwres_stor)/=0 ) CALL read_error(3, 'gwres_stor')

! srunoff
      ALLOCATE ( Imperv_stor(Nhru) )
      IF ( declvar(Srunoff_module, 'imperv_stor', 'nhru', Nhru, 'real', &
     &     'Storage on impervious area for each HRU', &
     &     'inches', Imperv_stor)/=0 ) CALL read_error(3, 'imperv_stor')

      ALLOCATE ( Infil(Nhru) )
      IF ( declvar(Srunoff_module, 'infil', 'nhru', Nhru, 'real', &
     &     'Infiltration to the capillary and preferential-flow reservoirs for each HRU', &
     &     'inches', Infil)/=0 ) CALL read_error(3, 'infil')

      ALLOCATE ( Sroff(Nhru) )
      IF ( declvar(Srunoff_module, 'sroff', 'nhru', Nhru, 'real', &
     &     'Surface runoff to the stream network for each HRU', &
     &     'inches', Sroff)/=0 ) CALL read_error(3, 'sroff')

! stream flow
      IF ( declvar(Strmflow_module, 'basin_cfs', 'one', 1, 'double', &
     &     'Streamflow leaving the basin through the stream network', &
     &     'cfs', Basin_cfs)/=0 ) CALL read_error(3, 'basin_cfs')

      IF ( declvar(Strmflow_module, 'basin_cms', 'one', 1, 'double', &
     &     'Streamflow leaving the basin through the stream network', &
     &     'cms', Basin_cms)/=0 ) CALL read_error(3, 'basin_cms')

      IF ( declvar(Strmflow_module, 'basin_stflow_in', 'one', 1, 'double', &
     &     'Basin area-weighted average lateral flow entering the stream network', &
     &     'inches', Basin_stflow_in)/=0 ) CALL read_error(3, 'basin_stflow_in')

      IF ( declvar(Strmflow_module, 'basin_stflow_out', 'one', 1, 'double', &
     &     'Basin area-weighted average streamflow leaving through the stream network', &
     &     'inches', Basin_stflow_out)/=0 ) CALL read_error(3, 'basin_stflow_out')

      IF ( declvar(Strmflow_module, 'basin_sroff_cfs', 'one', 1, 'double', &
     &     'Surface runoff leaving the basin through the stream network', &
     &     'cfs', Basin_sroff_cfs)/=0 ) CALL read_error(3, 'basin_sroff_cfs')

      IF ( declvar(Strmflow_module, 'basin_ssflow_cfs', 'one', 1, 'double', &
     &     'Interflow leaving the basin through the stream network',  &
     &     'cfs', Basin_ssflow_cfs)/=0 ) CALL read_error(3, 'basin_ssflow')

      IF ( declvar(Strmflow_module, 'basin_gwflow_cfs', 'one', 1, 'double', &
     &     'Groundwater flow leaving the basin through the stream network', &
     &     'cfs', Basin_gwflow_cfs)/=0 ) CALL read_error(3, 'basin_gwflow_cfs')

      IF ( Cascade_flag==1 .OR. Cascadegw_flag==1 .OR. Strmflow_flag>1 ) THEN
        IF ( Nsegment==0 .AND. Model/=99 ) STOP 'ERROR, nsegment=0, must be > 0 for selected module options'
      ENDIF

      IF ( Strmflow_flag>1 .OR. Model==99 ) THEN
        ALLOCATE ( Seginc_swrad(Nsegment) )
        IF ( declvar(Strmflow_module, 'seginc_swrad', 'nsegment', Nsegment, 'real', &
     &       'Area-weighted average solar radiation for each segment'// &
     &       ' from HRUs contributing flow to the segment', &
     &       'Langleys', Seginc_swrad)/=0 ) CALL read_error(3, 'seginc_swrad')

        ALLOCATE ( Seginc_ssflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seginc_ssflow', 'nsegment', Nsegment, 'real', &
     &       'Area-weighted average interflow for each segment from'// &
     &       ' HRUs contributing flow to the segment', &
     &       'cfs', Seginc_ssflow)/=0 ) CALL read_error(3, 'seginc_ssflow')

        ALLOCATE ( Seginc_gwflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seginc_gwflow', 'nsegment', Nsegment, 'real', &
     &       'Area-weighted average groundwater discharge for each'// &
     &       ' segment from HRUs contributing flow to the segment', &
     &       'cfs', Seginc_gwflow)/=0 ) CALL read_error(3, 'seginc_gwflow')

        ALLOCATE ( Seginc_sroff(Nsegment) )
        IF ( declvar(Strmflow_module, 'seginc_sroff', 'nsegment', Nsegment, 'real', &
     &       'Area-weighted average surface runoff for each'// &
     &       ' segment from HRUs contributing flow to the segment', &
     &       'cfs', Seginc_sroff)/=0 ) CALL read_error(3, 'seginc_sroff')

        ALLOCATE ( Seg_outflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_outflow', 'nsegment', Nsegment, 'real', &
     &       'Streamflow leaving a segment', &
     &       'cfs', Seg_outflow)/=0 ) CALL read_error(3, 'seg_outflow')

        ALLOCATE ( Seg_inflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_inflow', 'nsegment', Nsegment, 'real', &
     &       'Total flow entering a segment', &
     &       'cfs', Seg_inflow)/=0 ) CALL read_error(3, 'seg_inflow')

        ALLOCATE ( Seg_lateral_inflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_lateral_inflow', 'nsegment', Nsegment, 'double', &
     &       'Lateral inflow entering a segment', &
     &       'cfs', Seg_lateral_inflow)/=0 ) CALL read_error(3, 'seg_lateral_inflow')

        ALLOCATE ( Seg_upstream_inflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_upstream_inflow', 'nsegment', Nsegment, 'double', &
     &       'Sum of inflow from upstream segments', &
     &       'cfs', Seg_upstream_inflow)/=0 ) CALL read_error(3, 'seg_upstream_inflow')
      ENDIF

      IF ( (Strmflow_flag==2.AND.Nlake>0) .OR. Model==99 ) THEN
        IF ( declvar(Strmflow_module, 'basin_lake_stor', 'one', 1, 'double', &
     &       'Basin volume-weighted average storage for all lakes using broad-crested weir or gate opening routing', &
     &       'inches', Basin_lake_stor)/=0 ) CALL read_error(3, 'basin_lake_stor')
        ALLOCATE ( Elevlake(Nlake) )
        IF ( declvar(Strmflow_module, 'elevlake', 'nlake', Nlake, 'real', &
     &       'Elevation of each lake using broad-crested weir or gate opening routing', &
     &       'feet', Elevlake)/=0 ) CALL read_error(3, 'elevlake')
        IF ( Nratetbl>0 .OR. Model==99 ) THEN
          IF ( declvar(Strmflow_module, 'basin_2ndstflow', 'one', 1, 'double', &
     &         'Basin volume-weighted average streamflow from each lake with a second outlet', &
     &         'inches', Basin_2ndstflow)/=0 ) CALL read_error(3, 'basin_2ndstflow')
        ENDIF
      ENDIF

      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Dprst_vol_open(Nhru) )
        IF ( declvar(Srunoff_module, 'dprst_vol_open', 'nhru', Nhru, 'double', &
     &       'Storage volume in open surface depressions for each HRU', &
     &       'acre-inches', Dprst_vol_open)/=0 ) CALL read_error(3, 'dprst_vol_open')
        ALLOCATE ( Dprst_vol_clos(Nhru) )
        IF ( declvar(Srunoff_module, 'dprst_vol_clos', 'nhru', Nhru, 'double', &
     &       'Storage volume in closed surface depressions for each HRU', &
     &       'acre-inches', Dprst_vol_clos)/=0 ) CALL read_error(3, 'dprst_vol_clos')
        ALLOCATE ( Dprst_vol_open_max(Nhru), Dprst_vol_clos_max(Nhru), Dprst_vol_thres_open(Nhru) )
      ENDIF

      ALLOCATE ( Pkwater_equiv(Nhru) )
      IF ( declvar('snowcomp', 'pkwater_equiv', 'nhru', Nhru, 'double', &
     &     'Snowpack water equivalent on each HRU', &
     &     'inches', Pkwater_equiv)/=0 ) CALL read_error(3, 'pkwater_equiv')

      ALLOCATE ( Hru_outflow(Nhru) )
      IF ( declvar(MODNAME, 'hru_outflow', 'nhru', Nhru, 'double', &
     &     'Total flow leaving each HRU', &
     &     'cfs', Hru_outflow)/=0 ) CALL read_error(3, 'hru_outflow')

      ! Allocate parameters
      IF ( Temp_flag<7 .OR. Model==99 ) ALLOCATE ( Tsta_elev(Ntemp), Tsta_elev_meters(Ntemp), Tsta_elev_feet(Ntemp) )
      IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4 .OR. Model==99 ) ALLOCATE ( Hru_tsta(Nhru) )
      IF ( (Temp_flag/=3.AND.Temp_flag/=7.AND.Temp_flag/=8) .OR. Model==99 ) ALLOCATE ( Tmax_adj(Nhru), Tmin_adj(Nhru) )
      IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Precip_flag==5 .OR. Model==99 ) &
     &     ALLOCATE ( Psta_elev(Nrain), Psta_elev_meters(Nrain), Psta_elev_feet(Nrain) )
      IF ( Nsol>0 .OR. Model==99 ) ALLOCATE ( Hru_solsta(Nhru) )
      IF ( Et_flag==5 .OR. Et_flag==6 .OR. Et_flag==10 .OR. Model==99 ) ALLOCATE ( Potet_coef_hru_mo(Nhru,12) )
      IF ( Soilzone_flag==1 ) ALLOCATE ( Sat_threshold(Nhru) )
      ALLOCATE ( Soil_moist_max(Nhru), Soil_rechr_max(Nhru) )
      ALLOCATE ( Snowinfil_max(Nhru), Imperv_stor_max(Nhru), Tmax_hru(Nhru), Tmin_hru(Nhru) )
      ALLOCATE ( Tmax_allsnow_hru_f(Nhru) )
      IF ( Precip_flag==8 .OR. Model==99 ) ALLOCATE ( Tmax_allsnow_hru(Nhru) )
      IF ( Precip_flag==7 .OR. Precip_flag==8 .OR. Model==99 ) THEN
        IF ( Precip_flag==7 .OR. Precip_flag==8 .OR. Model==99 ) ALLOCATE ( Tmax_allrain_hru_month_f(Nhru,12) )
        IF ( Precip_flag==8 .OR. Model==99 ) ALLOCATE ( Tmax_allrain_hru_mo(Nhru,12) )
      ENDIF

      IF ( Timestep/=0 ) RETURN

! Declare Parameters
      IF ( Temp_flag<7 .OR. Model==99 ) THEN
        IF ( declparam(Temp_module, 'tsta_elev', 'ntemp', 'real', &
     &       '0', '-300.0', '30000.0', &
     &       'Temperature station elevation', &
     &       'Elevation of each temperature measurement station', &
     &       'elev_units')/=0 ) CALL read_error(1, 'tsta_elev')
      ENDIF

      IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4 .OR. Model==99 ) THEN
        IF ( declparam(Temp_module, 'hru_tsta', 'nhru', 'integer', &
     &       '1', 'bounded', 'ntemp', &
     &       'Index of base temperature station for HRU', &
     &       'Index of the base temperature station used for lapse rate calculations', &
     &       'none')/=0 ) CALL read_error(1, 'hru_tsta')
      ENDIF

      IF ( (Temp_flag/=3.AND.Temp_flag/=7.AND.Temp_flag/=8) .OR. Model==99 ) THEN
        IF ( declparam(Temp_module, 'tmax_adj', 'nhru', 'real', &
     &       '0.0', '-10.0', '10.0', &
     &       'HRU maximum temperature adjustment', &
     &       'Adjustment to maximum temperature for each HRU, estimated based on slope and aspect', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_adj')
        IF ( declparam(Temp_module, 'tmin_adj', 'nhru', 'real', &
     &       '0.0', '-10.0', '10.0', &
     &       'HRU minimum temperature adjustment', &
     &       'Adjustment to minimum temperature for each HRU, estimated based on slope and aspect', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmin_adj')
      ENDIF

      IF ( declparam(Precip_module, 'tmax_allrain', 'nmonths', 'real', &
     &     '40.0', '0.0', '90.0', &
     &     'Precipitation is rain if HRU max temperature >= this value', &
     &     'Monthly (January to December) maximum air temperature'// &
     &     ' when precipitation is assumed to be rain; if HRU air'// &
     &     ' temperature is greater than or equal to this value, precipitation is rain', &
     &     'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain')

      IF ( declparam(Precip_module, 'tmax_allsnow', 'one', 'real', &
     &     '32.0', '-10.0', '40.0', &
     &     'Maximum temperature when precipitation is all snow', &
     &     'Maximum air temperature when precipitation is assumed'// &
     &     ' to be snow; if HRU air temperature is less than or equal to this value, precipitation is snow', &
     &     'temp_units')/=0 ) CALL read_error(1, 'tmax_allsnow')

      IF ( Precip_flag==8 .OR. Model==99 ) THEN
        IF ( declparam(Precip_module, 'tmax_allsnow_hru', 'nhru','real', &
     &       '32.0', '-10.0', '40.0', &
     &       'Maximum temperature when precipitation is all snow for each HRU', &
     &       'Maximum air temperature when precipitation is assumed'// &
     &       ' to be snow for each HRU; if HRU air temperature is'// &
     &       ' less than or equal to this value, precipitation is snow', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_allsnow_hru')

        IF ( declparam(Precip_module, 'tmax_allrain_hru_mo', 'nhru,nmonths', 'real', &
     &       '40.0', '0.0', '90.0', &
     &       'Precipitation is rain if HRU max temperature >= this value for each month', &
     &       'Monthly maximum air temperature when precipitation is'// &
     &       ' assumed to be rain for each HRU; if HRU air'// &
     &       ' temperature is greater than or equal to this value, precipitation is rain', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain_hru_mo')
      ENDIF

      IF ( Precip_flag/=8 .OR. Model==99 ) THEN
        IF ( declparam(Precip_module, 'adjmix_rain', 'nmonths', 'real', &
     &       '1.0', '0.0', '3.0', &
     &       'Adjustment factor for rain in a rain/snow mix', &
     &       'Monthly (January to December) factor to adjust rain proportion in a mixed rain/snow event', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'adjmix_rain')
      ENDIF

      IF ( Precip_flag==5 .OR. Precip_flag==6 .OR. Model==99 ) THEN
        IF ( declparam(Precip_module, 'adjust_snow', 'nmonths', 'real', &
     &       '0.01', '0.0', '1.0', &
     &       'Monthly (January to December) downscaling adjustment factor for snow', &
     &       'Monthly (January to December) downscaling adjustment factor for snow', &
     &       'decimal fraction')/=0 ) CALL read_error(1,'adjust_snow')
        IF ( declparam(Precip_module, 'adjust_rain', 'nmonths', 'real', &
     &       '0.01', '0.0', '1.0', &
     &       'Monthly (January to December) downscaling adjustment factor for rain', &
     &       'Monthly (January to December) downscaling adjustment factor for rain', &
     &       'decimal fraction')/=0 ) CALL read_error(1,'adjust_rain')
      ENDIF

      IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Precip_flag==5 .OR. Model==99 ) THEN
        IF ( declparam(Precip_module, 'psta_elev', 'nrain', 'real', &
     &       '0', '-300.0', '30000.0', &
     &       'Precipitation station elevation', &
     &       'Elevation of each precipitation measurement station', &
     &       'elev_units')/=0 ) CALL read_error(1, 'psta_elev')
      ENDIF

     IF ( declparam(Temp_module, 'temp_units', 'one', 'integer', &
     &     '0', '0', '1', &
     &     'Units flag for measured temperature', &
     &     'Flag to indicate the units of measured air-temperature values (0=Fahrenheit; 1=Celsius)', &
     &     'none')/=0 ) CALL read_error(1, 'temp_units')

      IF ( Temp_flag<5 .OR. Model==99 ) THEN
        IF ( declparam(Temp_module, 'basin_tsta', 'one', 'integer', &
     &       '1', 'bounded', 'ntemp', &
     &       'Index of main temperature station', &
     &       'Index of temperature station used to compute basin temperature values', &
     &       'none')/=0 ) CALL read_error(1, 'basin_tsta')
      ENDIF

      IF ( declparam(Precip_module, 'precip_units', 'one', 'integer', &
     &     '0', '0', '1', &
     &     'Units for measured precipitation', &
     &     'Units for measured precipitation (0=inches; 1=mm)', &
     &     'none')/=0 ) CALL read_error(1, 'precip_units')

      IF ( declparam(Solrad_module, 'ppt_rad_adj', 'nmonths', 'real', &
     &     '0.02', '0.0', '0.5', &
     &     'Radiation reduced if basin precipitation above this value', &
     &     'Monthly minimum precipitation, if basin precipitation exceeds this value, radiation is'// &
     &     ' multiplied by radj_sppt or radj_wppt adjustment factor', &
     &     'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')

      IF ( declparam(Solrad_module, 'radmax', 'one', 'real', &
     &     '0.8', '0.1', '1.0', &
     &     'Maximum fraction of potential solar radiation (decimal)', &
     &     'Maximum fraction of the potential solar radiation that may reach the ground due to haze, dust, smog, and so forth', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'radmax')

      IF ( declparam(Solrad_module, 'radj_sppt', 'one', 'real', &
     &     '0.44', '0.0', '1.0', &
     &     'Adjustment to solar radiation on precipitation day - summer', &
     &     'Adjustment factor for computed solar radiation for summer day with greater than ppt_rad_adj inches of precipitation', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'radj_sppt')

      IF ( declparam(Solrad_module, 'radj_wppt', 'one', 'real', &
     &     '0.5', '0.0', '1.0', &
     &    'Adjustment to solar radiation on precipitation day - winter', &
     &     'Adjustment factor for computed solar radiation for winter day with greater than ppt_rad_adj inches of precipitation', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'radj_wppt')

      IF ( Nsol>0 .OR. Model==99 ) THEN
        IF ( declparam(Solrad_module, 'rad_conv', 'one', 'real', &
     &       '1.0', '0.1', '100.0', &
     &       'Conversion factor to Langleys for measured radiation', &
     &       'Conversion factor to Langleys for measured solar radiation', &
     &       'none')/=0 ) CALL read_error(1, 'rad_conv')
        IF ( declparam(Solrad_module, 'basin_solsta', 'one', 'integer', &
     &       '0', 'bounded', 'nsol', &
     &       'Index of main solar radiation station', &
     &       'Index of solar radiation station used to compute basin radiation values', &
     &       'none')/=0 ) CALL read_error(1, 'basin_solsta')
        IF ( declparam(Solrad_module, 'hru_solsta', 'nhru', 'integer', &
     &       '0', 'bounded', 'nsol', &
     &       'Index of solar radiation station associated with each HRU', &
     &       'Index of solar radiation station associated with each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'hru_solsta')
      ENDIF

      IF ( Et_flag==5 .OR. Et_flag==6 .OR. Et_flag==10 .OR. Model==99 ) THEN
        IF ( declparam(Et_module, 'potet_coef_hru_mo', 'nhru,nmonths', 'real', &
     &       '0.014', '0.005', '0.060', &
     &       'Monthly potential ET coefficient adjustment factor', &
     &       'Monthly potential ET coefficient adjustment factor for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'potet_coef_hru_mo')
      ENDIF

      IF ( Soilzone_flag==1 ) THEN
        IF ( declparam(Soilzone_module, 'sat_threshold', 'nhru', 'real', &
     &       '999.0', '1.0', '999.0', &
     &       'Soil saturation threshold, above field-capacity threshold', &
     &       'Water holding capacity of the gravity and preferential-flow reservoirs; difference between field capacity and'// &
     &       ' total soil saturation for each HRU', &
     &       'inches')/=0 ) CALL read_error(1, 'sat_threshold')
      ENDIF

      IF ( declparam(Soilzone_module, 'soil_moist_max', 'nhru', 'real', &
     &     '6.0', '0.001', '20.0', &
     &     'Maximum value of water for soil zone', &
     &     'Maximum available water holding capacity of capillary reservoir from land surface to rooting depth of the'// &
     &     ' major vegetation type of each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'soil_moist_max')

      IF ( declparam(Soilzone_module, 'soil_rechr_max', 'nhru', 'real', &
     &     '2.0', '0.001', '10.0', &
     &     'Maximum storage for soil recharge zone', &
     &     'Maximum storage for soil recharge zone (upper portion of capillary reservoir where losses occur as both'// &
     &     ' evaporation and transpiration); must be less than or equal to soil_moist_max', &
     &     'inches')/=0 ) CALL read_error(1, 'soil_rechr_max')

      IF ( declparam(Srunoff_module, 'snowinfil_max', 'nhru', 'real', &
     &     '2.0', '0.0', '20.0', &
     &     'Maximum snow infiltration per day', &
     &     'Maximum snow infiltration per day for each HRU', &
     &     'inches/day')/=0 ) CALL read_error(1, 'snowinfil_max')

      IF ( declparam(Srunoff_module, 'imperv_stor_max', 'nhru', 'real', &
     &     '0.0', '0.0', '10.0', &
     &     'HRU maximum impervious area retention storage', &
     &     'Maximum impervious area retention storage for each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'imperv_stor_max')

      END FUNCTION climateflow_decl

!***********************************************************************
!     climateflow_init - Initialize module - get parameter values,
!                        set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION climateflow_init()
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      USE PRMS_MODULE, ONLY: Temp_flag, Precip_flag, Et_flag, Et_module, Nhru, Nssr, Temp_module, Precip_module, Inputerror_flag, &
     &    Solrad_module, Soilzone_module, Srunoff_module, Nsegment, Dprst_flag, Ntemp, Nrain, Nsol, &
     &    Strmflow_flag, Nlake, Soilzone_flag
      USE PRMS_BASIN, ONLY: Hru_type, Elev_units, FEET2METERS, METERS2FEET, NEARZERO, Active_hrus, Hru_route_order
      IMPLICIT NONE
! Functions
      INTRINSIC DBLE
      INTEGER, EXTERNAL :: getparam
      REAL, EXTERNAL :: c_to_f
! Local variables
      INTEGER :: i, j
!***********************************************************************
      climateflow_init = 0

      IF ( Temp_flag<7 ) THEN
        IF ( getparam(Temp_module, 'tsta_elev', Ntemp, 'real', Tsta_elev)/=0 ) CALL read_error(2, 'tsta_elev')
        DO i = 1, Ntemp
          IF ( Elev_units==0 ) THEN
            Tsta_elev_feet(i) = Tsta_elev(i)
            Tsta_elev_meters(i) = Tsta_elev_feet(i)*FEET2METERS
          ELSE
            Tsta_elev_meters(i) = Tsta_elev(i)
            Tsta_elev_feet(i) = Tsta_elev_meters(i)*METERS2FEET
          ENDIF
        ENDDO
      ENDIF

      IF ( Temp_flag/=3 .AND. Temp_flag/=7 .AND. Temp_flag/=8 ) THEN
        IF ( getparam(Temp_module, 'tmax_adj', Nhru, 'real', Tmax_adj)/=0 ) CALL read_error(2, 'tmax_adj')
        IF ( getparam(Temp_module, 'tmin_adj', Nhru, 'real', Tmin_adj)/=0 ) CALL read_error(2, 'tmin_adj')
      ENDIF

      IF ( getparam(Temp_module, 'temp_units', 1, 'integer', Temp_units)/=0 ) CALL read_error(2, 'temp_units')

      IF ( Temp_flag<5 ) THEN
        IF ( getparam(Temp_module, 'basin_tsta', 1, 'integer', Basin_tsta)/=0 ) CALL read_error(2, 'basin_tsta')
        IF ( Basin_tsta<1 .OR. Basin_tsta> Ntemp ) THEN
          PRINT *, 'ERROR, basin_tsta value < 1 or > ntemp:', Basin_tsta, '; ntemp:', Ntemp
          Inputerror_flag = 1
        ENDIF
      ENDIF

      IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4 ) THEN
        IF ( getparam(Temp_module, 'hru_tsta', Nhru, 'integer', Hru_tsta)/=0 ) CALL read_error(2, 'hru_tsta')
        DO i = 1, Active_hrus
          j = Hru_route_order(i)
          IF ( Hru_tsta(j)<1 ) THEN
            IF ( Hru_type(j)/=0 ) THEN
              PRINT *, 'ERROR, hru_tsta value < 1 for HRU:', j, Hru_tsta(j)
              Inputerror_flag = 1
            ENDIF
          ELSEIF ( Hru_tsta(j)>Ntemp ) THEN
            PRINT *, 'ERROR, hru_tsta value > ntemp for HRU:', j, ', hru_tsta:', Hru_tsta(j), ', ntemp', Ntemp
            Inputerror_flag = 1
          ENDIF
        ENDDO
      ENDIF

      IF ( getparam(Precip_module, 'tmax_allsnow', 1, 'real', Tmax_allsnow)/=0 ) CALL read_error(2, 'tmax_allsnow')
      IF ( getparam(Precip_module, 'tmax_allrain', 12, 'real', Tmax_allrain)/=0 ) CALL read_error(2, 'tmax_allrain')
      IF ( Temp_units==0 ) THEN
        Tmax_allsnow_f = Tmax_allsnow
        Tmax_allrain_f = Tmax_allrain
      ELSE
        Tmax_allsnow_f = c_to_f(Tmax_allsnow)
        DO i = 1, 12
          Tmax_allrain_f(i) = c_to_f(Tmax_allrain(i))
        ENDDO
      ENDIF
      IF ( Precip_flag==8 ) THEN
        IF ( getparam(Precip_module, 'tmax_allsnow_hru', Nhru, 'real', Tmax_allsnow_hru)/=0 ) &
     &       CALL read_error(2, 'tmax_allsnow_hru')
        IF ( Temp_units==0 ) THEN
          Tmax_allsnow_hru_f = Tmax_allsnow_hru
        ELSE
          DO i = 1, Nhru
            Tmax_allsnow_hru_f(i) = c_to_f(Tmax_allsnow_hru(i))
          ENDDO
        ENDIF
        DEALLOCATE ( Tmax_allsnow_hru )
        IF ( getparam(Precip_module, 'tmax_allrain_hru_mo', Nhru*12, 'real', Tmax_allrain_hru_mo)/=0 ) &
     &       CALL read_error(2, 'tmax_allrain_hru_mo')
        IF ( Temp_units==0 ) THEN
          Tmax_allrain_hru_month_f = Tmax_allrain_hru_mo
        ELSE
          DO i = 1, Nhru
            DO j = 1, 12
              Tmax_allrain_hru_month_f(i,j) = c_to_f(Tmax_allrain_hru_mo(i,j))
            ENDDO
          ENDDO
        ENDIF
      ELSE
        IF ( getparam(Precip_module, 'adjmix_rain', 12, 'real', Adjmix_rain)/=0 ) CALL read_error(2, 'adjmix_rain')
        DO i = 1, Nhru
          Tmax_allsnow_hru_f(i) = Tmax_allsnow_f
        ENDDO
        IF ( Precip_flag==7 ) THEN
          DO i = 1, Nhru
            DO j = 1, 12
              Tmax_allrain_hru_month_f(i,j) = Tmax_allrain_f(j)
            ENDDO
          ENDDO
        ELSEIF ( Precip_flag==5 .OR. Precip_flag==6 ) THEN
          IF ( getparam(Precip_module, 'adjust_rain', 12, 'real', Adjust_rain)/=0 ) CALL read_error(2, 'adjust_rain')
          IF ( getparam(Precip_module, 'adjust_snow', 12, 'real', Adjust_snow)/=0 ) CALL read_error(2, 'adjust_snow')
        ENDIF
      ENDIF

      IF ( getparam(Precip_module, 'precip_units', 1, 'integer', Precip_units)/=0 ) CALL read_error(2, 'precip_units')

      IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Precip_flag==5 ) THEN
        IF ( getparam(Precip_module, 'psta_elev', Nrain, 'real', Psta_elev)/=0 ) CALL read_error(2, 'psta_elev')
        DO i = 1, Nrain
          IF ( Elev_units==0 ) THEN
            Psta_elev_feet(i) = Psta_elev(i)
            Psta_elev_meters(i) = Psta_elev_feet(i)*FEET2METERS
          ELSE
            Psta_elev_meters(i) = Psta_elev(i)
            Psta_elev_feet(i) = Psta_elev_meters(i)*METERS2FEET
          ENDIF
        ENDDO
      ENDIF

      IF ( getparam(Solrad_module, 'ppt_rad_adj', 12, 'real', Ppt_rad_adj)/=0 ) CALL read_error(2, 'ppt_rad_adj')

      IF ( Nsol>0 ) THEN
        IF ( getparam(Solrad_module, 'basin_solsta', 1, 'integer', Basin_solsta)/=0 ) CALL read_error(2, 'basin_solsta')
        IF ( Basin_solsta<1 .OR. Basin_solsta>Nsol ) THEN
          PRINT *, 'ERROR, basin_solsta value < 1 OR > nsol:', Basin_solsta, ', nsol:', Nsol
          Inputerror_flag = 1
        ENDIF
        IF ( getparam(Solrad_module, 'rad_conv', 1, 'real', Rad_conv)/=0 ) CALL read_error(2, 'rad_conv')
        IF ( getparam(Solrad_module, 'hru_solsta', Nhru, 'integer', Hru_solsta)/=0 ) CALL read_error(2, 'hru_solsta')
        DO i = 1, Active_hrus
          j = Hru_route_order(i)
          IF ( Hru_solsta(j)<0 .OR. Hru_solsta(j)>Nsol ) THEN
            PRINT *, 'ERROR, hru_solsta value < 1 or > nsol for HRU:', j, ', hru_solsta:', Hru_solsta(j), ', nsol:', Nsol
            Inputerror_flag = 1
          ENDIF
        ENDDO
      ENDIF

      IF ( getparam(Solrad_module, 'radmax', 1, 'real', Radmax)/=0 ) CALL read_error(2, 'radmax')
      IF ( getparam(Solrad_module, 'radj_sppt', 1, 'real', Radj_sppt)/=0 ) CALL read_error(2, 'radj_sppt')
      IF ( getparam(Solrad_module, 'radj_wppt', 1, 'real', Radj_wppt)/=0 ) CALL read_error(2, 'radj_wppt')

      IF ( Et_flag==5 .OR. Et_flag==6 .OR. Et_flag==10 ) THEN
        IF ( getparam(Et_module, 'potet_coef_hru_mo', Nhru*12, 'real', Potet_coef_hru_mo)/=0 ) &
     &       CALL read_error(2, 'potet_coef_hru_mo')
      ENDIF

      Tmaxf = 0.0
      Tminf = 0.0
      Tavgf = 0.0
      Tmaxc = 0.0
      Tminc = 0.0
      Tavgc = 0.0
      Tmax_hru = 0.0
      Tmin_hru = 0.0
      Solrad_tmax = 0.0
      Solrad_tmin = 0.0
      Basin_temp = 0.0D0
      Basin_tmax = 0.0D0
      Basin_tmin = 0.0D0
      Pptmix = 0
      Newsnow = 0
      Prmx = 0.0
      Basin_ppt = 0.0D0
      Basin_obs_ppt = 0.0D0
      Basin_rain = 0.0D0
      Basin_snow = 0.0D0
      Hru_ppt = 0.0
      Hru_rain = 0.0
      Hru_snow = 0.0
      Swrad = 0.0
      Orad = 0.0
      Basin_horad = 0.0D0
      Basin_potsw = 0.0D0
      Transp_on = 0
      Basin_transp_on = 0
      Basin_potet = 0.0D0
      Potet = 0.0
      IF ( Precip_flag==5 .OR. Precip_flag==6 ) THEN
        Tmax_rain_sta = 0.0
        Tmin_rain_sta = 0.0
      ENDIF
      IF ( Strmflow_flag>1 ) THEN
        Seginc_gwflow = 0.0
        Seginc_ssflow = 0.0
        Seginc_sroff = 0.0
        Seginc_swrad = 0.0
        Seg_inflow = 0.0
        Seg_outflow = 0.0
        Seg_lateral_inflow = 0.0D0
        Seg_upstream_inflow = 0.0D0
      ENDIF

! FLOW VARIABLES AND PARAMETERS
      IF ( Soilzone_flag==1 ) THEN
        IF ( getparam(Soilzone_module, 'sat_threshold', Nhru, 'real', Sat_threshold)/=0 ) CALL read_error(2, 'sat_threshold')
      ENDIF
      IF ( getparam(Soilzone_module, 'soil_moist_max', Nhru, 'real', Soil_moist_max)/=0 ) CALL read_error(2, 'soil_moist_max')
      IF ( getparam(Soilzone_module, 'soil_rechr_max', Nhru, 'real', Soil_rechr_max)/=0 ) CALL read_error(2, 'soil_rechr_max')

      ! Sanity checks for parameters
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Hru_type(i)==0 .OR. Hru_type(i)==2 ) CYCLE
        IF ( Soil_moist_max(i)<0.001 ) THEN
          PRINT *, 'ERROR, soil_moist_max value < 0.001, for HRU:', i, Soil_moist_max(i)
          Inputerror_flag = 1
        ENDIF
        IF ( Soil_rechr_max(i)<0.001 ) THEN
          PRINT *, 'ERROR, soil_rechr_max value < 0.001, for HRU:', i, Soil_rechr_max(i)
          Inputerror_flag = 1
        ENDIF
      ENDDO

      IF ( getparam(Srunoff_module, 'snowinfil_max', Nhru, 'real', Snowinfil_max)/=0 ) CALL read_error(2, 'snowinfil_max')
      IF ( getparam(Srunoff_module, 'imperv_stor_max', Nhru, 'real', Imperv_stor_max)/=0 ) CALL read_error(2, 'imperv_stor_max')

! initialize scalers
      Basin_perv_et = 0.0D0
      Basin_actet = 0.0D0
      Basin_lakeevap = 0.0D0
      Basin_swale_et = 0.0D0
      Basin_soil_to_gw = 0.0D0
      Basin_ssflow = 0.0D0
      Basin_soil_moist = 0.0D0
      Basin_ssstor = 0.0D0
      Basin_lake_stor = 0.0D0
      Basin_2ndstflow = 0.0D0
      IF ( Strmflow_flag==2 .AND. Nlake>0 ) Elevlake = 0.0
! initialize arrays (dimensioned Nssr)
      Ssr_to_gw = 0.0
      Ssres_in = 0.0
      Ssres_stor = 0.0
      Ssres_flow = 0.0
! initialize arrays (dimensioned Nhru)
      Slow_stor = 0.0
      Slow_flow = 0.0
      Soil_to_gw = 0.0
      Soil_to_ssr = 0.0
      Soil_moist = 0.0
      Hru_actet = 0.0
      Infil = 0.0
      Sroff = 0.0
      Imperv_stor = 0.0
      Soil_rechr = 0.0
      Gwres_stor = 0.0D0
      IF ( Dprst_flag==1 ) THEN
        Dprst_vol_open = 0.0D0
        Dprst_vol_clos = 0.0D0
        Dprst_vol_open_max = 0.0D0
        Dprst_vol_clos_max = 0.0D0
        Dprst_vol_thres_open = 0.0D0
      ENDIF
      Pkwater_equiv = 0.0D0
      Hru_outflow = 0.0D0
      Basin_cfs = 0.0D0
      Basin_cms = 0.0D0
      Basin_stflow_in = 0.0D0
      Basin_stflow_out = 0.0D0
      Basin_ssflow_cfs = 0.0D0
      Basin_sroff_cfs = 0.0D0
      Basin_gwflow_cfs = 0.0D0

      END FUNCTION climateflow_init

!***********************************************************************
!     Sets temperatures in both system of units for each HRU
!***********************************************************************
      SUBROUTINE temp_set(Ihru, Tmax, Tmin, Tmaxf, Tminf, Tavgf, Tmaxc, Tminc, Tavgc, Hru_area)
      USE PRMS_CLIMATEVARS, ONLY: Basin_temp, Basin_tmax, Basin_tmin, Temp_units, Tmax_hru, Tmin_hru
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(IN) :: Tmax, Tmin, Hru_area
      REAL, INTENT(OUT) :: Tmaxf, Tminf, Tavgf, Tmaxc, Tminc, Tavgc
! Functions
      REAL, EXTERNAL :: c_to_f, f_to_c
!***********************************************************************
      IF ( Temp_units==0 ) THEN
!       degrees F
        Tmaxf = Tmax
        Tminf = Tmin
        Tavgf = (Tmax+Tmin)*0.5
        Tmaxc = f_to_c(Tmax)
        Tminc = f_to_c(Tmin)
        Tavgc = f_to_c(Tavgf)
        Basin_temp = Basin_temp + Tavgf*Hru_area
      ELSE
!       degrees C
        Tmaxc = Tmax
        Tminc = Tmin
        Tavgc = (Tmax+Tmin)*0.5
        Tmaxf = c_to_f(Tmax)
        Tminf = c_to_f(Tmin)
        Tavgf = c_to_f(Tavgc)
        Basin_temp = Basin_temp + Tavgc*Hru_area
      ENDIF
 
      IF ( Tminf<-99.0 .OR. Tmaxf>150.0 ) THEN
        PRINT *, 'ERROR, invalid temperature value for HRU:', Ihru, &
     &           Tminf, Tmaxf, ' Date:', Nowyear, Nowmonth, Nowday
        STOP
      ENDIF
      Tmax_hru(Ihru) = Tmax
      Tmin_hru(Ihru) = Tmin

      Basin_tmax = Basin_tmax + Tmax*Hru_area
      Basin_tmin = Basin_tmin + Tmin*Hru_area

      END SUBROUTINE temp_set

!***********************************************************************
!     Computes precipitation form (rain, snow or mix) and depth for each HRU
!***********************************************************************
      SUBROUTINE precip_form(Precip, Hru_ppt, Hru_rain, Hru_snow, Tmaxf, &
     &           Tminf, Pptmix, Newsnow, Prmx, Tmax_allrain_f, Rain_adj, &
     &           Snow_adj, Adjmix_rain, Hru_area, Sum_obs, Tmax_allsnow_f)
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Basin_ppt, Basin_rain, Basin_snow
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Tmax_allrain_f, Tmax_allsnow_f, Rain_adj, Snow_adj
      REAL, INTENT(IN) :: Adjmix_rain, Tmaxf, Tminf, Hru_area
      DOUBLE PRECISION, INTENT(INOUT) :: Sum_obs
      INTEGER, INTENT(INOUT) :: Pptmix, Newsnow
      REAL, INTENT(INOUT) :: Precip, Hru_rain, Hru_snow, Prmx, Hru_ppt
      INTRINSIC ABS
! Local Variables
      REAL :: tdiff
!***********************************************************************
      ! basin precipitation before adjustments
      Sum_obs = Sum_obs + Precip*Hru_area

!******If maximum temperature is below or equal to the base temperature
!******for snow then precipitation is all snow
      IF ( Tmaxf<=Tmax_allsnow_f ) THEN
        Hru_ppt = Precip*Snow_adj
        Hru_snow = Hru_ppt
        Newsnow = 1

!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain
      ELSEIF ( Tminf>Tmax_allsnow_f .OR. Tmaxf>=Tmax_allrain_f ) THEN
        Hru_ppt = Precip*Rain_adj
        Hru_rain = Hru_ppt
        Prmx = 1.0

!******Otherwise precipitation is a mixture of rain and snow
      ELSE
        tdiff = Tmaxf - Tminf
        IF ( ABS(tdiff)<NEARZERO ) tdiff = 0.0001
        Prmx = ((Tmaxf-Tmax_allsnow_f)/tdiff)*Adjmix_rain

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain
        IF ( Prmx>=1.0 ) THEN
          Hru_ppt = Precip*Rain_adj
          Hru_rain = Hru_ppt
          Prmx = 1.0

!******If not, it is a rain/snow mixture
        ELSE
          Pptmix = 1
          Hru_ppt = Precip*Snow_adj
          Hru_rain = Prmx*Hru_ppt
          Hru_snow = Hru_ppt - Hru_rain
          Newsnow = 1
        ENDIF
      ENDIF
      Basin_ppt = Basin_ppt + Hru_ppt*Hru_area
      Basin_rain = Basin_rain + Hru_rain*Hru_area
      Basin_snow = Basin_snow + Hru_snow*Hru_area

      END SUBROUTINE precip_form

!***********************************************************************
!     climateflow_restart - write or read climateflow restart file
!***********************************************************************
      SUBROUTINE climateflow_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Temp_flag, Precip_flag, Nlake, &
     &    Dprst_flag, Et_flag, Nsol, Strmflow_flag, Nratetbl, Soilzone_flag
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Tmax_allsnow, Tmax_allsnow_f, Basin_ppt, Basin_rain, Basin_snow, Basin_obs_ppt, Basin_temp, &
     &          Basin_tmax, Basin_tmin, Solrad_tmax, Solrad_tmin, Basin_transp_on, Basin_potet, Basin_horad, &
     &          Basin_potsw, Temp_units, Precip_units, Radj_sppt, Radj_wppt, Radmax, Orad
        WRITE ( Restart_outunit ) Basin_cfs, Basin_cms, Basin_ssflow_cfs, Basin_sroff_cfs, Basin_stflow_in, &
     &          Basin_gwflow_cfs, Basin_stflow_out, Basin_ssflow, Basin_soil_to_gw, Basin_actet, &
     &          Basin_swale_et, Basin_perv_et, Basin_soil_moist, Basin_ssstor, Basin_lakeevap, Basin_lake_stor, Basin_2ndstflow
        WRITE ( Restart_outunit ) Tmax_hru
        WRITE ( Restart_outunit ) Tmin_hru
        WRITE ( Restart_outunit ) Tmax_allrain_f
        WRITE ( Restart_outunit ) Newsnow
        WRITE ( Restart_outunit ) Pptmix
        WRITE ( Restart_outunit ) Hru_ppt
        WRITE ( Restart_outunit ) Hru_rain
        WRITE ( Restart_outunit ) Hru_snow
        WRITE ( Restart_outunit ) Prmx
        WRITE ( Restart_outunit ) Tmaxf
        WRITE ( Restart_outunit ) Tminf
        WRITE ( Restart_outunit ) Tavgf
        WRITE ( Restart_outunit ) Tmaxc
        WRITE ( Restart_outunit ) Tminc
        WRITE ( Restart_outunit ) Tavgc
        WRITE ( Restart_outunit ) Transp_on
        WRITE ( Restart_outunit ) Potet
        WRITE ( Restart_outunit ) Swrad
        WRITE ( Restart_outunit ) Pkwater_equiv
        WRITE ( Restart_outunit ) Soil_moist_max
        WRITE ( Restart_outunit ) Soil_rechr_max
        WRITE ( Restart_outunit ) Hru_actet
        WRITE ( Restart_outunit ) Soil_to_gw
        WRITE ( Restart_outunit ) Slow_flow
        WRITE ( Restart_outunit ) Soil_moist
        WRITE ( Restart_outunit ) Soil_to_ssr
        WRITE ( Restart_outunit ) Ssres_in
        WRITE ( Restart_outunit ) Ssr_to_gw
        WRITE ( Restart_outunit ) Slow_stor
        WRITE ( Restart_outunit ) Ssres_stor
        WRITE ( Restart_outunit ) Ssres_flow
        WRITE ( Restart_outunit ) Soil_rechr
        IF ( Soilzone_flag==1 ) WRITE ( Restart_outunit ) Sat_threshold
        WRITE ( Restart_outunit ) Sroff
        WRITE ( Restart_outunit ) Imperv_stor
        WRITE ( Restart_outunit ) Infil
        WRITE ( Restart_outunit ) Gwres_stor
        WRITE ( Restart_outunit ) Snowinfil_max
        WRITE ( Restart_outunit ) Imperv_stor_max
        WRITE ( Restart_outunit ) Ppt_rad_adj
        WRITE ( Restart_outunit ) Tmax_allrain
        IF ( Precip_flag/=8 ) WRITE ( Restart_outunit ) Adjmix_rain
        WRITE ( Restart_outunit ) Hru_outflow
        IF ( Temp_flag<7 ) THEN
          WRITE ( Restart_outunit ) Tsta_elev_feet
          WRITE ( Restart_outunit ) Tsta_elev_meters
          WRITE ( Restart_outunit ) Tsta_elev
        ENDIF
        IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Precip_flag==5 ) THEN
          WRITE ( Restart_outunit ) Psta_elev_feet
          WRITE ( Restart_outunit ) Psta_elev_meters
          WRITE ( Restart_outunit ) Psta_elev
        ENDIF
        IF ( Precip_flag==5 .OR. Precip_flag==6 ) THEN
          WRITE ( Restart_outunit ) Tmax_rain_sta
          WRITE ( Restart_outunit ) Tmin_rain_sta
        ENDIF
        IF ( Precip_flag==5 .OR. Precip_flag==6 ) THEN
          WRITE ( Restart_outunit ) Adjust_snow
          WRITE ( Restart_outunit ) Adjust_rain
        ENDIF
        IF ( Et_flag==5 .OR. Et_flag==6 .OR. Et_flag==10 ) WRITE ( Restart_outunit ) Potet_coef_hru_mo
        IF ( Nsol>0 ) THEN
          WRITE ( Restart_outunit ) Hru_solsta
          WRITE ( Restart_outunit ) Rad_conv
          WRITE ( Restart_outunit ) Basin_solsta
        ENDIF
        IF ( Temp_flag<5 ) WRITE ( Restart_outunit ) Basin_tsta
        IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4 ) WRITE ( Restart_outunit ) Hru_tsta
        IF ( Temp_flag/=3 .AND. Temp_flag/=7 .AND. Temp_flag/=8 ) THEN
          WRITE ( Restart_outunit ) Tmax_adj
          WRITE ( Restart_outunit ) Tmin_adj
        ENDIF
        WRITE ( Restart_outunit ) Tmax_allsnow_hru_f
        IF ( Precip_flag==7 .OR. Precip_flag==8 ) WRITE ( Restart_outunit ) Tmax_allrain_hru_month_f
        IF ( Precip_flag==8 ) WRITE ( Restart_outunit ) Tmax_allrain_hru_mo
        IF ( Dprst_flag==1 ) THEN
          WRITE ( Restart_outunit ) Dprst_vol_open
          WRITE ( Restart_outunit ) Dprst_vol_clos
          WRITE ( Restart_outunit ) Dprst_vol_open_max
          WRITE ( Restart_outunit ) Dprst_vol_clos_max
          WRITE ( Restart_outunit ) Dprst_vol_thres_open
        ENDIF
        IF ( Strmflow_flag>1 ) THEN
          WRITE ( Restart_outunit ) Seginc_gwflow
          WRITE ( Restart_outunit ) Seginc_ssflow
          WRITE ( Restart_outunit ) Seginc_sroff
          WRITE ( Restart_outunit ) Seginc_swrad
          WRITE ( Restart_outunit ) Seg_inflow
          WRITE ( Restart_outunit ) Seg_outflow
          WRITE ( Restart_outunit ) Seg_lateral_inflow
          WRITE ( Restart_outunit ) Seg_upstream_inflow
        ENDIF
        IF ( Strmflow_flag==2 .AND. Nlake>0 ) THEN
          WRITE ( Restart_outunit ) Basin_lake_stor
          WRITE ( Restart_outunit ) Elevlake
          IF ( Nratetbl>0 ) WRITE ( Restart_outunit ) Basin_2ndstflow
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Tmax_allsnow, Tmax_allsnow_f, Basin_ppt, Basin_rain, Basin_snow, Basin_obs_ppt, Basin_temp, &
     &         Basin_tmax, Basin_tmin, Solrad_tmax, Solrad_tmin, Basin_transp_on, Basin_potet, Basin_horad, &
     &         Basin_potsw, Temp_units, Precip_units, Radj_sppt, Radj_wppt, Radmax, Orad
        READ ( Restart_inunit ) Basin_cfs, Basin_cms, Basin_ssflow_cfs, Basin_sroff_cfs, Basin_stflow_in, &
     &         Basin_gwflow_cfs, Basin_stflow_out, Basin_ssflow, Basin_soil_to_gw, Basin_actet, &
     &         Basin_swale_et, Basin_perv_et, Basin_soil_moist, Basin_ssstor, Basin_lakeevap, Basin_lake_stor, Basin_2ndstflow
        READ ( Restart_inunit ) Tmax_hru
        READ ( Restart_inunit ) Tmin_hru
        READ ( Restart_inunit ) Tmax_allrain_f
        READ ( Restart_inunit ) Newsnow
        READ ( Restart_inunit ) Pptmix
        READ ( Restart_inunit ) Hru_ppt
        READ ( Restart_inunit ) Hru_rain
        READ ( Restart_inunit ) Hru_snow
        READ ( Restart_inunit ) Prmx
        READ ( Restart_inunit ) Tmaxf
        READ ( Restart_inunit ) Tminf
        READ ( Restart_inunit ) Tavgf
        READ ( Restart_inunit ) Tmaxc
        READ ( Restart_inunit ) Tminc
        READ ( Restart_inunit ) Tavgc
        READ ( Restart_inunit ) Transp_on
        READ ( Restart_inunit ) Potet
        READ ( Restart_inunit ) Swrad
        READ ( Restart_inunit ) Pkwater_equiv
        READ ( Restart_inunit ) Soil_moist_max
        READ ( Restart_inunit ) Soil_rechr_max
        READ ( Restart_inunit ) Hru_actet
        READ ( Restart_inunit ) Soil_to_gw
        READ ( Restart_inunit ) Slow_flow
        READ ( Restart_inunit ) Soil_moist
        READ ( Restart_inunit ) Soil_to_ssr
        READ ( Restart_inunit ) Ssres_in
        READ ( Restart_inunit ) Ssr_to_gw
        READ ( Restart_inunit ) Slow_stor
        READ ( Restart_inunit ) Ssres_stor
        READ ( Restart_inunit ) Ssres_flow
        READ ( Restart_inunit ) Soil_rechr
        IF ( Soilzone_flag==1 ) READ ( Restart_inunit ) Sat_threshold
        READ ( Restart_inunit ) Sroff
        READ ( Restart_inunit ) Imperv_stor
        READ ( Restart_inunit ) Infil
        READ ( Restart_inunit ) Gwres_stor
        READ ( Restart_inunit ) Snowinfil_max
        READ ( Restart_inunit ) Imperv_stor_max
        READ ( Restart_inunit ) Ppt_rad_adj
        READ ( Restart_inunit ) Tmax_allrain
        IF ( Precip_flag/=8 ) READ ( Restart_inunit ) Adjmix_rain
        READ ( Restart_inunit ) Hru_outflow
        IF ( Temp_flag<7 ) THEN
          READ ( Restart_inunit ) Tsta_elev_feet
          READ ( Restart_inunit ) Tsta_elev_meters
          READ ( Restart_inunit ) Tsta_elev
        ENDIF
        IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Precip_flag==5 ) THEN
          READ ( Restart_inunit ) Psta_elev_feet
          READ ( Restart_inunit ) Psta_elev_meters
          READ ( Restart_inunit ) Psta_elev
        ENDIF
        IF ( Precip_flag==5 .OR. Precip_flag==6 ) THEN
          READ ( Restart_inunit ) Tmax_rain_sta
          READ ( Restart_inunit ) Tmin_rain_sta
        ENDIF
        IF ( Precip_flag==5 .OR. Precip_flag==6 ) THEN
          READ ( Restart_inunit ) Adjust_snow
          READ ( Restart_inunit ) Adjust_rain
        ENDIF
        IF ( Et_flag==5 .OR. Et_flag==6 .OR. Et_flag==10 ) READ ( Restart_inunit ) Potet_coef_hru_mo
        IF ( Nsol>0 ) THEN
          READ ( Restart_inunit ) Hru_solsta
          READ ( Restart_inunit ) Rad_conv
          READ ( Restart_inunit ) Basin_solsta
        ENDIF
        IF ( Temp_flag<5 ) READ ( Restart_inunit ) Basin_tsta
        IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4 ) READ ( Restart_inunit ) Hru_tsta
        IF ( Temp_flag/=3 .AND. Temp_flag/=7 .AND. Temp_flag/=8 ) THEN
          READ ( Restart_inunit ) Tmax_adj
          READ ( Restart_inunit ) Tmin_adj
        ENDIF
        READ ( Restart_inunit ) Tmax_allsnow_hru_f
        IF ( Precip_flag==7 .OR. Precip_flag==8 ) READ ( Restart_inunit ) Tmax_allrain_hru_month_f
        IF ( Precip_flag==8 ) READ ( Restart_inunit ) Tmax_allrain_hru_mo
        IF ( Dprst_flag==1 ) THEN
          READ ( Restart_inunit ) Dprst_vol_open
          READ ( Restart_inunit ) Dprst_vol_clos
          READ ( Restart_inunit ) Dprst_vol_open_max
          READ ( Restart_inunit ) Dprst_vol_clos_max
          READ ( Restart_inunit ) Dprst_vol_thres_open
        ENDIF
        IF ( Strmflow_flag>1 ) THEN
          READ ( Restart_inunit ) Seginc_gwflow
          READ ( Restart_inunit ) Seginc_ssflow
          READ ( Restart_inunit ) Seginc_sroff
          READ ( Restart_inunit ) Seginc_swrad
          READ ( Restart_inunit ) Seg_inflow
          READ ( Restart_inunit ) Seg_outflow
          READ ( Restart_inunit ) Seg_lateral_inflow
          READ ( Restart_inunit ) Seg_upstream_inflow
        ENDIF
        IF ( Strmflow_flag==2 .AND. Nlake>0 ) THEN
          READ ( Restart_inunit ) Basin_lake_stor
          READ ( Restart_inunit ) Elevlake
          IF ( Nratetbl>0 ) READ ( Restart_inunit ) Basin_2ndstflow
        ENDIF
      ENDIF
      END SUBROUTINE climateflow_restart
