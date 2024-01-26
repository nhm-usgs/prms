!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
      MODULE PRMS_CLIMATEVARS
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Common States and Fluxes'
      character(len=11), parameter :: MODNAME = 'climateflow'
      character(len=*), parameter :: Version_climateflow = '2024-01-25'
      INTEGER, SAVE :: Use_pandata, Solsta_flag
      ! Tmax_hru and Tmin_hru are in temp_units
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tmax_hru(:), Tmin_hru(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_elev_feet(:), Tsta_elev_meters(:)
      REAL, SAVE, ALLOCATABLE :: Psta_elev_feet(:), Psta_elev_meters(:)
!   Declared Variables - Precip
      INTEGER, SAVE, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      DOUBLE PRECISION, SAVE :: Basin_ppt, Basin_rain, Basin_snow, Basin_obs_ppt
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_ppt(:), Hru_rain(:), Hru_snow(:), Prmx(:)
!   Declared Variables - Temp
      DOUBLE PRECISION, SAVE :: Basin_temp, Basin_tmax, Basin_tmin
      DOUBLE PRECISION, SAVE :: Solrad_tmax, Solrad_tmin
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tmaxf(:), Tminf(:), Tavgf(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tmaxc(:), Tminc(:), Tavgc(:)
!   Declared Variables - Transp
      INTEGER, SAVE :: Basin_transp_on
      INTEGER, SAVE, ALLOCATABLE :: Transp_on(:)
!   Declared Parameters and Variables - Potential ET
      DOUBLE PRECISION, SAVE :: Basin_potet, Basin_humidity
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Potet(:)
      ! for potet_pt, potet_pm and potet_pm_sta
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tempc_dewpt(:), Vp_actual(:), Lwrad_net(:), Vp_slope(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Vp_sat(:)
      REAL, SAVE, ALLOCATABLE :: Humidity_percent(:, :)
!   Declared Parameters and Variables - Solar Radiation
      INTEGER, SAVE :: Basin_solsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_solsta(:), Hru_pansta(:)
      DOUBLE PRECISION, SAVE :: Basin_potsw, Basin_swrad, Basin_orad, Basin_horad, Orad, Rad_conv
      REAL, SAVE :: Rad_conv_sngl
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Swrad(:), Orad_hru(:)
      REAL, SAVE, ALLOCATABLE :: Ppt_rad_adj(:, :), Radmax(:, :), Radj_sppt(:), Radj_wppt(:)
!   Declared Parameters - Temp
      INTEGER, SAVE :: Temp_units, Basin_tsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_tsta(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_elev(:), Tmax_aspect_adjust(:, :), Tmin_aspect_adjust(:, :)
!   Declared Parameters - Precip
      INTEGER, SAVE :: Precip_units
      DOUBLE PRECISION, SAVE :: Ppt_zero_thresh
      REAL, SAVE :: Ppt_zero_thresh_sngl
      REAL, SAVE, ALLOCATABLE :: Adjmix_rain(:, :), Tmax_allrain(:, :), Tmax_allrain_offset(:, :)
      REAL, SAVE, ALLOCATABLE :: Psta_elev(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tmax_allsnow_f(:, :), Tmax_allsnow_c(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tmax_allrain_f(:, :), Tmax_allsnow(:, :)
!   Declared Parameters - Intcp
      REAL, SAVE, ALLOCATABLE :: Epan_coef_sngl(:, :), Potet_sublim(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Epan_coef(:, :)
      END MODULE PRMS_CLIMATEVARS

!***********************************************************************
! Declares parameters and variables related to flows from soilzone,
! smbal, ssflow, srunoff_carea, srunoff_smidx
!***********************************************************************
      MODULE PRMS_FLOWVARS
      IMPLICIT NONE
!   Declared Variables
      ! snow
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Pkwater_equiv(:)
      ! soilzone
      DOUBLE PRECISION, SAVE :: Basin_ssflow, Basin_soil_to_gw
      DOUBLE PRECISION, SAVE :: Basin_actet, Basin_lakeevap, Basin_soil_rechr
      DOUBLE PRECISION, SAVE :: Basin_swale_et, Basin_perv_et, Basin_sroff
      DOUBLE PRECISION, SAVE :: Basin_soil_moist, Basin_ssstor
      REAL, SAVE, ALLOCATABLE :: Hru_actet(:), Soil_moist(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_gw(:), Slow_flow(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_ssr(:), Ssres_in(:)
      REAL, SAVE, ALLOCATABLE :: Ssr_to_gw(:), Slow_stor(:), Pref_flow_stor(:)
      REAL, SAVE, ALLOCATABLE :: Ssres_stor(:), Ssres_flow(:), Soil_rechr(:)
      REAL, SAVE, ALLOCATABLE :: Soil_lower_stor_max(:), Soil_moist_tot(:), Soil_zone_max(:)
      DOUBLE PRECISION, SAVE :: Basin_ag_soil_moist, Basin_ag_soil_rechr
      REAL, SAVE, ALLOCATABLE :: Ag_soil_moist(:), Ag_soil_rechr(:), Ag_soil_rechr_max(:)
      ! srunoff
      REAL, SAVE, ALLOCATABLE :: Sroff(:), Imperv_stor(:), Infil(:), Hru_impervstor(:)
      ! Surface-Depression Storage
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open(:), Dprst_vol_clos(:), Dprst_stor_hru(:)
      ! gwflow
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwres_stor(:)
      ! lakes
      DOUBLE PRECISION, SAVE :: Basin_lake_stor
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_vol(:)
      ! streamflow
      DOUBLE PRECISION, SAVE :: Basin_cfs, Basin_cms, Basin_ssflow_cfs, Basin_sroff_cfs
      DOUBLE PRECISION, SAVE :: Basin_stflow_in, Basin_gwflow_cfs, Basin_stflow_out, Flow_out
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seg_upstream_inflow(:), Seg_lateral_inflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seg_outflow(:), Seg_inflow(:)
      ! glacr
      REAL, SAVE, ALLOCATABLE :: Glacier_frac(:), Alt_above_ela(:), Glrette_frac(:), Glacrb_melt(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Soil_moist_max(:), Soil_rechr_max(:), Sat_threshold(:)
      REAL, SAVE, ALLOCATABLE :: Snowinfil_max(:), Imperv_stor_max(:), Ssstor_init_frac(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_init_frac(:), Soil_rechr_init_frac(:), Soil_rechr_max_frac(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_moist_max(:), Ag_soil_rechr_max_frac(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_moist_init_frac(:), Ag_soil_rechr_init_frac(:)
      END MODULE PRMS_FLOWVARS

module PRMS_IT0_VARS
       IMPLICIT NONE
!   Global Variables
       DOUBLE PRECISION, SAVE :: It0_basin_ssstor, It0_basin_soil_moist
       DOUBLE PRECISION, SAVE :: It0_basin_ag_soil_moist, It0_basin_ag_soil_rechr
       DOUBLE PRECISION, SAVE, ALLOCATABLE :: It0_dprst_stor_hru(:)
       REAL, SAVE, ALLOCATABLE :: It0_soil_moist(:), It0_soil_rechr(:), It0_hru_impervstor(:), It0_ssres_stor(:)
       DOUBLE PRECISION, SAVE, ALLOCATABLE :: It0_pkwater_equiv(:)
       REAL, SAVE, ALLOCATABLE :: It0_ag_soil_moist(:), It0_ag_soil_rechr(:)
end module PRMS_IT0_VARS

!***********************************************************************
!     Main climateflow routine
!***********************************************************************
      INTEGER FUNCTION climateflow()
      USE PRMS_CONSTANTS, ONLY: DECL, INIT, CLEAN, ACTIVE, OFF, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: climateflow_decl, climateflow_init
      EXTERNAL :: climateflow_restart
!***********************************************************************
      climateflow = 0

      IF ( Process_flag==DECL ) THEN
        climateflow = climateflow_decl()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL climateflow_restart(READ_INIT)
        climateflow = climateflow_init()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL climateflow_restart(SAVE_INIT)
      ENDIF

      END FUNCTION climateflow

!***********************************************************************
!     climateflow_decl - declare climate and flow variables and parameters
!***********************************************************************
      INTEGER FUNCTION climateflow_decl()
      USE PRMS_CONSTANTS, ONLY: DOCUMENTATION, ACTIVE, OFF, MONTHS_PER_YEAR, ERROR_dim, &
     &    potet_pt_module, potet_pm_module, potet_pm_sta_module, climate_hru_module, &
     &    precip_laps_module, xyz_dist_module, ide_dist_module, temp_1sta_module, &
     &    temp_laps_module, temp_sta_module, temp_dist2_module, &
     &    ddsolrad_module, ccsolrad_module
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Nsegment, Nevap, Nlake, Ntemp, Nrain, Nsol, Ngw, &
     &    Model, Init_vars_from_file, Temp_flag, Precip_flag, Glacier_flag, &
     &    Strmflow_module, Temp_module, Stream_order_flag, Humidity_cbh_flag, &
     &    Precip_module, Solrad_module, Transp_module, Et_module, PRMS4_flag, &
     &    Soilzone_module, Srunoff_module, Call_cascade, Et_flag, Dprst_flag, Solrad_flag, AG_flag
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      USE PRMS_IT0_VARS
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, declparam
      EXTERNAL :: read_error, print_module
!***********************************************************************
      climateflow_decl = 0

      CALL print_module(MODDESC, MODNAME, Version_climateflow)

      ALLOCATE ( Tmaxf(Nhru) )
      IF ( declvar(Temp_module, 'tmaxf', 'nhru', Nhru, 'double', &
     &     'Maximum air temperature distributed to each HRU', &
     &     'degrees Fahrenheit', Tmaxf)/=0 ) CALL read_error(3, 'tmaxf')

      ALLOCATE ( Tminf(Nhru) )
      IF ( declvar(Temp_module, 'tminf', 'nhru', Nhru, 'double', &
     &     'Minimum air temperature distributed to each HRU', &
     &     'degrees Fahrenheit', Tminf)/=0 ) CALL read_error(3, 'tminf')

      ALLOCATE ( Tavgf(Nhru) )
      IF ( declvar(Temp_module, 'tavgf', 'nhru', Nhru, 'double', &
     &     'Average air temperature distributed to each HRU', &
     &     'degrees Fahrenheit', Tavgf)/=0 ) CALL read_error(3, 'tavgf')

      ALLOCATE ( Tmaxc(Nhru) )
      IF ( declvar(Temp_module, 'tmaxc', 'nhru', Nhru, 'double', &
     &     'Maximum air temperature distributed to each HRU', &
     &     'degrees Celsius', Tmaxc)/=0 ) CALL read_error(3, 'tmaxc')

      ALLOCATE ( Tminc(Nhru) )
      IF ( declvar(Temp_module, 'tminc', 'nhru', Nhru, 'double', &
     &     'Minimum air temperature distributed to each HRU', &
     &     'degrees Celsius', Tminc)/=0 ) CALL read_error(3, 'tminc')

      ALLOCATE ( Tavgc(Nhru) )
      IF ( declvar(Temp_module, 'tavgc', 'nhru', Nhru, 'double', &
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

      IF ( declvar(Temp_module, 'solrad_tmax', 'one', 1, 'double', &
     &     'Basin daily maximum temperature for use with solar radiation calculations', &
     &     'temp_units', Solrad_tmax)/=0 ) CALL read_error(3, 'solrad_tmax')

      IF ( declvar(Temp_module, 'solrad_tmin', 'one', 1, 'double', &
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
      IF ( declvar(Precip_module, 'prmx', 'nhru', Nhru, 'double', &
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
      IF ( declvar(Precip_module, 'hru_ppt', 'nhru', Nhru, 'double', &
     &     'Precipitation distributed to each HRU', &
     &     'inches', Hru_ppt)/=0 ) CALL read_error(3, 'hru_ppt')

      ALLOCATE ( Hru_rain(Nhru) )
      IF ( declvar(Precip_module, 'hru_rain', 'nhru', Nhru, 'double', &
     &     'Rain distributed to each HRU', &
     &     'inches', Hru_rain)/=0 ) CALL read_error(3, 'hru_rain')

      ALLOCATE ( Hru_snow(Nhru) )
      IF ( declvar(Precip_module, 'hru_snow', 'nhru', Nhru, 'double', &
     &     'Snow distributed to each HRU', &
     &     'inches', Hru_snow)/=0 ) CALL read_error(3, 'hru_snow')

! Solar Radiation variables
      ALLOCATE ( Swrad(Nhru) )
      IF ( declvar(Solrad_module, 'swrad', 'nhru', Nhru, 'double', &
     &     'Shortwave radiation distributed to each HRU', &
     &     'Langleys', Swrad)/=0 ) CALL read_error(3, 'swrad')

      IF ( declvar(Solrad_module, 'orad', 'one', 1, 'double', &
     &     'Measured or computed solar radiation on a horizontal surface', &
     &     'Langleys', Orad)/=0 ) CALL read_error(3, 'orad')

      IF ( declvar(Solrad_module, 'basin_horad', 'one', 1, 'double', &
     &     'Potential shortwave radiation for the basin centroid', &
     &     'Langleys', Basin_horad)/=0 ) CALL read_error(3, 'basin_horad')

      IF ( declvar(Solrad_module, 'basin_swrad', 'one', 1, 'double', &
     &     'Basin area-weighted average shortwave radiation', &
     &     'Langleys', Basin_swrad)/=0 ) CALL read_error(3, 'basin_swrad')

      IF ( declvar(Solrad_module, 'basin_potsw', 'one', 1, 'double', &
     &     'Basin area-weighted average shortwave radiation', &
     &     'Langleys', Basin_potsw)/=0 ) CALL read_error(3, 'basin_potsw')

      IF ( Solrad_flag==ddsolrad_module .OR. Solrad_flag==ccsolrad_module .OR. Model==DOCUMENTATION ) THEN
        IF ( declvar(Solrad_module, 'basin_orad', 'one', 1, 'double', &
     &       'Basin area-weighted average solar radiation on a horizontal surface', &
     &       'Langleys', Basin_orad)/=0 ) CALL read_error(3, 'basin_orad')

        ALLOCATE ( Orad_hru(Nhru) )
        IF ( declvar(Solrad_module, 'orad_hru', 'nhru', Nhru, 'double', &
     &       'Solar radiation on a horizontal surface for each HRU', &
     &       'Langleys', Orad_hru)/=0 ) CALL read_error(3, 'orad_hru')
      ENDIF

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
      IF ( declvar(Et_module, 'potet', 'nhru', Nhru, 'double', &
     &     'Potential ET for each HRU', &
     &     'inches', Potet)/=0 ) CALL read_error(3, 'potet')

      IF ( declvar(Et_module, 'basin_potet', 'one', 1, 'double', &
     &     'Basin area-weighted average potential ET', &
     &     'inches', Basin_potet)/=0 ) CALL read_error(3, 'basin_potet')

      IF ( Et_flag==potet_pt_module .OR. Et_flag==potet_pm_module .OR. &
     &     Et_flag==potet_pm_sta_module .OR. Model==DOCUMENTATION ) THEN
        IF ( declvar(Et_module, 'basin_humidity', 'one', 1, 'double', &
     &       'Basin area-weighted average humidity', &
     &       'percentage', Basin_humidity)/=0 ) CALL read_error(3, 'basin_humidity')
        ALLOCATE ( Tempc_dewpt(Nhru) )
        IF ( declvar(Et_module, 'tempc_dewpt', 'nhru', Nhru, 'double', &
     &       'Air temperature at dew point for each HRU', &
     &       'degrees Celsius', Tempc_dewpt)/=0 ) CALL read_error(3, 'tempc_dewpt')
        ALLOCATE ( Vp_actual(Nhru) )
        IF ( declvar(Et_module, 'vp_actual', 'nhru', Nhru, 'double', &
     &       'Actual vapor pressure for each HRU', &
     &       'kilopascals', Vp_actual)/=0 ) CALL read_error(3, 'vp_actual')
        ALLOCATE ( Lwrad_net(Nhru) )
        IF ( declvar(Et_module, 'lwrad_net', 'nhru', Nhru, 'double', &
     &       'Net long-wave radiation for each HRU', &
     &       'megajoules/m**2/day', Lwrad_net)/=0 ) CALL read_error(3, 'lwrad_net')
        ALLOCATE ( Vp_slope(Nhru) )
        IF ( declvar(Et_module, 'vp_slope', 'nhru', Nhru, 'double', &
     &       'Slope of saturation vapor pressure versus air temperature curve for each HRU', &
     &       'kilopascals/degrees Celsius', Vp_slope)/=0 ) CALL read_error(3, 'vp_slope')
        IF ( Et_flag==potet_pm_module .OR. Et_flag==potet_pm_sta_module .OR. Model==DOCUMENTATION ) THEN
          ALLOCATE ( Vp_sat(Nhru) )
          IF ( declvar(Et_module, 'vp_sat', 'nhru', Nhru, 'double', &
     &         'Saturation vapor pressure for each HRU', &
     &         'kilopascals', Vp_sat)/=0 ) CALL read_error(3, 'vp_sat')
        ENDIF
        IF ( (Et_flag==potet_pm_module .OR. Et_flag==potet_pt_module .AND. Humidity_cbh_flag==OFF) .OR. Model==DOCUMENTATION ) THEN
          ALLOCATE ( Humidity_percent(Nhru,MONTHS_PER_YEAR) )
          IF ( declparam(Et_module, 'humidity_percent', 'nhru,nmonths', 'real', &
     &         '0.0', '0.0', '100.0', &
     &         'Monthy humidity for each HRU', &
     &         'Monthy humidity for each HRU', &
     &         'percentage')/=0 ) CALL read_error(1, 'humidity_percent')
        ENDIF
      ENDIF

! Soilzone variables
      ALLOCATE ( Soil_rechr(Nhru), It0_soil_rechr(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_rechr', 'nhru', Nhru, 'real', &
     &     'Storage for recharge zone (upper portion) of the'// &
     &     ' capillary reservoir that is available for both evaporation and transpiration', &
     &     'inches', Soil_rechr)/=0 ) CALL read_error(3, 'soil_rechr')

      ALLOCATE ( Ssr_to_gw(Nssr) )
      IF ( declvar(Soilzone_module, 'ssr_to_gw', 'nssr', Nssr, 'real', &
     &     'Drainage from the gravity-reservoir to the associated GWR for each HRU', &
     &     'inches', Ssr_to_gw)/=0 ) CALL read_error(3, 'ssr_to_gw')

      ALLOCATE ( Ssres_stor(Nssr), It0_ssres_stor(Nhru) )
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

      ALLOCATE ( Soil_moist(Nhru), It0_soil_moist(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_moist', 'nhru', Nhru, 'real', &
     &     'Storage of capillary reservoir for each HRU', &
     &     'inches', Soil_moist)/=0 ) CALL read_error(3, 'soil_moist')

      ALLOCATE ( Soil_moist_tot(Nhru), Soil_lower_stor_max(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_moist_tot', 'nhru', Nhru, 'real', &
     &     'Total soil-zone water storage (soil_moist + ssres_stor)', &
     &     'inches', Soil_moist_tot)/=0 ) CALL read_error(3, 'soil_moist_tot')

      ALLOCATE ( Soil_zone_max(Nhru) )
!      IF ( declvar(Soilzone_module, 'soil_zone_max', 'nhru', Nhru, 'real', &
!     &     'Maximum storage of all soil zone reservoirs', &
!     &     'inches', Soil_zone_max)/=0 ) CALL read_error(3, 'soil_zone_max')

      ALLOCATE ( Pref_flow_stor(Nhru) )
      IF ( declvar(Soilzone_module, 'pref_flow_stor', 'nhru', Nhru, 'real', &
     &     'Storage in preferential-flow reservoir for each HRU', &
     &     'inches', Pref_flow_stor)/=0 ) CALL read_error(3, 'pref_flow_stor')

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

      IF ( declvar(Soilzone_module, 'basin_soil_rechr', 'one', 1, 'double', &
     &     'Basin area-weighted average storage for recharge zone;'// &
     &     ' upper portion of capillary reservoir where both evaporation and transpiration occurs', &
     &     'inches', Basin_soil_rechr)/=0 ) CALL read_error(3, 'basin_soil_rechr')

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

      IF ( declvar(Srunoff_module, 'basin_sroff', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff to the stream network', &
     &     'inches', Basin_sroff)/=0 ) CALL read_error(3, 'basin_sroff')

      ALLOCATE ( Hru_impervstor(Nhru), It0_hru_impervstor(Nhru) )
      IF ( declvar(Srunoff_module, 'hru_impervstor', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average storage on impervious area for each HRU', &
     &     'inches', Hru_impervstor)/=0 ) CALL read_error(3, 'hru_impervstor')

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
     &     'Basin area-weighted average surface runoff to the stream network', &
     &     'cfs', Basin_sroff_cfs)/=0 ) CALL read_error(3, 'basin_sroff_cfs')

      IF ( declvar(Strmflow_module, 'basin_ssflow_cfs', 'one', 1, 'double', &
     &     'Interflow leaving the basin through the stream network', &
     &     'cfs', Basin_ssflow_cfs)/=0 ) CALL read_error(3, 'basin_ssflow')

      IF ( declvar(Strmflow_module, 'basin_gwflow_cfs', 'one', 1, 'double', &
     &     'Basin area-weighted average of groundwater flow to the stream network', &
     &     'cfs', Basin_gwflow_cfs)/=0 ) CALL read_error(3, 'basin_gwflow_cfs')

      IF ( Call_cascade==1 .OR. Stream_order_flag==ACTIVE ) THEN
        IF ( Nsegment==0 .AND. Model/=DOCUMENTATION ) THEN
          PRINT *, 'ERROR, nsegment=0, must be > 0 for selected module options'
          ERROR STOP ERROR_dim
        ENDIF
      ENDIF

      IF ( Stream_order_flag==ACTIVE ) THEN
        ALLOCATE ( Seg_outflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_outflow', 'nsegment', Nsegment, 'double', &
     &       'Streamflow leaving a segment', &
     &       'cfs', Seg_outflow)/=0 ) CALL read_error(3, 'seg_outflow')

        ALLOCATE ( Seg_inflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_inflow', 'nsegment', Nsegment, 'double', &
     &       'Total flow entering a segment', &
     &       'cfs', Seg_inflow)/=0 ) CALL read_error(3, 'seg_inflow')

        IF ( declvar(Strmflow_module, 'flow_out', 'one', 1, 'double', &
     &       'Total flow out of model domain', &
     &       'cfs', Flow_out)/=0 ) CALL read_error(3, 'flow_out')

        ALLOCATE ( Seg_lateral_inflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_lateral_inflow', 'nsegment', Nsegment, 'double', &
     &       'Lateral inflow entering a segment', &
     &       'cfs', Seg_lateral_inflow)/=0 ) CALL read_error(3, 'seg_lateral_inflow')

        ALLOCATE ( Seg_upstream_inflow(Nsegment) )
        IF ( declvar(Strmflow_module, 'seg_upstream_inflow', 'nsegment', Nsegment, 'double', &
     &       'Sum of inflow from upstream segments', &
     &       'cfs', Seg_upstream_inflow)/=0 ) CALL read_error(3, 'seg_upstream_inflow')
      ENDIF

      IF ( Nlake>0 ) THEN 
        IF ( declvar(Soilzone_module, 'basin_lakeevap', 'one', 1, 'double', &
     &       'Basin area-weighted average lake evaporation', &
     &       'inches', Basin_lakeevap)/=0 ) CALL read_error(3, 'basin_lakeevap')
        ALLOCATE ( Lake_vol(Nlake) )
        IF ( declvar(Strmflow_module, 'lake_vol', 'nlake', Nlake, 'double', &
     &       'Storage in each lake using broad-crested weir or gate opening routing', &
     &       'acre-feet', Lake_vol)/=0 ) CALL read_error(3, 'lake_vol')
      ENDIF
      IF ( declvar(Strmflow_module, 'basin_lake_stor', 'one', 1, 'double', &
     &     'Basin volume-weighted average storage for all lakes using broad-crested weir or gate opening routing', &
     &     'inches', Basin_lake_stor)/=0 ) CALL read_error(3, 'basin_lake_stor')

      IF ( Dprst_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Dprst_vol_open(Nhru) )
        IF ( declvar(Srunoff_module, 'dprst_vol_open', 'nhru', Nhru, 'double', &
     &       'Storage volume in open surface depressions for each HRU', &
     &       'acre-inches', Dprst_vol_open)/=0 ) CALL read_error(3, 'dprst_vol_open')
        ALLOCATE ( Dprst_vol_clos(Nhru) )
        IF ( declvar(Srunoff_module, 'dprst_vol_clos', 'nhru', Nhru, 'double', &
     &       'Storage volume in closed surface depressions for each HRU', &
     &       'acre-inches', Dprst_vol_clos)/=0 ) CALL read_error(3, 'dprst_vol_clos')
        ALLOCATE ( Dprst_stor_hru(Nhru), It0_dprst_stor_hru(Nhru) )
        IF ( declvar(Srunoff_module, 'dprst_stor_hru', 'nhru', Nhru, 'double', &
     &       'Surface-depression storage for each HRU', &
     &       'inches', Dprst_stor_hru)/=0 ) CALL read_error(3, 'dprst_stor_hru')
      ENDIF

      ALLOCATE ( Pkwater_equiv(Nhru), It0_pkwater_equiv(Nhru) )
      IF ( declvar('snowcomp', 'pkwater_equiv', 'nhru', Nhru, 'double', &
     &     'Snowpack water equivalent on each HRU', &
     &     'inches', Pkwater_equiv)/=0 ) CALL read_error(3, 'pkwater_equiv')

! glacier variables
      IF ( Glacier_flag==1 .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Glacier_frac(Nhru) )
        IF ( declvar('glacr_melt', 'glacier_frac', 'nhru', Nhru, 'real', &
     &       'Fraction of glaciation (0=none; 1=100%)', &
     &       'decimal fraction', Glacier_frac)/=0 ) CALL read_error(3, 'glacier_frac')

        IF ( declvar('glacr_melt', 'glrette_frac', 'nhru', Nhru, 'real', &
     &       'Fraction of snow field (too small for glacier dynamics)', &
     &       'decimal fraction', Glrette_frac)/=0 ) CALL read_error(3, 'glrette_frac')

        ALLOCATE ( Alt_above_ela(Nhru) )
        IF ( declvar('glacr_melt', 'alt_above_ela', 'nhru', Nhru, 'real', &
     &       'Altitude above equilibrium line altitude (ELA)', &
     &       'elev_units', Alt_above_ela)/=0 ) CALL read_error(3, 'alt_above_ela')

        ALLOCATE ( Glacrb_melt(Nhru) )
        IF ( declvar('glacr_melt', 'glacrb_melt', 'nhru', Nhru, 'real', &
             'Glacier or glacierette basal melt, goes to soil', &
             'inches/day', Glacrb_melt)/=0 ) CALL read_error(3, 'glacrb_melt')
      ENDIF

      ! Allocate local variables
      IF ( Temp_flag<climate_hru_module .OR. Model==DOCUMENTATION ) ALLOCATE ( Tsta_elev_meters(Ntemp), Tsta_elev_feet(Ntemp) )
      IF ( Precip_flag==precip_laps_module .OR. Precip_flag==xyz_dist_module .OR. &
     &     Precip_flag==ide_dist_module .OR. Model==DOCUMENTATION ) &
     &     ALLOCATE ( Psta_elev_meters(Nrain), Psta_elev_feet(Nrain) )
      ALLOCATE ( Tmax_hru(Nhru), Tmin_hru(Nhru) )
      ALLOCATE ( Tmax_allrain_f(Nhru,MONTHS_PER_YEAR) )

! Declare Parameters
      IF ( Temp_flag<climate_hru_module .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Tsta_elev(Ntemp) )
        IF ( declparam(Temp_module, 'tsta_elev', 'ntemp', 'real', &
     &       '0.0', '-300.0', '30000.0', &
     &       'Temperature station elevation', &
     &       'Elevation of each air-temperature-measurement station', &
     &       'elev_units')/=0 ) CALL read_error(1, 'tsta_elev')
      ENDIF

      IF ( Temp_flag==temp_1sta_module .OR. Temp_flag==temp_laps_module .OR. Temp_flag==temp_sta_module &
     &     .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Hru_tsta(Nhru) )
        IF ( declparam(Temp_module, 'hru_tsta', 'nhru', 'integer', &
     &       '0', 'bounded', 'ntemp', &
     &       'Index of base temperature station for each HRU', &
     &       'Index of the base temperature station used for lapse rate calculations', &
     &       'none')/=0 ) CALL read_error(1, 'hru_tsta')
      ENDIF

      IF ( Temp_flag==temp_1sta_module .OR. Temp_flag==temp_laps_module .OR. Temp_flag==temp_dist2_module .OR. &
     &     Temp_flag==ide_dist_module .OR. Temp_flag==xyz_dist_module .OR. Temp_flag==temp_sta_module &
     &     .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Tmax_aspect_adjust(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(Temp_module, 'tmax_adj', 'nhru,nmonths', 'real', &
     &       '0.0', '-10.0', '10.0', &
     &       'HRU maximum temperature adjustment', &
     &       'Adjustment to maximum temperature for each HRU, estimated on the basis of slope and aspect', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_adj')

        ALLOCATE ( Tmin_aspect_adjust(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(Temp_module, 'tmin_adj', 'nhru,nmonths', 'real', &
     &       '0.0', '-10.0', '10.0', &
     &       'HRU minimum temperature adjustment', &
     &       'Adjustment to minimum temperature for each HRU, estimated on the basis of slope and aspect', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmin_adj')
      ENDIF

      ALLOCATE ( Potet_sublim(Nhru) )
      IF ( declparam('snowcomp', 'potet_sublim', 'nhru', 'real', &
     &     '0.5', '0.1', '0.75', &
     &     'Fraction of potential ET that is sublimated from snow for each HRU', &
     &     'Fraction of potential ET that is sublimated from snow in the canopy and snowpack for each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'potet_sublim')

      ALLOCATE ( Tmax_allrain_offset(Nhru,MONTHS_PER_YEAR), Tmax_allrain(Nhru,MONTHS_PER_YEAR) )
      IF ( PRMS4_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        IF ( declparam(Precip_module, 'tmax_allrain', 'nhru,nmonths', 'real', &
     &       '38.0', '-8.0', '75.0', &
     &       'Precipitation is rain if HRU max temperature >= this value', &
     &       'Monthly (January to December) maximum air temperature'// &
     &       ' when precipitation is assumed to be rain; if HRU air'// &
     &       ' temperature is greater than or equal to this value, precipitation is rain', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain')
      ENDIF
      IF ( PRMS4_flag==OFF .OR. Model==DOCUMENTATION ) THEN
        IF ( declparam(Precip_module, 'tmax_allrain_offset', 'nhru,nmonths', 'real', &
     &       '1.0', '0.0', '50.0', &
     &       'Precipitation is rain if HRU max temperature >= tmax_allsnow + this value', &
     &       'Monthly (January to December) maximum air temperature'// &
     &       ' when precipitation is assumed to be rain; if HRU air'// &
     &       ' temperature is greater than or equal to tmax_allsnow plus this value, precipitation is rain', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain_offset')
      ENDIF

      ALLOCATE ( Tmax_allsnow_f(Nhru,MONTHS_PER_YEAR), Tmax_allsnow_c(Nhru,MONTHS_PER_YEAR) )
      ALLOCATE ( Tmax_allsnow(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam(Precip_module, 'tmax_allsnow', 'nhru,nmonths', 'real', &
     &     '32.0', '-10.0', '40.0', &
     &     'Maximum temperature when precipitation is all snow', &
     &     'Maximum air temperature when precipitation is assumed'// &
     &     ' to be snow; if HRU air temperature is less than or equal to this value, precipitation is snow', &
     &     'temp_units')/=0 ) CALL read_error(1, 'tmax_allsnow')

      ALLOCATE ( Adjmix_rain(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam(Precip_module, 'adjmix_rain', 'nhru,nmonths', 'real', &
     &     '1.0', '0.0', '3.0', &
     &     'Adjustment factor for rain in a rain/snow mix', &
     &     'Monthly (January to December) multiplicative factor to adjust rain proportion in a mixed rain/snow event', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'adjmix_rain')

      IF ( declparam(Precip_module, 'ppt_zero_thresh', 'one', 'real', &
     &     '0.0', '0.0', '0.1', &
     &     'Precipitation below this amount is set to 0.0', &
     &     'Precipitation below this amount is set to 0.0', &
     &     'precip_units')/=0 ) CALL read_error(1, 'ppt_zero_thresh')

      IF ( Precip_flag==precip_laps_module .OR. Precip_flag==xyz_dist_module .OR. Precip_flag==ide_dist_module &
     &     .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Psta_elev(Nrain) )
        IF ( declparam(Precip_module, 'psta_elev', 'nrain', 'real', &
     &       '0.0', '-300.0', '30000.0', &
     &       'Precipitation station elevation', &
     &       'Elevation of each precipitation measurement station', &
     &       'elev_units')/=0 ) CALL read_error(1, 'psta_elev')
      ENDIF

      IF ( declparam(Temp_module, 'temp_units', 'one', 'integer', &
     &     '0', '0', '1', &
     &     'Units flag for measured temperature', &
     &     'Flag to indicate the units of measured air-temperature values (0=Fahrenheit; 1=Celsius)', &
     &     'none')/=0 ) CALL read_error(1, 'temp_units')

      IF ( Temp_flag==temp_1sta_module .OR. Temp_flag==temp_sta_module .OR. Temp_flag==temp_laps_module &
     &     .OR. Temp_flag==temp_dist2_module .OR. Model==DOCUMENTATION ) THEN
        IF ( declparam(Temp_module, 'basin_tsta', 'one', 'integer', &
     &       '0', 'bounded', 'ntemp', &
     &       'Index of main temperature station', &
     &       'Index of temperature station used to compute basin temperature values', &
     &       'none')/=0 ) CALL read_error(1, 'basin_tsta')
      ENDIF

      IF ( declparam(Precip_module, 'precip_units', 'one', 'integer', &
     &     '0', '0', '1', &
     &     'Units for measured precipitation', &
     &     'Units for measured precipitation (0=inches; 1=mm)', &
     &     'none')/=0 ) CALL read_error(1, 'precip_units')

      IF ( Solrad_flag==ddsolrad_module .OR. Solrad_flag==ccsolrad_module .OR. Model==DOCUMENTATION ) THEN
        IF ( Nsol>0 ) THEN
          IF ( declparam(Solrad_module, 'rad_conv', 'one', 'real', &
     &         '1.0', '0.1', '100.0', &
     &         'Conversion factor to Langleys for measured radiation', &
     &         'Conversion factor to Langleys for measured solar radiation', &
     &         'Langleys/radiation units')/=0 ) CALL read_error(1, 'rad_conv')

          IF ( declparam(Solrad_module, 'basin_solsta', 'one', 'integer', &
     &         '0', 'bounded', 'nsol', &
     &         'Index of main solar radiation station', &
     &         'Index of solar radiation station used to compute basin radiation values, used when dimension nsol>0', &
     &         'none')/=0 ) CALL read_error(1, 'basin_solsta')

          ALLOCATE ( Hru_solsta(Nhru) )
          IF ( declparam(Solrad_module, 'hru_solsta', 'nhru', 'integer', &
     &         '0', 'bounded', 'nsol', &
     &         'Index of solar radiation station associated with each HRU', &
     &         'Index of solar radiation station associated with each HRU', &
     &         'none')/=0 ) CALL read_error(1, 'hru_solsta')
        ENDIF

        ALLOCATE ( Ppt_rad_adj(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(Solrad_module, 'ppt_rad_adj', 'nhru,nmonths', 'real', &
     &       '0.02', '0.0', '0.5', &
     &       'Radiation reduced if HRU precipitation above this value', &
     &       'Monthly minimum precipitation, if HRU precipitation exceeds this value, radiation is'// &
     &       ' multiplied by radj_sppt or radj_wppt adjustment factor', &
     &       'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')
        ALLOCATE ( Radj_sppt(Nhru) )
        IF ( declparam(Solrad_module, 'radj_sppt', 'nhru', 'real', &
     &       '0.44', '0.0', '1.0', &
     &       'Adjustment to solar radiation on precipitation day - summer', &
     &       'Multiplicative adjustment factor for computed solar radiation for summer day with greater than'// &
     &       ' ppt_rad_adj inches of precipitation for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radj_sppt')
        ALLOCATE ( Radj_wppt(Nhru) )
        IF ( declparam(Solrad_module, 'radj_wppt', 'nhru', 'real', &
     &       '0.5', '0.0', '1.0', &
     &       'Adjustment to solar radiation on precipitation day - winter', &
     &       'Multiplicative adjustment factor for computed solar radiation for winter day with greater than'// &
     &       ' ppt_rad_adj inches of precipitation for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radj_wppt')
        ALLOCATE ( Radmax(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(Solrad_module, 'radmax', 'nhru,nmonths', 'real', &
     &       '0.8', '0.1', '1.0', &
     &       'Maximum fraction of potential solar radiation', &
     &       'Monthly (January to December) maximum fraction of the potential solar radiation'// &
     &       ' that may reach the ground due to haze, dust, smog, and so forth, for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radmax')
      ENDIF

      ALLOCATE ( Epan_coef(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam('intcp', 'epan_coef', 'nhru,nmonths', 'real', &
     &     '1.0', '0.01', '3.0', &
     &     'Evaporation pan coefficient', &
     &     'Monthly (January to December) evaporation pan coefficient for each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'epan_coef')

      Use_pandata = OFF
      IF ( Nevap>0 ) THEN
        Use_pandata = ACTIVE
        ALLOCATE ( Hru_pansta(Nhru) )
        IF ( declparam(Et_module, 'hru_pansta', 'nhru', 'integer', &
     &       '0', 'bounded', 'nevap', &
     &       'Index of pan evaporation station for each HRU', &
     &       'Index of pan evaporation station used to compute HRU potential ET', &
     &       'none')/=0 ) CALL read_error(1, 'hru_pansta')
      ENDIF

      ALLOCATE ( Sat_threshold(Nhru) )
      IF ( declparam(Soilzone_module, 'sat_threshold', 'nhru', 'real', &
     &     '999.0', '0.0', '999.0', &
     &     'Soil saturation threshold, above field-capacity threshold', &
     &     'Water holding capacity of the gravity and preferential-'// &
     &     'flow reservoirs; difference between field capacity and total soil saturation for each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'sat_threshold')

      ALLOCATE ( Soil_moist_max(Nhru) )
      IF ( declparam(Soilzone_module, 'soil_moist_max', 'nhru', 'real', &
     &     '2.0', '0.0', '30.0', &
     &     'Maximum value of water for soil zone', &
     &     'Maximum available water holding capacity of capillary'// &
     &     ' reservoir from land surface to rooting depth of the major vegetation type of each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'soil_moist_max')

      ALLOCATE ( Soil_rechr_max(Nhru) )
      IF ( PRMS4_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        IF ( declparam(Soilzone_module, 'soil_rechr_max', 'nhru', 'real', &
     &       '1.5', '0.0', '30.0', &
     &       'Maximum storage for soil recharge zone', &
     &       'Maximum storage for soil recharge zone (upper portion of'// &
     &       ' capillary reservoir where losses occur as both'// &
     &       ' evaporation and transpiration); must be less than or equal to soil_moist_max', &
     &       'inches')/=0 ) CALL read_error(1, 'soil_rechr_max')
      ENDIF
      IF ( PRMS4_flag==OFF .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Soil_rechr_max_frac(Nhru) )
        IF ( declparam(Soilzone_module, 'soil_rechr_max_frac', 'nhru', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Fraction of capillary reservoir where losses occur as both evaporation and transpiration (soil recharge zone)', &
     &       'Fraction of the capillary reservoir water-holding capacity (soil_moist_max) where losses occur as both'// &
     &       ' evaporation and transpiration (upper zone of capillary reservoir) for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'soil_rechr_max_frac')
      ENDIF

      ALLOCATE ( Snowinfil_max(Nhru) )
      IF ( declparam(Srunoff_module, 'snowinfil_max', 'nhru', 'real', &
     &     '2.0', '0.0', '20.0', &
     &     'Maximum snow infiltration per day', &
     &     'Maximum snow infiltration per day for each HRU', &
     &     'inches/day')/=0 ) CALL read_error(1, 'snowinfil_max')

      ALLOCATE ( Imperv_stor_max(Nhru) )
      IF ( declparam(Srunoff_module, 'imperv_stor_max', 'nhru', 'real', &
     &     '0.05', '0.0', '0.5', &
     &     'HRU maximum impervious area retention storage', &
     &     'Maximum impervious area retention storage for each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'imperv_stor_max')

      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==5 .OR. Model==DOCUMENTATION) THEN
        IF ( PRMS4_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
          IF ( declparam(Soilzone_module, 'soil_rechr_init', 'nhru', 'real', &
     &         '1.0', '0.0', '20.0', &
     &         'Initial storage of water for soil recharge zone', &
     &         'Initial storage for soil recharge zone (upper part of'// &
     &         ' capillary reservoir where losses occur as both'// &
     &         ' evaporation and transpiration) for each HRU; must be'// &
     &         ' less than or equal to soil_moist_init', &
     &         'inches')/=0 ) CALL read_error(1, 'soil_rechr_init')
          IF ( declparam(Soilzone_module, 'soil_moist_init', 'nhru', 'real', &
     &         '3.0', '0.0', '20.0', &
     &         'Initial value of available water in capillary reservoir', &
     &         'Initial value of available water in capillary reservoir for each HRU', &
     &         'inches')/=0 ) CALL read_error(1, 'soil_moist_init')
          IF ( declparam(Soilzone_module, 'ssstor_init', 'nssr', 'real', &
     &         '0.0', '0.0', '10.0', &
     &         'Initial storage in each GVR and PFR', &
     &         'Initial storage of the gravity and preferential-flow reservoirs for each HRU', &
     &         'inches')/=0 ) CALL read_error(1, 'ssstor_init')
        ENDIF
        IF ( PRMS4_flag==OFF .OR. Model==DOCUMENTATION ) THEN
          ALLOCATE ( Soil_rechr_init_frac(Nhru) )
          IF ( declparam(Soilzone_module, 'soil_rechr_init_frac', 'nhru', 'real', &
     &         '0.0', '0.0', '1.0', &
     &         'Initial fraction of available water in the soil recharge zone within the capillary reservoir', &
     &         'Initial fraction of available water in the capillary reservoir where losses occur'// &
     &         ' as both evaporation and transpiration (upper zone of capillary reservoir) for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'soil_rechr_init_frac')
          ALLOCATE ( Soil_moist_init_frac(Nhru) )
          IF ( declparam(Soilzone_module, 'soil_moist_init_frac', 'nhru', 'real', &
     &         '0.0', '0.0', '1.0', &
     &         'Initial fraction available water in the capillary reservoir', &
     &         'Initial fraction of available water in the capillary reservoir (fraction of soil_moist_max) for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'soil_moist_init_frac')
          ALLOCATE ( Ssstor_init_frac(Nhru) )
          IF ( declparam(Soilzone_module, 'ssstor_init_frac', 'nssr', 'real', &
     &         '0.0', '0.0', '1.0', &
     &         'Initial fraction of available water in the gravity plus preferential-flow reservoirs', &
     &         'Initial fraction of available water in the gravity plus preferential-flow reservoirs'// &
     &         ' (fraction of sat_threshold) for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'ssstor_init_frac')
        ENDIF
      ENDIF

      IF ( AG_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        IF ( declvar(Soilzone_module, 'basin_ag_soil_moist', 'one', 1, 'double', &
     &       'Basin area-weighted average soil agricultural capillary reservoir storage', &
     &       'inches', Basin_ag_soil_moist)/=0 ) CALL read_error(3, 'basin_ag_soil_moist')
        IF ( declvar(Soilzone_module, 'basin_ag_soil_rechr', 'one', 1, 'double', &
     &       'Basin area-weighted average storage for the agricultural recharge zone;'//&
     &       ' upper portion of agricultura capillary reservoir where both evaporation and transpiration occurs', &
     &       'inches', Basin_ag_soil_rechr)/=0 ) CALL read_error(3, 'basin_ag_soil_rechr')
        ALLOCATE ( Ag_soil_moist(Nhru), It0_ag_soil_moist(Nhru) )
        IF ( declvar(Soilzone_module, 'ag_soil_moist', 'nhru', Nhru, 'real', &
     &       'Storage of soil agriculture capillary reservoir for each HRU', &
     &       'inches', Ag_soil_moist)/=0 ) CALL read_error(3, 'ag_soil_moist')
        ALLOCATE ( Ag_soil_rechr(Nhru), It0_ag_soil_rechr(Nhru) )
        IF ( declvar(Soilzone_module, 'ag_soil_rechr', 'nhru', Nhru, 'real', &
     &       'Storage for upper portion of the soil agriculture capillary reservoir that is available for both'// &
     &       ' evaporation and transpiration', &
     &       'inches', Ag_soil_rechr)/=0 ) CALL read_error(3, 'ag_soil_rechr')
        ALLOCATE ( Ag_soil_moist_max(Nhru) )
        IF ( declparam(Soilzone_module, 'ag_soil_moist_max', 'nhru', 'real', &
     &       '-1.0', '-1.0', '20.0', &
     &       'Maximum value of water content for agriculture fraction of the soilzone', &
     &       'Maximum available water holding capacity of the agriculture'// &
     &       ' reservoir from land surface to rooting depth of the crop type of each HRU', &
     &       'inches')/=0 ) CALL read_error(1, 'ag_soil_moist_max')
        ALLOCATE ( Ag_soil_rechr_max_frac(Nhru), Ag_soil_rechr_max(Nhru) )
        IF ( declparam(Soilzone_module, 'ag_soil_rechr_max_frac', 'nhru', 'real', &
     &       '-1.0', '-1.0', '1.0', &
     &       'Fraction of agriculture reservoir where losses occur as both evaporation and transpiration (soil recharge zone)', &
     &       'Fraction of the agriculture reservoir water-holding capacity (ag_soil_moist_max) where losses occur as both'// &
     &       ' evaporation and transpiration (upper zone of agriculture reservoir) for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'ag_soil_rechr_max_frac')
        ALLOCATE ( Ag_soil_rechr_init_frac(Nhru) )
        IF ( declparam(Soilzone_module, 'ag_soil_rechr_init_frac', 'nhru', 'real', &
     &       '-1.0', '-1.0', '1.0', &
     &       'Initial fraction of available water in the soil recharge zone within the agriculture reservoir', &
     &       'Initial fraction of available water in the agriculture reservoir where losses occur'// &
     &       ' as both evaporation and transpiration (upper zone of agriculture reservoir) for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'ag_soil_rechr_init_frac')
        ALLOCATE ( Ag_soil_moist_init_frac(Nhru) )
        IF ( declparam(Soilzone_module, 'ag_soil_moist_init_frac', 'nhru', 'real', &
     &       '-1.0', '-1.0', '1.0', &
     &       'Initial fraction available water in the soil agriculture reservoir', &
     &       'Initial fraction of available water in the soil agriculture reservoir'// &
     &       ' (fraction of ag_soil_moist_max for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'ag_soil_moist_init_frac')
      ENDIF

      END FUNCTION climateflow_decl

!***********************************************************************
!     climateflow_init - Initialize module - get parameter values,
!                        set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION climateflow_init()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, MONTHS_PER_YEAR, DEBUG_less, &
     &    potet_pt_module, potet_pm_module, potet_pm_sta_module, climate_hru_module, &
     &    precip_laps_module, xyz_dist_module, ide_dist_module, temp_1sta_module, &
     &    temp_laps_module, temp_sta_module, temp_dist2_module, &
     &    FEET, FEET2METERS, METERS2FEET, FAHRENHEIT, INACTIVE, LAKE, ERROR_PARAM, ddsolrad_module, ccsolrad_module
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Nevap, Nlake, Ntemp, Nrain, Nsol, &
     &    Print_debug, Init_vars_from_file, Temp_flag, Precip_flag, &
     &    Temp_module, Precip_module, Solrad_module, Et_module, PRMS4_flag, &
     &    Soilzone_module, Srunoff_module, Et_flag, Dprst_flag, Solrad_flag, &
     &    Parameter_check_flag, Inputerror_flag, Humidity_cbh_flag, Glacier_flag, Stream_order_flag, &
     &    AG_flag, Nhru_nmonths
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      USE PRMS_BASIN, ONLY: Elev_units, Active_hrus, Hru_route_order, Hru_perv, Hru_area, Basin_area_inv, Hru_type
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: checkdim_param_limits, checkdim_bounded_limits, read_error
      DOUBLE PRECISION, EXTERNAL :: c_to_f, f_to_c
      INTRINSIC :: DBLE
! Local variables
      INTEGER :: i, j, ierr
      REAL, ALLOCATABLE :: tmax_allrain_sngl(:, :), tmax_allsnow_sngl(:, :)
!***********************************************************************
      climateflow_init = 0

      IF ( Temp_flag<climate_hru_module ) THEN
        IF ( getparam(Temp_module, 'tsta_elev', Ntemp, 'real', Tsta_elev)/=0 ) CALL read_error(2, 'tsta_elev')
        DO i = 1, Ntemp
          IF ( Elev_units==FEET ) THEN
            Tsta_elev_feet(i) = Tsta_elev(i)
            Tsta_elev_meters(i) = Tsta_elev_feet(i)*FEET2METERS
          ELSE
            Tsta_elev_meters(i) = Tsta_elev(i)
            Tsta_elev_feet(i) = Tsta_elev_meters(i)*METERS2FEET
          ENDIF
        ENDDO
      ENDIF

      IF ( getparam('snowcomp', 'potet_sublim', Nhru, 'real', Potet_sublim)/=0 ) CALL read_error(2, 'potet_sublim')

      IF ( Temp_flag==temp_1sta_module .OR. Temp_flag==temp_laps_module .OR. Temp_flag==temp_dist2_module .OR. &
     &     Temp_flag==ide_dist_module .OR. Temp_flag==xyz_dist_module .OR. Temp_flag==temp_sta_module ) THEN
        IF ( getparam(Temp_module, 'tmax_adj', Nhru_nmonths, 'real', Tmax_aspect_adjust)/=0 ) CALL read_error(2, 'tmax_adj')
        IF ( getparam(Temp_module, 'tmin_adj', Nhru_nmonths, 'real', Tmin_aspect_adjust)/=0 ) CALL read_error(2, 'tmin_adj')
      ENDIF

      IF ( getparam(Temp_module, 'temp_units', 1, 'integer', Temp_units)/=0 ) CALL read_error(2, 'temp_units')

      IF ( Temp_flag==temp_1sta_module .OR. Temp_flag==temp_sta_module .OR. Temp_flag==temp_laps_module .OR. &
     &     Temp_flag==temp_dist2_module ) THEN
        IF ( getparam(Temp_module, 'basin_tsta', 1, 'integer', Basin_tsta)/=0 ) CALL read_error(2, 'basin_tsta')
        CALL checkdim_param_limits(1, 'basin_tsta', 'ntemp', Basin_tsta, 1, Ntemp, Inputerror_flag)
      ELSE
        Basin_tsta = 0
      ENDIF

      IF ( Temp_flag==temp_1sta_module .OR. Temp_flag==temp_laps_module .OR. Temp_flag==temp_sta_module ) THEN
        IF ( getparam(Temp_module, 'hru_tsta', Nhru, 'integer', Hru_tsta)/=0 ) CALL read_error(2, 'hru_tsta')
        IF ( Parameter_check_flag>0 ) &
     &       CALL checkdim_bounded_limits('hru_tsta', 'ntemp', Hru_tsta, Nhru, 0, Ntemp, Inputerror_flag)
      ENDIF

      ALLOCATE ( tmax_allsnow_sngl(Nhru,MONTHS_PER_YEAR) )
      IF ( getparam(Precip_module, 'tmax_allsnow', Nhru_nmonths, 'real', tmax_allsnow_sngl)/=0 ) &
     &    CALL read_error(2, 'tmax_allsnow')
      Tmax_allsnow = DBLE( tmax_allsnow_sngl )

      IF ( PRMS4_flag==ACTIVE ) THEN
        ALLOCATE ( tmax_allrain_sngl(Nhru,MONTHS_PER_YEAR) )
        IF ( getparam(Precip_module, 'tmax_allrain', Nhru_nmonths, 'real', tmax_allrain_sngl)/=0 ) &
     &       CALL read_error(2, 'tmax_allrain')
        DO j = 1, MONTHS_PER_YEAR
          DO i = 1, Nhru
            Tmax_allrain_offset(i, j) = tmax_allrain_sngl(i, j) - tmax_allsnow_sngl(i, j)
            IF ( Tmax_allrain_offset(i, j)<0.0 ) THEN
              IF ( Print_debug>DEBUG_less ) PRINT *, 'WARNING, tmax_allsnow > tmax_allrain for HRU:', i, '; month:', j, &
     &                                               ' tmax_allrain set to tmax_allsnow'
              Tmax_allrain_offset(i, j) = 0.0
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE ( tmax_allrain_sngl )
      ELSE
        IF ( getparam(Precip_module, 'tmax_allrain_offset', Nhru_nmonths, 'real', Tmax_allrain_offset)/=0 ) &
     &                CALL read_error(2, 'tmax_allrain_offset')
      ENDIF

      ! Set tmax_allrain in units of the input values
      ! tmax_allsnow must be in the units of the input values
      IF ( Temp_units==FAHRENHEIT ) THEN
        Tmax_allsnow_f = Tmax_allsnow
        DO j = 1, MONTHS_PER_YEAR
          DO i = 1, Nhru
            Tmax_allrain_f(i, j) = Tmax_allsnow(i, j) + DBLE( Tmax_allrain_offset(i, j) )
            Tmax_allsnow_c(i, j) = f_to_c(Tmax_allsnow(i,j))
          ENDDO
        ENDDO
        Tmax_allrain = Tmax_allrain_f
      ELSE
        Tmax_allsnow_c = Tmax_allsnow
        DO i = 1, MONTHS_PER_YEAR
          DO j = 1, Nhru
            Tmax_allsnow_f(j, i) = c_to_f(Tmax_allsnow(j,i))
            Tmax_allrain(j, i) = Tmax_allsnow(j, i) + DBLE( Tmax_allrain_offset(j, i) )
            Tmax_allrain_f(j, i) = c_to_f(Tmax_allrain(j, i))
          ENDDO
        ENDDO
      ENDIF
      DEALLOCATE ( tmax_allsnow_sngl, Tmax_allsnow )
      DEALLOCATE ( Tmax_allrain_offset )

      IF ( getparam(Precip_module, 'adjmix_rain', Nhru_nmonths, 'real', Adjmix_rain)/=0 ) CALL read_error(2, 'adjmix_rain')

      IF ( getparam(Precip_module, 'precip_units', 1, 'integer', Precip_units)/=0 ) CALL read_error(2, 'precip_units')

      IF ( getparam(Precip_module, 'ppt_zero_thresh', 1, 'real', Ppt_zero_thresh_sngl)/=0 ) CALL read_error(2, 'ppt_zero_thresh')
      Ppt_zero_thresh = DBLE( Ppt_zero_thresh_sngl )

      IF ( Precip_flag==precip_laps_module .OR. Precip_flag==xyz_dist_module .OR. Precip_flag==ide_dist_module ) THEN
        IF ( getparam(Precip_module, 'psta_elev', Nrain, 'real', Psta_elev)/=0 ) CALL read_error(2, 'psta_elev')
        DO i = 1, Nrain
          IF ( Elev_units==FEET ) THEN
            Psta_elev_feet(i) = Psta_elev(i)
            Psta_elev_meters(i) = Psta_elev_feet(i)*FEET2METERS
          ELSE
            Psta_elev_meters(i) = Psta_elev(i)
            Psta_elev_feet(i) = Psta_elev_meters(i)*METERS2FEET
          ENDIF
        ENDDO
      ENDIF

      IF ( Solrad_flag==ddsolrad_module .OR. Solrad_flag==ccsolrad_module ) THEN
        Solsta_flag = OFF
        IF ( Nsol>0 ) THEN
          IF ( getparam(Solrad_module, 'basin_solsta', 1, 'integer', Basin_solsta)/=0 ) CALL read_error(2, 'basin_solsta')

          IF ( getparam(Solrad_module, 'rad_conv', 1, 'real', Rad_conv_sngl)/=0 ) CALL read_error(2, 'rad_conv')
          Rad_conv = DBLE( Rad_conv_sngl )

          IF ( getparam(Solrad_module, 'hru_solsta', Nhru, 'integer', Hru_solsta)/=0 ) CALL read_error(2, 'hru_solsta')

          IF ( Parameter_check_flag>0 ) THEN
            CALL checkdim_param_limits(1, 'basin_solsta', 'nsol', Basin_solsta, 1, Nsol, Inputerror_flag)
            CALL checkdim_bounded_limits('hru_solsta', 'nsol', Hru_solsta, Nhru, 0, Nsol, Inputerror_flag)
          ENDIF

          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            IF ( Hru_solsta(i)>0 ) THEN
              Solsta_flag = ACTIVE
              EXIT
            ENDIF
          ENDDO
        ENDIF
        IF ( getparam(Solrad_module, 'radj_sppt', Nhru, 'real', Radj_sppt)/=0 ) CALL read_error(2, 'radj_sppt')
        IF ( getparam(Solrad_module, 'radj_wppt', Nhru, 'real', Radj_wppt)/=0 ) CALL read_error(2, 'radj_wppt')
        IF ( getparam(Solrad_module, 'ppt_rad_adj', Nhru_nmonths, 'real', Ppt_rad_adj)/=0 ) &
     &       CALL read_error(2, 'ppt_rad_adj')
        IF ( getparam(Solrad_module, 'radmax', Nhru_nmonths, 'real', Radmax)/=0 ) CALL read_error(2, 'radmax')
      ELSE
        Basin_solsta = 0
        Rad_conv = 1.0D0
      ENDIF

      IF ( Use_pandata==ACTIVE ) THEN
        IF ( getparam('intcp', 'hru_pansta', Nhru, 'integer', Hru_pansta)/=0 ) CALL read_error(2, 'hru_pansta')
        IF ( Parameter_check_flag>0 ) &
     &       CALL checkdim_bounded_limits('hru_pansta', 'nevap', Hru_pansta, Nhru, 0, Nevap, Inputerror_flag)
      ENDIF

      IF ( getparam('intcp', 'epan_coef', Nhru_nmonths, 'real', Epan_coef)/=0 ) CALL read_error(2, 'epan_coef')
      Epan_coef = DBLE( Epan_coef_sngl )
      DEALLOCATE ( Epan_coef_sngl )

! FLOW VARIABLES AND PARAMETERS
      IF ( getparam(Soilzone_module, 'sat_threshold', Nhru, 'real', Sat_threshold)/=0 ) CALL read_error(2, 'sat_threshold')

      IF ( getparam(Soilzone_module, 'soil_moist_max', Nhru, 'real', Soil_moist_max)/=0 ) CALL read_error(2, 'soil_moist_max')

      IF ( PRMS4_flag==ACTIVE ) THEN
        IF ( getparam(Soilzone_module, 'soil_rechr_max', Nhru, 'real', Soil_rechr_max)/=0 ) CALL read_error(2, 'soil_rechr_max')
      ELSE
        IF ( getparam(Soilzone_module, 'soil_rechr_max_frac', Nhru, 'real', Soil_rechr_max_frac)/=0 ) &
     &       CALL read_error(2, 'soil_rechr_max_frac')
        Soil_rechr_max = Soil_rechr_max_frac * Soil_moist_max
      ENDIF

      ierr = 0
      IF ( Init_vars_from_file==OFF .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==5 ) THEN
        IF ( PRMS4_flag==ACTIVE ) THEN
          ! use PRMS4 parameters
          IF ( getparam(Soilzone_module, 'soil_moist_init', Nhru, 'real', Soil_moist)/=0 ) &
     &         CALL read_error(2, 'soil_moist_init')
          IF ( getparam(Soilzone_module, 'soil_rechr_init', Nhru, 'real', Soil_rechr)/=0 ) &
     &         CALL read_error(2, 'soil_rechr_init')
          IF ( getparam(Soilzone_module, 'ssstor_init', Nssr, 'real', Ssres_stor)/=0 ) &
     &         CALL read_error(2, 'ssstor_init')
! PRMS 5 parameters
        ELSE
          IF ( getparam(Soilzone_module, 'soil_moist_init_frac', Nhru, 'real', Soil_moist_init_frac)/=0 ) &
     &         CALL read_error(2, 'soil_moist_init_frac')
          IF ( getparam(Soilzone_module, 'soil_rechr_init_frac', Nhru, 'real', Soil_rechr_init_frac)/=0 ) &
     &         CALL read_error(2, 'soil_rechr_init_frac')
          IF ( getparam(Soilzone_module, 'ssstor_init_frac', Nssr, 'real', Ssstor_init_frac)/=0 ) &
     &         CALL read_error(2, 'ssstor_init_frac')
          Soil_rechr = Soil_rechr_init_frac * Soil_rechr_max
          Soil_moist = Soil_moist_init_frac * Soil_moist_max
          Ssres_stor = Ssstor_init_frac * Sat_threshold
          DEALLOCATE ( Ssstor_init_frac )
        ENDIF
        Slow_stor = Ssres_stor
      ENDIF

      IF ( AG_flag==ACTIVE ) THEN
        IF ( getparam(Soilzone_module, 'ag_soil_moist_max', Nhru, 'real', Ag_soil_moist_max)/=0 ) &
     &       CALL read_error(2, 'ag_soil_moist_max')
        IF ( Ag_soil_moist_max(1) < 0.0 ) then
          print *, 'WARNING, ag_soil_moist_max not specified, substituting soil_moist_max'
          Ag_soil_moist_max = Soil_moist_max
        ENDIF
        IF ( getparam(Soilzone_module, 'ag_soil_rechr_max_frac', Nhru, 'real', Ag_soil_rechr_max_frac)/=0 ) &
     &       CALL read_error(2, 'ag_soil_rechr_max_frac')
        IF ( Ag_soil_rechr_max_frac(1) < 0.0 ) THEN
          print *, 'WARNING, ag_soil_rechr_max_frac not specified, substituting soil_rechr_max_frac'
          Ag_soil_rechr_max_frac = Soil_rechr_max_frac
        ENDIF
        IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==5 ) THEN
          IF ( getparam(Soilzone_module, 'ag_soil_moist_init_frac', Nhru, 'real', Ag_soil_moist_init_frac)/=0 ) &
     &         CALL read_error(2, 'ag_soil_moist_init_frac')
          IF ( Ag_soil_moist_init_frac(1) < 0.0 ) then
            print *, 'WARNING, ag_soil_moist_init_frac not specified, substituting soil_moist_init_frac'
            Ag_soil_moist_init_frac = Soil_moist_init_frac
          ENDIF
          IF ( getparam(Soilzone_module, 'ag_soil_rechr_init_frac', Nhru, 'real', Ag_soil_rechr_init_frac)/=0 ) &
     &         CALL read_error(2, 'ag_soil_rechr_init_frac')
          IF ( Ag_soil_rechr_init_frac(1) < 0.0 ) then
            print *, 'WARNING, ag_soil_rechr_init_frac not specified, substituting soil_rechr_init_frac'
            Ag_soil_rechr_init_frac = Soil_rechr_init_frac
          ENDIF
        ENDIF
        Ag_soil_rechr_max = 0.0
        DO i = 1, Nhru
          IF ( Hru_type(i)==INACTIVE .OR. Hru_type(i)==LAKE ) CYCLE
          !IF ( Ag_soil_moist_max(i) < 4.0 ) THEN
          !  PRINT *, 'ag_soil_moist_max < 4.0, set to 4.0, HRU:', i, Ag_soil_moist_max(i)
          !  Ag_soil_moist_max(i) = 4.0
          !ENDIF
          !IF ( Ag_soil_rechr_max_frac(i) < 0.75 ) THEN
          !  PRINT *, 'ag_soil_rechr_max_frac < 0.75, set to 0.75, HRU:', i, Ag_soil_rechr_max_frac(i)
          !  Ag_soil_rechr_max_frac(i) = 0.75
          !ENDIF
          IF ( Ag_soil_rechr_max_frac(i) > 1.0 ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT 9022, i, Ag_soil_rechr_max_frac(i)
              ierr = 1
            ELSE
              IF ( Print_debug>DEBUG_less ) PRINT 9032, i, Ag_soil_rechr_max_frac(i)
              Ag_soil_rechr_max_frac(i) = 1.0
            ENDIF
          ENDIF
          Ag_soil_rechr_max(i) = Ag_soil_moist_max(i) * Ag_soil_rechr_max_frac(i)
        ENDDO
        IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==5 ) THEN
          Ag_soil_moist = Ag_soil_moist_init_frac * Ag_soil_moist_max
          Ag_soil_rechr = Ag_soil_rechr_init_frac * Ag_soil_rechr_max
        ENDIF
        ! AG consistency checks
        DO i = 1, Nhru
          IF ( Hru_type(i)==INACTIVE .OR. Hru_type(i)==LAKE ) CYCLE
          IF ( Ag_soil_rechr(i)>Ag_soil_rechr_max(i) ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT 9023, i, Ag_soil_rechr(i), Ag_soil_rechr_max(i)
              ierr = 1
            ELSE
              IF ( Print_debug>DEBUG_less ) PRINT 9033, i, Ag_soil_rechr(i), Ag_soil_rechr_max(i)
              Ag_soil_rechr(i) = Ag_soil_rechr_max(i)
            ENDIF
          ENDIF
          IF ( Ag_soil_moist(i)>Ag_soil_moist_max(i) ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT 9024, i, Ag_soil_moist(i), Ag_soil_moist_max(i)
              ierr = 1
            ELSE
              IF ( Print_debug>DEBUG_less ) PRINT 9034, i, Ag_soil_moist(i), Ag_soil_moist_max(i)
              Ag_soil_moist(i) = Ag_soil_moist_max(i)
            ENDIF
          ENDIF
          IF ( Ag_soil_rechr(i)>Ag_soil_moist(i) ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT 9025, i, Ag_soil_rechr(i), Ag_soil_moist(i)
              ierr = 1
            ELSE
              IF ( Print_debug>DEBUG_less ) PRINT 9035, i, Ag_soil_rechr(i), Ag_soil_moist(i)
              Ag_soil_rechr(i) = Ag_soil_moist(i)
            ENDIF
          ENDIF
        ENDDO
        DEALLOCATE ( Ag_soil_moist_init_frac, Ag_soil_rechr_init_frac, Ag_soil_rechr_max_frac )
      ENDIF
      IF ( PRMS4_flag == OFF ) THEN
        DEALLOCATE ( Soil_rechr_max_frac )
        IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==5 ) &
             DEALLOCATE ( Soil_moist_init_frac, Soil_rechr_init_frac )
      ENDIF
      ! check parameters
      Basin_soil_moist = 0.0D0 ! set because these are saved in It0 variables in prms_time
      Basin_ssstor = 0.0D0
      DO i = 1, Nhru
        IF ( Hru_type(i)==INACTIVE .OR. Hru_type(i)==LAKE ) CYCLE
        ! hru_type = land or swale or glacier
        IF ( .NOT.(Hru_perv(i)>0.0) ) THEN
          ! if no pervious set soil parameters and variables to 0.0
          Soil_moist(i) = 0.0
          Soil_rechr(i) = 0.0
        ENDIF
        IF ( AG_flag==OFF ) THEN
          IF ( Soil_moist_max(i)<0.00001 ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT 9006, i, Soil_moist_max(i)
              ierr = 1
            ELSE
              Soil_moist_max(i) = 0.00001
              IF ( Print_debug>DEBUG_less ) PRINT 9008, i
            ENDIF
          ENDIF
          IF ( Soil_rechr_max(i)<0.00001 ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT 9007, i, Soil_rechr_max(i)
              ierr = 1
            ELSE
              Soil_rechr_max(i) = 0.00001
              IF ( Print_debug>DEBUG_less ) PRINT 9009, i
            ENDIF
          ENDIF
        ENDIF
        IF ( Soil_rechr_max(i)>Soil_moist_max(i) ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT 9002, i, Soil_rechr_max(i), Soil_moist_max(i)
            ierr = 1
          ELSE
            IF ( Print_debug>DEBUG_less ) PRINT 9012, i, Soil_rechr_max(i), Soil_moist_max(i)
            Soil_rechr_max(i) = Soil_moist_max(i)
          ENDIF
        ENDIF
        IF ( Soil_rechr(i)>Soil_rechr_max(i) ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT 9003, i, Soil_rechr(i), Soil_rechr_max(i)
            ierr = 1
          ELSE
            IF ( Print_debug>DEBUG_less ) PRINT 9013, i, Soil_rechr(i), Soil_rechr_max(i)
            Soil_rechr(i) = Soil_rechr_max(i)
          ENDIF
        ENDIF
        IF ( Soil_moist(i)>Soil_moist_max(i) ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT 9004, i, Soil_moist(i), Soil_moist_max(i)
            ierr = 1
          ELSE
            IF ( Print_debug>DEBUG_less ) PRINT 9014, i, Soil_moist(i), Soil_moist_max(i)
            Soil_moist(i) = Soil_moist_max(i)
          ENDIF
        ENDIF
        IF ( Soil_rechr(i)>Soil_moist(i) ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT 9005, i, Soil_rechr(i), Soil_moist(i)
            ierr = 1
          ELSE
            IF ( Print_debug>DEBUG_less ) PRINT 9015, i, Soil_rechr(i), Soil_moist(i)
            Soil_rechr(i) = Soil_moist(i)
          ENDIF
        ENDIF
        IF ( Ssres_stor(i)>Sat_threshold(i) ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT *, 'ERROR, HRU:', i, Ssres_stor(i), Sat_threshold(i), ' ssres_stor > sat_threshold'
            ierr = 1
          ELSE
            PRINT *, 'WARNING, HRU:', i, Ssres_stor(i), Sat_threshold(i), ' ssres_stor > sat_threshold, ssres_stor set to max'
            Ssres_stor(i) = Sat_threshold(i)
          ENDIF
        ENDIF
        Basin_soil_moist = Basin_soil_moist + Soil_moist(i) * Hru_perv(i)
        Basin_ssstor = Basin_ssstor + Ssres_stor(i) * Hru_area(i)
      ENDDO
      Basin_soil_moist = Basin_soil_moist * Basin_area_inv
      Basin_ssstor = Basin_ssstor * Basin_area_inv

      IF ( ierr>0 ) STOP ERROR_PARAM

      IF ( getparam(Srunoff_module, 'snowinfil_max', Nhru, 'real', Snowinfil_max)/=0 ) CALL read_error(2, 'snowinfil_max')

      IF ( getparam(Srunoff_module, 'imperv_stor_max', Nhru, 'real', Imperv_stor_max)/=0 ) CALL read_error(2, 'imperv_stor_max')

! initialize arrays (dimensioned Nhru) as needed if inactive HRUs
      Tmaxf = 0.0D0
      Tminf = 0.0D0
      Tavgf = 0.0D0
      Tmaxc = 0.0D0
      Tminc = 0.0D0
      Tavgc = 0.0D0
      Tmax_hru = 0.0D0
      Tmin_hru = 0.0D0
      Pptmix = OFF
      Newsnow = OFF
      Prmx = 0.0D0
      Hru_ppt = 0.0D0
      Hru_rain = 0.0D0
      Hru_snow = 0.0D0
      Swrad = 0.0D0
      Potet = 0.0D0
      Slow_flow = 0.0
      Soil_to_gw = 0.0
      Soil_to_ssr = 0.0
      Hru_actet = 0.0
      Infil = 0.0
      Sroff = 0.0
! initialize arrays (dimensioned Nssr)
      Ssr_to_gw = 0.0
      Ssres_in = 0.0
      Ssres_flow = 0.0
      IF ( Solrad_flag==ddsolrad_module .OR. Solrad_flag==ccsolrad_module ) Orad_hru = 0.0D0
      IF ( Et_flag==potet_pt_module .OR. Et_flag==potet_pm_module .OR. Et_flag==potet_pm_sta_module ) THEN
        Tempc_dewpt = 0.0D0
        Vp_actual = 0.0D0
        Lwrad_net = 0.0D0
        Vp_slope = 0.0D0
        IF ( Et_flag==potet_pm_module .OR. Et_flag==potet_pm_sta_module ) Vp_sat = 0.0D0
        IF ( Et_flag==potet_pt_module .OR. Et_flag==potet_pm_module ) THEN
          IF ( Humidity_cbh_flag==OFF ) THEN
            IF ( getparam(Et_module, 'humidity_percent', Nhru_nmonths, 'real', Humidity_percent)/=0 ) &
     &           CALL read_error(2, 'humidity_percent')
          ENDIF
        ENDIF
      ENDIF

! initialize scalers
      Basin_humidity = 0.0D0
      Basin_lakeevap = 0.0D0
      Flow_out = 0.0D0

      IF ( Init_vars_from_file>0 .OR. ierr>0 ) RETURN

      Basin_lake_stor = 0.0D0
      Basin_transp_on = OFF
! initialize arrays (dimensioned Nsegment)
      IF ( Stream_order_flag==ACTIVE ) THEN
        Seg_inflow = 0.0D0
        Seg_outflow = 0.0D0
      ENDIF
      Transp_on = OFF
! initialize storage variables
      Imperv_stor = 0.0
      Pkwater_equiv = 0.0D0
      IF ( Glacier_flag==1 ) THEN
        Glacrb_melt = 0.0
        Glacier_frac = 0.0
        Alt_above_ela = 0.0
        Glrette_frac = 0.0
      ENDIF
      Slow_stor = 0.0
      Gwres_stor = 0.0D0
      IF ( Dprst_flag==ACTIVE ) THEN
        Dprst_vol_open = 0.0D0
        Dprst_vol_clos = 0.0D0
      ENDIF
! initialize arrays (dimensioned nlake)
      IF ( Nlake>0 ) Lake_vol = 0.0D0

 9002 FORMAT (/, 'ERROR, HRU: ', I0, ' soil_rechr_max > soil_moist_max', 2F10.5)
 9003 FORMAT (/, 'ERROR, HRU: ', I0, ' soil_rechr_init > soil_rechr_max', 2F10.5)
 9004 FORMAT (/, 'ERROR, HRU: ', I0, ' soil_moist_init > soil_moist_max', 2F10.5)
 9005 FORMAT (/, 'ERROR, HRU: ', I0, ' soil_rechr > soil_moist based on init and max values', 2F10.5)
 9022 FORMAT (/, 'ERROR, HRU: ', I0, ' ag_soil_rechr_max_frac > 1.0', F10.5)
 9023 FORMAT (/, 'ERROR, HRU: ', I0, ' ag_soil_rechr_init > ag_soil_rechr_max', 2F10.5)
 9024 FORMAT (/, 'ERROR, HRU: ', I0, ' ag_soil_moist_init > ag_soil_moist_max', 2F10.5)
 9025 FORMAT (/, 'ERROR, HRU: ', I0, ' ag_soil_rechr > ag_soil_moist based on init and max values', 2F10.5)
 9006 FORMAT (/, 'ERROR, HRU: ', I0, ' soil_moist_max < 0.00001', F10.5)
 9007 FORMAT (/, 'ERROR, HRU: ', I0, ' soil_rechr_max < 0.00001', F10.5)
 9008 FORMAT (/, 'WARNING, HRU: ', I0, ' soil_moist_max < 0.00001, set to 0.00001')
 9009 FORMAT (/, 'WARNING, HRU: ', I0, ' soil_rechr_max < 0.00001, set to 0.00001')
 9012 FORMAT ('WARNING, HRU: ', I0, ' soil_rechr_max > soil_moist_max,', 2F10.5, /, 9X, &
     &        'soil_rechr_max set to soil_moist_max')
 9013 FORMAT ('WARNING, HRU: ', I0, ' soil_rechr_init > soil_rechr_max,', 2F10.5, /, 9X, &
     &        'soil_rechr set to soil_rechr_max')
 9014 FORMAT ('WARNING, HRU: ', I0, ' soil_moist_init > soil_moist_max,', 2F10.5, /, 9X, &
     &        'soil_moist set to soil_moist_max')
 9015 FORMAT ('WARNING, HRU: ', I0, ' soil_rechr_init > soil_moist_init,', 2F10.5, /, 9X, &
     &        'soil_rechr set to soil_moist based on init and max values')
 9032 FORMAT ('WARNING, HRU: ', I0, ' ag_soil_rechr_max_frac > 1.0,', F10.5, /, 9X, &
     &        'ag_soil_rechr_max_frac set to 1.0')
 9033 FORMAT ('WARNING, HRU: ', I0, ' ag_soil_rechr_init > ag_soil_rechr_max,', 2F10.5, /, 9X, &
     &        'ag_soil_rechr set to ag_soil_rechr_max')
 9034 FORMAT ('WARNING, HRU: ', I0, ' ag_soil_moist_init > ag_soil_moist_max,', 2F10.5, /, 9X, &
     &        'ag_soil_moist set to ag_soil_moist_max')
 9035 FORMAT ('WARNING, HRU: ', I0, ' ag_soil_rechr_init > ag_soil_moist_init,', 2F10.5, /, 9X, &
     &        'ag_soil_rechr set to ag_soil_moist based on init and max values')

      END FUNCTION climateflow_init

!***********************************************************************
!     Sets temperatures in both system of units for each HRU
!***********************************************************************
      SUBROUTINE temp_set(Ihru, Tmax, Tmin, Tmaxf, Tminf, Tavgf, Tmaxc, Tminc, Tavgc, Hru_area)
      USE PRMS_CLIMATEVARS, ONLY: Basin_temp, Basin_tmax, Basin_tmin, Temp_units, Tmax_hru, Tmin_hru
      USE PRMS_CONSTANTS, ONLY: MINTEMP, MAXTEMP, ERROR_temp, DEBUG_less, ACTIVE
      USE PRMS_MODULE, ONLY: forcing_check_flag, Print_debug
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      DOUBLE PRECISION, INTENT(IN) :: Tmax, Tmin, Hru_area
      DOUBLE PRECISION, INTENT(OUT) :: Tmaxf, Tminf, Tavgf, Tmaxc, Tminc, Tavgc
! Functions
      DOUBLE PRECISION, EXTERNAL :: c_to_f, f_to_c
      EXTERNAL :: print_date
! Local Variable
!      INTEGER :: foo
!***********************************************************************
      IF ( forcing_check_flag == ACTIVE ) THEN
        IF ( Tmax < Tmin ) THEN
          IF ( Print_debug > DEBUG_less ) THEN
            PRINT '(A,I0)', 'Warning, adjusted tmax value < adjusted tmin value for HRU: ', Ihru
            PRINT '(4(A,F0.4))', '         tmax: ', Tmax, ' tmin: ', Tmin, ', Difference: ', Tmin-Tmax
!            PRINT '(A)',         '         values swapped'
            CALL print_date(0)
          ENDIF
!          foo = Tmax
!          Tmax = Tmin
!          Tmin = foo
        ENDIF
      ENDIF

      IF ( Temp_units==0 ) THEN
!       degrees Fahrenheit
        Tmaxf = Tmax
        Tminf = Tmin
        Tavgf = (Tmax+Tmin)*0.5D0
        Tmaxc = f_to_c(Tmax)
        Tminc = f_to_c(Tmin)
        Tavgc = f_to_c(Tavgf)
        Basin_temp = Basin_temp + Tavgf*Hru_area
      ELSE
!       degrees Celsius
        Tmaxc = Tmax
        Tminc = Tmin
        Tavgc = (Tmax+Tmin)*0.5D0
        Tmaxf = c_to_f(Tmax)
        Tminf = c_to_f(Tmin)
        Tavgf = c_to_f(Tavgc)
        Basin_temp = Basin_temp + Tavgc*Hru_area
      ENDIF

      IF ( forcing_check_flag == ACTIVE ) THEN
        IF ( Tminf<MINTEMP .OR. Tmaxf>MAXTEMP ) THEN
          PRINT '(A,I0,1X,F0.4,1X,F0.4,/)', ' ERROR, invalid temperature value for HRU: ', Ihru, Tminf, Tmaxf
          CALL print_date(1)
          ERROR STOP ERROR_temp
        ENDIF
      ENDIF
      Tmax_hru(Ihru) = Tmax ! in units temp_units
      Tmin_hru(Ihru) = Tmin ! in units temp_units

      Basin_tmax = Basin_tmax + Tmax*Hru_area
      Basin_tmin = Basin_tmin + Tmin*Hru_area

      END SUBROUTINE temp_set

!***********************************************************************
!     Computes precipitation form (rain, snow or mix) and depth for each HRU
!***********************************************************************
      SUBROUTINE precip_form(Precip, Hru_ppt, Hru_rain, Hru_snow, Tmaxf, &
     &           Tminf, Pptmix, Newsnow, Prmx, Tmax_allrain_f, Rain_adj, &
     &           Snow_adj, Adjmix_rain, Hru_area, Sum_obs, Tmax_allsnow_f, Ihru)
!      USE PRMS_CONSTANTS, ONLY: ACTIVE, DEBUG_minimum
!      USE PRMS_MODULE, ONLY: Print_debug, forcing_check_flag
      USE PRMS_CLIMATEVARS, ONLY: Basin_ppt, Basin_rain, Basin_snow
      IMPLICIT NONE
! Functions
      INTRINSIC :: ABS, DBLE
      EXTERNAL :: print_date
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(IN) :: Rain_adj, Snow_adj
      REAL, INTENT(IN) :: Adjmix_rain
      DOUBLE PRECISION, INTENT(IN) :: Tmaxf, Tminf, Hru_area, Tmax_allrain_f, Tmax_allsnow_f
      DOUBLE PRECISION, INTENT(INOUT) :: Sum_obs
      INTEGER, INTENT(INOUT) :: Pptmix, Newsnow
      DOUBLE PRECISION, INTENT(INOUT) :: Precip, Hru_rain, Hru_snow, Prmx, Hru_ppt
! Local Variables
      DOUBLE PRECISION :: tdiff
!***********************************************************************
      ! basin precipitation before adjustments
      Sum_obs = Sum_obs + Precip*Hru_area

!******If maximum temperature is below or equal to the base temperature
!******for snow then precipitation is all snow
      IF ( Tmaxf<=Tmax_allsnow_f ) THEN
        Hru_ppt = Precip*DBLE(Snow_adj)
        Hru_snow = Hru_ppt
        Newsnow = 1

!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain
      ELSEIF ( Tminf>Tmax_allsnow_f .OR. Tmaxf>=Tmax_allrain_f ) THEN
        Hru_ppt = Precip*DBLE(Rain_adj)
        Hru_rain = Hru_ppt
        Prmx = 1.0D0

!******Otherwise precipitation is a mixture of rain and snow
      ELSE
        tdiff = Tmaxf - Tminf
        IF ( tdiff<0.0D0 ) THEN
          PRINT *, 'ERROR, tmax < tmin (degrees Fahrenheit), tmax:', Tmaxf, ' tmin:', TminF
          CALL print_date(1)
        ENDIF
        IF ( ABS(tdiff)<0.0001D0 ) tdiff = 0.0001D0
        Prmx = ((Tmaxf-Tmax_allsnow_f)/tdiff)*DBLE(Adjmix_rain)
        IF ( Prmx<0.0D0 ) Prmx = 0.0D0

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain
!******If not, it is a rain/snow mixture
        IF ( Prmx<1.0D0 ) THEN
          Pptmix = 1
          Hru_ppt = Precip*DBLE(Snow_adj)
          Hru_rain = Prmx*Hru_ppt
          Hru_snow = Hru_ppt - Hru_rain
          Newsnow = 1
        ELSE
          Hru_ppt = Precip*DBLE(Rain_adj)
          Hru_rain = Hru_ppt
          Prmx = 1.0D0
        ENDIF
      ENDIF
      Basin_ppt = Basin_ppt + Hru_ppt*Hru_area
      Basin_rain = Basin_rain + Hru_rain*Hru_area
      Basin_snow = Basin_snow + Hru_snow*Hru_area

!      IF ( forcing_check_flag == ACTIVE ) THEN
        IF ( Hru_ppt < 0.0D0 .OR. Hru_rain < 0.0D0 .OR. Hru_snow < 0.0D0 ) THEN
!          IF ( Print_debug > DEBUG_minimum ) THEN
            PRINT '(A,I0)', 'Warning, adjusted precipitation value(s) < 0.0 for HRU: ', Ihru
            PRINT '(A,F0.4,A,F0.4,A)', '         hru_ppt: ', Hru_ppt, ' hru_rain: ', Hru_rain, ' hru_snow: ', Hru_snow
            CALL print_date(0)
!          ENDIF
        ENDIF
!      ENDIF

      END SUBROUTINE precip_form

!***********************************************************************
!     Write or read restart file
!***********************************************************************
      SUBROUTINE climateflow_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, ACTIVE
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Glacier_flag, &
     &    Dprst_flag, Stream_order_flag, Nlake, AG_flag
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Functions
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_transp_on, Basin_soil_moist, Basin_ssstor, Basin_lake_stor
        WRITE ( Restart_outunit ) Transp_on
        WRITE ( Restart_outunit ) Pkwater_equiv
        IF ( Glacier_flag==1 ) THEN
          WRITE ( Restart_outunit) Glacier_frac
          WRITE ( Restart_outunit) Glrette_frac
          WRITE ( Restart_outunit) Alt_above_ela
        ENDIF
        WRITE ( Restart_outunit ) Soil_moist
        WRITE ( Restart_outunit ) Slow_stor
        WRITE ( Restart_outunit ) Ssres_stor
        WRITE ( Restart_outunit ) Soil_rechr
        WRITE ( Restart_outunit ) Imperv_stor
        WRITE ( Restart_outunit ) Gwres_stor
        IF ( Dprst_flag==ACTIVE ) THEN
          WRITE ( Restart_outunit ) Dprst_vol_open
          WRITE ( Restart_outunit ) Dprst_vol_clos
        ENDIF
        IF ( Stream_order_flag==ACTIVE ) THEN
          WRITE ( Restart_outunit ) Seg_inflow
          WRITE ( Restart_outunit ) Seg_outflow
        ENDIF
        IF ( Nlake>0 ) WRITE ( Restart_outunit ) Lake_vol
        IF ( AG_flag==ACTIVE ) THEN
          WRITE ( Restart_outunit ) Ag_soil_moist
          WRITE ( Restart_outunit ) Ag_soil_rechr
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_transp_on, Basin_soil_moist, Basin_ssstor, Basin_lake_stor
        READ ( Restart_inunit ) Transp_on
        READ ( Restart_inunit ) Pkwater_equiv
        IF ( Glacier_flag==1 ) THEN
          READ ( Restart_inunit) Glacier_frac
          READ ( Restart_inunit) Glrette_frac
          READ ( Restart_inunit) Alt_above_ela
        ENDIF
        READ ( Restart_inunit ) Soil_moist
        READ ( Restart_inunit ) Slow_stor
        READ ( Restart_inunit ) Ssres_stor
        READ ( Restart_inunit ) Soil_rechr
        READ ( Restart_inunit ) Imperv_stor
        READ ( Restart_inunit ) Gwres_stor
        IF ( Dprst_flag==ACTIVE ) THEN
          READ ( Restart_inunit ) Dprst_vol_open
          READ ( Restart_inunit ) Dprst_vol_clos
        ENDIF
        IF ( Stream_order_flag==ACTIVE ) THEN
          READ ( Restart_inunit ) Seg_inflow
          READ ( Restart_inunit ) Seg_outflow
        ENDIF
        IF ( Nlake>0 ) READ ( Restart_inunit ) Lake_vol
        IF ( AG_flag==ACTIVE ) THEN
          READ ( Restart_inunit ) Ag_soil_moist
          READ ( Restart_inunit ) Ag_soil_rechr
        ENDIF
      ENDIF
      END SUBROUTINE climateflow_restart
