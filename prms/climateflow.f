!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
      MODULE PRMS_CLIMATEVARS
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Ntemp, Nrain, Nsol
      ! Tmax_hru and Tmin_hru are in temp_units
      REAL, SAVE, ALLOCATABLE :: Tmax_hru(:), Tmin_hru(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_elev_feet(:), Tsta_elev_meters(:)
      REAL, SAVE, ALLOCATABLE :: Psta_elev_feet(:), Psta_elev_meters(:)
      REAL, SAVE :: Tmax_allrain_f(12), Tmax_allsnow_f
!   Declared Variables - Precip
      INTEGER, SAVE, ALLOCATABLE :: Newsnow(:), Pptmix(:)
      DOUBLE PRECISION, SAVE :: Basin_ppt, Basin_rain, Basin_snow
      DOUBLE PRECISION, SAVE :: Basin_obs_ppt
      REAL, SAVE, ALLOCATABLE :: Hru_ppt(:), Hru_rain(:), Hru_snow(:)
      REAL, SAVE, ALLOCATABLE :: Prmx(:)
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
      REAL, SAVE, ALLOCATABLE :: Potet(:)
      REAL, SAVE, ALLOCATABLE :: Potet_coef_hru_mo(:, :)
!   Declared Parameters and Variables - Solar Radiation
      INTEGER, SAVE :: Basin_solsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_solsta(:)
      DOUBLE PRECISION, SAVE :: Basin_horad, Basin_potsw
      REAL, SAVE :: Radj_sppt, Radj_wppt, Radmax, Rad_conv, Orad
      REAL, SAVE, ALLOCATABLE :: Swrad(:), Ppt_rad_adj(:)
!   Declared Parameters - Temp
      INTEGER, SAVE :: Temp_units, Basin_tsta
      INTEGER, SAVE, ALLOCATABLE :: Hru_tsta(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_elev(:), Tmax_adj(:), Tmin_adj(:)
!   Declared Parameters - Precip
      INTEGER, SAVE :: Precip_units
      REAL, SAVE :: Tmax_allsnow
      REAL, SAVE, ALLOCATABLE :: Tmax_allrain(:), Psta_elev(:)
      REAL, SAVE, ALLOCATABLE :: Adjmix_rain(:), Adjust_snow(:)
      REAL, SAVE, ALLOCATABLE :: Adjust_rain(:)
!      CHARACTER(LEN=11), PARAMETER :: MODNAME = 'climateflow'
!      CHARACTER(LEN=26), PARAMETER :: PROCNAME =
!     +                                'Internal PRMS Definitions'
      END MODULE PRMS_CLIMATEVARS

!***********************************************************************
! Declares parameters and variables related to flows from soilzone,
! smbal, ssflow, srunoff_carea, srunoff_smidx
!***********************************************************************
      MODULE PRMS_FLOWVARS
      IMPLICIT NONE
!   Declared Variables
      ! intcp
      REAL, SAVE, ALLOCATABLE :: Hru_intcpevap(:)
      ! soilzone
      DOUBLE PRECISION, SAVE :: Basin_ssflow, Basin_soil_to_gw
      DOUBLE PRECISION, SAVE :: Basin_actet, Basin_lakeevap
      DOUBLE PRECISION, SAVE :: Basin_swale_et, Basin_perv_et
      DOUBLE PRECISION, SAVE :: Basin_soil_moist, Basin_ssstor
      REAL, SAVE, ALLOCATABLE :: Hru_actet(:), Soil_moist(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_gw(:), Slow_flow(:)
      REAL, SAVE, ALLOCATABLE :: Soil_to_ssr(:), Ssres_in(:)
      REAL, SAVE, ALLOCATABLE :: Ssr_to_gw(:), Slow_stor(:)
      REAL, SAVE, ALLOCATABLE :: Ssres_stor(:), Ssres_flow(:)
      REAL, SAVE, ALLOCATABLE :: Soil_rechr(:), Sat_threshold(:)
      ! srunoff
      DOUBLE PRECISION, SAVE :: Basin_imperv_stor, Basin_imperv_evap
      DOUBLE PRECISION, SAVE :: Basin_sroff, Basin_infil, Basin_dprst_wb
      DOUBLE PRECISION, SAVE :: Basin_hortonian, Basin_sroff_farflow
      DOUBLE PRECISION, SAVE :: Basin_hortonian_lakes, Strm_farfield
      ! Surface-Depression Storage
      DOUBLE PRECISION, SAVE :: Basin_dprst_volop, Basin_dprst_volcl
      DOUBLE PRECISION, SAVE :: Basin_dprst_evap, Basin_dprst_seep
      DOUBLE PRECISION, SAVE :: Basin_glacr_melt
      REAL, SAVE, ALLOCATABLE :: Sroff(:), Imperv_stor(:)
      REAL, SAVE, ALLOCATABLE :: Hru_impervevap(:), Hru_impervstor(:)
      REAL, SAVE, ALLOCATABLE :: Infil(:), Hru_hortonian_cascadeflow(:)
      REAL, SAVE, ALLOCATABLE :: Hortonian_lakes(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Strm_seg_in(:)
      ! gwflow
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwres_stor(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Soil_moist_max(:), Soil_rechr_max(:)
      REAL, SAVE, ALLOCATABLE :: Carea_max(:), Snowinfil_max(:)
      REAL, SAVE, ALLOCATABLE :: Imperv_stor_max(:)
      END MODULE PRMS_FLOWVARS

!***********************************************************************
!     Main climateflow routine
!***********************************************************************
      INTEGER FUNCTION climateflow()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: climateflow_decl, climateflow_init
!***********************************************************************
      climateflow = 0

      IF ( Process(:4)=='decl' ) THEN
        climateflow = climateflow_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        climateflow = climateflow_init()
      ENDIF

      END FUNCTION climateflow

!***********************************************************************
!     climateflow_decl - declare climate and flow variables and parameters
!***********************************************************************
      INTEGER FUNCTION climateflow_decl()
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      USE PRMS_MODULE, ONLY: Temp_flag, Precip_flag, Model, Nhru, Nssr,
     +    Nsegment, Strmflow_module, Et_flag, Strmflow_flag, Dprst_flag,
     +    Version_climateflow, Climateflow_nc, Temp_module,
     +    Precip_module, Solrad_module, Transp_module, Et_module,
     +    Soilzone_module, Srunoff_module, Ngw, Soilzone_flag
      USE PRMS_CASCADE, ONLY: Cascade_flag, Cascadegw_flag
      IMPLICIT NONE
! Functions
      INTEGER INDEX
      INTEGER, EXTERNAL :: declmodule, getdim, declvar, declparam
      EXTERNAL read_error
! Local Variables
!      INTEGER :: n
!***********************************************************************
      climateflow_decl = 1

      Version_climateflow =
     +'$Id: climateflow.f 4871 2012-10-04 16:44:17Z rsregan $'
      Climateflow_nc = INDEX( Version_climateflow, 'Z' )
!      n = INDEX( Version_climateflow, '.f' ) + 1
!      IF ( declmodule(Version_climateflow(6:n), PROCNAME,
!     +                Version_climateflow(n+2:Climateflow_nc))/=0 ) STOP

      Ntemp = getdim('ntemp')
      IF ( Ntemp==-1 ) CALL read_error(6, 'ntemp')

      Nrain = getdim('nrain')
      IF ( Nrain==-1 ) CALL read_error(6, 'nrain')

      Nsol = getdim('nsol')
      IF ( Nsol==-1 ) CALL read_error(6, 'nsol')

      ALLOCATE ( Tmaxf(Nhru) )
      IF ( declvar(Temp_module, 'tmaxf', 'nhru', Nhru, 'real',
     +     'Maximum air temperature distributed to each HRU',
     +     'degrees F',
     +     Tmaxf)/=0 ) CALL read_error(3, 'tmaxf')

      ALLOCATE ( Tminf(Nhru) )
      IF ( declvar(Temp_module, 'tminf', 'nhru', Nhru, 'real',
     +     'Minimum air temperature distributed to each HRU',
     +     'degrees F',
     +     Tminf)/=0 ) CALL read_error(3, 'tminf')

      ALLOCATE ( Tavgf(Nhru) )
      IF ( declvar(Temp_module, 'tavgf', 'nhru', Nhru, 'real',
     +     'Average air temperature distributed to each HRU',
     +     'degrees F',
     +     Tavgf)/=0 ) CALL read_error(3, 'tavgf')

      ALLOCATE ( Tmaxc(Nhru) )
      IF ( declvar(Temp_module, 'tmaxc', 'nhru', Nhru, 'real',
     +     'Maximum air temperature distributed to each HRU',
     +     'degrees Celsius',
     +     Tmaxc)/=0 ) CALL read_error(3, 'tmaxc')

      ALLOCATE ( Tminc(Nhru) )
      IF ( declvar(Temp_module, 'tminc', 'nhru', Nhru, 'real',
     +     'Minimum air temperature distributed to each HRU',
     +     'degrees Celsius',
     +     Tminc)/=0 ) CALL read_error(3, 'tminc')

      ALLOCATE ( Tavgc(Nhru) )
      IF ( declvar(Temp_module, 'tavgc', 'nhru', Nhru, 'real',
     +     'Average air temperature distributed to each HRU',
     +     'degrees Celsius',
     +     Tavgc)/=0 ) CALL read_error(3, 'tavgc')

      IF ( declvar(Temp_module, 'basin_tmax', 'one', 1, 'double',
     +     'Basin area-weighted average maximum air temperature',
     +     'temp_units',
     +     Basin_tmax)/=0 ) CALL read_error(3, 'basin_tmax')

      IF ( declvar(Temp_module, 'basin_tmin', 'one', 1, 'double',
     +     'Basin area-weighted average minimum air temperature',
     +     'temp_units',
     +     Basin_tmin)/=0 ) CALL read_error(3, 'basin_tmin')

      IF ( declvar(Temp_module, 'basin_temp', 'one', 1, 'double',
     +     'Basin area-weighted average air temperature',
     +     'temp_units',
     +     Basin_temp)/=0 ) CALL read_error(3, 'basin_temp')

      IF ( declvar(Temp_module, 'solrad_tmax', 'one', 1, 'real',
     +     'Basin daily maximum temperature for use with solar'//
     +     ' radiation calculations',
     +     'temp_units',
     +     Solrad_tmax)/=0 ) CALL read_error(3, 'solrad_tmax')

      IF ( declvar(Temp_module, 'solrad_tmin', 'one', 1, 'real',
     +     'Basin daily minimum temperature for use with solar'//
     +     ' radiation calculations',
     +     'temp_units',
     +     Solrad_tmin)/=0 ) CALL read_error(3, 'solrad_tmin')

      IF ( Temp_flag<7 .OR. Model==99 ) THEN
        ALLOCATE ( Tsta_elev(Ntemp) )
        ALLOCATE ( Tsta_elev_meters(Ntemp), Tsta_elev_feet(Ntemp) )
        IF ( declparam(Temp_module, 'tsta_elev', 'ntemp', 'real',
     +       '0', '-300.0', '30000.0',
     +       'Temperature station elevation',
     +       'Elevation of each temperature measurement station',
     +       'elev_units')/=0 ) CALL read_error(1, 'tsta_elev')
      ENDIF

      IF ( declparam(Temp_module, 'temp_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units flag for measured temperature',
     +     'Flag to indicate the units of measured air-temperature'//
     +     ' values (0=Fahrenheit; 1=Celsius)',
     +     'none')/=0 ) CALL read_error(1, 'temp_units')

      ! IF ( (Temp_flag<5 .OR. Model==99)
      !+     .OR. ((Temp_flag==7.OR.Temp_flag==8).AND.Ntemp>0) ) THEN
      IF ( Temp_flag<5 .OR. Model==99 ) THEN
        IF ( declparam(Temp_module, 'basin_tsta', 'one', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'Index of main temperature station',
     +       'Index of temperature station used to compute basin'//
     +       ' temperature values',
     +       'none')/=0 ) CALL read_error(1, 'basin_tsta')
      ENDIF

      IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4
     +     .OR. Model==99 ) THEN
        ALLOCATE (Hru_tsta(Nhru))
        IF ( declparam(Temp_module, 'hru_tsta', 'nhru', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'Index of base temperature station for HRU',
     +       'Index of the base temperature station used for lapse'//
     +       ' rate calculations',
     +       'none')/=0 ) CALL read_error(1, 'hru_tsta')
      ENDIF

      IF ( (Temp_flag/=3.AND.Temp_flag/=7.AND.Temp_flag/=8) .OR.
     +     Model==99 ) THEN
        ALLOCATE (Tmax_adj(Nhru))
        IF ( declparam(Temp_module, 'tmax_adj', 'nhru', 'real',
     +       '0.0', '-10.0', '10.0',
     +       'HRU maximum temperature adjustment',
     +       'Adjustment to maximum temperature for each HRU,'//
     +       ' estimated based on slope and aspect',
     +       'temp_units')/=0 ) CALL read_error(1, 'tmax_adj')

        ALLOCATE (Tmin_adj(Nhru))
        IF ( declparam(Temp_module, 'tmin_adj', 'nhru', 'real',
     +       '0.0', '-10.0', '10.0',
     +       'HRU minimum temperature adjustment',
     +       'Adjustment to minimum temperature for each HRU,'//
     +       ' estimated based on slope and aspect',
     +       'temp_units')/=0 ) CALL read_error(1, 'tmin_adj')
      ENDIF

! PRECIPITATION VARIABLES AND PARAMETERS
      ALLOCATE (Pptmix(Nhru))
      IF ( declvar(Precip_module, 'pptmix', 'nhru', Nhru, 'integer',
     +     'Flag to indicate if precipitation is a mixture of rain'//
     +     ' and snow for each HRU (0=no; 1=yes)',
     +     'none',
     +     Pptmix)/=0 ) CALL read_error(3, 'pptmix')

      ALLOCATE (Newsnow(Nhru))
      IF ( declvar(Precip_module, 'newsnow', 'nhru', Nhru, 'integer',
     +    'Flag to indicate if new snow fell on each HRU (0=no; 1=yes)',
     +    'none',
     +    Newsnow)/=0 ) CALL read_error(3, 'newsnow')

      ALLOCATE (Prmx(Nhru))
      IF ( declvar(Precip_module, 'prmx', 'nhru', Nhru, 'real',
     +   'Fraction of rain in a mixed precipitation event for each HRU',
     +     'decimal fraction',
     +     Prmx)/=0 ) CALL read_error(3, 'prmx')

      IF ( declvar(Precip_module, 'basin_rain', 'one', 1, 'double',
     +     'Basin area-weighted average rainfall',
     +     'inches',
     +     Basin_rain)/=0 ) CALL read_error(3, 'basin_rain')

      IF ( declvar(Precip_module, 'basin_snow', 'one', 1, 'double',
     +     'Basin area-weighted average snowfall for basin',
     +     'inches',
     +     Basin_snow)/=0 ) CALL read_error(3, 'basin_snow')

      IF ( declvar(Precip_module, 'basin_ppt', 'one', 1, 'double',
     +     'Basin area-weighted average precipitation',
     +     'inches',
     +     Basin_ppt)/=0 ) CALL read_error(3, 'basin_ppt')

! DANGER - Not sure what to do about this one.  For right now
!          I'm setting basin_ppt and basin_obs_ppt to the same
!          variable.  In the precip_1sta module, basin_obs_ppt
!          seems to be the area weighted precipitation average before
!          the correction factor is applied.  In other modules,
!          the correction "error" is applied to the station
!          precipitation rather than the hru precipitation.
      IF ( declvar(Precip_module, 'basin_obs_ppt', 'one', 1, 'double',
     +     'Basin area-weighted average measured precipitation',
     +     'inches',
     +     Basin_obs_ppt)/=0 ) CALL read_error(3, 'basin_obs_ppt')

      ALLOCATE (Hru_ppt(Nhru))
      IF ( declvar(Precip_module, 'hru_ppt', 'nhru', Nhru, 'real',
     +     'Precipitation distributed to each HRU',
     +     'inches',
     +     Hru_ppt)/=0 ) CALL read_error(3, 'hru_ppt')

      ALLOCATE (Hru_rain(Nhru))
      IF ( declvar(Precip_module, 'hru_rain', 'nhru', Nhru, 'real',
     +     'Rain distributed to each HRU',
     +     'inches',
     +     Hru_rain)/=0 ) CALL read_error(3, 'hru_rain')

      ALLOCATE (Hru_snow(Nhru))
      IF ( declvar(Precip_module, 'hru_snow', 'nhru', Nhru, 'real',
     +     'Snow distributed to each HRU',
     +     'inches',
     +     Hru_snow)/=0 ) CALL read_error(3, 'hru_snow')

      ALLOCATE ( Tmax_allrain(12) )
      IF ( declparam(Precip_module, 'tmax_allrain', 'nmonths', 'real',
     +     '40.0', '0.0', '90.0',
     +     'Precipitation is rain if HRU max temperature >= this value',
     +     'Monthly (January to December) maximum air temperature'//
     +     ' when precipitation is assumed to be rain; if HRU air'//
     +     ' temperature is greater than or equal to this value,'//
     +     ' precipitation is rain',
     +     'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain')

      IF ( Precip_flag/=8 .OR. Model==99 ) THEN
        IF ( declparam(Precip_module, 'tmax_allsnow', 'one', 'real',
     +       '32.0', '-10.0', '40.0',
     +       'Maximum temperature when precipitation is all snow',
     +       'Maximum air temperature when precipitation is assumed'//
     +       ' to be snow; if HRU air temperature is less than or'//
     +       ' equal to this value, precipitation is snow',
     +       'temp_units')/=0 ) CALL read_error(1, 'tmax_allsnow')
      ENDIF


      IF ( Precip_flag/=8 .OR. Model==99 ) THEN
        ALLOCATE ( Adjmix_rain(12) )
        IF ( declparam(Precip_module, 'adjmix_rain', 'nmonths', 'real',
     +       '1.0', '0.0', '3.0',
     +       'Adjustment factor for rain in a rain/snow mix',
     +       'Monthly (January to December) factor to adjust rain'//
     +       ' proportion in a mixed rain/snow event',
     +       'decimal fraction')/=0 ) CALL read_error(1, 'adjmix_rain')
      ENDIF

      IF ( Precip_flag==5 .OR. Precip_flag==6 .OR. Model==99 ) THEN
        ALLOCATE (Adjust_snow(12))
        IF ( declparam(Precip_module, 'adjust_snow', 'nmonths', 'real',
     +       '0.01', '0.0', '1.0',
     +       'Monthly (January to December) downscaling adjustment'//
     +       ' factor for snow',
     +       'Monthly (January to December) downscaling adjustment'//
     +       ' factor for snow',
     +       'decimal fraction')/=0 ) CALL read_error(1,'adjust_snow')

        ALLOCATE (Adjust_rain(12))
        IF ( declparam(Precip_module, 'adjust_rain', 'nmonths', 'real',
     +       '0.01', '0.0', '1.0',
     +       'Monthly (January to December) downscaling adjustment'//
     +       ' factor for rain',
     +       'Monthly (January to December) downscaling adjustment'//
     +       ' factor for rain',
     +       'decimal fraction')/=0 ) CALL read_error(1,'adjust_rain')
      ENDIF

      IF ( declparam(Precip_module, 'precip_units', 'one', 'integer',
     +     '0', '0', '1',
     +     'Units for measured precipitation',
     +     'Units for measured precipitation (0=inches; 1=mm)',
     +     'none')/=0 ) CALL read_error(1, 'precip_units')

      IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Precip_flag==5
     +     .OR. Model==99 ) THEN
        ALLOCATE ( Psta_elev(Nrain) )
        ALLOCATE ( Psta_elev_meters(Nrain), Psta_elev_feet(Nrain) )
        IF ( declparam(Precip_module, 'psta_elev', 'nrain', 'real',
     +       '0', '-300.0', '30000.0',
     +       'Precipitation station elevation',
     +       'Elevation of each precipitation measurement station',
     +       'elev_units')/=0 ) CALL read_error(1, 'psta_elev')
      ENDIF

! Solar Radiation variables and parameters
      ALLOCATE ( Swrad(Nhru) )
      IF ( declvar(Solrad_module, 'swrad', 'nhru', Nhru, 'real',
     +     'Shortwave radiation distributed to each HRU',
     +     'Langleys',
     +     Swrad)/=0 ) CALL read_error(3, 'swrad')

      IF ( declvar(Solrad_module, 'orad', 'one', 1, 'real',
     +     'Measured or computed solar radiation on a horizontal'//
     +     ' surface',
     +     'Langleys',
     +     Orad)/=0 ) CALL read_error(3, 'orad')

      IF ( declvar(Solrad_module, 'basin_horad', 'one', 1, 'double',
     +     'Potential shortwave radiation for the basin centroid',
     +     'Langleys',
     +     Basin_horad)/=0 ) CALL read_error(3, 'basin_horad')

      IF ( declvar(Solrad_module, 'basin_potsw', 'one', 1, 'double',
     +     'Basin area-weighted average potential shortwave'//
     +     ' radiation',
     +     'Langleys',
     +     Basin_potsw)/=0 ) CALL read_error(3, 'basin_potsw')

      ALLOCATE (Ppt_rad_adj(12))
      IF ( declparam(Solrad_module, 'ppt_rad_adj', 'nmonths', 'real',
     +     '0.02', '0.0', '0.5',
     +     'Radiation reduced if basin precipitation above this value',
     +     'Monthly minimum precipitation, if basin precipitation'//
     +     ' exceeds this value, radiation is'//
     +     ' multiplied by radj_sppt or radj_wppt adjustment factor',
     +     'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')

      IF ( declparam(Solrad_module, 'radmax', 'one', 'real',
     +     '0.8', '0.1', '1.0',
     +     'Maximum fraction of potential solar radiation (decimal)',
     +     'Maximum fraction of the potential solar radiation'//
     +     ' that may reach the ground due to haze, dust, smog, and'//
     +     ' so forth',
     +     'decimal fraction')/=0 ) CALL read_error(1, 'radmax')

      IF ( declparam(Solrad_module, 'radj_sppt', 'one', 'real',
     +     '0.44', '0.0', '1.0',
     +    'Adjustment to solar radiation on precipitation day - summer',
     +     'Adjustment factor for computed solar radiation for'//
     +     ' summer day with greater than ppt_rad_adj inches of'//
     +     ' precipitation',
     +     'decimal fraction')/=0 ) CALL read_error(1, 'radj_sppt')

      IF ( declparam(Solrad_module, 'radj_wppt', 'one', 'real',
     +     '0.5', '0.0', '1.0',
     +    'Adjustment to solar radiation on precipitation day - winter',
     +     'Adjustment factor for computed solar radiation for'//
     +     ' winter day with greater than ppt_rad_adj inches of'//
     +     ' precipitation',
     +     'decimal fraction')/=0 ) CALL read_error(1, 'radj_wppt')

      IF ( Nsol>0 .OR. Model==99 ) THEN
        IF ( declparam(Solrad_module, 'rad_conv', 'one', 'real',
     +       '1.0', '0.1', '100.0',
     +       'Conversion factor to Langleys for measured radiation',
     +     'Conversion factor to Langleys for measured solar radiation',
     +       'none')/=0 ) CALL read_error(1, 'rad_conv')

        IF ( declparam(Solrad_module, 'basin_solsta', 'one', 'integer',
     +       '0', 'bounded', 'nsol',
     +       'Index of main solar radiation station',
     +       'Index of solar radiation station used to compute basin'//
     +       ' radiation values',
     +       'none')/=0 ) CALL read_error(1, 'basin_solsta')

        ALLOCATE (Hru_solsta(Nhru))
        IF ( declparam(Solrad_module, 'hru_solsta', 'nhru', 'integer',
     +      '0', 'bounded', 'nsol',
     +      'Index of solar radiation station associated with each HRU',
     +      'Index of solar radiation station associated with each HRU',
     +      'none')/=0 ) CALL read_error(1, 'hru_solsta')
      ENDIF

! Interception Variables
      ALLOCATE ( Hru_intcpevap(Nhru) )
      IF ( declvar('intcp_dev', 'hru_intcpevap', 'nhru', Nhru, 'real',
     +     'Evaporation from the canopy for each HRU',
     +     'inches', Hru_intcpevap)/=0 )
     +     CALL read_error(3, 'hru_intcpevap')

! Transpiration Variables
      ALLOCATE (Transp_on(Nhru))
      IF ( declvar(Transp_module, 'transp_on', 'nhru', Nhru, 'integer',
     +     'Flag indicating whether transpiration is occurring'//
     +     ' (0=no; 1=yes)',
     +     'none',
     +     Transp_on)/=0 ) CALL read_error(3, 'transp_on')

      IF ( declvar(Transp_module, 'basin_transp_on', 'one', 1,'integer',
     +     'Flag indicating whether transpiration is occurring'//
     +     ' anywhere in the basin (0=no; 1=yes)',
     +     'none',
     +     Basin_transp_on)/=0 ) CALL read_error(3, 'basin_transp_on')

! Potential ET Variables
      ALLOCATE (Potet(Nhru))
      IF ( declvar(Et_module, 'potet', 'nhru', Nhru, 'real',
     +     'Potential ET for each HRU',
     +     'inches',
     +     Potet)/=0 ) CALL read_error(3, 'potet')

      IF ( declvar(Et_module, 'basin_potet', 'one', 1, 'double',
     +     'Basin area-weighted average potential ET',
     +     'inches',
     +     Basin_potet)/=0 ) CALL read_error(3, 'basin_potet')

      IF ( Et_flag==5 .OR. Et_flag==6 .OR. Et_flag==10 ) THEN
        ALLOCATE ( Potet_coef_hru_mo(Nhru,12) )
        IF ( declparam(Et_module, 'potet_coef_hru_mo', 'nhru,nmonths',
     +       'real',
     +       '0.014', '0.005', '0.060',
     +       'Monthly potential ET coefficient adjustment factor',
     +       'Monthly potential ET coefficient adjustment factor for'//
     +       ' each HRU',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'potet_coef_hru_mo')
      ENDIF

! FLOW VARIABLES AND PARAMETERS
! Declare Variables
      ALLOCATE ( Soil_rechr(Nhru) )
      IF ( declvar(Soilzone_module, 'soil_rechr', 'nhru', Nhru, 'real',
     +     'Storage for recharge zone (upper portion) of the'//
     +     ' capillary reservoir that is available for both'//
     +     ' evaporation and transpiration',
     +     'inches', Soil_rechr)/=0 ) CALL read_error(3, 'soil_rechr')

      ALLOCATE (Ssr_to_gw(Nssr))
      IF ( declvar(Soilzone_module, 'ssr_to_gw', 'nssr', Nssr, 'real',
     +     'Drainage from the gravity-reservoir to the associated'//
     +     ' GWR for each HRU',
     +     'inches',
     +     Ssr_to_gw)/=0 ) CALL read_error(3, 'ssr_to_gw')

      ALLOCATE (Ssres_stor(Nssr))
      IF ( declvar(Soilzone_module, 'ssres_stor', 'nssr', Nssr, 'real',
     +     'Storage in the gravity and preferential-flow reservoirs'//
     +     ' for each HRU',
     +     'inches',
     +     Ssres_stor)/=0 ) CALL read_error(3, 'ssres_stor')

      ALLOCATE (Slow_flow(Nhru))
      IF ( declvar(Soilzone_module, 'slow_flow', 'nhru', Nhru, 'real',
     +     'Interflow from gravity reservoir storage that flows to'//
     +     ' the stream network for each HRU',
     +     'inches',
     +     Slow_flow)/=0 ) CALL read_error(3, 'slow_flow')

      ALLOCATE (Ssres_flow(Nssr))
      IF ( declvar(Soilzone_module, 'ssres_flow', 'nssr', Nssr, 'real',
     +     'Interflow from gravity and preferential-flow reservoirs'//
     +     ' to the stream network for each HRU',
     +     'inches',
     +     Ssres_flow)/=0 ) CALL read_error(3, 'ssres_flow')

      IF ( declvar(Soilzone_module, 'basin_ssflow', 'one', 1, 'double',
     +     'Basin area-weighted average interflow from gravity and'//
     +     ' preferential-flow reservoirs to the stream network',
     +     'inches',
     +     Basin_ssflow)/=0 ) CALL read_error(3, 'basin_ssflow')

! soilzone
      IF ( declvar(Soilzone_module, 'basin_swale_et', 'one', 1,
     +     'double',
     +     'Basin area-weighted average ET from swale HRUs',
     +     'inches',
     +     Basin_swale_et)/=0 ) CALL read_error(3, 'basin_swale_et')

      IF ( declvar(Soilzone_module, 'basin_soil_moist', 'one', 1,
     +     'double',
     +     'Basin area-weighted average capillary reservoir storage',
     +     'inches',
     +     Basin_soil_moist)/=0 )
     +     CALL read_error(3, 'basin_soil_moist')

      IF ( declvar(Soilzone_module, 'basin_ssstor', 'one', 1, 'double',
     +     'Basin weighted average gravity and preferential-flow'//
     +     ' reservoir storage',
     +     'inches',
     +     Basin_ssstor)/=0 ) CALL read_error(3, 'basin_ssstor')

      ALLOCATE (Slow_stor(Nhru))
      IF ( declvar(Soilzone_module, 'slow_stor', 'nhru', Nhru, 'real',
     +     'Storage of gravity reservoir for each HRU',
     +     'inches',
     +     Slow_stor)/=0 ) CALL read_error(3, 'slow_stor')

      ALLOCATE (Soil_moist(Nhru))
      IF ( declvar(Soilzone_module, 'soil_moist', 'nhru', Nhru, 'real',
     +     'Storage of capillary reservoir for each HRU',
     +     'inches',
     +     Soil_moist)/=0 ) CALL read_error(3, 'soil_moist')

      ALLOCATE (Hru_actet(Nhru))
      IF ( declvar(Soilzone_module, 'hru_actet', 'nhru', Nhru, 'real',
     +     'Actual ET for each HRU',
     +     'inches',
     +     Hru_actet)/=0 ) CALL read_error(3, 'hru_actet')

      IF ( declvar(Soilzone_module, 'basin_actet', 'one', 1, 'double',
     +     'Basin area-weighted average actual ET',
     +     'inches',
     +     Basin_actet)/=0 ) CALL read_error(3, 'basin_actet')

      IF ( declvar(Soilzone_module, 'basin_perv_et', 'one', 1, 'double',
     +     'Basin area-weighted average ET from capillary reservoirs',
     +     'inches',
     +     Basin_perv_et)/=0 ) CALL read_error(3, 'basin_perv_et')

      IF ( declvar(Soilzone_module, 'basin_lakeevap', 'one', 1,
     +     'double',
     +     'Basin area-weighted average lake evaporation',
     +     'inches',
     +     Basin_lakeevap)/=0 ) CALL read_error(3, 'basin_lakeevap')

      ALLOCATE (Ssres_in(Nssr))
      IF ( declvar(Soilzone_module, 'ssres_in', 'nssr', Nssr, 'real',
     +     'Inflow to the gravity and preferential-flow reservoirs'//
     +     ' for each HRU',
     +     'inches',
     +     Ssres_in)/=0 ) CALL read_error(3, 'ssres_in')

      ALLOCATE (Soil_to_gw(Nhru))
      IF ( declvar(Soilzone_module, 'soil_to_gw', 'nhru', Nhru, 'real',
     +     'Portion of excess flow to the capillary reservoir that'//
     +     ' drains to the associated GWR for each HRU',
     +     'inches',
     +     Soil_to_gw)/=0 ) CALL read_error(3, 'soil_to_gw')

      ALLOCATE (Soil_to_ssr(Nhru))
      IF ( declvar(Soilzone_module, 'soil_to_ssr', 'nhru', Nhru, 'real',
     +     'Portion of excess flow to the capillary reservoir that'//
     +     ' flows to the gravity reservoir for each HRU',
     +     'inches',
     +     Soil_to_ssr)/=0 ) CALL read_error(3, 'soil_to_ssr')

      IF ( declvar(Soilzone_module, 'basin_soil_to_gw', 'one', 1,
     +     'double',
     +     'Basin average excess flow to capillary reservoirs that'//
     +     ' drains to GWRs',
     +     'inches',
     +     Basin_soil_to_gw)/=0 )
     +     CALL read_error(3, 'basin_soil_to_gw')

! gwflow
      ALLOCATE ( Gwres_stor(Ngw) )
      IF ( declvar('gwflow', 'gwres_stor', 'ngw', Ngw, 'double',
     +     'Storage in each GWR',
     +     'inches', Gwres_stor)/=0 ) CALL read_error(3, 'gwres_stor')

! srunoff
      IF ( declvar(Srunoff_module, 'basin_dprst_wb', 'one', 1, 'double',
     +     'Basin area-weighted average capillary reservoir storage',
     +     'inches', Basin_dprst_wb)/=0 )
     +     CALL read_error(3, 'basin_dprst_wb')

      ALLOCATE ( Imperv_stor(Nhru) )
      IF ( declvar(Srunoff_module, 'imperv_stor', 'nhru', Nhru, 'real',
     +     'Storage on impervious area for each HRU',
     +     'inches', Imperv_stor)/=0 ) CALL read_error(3, 'imperv_stor')

      ALLOCATE (Hru_impervevap(Nhru))
      IF ( declvar(Srunoff_module, 'hru_impervevap', 'nhru', Nhru,
     +     'real',
     +     'Evaporation from impervious area for each HRU',
     +     'inches',
     +     Hru_impervevap)/=0 ) CALL read_error(3, 'hru_impervevap')

      ALLOCATE (Hru_impervstor(Nhru))
      IF ( declvar(Srunoff_module, 'hru_impervstor', 'nhru', Nhru,
     +     'real',
     +     'Storage on impervious area for each HRU',
     +     'inches',
     +     Hru_impervstor)/=0 ) CALL read_error(3, 'hru_impervstor')

      IF ( declvar(Srunoff_module, 'basin_imperv_evap', 'one', 1,
     +     'double',
     +     'Basin area-weighted average evaporation from'//
     +     ' impervious area',
     +     'inches',
     +     Basin_imperv_evap)/=0 )
     +     CALL read_error(3, 'basin_imperv_evap')

      IF ( declvar(Srunoff_module, 'basin_imperv_stor', 'one', 1,
     +     'double',
     +     'Basin area-weighted average storage on'//
     +     ' impervious area',
     +     'inches',
     +     Basin_imperv_stor)/=0 )
     +     CALL read_error(3, 'basin_imperv_stor')

      ALLOCATE (Infil(Nhru))
      IF ( declvar(Srunoff_module, 'infil', 'nhru', Nhru, 'real',
     +     'Infiltration to the capillary and preferential-flow'//
     +     ' reservoirs for each HRU',
     +     'inches',
     +     Infil)/=0 ) CALL read_error(3, 'infil')

      IF ( declvar(Srunoff_module, 'basin_infil', 'one', 1, 'double',
     +     'Basin area-weighted average infiltration'//
     +     ' to the capillary reservoirs',
     +     'inches',
     +     Basin_infil)/=0 ) CALL read_error(3, 'basin_infil')

      IF ( Cascade_flag==1 .OR. Model==99 ) THEN
        ALLOCATE (Hru_hortonian_cascadeflow(Nhru))
        IF ( declvar(Srunoff_module, 'hru_hortonian_cascadeflow',
     +      'nhru',
     +       Nhru, 'real',
     +       'Cascading Hortonian surface runoff leaving each HRU',
     +       'inches',
     +       Hru_hortonian_cascadeflow)/=0 )
     +       CALL read_error(3, 'hru_hortonian_cascadeflow')

        ALLOCATE (Hortonian_lakes(Nhru))
        IF ( declvar(Srunoff_module, 'hortonian_lakes', 'nhru', Nhru,
     +       'real',
     +       'Surface runoff to lakes for each HRU',
     +       'inches',
     +       Hortonian_lakes)/=0 )
     +       CALL read_error(3, 'hortonian_lakes')

        IF ( declvar(Srunoff_module, 'basin_hortonian', 'one', 1,
     +       'double',
     +       'Basin area-weighted average Hortonian runoff',
     +       'inches', Basin_hortonian)/=0 )
     +       CALL read_error(3, 'basin_hortonian')

        IF ( declvar(Srunoff_module, 'basin_hortonian_lakes', 'one', 1,
     +       'double',
     +       'Basin area-weighted average Hortonian surface runoff'//
     +       ' to lakes',
     +       'inches',
     +       Basin_hortonian_lakes)/=0 )
     +       CALL read_error(3, 'basin_hortonian_lakes')

        IF ( declvar(Srunoff_module, 'basin_sroff_farflow', 'one',1,
     =       'double',
     +       'Basin area-weighted average cascading surface runoff'//
     +       ' to farfield',
     +       'inches',
     +       Basin_sroff_farflow)/=0 )
     +       CALL read_error(3, 'basin_sroff_farflow')
      ENDIF

      IF ( Nsegment>0 ) THEN
        ALLOCATE ( Strm_seg_in(Nsegment) )
        IF ( declvar(Srunoff_module, 'strm_seg_in', 'nsegment',
     +       Nsegment, 'double',
     +       'Flow in stream segments as a result of cascading flow',
     +       'cfs', Strm_seg_in)/=0 ) CALL read_error(3,'strm_seg_in')
      ENDIF
      IF ( Cascade_flag==1 .OR. Cascadegw_flag==1 .OR. Strmflow_flag>1
     +     .OR. Model==99 ) THEN
        IF ( Nsegment==0 ) STOP 'ERROR, nsegment=0, must be > 0'
      ENDIF

      IF ( Cascade_flag==1 .OR. Cascadegw_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(Srunoff_module, 'strm_farfield', 'one', 1,
     +       'double',
     +       'Flow out of basin as far-field flow',
     +       'cfs', Strm_farfield)/=0 )
     +       CALL read_error(3, 'strm_farfield')
      ENDIF

      IF ( declvar(Srunoff_module, 'basin_sroff', 'one', 1, 'double',
     +     'Basin area-weighted average surface runoff to the'//
     +     ' stream network',
     +     'inches',
     +     Basin_sroff)/=0 ) CALL read_error(3, 'basin_sroff')

      ALLOCATE (Sroff(Nhru))
      IF ( declvar(Srunoff_module, 'sroff', 'nhru', Nhru, 'real',
     +     'Surface runoff to the stream network for each HRU',
     +     'inches',
     +     Sroff)/=0 ) CALL read_error(3, 'sroff')

      IF ( Dprst_flag==1 ) THEN
        IF ( declvar(Srunoff_module, 'basin_dprst_volop', 'one', 1,
     +       'double',
     +       'Basin area-weighted average storage volume in open'//
     +       ' surface depressions',
     +       'inches', Basin_dprst_volop)/=0 )
     +       CALL read_error(3, 'basin_dprst_volop')
        IF ( declvar(Srunoff_module, 'basin_dprst_volcl', 'one', 1,
     +       'double',
     +       'Basin area-weighted average storage volume in closed'//
     +       ' surface depressions',
     +       'inches', Basin_dprst_volcl)/=0 )
     +       CALL read_error(3, 'basin_dprst_volcl')
        IF ( declvar(Srunoff_module, 'basin_dprst_evap', 'one', 1,
     +       'double',
     +       'Basin area-weighted average evaporation from'//
     +       ' surface depression storage',
     +       'inches', Basin_dprst_evap)/=0 )
     +       CALL read_error(3, 'basin_dprst_evap')
        IF ( declvar(Srunoff_module, 'basin_dprst_seep', 'one', 1,
     +       'double',
     +       'Basin area-weighted average seepage from'//
     +       ' surface depression storage',
     +       'inches', Basin_dprst_seep)/=0 )
     +       CALL read_error(3, 'basin_dprst_seep')
      ENDIF

!      IF ( declvar(Srunoff_module, 'basin_glacr_melt', 'one', 1,
!     +     'double',
!     +     'Basin area-weighted average of glacier melt that enters'//
!     +     ' soil recharge zone (melt: NEW SOURCE water to basin)',
!     +     'inches',
!     +     Basin_glacr_melt)/=0 )
!     +     CALL read_error(3, 'basin_glacr_melt')

! Declare Parameters
      IF ( Soilzone_flag==1 ) THEN
        ALLOCATE ( Sat_threshold(Nhru) )
        IF ( declparam(Soilzone_module, 'sat_threshold', 'nhru', 'real',
     +       '999.0', '1.0', '999.0',
     +      'Soil saturation threshold, above field-capacity threshold',
     +       'Water holding capacity of the gravity and preferential-'//
     +       'flow reservoirs; difference between field capacity and'//
     +       ' total soil saturation for each HRU',
     +       'inches')/=0 ) CALL read_error(1, 'sat_threshold')
      ENDIF

      ALLOCATE (Soil_moist_max(Nhru))
      IF ( declparam(Soilzone_module, 'soil_moist_max', 'nhru', 'real',
     +     '6.0', '0.001', '20.0',
     +     'Maximum value of water for soil zone',
     +     'Maximum available water holding capacity of capillary'//
     +     ' reservoir from land surface to rooting depth of the'//
     +     ' major vegetation type of each HRU',
     +     'inches')/=0 ) CALL read_error(1, 'soil_moist_max')

      ALLOCATE (Soil_rechr_max(Nhru))
      IF ( declparam(Soilzone_module, 'soil_rechr_max', 'nhru', 'real',
     +     '2.0', '0.001', '10.0',
     +     'Maximum storage for soil recharge zone',
     +     'Maximum storage for soil recharge zone (upper portion of'//
     +     ' capillary reservoir where losses occur as both'//
     +     ' evaporation and transpiration); must be less than or'//
     +     ' equal to soil_moist_max',
     +     'inches')/=0 ) CALL read_error(1, 'soil_rechr_max')

      ALLOCATE (Carea_max(Nhru))
      IF ( declparam(Srunoff_module, 'carea_max', 'nhru', 'real',
     +     '0.6', '0.0', '1.0',
     +     'Maximum contributing area',
     +     'Maximum possible area contributing to surface runoff'//
     +     ' expressed as a portion of the HRU area',
     +     'decimal fraction')/=0 ) CALL read_error(1, 'carea_max')

      ALLOCATE (Snowinfil_max(Nhru))
      IF ( declparam(Srunoff_module, 'snowinfil_max', 'nhru', 'real',
     +     '2.0', '0.0', '20.0',
     +     'Maximum snow infiltration per day',
     +     'Maximum snow infiltration per day for each HRU',
     +     'inches/day')/=0 ) CALL read_error(1, 'snowinfil_max')

      ALLOCATE (Imperv_stor_max(Nhru))
      IF ( declparam(Srunoff_module, 'imperv_stor_max', 'nhru', 'real',
     +     '0.0', '0.0', '10.0',
     +     'HRU maximum impervious area retention storage',
     +     'Maximum impervious area retention storage for each HRU',
     +     'inches')/=0 ) CALL read_error(1, 'imperv_stor_max')

      climateflow_decl = 0
      END FUNCTION climateflow_decl

!***********************************************************************
!     climateflow_init - Initialize module - get parameter values,
!                        set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION climateflow_init()
      USE PRMS_CLIMATEVARS
      USE PRMS_FLOWVARS
      USE PRMS_MODULE, ONLY: Temp_flag, Precip_flag, Et_flag, Et_module,
     +    Nhru, Nssr, Print_debug, Temp_module, Precip_module,
     +    Solrad_module, Soilzone_module, Srunoff_module, Nsegment,
     +    Soilzone_flag
      USE PRMS_BASIN, ONLY: Timestep, Hru_type, Elev_units, FEET2METERS,
     +    METERS2FEET, NEARZERO
      USE PRMS_CASCADE, ONLY: Cascade_flag
      IMPLICIT NONE
! Functions
      INTRINSIC DBLE
      INTEGER, EXTERNAL :: getparam
      REAL, EXTERNAL :: c_to_f
! Local variables
      INTEGER :: i, j, ierr
!***********************************************************************
      climateflow_init = 1

      IF ( Temp_flag<7 ) THEN 
        IF ( getparam(Temp_module, 'tsta_elev', Ntemp, 'real',
     +       Tsta_elev)/=0 ) CALL read_error(2, 'tsta_elev')
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
        IF ( getparam(Temp_module, 'tmax_adj', Nhru, 'real', Tmax_adj)
     +       /=0 ) CALL read_error(2, 'tmax_adj')

        IF ( getparam(Temp_module, 'tmin_adj', Nhru, 'real', Tmin_adj)
     +       /=0 ) CALL read_error(2, 'tmin_adj')
      ENDIF

      IF ( getparam(Temp_module, 'temp_units', 1, 'integer', Temp_units)
     +     /=0 ) CALL read_error(2, 'temp_units')

!      IF ( Temp_flag<5 .OR.
!     +     ((Temp_flag==7.OR.Temp_flag==8).AND.Ntemp>0) ) THEN
      IF ( Temp_flag<5 ) THEN
          IF ( getparam(Temp_module, 'basin_tsta', 1, 'integer',
     +       Basin_tsta)
     +       /=0 ) CALL read_error(2, 'basin_tsta')
        IF ( Basin_tsta<1 ) Basin_tsta = 1
        IF ( Basin_tsta>Ntemp ) THEN
          PRINT *, 'Warning, basin_tsta specified > ntemp, set to ntemp'
     +             , ', basin_tsta:', Basin_tsta, '; ntemp:', Ntemp
          Basin_tsta = Ntemp
        ENDIF
      ENDIF

      IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4 ) THEN
        IF ( getparam(Temp_module, 'hru_tsta', Nhru, 'integer',Hru_tsta)
     +       /=0 ) CALL read_error(2, 'hru_tsta')
        DO j = 1, Nhru
          IF ( Hru_tsta(j)<1 ) THEN
            IF ( Hru_type(j)/=0 )
     +           PRINT *, 'Warning, hru_tsta specified < 1, set to one',
     +                    ', HRU:', j, '; hru_tsta:', Hru_tsta(j)
            Hru_tsta(j) = 1
          ELSEIF ( Hru_tsta(j)>Ntemp ) THEN
            PRINT *, 'Warning, hru_tsta specified > ntemp, set to ntemp'
     +               , ', HRU:', j, '; hru_tsta:', Hru_tsta(j),
     +               '; ntemp', Ntemp
            Hru_tsta(j) = Ntemp
          ENDIF
        ENDDO
      ENDIF

      IF ( getparam(Precip_module, 'tmax_allrain', 12, 'real',
     +     Tmax_allrain)/=0 ) CALL read_error(2, 'tmax_allrain')

      IF ( getparam(Precip_module, 'tmax_allsnow', 1, 'real',
     +     Tmax_allsnow)
     +     /=0 ) CALL read_error(2, 'tmax_allsnow')

      IF ( getparam(Precip_module, 'adjmix_rain', 12, 'real',
     +     Adjmix_rain)
     +     /=0 ) CALL read_error(2, 'adjmix_rain')

      IF ( Temp_units==0 ) THEN
        Tmax_allrain_f = Tmax_allrain
        Tmax_allsnow_f = Tmax_allsnow
      ELSE
        DO i = 1, 12
          Tmax_allrain_f(i) = c_to_f(Tmax_allrain(i))
        ENDDO
        Tmax_allsnow_f = c_to_f(Tmax_allsnow)
      ENDIF

      IF ( Precip_flag==5 .OR. Precip_flag==6 ) THEN
        IF ( getparam(Precip_module, 'adjust_rain', 12, 'real',
     +       Adjust_rain)
     +       /=0 ) CALL read_error(2, 'adjust_rain')

        IF ( getparam(Precip_module, 'adjust_snow', 12, 'real',
     +       Adjust_snow)
     +       /=0 ) CALL read_error(2, 'adjust_snow')
      ENDIF

      IF ( getparam(Precip_module, 'precip_units', 1, 'integer',
     +     Precip_units)/=0 ) CALL read_error(2, 'precip_units')

      IF ( Precip_flag==2 .OR. Precip_flag==6 .OR. Precip_flag==5 ) THEN
        IF ( getparam(Precip_module, 'psta_elev', Nrain, 'real',
     +       Psta_elev)
     +       /=0 ) CALL read_error(2, 'psta_elev')
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

      IF ( getparam(Solrad_module, 'ppt_rad_adj', 12, 'real',
     +     Ppt_rad_adj)
     +     /=0 ) CALL read_error(2, 'ppt_rad_adj')

      IF ( Nsol>0 ) THEN
        IF ( getparam(Solrad_module, 'basin_solsta', 1, 'integer',
     +       Basin_solsta)/=0 ) CALL read_error(2, 'basin_solsta')
        IF ( Basin_solsta>Nsol ) Basin_solsta = 1

        IF ( getparam(Solrad_module, 'rad_conv', 1, 'real', Rad_conv)
     +       /=0 ) CALL read_error(2, 'rad_conv')

        IF ( getparam(Solrad_module, 'hru_solsta', Nhru, 'integer',
     +       Hru_solsta)/=0 ) CALL read_error(2, 'hru_solsta')
        ierr = 0
        DO j = 1, Nhru
          IF ( Hru_solsta(j)<0 ) Hru_solsta(j) = 0
          IF ( Hru_solsta(j)>Nsol ) THEN
            PRINT *, 'ERROR, measured solar radiation id > nsol for',
     +               ' HRU:', j, ' station:', Hru_solsta(j)
            ierr = 1
          ENDIF
        ENDDO
        IF ( ierr==1 ) STOP
      ENDIF

      IF ( getparam(Solrad_module, 'radmax', 1, 'real', Radmax)
     +     /=0 ) CALL read_error(2, 'radmax')

      IF ( getparam(Solrad_module, 'radj_sppt', 1, 'real', Radj_sppt)
     +     /=0 ) CALL read_error(2, 'radj_sppt')

      IF ( getparam(Solrad_module, 'radj_wppt', 1, 'real', Radj_wppt)
     +     /=0 ) CALL read_error(2, 'radj_wppt')

      IF ( Et_flag==5 .OR. Et_flag==6 .OR. Et_flag==10 ) THEN
        IF ( getparam(Et_module, 'potet_coef_hru_mo', Nhru*12, 'real',
     +       Potet_coef_hru_mo)/=0 )
     +       CALL read_error(2, 'potet_coef_hru_mo')
      ENDIF

      ALLOCATE ( Tmax_hru(Nhru), Tmin_hru(Nhru) )
      IF ( Timestep==0 ) THEN
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
      ENDIF

! FLOW VARIABLES AND PARAMETERS
      IF ( Soilzone_flag==1 ) THEN
        IF ( getparam(Soilzone_module, 'sat_threshold', Nhru, 'real',
     +       Sat_threshold)/=0 ) CALL read_error(2, 'sat_threshold')
      ENDIF

      IF ( getparam(Soilzone_module, 'soil_moist_max', Nhru, 'real',
     +     Soil_moist_max)/=0 ) CALL read_error(2, 'soil_moist_max')

      IF ( getparam(Soilzone_module, 'soil_rechr_max', Nhru, 'real',
     +     Soil_rechr_max)/=0 ) CALL read_error(2, 'soil_rechr_max')

      ! Sanity checks for parameters
      DO i = 1, Nhru
        IF ( Hru_type(i)==0 .OR. Hru_type(i)==2 ) CYCLE
        IF ( Soil_moist_max(i)<0.001 ) THEN
          IF ( ABS(Soil_moist_max(i)-0.001)>NEARZERO ) THEN
            IF ( Print_debug>-1 ) PRINT *, 'HRU:', i,
     +               ' soil_moist_max specified < 0.001, set to 0.001',
     +               Soil_moist_max(i)
          ENDIF
          Soil_moist_max(i) = 0.001
        ENDIF
        IF ( Soil_rechr_max(i)<0.001 ) THEN
          IF ( ABS(Soil_rechr_max(i)-0.001)>NEARZERO ) THEN
            IF ( Print_debug>-1 ) PRINT *, 'HRU:', i,
     +               ' soil_rechr_max specified < 0.001, set to 0.001',
     +               Soil_rechr_max(i)
          ENDIF
          Soil_rechr_max(i) = 0.001
        ENDIF
      ENDDO

      IF ( getparam(Srunoff_module, 'carea_max', Nhru, 'real',
     +     Carea_max)
     +     /=0 ) CALL read_error(2, 'carea_max')

      IF ( getparam(Srunoff_module, 'snowinfil_max', Nhru, 'real',
     +     Snowinfil_max)/=0 ) CALL read_error(2, 'snowinfil_max')

      IF ( getparam(Srunoff_module, 'imperv_stor_max', Nhru, 'real',
     +     Imperv_stor_max)/=0 ) CALL read_error(2, 'imperv_stor_max')

      IF ( Timestep==0 ) THEN
! initialize scalers
        Basin_perv_et = 0.0D0
        Basin_actet = 0.0D0
        Basin_lakeevap = 0.0D0
        Basin_swale_et = 0.0D0
        Basin_imperv_evap = 0.0D0
        Basin_imperv_stor = 0.0D0
        Basin_soil_to_gw = 0.0D0
        Basin_ssflow = 0.0D0
        Basin_soil_moist = 0.0D0
        Basin_ssstor = 0.0D0
        Basin_infil = 0.0D0
        Basin_sroff = 0.0D0
        Basin_glacr_melt = 0.0D0
! initialize arrays (dimensioned Nssr)
        Ssr_to_gw = 0.0
        Ssres_in = 0.0
        Ssres_stor = 0.0
        Ssres_flow = 0.0
! initialize arrays (dimensioned Nhru)
        Slow_stor = 0.0
        Slow_flow = 0.0
        Hru_impervstor = 0.0
        Soil_to_gw = 0.0
        Soil_to_ssr = 0.0
        Soil_moist = 0.0
        Hru_actet = 0.0
        Hru_impervevap = 0.0
        Infil = 0.0
        Sroff = 0.0
        Imperv_stor = 0.0
        IF ( Cascade_flag==1 ) THEN
          Hru_hortonian_cascadeflow = 0.0
          Hortonian_lakes = 0.0
        ENDIF
        Basin_hortonian = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Basin_sroff_farflow = 0.0D0
        IF ( Nsegment>0 ) Strm_seg_in = 0.0D0
        Strm_farfield = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_wb = 0.0D0
        Hru_intcpevap = 0.0
        Soil_rechr = 0.0
        Gwres_stor = 0.0D0
      ENDIF

      climateflow_init = 0
      END FUNCTION climateflow_init

!***********************************************************************
!     Sets temperatures in both system of units for each HRU
!***********************************************************************
      SUBROUTINE temp_set(Ihru, Tmax, Tmin, Tmaxf, Tminf, Tavgf, Tmaxc,
     +                    Tminc, Tavgc, Hru_area)
      USE PRMS_CLIMATEVARS, ONLY: Basin_temp, Basin_tmax, Basin_tmin,
     +    Temp_units, Tmax_hru, Tmin_hru
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(IN) :: Tmax, Tmin, Hru_area
      REAL, INTENT(OUT) :: Tmaxf, Tminf, Tavgf, Tmaxc, Tminc, Tavgc
! Functions
      REAL, EXTERNAL :: c_to_f, f_to_c
      EXTERNAL :: dattim
! Local Variables
      INTEGER :: nowtime(6)
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
        CALL dattim('now', nowtime)
        PRINT *, 'ERROR, invalid temperature value for HRU:', Ihru,
     +           Tminf, Tmaxf, ' Date:', nowtime(1), nowtime(2),
     +           nowtime(3)
        STOP
      ENDIF
      Tmax_hru(Ihru) = Tmax
      Tmin_hru(Ihru) = Tmin

      Basin_tmax = Basin_tmax + Tmax*Hru_area
      Basin_tmin = Basin_tmin + Tmin*Hru_area

      END SUBROUTINE temp_set

!***********************************************************************
!     Computes precipitation form (rain, snow or mix) and
!     depth for each HRU
!***********************************************************************
      SUBROUTINE precip_form(Precip, Hru_ppt, Hru_rain, Hru_snow, Tmaxf,
     +           Tminf, Pptmix, Newsnow, Prmx, Tmax_allrain_f, Rain_adj,
     +           Snow_adj, Adjmix_rain, Hru_area, Sum_obs)
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Tmax_allsnow_f, Basin_ppt, Basin_rain,
     +    Basin_snow
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Tmax_allrain_f, Rain_adj, Snow_adj
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
