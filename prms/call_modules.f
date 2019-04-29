!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      CHARACTER(LEN=16), SAVE :: Process
      CHARACTER(LEN=20), SAVE :: Precip_module, Temp_module, Et_module
      CHARACTER(LEN=20), SAVE :: Srunoff_module, Solrad_module
      CHARACTER(LEN=20), SAVE :: Strmflow_module, Transp_module
      CHARACTER(LEN=20), SAVE :: Soilzone_module, Soltab_module
      CHARACTER(LEN=20), SAVE :: Capillary_module
      CHARACTER(LEN=80), SAVE :: PRMS_versn
      CHARACTER(LEN=80), SAVE :: Version_basin, Version_basin_sum
      CHARACTER(LEN=80), SAVE :: Version_cascade, Version_ccsolrad
      INTEGER, SAVE :: Basin_nc, Basin_sum_nc, Cascade_nc, Ccsolrad_nc
      CHARACTER(LEN=80), SAVE :: Version_climateflow, Version_ddsolrad
      INTEGER, SAVE :: Climateflow_nc, Ddsolrad_nc, Call_modules_nc
      CHARACTER(LEN=80), SAVE :: Version_climate_hru, Version_frost_date
      INTEGER, SAVE :: Climate_hru_nc, Frost_date_nc, Gwflow_nc
      CHARACTER(LEN=80), SAVE :: Version_gwflow, Version_hru_sum
      CHARACTER(LEN=80), SAVE :: Version_ide_dist, Version_intcp
      CHARACTER(LEN=80), SAVE :: Version_map_results, Version_musroute
      INTEGER, SAVE :: Hru_sum_nc, Ide_dist_nc, Intcp_nc, Map_results_nc
      CHARACTER(LEN=80), SAVE :: Version_obs, Version_potet_hamon
      CHARACTER(LEN=80), SAVE :: Version_potet_jh, Version_potet_pan
      INTEGER, SAVE :: Musroute_nc, Obs_nc, Potet_hamon_nc, Potet_jh_nc
      CHARACTER(LEN=80), SAVE :: Version_precip_1sta, Version_snowcomp
      CHARACTER(LEN=80), SAVE :: Version_precip_dist2, Version_soilzone
      INTEGER, SAVE :: Potet_pan_nc, Precip_1sta_nc, Snowcomp_nc
      CHARACTER(LEN=80), SAVE :: Version_precip_laps, Version_soltab
      INTEGER, SAVE :: Precip_dist2_nc, Soilzone_nc, Precip_laps_nc
      CHARACTER(LEN=80), SAVE :: Version_srunoff_carea, Version_strmflow
      CHARACTER(LEN=80), SAVE :: Version_srunoff_smidx, Version_subbasin
      INTEGER, SAVE :: Soltab_nc, Srunoff_carea_nc, Srunoff_smidx_nc
      CHARACTER(LEN=80), SAVE :: Version_temp_1sta, Version_temp_dist2
      INTEGER, SAVE :: Strmflow_nc, Subbasin_nc, Temp_1sta_nc
      CHARACTER(LEN=80), SAVE :: Version_temp_laps, Version_transp_frost
      CHARACTER(LEN=80), SAVE :: Version_transp_tindex, Version_xyz_dist
      INTEGER, SAVE :: Temp_dist2_nc, Temp_laps_nc, Transp_frost_nc
      CHARACTER(LEN=80), SAVE :: Version_strmflow_lake
      INTEGER, SAVE :: Xyz_dist_nc, Strmflow_lake_nc, Transp_tindex_nc
      CHARACTER(LEN=80), SAVE :: Version_muskingum
      CHARACTER(LEN=80), SAVE :: Version_write_climate_hru
      INTEGER, SAVE :: Write_climate_hru_nc, Muskingum_nc
      CHARACTER(LEN=80), SAVE :: Version_potet_jh_hru_mo
      INTEGER, SAVE :: Potet_jh_hru_mo_nc
      INTEGER, SAVE :: Prms_summary_nc
      CHARACTER(LEN=80), SAVE :: Version_prms_summary
      CHARACTER(LEN=80), SAVE :: Version_ccsolrad_prms
      CHARACTER(LEN=80), SAVE :: Version_ddsolrad_prms
      CHARACTER(LEN=80), SAVE :: Version_smbal_prms
      INTEGER, SAVE :: Ccsolrad_prms_nc, Ddsolrad_prms_nc, Smbal_prms_nc
      CHARACTER(LEN=80), SAVE :: Version_potet_hamon_prms
      CHARACTER(LEN=80), SAVE :: Version_ssflow_prms
      INTEGER, SAVE :: Potet_hamon_prms_nc, Ssflow_prms_nc
      CHARACTER(LEN=80), SAVE :: Version_temp_2sta_prms
      CHARACTER(LEN=80), SAVE :: Version_soltab_prms
      INTEGER, SAVE :: Temp_2sta_prms_nc, Soltab_prms_nc
      CHARACTER(LEN=80), SAVE :: Version_potet_hs, Version_potet_pt
      INTEGER, SAVE :: Potet_hs_nc, Potet_pt_nc
      CHARACTER(LEN=80), SAVE, ALLOCATABLE :: Module_versions(:)
      INTEGER, SAVE :: Model, Process_flag, Call_cascade
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Npoigages
      INTEGER, SAVE :: Nsegment, Nsegmentp1, Nsfres, Ndepl, Nmodules
      INTEGER, SAVE :: Dprst_flag, Transp_flag, Nlake
      INTEGER, SAVE :: Sroff_flag, Solrad_flag, Et_flag, Subbasin_flag
      INTEGER, SAVE :: Strmflow_flag, Temp_flag, Precip_flag
      INTEGER, SAVE :: Soilzone_flag, Hru_sum_flag, Soltab_flag
      INTEGER, SAVE :: Et_climate_flag, Solrad_climate_flag
      INTEGER, SAVE :: Precip_climate_flag, Transp_climate_flag
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2;
!              5=ide_dist; 6=xyz_dist; 7=climate_hru
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 4=temp_2sta_prms;
!            5=ide_dist; 6=xyz_dist; 7=climate_hru
! Control parameters
      INTEGER, SAVE :: Print_debug, MapOutON_OFF, CsvON_OFF
      END MODULE PRMS_MODULE

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION call_modules(Arg)
      USE PRMS_MODULE
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTEGER, EXTERNAL :: check_dims, basin, climateflow
      INTEGER, EXTERNAL :: cascade, obs, soltab
      INTEGER, EXTERNAL :: transp_tindex, transp_frost, frost_date
      INTEGER, EXTERNAL :: temp_1sta, temp_dist2, temp_laps
      INTEGER, EXTERNAL :: precip_1sta, precip_laps, climate_hru
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_jh_hru_mo
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow
      INTEGER, EXTERNAL :: srunoff_smidx, srunoff_carea, soilzone
      INTEGER, EXTERNAL :: strmflow, strmflow_lake, musroute
      INTEGER, EXTERNAL :: subbasin, basin_sum, map_results
      INTEGER, EXTERNAL :: write_climate_hru, muskingum
      INTEGER, EXTERNAL :: potet_hs, potet_pt, prms_summary
      INTEGER, EXTERNAL :: soltab_prms, temp_2sta_prms, hru_sum_prms
      INTEGER, EXTERNAL :: ddsolrad_prms, ccsolrad_prms
      INTEGER, EXTERNAL :: smbal_prms, ssflow_prms, potet_hamon_prms
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL module_error, version_check
      EXTERNAL module_version_check, module_doc
! Local Variables
      INTEGER :: n
!***********************************************************************
      call_modules = 1

      Process = Arg
      IF ( Process(:3)=='run' ) THEN
        Process_flag = 0 !(0=run, 1=declare, 2=init, 3=clean, 4=setdims)

      ELSEIF ( Process(:4)=='decl' ) THEN
        Process_flag = 1

        PRMS_versn =
     +'$Id: call_modules.f 4774 2012-08-22 23:37:56Z rsregan $'

        IF ( check_dims()/=0 ) STOP

        IF ( Print_debug>-1 ) THEN
          PRINT 10, PRMS_versn(21:45)
          PRINT 15
          PRINT 16
        ENDIF
  10  FORMAT (///, 5X, 'Surface Water and Energy Budgets Simulated by:',
     +        /, 19X, 'PRMS Version 3.0.2', /, 13X, 'Tag: ', A, /)
  15  FORMAT (/, 'The following modules are available in this version:'
     +        , //, 5X, 'Process',  19X, 'Modules', /, 78('-'), /,
     +        'Basin Definition: basin', /,
     +        '  Cascading Flow: cascade', /,
     +        'Time Series Data: obs', /,
     +        '     Solar Table: soltab, soltab_prms(deprecated)', /, 
     +        ' HRU Temperature: temp_1sta, temp_laps, temp_dist2,', /,
     +        '                  climate_hru',
     +        ' temp_2sta_prms(deprecated)', /,
     +       '      HRU Precip: precip_1sta, precip_laps, precip_dist2,'
     +   , /, '                  climate_hru', /,
     +        '     HRU Climate: xyz_dist, ide_dist', /,
     +        '   HRU Solar Rad: ccsolrad, ddsolrad, climate_hru,', /,
     +        '                  ddsolrad_prms (deprecated)', /,
     +        '                  ccsolrad_prms(deprecated)', /,
     +        '   Transpiration: transp_frost, transp_tindex,', /,
     +        '                  climate_hru', /,
     +        '    Potential ET: potet_hamon, potet_jh, potet_pan,', /,
     +        '                  potet_hs, potet_pt, climate_hru', /,
     +        '                  potet_jh_hru_mo', /,
     +        '    Interception: intcp', /,
     +        '       Snow Melt: snowcomp', /,
     +        '  Surface Runoff: srunoff_smidx, srunoff_carea', /,
     +        '       Soil Zone: soilzone, smbal_prms(deprecated),' /,
     +        '                  ssflow_prms(deprecated)', /,
     +        '     Groundwater: gwflow', /,
     +        '  Stream Routing: strmflow, musroute, muskingum,', /,
     +        '                  strmflow_lake', /,
     +        '  Output Summary: basin_sum, subbasin, map_results', /,
     +    '                  prms_summary, hru_sum_prms(deprecated)', /,
     +        '   Preprocessing: frost_date, write_climate_hru')
  16  FORMAT (//, 'Selected modules listed in the order in which',
     +        ' they are called:', //, 10X, 'Process', 25X,
     +        'Module (source code version)', /, 78('-'))

        Call_modules_nc = INDEX( PRMS_versn, 'Z' )
        n = INDEX( PRMS_versn, '.f' ) + 1
        call_modules = declmodule(PRMS_versn(6:n),
     +                 'Internal PRMS Caller      ',
     +                 PRMS_versn(n+2:Call_modules_nc))

        IF ( Nmodules>0 ) CALL module_doc()

      ELSEIF ( Process(:4)=='init' ) THEN
        Process_flag = 2

        IF ( Nmodules>0 ) CALL module_version_check()

      ELSEIF ( Process(:7)=='setdims' ) THEN
        Process_flag = 4

      ELSE  !IF ( Process(:5)=='clean' ) THEN
        Process_flag = 3
      ENDIF

! All modules must be called for setdims, declare, initialize, and cleanup
      IF ( Process_flag/=0 ) THEN
        call_modules = basin()
        IF ( call_modules/=0 )
     +       CALL module_error('basin', Arg, call_modules)

        IF ( Call_cascade==1 .OR. Process_flag==1 ) THEN
          call_modules = cascade()
          IF ( call_modules/=0 )
     +         CALL module_error('cascade', Arg, call_modules)
        ENDIF

        call_modules = climateflow()
        IF ( call_modules/=0 )
     +       CALL module_error('climateflow', Arg, call_modules)

        IF ( Soltab_flag==1 ) THEN
          call_modules = soltab()
        ELSE ! IF ( Soltab_flag==2 ) THEN
          call_modules = soltab_prms() ! deprecated
        ENDIF
        IF ( call_modules/=0 )
     +       CALL module_error(Soltab_module, Arg, call_modules)

      ENDIF

      call_modules = obs()
      IF ( call_modules/=0 )
     +     CALL module_error('obs', Arg, call_modules)

      IF ( Temp_flag==6 ) THEN
        call_modules = xyz_dist()
        IF ( call_modules/=0 )
     +       CALL module_error(Precip_module, Arg, call_modules)
      ELSEIF ( Temp_flag==5 ) THEN
        call_modules = ide_dist()
          IF ( call_modules/=0 )
     +         CALL module_error(Precip_module, Arg, call_modules)
      ELSE
        IF ( Temp_flag==7 ) THEN
          call_modules = climate_hru()
        ELSEIF ( Temp_flag==1 ) THEN
          call_modules = temp_1sta()
        ELSEIF ( Temp_flag==2 ) THEN
          call_modules = temp_laps()
        ELSEIF ( Temp_flag==3 ) THEN
          call_modules = temp_dist2()
        ELSEIF ( Temp_flag==4 ) THEN
          call_modules = temp_2sta_prms() ! deprecated
        ENDIF
        IF ( call_modules/=0 )
     +       CALL module_error(Temp_module, Arg, call_modules)

        IF ( Precip_climate_flag==1 ) THEN
          call_modules = climate_hru()
        ELSEIF ( Precip_flag==1 ) THEN
          call_modules = precip_1sta()
        ELSEIF ( Precip_flag==2 ) THEN
          call_modules = precip_laps()
        ELSEIF ( Precip_flag==3 ) THEN
          call_modules = precip_dist2()
        ENDIF
        IF ( call_modules/=0 )
     +       CALL module_error(Precip_module, Arg, call_modules)
      ENDIF

      IF ( Model==6 ) THEN
        IF ( Process_flag==0 ) RETURN
      ENDIF

! frost_date is a pre-process module
      IF ( Model==9 ) THEN
        call_modules = frost_date()
        IF ( call_modules/=0 )
     +       CALL module_error('frost_date', Arg, call_modules)
        IF ( Process_flag==0 ) RETURN
        IF ( Process_flag==3 ) STOP
      ENDIF

      IF ( Solrad_flag==1 ) THEN
        call_modules = ddsolrad()
      ELSEIF ( Solrad_climate_flag==1 ) THEN
        call_modules = climate_hru()
      ELSEIF ( Solrad_flag==2 ) THEN
        call_modules = ccsolrad()
      ELSEIF ( Solrad_flag==3 ) THEN
        call_modules = ddsolrad_prms() ! deprecated
      ELSEIF ( Solrad_flag==4 ) THEN
        call_modules = ccsolrad_prms() ! deprecated
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Solrad_module, Arg, call_modules)

      IF ( Transp_flag==1 ) THEN
        call_modules = transp_tindex()
      ELSEIF ( Transp_flag==2 ) THEN
        call_modules = transp_frost()
      ELSEIF ( Transp_climate_flag==1 ) THEN
        call_modules = climate_hru()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Transp_module, Arg, call_modules)

      IF ( Model==8 ) THEN
        IF ( Process_flag==0 ) RETURN
      ENDIF

      IF ( Et_flag==1 ) THEN
        call_modules = potet_jh()
      ELSEIF ( Et_flag==9 ) THEN
        call_modules = potet_jh_hru_mo()
      ELSEIF ( Et_climate_flag==1 ) THEN
        call_modules = climate_hru()
      ELSEIF ( Et_flag==2 ) THEN
        call_modules = potet_hamon()
      ELSEIF ( Et_flag==4 ) THEN
        call_modules = potet_pan()
      ELSEIF ( Et_flag==3 ) THEN
        call_modules = potet_hamon_prms() ! deprecated
      ELSEIF ( Et_flag==10 ) THEN
        call_modules = potet_hs()
      ELSEIF ( Et_flag==5 ) THEN
        call_modules = potet_pt()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Et_module, Arg, call_modules)

      IF ( Model==4 ) THEN
        call_modules = write_climate_hru()
        IF ( call_modules/=0 )
     +       CALL module_error('write_climate_hru', Arg, call_modules)
        IF ( Process_flag==0 ) RETURN
      ENDIF

      IF ( Model==7 ) THEN
        IF ( Process_flag==0 ) RETURN
      ENDIF

      call_modules = intcp()
      IF ( call_modules/=0 )
     +     CALL module_error('intcp', Arg, call_modules)

      call_modules = snowcomp()
      IF ( call_modules/=0 )
     +     CALL module_error('snowcomp', Arg, call_modules)

      IF ( Sroff_flag==1 ) THEN
        call_modules = srunoff_smidx()
      ELSEIF ( Sroff_flag==2 ) THEN
        call_modules = srunoff_carea()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Srunoff_module, Arg, call_modules)

      IF ( Soilzone_flag==1 ) THEN
        call_modules = soilzone()
        IF ( call_modules/=0 )
     +       CALL module_error(Soilzone_module, Arg, call_modules)
      ELSE !IF ( Soilzone_flag==2 ) THEN
        call_modules = smbal_prms() ! deprecated
        IF ( call_modules/=0 )
     +       CALL module_error(Soilzone_module, Arg, call_modules)
        call_modules = ssflow_prms() ! deprecated
        IF ( call_modules/=0 )
     +       CALL module_error('ssflow_prms', Arg, call_modules)
      ENDIF

      call_modules = gwflow()
      IF ( call_modules/=0 )
     +     CALL module_error('gwflow', Arg, call_modules)

      IF ( Strmflow_flag==1 ) THEN
        call_modules = strmflow()
      ELSEIF ( Strmflow_flag==2 ) THEN
        call_modules = strmflow_lake()
      ELSEIF ( Strmflow_flag==3 ) THEN
        call_modules = musroute()
      ELSEIF ( Strmflow_flag==4 ) THEN
        call_modules = muskingum()
      ENDIF
      IF ( call_modules/=0 )
     +     CALL module_error(Strmflow_module, Arg, call_modules)

      IF ( Hru_sum_flag==1 ) THEN
        call_modules = hru_sum_prms()
        IF ( call_modules/=0 )
     +       CALL module_error('hru_sum_prms', Arg, call_modules)
      ENDIF

      call_modules = basin_sum()
      IF ( call_modules/=0 )
     +     CALL module_error('basin_sum', Arg, call_modules)

      IF ( mapOutON_OFF>0 ) THEN
        call_modules = map_results()
        IF ( call_modules/=0 )
     +       CALL module_error('map_results', Arg, call_modules)
      ENDIF

      IF ( CsvON_OFF>0 ) THEN
        call_modules = prms_summary()
        IF ( call_modules/=0 )
     +       CALL module_error('prms_summary', Arg, call_modules)
      ENDIF

      IF ( Subbasin_flag==1 ) THEN
        call_modules = subbasin()
        IF ( call_modules/=0 )
     +       CALL module_error('subbasin', Arg, call_modules)
      ENDIF

      IF ( Process_flag==3 ) PRINT 9001

      IF ( Process(:4)=='decl' ) PRINT 9002

 9001 FORMAT (/, 38('='), /, 'INFORMATION: Normal completion of PRMS',
     +        /, 38('='))
 9002 FORMAT (///, 'Messages from your model.', //,
     +        'Please give careful consideration to fixing all',
     +        ' ERRORS and WARNINGS.', /, 78('-'))

      call_modules = 0
      END FUNCTION call_modules

!***********************************************************************
!     declare the dimensions
!***********************************************************************
      INTEGER FUNCTION setdims()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix, call_modules
      INTEGER, EXTERNAL :: control_string, control_integer
      EXTERNAL read_error
! Control Parameters
      CHARACTER(LEN=16), SAVE :: Model_mode
      ! Maximum values are no longer limits
      INTEGER, PARAMETER :: MAXDIM = 500
! Local Variable
      INTEGER :: idim
!***********************************************************************
      setdims = 1

      ! debug print flag:
      ! -1=quiet - reduced screen output
      ! 0=none; 1=water balances; 2=basin;
      ! 4=basin_sum; 5=soltab; 7=soil zone;
      ! 9=snowcomp; 13=cascade; 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug')/=0 )
     +     CALL read_error(5, 'print_debug')

      IF ( control_string(Model_mode, 'model_mode')/=0 )
     +     CALL read_error(5, 'model_mode')
      IF ( Model_mode(:4)=='PRMS' .OR. Model_mode(:4)=='    ' .OR.
     +     Model_mode(:5)=='DAILY' ) THEN
        Model = 1
      ELSEIF ( Model_mode(:5)=='FROST' ) THEN
        Model = 9
      ELSEIF ( Model_mode(:13)=='WRITE_CLIMATE' ) THEN
        Model = 4
      ELSEIF ( Model_mode(:7)=='CLIMATE' ) THEN
        Model = 6
      ELSEIF ( Model_mode(:5)=='POTET' ) THEN
        Model = 7
      ELSEIF ( Model_mode(:9)=='TRANSPIRE' ) THEN
        Model = 8
      ELSEIF ( Model_mode(:13)=='DOCUMENTATION' ) THEN
        Model = 99
      ELSE
        Model = -1
        PRINT 9002, Model_mode
        STOP
      ENDIF

      IF ( control_string(Temp_module, 'temp_module')/=0 )
     +     CALL read_error(5, 'temp_module')
      IF ( control_string(Precip_module, 'precip_module')
     +     /=0 ) CALL read_error(5, 'precip_module')
      Precip_flag = 1 ! precip_prms or precip_1sta
      IF ( Precip_module(:11)=='precip_laps' ) THEN
        Precip_flag = 2
      ELSEIF ( Precip_module(:12)=='precip_dist2' ) THEN
        Precip_flag = 3
      ELSEIF ( Precip_module(:8)=='ide_dist' ) THEN
        Precip_flag = 5
        IF ( Temp_module(:8)/='ide_dist' ) THEN
          PRINT *, 'if ide_dist is specified for precipitation module,'
          PRINT *, 'it also must be specified for temperature module.'
          PRINT *, 'using ide_dist'
          Temp_module = 'ide_dist            '
          Temp_flag = 5
        ENDIF
      ELSEIF ( Precip_module(:11)=='climate_hru' ) THEN
        Precip_flag = 7
      ELSEIF ( Precip_module(:8)=='xyz_dist' ) THEN
        Precip_flag = 6
        IF ( Temp_module(:8)/='xyz_dist' ) THEN
          PRINT *, 'if xyz_dist is specified for precipitation module,'
          PRINT *, 'it also must be specified for temperature module.'
          PRINT *, 'using xyz_dist'
          Temp_module = 'xyz_dist            '
          Temp_flag = 6
        ENDIF
      ELSEIF ( Precip_module(:11)/='precip_1sta' .AND. 
     +         Precip_module(:11)/='precip_prms' ) THEN
        PRINT *, 'WARNING: Invalid precip_module value, reset to:',
     +           ' precip_1sta, value was: ', Precip_module
        Precip_module = 'precip_1sta         '
      ENDIF

      Temp_flag = 1 ! temp_1sta
      IF ( Temp_module(:9)=='temp_laps' ) THEN
        Temp_flag = 2
      ELSEIF ( Temp_module(:10)=='temp_dist2' ) THEN
        Temp_flag = 3
      ELSEIF ( Temp_module(:9)=='temp_2sta' ) THEN ! deprecated
        Temp_flag = 4
      ELSEIF ( Temp_module(:8)=='ide_dist' ) THEN
        Temp_flag = 5
        IF ( Precip_module(:8)/='ide_dist' ) THEN
          PRINT *, 'if ide_dist is specified for temperature module,'
          PRINT *, 'it also must be specified for precipitation module.'
          PRINT *, 'using ide_dist'
          Precip_module = 'ide_dist            '
          Precip_flag = 5
        ENDIF
      ELSEIF ( Temp_module(:11)=='climate_hru' ) THEN
        Temp_flag = 7
      ELSEIF ( Temp_module(:8)=='xyz_dist' ) THEN
        Temp_flag = 6
        IF ( Precip_module(:8)/='xyz_dist' ) THEN
          PRINT *, 'if xyz_dist is specified for temperature module,'
          PRINT *, 'it also must be specified for precipitation module.'
          PRINT *, 'using xyz_dist'
          Precip_module = 'xyz_dist            '
          Precip_flag = 6
        ENDIF
      ELSEIF ( Temp_module(:9)/='temp_1sta' ) THEN
        PRINT *, 'WARNING: Invalid temp_module value, reset to:',
     +           ' temp_1sta, value was: ', Temp_module
        Temp_module = 'temp_1sta           '
      ENDIF

      IF ( control_string(Transp_module, 'transp_module')
     +     /=0 ) CALL read_error(5, 'transp_module')
      Transp_flag = 1 ! transp_tindex
      IF ( Transp_module(:12)=='transp_frost' ) THEN
        Transp_flag = 2
      ELSEIF ( Transp_module(:11)=='climate_hru' ) THEN
        Transp_flag = 3
      ELSEIF ( Transp_module(:13)/='transp_tindex' ) THEN
        PRINT *, 'WARNING: Invalid transp_module value, reset to:',
     +           ' transp_tindex, value was: ', Transp_module
        Transp_module = 'transp_tindex       '
      ENDIF

      Hru_sum_flag = 0
      IF ( control_string(Et_module, 'et_module')/=0 )
     +     CALL read_error(5, 'et_module')
      Et_flag = 1 ! potet_jh
      IF ( Et_module(:15)=='potet_jh_hru_mo' ) THEN
        Et_flag = 9
      ELSEIF ( Et_module(:16)=='potet_hamon_prms' ) THEN  ! deprecated
        Et_flag = 3
        Hru_sum_flag = 1
      ELSEIF ( Et_module(:11)=='potet_hamon' ) THEN
        Et_flag = 2
      ELSEIF ( Et_module(:11)=='climate_hru' ) THEN
        Et_flag = 7
      ELSEIF ( Et_module(:8)=='potet_hs' ) THEN
        Et_flag = 10
      ELSEIF ( Et_module(:8)=='potet_pt' ) THEN
        Et_flag = 5
      ELSEIF ( Et_module(:9)=='potet_pan' ) THEN
        Et_flag = 4
      ELSEIF ( Et_module(:8)/='potet_jh' ) THEN
        PRINT *, 'WARNING: Invalid et_module value, reset to:',
     +           ' potet_jh, value was: ', Et_module
        Et_module = 'potet_jh            '
      ENDIF

      IF ( control_string(Srunoff_module, 'srunoff_module')
     +     /=0 ) CALL read_error(5, 'srunoff_module')
      Sroff_flag = 1 ! srunoff_smidx
      IF ( Srunoff_module(:13)=='srunoff_carea' ) THEN
        Sroff_flag = 2
      ELSEIF ( Srunoff_module(:13)/='srunoff_smidx' ) THEN
        PRINT *, 'WARNING: Invalid srunoff_module value, reset to:',
     +           ' srunoff_smidx, value was: ', Srunoff_module
        Srunoff_module = 'srunoff_smidx       '
      ENDIF

      IF ( control_string(Soilzone_module, 'soilzone_module')
     +     /=0 ) Soilzone_module = 'soilzone            '
      IF ( Soilzone_module(:5)/='smbal' .AND. ! deprecated
     +     Soilzone_module(:8)/='soilzone' ) THEN
        PRINT *, 'WARNING: Invalid soilzone_module value, reset to:',
     +           ' soilzone, value was: ', Soilzone_module
        Soilzone_module = 'soilzone            '
      ENDIF
      IF ( control_string(Capillary_module, 'capillary_module')
     +     /=0 ) Capillary_module = 'null'
      IF ( Capillary_module(:5)=='smbal' )
     +     Soilzone_module = 'smbal_prms          '

      Soilzone_flag = 2 ! smbal and ssflow - deprecated
      IF ( Soilzone_module(:8)=='soilzone' ) Soilzone_flag = 1

      IF ( control_string(Solrad_module, 'solrad_module')
     +     /=0 ) CALL read_error(5, 'solrad_module')
      Solrad_flag = 1 ! ddsolrad
      IF ( Solrad_module(:13)=='ddsolrad_prms' ) THEN ! deprecated
        Solrad_flag = 3
        Hru_sum_flag = 1
      ELSEIF ( Solrad_module(:13)=='ccsolrad_prms' ) THEN ! deprecated
        Solrad_flag = 4
        Hru_sum_flag = 1
      ELSEIF ( Solrad_module(:11)=='climate_hru' ) THEN
        Solrad_flag = 7
      ELSEIF ( Solrad_module(:8)=='ccsolrad' ) THEN
        Solrad_flag = 2
      ELSEIF ( Solrad_module(:8)/='ddsolrad' ) THEN
        PRINT *, 'WARNING: Invalid solrad_module value, reset to:',
     +           ' ddsolrad, value was: ', Solrad_module
        Solrad_module = 'ddsolrad            '
      ENDIF

      Precip_climate_flag = 0
      IF ( Precip_flag==7 .AND. Temp_flag/=7 ) THEN
          Precip_climate_flag = 1
      ENDIF

      Solrad_climate_flag = 0
      IF ( Solrad_flag==7 .AND. Temp_flag/=7 .AND. Precip_flag/=7 ) THEN
        Solrad_climate_flag = 1
      ENDIF

      Transp_climate_flag = 0
      IF ( Transp_flag==3 .AND. Temp_flag/=7 .AND. Precip_flag/=7 .AND.
     +     Solrad_flag/=7 ) THEN
        Transp_climate_flag = 1
      ENDIF

      Et_climate_flag = 0
      IF ( Et_flag==7 .AND. Temp_flag/=7 .AND. Precip_flag/=7 .AND.
     +     Solrad_flag/=7 .AND. Transp_flag/=3 ) THEN
        Et_climate_flag = 1
      ENDIF

      IF ( control_string(Strmflow_module, 'strmflow_module')
     +     /=0 ) CALL read_error(5, 'strmflow_module')
      IF ( Strmflow_module(:13)/='strmflow_lake' .AND.
     +     Strmflow_module(:8)/='strmflow' .AND.
     +     Strmflow_module(:9)/='muskingum' .AND.
     +     Strmflow_module(:8)/='musroute' ) THEN
        PRINT *, 'WARNING: Invalid strmflow_module value, reset to:',
     +           ' strmflow, value was: ', Strmflow_module
        Strmflow_module = 'strmflow            '
      ENDIF
      Strmflow_flag = 1 ! strmflow
      IF ( Strmflow_module(:13)=='strmflow_lake' ) THEN
        Strmflow_flag = 2
      ELSEIF ( Strmflow_module(:8)=='musroute' ) THEN
        Strmflow_flag = 3
      ELSEIF ( Strmflow_module(:9)=='muskingum' ) THEN
        Strmflow_flag = 4
      ENDIF

! if radpl solrad is specified, use soltab_prms
      IF ( Solrad_flag==3 .OR. Solrad_flag==4 ) THEN
        Soltab_module = 'soltab_prms         ' ! deprecated
        Soltab_flag = 2
        Hru_sum_flag = 1
      ELSE
        Soltab_module = 'soltab              '
        Soltab_flag = 1
      ENDIF

! cascade dimensions
      IF ( decldim('ncascade', 0, MAXDIM,
     +     'Number of HRU links for cascading flow')/=0 )
     +      CALL read_error(7, 'ncascade')
      IF ( decldim('ncascdgw', 0, MAXDIM,
     +     'Number of GWR links for cascading flow')/=0 )
     +      CALL read_error(7, 'ncascdgw')

! nsegment dimension
      idim = 0
      IF ( Strmflow_flag>1 ) idim = 1
      IF ( decldim('nsegment', idim, MAXDIM,
     +     'Number of stream-channel segments')/=0 )
     +     CALL read_error(7, 'nsegment')

      IF ( decldim('npoigages', 0, MAXDIM,
     +     'Number of points-of-interest streamflow gages')/=0 )
     +      CALL read_error(7, 'npoigages')

! subbasin dimensions
      IF ( control_integer(Subbasin_flag, 'subbasin_flag')/=0 )
     +     Subbasin_flag = 1
      IF ( decldim('nsub', 0, MAXDIM, 'Number of internal subbasins')
     +     /=0 ) CALL read_error(7, 'nsub')

      IF ( control_integer(Dprst_flag, 'dprst_flag')/=0 )
     +     Dprst_flag = 0

      IF ( control_integer(CsvON_OFF, 'csvON_OFF')/=0 ) CsvON_OFF = 0

! map results dimensions
      IF ( control_integer(MapOutON_OFF, 'mapOutON_OFF')
     +     /=0 ) MapOutON_OFF = 0
      idim = 0
      IF ( Model==0 .OR. MapOutON_OFF==1 ) idim = 1
      IF ( decldim('nhrucell', idim, MAXDIM,
     +     'Number of unique intersections between HRUs and mapped'//
     +     ' spatial units of a target map')
     +     /=0 ) CALL read_error(7, 'nhrucell')
      IF ( decldim('ngwcell', idim, MAXDIM,
     +     'Number of spatial units in the target grid for mapped'//
     +     ' results')/=0 ) CALL read_error(7, 'ngwcell')

! spatial units
      IF ( decldim('ngw', 1, MAXDIM, 'Number of GWRs')
     +     /=0 ) CALL read_error(7, 'ngw')
      IF ( decldim('nhru', 1, MAXDIM,
     +     'Number of HRUs')/=0 )
     +     CALL read_error(7, 'nhru')
      IF ( decldim('nssr', 1, MAXDIM, 'Number of subsurface reservoirs')
     +     /=0 ) CALL read_error(7, 'nssr')

      idim = 0
      IF ( Strmflow_flag==2 ) idim = 1
      IF ( decldim('nsfres', idim, MAXDIM,
     +     'Number of storage-detention reservoirs and lakes'//
     +     ' connected to the stream network')/=0 )
     +     CALL read_error(7, 'nsfres')
      IF ( decldim('nlake', idim, MAXDIM,
     +     'Number of lake HRUs')/=0 ) CALL read_error(7, 'nlake')

! Time-series data stations, need to know if in Data File
      IF ( decldim('nrain', 0, MAXDIM, 'Number of precipitation'//
     +     ' measurement stations')/=0 ) CALL read_error(7, 'nrain')
      IF ( decldim('nsol', 0, MAXDIM,
     +     'Number of solar-radiation measurement stations')/=0 )
     +       CALL read_error(7, 'nsol')
      IF ( decldim('ntemp', 0, MAXDIM,
     +     'Number of air-temperature measurement stations')
     +     /=0 ) CALL read_error(7, 'ntemp')

! depletion curves
      Ndepl = 1
      IF ( decldim('ndepl', Ndepl, MAXDIM,
     +     'Number of snow-depletion curves')/=0 )
     +      CALL read_error(7, 'ndelp')
      IF ( decldim('ndeplval', Ndepl*11, MAXDIM, 'Number of values in'//
     +     ' all snow-depletion curves (set to ndepl*11)')
     +     /=0 ) CALL read_error(7, 'ndelplval')

      IF ( decldim('nmodules', 0, MAXDIM,
     +     'Number of declared modules in a simulation')/=0 )
     +      CALL read_error(7, 'nmodules')

! fixed dimensions
      IF ( declfix('ndays', 366, 366,
     +     'Maximum number of days in a year ')/=0 )
     +     CALL read_error(7, 'ndays')
      IF ( declfix('nmonths', 12, 12, 'Number of months in a year')
     +     /=0 ) CALL read_error(7, 'nmonths')

      IF ( call_modules('setdims')/=0 ) STOP 'ERROR, in setdims'

 9002 FORMAT ('ERROR in setdims: model_mode: ', A, ' not implemented',/)

      setdims = 0
      END FUNCTION setdims

!***********************************************************************
!     Get and check consistency of dimensions with flags
!***********************************************************************
      INTEGER FUNCTION check_dims()
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Nsub, Nmodules, Nsfres,
     +    Nlake, Nsegment, Nsegmentp1, Ndepl, Soilzone_flag, Model,
     +    Strmflow_flag, Subbasin_flag
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getdim
!***********************************************************************
      check_dims = 1

      Nhru = getdim('nhru')
      IF ( Nhru==-1 ) CALL read_error(7, 'nhru')

      Nssr = getdim('nssr')
      IF ( Nssr==-1 ) CALL read_error(7, 'nssr')

      Ngw = getdim('ngw')
      IF ( Ngw==-1 ) CALL read_error(7, 'ngw')

      IF ( Nhru==0 .OR. Nssr==0 .OR. Ngw==0 ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must be > 0: nhru=', Nhru,
     +           ' nssr=', Nssr, ' ngw=', Ngw
        STOP
      ENDIF

      IF ( Soilzone_flag==1 ) THEN
        IF ( Nssr/=Nhru ) THEN
          PRINT *, 'ERROR, nhru must equal nssr to use soilzone module',
     +             ' nssr=', Nssr, ' nhru=', Nhru
          STOP
        ENDIF
      ENDIF

      Nmodules = getdim('nmodules')
      IF ( Nmodules==-1 ) CALL read_error(7, 'nmodules')

      Nsfres = getdim('nsfres')
      IF ( Nsfres==-1 ) CALL read_error(7, 'nsfres')
      Nlake = getdim('nlake')
      IF ( Nlake==-1 ) CALL read_error(7, 'nlake')
      IF ( Nsfres>0 .AND. Nlake==0 ) THEN
        PRINT *, 'ERROR, dimension nsfres has been changed to nlake.'
        PRINT *, '       All references to nsfres must be changed to',
     +           ' nlake in your Parameter File'
        PRINT *, 'The following parameter names have been changed'
        PRINT *, '   old name        new name'
        PRINT *, 'sfres_type       lake_type'
        PRINT *, 'sfres_out2       lake_out2'
        PRINT *, 'sfres_out2_a     lake_out2_a'
        PRINT *, 'sfres_out2_b     lake_out2_b'
        PRINT *, 'ratetbl_sfres    ratetbl_lake'
        PRINT *, 'sfres_qro        lake_qro'
        PRINT *, 'sfres_din1       lake_din1'
        PRINT *, 'sfres_init       lake_init'
        PRINT *, 'sfres_coef       lake_coef'
        PRINT *, 'sfres_vol_init   lake_vol_init'
        PRINT *, 'elevsurf_init    elevlake_init'
        PRINT *, 'sfres_hru        lake_hru'
        PRINT *, 'seg_res_id       seg_lake_id'
        PRINT *, 'sfres_seep_elev  lake_seep_elev'
        PRINT *, 'Data File: sfres_elev changed to lake_elev'
        PRINT *, ' '
        STOP
      ENDIF

      IF ( Strmflow_flag==2 ) THEN
        IF ( Nlake==0 ) THEN
          PRINT *, 
     +       'ERROR, nlake must be at least 1 when using strmflow_lake'
          STOP
        ENDIF
      ENDIF

      Ndepl = getdim('ndepl')
      IF ( Ndepl==-1 ) CALL read_error(7, 'ndepl')

      Nsub = getdim('nsub')
      IF ( Nsub==-1 ) CALL read_error(7, 'nsub')
      IF ( Subbasin_flag==1 .AND. Nsub==0 ) Subbasin_flag = 0

      Nsegment = getdim('nsegment')
      IF ( Nsegment==-1 ) CALL read_error(7, 'nsegment')
      Nsegmentp1 = Nsegment + 1

      IF ( Nsub>0 .OR. Strmflow_flag>1 ) THEN
        IF ( Nhru/=Ngw .OR. Nhru/=Nssr ) THEN
          PRINT 9001, Nhru, Nssr, Ngw
          STOP
        ENDIF
      ENDIF

 9001 FORMAT ('ERROR, nhru must equal nssr and ngw when using',
     +        ' subbasins, cascades, musroute, or strmflow_lake',
     +        ' nhru=', I8, ' nssr=', I8, ' ngw=', I8)

      check_dims = 0
      END FUNCTION check_dims

!**********************************************************************
!     Module documentation
!**********************************************************************
      SUBROUTINE module_doc()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basin, climateflow, cascade, obs, soltab
      INTEGER, EXTERNAL :: transp_tindex, transp_frost, frost_date
      INTEGER, EXTERNAL :: temp_1sta, temp_dist2, temp_laps
      INTEGER, EXTERNAL :: precip_1sta, precip_laps, climate_hru
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow, prms_summary
      INTEGER, EXTERNAL :: srunoff_smidx, srunoff_carea, soilzone
      INTEGER, EXTERNAL :: strmflow, strmflow_lake, musroute, muskingum
      INTEGER, EXTERNAL :: subbasin, basin_sum, map_results
      INTEGER, EXTERNAL :: write_climate_hru
      INTEGER, EXTERNAL :: potet_hs, potet_pt, potet_jh_hru_mo
      INTEGER, EXTERNAL :: soltab_prms, temp_2sta_prms, hru_sum_prms
      INTEGER, EXTERNAL :: ddsolrad_prms, ccsolrad_prms
      INTEGER, EXTERNAL :: smbal_prms, ssflow_prms, potet_hamon_prms
      INTEGER, EXTERNAL :: declparam
      EXTERNAL read_error
! Local variable
      INTEGER :: call_modules
!**********************************************************************
      ALLOCATE ( Module_versions(Nmodules) )
      IF ( declparam('call_modules', 'module_versions', 'nmodules',
     +     'string',
     +     '#', '#', 'z',
     +     'Version identifiers associated with the Parameter File',
     +     'Version identifiers associated with the Parameter File',
     +     'string')/=0 ) CALL read_error(1, 'module_versions')

      Version_basin = ' '
      Version_basin_sum = ' '
      Version_hru_sum = ' '
      Version_obs = ' '
      Version_snowcomp = ' '
      Version_gwflow = ' '
      Version_intcp = ' '
      Version_climateflow = ' '
      Version_cascade = ' '
      Version_ccsolrad = ' '
      Version_ddsolrad = ' '
      Version_climate_hru = ' '
      Version_frost_date = ' '
      Version_ide_dist = ' '
      Version_map_results = ' '
      Version_musroute = ' '
      Version_muskingum = ' '
      Version_potet_hamon = ' '
      Version_potet_jh = ' '
      Version_potet_jh_hru_mo = ' '
      Version_potet_pan = ' '
      Version_precip_1sta = ' '
      Version_soilzone = ' '
      Version_precip_dist2 = ' '
      Version_precip_laps = ' '
      Version_soltab = ' '
      Version_srunoff_carea = ' '
      Version_strmflow = ' '
      Version_strmflow_lake = ' '
      Version_srunoff_smidx = ' '
      Version_subbasin = ' '
      Version_temp_1sta = ' '
      Version_temp_dist2 = ' '
      Version_temp_laps = ' '
      Version_transp_frost = ' '
      Version_transp_tindex = ' '
      Version_write_climate_hru = ' '
      Version_prms_summary = ' '
      Version_potet_hs = ' '
      Version_potet_pt = ' '
      Version_ccsolrad_prms = ' '
      Version_ddsolrad_prms = ' '
      Version_potet_hamon_prms = ' '
      Version_smbal_prms = ' '
      Version_ssflow_prms = ' '
      Version_temp_2sta_prms = ' '

      IF ( Model==99 ) THEN
        call_modules = basin()
        call_modules = climateflow()
        call_modules = cascade()
        call_modules = soltab()
        call_modules = soltab_prms() ! deprecated
        call_modules = obs()
        call_modules = temp_1sta()
        call_modules = temp_laps()
        call_modules = temp_2sta_prms() ! deprecated
        call_modules = temp_dist2()
        call_modules = xyz_dist()
        call_modules = ide_dist()
        call_modules = climate_hru()
        call_modules = frost_date()
        call_modules = transp_tindex()
        call_modules = transp_frost()
        call_modules = precip_1sta()
        call_modules = precip_laps()
        call_modules = precip_dist2()
        call_modules = ddsolrad()
        call_modules = ddsolrad_prms() ! deprecated
        call_modules = ccsolrad()
        call_modules = ccsolrad_prms() ! deprecated
        call_modules = potet_jh()
        call_modules = potet_jh_hru_mo()
        call_modules = potet_hamon()
        call_modules = potet_hamon_prms() ! deprecated
        call_modules = potet_hs()
        call_modules = potet_pt()
        call_modules = potet_pan()
        call_modules = write_climate_hru()
        call_modules = intcp()
        call_modules = snowcomp()
        call_modules = srunoff_smidx()
        call_modules = srunoff_carea()
        call_modules = smbal_prms() ! deprecated
        call_modules = ssflow_prms() ! deprecated
        call_modules = soilzone()
        call_modules = gwflow()
        call_modules = strmflow()
        call_modules = strmflow_lake()
        call_modules = musroute()
        call_modules = muskingum()
        call_modules = subbasin()
        call_modules = hru_sum_prms()
        call_modules = basin_sum()
        call_modules = prms_summary()
        call_modules = map_results()
        call_modules = 0
        PRINT 9001
        STOP
      ENDIF

 9001 FORMAT (//, ' _name files were generated and written to files', /,
     +        ' in the directory containing the control file.', /,
     +        ' Ignore any messages that follow this one.', /,
     +        ' Note, no simulation was computed.', /)

      END SUBROUTINE module_doc

!**********************************************************************
!     Module version check strings
!**********************************************************************
      SUBROUTINE module_version_check()
      USE PRMS_MODULE
! Functions
      INTEGER, EXTERNAL :: getparamstring
      EXTERNAL read_error, version_check
! Local Variable
      INTEGER :: i
!**********************************************************************
! compare module versions
      DO i = 1, Nmodules
        IF ( getparamstring('call_modules', 'module_versions', Nmodules,
     +       'string', i-1, Module_versions(i))/=0 )
     +       CALL read_error(2, 'module_versions')
        IF ( Module_versions(i)(:1)=='#' ) CYCLE
        IF ( Module_versions(i)(18:29)=='call_modules' ) THEN
          CALL version_check(Module_versions(i), Call_modules_nc,
     +                       PRMS_versn)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='basin_sum' ) THEN
          CALL version_check(Module_versions(i), Basin_sum_nc,
     +                       Version_basin_sum)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:22)=='basin' ) THEN
          CALL version_check(Module_versions(i), Basin_nc,
     +                       Version_basin)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='climateflow' ) THEN
          CALL version_check(Module_versions(i), Climateflow_nc,
     +                       Version_climateflow)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:29)=='hru_sum_prms' ) THEN
          CALL version_check(Module_versions(i), Hru_sum_nc,
     +                       Version_hru_sum)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:22)=='intcp' ) THEN
          CALL version_check(Module_versions(i), Intcp_nc,
     +                       Version_intcp)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:23)=='gwflow' ) THEN
          CALL version_check(Module_versions(i), Gwflow_nc,
     +                       Version_gwflow)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:20)=='obs' ) THEN
          CALL version_check(Module_versions(i), Obs_nc,
     +                       Version_obs)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:24)=='cascade' ) THEN
          CALL version_check(Module_versions(i), Cascade_nc,
     +                       Version_cascade)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='soltab_prms' ) THEN
          CALL version_check(Module_versions(i), Soltab_prms_nc,
     +                       Version_soltab_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:23)=='soltab' ) THEN
          CALL version_check(Module_versions(i), Soltab_nc,
     +                       Version_soltab)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='ccsolrad_prms' ) THEN
          CALL version_check(Module_versions(i), Ccsolrad_prms_nc,
     +                       Version_ccsolrad_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='ccsolrad' ) THEN
          CALL version_check(Module_versions(i), Ccsolrad_nc,
     +                       Version_ccsolrad)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='ddsolrad_prms' ) THEN
          CALL version_check(Module_versions(i), Ddsolrad_prms_nc,
     +                       Version_ddsolrad_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='ddsolrad' ) THEN
          CALL version_check(Module_versions(i), Ddsolrad_nc,
     +                       Version_ddsolrad)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:33)=='potet_hamon_prms' ) THEN
          CALL version_check(Module_versions(i), Potet_hamon_prms_nc,
     +                       Version_potet_hamon_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='potet_hamon' ) THEN
          CALL version_check(Module_versions(i), Potet_hamon_nc,
     +                       Version_potet_hamon)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='climate_hru' ) THEN
          CALL version_check(Module_versions(i), Climate_hru_nc,
     +                       Version_climate_hru)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:27)=='frost_date' ) THEN
          CALL version_check(Module_versions(i), Frost_date_nc,
     +                       Version_frost_date)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='ide_dist' ) THEN
          CALL version_check(Module_versions(i), Ide_dist_nc,
     +                       Version_ide_dist)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='xyz_dist' ) THEN
          CALL version_check(Module_versions(i), Xyz_dist_nc,
     +                       Version_xyz_dist)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='map_results' ) THEN
          CALL version_check(Module_versions(i), Map_results_nc,
     +                       Version_map_results)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='musroute' ) THEN
          CALL version_check(Module_versions(i), Musroute_nc,
     +                       Version_musroute)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='muskingum' ) THEN
          CALL version_check(Module_versions(i), Muskingum_nc,
     +                       Version_muskingum)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:32)=='potet_jh_hru_mo' ) THEN
          CALL version_check(Module_versions(i), Potet_jh_hru_mo_nc,
     +                       Version_potet_jh_hru_mo)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='potet_jh' ) THEN
          CALL version_check(Module_versions(i), Potet_jh_nc,
     +                       Version_potet_jh)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='potet_pan' ) THEN
          CALL version_check(Module_versions(i), Potet_pan_nc,
     +                       Version_potet_pan)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='precip_1sta' ) THEN
          CALL version_check(Module_versions(i), Precip_1sta_nc,
     +                       Version_precip_1sta)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='precip_laps' ) THEN
          CALL version_check(Module_versions(i), Precip_laps_nc,
     +                       Version_precip_laps)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:29)=='precip_dist2' ) THEN
          CALL version_check(Module_versions(i), Precip_dist2_nc,
     +                       Version_precip_dist2)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='soilzone' ) THEN
          CALL version_check(Module_versions(i), Soilzone_nc,
     +                       Version_soilzone)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='snowcomp' ) THEN
          CALL version_check(Module_versions(i), Snowcomp_nc,
     +                       Version_snowcomp)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='srunoff_carea' ) THEN
          CALL version_check(Module_versions(i), Srunoff_carea_nc,
     +                       Version_srunoff_carea)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='srunoff_smidx' ) THEN
          CALL version_check(Module_versions(i), Srunoff_smidx_nc,
     +                       Version_srunoff_smidx)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:27)=='temp_dist2' ) THEN
          CALL version_check(Module_versions(i), Temp_dist2_nc,
     +                       Version_temp_dist2)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='temp_laps' ) THEN
          CALL version_check(Module_versions(i), Temp_laps_nc,
     +                       Version_temp_laps)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:26)=='temp_1sta' ) THEN
          CALL version_check(Module_versions(i), Temp_1sta_nc,
     +                       Version_temp_1sta)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:31)=='temp_2sta_prms' ) THEN
          CALL version_check(Module_versions(i), Temp_2sta_prms_nc,
     +                       Version_temp_2sta_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='strmflow_lake' ) THEN
          CALL version_check(Module_versions(i), Strmflow_lake_nc,
     +                       Version_strmflow_lake)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='strmflow' ) THEN
          CALL version_check(Module_versions(i), Strmflow_nc,
     +                       Version_strmflow)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='subbasin' ) THEN
          CALL version_check(Module_versions(i), Subbasin_nc,
     +                       Version_subbasin)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:29)=='transp_frost' ) THEN
          CALL version_check(Module_versions(i), Transp_frost_nc,
     +                       Version_transp_frost)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:30)=='transp_tindex' ) THEN
          CALL version_check(Module_versions(i), Transp_tindex_nc,
     +                       Version_transp_tindex)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:27)=='smbal_prms' ) THEN
          CALL version_check(Module_versions(i), Smbal_prms_nc,
     +                       Version_smbal_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:28)=='ssflow_prms' ) THEN
          CALL version_check(Module_versions(i), Ssflow_prms_nc,
     +                       Version_ssflow_prms)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='potet_hs' ) THEN
          CALL version_check(Module_versions(i), Potet_hs_nc,
     +                       Version_potet_hs)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:25)=='potet_pt' ) THEN
          CALL version_check(Module_versions(i), Potet_pt_nc,
     +                       Version_potet_pt)
          CYCLE
        ENDIF
        IF ( Module_versions(i)(18:34)=='write_climate_hru' ) THEN
          CALL version_check(Module_versions(i), Write_climate_hru_nc,
     +                       Version_write_climate_hru)
        ENDIF
        IF ( Module_versions(i)(18:34)=='prms_summary' ) THEN
          CALL version_check(Module_versions(i), Prms_summary_nc,
     +                       Version_prms_summary)
        ENDIF
      ENDDO

      END SUBROUTINE module_version_check
