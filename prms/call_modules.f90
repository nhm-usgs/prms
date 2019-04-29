!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAXFILE_LENGTH = 128
      CHARACTER(LEN=72), PARAMETER :: &
     &  EQULS = '========================================================================'
      CHARACTER(LEN=12), PARAMETER :: MODNAME = 'call_modules'
      CHARACTER(LEN=16), SAVE :: Process
      CHARACTER(LEN=80), SAVE :: PRMS_versn
      INTEGER, SAVE :: Model, Process_flag, Call_cascade, Ncascade, Ncascdgw
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Npoigages, Nhrucell, Nlake
      INTEGER, SAVE :: Ntemp, Nrain, Nsol, Nsegment, Nsegmentp1, Ndepl
      INTEGER, SAVE :: Transp_flag, Sroff_flag, Solrad_flag, Et_flag
      INTEGER, SAVE :: Soltab_flag, Soilzone_flag, Hru_sum_flag
      INTEGER, SAVE :: Strmflow_flag, Temp_flag, Precip_flag, Et_climate_flag, Solrad_climate_flag
      INTEGER, SAVE :: Precip_combined_flag, Temp_combined_flag, Et_combined_flag
      INTEGER, SAVE :: Precip_climate_flag, Transp_climate_flag, Orad_flag, Inputerror_flag
      INTEGER, SAVE :: Nratetbl
      INTEGER, SAVE :: PRMS_output_unit, Restart_inunit, Restart_outunit
!   Declared Variables used by GSFLOW only, in so that soilzone can be one version
      INTEGER, SAVE :: Kkiter
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 8=climate_hru_mo
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 8=climate_hru_mo
! Control parameters
      INTEGER, SAVE :: Print_debug, MapOutON_OFF, CsvON_OFF, Dprst_flag, Subbasin_flag, Parameter_check_flag
      INTEGER, SAVE :: Init_vars_from_file, Save_vars_to_file, Cascade_flag, Cascadegw_flag
      CHARACTER(LEN=MAXFILE_LENGTH) :: Tmin_day, Tmax_day, Precip_day, Potet_day, Swrad_day, Transp_day
      CHARACTER(LEN=MAXFILE_LENGTH) :: Model_output_file, Var_init_file, Var_save_file, Csv_output_file
      CHARACTER(LEN=20), SAVE :: Precip_module, Temp_module, Et_module, Srunoff_module, Solrad_module
      CHARACTER(LEN=20), SAVE :: Strmflow_module, Transp_module, Soilzone_module, Soltab_module, Capillary_module
      CHARACTER(LEN=16), SAVE :: Model_mode
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
      INTEGER, EXTERNAL :: cascade, obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: transp_frost, frost_date
      INTEGER, EXTERNAL :: temp_1sta_laps, temp_dist2
      INTEGER, EXTERNAL :: precip_1sta_laps, climate_hru_combined
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon, potet_hs, potet_pt
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow
      INTEGER, EXTERNAL :: srunoff, soilzone
      INTEGER, EXTERNAL :: strmflow, subbasin, basin_sum, map_results, strmflow_in_out
      INTEGER, EXTERNAL :: write_climate_hru, muskingum, prms_summary, strmflow_lake
      INTEGER, EXTERNAL :: declvar
      EXTERNAL module_error, read_error, module_doc, print_module, PRMS_open_output_file
      INTEGER, EXTERNAL :: soltab_prms, temp_2sta_prms, hru_sum_prms
      INTEGER, EXTERNAL :: ddsolrad_prms, ccsolrad_prms
      INTEGER, EXTERNAL :: smbal_prms, ssflow_prms, potet_hamon_prms
! Local Variables
      INTEGER :: iret
!***********************************************************************
      call_modules = 1

      Process = Arg

      IF ( Process(:3)=='run' ) THEN
        Process_flag = 0 !(0=run, 1=declare, 2=init, 3=clean, 4=setdims)

      ELSEIF ( Process(:4)=='decl' ) THEN
        Process_flag = 1
        Inputerror_flag = 0

        PRMS_versn = '$Id: call_modules.f90 5628 2013-04-24 21:35:47Z rsregan $'

        IF ( check_dims()/=0 ) STOP

        IF ( Print_debug>-1 ) THEN
          PRINT 10, PRMS_versn(22:47)
          PRINT 15
          PRINT 16
        ENDIF
  10  FORMAT (///, 5X, 'Surface Water and Energy Budgets Simulated by:', /, 19X, 'PRMS Version 3.0.5', /, 13X, 'Tag: ', A, /)
  15  FORMAT (/, 'The following modules are available in this version:', //, 5X, 'Process',  19X, 'Modules', /, 78('-'), /, &
     &        'Basin Definition: basin', /, &
     &        '  Cascading Flow: cascade', /, &
     &        'Time Series Data: obs', /, &
     &        '     Solar Table: soltab', /, &
     &        ' HRU Temperature: temp_1sta, temp_laps, temp_dist2,', /, &
     &        '                  climate_hru, climate_hru_mo', /, &
     &        '      HRU Precip: precip_1sta, precip_laps, precip_dist2,', /, &
     &        '                  climate_hru, climate_hru_mo,', /, &
     &        '                  temp_2sta_prms(deprecated)', /, &
     &        '     HRU Climate: xyz_dist, ide_dist', /, &
     &        '   HRU Solar Rad: ccsolrad, ddsolrad, climate_hru,', /, &
     &        '                  climate_hru_mo, ddsolrad_prms (deprecated)', /, &
     &        '                  ccsolrad_prms(deprecated)', /, &
     &        '   Transpiration: transp_tindex, climate_hru,', /, &
     &        '                  transp_frost, climate_hru_mo', /, &
     &        '    Potential ET: potet_hamon, potet_jh, potet_pan,', /, &
     &        '                  climate_hru, potet_hs, potet_pt,', /, &
     &        '                  potet_jh_hru_mo, climate_hru_mo', /, &
     &        '    Interception: intcp', /, &
     &        '       Snow Melt: snowcomp', /, &
     &        '  Surface Runoff: srunoff_smidx, srunoff_carea', /, &
     &        '       Soil Zone: soilzone, smbal_prms(deprecated),' /, &
     &        '                  ssflow_prms(deprecated)', /, &
     &        '     Groundwater: gwflow', /, &
     &        '  Stream Routing: strmflow, muskingum, strmflow_in_out,', /, &
     &        '                  strmflow_lake', /, &
     &        '  Output Summary: basin_sum, subbasin, map_results,', /, &
     &        '                  prms_summary, hru_sum_prms(deprecated)', /, &
     &        '   Preprocessing: frost_date, write_climate_hru')
  16  FORMAT (//, 'Selected modules listed in the order in which they are called:', //, 10X, 'Process', 25X, &
     &        'Module (source code version)', /, 78('-'))

        CALL print_module(PRMS_versn, 'Internal PRMS Caller      ', 90)

        IF ( Model==99 ) THEN
          CALL module_doc()
          call_modules = 0
          RETURN
        ENDIF

        IF ( Model==0 ) THEN
          IF ( declvar(MODNAME, 'KKITER', 'one', 1, 'integer', &
     &         'Current iteration in GSFLOW simulation', 'none', KKITER)/=0 ) CALL read_error(3, 'KKITER')
        ENDIF
        Kkiter = 1 ! set for PRMS-only mode

      ELSEIF ( Process(:4)=='init' ) THEN
        Process_flag = 2

      ELSEIF ( Process(:7)=='setdims' ) THEN
        Process_flag = 4

      ELSE  !IF ( Process(:5)=='clean' ) THEN
        Process_flag = 3
        IF ( Save_vars_to_file==1 ) THEN
          CALL PRMS_open_output_file(Restart_outunit, Var_save_file, 'var_save_file', 1, iret)
          IF ( iret/=0 ) STOP
        ENDIF

      ENDIF

! All modules must be called for setdims, declare, initialize, and cleanup
      IF ( Process_flag/=0 ) THEN
        call_modules = basin()
        IF ( call_modules/=0 ) CALL module_error('basin', Arg, call_modules)

        IF ( Call_cascade==1 ) THEN
          call_modules = cascade()
          IF ( call_modules/=0 ) CALL module_error('cascade', Arg, call_modules)
        ENDIF

        call_modules = climateflow()
        IF ( call_modules/=0 ) CALL module_error('climateflow', Arg, call_modules)

        IF ( Soltab_flag==1 ) THEN
          call_modules = soltab()
        ELSE ! IF ( Soltab_flag==2 ) THEN
          call_modules = soltab_prms() ! deprecated
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Soltab_module, Arg, call_modules)

      ENDIF

      call_modules = obs()
      IF ( call_modules/=0 ) CALL module_error('obs', Arg, call_modules)

      IF ( Temp_flag==6 ) THEN
        call_modules = xyz_dist()
        IF ( call_modules/=0 ) CALL module_error(Precip_module, Arg, call_modules)
      ELSEIF ( Temp_flag==5 ) THEN
        call_modules = ide_dist()
        IF ( call_modules/=0 ) CALL module_error(Precip_module, Arg, call_modules)
      ELSE
        IF ( Temp_combined_flag==2 ) THEN
          call_modules = climate_hru_combined()
        ELSEIF ( Temp_combined_flag==1 ) THEN
          call_modules = temp_1sta_laps()
        ELSEIF ( Temp_flag==3 ) THEN
          call_modules = temp_dist2()
        ELSEIF ( Temp_flag==4 ) THEN
          call_modules = temp_2sta_prms() ! deprecated
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Temp_module, Arg, call_modules)

        IF ( Precip_climate_flag>0 ) THEN
          call_modules = climate_hru_combined()
        ELSEIF ( Precip_combined_flag==1 ) THEN
          call_modules = precip_1sta_laps()
        ELSEIF ( Precip_flag==3 ) THEN
          call_modules = precip_dist2()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Precip_module, Arg, call_modules)
      ENDIF

      IF ( Model==6 ) THEN
        IF ( Process_flag==0 ) RETURN
      ENDIF

! frost_date is a pre-process module
      IF ( Model==9 ) THEN
        call_modules = frost_date()
        IF ( call_modules/=0 ) CALL module_error('frost_date', Arg, call_modules)
        IF ( Process_flag==0 ) RETURN
        IF ( Process_flag==3 ) STOP
      ENDIF

      IF ( Solrad_flag==1 ) THEN
        call_modules = ddsolrad()
      ELSEIF ( Solrad_climate_flag>0 ) THEN
        call_modules = climate_hru_combined()
      ELSEIF ( Solrad_flag==2 ) THEN
        call_modules = ccsolrad()
      ELSEIF ( Solrad_flag==3 ) THEN
        call_modules = ddsolrad_prms() ! deprecated
      ELSEIF ( Solrad_flag==4 ) THEN
        call_modules = ccsolrad_prms() ! deprecated
      ENDIF
      IF ( call_modules/=0 ) CALL module_error(Solrad_module, Arg, call_modules)

      IF ( Transp_flag==1 ) THEN
        call_modules = transp_tindex()
      ELSEIF ( Transp_flag==2 ) THEN
        call_modules = transp_frost()
      ELSEIF ( Transp_climate_flag>0 ) THEN
        call_modules = climate_hru_combined()
      ENDIF
      IF ( call_modules/=0 ) CALL module_error(Transp_module, Arg, call_modules)

      IF ( Model==8 ) THEN
        IF ( Process_flag==0 ) RETURN
      ENDIF

      IF ( Et_combined_flag==1 ) THEN
        call_modules = potet_jh()
      ELSEIF ( Et_climate_flag>0 ) THEN
        call_modules = climate_hru_combined()
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
      IF ( call_modules/=0 ) CALL module_error(Et_module, Arg, call_modules)

      IF ( Model==4 ) THEN
        call_modules = write_climate_hru()
        IF ( call_modules/=0 ) CALL module_error('write_climate_hru', Arg, call_modules)
        IF ( Process_flag==0 ) RETURN
      ENDIF

      IF ( Model==7 ) THEN
        IF ( Process_flag==0 ) RETURN
      ENDIF

      call_modules = intcp()
      IF ( call_modules/=0 ) CALL module_error('intcp', Arg, call_modules)

      call_modules = snowcomp()
      IF ( call_modules/=0 ) CALL module_error('snowcomp', Arg, call_modules)

      call_modules = srunoff()
      IF ( call_modules/=0 ) CALL module_error(Srunoff_module, Arg, call_modules)

      IF ( Soilzone_flag==1 ) THEN
        call_modules = soilzone()
        IF ( call_modules/=0 ) CALL module_error(Soilzone_module, Arg, call_modules)
      ELSE !IF ( Soilzone_flag==2 ) THEN
        call_modules = smbal_prms() ! deprecated
        IF ( call_modules/=0 ) CALL module_error(Soilzone_module, Arg, call_modules)
        call_modules = ssflow_prms() ! deprecated
        IF ( call_modules/=0 ) CALL module_error('ssflow_prms', Arg, call_modules)
      ENDIF

      call_modules = gwflow()
      IF ( call_modules/=0 ) CALL module_error('gwflow', Arg, call_modules)

      IF ( Strmflow_flag==1 ) THEN
        call_modules = strmflow()
      ELSEIF ( Strmflow_flag==4 ) THEN
        call_modules = muskingum()
      ELSEIF ( Strmflow_flag==2 ) THEN
        call_modules = strmflow_lake()
      ELSEIF ( Strmflow_flag==5 ) THEN
        call_modules = strmflow_in_out()
      ENDIF
      IF ( call_modules/=0 ) CALL module_error(Strmflow_module, Arg, call_modules)

      IF ( Hru_sum_flag==1 ) THEN
        call_modules = hru_sum_prms()
        IF ( call_modules/=0 ) CALL module_error('hru_sum_prms', Arg, call_modules)
      ENDIF

      call_modules = basin_sum()
      IF ( call_modules/=0 ) CALL module_error('basin_sum', Arg, call_modules)

      IF ( MapOutON_OFF>0 ) THEN
        call_modules = map_results()
        IF ( call_modules/=0 ) CALL module_error('map_results', Arg, call_modules)
      ENDIF

      IF ( CsvON_OFF>0 ) THEN
        call_modules = prms_summary()
        IF ( call_modules/=0 ) CALL module_error('prms_summary', Arg, call_modules)
      ENDIF

      IF ( Subbasin_flag==1 ) THEN
        call_modules = subbasin()
        IF ( call_modules/=0 ) CALL module_error('subbasin', Arg, call_modules)
      ENDIF

      IF ( Process_flag==3 ) THEN
        PRINT 9001
      ELSEIF ( Process_flag==1 ) THEN
        PRINT 9002
      ELSEIF ( Process_flag==2 ) THEN
        IF ( Inputerror_flag==1 ) THEN
          IF ( Model==99 ) STOP
          PRINT '(//,A,/,A)', '**FIX input errors in your Parameter File to continue**', &
     &          'NOTE: some errors may be due to use of defalut values'
          PRINT '(/,A,/,A,/,A,/,A,/,A,/)', &
     &          'If input errors are related to paramters used for automated', &
     &          'calibration processes, with CAUTION, set control parameter', &
     &          'ignore_calib_parameters to 1. After calibration set the', &
     &          'paramter to 0 to verify that those calibration parameters', &
     &          'have valid and compatible values.'
          STOP
        ENDIF
      ENDIF

 9001 FORMAT (/, 38('='), /, 'INFORMATION: Normal completion of PRMS', /, 38('='))
 9002 FORMAT (///, 'Messages from your model.', //, 'Please give careful consideration to fixing all', &
     &        ' ERRORS and WARNINGS.', /, 78('-'))

      END FUNCTION call_modules

!***********************************************************************
!     declare the dimensions
!***********************************************************************
      INTEGER FUNCTION setdims()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix, call_modules, numchars, control_string, control_integer
      EXTERNAL read_error, PRMS_open_output_file, PRMS_open_input_file
! Local Variables
      ! Maximum values are no longer limits
      INTEGER, PARAMETER :: MAXDIM = 500
      INTEGER :: idim, iret
!***********************************************************************
      setdims = 1

      ! debug print flag:
      ! -1=quiet - reduced screen output
      ! 0=none; 1=water balances; 2=basin;
      ! 4=basin_sum; 5=soltab; 7=soil zone;
      ! 9=snowcomp; 13=cascade; 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug')/=0 ) CALL read_error(5, 'print_debug')

      IF ( control_string(Model_mode, 'model_mode')/=0 ) CALL read_error(5, 'model_mode')
      IF ( Model_mode(:4)=='PRMS' .OR. Model_mode(:4)=='    ' .OR. Model_mode(:5)=='DAILY' ) THEN
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
        PRINT '(/,2A)', 'ERROR, invalid model_mode value: ', Model_mode
        STOP
      ENDIF

      ! Open PRMS module output file
      IF ( control_string(Model_output_file, 'model_output_file')/=0 ) CALL read_error(5, 'prms.out')
      CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
      IF ( iret/=0 ) STOP
      IF ( Print_debug>-1 ) THEN
        PRINT '(/, 2A)', 'Model output written to file: ', Model_output_file(:numchars(Model_output_file))
        PRINT *, EQULS
      ENDIF

      ! Check for restart files
      IF ( control_integer(Init_vars_from_file, 'init_vars_from_file')/=0 ) Init_vars_from_file = 0
      IF ( Init_vars_from_file==1 ) THEN
        IF ( control_string(Var_init_file, 'var_init_file')/=0 ) CALL read_error(5, 'restart.in')
        CALL PRMS_open_input_file(Restart_inunit, Var_init_file, 'var_init_file', 1, iret)
        IF ( iret/=0 ) STOP
      ENDIF
      IF ( control_integer(Save_vars_to_file, 'save_vars_to_file')/=0 ) Save_vars_to_file = 0
      IF ( Save_vars_to_file==1 ) THEN
        IF ( control_string(Var_save_file, 'var_save_file')/=0 ) CALL read_error(5, 'restart.out')
      ENDIF

      IF ( control_string(Temp_module, 'temp_module')/=0 ) CALL read_error(5, 'temp_module')
      IF ( control_string(Precip_module, 'precip_module')/=0 ) CALL read_error(5, 'precip_module')
      Precip_combined_flag = 0
      Precip_flag = 1 ! precip_prms or precip_1sta
      IF ( Precip_module(:11)=='precip_laps' ) THEN
        Precip_flag = 2
      ELSEIF ( Precip_module(:12)=='precip_dist2' ) THEN
        Precip_flag = 3
      ELSEIF ( Precip_module(:8)=='ide_dist' ) THEN
        Precip_flag = 5
        IF ( Temp_module(:8)/='ide_dist' ) THEN
          PRINT '(/,A,/,2A)', 'ERROR, if ide_dist is specified for precip_module,', &
     &        'it also must be specified for temp_module: ', Temp_module
          Inputerror_flag = 1
        ENDIF
      ELSEIF ( Precip_module(:14)=='climate_hru_mo' ) THEN
        Precip_flag = 8
      ELSEIF ( Precip_module(:11)=='climate_hru' ) THEN
        Precip_flag = 7
      ELSEIF ( Precip_module(:8)=='xyz_dist' ) THEN
        Precip_flag = 6
        IF ( Temp_module(:8)/='xyz_dist' ) THEN
          PRINT '(/,A,/,2A)', 'ERROR, if xyz_dist is specified for precip_module,', &
     &                        'it also must be specified for temp_module: ', Temp_module
          Inputerror_flag = 1
        ENDIF
      ELSEIF ( Precip_module(:11)/='precip_1sta' .AND. Precip_module(:11)/='precip_prms' ) THEN
        PRINT '(/,2A)', 'ERROR: invalid precip_module value: ', Precip_module
        Inputerror_flag = 1
      ENDIF
      IF ( Precip_flag==1 .OR. Precip_flag==2 ) Precip_combined_flag = 1

      Temp_combined_flag = 0
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
          PRINT '(/,A,/,2A)', 'ERROR, if ide_dist is specified for temp_module,', &
     &          'it also must be specified for precip_module: ', Precip_module
          Inputerror_flag = 1
        ENDIF
      ELSEIF ( Temp_module(:14)=='climate_hru_mo' ) THEN
        Temp_flag = 8
      ELSEIF ( Temp_module(:11)=='climate_hru' ) THEN
        Temp_flag = 7
      ELSEIF ( Temp_module(:8)=='xyz_dist' ) THEN
        Temp_flag = 6
        IF ( Precip_module(:8)/='xyz_dist' ) THEN
          PRINT '(/,A,/,2A)', 'ERROR, if xyz_dist is specified for temp_module,', &
     &          'it also must be specified for precip_module: ', Precip_module
          Inputerror_flag = 1
        ENDIF
      ELSEIF ( Temp_module(:9)/='temp_1sta' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid temp_module value: ', Temp_module
        Inputerror_flag = 1
      ENDIF
      IF ( Temp_flag==1 .OR. Temp_flag==2 ) Temp_combined_flag = 1
      IF ( Temp_flag==7 .OR. Temp_flag==8 ) Temp_combined_flag = 2

      IF ( control_string(Transp_module, 'transp_module')/=0 ) CALL read_error(5, 'transp_module')
      Transp_flag = 1 ! transp_tindex
      IF ( Transp_module(:12)=='transp_frost' ) THEN
        Transp_flag = 2
      ELSEIF ( Transp_module(:14)=='climate_hru_mo' ) THEN
        Transp_flag = 4
        IF ( Temp_flag==7 .OR. Precip_flag==7 ) THEN
          PRINT *, 'precip_module or temp_module = climate_hru'
          PRINT *, 'but specified climate_hru_mo for transp_module'
          PRINT *, 'transp_module set to climate_hru'
          Transp_module = 'climate_hru         '
          Transp_flag = 3
        ENDIF
      ELSEIF ( Transp_module(:11)=='climate_hru' ) THEN
        Transp_flag = 3
        IF ( Temp_flag==8 .OR. Precip_flag==8 ) THEN
          PRINT *, 'precip_module or temp_module = climate_hru_mo'
          PRINT *, 'but specified climate_hru for transp_module'
          PRINT *, 'transp_module set to climate_hru_mo'
          Transp_module = 'climate_hru_mo      '
          Transp_flag = 4
        ENDIF
      ELSEIF ( Transp_module(:13)/='transp_tindex' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid transp_module value: ', Transp_module
        Inputerror_flag = 1
      ENDIF

      Hru_sum_flag = 0

      IF ( control_string(Et_module, 'et_module')/=0 ) CALL read_error(5, 'et_module')
      Et_combined_flag = 0
      Et_flag = 1 ! potet_jh
      IF ( Et_module(:15)=='potet_jh_hru_mo' ) THEN
        Et_flag = 9
      ELSEIF ( Et_module(:16)=='potet_hamon_prms' ) THEN  ! deprecated
        Et_flag = 3
        Hru_sum_flag = 1
      ELSEIF ( Et_module(:11)=='potet_hamon' ) THEN
        Et_flag = 2
      ELSEIF ( Et_module(:14)=='climate_hru_mo' ) THEN
        Et_flag = 8
        IF ( Temp_flag==7 .OR. Precip_flag==7 ) THEN
          PRINT *, 'precip_module or temp_module = climate_hru'
          PRINT *, 'but specified climate_hru_mo for et_module'
          PRINT *, 'et_module set to climate_hru'
          Et_module = 'climate_hru         '
        ENDIF
      ELSEIF ( Et_module(:11)=='climate_hru' ) THEN
        Et_flag = 7
        IF ( Temp_flag==8 .OR. Precip_flag==8 ) THEN
          PRINT *, 'precip_module or temp_module = climate_hru_mo'
          PRINT *, 'but specified climate_hru for et_module'
          PRINT *, 'et_module set to climate_hru_mo'
          Et_module = 'climate_hru_mo      '
        ENDIF
      ELSEIF ( Et_module(:8)=='potet_hs' ) THEN
        Et_flag = 10
      ELSEIF ( Et_module(:8)=='potet_pt' ) THEN
        Et_flag = 5
      ELSEIF ( Et_module(:9)=='potet_pan' ) THEN
        Et_flag = 4
      ELSEIF ( Et_module(:8)/='potet_jh' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid et_module value: ', Et_module
        Inputerror_flag = 1
      ENDIF
      IF ( Et_flag==1 .OR. Et_flag==9 ) Et_combined_flag = 1

      IF ( control_string(Srunoff_module, 'srunoff_module')/=0 ) CALL read_error(5, 'srunoff_module')
      Sroff_flag = 1 ! srunoff_smidx
      IF ( Srunoff_module(:13)=='srunoff_carea' ) THEN
        Sroff_flag = 2
      ELSEIF ( Srunoff_module(:13)/='srunoff_smidx' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid srunoff_module value: ', Srunoff_module
        Inputerror_flag = 1
      ENDIF

      IF ( control_string(Soilzone_module, 'soilzone_module')/=0) Soilzone_module = 'soilzone            '
      IF ( Soilzone_module(:5)/='smbal' .AND. Soilzone_module(:8)/='soilzone' ) THEN
        PRINT '(/,A,/A)', 'ERROR: invalid soilzone_module value: ', Soilzone_module
        Inputerror_flag = 1
      ENDIF
      IF ( control_string(Capillary_module, 'capillary_module')/=0 ) Capillary_module = 'null'
      IF ( Capillary_module(:5)=='smbal' ) Soilzone_module = 'smbal_prms          '
      IF ( Soilzone_module(:5)=='smbal' ) Soilzone_module = 'smbal_prms          '

      Soilzone_flag = 2 ! smbal and ssflow - deprecated
      IF ( Soilzone_module(:8)=='soilzone' ) Soilzone_flag = 1

      IF ( control_string(Solrad_module, 'solrad_module')/=0 ) CALL read_error(5, 'solrad_module')
      Orad_flag = 0
      Solrad_flag = 1 ! ddsolrad
      IF ( Solrad_module(:13)=='ddsolrad_prms' ) THEN ! deprecated
        Solrad_flag = 3
        Hru_sum_flag = 1
      ELSEIF ( Solrad_module(:13)=='ccsolrad_prms' ) THEN ! deprecated
        Solrad_flag = 4
        Hru_sum_flag = 1
      ELSEIF ( Solrad_module(:14)=='climate_hru_mo' ) THEN
        Solrad_flag = 8
        IF ( Precip_flag==7 .OR. Temp_flag==7 .OR. Et_flag==7 ) THEN
          PRINT *, 'climate_hru specified for other modules'
          PRINT *, 'but solrad_module specified as climate_hru_mo'
          PRINT *, 'solrad_module set to climate_hru'
          Solrad_flag = 7
          Solrad_module = 'climate_hru         '
        ENDIF
        IF ( control_integer(Orad_flag, 'orad_flag')/=0 ) Orad_flag = 0
      ELSEIF ( Solrad_module(:11)=='climate_hru' ) THEN
        Solrad_flag = 7
        IF ( Precip_flag==8 .OR. Temp_flag==8 .OR. Et_flag==8 ) THEN
          PRINT *, 'climate_hru_mo specified for other modules'
          PRINT *, 'but solrad_module specified as climate_hru;'
          PRINT *, 'solrad_module set to climate_hru_mo'
          Solrad_flag = 8
          Solrad_module = 'climate_hru_mo      '
          IF ( control_integer(Orad_flag, 'orad_flag')/=0 ) Orad_flag = 0
        ENDIF
        IF ( control_integer(Orad_flag, 'orad_flag')/=0 ) Orad_flag = 0
      ELSEIF ( Solrad_module(:8)=='ccsolrad' ) THEN
        Solrad_flag = 2
      ELSEIF ( Solrad_module(:8)/='ddsolrad' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid solrad_module value: ', Solrad_module
        Inputerror_flag = 1
      ENDIF

      Precip_climate_flag = 0
      IF ( Precip_flag==7 .AND. Temp_flag/=7 ) THEN
        Precip_climate_flag = 1
      ELSEIF ( Precip_flag==8 .AND. Temp_flag/=8 ) THEN
        Precip_climate_flag = 2
      ENDIF

      Solrad_climate_flag = 0
      IF ( Solrad_flag==7 .AND. Temp_flag/=7 .AND. Precip_flag/=7 ) THEN
        Solrad_climate_flag = 1
      ELSEIF ( Solrad_flag==8 .AND. Temp_flag/=8 .AND. Precip_flag/=8 ) THEN
        Solrad_climate_flag = 2
      ENDIF

      Transp_climate_flag = 0
      IF ( Transp_flag==3 .AND. Temp_flag/=7 .AND. Precip_flag/=7 .AND. Solrad_flag/=7 ) THEN
        Transp_climate_flag = 1
      ELSEIF ( Transp_flag==4 .AND. Temp_flag/=8 .OR. Precip_flag==8 .AND. Solrad_flag/=8 ) THEN
        Transp_climate_flag = 2
      ENDIF

      Et_climate_flag = 0
      IF ( Et_flag==7 .AND. Temp_flag/=7 .AND. Precip_flag/=7 .AND. Solrad_flag/=7 .AND. Transp_flag/=3 ) THEN
        Et_climate_flag = 1
      ELSEIF ( Et_flag==8 .AND. Temp_flag/=8 .AND. Precip_flag/=8 .AND. Solrad_flag/=8 .AND. Transp_flag/=4 ) THEN
        Et_climate_flag = 2
      ENDIF

      IF ( control_string(Strmflow_module, 'strmflow_module')/=0 ) CALL read_error(5, 'strmflow_module')
      IF ( Strmflow_module(:15)/='strmflow_in_out' .AND. Strmflow_module(:13)/='strmflow_lake' &
     &     .AND. Strmflow_module(:8)/='strmflow' .AND. Strmflow_module(:9)/='muskingum' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid strmflow_module value: ', Strmflow_module
        Inputerror_flag = 1
      ENDIF
      Strmflow_flag = 1 ! strmflow
      IF ( Strmflow_module(:15)=='strmflow_in_out' ) THEN
        Strmflow_flag = 5
      ELSEIF ( Strmflow_module(:13)=='strmflow_lake' ) THEN
        Strmflow_flag = 2
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
      IF ( decldim('ncascade', 0, MAXDIM, &
     &     'Number of HRU links for cascading flow')/=0 ) CALL read_error(7, 'ncascade')
      IF ( decldim('ncascdgw', 0, MAXDIM, &
     &     'Number of GWR links for cascading flow')/=0 ) CALL read_error(7, 'ncascdgw')

! nsegment dimension
      IF ( decldim('nsegment', 0, MAXDIM, 'Number of stream-channel segments')/=0 ) CALL read_error(7, 'nsegment')

      IF ( decldim('npoigages', 0, MAXDIM, 'Number of points-of-interest streamflow gages')/=0 ) &
     &     CALL read_error(7, 'npoigages')

! subbasin dimensions
      IF ( control_integer(Subbasin_flag, 'subbasin_flag')/=0 ) Subbasin_flag = 1
      IF ( decldim('nsub', 0, MAXDIM, 'Number of internal subbasins')/=0 ) CALL read_error(7, 'nsub')

      IF ( control_integer(Dprst_flag, 'dprst_flag')/=0 ) Dprst_flag = 0

      IF ( control_integer(CsvON_OFF, 'csvON_OFF')/=0 ) CsvON_OFF = 0

      IF ( control_integer(Parameter_check_flag, 'parameter_check_flag')/=0 ) Parameter_check_flag = 1

! map results dimensions
      IF ( control_integer(MapOutON_OFF, 'mapOutON_OFF')/=0 ) MapOutON_OFF = 0
      idim = 0
      IF ( Model==0 .OR. MapOutON_OFF==1 ) idim = 1
      IF ( decldim('nhrucell', idim, MAXDIM, &
     &     'Number of unique intersections between HRUs and mapped spatial units of a target map')/=0 ) &
     &     CALL read_error(7, 'nhrucell')
      IF ( decldim('ngwcell', idim, MAXDIM, &
     &     'Number of spatial units in the target grid for mapped results')/=0 ) CALL read_error(7, 'ngwcell')

! cascade
      IF ( control_integer(Cascade_flag, 'cascade_flag')/=0 ) Cascade_flag = 1
      IF ( control_integer(Cascadegw_flag, 'cascadegw_flag')/=0 ) Cascadegw_flag = 1

! spatial units
      IF ( decldim('ngw', 1, MAXDIM, 'Number of GWRs')/=0 ) CALL read_error(7, 'ngw')
      IF ( decldim('nhru', 1, MAXDIM, 'Number of HRUs')/=0 ) CALL read_error(7, 'nhru')
      IF ( decldim('nssr', 1, MAXDIM, 'Number of subsurface reservoirs')/=0 ) CALL read_error(7, 'nssr')
      IF ( decldim('nlake', 0, MAXDIM, 'Number of lake HRUs')/=0 ) CALL read_error(7, 'nlake')

! Time-series data stations, need to know if in Data File
      IF ( decldim('nrain', 0, MAXDIM, 'Number of precipitation measurement stations')/=0 ) CALL read_error(7, 'nrain')
      IF ( decldim('nsol', 0, MAXDIM, 'Number of solar-radiation measurement stations')/=0 ) CALL read_error(7, 'nsol')
      IF ( decldim('ntemp', 0, MAXDIM, 'Number of air-temperature measurement stations')/=0 ) CALL read_error(7, 'ntemp')
      IF ( decldim('nratetbl', 0, MAXDIM, 'Number of rating-table data sets for lake elevations') &
     &     /=0 ) CALL read_error(7, 'nratetbl')

! depletion curves
      IF ( decldim('ndepl', 1, MAXDIM, 'Number of snow-depletion curves')/=0 ) CALL read_error(7, 'ndelp')
      IF ( decldim('ndeplval', 11, MAXDIM, 'Number of values in all snow-depletion curves (set to ndepl*11)')/=0 ) &
     &     CALL read_error(7, 'ndelplval')

! fixed dimensions
      IF ( declfix('ndays', 366, 366, 'Maximum number of days in a year ')/=0 ) CALL read_error(7, 'ndays')
      IF ( declfix('nmonths', 12, 12, 'Number of months in a year')/=0 ) CALL read_error(7, 'nmonths')

      IF ( call_modules('setdims')/=0 ) STOP 'ERROR, in setdims'

      IF ( Inputerror_flag==1 ) THEN
        PRINT '(//,A,/,A)', '**FIX input errors in your Control File to continue**', &
     &        'NOTE: some errors may be due to use of defalut values'
        STOP
      ENDIF

      setdims = 0
      END FUNCTION setdims

!***********************************************************************
!     Get and check consistency of dimensions with flags
!***********************************************************************
      INTEGER FUNCTION check_dims()
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Nsub, Nsegment, Nlake, Nsegmentp1, Ndepl, Model, &
     &    Subbasin_flag, Nhrucell, Ntemp, Nrain, Nsol, Inputerror_flag, &
     &    Ncascade, Ncascdgw, Cascade_flag, Cascadegw_flag, Call_cascade, Nratetbl, &
     &    Soilzone_flag, Strmflow_flag, Dprst_flag
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getdim
!***********************************************************************

      Nhru = getdim('nhru')
      IF ( Nhru==-1 ) CALL read_error(7, 'nhru')

      Nssr = getdim('nssr')
      IF ( Nssr==-1 ) CALL read_error(7, 'nssr')

      Ngw = getdim('ngw')
      IF ( Ngw==-1 ) CALL read_error(7, 'ngw')

      IF ( Nhru==0 .OR. Nssr==0 .OR. Ngw==0 ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must be > 0: nhru=', Nhru, ', nssr=', Nssr, ', ngw=', Ngw
        Inputerror_flag = 1
      ENDIF

      IF ( Soilzone_flag==1 .AND. Nssr/=Nhru ) THEN
        PRINT *, 'ERROR, nssr must equal nhru to use the soilzone module: nssr=', Nssr, ' nhru=', Nhru
        Inputerror_flag = 1
      ENDIF

      Ntemp = getdim('ntemp')
      IF ( Ntemp==-1 ) CALL read_error(6, 'ntemp')

      Nrain = getdim('nrain')
      IF ( Nrain==-1 ) CALL read_error(6, 'nrain')

      Nsol = getdim('nsol')
      IF ( Nsol==-1 ) CALL read_error(6, 'nsol')

      Ncascade = getdim('ncascade')
      IF ( Ncascade==-1 ) CALL read_error(7, 'ncascade')
      Ncascdgw = getdim('ncascdgw')
      IF ( Ncascdgw==-1 ) CALL read_error(7, 'ncascdgw')
      IF ( Ncascade==0 ) Cascade_flag = 0
      IF ( Ncascdgw==0 ) Cascadegw_flag = 0
      IF ( Cascade_flag/=1 .AND. Cascadegw_flag/=1 .AND. Model/=99 ) THEN
        Call_cascade = 0
      ELSE
        Call_cascade = 1
      ENDIF

      Nlake = getdim('nlake')
      IF ( Nlake==-1 ) CALL read_error(7, 'nlake')

      Ndepl = getdim('ndepl')
      IF ( Ndepl==-1 ) CALL read_error(7, 'ndepl')

      Nsub = getdim('nsub')
      IF ( Nsub==-1 ) CALL read_error(7, 'nsub')
      IF ( Subbasin_flag==1 .AND. Nsub==0 ) Subbasin_flag = 0

      Nsegment = getdim('nsegment')
      IF ( Nsegment==-1 ) CALL read_error(7, 'nsegment')
      Nsegmentp1 = Nsegment + 1

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell==-1 ) CALL read_error(6, 'nhrucell')

      Nratetbl = getdim('nratetbl')
      IF ( Nratetbl==-1 ) CALL read_error(6, 'nratetbl')

      IF ( Nsub>0 .OR. Strmflow_flag>1 .OR. Dprst_flag==1 ) THEN
        IF ( Nhru/=Ngw .OR. Nhru/=Nssr ) THEN
          PRINT 9001, Nhru, Nssr, Ngw
          Inputerror_flag = 1
        ENDIF
      ENDIF

      IF ( Model==99 ) THEN
        IF ( Ntemp==0 ) Ntemp = 1
        IF ( Nrain==0 ) Nrain = 1
        IF ( Nlake==0 ) Nlake = 1
        IF ( Nsegment==0 ) Nsegment = 1
        IF ( Nsol==0 ) Nsol = 1
        IF ( Ncascade==0 ) Ncascade = 1
        IF ( Ncascdgw==0 ) Ncascdgw = 1
        IF ( Ndepl==0 ) Ndepl = 1
        IF ( Nsub==0 ) Nsub = 1
        IF ( Nhrucell==0 ) Nhrucell = 1
        IF ( Nratetbl==0 ) Nratetbl = 1
      ENDIF

 9001 FORMAT ('ERROR, nhru must equal nssr and ngw when using:', /, &
     &        'subbasins, cascades, surface depressions, muskingum, or strmflow_lake nhru=', &
     &        I8, ' nssr=', I8, ' ngw=', I8)

      check_dims = Inputerror_flag
      END FUNCTION check_dims

!**********************************************************************
!     Module documentation
!**********************************************************************
      SUBROUTINE module_doc()
      USE PRMS_MODULE, ONLY: Model, Precip_flag, Strmflow_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basin, climateflow
      INTEGER, EXTERNAL :: cascade, obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: transp_frost, frost_date
      INTEGER, EXTERNAL :: temp_1sta_laps, temp_dist2
      INTEGER, EXTERNAL :: precip_1sta_laps, climate_hru_combined
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon, potet_hs, potet_pt
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow
      INTEGER, EXTERNAL :: srunoff, soilzone
      INTEGER, EXTERNAL :: strmflow, subbasin, basin_sum, map_results, strmflow_in_out
      INTEGER, EXTERNAL :: write_climate_hru, muskingum, prms_summary, strmflow_lake
      INTEGER, EXTERNAL :: xyzsetdims, strmlksetdims
! Local variable
      INTEGER :: test
!**********************************************************************
      IF ( Model==99 ) THEN
        IF ( Precip_flag/=6 ) test = xyzsetdims()
        IF ( Strmflow_flag/=2 ) test = strmlksetdims()
        test = basin()
        test = climateflow()
        test = cascade()
        test = soltab()
        test = obs()
        test = temp_1sta_laps()
        test = temp_dist2()
        test = xyz_dist()
        test = ide_dist()
        test = climate_hru_combined()
        test = frost_date()
        test = transp_tindex()
        test = transp_frost()
        test = precip_1sta_laps()
        test = precip_dist2()
        test = ddsolrad()
        test = ccsolrad()
        test = potet_jh()
        test = potet_hamon()
        test = potet_hs()
        test = potet_pt()
        test = potet_pan()
        test = write_climate_hru()
        test = intcp()
        test = snowcomp()
        test = srunoff()
        test = soilzone()
        test = gwflow()
        test = strmflow()
        test = strmflow_lake()
        test = strmflow_in_out()
        test = muskingum()
        test = subbasin()
        test = basin_sum()
        test = prms_summary()
        test = map_results()
        PRINT 9001
      ENDIF

 9001 FORMAT (//, ' _name files were generated and written to files', /, &
     &        ' in the directory containing the control file.', /, &
     &        ' Ignore any messages that follow this one.', /, &
     &        ' Note, no simulation was computed.', /)

      END SUBROUTINE module_doc
