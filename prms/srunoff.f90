!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a
! non-linear (smidx) and linear (carea) variable-source-area method
! Combinded smidx and carea modules 3/12/2013
!
!     version: 2.2 added cascading flow for infiltration and runoff
! rsr, 10/30/2008 added depression storage code
! rsr, 04/11/2011 changed so dprst_area to be a parameter (does not change)
! rsr, 11/1/2008, routines imperv_et, check_capacity, imperv_sroff,
!                          dprst_comp in smidx only
!***********************************************************************
      MODULE PRMS_SRUNOFF
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: BALUNT
      CHARACTER(LEN=13), SAVE :: MODNAME
      REAL, SAVE, ALLOCATABLE :: Carea_dif(:)
      REAL, SAVE :: Srp, Sri
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sroff_down, Basin_sroff_upslope, Basin_sroffi, Basin_sroffp
      DOUBLE PRECISION, SAVE :: Basin_imperv_stor, Basin_imperv_evap, Basin_sroff, Basin_infil
      DOUBLE PRECISION, SAVE :: Basin_sroff_farflow
      DOUBLE PRECISION, SAVE :: Basin_hortonian, Basin_hortonian_lakes, Strm_farfield
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Strm_seg_in(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sroffp(:), Hru_sroffi(:), Upslope_hortonian(:), Imperv_evap(:), Hortonian_flow(:)
      REAL, SAVE, ALLOCATABLE :: Hru_impervevap(:), Hru_impervstor(:), Hortonian_lakes(:), Hru_hortonian_cascadeflow(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Smidx_coef(:), Smidx_exp(:)
      REAL, SAVE, ALLOCATABLE :: Carea_min(:), Carea_max(:)
!   Declared Parameters for Depression Storage
      REAL, SAVE, ALLOCATABLE :: Sro_to_dprst(:), Va_clos_exp(:), Va_open_exp(:), Dprst_flow_coef(:), Dprst_frac_init(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_open(:), Dprst_seep_rate_clos(:), Sro_to_dprst_imperv(:), Dprst_et_coef(:)
!   Declared Variables for Depression Storage
      DOUBLE PRECISION, SAVE :: Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep, Basin_dprst_wb
      DOUBLE PRECISION, SAVE :: Basin_dprst_volop, Basin_dprst_volcl
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open(:), Dprst_area_clos(:), Dprst_sroff_hru(:), Dprst_insroff_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_evap_hru(:), Dprst_seep_hru(:)
      END MODULE PRMS_SRUNOFF

!***********************************************************************
!     Main srunoff routine
!***********************************************************************
      INTEGER FUNCTION srunoff()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srunoffdecl, srunoffinit, srunoffrun
      EXTERNAL :: srunoff_restart
!***********************************************************************
      srunoff = 0

      IF ( Process(:3)=='run' ) THEN
        srunoff = srunoffrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        srunoff = srunoffdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL srunoff_restart(1)
        ELSE
          srunoff = srunoffinit()
        ENDIF
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL srunoff_restart(0)
      ENDIF

      END FUNCTION srunoff

!***********************************************************************
!     srunoffdecl - set up parameters for surface runoff computations
!   Declared Parameters
!     smidx_coef, smidx_exp, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max, soil_rechr_max, carea_min
!***********************************************************************
      INTEGER FUNCTION srunoffdecl()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Model, Dprst_flag, Nhru, Nsegment, Print_debug, Strmflow_flag, &
     &    Cascade_flag, Cascadegw_flag, Sroff_flag, Nlake
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, declparam
      EXTERNAL read_error, print_module, PRMS_open_module_file
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_srunoff
!***********************************************************************
      srunoffdecl = 0

      Version_srunoff = '$Id: srunoff.f90 5609 2013-04-23 18:48:02Z rsregan $'
      IF ( Sroff_flag==1 ) THEN
        Version_srunoff = '$Id: srunoff_smidx'//Version_srunoff(13:80)
        MODNAME = 'srunoff_smidx'
      ELSE
        Version_srunoff = '$Id: srunoff_carea'//Version_srunoff(13:80)
        MODNAME = 'srunoff_carea'
      ENDIF
      CALL print_module(Version_srunoff, 'Surface Runoff            ', 90)

      IF ( Print_debug==1 ) THEN
        CALL PRMS_open_module_file(BALUNT, 'srunoff.wbal')
        IF ( Cascade_flag==1 ) THEN
          WRITE ( BALUNT, 9002 )
        ELSE
          WRITE ( BALUNT, 9001 )
        ENDIF
      ENDIF

      IF ( declvar(MODNAME, 'basin_imperv_evap', 'one', 1, 'double', &
     &     'Basin area-weighted average evaporation from impervious area', &
     &     'inches', Basin_imperv_evap)/=0 ) CALL read_error(3, 'basin_imperv_evap')

      IF ( declvar(MODNAME, 'basin_imperv_stor', 'one', 1, 'double', &
     &     'Basin area-weighted average storage on impervious area', &
     &     'inches', Basin_imperv_stor)/=0 ) CALL read_error(3, 'basin_imperv_stor')

      IF ( declvar(MODNAME, 'basin_infil', 'one', 1, 'double', &
     &     'Basin area-weighted average infiltration to the capillary reservoirs', &
     &     'inches', Basin_infil)/=0 ) CALL read_error(3, 'basin_infil')

      IF ( declvar(MODNAME, 'basin_sroff', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff to the stream network', &
     &     'inches', Basin_sroff)/=0 ) CALL read_error(3, 'basin_sroff')

      IF ( declvar(MODNAME, 'basin_sroffi', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff from impervious areas', &
     &     'inches', Basin_sroffi)/=0 ) CALL read_error(3, 'basin_sroffi')

      IF ( declvar(MODNAME, 'basin_sroffp', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff from pervious areas', &
     &     'inches', Basin_sroffp)/=0 ) CALL read_error(3, 'basin_sroffp')

      IF ( declvar(MODNAME, 'basin_hortonian', 'one', 1, 'double', &
     &     'Basin area-weighted average Hortonian runoff', &
     &     'inches', Basin_hortonian)/=0 ) CALL read_error(3, 'basin_hortonian')

      ALLOCATE ( Hortonian_flow(Nhru) )
      IF ( declvar(MODNAME, 'hortonian_flow', 'nhru', Nhru, 'real', &
     &     'Hortonian surface runoff reaching stream network for each HRU', &
     &     'inches', Hortonian_flow)/=0 ) CALL read_error(3, 'hortonian_flow')

      ALLOCATE ( Imperv_evap(Nhru) )
      IF ( declvar(MODNAME, 'imperv_evap', 'nhru', Nhru, 'real', &
     &     'Evaporation from impervious area for each HRU', &
     &     'inches', Imperv_evap)/=0 ) CALL read_error(3, 'imperv_evap')

      ALLOCATE ( Hru_impervevap(Nhru) )
      IF ( declvar(MODNAME, 'hru_impervevap', 'nhru', Nhru, 'real', &
     &     'Evaporation from impervious area for each HRU', &
     &     'inches', Hru_impervevap)/=0 ) CALL read_error(3, 'hru_impervevap')

      ALLOCATE ( Hru_impervstor(Nhru) )
      IF ( declvar(MODNAME, 'hru_impervstor', 'nhru', Nhru, 'real', &
     &     'Storage on impervious area for each HRU', &
     &     'inches', Hru_impervstor)/=0 ) CALL read_error(3, 'hru_impervstor')

      ALLOCATE ( Hru_sroffp(Nhru) )
      IF ( declvar(MODNAME, 'hru_sroffp', 'nhru', Nhru, 'real', &
     &     'Surface runoff from pervious areas for each HRU', &
     &     'inches', Hru_sroffp)/=0 ) CALL read_error(3, 'hru_sroffp')

      ALLOCATE ( Hru_sroffi(Nhru) )
      IF ( declvar(MODNAME, 'hru_sroffi', 'nhru', Nhru, 'real', &
     &     'Surface runoff from impervious areas for each HRU', &
     &     'inches', Hru_sroffi)/=0 ) CALL read_error(3, 'hru_sroffi')

! Depression storage variables
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'basin_dprst_sroff', 'one', 1, 'double', &
     &       'Basin area-weighted average surface runoff from open surface depression storage', &
     &       'inches', Basin_dprst_sroff)/=0 ) CALL read_error(3, 'basin_dprst_sroff')
        IF ( declvar(MODNAME, 'basin_dprst_evap', 'one', 1, 'double', &
     &       'Basin area-weighted average evaporation from surface depression storage', &
     &       'inches', Basin_dprst_evap)/=0 ) CALL read_error(3, 'basin_dprst_evap')
        IF ( declvar(MODNAME, 'basin_dprst_seep', 'one', 1, 'double', &
     &       'Basin area-weighted average seepage from surface depression storage', &
     &       'inches', Basin_dprst_seep)/=0 ) CALL read_error(3, 'basin_dprst_seep')
        IF ( declvar(MODNAME, 'basin_dprst_volop', 'one', 1, 'double', &
     &       'Basin area-weighted average storage volume in open surface depressions', &
     &       'inches', Basin_dprst_volop)/=0 ) CALL read_error(3, 'basin_dprst_volop')
        IF ( declvar(MODNAME, 'basin_dprst_volcl', 'one', 1, 'double', &
     &       'Basin area-weighted average storage volume in closed surface depressions', &
     &       'inches', Basin_dprst_volcl)/=0 ) CALL read_error(3, 'basin_dprst_volcl')
        ALLOCATE ( Dprst_sroff_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_sroff_hru', 'nhru', Nhru, 'real', &
     &       'Surface runoff from open surface depression for each HRU', &
     &       'inches', Dprst_sroff_hru)/=0 ) CALL read_error(3, 'dprst_sroff_hru')
        ALLOCATE ( Dprst_insroff_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_insroff_hru', 'nhru', Nhru, 'real', &
     &       'Surface runoff from pervious and impervious portions into open and closed surface depressions for each HRU', &
     &       'inches', Dprst_insroff_hru)/=0 ) CALL read_error(3, 'dprst_insroff_hru')
        ALLOCATE ( Dprst_area_open(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_open', 'nhru', Nhru, 'real', &
     &       'Surface area of open surface depressions based on storage volume for each HRU', &
     &       'acres', Dprst_area_open)/=0 ) CALL read_error(3, 'dprst_area_open')
        ALLOCATE ( Dprst_area_clos(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_clos', 'nhru', Nhru, 'real', &
     &       'Surface area of closed surface depressions based on storage volume for each HRU', &
     &       'acres', Dprst_area_clos)/=0 ) CALL read_error(3, 'dprst_area_clos')
        ALLOCATE ( Dprst_seep_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_seep_hru', 'nhru', Nhru, 'real', &
     &       'Seepage from surface depression storage to associated GWR for each HRU', &
     &       'inches', Dprst_seep_hru)/=0 ) CALL read_error(3, 'dprst_seep_hru')
        ALLOCATE ( Dprst_evap_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_evap_hru', 'nhru', Nhru, 'real', &
     &       'Evaporation from surface depression storage for each HRU', &
     &       'inches', Dprst_evap_hru)/=0 ) CALL read_error(3, 'dprst_evap_hru')
        IF ( declvar(MODNAME, 'basin_dprst_wb', 'one', 1, 'double', &
     &       'Basin area-weighted average capillary reservoir storage', &
     &       'inches', Basin_dprst_wb)/=0 ) CALL read_error(3, 'basin_dprst_wb')
      ENDIF

! cascading variables and parameters
      ALLOCATE ( Upslope_hortonian(Nhru) ) ! needs to be allocated for all simulations
      IF ( Cascade_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'basin_sroff_down', 'one', 1, 'double', &
     &       'Basin area-weighted average of cascading surface runoff', &
     &       'inches', Basin_sroff_down)/=0 ) CALL read_error(3, 'basin_sroff_down')
        IF ( declvar(MODNAME, 'basin_sroff_upslope', 'one', 1, 'double', &
     &       'Basin area-weighted average of cascading surface runoff received from upslope HRUs', &
     &       'inches', Basin_sroff_upslope)/=0 ) CALL read_error(3, 'basin_sroff_upslope')
        IF ( declvar(MODNAME, 'basin_sroff_farflow', 'one', 1, 'double', &
     &       'Basin area-weighted average cascading surface runoff to farfield', &
     &       'inches', Basin_sroff_farflow)/=0 ) CALL read_error(3, 'basin_sroff_farflow')
        IF ( declvar(MODNAME, 'upslope_hortonian', 'nhru', Nhru, 'real', &
     &       'Hortonian surface runoff received from upslope HRUs', &
     &       'inches', Upslope_hortonian)/=0 ) CALL read_error(3, 'upslope_hortonian')
        ALLOCATE ( Hru_hortonian_cascadeflow(Nhru) )
        IF ( declvar(MODNAME, 'hru_hortonian_cascadeflow', 'nhru', Nhru, 'real', &
     &       'Cascading Hortonian surface runoff leaving each HRU', &
     &       'inches', Hru_hortonian_cascadeflow)/=0 ) CALL read_error(3, 'hru_hortonian_cascadeflow')
        IF ( Nlake>0 ) THEN
          IF ( declvar(MODNAME, 'basin_hortonian_lakes', 'one', 1, 'double', &
     &         'Basin area-weighted average Hortonian surface runoff to lakes', &
     &         'inches', Basin_hortonian_lakes)/=0 ) CALL read_error(3, 'basin_hortonian_lakes')
          ALLOCATE ( Hortonian_lakes(Nhru) )
          IF ( declvar(MODNAME, 'hortonian_lakes', 'nhru', Nhru, 'real', &
     &         'Surface runoff to lakes for each HRU', &
     &         'inches', Hortonian_lakes)/=0 ) CALL read_error(3, 'hortonian_lakes')
        ENDIF
      ENDIF

      IF ( Nsegment>0 ) THEN
        ALLOCATE ( Strm_seg_in(Nsegment) )
        IF ( declvar(MODNAME, 'strm_seg_in', 'nsegment', Nsegment, 'double', &
     &       'Flow in stream segments as a result of cascading flow', &
     &       'cfs', Strm_seg_in)/=0 ) CALL read_error(3,'strm_seg_in')
      ENDIF

      IF ( Cascade_flag==1 .OR. Cascadegw_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'strm_farfield', 'one', 1, 'double', &
     &       'Flow out of basin as far-field flow', &
     &       'cfs', Strm_farfield)/=0 ) CALL read_error(3, 'strm_farfield')
      ENDIF

! Allocate arrays for local variables and variables from other modules
      ALLOCATE ( Carea_max(Nhru) )
      IF ( Sroff_flag==1 ) THEN
        ALLOCATE ( Smidx_coef(Nhru), Smidx_exp(Nhru) )
      ELSE
        ALLOCATE ( Carea_min(Nhru), Carea_dif(Nhru) )
      ENDIF
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Dprst_flow_coef(Nhru), Sro_to_dprst(Nhru), Va_open_exp(Nhru), Va_clos_exp(Nhru) )
        ALLOCATE ( Dprst_seep_rate_open(Nhru), Dprst_seep_rate_clos(Nhru) )
        ALLOCATE ( Sro_to_dprst_imperv(Nhru), Dprst_et_coef(Nhru) )
      ENDIF

      IF ( Timestep/=0 ) RETURN

! Declare parameters
      IF ( Sroff_flag==1 ) THEN
        IF ( declparam(MODNAME, 'smidx_coef', 'nhru', 'real', &
     &       '0.01', '0.0001', '1.0', &
     &       'Coefficient in contributing area computations', &
     &       'Coefficient in non-linear contributing area algorithm for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'smidx_coef')
        IF ( declparam(MODNAME, 'smidx_exp', 'nhru', 'real', &
     &       '0.3', '0.2', '0.8', &
     &       'Exponent in contributing area computations', &
     &       'Exponent in non-linear contributing area algorithm for each HRU', &
     &       '1.0/inch')/=0 ) CALL read_error(1, 'smidx_exp')
      ELSE
        IF ( declparam(MODNAME, 'carea_min', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Minimum contributing area', &
     &       'Minimum possible area contributing to surface runoff expressed as a portion of the area for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'carea_min')
      ENDIF

      IF ( declparam(MODNAME, 'carea_max', 'nhru', 'real', &
     &     '0.6', '0.0', '1.0', &
     &     'Maximum contributing area', &
     &     'Maximum possible area contributing to surface runoff expressed as a portion of the HRU area', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'carea_max')

! Depression Storage parameters:
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'dprst_flow_coef', 'nhru', 'real', &
     &       '0.05', '0.0001', '1.0', &
     &       'Coefficient in linear flow routing equation for open surface depressions', &
     &       'Coefficient in linear flow routing equation for open surface depressions for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_flow_coef')
        IF ( declparam(MODNAME, 'dprst_seep_rate_open', 'nhru', 'real', &
     &       '0.02', '0.0001', '1.0', &
     &       'Coefficient used in linear seepage flow equation for open surface depressions', &
     &       'Coefficient used in linear seepage flow equation for open surface depressions for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_seep_rate_open')
        IF ( declparam(MODNAME, 'dprst_seep_rate_clos', 'nhru', 'real', &
     &       '0.02', '0.0001', '1.0', &
     &       'Coefficient used in linear seepage flow equation for closed surface depressions', &
     &       'Coefficient used in linear seepage flow equation for closed surface depressions for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_seep_rate_clos')
        IF ( declparam(MODNAME, 'sro_to_dprst', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Fraction of pervious surface runoff that flows into surface depression storage', &
     &       'Fraction of pervious surface runoff that'// &
     &       ' flows into surface depression storage; the remainder flows to a stream network for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst')
        IF ( declparam(MODNAME, 'sro_to_dprst_imperv', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Fraction of impervious surface runoff that flows into surface depression storage', &
     &       'Fraction of impervious surface runoff that'// &
     &       ' flows into surface depression storage; the remainder flows to a stream network for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst_imperv')
        IF ( declparam(MODNAME, 'dprst_et_coef', 'nhru', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_et_coef')
        ALLOCATE ( Dprst_frac_init(Nhru) )
        IF ( declparam(MODNAME, 'dprst_frac_init', 'nhru', 'real', &
     &       '0.5', '0.0', '1.0', &
     &       'Fraction of maximum storage that contains water at the start of a simulation', &
     &       'Fraction of maximum surface depression storage that contains water at the start of a simulation', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_init')
        IF ( declparam(MODNAME, 'va_open_exp', 'nhru', 'real', &
     &       '1.0', '0.0001', '10.0', &
     &       'Coefficient in the exponential equation to compute current surface area of open surface depressions', &
     &       'Coefficient in the exponential equation relating'// &
     &       ' maximum surface area to the fraction that open depressions are full to compute current surface area for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'va_open_exp')
        IF ( declparam(MODNAME, 'va_clos_exp', 'nhru', 'real', &
     &       '1.0', '0.0001', '10.0', &
     &       'Coefficient in the exponential equation to computecurrent surface area of closed surface depressions', &
     &       'Coefficient in the exponential equation relating'// &
     &       ' maximum surface area to the fraction that closed depressions are full to compute current surface area'// &
     &       ' for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'va_clos_exp')
      ENDIF

 9001 FORMAT ('    Date     Water Bal     Robal      Sroff   Infiltrat Impervevap Impervstor Dprst_evap Dprst_seep')
 9002 FORMAT ('    Date     Water Bal     Robal      Sroff   Sroffdown  Srofflake Infiltrat  Impervevap Impervstor    Farflow', &
     &        ' Dprst_evap Dprst_seep Dprst_srof')

      END FUNCTION srunoffdecl

!***********************************************************************
!     srunoffinit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srunoffinit()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Dprst_flag, Nhru, Nsegment, Nlake, Cascade_flag, Sroff_flag
      USE PRMS_BASIN, ONLY: Dprst_area_max, Dprst_area_clos_max, Dprst_area_open_max, Basin_area_inv, &
     &    Hru_area, Active_hrus, Hru_route_order, Op_flow_thres, Dprst_depth_avg
      USE PRMS_FLOWVARS, ONLY: Dprst_vol_open_max, Dprst_vol_clos_max, Dprst_vol_open, Dprst_vol_clos, Dprst_vol_thres_open
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error, PRMS_open_module_file
! Local Variables
      INTEGER :: i, j
!***********************************************************************
      srunoffinit = 0

      Basin_infil = 0.0D0
      Basin_sroff = 0.0D0
      Basin_imperv_evap = 0.0D0
      Basin_imperv_stor = 0.0D0
      Basin_sroffi = 0.0D0
      Basin_sroffp = 0.0D0
      Basin_hortonian = 0.0D0
      Hortonian_flow = 0.0
      Imperv_evap = 0.0
      Hru_impervevap = 0.0
      Hru_impervstor = 0.0
      Hru_sroffp = 0.0
      Hru_sroffi = 0.0
      Upslope_hortonian = 0.0
      IF ( Nsegment>0 ) Strm_seg_in = 0.0D0

      IF ( Cascade_flag==1 ) THEN
        Basin_sroff_upslope = 0.0D0
        Basin_sroff_down = 0.0D0
        Basin_sroff_farflow = 0.0D0
        Strm_farfield = 0.0D0
        Hru_hortonian_cascadeflow = 0.0
        IF ( Nlake>0 ) THEN
          Basin_hortonian_lakes = 0.0D0
          Hortonian_lakes = 0.0
        ENDIF
      ENDIF

      IF ( getparam(MODNAME, 'carea_max', Nhru, 'real', Carea_max)/=0 ) CALL read_error(2, 'carea_max')
      IF ( Sroff_flag==1 ) THEN
! Smidx parameters
        IF ( getparam(MODNAME, 'smidx_coef', Nhru, 'real', Smidx_coef)/=0 ) CALL read_error(2, 'smidx_coef')
        IF ( getparam(MODNAME, 'smidx_exp', Nhru, 'real', Smidx_exp)/=0 ) CALL read_error(2, 'smidx_exp')
      ELSE
! Carea parameters
        IF ( getparam(MODNAME, 'carea_min', Nhru, 'real', Carea_min)/=0 ) CALL read_error(2, 'carea_min')
        DO i = 1, Nhru
          Carea_dif(i) = Carea_max(i) - Carea_min(i)
        ENDDO
      ENDIF

! Depression Storage parameters and variables:
      Basin_dprst_sroff = 0.0D0
      Basin_dprst_evap = 0.0D0
      Basin_dprst_seep = 0.0D0
      Basin_dprst_volop = 0.0D0
      Basin_dprst_volcl = 0.0D0
      IF ( Dprst_flag==1 ) THEN
        Basin_dprst_wb = 0.0D0
        Dprst_evap_hru = 0.0
        Dprst_seep_hru = 0.0
        Dprst_sroff_hru = 0.0
        Dprst_insroff_hru = 0.0
        IF ( getparam(MODNAME, 'dprst_flow_coef', Nhru, 'real', Dprst_flow_coef)/=0 ) CALL read_error(2, 'dprst_flow_coef')
        IF ( getparam(MODNAME, 'dprst_seep_rate_open', Nhru, 'real', Dprst_seep_rate_open)/=0 )  &
     &       CALL read_error(2, 'dprst_seep_rate_open')
        IF ( getparam(MODNAME, 'dprst_seep_rate_clos', Nhru, 'real', Dprst_seep_rate_clos)/=0 ) &
     &       CALL read_error(2, 'dprst_seep_rate_clos')
        IF ( getparam(MODNAME, 'dprst_frac_init', Nhru, 'real', Dprst_frac_init)/=0 ) CALL read_error(2, 'dprst_frac_init')
        IF ( getparam(MODNAME, 'va_open_exp', Nhru, 'real', Va_open_exp)/=0 ) CALL read_error(2, 'va_open_exp')
        IF ( getparam(MODNAME, 'va_clos_exp', Nhru, 'real', Va_clos_exp)/=0 ) CALL read_error(2, 'va_clos_exp')
        IF ( getparam(MODNAME, 'sro_to_dprst', Nhru, 'real', Sro_to_dprst)/=0 ) CALL read_error(2, 'sro_to_dprst')
        IF ( getparam(MODNAME, 'sro_to_dprst_imperv', Nhru, 'real', Sro_to_dprst_imperv)/=0 ) &
     &       CALL read_error(2, 'sro_to_dprst_imperv')
        IF ( getparam(MODNAME, 'dprst_et_coef', Nhru, 'real', Dprst_et_coef)/=0 ) CALL read_error(2, 'dprst_et_coef')

        Dprst_area_open = 0.0
        Dprst_area_clos = 0.0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          CALL dprst_init(i, Dprst_frac_init(i), Sro_to_dprst(i), Dprst_flow_coef(i), &
     &                    Dprst_seep_rate_open(i), Dprst_seep_rate_clos(i), Op_flow_thres(i), Dprst_area_max(i), &
     &                    Dprst_area_clos_max(i), Dprst_vol_clos_max(i), Dprst_depth_avg(i), &
     &                    Dprst_vol_open_max(i), Dprst_vol_open(i), Dprst_vol_clos(i), Dprst_vol_thres_open(i), &
     &                    Dprst_area_open(i), Dprst_area_clos(i), Dprst_area_open_max(i), Va_open_exp(i), Va_clos_exp(i), &
     &                    Sro_to_dprst_imperv(i) )
        ENDDO
        DEALLOCATE ( Dprst_frac_init )
        Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
        Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
!******************************************************************
      ENDIF

      END FUNCTION srunoffinit

!***********************************************************************
!     srunoffrun - Computes surface runoff using contributing area
!                       computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srunoffrun()
      USE PRMS_SRUNOFF
      USE PRMS_MODULE, ONLY: Dprst_flag, Nhru, Print_debug, Cascade_flag, Sroff_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, DNEARZERO, Hru_perv, Hru_imperv, Hru_frac_imperv, Hru_frac_perv, &
     &    NEARZERO, Dprst_area_max, Hru_area, Hru_type, Basin_area_inv, &
     &    Dprst_area_clos_max, Dprst_area_open_max, Dprst_frac_hru, Dprst_frac_open, Dprst_frac_clos
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Sroff, Infil, Imperv_stor_max, Snowinfil_max, Soil_moist, Imperv_stor, &
     &    Pkwater_equiv, Soil_rechr, Soil_rechr_max, &
     &    Dprst_vol_open_max, Dprst_vol_clos_max, Dprst_vol_open, Dprst_vol_clos, Dprst_vol_thres_open
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_INTCP, ONLY: Net_rain, Net_snow, Net_ppt, Hru_intcpevap
      USE PRMS_SNOW, ONLY: Pptmix_nopack, Snow_evap, Snowcov_area, Snowmelt
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_et, compute_infil_smidx, run_cascade_sroff, dprst_comp, perv_sroff_smidx
! Local Variables
      INTEGER :: i, k, dprst_chk
      REAL :: srunoff, robal, hru_sroff_down, farflow, avail_et, harea, himperv, hperv, last_stor, perv_frac, imperv_frac, dprst_in
      DOUBLE PRECISION :: last_dprst_stor, basin_robal, new_dprst_stor, runoff, dprst_hru_wb
!***********************************************************************
      srunoffrun = 0

      ! soil_moist and soil_rechr are used from the last time step

      Basin_sroffi = 0.0D0
      Basin_sroffp = 0.0D0
      basin_robal = 0.0D0
      Basin_sroff = 0.0D0
      Basin_infil = 0.0D0
      Basin_imperv_evap = 0.0D0
      Basin_imperv_stor = 0.0D0
      Basin_hortonian = 0.0D0

      IF ( Cascade_flag==1 ) THEN
        Basin_sroff_down = 0.0D0
        Basin_sroff_upslope = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Upslope_hortonian = 0.0
        Strm_seg_in = 0.0D0
        Strm_farfield = 0.0D0
        Basin_sroff_farflow = 0.0D0
      ENDIF

      IF ( Dprst_flag==1 ) THEN
        Basin_dprst_sroff = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
        Basin_dprst_wb = 0.0D0
      ENDIF

      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        harea = Hru_area(i)
        runoff = 0.0D0

        IF ( Hru_type(i)/=2 ) THEN
          Infil(i) = 0.0
          last_stor = Imperv_stor(i)
          hperv = Hru_perv(i)
          himperv = Hru_imperv(i)
          perv_frac = Hru_frac_perv(i)
          imperv_frac = Hru_frac_imperv(i)
          Srp = 0.0
          Sri = 0.0
          Hru_sroffp(i) = 0.0
          Hru_sroffi(i) = 0.0
          Imperv_evap(i) = 0.0
          Hru_impervevap(i) = 0.0

!******Compute runoff for pervious, impervious, and depression storage area
          IF ( Sroff_flag==1 ) THEN
            CALL compute_infil_smidx(Pptmix_nopack(i), Smidx_coef(i), Smidx_exp(i), Soil_moist(i), Soil_moist_max(i), &
     &           Carea_max(i), Net_rain(i), Net_ppt(i), himperv, Imperv_stor(i), Imperv_stor_max(i), Snowmelt(i), &
     &           Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i), Infil(i), Hru_type(i), Upslope_hortonian(i))
          ELSE
            CALL compute_infil_carea(Pptmix_nopack(i), Carea_min(i), Carea_dif(i), Soil_moist(i), Soil_moist_max(i), &
     &           Soil_rechr(i), Soil_rechr_max(i), Net_rain(i), Net_ppt(i), himperv, Imperv_stor(i), Imperv_stor_max(i), &
     &           Snowmelt(i), Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i), Infil(i), Hru_type(i), Upslope_hortonian(i))
          ENDIF

          avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)
          dprst_chk = 0
          IF ( Dprst_flag==1 ) THEN
            last_dprst_stor = 0.0D0
            IF ( Dprst_area_max(i)>NEARZERO ) THEN
              dprst_chk = 1
              ! last depression storage on whole HRU
              IF ( Dprst_vol_open(i)>DNEARZERO ) last_dprst_stor = Dprst_vol_open(i)/harea
              IF ( Dprst_vol_clos(i)>DNEARZERO ) last_dprst_stor = last_dprst_stor + Dprst_vol_clos(i)/harea
            ENDIF
          ENDIF
          dprst_in = 0.0
!         ******Compute the depression storage component
!         only call if total depression surface area for each HRU is > 0.0
          IF ( dprst_chk==1 ) THEN
            CALL dprst_comp(Dprst_vol_clos_max(i), Dprst_vol_clos(i), Dprst_area_clos_max(i), Dprst_area_clos(i), &
     &           Dprst_vol_open_max(i), Dprst_vol_open(i), Dprst_area_open_max(i), Dprst_area_open(i), &
     &           Dprst_flow_coef(i), Dprst_sroff_hru(i), Dprst_seep_rate_open(i), Dprst_seep_rate_clos(i), &
     &           Dprst_seep_hru(i), Va_open_exp(i), Va_clos_exp(i), Sro_to_dprst(i), Dprst_evap_hru(i), &
     &           Dprst_insroff_hru(i), Dprst_vol_thres_open(i), perv_frac, imperv_frac, &
     &           harea, avail_et, i, Dprst_frac_open(i), Dprst_frac_clos(i), Sro_to_dprst_imperv(i), Dprst_et_coef(i), dprst_in)
            Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open(i)
            Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos(i)
            avail_et = avail_et - Dprst_evap_hru(i)
            Basin_dprst_evap = Basin_dprst_evap + Dprst_evap_hru(i)*harea
            Basin_dprst_seep = Basin_dprst_seep + Dprst_seep_hru(i)*harea
            runoff = runoff + Dprst_sroff_hru(i)*harea
            Basin_dprst_sroff = Basin_dprst_sroff + Dprst_sroff_hru(i)*harea
          ENDIF
!         **********************************************************

!******Compute runoff for pervious and impervious area, and depression storage area
          runoff = runoff + Srp*hperv + Sri*himperv
          srunoff = runoff/harea

!******Compute HRU weighted average (to units of inches/dt)
          hru_sroff_down = 0.0
          farflow = 0.0

          IF ( Hru_type(i)==1 ) THEN
            IF ( Cascade_flag==1 ) THEN
              IF ( Ncascade_hru(i)>0 .AND. srunoff>0.0 ) &
     &             CALL run_cascade_sroff(i, Ncascade_hru(i), srunoff, hru_sroff_down, farflow)
              Hru_hortonian_cascadeflow(i) = hru_sroff_down + farflow
              Basin_sroff_upslope = Basin_sroff_upslope + Upslope_hortonian(i)*harea
              Basin_sroff_down = Basin_sroff_down + hru_sroff_down*harea
              Basin_sroff_farflow = Basin_sroff_farflow + farflow*harea
            ENDIF
            Basin_hortonian = Basin_hortonian + srunoff*harea
            Basin_sroff = Basin_sroff + srunoff*harea
          ENDIF

          Basin_infil = Basin_infil + Infil(i)*hperv

!******Compute evaporation from impervious area
          IF ( himperv>NEARZERO ) THEN
            IF ( Imperv_stor(i)>0.0 ) THEN
              CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i), Snowcov_area(i), imperv_frac, avail_et)
              Hru_impervevap(i) = Imperv_evap(i)*imperv_frac
              avail_et = avail_et - Hru_impervevap(i)
              IF ( avail_et<0.0 ) THEN
                 ! sanity check
!                IF ( avail_et<-1.0E-5 ) PRINT*, 'avail_et<0 in srunoff imperv', i, Nowmonth, Nowday, avail_et
                Hru_impervevap(i) = Hru_impervevap(i) + avail_et
                IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
                Imperv_evap(i) = Hru_impervevap(i)/imperv_frac
                Imperv_stor(i) = Imperv_stor(i) - avail_et/imperv_frac
              ENDIF
            ENDIF
            Basin_imperv_evap = Basin_imperv_evap + Hru_impervevap(i)*harea
            Hru_impervstor(i) = Imperv_stor(i)*imperv_frac
            Basin_imperv_stor = Basin_imperv_stor + Imperv_stor(i)*himperv
          ENDIF

          Hru_sroffp(i) = Srp*perv_frac
          Hru_sroffi(i) = Sri*imperv_frac
          Basin_sroffp = Basin_sroffp + Srp*hperv
          Basin_sroffi = Basin_sroffi + Sri*himperv

          IF ( Dprst_flag==1 ) THEN
            new_dprst_stor = 0.0D0
            dprst_hru_wb = 0.0
            IF ( Dprst_area_max(i)>NEARZERO ) THEN
              new_dprst_stor = (Dprst_vol_open(i) + Dprst_vol_clos(i))/harea
              dprst_hru_wb = last_dprst_stor - new_dprst_stor - Dprst_seep_hru(i) - Dprst_sroff_hru(i) &
     &                       - Dprst_evap_hru(i) + Dprst_insroff_hru(i) + dprst_in/harea
              Basin_dprst_wb = Basin_dprst_wb + dprst_hru_wb*harea
            ENDIF
          ENDIF

          IF ( Print_debug==1 ) THEN
            robal = Snowmelt(i) - srunoff &!includes dprst runoff, if any
     &              - Infil(i)*perv_frac - Hru_impervevap(i) + (last_stor-Imperv_stor(i))*imperv_frac
            IF ( Net_ppt(i)>0.0 ) THEN
              IF ( Pptmix_nopack(i)==1 ) THEN
                robal = robal + Net_rain(i)
              ELSEIF ( Snowmelt(i)<=0.0 .AND. Pkwater_equiv(i)<DNEARZERO) THEN
                IF ( Snow_evap(i)<NEARZERO ) THEN
                  robal = robal + Net_ppt(i)
                ELSEIF ( Net_snow(i)<NEARZERO ) THEN
                  robal = robal + Net_ppt(i)
                ENDIF
              ENDIF
            ENDIF
            IF ( Cascade_flag==1 ) robal = robal + Upslope_hortonian(i) - hru_sroff_down - farflow
            IF ( Dprst_flag==1 ) THEN
              IF ( Dprst_area_max(i)>NEARZERO ) &
     &             robal = robal - Dprst_seep_hru(i) - Dprst_evap_hru(i) &
     &                     + last_dprst_stor - new_dprst_stor
            ENDIF
            basin_robal = basin_robal + robal
            IF ( ABS(robal)>1.0D-4 ) THEN
              IF ( Dprst_flag==1 ) THEN
                WRITE ( BALUNT, * ) 'dprst', last_dprst_stor, new_dprst_stor, Dprst_seep_hru(i), Dprst_evap_hru(i), &
     &                  Dprst_sroff_hru(i), Snowmelt(i), Net_rain(i), Dprst_insroff_hru(i), Upslope_hortonian(i)
                WRITE ( BALUNT, * ) dprst_hru_wb, Dprst_vol_open(i), Dprst_vol_clos(i), &
     &                  (Dprst_vol_open(i)+Dprst_vol_clos(i))/harea, Dprst_area_max(i), Pkwater_equiv(i), &
     &                  Dprst_area_clos(i), Snowcov_area(i), dprst_in, dprst_in/harea, Srp, Sri, Sro_to_dprst(i)
                WRITE ( BALUNT, * ) robal, Net_rain(i), Net_ppt(i), Dprst_insroff_hru(i), &
     &                  Net_rain(i)*Dprst_frac_hru(i), Dprst_frac_hru(i), runoff, srunoff, Pptmix_nopack(i)
                WRITE ( BALUNT, * ) Infil(i), perv_frac, Hru_impervevap(i), last_stor, Imperv_stor(i), imperv_frac, &
     &                              Dprst_sroff_hru(i)
              ENDIF
              IF ( ABS(robal)>5.0D-4 ) THEN
                WRITE ( BALUNT, * ) 'possible HRU water balance ERROR'
              ELSE
                WRITE ( BALUNT, * ) 'HRU robal rounding issue'
              ENDIF
              IF ( Cascade_flag==1 ) THEN
                WRITE ( BALUNT, '(2I3,I6,18F10.6,I3)' ) Nowmonth, Nowday, i, robal, Snowmelt(i), &
     &                  Upslope_hortonian(i), last_stor, hru_sroff_down, Infil(i), srunoff, &
     &                  Imperv_stor(i), Imperv_evap(i), Net_ppt(i), Pkwater_equiv(i), Snow_evap(i), Net_snow(i), &
     &                  farflow, Net_rain(i), Srp, Sri, runoff, Pptmix_nopack(i)
              ELSE
                WRITE ( BALUNT,'(2I3,I4,16F10.7,I5)' ) Nowmonth, Nowday, i, robal, Snowmelt(i), last_stor, Infil(i), &
     &                  srunoff, Imperv_stor(i), Imperv_evap(i), Hru_impervevap(i), imperv_frac, Net_ppt(i), &
     &                  Pkwater_equiv(i), Snow_evap(i), Net_snow(i), Net_rain(i), Srp, Sri, Pptmix_nopack(i)
              ENDIF
            ENDIF
          ENDIF
          Sroff(i) = srunoff
          Hortonian_flow(i) = srunoff
        ELSE
! HRU is a lake
!     eventually add code for lake area less than hru_area
!     that includes soil_moist for fraction of hru_area that is dry bank
          ! Sanity check
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i)>0.0 ) &
     &         PRINT *, 'srunoff lake ERROR', Infil(i), Sroff(i), Imperv_stor(i), Imperv_evap(i), i
          IF ( Cascade_flag==1 ) THEN
            Hortonian_lakes(i) = Upslope_hortonian(i)
            Basin_hortonian_lakes = Basin_hortonian_lakes + Hortonian_lakes(i)*harea
          ENDIF
        ENDIF

      ENDDO

!******Compute basin weighted averages (to units of inches/dt)
      !rsr, should be land_area???
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_imperv_evap = Basin_imperv_evap*Basin_area_inv
      Basin_imperv_stor = Basin_imperv_stor*Basin_area_inv
      Basin_infil = Basin_infil*Basin_area_inv
      Basin_sroffp = Basin_sroffp*Basin_area_inv
      Basin_sroffi = Basin_sroffi*Basin_area_inv
      Basin_hortonian = Basin_hortonian*Basin_area_inv
      IF ( Cascade_flag==1 ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_farflow = Basin_sroff_farflow*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
      ENDIF

      IF ( Dprst_flag==1 ) THEN
        Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
        Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
        Basin_dprst_evap = Basin_dprst_evap*Basin_area_inv
        Basin_dprst_seep = Basin_dprst_seep*Basin_area_inv
        Basin_dprst_sroff = Basin_dprst_sroff*Basin_area_inv
        Basin_dprst_wb = Basin_dprst_wb*Basin_area_inv
      ENDIF

      IF ( Print_debug==1 ) THEN
        robal = Basin_sroff - Basin_sroffp - Basin_sroffi - Basin_dprst_sroff
        IF ( Cascade_flag==1 ) THEN
          robal = robal + Basin_sroff_down + Basin_sroff_farflow
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, basin_robal, robal, Basin_sroff, Basin_sroff_down, &
     &            Basin_hortonian_lakes, Basin_infil, Basin_imperv_evap, Basin_imperv_stor, Basin_sroff_farflow, &
     &            Basin_dprst_evap, Basin_dprst_seep, Basin_dprst_sroff
        ELSE
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, basin_robal, robal, Basin_sroff, Basin_infil, Basin_imperv_evap, &
     &            Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, Basin_sroffp, Basin_sroffi, Basin_dprst_seep, &
     &            Basin_dprst_sroff
        ENDIF
        IF ( ABS(basin_robal)>1.0D-3 ) THEN
          WRITE ( BALUNT, * ) 'possible basin water balance ERROR'
        ELSEIF ( ABS(basin_robal)>5.0D-4 ) THEN
          WRITE ( BALUNT, * ) 'basin_robal rounding issue'
        ENDIF
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), 12F11.4)

      END FUNCTION srunoffrun

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil_carea(Pptmix_nopack, Carea_min, Carea_dif, Soil_moist, Soil_moist_max, Soil_rechr, &
     &           Soil_rechr_max, Net_rain, Net_ppt, Hru_imperv, Imperv_stor, Imperv_stor_max, Snowmelt, Snowinfil_max, &
     &           Net_snow, Pkwater_equiv, Infil, Hru_type, Upslope_hortonian)
      USE PRMS_SRUNOFF, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_sroff, perv_imperv_comp_carea, check_capacity
! Arguments
      INTEGER, INTENT(IN) :: Pptmix_nopack, Hru_type
      REAL, INTENT(IN) :: Carea_min, Soil_rechr_max, Carea_dif, Soil_moist_max, Soil_moist, Net_rain, Net_ppt
      REAL, INTENT(IN) :: Imperv_stor_max, Hru_imperv, Snowmelt, Snowinfil_max, Net_snow, Soil_rechr, Upslope_hortonian
      DOUBLE PRECISION, INTENT(IN) :: Pkwater_equiv
      REAL, INTENT(INOUT) :: Imperv_stor, Infil
! Local Variables
      REAL :: ppti, pptp, snri
!***********************************************************************
! compute runoff from cascading Hortonian flow
      IF ( Upslope_hortonian>0.0 ) THEN
        pptp = Upslope_hortonian
        ppti = Upslope_hortonian
        CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv, Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max, &
     &       Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack==1 ) THEN
        pptp = Net_rain
        ppti = Net_rain
        CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv, Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max, &
     &       Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      ENDIF

!******If precipitation on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN
        IF ( Pkwater_equiv>DNEARZERO .OR. ABS(Net_ppt-Net_snow)<NEARZERO ) THEN
!******Pervious area computations
          Infil = Infil + Snowmelt
          IF ( Infil>0 .AND. Hru_type==1 ) CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max, Srp, Infil)
!******Impervious area computations
          IF ( Hru_imperv>NEARZERO ) THEN
            CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Snowmelt, snri, Hru_type)
            Sri = Sri + snri
          ENDIF
        ELSE
!******Snowmelt occurred and depleted the snowpack
          pptp = Snowmelt
          ppti = Snowmelt

          CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv, Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max, &
     &         Imperv_stor_max, Imperv_stor, Infil, Hru_type)
        ENDIF
!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.
      ELSEIF ( Pkwater_equiv<DNEARZERO ) THEN
!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.
        IF ( Net_snow<NEARZERO .AND. Net_rain>0.0 ) THEN
! no snow, some rain
          pptp = Net_rain
          ppti = Net_rain
          CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv, Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max, &
     &         Imperv_stor_max, Imperv_stor, Infil, Hru_type)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.
      ELSEIF ( Infil>0.0 .AND. Hru_type==1 ) THEN
        CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max, Srp, Infil)
      ENDIF

      END SUBROUTINE compute_infil_carea

!***********************************************************************
!***********************************************************************
      SUBROUTINE perv_imperv_comp_carea(Pptp, Ppti, Hru_imperv, Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max, &
     &           Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      USE PRMS_SRUNOFF, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      EXTERNAL perv_sroff_carea, imperv_sroff
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Pptp, Ppti, Carea_min, Soil_rechr, Soil_rechr_max, Imperv_stor_max, Carea_dif, Hru_imperv
      REAL, INTENT(INOUT) :: Infil, Imperv_stor
! Local Variables
      REAL :: inp, snrp, snri
!***********************************************************************
!******Pervious area computations
      IF ( Pptp>0.0 ) THEN
        CALL perv_sroff_carea(Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max, Pptp, inp, snrp, Hru_type)
        Infil = Infil + inp
        Srp = Srp + snrp
        IF ( Srp<0.0 ) Srp = 0.0
      ENDIF

!******Impervious area computations
      IF ( Ppti>0.0 .AND. Hru_imperv>NEARZERO ) THEN
        CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, snri, Hru_type)
        Sri = Sri + snri
      ENDIF

      END SUBROUTINE perv_imperv_comp_carea

!***********************************************************************
!      Subroutine to compute runoff from pervious area using
!      contributing area computations
!***********************************************************************

      SUBROUTINE perv_sroff_carea(Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max, Pptp, Infil, Srp, Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Carea_min, Soil_rechr, Soil_rechr_max, Pptp, Carea_dif
      REAL, INTENT(OUT) :: Infil, Srp
! Local Variables
      REAL :: ca_fraction
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        ca_fraction = Carea_min + Carea_dif*(Soil_rechr/Soil_rechr_max)
        IF ( ca_fraction>1.0 ) ca_fraction = 1.0
        Srp = ca_fraction*Pptp
        IF ( Srp<0.0 ) Srp = 0.0
        Infil = Pptp - Srp
      ELSE ! Hru_type=3
        Srp = 0.0
        Infil = Pptp
      ENDIF

      END SUBROUTINE perv_sroff_carea

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil_smidx(Pptmix_nopack, Smidx_coef, Smidx_exp, Soil_moist, Soil_moist_max, Carea_max, &
     &           Net_rain, Net_ppt, Hru_imperv, Imperv_stor, Imperv_stor_max, Snowmelt, Snowinfil_max, Net_snow, &
     &           Pkwater_equiv, Infil, Hru_type, Upslope_hortonian)
      USE PRMS_SRUNOFF, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_sroff, perv_imperv_comp_smidx, check_capacity
! Arguments
      INTEGER, INTENT(IN) :: Pptmix_nopack, Hru_type
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Carea_max, Soil_moist_max, Soil_moist, Net_rain, Net_ppt
      REAL, INTENT(IN) :: Imperv_stor_max, Hru_imperv, Snowmelt, Snowinfil_max, Net_snow, Upslope_hortonian
      DOUBLE PRECISION, INTENT(IN) :: Pkwater_equiv
      REAL, INTENT(INOUT) :: Imperv_stor, Infil
! Local Variables
      REAL :: ppti, pptp, ptc, snri
!***********************************************************************
! compute runoff from cascading Hortonian flow
      IF ( Upslope_hortonian>0.0 ) THEN
        ptc = Upslope_hortonian
        pptp = Upslope_hortonian
        ppti = Upslope_hortonian
        CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv, Smidx_coef, Smidx_exp, Soil_moist, Carea_max, &
     &       Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack==1 ) THEN
        ptc = Net_rain
        pptp = Net_rain
        ppti = Net_rain
        CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv, Smidx_coef, Smidx_exp, Soil_moist, Carea_max, &
     &       Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      ENDIF

!******If precipitation on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN
        IF ( Pkwater_equiv>DNEARZERO .OR. ABS(Net_ppt-Net_snow)<NEARZERO ) THEN
!******Pervious area computations
          Infil = Infil + Snowmelt
          IF ( Infil>0 .AND. Hru_type==1 ) CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max, Srp, Infil)
!******Impervious area computations
          IF ( Hru_imperv>NEARZERO ) THEN
            CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Snowmelt, snri, Hru_type)
            Sri = Sri + snri
          ENDIF
        ELSE
!******Snowmelt occurred and depleted the snowpack
          ptc = Net_ppt
          pptp = Snowmelt
          ppti = Snowmelt

          CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv, Smidx_coef, Smidx_exp, Soil_moist, Carea_max, &
     &         Imperv_stor_max, Imperv_stor, Infil, Hru_type)
        ENDIF
!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.
      ELSEIF ( Pkwater_equiv<DNEARZERO ) THEN
!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.
        IF ( Net_snow<NEARZERO .AND. Net_rain>0.0 ) THEN
! no snow, some rain
          ptc = Net_rain
          pptp = Net_rain
          ppti = Net_rain
          CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv, Smidx_coef, Smidx_exp, Soil_moist, Carea_max, &
     &         Imperv_stor_max, Imperv_stor, Infil, Hru_type)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.
      ELSEIF ( Infil>0.0 .AND. Hru_type==1 ) THEN
        CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max, Srp, Infil)
      ENDIF

      END SUBROUTINE compute_infil_smidx

!***********************************************************************
!***********************************************************************
      SUBROUTINE perv_imperv_comp_smidx(Ptc, Pptp, Ppti, Hru_imperv, Smidx_coef, Smidx_exp, Soil_moist, Carea_max, &
     &           Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      USE PRMS_SRUNOFF, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      EXTERNAL perv_sroff_smidx, imperv_sroff
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Ptc, Pptp, Ppti, Imperv_stor_max, Smidx_coef, Smidx_exp, Soil_moist, Carea_max, Hru_imperv
      REAL, INTENT(INOUT) :: Infil, Imperv_stor
! Local Variables
      REAL :: inp, snrp, snri
!***********************************************************************
!******Pervious area computations
      IF ( Pptp>0.0 ) THEN
        CALL perv_sroff_smidx(Smidx_coef, Smidx_exp, Soil_moist, Carea_max, Pptp, Ptc, inp, snrp, Hru_type)
        Infil = Infil + inp
        Srp = Srp + snrp
        IF ( Srp<0.0 ) Srp = 0.0
      ENDIF

!******Impervious area computations
      IF ( Ppti>0.0 .AND. Hru_imperv>NEARZERO ) THEN
        CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, snri, Hru_type)
        Sri = Sri + snri
      ENDIF

      END SUBROUTINE perv_imperv_comp_smidx

!***********************************************************************
!      Subroutine to compute runoff from pervious area using non-linear
!      contributing area computations
!***********************************************************************

      SUBROUTINE perv_sroff_smidx(Smidx_coef, Smidx_exp, Soil_moist, Carea_max, Pptp, Ptc, Infil, Srp, Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Soil_moist, Pptp, Ptc, Carea_max
      REAL, INTENT(OUT) :: Infil, Srp
! Local Variables
      REAL :: smidx, ca_fraction
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        smidx = Soil_moist + (0.5*Ptc)
        ca_fraction = Smidx_coef*10.0**(Smidx_exp*smidx)
        IF ( ca_fraction>Carea_max ) ca_fraction = Carea_max
        Srp = ca_fraction*Pptp
        IF ( Srp<0.0 ) Srp = 0.0
        Infil = Pptp - Srp
      ELSE ! Hru_type=3
        Srp = 0.0
        Infil = Pptp
      ENDIF

      END SUBROUTINE perv_sroff_smidx

!***********************************************************************
!      Subroutine to compute evaporation from impervious area at
!      potential ET rate up to available ET
!***********************************************************************
      SUBROUTINE imperv_et(Imperv_stor, Potet, Imperv_evap, Sca, Hru_frac_imperv, Avail_et)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Potet, Sca, Avail_et, Hru_frac_imperv
      REAL, INTENT(INOUT) :: Imperv_stor
      REAL, INTENT(OUT) :: Imperv_evap
!***********************************************************************
      IF ( Sca<1.0 ) THEN
        IF ( Potet<Imperv_stor ) THEN
          Imperv_evap = Potet*(1.0-Sca)
        ELSE
          Imperv_evap = Imperv_stor*(1.0-Sca)
        ENDIF
        IF ( Imperv_evap*Hru_frac_imperv>Avail_et ) Imperv_evap = Avail_et/Hru_frac_imperv
        Imperv_stor = Imperv_stor - Imperv_evap
      ELSE
        Imperv_evap = 0.0
      ENDIF
      !rsr, sanity check
      IF ( Imperv_stor<0.0 ) Imperv_stor = 0.0

      END SUBROUTINE imperv_et

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_sroff(Ihru, Ncascade_hru, Runoff, Hru_sroff_down, Farflow)
      USE PRMS_MODULE, ONLY: Nsegmentp1
      USE PRMS_SRUNOFF, ONLY: Strm_farfield, Upslope_hortonian, Strm_seg_in
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt, Cascade_area
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Ncascade_hru
      REAL, INTENT(INOUT) :: Runoff, Hru_sroff_down, Farflow
! Local Variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j>0 ) THEN
          Upslope_hortonian(j) = Upslope_hortonian(j) + Runoff*Hru_down_fracwt(k, Ihru)
          Hru_sroff_down = Hru_sroff_down + Runoff*Hru_down_frac(k,Ihru)

! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          IF ( j/=Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) + Runoff*Cfs_conv*Cascade_area(k, Ihru)
          ELSE
            Strm_farfield = Strm_farfield + Runoff*Cfs_conv*Cascade_area(k, Ihru)
            Farflow = Farflow + Runoff*Hru_down_frac(k, Ihru)
          ENDIF
        ENDIF
      ENDDO

! reset Sroff as it accumulates flow to streams
      Runoff = Runoff - Hru_sroff_down - Farflow
      IF ( Runoff<0.0 ) THEN
        IF ( Hru_sroff_down>ABS(Runoff) ) THEN
          Hru_sroff_down = Hru_sroff_down - Runoff
        ELSE
          DO k = 1, Ncascade_hru
            j = Hru_down(k, Ihru)
            IF ( Strm_seg_in(j)>ABS(Runoff) ) THEN
              Strm_seg_in(j) = Strm_seg_in(j) - Runoff
              EXIT
            ENDIF
          ENDDO
        ENDIF
        Runoff = 0.0
      ENDIF

      END SUBROUTINE run_cascade_sroff

!***********************************************************************
!      Subroutine to compute runoff from impervious area
!***********************************************************************
      SUBROUTINE imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, Sri, Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Imperv_stor_max, Ppti
      REAL, INTENT(INOUT) :: Imperv_stor
      REAL, INTENT(OUT) :: Sri
! Local Variables
      REAL :: avail_stor
!***********************************************************************
      IF ( Hru_type==1 ) THEN
        avail_stor = Imperv_stor_max - Imperv_stor
        IF ( Ppti>avail_stor ) THEN
          Imperv_stor = Imperv_stor_max
          Sri = Ppti - avail_stor
        ELSE
          Imperv_stor = Imperv_stor + Ppti
          Sri = 0.0
        ENDIF
      ELSE ! Hru_type=3
        Imperv_stor = Imperv_stor + Ppti
        Sri = 0.0
      ENDIF

      END SUBROUTINE imperv_sroff

!***********************************************************************
! fill soil to soil_moist_max, if more than capacity restrict
! infiltration by snowinfil_max, with excess added to runoff
!***********************************************************************
      SUBROUTINE check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max, Srp, Infil)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Soil_moist_max, Soil_moist, Snowinfil_max
      REAL, INTENT(INOUT) :: Infil, Srp
! Local Variables
      REAL :: capacity, excess
!***********************************************************************
      capacity = Soil_moist_max - Soil_moist
      excess = Infil - capacity
      IF ( excess>Snowinfil_max ) THEN
        Srp = Srp + excess - Snowinfil_max
        Infil = Snowinfil_max + capacity
      ENDIF

      END SUBROUTINE check_capacity

!***********************************************************************
! Initialize depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_init(Ihru, Dprst_frac_init, Sro_to_dprst, Dprst_flow_coef, Dprst_seep_rate_open, &
     &                      Dprst_seep_rate_clos, Op_flow_thres, Dprst_area_max, &
     &                      Dprst_area_clos_max, Dprst_vol_clos_max, Dprst_depth_avg, Dprst_vol_open_max, &
     &                      Dprst_vol_open, Dprst_vol_clos, Dprst_vol_thres_open, Dprst_area_open, Dprst_area_clos, &
     &                      Dprst_area_open_max, Va_open_exp, Va_clos_exp, Sro_to_dprst_imperv )
      USE PRMS_MODULE, ONLY: Parameter_check_flag, Inputerror_flag
      USE PRMS_BASIN, ONLY: DNEARZERO, NEARZERO
      USE PRMS_SRUNOFF, ONLY: Basin_dprst_volop, Basin_dprst_volcl
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(IN) :: Sro_to_dprst, Dprst_flow_coef, Dprst_seep_rate_open, Dprst_seep_rate_clos
      REAL, INTENT(IN) :: Op_flow_thres, Dprst_depth_avg, Va_open_exp, Va_clos_exp, Sro_to_dprst_imperv
      REAL, INTENT(INOUT) :: Dprst_frac_init, Dprst_area_open, Dprst_area_clos
      DOUBLE PRECISION, INTENT(IN) :: Dprst_area_max, Dprst_area_clos_max, Dprst_area_open_max
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_vol_clos_max, Dprst_vol_open_max, Dprst_vol_open, Dprst_vol_clos
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_vol_thres_open
! Functions
      INTRINSIC EXP, LOG
! Local Variables
      INTEGER :: ierr
      DOUBLE PRECISION :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r
!***********************************************************************
      ierr = 0
      IF ( Dprst_frac_init>1.0 .OR. Dprst_frac_init<0.0 ) THEN
        IF ( Parameter_check_flag==1 ) THEN
          PRINT *, 'ERROR, dprst_frac_init < 0.0 or > 1.0 for HRU:', Ihru, Dprst_frac_init
          ierr = 1
        ELSEIF ( Dprst_frac_init>1.0 ) THEN
          PRINT *, 'dprst_frac_init > 1.0, set to 1.0 for HRU:', Ihru
          Dprst_frac_init = 1.0
        ELSE
          PRINT *, 'dprst_frac_init < 0.0, set to 0.0 for HRU:', Ihru
          Dprst_frac_init = 0.0
        ENDIF
      ENDIF
      IF ( Sro_to_dprst>1.0 .OR. Sro_to_dprst<0.0 ) THEN
        PRINT *, 'ERROR, sro_to_dprst < 0.0 or > 1.0 for HRU:', Ihru, Sro_to_dprst
        ierr = 1
      ENDIF
      IF ( Sro_to_dprst_imperv>1.0 .OR. Sro_to_dprst_imperv<0.0 ) THEN
        PRINT *, 'ERROR, sro_to_dprst_imperv < 0.0 or > 1.0 for HRU:', Ihru, Sro_to_dprst_imperv
        ierr = 1
      ENDIF
      IF ( Dprst_flow_coef<0.0 ) THEN
        PRINT *, 'ERROR, dprst_flow_coef < 0.0 for HRU:', Ihru, Dprst_flow_coef
        ierr = 1
      ENDIF
      IF ( Dprst_seep_rate_open<0.0 ) THEN
        PRINT *, 'ERROR, dprst_seep_rate_open < 0.0 for HRU:', Ihru, Dprst_seep_rate_open
        ierr = 1
      ENDIF
      IF ( Dprst_seep_rate_clos<0.0 ) THEN
        PRINT *, 'ERROR, dprst_seep_rate_clos < 0.0 for HRU:', Ihru, Dprst_seep_rate_clos
        ierr = 1
      ENDIF
      IF ( ierr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF

      IF ( Dprst_area_max>DNEARZERO ) THEN
!       calculate open and closed volumes (acre-inches) of depression storage by HRU
!       Dprst_area_open_max is the maximum open depression area (acres) that can generate surface runoff:
        Dprst_vol_clos_max = Dprst_area_clos_max*Dprst_depth_avg
        Dprst_vol_open_max = Dprst_area_open_max*Dprst_depth_avg

!       calculate the intial open and closed depression storage volume:
        Dprst_vol_open = Dprst_frac_init*Dprst_vol_open_max
        Dprst_vol_clos = Dprst_frac_init*Dprst_vol_clos_max

!       threshold volume is calculated as the % of maximum open
!       depression storage above which flow occurs *  total open depression storage volume
        Dprst_vol_thres_open = Op_flow_thres*Dprst_vol_open_max

!       initial open and closed storage volume as fraction of total open and closed storage volume

!       Open depression surface area for each HRU:
        IF ( Dprst_vol_open>DNEARZERO ) THEN
          open_vol_r = Dprst_vol_open/Dprst_vol_open_max
          IF ( open_vol_r<DNEARZERO ) THEN
            frac_op_ar = 0.0D0
          ELSEIF ( open_vol_r>1.0D0 ) THEN
            frac_op_ar = 1.0D0
          ELSE
            frac_op_ar = EXP(Va_open_exp*LOG(open_vol_r))
          ENDIF
          Dprst_area_open = Dprst_area_open_max*frac_op_ar
          IF ( Dprst_area_open>Dprst_area_open_max ) Dprst_area_open = Dprst_area_open_max
          IF ( Dprst_area_open<NEARZERO ) Dprst_area_open = 0.0
        ENDIF

!       Closed depression surface area for each HRU:
        IF ( Dprst_vol_clos>DNEARZERO ) THEN
          clos_vol_r = Dprst_vol_clos/Dprst_vol_clos_max
          IF ( clos_vol_r<DNEARZERO ) THEN
            frac_cl_ar = 0.0D0
          ELSEIF ( clos_vol_r>1.0D0 ) THEN
            frac_cl_ar = 1.0D0
          ELSE
            frac_cl_ar = EXP(Va_clos_exp*LOG(clos_vol_r))
          ENDIF
          Dprst_area_clos = Dprst_area_clos_max*frac_cl_ar
          IF ( Dprst_area_clos>Dprst_area_clos_max ) Dprst_area_clos = Dprst_area_clos_max
          IF ( Dprst_area_clos<NEARZERO ) Dprst_area_clos = 0.0
        ENDIF

!       calculate the basin open and closed depression storage volumes
        Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open
        Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos
      ENDIF
      END SUBROUTINE dprst_init

!***********************************************************************
!     Compute depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_comp(Dprst_vol_clos_max, Dprst_vol_clos, Dprst_area_clos_max, Dprst_area_clos, &
     &           Dprst_vol_open_max, Dprst_vol_open, Dprst_area_open_max, Dprst_area_open, Dprst_flow_coef, &
     &           Dprst_sroff_hru, Dprst_seep_rate_open, Dprst_seep_rate_clos, Dprst_seep_hru, Va_open_exp, &
     &           Va_clos_exp, Sro_to_dprst, Dprst_evap_hru, Dprst_insroff_hru, &
     &           Dprst_vol_thres_open, Hru_frac_perv, Hru_frac_imperv, Hruarea, Avail_et, Ihru, &
     &           Dprst_frac_open, Dprst_frac_clos, Sro_to_dprst_imperv, Dprst_et_coef, Dprst_in)
      USE PRMS_SRUNOFF, ONLY: Srp, Sri
      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      USE PRMS_INTCP, ONLY: Net_rain, Net_snow
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Pkwater_equiv
      USE PRMS_SNOW, ONLY: Snowmelt, Pptmix_nopack, Snowcov_area
      IMPLICIT NONE
      INTRINSIC EXP, LOG, MAX, ABS
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(IN) :: Dprst_flow_coef, Va_open_exp, Va_clos_exp, Sro_to_dprst
      REAL, INTENT(IN) :: Dprst_seep_rate_open, Dprst_seep_rate_clos, Hruarea, Sro_to_dprst_imperv, Dprst_et_coef
      REAL, INTENT(IN) :: Hru_frac_perv, Hru_frac_imperv, Dprst_frac_open, Dprst_frac_clos
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_thres_open, Dprst_area_open_max, Dprst_area_clos_max
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_clos_max, Dprst_vol_open_max
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_vol_open, Dprst_vol_clos
      REAL, INTENT(INOUT) :: Avail_et, Dprst_in
      REAL, INTENT(OUT) :: Dprst_area_open, Dprst_area_clos, Dprst_evap_hru, Dprst_sroff_hru
      REAL, INTENT(OUT) :: Dprst_seep_hru, Dprst_insroff_hru
! Local Variables
      REAL :: inflow, dprst_avail_et
      DOUBLE PRECISION :: dprst_srp, dprst_sri, seep_open, seep_clos, tmp
      DOUBLE PRECISION :: dprst_srp_open, dprst_srp_clos, dprst_sri_open, dprst_sri_clos
      DOUBLE PRECISION :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r
      DOUBLE PRECISION :: dprst_evap_open, dprst_evap_clos, unsatisfied_et
!***********************************************************************
!     add the hortonian flow and snowmelt to the depression storage volumes:
      inflow = Snowmelt(Ihru)
      IF ( Pptmix_nopack(Ihru)==1 ) THEN
        inflow = inflow + Net_rain(Ihru)
      ELSEIF ( Snowmelt(Ihru)<NEARZERO ) THEN
        IF ( Net_snow(Ihru)<NEARZERO .AND. Net_rain(Ihru)>0.0 ) THEN
          IF ( Pkwater_equiv(Ihru)<DNEARZERO ) inflow = inflow + Net_rain(Ihru)
        ENDIF
      ENDIF
      Dprst_in = inflow*Dprst_area_open_max
      Dprst_vol_open = Dprst_vol_open + Dprst_in
      tmp = inflow*Dprst_area_clos_max
      Dprst_vol_clos = Dprst_vol_clos + tmp
      Dprst_in = Dprst_in + tmp

      ! add any pervious surface runoff fraction to depressions
      dprst_srp = 0.0D0
      dprst_sri = 0.0D0
      IF ( Srp>NEARZERO ) THEN
        dprst_srp = Srp*Hru_frac_perv*Sro_to_dprst*Hruarea
        dprst_srp_open = dprst_srp*Dprst_frac_open ! acre-inches
        dprst_srp_clos = dprst_srp*Dprst_frac_clos
        dprst_srp = (dprst_srp_open+dprst_srp_clos)/Hruarea
        Srp = Srp - dprst_srp/Hru_frac_perv
        IF ( Srp<0.0 ) THEN
          IF ( Srp<-NEARZERO ) PRINT *, 'dprst srp<0.0', Srp, dprst_srp
          ! may need to adjust dprst_srp and volumes
          Srp = 0.0
        ENDIF
        Dprst_vol_open = Dprst_vol_open + dprst_srp_open
        Dprst_vol_clos = Dprst_vol_clos + dprst_srp_clos
      ENDIF
      IF ( Sri>NEARZERO ) THEN
        dprst_sri = Sri*Hru_frac_imperv*Sro_to_dprst_imperv*Hruarea
        dprst_sri_open = dprst_sri*Dprst_frac_open
        dprst_sri_clos = dprst_sri*Dprst_frac_clos
        dprst_sri = (dprst_sri_open+dprst_sri_clos)/Hruarea
        Sri = Sri - dprst_sri/Hru_frac_imperv
        IF ( Sri<0.0 ) THEN
          IF ( Sri<-NEARZERO ) PRINT *, 'dprst sri<0.0', Sri, dprst_sri
          ! may need to adjust dprst_sri and volumes
          Sri = 0.0
        ENDIF
        Dprst_vol_open = Dprst_vol_open + dprst_sri_open
        Dprst_vol_clos = Dprst_vol_clos + dprst_sri_clos
      ENDIF
      Dprst_insroff_hru = dprst_srp + dprst_sri

!     Open depression surface area for each HRU:
      Dprst_area_open = 0.0
      open_vol_r = 0.0D0
      IF ( Dprst_vol_open>DNEARZERO ) THEN
        open_vol_r = Dprst_vol_open/Dprst_vol_open_max
        IF ( open_vol_r<DNEARZERO ) THEN
          frac_op_ar = 0.0D0
        ELSEIF ( open_vol_r>1.0D0 ) THEN
          frac_op_ar = 1.0D0
        ELSE
          frac_op_ar = EXP(Va_open_exp*LOG(open_vol_r))
        ENDIF
        Dprst_area_open = Dprst_area_open_max*frac_op_ar
        IF ( Dprst_area_open>Dprst_area_open_max ) Dprst_area_open = Dprst_area_open_max
        IF ( Dprst_area_open<NEARZERO ) Dprst_area_open = 0.0
      ENDIF

!     Closed depression surface area for each HRU:
      Dprst_area_clos = 0.0
      clos_vol_r = 0.0D0
      IF ( Dprst_vol_clos>DNEARZERO ) THEN
        clos_vol_r = Dprst_vol_clos/Dprst_vol_clos_max
        IF ( clos_vol_r<DNEARZERO ) THEN
          frac_cl_ar = 0.0D0
        ELSEIF ( clos_vol_r>1.0D0 ) THEN
          frac_cl_ar = 1.0D0
        ELSE
          frac_cl_ar = EXP(Va_clos_exp*LOG(clos_vol_r))
        ENDIF
        Dprst_area_clos = Dprst_area_clos_max*frac_cl_ar
        IF ( Dprst_area_clos>Dprst_area_clos_max ) Dprst_area_clos = Dprst_area_clos_max
        IF ( Dprst_area_clos<NEARZERO ) Dprst_area_clos = 0.0
      ENDIF

      ! evaporate water from depressions based on snowcov_area
      ! dprst_evap_open & dprst_evap_clos = inches-acres on the HRU
      unsatisfied_et = Avail_et
      dprst_avail_et = (Potet(Ihru)*(1.0-Snowcov_area(Ihru)))*Dprst_et_coef
      Dprst_evap_hru = 0.0
      IF ( dprst_avail_et>0.0D0 ) THEN
        dprst_evap_open = 0.0D0
        dprst_evap_clos = 0.0D0
        IF ( Dprst_area_open>DNEARZERO ) THEN
          dprst_evap_open = MIN(Dprst_area_open*dprst_avail_et, Dprst_vol_open)
          IF ( dprst_evap_open/Hruarea>unsatisfied_et ) THEN
            !IF ( Print_debug>-1 ) THEN
            !  PRINT *, 'Warning, open dprst evaporation > available ET, HRU:, ', Ihru, unsatisfied_et, dprst_evap_open*Dprst_frac_open
            !  PRINT *, 'Set to available ET, perhaps dprst_et_coef specified too large'
            !  PRINT *, 'Set print_debug to -1 to turn off message'
            !ENDIF
            dprst_evap_open = unsatisfied_et*Hruarea
          ENDIF
          IF ( dprst_evap_open>Dprst_vol_open ) print *, '>', dprst_evap_open, dprst_vol_open
          IF ( dprst_evap_open>Dprst_vol_open ) dprst_evap_open = Dprst_vol_open
          unsatisfied_et = unsatisfied_et - dprst_evap_open/Hruarea
          Dprst_vol_open = Dprst_vol_open - dprst_evap_open
        ENDIF
        IF ( Dprst_area_clos>DNEARZERO ) THEN
          dprst_evap_clos = MIN(Dprst_area_clos*dprst_avail_et, Dprst_vol_clos)
          IF ( dprst_evap_clos/Hruarea>unsatisfied_et ) THEN
            !IF ( Print_debug>-1 ) THEN
            !  PRINT *, 'Warning, closed dprst evaporation > available ET, HRU:, ', Ihru, unsatisfied_et, dprst_evap_clos*Dprst_frac_clos
            !  PRINT *, 'Set to available ET, perhaps dprst_et_coef specified too large'
            !  PRINT *, 'Set print_debug to -1 to turn off message'
            !ENDIF
            dprst_evap_clos = unsatisfied_et*Hruarea
          ENDIF
          IF ( dprst_evap_clos>Dprst_vol_clos ) dprst_evap_clos = Dprst_vol_clos
          Dprst_vol_clos = Dprst_vol_clos - dprst_evap_clos
        ENDIF
        Dprst_evap_hru = (dprst_evap_open + dprst_evap_clos)/Hruarea
      ENDIF

      ! compute seepage
      Dprst_seep_hru = 0.0
      Dprst_sroff_hru = 0.0
      IF ( Dprst_vol_open>DNEARZERO ) THEN
        seep_open = Dprst_vol_open*Dprst_seep_rate_open
        Dprst_vol_open = Dprst_vol_open - seep_open
        IF ( Dprst_vol_open<0.0D0 ) THEN
          seep_open = seep_open + Dprst_vol_open
          Dprst_vol_open = 0.0D0
        ENDIF
        Dprst_seep_hru = seep_open/Hruarea

      ! compute open surface runoff
        Dprst_sroff_hru = MAX(0.0, Dprst_vol_open-Dprst_vol_open_max)
        Dprst_sroff_hru = Dprst_sroff_hru + &
     &                    MAX(0.0, Dprst_vol_open-Dprst_sroff_hru-Dprst_vol_thres_open)*Dprst_flow_coef
        Dprst_vol_open = Dprst_vol_open - Dprst_sroff_hru
        Dprst_sroff_hru = Dprst_sroff_hru/Hruarea
      ENDIF

      IF ( Dprst_area_clos>NEARZERO ) THEN
        seep_clos = Dprst_vol_clos*Dprst_seep_rate_clos
        Dprst_vol_clos = Dprst_vol_clos - seep_clos
        IF ( Dprst_vol_clos<0.0D0 ) THEN
          seep_clos = seep_clos + Dprst_vol_clos
          Dprst_vol_clos = 0.0D0
        ENDIF
        Dprst_seep_hru = Dprst_seep_hru + seep_clos/Hruarea
      ENDIF

      ! sanity checks
      IF ( Dprst_vol_open<0.0 ) THEN
        IF ( Dprst_vol_open<-NEARZERO ) PRINT *, 'issue, dprst_vol_open<0.0', Dprst_vol_open
        Dprst_vol_open = 0.0D0
      ENDIF
      IF ( Dprst_vol_clos<0.0 ) THEN
        IF ( Dprst_vol_clos<-NEARZERO ) PRINT *, 'issue, dprst_vol_clos<0.0', Dprst_vol_clos
        Dprst_vol_clos = 0.0D0
      ENDIF
      
      END SUBROUTINE dprst_comp

!***********************************************************************
!     srunoff_restart - write or read srunoff restart file
!***********************************************************************
      SUBROUTINE srunoff_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Nsegment, Nlake, &
     &    Sroff_flag, Dprst_flag, Cascade_flag
      USE PRMS_SRUNOFF
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_sroff_down, Basin_sroff_upslope, Basin_sroffi, Basin_sroffp, &
     &                            Basin_imperv_stor, Basin_imperv_evap, Basin_sroff, Basin_infil, Basin_hortonian
        WRITE ( Restart_outunit ) Imperv_evap
        WRITE ( Restart_outunit ) Hru_impervevap
        WRITE ( Restart_outunit ) Hru_impervstor
        WRITE ( Restart_outunit ) Hru_sroffp
        WRITE ( Restart_outunit ) Hru_sroffi
        WRITE ( Restart_outunit ) Hortonian_flow
        WRITE ( Restart_outunit ) Carea_max
        IF ( Sroff_flag==1 ) THEN
          WRITE ( Restart_outunit ) Smidx_coef
          WRITE ( Restart_outunit ) Smidx_exp
        ELSE
          WRITE ( Restart_outunit ) Carea_min
          WRITE ( Restart_outunit ) Carea_dif
        ENDIF
        IF ( Nsegment>0 ) WRITE ( Restart_outunit ) Strm_seg_in
        IF ( Cascade_flag==1 ) THEN
          WRITE ( Restart_outunit ) Basin_sroff_farflow, Strm_farfield
          WRITE ( Restart_outunit ) Upslope_hortonian
          WRITE ( Restart_outunit ) Hru_hortonian_cascadeflow
          IF ( Nlake>0 ) WRITE ( Restart_outunit ) Hortonian_lakes
        ENDIF
        IF ( Dprst_flag==1 ) THEN
          WRITE ( Restart_outunit ) Basin_dprst_wb, Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep, &
      &                             Basin_dprst_volop, Basin_dprst_volcl
          WRITE ( Restart_outunit ) Sro_to_dprst
          WRITE ( Restart_outunit ) Sro_to_dprst_imperv
          WRITE ( Restart_outunit ) Va_clos_exp
          WRITE ( Restart_outunit ) Va_open_exp
          WRITE ( Restart_outunit ) Dprst_flow_coef
          WRITE ( Restart_outunit ) Dprst_seep_rate_open
          WRITE ( Restart_outunit ) Dprst_seep_rate_clos
          WRITE ( Restart_outunit ) Dprst_evap_hru
          WRITE ( Restart_outunit ) Dprst_seep_hru
          WRITE ( Restart_outunit ) Dprst_area_open
          WRITE ( Restart_outunit ) Dprst_area_clos
          WRITE ( Restart_outunit ) Dprst_sroff_hru
          WRITE ( Restart_outunit ) Dprst_insroff_hru
          WRITE ( Restart_outunit ) Dprst_et_coef
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_sroff_down, Basin_sroff_upslope, Basin_sroffi, Basin_sroffp, &
     &                          Basin_imperv_stor, Basin_imperv_evap, Basin_sroff, Basin_infil, Basin_hortonian
        READ ( Restart_inunit ) Imperv_evap
        READ ( Restart_inunit ) Hru_impervevap
        READ ( Restart_inunit ) Hru_impervstor
        READ ( Restart_inunit ) Hru_sroffp
        READ ( Restart_inunit ) Hru_sroffi
        READ ( Restart_inunit ) Hortonian_flow
        READ ( Restart_inunit ) Carea_max
        IF ( Sroff_flag==1 ) THEN
          READ ( Restart_inunit ) Smidx_coef
          READ ( Restart_inunit ) Smidx_exp
        ELSE
          READ ( Restart_inunit ) Carea_min
          READ ( Restart_inunit ) Carea_dif
        ENDIF
        IF ( Nsegment>0 ) READ ( Restart_inunit ) Strm_seg_in
        IF ( Cascade_flag==1 ) THEN
          READ ( Restart_inunit ) Basin_sroff_farflow, Strm_farfield
          READ ( Restart_inunit ) Upslope_hortonian
          READ ( Restart_inunit ) Hru_hortonian_cascadeflow
          IF ( Nlake>0 ) READ ( Restart_inunit ) Hortonian_lakes
        ENDIF
        IF ( Dprst_flag==1 ) THEN
          READ ( Restart_inunit ) Basin_dprst_wb, Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep, &
      &                           Basin_dprst_volop, Basin_dprst_volcl
          READ ( Restart_inunit ) Sro_to_dprst
          READ ( Restart_inunit ) Sro_to_dprst_imperv
          READ ( Restart_inunit ) Va_clos_exp
          READ ( Restart_inunit ) Va_open_exp
          READ ( Restart_inunit ) Dprst_flow_coef
          READ ( Restart_inunit ) Dprst_seep_rate_open
          READ ( Restart_inunit ) Dprst_seep_rate_clos
          READ ( Restart_inunit ) Dprst_evap_hru
          READ ( Restart_inunit ) Dprst_seep_hru
          READ ( Restart_inunit ) Dprst_area_open
          READ ( Restart_inunit ) Dprst_area_clos
          READ ( Restart_inunit ) Dprst_sroff_hru
          READ ( Restart_inunit ) Dprst_insroff_hru
          READ ( Restart_inunit ) Dprst_et_coef
        ENDIF
      ENDIF
      END SUBROUTINE srunoff_restart
