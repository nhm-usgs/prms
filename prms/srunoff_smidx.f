!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a
! non-linear variable-source-area method allowing for cascading flow
!
!     version: 2.2 added cascading flow for infiltration and runoff
! rsr, 10/30/2008 added depression storage code
! rsr, 04/11/2011 changed so dprst_area to be a parameter (does not change)
! rsr, 11/1/2008, routines imperv_et, check_capacity, imperv_sroff,
!                          dprst_comp in smidx only
!***********************************************************************
      MODULE PRMS_SRUNOFF_SMIDX
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 197
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_thres_open(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open_max(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_clos_max(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_clos_hru(:)
      REAL, SAVE :: Srp, Sri
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sroff_down, Basin_sroff_upslope
      DOUBLE PRECISION, SAVE :: Basin_sroffi, Basin_sroffp
      REAL, SAVE, ALLOCATABLE :: Imperv_stor(:), Imperv_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sroffp(:), Hru_sroffi(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_hortonian(:), Hortonian_flow(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Smidx_coef(:), Smidx_exp(:)
!   Declared Parameters for Depression Storage
      REAL, SAVE, ALLOCATABLE :: Op_flow_thres(:), Sro_to_dprst(:)
      REAL, SAVE, ALLOCATABLE :: Va_clos_exp(:), Va_open_exp(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_flow_coef(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_init(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_open(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_depth_avg(:)
!   Declared Variables for Depression Storage
      DOUBLE PRECISION, SAVE :: Basin_dprst_volop, Basin_dprst_volcl
      DOUBLE PRECISION, SAVE :: Basin_dprst_sroff, Basin_dprst_evap
      DOUBLE PRECISION, SAVE :: Basin_dprst_seep
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_evap_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open(:), Dprst_area_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_sroff_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_insroff_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_hru(:)
      END MODULE PRMS_SRUNOFF_SMIDX

!***********************************************************************
!     Main srunoff_smidx routine
!***********************************************************************
      INTEGER FUNCTION srunoff_smidx()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srunoffsmidxdecl, srunoffsmidxinit
      INTEGER, EXTERNAL :: srunoffsmidxrun
!***********************************************************************
      srunoff_smidx = 0

      IF ( Process(:3)=='run' ) THEN
        srunoff_smidx = srunoffsmidxrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        srunoff_smidx = srunoffsmidxdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        srunoff_smidx = srunoffsmidxinit()
      ENDIF

      END FUNCTION srunoff_smidx

!***********************************************************************
!     srunoffsmidxdecl - set up parameters for surface runoff computations
!   Declared Parameters
!     smidx_coef, smidx_exp, carea_max, imperv_stor_max, snowinfil_max
!     soil_moist_max
!***********************************************************************
      INTEGER FUNCTION srunoffsmidxdecl()
      USE PRMS_SRUNOFF_SMIDX
      USE PRMS_MODULE, ONLY: Model, Dprst_flag, Cascade_flag, Nhru,
     +    Print_debug, Version_srunoff_smidx, Srunoff_smidx_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declvar, declparam
      EXTERNAL read_error
!***********************************************************************
      srunoffsmidxdecl = 1

      Version_srunoff_smidx =
     +'$Id: srunoff_smidx.f 3817 2011-10-25 22:43:15Z rsregan $'
      Srunoff_smidx_nc = INDEX( Version_srunoff_smidx, ' $' ) + 1
      IF ( Print_debug>-1 ) THEN
        IF ( declmodule(Version_srunoff_smidx(:Srunoff_smidx_nc))/=0 )
     +       STOP
      ENDIF

! srunoff variables
      ALLOCATE ( Imperv_stor(Nhru) )
      IF ( declvar('srunoff', 'imperv_stor', 'nhru', Nhru, 'real',
     +     'Storage on impervious area for each HRU',
     +     'inches', Imperv_stor)/=0 )
     +     CALL read_error(3, 'imperv_stor')

      ALLOCATE ( Imperv_evap(Nhru) )
      IF ( declvar('srunoff', 'imperv_evap', 'nhru', Nhru, 'real',
     +     'Evaporation from impervious area for each HRU',
     +     'inches', Imperv_evap)/=0 ) CALL read_error(3, 'imperv_evap')

      IF ( declvar('srunoff', 'basin_sroffi', 'one', 1, 'double',
     +     'Basin area-weighted average surface runoff from'//
     +     ' impervious areas',
     +     'inches', Basin_sroffi)/=0 )
     +      CALL read_error(3, 'basin_sroffi')

      IF ( declvar('srunoff', 'basin_sroffp', 'one', 1, 'double',
     +     'Basin area-weighted average surface runoff from pervious'//
     +     ' areas',
     +     'inches', Basin_sroffp)/=0 )
     +     CALL read_error(3, 'basin_sroffp')

      ALLOCATE ( Hru_sroffp(Nhru) )
      IF ( declvar('srunoff', 'hru_sroffp', 'nhru', Nhru, 'real',
     +     'Surface runoff from pervious areas for each HRU',
     +     'inches', Hru_sroffp)/=0 )
     +     CALL read_error(3, 'hru_sroffp')

      ALLOCATE ( Hru_sroffi(Nhru) )
      IF ( declvar('srunoff', 'hru_sroffi', 'nhru', Nhru, 'real',
     +     'Surface runoff from impervious areas for each HRU',
     +     'inches', Hru_sroffi)/=0 )
     +     CALL read_error(3, 'hru_sroffi')

! Depression storage variables
      IF ( Dprst_flag>0 .OR. Model==99 ) THEN
        IF ( declvar('srunoff', 'basin_dprst_sroff', 'one', 1, 'double',
     +       'Basin area-weighted average surface runoff from'//
     +       ' open surface depression storage',
     +       'inches', Basin_dprst_sroff)/=0 )
     +       CALL read_error(3, 'basin_dprst_sroff')

        IF ( declvar('srunoff', 'basin_dprst_volop', 'one', 1, 'double',
     +       'Basin area-weighted average storage volume in open'//
     +       ' surface depressions',
     +       'inches', Basin_dprst_volop)/=0 )
     +       CALL read_error(3, 'basin_dprst_volop')

        IF ( declvar('srunoff', 'basin_dprst_volcl', 'one', 1, 'double',
     +      'Basin area-weighted average storage volume in closed'//
     +      ' surface depressions',
     +      'inches', Basin_dprst_volcl)/=0 )
     +      CALL read_error(3, 'basin_dprst_volcl')

        ALLOCATE ( Dprst_seep_hru(Nhru) )
        IF ( declvar('srunoff', 'dprst_seep_hru', 'nhru', Nhru, 'real',
     +       'Seepage from surface depression storage to associated'//
     +       ' GWR for each HRU',
     +       'inches',
     +       Dprst_seep_hru)/=0 ) CALL read_error(3, 'dprst_seep_hru')

        ALLOCATE ( Dprst_vol_open(Nhru) )
        IF ( declvar('srunoff', 'dprst_vol_open', 'nhru', Nhru,'double',
     +       'Storage volume in open surface depressions for each HRU',
     +       'acre-inches',
     +       Dprst_vol_open)/=0 ) CALL read_error(3, 'dprst_vol_open')

        ALLOCATE ( Dprst_vol_clos(Nhru) )
        IF ( declvar('srunoff', 'dprst_vol_clos', 'nhru', Nhru,'double',
     +      'Storage volume in closed surface depressions for each HRU',
     +      'acre-inches',
     +      Dprst_vol_clos)/=0 ) CALL read_error(3, 'dprst_vol_clos')

        ALLOCATE ( Dprst_evap_hru(Nhru) )
        IF ( declvar('srunoff', 'dprst_evap_hru', 'nhru', Nhru, 'real',
     +       'Evaporation from surface depression storage for each HRU',
     +       'inches', Dprst_evap_hru)/=0 )
     +       CALL read_error(3, 'dprst_evap_hru')

        ALLOCATE ( Dprst_sroff_hru(Nhru) )
        IF ( declvar('srunoff', 'dprst_sroff_hru', 'nhru', Nhru, 'real',
     +       'Surface runoff from open surface depression storage for'//
     +       ' each HRU',
     +       'inches', Dprst_sroff_hru)/=0 )
     +       CALL read_error(3, 'dprst_sroff_hru')

        ALLOCATE ( Dprst_insroff_hru(Nhru) )
        IF ( declvar('srunoff', 'dprst_insroff_hru', 'nhru', Nhru,
     +       'real',
     +       'Surface runoff from pervious and impervious portions'//
     +       ' into surface depression storage for each HRU',
     +       'inches', Dprst_insroff_hru)/=0 )
     +       CALL read_error(3, 'dprst_insroff_hru')

        IF ( declvar('srunoff', 'basin_dprst_evap', 'one', 1, 'double',
     +       'Basin area-weighted average evaporation from'//
     +       ' surface depression storage',
     +       'inches', Basin_dprst_evap)/=0 )
     +       CALL read_error(3, 'basin_dprst_evap')

        IF ( declvar('srunoff', 'basin_dprst_seep', 'one', 1, 'double',
     +       'Basin area-weighted average seepage from'//
     +       ' surface depression storage',
     +       'inches', Basin_dprst_seep)/=0 )
     +       CALL read_error(3, 'basin_dprst_seep')

        ALLOCATE ( Dprst_area_open(Nhru) )
        IF ( declvar('srunoff', 'dprst_area_open', 'nhru', Nhru, 'real',
     +       'Surface area of open surface depressions based on'//
     +       ' volume for each HRU',
     +       'acres', Dprst_area_open)/=0 )
     +       CALL read_error(3, 'dprst_area_open')

        ALLOCATE ( Dprst_area_clos(Nhru) )
        IF ( declvar('srunoff', 'dprst_area_clos', 'nhru', Nhru, 'real',
     +       'Surface area of closed surface depressions based on'//
     +       ' volume for each HRU', 'acres', Dprst_area_clos)/=0 )
     +       CALL read_error(3, 'dprst_area_clos')
      ENDIF

      ALLOCATE ( Hortonian_flow(Nhru) )
      IF ( declvar('srunoff', 'hortonian_flow', 'nhru', Nhru, 'real',
     +  'Hortonian surface runoff reaching stream network for each HRU',
     +     'inches', Hortonian_flow)/=0 )
     +     CALL read_error(3, 'hortonian_flow')

! cascading variables and parameters
      ALLOCATE ( Upslope_hortonian(Nhru) )
      IF ( Cascade_flag==1 .OR. Model==99 ) THEN
        IF ( declvar('srunoff', 'upslope_hortonian', 'nhru', Nhru,
     +       'real',
     +       'Hortonian surface runoff received from upslope HRUs',
     +       'inches', Upslope_hortonian)/=0 )
     +       CALL read_error(3, 'upslope_hortonian')

        IF ( declvar('srunoff', 'basin_sroff_down', 'one', 1, 'double',
     +       'Basin area-weighted average of cascading surface runoff',
     +       'inches',
     +       Basin_sroff_down)/=0 )
     +       CALL read_error(3, 'basin_sroff_down')

        IF ( declvar('srunoff', 'basin_sroff_upslope', 'one',1,'double',
     +       'Basin area-weighted average of cascading surface runoff'//
     +       ' received from upslope HRUs',
     +       'inches',
     +       Basin_sroff_upslope)/=0 )
     +       CALL read_error(3, 'basin_sroff_upslope')
      ENDIF

! Declare parameters
      ALLOCATE ( Smidx_coef(Nhru) )
      IF ( declparam('srunoff', 'smidx_coef', 'nhru', 'real',
     +     '0.01', '0.0001', '1.0',
     +     'Coefficient in contributing area computations',
     +     'Coefficient in non-linear contributing area algorithm',
     +     'decimal fraction')/=0 ) CALL read_error(1, 'smidx_coef')

      ALLOCATE ( Smidx_exp(Nhru) )
      IF ( declparam('srunoff', 'smidx_exp', 'nhru', 'real',
     +     '0.3', '0.2', '0.8',
     +     'Exponent in contributing area computations',
     +     'Exponent in non-linear contributing area algorithm',
     +     '1.0/inch')/=0 ) CALL read_error(1, 'smidx_exp')

! Depression Storage parameters:
      IF ( Dprst_flag>0 .OR. Model==99 ) THEN
        ALLOCATE ( Dprst_depth_avg(Nhru) )
        IF ( declparam('srunoff', 'dprst_depth_avg', 'nhru', 'real',
     +       '132.0', '0.0', '10000.0',
     +       'Average depth of surface depressions at maximum storage'//
     +       ' capacity',
     +       'Average depth of surface depressions at maximum storage'//
     +       ' capacity',
     +       'inches')/=0 ) CALL read_error(1, 'dprst_depth_avg')

        ALLOCATE ( Dprst_flow_coef(Nhru) )
        IF ( declparam('srunoff', 'dprst_flow_coef', 'nhru', 'real',
     +       '0.05', '0.0001', '1.0',
     +       'Coefficient in linear flow routing equation for open'//
     +       ' surface depressions',
     +       'Coefficient in linear flow routing equation for open'//
     +       ' surface depressions',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_flow_coef')

        ALLOCATE ( Dprst_seep_rate_open(Nhru) )
        IF ( declparam('srunoff', 'dprst_seep_rate_open', 'nhru',
     +       'real',
     +       '0.02', '0.0001', '1.0',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' open surface depressions',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' open surface depressions',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_seep_rate_open')

        ALLOCATE ( Dprst_seep_rate_clos(Nhru) )
        IF ( declparam('srunoff', 'dprst_seep_rate_clos', 'nhru',
     +       'real',
     +       '0.02', '0.0001', '1.0',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' closed surface depressions',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' closed surface depressions',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_seep_rate_clos')

        ALLOCATE ( Op_flow_thres(Nhru) )
        IF ( declparam('srunoff', 'op_flow_thres', 'nhru', 'real',
     +       '1.0', '0.0', '1.0',
     +       'Fraction of open depression storage above'//
     +       ' which surface runoff occurs for each timestep',
     +       'Fraction of open depression storage above'//
     +       ' which surface runoff occurs; any water above'//
     +       ' maximum open storage capacity spills as surface runoff',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'op_flow_thres')

        ALLOCATE ( Sro_to_dprst(Nhru) )
        IF ( declparam('srunoff', 'sro_to_dprst', 'nhru', 'real',
     +       '0.2', '0.0', '1.0',
     +       'Fraction of pervious and impervious surface runoff that'//
     +       ' flows into surface depression storage',
     +       'Fraction of pervious and impervious surface runoff that'//
     +       ' flows into surface depression storage; the remainder'//
     +       ' flows to a stream network',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'sro_to_dprst')

        ALLOCATE ( Dprst_frac_init(Nhru) )
        IF ( declparam('srunoff', 'dprst_frac_init', 'nhru', 'real',
     +       '0.5', '0.0', '1.0',
     +       'Fraction of maximum storage that contains water at the'//
     +       ' start of a simulation',
     +       'Fraction of maximum storage that contains water at the'//
     +       ' start of a simulation',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_frac_init')

        ALLOCATE ( Va_open_exp(Nhru) )
        IF ( declparam('srunoff', 'va_open_exp', 'nhru', 'real',
     +       '1.0', '0.0001', '10.0',
     +       'Coefficient in the exponential equation to compute'//
     +       ' current surface area of open surface depressions',
     +       'Coefficient in the exponential equation relating'//
     +       ' maximum surface area to the fraction that open'//
     +       ' depressions are full to compute surface area',
     +       'none')/=0 ) CALL read_error(1, 'va_open_exp')

        ALLOCATE ( Va_clos_exp(Nhru) )
        IF ( declparam('srunoff', 'va_clos_exp', 'nhru', 'real',
     +       '1.0', '0.0001', '10.0',
     +       'Coefficient in the exponential equation to compute'//
     +       ' current surface area of closed surface depressions',
     +       'Coefficient in the exponential equation relating'//
     +       ' maximum surface area to the fraction that closed'//
     +       ' depressions are full to compute surface area',
     +       'none')/=0 ) CALL read_error(1, 'va_clos_exp')
        ALLOCATE ( Dprst_frac_clos_hru(Nhru) )
      ENDIF

      srunoffsmidxdecl = 0
      END FUNCTION srunoffsmidxdecl

!***********************************************************************
!     srunoffsmidxinit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srunoffsmidxinit()
      USE PRMS_SRUNOFF_SMIDX
      USE PRMS_MODULE, ONLY: Dprst_flag, Cascade_flag, Nhru, Print_debug
      USE PRMS_BASIN, ONLY: Dprst_area_max, DNEARZERO, Timestep,
     +    Dprst_area_clos_max, Dprst_area_open_max, Basin_area_inv,
     +    Hru_area, Active_hrus, Hru_route_order, NEARZERO
      USE PRMS_FLOWVARS, ONLY: Hru_hortonian_cascadeflow
      IMPLICIT NONE
      INTRINSIC EXP, LOG
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, j, ierr
      REAL :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r
!***********************************************************************
      srunoffsmidxinit = 1

      IF ( Print_debug==1 ) THEN
        OPEN ( BALUNT, FILE='srunoff_smidx.wbal' )
        IF ( Cascade_flag==1 ) THEN
          WRITE ( BALUNT, 9002 )
        ELSE
          WRITE ( BALUNT, 9001 )
        ENDIF
      ENDIF

      IF ( Timestep==0 ) THEN
        Imperv_stor = 0.0
        Imperv_evap = 0.0
        Upslope_hortonian = 0.0
        Hortonian_flow = 0.0
        IF ( Dprst_flag>0 ) THEN
          Dprst_seep_hru = 0.0
          Dprst_evap_hru = 0.0
          Dprst_sroff_hru = 0.0
          Dprst_insroff_hru = 0.0
        ENDIF
        Hru_sroffi = 0.0
        Hru_sroffp = 0.0
        Basin_sroffi = 0.0D0
        Basin_sroffp = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_sroff = 0.0D0
        IF ( Cascade_flag==1 ) THEN
          Basin_sroff_upslope = 0.0D0
          Basin_sroff_down = 0.0D0
        ENDIF
      ENDIF

! Smidx parameters
      IF ( getparam('srunoff', 'smidx_coef', Nhru, 'real', Smidx_coef)
     +     /=0 ) CALL read_error(2, 'smidx_coef')
      IF ( getparam('srunoff', 'smidx_exp', Nhru, 'real', Smidx_exp)
     +     /=0 ) CALL read_error(2, 'smidx_exp')

! Depression Storage parameters and variables:
      IF ( Dprst_flag>0 ) THEN
        IF ( getparam('srunoff', 'dprst_flow_coef', Nhru, 'real',
     +       Dprst_flow_coef)/=0 )
     +       CALL read_error(2, 'dprst_flow_coef')
        IF ( getparam('srunoff', 'dprst_seep_rate_open', Nhru, 'real',
     +       Dprst_seep_rate_open)/=0 )
     +       CALL read_error(2, 'dprst_seep_rate_open')
        IF ( getparam('srunoff', 'dprst_seep_rate_clos', Nhru, 'real',
     +       Dprst_seep_rate_clos)/=0 )
     +       CALL read_error(2, 'dprst_seep_rate_clos')
        IF ( getparam('srunoff', 'dprst_frac_init', Nhru, 'real',
     +       Dprst_frac_init)/=0 ) CALL read_error(2, 'dprst_frac_init')
        IF ( getparam('srunoff', 'va_open_exp', Nhru, 'real',
     +       Va_open_exp)/=0 ) CALL read_error(2, 'va_open_exp')
        IF ( getparam('srunoff', 'va_clos_exp', Nhru, 'real',
     +       Va_clos_exp)/=0 ) CALL read_error(2, 'va_clos_exp')
        IF ( getparam('srunoff', 'op_flow_thres', Nhru, 'real',
     +       Op_flow_thres)/=0 ) CALL read_error(2, 'op_flow_thres')
        IF ( getparam('srunoff', 'sro_to_dprst', Nhru, 'real',
     +       Sro_to_dprst)/=0 ) CALL read_error(2, 'sro_to_dprst')
        IF ( getparam('srunoff', 'dprst_depth_avg', Nhru, 'real',
     +       Dprst_depth_avg)/=0 ) CALL read_error(2, 'dprst_depth_avg')

        ALLOCATE ( Dprst_vol_thres_open(Nhru) )
        ALLOCATE ( Dprst_vol_open_max(Nhru), Dprst_vol_clos_max(Nhru) )
        ierr = 0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
        Dprst_vol_clos_max = 0.0D0
        Dprst_vol_open_max = 0.0D0
        IF ( Timestep==0 ) THEN
          Dprst_area_open = 0.0
          Dprst_area_clos = 0.0
          Dprst_vol_open = 0.0D0
          Dprst_vol_clos = 0.0D0
        ENDIF
        Dprst_vol_thres_open = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
!******************************************************************
! Following lines added for Depression Storage:
          IF ( Dprst_frac_init(i)>1.0 ) THEN
            PRINT *, 'dprst_frac_init>1.0, set to 1.0 for HRU:', i
            Dprst_frac_init(i) = 1.0
          ENDIF
          IF ( Dprst_frac_init(i)<NEARZERO ) Dprst_frac_init(i) = 0.0
          IF ( Sro_to_dprst(i)>1.0 ) THEN
            PRINT *, 'sro_to_dprst>1.0, set to 1.0 for HRU:', i
            Sro_to_dprst(i) = 1.0
          ENDIF
          IF ( Dprst_flow_coef(i)>1.0 ) THEN
            PRINT *, 'dprst_flow_coef>1.0, set to 1.0 for HRU:', i
            Dprst_flow_coef(i) = 1.0
          ENDIF
          IF ( Dprst_seep_rate_open(i)>1.0 ) THEN
            PRINT *, 'dprst_seep_rate_open>1.0, set to 1.0 for HRU:', i
            Dprst_seep_rate_open(i) = 1.0
          ENDIF
          IF ( Dprst_seep_rate_clos(i)>1.0 ) THEN
            PRINT *, 'dprst_seep_rate_clos>1.0, set to 1.0 for HRU:', i
            Dprst_seep_rate_clos(i) = 1.0
          ENDIF
          IF ( Op_flow_thres(i)>1.0 ) THEN
            PRINT *, 'op_flow_thres>1.0, set to 1.0 for HRU:', i
            Op_flow_thres(i) = 1.0
          ENDIF

          IF ( Dprst_area_max(i)>NEARZERO ) THEN
            Dprst_frac_clos_hru(i) = Dprst_area_clos_max(i)/Hru_area(i)
            IF ( Dprst_frac_clos_hru(i)<NEARZERO )
     +           Dprst_frac_clos_hru(i) = 0.0

!       calculate open and closed volumes (acre-inches) of depression storage by HRU
!       Dprst_area_open_max is the maximum open depression area (acres)
!          that can generate surface runoff:
!       Old parameter dprst_dem is input in inches for starkweather model
!           Dprst_vol_clos_max(i) = Dprst_dem(i)*Dprst_area_clos_max(i)
!           Dprst_vol_open_max(i) = Dprst_dem(i)*Dprst_area_open_max(i)
            ! for all new models
            Dprst_vol_clos_max(i) = Dprst_area_clos_max(i)
     +                              *Dprst_depth_avg(i)
            Dprst_vol_open_max(i) = Dprst_area_open_max(i)
     +                              *Dprst_depth_avg(i)
!       calculate the intial open and closed depression storage volume:
            IF ( Timestep==0 ) THEN
              Dprst_vol_open(i)=Dprst_frac_init(i)*Dprst_vol_open_max(i)
              Dprst_vol_clos(i)=Dprst_frac_init(i)*Dprst_vol_clos_max(i)
            ENDIF

!       threshold volume is calculated as the % of maximum open
!       depression storage above which flow occurs *  total open
!       depression storage volume
            Dprst_vol_thres_open(i) = Op_flow_thres(i)
     +                                *Dprst_vol_open_max(i)

!       initial open and closed storage volume as fraction of total open and closed
!       storage volume

!       Open depression surface area for each HRU:
            IF ( Dprst_vol_open(i)>DNEARZERO ) THEN
              open_vol_r = Dprst_vol_open(i)/Dprst_vol_open_max(i)
              IF ( open_vol_r<NEARZERO ) THEN
                frac_op_ar = 0.0
              ELSEIF ( open_vol_r==1.0 ) THEN
                frac_op_ar = 1.0
              ELSE
                frac_op_ar = EXP(Va_open_exp(i)*LOG(open_vol_r))
              ENDIF
              Dprst_area_open(i) = Dprst_area_open_max(i)*frac_op_ar
              IF ( Dprst_area_open(i)>Dprst_area_open_max(i) )
     +             Dprst_area_open(i) = Dprst_area_open_max(i)
              IF ( Dprst_area_open(i)<NEARZERO )Dprst_area_open(i) = 0.0
            ENDIF

!       Closed depression surface area for each HRU:
            IF ( Dprst_vol_clos(i)>DNEARZERO ) THEN
              clos_vol_r = Dprst_vol_clos(i)/Dprst_vol_clos_max(i)
              IF ( clos_vol_r<NEARZERO ) THEN
                frac_cl_ar = 0.0
              ELSEIF ( clos_vol_r==1.0 ) THEN
                frac_cl_ar = 1.0
              ELSE
                frac_cl_ar = EXP(Va_clos_exp(i)*LOG(clos_vol_r))
              ENDIF
              Dprst_area_clos(i) = Dprst_area_clos_max(i)*frac_cl_ar
              IF ( Dprst_area_clos(i)>Dprst_area_clos_max(i) )
     +             Dprst_area_clos(i) = Dprst_area_clos_max(i)
              IF ( Dprst_area_clos(i)<NEARZERO )Dprst_area_clos(i) = 0.0
            ENDIF

!       calculate the basin open and closed depression storage volumes
            Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open(i)
            Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos(i)
          ENDIF
        ENDDO
        IF ( ierr==1 ) STOP
        DEALLOCATE ( Dprst_frac_init, Op_flow_thres, Dprst_depth_avg )
        Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
        Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
!******************************************************************
      ENDIF

 9001 FORMAT ('    Date     Water Bal     Robal      Sroff',
     +        '   Infiltrat Impervevap Impervstor Dprst_evap',
     +        ' Dprst_seep')
 9002 FORMAT ('    Date     Water Bal     Robal      Sroff   Sroffdown',
     +        '   Srofflake Infiltrat Impervevap Impervstor    Farflow',
     +        ' Dprst_evap Dprst_seep')

      srunoffsmidxinit = 0
      END FUNCTION srunoffsmidxinit

!***********************************************************************
!     srunoffsmidxrun - Computes surface runoff using contributing area
!                  computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srunoffsmidxrun()
      USE PRMS_SRUNOFF_SMIDX
      USE PRMS_MODULE, ONLY: Dprst_flag, Cascade_flag, Nhru, Print_debug
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, DNEARZERO,
     +    Hru_perv, Hru_imperv, Hru_frac_imperv, Hru_frac_perv,
     +    NEARZERO, Dprst_area_max, Hru_area, Hru_type, Basin_area_inv,
     +    Dprst_area_clos_max, Dprst_area_open_max, Dprst_frac_hru
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Hru_impervevap,
     +    Sroff, Basin_sroff, Infil, Carea_max, Imperv_stor_max,
     +    Hru_impervstor, Basin_imperv_evap, Basin_imperv_stor,
     +    Basin_infil, Hru_hortonian_cascadeflow, Basin_hortonian_lakes,
     +    Strm_seg_in, Strm_farfield, Hortonian_lakes, Snowinfil_max,
     +    Soil_moist, Basin_hortonian, Basin_sroff_farflow
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_INTCP, ONLY: Hru_intcpevap, Net_rain, Net_snow, Net_ppt
      USE PRMS_SNOW, ONLY: Pkwater_equiv, Pptmix_nopack, Snow_evap,
     +    Snowcov_area, Snowmelt
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_et, compute_infil_smidx, run_cascade_smidx
      EXTERNAL dprst_comp, perv_sroff_smidx, read_error
! Local Variables
      INTEGER :: i, k
      REAL :: srunoff, robal, hru_sroff_down, farflow, avail_et
      REAL :: harea, himperv, hperv, last_stor
      DOUBLE PRECISION :: last_dprst_stor, basin_robal, new_dprst_stor
      DOUBLE PRECISION :: runoff, tmp1, tmp2
!***********************************************************************
      srunoffsmidxrun = 1

      !rsr, soil_moist is used from the last time step

      Basin_sroffi = 0.0D0
      Basin_sroffp = 0.0D0
      basin_robal = 0.0D0
      Basin_sroff = 0.0D0
      Basin_infil = 0.0D0
      Basin_imperv_evap = 0.0D0
      Basin_imperv_stor = 0.0D0

      IF ( Cascade_flag==1 ) THEN
        Basin_sroff_down = 0.0D0
        Basin_sroff_upslope = 0.0D0
        Basin_hortonian = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Upslope_hortonian = 0.0
        Strm_seg_in = 0.0D0
        Strm_farfield = 0.0D0
        Basin_sroff_farflow = 0.0D0
      ENDIF

      IF ( Dprst_flag>0 ) THEN
        Basin_dprst_sroff = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
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
          Srp = 0.0
          Sri = 0.0
          Hru_sroffp(i) = 0.0
          Hru_sroffi(i) = 0.0

          IF ( Dprst_flag>0 ) THEN
            IF ( Print_debug==1 ) THEN
              ! last depression storage on whole HRU
              last_dprst_stor = 0.0D0
              IF ( Dprst_vol_open(i)>DNEARZERO ) last_dprst_stor =
     +             Dprst_vol_open(i)/harea
              IF ( Dprst_vol_clos(i)>DNEARZERO ) last_dprst_stor =
     +             last_dprst_stor + Dprst_vol_clos(i)/harea
            ENDIF
          ENDIF
!     *************************

!******Compute runoff for pervious, impervious, and depression storage area
          CALL compute_infil_smidx(i, Pptmix_nopack(i), Smidx_coef(i),
     +         Smidx_exp(i), Soil_moist(i), Soil_moist_max(i),
     +         Carea_max(i), Net_rain(i), Net_ppt(i), himperv,
     +         Imperv_stor(i), Imperv_stor_max(i), Snowmelt(i),
     +         Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i),
     +         Infil(i), Hru_type(i), Upslope_hortonian(i))

          avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)

!         ******Compute the depression storage component
!         only call if total depression surface area for each HRU is > 0.0
          IF ( Dprst_flag>0 ) THEN
            IF ( Dprst_area_max(i)>NEARZERO ) THEN
              CALL dprst_comp(Dprst_vol_clos_max(i), Dprst_vol_clos(i),
     +             Dprst_area_clos_max(i), Dprst_area_clos(i),
     +             Dprst_vol_open_max(i), Dprst_vol_open(i),
     +             Dprst_area_open_max(i), Dprst_area_open(i),
     +             Dprst_flow_coef(i), Dprst_sroff_hru(i),
     +             Dprst_seep_rate_open(i), Dprst_seep_rate_clos(i),
     +             Dprst_seep_hru(i), Va_open_exp(i), Va_clos_exp(i),
     +             Sro_to_dprst(i), Snowcov_area(i), Dprst_evap_hru(i),
     +             Dprst_frac_clos_hru(i), Dprst_insroff_hru(i),
     +             Dprst_vol_thres_open(i), Hru_frac_perv(i),
     +             Hru_frac_imperv(i), harea, avail_et, Potet(i), i)
              Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open(i)
              Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos(i)
              avail_et = avail_et - Dprst_evap_hru(i)
              Basin_dprst_evap = Basin_dprst_evap +
     +                           Dprst_evap_hru(i)*harea
              Basin_dprst_seep = Basin_dprst_seep
     +                           + Dprst_seep_hru(i)*harea
              runoff = Dprst_sroff_hru(i)*harea
              Basin_dprst_sroff = Basin_dprst_sroff + runoff
            ENDIF
          ENDIF
!         **********************************************************

!******Compute runoff for pervious and impervious area, and depression storage area
! There must be some pervious area for this HRU
          runoff = runoff + Srp*hperv + Sri*himperv
          srunoff = runoff/harea

!******Compute HRU weighted average (to units of inches/dt)
          hru_sroff_down = 0.0
          farflow = 0.0

          IF ( Hru_type(i)==1 ) THEN
            IF ( Cascade_flag==1 ) THEN
              IF ( Ncascade_hru(i)>0 .AND. srunoff>0.0 )
     +             CALL run_cascade_smidx(i, Ncascade_hru(i), srunoff,
     +                  hru_sroff_down, farflow)
              Hru_hortonian_cascadeflow(i) = hru_sroff_down + farflow
              Basin_sroff_upslope = Basin_sroff_upslope +
     +                              Upslope_hortonian(i)*harea
              Basin_sroff_down = Basin_sroff_down +
     +                           hru_sroff_down*harea
              Basin_sroff_farflow = Basin_sroff_farflow + farflow*harea
              Basin_hortonian = Basin_hortonian + srunoff*harea
            ENDIF
            Basin_sroff = Basin_sroff + srunoff*harea
          ENDIF

          Basin_infil = Basin_infil + Infil(i)*hperv

!******Compute evaporation from impervious area
          IF ( himperv>NEARZERO ) THEN
            IF ( Imperv_stor(i)>0.0 ) THEN
              CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i),
     +             Snowcov_area(i), Hru_frac_imperv(i), avail_et)
              Hru_impervevap(i) = Imperv_evap(i)*Hru_frac_imperv(i)
              avail_et = avail_et - Hru_impervevap(i)
              IF ( avail_et<0.0 ) THEN
                !rsr, sanity check
!                IF ( avail_et<-1.0E-5 )
!     +               PRINT*, 'avail_et<0 in srunoff imperv', i,
!     +                       Nowmonth, Nowday, avail_et
                Hru_impervevap(i) = Hru_impervevap(i) + avail_et
                IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
                Imperv_evap(i) = Hru_impervevap(i)/Hru_frac_imperv(i)
                Imperv_stor(i) = Imperv_stor(i)
     +                           - avail_et/Hru_frac_imperv(i)
              ENDIF
            ELSE
              Imperv_evap(i) = 0.0
              Hru_impervevap(i) = 0.0
            ENDIF
            Basin_imperv_evap = Basin_imperv_evap +
     +                          Hru_impervevap(i)*harea
            Hru_impervstor(i) = Imperv_stor(i)*Hru_frac_imperv(i)
            Basin_imperv_stor = Basin_imperv_stor +
     +                          Imperv_stor(i)*himperv
          ENDIF

          Hru_sroffp(i) = Srp*Hru_frac_perv(i)
          Hru_sroffi(i) = Sri*Hru_frac_imperv(i)
          Basin_sroffp = Basin_sroffp + Srp*hperv
          Basin_sroffi = Basin_sroffi + Sri*himperv

          IF ( Print_debug==1 ) THEN
            robal = Snowmelt(i) - srunoff !includes dprst runoff, if any
     +              - Infil(i)*Hru_frac_perv(i) - Hru_impervevap(i)
     +              + (last_stor-Imperv_stor(i))*Hru_frac_imperv(i)
            IF ( Net_ppt(i)>0.0 ) THEN
              IF ( Pptmix_nopack(i)==1 ) THEN
                robal = robal + Net_rain(i)
              ELSEIF ( Snowmelt(i)<NEARZERO .AND.
     +                 Snow_evap(i)<NEARZERO .AND.
     +                 Pkwater_equiv(i)<NEARZERO) THEN
                robal = robal + Net_ppt(i)
              ELSEIF ( Snowmelt(i)<NEARZERO .AND.
     +                 Snow_evap(i)>0.0 .AND.
     +                 Pkwater_equiv(i)<NEARZERO .AND.
     +                 Net_snow(i)<NEARZERO ) THEN
                robal = robal + Net_ppt(i)
              ENDIF
            ENDIF
            IF ( Cascade_flag==1 ) robal = robal + Upslope_hortonian(i)
     +                                     - hru_sroff_down - farflow
            IF ( Dprst_flag>0 ) THEN
              new_dprst_stor = 0.0D0
              IF ( Dprst_vol_open(i)>DNEARZERO ) new_dprst_stor =
     +            Dprst_vol_open(i)/harea
              IF ( Dprst_vol_clos(i)>DNEARZERO ) new_dprst_stor =
     +             new_dprst_stor + Dprst_vol_clos(i)/harea
              tmp1 = last_dprst_stor - new_dprst_stor -Dprst_seep_hru(i)
     +               - Dprst_evap_hru(i)
     + !+ Dprst_insroff_hru(i) - Dprst_sroff_hru(i)
              robal = robal + tmp1
            ENDIF
            basin_robal = basin_robal + robal
            IF ( ABS(robal)>1.0D-4 ) THEN
              IF ( Dprst_flag>0 ) then
              tmp2 = last_dprst_stor - new_dprst_stor -Dprst_seep_hru(i)
     +               - Dprst_evap_hru(i) - Dprst_sroff_hru(i)
     +               + Snowmelt(i) + Net_rain(i) + Upslope_hortonian(i)
     +               + Dprst_insroff_hru(i)
              WRITE( BALUNT, * ) 'tmp2',last_dprst_stor,new_dprst_stor,
     +               Dprst_seep_hru(i),Dprst_evap_hru(i),
     +               Dprst_sroff_hru(i),Snowmelt(i),Net_rain(i),
     +               Dprst_frac_hru(i), Dprst_insroff_hru(i)
              WRITE( BALUNT, * ) tmp1, tmp2, Dprst_vol_open(i),
     + Dprst_vol_clos(i), (Dprst_vol_open(i)+Dprst_vol_clos(i))/harea,
     + Dprst_seep_hru(i), last_dprst_stor, Dprst_evap_hru(i),
     + Dprst_area_max(i),runoff,srunoff,Pkwater_equiv(i),new_dprst_stor,
     + Dprst_area_clos(i),Snowcov_area(i), Dprst_insroff_hru(i)
              WRITE( BALUNT, * ) robal, Net_rain(i), Net_ppt(i),
     +                          Dprst_insroff_hru(i),
     + Net_rain(i)*Dprst_frac_hru(i),Snowmelt(i), Dprst_frac_hru(i)
              ENDIF
              IF ( ABS(robal)>5.0D-4 ) THEN
                WRITE (BALUNT, *) 'possible HRU water balance error'
              ELSE
                WRITE (BALUNT, *) 'HRU robal rounding issue'
              ENDIF
              IF ( Cascade_flag==1 ) THEN
                WRITE ( BALUNT, '(2I3,I6,18F10.6,I3)' ) Nowmonth,
     +                 Nowday, i, robal, Snowmelt(i),
     +                 Upslope_hortonian(i), last_stor,
     +                 hru_sroff_down, Infil(i), srunoff,
     +                 Imperv_stor(i), Imperv_evap(i), Net_ppt(i),
     +                 Pkwater_equiv(i),  Snow_evap(i), Net_snow(i),
     +                 farflow, Net_rain(i), Srp, Sri, runoff,
     +                 Pptmix_nopack(i)
              ELSE
                WRITE ( BALUNT,'(2I3,I4,14F10.7,I5)' ) Nowmonth, Nowday,
     +                 i, robal, Snowmelt(i), last_stor, Infil(i),
     +                 srunoff, Imperv_stor(i), Imperv_evap(i),
     +                 Hru_impervevap(i), Hru_frac_imperv(i),
     +                 Net_ppt(i), Pkwater_equiv(i), Snow_evap(i),
     +                 Net_snow(i), Net_rain(i), Pptmix_nopack(i)
              ENDIF
            ENDIF
          ENDIF
          Sroff(i) = srunoff
          Hortonian_flow(i) = srunoff
        ELSE
! HRU is a lake
!rsr, eventually add code for lake area less than hru_area
!     that includes soil_moist for fraction of hru_area that is dry bank
          ! Sanity check
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i)>0.0 )
     +         PRINT *, 'smidx lake error', Infil(i), Sroff(i),
     +               Imperv_stor(i), Imperv_evap(i), i
          IF ( Cascade_flag==1 ) THEN
            Hortonian_lakes(i) = Upslope_hortonian(i)
            Basin_hortonian_lakes = Basin_hortonian_lakes +
     +                              Hortonian_lakes(i)*harea
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
      IF ( Cascade_flag==1 ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_farflow = Basin_sroff_farflow*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
        Basin_hortonian = Basin_hortonian*Basin_area_inv
      ENDIF

      IF ( Dprst_flag>0 ) THEN
        Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
        Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
        Basin_dprst_evap = Basin_dprst_evap*Basin_area_inv
        Basin_dprst_seep = Basin_dprst_seep*Basin_area_inv
        Basin_dprst_sroff = Basin_dprst_sroff*Basin_area_inv
      ENDIF

      IF ( Print_debug==1 ) THEN
        robal = Basin_sroff - Basin_sroffp - Basin_sroffi
     +          - Basin_dprst_sroff
        IF ( Cascade_flag==1 ) THEN
          robal = robal + Basin_sroff_down + Basin_sroff_farflow
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, basin_robal,
     +           robal, Basin_sroff, Basin_sroff_down,
     +           Basin_hortonian_lakes, Basin_infil, Basin_imperv_evap,
     +           Basin_imperv_stor, Basin_sroff_farflow,
     +           Basin_dprst_evap, Basin_dprst_seep
        ELSE
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, basin_robal,
     +           robal, Basin_sroff, Basin_infil, Basin_imperv_evap,
     +           Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep,
     +           Basin_sroffp, Basin_sroffi, Basin_dprst_seep
        ENDIF
        IF ( ABS(basin_robal)>1.0D-3 ) THEN
          WRITE ( BALUNT, * ) 'possible basin water balance error'
        ELSEIF ( ABS(basin_robal)>5.0D-4 ) THEN
          WRITE ( BALUNT, * ) 'basin_robal rounding issue'
        ENDIF
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), 11F11.4)

      srunoffsmidxrun = 0
      END FUNCTION srunoffsmidxrun

!***********************************************************************
!      Subroutine to compute evaporation from impervious area at
!      potential ET rate up to available ET
!***********************************************************************
      SUBROUTINE imperv_et(Imperv_stor, Potet, Imperv_evap, Sca,
     +                     Hru_frac_imperv, Avail_et)
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
        IF ( Imperv_evap*Hru_frac_imperv>Avail_et ) Imperv_evap =
     +       Avail_et/Hru_frac_imperv
        Imperv_stor = Imperv_stor - Imperv_evap
      ELSE
        Imperv_evap = 0.0
      ENDIF
      !rsr, sanity check
      IF ( Imperv_stor<0.0 ) Imperv_stor = 0.0

      END SUBROUTINE imperv_et

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil_smidx(I, Pptmix_nopack, Smidx_coef,
     +           Smidx_exp, Soil_moist, Soil_moist_max, Carea_max,
     +           Net_rain, Net_ppt, Hru_imperv, Imperv_stor,
     +           Imperv_stor_max, Snowmelt, Snowinfil_max, Net_snow,
     +           Pkwater_equiv, Infil, Hru_type, Upslope_hortonian)
      USE PRMS_SRUNOFF_SMIDX, ONLY: Srp, Sri, Dprst_vol_open,
     +    Dprst_vol_clos
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO, Dprst_area_open_max,
     +    Dprst_area_clos_max
      USE PRMS_MODULE, ONLY: Dprst_flag
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_sroff, perv_imperv_comp_smidx, check_capacity
! Arguments
      ! I = HRU id
      INTEGER, INTENT(IN) :: Pptmix_nopack, Hru_type, I
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Carea_max
      REAL, INTENT(IN) :: Soil_moist_max, Soil_moist, Net_rain, Net_ppt
      REAL, INTENT(IN) :: Imperv_stor_max, Hru_imperv
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow
      REAL, INTENT(IN) :: Pkwater_equiv, Upslope_hortonian
      REAL, INTENT(INOUT) :: Imperv_stor, Infil
! Local Variables
      REAL :: ppti, pptp, ptc, snri
!***********************************************************************
! compute runoff from cascading Hortonian flow
      IF ( Upslope_hortonian>0.0 ) THEN
        ptc = Upslope_hortonian
        pptp = Upslope_hortonian
        ppti = Upslope_hortonian
        CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv,
     +       Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +       Imperv_stor_max, Imperv_stor, Infil, Hru_type)

!       add the hortonian flow to the depression storage volumes:
        IF ( Dprst_flag>0 ) THEN
          IF ( Dprst_area_open_max(I)>NEARZERO )
     +         Dprst_vol_open(I) = Dprst_vol_open(I) +
     +                         Upslope_hortonian*Dprst_area_open_max(I)
          IF ( Dprst_area_clos_max(I)>NEARZERO )
     +         Dprst_vol_clos(I) = Dprst_vol_clos(I) +
     +                         Upslope_hortonian*Dprst_area_clos_max(I)
        ENDIF
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack==1 ) THEN
        ptc = Net_rain
        pptp = Net_rain
        ppti = Net_rain
        CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv,
     +       Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +       Imperv_stor_max, Imperv_stor, Infil, Hru_type)

!       add the rainfall to the depression storage volumes:
        IF ( Dprst_flag>0 ) THEN
          IF ( Dprst_area_open_max(I)>NEARZERO )
     +         Dprst_vol_open(I) = Dprst_vol_open(I)
     +                             + Net_rain*Dprst_area_open_max(I)
          IF ( Dprst_area_clos_max(I)>NEARZERO )
     +         Dprst_vol_clos(I) = Dprst_vol_clos(I)
     +                             + Net_rain*Dprst_area_clos_max(I)
        ENDIF
      ENDIF

!******If precip on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN

        IF ( Pkwater_equiv>NEARZERO .OR.
     +       ABS(Net_ppt-Net_snow)<NEARZERO ) THEN

!******Pervious area computations
          Infil = Infil + Snowmelt
          IF ( Infil>0 .AND. Hru_type==1 )
     +         CALL check_capacity(Soil_moist_max, Soil_moist,
     +                             Snowinfil_max, Srp, Infil)

!******Impervious area computations
          IF ( Hru_imperv>NEARZERO ) THEN
            CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Snowmelt,
     +           snri, Hru_type)
            Sri = Sri + snri
          ENDIF

        ELSE

!******Snowmelt occurred and depleted the snowpack
          ptc = Net_ppt
          pptp = Snowmelt
          ppti = Snowmelt

          CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv,
     +         Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +         Imperv_stor_max, Imperv_stor, Infil, Hru_type)

        ENDIF

!       add the snowmelt to the depression storage volumes:
        IF ( Dprst_flag>0 ) THEN
          IF ( Dprst_area_open_max(I)>NEARZERO )
     +         Dprst_vol_open(I) = Dprst_vol_open(I)
     +                             + Snowmelt*Dprst_area_open_max(I)
          IF ( Dprst_area_clos_max(I)>NEARZERO )
     +         Dprst_vol_clos(I) = Dprst_vol_clos(I)
     +                             + Snowmelt*Dprst_area_clos_max(I)
        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( Pkwater_equiv<NEARZERO ) THEN

!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.

        IF ( Net_snow<NEARZERO .AND. Net_rain>0.0 ) THEN
! no snow, some rain
          ptc = Net_rain
          pptp = Net_rain
          ppti = Net_rain

          CALL perv_imperv_comp_smidx(ptc, pptp, ppti, Hru_imperv,
     +         Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +         Imperv_stor_max, Imperv_stor, Infil, Hru_type)

!         add the rainfall to the depression storage volumes:
          IF ( Dprst_flag>0 ) THEN
            IF ( Dprst_area_open_max(I)>NEARZERO )
     +           Dprst_vol_open(I) = Dprst_vol_open(I)
     +           + Net_rain*Dprst_area_open_max(I)
            IF ( Dprst_vol_clos(I)>DNEARZERO )
     +           Dprst_vol_clos(I) = Dprst_vol_clos(I)
     +           + Net_rain*Dprst_area_clos_max(I)
          ENDIF
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.
      ELSEIF ( Infil>0.0 .AND. Hru_type==1 ) THEN
        CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max,
     +       Srp, Infil)

      ENDIF

      END SUBROUTINE compute_infil_smidx

!***********************************************************************
!***********************************************************************
      SUBROUTINE perv_imperv_comp_smidx(Ptc, Pptp, Ppti, Hru_imperv,
     +           Smidx_coef, Smidx_exp, Soil_moist, Carea_max,
     +           Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      USE PRMS_SRUNOFF_SMIDX, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO, NEARZERO
      IMPLICIT NONE
      EXTERNAL perv_sroff_smidx, imperv_sroff
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Ptc, Pptp, Ppti, Imperv_stor_max
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Soil_moist, Carea_max
      REAL, INTENT(IN) :: Hru_imperv
      REAL, INTENT(INOUT) :: Infil, Imperv_stor
! Local Variables
      REAL :: inp, snrp, snri
!***********************************************************************
!******Pervious area computations
      IF ( Pptp>0.0 ) THEN
        CALL perv_sroff_smidx(Smidx_coef, Smidx_exp, Soil_moist,
     +       Carea_max, Pptp, Ptc, inp, snrp, Hru_type)
        Infil = Infil + inp
        Srp = Srp + snrp
        IF ( Srp<0.0 ) Srp = 0.0
      ENDIF

!******Impervious area computations
      IF ( Ppti>0.0 .AND. Hru_imperv>NEARZERO ) THEN
        CALL imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, snri,
     +       Hru_type)
        Sri = Sri + snri
      ENDIF

      END SUBROUTINE perv_imperv_comp_smidx

!***********************************************************************
!      Subroutine to compute runoff from pervious area using non-linear
!      contributing area computations
!***********************************************************************

      SUBROUTINE perv_sroff_smidx(Smidx_coef, Smidx_exp, Soil_moist,
     +           Carea_max, Pptp, Ptc, Infil, Srp, Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Smidx_coef, Smidx_exp, Soil_moist, Pptp, Ptc
      REAL, INTENT(IN) :: Carea_max
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
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_smidx(Ihru, Ncascade_hru, Runoff,
     +           Hru_sroff_down, Farflow)
      USE PRMS_SRUNOFF_SMIDX, ONLY: Upslope_hortonian
      USE PRMS_MODULE, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt,
     +    Cascade_area
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Ncascade_hru
      REAL, INTENT(INOUT) :: Hru_sroff_down, Farflow, Runoff
! Local Variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j>0 ) THEN
          Upslope_hortonian(j) = Upslope_hortonian(j)
     +                           + Runoff*Hru_down_fracwt(k, Ihru)
          Hru_sroff_down = Hru_sroff_down + Runoff*Hru_down_frac(k,Ihru)

! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          IF ( j/=Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) +
     +                       Runoff*Cfs_conv*Cascade_area(k, Ihru)
          ELSE
            Strm_farfield = Strm_farfield +
     +                      Runoff*Cfs_conv*Cascade_area(k, Ihru)
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

      END SUBROUTINE run_cascade_smidx

!***********************************************************************
!      Subroutine to compute runoff from impervious area
!***********************************************************************
      SUBROUTINE imperv_sroff(Imperv_stor_max, Imperv_stor, Ppti, Sri,
     +           Hru_type)
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
      SUBROUTINE check_capacity(Soil_moist_max, Soil_moist,
     +           Snowinfil_max, Srp, Infil)
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
!      Subroutine to compute depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_comp(Dprst_vol_clos_max, Dprst_vol_clos,
     +           Dprst_area_clos_max, Dprst_area_clos,
     +           Dprst_vol_open_max, Dprst_vol_open,
     +           Dprst_area_open_max, Dprst_area_open, Dprst_flow_coef,
     +           Dprst_sroff_hru, Dprst_seep_rate_open,
     +           Dprst_seep_rate_clos, Dprst_seep_hru, Va_open_exp,
     +           Va_clos_exp, Sro_to_dprst, Snowcov_area,
     +           Dprst_evap_hru, Dprst_frac_clos_hru, Dprst_insroff_hru,
     +           Dprst_vol_thres_open, Hru_frac_perv, Hru_frac_imperv,
     +           Hruarea, Avail_et, Potet, Ihru)
      USE PRMS_SRUNOFF_SMIDX, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      IMPLICIT NONE
      INTRINSIC EXP, LOG, MAX
! Arguments
      INTEGER, INTENT(IN) :: Ihru
      REAL, INTENT(IN) :: Dprst_flow_coef, Va_open_exp, Va_clos_exp
      REAL, INTENT(IN) :: Snowcov_area, Sro_to_dprst, Potet
      REAL, INTENT(IN) :: Dprst_seep_rate_open
      REAL, INTENT(IN) :: Dprst_seep_rate_clos, Hruarea
      REAL, INTENT(IN) :: Hru_frac_perv, Hru_frac_imperv
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_thres_open
      REAL, INTENT(IN) :: Dprst_frac_clos_hru
      REAL, INTENT(IN) :: Dprst_area_open_max
      REAL, INTENT(IN) :: Dprst_area_clos_max
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_clos_max
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_open_max
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_vol_open, Dprst_vol_clos
      REAL, INTENT(INOUT) :: Avail_et
      REAL, INTENT(OUT) :: Dprst_area_open, Dprst_area_clos
      REAL, INTENT(OUT) :: Dprst_evap_hru, Dprst_sroff_hru
      REAL, INTENT(OUT) :: Dprst_seep_hru, Dprst_insroff_hru
! Local Variables
      REAL :: dprst_et, frac_cl_ar, frac_op_ar
      REAL :: dprst_evap_open, dprst_evap_clos
      REAL :: seep_open, seep_clos, tmp, dprst_evap_vol
      REAL :: open_vol_r, clos_vol_r, dprst_srp, dprst_sri
      REAL :: dprst_srp_open, dprst_srp_clos
      REAL :: dprst_sri_open, dprst_sri_clos
!***********************************************************************

      ! add any pervious surface runoff fraction to depressions
      IF ( Srp>NEARZERO ) THEN
        ! dprst_srp = inches of surface runoff over HRU
        dprst_srp = Srp*Hru_frac_perv*Sro_to_dprst
        IF ( Dprst_area_open_max>NEARZERO ) THEN
          dprst_srp_open = dprst_srp*Dprst_area_open_max
          Dprst_vol_open = Dprst_vol_open + dprst_srp_open
        ELSE
          dprst_srp_open = 0.0
        ENDIF
        IF ( Dprst_area_clos_max>NEARZERO ) THEN
          dprst_srp_clos = dprst_srp*Dprst_area_clos_max
          Dprst_vol_clos = Dprst_vol_clos + dprst_srp_clos
        ELSE
          dprst_srp_clos = 0.0
        ENDIF
        Dprst_insroff_hru = (dprst_srp_open + dprst_srp_clos)/Hruarea
        Srp = Srp - Dprst_insroff_hru/Hru_frac_perv
        IF ( Srp<0.0 ) THEN
          PRINT *, 'dprst srp<0.0', Srp
          ! may need to adjust volumes
          Srp = 0.0
        ENDIF
      ENDIF

      ! add any impervious surface runoff fraction to depressions
      IF ( Sri>NEARZERO ) THEN
        ! dprst_sri = inches of surface runoff over HRU
        dprst_sri = Sri*Hru_frac_imperv*Sro_to_dprst
        IF ( Dprst_area_open_max>NEARZERO ) THEN
          dprst_sri_open = dprst_sri*Dprst_area_open_max
          Dprst_vol_open = Dprst_vol_open + dprst_sri_open
        ELSE
          dprst_sri_open = 0.0
        ENDIF
        IF ( Dprst_area_clos_max>NEARZERO ) THEN
          dprst_sri_clos = dprst_sri*Dprst_area_clos_max
          Dprst_vol_clos = Dprst_vol_clos + dprst_sri_clos
        ELSE
          dprst_sri_clos = 0.0
        ENDIF
        tmp = (dprst_sri_open + dprst_sri_clos)/Hruarea
        Dprst_insroff_hru = Dprst_insroff_hru + tmp
        Sri = Sri - tmp/Hru_frac_imperv
        IF ( Sri<0.0 ) THEN
          PRINT *, 'dprst sri<0.0', Sri
          ! may need to adjust volumes
          Sri = 0.0
        ENDIF
      ENDIF

!     Open depression surface area for each HRU:
      Dprst_area_open = 0.0
      open_vol_r = 0.0
      IF ( Dprst_vol_open>DNEARZERO ) THEN
        open_vol_r = Dprst_vol_open/Dprst_vol_open_max
        IF ( open_vol_r<NEARZERO ) THEN
          frac_op_ar = 0.0
        ELSEIF ( open_vol_r>1.0 ) THEN
          frac_op_ar = 1.0
        ELSE
          frac_op_ar = EXP(Va_open_exp*LOG(open_vol_r))
        ENDIF
        Dprst_area_open = Dprst_area_open_max*frac_op_ar
        IF ( Dprst_area_open>Dprst_area_open_max )
     +       Dprst_area_open = Dprst_area_open_max
        IF ( Dprst_area_open<NEARZERO ) Dprst_area_open = 0.0
      ENDIF

!     Closed depression surface area for each HRU:
      Dprst_area_clos = 0.0
      clos_vol_r = 0.0
      IF ( Dprst_vol_clos>DNEARZERO ) THEN
        clos_vol_r = Dprst_vol_clos/Dprst_vol_clos_max
        IF ( clos_vol_r<NEARZERO ) THEN
          frac_cl_ar = 0.0
        ELSEIF ( clos_vol_r>1.0 ) THEN
          frac_cl_ar = 1.0
        ELSE
          frac_cl_ar = EXP(Va_clos_exp*LOG(clos_vol_r))
        ENDIF
        Dprst_area_clos = Dprst_area_clos_max*frac_cl_ar
        IF ( Dprst_area_clos>Dprst_area_clos_max )
     +       Dprst_area_clos = Dprst_area_clos_max
        IF ( Dprst_area_clos<NEARZERO ) Dprst_area_clos = 0.0
      ENDIF

      ! evaporate water from depressions based on snowcov_area
      ! dprst_evap_open & dprst_evap_clos = inches-acres on the HRU
      dprst_evap_clos = 0.0
      dprst_evap_open = 0.0
      dprst_evap_vol = Potet*(1.0-Snowcov_area)*(Dprst_area_open
     +               + Dprst_area_clos)
      IF ( dprst_evap_vol<0.0 ) PRINT*, 'dprstevap<0', dprst_evap_vol
      Dprst_evap_hru = MIN(Avail_et, dprst_evap_vol/Hruarea)
      IF ( Dprst_evap_hru<NEARZERO ) THEN
        Dprst_evap_hru = 0.0
        dprst_evap_vol = 0.0
      ELSE
        IF ( Dprst_vol_clos>DNEARZERO )
     +       dprst_evap_clos = Dprst_evap_hru*Dprst_frac_clos_hru
        IF ( Dprst_vol_open>DNEARZERO )
     +       dprst_evap_open = Dprst_evap_hru - dprst_evap_clos
        IF ( dprst_evap_open>NEARZERO ) THEN
          dprst_evap_open = dprst_evap_open*Hruarea
          IF ( Dprst_vol_open>dprst_evap_open ) THEN
            Dprst_vol_open = Dprst_vol_open - dprst_evap_open
          ELSE
            dprst_evap_open = Dprst_vol_open
            Dprst_vol_open = 0.0D0
          ENDIF
        ELSE
          dprst_evap_open = 0.0
        ENDIF
        IF ( dprst_evap_clos>0.0 ) THEN
        !rsr, increase possible ET demand from closed if column
        !     could be increasing excessively
          dprst_evap_clos = dprst_evap_clos*Hruarea
          IF ( clos_vol_r>1.0 ) THEN
            dprst_evap_clos = dprst_evap_clos*clos_vol_r
            IF ( clos_vol_r>1.20 )
     +         PRINT *, 'Closed depression built column over 120% full',
     +              clos_vol_r, ' for HRU:', Ihru, Dprst_vol_clos_max,
     +              Dprst_vol_clos, dprst_evap_clos
            dprst_evap_clos = MIN(Avail_et*Hruarea-dprst_evap_open,
     +                            dprst_evap_clos)
          ENDIF
          IF ( Dprst_vol_clos>dprst_evap_clos ) THEN
            Dprst_vol_clos = Dprst_vol_clos - dprst_evap_clos
          ELSE
            dprst_evap_clos = Dprst_vol_clos
            Dprst_vol_clos = 0.0D0
          ENDIF
        ENDIF
      ENDIF

      Dprst_evap_hru = (dprst_evap_clos + dprst_evap_open)/Hruarea
      IF ( Dprst_evap_hru>Avail_et ) THEN
        ! ET from canopy, snow, and depressions exceeds Potet
        !    reduce depression ET, first from closed, then open
        dprst_et = Avail_et - Dprst_evap_hru
        PRINT *, 'Depression ET > available', dprst_et, Dprst_evap_hru,
     +           dprst_evap_open, dprst_evap_clos, Avail_et,
     +           Dprst_vol_clos, Dprst_vol_clos_max,Dprst_area_clos_max,
     +           Dprst_area_open_max, Dprst_area_clos
        IF ( dprst_evap_clos>NEARZERO ) THEN
          tmp = dprst_et*Hruarea
          dprst_evap_clos = dprst_evap_clos + tmp
          IF ( dprst_evap_clos<0.0 ) THEN
!            PRINT *, 'dprst_evap_clos<0', dprst_evap_clos
            dprst_et = dprst_evap_clos/Hruarea
            Dprst_vol_clos = Dprst_vol_clos - tmp + dprst_evap_clos
            dprst_evap_clos = 0.0
          ELSE
            Dprst_vol_clos = Dprst_vol_clos - dprst_et*Hruarea
            dprst_et = 0.0
          ENDIF
          IF ( Dprst_vol_clos<DNEARZERO ) THEN
!            PRINT *, 'too much closed evap', Dprst_vol_clos
            dprst_evap_clos = dprst_evap_clos + Dprst_vol_clos
            Dprst_vol_clos = 0.0D0
            dprst_et = 0.0
          ENDIF
        ENDIF
        IF ( dprst_et<0.0 ) THEN
          IF ( dprst_evap_open>NEARZERO ) THEN
            tmp = dprst_et*Hruarea
            dprst_evap_open = dprst_evap_open + tmp
            IF ( dprst_evap_open<0.0 ) THEN
              PRINT *, 'dprst_evap_open<0', dprst_evap_open
              Dprst_vol_open = Dprst_vol_open - tmp + dprst_evap_open
              dprst_evap_open = 0.0
            ELSE
              Dprst_vol_open = Dprst_vol_open - dprst_evap_open
            ENDIF
            IF ( Dprst_vol_open<DNEARZERO ) THEN
!              PRINT *, 'too much open evap', Dprst_vol_open
              dprst_evap_open = dprst_evap_open + Dprst_vol_open
              Dprst_vol_open = 0.0
            ENDIF
          ENDIF
        ELSE
          PRINT *, 'water balance DPRST evap problem', dprst_et
        ENDIF
        Dprst_evap_hru = (dprst_evap_clos + dprst_evap_open)/Hruarea
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
        IF ( Dprst_vol_open>Dprst_vol_thres_open ) THEN
          IF ( Dprst_vol_open>Dprst_vol_open_max ) THEN
            Dprst_sroff_hru = Dprst_vol_open - Dprst_vol_open_max
            IF ( Dprst_vol_open_max>Dprst_vol_thres_open )
     +           Dprst_sroff_hru = Dprst_sroff_hru + Dprst_flow_coef*
     +                         (Dprst_vol_open_max-Dprst_vol_thres_open)
          ELSE
            Dprst_sroff_hru = Dprst_flow_coef*
     +                            (Dprst_vol_open-Dprst_vol_thres_open)
          ENDIF
          Dprst_vol_open = Dprst_vol_open - Dprst_sroff_hru
          IF ( Dprst_vol_open<0.0D0 ) THEN
            Dprst_sroff_hru = Dprst_sroff_hru + Dprst_vol_open
            Dprst_vol_open = 0.0D0
          ENDIF
          Dprst_sroff_hru = Dprst_sroff_hru/Hruarea
        ENDIF
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

      END SUBROUTINE dprst_comp
