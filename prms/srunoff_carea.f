!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a linear
! variable-source-area method allowing for cascading flow
!
!     version: 2.2 added cascading flow for infiltration and runoff
! rsr, 10/30/2008 added depression storage code
! rsr, 04/11/2011 changed so dprst_area to be a parameter (does not change)
! rsr, 11/1/2008, routines imperv_et, check_capacity, imperv_sroff,
!                          dprst_comp in smidx only
!***********************************************************************
      MODULE PRMS_SRUNOFF_CAREA
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: BALUNT
      CHARACTER(LEN=13), SAVE :: MODNAME
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_thres_open(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open_max(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_clos_max(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_clos_hru(:)
      REAL, SAVE, ALLOCATABLE :: Carea_dif(:)
      REAL, SAVE :: Srp, Sri
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sroff_down, Basin_sroff_upslope
      DOUBLE PRECISION, SAVE :: Basin_sroffi, Basin_sroffp
      REAL, SAVE, ALLOCATABLE :: Imperv_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sroffp(:), Hru_sroffi(:)
      REAL, SAVE, ALLOCATABLE :: Upslope_hortonian(:), Hortonian_flow(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Carea_min(:)
!   Declared Parameters for Depression Storage
      REAL, SAVE, ALLOCATABLE :: Sro_to_dprst(:)
      REAL, SAVE, ALLOCATABLE :: Va_clos_exp(:), Va_open_exp(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_flow_coef(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_init(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_open(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_clos(:)
!   Declared Variables for Depression Storage
      DOUBLE PRECISION, SAVE :: Basin_dprst_sroff
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open(:), Dprst_area_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_sroff_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_insroff_hru(:)
      END MODULE PRMS_SRUNOFF_CAREA

!***********************************************************************
!     Main srunoff_carea routine
!***********************************************************************
      INTEGER FUNCTION srunoff_carea()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srunoffcareadecl, srunoffcareainit
      INTEGER, EXTERNAL :: srunoffcarearun
!***********************************************************************
      srunoff_carea = 0

      IF ( Process(:3)=='run' ) THEN
        srunoff_carea = srunoffcarearun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        srunoff_carea = srunoffcareadecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        srunoff_carea = srunoffcareainit()
      ENDIF

      END FUNCTION srunoff_carea

!***********************************************************************
!     srunoffcareadecl - set up parameters for surface runoff computations
!   Declared Parameters
!     carea_min, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max, soil_rechr_max
!***********************************************************************
      INTEGER FUNCTION srunoffcareadecl()
      USE PRMS_SRUNOFF_CAREA
      USE PRMS_MODULE, ONLY: Model, Dprst_flag, Nhru
      USE PRMS_CASCADE, ONLY: Cascade_flag
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declvar, declparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: n, nc
      CHARACTER(LEN=80), SAVE :: Version_srunoff_carea
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Surface Runoff'
!***********************************************************************
      srunoffcareadecl = 0

      Version_srunoff_carea =
     +'$Id: srunoff_carea.f 5232 2013-01-15 18:55:17Z rsregan $'
      nc = INDEX( Version_srunoff_carea, 'Z' )
      n = INDEX( Version_srunoff_carea, '.f' ) + 1
      IF ( declmodule(Version_srunoff_carea(6:n), PROCNAME,
     +     Version_srunoff_carea(n+2:nc))/=0 ) STOP
      MODNAME = 'srunoff_carea'

! srunoff variables
      ALLOCATE ( Imperv_evap(Nhru) )
      IF ( declvar(MODNAME, 'imperv_evap', 'nhru', Nhru, 'real',
     +     'Evaporation from impervious area for each HRU',
     +     'inches', Imperv_evap)/=0 ) CALL read_error(3, 'imperv_evap')

      IF ( declvar(MODNAME, 'basin_sroffi', 'one', 1, 'double',
     +     'Basin area-weighted average surface runoff from'//
     +     ' impervious areas',
     +     'inches', Basin_sroffi)/=0 )
     +      CALL read_error(3, 'basin_sroffi')

      IF ( declvar(MODNAME, 'basin_sroffp', 'one', 1, 'double',
     +     'Basin area-weighted average surface runoff from pervious'//
     +     ' areas',
     +     'inches', Basin_sroffp)/=0 )
     +     CALL read_error(3, 'basin_sroffp')

      ALLOCATE ( Hru_sroffp(Nhru) )
      IF ( declvar(MODNAME, 'hru_sroffp', 'nhru', Nhru, 'real',
     +     'Surface runoff from pervious areas for each HRU',
     +     'inches', Hru_sroffp)/=0 )
     +     CALL read_error(3, 'hru_sroffp')

      ALLOCATE ( Hru_sroffi(Nhru) )
      IF ( declvar(MODNAME, 'hru_sroffi', 'nhru', Nhru, 'real',
     +     'Surface runoff from impervious areas for each HRU',
     +     'inches', Hru_sroffi)/=0 )
     +     CALL read_error(3, 'hru_sroffi')

! Depression storage variables
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'basin_dprst_sroff', 'one', 1, 'double',
     +       'Basin area-weighted average surface runoff from'//
     +       ' open surface depression storage',
     +       'inches', Basin_dprst_sroff)/=0 )
     +       CALL read_error(3, 'basin_dprst_sroff')

        ALLOCATE ( Dprst_vol_open(Nhru) )
        IF ( declvar(MODNAME, 'dprst_vol_open', 'nhru', Nhru,'double',
     +       'Storage volume in open surface depressions for each HRU',
     +       'acre-inches',
     +       Dprst_vol_open)/=0 ) CALL read_error(3, 'dprst_vol_open')

        ALLOCATE ( Dprst_vol_clos(Nhru) )
        IF ( declvar(MODNAME, 'dprst_vol_clos', 'nhru', Nhru,'double',
     +      'Storage volume in closed surface depressions for each HRU',
     +      'acre-inches',
     +      Dprst_vol_clos)/=0 ) CALL read_error(3, 'dprst_vol_clos')

        ALLOCATE ( Dprst_sroff_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_sroff_hru', 'nhru', Nhru, 'real',
     +       'Surface runoff from open surface depression storage for'//
     +       ' each HRU',
     +       'inches', Dprst_sroff_hru)/=0 )
     +       CALL read_error(3, 'dprst_sroff_hru')

        ALLOCATE ( Dprst_insroff_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_insroff_hru', 'nhru', Nhru,
     +       'real',
     +       'Surface runoff from pervious and impervious portions'//
     +       ' into surface depression storage for each HRU',
     +       'inches', Dprst_insroff_hru)/=0 )
     +       CALL read_error(3, 'dprst_insroff_hru')

        ALLOCATE ( Dprst_area_open(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_open', 'nhru', Nhru, 'real',
     +       'Surface area of open surface depressions based on'//
     +       ' volume for each HRU',
     +       'acres', Dprst_area_open)/=0 )
     +       CALL read_error(3, 'dprst_area_open')

        ALLOCATE ( Dprst_area_clos(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_clos', 'nhru', Nhru, 'real',
     +       'Surface area of closed surface depressions based on'//
     +       ' volume for each HRU', 'acres', Dprst_area_clos)/=0 )
     +       CALL read_error(3, 'dprst_area_clos')
      ENDIF

      ALLOCATE ( Hortonian_flow(Nhru) )
      IF ( declvar(MODNAME, 'hortonian_flow', 'nhru', Nhru, 'real',
     +  'Hortonian surface runoff reaching stream network for each HRU',
     +     'inches', Hortonian_flow)/=0 )
     +     CALL read_error(3, 'hortonian_flow')

! cascading variables and parameters
      ALLOCATE ( Upslope_hortonian(Nhru) )
      IF ( Cascade_flag==1 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'upslope_hortonian', 'nhru', Nhru,
     +       'real',
     +       'Hortonian surface runoff received from upslope HRUs',
     +       'inches', Upslope_hortonian)/=0 )
     +       CALL read_error(3, 'upslope_hortonian')

        IF ( declvar(MODNAME, 'basin_sroff_down', 'one', 1, 'double',
     +       'Basin area-weighted average of cascading surface runoff',
     +       'inches',
     +       Basin_sroff_down)/=0 )
     +       CALL read_error(3, 'basin_sroff_down')

        IF ( declvar(MODNAME, 'basin_sroff_upslope', 'one',1,'double',
     +       'Basin area-weighted average of cascading surface runoff'//
     +       ' received from upslope HRUs',
     +       'inches',
     +       Basin_sroff_upslope)/=0 )
     +       CALL read_error(3, 'basin_sroff_upslope')
      ENDIF

! Allocate arrays for local variables and variables from other modules
      ALLOCATE ( Carea_min(Nhru), Carea_dif(Nhru) )
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Dprst_flow_coef(Nhru), Sro_to_dprst(Nhru) )
        ALLOCATE ( Dprst_seep_rate_open(Nhru) )
        ALLOCATE ( Dprst_seep_rate_clos(Nhru) )
        ALLOCATE ( Va_open_exp(Nhru), Va_clos_exp(Nhru) )
        ALLOCATE ( Dprst_frac_clos_hru(Nhru) )
        ALLOCATE ( Dprst_vol_thres_open(Nhru) )
        ALLOCATE ( Dprst_vol_open_max(Nhru), Dprst_vol_clos_max(Nhru) )
      ENDIF

! Declare parameters
      IF ( declparam(MODNAME, 'carea_min', 'nhru', 'real',
     +     '0.2', '0.0', '1.0',
     +     'Minimum contributing area',
     +     'Minimum possible area contributing to surface runoff'//
     +     ' expressed as a portion of the area for each HRU',
     +     'decimal fraction')/=0 ) CALL read_error(1, 'carea_min')

! Depression Storage parameters:
      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'dprst_flow_coef', 'nhru', 'real',
     +       '0.05', '0.0001', '1.0',
     +       'Coefficient in linear flow routing equation for open'//
     +       ' surface depressions',
     +       'Coefficient in linear flow routing equation for open'//
     +       ' surface depressions',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_flow_coef')
        IF ( declparam(MODNAME, 'dprst_seep_rate_open', 'nhru',
     +       'real',
     +       '0.02', '0.0001', '1.0',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' open surface depressions',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' open surface depressions',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_seep_rate_open')
        IF ( declparam(MODNAME, 'dprst_seep_rate_clos', 'nhru',
     +       'real',
     +       '0.02', '0.0001', '1.0',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' closed surface depressions',
     +       'Coefficient used in linear seepage flow equation for'//
     +       ' closed surface depressions',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_seep_rate_clos')
        IF ( declparam(MODNAME, 'sro_to_dprst', 'nhru', 'real',
     +       '0.2', '0.0', '1.0',
     +       'Fraction of pervious and impervious surface runoff that'//
     +       ' flows into surface depression storage',
     +       'Fraction of pervious and impervious surface runoff that'//
     +       ' flows into surface depression storage; the remainder'//
     +       ' flows to a stream network',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'sro_to_dprst')
        ALLOCATE ( Dprst_frac_init(Nhru) )
        IF ( declparam(MODNAME, 'dprst_frac_init', 'nhru', 'real',
     +       '0.5', '0.0', '1.0',
     +       'Fraction of maximum storage that contains water at the'//
     +       ' start of a simulation',
     +       'Fraction of maximum surface depression storage that'//
     +       ' contains water at the start of a simulation',
     +       'decimal fraction')/=0 )
     +       CALL read_error(1, 'dprst_frac_init')
        IF ( declparam(MODNAME, 'va_open_exp', 'nhru', 'real',
     +       '1.0', '0.0001', '10.0',
     +       'Coefficient in the exponential equation to compute'//
     +       ' current surface area of open surface depressions',
     +       'Coefficient in the exponential equation relating'//
     +       ' maximum surface area to the fraction that open'//
     +       ' depressions are full to compute current surface area',
     +       'none')/=0 ) CALL read_error(1, 'va_open_exp')
        IF ( declparam(MODNAME, 'va_clos_exp', 'nhru', 'real',
     +       '1.0', '0.0001', '10.0',
     +       'Coefficient in the exponential equation to compute'//
     +       ' current surface area of closed surface depressions',
     +       'Coefficient in the exponential equation relating'//
     +       ' maximum surface area to the fraction that closed'//
     +       ' depressions are full to compute current surface area',
     +       'none')/=0 ) CALL read_error(1, 'va_clos_exp')
      ENDIF

      END FUNCTION srunoffcareadecl

!***********************************************************************
!     srunoffcareainit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srunoffcareainit()
      USE PRMS_SRUNOFF_CAREA
      USE PRMS_MODULE, ONLY: Dprst_flag, Nhru, Print_debug,
     +    Inputerror_flag, Parameter_check_flag
      USE PRMS_CASCADE, ONLY: Cascade_flag
      USE PRMS_BASIN, ONLY: Dprst_area_max, DNEARZERO, Timestep,
     +    Dprst_area_clos_max, Dprst_area_open_max, Basin_area_inv,
     +    Hru_area, Active_hrus, Hru_route_order, NEARZERO,
     +    Op_flow_thres, Dprst_depth_avg
      USE PRMS_FLOWVARS, ONLY: Carea_max, Hru_hortonian_cascadeflow,
     +    Basin_dprst_volop, Basin_dprst_volcl
      IMPLICIT NONE
      INTRINSIC EXP, LOG
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error, PRMS_open_module_file
! Local Variables
      INTEGER :: i, ii, j, ierr
      DOUBLE PRECISION :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r
!***********************************************************************
      srunoffcareainit = 0

      IF ( Print_debug==1 ) THEN
        CALL PRMS_open_module_file(BALUNT, 'srunoff_carea.wbal')
        IF ( Cascade_flag==1 ) THEN
          WRITE ( BALUNT, 9002 )
        ELSE
          WRITE ( BALUNT, 9001 )
        ENDIF
      ENDIF

      IF ( Timestep==0 ) THEN
        Imperv_evap = 0.0
        Upslope_hortonian = 0.0
        Hortonian_flow = 0.0
        IF ( Dprst_flag==1 ) THEN
          Dprst_sroff_hru = 0.0
          Dprst_insroff_hru = 0.0
        ENDIF
        Hru_sroffi = 0.0
        Hru_sroffp = 0.0
        Basin_sroffi = 0.0D0
        Basin_sroffp = 0.0D0
        Basin_dprst_sroff = 0.0D0
        IF ( Cascade_flag==1 ) THEN
          Basin_sroff_upslope = 0.0D0
          Basin_sroff_down = 0.0D0
        ENDIF
      ENDIF

! Carea parameters
      IF ( getparam(MODNAME, 'carea_min', Nhru, 'real', Carea_min)
     +     /=0 ) CALL read_error(2, 'carea_min')

! Depression Storage parameters and variables:
      IF ( Dprst_flag==1 ) THEN
        IF ( getparam(MODNAME, 'dprst_flow_coef', Nhru, 'real',
     +       Dprst_flow_coef)/=0 )
     +       CALL read_error(2, 'dprst_flow_coef')
        IF ( getparam(MODNAME, 'dprst_seep_rate_open', Nhru, 'real',
     +       Dprst_seep_rate_open)/=0 )
     +       CALL read_error(2, 'dprst_seep_rate_open')
        IF ( getparam(MODNAME, 'dprst_seep_rate_clos', Nhru, 'real',
     +       Dprst_seep_rate_clos)/=0 )
     +       CALL read_error(2, 'dprst_seep_rate_clos')
        IF ( getparam(MODNAME, 'dprst_frac_init', Nhru, 'real',
     +       Dprst_frac_init)/=0 ) CALL read_error(2, 'dprst_frac_init')
        IF ( getparam(MODNAME, 'va_open_exp', Nhru, 'real',
     +       Va_open_exp)/=0 ) CALL read_error(2, 'va_open_exp')
        IF ( getparam(MODNAME, 'va_clos_exp', Nhru, 'real',
     +       Va_clos_exp)/=0 ) CALL read_error(2, 'va_clos_exp')
        IF ( getparam(MODNAME, 'sro_to_dprst', Nhru, 'real',
     +       Sro_to_dprst)/=0 ) CALL read_error(2, 'sro_to_dprst')

        Dprst_vol_clos_max = 0.0D0
        Dprst_vol_open_max = 0.0D0
        IF ( Timestep==0 ) THEN
          Dprst_area_open = 0.0
          Dprst_area_clos = 0.0
          Dprst_vol_open = 0.0D0
          Dprst_vol_clos = 0.0D0
          Dprst_frac_clos_hru = 0.0
        ENDIF
        Dprst_vol_thres_open = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
!******************************************************************
! Following lines added for Depression Storage:
          ierr = 0
          IF ( Dprst_frac_init(i)>1.0 .OR. Dprst_frac_init(i)<0.0 )
     +         THEN
            IF ( Parameter_check_flag==1 ) THEN
              PRINT *, 'ERROR, dprst_frac_init < 0.0 or > 1.0 for HRU:',
     +                 i, Dprst_frac_init(i)
              ierr = 1
            ELSEIF ( Dprst_frac_init(i)>1.0 ) THEN
              PRINT *, 'dprst_frac_init > 1.0, set to 1.0 for HRU:', i
              Dprst_frac_init(i) = 1.0
            ELSE
              PRINT *, 'dprst_frac_init < 0.0, set to 0.0 for HRU:', i
              Dprst_frac_init(i) = 0.0
            ENDIF
          ENDIF
          IF ( Sro_to_dprst(i)>1.0 .OR. Sro_to_dprst(i)<0.0 ) THEN
            PRINT *, 'ERROR, sro_to_dprst < 0.0 or > 1.0 for HRU:', i,
     +               Sro_to_dprst(i)
            ierr = 1
          ENDIF
          IF ( Dprst_flow_coef(i)<0.0 ) THEN
            PRINT *, 'ERROR, dprst_flow_coef < 0.0 for HRU:', i,
     +               Dprst_flow_coef(i)
            ierr = 1
          ENDIF
          IF ( Dprst_seep_rate_open(i)<0.0 ) THEN
            PRINT *, 'ERROR, dprst_seep_rate_open < 0.0 for HRU:', i,
     +               Dprst_seep_rate_open(i)
            ierr = 1
          ENDIF
          IF ( Dprst_seep_rate_clos(i)<0.0 ) THEN
            PRINT *, 'ERROR, dprst_seep_rate_clos < 0.0 for HRU:', i,
     +               Dprst_seep_rate_clos(i)
            ierr = 1
          ENDIF
          IF ( Op_flow_thres(i)>1.0 .OR. Op_flow_thres(i)<0.0 ) THEN
            PRINT *, 'ERROR, op_flow_thres < 0.0 or > 1.0 for HRU:', i,
     +               Op_flow_thres(i)
            ierr = 1
          ENDIF
          IF ( ierr==1 ) THEN
            Inputerror_flag = 1
            CYCLE
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
              IF ( open_vol_r<DNEARZERO ) THEN
                frac_op_ar = 0.0D0
              ELSEIF ( open_vol_r>1.0D0 ) THEN
                frac_op_ar = 1.0D0
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
              IF ( clos_vol_r<DNEARZERO ) THEN
                frac_cl_ar = 0.0D0
              ELSEIF ( clos_vol_r>1.0D0 ) THEN
                frac_cl_ar = 1.0D0
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
        DEALLOCATE ( Dprst_frac_init, Op_flow_thres, Dprst_depth_avg )
        Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
        Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
!******************************************************************
      ENDIF

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        Carea_dif(i) = Carea_max(i) - Carea_min(i)
      ENDDO

 9001 FORMAT ('    Date     Water Bal     Robal      Sroff',
     +        '   Infiltrat Impervevap Impervstor Dprst_evap',
     +        ' Dprst_seep')
 9002 FORMAT ('    Date     Water Bal     Robal      Sroff   Sroffdown',
     +        '   Srofflake Infiltrat Impervevap Impervstor    Farflow',
     +        ' Dprst_evap Dprst_seep Dprst_srof')

      END FUNCTION srunoffcareainit

!***********************************************************************
!     srunoffcarearun - Computes surface runoff using contributing area
!                       computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srunoffcarearun()
      USE PRMS_SRUNOFF_CAREA
      USE PRMS_MODULE, ONLY: Dprst_flag, Nhru, Print_debug
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, DNEARZERO,
     +    Hru_perv, Hru_imperv, Hru_frac_imperv, Hru_frac_perv,
     +    NEARZERO, Dprst_area_max, Hru_area, Hru_type, Basin_area_inv,
     +    Dprst_area_clos_max, Dprst_area_open_max, Dprst_frac_hru,
     +    Dprst_frac_open, Dprst_frac_clos
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Hru_impervevap,
     +    Sroff, Basin_sroff, Infil, Soil_rechr_max, Imperv_stor_max,
     +    Hru_impervstor, Basin_imperv_evap, Basin_imperv_stor,
     +    Basin_infil, Hru_hortonian_cascadeflow, Basin_hortonian_lakes,
     +    Strm_seg_in, Strm_farfield, Hortonian_lakes, Snowinfil_max,
     +    Soil_moist, Basin_hortonian, Basin_sroff_farflow, Imperv_stor,
     +    Basin_dprst_volop, Basin_dprst_volcl, Basin_dprst_evap,
     +    Basin_dprst_seep, Basin_dprst_wb, Hru_intcpevap,
     +    Dprst_evap_hru, Dprst_seep_hru, Soil_rechr, Pkwater_equiv
      USE PRMS_CASCADE, ONLY: Ncascade_hru, Cascade_flag
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_INTCP, ONLY: Net_rain, Net_snow, Net_ppt
      USE PRMS_SNOW, ONLY: Pptmix_nopack, Snow_evap,
     +    Snowcov_area, Snowmelt
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_et, compute_infil_carea, run_cascade_carea
      EXTERNAL dprst_comp, perv_sroff_carea
! Local Variables
      INTEGER :: i, k
      REAL :: srunoff, robal, hru_sroff_down, farflow, avail_et
      REAL :: harea, himperv, hperv, last_stor
      DOUBLE PRECISION :: last_dprst_stor, basin_robal, new_dprst_stor
      DOUBLE PRECISION :: runoff, tmp1, dprst_hru_wb
      REAL :: perv_frac, imperv_frac, dprst_in
!***********************************************************************
      srunoffcarearun = 0

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
          CALL compute_infil_carea(Pptmix_nopack(i), Carea_min(i),
     +         Carea_dif(i), Soil_moist(i), Soil_moist_max(i),
     +         Soil_rechr(i), Soil_rechr_max(i), Net_rain(i),
     +         Net_ppt(i), himperv, Imperv_stor(i), Imperv_stor_max(i),
     +         Snowmelt(i), Snowinfil_max(i), Net_snow(i),
     +         Pkwater_equiv(i), Infil(i), Hru_type(i),
     +         Upslope_hortonian(i))

          avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)

!         ******Compute the depression storage component
!         only call if total depression surface area for each HRU is > 0.0
          IF ( Dprst_flag==1 ) THEN
            ! last depression storage on whole HRU
            last_dprst_stor = 0.0D0
            IF ( Dprst_vol_open(i)>DNEARZERO )
     +            last_dprst_stor = Dprst_vol_open(i)/harea
            IF ( Dprst_vol_clos(i)>DNEARZERO )
     +           last_dprst_stor = last_dprst_stor +
     +           Dprst_vol_clos(i)/harea
            dprst_in = 0.0
            IF ( Dprst_area_max(i)>NEARZERO ) THEN
              tmp1 = 0.0
!       add the hortonian flow and snowmelt to the depression storage volumes:
              IF ( Dprst_area_open_max(i)>NEARZERO ) THEN
                dprst_in = Snowmelt(i) + Upslope_hortonian(i)
                IF ( Pptmix_nopack(i)==1 ) THEN
                  dprst_in = dprst_in + Net_rain(i)
                ELSEIF ( Snowmelt(i)<NEARZERO ) THEN
                  IF ( Net_snow(i)<NEARZERO .AND. Net_rain(i)>0.0 ) THEN
                    IF ( Pkwater_equiv(i)<DNEARZERO )
     +                   dprst_in = dprst_in + Net_rain(i)
                  ENDIF
                ENDIF
                dprst_in = dprst_in*Dprst_area_open_max(i)
                Dprst_vol_open(i) = Dprst_vol_open(i) + dprst_in
              ENDIF
              IF ( Dprst_area_clos_max(i)>NEARZERO ) THEN
                tmp1 = Snowmelt(i) + Upslope_hortonian(i)
                IF ( Pptmix_nopack(i)==1 ) THEN
                  tmp1 = tmp1 + Net_rain(i)
                ELSEIF ( Snowmelt(i)<NEARZERO ) THEN
                  IF ( Net_snow(i)<NEARZERO .AND. Net_rain(i)>0.0 ) THEN
                    IF ( Pkwater_equiv(i)<DNEARZERO )
     +                   tmp1 = tmp1 + Net_rain(i)
                  ENDIF
                ENDIF
                tmp1 = tmp1*Dprst_area_clos_max(i)
                Dprst_vol_clos(i) = Dprst_vol_clos(i) + tmp1
                dprst_in = dprst_in + tmp1
              ENDIF
              CALL dprst_comp(Dprst_vol_clos_max(i), Dprst_vol_clos(i),
     +             Dprst_area_clos_max(i), Dprst_area_clos(i),
     +             Dprst_vol_open_max(i), Dprst_vol_open(i),
     +             Dprst_area_open_max(i), Dprst_area_open(i),
     +             Dprst_flow_coef(i), Dprst_sroff_hru(i),
     +             Dprst_seep_rate_open(i), Dprst_seep_rate_clos(i),
     +             Dprst_seep_hru(i), Va_open_exp(i), Va_clos_exp(i),
     +             Sro_to_dprst(i), Snowcov_area(i), Dprst_evap_hru(i),
     +             Dprst_frac_clos_hru(i), Dprst_insroff_hru(i),
     +             Dprst_vol_thres_open(i), perv_frac, imperv_frac,
     +             harea, avail_et, Potet(i), i, Dprst_frac_open(i),
     +             Dprst_frac_clos(i))
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
          runoff = runoff + Srp*hperv + Sri*himperv
          srunoff = runoff/harea

!******Compute HRU weighted average (to units of inches/dt)
          hru_sroff_down = 0.0
          farflow = 0.0

          IF ( Hru_type(i)==1 ) THEN
            IF ( Cascade_flag==1 ) THEN
              IF ( Ncascade_hru(i)>0 .AND. srunoff>0.0 )
     +             CALL run_cascade_carea(i, Ncascade_hru(i), srunoff,
     +                  hru_sroff_down, farflow)
              Hru_hortonian_cascadeflow(i) = hru_sroff_down + farflow
              Basin_sroff_upslope = Basin_sroff_upslope +
     +                              Upslope_hortonian(i)*harea
              Basin_sroff_down = Basin_sroff_down +
     +                           hru_sroff_down*harea
              Basin_sroff_farflow = Basin_sroff_farflow + farflow*harea
            ENDIF
            Basin_hortonian = Basin_hortonian + srunoff*harea
            Basin_sroff = Basin_sroff + srunoff*harea
          ENDIF

          Basin_infil = Basin_infil + Infil(i)*hperv

!******Compute evaporation from impervious area
          IF ( himperv>NEARZERO ) THEN
            IF ( Imperv_stor(i)>0.0 ) THEN
              CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i),
     +             Snowcov_area(i), imperv_frac, avail_et)
              Hru_impervevap(i) = Imperv_evap(i)*imperv_frac
              avail_et = avail_et - Hru_impervevap(i)
              IF ( avail_et<0.0 ) THEN
                ! sanity check
!                IF ( avail_et<-1.0E-5 )
!     +               PRINT*, 'avail_et<0 in srunoff imperv', i,
!     +                       Nowmonth, Nowday, avail_et
                Hru_impervevap(i) = Hru_impervevap(i) + avail_et
                IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
                Imperv_evap(i) = Hru_impervevap(i)/imperv_frac
                Imperv_stor(i) = Imperv_stor(i) - avail_et/imperv_frac
              ENDIF
            ENDIF
            Basin_imperv_evap = Basin_imperv_evap +
     +                          Hru_impervevap(i)*harea
            Hru_impervstor(i) = Imperv_stor(i)*imperv_frac
            Basin_imperv_stor = Basin_imperv_stor +
     +                          Imperv_stor(i)*himperv
          ENDIF

          Hru_sroffp(i) = Srp*perv_frac
          Hru_sroffi(i) = Sri*imperv_frac
          Basin_sroffp = Basin_sroffp + Srp*hperv
          Basin_sroffi = Basin_sroffi + Sri*himperv

          IF ( Dprst_flag==1 ) THEN
            new_dprst_stor = 0.0D0
            dprst_hru_wb = 0.0
            IF ( Dprst_area_max(i)>NEARZERO ) THEN
              IF ( Dprst_vol_open(i)>DNEARZERO ) new_dprst_stor =
     +             Dprst_vol_open(i)/harea
              IF ( Dprst_vol_clos(i)>DNEARZERO ) new_dprst_stor =
     +             new_dprst_stor + Dprst_vol_clos(i)/harea
              dprst_hru_wb = last_dprst_stor - new_dprst_stor
     +                       - Dprst_seep_hru(i) - Dprst_sroff_hru(i)
     +                       - Dprst_evap_hru(i) + Dprst_insroff_hru(i)
     +                       + dprst_in/harea
              Basin_dprst_wb = Basin_dprst_wb + dprst_hru_wb*harea
            ENDIF
          ENDIF

          IF ( Print_debug==1 ) THEN
            robal = Snowmelt(i) - srunoff !includes dprst runoff, if any
     +              - Infil(i)*perv_frac - Hru_impervevap(i)
     +              + (last_stor-Imperv_stor(i))*imperv_frac
            IF ( Net_ppt(i)>0.0 ) THEN
              IF ( Pptmix_nopack(i)==1 ) THEN
                robal = robal + Net_rain(i)
              ELSEIF ( Snowmelt(i)<=0.0 .AND.
     +                 Pkwater_equiv(i)<DNEARZERO) THEN
                IF ( Snow_evap(i)<NEARZERO ) THEN
                  robal = robal + Net_ppt(i)
                ELSEIF ( Net_snow(i)<NEARZERO ) THEN
                  robal = robal + Net_ppt(i)
                ENDIF
              ENDIF
            ENDIF
            IF ( Cascade_flag==1 ) robal = robal + Upslope_hortonian(i)
     +                                     - hru_sroff_down - farflow
            IF ( Dprst_flag==1 ) robal = robal + last_dprst_stor -
     +           new_dprst_stor - Dprst_seep_hru(i) - Dprst_evap_hru(i)
            basin_robal = basin_robal + robal
            IF ( ABS(robal)>1.0D-4 ) THEN
              IF ( Dprst_flag==1 ) THEN
              WRITE( BALUNT, * ) 'dprst',last_dprst_stor,new_dprst_stor,
     +               Dprst_seep_hru(i), Dprst_evap_hru(i),
     +               Dprst_sroff_hru(i), Snowmelt(i), Net_rain(i),
     +               Dprst_frac_hru(i), Dprst_insroff_hru(i),
     +               Upslope_hortonian(i)
              WRITE( BALUNT, * ) dprst_hru_wb, Dprst_vol_open(i),
     + Dprst_vol_clos(i), (Dprst_vol_open(i)+Dprst_vol_clos(i))/harea,
     + Dprst_area_max(i), runoff, srunoff, Pkwater_equiv(i),
     + Dprst_area_clos(i), Snowcov_area(i), dprst_in, dprst_in/harea
              WRITE( BALUNT, * ) robal, Net_rain(i), Net_ppt(i),
     +                           Dprst_insroff_hru(i),
     + Net_rain(i)*Dprst_frac_hru(i),Snowmelt(i), Dprst_frac_hru(i)
              ENDIF
              IF ( ABS(robal)>5.0D-4 ) THEN
                WRITE ( BALUNT, * ) 'possible HRU water balance ERROR'
              ELSE
                WRITE ( BALUNT, * ) 'HRU robal rounding issue'
              ENDIF
              IF ( Cascade_flag==1 ) THEN
                WRITE ( BALUNT, '(2I3,I6,18F10.6,I3)' ) Nowmonth,
     +                 Nowday, i, robal, Snowmelt(i),
     +                 Upslope_hortonian(i), last_stor,
     +                 hru_sroff_down, Infil(i), srunoff,
     +                 Imperv_stor(i), Imperv_evap(i), Net_ppt(i),
     +                 Pkwater_equiv(i), Snow_evap(i), Net_snow(i),
     +                 farflow, Net_rain(i), Srp, Sri, runoff,
     +                 Pptmix_nopack(i)
              ELSE
                WRITE ( BALUNT,'(2I3,I4,16F10.7,I5)' ) Nowmonth, Nowday,
     +                 i, robal, Snowmelt(i), last_stor, Infil(i),
     +                 srunoff, Imperv_stor(i), Imperv_evap(i),
     +                 Hru_impervevap(i), imperv_frac, Net_ppt(i),
     +                 Pkwater_equiv(i), Snow_evap(i), Net_snow(i),
     +                 Net_rain(i), Srp, Sri, Pptmix_nopack(i)
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
          IF ( Infil(i)+Sroff(i)+Imperv_stor(i)+Imperv_evap(i)>0.0 )
     +         PRINT *, 'carea lake ERROR', Infil(i), Sroff(i),
     +                  Imperv_stor(i), Imperv_evap(i), i
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
        robal = Basin_sroff - Basin_sroffp - Basin_sroffi
     +          - Basin_dprst_sroff
        IF ( Cascade_flag==1 ) THEN
          robal = robal + Basin_sroff_down + Basin_sroff_farflow
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, basin_robal,
     +           robal, Basin_sroff, Basin_sroff_down,
     +           Basin_hortonian_lakes, Basin_infil, Basin_imperv_evap,
     +           Basin_imperv_stor, Basin_sroff_farflow,
     +           Basin_dprst_evap, Basin_dprst_seep, Basin_dprst_sroff
        ELSE
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, basin_robal,
     +           robal, Basin_sroff, Basin_infil, Basin_imperv_evap,
     +           Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep,
     +           Basin_sroffp, Basin_sroffi, Basin_dprst_seep
        ENDIF
        IF ( ABS(basin_robal)>1.0D-3 ) THEN
          WRITE ( BALUNT, * ) 'possible basin water balance ERROR'
        ELSEIF ( ABS(basin_robal)>5.0D-4 ) THEN
          WRITE ( BALUNT, * ) 'basin_robal rounding issue'
        ENDIF
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), 12F11.4)

      END FUNCTION srunoffcarearun

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil_carea(Pptmix_nopack, Carea_min,
     +           Carea_dif, Soil_moist, Soil_moist_max, Soil_rechr,
     +           Soil_rechr_max, Net_rain, Net_ppt, Hru_imperv,
     +           Imperv_stor, Imperv_stor_max, Snowmelt, Snowinfil_max,
     +           Net_snow, Pkwater_equiv, Infil, Hru_type,
     +           Upslope_hortonian)
      USE PRMS_SRUNOFF_CAREA, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL imperv_sroff, perv_imperv_comp_carea, check_capacity
! Arguments
      INTEGER, INTENT(IN) :: Pptmix_nopack, Hru_type
      REAL, INTENT(IN) :: Carea_min, Soil_rechr_max, Carea_dif
      REAL, INTENT(IN) :: Soil_moist_max, Soil_moist, Net_rain, Net_ppt
      REAL, INTENT(IN) :: Imperv_stor_max, Hru_imperv
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow
      REAL, INTENT(IN) :: Soil_rechr, Upslope_hortonian
      DOUBLE PRECISION, INTENT(IN) :: Pkwater_equiv
      REAL, INTENT(INOUT) :: Imperv_stor, Infil
! Local Variables
      REAL :: ppti, pptp, snri
!***********************************************************************
! compute runoff from cascading Hortonian flow
      IF ( Upslope_hortonian>0.0 ) THEN
        pptp = Upslope_hortonian
        ppti = Upslope_hortonian
        CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv,
     +       Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +       Imperv_stor_max, Imperv_stor, Infil, Hru_type)

      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack==1 ) THEN
        pptp = Net_rain
        ppti = Net_rain
        CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv,
     +       Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +       Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      ENDIF

!******If precipitation on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN

        IF ( Pkwater_equiv>DNEARZERO .OR.
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
          pptp = Snowmelt
          ppti = Snowmelt

          CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv,
     +         Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +         Imperv_stor_max, Imperv_stor, Infil, Hru_type)

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

          CALL perv_imperv_comp_carea(pptp, ppti, Hru_imperv,
     +         Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +         Imperv_stor_max, Imperv_stor, Infil, Hru_type)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.
      ELSEIF ( Infil>0.0 .AND. Hru_type==1 ) THEN
        CALL check_capacity(Soil_moist_max, Soil_moist, Snowinfil_max,
     +       Srp, Infil)

      ENDIF

      END SUBROUTINE compute_infil_carea

!***********************************************************************
!***********************************************************************
      SUBROUTINE perv_imperv_comp_carea(Pptp, Ppti, Hru_imperv,
     +           Carea_min, Carea_dif, Soil_rechr, Soil_rechr_max,
     +           Imperv_stor_max, Imperv_stor, Infil, Hru_type)
      USE PRMS_SRUNOFF_CAREA, ONLY: Srp, Sri
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
      EXTERNAL perv_sroff_carea, imperv_sroff
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Pptp, Ppti, Carea_min, Soil_rechr
      REAL, INTENT(IN) :: Soil_rechr_max, Imperv_stor_max, Carea_dif
      REAL, INTENT(IN) :: Hru_imperv
      REAL, INTENT(INOUT) :: Infil, Imperv_stor
! Local Variables
      REAL :: inp, snrp, snri
!***********************************************************************
!******Pervious area computations
      IF ( Pptp>0.0 ) THEN
        CALL perv_sroff_carea(Carea_min, Carea_dif, Soil_rechr,
     +       Soil_rechr_max, Pptp, inp, snrp, Hru_type)
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

      END SUBROUTINE perv_imperv_comp_carea

!***********************************************************************
!      Subroutine to compute runoff from pervious area using
!      contributing area computations
!***********************************************************************

      SUBROUTINE perv_sroff_carea(Carea_min, Carea_dif, Soil_rechr,
     +           Soil_rechr_max, Pptp, Infil, Srp, Hru_type)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Hru_type
      REAL, INTENT(IN) :: Carea_min, Soil_rechr, Soil_rechr_max, Pptp
      REAL, INTENT(IN) :: Carea_dif
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
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_carea(Ihru, Ncascade_hru, Runoff,
     +           Hru_sroff_down, Farflow)
      USE PRMS_SRUNOFF_CAREA, ONLY: Upslope_hortonian
      USE PRMS_MODULE, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt,
     +    Cascade_area
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

      END SUBROUTINE run_cascade_carea
