!***********************************************************************
! Kinematic routing of surface runoff
!***********************************************************************
      MODULE PRMS_KROUT_OFPL
      IMPLICIT NONE
!   Local Variables
      DOUBLE PRECISION, PARAMETER :: IM2FS = 1.0D0/(12.0D0*60.0D0)
      character(len=*), parameter :: MODDESC = 'Kinematic Overland Flow'
      character(len=*), parameter :: MODNAME = 'krout_ofpl'
      character(len=*), parameter :: Version_krout_ofpl = '2024-01-31'
      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0
      INTEGER, SAVE :: Dtmflg, Type56_flag
      DOUBLE PRECISION, SAVE :: Chifactor
      INTEGER, SAVE, ALLOCATABLE :: Mocgrids(:), Nxs(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hmx(:), Dx(:), Dtdx(:), Dts(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Qimp(:), Qprv(:), H(:), Q(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hp(:), Qp(:), H_xsp(:, :), H_xsi(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Qin(:, :), Ofp_xmoc(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ofp_amoc(:, :), Ofp_impv_cmp1(:), Ofp_cmp1(:)
!sed  DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sed(:), Sed_a(:), Sedp(:), Sedin(:, :)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Storm_ofvol
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Q_ofpl(:), Storm_vol_ofpl(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Q_ndels(:, :), Q_xsp(:, :), Q_xsi(:, :)
!sed  DOUBLE PRECISION, SAVE :: Storm_ofsed
!sed  DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sed_ofpl(:), Sed_ndels(:, :), Sed_p(:, :)
!sed  DOUBLE PRECISION, SAVE, ALLOCATABLE :: Storm_sed_ofpl(:)
!   Declared Parameters
      INTEGER, SAVE :: Ofp_rtemethod
      INTEGER, SAVE, ALLOCATABLE :: Ofp_type(:), Ofp_ndx(:)
      DOUBLE PRECISION, SAVE :: Ofp_theta, Ofp_chi
!sed  INTEGER, SAVE :: Sed_route
!sed  DOUBLE PRECISION, SAVE, ALLOCATABLE :: Kr(:), Hc(:), Kf(:), Mm(:), En(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ofp_thresh(:), Ofp_length(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ofp_alpha(:), Ofp_cmp(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ofp_impv_alpha(:), Ofp_rough(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ofp_impv_cmp(:), Ofp_route_time(:)
      END MODULE PRMS_KROUT_OFPL

!***********************************************************************
!     Main krout_ofpl routine
!***********************************************************************
      INTEGER FUNCTION krout_ofpl()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN !, SAVE_INIT, READ_INIT
      USE PRMS_MODULE, ONLY: Process_flag !, Init_vars_from_file, Save_vars_to_file
      IMPLICIT NONE
      ! Functions
      INTEGER, EXTERNAL :: kofdecl, kofinit, kofrun
      !EXTERNAL :: kof_restart
!***********************************************************************
      krout_ofpl = 0

      IF ( Process_flag==RUN ) THEN
        krout_ofpl = kofrun()
      ELSEIF ( Process_flag==DECL ) THEN
        krout_ofpl = kofdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        !IF ( Init_vars_from_file==READ_INIT ) CALL kof_restart(1)
        krout_ofpl = kofinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        !IF ( Save_vars_to_file==1 ) CALL kof_restart(READ_INIT)
      ENDIF

      END FUNCTION krout_ofpl

!***********************************************************************
!     kofdecl - set up parameters for kinematic routing of
!               overland flow planes - used for timestep of 15 minutes
!               or less unless type '99' flowplane used.
!***********************************************************************
      INTEGER FUNCTION kofdecl()
      USE PRMS_KROUT_OFPL
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_GRNAMPT, ONLY: Ncdels, Nofxs, Ncmoc
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL :: read_error, print_module
!***********************************************************************
      kofdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_krout_ofpl)

      ALLOCATE ( Q_ofpl(Nhru) )
      IF ( declvar(MODNAME, 'q_ofpl', 'nhru', Nhru, 'double', &
           'Flow from overland flow plane', &
           'cfs', Q_ofpl)/=0 ) CALL read_error(3, 'q_ofpl')

      ALLOCATE ( Q_ndels(Ncdels, Nhru) )
      IF ( declvar(MODNAME, 'q_ndels', 'ncdels,nhru', Ncdels*Nhru, 'double', &
           'Flow from overland flow plane for each sub timestep', &
           'cfs', Q_ndels)/=0 ) CALL read_error(3, 'q_ndels')

      ALLOCATE ( Storm_vol_ofpl(Nhru) )
      IF ( declvar(MODNAME, 'storm_vol_ofpl', 'nhru', Nhru, 'double', &
           'Storm total of flow volume from an overland flow plane', &
           'cubic feet', Storm_vol_ofpl)/=0 ) CALL read_error(3, 'storm_vol_ofpl')

      IF ( declvar(MODNAME, 'storm_ofvol', 'one', 1, 'real', &
           'Storm total of flow volume from all overland flow planes', &
           'cubic feet', Storm_ofvol)/=0 ) CALL read_error(3, 'storm_ofvol')

      ALLOCATE ( Q_xsp(Nofxs, Nhru) )
      IF ( declvar(MODNAME, 'q_xsp', 'nofxs,nhru', Nofxs*Nhru, 'double', &
           'Discharge (unit length) in the pervious portion of each'// &
           ' overland flow plane cross section at the end of a data timestep', &
           'cfs', Q_xsp)/=0 ) CALL read_error(3, 'q_xsp')

      ALLOCATE ( Q_xsi(Nofxs, Nhru) )
      IF ( declvar(MODNAME, 'q_xsi', 'nofxs,nhru', Nofxs*Nhru, 'double', &
           'Discharge (unit length) in the impervious portion of each'// &
           ' overland flow plane cross section at the end of a data timestep', &
           'cfs', Q_xsi)/=0 ) CALL read_error(3, 'q_xsi')

!sed  ALLOCATE ( Sed_ofpl(Nhru) )
!sed  IF ( declvar(MODNAME, 'sed_ofpl', 'nhru', Nhru, 'double', &
!sed       'Sediment discharge for overland flow plane', &
!sed       'tons/day', Sed_ofpl)/=0 ) CALL read_error(3, 'sed_ofpl')
!sed  ALLOCATE ( Sed_ndels(Ncdels, Nhru) )
!sed  IF ( declvar(MODNAME, 'sed_ndels', 'ncdels,nhru', Ncdels*Nhru, 'double', &
!sed       'Sediment discharge for overland flow plane for each sub timestep', &
!sed       'tons/day', Sed_ndels)/=0 ) CALL read_error(3, 'sed_ndels')
!sed  ALLOCATE ( Storm_sed_ofpl(Nhru) )
!sed  IF ( declvar(MODNAME, 'storm_sed_ofpl', 'nhru', Nhru, 'double', &
!sed       'Storm total of sediment discharge from overland flow plane', &
!sed       'tons', Storm_sed_ofpl)/=0 ) CALL read_error(3, 'storm_sed_ofpl')
!sed  IF ( declvar(MODNAME, 'storm_ofsed', 'one', 1, 'double', &
!sed       'Storm total of sediment discharge from all overland flow planes', &
!sed       'tons', Storm_ofsed)/=0 ) CALL read_error(3, 'storm_sed_ofpl')
!sed  ALLOCATE ( Sed_p(Nofxs, Nhru) )
!sed  IF ( declvar(MODNAME, 'sed_p', 'nofxs,nhru', Nofxs*Nhru, 'double', &
!sed       'Sediment discharge in the pervious portion (unit length)'// &
!sed       ' of each overland flow plane cross section at the end of a data timestep', &
!sed       'tons/day', Sed_p)/=0 ) CALL read_error(3, 'sed_p')

! Allocate arrays for parameters, local and variables from other modules
      ALLOCATE ( Ofp_type(Nhru), Ofp_ndx(Nhru), Ofp_thresh(Nhru), Ofp_impv_alpha(Nhru), Ofp_impv_cmp(Nhru) )
      ALLOCATE ( Ofp_length(Nhru), Ofp_rough(Nhru), Ofp_alpha(Nhru), Ofp_cmp(Nhru), Ofp_route_time(Nhru) )
!sed  ALLOCATE ( Kr(Nhru), Hc(Nhru), Kf(Nhru), Mm(Nhru), En(Nhru) )
! Allocate arrays for local states + states retrieved from other modules
      ALLOCATE ( Ofp_impv_cmp1(Nhru), Ofp_cmp1(Nhru) )
      ALLOCATE ( H_xsp(Nofxs, Nhru), H_xsi(Nofxs, Nhru), Hmx(Nhru) )
      ALLOCATE ( Ofp_xmoc(Ncmoc, Nhru), Ofp_amoc(Ncmoc, Nhru) )
      ALLOCATE ( Mocgrids(Nhru), Dx(Nhru), Dtdx(Nhru), Dts(Nhru), Nxs(Nhru) )
      ALLOCATE ( Qin(Ncdels, Nhru), Qimp(Ncdels), Qprv(Ncdels) )
      ALLOCATE ( H(Nofxs), Q(Nofxs), Hp(Nofxs), Qp(Nofxs) )
!sed  ALLOCATE ( Sedin(Ncdels, Nhru), Sed_a(Ncdels), Sed(Nofxs), Sedp(Nofxs) )

! Declare parameters
!sed  IF ( declparam(MODNAME, 'sed_route', 'one', 'integer', &
!sed       '0', '0', '1', &
!sed       'Sediment routing flag', &
!sed       'Switch to indicate whether sediment routing is to be done along with the flow routing (0=no; 1=yes)', &
!sed       'none')/=0 ) CALL read_error(1, 'sed_route')
!sed  IF ( declparam(MODNAME, 'kr', 'nhru', 'double', &
!sed       '1.', '0.', '100.', &
!sed       'Coefficient in soil detachment relation', &
!sed       'Parametric coefficient in rain drop-flow depth soil detachment relation', &
!sed       'none')/=0 ) CALL read_error(1, 'kr')
!sed  IF ( declparam(MODNAME, 'hc', 'nhru', 'double', &
!sed       '10.', '0.', '100.', &
!sed       'Coefficient in soil detachment relation', &
!sed       'Parametric coefficient in rain drop-flow depth soil detachment relation', &
!sed       'none')/=0 ) CALL read_error(1, 'hc')
!sed  IF ( declparam(MODNAME, 'kf', 'nhru', 'double', &
!sed       '1.', '0.', '100.', &
!sed       'Coefficient in runoff detachment relation', &
!sed       'Parametric coefficient in runoff detachment relation', &
!sed       'none')/=0 ) CALL read_error(1, 'kf')
!sed  IF ( declparam(MODNAME, 'mm', 'nhru', 'double', &
!sed       '1.', '0.', '100.', &
!sed       'Coefficient in sediment transport capacity relation', &
!sed       'Parametric coefficient in sediment transport capacity relation', &
!sed       'none')/=0 ) CALL read_error(1, 'mm')
!sed  IF ( declparam(MODNAME, 'en', 'nhru', 'double', &
!sed       '1.5', '0.', '100.', &
!sed       'Coefficient in sediment transport capacity relation', &
!sed       'Parametric coefficient in sediment transport capacity relation', &
!sed       'none')/=0 ) CALL read_error(1, 'en')

      IF ( declparam(MODNAME, 'ofp_rtemethod', 'one', 'integer', &
           '0', '0', '3', &
           'Overland Flow routing method flag', &
           'Kinematic wave method (0=explicit finite-difference; 1=method of characteristics;'// &
           ' 2=implicit finite-difference 3=Muskingum-Cunge diffusion wave)', &
           'none')/=0 ) CALL read_error(1, 'ofp_rtemethod')
 
      IF ( declparam(MODNAME, 'ofp_type', 'nhru', 'integer', &
           '4', '4', '99', &
           'Overland flow plane routing type', &
           'Overland flow plane routing type (4=explicit specification of the kinematic parameters'// &
           ' alpha (ofp_alpha) and m (ofp_cmp); 5=turbulent; 6=laminar;'// &
           ' 99=no routing done - the precipitation excess is used as outflow from the plane)', &
           'none')/=0 ) CALL read_error(1, 'ofp_type')

      IF ( declparam(MODNAME, 'ofp_ndx', 'nhru', 'integer', &
           '1', '0', '10', &
           'Number of intervals for routing', &
           'Number of intervals into which the length of the overland'// &
           ' flow plane is subdivided for finite-difference computations.  Use 1 for ofp_type 99', &
           'none')/=0 ) CALL read_error(1, 'ofp_ndx')

      IF ( declparam(MODNAME, 'ofp_thresh', 'nhru', 'double', &
           '0.0', '0.0', '6.0', &
           'Minimum depth of flow to continue overland flow routing', &
           'Minimum depth of flow to continue overland flow routing', &
           'inches')/=0 ) CALL read_error(1, 'ofp_thresh')
                        
      IF ( declparam(MODNAME, 'ofp_impv_alpha', 'nhru', 'double', &
           '2.0', '0.001', '100.0', &
           'Kinematic parameter alpha for imperv area, ofp_type 4', &
           'Kinematic parameter alpha for imperv area, ofp_type 4', &
           'none')/=0 ) CALL read_error(1, 'ofp_impv_alpha')

      IF ( declparam(MODNAME, 'ofp_impv_cmp', 'nhru', 'double', &
           '1.67', '0.5', '3.0', &
           'Kinematic parameter m for imperv area, ofp_type 4', &
           'Kinematic parameter m for imperv area, ofp_type 4', &
           'none')/=0 ) CALL read_error(1, 'ofp_impv_cmp')

      IF ( declparam(MODNAME, 'ofp_length', 'nhru', 'double', &
           '1.0', '1.0', '100000.0', &
           'Length of overland flow plane', 'Length of overland flow plane', &
           'feet')/=0 ) CALL read_error(1, 'ofp_length')

      IF ( declparam(MODNAME, 'ofp_rough', 'nhru', 'double', &
           '0.005', '0.001', '40000.0', &
           'Roughness parameter', &
           'Roughness parameter, ofp_types 5 and 6', &
           'none')/=0 ) CALL read_error(1, 'ofp_rough')

      IF ( declparam(MODNAME, 'ofp_alpha', 'nhru', 'double', &
           '0.003', '0.001', '100.0', &
           'Kinematic parameter alpha, ofp_type 4', &
           'Kinematic parameter alpha, ofp_type 4', &
           'none')/=0 ) CALL read_error(1, 'ofp_alpha')

      IF ( declparam(MODNAME, 'ofp_cmp', 'nhru', 'double', &
           '1.67', '0.5', '3.0', &
           'Kinematic parameter m, ofp_type 4', &
           'Kinematic parameter m, ofp_type 4', &
           'none')/=0 ) CALL read_error(1, 'ofp_cmp')

      IF ( declparam(MODNAME, 'ofp_route_time', 'nhru', 'double', &
           '5.0', '0.1', '15.0', &
           'Time interval for overland flow routing', &
           'Time interval for overland flow routing, should be less'// &
           ' or equal to data timestep, and evenly divisible into data timestep', &
           'minutes')/=0 ) CALL read_error(1, 'ofp_route_time')

      IF ( declparam(MODNAME, 'ofp_theta', 'one', 'double', &
           '0.5', '0.5', '1.0', &
           'Finite-difference spatial weighting factor', &
           'Finite-difference spatial weighting factor', &
           'decimal fraction')/=0 ) CALL read_error(1, 'ofp_theta')

      IF ( declparam(MODNAME, 'ofp_chi', 'one', 'double', &
           '0.6', '0.5', '1.0', &
           'Finite-difference weighting factor', &
           'Finite-difference weighting factor', &
           'decimal fraction')/=0 ) CALL read_error(1, 'ofp_chi')

      END FUNCTION kofdecl
 
!***********************************************************************
!     kofinit - Initialize kinroute_ofpl module - get parameter values,
!               check som parameter values
!***********************************************************************
      INTEGER FUNCTION kofinit()
      USE PRMS_CONSTANTS, ONLY: DNEARZERO
      USE PRMS_KROUT_OFPL
      USE PRMS_MODULE, ONLY: Nhru, Inputerror_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_frac_imperv
      USE PRMS_SOLTAB, ONLY: Hru_slope
      USE PRMS_GRNAMPT, ONLY: Nofxs
      IMPLICIT NONE
      INTRINSIC :: DBLE
      EXTERNAL :: Ofp_AlphaRm, read_error
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, maxnxs, j, ierr
!***********************************************************************
      kofinit = 0

!sed  IF ( getparam(MODNAME, 'sed_route', 1, 'integer', Sed_route)/=0 ) CALL read_error(2, 'sed_route')
!sed  IF ( Sed_route==1 ) THEN
!sed    ALLOCATE (Kr(Nhru), Hc(Nhru), Kf(Nhru), Mm(Nhru), En(Nhru))
!sed    IF ( getparam(MODNAME, 'kr', Nhru, 'double', Kr)/=0 ) CALL read_error(2, 'kr')
!sed    IF ( getparam(MODNAME, 'hc', Nhru, 'double', Hc)/=0 ) CALL read_error(2, 'hc')
!sed    IF ( getparam(MODNAME, 'kf', Nhru, 'double', Kf)/=0 ) CALL read_error(2, 'kf')
!sed    IF ( getparam(MODNAME, 'mm', Nhru, 'double', Mm)/=0 ) CALL read_error(2, 'mm')
!sed    IF ( getparam(MODNAME, 'en', Nhru, 'double', En)/=0 ) CALL read_error(2, 'en')
!sed  ENDIF

      IF ( getparam(MODNAME, 'ofp_type', Nhru, 'integer', Ofp_type)/=0 ) CALL read_error(2, 'ofp_type')
      IF ( getparam(MODNAME, 'ofp_route_time', Nhru, 'double', Ofp_route_time)/=0 ) CALL read_error(2, 'ofp_route_time')
      IF ( getparam(MODNAME, 'ofp_alpha', Nhru, 'double', Ofp_alpha)/=0 ) CALL read_error(2, 'ofp_alpha')
      IF ( getparam(MODNAME, 'ofp_impv_alpha', Nhru, 'double', Ofp_impv_alpha)/=0 ) CALL read_error(2, 'ofp_impv_alpha')
      IF ( getparam(MODNAME, 'ofp_ndx', Nhru, 'integer', Ofp_ndx)/=0 ) CALL read_error(2, 'ofp_ndx')
      IF ( getparam(MODNAME, 'ofp_length', Nhru, 'double', Ofp_length)/=0 ) CALL read_error(2, 'ofp_length')
      IF ( getparam(MODNAME, 'ofp_impv_cmp', Nhru, 'double', Ofp_impv_cmp)/=0 ) CALL read_error(2, 'ofp_impv_cmp')
      IF ( getparam(MODNAME, 'ofp_chi', 1, 'double', Ofp_chi)/=0 ) CALL read_error(2, 'ofp_chi')
      IF ( getparam(MODNAME, 'ofp_cmp', Nhru, 'double', Ofp_cmp)/=0 ) CALL read_error(2, 'ofp_cmp')
      IF ( Ofp_chi==ZERO ) THEN
        PRINT *, 'ERROR, ofp_chi cannot be 0.0'
        Inputerror_flag = 1
      ENDIF

      Type56_flag = 0
      ierr = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Ofp_type(i)>6 .AND. Ofp_type(i)<99 ) THEN
          PRINT *, 'Invalid ofp_type for HRU:', i, ' ofp_type:', Ofp_type(i)
          Inputerror_flag = 1
        ELSEIF ( Ofp_type(i)==5 .OR. Ofp_type(i)==6 ) THEN
          Type56_flag = 1
        ENDIF
        IF ( Ofp_route_time(i)==ZERO ) THEN
          PRINT *, 'ERROR, ofp_route_time cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
        IF ( Ofp_alpha(i)==ZERO ) THEN
          PRINT *, 'ERROR, ofp_alpha cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
        IF ( Ofp_impv_alpha(i)==ZERO ) THEN
          PRINT *, 'ERROR, ofp_impv_alpha cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
        IF ( Ofp_ndx(i)==0 ) THEN
          PRINT *, 'ERROR, ofp_ndx cannot be 0, HRU:', i
          ierr = 1
        ENDIF
        IF ( Ofp_length(i)==ZERO ) THEN
          PRINT *, 'ERROR, ofp_length cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
        IF ( Ofp_impv_cmp(i)==ZERO ) THEN
          PRINT *, 'ERROR, ofp_impv_cmp cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
        IF ( Ofp_cmp(i)==ZERO ) THEN
          PRINT *, 'ERROR, ofp_cmp cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
      ENDDO

      IF ( getparam(MODNAME, 'ofp_thresh', Nhru, 'double', Ofp_thresh)/=0 ) CALL read_error(2, 'ofp_thresh')
      IF ( Type56_flag==1 ) THEN
        IF ( getparam(MODNAME, 'ofp_rough', Nhru, 'double', Ofp_rough)/=0 ) CALL read_error(2, 'ofp_rough')
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Ofp_rough(i)==ZERO ) THEN
            PRINT *, 'ERROR, ofp_rough cannot be 0.0, HRU:', i
            ierr = 1
          ENDIF
        ENDDO
      ENDIF
      IF ( ierr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF

      IF ( getparam(MODNAME, 'ofp_rtemethod', 1, 'integer',  Ofp_rtemethod)/=0 ) CALL read_error(2, 'ofp_rtemethod')
      IF ( getparam(MODNAME, 'ofp_theta', 1, 'double', Ofp_theta)/=0 ) CALL read_error(2, 'ofp_theta')

! initialize ofpl variables at the beginning of a run
      H_xsp = ZERO
      H_xsi = ZERO
      Q_xsp = ZERO
      Q_xsi = ZERO
      Storm_ofvol = ZERO
      Hmx = ZERO
      Storm_vol_ofpl = ZERO
      Q_ofpl = ZERO
      Q_ndels = ZERO
!sed  Storm_ofsed = ZERO
!sed  Storm_sed_ofpl = ZERO
!sed  Sed_p = ZERO
!sed  Sed_ofpl = ZERO
!sed  Sed_ndels = ZERO
 
      Chifactor = (1.0D0-Ofp_chi)/Ofp_chi

      maxnxs = 0
      DO i = 1, Nhru
        Nxs(i) = Ofp_ndx(i) + 1
        IF ( Nxs(i)>maxnxs ) maxnxs = Nxs(i)
        Dts(i) = Ofp_route_time(i)*60.0D0
        IF ( Ofp_type(i)==99 ) THEN
          Dx(i) = Ofp_length(i)
          Dtdx(i) = ZERO
        ELSE
! compute alpha and cmp for pervious portion of flow plane
          IF ( Ofp_type(i)==5 .OR. Ofp_type(i)==6 ) THEN
            CALL Ofp_AlphaRm(Ofp_type(i), Ofp_rough(i), Hru_slope(i), Ofp_alpha(i), Ofp_cmp(i))
! compute alpha and cmp for impervious portion of flow plane
            IF ( Hru_frac_imperv(i)>0.0 ) &
                 CALL Ofp_AlphaRm(Ofp_type(i), Ofp_rough(i), Hru_slope(i), Ofp_impv_alpha(i), Ofp_impv_cmp(i))
          ENDIF
          Dx(i) = Ofp_length(i)/Ofp_ndx(i)
          Dtdx(i) = Dts(i)/Dx(i)
        ENDIF
        IF ( Ofp_type(i)==4 ) THEN
          Ofp_impv_cmp1(i) = 1.0D0/Ofp_impv_cmp(i)
          Ofp_cmp1(i) = 1.0D0/Ofp_cmp(i)
        ENDIF
      ENDDO
      IF ( maxnxs>Nofxs) THEN
        PRINT *, 'Increase dimension nofxs in the parameter file to:', maxnxs
        STOP
      ENDIF
      IF ( Type56_flag/=1 ) DEALLOCATE ( Ofp_rough )

! initialize MOC arrays
      IF ( Ofp_rtemethod==1 ) THEN
!sed    IF ( Sed_route==1 ) THEN
!sed      PRINT *, '***Cannot route sediment with MOC routing', Sed_route
!sed      STOP
!sed    ENDIF
        Mocgrids = 0
        Ofp_xmoc = ZERO
        Ofp_amoc = ZERO
      ENDIF

      Dtmflg = 0

      END FUNCTION kofinit
 
!***********************************************************************
!     kofrun -
!***********************************************************************
      INTEGER FUNCTION kofrun()
      USE PRMS_CONSTANTS, ONLY: DNEARZERO, LAKE, SWALE
      USE PRMS_KROUT_OFPL
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Hru_frac_imperv, Hru_frac_perv
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Ncascade_hru
      USE PRMS_SET_TIME, ONLY: Timestep_minutes, Storm_status, Storm_num
      USE PRMS_SRUNOFF, ONLY: Hortonian_lakes
      USE PRMS_GRNAMPT, ONLY: Infil_dt, Ppt_exc, Ppt_exc_imp, Hru_rain_int
      USE PRMS_SOLTAB, ONLY: Hru_slope_ts
      IMPLICIT NONE
      INTRINSIC :: NINT, SNGL, INT, ABS, EXP, DBLE
      EXTERNAL :: IterateFd, ofpl_init, error_stop
      EXTERNAL :: MuskDifWave, MethofCharInit, MethofChar
! Local Variables
      INTEGER :: ndels, ip, i, idels, ii, iof, imx, imp, iofdn
      INTEGER :: nxi, nxsiof, j, jm1, ofptype, kk
!     INTEGER infdels
      DOUBLE PRECISION :: pt, alp, em, eminv, rt, fac, ptmrt, pe, prr, pr, qdx, qdt
      DOUBLE PRECISION :: ofpl_vol, em1, theta
      DOUBLE PRECISION :: dmyp1, dmyp2, flwareain, flwareaout, ain, alpdts, alpemdts
      DOUBLE PRECISION :: qout, alpem, dt_dx, dtdxchi_inv, em1abs
!sed  DOUBLE PRECISION :: chdt, cqdx, dc, ef, er, hbar, hdtqdx, sdout, tc, tr, tx
!***********************************************************************
      kofrun = 0

      IF ( Storm_status==0 ) RETURN

      IF ( Storm_status==3 ) THEN
! compute volume left on the flow plane
        ofpl_vol = ZERO
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          ofpl_vol = ofpl_vol + Q_ofpl(i)*Dts(i)
        ENDDO
        PRINT *, 'Unit volume left on overland flow planes =', ofpl_vol
        PRINT *, 'For storm number:', Storm_num
        RETURN
      ELSEIF ( Storm_status==1 ) THEN
! zero storm variables in preparation for storm
        CALL ofpl_init()
        IF ( Ofp_rtemethod==1 ) THEN
! Initialize MOC arrays
          flwareain = ZERO
          flwareaout = ZERO
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
!            IF ( Hru_type(i)==LAKE ) CYCLE
            Mocgrids(i) = Nxs(i)
            CALL MethofCharInit(flwareain, flwareaout, Dx(i), Mocgrids(i), Ofp_xmoc(:, i), Ofp_amoc(:, i))
          ENDDO
        ENDIF
      ENDIF

      dmyp1 = ZERO
      dmyp2 = ZERO
 
! Storm_status equals 1 or 2
 
!          ** IF FLOW ROUTING SWITCH IS ON (=1) DO OVERLAND **
!                         FLOW AND SEDIMENT
 
! initialize Qin and Sedin for cascading overland flow planes
      DO ii = 1, Active_hrus
        iof = Hru_route_order(ii)
        ndels = NINT(Timestep_minutes/Ofp_route_time(iof))
        DO i = 1, ndels
          Qin(i, iof) = ZERO
!sed      Sedin(i, iof) = ZERO
        ENDDO
      ENDDO
 
      DO kk = 1, Active_hrus
        iof = Hru_route_order(kk)
        IF ( Hru_type(iof)==SWALE ) CYCLE ! no overland flow if swale ???
        IF ( Hru_type(iof)==LAKE ) THEN
          Hortonian_lakes(iof) = 0.0
          ndels = NINT(Timestep_minutes/Ofp_route_time(iof))
          DO i = 1, ndels
            Hortonian_lakes(iof) = Hortonian_lakes(iof) + SNGL( Qin(i, iof) )
          ENDDO
          CYCLE
        ENDIF
!sed    Sed_ofpl(iof) = ZERO
 
        nxsiof = Nxs(iof)
        dt_dx = Dtdx(iof)
        dtdxchi_inv = 1.0D0/(dt_dx*Ofp_chi)
        rt = Ofp_route_time(iof)
        ndels = NINT(Timestep_minutes/rt)
 
! Qprv is pervious flow as ft^2/sec
! Qimp is impervious flow as ft^2/sec
! Sed_a is sediment flow with pervious flow
        DO i = 1, ndels
          Qimp(i) = ZERO
          Qprv(i) = ZERO
!sed      Sed_a(i) = ZERO
        ENDDO
 
!   pt = infiltration delta time { = min(dt,5) }
!   idels = number of pt intervals in a dt interval
        pt = Infil_dt(iof)
        ptmrt = pt - rt
 
!       ** ADJUST ppt_exc AS NECESSARY FOR DIFFERENCE BETWEEN **
!          UPE TIME AND ROUTE TIME
 
        ofptype = Ofp_type(iof)
        IF ( Timestep_minutes>15.0D0 ) THEN
          ofptype = 99
          IF ( Dtmflg==0 ) THEN
            Dtmflg = 1
!            PRINT *, '*****Warning*****'
!            PRINT *, 'Overland flow type set to 99, timestep>15 minutes', Timestep_minutes
!            PRINT *, '*****'
          ENDIF
        ENDIF
 
!warning, possible truncation problem dividing rt/pt and expecting
!         whole number in setting idels and in setting ip below
        fac = rt/pt
        idels = NINT(fac)
!       infdels = NINT(Timestep_minutes/pt)
 
        imx = 1
        IF ( Hru_frac_imperv(iof)>0.0 ) imx = 2
        DO imp = 1, imx
          IF ( imp==2 ) THEN
            alp = Ofp_impv_alpha(iof)
            em = Ofp_impv_cmp(iof)
            eminv = Ofp_impv_cmp1(iof)
          ELSE
            alp = Ofp_alpha(iof)
            em = Ofp_cmp(iof)
            eminv = Ofp_cmp1(iof)
          ENDIF
          alpdts = alp*Dts(iof)
          alpemdts = alpdts*em
          em1 = em - 1.0D0
          em1abs = ABS(em1)
          alpem = alp*em
          IF ( ofptype/=99 ) THEN
            IF ( imp==2 ) THEN
              DO nxi = 1, nxsiof
                H(nxi) = H_xsi(nxi, iof)
                Q(nxi) = Q_xsi(nxi, iof)
              ENDDO
            ELSE
              DO nxi = 1, nxsiof
                H(nxi) = H_xsp(nxi, iof)
                Q(nxi) = Q_xsp(nxi, iof)
!sed            Sed(nxi) = Sed_p(nxi, iof)
              ENDDO
            ENDIF
          ENDIF
 
          ip = 0
          DO i = 1, ndels
! pt<rt
            IF ( ptmrt<-DNEARZERO ) THEN
              pe = ZERO
              prr = ZERO
              DO ii = 1, idels
                ip = ip + 1
                IF ( imp==1 ) THEN
                  pe = pe + Ppt_exc(ip, iof)
                ELSE
                  pe = pe + Ppt_exc_imp(ip, iof)
                ENDIF
                prr = prr + Hru_rain_int(ip, iof)
              ENDDO
! pt>rt
            ELSEIF ( ptmrt>DNEARZERO ) THEN
              print *, 'pt>rt', pt, rt, ptmrt
              ip = INT(fac*(i-1)) + 1
!             IF ( ip>infdels ) PRINT *, 'idels issue, ofp', infdels, &
!                                        ip, ndels, fac, i, Timestep_minutes, rt, Nowtime
              IF ( imp==1 ) THEN
                pe = Ppt_exc(ip, iof)*fac
              ELSE
                pe = Ppt_exc_imp(ip, iof)*fac
              ENDIF
              prr = Hru_rain_int(ip, iof)*fac
! pt=rt
            ELSE
              IF ( imp==1 ) THEN
                pe = Ppt_exc(i, iof)
              ELSE
                pe = Ppt_exc_imp(i, iof)
              ENDIF
              prr = Hru_rain_int(i, iof)
            ENDIF
! pr is precip excess in ft/s for impervious or pervious percent
! prr is the rain intensity within mms timestep, ft/s
! qdx is precip excess in ft^3/s for impervious or pervious percent
!     (assumed times a unit width of 1 ft)
! qdt is precip excess in ft^2/s for impervious or pervious percent
!     (assumed times a unit width of 1 ft)
 
            IF ( ofptype==99 ) THEN
              pr = IM2FS*pe/rt         !rsr??? rt used to be pt
              qdx = pr*Ofp_length(iof)
              IF ( imp==2 ) THEN
                Qimp(i) = qdx + Qin(i, iof)      !rsr??? need to verify
              ELSE
                Qprv(i) = qdx + Qin(i, iof)
!rsr what about Sed_a???
!               Sed_a(i) = ??*qdx*Hru_frac_perv(iof)
              ENDIF
            ELSE
              IF ( pe>DNEARZERO ) THEN
                pr = IM2FS*pe/rt       !rsr??? rt used to be pt
                prr = IM2FS*prr/rt     !rsr??? rt used to be pt
                qdt = pr*Dts(iof)
                qdx = pr*Dx(iof)
              ELSE
                qdt = ZERO
                qdx = ZERO
                prr = ZERO
                pr = ZERO
              ENDIF
 
              IF ( Ofp_rtemethod==1 ) THEN
                ain = Qin(i, iof)*dt_dx
                CALL MethofChar(ain, alp, em, em1, em1abs, qdt, pr, Dx(iof), alpemdts, alpdts, ofptype, &
                                Mocgrids(iof), Ofp_xmoc(1, iof), Ofp_amoc(1, iof), Qp(nxsiof), Hp(nxsiof))
!               IF ( Hp(nxsiof)>Hmx(iof) ) Hmx(iof) = Qp(nxsiof)
!               Sed_a needs to be set here
                qout = Qp(nxsiof)
                IF ( imp==2 ) THEN
                  Qimp(i) = qout
                ELSE
                  Qprv(i) = qout
                ENDIF
                CYCLE
              ENDIF
 
              IF ( Hmx(iof)<=Ofp_thresh(iof) .AND. pe<DNEARZERO .AND. Qin(i, iof)<DNEARZERO ) THEN
                IF ( imp==2 ) THEN
                  Qimp(i) = ZERO
                ELSE
                  Qprv(i) = ZERO
!sed              Sed_a(i) = ZERO
                  DO j = 1, nxsiof
                    Hp(j) = ZERO
                    Qp(j) = ZERO
!sed                Sedp(j) = ZERO
                  ENDDO
                  Hmx(iof) = ZERO
                ENDIF
              ELSE
 
!            ** ROUTE OVERLAND FLOW **
                Hmx(iof) = ZERO
                IF ( Qin(i, iof)>ZERO ) THEN
                  Qp(1) = Qin(i, iof)
                  Hp(1) = (Qp(1)/alp)**eminv
!sed              Sedp(1) = Sedin(i, iof)
                ELSE
                  Qp(1) = ZERO
                  Hp(1) = ZERO
!sed              Sedp(1) = ZERO
                ENDIF
! hm = H(j-1), hpm = Hp(j-1), hj = H(j), hpj = Hp(j)
! qm = Q(j-1), qpm = Qp(j-1), qj = Q(j), qpj = Qp(j)
 
                DO j = 2, nxsiof
 
                  jm1 = j - 1
                  IF ( em1abs>DNEARZERO .OR. Ofp_rtemethod/=2 )THEN
                    theta = ZERO
                    IF ( H(j)>ZERO ) theta = dt_dx*em*Q(j)/H(j)
                    IF ( theta<=1.0D0 .OR. (j==2.AND.Qp(1)<DNEARZERO) ) THEN
                      Hp(j) = H(j) + qdt + dt_dx*(Q(jm1)-Q(j))
                      IF ( Hp(j)>ZERO ) THEN
                        Qp(j) = alp*(Hp(j)**em)
                      ELSE
                        Hp(j) = ZERO
                        Qp(j) = ZERO
                      ENDIF
                    ELSE
                      Qp(j) = Qp(jm1) + qdx - (Hp(jm1)-H(jm1))/dt_dx
                      IF ( Qp(j)>ZERO ) THEN
                        Hp(j) = (Qp(j)/alp)**em1
                      ELSE
                        Hp(j) = ZERO
                        Qp(j) = ZERO
                      ENDIF
                    ENDIF
 
                    IF ( Ofp_rtemethod==2 .AND. Hp(j)>ZERO ) THEN
! could use computed theta value if in coded range
!                     IF ( theta<0.6D0 .OR. theta>1.0D0 ) theta = Ofp_theta
                      theta = Ofp_theta
 
                      CALL IterateFd(alp, em, alpem, em1, em1abs, dtdxchi_inv, Ofp_chi, Chifactor, &
                                     theta, qdx, Q(jm1), Qp(jm1), Q(j), H(jm1), Hp(jm1), H(j), Qp(j), Hp(j), 1)
                    ENDIF
 
                  ELSE
! only called when em = 1.0 and Ofp_rtemethod=2
                    CALL IterateFd(alp, em, alpem, em1, em1abs, dtdxchi_inv, Ofp_chi, Chifactor, &
                                   Ofp_theta, qdx, Q(jm1), Qp(jm1), Q(j), H(jm1), Hp(jm1), H(j), Qp(j), Hp(j), 4)
                  ENDIF
 
!*** Muskingum-Cunge Diffusion Wave Method
                  IF ( Ofp_rtemethod==3 ) CALL MuskDifWave(iof, j, ofptype, dmyp1, dmyp2, &
                       alp, em, eminv, em1, em1abs, Dx(iof), dt_dx, Hru_slope_ts(iof), qdx, Q(jm1), Qp(jm1), Q(j), &
                       H(jm1), Hp(jm1), H(j), Qp(j), Hp(j))
 
                  IF ( Hp(j)>Hmx(iof) ) Hmx(iof) = Hp(j)
 
!              ** ROUTE SEDIMENT **
!sed              IF ( Sed_route==1 .AND. imp==1 ) THEN
!sed                IF ( Hp(j)<DCLOSEZERO ) THEN
!sed                  Sedp(j) = ZERO
!sed                ELSE
!sed                  hbar = (Hp(jm1)+Hp(j))*0.5D0
!sed                  chdt = Sed(j)*H(j)/Dts(iof)
!sed                  cqdx = Sedp(jm1)*Qp(jm1)/Dx(iof)
!sed                  hdtqdx = Hp(j)/Dts(iof) + Qp(j)/Dx(iof)
!sed                  tc = Mm(iof)*hbar**En(iof)
!sed                  tr = (Sed(jm1)*Q(jm1)+Sed(j)*Q(j))*0.5D0
!sed                  dc = tc - tr
!sed                  IF ( dc<DCLOSEZERO ) THEN
!sed                    er = ZERO
!sed                    ef = dc/Dx(iof)
!sed                  ELSE
!sed                    er = Kr(iof)*prr*prr*EXP(-Hc(iof)*hbar*hbar)
!sed                    tx = tr + er*Dx(iof)
!sed                    IF ( tx>=tc ) THEN
!sed                      ef = ZERO
!sed                      er = (tc-tr)/Dx(iof)
!sed                    ELSE
!sed                      ef = Kf(iof)*(tc-tx)/Dx(iof)
!sed                    ENDIF
!sed                  ENDIF
!sed                  Sedp(j) = (ef+er+chdt+cqdx)/hdtqdx
!sed                ENDIF
!sed              ENDIF
 
                ENDDO

                DO j = 1, nxsiof
                  Q(j) = Qp(j)
                  H(j) = Hp(j)
!sed              Sed(j) = Sedp(j)
                ENDDO
 
                qout = Qp(nxsiof)
                IF ( imp/=2 ) THEN
                  Qprv(i) = qout
!sed              IF ( Sed_route==1 ) Sed_a(i) = Sedp(nxsiof)*qout*Hru_frac_perv(iof)
                ELSE
                  Qimp(i) = qout
                ENDIF
 
              ENDIF
 
            ENDIF
 
!*** End of i=1, ndels loop
          ENDDO
 
          IF ( ofptype/=99 ) THEN
            IF ( imp==1 ) THEN
              DO nxi = 1, nxsiof
                H_xsp(nxi, iof) = Hp(nxi)
                Q_xsp(nxi, iof) = Qp(nxi)
!sed            Sed_p(nxi, iof) = Sedp(nxi)
              ENDDO
            ELSE
              DO nxi = 1, nxsiof
                H_xsi(nxi, iof) = Hp(nxi)
                Q_xsi(nxi, iof) = Qp(nxi)
              ENDDO
            ENDIF
          ENDIF
 
!*** End of imp loop
        ENDDO
 
        DO i = 1, ndels
          qout = Hru_frac_perv(iof)*Qprv(i) + Hru_frac_perv(iof)*Qimp(i)
          Q_ndels(i, iof) = qout
          Storm_vol_ofpl(iof) = Storm_vol_ofpl(iof) + qout*Dts(iof)
!sed      IF ( Sed_route==1 ) THEN
!sed        sdout = Sed_ofpl(iof) + Sed_a(i)*Dts(iof)*Ofp_length(iof)*Hru_frac_perv(iof)
!sed        Sed_ndels(i, iof) = sdout
!sed        Sed_ofpl(iof) = Sed_ofpl(iof) + sdout
!sed        Storm_sed_ofpl(iof) = Storm_sed_ofpl(iof) + Sed_ofpl(iof)
!sed        Storm_ofsed = Storm_ofsed + Sed_ofpl(iof)
!sed      ENDIF
! Add output of flow plane to its downstream neighbors
! Sed_a not set to a value when not routing, need to fix.
          DO ii = 1, Ncascade_hru(iof)
            iofdn = Hru_down(ii, iof)
            !rsr, if stream, ignore cascade
            IF ( iofdn>0 ) THEN
              Qin(i, iofdn) = Qin(i, iofdn) + qout*Hru_down_frac(ii, iof)
!sed          Sedin(i, iofdn) = Sedin(i, iofdn) + sdout*Hru_down_frac(ii, iof)
            ENDIF
          ENDDO
!*** End of i=1, ndels loop
        ENDDO
 
        Q_ofpl(iof) = Q_ndels(ndels, iof)
        !includes cascades, danger
        Storm_ofvol = Storm_ofvol + Storm_vol_ofpl(iof)
 
!*** End of nhru flow planes (iof) loop
      ENDDO

      END FUNCTION kofrun
 
!***********************************************************************
!      Subroutine to initialize overland flow plane variables
!***********************************************************************
      SUBROUTINE ofpl_init()
      USE PRMS_CONSTANTS, ONLY: LAKE
      USE PRMS_KROUT_OFPL, ONLY: Storm_ofvol, Hmx, Q_xsp, Storm_vol_ofpl, Q_xsi, H_xsp, H_xsi, ZERO
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_type, Hru_route_order
      USE PRMS_GRNAMPT, ONLY: Nofxs
!sed  USE PRMS_KROUT_OFPL, ONLY:Sed_p, Storm_ofsed, Storm_sed_ofpl
      IMPLICIT NONE
! Local Variables
      INTEGER i, j, ii
!***********************************************************************
      Storm_ofvol = ZERO
!sed  Storm_ofsed = ZERO
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Hru_type(i)==LAKE ) CYCLE
        Hmx(i) = ZERO
        Storm_vol_ofpl(i) = ZERO
!sed    Storm_sed_ofpl(i) = ZERO
        DO j = 1, Nofxs
          H_xsp(j, i) = ZERO
          H_xsi(j, i) = ZERO
          Q_xsp(j, i) = ZERO
          Q_xsi(j, i) = ZERO
!sed      Sed_p(j, i) = ZERO
        ENDDO
      ENDDO
 
      END SUBROUTINE ofpl_init
 
!***********************************************************************
! Computes the parameters alpha and rm for a given overland flow plane
!***********************************************************************
      SUBROUTINE Ofp_AlphaRm(Ioftype, Rough, Slope, Alpha, Rm)
      IMPLICIT NONE
      INTRINSIC SQRT
!     kinmat_vis is the kinematic viscocity of water at 50 deg F
      DOUBLE PRECISION, PARAMETER :: KINMAT_VIS = 0.0000141D0, GRAVITY_VEL = 32.2D0
      DOUBLE PRECISION, PARAMETER :: FIVETHIRDS = 5.0D0/3.0D0
!***********************************************************************
!     + + + ARGUMENT DEFINITIONS + + +
!     IOFTYPE  - Overland Flow Plane Type
!      5 = overland flow (turbulent)
!      6 = overland flow (laminar)
!     ROUGH  - Roughness
!     SLOPE  - Slope
!     ALPHA  - Kinematic-wave routing parameter (L^-1/3/T)
!     RM     - Routing parameter for kinematic wave equation
!***********************************************************************
      INTEGER, INTENT(IN) :: Ioftype
      REAL, INTENT(IN) :: Slope
      DOUBLE PRECISION, INTENT(IN) :: Rough
      DOUBLE PRECISION, INTENT(OUT) :: Alpha, Rm
!***********************************************************************
! Overland flow (turbulent)
      IF ( Ioftype==5 ) THEN
        Alpha = 1.486D0*SQRT(Slope)/Rough
        Rm = FIVETHIRDS
! Overland flow (laminar)
!     ELSEIF ( Ioftype==6 ) THEN
      ELSE
        Alpha = (8.0D0*GRAVITY_VEL*Slope)/(Rough*KINMAT_VIS)
        Rm = 3.0D0
      ENDIF
 
      END SUBROUTINE Ofp_AlphaRm
