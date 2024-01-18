!***********************************************************************
! Module Kinroute_chan - Kinematic routing from PRMS, modified for MMS
      !rsr??? caution, need to check for storm on first timestep,
      ! probably does not work
      !rsr??? need to check units of sediment variables and verify code
      !???rsr, how does interflow and gwflow get added to each reservoir
      !rsr, added hard-coded 'hybrid' DRBC specific code 11/29/07
      !     which sets inflow to channels below reservoirs to gage flow
!***********************************************************************
      MODULE PRMS_KROUT_CHAN
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Kinematic Streamflow Routing'
      character(len=*), parameter :: MODNAME = 'krout_chan'
      character(len=*), parameter :: Version_krout_chan = '2024-01-05'
!    Qsrolat  - Lateral surface runoff in ft^2/s
      INTEGER, PARAMETER :: MAX_ITER = 20
      DOUBLE PRECISION, PARAMETER :: CONVRG = 0.0001D0, TWOTHIRDS = 2.0D0/3.0D0
      DOUBLE PRECISION, PARAMETER :: FOURTHIRDS = 4.0D0/3.0D0, FIVETHIRDS = 5.0D0/3.0D0
      DOUBLE PRECISION, PARAMETER :: FTSQ2ACRE_CONV = 43560.0D0, LENGTH_CONV = 12.0D0
      DOUBLE PRECISION, PARAMETER :: AREACONV = 1.0D0/FTSQ2ACRE_CONV, ZERO = 0.0D0
      DOUBLE PRECISION, PARAMETER :: FT3TOINCHES = LENGTH_CONV*AREACONV
      INTEGER, SAVE :: Mxnsos, NlakeP1, Maxup !, Nstrahler
      INTEGER, SAVE :: File_unit !, File_unit2
      INTEGER, SAVE, ALLOCATABLE :: Rb_hru(:), Lb_hru(:), Rb_hru_area(:), Lb_hru_area(:)
      DOUBLE PRECISION, SAVE :: Chifactor, Ofarea_total, Chan_sum_init
      INTEGER, SAVE, ALLOCATABLE :: Latsw(:), Upsw(:), Nxs(:), Mocgrids(:), Upchan(:, :) !, Strahler_chan(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Qsrolat(:), Ofar(:), Czero(:), Cone(:), Ctwo(:), Qmxa(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Chan_loss_ftsec(:), Uprch_area(:), Alpr1(:), Cmp1(:), Dx(:), Dts(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tc_topw(:), Dtdx(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Chan_parm2(:), Qp(:), Ap(:), Q(:), A(:), Tc_half_topw(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Chan_xmoc(:, :), Chan_amoc(:, :), A_xs(:, :), Qinpast_xs(:, :), Wv15(:,:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Wv5(:, :), S5(:, :), C5(:, :), S15(:, :), C15(:, :), Qout(:, :)
!sed  DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sedout(:, :), Sed_xs(:, :), Sed(:), Sedinlat(:), Sedp(:), Sed_chan(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Chanvol_daysum, Chanvol_nchansum, Chanvol_sum
      DOUBLE PRECISION, SAVE :: St_sroff, Storm_routvol, Dt_sroff, Qday_sum, Storm_pk_obs, Storm_pk_sim !, Sed_tot
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Qin_chan(:), Qin_totchan(:), Qinlat(:), Qinlat_chan(:), Contrib_area_chan(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Q_chan(:), Qin(:), Qin_instant(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Q_xs(:, :), Q_chan_timestep(:), Qinlat_chan_ts(:)
!****
!CAUTION: puts Din1, Lake_sto, Lake_outq
!****
      DOUBLE PRECISION, ALLOCATABLE :: Lake_sto(:), Lake_outq(:), Din1(:)
!   Declared Parameters
      INTEGER, SAVE :: Chan_rtemethod, Outlet_chan, Outlet_sta
      DOUBLE PRECISION, SAVE :: Chan_theta, Chan_chi
      INTEGER, SAVE, ALLOCATABLE :: Lat_inflowr(:), Lat_inflowl(:), Chan_type(:), Chan_ndx(:)
      INTEGER, SAVE, ALLOCATABLE :: Upst_inflow2(:), Upst_inflow3(:), Upst_inflow1(:), Lake_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Q_gain_id(:), Q_divert_id(:), Gain_type(:), Gain_flowid(:), Nsos(:)
      INTEGER, SAVE, ALLOCATABLE :: Upst_res1(:), Upst_res2(:), Upst_res3(:)
!      INTEGER, SAVE, ALLOCATABLE :: Strahler_num(:), Hru_strahler(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Chan_thresh(:), Chan_length(:), Chan_rough(:), Chan_width(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Chan_alpha(:), Chan_cmp(:), Chan_slope(:), Chan_parm1(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Chan_t3_lbratio(:), Chan_t3_rbratio(:), Chan_route_time(:), Chan_loss_rate(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Wpcoef_a(:), Wpcoef_b(:), Musk_wghtfac(:), Musk_travel_time(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: O2(:, :), S2(:, :), Lake_coef(:), Lake_din1(:)
      END MODULE PRMS_KROUT_CHAN

!***********************************************************************
!     Main krout_chan routine
!***********************************************************************
      INTEGER FUNCTION krout_chan()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN !, OFF, ACTIVE, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag !, Init_vars_from_file, Save_vars_to_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: kchdecl, kchinit, kchrun, kchclean
      !EXTERNAL :: krout_restart
!***********************************************************************
      krout_chan = 0

      IF ( Process_flag==RUN ) THEN
        krout_chan = kchrun()
      ELSEIF ( Process_flag==DECL ) THEN
        krout_chan = kchdecl()
      ELSEIF ( Process_flag==INIT ) THEN
!        IF ( Init_vars_from_file>OFF ) CALL krout_restart(READ_INIT)
        krout_chan = kchinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
!        IF ( Save_vars_to_file==ACTIVE ) CALL krout_restart(SAVE_INIT)
        krout_chan = kchclean()
      ENDIF

      END FUNCTION krout_chan

!***********************************************************************
!     kchdecl - set up parameters for  channel routing computations
! Parameters
!     chan_type, chan_ndx, chan_thresh, chan_length, chan_slope
!     chan_rough, chan_width, chan_parm1, chan_alpha, chan_cmp
!     chan_t3_lbratio, chan_t3_rbratio, chan_route_time
!     chan_loss_rate, chan_theta, chan_chi, chan_rtemethod
!     wpcoef_a, wpcoef_b, musk_wghtfac, musk_travel_time
!     lat_inflowr, lat_inflowl, upst_res1, upst_res2, upst_res3
!     ofp_length, sed_route, ofp_route_time
!     lake_coef, lake_type, o2, s2, nsos, outlet_chan
!***********************************************************************
      INTEGER FUNCTION kchdecl()
      USE PRMS_CONSTANTS, ONLY: ERROR_DIM, DOCUMENTATION, MAXDIM
      USE PRMS_KROUT_CHAN
      USE PRMS_MODULE, ONLY: Model, Nsegment, Nlake
      USE PRMS_GRNAMPT, ONLY: Ncmoc, Ncdels, Nchxs
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: MOD, MAX
      INTEGER, EXTERNAL :: declparam, declvar, getdim, decldim
      EXTERNAL :: read_error, print_module, PRMS_open_module_file, error_stop
!***********************************************************************
      kchdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_krout_chan)
      IF ( MOD(Ncmoc, 2)/=0 .AND. Model/=99 ) THEN
        PRINT *, 'ERROR, dimension ncmoc must be an even number:', Ncmoc
        STOP
      ENDIF

      IF ( decldim('mxnsos', 0, MAXDIM, &
     &     'Maximum number of storage/outflow table values for storage-detention reservoirs and lakes connected to'// &
     &     ' the stream network using Puls routing')/=0 ) CALL read_error(7, 'mxnsos')

      IF ( Nsegment==0 ) STOP 'ERROR, dimension nsegment=0 and kinematic channel routing active'
      IF ( Ncdels==0 ) STOP 'ERROR, dimension ncdels=0 and kinematic channel routing active'
      IF ( Nchxs==0 ) STOP 'ERROR, dimension nchxs=0 and kinematic channel routing active'

      NlakeP1 = Nlake + 1

!      Nstrahler = get dim('nstrahler')
!      IF ( Nstrahler==-1 ) CALL read_error(7, 'nstrahler')

! Declare Variables
      ALLOCATE ( Contrib_area_chan(Nsegment) )
      IF ( declvar(MODNAME, 'contrib_area_chan', 'nsegment', Nsegment, 'double', &
           'Contributing area for each channel', &
           'acres', Contrib_area_chan)/=0 ) CALL read_error(3, 'contrib_area_chan')

      IF ( declvar(MODNAME, 'storm_routvol', 'one', 1, 'double', &
           'Routed flow volume at last channel reach for storm, cumulative total', &
           'ft3', Storm_routvol)/=0 ) CALL read_error(3, 'storm_routvol')

      IF ( declvar(MODNAME, 'storm_pk_obs', 'one', 1, 'double', &
           'Measured storm peak used in objective function', &
           'cfs', Storm_pk_obs)/=0 ) CALL read_error(3, 'storm_pk_obs')

      IF ( declvar(MODNAME, 'storm_pk_sim', 'one', 1, 'double', &
           'Simulated storm peak used in objective function', &
           'cfs', Storm_pk_sim)/=0 ) CALL read_error(3, 'storm_pk_sim')

      ALLOCATE ( Q_chan(Nsegment) )
      IF ( declvar(MODNAME, 'q_chan', 'nsegment', Nsegment, 'double', &
           'Flow from channel segment', &
           'cfs', Q_chan)/=0 ) CALL read_error(3, 'q_chan')

      ALLOCATE ( Q_chan_timestep(Nsegment) )
      IF ( declvar(MODNAME, 'q_chan_timestep', 'nsegment', Nsegment, 'double', &
           'Average flow from channel segment for time step', &
           'cfs', Q_chan_timestep)/=0 ) CALL read_error(3, 'q_chan_timestep')

      ALLOCATE ( Qinlat_chan_ts(Nsegment) )
      IF ( declvar(MODNAME, 'qinlat_chan_ts', 'nsegment', Nsegment, 'double', &
           'Average lateral inflow to a channel segment for time step', &
           'cfs', Qinlat_chan_ts)/=0 ) CALL read_error(3, 'qinlat_chan_ts')

      ALLOCATE ( Qin_chan(Nsegment) )
      IF ( declvar(MODNAME, 'qin_chan', 'nsegment', Nsegment, 'double', &
           'Channel inflow to channel segment (including diversions)', &
           'cfs', Qin_chan)/=0 ) CALL read_error(3, 'qin_chan')

      ALLOCATE ( Qin_instant(Nsegment) )
      IF ( declvar(MODNAME, 'qin_instant', 'nsegment', Nsegment, 'double', &
     &     'Channel inflow to channel segment at end of timestep', &
     &     'cfs', Qin_instant)/=0 ) CALL read_error(3, 'qin_instant')

      ALLOCATE ( Qin_totchan(Nsegment) )
      IF ( declvar(MODNAME, 'qin_totchan', 'nsegment', Nsegment, 'double', &
     &     'Total inflow to channel segment (includes lateral flow)', &
     &     'cfs', Qin_totchan)/=0 ) CALL read_error(3, 'qin_totchan')

      ALLOCATE ( Qinlat(Ncdels) )
      IF ( declvar(MODNAME, 'qinlat', 'ncdels', Ncdels, 'double', &
     &     'Lateral inflow for each cdel', &
     &     'cfs per foot', Qinlat)/=0 ) CALL read_error(3, 'qinlat')

      ALLOCATE ( Qinlat_chan(Nsegment) )
      IF ( declvar(MODNAME, 'qinlat_chan', 'nsegment', Nsegment, 'double', &
     &     'Lateral inflow for each channel', &
     &     'cfs', Qinlat_chan)/=0 ) CALL read_error(3, 'qinlat_chan')
     
      ALLOCATE ( Qin(Ncdels) )
      IF ( declvar(MODNAME, 'qin', 'ncdels', Ncdels, 'double', &
     &     'upstream inflow for each cdel', &
     &     'cfs', Qin)/=0 ) CALL read_error(3, 'qin')

      ALLOCATE ( Q_xs(Nchxs, Nsegment) )
      IF ( declvar(MODNAME, 'q_xs', 'nchxs,nsegment', Nchxs*Nsegment, 'double', &
     &     'Flow in each channel cross section at the end of timestep', &
     &     'cfs', Q_xs)/=0 ) CALL read_error(3, 'q_xs')

!sed  ALLOCATE ( Sed_chan(Nsegment) )
!sed  IF ( declvar(MODNAME, 'sed_chan', 'nsegment', Nsegment, 'double', &
!sed       'Sediment transport from channel segment', &
!sed       'tons', Sed_chan)/=0 ) CALL read_error(3, 'sed_chan')

!sed  IF ( declvar(MODNAME, 'sed_tot', 'one', 1, 'double', &
!sed       'Accumulated sediment transport from last channel segment', &
!sed       'tons', Sed_tot)/=0 ) CALL read_error(3, 'sed_tot')

      IF ( declvar(MODNAME, 'chanvol_daysum', 'one', 1, 'double', &
           'Accumulated flow in the last channel segment for a day', &
           'cubic feet', Chanvol_daysum)/=0 ) CALL read_error(3, 'chanvol_daysum')

      IF ( declvar(MODNAME, 'chanvol_nchansum', 'one', 1, 'double', &
           'Accumulated flow in the last channel segment', &
           'cubic feet', Chanvol_nchansum)/=0 ) CALL read_error(3, 'chanvol_nchansum')

      IF ( declvar(MODNAME, 'chanvol_sum', 'one', 1, 'double', &
           'Accumulated flow volume in all channel segments', &
           'cubic feet', Chanvol_sum)/=0 ) CALL read_error(3, 'chanvol_sum')

      IF ( declvar(MODNAME, 'qday_sum', 'one', 1, 'double', &
           'Accumulated flow in the last channel segment', &
           'cubic feet', Qday_sum)/=0 ) CALL read_error(3, 'qday_sum')

      IF ( declvar(MODNAME, 'dt_sroff', 'one', 1, 'double', &
           'Total basin surface runoff for a timestep', &
           'inches', Dt_sroff)/=0 ) CALL read_error(3, 'qday_sum')

      IF ( declvar(MODNAME, 'st_sroff', 'one', 1, 'double', &
           'Accumulated basin surface runoff during a storm', &
           'inches', St_sroff)/=0 ) CALL read_error(3, 'st_sroff')

! Declare Parameters
!      ALLOCATE ( Hru_strahler(nstrahler) )
!      IF ( decl param(MODNAME, 'hru_strahler', 'nstrahler', 'integer', &
!          '0', 'bounded', 'Nsegment', &
!          'Hru associated with each channel segment Strahler number', &
!          'Hru associated with each channel segment Strahler number', &
!          'none')/=0 ) CALL read_error(1, 'hru_strahler')
!
!      ALLOCATE ( Strahler_num(Nstrahler) )
!      IF ( decl param(MODNAME, 'strahler_num', 'nstrahler', 'integer', &
!          '1', '0', '10', &
!          'Strahler of channel segment', &
!          'Strahler of channel segments, could be more than segments', &
!          'none')/=0 ) CALL read_error(1, 'strahler_num')

      ALLOCATE ( Chan_type(Nsegment) )
      IF ( declparam(MODNAME, 'chan_type', 'nsegment', 'integer', &
           '4', '1', '12', &
           'Type of channel segment', &
           'Channel type (1=rectangular open channel; 2=circular pipe segment; 3=triangular open channel;'// &
           ' 4=explicit specification of the kinematic parameters alpha (chan_alpha) and m (chan_cmp); 7=junction;'// &
           ' 8=reservoir, Puls routing; 9=reservoir, linear routng; 10=gutter; 11=arbitrary cross section; 12=muskingum)', &
           'none')/=0 ) CALL read_error(1, 'chan_type')

      ALLOCATE ( Chan_ndx(Nsegment) )
      IF ( declparam(MODNAME, 'chan_ndx', 'nsegment', 'integer', &
           '1', '0', '10', &
           'Number of intervals for routing or reservoir seq. number', &
           'Number of intervals into which the length of the channel'// &
           ' is subdivided for finite-difference computations. If'// &
           ' this is a reservoir, then it is the reservoir sequence number', &
           'none')/=0 ) CALL read_error(1, 'chan_ndx')

      ALLOCATE ( Chan_thresh(Nsegment) )
      IF ( declparam(MODNAME, 'chan_thresh', 'nsegment', 'double', &
           '0.0', '0.0', '6.0', &
           'Minimum depth of flow to continue channel flow routing', &
           'Minimum depth of flow to continue channel flow routing', &
           'inches')/=0 ) CALL read_error(1, 'chan_thresh')

      ALLOCATE ( Chan_length(Nsegment) )
      IF ( declparam(MODNAME, 'chan_length', 'nsegment', 'double', &
           '10.0', '1.0', '100000.0', &
           'Length of channel segment', 'Length of channel segment', &
           'feet')/=0 ) CALL read_error(1, 'chan_length')

      ALLOCATE ( Chan_slope(Nsegment) )
      IF ( declparam(MODNAME, 'chan_slope', 'nsegment', 'double', &
           '0.1', '0.0001', '3.0', &
           'Slope of channel segment', &
           'Slope of channel segment, chan_type = 1, 2, 3, 10, or 11', &
           'decimal percent')/=0 ) CALL read_error(1, 'chan_slope')

      ALLOCATE ( Chan_rough(Nsegment) )
      IF ( declparam(MODNAME, 'chan_rough', 'nsegment', 'double', &
           '0.005', '0.001', '1.0', &
           'Roughness parameter', &
           'Roughness parameter, chan_type = 1, 2, 3, 10, or 11', &
           'none')/=0 ) CALL read_error(1, 'chan_rough')

      ALLOCATE ( Chan_width(Nsegment) )
      IF ( declparam(MODNAME, 'chan_width', 'nsegment', 'double', &
           '1.0', '0.0', '1000.0', &
           'Channel width', 'Channel width', &
           'feet')/=0 ) CALL read_error(1, 'chan_width')

      ALLOCATE ( Chan_parm1(Nsegment) )
      IF ( declparam(MODNAME, 'chan_parm1', 'nsegment', 'double', &
           '0.0', '0.0', '1000.0', &
           'Parameter 1 for computation of alpha and m for chan_type='// &
           ' 2 & 10', 'Parameter 1 for chan_type= 2: Pipe diameter;'// &
           ' 10: Ratio of horizontal to vertical change in gutter side slope', &
           'none')/=0 ) CALL read_error(1, 'chan_parm1')

      ALLOCATE ( Chan_t3_lbratio(Nsegment) )
      IF ( declparam(MODNAME, 'chan_t3_lbratio', 'nsegment', 'double', &
           '0.0', '0.0', '500.0', &
           'Side-slope ratio, left bank', &
           'Ratio of horizontal to vertical change in side slope for triangular channel left bank, chan_type 3', &
           'none')/=0 ) CALL read_error(1, 'chan_t3_lbratio')

      ALLOCATE ( Chan_t3_rbratio(Nsegment) )
      IF ( declparam(MODNAME, 'chan_t3_rbratio', 'nsegment', 'double', &
           '0.0', '0.0', '500.0', &
           'Side-slope ratio, right bank', &
           'Ratio of horizontal to vertical change in side slope for triangular channel, right bank, chan_type 3', &
           'none')/=0 ) CALL read_error(1, 'chan_t3_rbratio')

      ALLOCATE ( Chan_alpha(Nsegment) )
      IF ( declparam(MODNAME, 'chan_alpha', 'nsegment', 'double', &
           '0.003', '0.001', '100.0', &
           'Kinematic parameter alpha, chan_type=4', &
           'Kinematic parameter alpha, chan_type=4', &
           'none')/=0 ) CALL read_error(1, 'chan_alpha')

      ALLOCATE ( Chan_cmp(Nsegment) )
      IF ( declparam(MODNAME, 'chan_cmp', 'nsegment', 'double', &
           '1.67', '0.5', '3.0', &
           'Kinematic parameter m, chan_type=4', &
           'Kinematic parameter m, chan_type=4', &
           'none')/=0 ) CALL read_error(1, 'chan_cmp')

      ALLOCATE ( Chan_route_time(Nsegment) )
      IF ( declparam(MODNAME, 'chan_route_time', 'nsegment', 'double', &
           '5.0', '0.1', '15.0', &
           'Time interval for channel flow routing', &
           'Time interval for channel flow routing, should be less'// &
           ' or equal to data timestep, and evenly divisible into data timestep', &
           'minutes')/=0 ) CALL read_error(1, 'chan_route_time')

      ALLOCATE ( Musk_wghtfac(Nsegment) )
      IF ( declparam(MODNAME, 'musk_wghtfac', 'nsegment', 'double', &
           '0.25', '0.0', '0.5', &
           'Segment Muskingum weighting factor', &
           'Segment Muskingum weighting factor, chan_rtemethod = 1', &
           'none')/=0 ) CALL read_error(1, 'musk_wghtfac')

      ALLOCATE ( Musk_travel_time(Nsegment) )
      IF ( declparam(MODNAME, 'musk_travel_time', 'nsegment', 'double', &
           '0.5', '0.0', '100.0', &
           'Segment travel time', &
           'Segment travel time, chan_rtemethod = 1', &
           'hours')/=0 ) CALL read_error(1, 'musk_travel_time')

      ALLOCATE ( Chan_loss_rate(Nsegment) )
      IF ( declparam(MODNAME, 'chan_loss_rate', 'nsegment', 'double', &
           '0.0', '0.0', '100000.0', &
           'Loss rate for each channel reach', &
           'Loss rate for each channel reach', &
           'inches/hour')/=0 ) CALL read_error(1, 'chan_loss_rate')

      ALLOCATE ( Wpcoef_a(Nsegment) )
      IF ( declparam(MODNAME, 'wpcoef_a', 'nsegment', 'double', &
           '0.0', '0.0', '10.0', &
           'Wetted perimeter coef a in wp=a*AREA**b', &
           'Wetted perimeter coef a in wp=a*AREA**b', &
           'none')/=0 ) CALL read_error(1, 'wpcoef_a')

      ALLOCATE ( Wpcoef_b(Nsegment) )
      IF ( declparam(MODNAME, 'wpcoef_b', 'nsegment', 'double', &
           '0.0', '0.0', '10.0', &
           'Wetted perimeter coef b in wp=a*AREA**b', &
           'Wetted perimeter coef b in wp=a*AREA**b', &
           'none')/=0 ) CALL read_error(1, 'wpcoef_b')

      IF ( declparam(MODNAME, 'chan_theta', 'one', 'double', &
           '0.5', '0.5', '1.0', &
           'Finite-difference spatial weighting factor', &
           'Finite-difference spatial weighting factor', &
           'decimal fraction')/=0 ) CALL read_error(1, 'chan_theta')

      IF ( declparam(MODNAME, 'chan_chi', 'one', 'double', &
           '0.6', '0.5', '1.0', &
           'Finite-difference weighting factor', &
           'Finite-difference weighting factor', &
           'decimal fraction')/=0 ) CALL read_error(1, 'chan_chi')

      IF ( declparam(MODNAME, 'chan_rtemethod', 'one', 'integer', &
           '0', '0', '4', &
           'Channel routing method', &
           'Switch to indicate routing solution method'// &
           ' (0=Explicit finite-difference kinematic wave;'// &
           ' 1=Muskingum, fixed travel time;'// &
           ' 2=Implicit finite-difference kinematic wave;'// &
           ' 3=Muskingum-Cunge diffusion wave;'// &
           ' 4=Method of Characteristics)', &
           'none')/=0 ) CALL read_error(1, 'chan_rtemethod')

      IF ( declparam(MODNAME, 'outlet_sta', 'one', 'integer', &
           '0', 'bounded', 'nobs', &
           'Index of measurement station to use for basin outlet', &
           'Index of measurement station to use for basin outlet', &
           'none')/=0 ) CALL read_error(1, 'outlet_sta')

      IF ( declparam(MODNAME, 'outlet_chan', 'one', 'integer', &
           '0', 'bounded', 'nsegment', &
           'Channel segment number of outlet', &
           'Channel segment number of outlet', &
           'none')/=0 ) CALL read_error(1, 'outlet_chan')

      ALLOCATE ( Lat_inflowr(Nsegment) )
      IF ( declparam(MODNAME, 'lat_inflowr', 'nsegment', 'integer', &
           '0', 'bounded', 'nhru', &
           'Lateral inflow from right ofplane', &
           'Overland flow plane index number for lateral inflow into current channel, right side', &
           'none')/=0 ) CALL read_error(1, 'lat_inflowr')

      ALLOCATE ( Lat_inflowl(Nsegment) )
      IF ( declparam(MODNAME, 'lat_inflowl', 'nsegment', 'integer', &
           '0', 'bounded', 'nhru', &
           'Lateral inflow from left ofplane', &
           'Overland flow plane index number for lateral inflow into current channel, left side', &
           'none')/=0 ) CALL read_error(1, 'lat_inflowl')

!sed  IF ( declparam(MODNAME, 'sed_route', 'one', 'integer', &
!sed       '0', '0', '1', &
!sed       'Sediment routing flag', &
!sed       'Switch to indicate whether sediment routing is to be done along with the flow routing (0=no; 1=yes)', &
!sed       'none')/=0 ) CALL read_error(1, 'sed_route')

      IF ( Nlake > 0 .OR. Model == DOCUMENTATION ) THEN
        Mxnsos = getdim('mxnsos')
        IF ( Mxnsos.EQ.-1 ) RETURN
        IF ( Mxnsos < 1 ) Mxnsos = 1
        ALLOCATE ( Wv5(Mxnsos, Nlake), S5(Mxnsos, Nlake) )
        ALLOCATE ( C5(Mxnsos, Nlake), Wv15(Mxnsos, Nlake) )
        ALLOCATE ( S15(Mxnsos, Nlake), C15(Mxnsos, Nlake) )
        ALLOCATE ( Lake_coef(Nlake))

        ALLOCATE ( Lake_din1(Nlake) )
        IF ( declparam(MODNAME, 'lake_din1', 'nlake', 'double', &
             '0.1', '0.0', '1.0', &
             'Surface reservoir inflow from the previous time step', &
             'Surface reservoir inflow from the previous time step', &
             'cfs')/=0 ) CALL read_error(1, 'Lake_din1')

        IF ( declparam(MODNAME, 'lake_coef', 'nlake', 'double', &
             '0.1', '0.0', '1.0', &
             'Linear reservoir routing coefficient', &
             'Coefficient to route reservoir storage to streamflow using equation: res_flow = lake_coef * res_stor', &
             '1/day')/=0 ) CALL read_error(1, 'lake_coef')

        ALLOCATE ( O2(Mxnsos, Nlake) )
        IF ( declparam(MODNAME, 'o2', 'mxnsos,nlake', 'double', &
             '0.0', '0.0', '100000.0', &
            'Outflow values in outflow/storage table for Puls routing', &
            'Outflow values in outflow/storage table for Puls routing', &
             'cfs')/=0 ) CALL read_error(1, 'o2')

        ALLOCATE ( S2(Mxnsos, Nlake) )
        IF ( declparam(MODNAME, 's2', 'mxnsos,nlake', 'double', &
             '0.0', '0.0', '100000.0', &
            'Storage values in outflow/storage table for Puls routing', &
            'Storage values in outflow/storage table for Puls routing', &
             'cfs-days')/=0 ) CALL read_error(1, 's2')

        ALLOCATE ( Nsos(Nlake) )
        IF ( declparam(MODNAME, 'nsos', 'nlake', 'integer', &
             '0', '0', '10', &
          'Number of storage/outflow values in table for Puls routing', &
          'Number of storage/outflow values in table for Puls routing', &
          'none')/=0 ) CALL read_error(1, 'nsos')

        ALLOCATE ( Lake_type(Nlake) )
        IF ( declparam(MODNAME, 'lake_type', 'nlake', 'integer', &
             '8', '8', '12', &
             'Type of surface reservoir', &
             'Type of surface reservoir (8=Puls routing; 9=Linear routing; 10=Flow through;'// &
             ' 11=Broad crested weir; 12=Gate opening)', &
             'none')/=0 ) CALL read_error(1, 'lake_type')

        ALLOCATE ( Upst_res1(Nlake) )
        IF ( declparam(MODNAME, 'upst_res1', 'nlake', 'integer', &
             '0', 'bounded', 'nlake', &
             'Upstream reservoir index number 1', &
             'Index number for the first upstream reservoir whose outflow is inflow to this reservoir', &
             'none')/=0 ) CALL read_error(1, 'upst_res1')

        ALLOCATE ( Upst_res2(Nlake) )
        IF ( declparam(MODNAME, 'upst_res2', 'nlake', 'integer', &
             '0', 'bounded', 'nlake', &
             'Upstream reservoir index number 2', &
             'Index number for the second upstream reservoir whose outflow is inflow to this reservoir', &
             'none')/=0 ) CALL read_error(1, 'upst_res2')

        ALLOCATE ( Upst_res3(Nlake) )
        IF ( declparam(MODNAME, 'upst_res3', 'nlake', 'integer', &
             '0', 'bounded', 'nlake', &
             'Upstream reservoir index number 3', &
             'Index number for the third upstream reservoir whose outflow is inflow to this reservoir', &
             'none')/=0 ) CALL read_error(1, 'upst_res3')

        ALLOCATE ( Lake_sto(Nlake), Lake_outq(NlakeP1), Din1(Nlake) )
      ENDIF

! Allocate arrays for local variables
      ALLOCATE ( Qsrolat(Ncdels) )
      ALLOCATE ( Latsw(Nsegment), Upsw(Nsegment), Nxs(Nsegment) )
      ALLOCATE ( Mocgrids(Nsegment), Czero(Nsegment), Cone(Nsegment), Ctwo(Nsegment) )
      ALLOCATE ( Chan_loss_ftsec(Nsegment), Uprch_area(Nsegment), Ofar(Nsegment) )
      ALLOCATE ( Rb_hru(Nsegment), Rb_hru_area(Nsegment), Lb_hru(Nsegment), Lb_hru_area(Nsegment) )
      ALLOCATE ( Chan_xmoc(Ncmoc, Nsegment), Chan_amoc(Ncmoc, Nsegment) )
      ALLOCATE ( Alpr1(Nsegment), Cmp1(Nsegment), Chan_parm2(Nsegment) )
      ALLOCATE ( Tc_topw(Nsegment), Tc_half_topw(Nsegment) )
      ALLOCATE ( Dx(Nsegment), Dts(Nsegment), Dtdx(Nsegment), Qmxa(Nsegment) )
      ALLOCATE ( Qout(Ncdels, Nsegment), A_xs(Nchxs, Nsegment), Qinpast_xs(Nchxs, Nsegment) )
!      ALLOCATE ( Strahler_chan(Nsegment) )
      ALLOCATE ( Qp(Nchxs), Ap(Nchxs), Q(Nchxs), A(Nchxs) )
!sed  ALLOCATE ( Sedin(Ncdels), Sedinlat(Ncdels), Sedp(Nchxs) )
!sed  ALLOCATE ( Sed(Nchxs), Sedout(Ncdels, Nsegment), Sed_xs(Nchxs, Nsegment) )
!sed  ALLOCATE ( Sedout(Ncdels, Nsegment )

      END FUNCTION kchdecl

!***********************************************************************
!     kchinit - Initialize kinroute_chan module - get parameter values,
!               compute and check areas and types
!***********************************************************************
      INTEGER FUNCTION kchinit()
      USE PRMS_CONSTANTS, ONLY: ERROR_param, DNEARZERO, ERROR_dim, MAXFILE_LENGTH
      USE PRMS_MODULE, ONLY: Model_output_file, Nlake, Nsegment, Inputerror_flag, PRMS_output_unit, Nobs
      USE PRMS_BASIN, ONLY: Hru_area_dble, Active_area
      USE PRMS_ROUTING, ONLY: Tosegment, Segment_order
      USE PRMS_KROUT_CHAN
      USE PRMS_KROUT_OFPL, ONLY: Ofp_length, Ofp_route_time
      USE PRMS_GRNAMPT, ONLY: Nchxs
      IMPLICIT NONE
! Functions
      INTRINSIC :: INDEX, CHAR, DBLE
      EXTERNAL :: storminit, musk_init, AlphaRm, PRMS_open_output_file, read_error, error_stop, Dimchk
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: nc
!      INTEGER :: ichan
      INTEGER :: i, j, ios, found1, found2, jdown, toseg, k, kk
      INTEGER, ALLOCATABLE :: upsw_used(:)
      DOUBLE PRECISION :: ave_travel_time, min_travel_time, perdif, fac, o1
      CHARACTER(LEN=MAXFILE_LENGTH) :: output_path
!***********************************************************************
      kchinit = 0

!      IF ( getparam(MODNAME, 'strahler_num', Nstrahler, 'integer', Strahler_num)/=0 ) CALL read_error(2, 'strahler_num')
!      IF ( getparam(MODNAME, 'hru_strahler', Nstrahler, 'integer', Hru_strahler)/=0 ) CALL read_error(2, 'hru_strahler')

      IF ( getparam(MODNAME, 'chan_type', Nsegment, 'integer', Chan_type)/=0 ) CALL read_error(2, 'chan_type')
      IF ( getparam(MODNAME, 'chan_ndx', Nsegment, 'integer', Chan_ndx)/=0 ) CALL read_error(2, 'chan_ndx')
      IF ( getparam(MODNAME, 'chan_length', Nsegment, 'double', Chan_length)/=0 ) CALL read_error(2, 'chan_length')
      IF ( getparam(MODNAME, 'chan_width', Nsegment, 'double', Chan_width)/=0 ) CALL read_error(2, 'chan_width')
      IF ( getparam(MODNAME, 'chan_route_time', Nsegment, 'double', Chan_route_time)/=0 ) CALL read_error(2, 'chan_route_time')
      IF ( getparam(MODNAME, 'lat_inflowr', Nsegment, 'integer', Lat_inflowr)/=0 ) CALL read_error(2, 'lat_inflowr')
      IF ( getparam(MODNAME, 'lat_inflowl', Nsegment, 'integer', Lat_inflowl)/=0 ) CALL read_error(2, 'lat_inflowl')
      IF ( getparam(MODNAME, 'chan_rtemethod', 1, 'integer', Chan_rtemethod)/=0 ) CALL read_error(2, 'chan_rtemethod')
      IF ( getparam(MODNAME, 'chan_loss_rate', Nsegment, 'real', Chan_loss_rate)/=0 ) CALL read_error(2, 'chan_loss_rate')
      IF ( getparam(MODNAME, 'outlet_chan', 1, 'integer', Outlet_chan)/=0 ) CALL read_error(2, 'outlet_chan')
      IF ( Outlet_chan==0 ) Outlet_chan = Nsegment

      IF ( Nobs>0 ) THEN
        IF ( getparam(MODNAME, 'outlet_sta', 1, 'integer', Outlet_sta)/=0 ) CALL read_error(2, 'outlet_sta')
        IF ( Outlet_sta<1 .OR. Outlet_sta>Nobs ) THEN
          PRINT *, 'ERROR, invalid value specified for outlet_sta:', Outlet_sta
          Inputerror_flag = 1
        ENDIF
      ENDIF

      found1 = 0
      found2 = 0
      DO i = 1, Nsegment
        IF ( Chan_ndx(i)==0 ) THEN
          PRINT *, 'ERROR, chan_ndx cannot be 0, segment:', i
          Inputerror_flag = 1
        ENDIF
        IF ( Chan_length(i)==0.0 ) THEN
          PRINT *, 'ERROR, chan_length cannot be 0.0, segment:', i
          Inputerror_flag = 1
        ENDIF
        IF ( (Chan_type(i)<5 .OR. Chan_type(i)>9) .AND. found1==0 ) THEN
          found1 = 1
          IF ( getparam(MODNAME, 'chan_thresh', Nsegment, 'double', Chan_thresh)/=0 ) CALL read_error(2, 'chan_thresh')
          IF ( getparam(MODNAME, 'chan_slope', Nsegment, 'double', Chan_slope)/=0 ) CALL read_error(2, 'chan_slope')
          IF ( getparam(MODNAME, 'chan_rough', Nsegment, 'double', Chan_rough)/=0 ) CALL read_error(2, 'chan_rough')
          IF ( getparam(MODNAME, 'chan_alpha', Nsegment, 'double', Chan_alpha)/=0 ) CALL read_error(2, 'chan_alpha')
          IF ( getparam(MODNAME, 'chan_cmp', Nsegment, 'double', Chan_cmp)/=0 ) CALL read_error(2, 'chan_cmp')
          IF ( getparam(MODNAME, 'wpcoef_a', Nsegment, 'double', Wpcoef_a)/=0 ) CALL read_error(2, 'wpcoef_a')
          IF ( getparam(MODNAME, 'wpcoef_b', Nsegment, 'double', Wpcoef_b)/=0 ) CALL read_error(2, 'wpcoef_b')
        ENDIF

        IF ( Chan_type(i)==3 .AND. found2==0 ) THEN
          IF ( getparam(MODNAME, 'chan_t3_lbratio', Nsegment, 'double', Chan_t3_lbratio)/=0 ) CALL read_error(2, 'chan_t3_lbratio')
          IF ( getparam(MODNAME, 'chan_t3_rbratio', Nsegment, 'double', Chan_t3_rbratio)/=0 ) CALL read_error(2, 'chan_t3_rbratio')
        ENDIF
      ENDDO

      IF ( Chan_rtemethod==0 .OR. Chan_rtemethod==2 ) THEN
        IF ( getparam(MODNAME, 'chan_theta', 1, 'double', Chan_theta)/=0 ) CALL read_error(2, 'chan_theta')
        IF ( getparam(MODNAME, 'chan_chi', 1, 'double', Chan_chi)/=0 ) CALL read_error(2, 'chan_chi')
        IF ( Chan_chi==0.0 ) STOP 'ERROR, chan_chi must be > 0.0'
        Chifactor = (1.0-Chan_chi)/Chan_chi
      ENDIF

      IF ( Chan_rtemethod==3 ) THEN
        IF ( getparam(MODNAME, 'chan_parm1', Nsegment, 'double', Chan_parm1)/=0 ) RETURN
      ENDIF

!***compute upsw latsw and flgth for each channel
      Upsw = 0
      Maxup = 0
      DO i = 1, Nsegment
        Contrib_area_chan(i) = ZERO
        IF ( Lat_inflowr(i) == 0 .AND. Lat_inflowl(i) == 0 ) THEN
          Latsw(i) = 0
        ELSE
          !rsr, note does not include cascading areas, FIX
          IF ( Lat_inflowr(i)>0 ) Contrib_area_chan(i) = Hru_area_dble(Lat_inflowr(i))
          IF ( Lat_inflowl(i)>0 ) Contrib_area_chan(i) = Contrib_area_chan(i) + Hru_area_dble(Lat_inflowl(i))
          Latsw(i) = 1
        ENDIF

        toseg = Tosegment(i)
        IF ( toseg>0 ) THEN
          Upsw(toseg) = Upsw(toseg) + 1
          IF ( Upsw(toseg)>Maxup ) Maxup = Upsw(toseg)
          IF ( Chan_route_time(i)>Chan_route_time(toseg) ) THEN
            Inputerror_flag = 1
            PRINT *,  'ERROR, routing time for upstream channel:', toseg, &
     &                ' must be less than or equal to downstream channel:', i
          ENDIF
        ENDIF

        Chan_parm2(i) = ZERO
        IF ( Chan_type(i) == 1 ) THEN
          IF ( Chan_slope(i) < DNEARZERO .OR. Chan_rough(i) < DNEARZERO .OR. Chan_width(i) < DNEARZERO ) THEN
            PRINT *, 'Rectangular channel, type 1, parameters  chan_slope, chan_rough, and chan_width should be'// &
                     ' defined greater than 0. , chan#= ', i
            CALL error_stop('rectangular channel', ERROR_param)
          ENDIF
          Chan_parm1(i) = Chan_width(i)

        ELSEIF ( Chan_type(i) == 3 ) THEN
          IF ( Chan_slope(i) < DNEARZERO .OR. Chan_rough(i) < DNEARZERO &
               .OR. Chan_t3_rbratio(i) < DNEARZERO .OR. Chan_t3_lbratio(i) < DNEARZERO ) THEN
            PRINT *, 'Triangular channel, type 3, parameters chan_slope, chan_rough, chan_t3_rbratio and'// &
                  ' chan_t3_lratio should be defined greater than 0.0, chan#= ', i
            CALL error_stop('triangular channel', ERROR_param)
          ENDIF
          Tc_topw(i) = Chan_t3_lbratio(i) + Chan_t3_rbratio(i)
          Tc_half_topw(i) = Tc_topw(i) * 0.5D0
          Chan_parm1(i) = Chan_t3_lbratio(i)
          Chan_parm2(i) = Chan_t3_rbratio(i)

        ELSEIF ( Chan_type(i) == 4 ) THEN
          IF ( Chan_alpha(i) < DNEARZERO .OR. Chan_cmp(i) < DNEARZERO) THEN
            PRINT *, 'Rectangular channel, type 1, parameters chan_alpha, chan_cmp should be defined greater'// &
                     ' than 0. , chan#=  ', i
            CALL error_stop('rectangular channel', ERROR_param)
          ENDIF
          Chan_parm1(i) = Wpcoef_a(i)
          Chan_parm2(i) = Wpcoef_b(i)

!          ichan = 1
!          DO j = 1, Nstrahler
!            k = Lat_inflowl(ichan)
!            kk = Lat_inflowr(ichan)
!            jup = 0
!            IF ( k>0 ) THEN
!              IF ( Hru_type(k) == 2 ) THEN
!                Strahler_chan(ichan) = 0
!                jup = 1
!              ENDIF
!            ENDIF
!            IF ( kk > 0 ) THEN
!              IF ( Hru_type(kk) == 2 ) THEN
!                Strahler_chan(ichan) = 0
!                jup = 1
!              ENDIF
!            ENDIF
!            IF ( jup == 0 ) THEN
!              Strahler_chan(ichan) = Strahler_num(j)
!              ichan = ichan + 1
!            ENDIF
!          ENDDO
!          !rsr, assume constant chan_alpha
!          DO j = 1, Nsegment
!            IF ( Strahler_chan(j) /= 0 ) Chan_alpha(j) = (7-Strahler_chan(j)) * Chan_alpha(j)
!          ENDDO

        ELSEIF ( Chan_type(i) == 2 .OR. Chan_type(i) == 10 ) THEN
          IF ( Chan_parm1(i) < DNEARZERO ) THEN
            PRINT *, 'Channel type 2 or 10, parameters  chan_slope, chan_rough, and chan_parm1 should be'// &
                     ' defined greater than 0., chan#= ', i
            CALL error_stop('channel type 2 or 10', ERROR_param)
          ENDIF

        ELSEIF ( Chan_type(i) == 11 ) THEN
          Chan_parm1(i) = Wpcoef_a(i)
          Chan_parm2(i) = Wpcoef_b(i)

        ELSEIF ( Chan_type(i) < 7 .OR. Chan_type(i) > 12 ) THEN
          PRINT *, 'ERROR, invalid chan_type for channel:', i, ', chan_type:', Chan_type(i)
          CALL error_stop('krout_chan', ERROR_param)

!       ELSEIF ( Chan_type(i) == 8 .OR. Chan_type(i) == 9 ) THEN
!         PRINT *, 'Reservoir routing not yet implemented, chan_types 8 and 9 are not valid, chan#=  ', i
!         CALL error_stop('reservoir', ERROR_param)

        ENDIF

      ENDDO
      ALLOCATE ( Upchan(Nsegment,Maxup), upsw_used(Nsegment) )
      upsw_used = Upsw
      DO i = 1, Nsegment
        toseg = Tosegment(i)
        IF ( toseg>0 ) THEN
          Upchan(toseg,upsw_used(toseg)) = i
          upsw_used(toseg) = upsw_used(toseg) - 1
        ENDIF
      ENDDO
      DEALLOCATE ( upsw_used )

      fac = 1.0D0 / (LENGTH_CONV*3600.0D0)
      Ofar = ZERO
      Uprch_area = ZERO
      DO j = 1, Nsegment
        i = Segment_order(i)
        Nxs(i) = Chan_ndx(i) + 1
        CALL Dimchk( 'nchxs', Nchxs, Nxs(i) )
! channel type is 1, 2, 3, 4, 10, or 11
        IF ( Chan_type(i) < 5 .OR. Chan_type(i) > 9 ) THEN
          IF ( Chan_type(i) /= 4 ) &
               CALL AlphaRm(Chan_type(i), Chan_rough(i), Chan_slope(i), Chan_parm1(i), Chan_parm2(i), Chan_alpha(i), Chan_cmp(i))
          Alpr1(i) = 1.0D0 / Chan_alpha(i)
          Cmp1(i) = 1.0D0 / Chan_cmp(i)
        ENDIF
        Dx(i) = Chan_length(i) / Chan_ndx(i)
        Dts(i) = Chan_route_time(i) * 60.0D0
        Dtdx(i) = Dts(i) / Dx(i)
        Chan_loss_ftsec(i) = Chan_loss_rate(i)*fac
        !       chanarea = Chan_length(iorder)*Chan_width(iorder)*fac
        IF ( Lat_inflowr(i) > 0) Ofar(i) = Chan_length(i) * Ofp_length(Lat_inflowr(i)) ! + chanarea
        IF ( Lat_inflowl(i) > 0) Ofar(i) = Ofar(i) + Chan_length(i) * Ofp_length(Lat_inflowl(i)) ! + chanarea
        jdown = Tosegment(i)
        IF ( jdown>0 ) Uprch_area(jdown) = Uprch_area(jdown) + Ofar(i) + Uprch_area(i)
      ENDDO

      Ofarea_total = (Uprch_area(Outlet_chan) + Ofar(Outlet_chan)) * AREACONV
      perdif = ABS(Ofarea_total/Active_area-1.0D0)
      IF ( perdif>=0.05D0 ) THEN
        perdif = perdif*100.0D0
        WRITE ( PRMS_output_unit, * ) 'Sum of routed areas differs from basin area by more than 5 percent, percent =', perdif
        WRITE ( PRMS_output_unit, * ) 'Sum of routed area =', Ofarea_total
        WRITE ( PRMS_output_unit, * ) 'Active Basin area =', Active_area
      ENDIF

! initialize Method of Characteristics arrays
      IF ( Chan_rtemethod == 4 ) THEN
        Mocgrids = 0
        Chan_xmoc = ZERO
        Chan_amoc = ZERO
      ENDIF

      IF ( Chan_rtemethod == 1 ) THEN
        IF ( getparam(MODNAME, 'musk_wghtfac', Nsegment, 'integer', Musk_wghtfac)/=0 ) CALL read_error(2, 'musk_wghtfac')
        IF ( getparam(MODNAME, 'musk_travel_time', Nsegment, 'integer', Musk_travel_time)/=0 ) CALL read_error(2, 'musk_travel_time')
        min_travel_time = 1.0D0
        ave_travel_time = ZERO
        DO i = 1, Nsegment
          ave_travel_time = ave_travel_time + Musk_travel_time(i)
          IF ( min_travel_time > Musk_travel_time(i) ) min_travel_time = Musk_travel_time(i)
        ENDDO
        ave_travel_time = ave_travel_time/Nsegment
        WRITE ( PRMS_output_unit, * ) 'Minimum travel time for Method of Characteristics routing:', min_travel_time
        WRITE ( PRMS_output_unit, * ) 'Average travel time for Method of Characteristics routing:', min_travel_time
        CALL musk_init()
      ENDIF

!** reservoir computations  **************

      IF ( Nlake > 0 ) THEN
        IF ( getparam(MODNAME, 'lake_din1', Nlake, 'double', Lake_din1)/=0 ) CALL read_error(2, 'lake_din1')
        IF ( getparam(MODNAME, 's2', Mxnsos*Nlake, 'double', O2)/=0 ) CALL read_error(2, 's2')
        IF ( getparam(MODNAME, 's2', Mxnsos*Nlake, 'double', S2)/=0 ) CALL read_error(2, 's2')
        IF ( getparam(MODNAME, 'lake_type', Nlake, 'integer', Lake_type)/=0 ) CALL read_error(2, 'lake_type')
        IF ( getparam(MODNAME, 'lake_coef', Nlake, 'integer', Lake_coef)/=0 ) CALL read_error(2, 'lake_coef')
        IF ( getparam(MODNAME, 'nsos', Nlake, 'integer', Nsos)/=0 ) CALL read_error(2, 'nsos')
        IF ( getparam(MODNAME, 'upst_res1', Nlake, 'integer', Upst_res1)/=0 ) CALL read_error(2, 'upst_res1')
        IF ( getparam(MODNAME, 'upst_res2', Nlake, 'integer', Upst_res2)/=0 ) CALL read_error(2, 'upst_res2')
        IF ( getparam(MODNAME, 'upst_res3', Nlake, 'integer', Upst_res3)/=0 ) CALL read_error(2, 'upst_res3')
        S5 = ZERO
        C5 = ZERO
        S15 = ZERO
        C15 = ZERO
        DO j = 1, Nlake
          Din1(j) = Lake_din1(j)
          IF ( Lake_type(j) == 8 ) THEN
            kk = Nsos(j)
            DO k = 1, kk
              o1 = O2(k, j)*0.5
              Wv5(k, j) = S2(k, j)*288.0 + o1
              Wv15(k, j) = S2(k, j)*96.0 + o1
            ENDDO
            DO k = 2, kk
              o1 = O2(k, j) - O2(k-1, j)
              S5(k, j) = o1/(Wv5(k, j)-Wv5(k-1, j))
              C5(k, j) = O2(k, j) - S5(k, j)*Wv5(k, j)
              S15(k, j) = o1/(Wv15(k, j)-Wv15(k-1, j))
              C15(k, j) = O2(k, j) - S15(k, j)*Wv15(k, j)
            ENDDO
          ENDIF
        ENDDO
      ENDIF

! set parameters and switches at beginning of run
      CALL storminit()

      DO i = 1, Nsegment
        IF ( Latsw(i) /= 0 ) THEN
          IF ( Lat_inflowr(i) /= 0 ) THEN
            IF ( Ofp_route_time(Lat_inflowr(i)) > Chan_route_time(i) ) THEN
              PRINT *, 'Routing time for lateral inflow must be less than or equal to this'// &
                       ' channels routing time, chan#= ', i
              RETURN
            ENDIF
          ENDIF
          IF ( Lat_inflowl(i) /= 0 ) THEN
            IF ( Ofp_route_time(Lat_inflowl(i)) > Chan_route_time(i) ) THEN
              PRINT *, 'Routing time for lateral inflow must be less than or equal to this'// &
                       ' channels routing time, chan#= ', i
              RETURN
            ENDIF
          ENDIF

        ENDIF
      ENDDO

      Qinlat_chan = ZERO
      Q_xs = ZERO
      Qin = ZERO
      Qinpast_xs = ZERO
      A_xs = ZERO
!sed  Sed_xs = ZERO
!sed  Sedout = ZERO
!sed  Sed_chan = ZERO
!sed  Sed_tot = ZERO
      Storm_pk_obs = ZERO
      Storm_pk_sim = ZERO
      Qinlat = ZERO

!-----open the storm file.
      File_unit = 880
      nc = INDEX(Model_output_file,CHAR(0)) - 1
      IF ( nc>MAXFILE_LENGTH-6 ) nc = MAXFILE_LENGTH - 6
      output_path(1:nc+6) = output_path(1:nc)//'.storm'
      CALL PRMS_open_output_file(File_unit, output_path, 'storm_output_file', 0, ios)
      IF ( ios/=0 ) STOP
      WRITE (*, '(//,2A,//)') '***Storm output in: ', output_path(1:nc)
      WRITE (File_unit, 9001)

!      CALL PRMS_open_output_file(File_unit2, output_path(1:nc)//'.final', 'storm_final_file', 0, ios)
!      IF ( ios/=0 ) STOP
!      WRITE (File_unit2, 9002)

 9001 FORMAT (' #   meas    storm   routed    obs       sim    meas ', &
              4('   storm'), /, '     pptsum   vol      vol   outflow', &
              '    peak     peak   pptexc  ssfsum  gwfsum  sroff'/'___', &
              2(' _______'), ' ________', 2(' ________'), 5(' _______'))
!9002 FORMAT ('Channel Volume:  Initial   Final   Storm net Net Inches', /, 15x, 4('--------- '))

      END FUNCTION kchinit

!***********************************************************************
!     kchrun - Adds inflows to channels and uses kinematic routing
!              procedures to compute channel flow
!***********************************************************************
      INTEGER FUNCTION kchrun()
      USE PRMS_KROUT_CHAN
      USE PRMS_MODULE, ONLY: Nobs, Nsegment
      USE PRMS_SET_TIME, ONLY: Timestep_seconds, Storm_status, Storm_num
      USE PRMS_BASIN, ONLY: Hru_area_dble
      USE PRMS_ROUTING, ONLY: Use_transfer_segment, Segment_order, Obsin_segment
      USE PRMS_OBS, ONLY: Streamflow_cfs
      USE PRMS_INTCP, ONLY: Basin_net_ppt
      USE PRMS_FLOWVARS, ONLY: Ssres_flow, Basin_ssflow
      USE PRMS_GRNAMPT, ONLY: Storm_pptexc, Storm_obsvol, Storm_obspk
      USE PRMS_GWFLOW, ONLY: Gwres_flow, Basin_gwflow
      USE PRMS_KROUT_OFPL, ONLY: Ofp_route_time !, Sedin, Sed_route
      USE PRMS_WATER_USE, ONLY: Segment_gain, Segment_transfer
      IMPLICIT NONE
! Functions and Externals
      INTRINSIC :: DBLE
      EXTERNAL :: AccumLat, reservoir, storminit, rte_run1, rte_compvars, rte_inflow
      EXTERNAL :: rte_runloop, rte_flowleft, rte_chaninit
! Local Variables
      INTEGER :: k, ires, ich, jch, nxsj, ndels, chantype, jj, i, ihru
      DOUBLE PRECISION :: alpha, alpha1, m, m_inv, m_1, m_1abs, b1, st_vol, ssres_flow_dble, gwres_flow_dble
      DOUBLE PRECISION :: dt_dx, alpha_m, dtdxchi_inv, alpdts, alp_mdts, dt_conv, cnv
!     DOUBLE PRECISION :: chan_sum_final, chan_net
! Save Variables
      DOUBLE PRECISION, SAVE :: st_ppt_sum, st_ssf_sum, st_gwf_sum, q_peak, Storm_vol_obs
!***********************************************************************
      kchrun = 0

! check for whether storm period
! Storm_status =  Switch signifying storm status,
!   0=not in storm (daily mode)
!   1=first time step of storm
!   2=middle of storm
!   3=storm end

      IF ( Storm_status==0 ) RETURN

!  if Storm_status = 1 initialize storm variables
      IF ( Storm_status == 1 ) THEN
        CALL storminit()
        Storm_pk_obs = ZERO
        Storm_pk_sim = ZERO
        Dt_sroff = ZERO
        st_ppt_sum = ZERO
        st_ssf_sum = ZERO
        st_gwf_sum = ZERO
        q_peak = ZERO

        CALL rte_chaninit()
        Chanvol_sum = Chan_sum_init

!sed    IF ( Sed_route == 1 ) THEN
!sed      Sed_xs = ZERO
!sed      Sedout = ZERO
!sed    ENDIF

! if Storm_status = 3, then write out totals from previous storm
      ELSEIF ( Storm_status == 3 ) THEN
        st_vol = St_sroff + st_ssf_sum + st_gwf_sum
        IF ( Nobs > 0 ) THEN
          Storm_vol_obs = Storm_obsvol(Outlet_sta)
          Storm_pk_obs = Storm_obspk(Outlet_sta)
        ENDIF
        Storm_pk_sim = q_peak

        WRITE ( File_unit, 9001 ) Storm_num, st_ppt_sum, st_vol, Storm_routvol, Storm_vol_obs, &
                                  q_peak, Storm_pk_obs, Storm_pptexc, st_ssf_sum, st_gwf_sum, St_sroff
!       WRITE (File_unit, *) Nowtime
!       PRINT *, Nowtime

! Compute flow left in channels at the end of the storm
!       CALL rte_flowleft(chan_sum_final)
!       chan_net = chan_sum_final - Chan_sum_init
!       WRITE ( File_unit2, 9002 ) Chan_sum_init, chan_sum_final, chan_net, chan_net*FT3TOINCHES/Ofarea_total
        RETURN
      ENDIF

      CALL rte_run1()

!***
!*** loop for each channel and reservoir segment(Nsegment)***
!***
      DO i = 1, Nsegment
        ich = Segment_order(i)
        Qinlat_chan(ich) = ZERO
        jch = ich
        CALL rte_compvars(nxsj, chantype, dt_dx, dtdxchi_inv, alpha, alpha1, m, m_inv, m_1, m_1abs, alpdts, &
                          alpha_m, alp_mdts, b1, ndels, Nxs(ich), Chan_type(ich), Dtdx(ich), Chan_alpha(ich), &
                          Alpr1(ich), Chan_cmp(ich), Cmp1(ich), Dts(ich), Chan_route_time(ich), Chan_chi)

        CALL rte_inflow(ich, ndels, Upsw(ich)) ! segment order in routine ??
!***
!*** Determine and accumulate lateral inflows to segments ***
!***
        Qinlat = ZERO
        Qsrolat = ZERO
!sed    Sedinlat = ZERO

!*** Determine and accumulate upstream input to segment ich ***
        IF ( Latsw(ich) == 1 ) THEN
         ssres_flow_dble = DBLE(Ssres_flow(Rb_hru(ich)))
         gwres_flow_dble = DBLE(Gwres_flow(Rb_hru(ich)))
!***     Determine lateral inflow from right bank
          ihru = Lat_inflowr(ich)
          IF ( ihru>0 ) &
     &         CALL AccumLat(ihru, ndels, Hru_area_dble(ihru), ssres_flow_dble, &
     &                      gwres_flow_dble, Chan_length(ich), Ofp_route_time(ihru), Qinlat, Qsrolat)
          !***     Determine lateral inflow from left bank
          ihru = Lat_inflowl(ich)
          IF ( ihru>0 ) &
     &         CALL AccumLat(ihru, ndels, Hru_area_dble(ihru), ssres_flow_dble, &
     &                      gwres_flow_dble, Chan_length(ich), Ofp_route_time(ihru), Qinlat, Qsrolat)

          dt_conv = Chan_length(ich)*Timestep_seconds / ndels
          DO k = 1, ndels
            Dt_sroff = Dt_sroff + Qsrolat(k) * dt_conv
          ENDDO
        ENDIF

        Qin_chan(ich) = Qin(1)
        Qin_totchan(ich) = Qin(1) + Qinlat(1)*Dx(ich) !cfs
        Qin_instant(ich) = Qin(ndels)

!*** Setup channel routing ***

!*** Check for junction
! junctions simply set output from junction equal to input to junction
        IF ( chantype == 7 ) THEN
!sed      IF ( Sed_route == 1 ) THEN
!sed        DO k = 1, ndels
!sed          Sedout(k, ich) = Sedin(k)
!sed        ENDDO
!sed      ENDIF
          DO k = 1, ndels
            Qout(k, ich) = Qin(k)
          ENDDO
          Q_chan(ich) = Qin(ndels)

!*** Check for reservoir
        ELSEIF ( chantype == 8 .OR. chantype == 9 ) THEN
          ires = Chan_ndx(ich)
          CALL reservoir(ires, ndels, chantype, Chan_route_time(ich), Qout(1, ich), Wv5(1, ires), C5(1, ires), &
                         S5(1, ires), Wv15(1, ires), C15(1, ires), S15(1, ires), Nsos(ires), Lake_coef(ires), &
                         Upst_res1(ires), Upst_res2(ires), Upst_res3(ires), Lake_sto(ires), Din1(ires))
          Q_chan(ich) = Qout(ndels, ich)

!*** Must be a channel, chantype = 1, 2, 3, 4, 10, or 11
        ELSE
          CALL rte_runloop(jch, ndels, nxsj, chantype, Chan_loss_ftsec(ich), Chan_width(ich), &
                           Tc_half_topw(ich), Tc_topw(ich), Wpcoef_a(ich), Wpcoef_b(ich), Dts(ich), &
                           dt_dx, alpha, alpha1, m, m_1, m_inv, m_1abs, Dx(ich), alp_mdts, alpdts, alpha_m, &
                           Chan_parm1(ich), Chan_parm2(ich), Chan_slope(ich), dtdxchi_inv, b1, &
                           Mocgrids(ich), Chan_xmoc(1, ich), Chan_amoc(1, ich), Qmxa(ich), Qout(1, ich), &
                           Chan_thresh(ich), Czero(ich), Cone(ich), Ctwo(ich), Q_chan(ich), Q_xs(1, ich), &
                           A_xs(1, ich), Qinpast_xs(1, ich))
!sed                       A_xs(1, ich), Sed_xs(1, ich), Sedout(1, ich), &
!sed                       Sed_chan(ich), Qinpast_xs(1, ich))

          !???RSR??? need to figure out where this goes
          IF ( Obsin_segment(ich)>0 ) Q_chan(ich) = Streamflow_cfs(Obsin_segment(ich))
          IF ( Use_transfer_segment==1 ) &
               Q_chan(ich) = Q_chan(ich) + DBLE( Segment_gain(ich) - Segment_transfer(ich) )
        ENDIF
        Q_chan_timestep(ich) = ZERO
        Qinlat_chan_ts(ich) = ZERO
        cnv = Chan_length(ich)*Dts(ich)
        DO jj = 1, ndels
          Q_chan_timestep(ich) = Q_chan_timestep(ich) + Qout(jj, ich)*Dts(ich)
          Qinlat_chan_ts(ich) = Qinlat_chan_ts(ich) + Qinlat(jj)*cnv
        ENDDO
        Q_chan_timestep(ich) = Q_chan_timestep(ich) / Timestep_seconds
        Qinlat_chan_ts(ich) = Qinlat_chan_ts(ich) / Timestep_seconds

!*** End of Nsegment (ich) loop
      ENDDO

      Qday_sum = Qday_sum + (Q_chan(Outlet_chan)*Timestep_seconds)
!sed  IF ( Sed_route == 1 ) Sed_tot = Sed_tot + Sed_chan(Outlet_chan)
      Dt_sroff = Dt_sroff*FT3TOINCHES/Ofarea_total
      St_sroff = St_sroff + Dt_sroff

!   Compute streamflow for this time step.
      st_ppt_sum = st_ppt_sum + Basin_net_ppt
      st_ssf_sum = st_ssf_sum + Basin_ssflow
      st_gwf_sum = st_gwf_sum + Basin_gwflow

      Storm_routvol = Chanvol_nchansum*FT3TOINCHES/Ofarea_total

      IF ( Q_chan(Outlet_chan) > q_peak ) q_peak = Q_chan(Outlet_chan)

! Compute flow left in channel segments at the end of a timestep
      CALL rte_flowleft(Chanvol_sum)

 9001 FORMAT (I3, 2F8.2, F9.3, F8.2, 2F9.1, 4F8.3)
!9002 FORMAT (14X, 3F10.0, F10.3)

      END FUNCTION kchrun

!***********************************************************************
!     kchclean - Close the channel file
!***********************************************************************
      INTEGER FUNCTION kchclean()
      USE PRMS_KROUT_CHAN, ONLY: File_unit
      IMPLICIT NONE
!***********************************************************************
      CLOSE (UNIT=File_unit)
!     CLOSE (UNIT=File_unit2)
      kchclean = 0
      END FUNCTION kchclean

!***********************************************************************
!***********************************************************************
      SUBROUTINE storminit()
      USE PRMS_KROUT_CHAN, ONLY: Dt_sroff, St_sroff, Chanvol_daysum, &
                                 Chanvol_nchansum, Chanvol_sum, Storm_routvol, Qin_chan, &
                                 Qin_totchan, Qout, Q_chan, Qday_sum, Q_chan_timestep, Qinlat_chan_ts, ZERO
      IMPLICIT NONE
!***********************************************************************
      Dt_sroff = ZERO
      St_sroff = ZERO
      Storm_routvol = ZERO
      Chanvol_daysum = ZERO
      Chanvol_nchansum = ZERO
      Chanvol_sum = ZERO
      Qday_sum = ZERO
      Qout = ZERO
      Q_chan =-11.0D0
      Qin_chan = -11.0D0
      Q_chan_timestep = -11.0D0
      Qin_totchan = -11.0D0
      Qinlat_chan_ts = -11.0D0

      !rsr, initialize Q_chan and Qin_chan from daily flows
      END SUBROUTINE storminit

!***********************************************************************
!  Accumulate the lateral inflows and sediment over the timesteps
!***********************************************************************
      SUBROUTINE AccumLat(Jlat, Ndels, Hru_area, Ssflow, Gwflow, Chlt, Ortm, Qinlat, Qsrolat)
      USE PRMS_KROUT_CHAN, ONLY: ZERO
      USE PRMS_KROUT_OFPL, ONLY: Q_ndels !, Sed_ndels
!sed  USE PRMS_KROUT_CHAN, ONLY: Sedinlat
      USE PRMS_SET_TIME, ONLY: Timestep_minutes
      IMPLICIT NONE
      INTRINSIC :: NINT
      EXTERNAL :: AccumData
! Variable Definitions
!    Ndels    - Number of timesteps in a data time step
!    ndelslat - Number of timesteps in a data time step, lateral
!    (ofp_route_time)
! Arguments
      INTEGER, INTENT(IN) :: Jlat, Ndels
      DOUBLE PRECISION, INTENT(IN) :: Chlt, Ssflow, Gwflow, Hru_area, Ortm
      DOUBLE PRECISION, INTENT(INOUT) :: Qinlat(Ndels)
      DOUBLE PRECISION, INTENT(INOUT) :: Qsrolat(Ndels)
! Local Variables
      INTEGER :: k, ndelslat, have_sro
      DOUBLE PRECISION :: flw_pct, unit_flw, sro(Ndels)
!***********************************************************************
      ndelslat = NINT( Timestep_minutes/Ortm )
      sro = ZERO !sro in cfs or ft^2/s???
      CALL AccumData( Ndels, ndelslat, Q_ndels(:, Jlat), sro )
      have_sro = 0
      DO k = 1, Ndels
        Qsrolat(k) = Qsrolat(k) + sro(k)
        IF ( sro(k) > 0 ) have_sro = 1
      ENDDO
!sed  CALL AccumData( Ndels, ndelslat, Sed_ndels(:, Jlat), Sedinlat )
      flw_pct = DBLE( (Ssflow + Gwflow) ) * Hru_area
      IF ( flw_pct > ZERO .OR. have_sro == 1 ) THEN
        unit_flw = flw_pct/Chlt
        DO k = 1, Ndels
          Qinlat(k) = Qinlat(k) + sro(k) + unit_flw !what are units???
        ENDDO
      ENDIF
      END SUBROUTINE AccumLat

!***********************************************************************
! Initialize channel cross sections to percentage of flow and MOC arrays
!***********************************************************************
      SUBROUTINE rte_chaninit()
      USE PRMS_KROUT_CHAN, ONLY: Chan_rtemethod, AREACONV, &
     &    Mocgrids, Ofarea_total, Ofar, Uprch_area, Chan_sum_init, Q_xs, Chan_xmoc, Chan_amoc, Alpr1, Cmp1, Dx, &
     &    Dts, A_xs, Qout, Qinpast_xs, Qmxa, Nxs, ZERO
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_FLOWVARS, ONLY: Basin_cfs
      USE PRMS_SET_TIME, ONLY: Timestep_seconds
      USE PRMS_GRNAMPT, ONLY: Ncdels
!sed  USE PRMS_KROUT_OFPL, ONLY: Sed_route
      IMPLICIT NONE
! Functions
      EXTERNAL :: MethofCharInit
!     EXTERNAL :: error_stop
! Local Variables
      INTEGER :: i, j, k, nxsj
      DOUBLE PRECISION :: aquo, accumf, percentq, qmax, flwareain, flwareaout
!***********************************************************************
!sed  IF ( (Chan_rtemethod == 4 .OR. Chan_rtemethod == 1) .AND. Sed_route == 1 ) THEN
!sed    CALL error_stop ('cannot route sediment with MOC or Muskingum routing', ERROR_param)
!sed  ENDIF

      Chan_sum_init = ZERO
      DO i = 1, Nsegment
        nxsj = Nxs(i)
        aquo = Ofar(i) / (nxsj-1)
        accumf = Uprch_area(i)
        DO j = 1, nxsj
          percentq = (accumf*AREACONV / Ofarea_total) * Basin_cfs
          Q_xs(j, i) = percentq
          Qinpast_xs(j, i) = percentq
          A_xs(j, i) = (Alpr1(i)*percentq)**Cmp1(i)
          Chan_sum_init = Chan_sum_init + percentq*Timestep_seconds
          accumf = accumf + aquo
        ENDDO
        qmax = Q_xs(nxsj, i)
        Qmxa(i) = qmax
! Initialize Qout at all timesteps to output Q of channel
        DO k = 1, Ncdels
          Qout(k, i) = qmax
        ENDDO
! Initialize MOC arrays
        IF ( Chan_rtemethod == 4 ) THEN
          Mocgrids(i) = Nxs(i)
          flwareain = Q_xs(1, i)*Dts(i) / Dx(i)
          flwareaout = Q_xs(nxsj, i) * Dts(i)/Dx(i)
          CALL MethofCharInit(flwareain, flwareaout, Dx(i), Mocgrids(i), Chan_xmoc(1, i), Chan_amoc(1, i))
        ENDIF
      ENDDO

      END SUBROUTINE rte_chaninit

!***********************************************************************
! initial run processing
!***********************************************************************
      SUBROUTINE rte_run1()
      USE PRMS_KROUT_CHAN, ONLY: FTSQ2ACRE_CONV, LENGTH_CONV, Chanvol_daysum, Dt_sroff, Qday_sum, ZERO
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_tot
      USE PRMS_BASIN, ONLY: Hru_area_dble
      USE PRMS_FLOWVARS, ONLY: Ssres_flow
      USE PRMS_SET_TIME, ONLY: Timestep_seconds, Newday
      USE PRMS_GWFLOW, ONLY: Gwres_flow
      IMPLICIT NONE
      INTRINSIC :: DBLE
! Local Variables
      DOUBLE PRECISION :: cfs_conv
!***********************************************************************
      cfs_conv = FTSQ2ACRE_CONV / (LENGTH_CONV*Timestep_seconds)

      IF ( Newday == 1 ) THEN
        Chanvol_daysum = ZERO
        Qday_sum = ZERO
!sed    Sed_tot = ZERO
      ENDIF

      Ssres_flow = DBLE( Ssres_flow ) * Hru_area_dble * cfs_conv
      Gwres_flow = DBLE( Gwres_flow ) * Hru_area_dble * cfs_conv

      Dt_sroff = ZERO

      END SUBROUTINE rte_run1

!***********************************************************************
! set computational variables
!***********************************************************************
      SUBROUTINE rte_compvars(Nxsj, Chantype, Dt_dx, Dtdxchi_inv, Alpha, &
                              Alpha1, M, M_inv, M_1, M_1abs, Alpdts, Alpha_m, Alp_mdts, B1, Ndels, Nxs, &
                              Chan_type, Dtdx, Alpr, Alpr1, Cmp, Cmp1, Dts, Rtm, Chan_chi)
      USE PRMS_SET_TIME, ONLY: Timestep_minutes
      IMPLICIT NONE
! Functions
      INTRINSIC :: ABS, NINT
! Arguments
      INTEGER, INTENT(IN) :: Nxs, Chan_type
      INTEGER, INTENT(OUT) :: Nxsj, Chantype, Ndels
      DOUBLE PRECISION, INTENT(IN) :: Dtdx, Alpr, Alpr1, Cmp, Cmp1, Rtm, Chan_chi, Dts
      DOUBLE PRECISION, INTENT(OUT) :: Dt_dx, Dtdxchi_inv, Alpha, Alpha1, Alpdts
      DOUBLE PRECISION, INTENT(OUT) :: Alpha_m, Alp_mdts, M, M_inv, M_1, M_1abs, B1
!***********************************************************************
      Nxsj = Nxs
      Chantype = Chan_type
      Dt_dx = Dtdx
      Dtdxchi_inv = 1.0D0 / (Dt_dx*Chan_chi)
      Alpha = Alpr
      Alpha1 = Alpr1
      M = Cmp
      M_inv = Cmp1
      M_1 = M - 1.0D0
      M_1abs = ABS(M_1)
      Alpdts = Alpha * Dts
      Alpha_m = Alpha * M
      Alp_mdts = Alpdts * M
      B1 = Alpha_m * Dt_dx
      Ndels = NINT( Timestep_minutes/Rtm )
      END SUBROUTINE rte_compvars

!***********************************************************************
! compute inflow to a channel segment (upstream gains and diversions)
!***********************************************************************
      SUBROUTINE rte_inflow(Jch, Ndels, Upsw)
      USE PRMS_KROUT_CHAN, ONLY: Chan_route_time, Qin, Qout, Upchan, ZERO
!sed  USE PRMS_KROUT_OFPL, ONLY: Sed_route, Sedin, Sedout
      USE PRMS_SET_TIME, ONLY: Timestep_minutes
      USE PRMS_ROUTING, ONLY: Use_transfer_segment
      USE PRMS_WATER_USE, ONLY: Segment_transfer, Segment_gain
      IMPLICIT NONE
! Functions
      INTRINSIC :: NINT
      EXTERNAL :: AccumQSed
! Arguments
      INTEGER, INTENT(IN) :: Jch, Ndels, Upsw
! Local Variables
      INTEGER :: k, ndelsup, iup
      DOUBLE PRECISION :: qinit
!***********************************************************************
! check for gain inflow, allow for more than one gain
      qinit = 0.0
      IF ( Use_transfer_segment==1 ) qinit = DBLE( Segment_gain(Jch) - Segment_transfer(Jch) )

!*** Initialize flow arrays for upstream and lateral input
      DO k = 1, Ndels
        Qin(k) = qinit
      ENDDO
!*** Initialize sediment array for upstream and lateral input
!sed  IF ( Sed_route == 1 ) Sedin = ZERO
!*** Determine and accumulate upstream input to channel branch jch ***
      IF ( Upsw > 0 ) THEN
        DO k = 1, Upsw
          iup = Upchan(Jch, k)
          IF ( iup > 0 ) THEN
            ndelsup = NINT( Timestep_minutes/Chan_route_time(iup) )
            CALL AccumQSed(Ndels, ndelsup, Qout(:, iup), Qin)
!sed        CALL AccumQSed(Ndels, ndelsup, Qout(:, iup), Sedout(:, iup), Qin, Sedin)
          ENDIF
        ENDDO
!*** Add in sediment
!sed    IF ( Sed_route == 1 ) THEN
!sed      DO k = 1, Ndels
!sed        IF ( Qin(k) > ZERO .OR. Qout(k, Jch) > ZERO ) THEN
!sed          Sedin(k) = (Sedin(k)*Qin(k)+Sedout(k, Jch)*Qout(k, Jch)) / (Qin(k)+Qout(k, Jch))
!sed        ELSE
!sed          Sedin(k) = ZERO
!sed        ENDIF
!sed      ENDDO
!sed    ENDIF
      ENDIF
      END SUBROUTINE rte_inflow

!***********************************************************************
!  Accumulate the upstream flow and sediment over the timesteps
!***********************************************************************
      SUBROUTINE AccumQSed(Ndels, Ndelsup, Qout, Qin)
!sed  SUBROUTINE AccumQSed(Ndels, Ndelsup, Qout, Sedout, Qin, Sedin)
!sed  USE PRMS_KROUT_OFPL, ONLY: Sed_route
      IMPLICIT NONE
      EXTERNAL AccumData
! Argument Definitions
!    Ndels   - Number of timesteps in a data time step of channel
!    Ndelsup - Number of timesteps in a data time step, upstream
!    Qout    -
!    Sedout  -
!    Qin     -
!    Sedin   -
      INTEGER, INTENT(IN) :: Ndels, Ndelsup
      DOUBLE PRECISION, INTENT(IN) :: Qout(Ndels)
      DOUBLE PRECISION, INTENT(INOUT) :: Qin(Ndels)
!sed  DOUBLE PRECISION, INTENT(IN) :: Sedout(Ndels)
!sed  DOUBLE PRECISION, INTENT(INOUT) :: Sedin(Ndels)
!***********************************************************************
      CALL AccumData(Ndels, Ndelsup, Qout, Qin)
!sed  IF ( Sed_route == 1 ) CALL AccumData(Ndels, Ndelsup, Sedout, Sedin)
      END SUBROUTINE AccumQSed

!***********************************************************************
!  Accumulate values over the timesteps of upstream and current channel
!***********************************************************************
      SUBROUTINE AccumData(Ndels, Ndelsup, Dataout, Datain)
      IMPLICIT NONE
! Argument Definitions
!    Ndels   - Number of timesteps in a data time step of channel
!    Ndelsup - Number of timesteps in a data time step, upstream
!    Dataout    -
!    Datain     -
      INTEGER, INTENT(IN) :: Ndels, Ndelsup
      DOUBLE PRECISION, INTENT(IN) :: Dataout(Ndels)
      DOUBLE PRECISION, INTENT(INOUT) :: Datain(Ndels)
! Local Variables
      INTEGER :: kdels, k, kk, ki
      DOUBLE PRECISION :: fac
!***********************************************************************
      IF ( Ndelsup > Ndels ) THEN
        kdels = Ndelsup/Ndels
        fac = 1.0D0 / kdels
        ki = 0
        DO k = 1, Ndels
          DO kk = 1, kdels
            ki = ki + 1
            Datain(k) = Datain(k) + Dataout(ki) * fac
          ENDDO
        ENDDO
      ELSE
        DO k = 1, Ndels
          Datain(k) = Datain(k) + Dataout(k)
        ENDDO
      ENDIF
      END SUBROUTINE AccumData

!***********************************************************************
!***********************************************************************
      SUBROUTINE rte_runloop(Jch, Ndels, Nxsj, Chantype, Chan_loss_ftsec, Chan_width, Tc_half_topw, &
                             Tc_topw, Wpcoef_a, Wpcoef_b, Dts, Dt_dx, Alpha, Alpha1, M, M_1, M_inv, M_1abs, Dx, &
                             Alp_mdts, Alpdts, Alpha_m, Chan_parm1, Chan_parm2, Chan_slope, Dtdxchi_inv, B1, &
                             Mocgrids, Chan_xmoc, Chan_amoc, Qmxa, Qout, Thres, Czero, Cone, Ctwo, Q_chan, Q_xs, &
                             A_xs, Qinpast_xs)
!sed                         A_xs, Sed_xs, Sedout, Sed_chan, Qinpast_xs)
      USE PRMS_CONSTANTS, ONLY: DNEARZERO
      USE PRMS_KROUT_CHAN, ONLY: Chan_rtemethod, Chanvol_daysum, Qin, Qinlat, &
          Chanvol_nchansum, Qp, Ap, Q, A, Outlet_chan, Qinlat_chan, ZERO !, Sedinlat
!sed  USE PRMS_KROUT_OFPL, ONLY: Sed_route, Sedin, Sed, Sedp
      USE PRMS_GRNAMPT, ONLY: Ncdels, Nchxs, Ncmoc
      IMPLICIT NONE
! Functions
      INTRINSIC :: SQRT
      EXTERNAL :: KW_loop, Musk_loop, DW_loop, MethofChar, M1_impl_loop
! Arguments
      INTEGER, INTENT(IN) :: Jch, Ndels, Chantype, Nxsj
      INTEGER, INTENT(INOUT) :: Mocgrids
      DOUBLE PRECISION, INTENT(IN) :: Chan_loss_ftsec, Chan_width, Tc_half_topw, Tc_topw, Wpcoef_a, Wpcoef_b
      DOUBLE PRECISION, INTENT(IN) :: Alpha, Alpha1, Alp_mdts, Alpdts, Alpha_m, Dts, M, M_1, M_inv, M_1abs, Thres, Dx, Dt_dx, B1
      DOUBLE PRECISION, INTENT(IN) :: Chan_parm1, Chan_parm2, Chan_slope, Dtdxchi_inv, Czero, Cone, Ctwo
      DOUBLE PRECISION, INTENT(OUT) :: Q_chan, Qout(Ncdels)
!sed  DOUBLE PRECISION, INTENT(OUT) :: Sed_chan
!sed  DOUBLE PRECISION, INTENT(INOUT) Sed_xs(Nchxs), Sedout(Ncdels)
      DOUBLE PRECISION, INTENT(INOUT) :: Qmxa
      DOUBLE PRECISION, INTENT(INOUT) :: Qinpast_xs(Nchxs), Q_xs(Nchxs), A_xs(Nchxs), Chan_amoc(Ncmoc), Chan_xmoc(Ncmoc)
! Local Variables
      INTEGER :: k, j
      DOUBLE PRECISION :: flow_h, chan_loss_ft2, wp, q_lat, a_lat, ain, qdt, qlatdx
!sed  DOUBLE PRECISION :: sed_l
!***********************************************************************
!*** Initialize H, Q, and Sed arrays***
      DO j = 1, Nxsj
        Q(j) = Q_xs(j)
        A(j) = A_xs(j)
      ENDDO

!sed  IF ( Sed_route == 1 ) THEN
!sed    DO j = 1, Nxsj
!sed      Sed(j) = Sed_xs(j)
!sed      Sedp(j) = ZERO
!sed    ENDDO
!sed  ENDIF

      IF ( Chantype < 3 .OR. Chantype > 9 ) THEN
        chan_loss_ft2 = Chan_loss_ftsec * Chan_width
      ELSE
        chan_loss_ft2 = ZERO
      ENDIF

      DO k = 1, Ndels
        Qp(1) = Qin(k)
        Ap(1) = (Alpha1 * Qp(1))**M_inv

        IF ( Chantype == 3 ) THEN
          flow_h = SQRT(Ap(1) / Tc_half_topw)
          chan_loss_ft2 = flow_h * Tc_topw * Chan_loss_ftsec
        ELSEIF ( Chantype == 4 ) THEN
          wp = Wpcoef_a * Ap(1)**Wpcoef_b
          chan_loss_ft2 = wp * Chan_loss_ftsec
        ENDIF

        q_lat = Qinlat(k) - chan_loss_ft2 !rsr, are units correct, ft^2/s?
        a_lat = q_lat * Dts

        IF ( Qmxa > Thres .OR. q_lat > ZERO .OR. Qp(1) > ZERO ) THEN
          Qmxa = Qp(1)
          qlatdx = q_lat * Dx
          Qinlat_chan(Jch) = Qinlat_chan(Jch) + qlatdx
!sed      IF ( Sed_route == 1 ) THEN
!sed        Sedp(1) = Sedin(k)
!sed        sed_l = Sedinlat(k)
!sed      ENDIF

! ai = a(j-1), api = ap(j-1), aj = a(j), apj = ap(j)
! qi = q(j-1), qpi = qp(j-1), qj = q(j), qpj = qp(j)
! from dr3m: QS(j-1)=qi; QS(j)=qj; XQ(j-1)=qpi; XQ(j)=qpj
! from dr3m: AR(j-1)=ai; AR(j)=aj; XA(j-1)=api; XA(j)=apj

! Explicit or Implicit Kinematic Wave
          IF ( Chan_rtemethod == 0 .OR. Chan_rtemethod == 2 ) THEN
            CALL KW_loop(Nxsj, Alpha, Alpha_m, M, M_1, M_1abs, M_inv, &
                         B1, a_lat, qlatdx, Dtdxchi_inv, Dt_dx, Q, Qp, A, Ap, Qmxa)
!sed                     B1, a_lat, qlatdx, sed_l, Dtdxchi_inv, Dts, &
!sed                     Dt_dx, Dx, Q, Qp, A, Ap, Qmxa, Sed, Sedp)

! Muskingum
          ELSEIF ( Chan_rtemethod == 1 ) THEN
            CALL Musk_loop(Nxsj, Czero, Cone, Ctwo, Qin(k), qlatdx, Q_xs, Qinpast_xs, Qp, Qmxa)

! Muskingum-Cunge Diffusive Wave
          ELSEIF ( Chan_rtemethod == 3 ) THEN
            CALL DW_loop(Jch, Nxsj, Alpha, M, M_1, M_1abs, M_inv, B1, &
                         a_lat, qlatdx, Chantype, Chan_parm1, &
                         Chan_parm2, Chan_slope, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa)
!sed                     Chan_parm2, Chan_slope, Dts, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa, Sed, Sedp, Sed_route)

! Method of Characteristics
          ELSEIF ( Chan_rtemethod == 4 ) THEN
            ain = Qin(k) * Dt_dx
            CALL MethofChar(ain, Alpha, M, M_1, M_1abs, a_lat, q_lat, &
                            Dx, Alp_mdts, Alpdts, Chantype, Mocgrids, &
                            Chan_xmoc, Chan_amoc, Qp(Nxsj), Ap(Nxsj))
            IF ( Qp(Nxsj)>Qmxa ) Qmxa = Qp(Nxsj)

! Kinematic Wave Implicit solution when M = 1
! (no explicit first solution)
          ELSEIF ( M_1abs < DNEARZERO .AND. Chan_rtemethod /= 2 ) THEN
            CALL M1_impl_loop(Nxsj, Alpha, M, Alpha_m, M_1, M_1abs, &
                              Dtdxchi_inv, qlatdx, Q, Qp, A, Ap, Qmxa)
!sed                          Dtdxchi_inv, qlatdx, Dts, Dx, Q, Qp, A, Ap, Qmxa, sed_l, Sed, Sedp)
          ENDIF

        ELSE
          DO j = 1, Nxsj
            Qp(j) = ZERO
            Ap(j) = ZERO
          ENDDO
!sed      IF ( Sed_route == 1 ) THEN
!sed        DO j = 1, Nxsj
!sed          Sedp(j) = ZERO
!sed        ENDDO
!sed      ENDIF
          Qmxa = ZERO

        ENDIF

        IF ( k < Ndels ) THEN
          DO j = 1, Nxsj
            Q(j) = Qp(j)
            A(j) = Ap(j)
          ENDDO
!sed      IF ( Sed_route == 1 ) THEN
!sed        DO j = 1, Nxsj
!sed          Sed(j) = Sedp(j)
!sed        ENDDO
!sed      ENDIF
        ENDIF

        Qout(k) = Qp(Nxsj)
!sed    IF ( Sed_route == 1 ) Sedout(k) = Sedp(Nxsj)

        IF ( Jch==Outlet_chan ) THEN
          qdt = Qp(Nxsj)*Dts
          Chanvol_nchansum = Chanvol_nchansum + qdt
          Chanvol_daysum = Chanvol_daysum + qdt
        ENDIF

!*** End of ndels (k) loop
      ENDDO

      DO j = 1, Nxsj
        Q_xs(j) = Qp(j)
        A_xs(j) = Ap(j)
      ENDDO
!sed  IF ( Sed_route == 1 ) THEN
!sed    DO j = 1, Nxsj
!sed      Sed_xs(j) = Sedp(j)
!sed    ENDDO
!sed    Sed_chan = Sedp(Nxsj)
!sed  ENDIF

      Q_chan = Qp(Nxsj)

      END SUBROUTINE rte_runloop

!***********************************************************************
! Flow Wave and Sedimet Routing Loop using
! Kinematice Explicit and Implicit Finite Difference Solutions
!***********************************************************************
      SUBROUTINE KW_loop(Nxsj, Alpha, Alpha_m, M, M_1, M_1abs, M_inv, &
                         B1, A_lat, Qlatdx, Dtdxchi_inv, Dt_dx, Q, Qp, A, Ap, Qmxa)
!sed                     B1, A_lat, Qlatdx, Sed_l, Dtdxchi_inv, Dts, &
!sed                     Dt_dx, Dx, Q, Qp, A, Ap, Qmxa, Sed, Sedp)
      USE PRMS_KROUT_CHAN, ONLY: Chan_rtemethod, Chifactor, Chan_chi, Chan_theta, ZERO
!sed  USE PRMS_KROUT_OFPL, ONLY: Sed_route
      IMPLICIT NONE
! Functions
      EXTERNAL :: ExplFd, IterateFd
!sed  EXTERNAL :: SedrouteChan
! Arguments
      INTEGER Nxsj
      DOUBLE PRECISION, INTENT(IN) :: Alpha, Alpha_m, A_lat, M, M_1, M_1abs, M_inv
      DOUBLE PRECISION, INTENT(IN) :: Dtdxchi_inv, Dt_dx, B1, Qlatdx, Q(Nxsj), A(Nxsj)
!sed  DOUBLE PRECISION, INTENT(IN) :: Sed_l, Sed(Nxsj), Dx, Dts
      DOUBLE PRECISION, INTENT(INOUT) :: Qmxa, Qp(Nxsj), Ap(Nxsj)
!sed  DOUBLE PRECISION, INTENT(INOUT) :: Sedp(Nxsj)
! Local Variables
      INTEGER :: j, jm1
      DOUBLE PRECISION :: theta
!***********************************************************************
      DO j = 2, Nxsj
        jm1 = j - 1
! Begin with explicit solution
        CALL ExplFd(Alpha, M, M_inv, Dt_dx, B1, M_1, A_lat, Qlatdx, &
                    Q(jm1), Qp(jm1), Q(j), A(jm1), Ap(jm1), A(j), Qp(j), Ap(j), theta)
!*** Implicit finite difference method***
        IF ( Chan_rtemethod == 2 .AND. Ap(j) > ZERO ) THEN
!         IF ( theta < 0.6 .OR. theta > 1.0 ) theta = Chan_theta
          theta = Chan_theta

          CALL IterateFd(Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv, &
                         Chan_chi, Chifactor, theta, Qlatdx, Q(jm1), &
                         Qp(jm1), Q(j), A(jm1), Ap(jm1), A(j), Qp(j), Ap(j), 2)
        ENDIF
        IF ( Qp(j) > Qmxa ) Qmxa = Qp(j)
!sed    IF ( Sed_route == 1 ) CALL SedrouteChan(A(j), Ap(j), Qp(jm1), Qp(j), Dts, Dx, Sed(j), Sedp(jm1), Sed_l, Sedp(j))
      ENDDO
      END SUBROUTINE KW_loop

!***********************************************************************
! Muskingum routing using fixed travel times
!***********************************************************************
      SUBROUTINE Musk_loop(Nxsj, Czero, Cone, Ctwo, Qin, Qlatdx, Qoutpast_xs, Qinpast_xs, Qp, Qmxa)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Nxsj
      DOUBLE PRECISION, INTENT(IN) :: Czero, Cone, Ctwo, Qin, Qlatdx, Qoutpast_xs(Nxsj)
      DOUBLE PRECISION, INTENT(INOUT) :: Qmxa, Qinpast_xs(Nxsj)
      DOUBLE PRECISION, INTENT(OUT) :: Qp(Nxsj)
! Local Variables
      INTEGER j
      DOUBLE PRECISION :: qin_current, q
!***********************************************************************
      qin_current = Qin + Qlatdx
      DO j = 1, Nxsj
        q = (Czero * qin_current) + (Cone * Qinpast_xs(j)) + (Ctwo * Qoutpast_xs(j))
        IF ( q > Qmxa ) Qmxa = q
        Qinpast_xs(j) = qin_current
        qin_current = q
        Qp(j) = q
      ENDDO
      END SUBROUTINE Musk_loop

!***********************************************************************
! Flow Wave and Sedimet Routing Loop using Kinematic Wave explicit
! and then Muskingum-Cunge Diffusion Wave solutions
!***********************************************************************
      SUBROUTINE DW_loop(Jch, Nxsj, Alpha, M, M_1, M_1abs, M_inv, B1, &
                         A_lat, Qlatdx, Chantype, Chan_parm1, &
                         Chan_parm2, Chan_slope, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa)
!sed                     A_lat, Qlatdx, Sed_l, Chantype, Chan_parm1, &
!sed                     Chan_parm2, Chan_slope, Dts, Dt_dx, Dx, Q, Qp, &
!sed                     A, Ap, Qmxa, Sed, Sedp, Sed_route)
      IMPLICIT NONE
! Functions
      EXTERNAL :: ExplFd, MuskDifWave
!sed  EXTERNAL :: SedrouteChan
! Arguments
      INTEGER, INTENT(IN) :: Jch, Nxsj, Chantype
!sed  INTEGER, INTENT(IN) :: Sed_route
      DOUBLE PRECISION, INTENT(IN) :: Alpha, A_lat, B1, M, M_1, M_1abs, M_inv
      DOUBLE PRECISION, INTENT(IN) :: Chan_parm1, Chan_parm2, Chan_slope, Dt_dx, Dx, Qlatdx
      DOUBLE PRECISION, INTENT(IN) :: Q(Nxsj), A(Nxsj)
!sed  DOUBLE PRECISION, INTENT(IN) :: Sed_l, Sed(Nxsj), Dts
      DOUBLE PRECISION, INTENT(INOUT) :: Qmxa, Qp(Nxsj), Ap(Nxsj)
!sed  DOUBLE PRECISION, INTENT(INOUT) :: Sedp(Nxsj)
! Local Variables
      INTEGER :: j, jm1
      DOUBLE PRECISION :: theta
!***********************************************************************
      DO j = 2, Nxsj
        jm1 = j - 1
! Begin with explicit solution
        CALL ExplFd(Alpha, M, M_inv, Dt_dx, B1, M_1, A_lat, Qlatdx, &
                    Q(jm1), Qp(jm1), Q(j), A(jm1), Ap(jm1), A(j), Qp(j), Ap(j), theta)
!*** Muskingum-Cunge Diffusion Wave Method
        CALL MuskDifWave(Jch, j, Chantype, Chan_parm1, Chan_parm2, &
                         Alpha, M, M_inv, M_1, M_1abs, Dx, Dt_dx, &
                         Chan_slope, Qlatdx, Q(jm1), Qp(jm1), Q(j), &
                         A(jm1), Ap(jm1), A(j), Qp(j), Ap(j))
        IF ( Qp(j)>Qmxa ) Qmxa = Qp(j)
!sed    IF ( Sed_route==1 ) CALL SedrouteChan(A(j), Ap(j), Qp(jm1), &
!sed         Qp(j), Dts, Dx, Sed(j), Sedp(jm1), Sed_l, Sedp(j))
      ENDDO
      END SUBROUTINE DW_loop

!***********************************************************************
! only called when M = 1.0 and Chan_rtemethod=2 (Implicit)
!***********************************************************************
      SUBROUTINE M1_impl_loop(Nxsj, Alpha, M, Alpha_m, M_1, M_1abs, &
                              Dtdxchi_inv, Qlatdx, Q, Qp, A, Ap, Qmxa)
!sed                          Dtdxchi_inv, Qlatdx, Dts, Dx, Q, Qp, A, &
!sed                          Ap, Qmxa, Sed_l, Sed, Sedp)
      USE PRMS_KROUT_CHAN, ONLY: Chifactor, Chan_chi, Chan_theta
!sed  USE PRMS_KROUT_OFPL, ONLY: Sed_route
      IMPLICIT NONE
! Functions
      EXTERNAL :: IterateFd
!sed  EXTERNAL :: SedrouteChan
! Arguments
      INTEGER, INTENT(IN) :: Nxsj
      DOUBLE PRECISION, INTENT(IN) :: Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv, Qlatdx
      DOUBLE PRECISION, INTENT(IN) :: Q(Nxsj), A(Nxsj)
!sed  DOUBLE PRECISION, INTENT(IN) :: Sed_l, Sed(Nxsj), Dts, Dx
      DOUBLE PRECISION, INTENT(INOUT) :: Qmxa, Qp(Nxsj), Ap(Nxsj)
!sed  DOUBLE PRECISION, INTENT(INOUT) :: Sedp(Nxsj)
! Local Variables
      INTEGER :: j, jm1
!***********************************************************************
      DO j = 2, Nxsj
        jm1 = j - 1
        CALL IterateFd(Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv, &
                       Chan_chi, Chifactor, Chan_theta, Qlatdx, Q(jm1), &
                       Qp(jm1), Q(j), A(jm1), Ap(jm1), A(j), Qp(j), Ap(j), 3)
        IF ( Qp(j) > Qmxa ) Qmxa = Qp(j)
!sed    IF ( Sed_route == 1 ) CALL SedrouteChan(A(j), Ap(j), Qp(jm1), &
!sed         Qp(j), Dts, Dx, Sed(j), Sedp(jm1), Sed_l, Sedp(j))
      ENDDO
      END SUBROUTINE M1_impl_loop

!***********************************************************************
! Compute flow left in channel segments at the end of a timestep
!***********************************************************************
      SUBROUTINE rte_flowleft(Chanvol_sum)
      USE PRMS_KROUT_CHAN, ONLY: Nxs, Q_xs, ZERO
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_SET_TIME, ONLY: Timestep_seconds
      IMPLICIT NONE
! Arguments
      DOUBLE PRECISION, INTENT(OUT) :: Chanvol_sum
! Local Variables
      INTEGER :: i, j
!***********************************************************************
      Chanvol_sum = ZERO
      DO i = 1, Nsegment
        DO j = 1, Nxs(i)
          Chanvol_sum = Chanvol_sum + Q_xs(j, i)
        ENDDO
      ENDDO
      Chanvol_sum = Chanvol_sum * Timestep_seconds
      END SUBROUTINE rte_flowleft

!***********************************************************************
!     Route flow through reservoirs
!***********************************************************************
      SUBROUTINE reservoir(Ires, Ndels, Chan_type, Rtm, Qout, Wv5, C5, &
                           S5, Wv15, C15, S15, Nsos, Lake_coef, Up1, Up2, Up3, Lake_sto, Din1)
      USE PRMS_CONSTANTS, ONLY: DNEARZERO
      USE PRMS_KROUT_CHAN, ONLY: Lake_outq, Mxnsos, Qin, ZERO
      IMPLICIT NONE
      INTRINSIC :: EXP, ABS
! Arguments
      INTEGER, INTENT(IN) :: Ires, Ndels, Chan_type, Nsos, Up1, Up2, Up3
      DOUBLE PRECISION, INTENT(IN) :: Rtm, Lake_coef
      DOUBLE PRECISION, INTENT(IN) :: Wv5(Mxnsos), C5(Mxnsos), S5(Mxnsos)
      DOUBLE PRECISION, INTENT(IN) :: Wv15(Mxnsos), C15(Mxnsos), S15(Mxnsos)
      DOUBLE PRECISION, INTENT(INOUT) :: Lake_sto, Din1
      DOUBLE PRECISION, INTENT(OUT) :: Qout(Ndels)
! Local Variables
      INTEGER :: jj, j, n
      DOUBLE PRECISION :: in1, in2, total, avin, s2o2, qr
      DOUBLE PRECISION :: dtr, dtr1, q2, xkt, dtr12, c2, dtnd1
!***********************************************************************
      dtr = 1440.0D0 / Rtm
      dtr1 = 1.0D0 / dtr
      dtnd1 = 1.0D0 / Ndels
      total = ZERO

!rsr, need???
!   to add code to get surface runoff, interflow and gwflow to reservoir

      !rsr, added qr, not sure if it is correct
      qr = ZERO
      IF ( Up1 > 0 ) qr = Lake_outq(Up1)
      IF ( Up2 > 0 ) qr = qr + Lake_outq(Up2)
      IF ( Up3 > 0 ) qr = qr + Lake_outq(Up3)

      in2 = Din1 + qr

      IF ( Chan_type == 8 ) THEN

! Compute outflow using Puls routing method
        DO j = 1, Ndels
          s2o2 = Lake_sto * dtr - Lake_outq(Ires) * 0.5D0
          IF ( s2o2 < ZERO ) s2o2 = ZERO
          in1 = in2
          in2 = Qin(j)
          total = total + in2
          avin = (in1+in2) * 0.5D0
          s2o2 = s2o2 + avin

          n = Nsos
          IF ( ABS(Rtm-5.0D0)<DNEARZERO ) THEN
            DO jj = 2, Nsos
              IF ( s2o2 < Wv5(jj) ) THEN
                n = jj
                EXIT
              ENDIF
            ENDDO
            q2 = S5(n) * s2o2 + C5(n)
          ELSE
            DO jj = 2, Nsos
              IF ( s2o2 < Wv15(jj) ) THEN
                n = jj
                EXIT
              ENDIF
            ENDDO
            q2 = S15(n) * s2o2 + C15(n)
          ENDIF

          IF ( q2 < ZERO ) q2 = ZERO
          s2o2 = s2o2 - q2        !???rsr, not in strmflow module
          IF ( s2o2 < 0.0 ) THEN
            q2 = s2o2 + q2 * 0.5D0
            s2o2 = ZERO
          ENDIF
          Lake_sto = (s2o2-q2*0.5) * dtr1
          IF ( Lake_sto < ZERO ) THEN
            q2 = s2o2 + Lake_outq(Ires) * 0.5D0
            Lake_sto = ZERO
          ENDIF
          Qout(j) = q2
        ENDDO
        Lake_outq(Ires) = q2

! Compute outflow using linear routing method
      ELSE
        xkt = Lake_coef*dtr1
        dtr12 = dtr1*.5
        DO j = 1, Ndels
          in1 = in2
          in2 = Qin(j)
          total = total + in2
          avin = (in1+in2)*dtr12
          c2 = 1. - EXP(-xkt)
          q2 = (avin*(1.-(c2/xkt))) + Lake_sto*c2
          IF ( q2 < ZERO ) q2 = ZERO
          Lake_sto = Lake_sto + avin - q2
          Qout(j) = q2 * dtr
        ENDDO
        Lake_outq(Ires) = q2*dtr
      ENDIF

      Din1 = total*dtnd1

      END SUBROUTINE reservoir

!***********************************************************************
!     Computes the parameters alpha and rm for a given channel
!***********************************************************************
      SUBROUTINE AlphaRm(Ichtype, Rough, Slope, Parm1, Parm2, Alpha, Rm)
      USE PRMS_KROUT_CHAN, ONLY: TWOTHIRDS, FOURTHIRDS, FIVETHIRDS, ZERO
      IMPLICIT NONE
! Functions
      INTRINSIC :: SQRT
!***********************************************************************
!     + + + ARGUMENT DEFINITIONS + + +
!     ICHTYPE  - Channel Type
!      1 = rectangular
!      2 = circular pipe
!      3 = triangular
!      4 = explicit def
!      7 = junction
!      8 = reservior, Puls
!      9 = reservoir, linear
!     10 = gutter
!     11 = open-channel with arbitrary cross section based on
!          wetted perimeter as a power FUNCTION of flow area
!     ROUGH  - Channel Roughness
!     SLOPE  - Channel Slope
!     PARM1  - Parameter 1
!     PARM2  - Parameter 2
!     ALPHA  - Kinematic-wave routing parameter (L^-1/3/T)
!     RM     - Routing parameter for kinematic wave equation
!***********************************************************************
      INTEGER, INTENT(IN) :: Ichtype
      DOUBLE PRECISION, INTENT(IN) :: Rough, Slope, Parm1, Parm2
      DOUBLE PRECISION, INTENT(OUT) :: Alpha, Rm
! Local Variables
      DOUBLE PRECISION :: side, sqrtslope, turbulent_fac
!***********************************************************************
      sqrtslope = SQRT(Slope)
      turbulent_fac = (1.486*sqrtslope) / Rough

! Rectangular open channel
!    parm1=channel width
      IF ( Ichtype==1 ) THEN
!       the following holds for width approximates hydraulic radius
!       meaning depth is very small compared to width.
        Alpha = turbulent_fac / (Parm1**TWOTHIRDS)
        Rm = FIVETHIRDS

! Triangular channel,
!    parm1=width from left bank to center at 1 ft depth
!    parm2=width from right bank to center at 1 ft depth
      ELSEIF ( Ichtype==3 ) THEN
        side = SQRT(Parm1+Parm2) / (SQRT(1.0D0+Parm1**2.0D0)+SQRT(1.0D0+Parm2**2.0D0))
        Alpha = ((1.18*sqrtslope)/Rough) * side**TWOTHIRDS
        Rm = FOURTHIRDS

! User specified alpha and rm
      ELSEIF ( Ichtype==4 ) THEN
        Alpha = Parm1
        Rm = Parm2

! Circular Pipe segment, parm1 = pipe diameter
      ELSEIF ( Ichtype==2 ) THEN
        Alpha = turbulent_fac * (Parm1/4.0D0)**TWOTHIRDS
        Rm = 1.0D0


! Junction or reservoir (Ichtype, 7, 8, or 9)
      ELSEIF ( Ichtype>6 .AND. Ichtype<10 ) THEN
        Alpha = ZERO
        Rm = ZERO

! Gutter, parm1 = gutter cross slope (ft horizontal/ft vertical)
      ELSEIF ( Ichtype==10 ) THEN
        side = SQRT(Parm1) / (1.0D0+SQRT(1.0D0+Parm1**2.0D0))
        Alpha = (1.18*sqrtslope) / (Rough*side**TWOTHIRDS)
        Rm = FOURTHIRDS

! Open channel adjusted at each time step
! assumes Chan_type is 1 and Wpcoef_a and Wpcoef_b are defined
!    Parm1 = a1, Parm2 = b1
!            a1 and b1 are based on WP = a1*A**b1
      ELSEIF ( Ichtype==11 ) THEN
        Alpha = turbulent_fac/(Parm1**TWOTHIRDS)
!       Rm = (5.0D0-2.0D0*Parm2) / 3.0D0
        Rm = FIVETHIRDS - TWOTHIRDS*Parm2

!     ELSEIF ( Ichtype < 0 .OR. Ichtype > 11 ) THEN
      ELSE
        PRINT *, 'Error in AlphaRm, invalid channel type', Ichtype
        STOP
      ENDIF

      END SUBROUTINE AlphaRm

!***********************************************************************
!     Perform sediment routing
!***********************************************************************
      SUBROUTINE SedrouteChan(Aj, Apj, Qpi, Qpj, Dt, Dx, Sedj, Sedpi, Sedl, Sedpj)
      USE PRMS_KROUT_CHAN, ONLY: ZERO
      IMPLICIT NONE
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: Aj, Apj, Qpi, Qpj, Dt, Dx, Sedj, Sedpi, Sedl
      DOUBLE PRECISION, INTENT(OUT) :: Sedpj
! Local Variables
      DOUBLE PRECISION :: cadt, cqdx, adtqdx
!***********************************************************************
      IF ( Apj < ZERO ) THEN
        Sedpj = ZERO
      ELSE
        cadt = Sedj*Aj / Dt
        cqdx = Sedpi*Qpi / Dx
        adtqdx = (Apj/Dt) + (Qpj/Dx)
        Sedpj = (Sedl+cadt+cqdx) / adtqdx
      ENDIF
      END SUBROUTINE SedrouteChan

!***********************************************************************
!     Routes water between nodes in the system using fixed routing times
!
!                   created by Tom Ryan USBR (801) 524-5573
!
!                               April 24, 1992
!  REVISED: 11/25/98 by Mark Mastin to include Muskingum routing in Tom
!        Ryan's fixroute module. Changed name from fixroute to musroute.
!
!  Revised: 11/8/04 by Steve Regan for use with MMS kinematic routing
!
!  The Muskingum equation is described in 'Hydrology for Engineers',
!  3rd ed. by Linsley, R.K, Kohler, M.A., and Paulhus, J.L.H., 1982
!  p. 275 and in 'Water in Environmental Planning' by Dunne, T., and
!  Leopold, L.B. 1978 p. 357.
!
!  Note that the Muskingum equation assumes a linear relation of storage
!  to the inflow/outflow relation and therefore the relation is the same
!  throughout the range of the hydrograph.  The route_time parameter in
!  the fixroute module is replaced by two new parameters,
!  Musk_travel_time and Musk_wghtfac, which are described below:
!
!  The Muskingum method is based on the equation:
!    S = K[xIn + (1 - x)Out]
!    where S is storage, K is the storage coefficient (travel_time),
!          x is a coefficient between 0 and .5 (wghtfac),
!          In is inflow, and Out is outflow.
!
!  Solving for the Outflow at day 2 (Out2); and knowing the inflow at
!  day 1 (In1); the inflow at day 2 (In2); and the outflow at day 1
!  (Out1); the storage equation can be written as follows:
!
!   Out2 = czero*In2 + cone*In1 + ctwo*Out1
!
!   where czero = -((Kx - dthalf)    / (K - Kx + dthalf))
!         cone  =  (Kx + dthalf)     / (K - Kx + dthalf)
!         ctwo  =  (K - Kx - dthalf) / (K - Kx + dthalf)
!
!   assuming a time step of dt with dt and K in units of hours
!
!   Compute the three constants in the Muskingum routing equation based
!   on the values of Musk_travel_time, Musk_wghtfac, and the specified
!   routing period, Chan_route_time
!
!***********************************************************************
!     muskinit - Initialize Muskingum routing variables
!***********************************************************************
      SUBROUTINE musk_init()
      USE PRMS_KROUT_CHAN, ONLY: Czero, Cone, Ctwo, Musk_wghtfac, Musk_travel_time, Chan_route_time, ZERO
      USE PRMS_MODULE, ONLY: Nsegment
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
      DOUBLE PRECISION :: dt, tt, wt, wtt, dd
!***********************************************************************
! Chan_route_time in minutes, dt in hours
      DO i = 1, Nsegment
        dt = 0.5D0*Chan_route_time(i) / 60.0D0
        tt = Musk_travel_time(i)
        wt = Musk_wghtfac(i)
        wtt = wt*tt
        dd = 1.0D0 / (tt-wtt+dt)
        Czero(i) = (-wtt+dt) * dd
        Cone(i) = (wtt+dt) * dd
        Ctwo(i) = (tt-wtt-dt) * dd
!  if Ctwo is <= 0.0 then short travel time though reach (less than
!  routing time), thus outflow is mainly = inflow w/ small influence of
!  previous inflow. Therefore, keep Czero as is, and lower Cone by Ctwo,
!  set Ctwo=0
!
!  if Czero is <= 0.0 then long travel time through reach (greater than
!  routing time), thus mainly dependent on yesterdays flows.  Therefore,
!  keep Ctwo as is, reduce Cone by Czero and set Czero=0
!
! SHORT travel time
        IF ( Ctwo(i) <= ZERO ) THEN
          Cone(i) = Cone(i) + Ctwo(i)
          Ctwo(i) = ZERO
        ENDIF
! LONG travel time
        IF ( Czero(i) <= ZERO ) THEN
          Cone(i) = Cone(i) + Czero(i)
          Czero(i) = ZERO
        ENDIF
      ENDDO
      END SUBROUTINE musk_init

!***********************************************************************
!     The following four routines are required for the krout_ofpl_prms
!     module. (ExplFd, IterateFd, MuskDifWave, Tpwd)
!***********************************************************************
!     Explicit finite-difference method to solve for unknown flow Qpj.
!***********************************************************************
      SUBROUTINE ExplFd(Alpha, M, Minv, Dtdx, B1, M_1, Alat, Qldx, Qi, &
     &                  Qpi, Qj, Ai, Api, Aj, Qpj, Apj, Theta)
      USE PRMS_CONSTANTS, ONLY: DNEARZERO
      USE PRMS_KROUT_CHAN, ONLY: ZERO
      IMPLICIT NONE
! Arguments Definitions
!     A = flow areas
!     Q = discharge
!     Dtdx = dt/dx
!     B1 = Alpha*M*Dtdx
!     M_1 = M - 1.0
      DOUBLE PRECISION, INTENT(IN) :: Alpha, M, Minv, Dtdx, B1, M_1, Alat, Qldx
      DOUBLE PRECISION, INTENT(IN) :: Qi, Qpi, Qj, Ai, Api, Aj
      DOUBLE PRECISION, INTENT(OUT) :: Qpj, Apj, Theta
!***********************************************************************
      IF ( Ai > ZERO .OR. Qldx > ZERO ) THEN
! Compute theta, a computed stability parameter
! if Q = 0, theta = Alpha*M*(hi**(M-1)*dt/dx or dt/dx*M*qi/hi
        IF ( Qldx < DNEARZERO ) THEN
          Theta = B1*Ai**M_1
        ELSE
          Theta = (Alpha / Qldx) * ((Alat+Ai)**M-Ai**M)
        ENDIF

        IF ( Theta > 1.0D0 ) THEN
          Qpj = Qpi + Qldx - (Api-Ai) / Dtdx
          IF ( Qpj < DNEARZERO ) THEN
            Qpj = ZERO
            Apj = ZERO
          ELSE
            Apj = (Qpj/Alpha)**Minv
          ENDIF
          RETURN
        ENDIF
      ENDIF

      Apj = Aj + Alat + Dtdx*(Qi-Qj)
      IF ( Apj < DNEARZERO ) THEN
        Apj = ZERO
        Qpj = ZERO
      ELSE
        Qpj = Alpha * (Apj**M)
      ENDIF

      END SUBROUTINE ExplFd

!***********************************************************************
!     Solve for the unknown flow area (Apj) by an iterative non-linear
!     finite difference scheme. Newtons second order method is used
!     for the root of the non-linear equation. Taken from dr3m.
!***********************************************************************
      SUBROUTINE IterateFd(Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv, &
     &                     Chi, Chifact, Theta, Qldx, Qi, Qpi, Qj, Ai, &
     &                     Api, Aj, Qpj, Apj, Modnum)
      USE PRMS_KROUT_CHAN, ONLY: CONVRG
      USE PRMS_CONSTANTS, ONLY: DNEARZERO
      IMPLICIT NONE
      INTRINSIC :: ABS
      INTEGER, PARAMETER :: MAX_ITER = 30
! Arguments
      INTEGER, INTENT(IN) :: Modnum
      DOUBLE PRECISION, INTENT(IN) :: Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv
      DOUBLE PRECISION, INTENT(IN) :: Chi, Chifact, Theta, Qldx, Qi, Qpi, Qj, Ai, Api, Aj
      DOUBLE PRECISION, INTENT(INOUT) :: Qpj, Apj
! Local Variables
      INTEGER :: iter, cnvg
      DOUBLE PRECISION :: x2, x3, a4, a7, c2, fx, fpx, fppx, h, x, soln
!***********************************************************************
!  begin with explicit solution for flow area
      x2 = Theta * Dtdxchi_inv
      x3 = (1.0D0-Theta) * Dtdxchi_inv
      a4 = M - 2.0D0
      a7 = Alpha_m*M_1

      c2 = -Qpi + Chifact*(Qj-Qi) - x2*Aj + x3*(Api-Ai) - Qldx/Chi

      iter = 0
      IF ( M_1abs < DNEARZERO ) THEN
        soln = -c2 / (Alpha+x2)
      ELSE
        soln = Apj
        cnvg = 0
        DO WHILE ( iter<MAX_ITER .AND. cnvg==0 )
          iter = iter + 1
          fx = Alpha*soln**M + x2*soln + c2
          IF ( ABS(fx) < DNEARZERO ) THEN
            cnvg = 1
          ELSE
            fpx = Alpha_m*soln**M_1 + x2
            IF ( ABS(fpx) < DNEARZERO ) STOP 'in IterateFd'
            fppx = a7 * soln**a4
            h = -fpx/fx + 0.5D0 * fppx / fpx
            x = soln + 1.0D0 / h
            IF ( x<DNEARZERO ) THEN
              cnvg = 1
            ELSE
              IF ( ABS(x-soln)/soln < CONVRG ) cnvg = 1
              soln = x
            ENDIF
          ENDIF
        ENDDO
! solution did not converge, use last solution
        IF ( cnvg == 0 ) THEN
          PRINT *, 'iterations exceeded in IterateFd'
          PRINT *, Aj, soln, Ai, Qi, Api, Qpi, Qpj, Apj, Modnum
          STOP 'iterations exceeded in IterateFd'
        ENDIF
      ENDIF

      Apj = soln
      Qpj = Alpha * Apj**M
      IF ( iter > 15 ) PRINT *, 'iterations in IterateFd=', iter

      END SUBROUTINE IterateFd

!***********************************************************************
!     Solves for the unknown flow discharge Qpj by the variable-
!     parameter Muskingum-Cunge diffusion wave method.
!     (see ASCE J. of Hydr. Div., Vol. 104, no. HY12, December 1978.)
!***********************************************************************
      SUBROUTINE MuskDifWave(Ibrnch, Iseg, Jtype, Param1, Param2, Alpha, &
     &                       M, Minv, M_1, M_1abs, Dx, Dtdx, Slope, &
     &                       Qlatdx, Qi, Qpi, Qj, Ai, Api, Aj, Qpj, Apj)
      USE PRMS_CONSTANTS, ONLY: DNEARZERO
      USE PRMS_KROUT_CHAN, ONLY: CONVRG
      IMPLICIT NONE
      INTRINSIC ABS
! Functions
      DOUBLE PRECISION, EXTERNAL :: Tpwd
      INTEGER, PARAMETER :: MAX_ITER = 20
! Arguments Definitions
!     Ibrnch - index number of channel branch
!     Iseg   - index number of segment
!     Jtype  - type of segment
!     Param1 - first parameter for segment, value depends on JTYPE
!     Param2 - second parameter for segment, value depends on JTYPE
!     Alpha  - alpha for segment
!     M      - M for segment
!     Minv   - 1.0/M
!     M_1    - M - 1.0
!     M_1abs - abs(M_1)
!     Dx     - routing space step for segment
!     Dtdx   - DT in seconds divided by DX
!     Slope  - slope of segment
!     Qlatdx - lateral inflow for segment times segment length
!     Qi     - discharge at j-1 time at x
!     Qpi    - discharge at j-1 time at x
!     Qj     - discharge at j time at x + dx
!     Ai     - flow area at j-1 time at x
!     Api    - flow area at j-1 time at x
!     Aj     - flow area at j time at x + dx
!     Qpj    - discharge at j time at x + dx
!     Apj    - flow area at j time at x + dx
      INTEGER, INTENT(IN) :: Ibrnch, Iseg, Jtype
      DOUBLE PRECISION, INTENT(IN) :: Param1, Param2, Alpha, M, Minv, M_1, M_1abs, Dx
      DOUBLE PRECISION, INTENT(IN) :: Dtdx, Slope, Qlatdx, Qi, Qpi, Qj, Ai, Api, Aj
      DOUBLE PRECISION, INTENT(INOUT) :: Qpj, Apj
! Local Variables
      INTEGER :: iter, cnvg
      DOUBLE PRECISION :: alpm, q0, a0, celx, qx, cel, qr, c, r
      DOUBLE PRECISION :: cc, c0, c1, c2, c3, q, dtnew, slpdx
! Save Varialbes
      INTEGER, SAVE :: k1
      DATA k1/0/
!***********************************************************************
      alpm = Alpha * M
!
!     .....assign first guess at  XQ(J)  and  XA(J) from KW soln.....
      q0 = Qpj
      a0 = Apj
!
!     .....compute sum of wave celerity's and unit-width discharges
!          at the three known nodal points in the computational box.....
      cel = Alpha
      IF ( M_1abs > DNEARZERO ) THEN
        celx = alpm * (Ai**M_1+Aj**M_1+Api**M_1)
        qx = Qi / Tpwd(Jtype, Param1, Param2, Ai) + Qj/Tpwd(Jtype, Param1, Param2, Aj) &
     &       + Qpi / Tpwd(Jtype, Param1, Param2, Api)
      ELSE
!       special case of pipe segment when M=1.0
!        cel = Alpha
        qx = (Qi+Qj+Qpi) / Tpwd(Jtype, Param1, Param2, a0)
      ENDIF
!
      slpdx = Slope * Dx
!
!     .....begin iteration.....
      cnvg = 0
      iter = 0
!
      DO WHILE ( iter<MAX_ITER .AND. cnvg==0 )
        iter = iter + 1
!
!         .....compute 4-pt average wave celerity.....
        IF ( M_1abs > DNEARZERO ) cel = (celx+(alpm*(a0**M_1)))*0.25
!
!       .....compute 4-pt average unit-width discharge.....
        qr = (qx+q0/Tpwd(Jtype, Param1, Param2, a0)) * 0.25

!       .....compute  C=Courant number  and  R=cell Reynolds number.....
        c = cel * Dtdx
        r = qr / (slpdx*cel)
!
!       .....compute routing coefficients.....
        cc = 1.0D0 / (1.0D0+c+r)
        c0 = cc * (-1.0D0+c+r)
        c1 = cc * (1.0D0+c-r)
        c2 = cc * (1.0D0-c+r)
        c3 = 2.0D0 * cc * c
!
!       .....compute next estimate of Q.....
!rsr ?? qlat*dx
        q = c0*Qpi + c1*Qi + c2*Qj + c3*Qlatdx

        IF ( q<DNEARZERO ) THEN
!         converged to zero, use previous value
          q = q0
          cnvg = 1
        ELSEIF ( ABS(q-q0)/q0<CONVRG ) THEN
!         converged to a solution
          cnvg = 1
        ELSE
!         compute next estimate of Q
          q0 = q
          a0 = (q0/Alpha)**Minv
        ENDIF
!
      ENDDO

      IF ( cnvg==0 ) THEN
!       max iterations reached without converging
        PRINT 9001, Jtype, Ibrnch, Iseg
        q = q0
      ENDIF
!
!     .....final estimate of unknowns.....
      Qpj = q
      Apj = (Qpj/Alpha)**Minv
!
      IF ( c>20.0D0 .AND. Ibrnch/=k1 ) THEN
!       .....courant number exceeds 20.0.....
        k1 = Ibrnch
        dtnew = 10.0D0 * Dx / cel
        PRINT 9002, Jtype, Ibrnch, Iseg, dtnew, c
      ENDIF
!
 9001 FORMAT (//, ' WARNING: Iterations exceed limit in Muskingum-Cunge routing', /, &
     &        10X, 'for channel type:', I3, ' channel', I5, ' segment', I5, /)
 9002 FORMAT (//, ' WARNING: Courant number exceeds 20.0 for channel type:', &
     &        I3, ' channel', I5, ' segment', I5, /, 10X, &
     &        'Routing computations may be inaccurate, try DT <', F6.0, &
     &        ' seconds', /, 10X, 'Courant number =', F7.1, /)

      END SUBROUTINE MuskDifWave

!***********************************************************************
!     Returns the value of the mean top width of a segment corresponding
!     to the flow area  A  for use in the diffusion wave routing method.
!     For zero flow area, 1.E-5 is returned.
!     The values for PARAM1 and PARAM2 are dependent on JTYPE:
!***********************************************************************
      DOUBLE PRECISION FUNCTION Tpwd(Jtype, Param1, Param2, A)
      USE PRMS_CONSTANTS, ONLY: PI, PI_4, DNEARZERO, ERROR_param
      IMPLICIT NONE
! Functions
      INTRINSIC :: SQRT
      EXTERNAL :: error_stop
! Arguments Definitions
!     Jtype     segment type            PARAM1             PARAM2
!     _____  ___________________  __________________  ________________
!       1    rectangular channel    channel width         not used
!       2           pipe            pipe diameter         not used
!       3     triangular x-sect     one side slope    other side slope
!       4      user-specified XS       wpcoef_a           wpcoef_b
!       5    overland, turbulent       not used           not used
!       6     overland, laminar        not used           not used
!      10          gutter         gutter cross slope      not used
!      11      arbirtray channel       wpcoef_a           wpcoef_b
!     A      - flow area for which the average top width is to be
!              computed.  Used for gutter and triangular segment types
!              (JTYPE = 1 or 3).
      INTEGER, INTENT(IN) :: Jtype
      DOUBLE PRECISION, INTENT(IN) :: Param1, Param2, A
! Local Variables
      DOUBLE PRECISION :: depth, z, tw
!***********************************************************************
      tw = 1.0D-5
!
! rectangler cross section segment
!rsr, Param1 is set to channel width, should be hydraulic radius
!rsr, tw is not computed for each time step, need to change
      IF ( Jtype == 1 ) THEN
        tw = Param1
! circular pipe segment
!     tw = pi * diameter / 4
      ELSEIF ( Jtype == 2 ) THEN
        tw = PI_4 * Param1
! triangular segment
      ELSEIF ( Jtype == 3 ) THEN
        IF ( A > DNEARZERO ) THEN
          z = Param1 + Param2
          depth = (1.4142/SQRT(z)) * SQRT(A)
          tw = z * depth * 0.5D0
        ENDIF
! user-specified, Parm1&2 = Wpcoef_a & Wpcoef_b
      ELSEIF ( Jtype == 4 ) THEN
        IF ( A>DNEARZERO ) tw = Param1 * (A**Param2)
! overland-flow segment: turbulent(5); laminar(6)
!     set to 1.0 as segment has unit width
      ELSEIF ( Jtype==5 .OR. Jtype==6 ) THEN
        tw = 1.0D0
! gutter segment
      ELSEIF ( Jtype == 10 ) THEN
        IF ( A > DNEARZERO ) THEN
          z = Param1
          depth = (1.4142/SQRT(z)) * SQRT(A)
          tw = z * depth * 0.5
        ENDIF
! arbitrary XS, Param1&2 = Wpcoef_a & Wpcoef_b, set tw to wp
      ELSEIF ( Jtype == 11 ) THEN
        IF ( A > DNEARZERO ) tw = Param1 * (A**Param2)
      ELSE
        CALL error_stop('invalid channel type in Tpwd', ERROR_param)
      ENDIF

      Tpwd = tw
      END FUNCTION Tpwd

!***********************************************************************
!    This routine must be called once preceding a storm for each segment
!    to define initial conditions used in the method of characteristics
!    solution for flow routing.
!***********************************************************************
      SUBROUTINE MethofCharInit(Ain, Aout, Dx, Ngrids, Xmoc, Amoc)
      USE PRMS_KROUT_CHAN, ONLY: ZERO
      IMPLICIT NONE
! Argument Definitions
!     Ain    - flow area at upstream end of channel
!     Aout   - flow area at downstream end of channel
!     Dx     - Flow length
!     Ngrids - Number of segments in channel or overland flow plane
!     Xmoc   - X characteristic (feet)
!     Amoc   - Area characteristic (ft^2)
      INTEGER, INTENT(IN) :: Ngrids
      DOUBLE PRECISION, INTENT(IN) :: Ain, Aout, Dx
      DOUBLE PRECISION, INTENT(OUT) :: Xmoc(Ngrids), Amoc(Ngrids)
! Functions
      INTRINSIC :: FLOAT
! Local Variables
      INTEGER :: i, n, nr
      DOUBLE PRECISION :: dxn, dan
!***********************************************************************
!     ..... DEFINE INITIAL CONDITIONS FOR STORM .....

      Xmoc(1) = ZERO
      Xmoc(Ngrids) = Dx
      Amoc(1) = Ain
      Amoc(Ngrids) = Aout
!
      n = Ngrids - 1
      nr = FLOAT( n )
      dxn = Dx / nr
      dan = (Ain-Aout) / nr
      DO i = 2, n
        Xmoc(i) = Xmoc(i-1) + dxn
        Amoc(i) = Amoc(i-1) + dan
      ENDDO
      END SUBROUTINE MethofCharInit

!***********************************************************************
!     This routine uses the method of characteristics for kinematic wave
!     routing.  It operates on a time step basis.  It returns the values
!     of discharge (QD) and area (AD) at the downstream end of the
!     segment after a time step in seconds.
!***********************************************************************
      SUBROUTINE MethofChar(Ain, Alpha, M, M_1, M_1abs, Alat, Qlat, Dx, &
     &                      Alpmdts, Alpdts, Jtype, Ngrids, Xmoc, Amoc, Qpj, Apj)
      USE PRMS_CONSTANTS, ONLY: DNEARZERO
      USE PRMS_GRNAMPT, ONLY: Ncmoc
      IMPLICIT NONE
! Functions
      INTRINSIC :: ABS
      EXTERNAL :: mocdimen
! Argument Definitions
!     ALPHA  - alpha
!     M      - m
!     M_1    - m - 1
!     M_1abs - abs(m-1)
!     Alat   - lateral inflow * dts
!     Qlat   - lateral inflow
!     Dx     - segment flow length
!     Alpmdts - alpha*m*dts
!     Alpdts - alpha*dts
!     Jtype  - type of channel segment
!     NGRIDS -
!     X      -
!     A      -
!     QD     - discharge
!     AD     - area
      INTEGER, INTENT(IN) :: Jtype
      INTEGER, INTENT(INOUT) :: Ngrids
      DOUBLE PRECISION, INTENT(IN) :: Ain, Alpha, M, M_1, M_1abs, Alat, Qlat, Alpmdts, Alpdts, Dx
      DOUBLE PRECISION, INTENT(INOUT) :: Xmoc(*), Amoc(*)
      DOUBLE PRECISION, INTENT(OUT) :: Qpj, Apj
! Local Variables
      INTEGER :: i, k, j, n, imj, ii
!***********************************************************************
!     CHECK THAT ARRAY DIMENSIONS WILL NOT BE EXCEEDED
      IF ( Ngrids>=Ncmoc ) CALL mocdimen(Ngrids, Xmoc, Amoc)
!     IF QLAT AND Amoc(1) = 0. DON'T ADD CHARACTERISTIC
      j = 1
      IF ( Qlat < DNEARZERO .AND. ABS(Amoc(1))<DNEARZERO ) j = 0
      ii = Ngrids
      Ngrids = Ngrids - (1-j)
!
!     ..... ADVANCE CHARACTERISTICS .....
      n = Ngrids + 2
      DO k = 1, Ngrids
        i = n - k
        imj = i - j
        IF ( M_1abs < DNEARZERO ) THEN
          Xmoc(i) = Xmoc(imj) + Alpdts
          Amoc(i) = Amoc(imj) + Alat
        ELSEIF ( Alat*1000.0D0 > Amoc(imj) ) THEN
          Xmoc(i) = Xmoc(imj) + Alpha/Qlat*((Alat+Amoc(imj))**M-Amoc(imj)**M)
          Amoc(i) = Amoc(imj) + Alat
        ELSE
          Xmoc(i) = Xmoc(imj) + Alpmdts*Amoc(imj)**M_1
          Amoc(i) = Amoc(imj)
        ENDIF
!
!     ..... KEEP TRACK OF LAST CHARACTERISTIC THAT LEAVES SEGMENT
        IF ( Xmoc(i) >= Dx ) ii = i
      ENDDO
!
!     ..... ASSIGN AREA AT U/P BOUDNARY .....
      Amoc(1) = Ain

!     ..... INTERPOLATE FOR AREA AT D/S BOUNDARY AND ASSIGN NGRIDS .....
      Apj = Amoc(ii) - (Amoc(ii)-Amoc(ii-1))*((Xmoc(ii)-Dx)/(Xmoc(ii)-Xmoc(ii-1)))
      Qpj = Alpha * (Apj**M)
      Ngrids = ii
      Xmoc(Ngrids) = Dx
      Amoc(Ngrids) = Apj

      IF ( Jtype/=5 .AND. Jtype/=6 .AND. Ngrids>2 ) THEN
        IF ( M_1abs>DNEARZERO ) THEN
!     ..... TAKE CARE OF SHOCKS .....
          i = Ngrids - 1
          DO WHILE ( i > 2 )
            IF ( Xmoc(i) <= Xmoc(i-1) ) THEN
              k = i - 1
              IF ( ABS(Amoc(k)) > DNEARZERO ) THEN
                Xmoc(k) = (Xmoc(i)+Xmoc(k)) * 0.5D0
                Amoc(k) = (Amoc(i)+Amoc(k)) * 0.5D0
              ENDIF
              Ngrids = Ngrids - 1
              DO k = i, Ngrids
                Xmoc(k) = Xmoc(k+1)
                Amoc(k) = Amoc(k+1)
              ENDDO
            ENDIF
            i = i - 1
          ENDDO
        ENDIF
      ENDIF
      END SUBROUTINE MethofChar

!***********************************************************************
!     This routine drops out characteristics for a segment if the
!     dimensions of the X and A arrays are going to be exceeded.
!***********************************************************************
      SUBROUTINE mocdimen(Ngrids, Xmoc, Amoc)
      IMPLICIT NONE
! Arguments definitions
!     Ngrids - Number of characteristics for segment
!     Xmoc   - X characteristic (feet)
!     Amoc   - Area characteristic (ft^2)
      INTEGER, INTENT(INOUT) :: Ngrids
      DOUBLE PRECISION, INTENT(INOUT) :: Xmoc(Ngrids), Amoc(Ngrids)
! Local Variables
      INTEGER :: i, j
!***********************************************************************
      PRINT *, 'MAXMOC exceeded, reduced in mocdimen', Ngrids
      j = 2
      DO i = 4, Ngrids, 2
        j = j + 1
        Xmoc(j) = Xmoc(i)
        Amoc(j) = Amoc(i)
      ENDDO
      Ngrids = Ngrids / 2 + 1
      END SUBROUTINE mocdimen

!***********************************************************************
!***********************************************************************
      SUBROUTINE Dimchk(Dimname, Maxdim, Dimen)
      USE PRMS_CONSTANTS, ONLY: ERROR_dim
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Dimname
      INTEGER, INTENT(IN) :: Maxdim, Dimen
!***********************************************************************
      IF ( Dimen > Maxdim .OR. Dimen < 1 ) THEN
        PRINT *, ' ***Dimension error: ', Dimname, ' valid range is 1 to', Maxdim, ' specified:', Dimen
        STOP ERROR_dim
      ENDIF
      END SUBROUTINE Dimchk
