!***********************************************************************
! Module Kinroute_chan - Kinematic routing from PRMS, modified for MMS
      !rsr??? caution, need to check for storm on first timestep,
      ! probably does not work
      !rsr??? need to check units of sediment variables and verify code
      !???rsr, how does interflow and gwflow get added to each reservoir
      !rsr, added hard-coded 'hybrid' DRBC specific code 11/29/07
      !     which sets inflow to channels below reservoirs to gage flow
!***********************************************************************
      !rsr, danger need to add ngain
      MODULE PRMS_KROUT_CHAN
      IMPLICIT NONE
      character(len=*), parameter :: MODDESC = 'Streamflow Routing'
      character(len=14), parameter :: MODNAME = 'krout_chan'
      character(len=*), parameter :: Version_krout_chan = '2022-04-01'
!   Local Variables
!    Qsrolat  - Lateral surface runoff in ft^2/s
      INTEGER, PARAMETER :: MAX_ITER = 20
      REAL, PARAMETER :: CONVRG = 0.0001, TWOTHIRDS = 2.0 / 3.0
      REAL, PARAMETER :: FOURTHIRDS = 4.0 / 3.0, FIVETHIRDS = 5.0 / 3.0
      REAL, PARAMETER :: FTSQ2ACRE_CONV = 43560.0, LENGTH_CONV = 12.0
      REAL, PARAMETER :: AREACONV = 1.0 / FTSQ2ACRE_CONV
      REAL, PARAMETER :: FT3TOINCHES = LENGTH_CONV * AREACONV
      INTEGER, SAVE :: Mxnsos, NlakeP1
!      INTEGER, SAVE :: Nstrahler
      INTEGER, SAVE :: Nchan, Ncdels, Ncmoc, Nchxs, Ndivert, Ngain
      INTEGER, SAVE :: File_unit
      REAL, SAVE :: Chifactor, Ofarea_total, Chan_sum_init, Dtsec
      INTEGER, SAVE, ALLOCATABLE :: Latsw(:), Upsw(:), Nxs(:)
      INTEGER, SAVE, ALLOCATABLE :: Mocgrids(:)
      INTEGER, SAVE, ALLOCATABLE :: Rb_hru(:), Lb_hru(:)
!      INTEGER, SAVE, ALLOCATABLE :: Strahler_chan(:)
      INTEGER, SAVE, ALLOCATABLE :: Rb_hru_area(:), Lb_hru_area(:)
      INTEGER, SAVE, ALLOCATABLE :: Upchan(:, :)
      REAL, SAVE, ALLOCATABLE :: Qsrolat(:), Ofar(:)
      REAL, SAVE, ALLOCATABLE :: Czero(:), Cone(:), Ctwo(:), Qmxa(:)
      REAL, SAVE, ALLOCATABLE :: Chan_loss_ftsec(:), Uprch_area(:)
      REAL, SAVE, ALLOCATABLE :: Tc_topw(:), Dtdx(:)
      REAL, SAVE, ALLOCATABLE :: Alpr1(:), Cmp1(:), Dx(:), Dts(:)
      REAL, SAVE, ALLOCATABLE :: Chan_parm2(:), Qp(:), Ap(:), Q(:), A(:)
      REAL, SAVE, ALLOCATABLE :: Chan_xmoc(:, :), Chan_amoc(:, :)
      REAL, SAVE, ALLOCATABLE :: A_xs(:, :), Qinpast_xs(:, :), Wv15(:,:)
      REAL, SAVE, ALLOCATABLE :: Wv5(:, :), S5(:, :), C5(:, :)
      REAL, SAVE, ALLOCATABLE :: S15(:, :), C15(:, :), Qout(:, :)
      REAL, SAVE, ALLOCATABLE :: Tc_half_topw(:)
!sed  REAL, SAVE, ALLOCATABLE :: Sedout(:, :), Sed_xs(:, :)
!sed  REAL, SAVE, ALLOCATABLE :: Sed(:), Sedinlat(:), Sedin(:), Sedp(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Chanvol_daysum, Chanvol_nchansum, Chanvol_sum, Qday_sum
      REAL, SAVE :: St_sroff, Storm_routvol, Dt_sroff
      REAL, SAVE :: Storm_pk_obs, Storm_pk_sim
!sed  REAL, SAVE :: Sed_tot
!sed  REAL, SAVE, ALLOCATABLE :: Sed_chan(:)
      REAL, SAVE, ALLOCATABLE :: Qin_chan(:), Qin_totchan(:), Qinlat(:)
      REAL, SAVE, ALLOCATABLE :: Qinlat_chan(:), Contrib_area_chan(:)
      REAL, SAVE, ALLOCATABLE :: Q_chan(:), Qin(:), Q_divert(:)
      REAL, SAVE, ALLOCATABLE :: Q_gain(:), Qin_instant(:)
      REAL, SAVE, ALLOCATABLE :: Q_xs(:, :), Q_chan_timestep(:)
      REAL, SAVE, ALLOCATABLE :: Qinlat_chan_ts(:)
!   Declared Variables from other modules - ofpl
      REAL, ALLOCATABLE :: Q_ndels(:, :)
!sed  REAL, ALLOCATABLE :: Sed_ndels(:, :)
!   Declared Variables from other modules - mga
      REAL :: Storm_pptexc
      REAL, ALLOCATABLE :: Storm_obsvol(:), Storm_obspk(:)
!****
!CAUTION: puts Din1, Lake_sto, Lake_outq
!****
      REAL, ALLOCATABLE :: Lake_sto(:), Lake_outq(:), Din1(:)
!   Declared Parameters
!sed  INTEGER, SAVE :: Sed_route
      INTEGER, SAVE :: Chan_rtemethod, Outlet_sta, Outlet_chan
      REAL, SAVE :: Chan_theta, Chan_chi
      INTEGER, SAVE, ALLOCATABLE :: Lat_inflowr(:), Lat_inflowl(:)
      INTEGER, SAVE, ALLOCATABLE :: Upst_inflow2(:), Upst_inflow3(:)
      INTEGER, SAVE, ALLOCATABLE :: Upst_inflow1(:), Lake_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Q_gain_id(:), Q_divert_id(:)
      INTEGER, SAVE, ALLOCATABLE :: Gain_type(:), Gain_flowid(:)
      INTEGER, SAVE, ALLOCATABLE :: Chan_type(:), Chan_ndx(:), Nsos(:)
      INTEGER, SAVE, ALLOCATABLE :: Upst_res1(:), Upst_res2(:), Upst_res3(:)
!      INTEGER, SAVE, ALLOCATABLE :: Strahler_num(:), Hru_strahler(:)
      REAL, SAVE, ALLOCATABLE :: Chan_thresh(:), Chan_length(:)
      REAL, SAVE, ALLOCATABLE :: Chan_rough(:), Chan_width(:)
      REAL, SAVE, ALLOCATABLE :: Chan_alpha(:), Chan_cmp(:)
      REAL, SAVE, ALLOCATABLE :: Chan_slope(:), Chan_parm1(:)
      REAL, SAVE, ALLOCATABLE :: Lake_coef(:)
      REAL, SAVE, ALLOCATABLE :: Chan_t3_lbratio(:), Chan_t3_rbratio(:)
      REAL, SAVE, ALLOCATABLE :: Chan_route_time(:), Chan_loss_rate(:)
      REAL, SAVE, ALLOCATABLE :: Wpcoef_a(:), Wpcoef_b(:), Lake_din1(:)
      REAL, SAVE, ALLOCATABLE :: Musk_wghtfac(:), Musk_travel_time(:)
      REAL, SAVE, ALLOCATABLE :: Ofp_route_time(:), Ofp_length(:)
      REAL, SAVE, ALLOCATABLE :: O2(:, :), S2(:, :)
      END MODULE PRMS_KROUT_CHAN

!***********************************************************************
!     Main krout_chan routine
!***********************************************************************
      INTEGER FUNCTION krout_chan()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, OFF, ACTIVE, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Init_vars_from_file, Save_vars_to_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: kchdecl, kchinit, kchrun, kchclean
      EXTERNAL :: krout_restart
!***********************************************************************
      krout_chan = 0

      IF ( Process_flag==RUN ) THEN
        krout_chan = kchrun()
      ELSEIF ( Process_flag==DECL ) THEN
        krout_chan = kchdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL krout_restart(READ_INIT)
        krout_chan = kchinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL krout_restart(SAVE_INIT)
      ENDIF

      END FUNCTION krout_chan

!***********************************************************************
!     kchdecl - set up parameters for  channel routing computations
! Parameters
!     chan_type, chan_ndx, chan_thresh, chan_length, chan_slope
!     chan_rough, chan_width, chan_parm1, chan_alpha, chan_cmp
!     chan_t3_lbratio, chan_t3_rbratio, chan_route_time
!     chan_loss_rate, chan_theta, chan_chi, chan_rtemethod
!     upst_inflow1, upst_inflow2, upst_inflow3, wpcoef_a, wpcoef_b
!     musk_wghtfac, musk_travel_time, q_gain_id, q_divert_id
!     lat_inflowr, lat_inflowl, upst_res1, upst_res2, upst_res3
!     ofp_length, sed_route, ofp_route_time
!     lake_coef, lake_type, o2, s2, nsos, outlet_chan
!***********************************************************************
      INTEGER FUNCTION kchdecl()
      USE PRMS_CONSTANTS, ONLY: ERROR_DIM, DOCUMENTATION
      USE PRMS_KROUT_CHAN
      USE PRMS_MODULE, ONLY: Model, Nhru, Nobs, Nsegment, Nlake
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: MOD, MAX
      INTEGER, EXTERNAL :: declparam, declvar, getdim
      EXTERNAL :: read_error, print_module, PRMS_open_module_file, error_stop
!***********************************************************************
      kchdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_krout_chan)

      NlakeP1 = Nlake + 1

      Mxnsos = getdim('mxnsos')
      IF ( Mxnsos == -1 ) CALL read_error(7, 'mxnsos')

      Nchan = getdim('nchan')
      IF ( Nchan == -1 ) CALL read_error(7, 'nchan')

      Ncdels = getdim('ncdels')
      IF ( Ncdels == -1 ) CALL read_error(7, 'ncdels')

      Ncmoc = getdim('ncmoc')
      IF ( Ncmoc == -1 ) CALL read_error(7, 'ncmoc')
      IF ( Ncmoc == 0 ) Ncmoc = 2
      IF ( MOD(Ncmoc, 2) /= 0 .AND. Model/=99 ) CALL error_stop('ncmoc must be an even number', ERROR_dim)

      Nchxs = getdim('nchxs')
      IF ( Nchxs == -1 ) CALL read_error(7, 'nchxs')

      Ngain = getdim('ngain')
      IF ( Ngain == -1 ) CALL read_error(7, 'ngain')

      Ndivert = getdim('ndivert')
      IF ( Ndivert == -1 ) CALL read_error(7, 'ndivert')

!      Nstrahler = get dim('nstrahler')
!      IF ( Nstrahler==-1 ) CALL read_error(7, 'nstrahler')

! Declare Variables
      ALLOCATE ( Contrib_area_chan(Nsegment) )
      IF ( declvar(MODNAME, 'contrib_area_chan', 'nsegment', Nsegment, 'real', &
           'Contributing area for each channel', &
           'acres', Contrib_area_chan)/=0 ) CALL read_error(3, 'contrib_area_chan')

      IF ( declvar(MODNAME, 'storm_routvol', 'one', 1, 'real', &
           'Routed flow volume at last channel reach for storm, cumulative total', &
           'ft3', Storm_routvol)/=0 ) CALL read_error(3, 'storm_routvol')

      IF ( declvar(MODNAME, 'storm_pk_obs', 'one', 1, 'real', &
           'Measured storm peak used in objective function', &
           'cfs', Storm_pk_obs)/=0 ) CALL read_error(3, 'storm_pk_obs')

      IF ( declvar(MODNAME, 'storm_pk_sim', 'one', 1, 'real', &
           'Simulated storm peak used in objective function', &
           'cfs', Storm_pk_sim)/=0 ) CALL read_error(3, 'storm_pk_sim')

      ALLOCATE (Q_chan(Nchan))
      IF ( declvar('MODNAME', 'q_chan', 'nchan', Nchan, 'real', &
           'Flow from channel segment', 'cfs', &
           Q_chan)/=0 ) CALL read_error(3, 'storm_pk_sim')

      ALLOCATE ( Q_chan_timestep(Nsegment) )
      IF ( declvar(MODNAME, 'q_chan_timestep', 'nsegment', Nsegment, 'real', &
           'Average flow from channel segment for time step', &
           'cfs', Q_chan_timestep)/=0 ) CALL read_error(3, 'q_chan_timestep')

      ALLOCATE ( Qinlat_chan_ts(Nsegment) )
      IF ( declvar(MODNAME, 'qinlat_chan_ts', 'nsegment', Nsegment, 'real', &
           'Average lateral inflow to a channel segment for time step', &
           'cfs', Qinlat_chan_ts)/=0 ) CALL read_error(3, 'qinlat_chan_ts')

      ALLOCATE ( Qin_chan(Nsegment) )
      IF ( declvar(MODNAME, 'qin_chan', 'nsegment', Nsegment, 'real', &
           'Channel inflow to channel segment (including diversions)', &
           'cfs', Qin_chan)/=0 ) CALL read_error(3, 'qin_chan')

      ALLOCATE ( Qin_instant(Nsegment) )
      IF ( declvar(MODNAME, 'qin_instant', 'nsegment', Nsegment, 'real', &
     &     'Channel inflow to channel segment at end of timestep', &
     &     'cfs', Qin_instant)/=0 ) CALL read_error(3, 'qin_instant')

      ALLOCATE ( Qin_totchan(Nsegment) )
      IF ( declvar(MODNAME, 'qin_totchan', 'nsegment', Nsegment, 'real', &
     &     'Total inflow to channel segment (includes lateral flow)', &
     &     'cfs', Qin_totchan)/=0 ) CALL read_error(3, 'qin_totchan')

      ALLOCATE ( Qinlat(Ncdels) )
      IF ( declvar(MODNAME, 'qinlat', 'ncdels', Ncdels, 'real', &
     &     'Lateral inflow for each cdel', &
     &     'cfs per foot', Qinlat)/=0 ) CALL read_error(3, 'qinlat')

      ALLOCATE ( Qinlat_chan(Nsegment) )
      IF ( declvar(MODNAME, 'qinlat_chan', 'nsegment', Nsegment, 'real', &
     &     'Lateral inflow for each channel', &
     &     'cfs', Qinlat_chan)/=0 ) CALL read_error(3, 'qinlat_chan')
     
      ALLOCATE ( Qin(Ncdels) )
      IF ( declvar(MODNAME, 'qin', 'ncdels', Ncdels, 'real', &
     &     'upstream inflow for each cdel', &
     &     'cfs', Qin)/=0 ) CALL read_error(3, 'qin')

      ALLOCATE ( Q_xs(Nchxs, Nsegment) )
      IF ( declvar(MODNAME, 'q_xs', 'nchxs,nsegment', Nchxs*Nsegment, 'real', &
     &     'Flow in each channel cross section at the end of timestep', &
     &     'cfs', Q_xs)/=0 ) CALL read_error(3, 'q_xs')

!sed  ALLOCATE ( Sed_chan(Nchan) )
!sed  IF ( declvar('MODNAME', 'sed_chan', 'nchan', Nchan, 'real', &
!sed       'Sediment transport from channel segment', &
!sed       'tons', Sed_chan)/=0 ) CALL read_error(3, 'sed_chan')

!sed  IF ( declvar('MODNAME', 'sed_tot', 'one', 1, 'real',
!sed       'Accumulated sediment transport from last channel segment',
!sed       'tons', Sed_tot)/=0 ) CALL read_error(3, 'sed_tot')

      IF ( declvar('MODNAME', 'chanvol_daysum', 'one', 1, 'double', &
           'Accumulated flow in the last channel segment for a day', &
           'cubic feet', Chanvol_daysum)/=0 ) CALL read_error(3, 'chanvol_daysum')

      IF ( declvar('MODNAME', 'chanvol_nchansum', 'one', 1, 'double', &
           'Accumulated flow in the last channel segment', &
           'cubic feet', Chanvol_nchansum)/=0 ) CALL read_error(3, 'chanvol_nchansum')

      IF ( declvar('MODNAME', 'chanvol_sum', 'one', 1, 'double', &
           'Accumulated flow volume in all channel segments', &
           'cubic feet', Chanvol_sum)/=0 ) CALL read_error(3, 'chanvol_sum')

      IF ( declvar('MODNAME', 'qday_sum', 'one', 1, 'double', &
           'Accumulated flow in the last channel segment', &
           'cubic feet', Qday_sum)/=0 ) CALL read_error(3, 'qday_sum')

      IF ( declvar('MODNAME', 'dt_sroff', 'one', 1, 'real', &
           'Total basin surface runoff for a timestep', &
           'inches', Dt_sroff)/=0 ) CALL read_error(3, 'qday_sum')

      IF ( declvar('MODNAME', 'st_sroff', 'one', 1, 'real', &
           'Accumulated basin surface runoff during a storm', &
           'inches', St_sroff)/=0 ) CALL read_error(3, 'st_sroff')

! Declare Parameters
!      ALLOCATE ( Hru_strahler(nstrahler) )
!      IF ( decl param('MODNAME', 'hru_strahler', 'nstrahler', 'integer', &
!          '0', 'bounded', 'nchan', &
!          'Hru associated with each channel segment Strahler number', &
!          'Hru associated with each channel segment Strahler number', &
!          'none')/=0 ) CALL read_error(1, 'hru_strahler')
!
!      ALLOCATE ( Strahler_num(Nstrahler) )
!      IF ( decl param('MODNAME', 'strahler_num', 'nstrahler', 'integer', &
!          '1', '0', '10', &
!          'Strahler of channel segment', &
!          'Strahler of channel segments, could be more than segments', &
!          'none')/=0 ) CALL read_error(1, 'strahler_num')

      ALLOCATE ( Chan_type(Nchan) )
      IF ( declparam('MODNAME', 'chan_type', 'nchan', 'integer', &
           '4', '1', '12', &
           'Type of channel segment', &
           'Channel type (1=rectangular open channel; 2=circular pipe segment; 3=triangular open channel;'// &
           ' 4=explicit specification of the kinematic parameters alpha (chan_alpha) and m (chan_cmp); 7=junction;'// &
           ' 8=reservoir, Puls routing; 9=reservoir, linear routng; 10=gutter; 11=arbitrary cross section; 12=muskingum)', &
           'none')/=0 ) CALL read_error(1, 'chan_type')

      ALLOCATE ( Chan_ndx(Nchan) )
      IF ( declparam('MODNAME', 'chan_ndx', 'nchan', 'integer', &
           '1', '0', '10', &
           'Number of intervals for routing or reservoir seq. number', &
           'Number of intervals into which the length of the channel'// &
           ' is subdivided for finite-difference computations. If'// &
           ' this is a reservoir, then it is the reservoir sequence number', &
           'none')/=0 ) CALL read_error(1, 'chan_ndx')

      ALLOCATE ( Chan_thresh(Nchan) )
      IF ( declparam('MODNAME', 'chan_thresh', 'nchan', 'real', &
           '0.0', '0.0', '6.0', &
           'Minimum depth of flow to continue routing', &
           'Minimum depth of flow to continue channel flow routing', &
           'inches')/=0 ) CALL read_error(1, 'chan_thresh')

      ALLOCATE ( Chan_length(Nchan) )
      IF ( declparam('MODNAME', 'chan_length', 'nchan', 'real', &
           '10.0', '1.0', '100000.0', &
           'Length of channel segment', 'Length of channel segment', &
           'feet')/=0 ) CALL read_error(1, 'chan_length')

      ALLOCATE ( Chan_slope(Nchan) )
      IF ( declparam('MODNAME', 'chan_slope', 'nchan', 'real', &
           '0.1', '0.0001', '3.0', &
           'Slope of channel segment', &
           'Slope of channel segment, chan_type = 1, 2, 3, 10, or 11', &
           'decimal percent')/=0 ) CALL read_error(1, 'chan_slope')

      ALLOCATE ( Chan_rough(Nchan) )
      IF ( declparam('MODNAME', 'chan_rough', 'nchan', 'real', &
           '0.005', '0.001', '1.0', &
           'Roughness parameter', &
           'Roughness parameter, chan_type = 1, 2, 3, 10, or 11', &
           'none')/=0 ) CALL read_error(1, 'chan_rough')

      ALLOCATE ( Chan_width(Nchan) )
      IF ( declparam('MODNAME', 'chan_width', 'nchan', 'real', &
           '1.0', '0.0', '1000.0', &
           'Channel width', 'Channel width', &
           'feet')/=0 ) CALL read_error(1, 'chan_width')

      ALLOCATE ( Chan_parm1(Nchan) )
      IF ( declparam('MODNAME', 'chan_parm1', 'nchan', 'real', &
           '0.0', '0.0', '1000.0', &
           'Parameter 1 for computation of alpha and m for chan_type='// &
           ' 2 & 10', 'Parameter 1 for chan_type= 2: Pipe diameter;'// &
           ' 10: Ratio of horizontal to vertical change in gutter side slope', &
           'none')/=0 ) CALL read_error(1, 'chan_parm1')

      ALLOCATE ( Chan_t3_lbratio(Nchan) )
      IF ( declparam('MODNAME', 'chan_t3_lbratio', 'nchan', 'real', &
           '0.0', '0.0', '500.0', &
           'Side-slope ratio, left bank', &
           'Ratio of horizontal to vertical change in side slope for triangular channel left bank, chan_type 3', &
           'none')/=0 ) CALL read_error(1, 'chan_t3_lbratio')

      ALLOCATE ( Chan_t3_rbratio(Nchan) )
      IF ( declparam('MODNAME', 'chan_t3_rbratio', 'nchan', 'real', &
           '0.0', '0.0', '500.0', &
           'Side-slope ratio, right bank', &
           'Ratio of horizontal to vertical change in side slope for triangular channel, right bank, chan_type 3', &
           'none')/=0 ) CALL read_error(1, 'chan_t3_rbratio')

      ALLOCATE ( Chan_alpha(Nchan) )
      IF ( declparam('MODNAME', 'chan_alpha', 'nchan', 'real', &
           '0.003', '0.001', '100.0', &
           'Kinematic parameter alpha, chan_type 4', &
           'Kinematic parameter alpha, chan_type 4', &
           'none')/=0 ) CALL read_error(1, 'chan_alpha')

      ALLOCATE ( Chan_cmp(Nchan) )
      IF ( declparam('MODNAME', 'chan_cmp', 'nchan', 'real', &
           '1.67', '0.5', '3.0', &
           'Kinematic parameter m, chan_type 4', &
           'Kinematic parameter m, chan_type 4', &
           'none')/=0 ) CALL read_error(1, 'chan_cmp')

      ALLOCATE (Chan_route_time(Nchan))
      IF ( declparam('MODNAME', 'chan_route_time', 'nchan', 'real', &
           '5.0', '0.1', '15.0', &
           'Time interval for channel flow routing', &
           'Time interval for channel flow routing, should be less'// &
           ' or equal to data timestep, and evenly divisible into data timestep', &
           'minutes')/=0 ) CALL read_error(1, 'chan_route_time')

      ALLOCATE ( Upst_inflow1(Nchan) )
      IF ( declparam('MODNAME', 'upst_inflow1', 'nchan', 'integer', &
           '0', 'bounded', 'nchan', &
           'Upstream inflow channel index number 1', &
           'Channel index number for upstream inflow into current channel', &
           'none')/=0 ) CALL read_error(1, 'upst_inflow1')

      ALLOCATE ( Upst_inflow2(Nchan) )
      IF ( declparam('MODNAME', 'upst_inflow2', 'nchan', 'integer', &
           '0', 'bounded', 'nchan', &
           'Upstream inflow channel index number 2', &
           'Channel index number for upstream inflow into current channel', &
           'none')/=0 ) CALL read_error(1, 'upst_inflow2')

      ALLOCATE ( Upst_inflow3(Nchan) )
      IF ( declparam('MODNAME', 'upst_inflow3', 'nchan', 'integer', &
           '0', 'bounded', 'nchan', &
           'Upstream inflow channel index number 3', &
           'Channel index number for upstream inflow into current channel', &
           'none')/=0 ) CALL read_error(1, 'upst_inflow3')

      ALLOCATE ( Musk_wghtfac(Nchan) )
      IF ( declparam('MODNAME', 'musk_wghtfac', 'nchan', 'real', &
           '0.25', '0.0', '0.5', &
           'Segment Muskingum weighting factor', &
           'Segment Muskingum weighting factor, chan_rtemethod = 1', &
           'none')/=0 ) CALL read_error(1, 'musk_wghtfac')

      ALLOCATE ( Musk_travel_time(Nchan) )
      IF ( declparam('MODNAME', 'musk_travel_time', 'nchan', 'real', &
           '0.5', '0.0', '100.0', &
           'Segment travel time', &
           'Segment travel time, chan_rtemethod = 1', &
           'hours')/=0 ) CALL read_error(1, 'musk_travel_time')

      IF ( Ngain > 0 .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Gain_type(Ngain) )
        IF ( declparam('MODNAME', 'gain_type', 'ngain', 'integer', &
             '1', '1', '2', &
             'Inflow gain type', &
             'Inflow gain type (1=set inflow to subbasin to flow; 2=internal flow addition)',  &
             'none')/=0 ) CALL read_error(1, 'gain_type')

        ALLOCATE ( Gain_flowid(Ngain) )
        IF ( declparam('MODNAME', 'gain_flowid', 'ngain', 'integer', &
             '1', 'bounded', 'nobs', &
             'Index of measured streamflow station', &
             'Index of measured streamflow station', &
             'none')/=0 ) CALL read_error(1, 'gain_flowid')

        ALLOCATE ( Q_gain_id(Ngain) )
        IF ( declparam('MODNAME', 'q_gain_id', 'ngain', 'integer', &
             '0', 'bounded', 'ngain', &
             'Gain channel index number', &
             'Channel index number for upstream inflow as a gain (0 = not included in network)', &
             'none')/=0 ) CALL read_error(1, 'q_gain_id')

        ALLOCATE ( Q_gain(Ngain) )
        IF ( declvar('MODNAME', 'q_gain', 'ngain', Ngain, 'real', &
             'Inflow to corresponding stream segment as a gain', &
             'cfs', Q_gain)/=0 ) CALL read_error(3, 'q_gain')
        Q_gain = 0.0
      ENDIF

      IF ( Ndivert > 0 .OR. Model == DOCUMENTATION ) THEN
        ALLOCATE ( Q_divert_id(Ndivert), Q_divert(Ndivert) )
        IF ( declparam('MODNAME', 'q_divert_id', 'ndivert', 'integer', &
             '0', 'bounded', 'ndivert', &
             'Divert channel index number', &
             'Channel index number for removing flow as a diversion (0 = not included in network)', &
             'none')/=0 ) CALL read_error(1, 'q_divert_id')

        IF ( declvar('obsrte', 'q_divert', 'ndivert', Ndivert, 'real', &
             'Outflow from corresponding stream segment as a diversion', &
             'cfs', Q_divert)/=0 ) CALL read_error(3, 'q_divert')
        Q_divert = 0.0
      ENDIF

      ALLOCATE ( Chan_loss_rate(Nchan) )
      IF ( declparam('MODNAME', 'chan_loss_rate', 'nchan', 'real', &
           '0.0', '0.0', '100000.0', &
           'Loss rate for each channel reach', &
           'Loss rate for each channel reach', &
           'inches/hour')/=0 ) CALL read_error(1, 'chan_loss_rate')

      ALLOCATE ( Wpcoef_a(Nchan) )
      IF ( declparam('MODNAME', 'wpcoef_a', 'nchan', 'real', &
           '0.0', '0.0', '10.0', &
           'Wetted perimeter coef a in wp=a*AREA**b', &
           'Wetted perimeter coef a in wp=a*AREA**b', &
           'none')/=0 ) CALL read_error(1, 'wpcoef_a')

      ALLOCATE ( Wpcoef_b(Nchan) )
      IF ( declparam('MODNAME', 'wpcoef_b', 'nchan', 'real', &
           '0.0', '0.0', '10.0', &
           'Wetted perimeter coef b in wp=a*AREA**b', &
           'Wetted perimeter coef b in wp=a*AREA**b', &
           'none')/=0 ) CALL read_error(1, 'wpcoef_b')

      IF ( declparam('MODNAME', 'chan_theta', 'one', 'real', &
           '0.5', '0.5', '1.0', &
           'Finite-difference spatial weighting factor', &
           'Finite-difference spatial weighting factor', &
           'decimal fraction')/=0 ) CALL read_error(1, 'chan_theta')

      IF ( declparam('MODNAME', 'chan_chi', 'one', 'real', &
           '0.6', '0.5', '1.0', &
           'Finite-difference weighting factor', &
           'Finite-difference weighting factor', &
           'decimal fraction')/=0 ) CALL read_error(1, 'chan_chi')

      IF ( declparam('MODNAME', 'chan_rtemethod', 'one', 'integer', &
           '0', '0', '4', &
           'Channel routing method', &
           'Switch to indicate routing solution method (0=Explicit finite-difference kinematic wave;'// &
           ' 1=Muskingum, fixed travel time; 2=Implicit finite-difference kinematic wave;'// &
           ' 3=Muskingum-Cunge diffusion wave; 4=Method of Characteristics)', &
           'none')/=0 ) CALL read_error(1, 'chan_rtemethod')

      IF ( declparam('MODNAME', 'outlet_sta', 'one', 'integer', &
           '0', 'bounded', 'nobs', &
           'Index of measurement station to use for basin outlet', &
           'Index of measurement station to use for basin outlet', &
           'none')/=0 ) CALL read_error(1, 'outlet_sta')

      IF ( declparam('MODNAME', 'outlet_chan', 'one', 'integer', &
           '0', 'bounded', 'nchan', &
           'Channel segment number of outlet', &
           'Channel segment number of outlet', &
           'none')/=0 ) CALL read_error(1, 'outlet_chan')

      ALLOCATE ( Lat_inflowr(Nchan) )
      IF ( declparam('MODNAME', 'lat_inflowr', 'nchan', 'integer', &
           '0', 'bounded', 'nhru', &
           'Lateral inflow from right ofplane', &
           'Overland flow plane index number for lateral inflow into current channel, right side', &
           'none')/=0 ) CALL read_error(1, 'lat_inflowr')

      ALLOCATE ( Lat_inflowl(Nchan) )
      IF ( declparam('MODNAME', 'lat_inflowl', 'nchan', 'integer', &
           '0', 'bounded', 'nhru', &
           'Lateral inflow from left ofplane', &
           'Overland flow plane index number for lateral inflow into current channel, left side', &
           'none')/=0 ) CALL read_error(1, 'lat_inflowl')

      ALLOCATE ( Ofp_length(Nhru) )
      IF ( declparam('MODNAME', 'lat_inflowl', 'nhru', 'real', &
           '1.0', '1.0', '100000.0', &
           'Length of overland flow plane', &
           'Length of overland flow plane', &
           'feet')/=0 ) CALL read_error(1, 'lat_inflowl')

!sed  IF ( declparam('MODNAME', 'sed_route', 'one', 'integer', &
!sed       '0', '0', '1', &
!sed       'Sediment routing flag', &
!sed       'Switch to indicate whether sediment routing is to be done along with the flow routing (0=no; 1=yes)', &
!sed       'none')/=0 ) CALL read_error(1, 'sed_route')

      ALLOCATE ( Ofp_route_time(Nhru) )
      IF ( declparam('MODNAME', 'ofp_route_time', 'nhru', 'real', &
           '5.0', '0.1', '15.0', &
           'Time interval for overland flow routing', &
           'Time interval for overland flow routing, should be less'// &
           ' or equal to data timestep, and evenly divisible into data timestep', &
           'minutes')/=0 ) CALL read_error(1, 'ofp_route_time')

      IF ( Nlake > 0 .OR. Model == DOCUMENTATION ) THEN
        IF ( Mxnsos < 1 ) Mxnsos = 1
        ALLOCATE ( Wv5(Mxnsos, Nlake), S5(Mxnsos, Nlake) )
        ALLOCATE ( C5(Mxnsos, Nlake), Wv15(Mxnsos, Nlake) )
        ALLOCATE ( S15(Mxnsos, Nlake), C15(Mxnsos, Nlake) )
        ALLOCATE ( Lake_coef(Nlake))

        ALLOCATE ( Lake_din1(Nlake) )
        IF ( declparam('strmflow', 'lake_din1', 'nlake', 'real', &
             '0.1', '0.0', '1.0', &
             'Surface reservoir inflow from the previous time step', &
             'Surface reservoir inflow from the previous time step', &
             'cfs')/=0 ) CALL read_error(1, 'Lake_din1')

        IF ( declparam('MODNAME', 'lake_coef', 'nlake', 'real', &
             '0.1', '0.0', '1.0', &
             'Linear reservoir routing coefficient', &
             'Coefficient to route reservoir storage to streamflow using equation: res_flow = lake_coef * res_stor', &
             '1/day')/=0 ) CALL read_error(1, 'Lake_coef')

        ALLOCATE ( O2(Mxnsos, Nlake) )
        IF ( declparam('strmflow', 'o2', 'mxnsos,nlake', 'real', &
             '0.0', '0.0', '100000.0', &
            'Outflow values in outflow/storage table for Puls routing', &
            'Outflow values in outflow/storage table for Puls routing', &
             'cfs')/=0 ) CALL read_error(1, 'o2')

        ALLOCATE ( S2(Mxnsos, Nlake) )
        IF ( declparam('strmflow', 's2', 'mxnsos,nlake', 'real', &
             '0.0', '0.0', '100000.0', &
            'Storage values in outflow/storage table for Puls routing', &
            'Storage values in outflow/storage table for Puls routing', &
             'cfs-days')/=0 ) CALL read_error(1, 's2')

        ALLOCATE ( Nsos(Nlake) )
        IF ( declparam('strmflow', 'nsos', 'nlakeake', 'integer', &
             '0', '0', '10', &
          'Number of storage/outflow values in table for Puls routing', &
          'Number of storage/outflow values in table for Puls routing', &
             'none')/=0 ) CALL read_error(1, 'nsos')

        ALLOCATE ( Lake_type(Nlake) )
        IF ( declparam('strmflow', 'lake_type', 'nlake', 'integer', &
             '8', '8', '12', &
             'Type of surface reservoir', &
             'Type of surface reservoir (8=Puls routing; 9=Linear routing; 10=Flow through;'// &
             ' 11=Broad crested weir; 12=Gate opening)', &
             'none')/=0 ) CALL read_error(1, 'lake_type')

        ALLOCATE ( Upst_res1(Nlake) )
        IF ( declparam('strmflow', 'upst_res1', 'nlake', 'integer', &
             '0', 'bounded', 'nlake', &
             'Upstream reservoir index number 1', &
             'Index number for the first upstream reservoir whose outflow is inflow to this reservoir', &
             'none')/=0 ) CALL read_error(1, 'upst_res1')

        ALLOCATE ( Upst_res2(Nlake) )
        IF ( declparam('strmflow', 'upst_res2', 'nlake', 'integer', &
             '0', 'bounded', 'nlake', &
             'Upstream reservoir index number 2', &
             'Index number for the second upstream reservoir whose outflow is inflow to this reservoir', &
             'none')/=0 ) CALL read_error(1, 'upst_res2')

        ALLOCATE ( Upst_res3(Nlake) )
        IF ( declparam('strmflow', 'upst_res3', 'nlake', 'integer', &
             '0', 'bounded', 'nlake', &
             'Upstream reservoir index number 3', &
             'Index number for the third upstream reservoir whose outflow is inflow to this reservoir', &
             'none')/=0 ) CALL read_error(1, 'upst_res3')

        ALLOCATE ( Lake_sto(Nlake), Lake_outq(NlakeP1), Din1(Nlake) )
      ENDIF

! Allocate arrays for local variables
      ALLOCATE ( Qsrolat(Ncdels), Q_ndels(Ncdels, Nhru) )
      ALLOCATE ( Latsw(Nchan), Upsw(Nchan), Upchan(3, Nchan), Nxs(Nchan) )
      ALLOCATE ( Mocgrids(Nchan), Czero(Nchan), Cone(Nchan), Ctwo(Nchan) )
      ALLOCATE ( Chan_loss_ftsec(Nchan), Uprch_area(Nchan), Ofar(Nchan) )
      ALLOCATE ( Rb_hru(Nchan), Rb_hru_area(Nchan) )
      ALLOCATE ( Lb_hru(Nchan), Lb_hru_area(Nchan) )
      ALLOCATE ( Chan_xmoc(Ncmoc, Nchan), Chan_amoc(Ncmoc, Nchan) )
      ALLOCATE ( Alpr1(Nchan), Cmp1(Nchan), Chan_parm2(Nchan) )
      ALLOCATE ( Tc_topw(Nchan), Tc_half_topw(Nchan) )
      ALLOCATE ( Dx(Nchan), Dts(Nchan), Dtdx(Nchan), Qmxa(Nchan) )
      ALLOCATE ( Qout(Ncdels, Nchan), A_xs(Nchxs, Nchan) )
      ALLOCATE ( Qinpast_xs(Nchxs, Nchan) )
!      ALLOCATE ( Strahler_chan(Nchan) )
      ALLOCATE ( Storm_obsvol(MAX(Nobs,1)), Storm_obspk(MAX(Nobs,1)) )
      ALLOCATE ( Qp(Nchxs), Ap(Nchxs), Q(Nchxs), A(Nchxs) )
!sed  ALLOCATE ( Sedin(Ncdels), Sedinlat(Ncdels), Sedp(Nchxs) )
!sed  ALLOCATE ( Sed(Nchxs), Sedout(Ncdels, Nchan), Sed_xs(Nchxs, Nchan) )
!sed  ALLOCATE ( Sed_ndels(Ncdels, Nhru), Sedout(Ncdels, Nchan )

      END FUNCTION kchdecl

!***********************************************************************
!     kchinit - Initialize kinroute_chan module - get parameter values,
!               compute and check areas and types
!***********************************************************************
      INTEGER FUNCTION kchinit()
      USE PRMS_CONSTANTS, ONLY: ERROR_param, CLOSEZERO, ERROR_dim
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Timestep
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv
      USE PRMS_KROUT_CHAN
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam, control_string
      INTRINSIC :: INDEX, CHAR
      EXTERNAL :: storminit, musk_init, AlphaRm, Dimchk
! Local Variables
      INTEGER :: iru, nc !, ichan
      LOGICAL :: filflg
      INTEGER :: i, j, k, kk, nd, ng, iprint, jup
      REAL :: ave_travel_time, min_travel_time, perdif, o1, fac
      CHARACTER(LEN=256) :: output_path
!***********************************************************************
      kchinit = 0

!      IF ( getparam('MODNAME', 'strahler_num', Nstrahler, 'integer', Strahler_num)/=0 ) CALL read_error(2, 'strahler_num')
!
!      IF ( getparam('MODNAME', 'hru_strahler', Nstrahler, 'integer', Hru_strahler)/=0 ) CALL read_error(2, 'hru_strahler')

      IF ( getparam('MODNAME', 'chan_type', Nchan, 'integer', Chan_type)/=0 ) CALL read_error(2, 'chan_type')

      IF ( getparam('MODNAME', 'chan_ndx', Nchan, 'integer', Chan_ndx)/=0 ) CALL read_error(2, 'chan_ndx')

      IF ( getparam('MODNAME', 'chan_thresh', Nchan, 'real', Chan_thresh)/=0 ) CALL read_error(2, 'chan_thresh')

      IF ( getparam('MODNAME', 'chan_length', Nchan, 'real', Chan_length)/=0 ) CALL read_error(2, 'chan_length')

      IF ( getparam('MODNAME', 'chan_slope', Nchan, 'real', Chan_slope)/=0 ) CALL read_error(2, 'chan_slope')

      IF ( getparam('MODNAME', 'chan_width', Nchan, 'real', Chan_width)/=0 ) CALL read_error(2, 'chan_width')

      IF ( getparam('MODNAME', 'chan_t3_lbratio', Nchan, 'real', Chan_t3_lbratio)/=0 ) CALL read_error(2, 'chan_t3_lbratio')

      IF ( getparam('MODNAME', 'chan_t3_rbratio', Nchan, 'real', Chan_t3_rbratio)/=0 ) CALL read_error(2, 'chan_t3_rbratio')

      IF ( getparam('MODNAME', 'chan_rough', Nchan, 'real', Chan_rough)/=0 ) CALL read_error(2, 'chan_rough')

      IF ( getparam('MODNAME', 'chan_alpha', Nchan, 'real', Chan_alpha)/=0 ) CALL read_error(2, 'chan_alpha')

      IF ( getparam('MODNAME', 'chan_cmp', Nchan, 'real', Chan_cmp)/=0 ) CALL read_error(2, 'chan_cmp')

      IF ( getparam('MODNAME', 'chan_route_time', Nchan, 'real', Chan_route_time)/=0 ) CALL read_error(2, 'chan_route_time')

      IF ( getparam('MODNAME', 'upst_inflow1', Nchan, 'integer', Upst_inflow1)/=0 ) CALL read_error(2, 'upst_inflow1')

      IF ( getparam('MODNAME', 'upst_inflow2', Nchan, 'integer', Upst_inflow2)/=0 ) CALL read_error(2, 'upst_inflow2')

      IF ( getparam('MODNAME', 'upst_inflow3', Nchan, 'integer', Upst_inflow3)/=0 ) CALL read_error(2, 'upst_inflow3')

      IF ( getparam('MODNAME', 'lat_inflowr', Nchan, 'integer', Lat_inflowr)/=0 ) CALL read_error(2, 'lat_inflowr')

      IF ( getparam('MODNAME', 'lat_inflowl', Nchan, 'integer', Lat_inflowl)/=0 ) CALL read_error(2, 'lat_inflowl')


!sed  IF ( getparam('MODNAME', 'sed_route', 1, 'integer', Sed_route)/=0 ) CALL read_error(2, 'sed_route')

      IF ( getparam('MODNAME', 'ofp_route_time', Nhru, 'real', Ofp_route_time)/=0 ) CALL read_error(2, 'ofp_route_time')

      IF ( getparam('MODNAME', 'wpcoef_a', Nchan, 'real', Wpcoef_a)/=0 ) CALL read_error(2, 'wpcoef_a')

      IF ( getparam('MODNAME', 'wpcoef_b', Nchan, 'real', Wpcoef_b)/=0 ) CALL read_error(2, 'wpcoef_b')

      IF ( getparam('MODNAME', 'chan_rtemethod', 1, 'integer', Chan_rtemethod)/=0 ) CALL read_error(2, 'chan_rtemethod')

      IF ( getparam('MODNAME', 'chan_theta', 1, 'real', Chan_theta)/=0 ) CALL read_error(2, 'chan_theta')

      IF ( getparam('MODNAME', 'chan_chi', 1, 'real', Chan_chi)/=0 ) CALL read_error(2, 'chan_chi')
      Chifactor = (1.0-Chan_chi)/Chan_chi

      IF ( getparam('MODNAME', 'chan_parm1', Nchan, 'real', Chan_parm1)/=0 ) CALL read_error(2, 'chan_parm1')

      IF ( getparam('MODNAME', 'outlet_sta', 1, 'integer', Outlet_sta)/=0 ) CALL read_error(2, 'outlet_sta')
      IF ( Outlet_sta == 0 ) Outlet_sta = 1

      IF ( getparam('MODNAME', 'outlet_chan', 1, 'integer', Outlet_chan)/=0 ) CALL read_error(2, 'outlet_chan')
      IF ( Outlet_chan == 0 ) Outlet_chan = Nchan

!***compute upsw latsw and flgth for each channel
      DO i = 1, Nchan
        Contrib_area_chan(i) = 0.0
        IF ( Lat_inflowr(i) == 0 .AND. Lat_inflowl(i) == 0 ) THEN
          Latsw(i) = 0
        ELSE
          !rsr, note does not include cascading areas, FIX
          IF ( Lat_inflowr(i)>0 ) Contrib_area_chan(i) = Hru_area(Lat_inflowr(i))
          IF ( Lat_inflowl(i)>0 ) Contrib_area_chan(i) = Contrib_area_chan(i) + Hru_area(Lat_inflowl(i))
          Latsw(i) = 1
        ENDIF
        Upchan(1, i) = Upst_inflow1(i)
        Upchan(2, i) = Upst_inflow2(i)
        Upchan(3, i) = Upst_inflow3(i)
        IF ( Upst_inflow1(i) == 0 .AND. Upst_inflow2(i) == 0 .AND. Upst_inflow3(i) == 0 ) THEN
          Upsw(i) = 0
        ELSE
          iprint = 0
          IF ( Upst_inflow1(i) /= 0 ) THEN
            IF ( Chan_route_time(Upst_inflow1(i)) > Chan_route_time(i) ) iprint = 1
          ENDIF
          IF ( Upst_inflow2(i) /= 0 ) THEN
            IF ( Chan_route_time(Upst_inflow2(i)) > Chan_route_time(i) ) iprint = 1
          ENDIF
          IF ( Upst_inflow3(i) /= 0 ) THEN
            IF ( Chan_route_time(Upst_inflow3(i)) > Chan_route_time(i) ) iprint = 1
          ENDIF
          IF ( iprint == 1 ) THEN
            PRINT *, 'Routing time for upstream channel must be less than or equal to this'// &
                     ' channels routing time, chan#= ', i
            CALL error_stop('routing times', ERROR_param)
          ENDIF
          Upsw(i) = 1
        ENDIF

        Chan_parm2(i) = 0.0
        IF ( Chan_type(i) == 1 ) THEN
          IF ( Chan_slope(i) < CLOSEZERO .OR. Chan_rough(i) < CLOSEZERO .OR. Chan_width(i) < CLOSEZERO ) THEN
            PRINT *, 'Rectangular channel, type 1, parameters  chan_slope, chan_rough, and chan_width should be'// &
                     ' defined greater than 0. , chan#= ', i
            CALL error_stop('rectangular channel', ERROR_param)
          ENDIF
          Chan_parm1(i) = Chan_width(i)

        ELSEIF ( Chan_type(i) == 3 ) THEN
          IF ( Chan_slope(i) < CLOSEZERO .OR. Chan_rough(i) < CLOSEZERO &
               .OR. Chan_t3_rbratio(i) < CLOSEZERO .OR. Chan_t3_lbratio(i) < CLOSEZERO ) THEN
            PRINT *, 'Triangular channel, type 3, parameters chan_slope, chan_rough, chan_t3_rbratio and'// &
                  ' chan_t3_lratio should be defined greater than 0.0, chan#= ', i
            CALL error_stop('triangular channel', ERROR_param)
          ENDIF
          Tc_topw(i) = Chan_t3_lbratio(i) + Chan_t3_rbratio(i)
          Tc_half_topw(i) = Tc_topw(i) * 0.5
          Chan_parm1(i) = Chan_t3_lbratio(i)
          Chan_parm2(i) = Chan_t3_rbratio(i)

        ELSEIF ( Chan_type(i) == 4 ) THEN
          IF ( Chan_alpha(i) < CLOSEZERO .OR. Chan_cmp(i) < CLOSEZERO) THEN
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
!          DO j = 1, Nchan
!            IF ( Strahler_chan(j) /= 0 )
!     &           Chan_alpha(j) = (7-Strahler_chan(j)) * Chan_alpha(j)
!          ENDDO

        ELSEIF ( Chan_type(i) == 2 .OR. Chan_type(i) == 10 ) THEN
          IF ( Chan_parm1(i) < CLOSEZERO ) THEN
            PRINT *, 'Channel type 2 or 10, parameters  chan_slope, chan_rough, and chan_parm1 should be'// &
                     ' defined greater than 0., chan#= ', i
            CALL error_stop('channel type 2 or 10', ERROR_param)
          ENDIF

        ELSEIF ( Chan_type(i) == 11 ) THEN
          Chan_parm1(i) = Wpcoef_a(i)
          Chan_parm2(i) = Wpcoef_b(i)

        ELSEIF ( Chan_type(i) < 7 .OR. Chan_type(i) > 12 ) THEN
          PRINT *, 'Invalid chan_type for channel:', i
          CALL error_stop('channel', ERROR_param)

!       ELSEIF ( Chan_type(i) == 8 .OR. Chan_type(i) == 9 ) THEN
!         PRINT *, 'Reservoir routing not yet implemented, chan_types 8 and 9 are not valid, chan#=  ', i
!         CALL error_stop('reservoir', ERROR_param)

        ENDIF

      ENDDO

      IF ( getparam('MODNAME', 'ofp_length', Nhru, 'real', Ofp_length)/=0 ) CALL read_error(2, 'ofp_length')

      DO i = 1, Nchan
        Nxs(i) = Chan_ndx(i) + 1
        CALL Dimchk( 'nchxs', Nchxs, Nxs(i) )
! channel type is 1, 2, 3, 4, 10, or 11
        IF ( Chan_type(i) < 5 .OR. Chan_type(i) > 9 ) THEN
          IF ( Chan_type(i) /= 4 ) &
               CALL AlphaRm(Chan_type(i), Chan_rough(i), Chan_slope(i), Chan_parm1(i), Chan_parm2(i), Chan_alpha(i), Chan_cmp(i))
          Alpr1(i) = 1.0 / Chan_alpha(i)
          Cmp1(i) = 1.0 / Chan_cmp(i)
        ENDIF
        Dx(i) = Chan_length(i) / Chan_ndx(i)
        Dts(i) = Chan_route_time(i) * 60.0
        Dtdx(i) = Dts(i) / Dx(i)
      ENDDO

! initialize Method of Characteristics arrays
      IF ( Chan_rtemethod == 4 ) THEN
        Mocgrids = 0
        Chan_xmoc = 0.0
        Chan_amoc = 0.0
      ENDIF

      IF ( Ngain > 0 ) THEN
        IF ( getparam('MODNAME', 'gain_type', Ngain, 'integer', Gain_type)/=0 ) CALL read_error(2, 'gain_type')

        IF ( getparam('MODNAME', 'gain_flowid', Ngain, 'integer', Gain_flowid)/=0 ) CALL read_error(2, 'gain_flowid')

        IF ( getparam('MODNAME', 'q_gain_id', Ngain, 'integer', Q_gain_id)/=0 ) CALL read_error(2, 'q_gain_id')
        ng = 0
        DO i = 1, Ngain
          IF ( Q_gain_id(i) > 0 ) ng = ng + 1
        ENDDO
        IF ( Ngain /= ng ) CALL error_stop('ngain dimension problem', ERROR_dim)
      ENDIF

      IF ( Ndivert > 0 ) THEN
        IF ( getparam('MODNAME', 'q_divert_id', Ndivert, 'integer', Q_divert_id)/=0 ) CALL read_error(2, 'q_divert_id')
        nd = 0
        DO i = 1, Ndivert
          IF ( Q_divert_id(i) > 0 ) nd = nd + 1
        ENDDO
        IF ( Ndivert /= nd ) CALL error_stop('ndivert dimension problem', ERROR_dim)
      ENDIF

      IF ( Chan_rtemethod == 1 ) THEN
        IF ( getparam('MODNAME', 'musk_wghtfac', Nchan, 'integer', Musk_wghtfac)/=0 ) CALL read_error(2, 'musk_wghtfac')
        IF ( getparam('MODNAME', 'musk_travel_time', Nchan, 'integer', Musk_travel_time)/=0 ) &
             CALL read_error(2, 'musk_travel_time')
        min_travel_time = 1.0
        ave_travel_time = 0.0
        DO i = 1, Nchan
          ave_travel_time = ave_travel_time + Musk_travel_time(i)
          IF ( min_travel_time > Musk_travel_time(i) ) min_travel_time = Musk_travel_time(i)
        ENDDO
        ave_travel_time = ave_travel_time/Nchan
        CALL dpreal('Minimum travel time = ', min_travel_time, 1, 0)
        CALL dpreal('Average travel time = ', ave_travel_time, 1, 0)
        CALL musk_init()
      ENDIF

      IF ( getparam('MODNAME', 'chan_loss_rate', Nchan, 'real', Chan_loss_rate)/=0 ) CALL read_error(2, 'chan_loss_rate')

      Ofar = 0.0
      Uprch_area = 0.0
      DO i = 1, Nchan
!       fac = 0.5
!       IF ( Lat_inflowr(i) == 0 .OR. Lat_inflowl(i) == 0 ) fac = 1.0
!       chanarea = Chan_length(i) * Chan_width(i) * fac
!rsr, note, chan_width not present, need to add to USE list
        IF ( Lat_inflowr(i) > 0) Ofar(i) = Chan_length(i) * Ofp_length(Lat_inflowr(i)) !+ chanarea
        IF ( Lat_inflowl(i) > 0) Ofar(i) = Ofar(i) + Chan_length(i) * Ofp_length(Lat_inflowl(i)) !+ chanarea

        DO j = 1, 3
          jup = Upchan(j, i)
          IF ( jup > 0 ) Uprch_area(i) = Uprch_area(i) + Ofar(jup) + Uprch_area(jup)
        ENDDO
      ENDDO
      Ofarea_total = (Uprch_area(Outlet_chan) + Ofar(Outlet_chan)) * AREACONV
      perdif = ABS( Ofarea_total*Basin_area_inv-1.0 )

!      PRINT *, 'Sum of routed areas differs from basin area by percent =  ', perdif

      IF ( perdif >= 0.05 ) THEN
        perdif = perdif*100.
        PRINT *, 'Sum of routed areas differs from basin area by more than 5 percent, percent =  ', perdif
        PRINT *, 'Sum of routed area = ', Ofarea_total
        PRINT *, 'Basin area = ', 1.0/Basin_area_inv
!        RETURN
      ENDIF

      fac = 1.0/(LENGTH_CONV*3600.0)
      DO i = 1, Nchan
        Chan_loss_ftsec(i) = Chan_loss_rate(i)*fac
        IF ( Latsw(i) == 1 ) THEN
          IF ( Lat_inflowr(i) > 0 ) THEN
            iru = Lat_inflowr(i)
            Rb_hru_area(iru) = Chan_length(i) * Ofp_length(Lat_inflowr(i)) * AREACONV
            Rb_hru(i) = iru
          ENDIF
          IF ( Lat_inflowl(i) > 0 ) THEN
            iru = Lat_inflowl(i)
            Lb_hru_area = Chan_length(i) * Ofp_length(Lat_inflowl(i)) * AREACONV
            Lb_hru = iru
          ENDIF
        ENDIF
      ENDDO

!** reservoir computations  **************

      IF ( Nlake > 0 ) THEN

        IF ( getparam('MODNAME', 'lake_din1', Nlake, 'real', Lake_din1)/=0 ) CALL read_error(2, 'lake_din1')

        IF ( getparam('MODNAME', 's2', Mxnsos*Nlake, 'real', O2)/=0 ) CALL read_error(2, 's2')

        IF ( getparam('MODNAME', 's2', Mxnsos*Nlake, 'real', S2)/=0 ) CALL read_error(2, 's2')

        IF ( getparam('MODNAME', 'lake_type', Nlake, 'integer', Lake_type)/=0 ) CALL read_error(2, 'lake_type')

        IF ( getparam('MODNAME', 'lake_coef', Nlake, 'integer', Lake_coef)/=0 ) CALL read_error(2, 'lake_coef')

        IF ( getparam('MODNAME', 'nsos', Nlake, 'integer', Nsos)/=0 ) CALL read_error(2, 'nsos')

        IF ( getparam('MODNAME', 'upst_res1', Nlake, 'integer', Upst_res1)/=0 ) CALL read_error(2, 'upst_res1')

        IF ( getparam('MODNAME', 'upst_res2', Nlake, 'integer', Upst_res2)/=0 ) CALL read_error(2, 'upst_res2')

        IF ( getparam('MODNAME', 'upst_res3', Nlake, 'integer', Upst_res3)/=0 ) CALL read_error(2, 'upst_res3')

        S5 = 0.0
        C5 = 0.0
        S15 = 0.0
        C15 = 0.0
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

      DO i = 1, Nchan
        Qinlat_chan(i) = 0.0
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
              CALL dpint4('Routing time for lateral inflow must be less than or equal to this'// &
                          ' channels routing time, chan#= ', i, 1, 0)
              RETURN
            ENDIF
          ENDIF

        ENDIF
      ENDDO
      Q_xs = 0.0
      Qin = 0.0
      Qinpast_xs = 0.0
      A_xs = 0.0
!sed  Sed_xs = 0.0
!sed  Sedout = 0.0
!sed  Sed_chan = 0.0
!sed  Sed_tot = 0.0
      Storm_pk_obs = 0.0
      Storm_pk_sim = 0.0
      Qinlat = 0.0

!-----open the storm file.
      File_unit = 880
      IF ( Timestep==1 ) THEN
        output_path = ' '
        IF ( control_string(output_path, 'model_output_file') /= 0 ) RETURN
        nc = INDEX(output_path,CHAR(0)) - 1
        IF ( nc > 250 ) nc = 250
        output_path(1:nc+6) = output_path(1:nc)//'.storm'
!rsr    i = getoutname(output_path, '.storm')
        filflg = .FALSE.
        INQUIRE (FILE=output_path, EXIST=filflg)
        IF ( filflg ) THEN
          OPEN (UNIT=File_unit, FILE=output_path, STATUS='old')
          CLOSE (UNIT=File_unit, STATUS='delete')
        ENDIF
        nc = nc + 6
        WRITE (*, '(//,2A,//)') '***Storm output in: ', output_path(1:nc)
        OPEN (UNIT=File_unit, FILE=output_path(1:nc), ACCESS='sequential', FORM='formatted', STATUS='new')
      ENDIF

      WRITE (File_unit, 9001)
!     WRITE (File_unit+1, 9002)

      kchinit = 0

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
      USE PRMS_SET_TIME, ONLY: Subdaily_status, Subdaily_num, Timestep_seconds
      USE PRMS_MODULE, ONLY: Nobs
      USE PRMS_OBS, ONLY: Runoff
      USE PRMS_INTCP, ONLY: Basin_net_ppt
      USE PRMS_FLOWVARS, ONLY: Ssres_flow, Basin_ssflow
      USE PRMS_GWFLOW, ONLY: Gwres_flow, Basin_gwflow
      IMPLICIT NONE
      INTRINSIC SNGL
! Functions and Externals
      EXTERNAL AccumLat, reservoir, storminit, Dimchk, rte_chaninit
      EXTERNAL :: rte_run1, rte_compvars, rte_inflow, rte_runloop, rte_flowleft
! Local Variables
      INTEGER :: k, ires, ich, jch, nxsj, ndels, chantype, jj
      REAL :: alpha, alpha1, m, m_inv, m_1, m_1abs, b1, st_vol
      REAL :: dt_dx, alpha_m, dtdxchi_inv, alpdts, alp_mdts, dt_conv
      REAL :: cnv
!     REAL :: chan_sum_final, chan_net
! Save Variables
      REAL, SAVE :: st_ppt_sum, st_ssf_sum, st_gwf_sum, q_peak
!***********************************************************************
      kchrun = 1

      Dtsec = Timestep_seconds

! check for whether storm period
! Subdaily_status =  Switch signifying storm status,
!   0=not in storm (daily mode)
!   1=first time step of storm
!   2=middle of storm
!   3=storm end

      IF ( Subdaily_status == 0 ) THEN
        kchrun = 0
        RETURN
      ENDIF

!  if Subdaily_status = 1 initialize storm variables
      IF ( Subdaily_status==1 ) THEN
        CALL storminit()
        Storm_pk_obs = 0.0
        Storm_pk_sim = 0.0
        Dt_sroff = 0.0
        st_ppt_sum = 0.0
        st_ssf_sum = 0.0
        st_gwf_sum = 0.0
        q_peak = 0.0

        CALL rte_chaninit()
        Chanvol_sum = Chan_sum_init

!sed    IF ( Sed_route == 1 ) THEN
!sed      Sed_xs = 0.0
!sed      Sedout = 0.0
!sed    ENDIF

! if Subdaily_status = 3, then write out totals from previous storm
      ELSEIF ( Subdaily_status == 3 ) THEN
        IF ( Nobs > 0 ) THEN
!          IF ( getvar('mga', 'storm_obsvol', Nobs, 'real', Storm_obsvol) /= 0 ) RETURN
!          IF ( getvar('mga', 'storm_obspk', Nobs, 'real', Storm_obspk) /= 0 ) RETURN
        ELSE
          Storm_obsvol(Outlet_sta) = 0.0
          Storm_obspk(Outlet_sta) = 0.0
        ENDIF
        st_vol = St_sroff + st_ssf_sum + st_gwf_sum
        Storm_pk_obs = Storm_obspk(Outlet_sta)
        Storm_pk_sim = q_peak

        WRITE (File_unit, 9001) Subdaily_num, st_ppt_sum, st_vol, Storm_routvol, Storm_obsvol(Outlet_sta), &
                                q_peak, Storm_pk_obs, Storm_pptexc, st_ssf_sum, st_gwf_sum, St_sroff
!       CALL dattim('now', nowtime)
!       WRITE (File_unit, *) nowtime
!       PRINT *, nowtime
! Compute flow left in channels at the end of the storm
!       CALL rte_flowleft(chan_sum_final)
!       chan_net = chan_sum_final - Chan_sum_init
!       WRITE (File_unit+1, 9002) Chan_sum_init, chan_sum_final,
!    +                            chan_net,
!    +                            chan_net*FT3TOINCHES/Ofarea_total

!       CALL storminit()

        kchrun = 0
        RETURN
      ENDIF

      CALL rte_run1()

!      IF ( getvar('ofroute', 'q_ndels', Ncdels*Nhru, 'real', Q_ndels)/= 0 ) RETURN

!sed  IF ( getvar('ofroute', 'sed_ndels', Ncdels*Nhru, 'real', Sed_ndels) /= 0 ) RETURN

!***
!*** loop for each channel and reservoir segment(nchan)***
!***
      DO ich = 1, Nchan
        Qinlat_chan(ich) = 0.0
        jch = ich
        CALL rte_compvars(nxsj, chantype, dt_dx, dtdxchi_inv, alpha, alpha1, m, m_inv, m_1, m_1abs, alpdts, &
                          alpha_m, alp_mdts, b1, ndels, Nxs(ich), Chan_type(ich), Dtdx(ich), Chan_alpha(ich), &
                          Alpr1(ich), Chan_cmp(ich), Cmp1(ich), Dts(ich), Chan_route_time(ich), Chan_chi)
        CALL Dimchk('ncdels', Ncdels, ndels)

        CALL rte_inflow(jch, ndels, Upsw(ich), Upchan(1, ich))
!***
!*** Determine and accumulate lateral inflows to segments ***
!***
        Qinlat = 0.0
        Qsrolat = 0.0
!sed    Sedinlat = 0.0

!*** Determine and accumulate upstream input to segment ich ***
        IF ( Latsw(ich) == 1 ) THEN
!***     Determine lateral inflow from right bank
          IF ( Lat_inflowr(ich) > 0 ) &
               CALL AccumLat(Lat_inflowr(ich), ndels, Rb_hru_area(ich), Ssres_flow(Rb_hru(ich)), &
                            Gwres_flow(Rb_hru(ich)), Chan_length(ich), Ofp_route_time(Lat_inflowr(ich)), Qinlat, Qsrolat)

!***     Determine lateral inflow from left bank
          IF ( Lat_inflowl(ich) > 0 ) &
               CALL AccumLat(Lat_inflowl(ich), ndels, Lb_hru_area(ich), Ssres_flow(Lb_hru(ich)), &
                            Gwres_flow(Lb_hru(ich)), Chan_length(ich), Ofp_route_time(Lat_inflowl(ich)), Qinlat, Qsrolat)

          dt_conv = Chan_length(ich) * Dtsec / ndels
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
!sed                       A_xs(1, ich), Sed_xs(1, ich), Sedout(1, ich), Sed_chan(ich), Qinpast_xs(1, ich))
          IF ( Ngain > 0 ) THEN
            DO jj = 1, Ngain
              IF ( Q_gain_id(jj) == ich ) THEN
                IF ( Gain_type(jj) == 1 ) THEN
                  Q_chan(ich) = Runoff(Gain_flowid(jj))
                ELSE
                  Q_chan(ich) = Q_chan(ich) + Runoff(Gain_flowid(jj))
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
        Q_chan_timestep(ich) = 0.0
        Qinlat_chan_ts(ich) = 0.0
        cnv = Chan_length(ich)*Dts(ich)
        DO jj = 1, ndels
          Q_chan_timestep(ich) = Q_chan_timestep(ich) + Qout(jj, ich)*Dts(ich)
          Qinlat_chan_ts(ich) = Qinlat_chan_ts(ich) + Qinlat(jj)*cnv
        ENDDO
        Q_chan_timestep(ich) = Q_chan_timestep(ich)/Dtsec
        Qinlat_chan_ts(ich) = Qinlat_chan_ts(ich)/Dtsec

!*** End of nchan (ich) loop
      ENDDO

      Qday_sum = Qday_sum + (Q_chan(Outlet_chan)*Dtsec)
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

      kchrun = 0

 9001 FORMAT (I3, 2F8.2, F9.3, F8.2, 2F9.1, 4F8.3)
!9002 FORMAT (14x, 3F10.0, F10.3)

      END FUNCTION kchrun

!***********************************************************************
!     kchclean - Close the channel file
!***********************************************************************
      INTEGER FUNCTION kchclean()
      USE PRMS_KROUT_CHAN, ONLY:File_unit
      IMPLICIT NONE
!***********************************************************************
      CLOSE (UNIT=File_unit)
!     CLOSE (UNIT=File_unit+1)
      kchclean = 0
      END FUNCTION kchclean

!***********************************************************************
!***********************************************************************
      SUBROUTINE storminit()
      USE PRMS_KROUT_CHAN, ONLY:Dt_sroff, St_sroff, Chanvol_daysum, &
          Chanvol_nchansum, Chanvol_sum, Storm_routvol, Qin_chan, &
          Qin_totchan, Qout, Q_chan, Qday_sum, Nchan, Q_chan_timestep, Qinlat_chan_ts
      IMPLICIT NONE
      INTEGER :: i
!***********************************************************************
      Dt_sroff = 0.0
      St_sroff = 0.0
      Storm_routvol = 0.0
      Chanvol_daysum = 0.0
      Chanvol_nchansum = 0.0
      Chanvol_sum = 0.0
      Qday_sum = 0.0
      Qout = 0.0
      DO i = 1, Nchan
        Q_chan(i) =-11.0
        Qin_chan(i) = -11.0
        Q_chan_timestep(i) = -11.0
        Qin_totchan(i) = -11.0
        Qinlat_chan_ts(i) = -11.0
      ENDDO
      !rsr, initialize Q_chan and Qin_chan from daily flows
      END SUBROUTINE storminit

!***********************************************************************
!  Accumulate the lateral inflows and sediment over the timesteps
!***********************************************************************
      SUBROUTINE AccumLat(Jlat, Ndels, Hru_area, Ssflow, Gwflow, Chlt, Ortm, Qinlat, Qsrolat)
      USE PRMS_KROUT_CHAN, ONLY: Q_ndels
      USE PRMS_SET_TIME, ONLY: Timestep_minutes
!sed  USE PRMS_KROUT_CHAN, ONLY:Sedinlat, Sed_ndels
      IMPLICIT NONE
      INTRINSIC :: NINT
      EXTERNAL :: AccumData
! Variable Definitions
!    Ndels    - Number of timesteps in a data time step
!    ndelslat - Number of timesteps in a data time step, lateral
!    (ofp_route_time)
! Arguments
      INTEGER, INTENT(IN) :: Jlat, Ndels
      REAL, INTENT(IN) :: Hru_area, Chlt, Ssflow, Gwflow
      REAL, INTENT(IN) :: Ortm
      REAL, INTENT(INOUT) :: Qinlat(Ndels)
      REAL, INTENT(INOUT) :: Qsrolat(Ndels)
! Local Variables
      INTEGER :: k, ndelslat, have_sro
      REAL :: flw_pct, unit_flw, sro(Ndels)
!***********************************************************************
      ndelslat = NINT( Timestep_minutes/Ortm )
      sro = 0.0 !sro in cfs or ft^2/s???
      CALL AccumData( Ndels, ndelslat, Q_ndels(:, Jlat), sro )
      have_sro = 0
      DO k = 1, Ndels
        Qsrolat(k) = Qsrolat(k) + sro(k)
        IF ( sro(k) > 0 ) have_sro = 1
      ENDDO
!sed  CALL AccumData( Ndels, ndelslat, Sed_ndels(:, Jlat), Sedinlat )
      flw_pct = Hru_area * Ssflow + Hru_area * Gwflow
      IF ( flw_pct > 0.0 .OR. have_sro == 1 ) THEN
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
      USE PRMS_KROUT_CHAN, ONLY: Chan_rtemethod, AREACONV, Dtsec, Mocgrids, Nchan, Ofarea_total, Ofar, Uprch_area, &
          Chan_sum_init, Q_xs, Chan_xmoc, Chan_amoc, Alpr1, Cmp1, Dx, Dts, A_xs, Qout, Qinpast_xs, Qmxa, Nxs, Ncdels
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_route
      USE PRMS_CONSTANTS, ONLY: ERROR_param
      USE PRMS_FLOWVARS, ONLY: Basin_cfs
      IMPLICIT NONE
      INTRINSIC :: SNGL
      EXTERNAL :: MethofCharInit
!     EXTERNAL :: error_stop
! Local Variables
      INTEGER :: i, j, k, nxsj
      REAL :: aquo, accumf, percentq, qmax, flwareain, flwareaout
!***********************************************************************
!sed  IF ( (Chan_rtemethod == 4 .OR. Chan_rtemethod == 1) .AND. Sed_route == 1 ) THEN
!sed    CALL error_stop ('cannot route sediment with MOC or Muskingum routing', ERROR_param)
!sed  ENDIF

      Chan_sum_init = 0.0
      DO i = 1, Nchan
        nxsj = Nxs(i)
        aquo = Ofar(i)/(nxsj-1)
        accumf = Uprch_area(i)
        DO j = 1, nxsj
          percentq = (accumf*AREACONV/Ofarea_total)*Basin_cfs
          Q_xs(j, i) = percentq
          Qinpast_xs(j, i) = percentq
          A_xs(j, i) = (Alpr1(i)*percentq)**Cmp1(i)
          Chan_sum_init = Chan_sum_init + percentq*Dtsec
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
          flwareain = Q_xs(1, i)*Dts(i)/Dx(i)
          flwareaout = Q_xs(nxsj, i)*Dts(i)/Dx(i)
          CALL MethofCharInit(flwareain, flwareaout, Dx(i), Mocgrids(i), Chan_xmoc(1, i), Chan_amoc(1, i))
        ENDIF
      ENDDO

      END SUBROUTINE rte_chaninit

!***********************************************************************
! initial run processing
!***********************************************************************
      SUBROUTINE rte_run1()
      USE PRMS_KROUT_CHAN, ONLY:FTSQ2ACRE_CONV, LENGTH_CONV, Chanvol_daysum, Dt_sroff, Qday_sum
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_tot
      USE PRMS_BASIN, ONLY: Hru_area
      USE PRMS_SET_TIME, ONLY: Newday, Timestep_seconds
      USE PRMS_FLOWVARS, ONLY: Ssres_flow
      USE PRMS_GWFLOW, ONLY: Gwres_flow
      IMPLICIT NONE
      INTRINSIC :: SNGL
! Local Variables
      REAL :: cfs_conv
!***********************************************************************
      cfs_conv = FTSQ2ACRE_CONV / (LENGTH_CONV*Timestep_seconds)

      IF ( Newday == 1 ) THEN
        Chanvol_daysum = 0.0D0
        Qday_sum = 0.0D0
!sed    Sed_tot = 0.0
      ENDIF

        Ssres_flow = Ssres_flow * Hru_area * cfs_conv
        Gwres_flow = Gwres_flow * Hru_area * cfs_conv

      Dt_sroff = 0.0

      END SUBROUTINE rte_run1

!***********************************************************************
! set computational variables
!***********************************************************************
      SUBROUTINE rte_compvars(Nxsj, Chantype, Dt_dx, Dtdxchi_inv, Alpha, Alpha1, M, M_inv, M_1, M_1abs, Alpdts, &
                              Alpha_m, Alp_mdts, B1, Ndels, Nxs, Chan_type, Dtdx, Alpr, Alpr1, Cmp, Cmp1, Dts, Rtm, Chan_chi)
      USE PRMS_SET_TIME, ONLY: Timestep_minutes
      IMPLICIT NONE
      INTRINSIC :: ABS, NINT
! Arguments
      INTEGER, INTENT(IN) :: Nxs, Chan_type
      INTEGER, INTENT(OUT) :: Nxsj, Chantype, Ndels
      REAL, INTENT(IN) :: Dtdx, Alpr, Alpr1, Cmp, Cmp1, Dts, Rtm
      REAL, INTENT(IN) :: Chan_chi
      REAL, INTENT(OUT) :: Dt_dx, Dtdxchi_inv, Alpha, Alpha1, Alpdts
      REAL, INTENT(OUT) :: Alpha_m, Alp_mdts, M, M_inv, M_1, M_1abs, B1
!***********************************************************************
      Nxsj = Nxs
      Chantype = Chan_type
      Dt_dx = Dtdx
      Dtdxchi_inv = 1.0/(Dt_dx*Chan_chi)
      Alpha = Alpr
      Alpha1 = Alpr1
      M = Cmp
      M_inv = Cmp1
      M_1 = M - 1.0
      M_1abs = ABS(M_1)
      Alpdts = Alpha*Dts
      Alpha_m = Alpha*M
      Alp_mdts = Alpdts*M
      B1 = Alpha_m*Dt_dx
      Ndels = NINT( Timestep_minutes/Rtm )
      END SUBROUTINE rte_compvars

!***********************************************************************
! compute inflow to a channel segment (upstream gains and diversions)
!***********************************************************************
      SUBROUTINE rte_inflow(Jch, Ndels, Upsw, Upchan)
      USE PRMS_KROUT_CHAN, ONLY: Q_gain_id, Q_divert_id, Q_gain, Chan_route_time, Ngain, Ndivert, Q_divert, Qin, Qout
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_route, Sedin, Sedout
      USE PRMS_SET_TIME, ONLY: Timestep_minutes
      IMPLICIT NONE
      INTRINSIC :: NINT
! Arguments
      INTEGER, INTENT(IN) :: Jch, Ndels, Upchan(3), Upsw
! Local Variables
      INTEGER :: k, ndelsup, iup
      REAL :: qinit
!***********************************************************************
! check for gain inflow, allow for more than one gain
      qinit = 0.0
      IF ( Ngain > 0 ) THEN
        DO k = 1, Ngain
          IF ( Q_gain_id(k) == Jch ) qinit = qinit + Q_gain(k)
        ENDDO
      ENDIF
! check for diverstion outflow, allow for more than one diversion
      IF ( Ndivert > 0 ) THEN
        DO k = 1, Ndivert
          IF ( Q_divert_id(k) == Jch ) qinit = qinit - Q_divert(k)
        ENDDO
      ENDIF
!*** Initialize flow arrays for upstream and lateral input
      DO k = 1, Ndels
        Qin(k) = qinit
      ENDDO
!*** Initialize sediment array for upstream and lateral input
!sed  IF ( Sed_route == 1 ) Sedin = 0.0
!*** Determine and accumulate upstream input to channel branch jch ***
      IF ( Upsw == 1 ) THEN
        DO k = 1, 3
          iup = Upchan(k)
          IF ( iup > 0 ) THEN
            ndelsup = NINT( Timestep_minutes/Chan_route_time(iup) )
            CALL AccumQSed(Ndels, ndelsup, Qout(1, iup), Qin)
!sed        CALL AccumQSed(Ndels, ndelsup, Qout(1, iup), Sedout(1, iup), Qin, Sedin)
          ENDIF
        ENDDO
!*** Add in sediment
!sed    IF ( Sed_route == 1 ) THEN
!sed      DO k = 1, Ndels
!sed        IF ( Qin(k) > 0. .OR. Qout(k, Jch) > 0. ) THEN
!sed          Sedin(k) = (Sedin(k)*Qin(k)+Sedout(k, Jch)*Qout(k, Jch)) / (Qin(k)+Qout(k, Jch))
!sed        ELSE
!sed          Sedin(k) = 0.0
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
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_route
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
      REAL, INTENT(IN) :: Qout(Ndels)
      REAL, INTENT(INOUT) :: Qin(Ndels)
!sed  REAL, INTENT(IN) :: Sedout(Ndels)
!sed  REAL, INTENT(INOUT) :: Sedin(Ndels)
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
      REAL, INTENT(IN) :: Dataout(Ndels)
      REAL, INTENT(INOUT) :: Datain(Ndels)
! Local Variables
      INTEGER :: kdels, k, kk, ki
      REAL :: fac
!***********************************************************************
      IF ( Ndelsup > Ndels ) THEN
        kdels = Ndelsup/Ndels
        fac = 1.0/kdels
        ki = 0
        DO k = 1, Ndels
          DO kk = 1, kdels
            ki = ki + 1
            Datain(k) = Datain(k) + Dataout(ki)*fac
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
      USE PRMS_CONSTANTS, ONLY: CLOSEZERO
      USE PRMS_KROUT_CHAN, ONLY: Chan_rtemethod, Ncdels, Nchxs, Chanvol_daysum, Ncmoc, Qin, Qinlat, &
          Chanvol_nchansum, Qp, Ap, Q, A, Outlet_chan, Qinlat_chan
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_route, Sedin, Sed, Sedinlat, Sedp
      IMPLICIT NONE
      INTRINSIC :: SQRT
      EXTERNAL :: KW_loop, Musk_loop, DW_loop, MethofChar, M1_impl_loop
! Arguments
      INTEGER, INTENT(IN) :: Jch, Ndels, Chantype, Nxsj
      INTEGER, INTENT(INOUT) :: Mocgrids
      REAL, INTENT(IN) :: Chan_loss_ftsec, Chan_width
      REAL, INTENT(IN) :: Tc_half_topw, Tc_topw, Wpcoef_a, Wpcoef_b
      REAL, INTENT(INOUT) :: Dts, Qmxa, Qout(Ncdels)
      REAL, INTENT(IN) :: Alpha, Alpha1, Alp_mdts, Alpdts, Alpha_m
      REAL, INTENT(IN) :: M, M_1, M_inv, M_1abs, Thres, Dx, Dt_dx, B1
      REAL, INTENT(IN) :: Chan_parm1, Chan_parm2, Chan_slope
      REAL, INTENT(IN) :: Dtdxchi_inv, Czero, Cone, Ctwo
      REAL, INTENT(OUT) :: Q_chan
!sed  REAL, INTENT(OUT) :: Sed_chan
      REAL, INTENT(INOUT) :: Q_xs(Nchxs), A_xs(Nchxs)
!sed  REAL, INTENT(INOUT) :: Sed_xs(Nchxs), Sedout(Ncdels)
      REAL Qinpast_xs(Nchxs)
      REAL Chan_amoc(Ncmoc), Chan_xmoc(Ncmoc)
! Local Variables
      INTEGER :: k, j
      REAL :: flow_h, chan_loss_ft2, wp, q_lat, a_lat, ain, qdt, qlatdx
!sed  REAL :: sed_l
!***********************************************************************
!*** Initialize H, Q, and Sed arrays***
      DO j = 1, Nxsj
        Q(j) = Q_xs(j)
        A(j) = A_xs(j)
      ENDDO

!sed  IF ( Sed_route == 1 ) THEN
!sed    DO j = 1, Nxsj
!sed      Sed(j) = Sed_xs(j)
!sed      Sedp(j) = 0.0
!sed    ENDDO
!sed  ENDIF

      IF ( Chantype < 3 .OR. Chantype > 9 ) THEN
        chan_loss_ft2 = Chan_loss_ftsec * Chan_width
      ELSE
        chan_loss_ft2 = 0.0
      ENDIF

      DO k = 1, Ndels
        Qp(1) = Qin(k)
        Ap(1) = (Alpha1*Qp(1))**M_inv

        IF ( Chantype == 3 ) THEN
          flow_h = SQRT(Ap(1) / Tc_half_topw)
          chan_loss_ft2 = flow_h * Tc_topw * Chan_loss_ftsec
        ELSEIF ( Chantype == 4 ) THEN
          wp = Wpcoef_a * Ap(1)**Wpcoef_b
          chan_loss_ft2 = wp * Chan_loss_ftsec
        ENDIF

        q_lat = Qinlat(k) - chan_loss_ft2 !rsr, are units correct, ft^2/s?
        a_lat = q_lat*Dts

        IF ( Qmxa > Thres .OR. q_lat > 0.0 .OR. Qp(1) > 0.0 ) THEN
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
            CALL KW_loop(Nxsj, Alpha, Alpha_m, M, M_1, M_1abs, M_inv, B1, a_lat, qlatdx, Dtdxchi_inv, &
                         Dt_dx, Q, Qp, A, Ap, Qmxa)
!sed                     B1, a_lat, qlatdx, sed_l, Dtdxchi_inv, Dts, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa, Sed, Sedp)

! Muskingum
          ELSEIF ( Chan_rtemethod == 1 ) THEN
            CALL Musk_loop(Nxsj, Czero, Cone, Ctwo, Qin(k), qlatdx, Q_xs, Qinpast_xs, Qp, Qmxa)

! Muskingum-Cunge Diffusive Wave
          ELSEIF ( Chan_rtemethod == 3 ) THEN
            CALL DW_loop(Jch, Nxsj, Alpha, M, M_1, M_1abs, M_inv, B1, a_lat, qlatdx, Chantype, Chan_parm1, &
                         Chan_parm2, Chan_slope, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa)
!sed                     Chan_parm2, Chan_slope, Dts, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa, Sed, Sedp, Sed_route)

! Method of Characteristics
          ELSEIF ( Chan_rtemethod == 4 ) THEN
            ain = Qin(k)*Dt_dx
            CALL MethofChar(ain, Alpha, M, M_1, M_1abs, a_lat, q_lat, Dx, Alp_mdts, Alpdts, Chantype, Mocgrids, &
                            Chan_xmoc, Chan_amoc, Qp(Nxsj), Ap(Nxsj), Ncmoc)
            IF ( Qp(Nxsj) > Qmxa ) Qmxa = Qp(Nxsj)

! Kinematic Wave Implicit solution when M = 1
! (no explicit first solution)
          ELSEIF ( M_1abs < CLOSEZERO .AND. Chan_rtemethod /= 2 ) THEN
            CALL M1_impl_loop(Nxsj, Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv, qlatdx, Q, Qp, A, Ap, Qmxa)
!sed                          Dtdxchi_inv, qlatdx, Dts, Dx, Q, Qp, A, Ap, Qmxa, sed_l, Sed, Sedp)
          ENDIF

        ELSE
          DO j = 1, Nxsj
            Qp(j) = 0.0
            Ap(j) = 0.0
          ENDDO
!sed      IF ( Sed_route == 1 ) THEN
!sed        DO j = 1, Nxsj
!sed          Sedp(j) = 0.0
!sed        ENDDO
!sed      ENDIF
          Qmxa = 0.0

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
      USE PRMS_KROUT_CHAN, ONLY: Chan_rtemethod, Chifactor, Chan_chi, Chan_theta
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_route
      IMPLICIT NONE
      EXTERNAL :: ExplFd, IterateFd
!sed  EXTERNAL :: SedrouteChan
! Arguments
      INTEGER Nxsj
      REAL, INTENT(IN) :: Alpha, Alpha_m, A_lat, M, M_1, M_1abs, M_inv
      REAL, INTENT(IN) :: Dtdxchi_inv, Dt_dx, B1
      REAL, INTENT(IN) :: Qlatdx
      REAL, INTENT(IN) :: Q(Nxsj), A(Nxsj)
!sed  REAL, INTENT(IN) :: Sed_l, Sed(Nxsj), Dx, Dts
      REAL, INTENT(INOUT) :: Qmxa, Qp(Nxsj), Ap(Nxsj)
!sed  REAL, INTENT(INOUT) :: Sedp(Nxsj)
! Local Variables
      INTEGER :: j, jm1
      REAL :: theta
!***********************************************************************
      DO j = 2, Nxsj
        jm1 = j - 1
! Begin with explicit solution
        CALL ExplFd(Alpha, M, M_inv, Dt_dx, B1, M_1, A_lat, Qlatdx, &
                    Q(jm1), Qp(jm1), Q(j), A(jm1), Ap(jm1), A(j), Qp(j), Ap(j), theta)
!*** Implicit finite difference method***
        IF ( Chan_rtemethod == 2 .AND. Ap(j) > 0.0 ) THEN
!         IF ( theta < 0.6 .OR. theta > 1.0 ) theta = Chan_theta
          theta = Chan_theta

          CALL IterateFd(Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv, Chan_chi, Chifactor, theta, Qlatdx, Q(jm1), &
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
      REAL, INTENT(IN) :: Czero, Cone, Ctwo, Qin, Qlatdx
      REAL, INTENT(IN) :: Qoutpast_xs(Nxsj)
      REAL, INTENT(INOUT) :: Qmxa, Qinpast_xs(Nxsj)
      REAL, INTENT(OUT) :: Qp(Nxsj)
! Local Variables
      INTEGER j
      REAL qin_current, q
!***********************************************************************
      qin_current = Qin + Qlatdx
      DO j = 1, Nxsj
        q = Czero*qin_current + Cone*Qinpast_xs(j) + Ctwo*Qoutpast_xs(j)
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
      SUBROUTINE DW_loop(Jch, Nxsj, Alpha, M, M_1, M_1abs, M_inv, B1, A_lat, Qlatdx, Chantype, Chan_parm1, &
                         Chan_parm2, Chan_slope, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa)
!sed                     A_lat, Qlatdx, Sed_l, Chantype, Chan_parm1, &
!sed                     Chan_parm2, Chan_slope, Dts, Dt_dx, Dx, Q, Qp, A, Ap, Qmxa, Sed, Sedp, Sed_route)
      IMPLICIT NONE
      EXTERNAL :: ExplFd, MuskDifWave
!sed  EXTERNAL :: SedrouteChan
! Arguments
      INTEGER, INTENT(IN) :: Jch, Nxsj, Chantype
!sed  INTEGER, INTENT(IN) :: Sed_route
      REAL, INTENT(IN) :: Alpha, A_lat, B1, M, M_1, M_1abs, M_inv
      REAL, INTENT(IN) :: Chan_parm1, Chan_parm2, Chan_slope
      REAL, INTENT(IN) :: Dt_dx, Dx, Qlatdx
      REAL, INTENT(IN) :: Q(Nxsj), A(Nxsj)
!sed  REAL, INTENT(IN) :: Sed_l, Sed(Nxsj), Dts
      REAL, INTENT(INOUT) :: Qmxa, Qp(Nxsj), Ap(Nxsj)
!sed  REAL, INTENT(INOUT) :: Sedp(Nxsj)
! Local Variables
      INTEGER :: j, jm1
      REAL :: theta
!***********************************************************************
      DO j = 2, Nxsj
        jm1 = j - 1
! Begin with explicit solution
        CALL ExplFd(Alpha, M, M_inv, Dt_dx, B1, M_1, A_lat, Qlatdx, &
                    Q(jm1), Qp(jm1), Q(j), A(jm1), Ap(jm1), A(j), Qp(j), Ap(j), theta)
!*** Muskingum-Cunge Diffusion Wave Method
        CALL MuskDifWave(Jch, j, Chantype, Chan_parm1, Chan_parm2, Alpha, M, M_inv, M_1, M_1abs, Dx, Dt_dx, &
                         Chan_slope, Qlatdx, Q(jm1), Qp(jm1), Q(j), A(jm1), Ap(jm1), A(j), Qp(j), Ap(j))
        IF ( Qp(j) > Qmxa ) Qmxa = Qp(j)
!sed    IF ( Sed_route == 1 ) CALL SedrouteChan(A(j), Ap(j), Qp(jm1), Qp(j), Dts, Dx, Sed(j), Sedp(jm1), Sed_l, Sedp(j))
      ENDDO
      END SUBROUTINE DW_loop

!***********************************************************************
! only called when M = 1.0 and Chan_rtemethod=2 (Implicit)
!***********************************************************************
      SUBROUTINE M1_impl_loop(Nxsj, Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv, Qlatdx, Q, Qp, A, Ap, Qmxa)
!sed                          Dtdxchi_inv, Qlatdx, Dts, Dx, Q, Qp, A, Ap, Qmxa, Sed_l, Sed, Sedp)
      USE PRMS_KROUT_CHAN, ONLY: Chifactor, Chan_chi, Chan_theta
!sed  USE PRMS_KROUT_CHAN, ONLY: Sed_route
      IMPLICIT NONE
      EXTERNAL :: IterateFd
!sed  EXTERNAL :: SedrouteChan
! Arguments
      INTEGER, INTENT(IN) :: Nxsj
      REAL, INTENT(IN) :: Alpha, M, Alpha_m, M_1, M_1abs
      REAL, INTENT(IN) :: Dtdxchi_inv, Qlatdx
      REAL, INTENT(IN) :: Q(Nxsj), A(Nxsj)
!sed  REAL, INTENT(IN) :: Sed_l, Sed(Nxsj), Dts, Dx
      REAL, INTENT(INOUT) :: Qmxa, Qp(Nxsj), Ap(Nxsj)
!sed  REAL, INTENT(INOUT) :: Sedp(Nxsj)
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
      USE PRMS_KROUT_CHAN, ONLY: Nxs, Nchan, Q_xs, Dtsec
      IMPLICIT NONE
! Arguments
      REAL, INTENT(OUT) :: Chanvol_sum
! Local Variables
      INTEGER :: i, j
!***********************************************************************
      Chanvol_sum = 0.0
      DO i = 1, Nchan
        DO j = 1, Nxs(i)
          Chanvol_sum = Chanvol_sum + Q_xs(j, i)
        ENDDO
      ENDDO
      Chanvol_sum = Chanvol_sum*Dtsec
      END SUBROUTINE rte_flowleft

!***********************************************************************
!     Route flow through reservoirs
!***********************************************************************
      SUBROUTINE reservoir(Ires, Ndels, Chan_type, Rtm, Qout, Wv5, C5, &
                           S5, Wv15, C15, S15, Nsos, Lake_coef, Up1, Up2, Up3, Lake_sto, Din1)
      USE PRMS_CONSTANTS, ONLY: CLOSEZERO
      USE PRMS_KROUT_CHAN, ONLY: Lake_outq, Mxnsos, Qin
      IMPLICIT NONE
      INTRINSIC :: EXP, ABS
! Arguments
      INTEGER, INTENT(IN) :: Ires, Ndels, Chan_type, Nsos, Up1, Up2, Up3
      REAL, INTENT(IN) :: Rtm, Lake_coef
      REAL, INTENT(IN) :: Wv5(Mxnsos), C5(Mxnsos), S5(Mxnsos)
      REAL, INTENT(IN) :: Wv15(Mxnsos), C15(Mxnsos), S15(Mxnsos)
      REAL, INTENT(INOUT) :: Lake_sto, Din1
      REAL, INTENT(OUT) :: Qout(Ndels)
! Local Variables
      INTEGER :: jj, j, n
      REAL :: in1, in2, total, avin, s2o2, qr
      REAL :: dtr, dtr1, q2, xkt, dtr12, c2, dtnd1
!***********************************************************************
      dtr = 1440.0 / Rtm
      dtr1 = 1.0 / dtr
      dtnd1 = 1.0 / Ndels
      total = 0.0

!rsr, need???
!   to add code to get surface runoff, interflow and gwflow to reservoir

      !rsr, added qr, not sure if it is correct
      qr = 0.0
      IF ( Up1 > 0 ) qr = Lake_outq(Up1)
      IF ( Up2 > 0 ) qr = qr + Lake_outq(Up2)
      IF ( Up3 > 0 ) qr = qr + Lake_outq(Up3)

      in2 = Din1 + qr

      IF ( Chan_type == 8 ) THEN

! Compute outflow using Puls routing method
        DO j = 1, Ndels
          s2o2 = Lake_sto * dtr - Lake_outq(Ires) * 0.5
          IF ( s2o2 < 0. ) s2o2 = 0.
          in1 = in2
          in2 = Qin(j)
          total = total + in2
          avin = (in1+in2) * 0.5
          s2o2 = s2o2 + avin

          n = Nsos
          IF ( ABS(Rtm-5.) < CLOSEZERO ) THEN
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

          IF ( q2 < 0.0 ) q2 = 0.0
          s2o2 = s2o2 - q2        !???rsr, not in strmflow module
          IF ( s2o2 < 0.0 ) THEN
            q2 = s2o2 + q2 * 0.5
            s2o2 = 0.0
          ENDIF
          Lake_sto = (s2o2-q2*0.5) * dtr1
          IF ( Lake_sto < 0.0 ) THEN
            q2 = s2o2 + Lake_outq(Ires) * 0.5
            Lake_sto = 0.0
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
          IF ( q2 < 0.0 ) q2 = 0.0
          Lake_sto = Lake_sto + avin - q2
          Qout(j) = q2*dtr
        ENDDO
        Lake_outq(Ires) = q2*dtr
      ENDIF

      Din1 = total*dtnd1

      END SUBROUTINE reservoir

!***********************************************************************
!     Computes the parameters alpha and rm for a given channel
!***********************************************************************
      SUBROUTINE AlphaRm(Ichtype, Rough, Slope, Parm1, Parm2, Alpha, Rm)
      USE PRMS_KROUT_CHAN, ONLY: TWOTHIRDS, FOURTHIRDS, FIVETHIRDS
      IMPLICIT NONE
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
      REAL, INTENT(IN) :: Rough, Slope, Parm1, Parm2
      REAL, INTENT(OUT) :: Alpha, Rm
! Local Variables
      REAL :: side, sqrtslope, turbulent_fac
!***********************************************************************
      sqrtslope = SQRT(Slope)
      turbulent_fac = (1.486*sqrtslope)/Rough

! Rectangular open channel
!    parm1=channel width
      IF ( Ichtype==1 ) THEN
!       the following holds for width approximates hydraulic radius
!       meaning depth is very small compared to width.
        Alpha = turbulent_fac/(Parm1**TWOTHIRDS)
        Rm = FIVETHIRDS

! Triangular channel,
!    parm1=width from left bank to center at 1 ft depth
!    parm2=width from right bank to center at 1 ft depth
      ELSEIF ( Ichtype==3 ) THEN
        side = SQRT(Parm1+Parm2)/(SQRT(1.0+Parm1**2.0)+SQRT(1.0+Parm2**2.0))
        Alpha = ((1.18*sqrtslope)/Rough)*side**TWOTHIRDS
        Rm = FOURTHIRDS

! User specified alpha and rm
      ELSEIF ( Ichtype==4 ) THEN
        Alpha = Parm1
        Rm = Parm2

! Circular Pipe segment, parm1 = pipe diameter
      ELSEIF ( Ichtype==2 ) THEN
        Alpha = turbulent_fac*(Parm1/4.0)**TWOTHIRDS
        Rm = 1.0


! Junction or reservoir (Ichtype, 7, 8, or 9)
      ELSEIF ( Ichtype>6 .AND. Ichtype<10 ) THEN
        Alpha = 0.0
        Rm = 0.0

! Gutter, parm1 = gutter cross slope (ft horizontal/ft vertical)
      ELSEIF ( Ichtype==10 ) THEN
        side = SQRT(Parm1)/(1.0+SQRT(1.0+Parm1**2.0))
        Alpha = 1.18*sqrtslope/Rough*side**TWOTHIRDS
        Rm = FOURTHIRDS

! Open channel adjusted at each time step
! assumes Chan_type is 1 and Wpcoef_a and Wpcoef_b are defined
!    Parm1 = a1, Parm2 = b1
!            a1 and b1 are based on WP = a1*A**b1
      ELSEIF ( Ichtype==11 ) THEN
        Alpha = turbulent_fac/(Parm1**TWOTHIRDS)
!       Rm = (5.0-2.0*Parm2)/3.0
        Rm = FIVETHIRDS - TWOTHIRDS*Parm2

!     ELSEIF ( Ichtype<0 .OR. Ichtype>11 ) THEN
      ELSE
        PRINT *, 'Error in ALPHA_RM, invalid channel type', Ichtype
        STOP
      ENDIF

      END SUBROUTINE AlphaRm

!***********************************************************************
!     Perform sediment routing
!***********************************************************************
      SUBROUTINE SedrouteChan(Aj, Apj, Qpi, Qpj, Dt, Dx, Sedj, Sedpi, Sedl, Sedpj)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Aj, Apj, Qpi, Qpj, Dt, Dx, Sedj, Sedpi, Sedl
      REAL, INTENT(OUT) :: Sedpj
! Local Variables
      REAL :: cadt, cqdx, adtqdx
!***********************************************************************
      IF ( Apj<0.0 ) THEN
        Sedpj = 0.0
      ELSE
        cadt = Sedj*Aj/Dt
        cqdx = Sedpi*Qpi/Dx
        adtqdx = (Apj/Dt) + (Qpj/Dx)
        Sedpj = (Sedl+cadt+cqdx)/adtqdx
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
      USE PRMS_KROUT_CHAN, ONLY: Czero, Cone, Ctwo, Musk_wghtfac, Musk_travel_time, Chan_route_time
      USE PRMS_MODULE, ONLY: Nsegment
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
      REAL :: dt, tt, wt, wtt, dd
!***********************************************************************
! Chan_route_time in minutes, dt in hours
      DO i = 1, Nsegment
        dt = 0.5*Chan_route_time(i)/60.0
        tt = Musk_travel_time(i)
        wt = Musk_wghtfac(i)
        wtt = wt*tt
        dd = 1.0/(tt-wtt+dt)
        Czero(i) = (-wtt+dt)*dd
        Cone(i) = (wtt+dt)*dd
        Ctwo(i) = (tt-wtt-dt)*dd
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
        IF ( Ctwo(i)<=0.0 ) THEN
          Cone(i) = Cone(i) + Ctwo(i)
          Ctwo(i) = 0.0
        ENDIF
! LONG travel time
        IF ( Czero(i)<=0.0 ) THEN
          Cone(i) = Cone(i) + Czero(i)
          Czero(i) = 0.0
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
      USE PRMS_CONSTANTS, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments Definitions
!     A = flow areas
!     Q = discharge
!     Dtdx = dt/dx
!     B1 = Alpha*M*Dtdx
!     M_1 = M - 1.0
      REAL, INTENT(IN) :: Alpha, M, Minv, Dtdx, B1, M_1, Alat, Qldx
      REAL, INTENT(IN) :: Qi, Qpi, Qj, Ai, Api, Aj
      REAL, INTENT(OUT) :: Qpj, Apj, Theta
!***********************************************************************
      IF ( Ai>0.0 .OR. Qldx>0.0 ) THEN
! Compute theta, a computed stability parameter
! if Q = 0, theta = Alpha*M*(hi**(M-1)*dt/dx or dt/dx*M*qi/hi
        IF ( Qldx<NEARZERO ) THEN
          Theta = B1*Ai**M_1
        ELSE
          Theta = Alpha/Qldx*((Alat+Ai)**M-Ai**M)
        ENDIF

        IF ( Theta>1.0-NEARZERO ) THEN
          Qpj = Qpi + Qldx - (Api-Ai)/Dtdx
          IF ( Qpj<NEARZERO ) THEN
            Qpj = 0.0
            Apj = 0.0
          ELSE
            Apj = (Qpj/Alpha)**Minv
          ENDIF
          RETURN
        ENDIF
      ENDIF

      Apj = Aj + Alat + Dtdx*(Qi-Qj)
      IF ( Apj<NEARZERO ) THEN
        Apj = 0.0
        Qpj = 0.0
      ELSE
        Qpj = Alpha*(Apj**M)
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
      USE PRMS_CONSTANTS, ONLY: NEARZERO
      IMPLICIT NONE
      INTRINSIC :: ABS
      INTEGER, PARAMETER :: MAX_ITER = 30
! Arguments
      INTEGER, INTENT(IN) :: Modnum
      REAL, INTENT(IN) :: Alpha, M, Alpha_m, M_1, M_1abs, Dtdxchi_inv
      REAL, INTENT(IN) :: Chi, Chifact, Theta, Qldx, Qi, Qpi, Qj, Ai, Api, Aj
      REAL, INTENT(INOUT) :: Qpj, Apj
! Local Variables
      INTEGER :: iter, cnvg
      REAL :: x2, x3, a4, a7, c2, fx, fpx, fppx, h, x, soln
!***********************************************************************
!  begin with explicit solution for flow area
      x2 = Theta*Dtdxchi_inv
      x3 = (1.0-Theta)*Dtdxchi_inv
      a4 = M - 2.0
      a7 = Alpha_m*M_1

      c2 = -Qpi + Chifact*(Qj-Qi) - x2*Aj + x3*(Api-Ai) - Qldx/Chi

      iter = 0
      IF ( M_1abs<NEARZERO ) THEN
        soln = -c2/(Alpha+x2)
      ELSE
        soln = Apj
        cnvg = 0
        DO WHILE ( iter<MAX_ITER .AND. cnvg==0 )
          iter = iter + 1
          fx = Alpha*soln**M + x2*soln + c2
          IF ( ABS(fx)<NEARZERO ) THEN
            cnvg = 1
          ELSE
            fpx = Alpha_m*soln**M_1 + x2
            IF ( ABS(fpx)<NEARZERO ) STOP 'in IterateFd'
            fppx = a7*soln**a4
            h = -fpx/fx + 0.5*fppx/fpx
            x = soln + 1.0/h
            IF ( x<NEARZERO ) THEN
              cnvg = 1
            ELSE
              IF ( ABS(x-soln)/soln<CONVRG ) cnvg = 1
              soln = x
            ENDIF
          ENDIF
        ENDDO
! solution did not converge, use last solution
        IF ( cnvg==0 ) THEN
          PRINT *, 'iterations exceeded in IterateFd'
          PRINT *, Aj, soln, Ai, Qi, Api, Qpi, Qpj, Apj, Modnum
          STOP 'iterations exceeded in IterateFd'
        ENDIF
      ENDIF

      Apj = soln
      Qpj = Alpha*Apj**M
      IF ( iter>15 ) PRINT *, 'iterations in IterateFd=', iter

      END SUBROUTINE IterateFd

!***********************************************************************
!     Solves for the unknown flow discharge Qpj by the variable-
!     parameter Muskingum-Cunge diffusion wave method.
!     (see ASCE J. of Hydr. Div., Vol. 104, no. HY12, December 1978.)
!***********************************************************************
      SUBROUTINE MuskDifWave(Ibrnch, Iseg, Jtype, Param1, Param2, Alpha, &
     &                       M, Minv, M_1, M_1abs, Dx, Dtdx, Slope, &
     &                       Qlatdx, Qi, Qpi, Qj, Ai, Api, Aj, Qpj, Apj)
      USE PRMS_KROUT_CHAN, ONLY: CONVRG
      USE PRMS_CONSTANTS, ONLY: NEARZERO, CLOSEZERO
      IMPLICIT NONE
      INTRINSIC ABS
      REAL, EXTERNAL :: Tpwd
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
      REAL, INTENT(IN) :: Param1, Param2, Alpha, M, Minv, M_1, M_1abs, Dx
      REAL, INTENT(IN) :: Dtdx, Slope, Qlatdx, Qi, Qpi, Qj, Ai, Api, Aj
      REAL, INTENT(INOUT) :: Qpj, Apj
! Local Variables
      INTEGER :: iter, cnvg
      REAL :: alpm, q0, a0, celx, qx, cel, qr, c, r
      REAL :: cc, c0, c1, c2, c3, q, dtnew, slpdx
! Save Varialbes
      INTEGER, SAVE :: k1
      DATA k1/0/
!***********************************************************************
      alpm = Alpha*M
!
!     .....assign first guess at  XQ(J)  and  XA(J) from KW soln.....
      q0 = Qpj
      a0 = Apj
!
!     .....compute sum of wave celerity's and unit-width discharges
!          at the three known nodal points in the computational box.....
      cel = Alpha
      IF ( M_1abs>NEARZERO ) THEN
        celx = alpm*(Ai**M_1+Aj**M_1+Api**M_1)
        qx = Qi/Tpwd(Jtype, Param1, Param2, Ai) + Qj/Tpwd(Jtype, Param1, Param2, Aj) &
     &       + Qpi/Tpwd(Jtype, Param1, Param2, Api)
      ELSE
!       special case of pipe segment when M=1.0
!        cel = Alpha
        qx = (Qi+Qj+Qpi)/Tpwd(Jtype, Param1, Param2, a0)
      ENDIF
!
      slpdx = Slope*Dx
!
!     .....begin iteration.....
      cnvg = 0
      iter = 0
!
      DO WHILE ( iter<MAX_ITER .AND. cnvg==0 )
        iter = iter + 1
!
!         .....compute 4-pt average wave celerity.....
        IF ( M_1abs>NEARZERO ) cel = (celx+(alpm*(a0**M_1)))*0.25
!
!       .....compute 4-pt average unit-width discharge.....
        qr = (qx+q0/Tpwd(Jtype, Param1, Param2, a0))*0.25

!       .....compute  C=Courant number  and  R=cell Reynolds number.....
        c = cel*Dtdx
        r = qr/(slpdx*cel)
!
!       .....compute routing coefficients.....
        cc = 1.0/(1.0+c+r)
        c0 = cc*(-1.0+c+r)
        c1 = cc*(1.0+c-r)
        c2 = cc*(1.0-c+r)
        c3 = 2.0*cc*c
!
!       .....compute next estimate of Q.....
!rsr ?? qlat*dx
        q = c0*Qpi + c1*Qi + c2*Qj + c3*Qlatdx

        IF ( q<CLOSEZERO ) THEN
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
      IF ( c>20.0 .AND. Ibrnch/=k1 ) THEN
!       .....courant number exceeds 20.0.....
        k1 = Ibrnch
        dtnew = 10.0*Dx/cel
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
      REAL FUNCTION Tpwd(Jtype, Param1, Param2, A)
      USE PRMS_CONSTANTS, ONLY: NEARZERO, ERROR_param
      IMPLICIT NONE
      REAL, PARAMETER :: PI_4 = ACOS(-1.0) / 4.0
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
      REAL, INTENT(IN) :: Param1, Param2, A
! Local Variables
      REAL :: depth, z, tw
!***********************************************************************
      tw = 1.0E-5
!
! rectangler cross section segment
!rsr, Param1 is set to channel width, should be hydraulic radius
!rsr, tw is not computed for each time step, need to change
      IF ( Jtype==1 ) THEN
        tw = Param1
! circular pipe segment
!     tw = pi * diameter / 4
      ELSEIF ( Jtype==2 ) THEN
        tw = PI_4 * Param1
! triangular segment
      ELSEIF ( Jtype==3 ) THEN
        IF ( A>NEARZERO ) THEN
          z = Param1 + Param2
          depth = (1.4142/SQRT(z)) * SQRT(A)
          tw = z*depth*0.5
        ENDIF
! user-specified, Parm1&2 = Wpcoef_a & Wpcoef_b
      ELSEIF ( Jtype==4 ) THEN
        IF ( A>NEARZERO ) tw = Param1 * (A**Param2)
! overland-flow segment: turbulent(5); laminar(6)
!     set to 1.0 as segment has unit width
      ELSEIF ( Jtype==5 .OR. Jtype==6 ) THEN
        tw = 1.0
! gutter segment
      ELSEIF ( Jtype==10 ) THEN
        IF ( A>NEARZERO ) THEN
          z = Param1
          depth = (1.4142/SQRT(z)) * SQRT(A)
          tw = z * depth * 0.5
        ENDIF
! arbitrary XS, Param1&2 = Wpcoef_a & Wpcoef_b, set tw to wp
      ELSEIF ( Jtype==11 ) THEN
        IF ( A>NEARZERO ) tw = Param1 * (A**Param2)
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
      IMPLICIT NONE
! Argument Definitions
!     Ain    - flow area at upstream end of channel
!     Aout   - flow area at downstream end of channel
!     Dx     - Flow length
!     Ngrids - Number of segments in channel or overland flow plane
!     Xmoc   - X characteristic (feet)
!     Amoc   - Area characteristic (ft^2)
      INTEGER, INTENT(IN) :: Ngrids
      REAL, INTENT(IN) :: Ain, Aout, Dx
      REAL, INTENT(OUT) :: Xmoc(Ngrids), Amoc(Ngrids)
! Functions
      INTRINSIC :: FLOAT
! Local Variables
      INTEGER :: i, n
      REAL :: dxn, dan, nr
!***********************************************************************
!     ..... DEFINE INITIAL CONDITIONS FOR STORM .....

      Xmoc(1) = 0.0
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
      USE PRMS_CONSTANTS, ONLY: NEARZERO
      USE PRMS_KROUT_CHAN, ONLY: Ncmoc
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
      REAL, INTENT(IN) :: Ain, Alpha, M, M_1, M_1abs, Alat, Qlat, Alpmdts, Alpdts, Dx
      REAL, INTENT(INOUT) :: Xmoc(*), Amoc(*)
      REAL, INTENT(OUT) :: Qpj, Apj
! Local Variables
      INTEGER :: i, k, j, n, imj, ii
!***********************************************************************
!     CHECK THAT ARRAY DIMENSIONS WILL NOT BE EXCEEDED
      IF ( Ngrids>=Ncmoc ) CALL mocdimen(Ngrids, Xmoc, Amoc)
!     IF QLAT AND Amoc(1) = 0. DON'T ADD CHARACTERISTIC
      j = 1
      IF ( Qlat<NEARZERO .AND. ABS(Amoc(1))<NEARZERO ) j = 0
      ii = Ngrids
      Ngrids = Ngrids - (1-j)
!
!     ..... ADVANCE CHARACTERISTICS .....
      n = Ngrids + 2
      DO k = 1, Ngrids
        i = n - k
        imj = i - j
        IF ( M_1abs<NEARZERO ) THEN
          Xmoc(i) = Xmoc(imj) + Alpdts
          Amoc(i) = Amoc(imj) + Alat
        ELSEIF ( Alat*1000.0>Amoc(imj) ) THEN
          Xmoc(i) = Xmoc(imj) + Alpha/Qlat*((Alat+Amoc(imj))**M-Amoc(imj)**M)
          Amoc(i) = Amoc(imj) + Alat
        ELSE
          Xmoc(i) = Xmoc(imj) + Alpmdts*Amoc(imj)**M_1
          Amoc(i) = Amoc(imj)
        ENDIF
!
!     ..... KEEP TRACK OF LAST CHARACTERISTIC THAT LEAVES SEGMENT
        IF ( Xmoc(i)>=Dx ) ii = i
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
        IF ( M_1abs>NEARZERO ) THEN
!     ..... TAKE CARE OF SHOCKS .....
          i = Ngrids - 1
          DO WHILE ( i>2 )
            IF ( Xmoc(i)<=Xmoc(i-1) ) THEN
              k = i - 1
              IF ( ABS(Amoc(k))>NEARZERO ) THEN
                Xmoc(k) = (Xmoc(i)+Xmoc(k)) * 0.5
                Amoc(k) = (Amoc(i)+Amoc(k)) * 0.5
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
      REAL, INTENT(INOUT) :: Xmoc(Ngrids), Amoc(Ngrids)
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
      Ngrids = Ngrids/2 + 1
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
      IF ( Dimen>Maxdim .OR. Dimen<1 ) THEN
        PRINT *, ' ***Dimension error: ', Dimname, ' valid range is 1 to', Maxdim, ' specified:', Dimen
        STOP ERROR_dim
      ENDIF
      END SUBROUTINE Dimchk
