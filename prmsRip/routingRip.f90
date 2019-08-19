!***********************************************************************
! Defines stream and lake routing parameters and variables
!***********************************************************************
      MODULE PRMS_ROUTING
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=7), SAVE :: MODNAME
      DOUBLE PRECISION, SAVE :: Cfs2acft
      DOUBLE PRECISION, SAVE :: Segment_area
      INTEGER, SAVE :: Use_transfer_segment, Noarea_flag, Hru_seg_cascades
      INTEGER, SAVE, ALLOCATABLE :: Segment_order(:), Segment_up(:), Seg_hru_num(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_hruarea(:)
      CHARACTER(LEN=80), SAVE :: Version_routing
      !CHARACTER(LEN=32), SAVE :: Outfmt
      INTEGER, SAVE, ALLOCATABLE :: Ts_i(:)
      REAL, SAVE, ALLOCATABLE :: Ts(:), C0(:), C1(:), C2(:)
      REAL, SAVE, ALLOCATABLE :: Ripst_area_max(:), Ripst_area(:), Ripst_depth(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ripst_vol_max(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_segment_storage
      DOUBLE PRECISION, SAVE :: Flow_to_lakes, Flow_to_ocean, Flow_to_great_lakes, Flow_out_region
      DOUBLE PRECISION, SAVE :: Flow_in_region, Flow_in_nation, Flow_headwater, Flow_out_NHM
      DOUBLE PRECISION, SAVE :: Flow_in_great_lakes, Flow_replacement, Flow_terminus
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seginc_ssflow(:), Seginc_sroff(:), Segment_delta_flow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seginc_gwflow(:), Seginc_swrad(:), Seginc_potet(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_outflow(:), Seg_ssflow(:), Seg_sroff(:), Seg_gwflow(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Segment_type(:), Tosegment(:), Hru_segment(:), Obsin_segment(:), Obsout_segment(:)
      REAL, SAVE, ALLOCATABLE :: Seg_depth(:), K_coef(:), X_coef(:), Mann_n(:), Seg_width(:), Segment_flow_init(:)
      REAL, SAVE, ALLOCATABLE :: Seg_length(:), Seg_slope(:)
!   Declared Parameters for Overbank and bank Storage
      REAL, SAVE, ALLOCATABLE :: Transmiss_seg(:), Ripst_areafr_max(:)
!   Declared Parameters for Overbank Storage
      REAL, SAVE, ALLOCATABLE :: Tr_ratio(:), Porosity_seg(:), Ripst_et_coef(:), Ripst_frac_init(:)
!   Declared Variables for Overbank Storage
      DOUBLE PRECISION, SAVE :: Basin_ripst_evap, Basin_ripst_contrib, Basin_ripst_vol, Basin_ripst_area
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ripst_stor_hru(:), Ripst_vol(:), Seg_ripflow(:)
      REAL, SAVE, ALLOCATABLE :: Ripst_evap_hru(:), Ripst_frac(:)
!   Declared Parameters for Bank Storage
      REAL, SAVE, ALLOCATABLE :: Specyield_seg(:), Bankst_head_init(:)
      INTEGER, SAVE, ALLOCATABLE :: Bankfinite_hru(:)
!   Declared Variables for Bank Storage
      DOUBLE PRECISION, SAVE :: Basin_bankst_head, Basin_bankst_seep_rate
      DOUBLE PRECISION, SAVE :: Basin_bankst_seep, Basin_bankst_vol, Basin_bankst_area
      REAL, SAVE, ALLOCATABLE :: Bankst_head(:), Bankst_seep_rate(:), Bankst_seep_hru(:)
      REAL, SAVE, ALLOCATABLE :: Bankst_stor_hru(:), Bankst_head_pts(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Stage_ante(:), Stage_ts(:), Seg_bankflow(:)
      END MODULE PRMS_ROUTING

!***********************************************************************
!     Main routing routine
!***********************************************************************
      INTEGER FUNCTION routing()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: routingdecl, routinginit, route_run
      EXTERNAL :: routing_restart
!***********************************************************************
      routing = 0

      IF ( Process(:3)=='run' ) THEN
        routing = route_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        routing = routingdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file>0 ) CALL routing_restart(1)
        routing = routinginit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL routing_restart(0)
      ENDIF

      END FUNCTION routing

!***********************************************************************
!     routingdecl - set up parameters
!***********************************************************************
      INTEGER FUNCTION routingdecl()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Model, Strmflow_flag, Cascade_flag, &
     &    Ripst_flag, Stream_temp_flag, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
!***********************************************************************
      routingdecl = 0

      Version_routing = 'routing.f90 2019-06-05 17:22:00Z'
      CALL print_module(Version_routing, 'Routing Initialization      ', 90)
      MODNAME = 'routing'

! Declared Variables
      ALLOCATE ( Hru_outflow(Nhru) )
      IF ( declvar(MODNAME, 'hru_outflow', 'nhru', Nhru, 'double', &
     &     'Total flow leaving each HRU', &
     &     'cfs', Hru_outflow)/=0 ) CALL read_error(3, 'hru_outflow')

      IF ( declvar(MODNAME, 'flow_to_lakes', 'one', 1, 'double', &
     &     'Total flow to lakes (segment_type=2)', &
     &     'cfs', Flow_to_lakes)/=0 ) CALL read_error(3, 'flow_to_lakes')

      IF ( declvar(MODNAME, 'flow_terminus', 'one', 1, 'double', &
     &     'Total flow to terminus segments (segment_type=9)', &
     &     'cfs', Flow_terminus)/=0 ) CALL read_error(3, 'flow_terminus')

      IF ( declvar(MODNAME, 'flow_to_ocean', 'one', 1, 'double', &
     &     'Total flow to oceans (segment_type=8)', &
     &     'cfs', Flow_to_ocean)/=0 ) CALL read_error(3, 'flow_to_ocean')

      IF ( declvar(MODNAME, 'flow_to_great_lakes', 'one', 1, 'double', &
     &     'Total flow to Great Lakes (segment_type=11)', &
     &     'cfs', Flow_to_great_lakes)/=0 ) CALL read_error(3, 'Flow_to_great_lakes')

      IF ( declvar(MODNAME, 'flow_out_region', 'one', 1, 'double', &
     &     'Total flow out of region (segment_type=7)', &
     &     'cfs', Flow_out_region)/=0 ) CALL read_error(3, 'flow_out_region')

      IF ( declvar(MODNAME, 'flow_out_NHM', 'one', 1, 'double', &
     &     'Total flow out of model domain to Mexico or Canada (segment_type=5)', &
     &     'cfs', Flow_out_NHM)/=0 ) CALL read_error(3, 'flow_out_NHM')

      IF ( declvar(MODNAME, 'flow_in_region', 'one', 1, 'double', &
     &     'Total flow into region (segment_type=6)', &
     &     'cfs', Flow_in_region)/=0 ) CALL read_error(3, 'flow_in_region')

      IF ( declvar(MODNAME, 'flow_in_nation', 'one', 1, 'double', &
     &     'Total flow into model domain from Mexico or Canada (segment_type=4)', &
     &     'cfs', Flow_in_nation)/=0 ) CALL read_error(3, 'flow_in_nation')

      IF ( declvar(MODNAME, 'flow_headwater', 'one', 1, 'double', &
     &     'Total flow out of headwater segments (segment_type=1)', &
     &     'cfs', Flow_headwater)/=0 ) CALL read_error(3, 'flow_headwater')

      IF ( declvar(MODNAME, 'flow_in_great_lakes', 'one', 1, 'double', &
     &     'Total flow out into model domain from Great Lakes (segment_type=10)', &
     &     'cfs', Flow_in_great_lakes)/=0 ) CALL read_error(3, 'flow_in_great_lakes')

      IF ( declvar(MODNAME, 'flow_replacement', 'one', 1, 'double', &
     &     'Total flow out from replacement flow (segment_type=3)', &
     &     'cfs', Flow_replacement)/=0 ) CALL read_error(3, 'flow_replacement')

      ! 0 = normal; 1 = headwater; 2 = lake; 3 = replacement flow; 4 = inbound to nation;
      ! 5 = outbound from nation; 6 = inbound to region; 7 = outbound from region;
      ! 8 = drains to ocean; 9 = sink (terminus to soil); 10 = inbound from Great Lakes;
      ! 11 = outbound to Great Lakes; 12 = ephemeral; + 100 user updated; 1000 user virtual segment
      ! 100 = user normal; 101 - 108 = not used; 109 sink (tosegment used by Lumen)

      IF ( Ripst_flag==1 .OR. Model==99 ) THEN
! Overbank storage variables
        IF ( declvar(MODNAME, 'basin_ripst_evap', 'one', 1, 'double', &
     &       'Basin area-weighted average evaporation from riparian overbank flow storage', &
     &       'inches', Basin_ripst_evap)/=0 ) CALL read_error(3, 'basin_ripst_evap')

        IF ( declvar(MODNAME, 'basin_ripst_contrib', 'one', 1, 'double', &
     &       'Basin area-weighted average contribution from riparian overbank flow storage into stream', &
     &       'inches', Basin_ripst_contrib)/=0 ) CALL read_error(3, 'basin_ripst_contrib')

        IF ( declvar(MODNAME, 'basin_ripst_vol', 'one', 1, 'double', &
     &       'Basin area-weighted average storage volume in riparian overbank flow storage', &
     &       'inches', Basin_ripst_vol)/=0 ) CALL read_error(3, 'basin_ripst_vol')

        IF ( declvar(MODNAME, 'basin_ripst_area', 'one', 1, 'double', &
     &       'Basin area of riparian overbank flow storage', &
     &       'acres', Basin_ripst_area)/=0 ) CALL read_error(3, 'basin_ripst_area')

        ALLOCATE ( Seg_ripflow(Nsegment) )
        IF ( declvar(MODNAME, 'seg_ripflow', 'nsegment', Nsegment, 'double', &
     &       'Riparian area contribution to streamflow, negative if steam goes overbank', &
     &       'cfs', Seg_ripflow)/=0 ) CALL read_error(3, 'seg_ripflow')

        ALLOCATE ( Ripst_stor_hru(Nhru) )
        IF ( declvar(MODNAME, 'ripst_stor_hru', 'nhru', Nhru, 'double', &
     &       'Riparian overbank flow storage for each HRU', &
     &       'inches', Ripst_stor_hru)/=0 ) CALL read_error(3, 'ripst_stor_hru')

        ALLOCATE ( Ripst_evap_hru(Nhru) )
        IF ( declvar(MODNAME, 'ripst_evap_hru', 'nhru', Nhru, 'real', &
     &       'Evaporation from riparian overbank flow storage for each HRU', &
     &       'inches', Ripst_evap_hru)/=0 ) CALL read_error(3, 'ripst_evap_hru')

        ALLOCATE ( Ripst_vol(Nhru) )
        IF ( declvar(MODNAME, 'ripst_vol', 'nhru', Nhru, 'double', &
     &       'Volume in riparian overbank flow storage for each HRU', &
     &       'acre-inches', Ripst_vol)/=0 ) CALL read_error(3, 'ripst_vol')

        ALLOCATE ( Ripst_frac(Nhru) )
        IF ( declvar(MODNAME, 'ripst_frac', 'nhru', Nhru, 'real', &
     &      'Volume and area fraction of riparian overbank flow storage of the maximum storage for each HRU', &
     &      'decimal fraction', Ripst_frac)/=0 ) CALL read_error(3, 'ripst_frac')

        ALLOCATE ( Ripst_vol_max(Nhru), Ripst_area(Nhru), Ripst_area_max(Nhru), Ripst_depth(Nhru) )
        ALLOCATE ( Seg_hru_num(Nsegment) )

! Bank storage variables
        IF ( declvar(MODNAME, 'basin_bankst_head', 'one', 1, 'double', &
     &      'Basin bank storage area only area-weighted average head of bank storage above groundwater head', &
     &      'meters', Basin_bankst_head)/=0 ) CALL read_error(3, 'basin_bankst_head')

        IF ( declvar(MODNAME, 'basin_bankst_seep', 'one', 1, 'double', &
     &      'Basin area-weighted average seepage from bank storage to streams, negative is out of stream', &
     &      'inches', Basin_bankst_seep)/=0 ) CALL read_error(3, 'basin_bankst_seep')

        IF ( declvar(MODNAME, 'basin_bankst_vol', 'one', 1, 'double', &
     &      'Basin area-weighted average bank storage', &
     &      'inches', Basin_bankst_vol)/=0 ) CALL read_error(3, 'basin_bankst_vol')

        IF ( declvar(MODNAME, 'basin_bankst_area', 'one', 1, 'double', &
     &      'Basin area bank storage, if all semi-infinite will be area of basin', &
     &      'acres', Basin_bankst_area)/=0 ) CALL read_error(3, 'basin_bankst_area')

        IF ( declvar(MODNAME, 'basin_bankst_seep_rate', 'one', 1, 'double', &
     &      'Basin rate of seepage from bank storage into stream per unit length stream', &
     &      'meter3/day/meter', Basin_bankst_seep_rate)/=0 ) CALL read_error(3, 'basin_bankst_seep_rate')

        ALLOCATE ( Bankst_head(Nhru) )
        IF ( declvar(MODNAME, 'bankst_head', 'nhru', Nhru, 'real', &
     &       'Bank storage area only average head of bank storage above groundwater head', &
     &       'meters', Bankst_head)/=0 ) CALL read_error(3, 'bankst_head')

        ALLOCATE ( Seg_bankflow(Nsegment) )
        IF ( declvar(MODNAME, 'seg_bankflow', 'nsegment', Nsegment, 'double', &
     &       'Bank storage area contribution to streamflow can be negative if steam losing water', &
     &       'cfs', Seg_bankflow)/=0 ) CALL read_error(3, 'seg_bankflow')

        ALLOCATE ( Bankst_head_pts(Nhru) )
        IF ( declvar(MODNAME, 'bankst_head_pts', 'nhru', Nhru, 'real', &
     &       'Head of bank storage above groundwater head: at half width away', &
     &       'meters', Bankst_head_pts)/=0 ) CALL read_error(3, 'bankst_head_pts')

        ALLOCATE ( Stage_ante(Nsegment) )
        IF ( declvar(MODNAME, 'stage_ante', 'nsegment', Nsegment, 'double', &
     &       'Antecedent stage height of segment, estimated with Manning Equation', &
     &       'meters', stage_ante)/=0 ) CALL read_error(3, 'stage_ante')

        ALLOCATE ( Stage_ts(Nsegment) )
        IF ( declvar(MODNAME, 'stage_ts', 'nsegment', Nsegment, 'double', &
     &       'Stage height of segment, estimated with Manning Equation', &
     &       'meters', stage_ts)/=0 ) CALL read_error(3, 'stage_ts')

        ALLOCATE ( Bankst_seep_hru(Nhru) )
        IF ( declvar(MODNAME, 'bankst_seep_hru', 'nhru', Nhru, 'real', &
     &       'HRU average seepage from bank storage to associated stream_segment for each HRU', &
     &       'inches', Bankst_seep_hru)/=0 ) CALL read_error(3, 'bankst_seep_hru')

        ALLOCATE ( Bankst_stor_hru(Nhru) )
        IF ( declvar(MODNAME, 'bankst_stor_hru', 'nhru', Nhru, 'real', &
     &       'HRU average bank storage for each HRU', &
     &       'inches', Bankst_stor_hru)/=0 ) CALL read_error(3, 'bankst_stor_hru')

        ALLOCATE ( Bankst_seep_rate(Nsegment) )
        IF ( declvar(MODNAME, 'bankst_seep_rate', 'nsegment', Nsegment, 'real', &
     &       'Seepage rate from bank storage into stream per unit length segment', &
     &       'meter2/day', Bankst_seep_rate )/=0 ) CALL read_error(1, 'bankst_seep_rate')

      ENDIF

      IF (Ripst_flag==1 .OR. Strmflow_flag==6 .OR. Strmflow_flag==7 .OR. Model==99 ) THEN
        ALLOCATE ( Mann_n(Nsegment) )
        IF ( declparam( MODNAME, 'mann_n', 'nsegment', 'real', &
     &     '0.04', '0.001', '0.15', &
     &     'Mannings roughness coefficient', &
     &     'Mannings roughness coefficient for each segment', &
     &     'dimensionless')/=0 ) CALL read_error(1, 'mann_n')
      ENDIF

      IF (Stream_temp_flag==1 .OR. Ripst_flag==1 .OR. Strmflow_flag==6 .OR. Strmflow_flag==7 .OR. Model==99 ) THEN
        ALLOCATE ( Seg_slope(Nsegment) )
        IF ( declparam( MODNAME, 'seg_slope', 'nsegment', 'real', &
     &     '0.0001', '0.0000001', '2.0', &
     &     'Surface slope of each segment', &
     &     'Surface slope of each segment as approximation for bed slope of stream', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'seg_slope')

        ALLOCATE ( Seg_length(Nsegment) )
        IF ( declparam( MODNAME, 'seg_length', 'nsegment', 'real', &
     &     '1000.0', '0.001', '200000.0', &
     &     'Length of each segment', &
     &     'Length of each segment, bounds based on CONUS', &
     &     'meters')/=0 ) CALL read_error(1, 'seg_length')
      ENDIF

      IF (Ripst_flag==1 .OR. Strmflow_flag==6 .OR. Model==99 ) THEN
        ALLOCATE ( Seg_width(Nsegment) )
        IF ( declparam(MODNAME, 'seg_width', 'nsegment', 'real', &
     &       '15.0', '0.18', '40000.0', &
     &       'Segment river width', &
     &       'Segment river width, narrowest observed from Zimmerman 1967, Amazon biggest', &
     &       'meters')/=0 ) CALL read_error(1, 'seg_width')
      ENDIF

      IF (Ripst_flag==1 .OR. Strmflow_flag==7 .OR. Model==99 ) THEN
        ALLOCATE ( Seg_depth(Nsegment) )
        IF ( declparam(MODNAME, 'seg_depth', 'nsegment', 'real', &
     &       '1.0', '0.03', '250.0', &
     &       'Segment river depth', &
     &       'Segment river depth at bankfull, shallowest from Blackburn-Lynch 2017,'//&
     &       'Congo is deepest at 250 m but in the US it is probably the Hudson at 66 m', &
     &       'meters')/=0 ) CALL read_error(1, 'seg_depth')
      ENDIF

      ALLOCATE ( Segment_type(Nsegment) )
      IF ( declparam(MODNAME, 'segment_type', 'nsegment', 'integer', &
     &     '0', '0', '111', &
     &     'Segment type', &
     &     'Segment type (0=segment; 1=headwater; 2=lake; 3=replace inflow; 4=inbound to NHM;'// &
     &     ' 5=outbound from NHM; 6=inbound to region; 7=outbound from region; 8=drains to ocean;'// &
     &     ' 9=sink; 10=inbound from Great Lakes; 11=outbound to Great Lakes)', &
     &     'none')/=0 ) CALL read_error(1, 'segment_type')

      ! user updated values if different than tosegment_orig
      ! -5 = outbound from NHM; -6 = inbound from region; -7 = outbound from region;
      ! -8 = drains to ocean; -11 = drains to Great Lake
      ALLOCATE ( Tosegment(Nsegment) )
      IF ( declparam(MODNAME, 'tosegment', 'nsegment', 'integer', &
     &     '0', '-11', '1000000', &
     &     'The index of the downstream segment', &
     &     'Index of downstream segment to which the segment'// &
     &     ' streamflow flows, for segments that do not flow to another segment enter 0', &
     &     'none')/=0 ) CALL read_error(1, 'tosegment')

      IF ( Cascade_flag==0 .OR. Cascade_flag==2 .OR. Model==99 ) THEN
        Hru_seg_cascades = 1
        ALLOCATE ( Hru_segment(Nhru) )
        IF ( declparam(MODNAME, 'hru_segment', 'nhru', 'integer', &
     &       '0', 'bounded', 'nsegment', &
     &       'Segment index for HRU lateral inflows', &
     &       'Segment index to which an HRU contributes lateral flows'// &
     &       ' (surface runoff, interflow, and groundwater discharge)', &
     &       'none')/=0 ) CALL read_error(1, 'hru_segment')
      ELSE
        Hru_seg_cascades = 0
      ENDIF

      ALLOCATE ( Obsin_segment(Nsegment) )
      IF ( declparam(MODNAME, 'obsin_segment', 'nsegment', 'integer', &
     &     '0', 'bounded', 'nobs', &
     &     'Index of measured streamflow station that replaces inflow to a segment', &
     &     'Index of measured streamflow station that replaces inflow to a segment', &
     &     'none')/=0 ) CALL read_error(1, 'obsin_segment')

      ALLOCATE ( Obsout_segment(Nsegment) )
      IF ( declparam(MODNAME, 'obsout_segment', 'nsegment', 'integer', &
     &     '0', 'bounded', 'nobs', &
     &     'Index of measured streamflow station that replaces outflow from a segment', &
     &     'Index of measured streamflow station that replaces outflow from a segment', &
     &     'none')/=0 ) CALL read_error(1, 'obsout_segment')

      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 ) THEN
        ALLOCATE ( Segment_flow_init(Nsegment) )
        IF ( declparam(MODNAME, 'segment_flow_init', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0E7', &
     &       'Initial flow in each stream segment', &
     &       'Initial flow in each stream segment', &
     &       'cfs')/=0 ) CALL read_error(1, 'segment_flow_init')
! Bank Storage parameters:
        IF ( Ripst_flag==1 .OR. Model==99 ) THEN
          ALLOCATE ( Bankst_head_init(Nhru) )
          IF ( declparam(MODNAME, 'bankst_head_init', 'nhru', 'real', &
     &         '0.0', '0.0', '1000.0', &
     &         'Bank storage area only average initial head of bank storage above groundwater head', &
     &         'Bank storage area only average initial head of bank storage above groundwater head', &
     &         'meters')/=0 ) CALL read_error(1, 'bankst_head_init')

! Riparian Overbank Storage parameters:
          ALLOCATE ( Ripst_frac_init(Nhru) )
          IF ( declparam(MODNAME, 'ripst_frac_init', 'nhru', 'real', &
     &         '0.5', '0.0', '1.0', &
     &         'Fraction of maximum storage that contains water at the start of a simulation', &
     &         'Fraction of maximum riparian overbank flow storage that'// &
     &         ' contains water at the start of a simulation', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'ripst_frac_init')

        ENDIF
      ENDIF

      IF ( Ripst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Ripst_areafr_max(Nhru) )
        IF ( declparam(MODNAME, 'ripst_areafr_max', 'nhru', 'real', &
     &         '0.1', '0.0', '1.0', &
     &         'Surface area fraction of HRU that has possible riparian overbank or bank storage', &
     &         'Surface area fraction of HRU that has possible riparian overbank or bank storage;'// &
     &         ' if =0, then overbank storage is turned off, if also bankfinite_hru =1 bank storage is off', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'ripst_areafr_max')

        ALLOCATE ( Porosity_seg(Nsegment) )
        IF ( declparam(MODNAME, 'porosity_seg', 'nsegment', 'real', &
     &       '0.4', '0.15', '0.75', &
     &       'Porosity of soil of riparian overbank flow storage', &
     &       'Porosity of soil around segment involved in riparian overbank flow storage', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'porosity_seg')

        ALLOCATE ( Ripst_et_coef(Nhru) )
        IF ( declparam(MODNAME, 'ripst_et_coef', 'nhru', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to riparian overbank flow storage', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to riparian overbank flow storage', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'ripst_et_coef')

        ALLOCATE ( Tr_ratio(Nhru) )
        IF ( declparam(MODNAME, 'tr_ratio', 'nhru', 'real', &
     &       '0.5', '0.0', '1.0', &
     &       'Triangle to rectangle ratio describing vertical cross-section'// &
     &       ' shape of riparian overbank flow storage', &
     &       'Triangle to rectangle ratio describing vertical cross-section'// &
     &       ' shape of riparian overbank flow storage;'// &
     &       ' 1 is a triangle, 0 is a rectangle', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'tr_ratio')

        ALLOCATE ( Bankfinite_hru(Nhru) )
        IF ( declparam(MODNAME, 'bankfinite_hru', 'nhru', 'integer', &
     &         '0', '0', '1', &
     &         'Bank storage is finite flag', &
     &         '1 means the bank storage is considered finite and not semi-infinite', &
     &         'none')/=0 ) CALL read_error(1, 'bankfinite_hru')

        ALLOCATE ( Transmiss_seg(Nsegment) )
        IF ( declparam(MODNAME, 'transmiss_seg', 'nsegment', 'real', &
     &       '50.0', '0.00001', '100000', &
     &       'Effective transmissivity of groundwater aquifer beneath segment', &
     &       'Efective transmissivity of groundwater aquifer beneath segment;'// &
     &       ' 1.e-8 is unfractured basalt; 10000 is gravel', &
     &       'm squared/day')/=0 ) CALL read_error(1, 'transmiss_seg')

        ALLOCATE ( Specyield_seg(Nsegment) ) !Storativity approximated as Specific yield since storativity hard to measure
        IF ( declparam(MODNAME, 'specyield_seg', 'nsegment', 'real', &
     &       '0.2', '0.01', '0.5', &
     &       'Volume of water released from storage per unit aquifer surface per unit head decline', &
     &       'Volume of water released from storage per unit aquifer surface per unit head decline; '// &
     &       ' 0.01 is clay; 0.5 is peat', &
     &       'none')/=0 ) CALL read_error(1, 'specyield_seg')

! Not using at moment
!        ALLOCATE ( Gwdepth_seg(Nsegment) )
!        IF ( declparam(MODNAME, 'gwdepth_seg', 'nsegment', 'real', &
!     &       '100.0', '-10.0', '10000.0', &
!     &       'Depth to groundwater aquifer beneath segment', &
!     &       'Depth to groundwater aquifer beneath segment;'// &
!     &       ' CONUS goes to ~300 m, but worldwide higher', &
!     &       'meters')/=0 ) CALL read_error(1, 'gwdepth_seg')

      ENDIF


      IF ( Strmflow_flag==3 .OR. Strmflow_flag==4 .OR. Strmflow_flag==7 ) ALLOCATE ( K_coef(Nsegment) )
      IF ( Strmflow_flag==3 .OR. Strmflow_flag==4 .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'K_coef', 'nsegment', 'real', &
     &       '1.0', '0.01', '24.0', &
     &       'Muskingum storage coefficient', &
     &       'Travel time of flood wave from one segment to the next downstream segment,'// &
     &       ' called the Muskingum storage coefficient; enter 1.0 for reservoirs,'// &
     &       ' diversions, and segment(s) flowing out of the basin', &
     &       'hours')/=0 ) CALL read_error(1, 'K_coef')
      ENDIF

      IF ( Strmflow_flag==3 .OR. Strmflow_flag==4 .OR. Strmflow_flag==7 .OR. Model==99 ) THEN
        ALLOCATE ( X_coef(Nsegment) )
        IF ( declparam(MODNAME, 'x_coef', 'nsegment', 'real', &
     &       '0.2', '0.0', '0.5', &
     &       'Routing weighting factor', &
     &       'The amount of attenuation of the flow wave, called the'// &
     &       ' Muskingum routing weighting factor; enter 0.0 for'// &
     &       ' reservoirs, diversions, and segment(s) flowing out of the basin', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'x_coef')
      ENDIF

      IF ( Hru_seg_cascades==1 .OR. Model==99 ) THEN
        ALLOCATE ( Seginc_potet(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_potet', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average potential ET for each segment'// &
     &       ' from HRUs contributing flow to the segment', &
     &       'inches', Seginc_potet)/=0 ) CALL read_error(3, 'seginc_potet')

        ALLOCATE ( Seginc_swrad(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_swrad', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average solar radiation for each segment'// &
     &       ' from HRUs contributing flow to the segment', &
     &       'Langleys', Seginc_swrad)/=0 ) CALL read_error(3, 'seginc_swrad')

        ALLOCATE ( Seginc_ssflow(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_ssflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average interflow for each segment from'// &
     &       ' HRUs contributing flow to the segment', &
     &       'cfs', Seginc_ssflow)/=0 ) CALL read_error(3, 'seginc_ssflow')

        ALLOCATE ( Seginc_gwflow(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_gwflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average groundwater discharge for each'// &
     &       ' segment from HRUs contributing flow to the segment', &
     &       'cfs', Seginc_gwflow)/=0 ) CALL read_error(3, 'seginc_gwflow')

        ALLOCATE ( Seginc_sroff(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_sroff', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average surface runoff for each'// &
     &       ' segment from HRUs contributing flow to the segment', &
     &       'cfs', Seginc_sroff)/=0 ) CALL read_error(3, 'seginc_sroff')

        ALLOCATE ( Seg_ssflow(Nsegment) )
        IF ( declvar(MODNAME, 'seg_ssflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average interflow for each segment from'// &
     &       ' HRUs contributing flow to the segment and upstream HRUs', &
     &       'inches', Seg_ssflow)/=0 ) CALL read_error(3, 'seg_ssflow')

        ALLOCATE ( Seg_gwflow(Nsegment) )
        IF ( declvar(MODNAME, 'seg_gwflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average groundwater discharge for each segment from'// &
     &       ' HRUs contributing flow to the segment and upstream HRUs', &
     &       'inches', Seg_gwflow)/=0 ) CALL read_error(3, 'seg_gwflow')

        ALLOCATE ( Seg_sroff(Nsegment) )
        IF ( declvar(MODNAME, 'seg_sroff', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average surface runoff for each segment from'// &
     &       ' HRUs contributing flow to the segment and upstream HRUs', &
     &       'inches', Seg_sroff)/=0 ) CALL read_error(3, 'seg_sroff')
      ENDIF

      IF ( declvar(MODNAME, 'basin_segment_storage', 'one', 1, 'double', &
     &     'Basin area-weighted average storage in the stream network', &
     &     'inches', Basin_segment_storage)/=0 ) CALL read_error(3, 'basin_segment_storage')

      ALLOCATE ( Segment_delta_flow(Nsegment) )
      IF ( declvar(MODNAME, 'segment_delta_flow', 'nsegment', Nsegment, 'double', &
     &     'Cummulative flow in minus flow out for each stream segment', &
     &     'cfs', Segment_delta_flow)/=0 ) CALL read_error(3, 'segment_delta_flow')

      ! local arrays
      ALLOCATE ( Segment_order(Nsegment), Segment_up(Nsegment), Segment_hruarea(Nsegment) )

      END FUNCTION routingdecl

!**********************************************************************
!     routinginit - check for validity of parameters
!**********************************************************************
      INTEGER FUNCTION routinginit()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nsegment, Nhru, Init_vars_from_file, Strmflow_flag, &
     &    Water_use_flag, Segment_transferON_OFF, Inputerror_flag, Parameter_check_flag , &
     &    Ripst_flag, Stream_temp_flag !, Print_debug
      USE PRMS_SET_TIME, ONLY: Timestep_seconds
      USE PRMS_BASIN, ONLY: FT2_PER_ACRE, DNEARZERO, Active_hrus, Hru_route_order, Hru_area_dble, NEARZERO, &
     &    Hru_area, FEET2METERS, CFS2CMS_CONV !, Active_area
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      IMPLICIT NONE
! Functions
      INTRINSIC MOD, DBLE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: read_error
! Local Variable
      INTEGER :: i, j, test, lval, toseg, iseg, isegerr, ierr, eseg
      REAL :: k, x, d, x_max, velocity
      DOUBLE PRECISION :: flow
      INTEGER, ALLOCATABLE :: x_off(:)
      CHARACTER(LEN=10) :: buffer
!**********************************************************************
      routinginit = 0

      Use_transfer_segment = 0
      IF ( Water_use_flag==1 .AND. Segment_transferON_OFF==1 ) Use_transfer_segment = 1

      IF ( Init_vars_from_file==0 ) THEN
        Basin_segment_storage = 0.0D0
        Segment_delta_flow = 0.0D0
      ENDIF

      IF ( Hru_seg_cascades==1 ) THEN
        Seginc_potet = 0.0D0
        Seginc_gwflow = 0.0D0
        Seginc_ssflow = 0.0D0
        Seginc_sroff = 0.0D0
        Seginc_swrad = 0.0D0
        Seg_gwflow = 0.0D0
        Seg_ssflow = 0.0D0
        Seg_sroff = 0.0D0
      ENDIF
      Hru_outflow = 0.0D0
      Flow_to_ocean = 0.0D0
      Flow_to_great_lakes = 0.0D0
      Flow_out_region = 0.0D0
      Flow_out_NHM = 0.0D0
      Flow_terminus = 0.0D0
      Flow_to_lakes = 0.0D0
      Flow_in_nation = 0.0D0
      Flow_in_region = 0.0D0
      Flow_headwater = 0.0D0
      Flow_in_great_lakes = 0.0D0
      Flow_replacement = 0.0D0

      Cfs2acft = Timestep_seconds/FT2_PER_ACRE

      IF ( getparam(MODNAME, 'segment_type', Nsegment, 'integer', Segment_type)/=0 ) CALL read_error(2, 'segment_type')
      DO i = 1, Nsegment
        Segment_type(i) = MOD( Segment_type(i), 100 )
      ENDDO

      IF ( Ripst_flag==1 .OR. Strmflow_flag==6 .OR. Strmflow_flag==7 ) THEN
        IF ( getparam(MODNAME, 'mann_n', Nsegment, 'real', Mann_n)/=0 ) CALL read_error(2, 'mann_n')
      ENDIF
      IF ( Stream_temp_flag==1 .OR. Ripst_flag==1 .OR. Strmflow_flag==6 .OR. Strmflow_flag==7 ) THEN
        IF ( getparam( MODNAME, 'seg_length', Nsegment, 'real', Seg_length)/=0 ) CALL read_error(2, 'seg_length')
        IF ( getparam( MODNAME, 'seg_slope', Nsegment, 'real', Seg_slope)/=0 ) CALL read_error(2, 'seg_slope')
! find segments that are too short and print them out as they are found
        ierr = 0
        DO i = 1, Nsegment
           IF ( Seg_length(i)<NEARZERO ) THEN
              PRINT *, 'ERROR, seg_length too small for segment:', i, ', value:', Seg_length(i)
              ierr = 1
           ENDIF
        ENDDO
! exit if there are any segments that are too short
        IF ( ierr==1 ) THEN
           Inputerror_flag = ierr
           RETURN
        ENDIF
      ENDIF
      IF ( Ripst_flag==1 .OR. Strmflow_flag==6 ) THEN
        IF ( getparam(MODNAME, 'seg_width', Nsegment, 'real', Seg_width)/=0 ) CALL read_error(2, 'seg_width')
      ENDIF
      IF ( Ripst_flag==1 .OR. Strmflow_flag==7 ) THEN
        IF ( getparam(MODNAME, 'seg_depth', Nsegment, 'real', seg_depth)/=0 ) CALL read_error(2, 'seg_depth')
      ENDIF

      IF ( getparam(MODNAME, 'tosegment', Nsegment, 'integer', Tosegment)/=0 ) CALL read_error(2, 'tosegment')
      IF ( getparam(MODNAME, 'obsin_segment', Nsegment, 'integer', Obsin_segment)/=0 ) CALL read_error(2, 'obsin_segment')
      IF ( getparam(MODNAME, 'obsout_segment', Nsegment, 'integer', Obsout_segment)/=0 ) CALL read_error(2, 'obsout_segment')

      IF ( Strmflow_flag==3 .OR. Strmflow_flag==4 .OR. Strmflow_flag==7 ) THEN
        IF ( getparam(MODNAME, 'x_coef', Nsegment, 'real', X_coef)/=0 ) CALL read_error(2, 'x_coef')
        ALLOCATE ( C1(Nsegment), C2(Nsegment), C0(Nsegment), Ts(Nsegment), Ts_i(Nsegment) )
      ENDIF

      IF ( Strmflow_flag==3 .OR. Strmflow_flag==4 ) THEN
        IF ( getparam(MODNAME, 'K_coef', Nsegment, 'real', K_coef)/=0 ) CALL read_error(2, 'K_coef')
      ENDIF

! Riparian storage variables
      IF ( Ripst_flag==1 ) THEN
        Basin_bankst_seep = 0.D0
        Basin_bankst_seep_rate = 0.0D0
        Basin_bankst_head = 0.0D0
        Basin_bankst_vol = 0.0D0
        Basin_bankst_area = 0.0D0
        Basin_ripst_evap = 0.0D0
        Basin_ripst_contrib = 0.0D0
        Basin_ripst_vol = 0.0D0
        Basin_ripst_area = 0.D0
        Ripst_evap_hru = 0.0
        Ripst_frac = 0.0
        Bankst_seep_hru = 0.0
        Bankst_seep_rate = 0.0
        Bankst_head = 0.0
        Bankst_head_pts = 0.0
        Stage_ante = 0.0D0
        Stage_ts = 0.0D0
        Seg_bankflow = 0.0D0
        Seg_ripflow = 0.0D0
        Ripst_area = 0.0
      ENDIF

      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 ) THEN
        IF ( getparam(MODNAME, 'segment_flow_init',  Nsegment, 'real', Segment_flow_init)/=0 ) &
     &       CALL read_error(2,'segment_flow_init')
        DO i = 1, Nsegment
          Seg_outflow(i) = Segment_flow_init(i)
          IF ( Ripst_flag==1 ) THEN
            flow = Seg_outflow(i)*CFS2CMS_CONV
            Stage_ts(i) = (Mann_n(i)*flow/( Seg_width(i)*SQRT(Seg_slope(i)) ))**(5./3.)
          ENDIF
        ENDDO
        DEALLOCATE ( Segment_flow_init )
      ENDIF

! if cascades are active then ignore hru_segment
      Noarea_flag = 0
      IF ( Hru_seg_cascades==1 ) THEN
        IF ( getparam(MODNAME, 'hru_segment', Nhru, 'integer', Hru_segment)/=0 ) CALL read_error(2, 'hru_segment')
        Segment_hruarea = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          iseg = Hru_segment(i)
          IF ( iseg>0 ) Segment_hruarea(iseg) = Segment_hruarea(iseg) + Hru_area_dble(i)
        ENDDO
        Segment_area = 0.0D0
        DO j = 1, Nsegment
          Segment_area = Segment_area + Segment_hruarea(j)
          IF ( Segment_hruarea(j)<DNEARZERO ) THEN
            Noarea_flag = 1
            IF ( Parameter_check_flag>0 ) THEN
              WRITE ( buffer, '(I10)' ) j
              CALL write_outfile('WARNING, No HRUs are associated with segment:'//buffer)
              IF ( Tosegment(j)==0 ) PRINT *, 'WARNING, No HRUs and tosegment=0 for segment:', j
            ENDIF
          ENDIF
        ENDDO
!        IF ( Active_area/=Segment_area ) PRINT *, 'Not all area in model domain included with segments, basin area =', &
!     &                                            Active_area, ' segment area = ', Segment_area
      ENDIF

      IF ( Ripst_flag==1 ) THEN
        IF ( getparam(MODNAME, 'ripst_areafr_max', Nhru, 'real', Ripst_areafr_max)/=0 ) CALL read_error(2, 'ripst_areafr_max')
        IF ( getparam(MODNAME, 'ripst_et_coef', Nhru, 'real', Ripst_et_coef)/=0 ) CALL read_error(2, 'ripst_et_coef')
        IF ( getparam(MODNAME, 'tr_ratio', Nhru, 'real', Tr_ratio)/=0 ) CALL read_error(2, 'tr_ratio')
        IF ( getparam(MODNAME, 'bankfinite_hru', Nhru, 'integer', Bankfinite_hru)/=0 ) CALL read_error(2, 'bankfinite_hru')
        ! might be able to calculate if want bankfinite_hru = 1 or 0 based on ripst_areafr_max and transmiss_seg
        IF ( getparam(MODNAME, 'transmiss_seg', Nsegment, 'real', Transmiss_seg)/=0 ) CALL read_error(2, 'transmiss_seg')
        IF ( getparam(MODNAME, 'specyield_seg', Nsegment, 'real', Specyield_seg)/=0 ) CALL read_error(2, 'specyield_seg')
        IF ( getparam(MODNAME, 'porosity_seg', Nsegment, 'real', Porosity_seg)/=0 ) CALL read_error(2, 'porosity_seg')
        Seg_hru_num = 0
        DO i = 1, Active_hrus
          IF ( Hru_segment(i)>0) THEN
            IF (Bankfinite_hru(i)==1) Basin_bankst_area = Basin_bankst_area+Ripst_areafr_max(i)*Hru_area_dble(i) ! in inches
            IF (Bankfinite_hru(i)==0) Basin_bankst_area = Basin_bankst_area+Hru_area_dble(i) ! in inches
            Ripst_area_max(i) = Ripst_areafr_max(i)*Hru_area(i)
! depth of hyporheic estimated at stream depth/porosity, Harvey and Wagner (2000) ??
            Ripst_depth(i) = Seg_depth(Hru_segment(i)) / Porosity_seg(Hru_segment(i))
            IF (Ripst_areafr_max(i)==0.0) Ripst_depth(i) = 0.0
            Ripst_vol_max(i) = DBLE( Ripst_area_max(i)*Ripst_depth(i)*(1.0-0.5*Tr_ratio(i)) )
            Seg_hru_num(Hru_segment(i)) =Seg_hru_num(Hru_segment(i)) +1
          ENDIF
        ENDDO
        IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 ) THEN
          IF ( getparam(MODNAME, 'ripst_frac_init', Nhru, 'real', Ripst_frac_init)/=0 ) CALL read_error(2, 'ripst_frac_init')
          IF ( getparam(MODNAME, 'bankst_head_init', Nhru, 'real', Bankst_head_init)/=0 ) CALL read_error(2, 'bankst_head_init')
          CALL init_the_swamp()
          CALL init_bank_storage()
          DEALLOCATE ( Bankst_head_init, Ripst_frac_init )
        ENDIF
      ENDIF

      isegerr = 0
      Segment_up = 0
      ! Begin the loops for ordering segments
      DO j = 1, Nsegment
        iseg = Obsin_segment(j)
        toseg = Tosegment(j)
        IF ( toseg==j ) THEN
          PRINT *, 'ERROR, tosegment value (', toseg, ') equals itself for segment:', j
          isegerr = 1
        ELSEIF ( toseg>0 ) THEN
          IF ( Tosegment(toseg)==j ) THEN
            PRINT *, 'ERROR, circle found, segment:', j, ' sends flow to segment:', toseg, ' that sends it flow'
            isegerr = 1
          ELSE
            ! load segment_up with last stream segment that flows into a segment
            Segment_up(toseg) = j
          ENDIF
        ENDIF
      ENDDO

      IF ( Parameter_check_flag>0 ) THEN
        DO i = 1, Nsegment
          IF ( Segment_up(i)==0 .AND. Tosegment(i)==0 ) &
     &         PRINT *, 'WARNING, no other segment flows into segment:',  i, ' and tosegment=0'
        ENDDO
      ENDIF

      IF ( isegerr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF

      ! Begin the loops for ordering segments
      ALLOCATE ( x_off(Nsegment) )
      x_off = 0
      Segment_order = 0
      lval = 0
      iseg = 0
      eseg = 0
      DO WHILE ( lval<Nsegment )
        ierr = 1
        DO i = 1, Nsegment
          ! If segment "i" has not been crossed out consider it, else continue
          IF ( x_off(i)==1 ) CYCLE
          iseg = i
          ! Test to see if segment "i" is the tosegment from other segments
          test = 1
          DO j = 1, Nsegment
            IF ( Tosegment(j)==i ) THEN
              ! If segment "i" is a to segment, test to see if the originating
              ! segment has been crossed off the list.  if all have been, then
              ! put the segment in as an ordered segment
              IF ( x_off(j)==0 ) THEN
                test = 0
                eseg = j
                EXIT
              ENDIF
            ENDIF
          ENDDO
          IF ( test==1 ) THEN
            lval = lval + 1
            Segment_order(lval) = i
            x_off(i) = 1
            ierr = 0
          ENDIF
        ENDDO
        IF ( ierr==1 ) THEN
          PRINT *, 'ERROR, circular segments involving', iseg, 'and', eseg
          STOP
        ENDIF
      ENDDO
!      IF ( Print_debug==20 ) THEN
!        PRINT *, 'Stream Network Routing Order:'
!        PRINT '(10I5)', Segment_order
!        PRINT *, 'tosegment:'
!        PRINT '(10I5)', Tosegment
!      ENDIF
      DEALLOCATE ( x_off )

      IF ( Strmflow_flag==6 .OR. Strmflow_flag==5 ) RETURN ! mizuroute or strmflow_in_out
!
!      Compute the three constants in the Muskingum routing equation based
!      on the values of K_coef and a routing period of 1 hour. See the notes
!      at the top of this file.
!
      C0 = 0.0
      C1 = 0.0
      C2 = 0.0
!make sure K>0
      Ts = 1.0
      ierr = 0
      DO i = 1, Nsegment
        IF ( Strmflow_flag==7 ) THEN ! muskingum_mann
          velocity = (1./Mann_n(i))*SQRT(Seg_slope(i))*Seg_depth(i)**(2./3.) ! simplify if say width>>depth
          K_coef(i) = Seg_length(i)/(velocity*60.*60.) !want in hours, length should include sloped length
          !K_coef(i) = Seg_length(i)*sqrt(1+ Seg_slope(i)**2)/(velocity*60.*60.) !want in hours
        ENDIF

        IF ( Segment_type(i)==2 .AND. K_coef(i)<24.0 ) K_coef(i) = 24.0 !K_coef must be specified = 24.0 for lake segments'
        IF ( K_coef(i)<0.01 ) K_coef(i) = 0.01 !make compliant with old version of K_coef
        IF ( K_coef(i)>24.0 ) K_coef(i) = 24.0
        k = K_coef(i)
        x = X_coef(i)

! check the values of k and x to make sure that Muskingum routing is stable

        IF ( k<1.0 ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT '(/,A,I6,A,F6.2,/,9X,A,/)', 'WARNING, segment ', i, ' has K_coef < 1.0,', k, &
     &              'this may produce unstable results'
!              ierr = 1
          ENDIF
!          Ts(i) = 0.0 ! not sure why this was set to zero, causes divide by 0 if K_coef < 1, BUG FIX 10/18/2016 RSR
          Ts_i(i) = -1

        ELSEIF ( k<2.0 ) THEN
          Ts(i) = 1.0
          Ts_i(i) = 1

        ELSEIF ( k<3.0 ) THEN
          Ts(i) = 2.0
          Ts_i(i) = 2

        ELSEIF ( k<4.0 ) THEN
          Ts(i) = 3.0
          Ts_i(i) = 3

        ELSEIF ( k<6.0 ) THEN
          Ts(i) = 4.0
          Ts_i(i) = 4

        ELSEIF ( k<8.0 ) THEN
          Ts(i) = 6.0
          Ts_i(i) = 6

        ELSEIF ( k<12.0 ) THEN
          Ts(i) = 8.0
          Ts_i(i) = 8

        ELSEIF ( k<24.0 ) THEN
          Ts(i) = 12.0
          Ts_i(i) = 12

        ELSE
          Ts(i) = 24.0
          Ts_i(i) = 24

        ENDIF

!  x must be <= t/(2K) the C coefficents will be negative. Check for this for all segments
!  with Ts >= minimum Ts (1 hour).
        IF ( Ts(i)>0.1 ) THEN
          x_max = Ts(i) / (2.0 * k)
          IF ( x>x_max ) THEN
            PRINT *, 'ERROR, x_coef value is too large for stable routing for segment:', i, ' x_coef:', x
            PRINT *, '       a maximum value of:', x_max, ' is suggested'
            Inputerror_flag = 1
            CYCLE
          ENDIF
        ENDIF

        d = k - (k * x) + (0.5 * Ts(i))
        IF ( ABS(d)<NEARZERO ) THEN
          IF ( Parameter_check_flag>0 ) PRINT *, 'WARNING, segment ', i, ' computed value d <', NEARZERO, ', set to 0.0001'
          d = 0.0001
        ENDIF
        C0(i) = (-(k * x) + (0.5 * Ts(i))) / d
        C1(i) = ((k * x) + (0.5 * Ts(i))) / d
        C2(i) = (k - (k * x) - (0.5 * Ts(i))) / d

        ! the following code was in the original musroute, but, not in Linsley and others
        ! rsr, 3/1/2016 - having < 0 coefficient can cause negative flows as found by Jacob in GCPO headwater
!  if c2 is <= 0.0 then  short travel time though reach (less daily
!  flows), thus outflow is mainly = inflow w/ small influence of previous
!  inflow. Therefore, keep c0 as is, and lower c1 by c2, set c2=0

!  if c0 is <= 0.0 then long travel time through reach (greater than daily
!  flows), thus mainly dependent on yesterdays flows.  Therefore, keep
!  c2 as is, reduce c1 by c0 and set c0=0
! SHORT travel time
        IF ( C2(i)<0.0 ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT '(/,A)', 'WARNING, c2 < 0, set to 0, c1 set to c1 + c2'
            PRINT *, '        old c2:', C2(i), '; old c1:', C1(i), '; new c1:', C1(i) + C2(i)
            PRINT *, '        K_coef:', K_coef(i), '; x_coef:', x_coef(i)
          ENDIF
          C1(i) = C1(i) + C2(i)
          C2(i) = 0.0
        ENDIF

! LONG travel time
        IF ( C0(i)<0.0 ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT '(/,A)', 'WARNING, c0 < 0, set to 0, c0 set to c1 + c0'
            PRINT *, '      old c0:', C0(i), 'old c1:', C1(i), 'new c1:', C1(i) + C0(i)
            PRINT *, '        K_coef:', K_coef(i), '; x_coef:', x_coef(i)
          ENDIF
          C1(i) = C1(i) + C0(i)
          C0(i) = 0.0
        ENDIF

      ENDDO
      IF ( ierr==1 ) PRINT '(/,A,/)', '***Recommend that the Muskingum parameters be adjusted in the Parameter File'
      DEALLOCATE ( K_coef, X_coef)

      END FUNCTION routinginit

!***********************************************************************
!     route_run - Computes segment flow states and fluxes
!***********************************************************************
      INTEGER FUNCTION route_run()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nsegment, Cascade_flag, Glacier_flag
      USE PRMS_BASIN, ONLY: Hru_area, Hru_route_order, Active_hrus, NEARZERO, FT2_PER_ACRE
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Potet
      USE PRMS_SET_TIME, ONLY: Timestep_seconds, Cfs_conv
      USE PRMS_FLOWVARS, ONLY: Ssres_flow, Sroff, Seg_lateral_inflow !, Seg_outflow
      USE PRMS_WATER_USE, ONLY: Segment_transfer, Segment_gain
      USE PRMS_GWFLOW, ONLY: Gwres_flow
      USE PRMS_SRUNOFF, ONLY: Strm_seg_in
      USE PRMS_GLACR, ONLY: Glacr_flow
      IMPLICIT NONE
! Functions
      INTRINSIC DBLE
! Local Variables
      INTEGER :: i, j, jj, this_seg
      DOUBLE PRECISION :: tocfs
      LOGICAL :: found
!***********************************************************************
      route_run = 0

      Cfs2acft = Timestep_seconds/FT2_PER_ACRE

! seg variables are not computed if cascades are active as hru_segment is ignored
      IF ( Hru_seg_cascades==1 ) THEN
        ! add hru_ppt, hru_actet
        Seginc_gwflow = 0.0D0
        Seginc_ssflow = 0.0D0
        Seginc_sroff = 0.0D0
        Seginc_swrad = 0.0D0
        Seginc_potet = 0.0D0
        Seg_gwflow = 0.0D0
        Seg_sroff = 0.0D0
        Seg_ssflow = 0.0D0
      ENDIF
      IF ( Cascade_flag==0 ) THEN
        Seg_lateral_inflow = 0.0D0
      ELSE ! use strm_seg_in for cascade_flag = 1 or 2
        Seg_lateral_inflow = Strm_seg_in
      ENDIF

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        tocfs = DBLE( Hru_area(j) )*Cfs_conv
        Hru_outflow(j) = DBLE( (Sroff(j) + Ssres_flow(j) + Gwres_flow(j)) )*tocfs
        ! Note: glacr_flow (from glacier or snowfield) is added as a gain, outside stream network addition
        ! glacr_flow in inch^3, 1728=12^3
        IF ( Glacier_flag==1 ) Hru_outflow(j) = Hru_outflow(j) + Glacr_flow(j)/1728.0/Timestep_seconds
        IF ( Hru_seg_cascades==1 ) THEN
          i = Hru_segment(j)
          IF ( i>0 ) THEN
            Seg_gwflow(i) = Seg_gwflow(i) + Gwres_flow(j)
            Seg_sroff(i) = Seg_sroff(i) + Sroff(j)
            Seg_ssflow(i) = Seg_ssflow(i) + Ssres_flow(j)
            ! if cascade_flag = 2, seg_lateral_inflow set with strm_seg_in
            IF ( Cascade_flag==0 ) Seg_lateral_inflow(i) = Seg_lateral_inflow(i) + Hru_outflow(j)
            Seginc_sroff(i) = Seginc_sroff(i) + DBLE( Sroff(j) )*tocfs
            Seginc_ssflow(i) = Seginc_ssflow(i) + DBLE( Ssres_flow(j) )*tocfs
            Seginc_gwflow(i) = Seginc_gwflow(i) + DBLE( Gwres_flow(j) )*tocfs
            Seginc_swrad(i) = Seginc_swrad(i) + DBLE( Swrad(j)*Hru_area(j) )
            Seginc_potet(i) = Seginc_potet(i) + DBLE( Potet(j)*Hru_area(j) )
          ENDIF
        ENDIF
      ENDDO

      IF ( Use_transfer_segment==1 ) THEN
        DO i = 1, Nsegment
          Seg_lateral_inflow(i) = Seg_lateral_inflow(i) + DBLE( Segment_gain(i) - Segment_transfer(i) )
        ENDDO
      ENDIF

      IF ( Cascade_flag==1 ) RETURN

! Divide solar radiation and PET by sum of HRU area to get avarage
      IF ( Noarea_flag==0 ) THEN
        DO i = 1, Nsegment
          Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
          Seginc_potet(i) = Seginc_potet(i)/Segment_hruarea(i)
        ENDDO

! If there are no HRUs associated with a segment, then figure out some
! other way to get the solar radiation, the following is not great
      ELSE !     IF ( Noarea_flag==1 ) THEN
        DO i = 1, Nsegment
! This reworked by markstrom
          IF ( Segment_hruarea(i)>NEARZERO ) THEN
            Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
            Seginc_potet(i) = Seginc_potet(i)/Segment_hruarea(i)
          ELSE

! Segment does not have any HRUs, check upstream segments.
            this_seg = i
            found = .false.
            do
              if (Segment_hruarea(this_seg) <= NEARZERO) then

                 ! Hit the headwater segment without finding any HRUs (i.e. sources of streamflow)
                 if (segment_up(this_seg) .eq. 0) then
                     found = .false.
                     exit
                 endif

                 ! There is an upstream segment, check that segment for HRUs
                 this_seg = segment_up(this_seg)
              else
                  ! This segment has HRUs so there will be swrad and potet
                  Seginc_swrad(i) = Seginc_swrad(this_seg)/Segment_hruarea(this_seg)
                  Seginc_potet(i) = Seginc_potet(this_seg)/Segment_hruarea(this_seg)
                  found = .true.
                  exit
              endif
            enddo

            if (.not. found) then
! Segment does not have any upstream segments with HRUs, check downstream segments.

              this_seg = i
              found = .false.
              do
                if (Segment_hruarea(this_seg) <= NEARZERO) then

                   ! Hit the terminal segment without finding any HRUs (i.e. sources of streamflow)
                   if (tosegment(this_seg) .eq. 0) then
                     found = .false.
                     exit
                   endif

                   ! There is a downstream segment, check that segment for HRUs
                   this_seg = tosegment(this_seg)
                else
                    ! This segment has HRUs so there will be swrad and potet
                    Seginc_swrad(i) = Seginc_swrad(this_seg)/Segment_hruarea(this_seg)
                    Seginc_potet(i) = Seginc_potet(this_seg)/Segment_hruarea(this_seg)
                    found = .true.
                    exit
                endif
              enddo

              if (.not. found) then
!                write(*,*) "route_run: no upstream or downstream HRU found for segment ", i
!                write(*,*) "    no values for seginc_swrad and seginc_potet"
                Seginc_swrad(i) = -99.9
                Seginc_potet(i) = -99.9
              endif
            endif
          ENDIF
        ENDDO
      ENDIF

      END FUNCTION route_run

!***********************************************************************
! Initialize overbank riparian (swamp) hydrology
!***********************************************************************
      SUBROUTINE init_the_swamp()
      USE PRMS_BASIN, ONLY: Basin_area_inv, Hru_area_dble, Active_hrus
      USE PRMS_ROUTING, ONLY: Basin_ripst_vol, Basin_ripst_area, Ripst_vol, Ripst_frac, &
     &    Hru_segment, Ripst_frac_init, Basin_ripst_vol, Ripst_area, Ripst_area_max, &
     &    Ripst_vol_max, Ripst_stor_hru
      IMPLICIT NONE
! Functions
      INTRINSIC SNGL, DBLE
! Local Variables
      INTEGER :: i
!***********************************************************************
      DO i = 1, Active_hrus
        IF ( Hru_segment(i)>0) THEN
          Ripst_frac(i) = Ripst_frac_init(i)
          Ripst_vol(i) = DBLE(Ripst_frac(i))*Ripst_vol_max(i)
          Ripst_stor_hru(i) = Ripst_vol(i)/Hru_area_dble(i)
!         Filled riparian storage surface area for each HRU:
!          Fills outward from the river with one edge on river and with same depth and same side shape
!          this works out to keeping fraction same for area and volume filled
          Ripst_area(i) = Ripst_area_max(i)*Ripst_frac(i) !area
!         calculate the basin riparian storage volumes
          Basin_ripst_vol = Basin_ripst_vol + Ripst_vol(i)
          Basin_ripst_area =  Basin_ripst_area + Ripst_area(i)
        ENDIF
      ENDDO
      Basin_ripst_vol = Basin_ripst_vol*Basin_area_inv

      END SUBROUTINE init_the_swamp

!***********************************************************************
!     Compute overbank area (swamp) fill and drain
! Treat like a closed surface depression in that it can't spill.
! Right now, not getting water from anywhere but stream, and losing only
!  to ET and seep. Possibly should take water in from precipitation,
!  runoff, and interflow.
! This is called after bank storage has been removed, so not inside
!  hourly routing.
!***********************************************************************
      SUBROUTINE drain_the_swamp(Ihru)
      USE PRMS_ROUTING, ONLY: Seg_width, Seg_depth, Seg_width, Hru_segment, Mann_n, &
     &    Tr_ratio, Ripst_vol_max, Ripst_et_coef, Ripst_evap_hru, Seg_length, &
     &    Basin_ripst_vol, Basin_ripst_evap, Basin_ripst_contrib, Ripst_stor_hru, &
     &    Ripst_frac, Ripst_vol, Ripst_area_max, Ripst_area, Seg_slope, &
     &    Seg_hru_num, Seg_ripflow, Ripst_depth, Basin_ripst_area !, Transmiss_seg
      USE PRMS_MODULE, ONLY: Frozen_flag
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO, Hru_area, Hru_area_dble, FEET2METERS, &
     &    FT2_PER_ACRE, CFS2CMS_CONV
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_SET_TIME, ONLY: Timestep_seconds
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap, Dprst_evap_hru, Frozen, Thaw_depth, Soil_depth, &
    &     Dprst_seep_rate_open
      USE PRMS_INTCP, ONLY: Hru_intcpevap
      USE PRMS_SNOW, ONLY: Snowcov_area, Snow_evap
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, LOG, MIN, DBLE, SNGL
! Arguments
      INTEGER, INTENT(IN) :: Ihru
! Local Variables
      REAL :: ripst_avail_et, unsatisfied_et, ripst_evap, ripst_wid, thaw_frac
      REAL :: inflow, inflow_in, max_depth
      DOUBLE PRECISION :: seep, ripst_grnd, poss, seep_in, ripst_contrib_hru
!***********************************************************************
      thaw_frac = 1.0
      IF (Frozen_flag==1) THEN
        IF ( Frozen(Ihru)==1 ) THEN
          thaw_frac = 0.0
        ELSEIF ( Frozen(Ihru)==2) THEN
          thaw_frac = Thaw_depth(Ihru)/Soil_depth(Ihru)
        ENDIF
      ENDIF
!It won't get deeper than this depth, should be Seg_depth but not accurate or Seg_width and other terms not accurate
      !max_depth = Seg_depth(Hru_segment(Ihru))
      max_depth = Seg_depth(Hru_segment(Ihru))*20.0
! amount possible in cfs given a river depth
      poss = Seg_width(Hru_segment(Ihru))*SQRT(Seg_slope(Hru_segment(Ihru)))* &
     &            max_depth**(3./5.)/ ( CFS2CMS_CONV*Mann_n(Hru_segment(Ihru)) )
!inflow is water over bank, remove from Seg_outflow(Hru_segment(Ihru)) and give half to
! each side of bank, in acre inches
      inflow  = 0.0
      inflow_in = 0.0
! in cfs, amount over amount possible, no inflow if everything frozen, and then no outflow either
      IF (thaw_frac>0.0) THEN
        IF ( poss < Seg_outflow(Hru_segment(Ihru))) inflow = SNGL(Seg_outflow(Hru_segment(Ihru)) - poss)
! give it equally to each HRU surrounding it
        inflow = inflow/REAL(Seg_hru_num(Hru_segment(Ihru)))
!negative flow is out of stream into riparian
        Seg_ripflow(Hru_segment(Ihru)) = Seg_ripflow(Hru_segment(Ihru)) - inflow
        inflow_in = SNGL(inflow*Timestep_seconds/(FT2_PER_ACRE*12.0)) !inch acre
        Ripst_vol(Ihru) = Ripst_vol(Ihru) + inflow_in
        Ripst_frac(Ihru)= SNGL(Ripst_vol(Ihru)/(Ripst_vol_max(Ihru)*thaw_frac))
        IF (Ripst_frac(Ihru)>1.0) Ripst_frac(Ihru) = 1.0
! Filled riparian storage surface area for each HRU:
!  Fills outward from the river with one edge on river and with same depth and same side shape
!  this works out to keeping fraction same for area and volume filled
        Ripst_area(Ihru) = Ripst_area_max(Ihru)*Ripst_frac(Ihru)

        ! evaporate water from riparian area based on snowcov_area
        ! ripst_evap_open & ripst_evap_clos = inches-acres on the HRU
        unsatisfied_et = Potet(Ihru) - Snow_evap(Ihru) - Hru_intcpevap(Ihru) &
    &                 - Hru_impervevap(Ihru) - Dprst_evap_hru(Ihru)
        ripst_avail_et = 0.0
        ripst_avail_et = Potet(Ihru)*(1.0-Snowcov_area(Ihru))*Ripst_et_coef(Ihru)
        Ripst_evap_hru(Ihru) = 0.0
        IF ( ripst_avail_et>0.0 ) THEN
          ripst_evap = 0.0
          IF ( Ripst_area(Ihru)>0.0 ) THEN
            ripst_evap = MIN(Ripst_area(Ihru)*ripst_avail_et, SNGL(Ripst_vol(Ihru)))
            IF ( ripst_evap/Hru_area(Ihru)>unsatisfied_et ) THEN
              !IF ( Print_debug>-1 ) THEN
              !  PRINT *, 'Warning, ripst evaporation > available ET, HRU:, ', Ihru, &
!      &                 unsatisfied_et, ripst_evap*Ripst_frac(Ihru)
              !  PRINT *, 'Set to available ET, perhaps ripst_et_coef specified too large'
              !  PRINT *, 'Set print_debug to -1 to turn off message'
              !ENDIF
              ripst_evap = unsatisfied_et*Hru_area(Ihru)
            ENDIF
            IF ( ripst_evap>SNGL(Ripst_vol(Ihru)) ) ripst_evap = SNGL( Ripst_vol(Ihru) )
            Ripst_vol(Ihru) = Ripst_vol(Ihru) - DBLE( ripst_evap )
          ENDIF
          Ripst_evap_hru(Ihru) = ripst_evap/Hru_area(Ihru)
        ENDIF

        ! compute seepage
        seep = 0.0
        seep_in = 0.0
        IF ( Ripst_vol(Ihru)>NEARZERO) THEN
          ripst_wid =  SNGL(Ripst_area(Ihru)*FT2_PER_ACRE*(FEET2METERS**2.0)/Seg_length(Hru_segment(Ihru))) !meters
!assumed it was a one sided stream, here a headwater with both sides in one HRU
          IF ( Seg_hru_num(Hru_segment(Ihru))==1 ) ripst_wid = ripst_wid/2.0
! Stream ground area is stream side area (flat wall) and other side area (fraction of triangle (1) to rectangle (0))
          ripst_grnd = DBLE( Seg_length(Hru_segment(Ihru))*( ripst_wid*(1.0-Tr_ratio(Ihru))  + & !rectangle
     &              (SQRT( ripst_wid**2.0 + (Ripst_depth(Ihru)*thaw_frac)**2.0 )- Ripst_depth(Ihru)*thaw_frac)*Tr_ratio(Ihru) + & !triangle
     &              2.0*Ripst_depth(Ihru)*thaw_frac ) ) !stream and other side
!assumed it was a one sided stream, here a headwater with both sides in one HRU
          IF ( Seg_hru_num(Hru_segment(Ihru))==1 ) ripst_grnd = ripst_grnd*2.0
!seep in a day through ground surface area of riparian, m^3 into ft^3 to acre_in
!Transmissivity way too big
          !seep = ripst_grnd*DBLE( Transmiss_seg(Hru_segment(Ihru)) )/(FEET2METERS**3.0) !acre_in
!ground area to total surface area is 5/6, then use depression seep coeff but reduce because surface area smaller
          seep = ripst_grnd/(ripst_grnd+Ripst_area(Ihru)*FT2_PER_ACRE/ FEET2METERS**2.0 )/(5.0/6.0) &
     &         *Ripst_vol(Ihru)*Dprst_seep_rate_open(Ihru)/FT2_PER_ACRE/12.0
          !seep = 0.0 !if want to turn off seep
          seep_in = seep*FT2_PER_ACRE*12.0 ! inch acres
          Ripst_vol(Ihru) = Ripst_vol(Ihru) - seep_in
          IF ( Ripst_vol(Ihru)<0.0D0 ) THEN
            !IF ( Ripst_vol(Ihru)<-DNEARZERO ) PRINT *, 'issue, ripst_vol<0.0', Ihru, Ripst_vol(Ihru)
            seep_in = seep_in + Ripst_vol(Ihru)
            seep = seep_in/FT2_PER_ACRE/12.0 !ft^3
            Ripst_vol(Ihru) = 0.0D0
          ENDIF
        ENDIF
        IF ( Ripst_vol(Ihru)<0.0D0 ) THEN
          !IF ( Ripst_vol(Ihru)<-DNEARZERO ) PRINT *, 'issue, ripst_vol<0.0', Ihru, Ripst_vol(Ihru)
          Ripst_vol(Ihru) = 0.0D0
        ENDIF
        ripst_contrib_hru = seep_in - inflow_in !inch per acre
        ! seep goes back in stream as positive flow, cfs
        Seg_ripflow(Hru_segment(Ihru)) = Seg_ripflow(Hru_segment(Ihru))+ seep/Timestep_seconds
        !Seg_ripflow(Hru_segment(Ihru)) = 0.0 !if want to turn off overbank flow

        Ripst_frac(Ihru)= SNGL(Ripst_vol(Ihru)/(Ripst_vol_max(Ihru)*thaw_frac))
        Ripst_area(Ihru) = Ripst_area_max(Ihru)*Ripst_frac(Ihru)
        Ripst_stor_hru(Ihru) = Ripst_vol(Ihru)/Hru_area_dble(Ihru)
        Basin_ripst_vol = Basin_ripst_vol + Ripst_vol(Ihru)
        Basin_ripst_evap = Basin_ripst_evap + DBLE(Ripst_evap_hru(Ihru))*Hru_area_dble(Ihru)
        Basin_ripst_contrib = Basin_ripst_contrib + ripst_contrib_hru
        Basin_ripst_area =  Basin_ripst_area + Ripst_area(Ihru)
      ENDIF

      END SUBROUTINE drain_the_swamp

!***********************************************************************
! Initialize bank storage hydrology
!***********************************************************************
      SUBROUTINE init_bank_storage()
      USE PRMS_BASIN, ONLY: NEARZERO, Basin_area_inv, Hru_area_dble, Active_hrus, &
     &    FT2_PER_ACRE, FEET2METERS, CFS2CMS_CONV
      USE PRMS_ROUTING, ONLY: Basin_bankst_head, Bankst_head_init, Basin_bankst_area, &
     &    Basin_bankst_vol, Bankst_head, Hru_segment, Seg_width, Seg_length, &
     &    Bankst_stor_hru, Bankst_head_pts, Ripst_areafr_max, Bankfinite_hru
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      IMPLICIT NONE
! Functions
      INTRINSIC SNGL
! Local Variables
      INTEGER :: i
!***********************************************************************
      DO i = 1, Active_hrus
        IF ( Hru_segment(i)>0) THEN
          Bankst_head(i) = Bankst_head_init(i)
          Bankst_head_pts(i) =SNGL(Seg_outflow(Hru_segment(i))*CFS2CMS_CONV)*60.*60.*24. &
     &              /Seg_width(Hru_segment(i))/Seg_length(Hru_segment(i))
          IF (Bankfinite_hru(i)==1) THEN
            Bankst_stor_hru(i) = Ripst_areafr_max(i)*12.0*Bankst_head(i)/FEET2METERS !in inches
            Basin_bankst_head = Basin_bankst_head + Ripst_areafr_max(i)*Bankst_head(i)*Hru_area_dble(i) ! in meters
          ELSE
            Bankst_stor_hru(i) = 12.0*Bankst_head(i)/FEET2METERS !in inches
            Basin_bankst_head = Basin_bankst_head + Bankst_head(i)*Hru_area_dble(i) ! in meters
          ENDIF
          Basin_bankst_vol = Basin_bankst_vol+Bankst_stor_hru(i)*Hru_area_dble(i) ! in inches
        ENDIF
      ENDDO
      Basin_bankst_vol = Basin_bankst_vol*Basin_area_inv
      Basin_bankst_head = Basin_bankst_head/Basin_bankst_area

      END SUBROUTINE init_bank_storage

!***********************************************************************
!     Compute bank storage hydrology
! For the linear boundary-value problems discussed by Moench and Barlow (2000), the total
!  response of a stream–aquifer system to a time series of individual stresses (stream-stage
!  fluctuations or recharge) can be determined by superposition (or convolution) of the
!  system’s response to the individual stresses.
! Assume no layer of semi-pervious bank sediments, so storage right at the bank. Use a
!  finite confined aquifer with no overlying aquitard, or a finite water table aquifer
!  (swamp) with a specific yield of aquifer = 0, or ~ 0.
! This is saying very little water is released by the aquifer from the water table lowering
!  and the unsaturated zone is thin. This is true for shallow water table aquifers; see
!  Barlow et al (2000).
!***********************************************************************
      SUBROUTINE comp_bank_storage(Ihru)
      USE PRMS_ROUTING, ONLY: Bankst_seep_rate, Bankst_head, Bankst_head_pts, Hru_segment, &
     &    Bankst_seep_hru, Bankst_stor_hru, Stage_ts, Stage_ante, Seg_bankflow, Ripst_areafr_max, &
     &    Transmiss_seg, Seg_width, Seg_length, Specyield_seg, Bankfinite_hru, Seg_hru_num, &
     &    Basin_bankst_seep, Basin_bankst_head, Basin_bankst_vol
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, FT2_PER_ACRE, FEET2METERS, Hru_area, Hru_area_dble
      USE PRMS_FLOWVARS, ONLY: Gwres_stor
      IMPLICIT NONE
! Functions
      INTRINSIC SUM, SNGL, SQRT
      EXTERNAL LTST1
! Arguments
      INTEGER, INTENT(IN) :: Ihru
 ! Local Variables
      INTEGER :: h, t0
      INTEGER, PARAMETER :: nbankd = 2
      REAL, PARAMETER :: PI = 3.14159
      REAL :: area, str_wid, tot_wid, bank_wid, trans, a, xd, t, td
      REAL :: delt, delta_input(nbankd), delta_diff(nbankd), head(nbankd), seep(nbankd)
      REAL :: bank(nbankd), bankv(nbankd), ripfrac
      DOUBLE PRECISION :: input_net(nbankd), diff_net(nbankd), recharge(nbankd), stage(nbankd)
      DOUBLE PRECISION :: head_step, head_step_grad, seep_sum, head_sum
!***********************************************************************
      area  = Ripst_areafr_max(Ihru)*Hru_area(Ihru) !acres
      trans = Transmiss_seg(Hru_segment(Ihru))
!aquifer diffusivity, ratio of the transmissivity/storativity of the aquifer
      a = trans/Specyield_seg(Hru_segment(Ihru))
      str_wid = Seg_width(Hru_segment(Ihru))/2.0
      bank_wid = SNGL(area*FT2_PER_ACRE*(FEET2METERS**2.)/Seg_length(Hru_segment(Ihru))/str_wid) !dimensionless
      tot_wid = bank_wid+1.0 !dimensionless
      delt = 1.0 !fraction of day
! might want to interpolate a curve, so leaving nbankd as a dimension -- sh
      stage(1) = Stage_ante(Hru_segment(Ihru))
      stage(2) = Stage_ts(Hru_segment(Ihru))
      ! changes in a day
      DO h = 1, nbankd
        seep(h) = 0.0
        bank(h) = 0.0
        bankv(h) = 0.0
        recharge(h) = (h-1)*delt*Gwres_stor(Ihru)*FEET2METERS/12.D0 ! in meters, currently ignoring ET
! Can only use recharge change if say it's a leaky aquifer overlain by a water table aquitard.
! might want to do that. Also might want to go other way and make simpler, make it semi-infinite so then
! no numerical Laplace inverse, just can solve
        input_net(h) = stage(h) !+ recharge(h)
        diff_net(h) = stage(h) !- recharge(h) !FIX What is this vs input_net
      ENDDO
!Make head ideal flood wave for volume change and recharge ideal observed response at a well for vol change??
      DO h = 2,nbankd
        delta_input(h-1) = SNGL( (input_net(h)-input_net(h-1))/delt )
        delta_diff(h-1) = SNGL((diff_net(h)-diff_net(h-1))/delt )
      ENDDO
      Bankst_seep_hru(Ihru) = 0.0
      xd = 1.0+ bank_wid/2.0 ! at x = 1.0 is stage  which already know, calc at middle of bank storage area
      head=Bankst_head_pts(Ihru) !set at last height for initial
! Calculate heads, seepage, and bank storage using convolution
      ripfrac = Ripst_areafr_max(Ihru)
      IF (Bankfinite_hru(Ihru)==0) ripfrac = 1.0
      DO h = 1, (nbankd-1)
        head_sum = 0.0
        seep_sum = 0.0
        DO t0 = 1,h
          t = t0*delt
          td = t*a/(str_wid**2.0) !dimensionless
          IF (Bankfinite_hru(Ihru)==1) then !finite solution if transmissivity high, COMPUTATIONALLY EXPENSIVE, might eliminate
            CALL LTST1(td, xd, tot_wid, bank_wid, head_step, head_step_grad)
          ELSE IF (Bankfinite_hru(Ihru)==0) then !semi-infinite solution
            head_step = ERFC( (xd - 1.0)/SQRT((4.0*td)) )
            head_step_grad = -( 1.0/SQRT((PI*td)) )
          ENDIF
          !head is a function of xd
          head_sum = delta_input(h-t0+1)*head_step + head_sum
          !seep is per unit segment length rate goes out, not a function of xd
          seep_sum = delta_diff(h-t0+1)*head_step_grad + seep_sum
        ENDDO
        head(h+1)=head(h+1) + SNGL(head_sum*delt)
        seep(h+1)=SNGL((trans/str_wid)*seep_sum*delt)
        bank(h+1)=bank(h) - seep(h+1)*delt
        bankv(h+1)=bank(h+1)*Seg_length(Hru_segment(Ihru))
        !IF (Ihru==1) print*,h+1,stage(h+1),bank(h+1),seep(h+1),bankv(h+1) !for plotting daily pattern
      ENDDO
      IF ( Seg_hru_num(Hru_segment(Ihru))==1 ) THEN
!assumed it was a one sided stream, here a headwater with both sides in one HRU
        seep = seep*2.0
        bank = bank*2.0
        bankv = bankv*2.0
      ENDIF
      Bankst_head_pts(Ihru) = head(nbankd) ! meters
      !linear interpolation for total average head over bank storage area, meters
      Bankst_head(Ihru) = 0.5*(SNGL(stage(nbankd))+Bankst_head_pts(Ihru))
      ! Bankst_head_pts at finite edge of bank storage area is 0 (xd = 1, so head_step = 0)
      !  is only saved at the end of the timestep
      Bankst_head(Ihru) = Bankst_head(Ihru) + 0.5*Bankst_head_pts(Ihru)
      ! m2 per 24 hr per stream segment for both sides of stream
      ! seep hru is inch over hru seeping out per day
      Bankst_seep_hru(Ihru) = -12.0*bankv(nbankd)/SNGL(CFS2CMS_CONV*Hru_area(Ihru)*FT2_PER_ACRE)
      Bankst_seep_rate(Hru_segment(Ihru)) = Bankst_seep_rate(Hru_segment(Ihru)) - bank(nbankd)
      Bankst_stor_hru(Ihru) = Bankst_stor_hru(Ihru)- Bankst_seep_hru(Ihru) !inch over hru
      Seg_bankflow(Hru_segment(Ihru)) = Seg_bankflow(Hru_segment(Ihru))-bankv(nbankd)/(24.*60.*60.)/CFS2CMS_CONV
      !FIX area change?? no I don't think so
      Basin_bankst_seep = Basin_bankst_seep + Bankst_seep_hru(Ihru)*Hru_area_dble(Ihru)
      Basin_bankst_head = Basin_bankst_head + ripfrac*Bankst_head(Ihru)* Hru_area_dble(Ihru)
      Basin_bankst_vol = Basin_bankst_vol+Bankst_stor_hru(Ihru)*Hru_area_dble(Ihru)

      END SUBROUTINE comp_bank_storage

!***********************************************************************
!     Laplace transform leakage equation
!***********************************************************************
      SUBROUTINE LTST1(td, xd, tot_wid, bank_wid, head_step, head_step_grad)
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, LOG, DBLE
      EXTERNAL LINVST
! Arguments
      REAL, INTENT(IN) :: td, xd, tot_wid, bank_wid
      DOUBLE PRECISION, INTENT(OUT) :: head_step, head_step_grad
! Local Variables
      INTEGER, PARAMETER :: NS=12 ! Number of Stehfest terms, 8 usually sufficient but Barlow uses 12
      INTEGER :: expmax, i, NH
      DOUBLE PRECISION :: c1, c2, c3, c4, ff, fnum, fden, xLN2, p
      DOUBLE PRECISION :: caq, ca, re0, re0q, pdl, pdlq, xp, xpq, V(NS)
!***********************************************************************
      NH=NS/2
      CALL LINVST(NS,NH,V)
      xLN2=LOG(2.0)
!expmax is the maximum allowable absolute value of the exponential arguments
      expmax=708
      xp=0.0
      xpq=0.0
      DO i=1,NS
        p=xLN2*i/td
!calculate coeficients
        c1 = SQRT(p)
        c2 = p
        fnum = EXP(DBLE( -2.0*SQRT(p)*(tot_wid-xd) )) +1.0
        fden = EXP(DBLE( -2.0*SQRT(p)*bank_wid )) +1.0
        ff = fnum/fden
        c3 = fden
        c4 = c2*c3
        caq = -(c1/c4)*(EXP(DBLE( -2.0*SQRT(p)*bank_wid )) -1.0)
        ca = c1*(xd-1.0)
        IF (ca > expmax) ca = expmax
!calculate head and seepage terms
        re0 = ff*EXP(-ca)
        re0q = caq
        pdl = re0/c2
        pdlq = re0q
        xp = xp + V(i)*pdl
        xpq = xpq + V(i)*pdlq
      ENDDO
      head_step = xp*xLN2/td
      head_step_grad = xpq*xLN2/td

      END SUBROUTINE LTST1

!***********************************************************************
!     Stehfest coefficients for Laplace transform
!***********************************************************************
      SUBROUTINE LINVST(NS, NH, V)
      IMPLICIT NONE
! Functions
      INTRINSIC FLOOR
! Arguments
      INTEGER, INTENT(IN) :: NS,NH
      DOUBLE PRECISION, INTENT(OUT) :: V(NS)
! Local Variables
      INTEGER :: i, j, FI, SN, K1,K2
      DOUBLE PRECISION :: G(NS), HS(NH)
!***********************************************************************
      G(1)=1.0
      DO i=2,NS
        G(i)=G(i-1)*i
      ENDDO
      HS(1)=2.0/G(NH-1)
      DO i = 2,NH
        FI=i
        IF (i== NH) THEN
          HS(i)=(FI**NH)*G(2*i)/(G(i)*G(i-1))
        ELSE
          HS(i)=(FI**NH)*G(2*i)/(G(NH-i)*G(i)*G(i-1))
        ENDIF
      ENDDO
      SN=2*(NH-NH/2*2)-1
      DO i=1,NS
        V(i)=0.0
        K1=FLOOR((i+1)/2.0)
        K2=i
        IF (K2 > NH) K2 = NH
        DO j=K1,K2
          IF (2*j-i == 0) THEN
            V(i)=V(i)+HS(j)/(G(i-j))
          ELSEIF (i == j) THEN
            V(i)=V(i)+HS(j)/G(2*j-i)
          ELSE
            V(i)=V(i)+HS(j)/(G(i-j)*G(2*j-i))
          ENDIF
        ENDDO
        V(i)=SN*V(i)
        SN=-SN
      ENDDO

      END SUBROUTINE LINVST

!***********************************************************************
!     routing_restart - write or read restart file
!***********************************************************************
      SUBROUTINE routing_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Ripst_flag
      USE PRMS_ROUTING
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variables
      CHARACTER(LEN=7) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_segment_storage
        WRITE ( Restart_outunit ) Segment_delta_flow
        IF ( Ripst_flag==1 ) THEN
          WRITE ( Restart_outunit ) Basin_bankst_head
          WRITE ( Restart_outunit ) Basin_bankst_vol
          WRITE ( Restart_outunit ) Basin_bankst_seep_rate
          WRITE ( Restart_outunit ) Basin_bankst_seep
          WRITE ( Restart_outunit ) Bankst_head, Seg_bankflow
          WRITE ( Restart_outunit ) Bankst_head_pts
          WRITE ( Restart_outunit ) Bankst_stor_hru
          WRITE ( Restart_outunit ) Stage_ante, Stage_ts
          WRITE ( Restart_outunit ) Basin_ripst_evap, Basin_ripst_contrib
          WRITE ( Restart_outunit ) Basin_ripst_vol, Basin_ripst_area
          WRITE ( Restart_outunit ) Ripst_stor_hru, Ripst_vol
          WRITE ( Restart_outunit ) Seg_ripflow, Ripst_evap_hru, Ripst_frac
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_segment_storage
        READ ( Restart_inunit ) Segment_delta_flow
        IF ( Ripst_flag==1 ) THEN
          READ ( Restart_inunit ) Basin_segment_storage
          READ ( Restart_inunit ) Segment_delta_flow
          READ ( Restart_inunit ) Basin_bankst_head
          READ ( Restart_inunit ) Basin_bankst_vol
          READ ( Restart_inunit ) Basin_bankst_seep_rate
          READ ( Restart_inunit ) Basin_bankst_seep
          READ ( Restart_inunit ) Bankst_head, Seg_bankflow
          READ ( Restart_inunit ) Bankst_head_pts
          READ ( Restart_inunit ) Bankst_stor_hru
          READ ( Restart_inunit ) Stage_ante, Stage_ts
          READ ( Restart_inunit ) Basin_ripst_evap, Basin_ripst_contrib
          READ ( Restart_inunit ) Basin_ripst_vol, Basin_ripst_area
          READ ( Restart_inunit ) Ripst_stor_hru, Ripst_vol
          READ ( Restart_inunit ) Seg_ripflow, Ripst_evap_hru, Ripst_frac
        ENDIF
      ENDIF
      END SUBROUTINE routing_restart
