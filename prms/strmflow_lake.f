!***********************************************************************
! Computes basin streamflow and on-channel reservoir storage & outflows
!
! gwflow goes to GWR instead of to the lake unless specified as
! going to stream segment associated with the lake, which would be a
! problem, thus gw_upslope usually goes to GWR under the lake,
! but is included in strm_seg_in if gwflow is associated with a stream
! segment, set in gwflow, 06/15/2009
!***********************************************************************
      MODULE PRMS_STRMFLOW_LAKE
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Nhrup1, Mxnsos, Num, Ngate, Nstage
      INTEGER, SAVE :: Ngate2, Nstage2, Ngate3, Nstage3, Ngate4, Nstage4
      DOUBLE PRECISION, SAVE :: Cfs2acft
      REAL, SAVE, ALLOCATABLE :: C24(:,:), S24(:,:),Wvd(:,:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Q_down(:)
      CHARACTER(LEN=13), PARAMETER :: MODNAME = 'strmflow_lake'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Streamflow Routing'
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_lake_stor, Basin_2ndstflow
      REAL, SAVE, ALLOCATABLE :: Din1(:), Elevlake(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_outq(:), Lake_outq2(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_outcms(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_outvol(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_invol(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_vol(:), Lake_sto(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Q_segment(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_stream_in(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_precip(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_sroff(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_interflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_seep_in(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_evap(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_2gw(:)
!   Declared Parameters
      !rsr, no need for hru_sfres and sfres_gw
      INTEGER, SAVE, ALLOCATABLE :: Lake_type(:), Nsos(:), Lake_out2(:)
      INTEGER, SAVE, ALLOCATABLE :: Ratetbl_lake(:), Obsout_lake(:)
      REAL, SAVE, ALLOCATABLE :: Lake_qro(:), Lake_din1(:)
      REAL, SAVE, ALLOCATABLE :: Lake_init(:), Lake_coef(:)
      REAL, SAVE, ALLOCATABLE :: O2(:,:), S2(:,:)
      REAL, SAVE, ALLOCATABLE :: Rate_table(:,:), Rate_table2(:,:)
      REAL, SAVE, ALLOCATABLE :: Rate_table3(:,:), Rate_table4(:,:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage(:), Tbl_gate(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage2(:), Tbl_gate2(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage3(:), Tbl_gate3(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage4(:), Tbl_gate4(:)
      REAL, SAVE, ALLOCATABLE :: Lake_vol_init(:)
      REAL, SAVE, ALLOCATABLE :: Weir_coef(:), Weir_len(:)
      REAL, SAVE, ALLOCATABLE :: Elev_outflow(:), Elevlake_init(:)
      REAL, SAVE, ALLOCATABLE :: Lake_out2_a(:), Lake_out2_b(:)
      END MODULE PRMS_STRMFLOW_LAKE

!***********************************************************************
!     Main daily and storm stream flow routine
!***********************************************************************
      INTEGER FUNCTION strmflow_lake()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: strmlkdecl, strmlkinit, strmlkrun
      INTEGER, EXTERNAL :: strmlksetdims
!***********************************************************************
      strmflow_lake = 0

      IF ( Process(:3)=='run' ) THEN
        strmflow_lake = strmlkrun()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        strmflow_lake = strmlksetdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        strmflow_lake = strmlkdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        strmflow_lake = strmlkinit()
      ENDIF

      END FUNCTION strmflow_lake

!***********************************************************************
!     strmlksetdims - declares strmflow_lake module specific dimensions
!***********************************************************************
      INTEGER FUNCTION strmlksetdims()
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL read_error
! Local Variables
      ! Maximum values are no longer limits
      INTEGER, PARAMETER :: MAXDIM = 500
!***********************************************************************
      strmlksetdims = 1

      IF ( decldim('ngate', 0, MAXDIM,
     +     'Maximum number of reservoir gate-opening values'//
     +     ' (columns) for lake rating table 1')/=0 )
     +     CALL read_error(7, 'ngate')
      IF ( decldim('nstage', 0, MAXDIM,
     +     'Maximum number of lake elevations values (rows) for'//
     +     ' lake rating table 1')/=0 ) CALL read_error(7, 'nstage')

      IF ( decldim('ngate2', 0, MAXDIM,
     +     'Maximum number of reservoir gate-opening values'//
     +     ' (columns) for lake rating table 2')/=0 )
     +     CALL read_error(7, 'ngate2')
      IF ( decldim('nstage2', 0, MAXDIM,
     +     'Maximum number of lake elevations values (rows) for'//
     +     ' lake rating table 2')/=0 )
     +     CALL read_error(7, 'nstage2')

      IF ( decldim('ngate3', 0, MAXDIM,
     +     'Maximum number of reservoir gate-opening values'//
     +     ' (columns) for lake rating table 3')/=0 )
     +     CALL read_error(7, 'ngate3')
      IF ( decldim('nstage3', 0, MAXDIM,
     +     'Maximum number of lake elevations values (rows) for'//
     +     ' lake rating table 3')/=0 )
     +     CALL read_error(7, 'nstage3')

      IF ( decldim('ngate4', 0, MAXDIM,
     +     'Maximum number of reservoir gate-opening values'//
     +     ' (columns) for lake rating table 4')/=0 )
     +     CALL read_error(7, 'ngate4')
      IF ( decldim('nstage4', 0, MAXDIM,
     +     'Maximum number of lake elevations values (rows)'//
     +     ' for lake rating table 4')/=0 )
     +     CALL read_error(7, 'nstage4')

      IF ( decldim('mxnsos', 0, MAXDIM,
     +     'Maximum number of storage/outflow table values for'//
     +     ' storage-detention reservoirs and lakes connected to'//
     +     ' the stream network using Puls routing')/=0 )
     +     CALL read_error(7, 'mxnsos')

      strmlksetdims = 0
      END FUNCTION strmlksetdims

!***********************************************************************
!    strmlkdecl - set up parameters for streamflow and lake flow
!                 computations
!   Declared Parameters
!     lake_type, lake_init, lake_qro, lake_din1
!     lake_coef, o2, s2, nsos, hru_area, lake_hru
!     lake_out2, tbl_stage, tbl_gate, lake_vol_init, rate_table
!     weir_coef, weir_len, elev_outflow, elevlake_init, lake_out2_a
!     lake_out2_b
!***********************************************************************
      INTEGER FUNCTION strmlkdecl()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Model, Nhru, Nsegment, Nlake,
     +    Version_strmflow_lake, Strmflow_lake_nc
      USE PRMS_CASCADE, ONLY: Cascade_flag
      USE PRMS_OBS, ONLY: Nratetbl
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar, getdim
      EXTERNAL read_error
! Local Variables
      INTEGER :: n
!***********************************************************************
      strmlkdecl = 1

      Version_strmflow_lake =
     +'$Id: strmflow_lake.f 4794 2012-08-30 19:10:49Z rsregan $'
      Strmflow_lake_nc = INDEX( Version_strmflow_lake, 'Z' )
      n = INDEX( Version_strmflow_lake, '.f' ) + 1
      IF ( declmodule(Version_strmflow_lake(6:n), PROCNAME,
     +     Version_strmflow_lake(n+2:Strmflow_lake_nc))/=0 ) STOP

      Nhrup1 = Nhru + 1

      IF ( Nratetbl>4 ) THEN
        PRINT *, 'Lake module allows maximum of 4 rating tables'
        STOP
      ENDIF

! redeclare dimensions for Project Chief to know these should default to 1
      Ngate = 0
      Nstage = 0
      Ngate2 = 0
      Nstage2 = 0
      Ngate3 = 0
      Nstage3 = 0
      Ngate4 = 0
      Nstage4 = 0
      IF ( Nratetbl>0 ) THEN
        Ngate = getdim('ngate')
        IF ( Ngate==-1 ) CALL read_error(6, 'ngate')
        Nstage = getdim('nstage')
        IF ( Nstage==-1 ) CALL read_error(6, 'nstage')

        IF ( Nratetbl>1 ) THEN
          Ngate2 = getdim('ngate2')
          IF ( Ngate2==-1 ) CALL read_error(6, 'ngate2')
          Nstage2 = getdim('nstage2')
          IF ( Nstage2==-1 ) CALL read_error(6, 'nstage2')

          IF ( Nratetbl>2 ) THEN
            Ngate3 = getdim('ngate3')
            IF ( Ngate3==-1 ) CALL read_error(6, 'ngate3')
            Nstage3 = getdim('nstage3')
            IF ( Nstage3==-1 ) CALL read_error(6, 'nstage3')

            IF ( Nratetbl==4 ) THEN
              Ngate4 = getdim('ngate4')
              IF ( Ngate4==-1 ) CALL read_error(6, 'ngate4')
              Nstage4 = getdim('nstage4')
              IF ( Nstage4==-1 ) CALL read_error(6, 'nstage4')
            ENDIF
          ENDIF
        ENDIF
      ENDIF

! Dimension for Puls routing
      Mxnsos = getdim('mxnsos')
      IF ( Mxnsos==-1 ) CALL read_error(1, 'mxnsos')

! Declared Variables for all lake types
      ALLOCATE ( Q_segment(Nsegment) )
      IF ( declvar(MODNAME, 'q_segment', 'nsegment', Nsegment,
     +     'double',
     +     'Outflow from each stream segment',
     +     'cfs',
     +     Q_segment)/=0 ) CALL read_error(3, 'q_segment')

      ALLOCATE ( Lake_outq(Nlake) )
      IF ( declvar(MODNAME, 'lake_outq', 'nlake', Nlake, 'double',
     +   'Streamflow leaving each lake, includes in second outlet flow',
     +     'cfs',
     +     Lake_outq)/=0 ) CALL read_error(3, 'lake_outq')

      ALLOCATE ( Lake_outcms(Nlake) )
      IF ( declvar(MODNAME, 'lake_outcms', 'nlake', Nlake, 'double',
     +   'Streamflow leaving each lake, includes in second outlet flow',
     +     'cms',
     +     Lake_outcms)/=0 ) CALL read_error(3, 'lake_outcms')

! Declared Variables for Puls or linear routing
      ALLOCATE ( Lake_sto(Nlake) )
      IF ( declvar(MODNAME, 'lake_sto', 'nlake', Nlake, 'double',
     +     'Storage in each lake using Puls or linear storage'//
     +     ' routing',
     +     'cfs-days',
     +     Lake_sto)/=0 ) CALL read_error(3, 'lake_sto')

      ALLOCATE ( Din1(Nlake) )
      IF ( declvar(MODNAME, 'din1', 'nlake', Nlake, 'real',
     +     'Inflow from the previous time step to each lake'//
     +     ' using Puls or linear storage routing',
     +     'cfs',
     +     Din1)/=0 ) CALL read_error(3, 'din1')

      ALLOCATE ( Lake_stream_in(Nlake) )
      IF ( declvar(MODNAME, 'lake_stream_in', 'nlake', Nlake, 'double',
     +     'Total streamflow into each lake',
     +     'cfs', Lake_stream_in)/=0 )
     +     CALL read_error(3, 'lake_stream_in')

      ALLOCATE ( Lake_precip(Nlake) )
      IF ( declvar(MODNAME, 'lake_precip', 'nlake', Nlake, 'double',
     +     'Total precipitation into each lake',
     +     'cfs', Lake_precip)/=0 ) CALL read_error(3, 'lake_precip')

      IF ( Cascade_flag==1 ) THEN
        ALLOCATE ( Lake_sroff(Nlake) )
        IF ( declvar(MODNAME, 'lake_sroff', 'nlake', Nlake, 'double',
     +       'Total surface runoff into each lake',
     +       'cfs', Lake_sroff)/=0 ) CALL read_error(3, 'lake_sroff')
        ALLOCATE ( Lake_interflow(Nlake) )
        IF ( declvar(MODNAME, 'lake_interflow', 'nlake', Nlake,'double',
     +       'Total interflow into each lake',
     +       'cfs', Lake_interflow)/=0 )
     +       CALL read_error(3, 'lake_interflow')
      ENDIF

      ALLOCATE ( Lake_seep_in(Nlake) )
      IF ( declvar(MODNAME, 'lake_seep_in', 'nlake', Nlake, 'double',
     +     'Total interflow into each lake',
     +     'cfs', Lake_seep_in)/=0 )
     +     CALL read_error(3, 'lake_seep_in')

      ALLOCATE ( Lake_evap(Nlake) )
      IF ( declvar(MODNAME, 'lake_evap', 'nlake', Nlake, 'double',
     +     'Total evaporation from each lake',
     +     'cfs', Lake_evap)/=0 )
     +     CALL read_error(3, 'lake_evap')

      ALLOCATE ( Lake_2gw(Nlake) )
      IF ( declvar(MODNAME, 'lake_2gw', 'nlake', Nlake, 'double',
     +     'Total seepage from each lake',
     +     'inches', Lake_2gw)/=0 ) CALL read_error(3, 'lake_2gw')

! Declared Variables for broad-crested weir or gate opening routing
      ALLOCATE ( Lake_vol(Nlake) )
      IF ( declvar(MODNAME, 'lake_vol', 'nlake', Nlake, 'double',
     +     'Storage in each lake using broad-crested weir or gate'//
     +     ' opening routing',
     +     'acre-feet',
     +     Lake_vol)/=0 ) CALL read_error(3, 'lake_vol')

      ALLOCATE ( Elevlake(Nlake) )
      IF ( declvar(MODNAME, 'elevlake', 'nlake', Nlake, 'real',
     +     'Elevation of each lake using broad-crested weir or gate'//
     +     ' opening routing',
     +     'feet',
     +     Elevlake)/=0 ) CALL read_error(3, 'elevlake')

      ALLOCATE ( Lake_invol(Nlake) )
      IF ( declvar(MODNAME, 'lake_invol', 'nlake', Nlake, 'double',
     +     'Inflow to each lake using'//
     +     ' broad-crested weir or gate opening routing',
     +     'acre-feet',
     +     Lake_invol)/=0 ) CALL read_error(3, 'lake_invol')

      IF ( declvar(MODNAME, 'basin_lake_stor', 'one', 1, 'double',
     +     'Basin volume-weighted average storage for all lakes using'//
     +     ' broad-crested weir or gate opening routing',
     +     'inches',
     +     Basin_lake_stor)/=0 )
     +     CALL read_error(3, 'basin_lake_stor')

! Declared Variables for gate opening routing
      ALLOCATE ( Lake_outvol(Nlake) )
      IF ( declvar(MODNAME, 'lake_outvol', 'nlake', Nlake, 'double',
     +     'Outflow from each lake using gate opening routing',
     +     'acre-inches',
     +     Lake_outvol)/=0 ) CALL read_error(3, 'lake_outvol')

! Declared Variables for lakes with a second outlet and gate opening routing
      IF ( declvar(MODNAME, 'basin_2ndstflow', 'one', 1, 'double',
     +     'Basin volume-weighted average streamflow from each lake'//
     +     ' with a second outlet',
     +     'inches',
     +     Basin_2ndstflow)/=0 ) CALL read_error(3, 'basin_2ndstflow')

      ALLOCATE ( Lake_outq2(Nlake) )
      IF ( declvar(MODNAME, 'lake_outq2', 'nlake', Nlake, 'double',
     +     'Streamflow from second outlet for each lake'//
     +     ' with a second outlet',
     +     'cfs',
     +     Lake_outq2)/=0 ) CALL read_error(3, 'lake_outq2')

! Declared Parameters for all lake types
      ALLOCATE ( Lake_type(Nlake) )
      IF ( declparam(MODNAME, 'lake_type', 'nlake', 'integer',
     +     '1', '1', '6',
     +     'Type of lake',
     +     'Type of lake (1=Puls routing;'//
     +     ' 2=Linear routing; 3=Flow through;'//
     +     ' 4=Broad crested weir; 5=Gate opening; 6=measured flow)',
     +     'none')/=0 ) CALL read_error(1, 'lake_type')

      ALLOCATE ( Lake_qro(Nlake) )
      IF ( declparam(MODNAME, 'lake_qro', 'nlake', 'real',
     +     '0.1', '0.0', '1.0E7',
     +     'Initial daily mean outflow from each lake',
     +     'Initial daily mean outflow from each lake',
     +     'cfs')/=0 ) CALL read_error(1, 'lake_qro')

! Declared Parameters for Puls or linear routing
      ALLOCATE ( Lake_init(Nlake) )
      IF ( declparam(MODNAME, 'lake_init', 'nlake', 'real',
     +     '0.0', '0.0', '2.0E6',
     +     'Initial storage in each lake',
     +     'Initial storage in each lake using Puls or linear'//
     +     ' storage routing',
     +     'cfs-days')/=0 ) CALL read_error(1, 'lake_init')

      ALLOCATE ( Lake_din1(Nlake) )
      IF ( declparam(MODNAME, 'lake_din1', 'nlake', 'real',
     +     '0.1', '0.0', '1.0E7',
     +     'Initial inflow to each lake',
     +     'Initial inflow to each lake using Puls or linear'//
     +     ' storage routing',
     +     'cfs')/=0 ) CALL read_error(1, 'lake_din1')

! Declared Parameters for linear routing
      ALLOCATE ( Lake_coef(Nlake) )
      IF ( declparam(MODNAME, 'lake_coef', 'nlake', 'real',
     +     '0.1', '0.0001', '1.0',
     +     'Linear lake routing coefficient',
     +     'Coefficient in equation to route storage to'//
     +     ' streamflow for each lake using linear routing',
     +     '1/day')/=0 ) CALL read_error(1, 'lake_coef')

! Declared Parameters for Puls routing
      IF ( Mxnsos>0 .OR. Model==99 ) THEN
        ALLOCATE ( O2(Mxnsos, Nlake) )
        IF ( declparam(MODNAME, 'o2', 'mxnsos,nlake', 'real',
     +      '0.0', '0.0', '100000.0',
     +      'Outflow values in outflow/storage tables for Puls routing',
     +      'Outflow values in outflow/storage tables for each lake'//
     +      ' using Puls routing',
     +       'cfs')/=0 ) CALL read_error(1, 'o2')

        ALLOCATE ( S2(Mxnsos, Nlake) )
        IF ( declparam(MODNAME, 's2', 'mxnsos,nlake', 'real',
     +      '0.0', '0.0', '100000.0',
     +      'Storage values in outflow/storage tables for Puls routing',
     +      'Storage values in outflow/storage table for each lake'//
     +      ' using Puls routing',
     +      'cfs-days')/=0 ) CALL read_error(1, 's2')

        ALLOCATE ( Nsos(Nlake) )
        IF ( declparam(MODNAME, 'nsos', 'nlake', 'integer',
     +     '0', '0', '10',
     +     'Number of storage/outflow values in table for Puls routing',
     +     'Number of storage/outflow values in table for each lake'//
     +     ' using Puls routing',
     +     'none')/=0 ) CALL read_error(1, 'nsos')
      ENDIF

! Declared Parameters for broad-crested weir or gate opening routing
      ALLOCATE ( Elevlake_init(Nlake) )
      IF ( declparam(MODNAME, 'elevlake_init', 'nlake', 'real',
     +     '100.0', '0.0', '10000.0',
     +     'Initial lake surface elevation',
     +     'Initial lake surface elevation for each lake using'//
     +     ' broad-crested weir or gate opening routing',
     +     'feet')/=0 ) CALL read_error(1, 'elevlake_init')

      ALLOCATE ( Lake_vol_init(Nlake) )
      IF ( declparam(MODNAME, 'lake_vol_init', 'nlake', 'real',
     +     '100.0', '0.0', '10000.0',
     +     'Initial lake volume',
     +     'Initial lake volume for each lake using broad-crested'//
     +     ' weir or gate opening routing',
     +     'acre-feet')/=0 ) CALL read_error(1, 'lake_vol_init')

! Declared Parameters for broad-crested weir routing
      ALLOCATE ( Weir_coef(Nlake) )
      IF ( declparam(MODNAME, 'weir_coef', 'nlake', 'real',
     +    '2.7', '2.0', '3.0',
     +    'Broad-crested weir coefficent',
     +    'Coefficient for each lake using broad-crested weir equation',
     +    'none')/=0 ) CALL read_error(1, 'weir_coef')

      ALLOCATE ( Weir_len(Nlake) )
      IF ( declparam(MODNAME, 'weir_len', 'nlake', 'real',
     +    '5.0', '1.0', '1000.0',
     +    'Broad-crested weir length',
     +    'Weir length for each lake using broad-crested weir equation',
     +    'feet')/=0 ) CALL read_error(1, 'weir_len')

      ALLOCATE ( Elev_outflow(Nlake) )
      IF ( declparam(MODNAME, 'elev_outflow', 'nlake', 'real',
     +     '100.0', '0.0', '10000.0',
     +     'Elevation of the main outflow point',
     +     'Elevation of the main outflow point for each lake'//
     +     ' using broad-crested weir routing',
     +     'feet')/=0 ) CALL read_error(1, 'elev_outflow')

! Declared Parameters for gate opening routing
      ALLOCATE ( Lake_out2(Nlake) )
      IF ( declparam(MODNAME, 'lake_out2', 'nlake', 'integer',
     +     '0', '0', '1',
     +     'Switch to specify a second outlet from a lake',
     +     'Switch to specify a second outlet from each lake'//
     +     ' (0=no; 1=yes)',
     +     'none')/=0 ) CALL read_error(1, 'lake_out2')

      IF ( Nratetbl>0 .OR. Model==99 ) THEN
        IF ( Nstage<1 .OR. Ngate<1 ) THEN
          PRINT *, 'ERROR, nratetbl>0 and nstage or ngate = 0'
          STOP
        ENDIF

        ALLOCATE ( Ratetbl_lake(Nratetbl) )
        IF ( declparam(MODNAME, 'ratetbl_lake','nratetbl','integer',
     +       '1', 'bounded', 'nlake',
     +       'Index of lake associated with each rating table',
     +       'Index of lake associated with each rating table for'//
     +       ' each lake using gate opening routing',
     +       'none')/=0 ) CALL read_error(1, 'ratetbl_lake')

        ALLOCATE ( Rate_table(Nstage,Ngate) )
        IF ( declparam(MODNAME, 'rate_table', 'nstage,ngate', 'real',
     +       '5.0', '1.0', '1000.0',
     +       'Rating table 1 with stage (rows) and gate opening (cols)',
     +       'Rating table with stage (rows) and gate opening'//
     +       ' (cols) for the first lake using gate opening routing',
     +       'cfs')/=0 ) CALL read_error(1, 'rate_table')

        ALLOCATE ( Tbl_stage(Nstage) )
        IF ( declparam(MODNAME, 'tbl_stage', 'nstage', 'real',
     +       '1.0', '0.0', '10000.0',
     +       'Stage values for each row of rating table 1',
     +       'Stage values for each row of the rating table for the'//
     +       ' first lake using gate opening routing',
     +       'feet')/=0 ) CALL read_error(1, 'tbl_stage')

        ALLOCATE ( Tbl_gate(Ngate) )
        IF ( declparam(MODNAME, 'tbl_gate', 'ngate', 'real',
     +       '5.0', '1.0', '1000.0',
     +       'Gate openings for each column of rating table 1',
     +       'Gate openings for each column of the rating table'//
     +       ' for the first lake using gate opening routing',
     +       'inches')/=0 ) CALL read_error(1, 'tbl_gate')

        IF ( Nratetbl>1 .OR. Model==99 ) THEN
          IF ( Nstage2<1 .OR. Ngate2<1 ) THEN
            PRINT *, 'ERROR, nratetbl>1 and nstage2 or ngate2 = 0'
            STOP
          ENDIF
          ALLOCATE ( Rate_table2(Nstage2,Ngate2) )
          IF ( declparam(MODNAME, 'rate_table2', 'nstage2,ngate2',
     +         'real', '5.0', '1.0', '1000.0',
     +       'Rating table 2 with stage (rows) and gate opening (cols)',
     +         'Rating table with stage (rows) and gate opening'//
     +         ' (cols) for the second lake using gate opening routing',
     +         'cfs')/=0 ) CALL read_error(1, 'rate_table2')

          ALLOCATE ( Tbl_stage2(Nstage2) )
          IF ( declparam(MODNAME, 'tbl_stage2', 'nstage2', 'real',
     +         '1.0', '0.0', '10000.0',
     +         'Stage values for each row of rating table 2',
     +         'Stage values for each row of the rating table for the'//
     +         ' second lake using gate opening routing',
     +         'feet')/=0 ) CALL read_error(1, 'tbl_stage2')

          ALLOCATE ( Tbl_gate2(Ngate2) )
          IF ( declparam(MODNAME, 'tbl_gate2', 'ngate2', 'real',
     +         '5.0', '1.0', '1000.0',
     +         'Gate openings for each column of rating table 2',
     +         'Gate openings for each column of the rating table'//
     +         ' for the second lake using gate opening routing',
     +         'inches')/=0 ) CALL read_error(1, 'tbl_gate2')
        ENDIF

        IF ( Nratetbl>2 .OR. Model==99 ) THEN
          IF ( Nstage3<1 .OR. Ngate3<1 ) THEN
            PRINT *, 'ERROR, nratetbl>2 and nstage3 or ngate3 = 0'
            STOP
          ENDIF
          ALLOCATE ( Rate_table3(Nstage3,Ngate3) )
          IF ( declparam(MODNAME, 'rate_table3', 'nstage3,ngate3',
     +         'real', '5.0', '1.0', '1000.0',
     +       'Rating table 3 with stage (rows) and gate opening (cols)',
     +         'Rating table with stage (rows) and gate opening'//
     +         ' (cols) for the third lake using gate opening routing',
     +         'cfs')/=0 ) CALL read_error(1, 'rate_table3')

          ALLOCATE ( Tbl_stage3(Nstage3) )
          IF ( declparam(MODNAME, 'tbl_stage3', 'nstage3', 'real',
     +         '1.0', '0.0', '10000.0',
     +         'Stage values for each row of rating table 3',
     +         'Stage values for each row of the rating table for the'//
     +         ' third lake using gate opening routing',
     +         'ft')/=0 ) CALL read_error(1, 'tbl_stage3')

          ALLOCATE ( Tbl_gate3(Ngate3) )
          IF ( declparam(MODNAME, 'tbl_gate3', 'ngate3', 'real',
     +         '5.0', '1.0', '1000.0',
     +         'Gate openings for each column of rating table 3',
     +         'Gate openings for each column of the rating table'//
     +         ' for the third lake using gate opening routing',
     +         'inches')/=0 ) CALL read_error(1, 'tbl_gate3')
        ENDIF

        IF ( Nratetbl>3.OR. Model==99 ) THEN
          IF ( Nstage4<1 .OR. Ngate4<1 ) THEN
            PRINT *, 'ERROR, nratetbl>3 and nstage4 or ngate4 = 0'
            STOP
          ENDIF
          ALLOCATE ( Rate_table4(Nstage4,Ngate4) )
          IF ( declparam(MODNAME, 'rate_table4', 'nstage4,ngate4',
     +         'real', '5.0', '1.0', '1000.0',
     +       'Rating table 4 with stage (rows) and gate opening (cols)',
     +         'Rating table with stage (rows) and gate opening'//
     +         ' (cols) for the fourth lake using gate opening routing',
     +         'cfs')/=0 ) CALL read_error(1, 'rate_table4')

          ALLOCATE ( Tbl_stage4(Nstage4) )
          IF ( declparam(MODNAME, 'tbl_stage4', 'nstage4', 'real',
     +         '1.0', '0.0', '10000.0',
     +         'Stage values for each row of rating table 4',
     +         'Stage values for each row of the rating table for the'//
     +         ' fourth lake using gate opening routing',
     +         'feet')/=0 ) CALL read_error(1, 'tbl_stage4')

          ALLOCATE ( Tbl_gate4(Ngate4) )
          IF ( declparam(MODNAME, 'tbl_gate4', 'ngate4', 'real',
     +         '5.0', '1.0', '1000.0',
     +         'Gate openings for each column of rating table 4',
     +         'Gate openings for each column of the rating table'//
     +         ' for the fourth lake using gate opening routing',
     +         'inches')/=0 ) CALL read_error(1, 'tbl_gate4')
        ENDIF
      ENDIF

! Declared Parameters for lakes with a second outlet and gate opening routing
      ALLOCATE ( Lake_out2_a(Nlake) )
      IF ( declparam(MODNAME, 'lake_out2_a', 'nlake', 'real',
     +     '1.0', '0.0', '10000.0',
     +     'Outflow coefficient A for each lake with second outlet',
     +     'Coefficient A in outflow equation for each lake'//
     +     ' with a second outlet',
     +     'cfs/ft')/=0 ) CALL read_error(1, 'lake_out2_a')

      ALLOCATE ( Lake_out2_b(Nlake) )
      IF ( declparam(MODNAME, 'lake_out2_b', 'nlake', 'real',
     +     '100.0', '0.0', '10000.0',
     +     'Outflow coefficient A for each lake with second outlet',
     +     'Coefficient B in outflow equation for each lake'//
     +     ' with a second outlet',
     +     'cfs')/=0 ) CALL read_error(1, 'lake_out2_b')

! Declared Parameters for lakes with lake outflow set to measured streamflow
      ALLOCATE ( Obsout_lake(Nlake) )
      IF ( declparam(MODNAME, 'obsout_lake', 'nlake',
     +     'integer',
     +     '0', 'bounded', 'nobs',
     +     'Index of streamflow measurement station that'//
     +     ' specifies outflow from a lake',
     +     'Index of streamflow measurement station that'//
     +     ' specifies outflow from each lake (lake_type=6)',
     +     'none')/=0 ) CALL read_error(1, 'obsout_lake')

      strmlkdecl = 0
      END FUNCTION strmlkdecl

!***********************************************************************
!     strmlkinit - Initialize strmlake module - get parameter values,
!                  compute initial values
!***********************************************************************
      INTEGER FUNCTION strmlkinit()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Nlake
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Basin_area_inv, NEARZERO,
     +    Timestep
      USE PRMS_CASCADE, ONLY: Nwtrbdy, Cascade_flag
      USE PRMS_OBS, ONLY: Nobs, Nratetbl
      IMPLICIT NONE
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: j, k, kk, weir_rate_flag, weir_flag, secondoutflow_flag
      INTEGER :: gate_flag, puls_flag
      INTEGER :: param_problem, obs_flag, linear_flag, puls_lin_flag
      REAL :: tmp
!***********************************************************************
      strmlkinit = 1

      IF ( Nwtrbdy<1 ) STOP 'ERROR, nwtrbdy<1 in strmflow_lake'
      ALLOCATE ( Q_down(Nwtrbdy) )

      IF ( Timestep==1 ) THEN
        Basin_2ndstflow = 0.0D0
        Q_segment = 0.0D0
        Q_down = 0.0D0
        Lake_outq2 = 0.0D0
        Lake_outvol = 0.0D0
        Lake_invol = 0.0D0
        Lake_vol = 0.0D0
        Lake_sto = 0.0D0
        Lake_stream_in = 0.0D0
        Lake_precip = 0.0D0
        Lake_seep_in = 0.0D0
        Lake_evap = 0.0D0
        IF ( Cascade_flag==1 ) THEN
          Lake_sroff = 0.0D0
          Lake_interflow = 0.0D0
        ENDIF
        Lake_2gw = 0.0D0
      ENDIF

      IF ( getparam(MODNAME, 'lake_type', Nlake, 'integer',
     +     Lake_type)/=0 ) CALL read_error(2, 'lake_type')

      IF ( getparam(MODNAME, 'lake_qro', Nlake, 'real',
     +     Lake_qro)/=0 ) CALL read_error(2, 'lake_qro')

      puls_lin_flag = 0
      obs_flag = 0
      linear_flag = 0
      weir_rate_flag = 0
      weir_flag = 0
      gate_flag = 0
      puls_flag = 0
      DO j = 1, Nlake
        Lake_outq(j) = Lake_qro(j)
        Lake_outcms(j) = Lake_qro(j)*CFS2CMS_CONV
        IF ( Lake_type(j)==1 .OR. Lake_type(j)==2 ) THEN
          puls_lin_flag = 1
          IF ( Lake_type(j)==2 ) linear_flag = 1
          IF ( Lake_type(j)==1 ) puls_flag = 1
        ELSEIF ( Lake_type(j)==6 ) THEN
          obs_flag = 1
        ELSEIF ( Lake_type(j)==4 .OR. Lake_type(j)==5 ) THEN
          weir_rate_flag = 1
          IF ( Lake_type(j)==4 ) weir_flag = 1
          IF ( Lake_type(j)==5 ) gate_flag = 1
        ENDIF
      ENDDO

      IF ( gate_flag==1 ) THEN
        IF ( Nratetbl<1 ) STOP
     +       'ERROR, nratetbl = 0 and gate opening routing requested'
        IF ( getparam(MODNAME, 'lake_out2', Nlake, 'integer',
     +       Lake_out2)/=0  ) CALL read_error(2, 'lake_out2')
        DO j = 1, Nlake
          IF ( Lake_out2(j)==1 ) secondoutflow_flag = 1
        ENDDO
        IF ( getparam(MODNAME, 'rate_table', Nstage*Ngate, 'real',
     +       Rate_table)/=0 ) CALL read_error(2, 'rate_table')
        IF ( getparam(MODNAME, 'tbl_stage', Nstage, 'real',
     +       Tbl_stage)/=0 ) CALL read_error(2, 'tbl_stage')
        IF ( getparam(MODNAME, 'tbl_gate', Ngate, 'real',
     +       Tbl_gate)/=0 ) CALL read_error(2, 'tbl_gate')
        IF ( getparam(MODNAME, 'ratetbl_lake', Nratetbl, 'integer',
     +       Ratetbl_lake)/=0 ) CALL read_error(2, 'ratetbl_lake')

        IF ( Nratetbl>1 ) THEN
          IF ( getparam(MODNAME, 'rate_table2',Nstage2*Ngate2,'real',
     +         Rate_table2)/=0 ) CALL read_error(2, 'rate_table2')
          IF ( getparam(MODNAME, 'tbl_stage2', Nstage2, 'real',
     +         Tbl_stage2)/=0 ) CALL read_error(2, 'tbl_stage2')
          IF ( getparam(MODNAME, 'tbl_gate2', Ngate2, 'real',
     +         Tbl_gate2)/=0 ) CALL read_error(2, 'tbl_gate2')
        ENDIF
        IF ( Nratetbl>2 ) THEN
          IF ( getparam(MODNAME, 'rate_table3',Nstage3*Ngate3,'real',
     +         Rate_table3)/=0 ) CALL read_error(2, 'rate_table3')
          IF ( getparam(MODNAME, 'tbl_stage3', Nstage3, 'real',
     +         Tbl_stage3)/=0 ) CALL read_error(2, 'tbl_stage3')
          IF ( getparam(MODNAME, 'tbl_gate3', Ngate3, 'real',
     +         Tbl_gate3)/=0 ) CALL read_error(2, 'tbl_gate3')
        ENDIF
        IF ( Nratetbl>3 ) THEN
          IF ( getparam(MODNAME, 'rate_table4',Nstage4*Ngate4,'real',
     +         Rate_table4)/=0 ) CALL read_error(2, 'rate_table4')
          IF ( getparam(MODNAME, 'tbl_stage4', Nstage4, 'real',
     +         Tbl_stage4)/=0 ) CALL read_error(2, 'tbl_stage4')
          IF ( getparam(MODNAME, 'tbl_gate4', Ngate4, 'real',
     +         Tbl_gate4)/=0 ) CALL read_error(2, 'tbl_gate4')
        ENDIF
      ELSE
        DEALLOCATE ( Lake_out2 )
      ENDIF

      IF ( secondoutflow_flag==1 ) THEN
        IF ( getparam(MODNAME, 'lake_out2_a', Nlake, 'real',
     +       Lake_out2_a)/=0 ) CALL read_error(2, 'lake_out2_a')
        IF ( getparam(MODNAME, 'lake_out2_b', Nlake, 'real',
     +       Lake_out2_b)/=0 ) CALL read_error(2, 'lake_out2_b')
      ELSE
        DEALLOCATE ( Lake_out2_a, Lake_out2_b )
      ENDIF

      IF ( puls_lin_flag==1 ) THEN
        IF ( getparam(MODNAME, 'lake_init', Nlake, 'real',
     +       Lake_init)/=0 ) CALL read_error(2, 'lake_init')
        IF ( getparam(MODNAME, 'lake_din1', Nlake, 'real',
     +       Lake_din1)/=0 ) CALL read_error(2, 'lake_din1')
      ENDIF

      IF ( puls_flag==1 ) THEN
        IF ( Mxnsos==0 ) STOP
     +       'ERROR, dimension mxnsos = 0 and Puls routing requested'
        IF ( getparam(MODNAME, 'o2', Mxnsos*Nlake, 'real', O2)
     +       /=0 ) CALL read_error(2, 'o2')
        IF ( getparam(MODNAME, 's2', Mxnsos*Nlake, 'real', S2)
     +       /=0 ) CALL read_error(2, 's2')
        IF ( getparam(MODNAME, 'nsos', Nlake, 'integer', Nsos)
     +       /=0 ) CALL read_error(2, 'nsos')
        ALLOCATE ( Wvd(Mxnsos, Nlake), S24(Mxnsos, Nlake) )
        ALLOCATE ( C24(Mxnsos, Nlake) )
      ELSEIF ( Mxnsos>0 ) THEN
        DEALLOCATE ( Nsos )
      ENDIF

      IF ( linear_flag==1 ) THEN
        IF ( getparam(MODNAME, 'lake_coef', Nlake, 'real',
     +       Lake_coef)/=0 ) CALL read_error(2, 'lake_coef')
      ELSE
        DEALLOCATE ( Lake_coef )
      ENDIF

      IF ( obs_flag==1 ) THEN
        IF ( getparam(MODNAME, 'obsout_lake', Nlake, 'integer',
     +       Obsout_lake)/=0 ) CALL read_error(2, 'obsout_lake')
      ENDIF

      IF ( weir_rate_flag==1 ) THEN
        IF ( getparam(MODNAME, 'elevlake_init', Nlake, 'real',
     +       Elevlake_init)/=0 ) CALL read_error(2, 'elevlake_init')
        IF ( getparam(MODNAME, 'lake_vol_init', Nlake, 'real',
     +       Lake_vol_init)/=0 ) CALL read_error(2, 'lake_vol_init')
      ENDIF

      IF ( weir_flag==1 ) THEN
        IF ( getparam(MODNAME, 'weir_coef', Nlake, 'real',
     +       Weir_coef)/=0 ) STOP
        IF ( getparam(MODNAME, 'weir_len', Nlake, 'real',
     +       Weir_len)/=0 ) STOP
        IF ( getparam(MODNAME, 'elev_outflow', Nlake, 'real',
     +       Elev_outflow)/=0 ) CALL read_error(2, 'elev_outflow')
      ELSE
        DEALLOCATE ( Weir_coef, Weir_len, Elev_outflow )
      ENDIF

      Basin_lake_stor = 0.0D0
      Din1 = 0.0
      Elevlake = 0.0
      param_problem = 0
      DO j = 1, Nlake
        IF ( Lake_type(j)==1 .OR. Lake_type(j)==2 ) THEN
! stoin, a local variable was removed, unused
!         Stoin(j) = (Lake_init(j)*23.76)*Basin_area_inv
          Lake_sto(j) = Lake_init(j)
          Din1(j) = Lake_din1(j)
          IF ( Lake_type(j)==1 ) THEN
            kk = Nsos(j)
            IF ( kk>Mxnsos ) THEN
              PRINT *, 'ERROR, lake_type = 1, but, nsos>mxnsos, lake:',
     +                 j, ' nsos:', kk, ' mxnsos:', Mxnsos
              STOP
            ENDIF
            IF ( kk<1 ) THEN
              PRINT *, 'ERROR, lake_type = 1, but, nsos<1, lake:',
     +                 j, ' nsos:', kk, ' mxnsos:', Mxnsos
              STOP
            ENDIF
            DO k = 1, kk
              Wvd(k, j) = S2(k, j) + O2(k, j)*0.5
            ENDDO
            DO k = 2, kk
              tmp = Wvd(k, j) - Wvd(k-1, j)
              IF ( ABS(tmp)<NEARZERO ) tmp = 1.0
              S24(k, j) = (O2(k, j)-O2(k-1, j))/tmp
              C24(k, j) = O2(k, j) - S24(k, j)*Wvd(k, j)
            ENDDO
          ELSEIF ( Lake_type(j)==2 ) THEN
            IF ( Lake_coef(j)<NEARZERO ) THEN
              PRINT *, 'Warning, parameter lake_coef<=0 for lake:', j
              PRINT *, '         set to 0.1'
              Lake_coef(j) = 0.1
            ENDIF
          ENDIF
        ELSEIF ( Lake_type(j)==6 ) THEN
          IF ( Obsout_lake(j)==0 .OR. Obsout_lake(j)>Nobs ) THEN
            PRINT *, 'ERROR, invalid measured outflow for lake=', j
            param_problem = 1
          ENDIF
        ELSEIF ( Lake_type(j)==4 .OR. Lake_type(j)==5 ) THEN
          Elevlake(j) = Elevlake_init(j)
          IF ( Lake_vol_init(j)<0.0 ) THEN
            param_problem = 1
            PRINT *, 'ERROR, parameter lake_vol_init<0 for lake:', j
          ENDIF
          Lake_vol(j) = Lake_vol_init(j)
          Basin_lake_stor = Basin_lake_stor + Lake_vol(j)*12.0D0
          IF ( Lake_type(j)==4 ) THEN
            IF ( Elev_outflow(j)<0.0 ) THEN
              param_problem = 1
              PRINT *, 'ERROR, parameter elev_outflow<0 for lake:', j
            ENDIF
          ENDIF
        ELSE
          PRINT *, 'ERROR, invalid lake_type:', Lake_type(j),
     +             ' for lake=', j
          param_problem = 1
        ENDIF
      ENDDO
      IF ( param_problem>0 ) STOP
      Basin_lake_stor = Basin_lake_stor*Basin_area_inv

      DEALLOCATE ( Lake_init, Elevlake_init, Lake_vol_init )
      DEALLOCATE ( Lake_din1, Lake_qro )
      IF ( Mxnsos>0 ) DEALLOCATE ( O2, S2 )

      strmlkinit = 0
      END FUNCTION strmlkinit

!***********************************************************************
!     strmlkrun - Computes basin streamflow and on-channel reservoir
!                 storage and outflows
!***********************************************************************
      INTEGER FUNCTION strmlkrun()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Nsegment, Nlake, Print_debug
!markstro
      USE PRMS_BASIN, ONLY: Basin_area_inv, Hru_area, Basin_cfs,
     +    Basin_cms, Basin_stflow_in, Basin_sroff_cfs, Basin_ssflow_cfs,
     +    Basin_gwflow_cfs, CFS2CMS_CONV, NEARZERO, Lake_hru, DNEARZERO,
     +    Active_hrus, Hru_route_order, Lake_hru_id, Hru_type,
     +    Lake_area,
     +    Basin_stflow_out
!end markstro
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Hru_actet, Basin_sroff,
     +    Strm_seg_in, Hortonian_lakes
      USE PRMS_CASCADE, ONLY: Wtrbdy_down, Wtrbdy_route_order, Nwtrbdy,
     +    Cascade_flag
      USE PRMS_OBS, ONLY: Cfs_conv, Timestep_seconds, Runoff, Gate_ht,
     +    Nratetbl
      USE PRMS_SOILZONE, ONLY: Upslope_interflow, Upslope_dunnianflow
      USE PRMS_GWFLOW, ONLY: Basin_gwflow, Lake_seepage, Gw_seep_lakein
      IMPLICIT NONE
      INTRINSIC EXP, DBLE
      EXTERNAL table_comp
! Local Variables
      INTEGER :: j, jj, k, n, jjj, i
      INTEGER :: iwtrdn, ihru, lakeid
      REAL :: new_elevlake, elevold, head, head2, q1, q3, xkt, c2, fact
      DOUBLE PRECISION :: old_lake_vol, last_stor, q2, avin, evap
      DOUBLE PRECISION :: lake_out, lake_out1, s2o2, area_fac, diff_vol
      DOUBLE PRECISION :: lakein, scnd_cfs1, scnd_cfs2
!***********************************************************************
      Cfs2acft = Timestep_seconds/43560.0D0
      area_fac = Cfs_conv/Basin_area_inv

      Basin_2ndstflow = 0.0D0

      Q_down = 0.0D0
      Q_segment = 0.0D0
      Lake_stream_in = 0.0D0
      Lake_precip = 0.0D0
      IF ( Cascade_flag==1 ) THEN
        Lake_sroff = 0.0D0
        Lake_interflow = 0.0D0
      ENDIF
      Lake_seep_in = 0.0D0
      Lake_evap = 0.0D0
      Lake_2gw = 0.0D0

      DO j = 1, Active_hrus
        k = Hru_route_order(j)
        IF ( Hru_type(k)/=2 ) CYCLE
        fact = Hru_area(k)*Cfs_conv
        lakeid = Lake_hru_id(k)
        Lake_precip(lakeid) = Lake_precip(lakeid) + fact*Hru_ppt(k)
        IF ( Cascade_flag==1 ) THEN
          Lake_sroff(lakeid) = Lake_sroff(lakeid) +
     +                         fact*(Hortonian_lakes(k) +
     +                               Upslope_dunnianflow(k))
          Lake_interflow(lakeid) = Lake_interflow(lakeid) +
     +                             fact*Upslope_interflow(k)
        ENDIF
        Lake_seep_in(lakeid) = Lake_seep_in(lakeid) +
     +                         fact*Gw_seep_lakein(k)
        Lake_evap(lakeid) = Lake_evap(lakeid) + fact*Hru_actet(k)
        Lake_2gw(lakeid) = Lake_2gw(lakeid) + Lake_seepage(k)
      ENDDO

      last_stor = Basin_lake_stor
      Basin_lake_stor = 0.0D0
      Lake_2gw = 0.0D0
      DO j = 1, Nwtrbdy

        k = Wtrbdy_route_order(j)
        iwtrdn = Wtrbdy_down(k)
        ! if a segment add lateral inflow plus upstream flow
        IF ( k<=Nsegment ) THEN
          ! ignore segments in lakes, potential problem
          IF ( iwtrdn==-Nhrup1 ) CYCLE
          ! Strm_seg_in in cfs, q_down in cfs
          Q_segment(k) = Strm_seg_in(k) + Q_down(k)
          IF ( iwtrdn<0 ) THEN
            ! down water body is a lake
            ihru = ABS(iwtrdn)
            DO jj = 1, Nlake
              IF ( ihru==Lake_hru(jj) ) THEN
                iwtrdn = Nsegment + jj
                EXIT
              ENDIF
            ENDDO
          ENDIF
          ! add upstream flow to downstream water body
          IF ( iwtrdn>0 ) Q_down(iwtrdn) = Q_down(iwtrdn) + Q_segment(k)
          CYCLE
        ENDIF

        ! water body is a lake, compute inflows, later add outflow to
        ! downstream segments
        lakeid = k - Nsegment

        ! snowcomp shouldn't have snowpack as all
        ! precipitation should be added directly to lake

        ! units of lakein = cfs
        lakein = Q_down(k) + Lake_precip(lakeid) + Lake_seep_in(lakeid)
        IF ( Cascade_flag==1 ) lakein = lakein + Lake_sroff(lakeid) +
     +                                  Lake_interflow(lakeid) 

        Lake_stream_in(lakeid) = Q_down(k)

        ! q2 = lake out in cfs
        q2 = 0.0D0

!   Set outflow as a measured flow
        IF ( Lake_type(lakeid)==6 ) THEN
          q2 = Runoff(Obsout_lake(lakeid))
          IF ( q2<0.0D0 ) THEN
            PRINT *, 'Observed runoff value specified <0 as outflow',
     +               ' from lake:', lakeid, ' value:', q2
            PRINT *, '     runoff id:', Obsout_lake(lakeid),
     +               ' outflow set to 0.0'
            q2 = 0.0D0
          ENDIF

!   Compute outflow using Puls routing method
        ELSEIF ( Lake_type(lakeid)==1 ) THEN
          !rsr, why half of current in and last in???
          avin = (lakein+Din1(lakeid))*0.5D0
          s2o2 = Lake_sto(lakeid) -
     +           (Lake_evap(lakeid)+Lake_outq(lakeid))*0.5D0
          IF ( s2o2<DNEARZERO ) THEN
            PRINT *, 'Warning 1, lake storage for Puls routing < 0',
     +               ' lake:', lakeid, s2o2, ' set to 0'
            PRINT *, 'Probably need to increase lake_init'
            s2o2 = 0.0D0
          ENDIF
          s2o2 = s2o2 + avin
          n = Nsos(lakeid)
          DO jjj = 2, Nsos(lakeid)
            IF ( s2o2<Wvd(jjj, lakeid) ) THEN
              n = jjj
              EXIT
            ENDIF
          ENDDO
          q2 = S24(n, lakeid)*s2o2 + C24(n, lakeid)

          IF ( q2<DNEARZERO ) q2 = 0.0D0
          Lake_sto(lakeid) = s2o2 - q2*0.5D0
          IF ( Lake_sto(lakeid)<DNEARZERO ) THEN
            PRINT *, 'Warning 2, lake storage for Puls routing < 0',
     +               ' lake:', lakeid, Lake_sto(lakeid), ' set to 0'
            PRINT *, 'Probably need to increase lake_init'
            q2 = s2o2 + (Lake_outq(lakeid))*0.5D0
            Lake_sto(lakeid) = 0.0D0
          ENDIF

!   Compute outflow using linear reservoir method
        ELSEIF ( Lake_type(lakeid)==2 ) THEN
          !rsr, why half of current in and last in???
          avin = (lakein+Din1(lakeid)-Lake_evap(lakeid))*0.5D0
          xkt = Lake_coef(lakeid)
          c2 = 1.0 - EXP(-xkt)
          q2 = (avin*(1.0D0-(c2/xkt))) + Lake_sto(lakeid)*c2
          IF ( q2<DNEARZERO ) q2 = 0.0D0
          Lake_sto(lakeid) = Lake_sto(lakeid) + avin - q2
          IF ( Lake_sto(lakeid)<DNEARZERO ) THEN
            PRINT *, 'Warning, lake storage for linear routing < 0',
     +               ' lake:', lakeid, Lake_sto(lakeid), ' set to 0'
            PRINT *, 'Probably need to increase lake_init'
          ENDIF

!   Compute using flow through reservoir
        ELSEIF ( Lake_type(lakeid)==3 ) THEN
          ! lake_evap is not removed
          q2 = lakein

        ELSE ! 4 or 5; broad-crested weir or gate opening
          elevold = Elevlake(lakeid)
          old_lake_vol = Lake_vol(lakeid)

          ! units lake_invol = acft
          Lake_invol(lakeid) = lakein*Cfs2acft

          ! units lake_out = acft
          evap = Lake_evap(lakeid)*Cfs2acft
          lake_out = ((Lake_2gw(lakeid)+evap)/12.0D0)*Lake_area(lakeid)

          diff_vol = Lake_invol(lakeid) - lake_out

          q1 = 0.0
          q3 = 0.0

!   Compute using lake surface elevation and broad crested weir
          IF ( Lake_type(lakeid)==4 ) THEN

            head = elevold - Elev_outflow(lakeid)
            IF ( head>NEARZERO )
     +           q1 = (head**1.5) * Weir_coef(lakeid) * Weir_len(lakeid)
            lake_out1 = q1*Cfs2acft

            ! new_elevlake has units of feet
            new_elevlake = elevold +
     +                     (diff_vol-lake_out1)/Lake_area(lakeid)

            head2 = ((new_elevlake+elevold)*0.5) - Elev_outflow(lakeid)
            IF ( head2>NEARZERO )
     +          q3 = (head2**1.5) * Weir_coef(lakeid) * Weir_len(lakeid)

!  Compute using a rating table of lake surface elevation & gate opening
          ELSE ! type = 5

            DO i = 1, Nratetbl
              IF ( lakeid==Ratetbl_lake(i) ) THEN
                IF ( i==1 ) THEN
                  CALL table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage,
     +                 Rate_table, elevold, Gate_ht(i), q1,
     +                 Lake_area(lakeid))
                ELSEIF ( i==2 ) THEN
                  CALL table_comp(Ngate2, Nstage2, Tbl_gate2,
     +                 Tbl_stage2, Rate_table2, elevold, Gate_ht(i), q1,
     +                 Lake_area(lakeid))
                ELSEIF ( i==3 ) THEN
                  CALL table_comp(Ngate3, Nstage3, Tbl_gate3,
     +                 Tbl_stage3, Rate_table3, elevold, Gate_ht(i), q1,
     +                 Lake_area(lakeid))
                ELSEIF ( i==4 ) THEN
                  CALL table_comp(Ngate4, Nstage4, Tbl_gate4,
     +                 Tbl_stage4, Rate_table4, elevold, Gate_ht(i), q1,
     +                 Lake_area(lakeid))
                ENDIF
              ENDIF
            ENDDO

!  if lake has a second outlet then outflow in cfs is computed by
!       Q = (Lake_out2_a * Elevlake) - Lake_out2_b
!       (as per Rob Dudley email 7 Sep 2006)
            IF ( Lake_out2(lakeid)==1 ) THEN
              scnd_cfs1 = (Lake_out2_a(lakeid)*elevold) -
     +                     Lake_out2_b(lakeid)
              IF ( scnd_cfs1<DNEARZERO ) scnd_cfs1 = 0.0D0
            ELSE
              scnd_cfs1 = 0.0D0
            ENDIF

            lake_out1 = (q1 + scnd_cfs1)*Cfs2acft

            ! new_elevlake has units of feet
            new_elevlake = elevold +
     +                     (diff_vol-lake_out1)/Lake_area(lakeid)

            DO i = 1, Nratetbl
              IF ( lakeid==Ratetbl_lake(i) ) THEN
                IF ( i==1 ) THEN
                  CALL table_comp(Ngate, Nstage, Tbl_gate,
     +                 Tbl_stage, Rate_table, new_elevlake,
     +                 Gate_ht(i), q3, Lake_area(lakeid))
                ELSEIF ( i==2 ) THEN
                  CALL table_comp(Ngate2, Nstage2, Tbl_gate2,
     +                 Tbl_stage2, Rate_table2, new_elevlake,
     +                 Gate_ht(i), q3, Lake_area(lakeid))
                ELSEIF ( i==3 ) THEN
                  CALL table_comp(Ngate3, Nstage3, Tbl_gate3,
     +                 Tbl_stage3, Rate_table3, new_elevlake,
     +                 Gate_ht(i), q3, Lake_area(lakeid))
                ELSEIF ( i==4 ) THEN
                  CALL table_comp(Ngate4, Nstage4, Tbl_gate4,
     +                 Tbl_stage4, Rate_table4, new_elevlake,
     +                 Gate_ht(i), q3, Lake_area(lakeid))
                ENDIF
              ENDIF
            ENDDO

            IF ( Lake_out2(lakeid)==1 ) THEN
              scnd_cfs2 = (Lake_out2_a(lakeid)*new_elevlake) -
     +                    Lake_out2_b(lakeid)
              IF ( scnd_cfs2<DNEARZERO ) scnd_cfs2 = 0.0D0
            ELSE
              scnd_cfs2 = 0.0D0
            ENDIF

            Lake_outq2(lakeid) = (scnd_cfs1+scnd_cfs2)*0.5D0
            Basin_2ndstflow = Basin_2ndstflow +
     +                        Lake_outq2(lakeid)*Cfs2acft*12.0D0
          ENDIF

          q2 = (q1+q3)*0.5

!         !sanity check, rsr
          if(q2<0.0D0) print *, 'q2<0', q2, ' lake:', lakeid

          Lake_outvol(lakeid) = q2*Cfs2acft + lake_out +
     +                          Lake_outq2(lakeid)

          ! adjust lake storage
          Lake_vol(lakeid) = Lake_vol(lakeid) - Lake_outvol(lakeid)
          IF ( Lake_vol(lakeid)<DNEARZERO ) THEN
            PRINT *, 'Lake storage issue, lake_vol<0:', Lake_vol(lakeid)
     +               , ' lake:', lakeid
            IF ( q2*Cfs2acft>Lake_vol(lakeid) ) THEN
              PRINT *, 'stream flow out reduced and storage set to 0'
              q2 = q2 - Lake_vol(lakeid)/Cfs2acft
              Lake_outvol(lakeid) = q2*Cfs2acft + lake_out +
     +                              Lake_outq2(lakeid)
              Lake_vol(lakeid) = 0.0D0
            ELSE
              STOP 'ERROR, negative storage > available streamflow out'
            ENDIF
          ENDIF

          ! adjust lake elevation with stream and lateral inflows
          ! and streamflow, second outlet, GWR, and evaporation outflows
          Elevlake(lakeid) = Elevlake(lakeid) +
     +                       (Lake_invol(lakeid)-Lake_outvol(lakeid))/
     +                       Lake_area(lakeid)

          Basin_lake_stor = Basin_lake_stor + Lake_vol(lakeid)*12.0D0
        ENDIF

        ! could have more than one stream coming into segment plus lake outflow
        IF ( iwtrdn>0 ) Q_down(iwtrdn) = Q_down(iwtrdn) + q2
        Lake_outq(lakeid) = q2 + Lake_outq2(lakeid)
        Lake_outcms(lakeid) = q2*CFS2CMS_CONV

      ENDDO   ! end segment routing loop

      Basin_cfs = Q_segment(Nsegment) !rsr Nsegment must be last segment
      Basin_cms = Basin_cfs*CFS2CMS_CONV

! markstro not sure what to do here
      Basin_stflow_in = Basin_cfs/area_fac
      Basin_stflow_out = Basin_stflow_in
! end markstro
      Basin_lake_stor = Basin_lake_stor*Basin_area_inv
      Basin_2ndstflow = Basin_2ndstflow*Basin_area_inv

      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac

      strmlkrun = 0
      END FUNCTION strmlkrun

!=====================================================================
!    Rating table computation
!=====================================================================
      SUBROUTINE table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage,
     +           Rate_table, Elevlake, Gate_ht, Q2, Lake_area)
      USE PRMS_STRMFLOW_LAKE, ONLY: Cfs2acft
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ngate, Nstage
      REAL, INTENT(IN) :: Tbl_gate(Ngate), Tbl_stage(Nstage)
      REAL, INTENT(IN) :: Rate_table(Nstage, Ngate), Gate_ht, Elevlake
      DOUBLE PRECISION, INTENT(IN) :: Lake_area
      REAL, INTENT(OUT) :: Q2
! Local Variables
      INTEGER m, mm, stg1, stg2, gate1, gate2
      REAL :: diff_q_stg1, diff_q_stg2, ratiog, ratios
      REAL :: q_stg1, q_stg2, diffq
!***********************************************************************
      IF ( Elevlake<Tbl_stage(Nstage) ) THEN
        Q2 = 0.0

      ELSEIF ( Elevlake>Tbl_stage(1) ) THEN
        ! lake elevation is > maximum stage, spill all water
        Q2 = (Elevlake-Tbl_stage(1))*Lake_area/Cfs2acft
        PRINT*,'Warning, lake elevation > maximum stage in rating table'
     +         , ' all water above rating table spills'
        PRINT *, 'Lake elevation:', Elevlake, ' Rating table stage:',
     +           Tbl_stage(1), ' discharge to stream:', Q2
      ELSE
        stg2 = 1
        stg1 = 0
        DO m = 1, Nstage
          IF ( Elevlake>Tbl_stage(m) ) THEN
            IF ( m==1 ) THEN
              stg2 = 1
              stg1 = 1
            ELSE
              stg2 = m
              stg1 = m - 1
            ENDIF
            EXIT
          ENDIF
        ENDDO

        gate2 = Ngate
        gate1 = Ngate - 1
        IF ( Gate_ht<=Tbl_gate(Ngate) ) THEN
          DO mm = 1, Ngate
            IF ( Tbl_gate(mm)>Gate_ht ) THEN
              IF ( mm==1 ) THEN
                gate2 = 1
                gate1 = 1
              ELSE
                gate2 = mm
                gate1 = mm - 1
              ENDIF
              EXIT
            ENDIF
          ENDDO
        ENDIF

        IF ( stg1==0 ) THEN
          Q2 = Rate_table(1, gate2)

        ELSE

          diff_q_stg2 = Rate_table(stg2, gate2)
     +                  - Rate_table(stg2, gate1)

          diff_q_stg1 = Rate_table(stg1, gate2)
     +                  - Rate_table(stg1, gate1)

 !rsr, possible divide by 0.0???
          ratiog = (Gate_ht-Tbl_gate(gate1))
     +             /(Tbl_gate(gate2)-Tbl_gate(gate1))

          q_stg2 = (ratiog*diff_q_stg2) + Rate_table(stg2, gate1)

          q_stg1 = (ratiog*diff_q_stg1) + Rate_table(stg1, gate1)

 !rsr, possible divide by 0.0???
          ratios = (Elevlake-Tbl_stage(stg2))
     +             /(Tbl_stage(stg1)-Tbl_stage(stg2))

          diffq = q_stg1 - q_stg2

          Q2 = q_stg2 + (ratios*diffq)

        ENDIF
      ENDIF

      END SUBROUTINE table_comp
