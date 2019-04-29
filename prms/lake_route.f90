!***********************************************************************
! Routes water between segments and lakes in the stream network
!
! gwflow goes to GWR instead of to the lake unless specified as
! going to stream segment associated with the lake, which would be a
! problem, thus gw_upslope usually goes to GWR under the lake,
! but is included in strm_seg_in if gwflow is associated with a stream
! segment, set in gwflow, 06/15/2009
!***********************************************************************
      MODULE PRMS_LAKE_ROUTE
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=10), SAVE :: MODNAME
      INTEGER, SAVE :: Linear_flag, Weir_flag, Puls_flag, Weir_gate_flag, Puls_lin_flag, Gate_flag
      INTEGER, SAVE :: Obs_flag
      INTEGER, SAVE, ALLOCATABLE :: Lake_id(:)
      REAL, SAVE, ALLOCATABLE :: C24(:, :), S24(:, :), Wvd(:, :)
!   Dimensions
      INTEGER, SAVE :: Mxnsos, Ngate, Nstage, Ngate2, Nstage2, Ngate3, Nstage3, Ngate4, Nstage4
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Din1(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_outcfs(:), Lake_outcms(:), Lake_outvol(:), Lake_invol(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_vol(:), Lake_sto(:), Lake_inflow(:), Lake_outflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_stream_in(:), Lake_lateral_inflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_precip(:), Lake_sroff(:), Lake_interflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_seep_in(:), Lake_evap(:), Lake_2gw(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Lake_type(:), Obsout_lake(:), Nsos(:), Ratetbl_lake(:)
      REAL, SAVE, ALLOCATABLE :: Lake_qro(:), Lake_coef(:), Elev_outflow(:), Weir_coef(:), Weir_len(:)
      REAL, SAVE, ALLOCATABLE :: O2(:, :), S2(:, :)
      REAL, SAVE, ALLOCATABLE :: Lake_din1(:), Lake_init(:), Lake_vol_init(:)
      REAL, SAVE, ALLOCATABLE :: Rate_table(:, :), Rate_table2(:, :), Rate_table3(:, :), Rate_table4(:, :)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage(:), Tbl_gate(:), Tbl_stage2(:), Tbl_gate2(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage3(:), Tbl_gate3(:), Tbl_stage4(:), Tbl_gate4(:)
      END MODULE PRMS_LAKE_ROUTE

!***********************************************************************
!     Main daily lakes routine
!***********************************************************************
      INTEGER FUNCTION lake_route()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: lakertedecl, lakerteinit, lakertesetdims, lakerterun
      EXTERNAL :: lake_route_restart
!***********************************************************************
      lake_route = 0

      IF ( Process(:3)=='run' ) THEN
        lake_route = lakerterun()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        lake_route = lakertesetdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        lake_route = lakertedecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL lake_route_restart(1)
        lake_route = lakerteinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL lake_route_restart(0)
      ENDIF

      END FUNCTION lake_route

!***********************************************************************
!     declares Lake routing specific dimensions
!***********************************************************************
      INTEGER FUNCTION lakertesetdims()
      USE PRMS_MODULE, ONLY: MAXDIM
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL read_error
!***********************************************************************
      lakertesetdims = 0

      IF ( decldim('ngate', 0, MAXDIM, &
     &     'Maximum number of reservoir gate-opening values (columns) for lake rating table 1')/=0 ) &
     &     CALL read_error(7, 'ngate')
      IF ( decldim('nstage', 0, MAXDIM, &
     &     'Maximum number of lake elevations values (rows) for lake rating table 1')/=0 ) CALL read_error(7, 'nstage')

      IF ( decldim('ngate2', 0, MAXDIM, &
     &     'Maximum number of reservoir gate-opening values (columns) for lake rating table 2')/=0 ) &
     &     CALL read_error(7, 'ngate2')
      IF ( decldim('nstage2', 0, MAXDIM, &
     &     'Maximum number of lake elevations values (rows) for lake rating table 2')/=0 ) CALL read_error(7, 'nstage2')

      IF ( decldim('ngate3', 0, MAXDIM, &
     &     'Maximum number of reservoir gate-opening values (columns) for lake rating table 3')/=0 ) &
     &     CALL read_error(7, 'ngate3')
      IF ( decldim('nstage3', 0, MAXDIM, &
     &     'Maximum number of lake elevations values (rows) for lake rating table 3')/=0 ) CALL read_error(7, 'nstage3')

      IF ( decldim('ngate4', 0, MAXDIM, &
     &     'Maximum number of reservoir gate-opening values (columns) for lake rating table 4')/=0 ) &
     &     CALL read_error(7, 'ngate4')
      IF ( decldim('nstage4', 0, MAXDIM, &
     &     'Maximum number of lake elevations values (rows) for lake rating table 4')/=0 ) CALL read_error(7, 'nstage4')

      IF ( decldim('mxnsos', 0, MAXDIM, &
     &     'Maximum number of storage/outflow table values for storage-detention reservoirs and lakes connected to'// &
     &     ' the stream network using Puls routing')/=0 ) CALL read_error(7, 'mxnsos')

      END FUNCTION lakertesetdims

!***********************************************************************
!     Declare parameters and allocate arrays
!   Declared Parameters
!     lake_type, lake_init, lake_qro, lake_din1, lake_coef, o2, s2, nsos, hru_area, lake_hru
!     tbl_stage, tbl_gate, lake_vol_init, rate_table, weir_coef, weir_len, elev_outflow, elevlake_init
!***********************************************************************
      INTEGER FUNCTION lakertedecl()
      USE PRMS_LAKE_ROUTE
      USE PRMS_MODULE, ONLY: Model, Nsegment, Nlake, Nratetbl, Cascade_flag, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar, getdim
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_lake_route
!***********************************************************************
      lakertedecl = 0

      Version_lake_route = 'lake_route.f90 2015-03-09 22:49:39Z'
      CALL print_module(Version_lake_route, 'Lake Routing                ', 90)
      MODNAME = 'lake_route'

      IF ( Nlake<1 ) THEN
        PRINT *, 'ERROR, strmflow routing with lakes requires nlake > 0, specified as:', Nlake
        STOP
      ENDIF

      ! Dimension for Puls routing
      Mxnsos = getdim('mxnsos')
      IF ( Mxnsos==-1 ) CALL read_error(1, 'mxnsos')
      IF ( Model==99 .AND. Mxnsos<1 ) Mxnsos = 1

      ALLOCATE ( Lake_id(Nsegment) )
      IF ( Mxnsos>0 ) ALLOCATE ( Wvd(Mxnsos, Nlake), S24(Mxnsos, Nlake), C24(Mxnsos, Nlake) )

      Ngate = 0
      Nstage = 0
      Ngate2 = 0
      Nstage2 = 0
      Ngate3 = 0
      Nstage3 = 0
      Ngate4 = 0
      Nstage4 = 0
      IF ( Nratetbl>4 ) THEN
        IF ( Model==99 ) THEN
          Nratetbl = 4
        ELSE
          PRINT *, 'ERROR, lake routing allows maximum of 4 rating tables'
          PRINT *, 'nratetbl specified as:', Nratetbl
          STOP
        ENDIF
      ENDIF
      IF ( Nratetbl>0 .OR. Model==99 ) THEN
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
        IF ( Model==99 ) THEN
          IF ( Nstage==0 ) Nstage = 1
          IF ( Ngate==0 ) Ngate = 1
          IF ( Nstage2==0 ) Nstage2 = 1
          IF ( Ngate2==0 ) Ngate2 = 1
          IF ( Nstage3==0 ) Nstage3 = 1
          IF ( Ngate3==0 ) Ngate3 = 1
          IF ( Nstage4==0 ) Nstage4 = 1
          IF ( Ngate4==0 ) Ngate4 = 1
        ELSE
          IF ( Nstage<1 .OR. Ngate<1 ) STOP 'ERROR, nratetbl>0 and nstage or ngate = 0'
        ENDIF
        IF ( Nratetbl>1 ) THEN
          IF ( Model/=99 .AND. (Nstage2<1.OR.Ngate2<1) ) STOP 'ERROR, nratetbl>1 and nstage2 or ngate2 = 0'
        ENDIF
        IF ( Nratetbl>2 ) THEN
          IF ( Model/=99 .AND. (Nstage3<1 .OR. Ngate3<1) ) STOP 'ERROR, nratetbl>2 and nstage3 or ngate3 = 0'
        ENDIF
        IF ( Nratetbl>3 ) THEN
          IF ( Model/=99 .AND. (Nstage4<1 .OR. Ngate4<1) ) STOP 'ERROR, nratetbl>3 and nstage4 or ngate4 = 0'
        ENDIF
      ENDIF

      ! Lake declared variables
      ALLOCATE ( Lake_inflow(Nlake) )
      IF ( declvar(MODNAME, 'lake_inflow', 'nlake', Nlake, 'double', &
     &     'Total inflow to each lake', &
     &     'cfs', Lake_inflow)/=0 ) CALL read_error(3, 'lake_inflow')

      ALLOCATE ( Lake_outflow(Nlake) )
      IF ( declvar(MODNAME, 'lake_outflow', 'nlake', Nlake, 'double', &
     &     'Evaporation and seepage from each lake', &
     &     'cfs', Lake_outflow)/=0 ) CALL read_error(3, 'lake_outflow')

      ALLOCATE ( Lake_lateral_inflow(Nlake) )
      IF ( declvar(MODNAME, 'lake_lateral_inflow', 'nlake', Nlake, 'double', &
     &     'Lateral inflow to each lake', &
     &     'cfs', Lake_lateral_inflow)/=0 ) CALL read_error(3, 'lake_lateral_inflow')

      ALLOCATE ( Lake_outcfs(Nlake) )
      IF ( declvar(MODNAME, 'lake_outcfs', 'nlake', Nlake, 'double', &
     &     'Streamflow leaving each lake, includes in second outlet flow', &
     &     'cfs', Lake_outcfs)/=0 ) CALL read_error(3, 'lake_outcfs')

      ALLOCATE ( Lake_outcms(Nlake) )
      IF ( declvar(MODNAME, 'lake_outcms', 'nlake', Nlake, 'double', &
     &     'Streamflow leaving each lake, includes in second outlet flow', &
     &     'cms', Lake_outcms)/=0 ) CALL read_error(3, 'lake_outcms')

! Declared Variables for Puls or linear routing
      ALLOCATE ( Lake_sto(Nlake) )
      IF ( declvar(MODNAME, 'lake_sto', 'nlake', Nlake, 'double', &
     &     'Storage in each lake using Puls or linear storage routing', &
     &     'cfs-days', Lake_sto)/=0 ) CALL read_error(3, 'lake_sto')

      ALLOCATE ( Din1(Nlake) )
      IF ( declvar(MODNAME, 'din1', 'nlake', Nlake, 'real', &
     &     'Inflow from the previous time step to each lake using Puls or linear storage routing', &
     &     'cfs', Din1)/=0 ) CALL read_error(3, 'din1')

      ALLOCATE ( Lake_stream_in(Nlake) )
      IF ( declvar(MODNAME, 'lake_stream_in', 'nlake', Nlake, 'double', &
     &     'Total streamflow into each lake', &
     &     'cfs', Lake_stream_in)/=0 ) CALL read_error(3, 'lake_stream_in')

      ALLOCATE ( Lake_precip(Nlake) )
      IF ( declvar(MODNAME, 'lake_precip', 'nlake', Nlake, 'double', &
     &     'Total precipitation into each lake', &
     &     'cfs', Lake_precip)/=0 ) CALL read_error(3, 'lake_precip')

      IF ( Cascade_flag==1 ) THEN
        ALLOCATE ( Lake_sroff(Nlake) )
        IF ( declvar(MODNAME, 'lake_sroff', 'nlake', Nlake, 'double', &
     &       'Total surface runoff into each lake', &
     &       'cfs', Lake_sroff)/=0 ) CALL read_error(3, 'lake_sroff')
        ALLOCATE ( Lake_interflow(Nlake) )
        IF ( declvar(MODNAME, 'lake_interflow', 'nlake', Nlake,'double', &
     &       'Total interflow into each lake', &
     &       'cfs', Lake_interflow)/=0 ) CALL read_error(3, 'lake_interflow')
      ENDIF

      ALLOCATE ( Lake_seep_in(Nlake) )
      IF ( declvar(MODNAME, 'lake_seep_in', 'nlake', Nlake, 'double', &
     &     'Total seepage into each lake', &
     &     'cfs', Lake_seep_in)/=0 ) CALL read_error(3, 'lake_seep_in')

      ALLOCATE ( Lake_evap(Nlake) )
      IF ( declvar(MODNAME, 'lake_evap', 'nlake', Nlake, 'double', &
     &     'Total evaporation from each lake', &
     &     'cfs', Lake_evap)/=0 ) CALL read_error(3, 'lake_evap')

      ALLOCATE ( Lake_2gw(Nlake) )
      IF ( declvar(MODNAME, 'lake_2gw', 'nlake', Nlake, 'double', &
     &     'Total seepage from each lake', &
     &     'cfs', Lake_2gw)/=0 ) CALL read_error(3, 'lake_2gw')

! Declared Variables for broad-crested weir or gate opening routing
      ALLOCATE ( Lake_vol(Nlake) )
      IF ( declvar(MODNAME, 'lake_vol', 'nlake', Nlake, 'double', &
     &     'Storage in each lake using broad-crested weir or gate opening routing', &
     &     'acre-feet', Lake_vol)/=0 ) CALL read_error(3, 'lake_vol')

      ALLOCATE ( Lake_invol(Nlake) )
      IF ( declvar(MODNAME, 'lake_invol', 'nlake', Nlake, 'double', &
     &     'Inflow to each lake using broad-crested weir or gate opening routing', &
     &     'acre-feet', Lake_invol)/=0 ) CALL read_error(3, 'lake_invol')

! Declared Variables for gate opening routing
      ALLOCATE ( Lake_outvol(Nlake) )
      IF ( declvar(MODNAME, 'lake_outvol', 'nlake', Nlake, 'double', &
     &     'Outflow from each lake using broad-crested weir or gate opening routing', &
     &     'acre-inches', Lake_outvol)/=0 ) CALL read_error(3, 'lake_outvol')

! Declared Parameters
      ALLOCATE ( Lake_type(Nlake) )
      IF ( declparam(MODNAME, 'lake_type', 'nlake', 'integer', &
     &       '1', '1', '6', &
     &       'Type of lake routing method', &
     &       'Type of lake routing method (1=Puls routing; 2=linear routing; 3=flow through;'// &
     &       ' 4=broad crested weir; 5=gate opening; 6=measured flow)', &
     &       'none')/=0 ) CALL read_error(1, 'lake_type')

      ALLOCATE ( Lake_qro(Nlake) )
      IF ( declparam(MODNAME, 'lake_qro', 'nlake', 'real', &
     &     '0.1', '0.0', '1.0E7', &
     &     'Initial daily mean outflow from each lake', &
     &     'Initial daily mean outflow from each lake', &
     &     'cfs')/=0 ) CALL read_error(1, 'lake_qro')

! Declared Parameters for Puls or linear routing
      IF ( Init_vars_from_file==0 ) THEN
        ALLOCATE ( Lake_init(Nlake) )
        IF ( declparam(MODNAME, 'lake_init', 'nlake', 'real', &
     &       '0.0', '0.0', '1.0E7', &
     &       'Initial storage in each lake', &
     &       'Initial storage in each lake using Puls or linear storage routing', &
     &       'cfs-days')/=0 ) CALL read_error(1, 'lake_init')

        ALLOCATE ( Lake_din1(Nlake) )
        IF ( declparam(MODNAME, 'lake_din1', 'nlake', 'real', &
     &       '0.1', '0.0', '1.0E7', &
     &       'Initial inflow to each lake', &
     &       'Initial inflow to each lake using Puls or linear storage routing', &
     &       'cfs')/=0 ) CALL read_error(1, 'lake_din1')
      ENDIF

! Declared Parameters for linear routing
      ALLOCATE ( Lake_coef(Nlake) )
      IF ( declparam(MODNAME, 'lake_coef', 'nlake', 'real', &
     &     '0.1', '0.0001', '1.0', &
     &     'Linear lake routing coefficient', &
     &     'Coefficient in equation to route storage to streamflow for each lake using linear routing', &
     &     'fraction/day')/=0 ) CALL read_error(1, 'lake_coef')

! Declared Parameters for Puls routing
      IF ( Mxnsos>0 ) THEN
        ALLOCATE ( O2(Mxnsos, Nlake) )
        IF ( declparam(MODNAME, 'o2', 'mxnsos,nlake', 'real', &
     &       '0.0', '0.0', '1.0E7', &
     &       'Outflow values in outflow/storage tables for Puls routing', &
     &       'Outflow values in outflow/storage tables for each lake using Puls routing', &
     &       'cfs')/=0 ) CALL read_error(1, 'o2')

        ALLOCATE ( S2(Mxnsos, Nlake) )
        IF ( declparam(MODNAME, 's2', 'mxnsos,nlake', 'real', &
     &       '0.0', '0.0', '1.0E7', &
     &       'Storage values in outflow/storage tables for Puls routing', &
     &       'Storage values in outflow/storage table for each lake using Puls routing', &
     &       'cfs-days')/=0 ) CALL read_error(1, 's2')

        ALLOCATE ( Nsos(Nlake) )
        IF ( declparam(MODNAME, 'nsos', 'nlake', 'integer', &
     &       '0', 'bounded', 'mxnsos', &
     &       'Number of storage/outflow values in table for Puls routing', &
     &       'Number of storage/outflow values in table for each lake using Puls routing', &
     &       'none')/=0 ) CALL read_error(1, 'nsos')
      ENDIF

! Declared Parameters for broad-crested weir or gate opening routing
      IF ( Init_vars_from_file==0 ) THEN
        ALLOCATE ( Lake_vol_init(Nlake) )
        IF ( declparam(MODNAME, 'lake_vol_init', 'nlake', 'real', &
     &       '0.0', '0.0', '1.0E7', &
     &       'Initial lake volume', &
     &       'Initial lake volume for each lake using broad-crested weir or gate opening routing', &
     &       'acre-feet')/=0 ) CALL read_error(1, 'lake_vol_init')
      ENDIF

! Declared Parameters for broad-crested weir routing
      ALLOCATE ( Weir_coef(Nlake) )
      IF ( declparam(MODNAME, 'weir_coef', 'nlake', 'real', &
     &     '2.7', '2.0', '3.0', &
     &     'Broad-crested weir coefficent', &
     &     'Coefficient for lakes using broad-crested weir routing', &
     &     'none')/=0 ) CALL read_error(1, 'weir_coef')

      ALLOCATE ( Weir_len(Nlake) )
      IF ( declparam(MODNAME, 'weir_len', 'nlake', 'real', &
     &     '5.0', '1.0', '1000.0', &
     &     'Broad-crested weir length', &
     &     'Weir length for lakes using broad-crested weir routing', &
     &     'feet')/=0 ) CALL read_error(1, 'weir_len')

      ALLOCATE ( Elev_outflow(Nlake) )
      IF ( declparam(MODNAME, 'elev_outflow', 'nlake', 'real', &
     &     '0.0', '-300.0', '10000.0', &
     &     'Elevation of the main outflow point', &
     &     'Elevation of the main outflow point for each lake using broad-crested weir routing', &
     &     'feet')/=0 ) CALL read_error(1, 'elev_outflow')

! Declared Parameters for gate opening routing
      IF ( Nratetbl>0 ) THEN
        ALLOCATE ( Ratetbl_lake(Nratetbl), Rate_table(Nstage,Ngate), Tbl_stage(Nstage), Tbl_gate(Ngate) )
        IF ( declparam(MODNAME, 'ratetbl_lake', 'nratetbl', 'integer', &
     &       '0', 'bounded', 'nlake', &
     &       'Index of lake associated with each rating table', &
     &       'Index of lake associated with each rating table for'// &
     &       ' each lake using gate opening routing', &
     &       'none')/=0 ) CALL read_error(1, 'ratetbl_lake')
        IF ( declparam(MODNAME, 'rate_table', 'nstage,ngate', 'real', &
     &       '5.0', '-100.0', '1000.0', &
     &       'Rating table 1 with stage (rows) and gate opening (cols)', &
     &       'Rating table with stage (rows) and gate opening'// &
     &       ' (cols) for rating table 1 for lakes using gate opening routing and nratetbl>0', &
     &       'cfs')/=0 ) CALL read_error(1, 'rate_table')
        IF ( declparam(MODNAME, 'tbl_stage', 'nstage', 'real', &
     &       '5.0', '-100.0', '1000.0', &
     &       'Stage values for each row of rating table 1', &
     &       'Stage values for each row for rating table 1 for lakes using gate opening routing and nratetbl>0', &
     &       'feet')/=0 ) CALL read_error(1, 'tbl_stage')
        IF ( declparam(MODNAME, 'tbl_gate', 'ngate', 'real', &
     &       '0.0', '0.0', '20.0', &
     &       'Gate openings for each column of rating table 1', &
     &       'Gate openings for each column for rating table 1 for lakes using gate opening routing and nratetbl>0', &
     &       'inches')/=0 ) CALL read_error(1, 'tbl_gate')

        IF ( Nratetbl>1 ) THEN
          ALLOCATE ( Rate_table2(Nstage2,Ngate2), Tbl_stage2(Nstage2), Tbl_gate2(Ngate2) )
          IF ( declparam(MODNAME, 'rate_table2', 'nstage2,ngate2', 'real', &
     &         '5.0', '-100.0', '1000.0', &
     &         'Rating table 2 with stage (rows) and gate opening (cols)', &
     &         'Rating table with stage (rows) and gate opening'// &
     &         ' (cols) for rating table 2 for lakes using gate opening routing and nratetbl>1', &
     &         'cfs')/=0 ) CALL read_error(1, 'rate_table2')
          IF ( declparam(MODNAME, 'tbl_stage2', 'nstage2', 'real', &
     &         '5.0', '-100.0', '1000.0', &
     &         'Stage values for each row of rating table 2', &
     &         'Stage values for each row for rating table 2 for lakes using gate opening routing and nratetbl>1', &
     &         'feet')/=0 ) CALL read_error(1, 'tbl_stage2')
          IF ( declparam(MODNAME, 'tbl_gate2', 'ngate2', 'real', &
     &         '0.0', '0.0', '20.0', &
     &         'Gate openings for each column of rating table 2', &
     &         'Gate openings for each column for rating table 2 for lakes using gate opening routing and nratetbl>1', &
     &         'inches')/=0 ) CALL read_error(1, 'tbl_gate2')

          IF ( Nratetbl>2 ) THEN
            ALLOCATE ( Rate_table3(Nstage3,Ngate3), Tbl_stage3(Nstage3), Tbl_gate3(Ngate3) )
            IF ( declparam(MODNAME, 'rate_table3', 'nstage3,ngate3', 'real', &
     &           '5.0', '-100.0', '1000.0', &
     &           'Rating table 3 with stage (rows) and gate opening (cols)', &
     &           'Rating table with stage (rows) and gate opening'// &
     &           ' (cols) for rating table 3 for lakes using gate opening routing and nratetbl>2', &
     &           'cfs')/=0 ) CALL read_error(1, 'rate_table3')
            IF ( declparam(MODNAME, 'tbl_stage3', 'nstage3', 'real', &
     &           '5.0', '-100.0', '1000.0', &
     &           'Stage values for each row of rating table 3', &
     &           'Stage values for each row for rating table 3 for lakes using gate opening routing and nratetbl>2', &
     &           'feet')/=0 ) CALL read_error(1, 'tbl_stage3')
            IF ( declparam(MODNAME, 'tbl_gate3', 'ngate3', 'real', &
     &           '0.0', '0.0', '20.0', &
     &           'Gate openings for each column of rating table 3', &
     &           'Gate openings for each column for rating table 3 for lakes using gate opening routing and nratetbl>2', &
     &           'inches')/=0 ) CALL read_error(1, 'tbl_gate3')

            IF ( Nratetbl>3 ) THEN
              ALLOCATE ( Rate_table4(Nstage4,Ngate4), Tbl_stage4(Nstage4), Tbl_gate4(Ngate4) )
              IF ( declparam(MODNAME, 'rate_table4', 'nstage4,ngate4', 'real', &
     &             '5.0', '-100.0', '1000.0', &
     &             'Rating table 4 with stage (rows) and gate opening (cols)', &
     &             'Rating table with stage (rows) and gate opening'// &
     &             ' (cols) for rating table 4 for lakes using gate opening routing and nratetbl>3', &
     &             'cfs')/=0 ) CALL read_error(1, 'rate_table4')
              IF ( declparam(MODNAME, 'tbl_stage4', 'nstage4', 'real', &
     &             '5.0', '-100.0', '1000.0', &
     &             'Stage values for each row of rating table 4', &
     &             'Stage values for each row for rating table 4 for lakes using gate opening routing and nratetbl>3', &
     &             'feet')/=0 ) CALL read_error(1, 'tbl_stage4')
              IF ( declparam(MODNAME, 'tbl_gate4', 'ngate4', 'real', &
     &             '0.0', '0.0', '20.0', &
     &             'Gate openings for each column of rating table 4', &
     &             'Gate openings for each column for rating table 4 for lakes using gate opening routing and nratetbl>3', &
     &             'inches')/=0 ) CALL read_error(1, 'tbl_gate4')
            ENDIF
          ENDIF
        ENDIF
      ENDIF

! Declared Parameters for lakes with lake outflow set to measured streamflow
      ALLOCATE ( Obsout_lake(Nlake) )
      IF ( declparam(MODNAME, 'obsout_lake', 'nlake', 'integer', &
     &     '0', 'bounded', 'nobs', &
     &     'Index of streamflow measurement station that specifies outflow from a lake', &
     &     'Index of streamflow measurement station that specifies outflow from each lake using measured flow replacement', &
     &     'none')/=0 ) CALL read_error(1, 'obsout_lake')

      END FUNCTION lakertedecl

!***********************************************************************
!     Initialize lake_route module - get parameter values, compute initial values
!***********************************************************************
      INTEGER FUNCTION lakerteinit()
      USE PRMS_LAKE_ROUTE
      USE PRMS_MODULE, ONLY: Nsegment, Inputerror_flag, Nlake, Nratetbl, &
     &    Cascade_flag, Nobs, Init_vars_from_file
      USE PRMS_BASIN, ONLY: NEARZERO, Active_hrus, Hru_route_order, Gwr_type, Basin_area_inv, &
     &    CFS2CMS_CONV, Lake_hru, Lake_hru_id, Hru_segment
      USE PRMS_FLOWVARS, ONLY: Basin_lake_stor
      USE PRMS_ROUTING, ONLY: Segment_type
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, ierr, j, k, kk, ii
      REAL :: tmp
!***********************************************************************
      lakerteinit = 0

      IF ( getparam(MODNAME, 'lake_type', Nlake, 'integer', Lake_type)/=0 ) CALL read_error(2, 'lake_type')
      IF ( getparam(MODNAME, 'lake_qro', Nlake, 'real', Lake_qro)/=0 ) CALL read_error(2, 'lake_qro')

      Puls_lin_flag = 0
      Obs_flag = 0
      Linear_flag = 0
      Weir_gate_flag = 0
      Weir_flag = 0
      Gate_flag = 0
      Puls_flag = 0
      DO j = 1, Nlake
        Lake_outcfs(j) = Lake_qro(j)
        Lake_outcms(j) = Lake_qro(j)*CFS2CMS_CONV
        IF ( Lake_type(j)==1 .OR. Lake_type(j)==2 ) THEN
          Puls_lin_flag = 1
          IF ( Lake_type(j)==2 ) Linear_flag = 1
          IF ( Lake_type(j)==1 ) Puls_flag = 1
        ELSEIF ( Lake_type(j)==4 .OR. Lake_type(j)==5 ) THEN
          Weir_gate_flag = 1
          Gwr_type(Lake_hru(j)) = 2
          IF ( Lake_type(j)==4 ) Weir_flag = 1
          IF ( Lake_type(j)==5 ) Gate_flag = 1
        ELSEIF ( Lake_type(j)==6 ) THEN
          Obs_flag = 1
        ENDIF
      ENDDO

      Lake_id = 0
      DO i = 1, Nsegment
        DO kk = 1, Active_hrus
          j = Hru_route_order(kk)
          IF ( Hru_segment(j)==i ) THEN
            IF ( Segment_type(i)==2 ) Lake_id(i) = Lake_hru_id(j)
          ENDIF
        ENDDO
      ENDDO
      IF ( Init_vars_from_file==0 ) THEN
        Lake_outvol = 0.0D0
        Lake_invol = 0.0D0
        Lake_vol = 0.0D0
        Lake_sto = 0.0D0
        Lake_precip = 0.0D0
        Lake_seep_in = 0.0D0
        Lake_evap = 0.0D0
        Lake_2gw = 0.0D0
        Lake_inflow = 0.0D0
        Lake_outflow = 0.0D0
        Lake_stream_in = 0.0D0
        Lake_lateral_inflow = 0.0D0
        Din1 = 0.0
        Basin_lake_stor = 0.0D0
        IF ( Cascade_flag==1 ) THEN
          Lake_sroff = 0.0D0
          Lake_interflow = 0.0D0
        ENDIF
      ENDIF

      IF ( Gate_flag==1 ) THEN
        IF ( Nratetbl<1 ) STOP 'ERROR, nratetbl = 0 and gate opening routing requested'
        IF ( getparam(MODNAME, 'rate_table', Nstage*Ngate, 'real', Rate_table)/=0 ) CALL read_error(2, 'rate_table')
        IF ( getparam(MODNAME, 'tbl_stage', Nstage, 'real', Tbl_stage)/=0 ) CALL read_error(2, 'tbl_stage')
        IF ( getparam(MODNAME, 'tbl_gate', Ngate, 'real', Tbl_gate)/=0 ) CALL read_error(2, 'tbl_gate')
        IF ( getparam(MODNAME, 'ratetbl_lake', Nratetbl, 'integer', Ratetbl_lake)/=0 ) CALL read_error(2, 'ratetbl_lake')

        IF ( Nratetbl>1 ) THEN
          IF ( getparam(MODNAME, 'rate_table2', Nstage2*Ngate2, 'real', Rate_table2)/=0 ) CALL read_error(2, 'rate_table2')
          IF ( getparam(MODNAME, 'tbl_stage2', Nstage2, 'real', Tbl_stage2)/=0 ) CALL read_error(2, 'tbl_stage2')
          IF ( getparam(MODNAME, 'tbl_gate2', Ngate2, 'real', Tbl_gate2)/=0 ) CALL read_error(2, 'tbl_gate2')

          IF ( Nratetbl>2 ) THEN
            IF ( getparam(MODNAME, 'rate_table3',Nstage3*Ngate3, 'real', Rate_table3)/=0 ) &
     &           CALL read_error(2, 'rate_table3')
            IF ( getparam(MODNAME, 'tbl_stage3', Nstage3, 'real', Tbl_stage3)/=0 ) CALL read_error(2, 'tbl_stage3')
            IF ( getparam(MODNAME, 'tbl_gate3', Ngate3, 'real', Tbl_gate3)/=0 ) CALL read_error(2, 'tbl_gate3')

            IF ( Nratetbl>3 ) THEN
              IF ( getparam(MODNAME, 'rate_table4', Nstage4*Ngate4, 'real', Rate_table4)/=0 ) &
     &             CALL read_error(2, 'rate_table4')
              IF ( getparam(MODNAME, 'tbl_stage4', Nstage4, 'real', Tbl_stage4)/=0 ) CALL read_error(2, 'tbl_stage4')
              IF ( getparam(MODNAME, 'tbl_gate4', Ngate4, 'real', Tbl_gate4)/=0 ) CALL read_error(2, 'tbl_gate4')
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IF ( Puls_lin_flag==1 ) THEN
        IF ( Init_vars_from_file==0 ) THEN
          IF ( getparam(MODNAME, 'lake_init', Nlake, 'real', Lake_init)/=0 ) CALL read_error(2, 'lake_init')
          IF ( getparam(MODNAME, 'lake_din1', Nlake, 'real', Lake_din1)/=0 ) CALL read_error(2, 'lake_din1')
          Lake_sto = Lake_init
          Din1 = Lake_din1
        ENDIF
      ENDIF

      IF ( Puls_flag==1 ) THEN
        IF ( Mxnsos==0 ) STOP 'ERROR, dimension mxnsos = 0 and Puls routing requested'
        IF ( getparam(MODNAME, 'o2', Mxnsos*Nlake, 'real', O2)/=0 ) CALL read_error(2, 'o2')
        IF ( getparam(MODNAME, 's2', Mxnsos*Nlake, 'real', S2)/=0 ) CALL read_error(2, 's2')
        IF ( getparam(MODNAME, 'nsos', Nlake, 'integer', Nsos)/=0 ) CALL read_error(2, 'nsos')
      ELSEIF ( Mxnsos>0 ) THEN
        Nsos = 1
      ENDIF

      IF ( Linear_flag==1 ) THEN
        IF ( getparam(MODNAME, 'lake_coef', Nlake, 'real', Lake_coef)/=0 ) CALL read_error(2, 'lake_coef')
      ELSE
        Lake_coef = 1.0
      ENDIF

      IF ( Weir_gate_flag==1 ) THEN
        IF ( Init_vars_from_file==0 ) THEN
          IF ( getparam(MODNAME, 'lake_vol_init', Nlake, 'real', Lake_vol_init)/=0 ) CALL read_error(2, 'lake_vol_init')
          Lake_vol = Lake_vol_init
        ENDIF
      ENDIF

      IF ( Weir_flag==1 ) THEN
        IF ( getparam(MODNAME, 'weir_coef', Nlake, 'real', Weir_coef)/=0 ) CALL read_error(2, 'weir_coef')
        IF ( getparam(MODNAME, 'weir_len', Nlake, 'real', Weir_len)/=0 ) CALL read_error(2, 'weir_len')
        IF ( getparam(MODNAME, 'elev_outflow', Nlake, 'real', Elev_outflow)/=0 ) CALL read_error(2, 'elev_outflow')
      ELSE
        Weir_coef = 1.0
        Weir_len = 1.0
        Elev_outflow = 0.0
      ENDIF

      IF ( Obs_flag==1 ) THEN
        IF ( getparam(MODNAME, 'obsout_lake', Nlake, 'integer', Obsout_lake)/=0 ) CALL read_error(2, 'obsout_lake')
      ELSE
        Obsout_lake = 1
      ENDIF

      DO j = 1, Nlake
        ierr = 0
        IF ( Lake_type(j)==1 .OR. Lake_type(j)==2 ) THEN
          IF ( Lake_type(j)==1 ) THEN
            kk = Nsos(j)
            IF ( kk>Mxnsos ) THEN
              PRINT *, 'ERROR, lake_type = 1, but, nsos>mxnsos, lake:', j, ', nsos:', kk, ', mxnsos:', Mxnsos
              ierr = 1
            ELSEIF ( kk<1 ) THEN
              PRINT *, 'ERROR, lake_type = 1, but, nsos<1, lake:', j, ' nsos:', kk, ' mxnsos:', Mxnsos
              ierr = 1
            ENDIF
          ENDIF
        ELSEIF ( Weir_gate_flag==1 ) THEN
          IF ( Lake_hru(j)==0 ) THEN
            PRINT *, 'ERROR, lake_type = 4 or 5 and lake_hru for lake:', j, ' specified as 0'
            ierr = 1
          ENDIF
!          IF ( Lake_type(j)==4 ) THEN
!            IF ( Elev_outflow(j)<0.0 ) THEN
!              PRINT *, 'ERROR, elev_outflow < 0.0 for lake:', j, Elev_outflow(j)
!              ierr = 1
!            ENDIF
!          ENDIF
        ELSEIF ( Lake_type(j)==6 ) THEN
          IF ( Obsout_lake(j)==0 .OR. Obsout_lake(j)>Nobs ) THEN
            PRINT *, 'ERROR, obsout_lake value = 0 or > nobs for lake:', j, Obsout_lake(j)
            ierr = 1
          ENDIF
        ELSE
          PRINT *, 'ERROR, invalid lake_type for lake:', j, Lake_type(j)
          ierr = 1
        ENDIF
        IF ( ierr==1 ) THEN
          Inputerror_flag = 1
          CYCLE
        ENDIF
        IF ( Lake_type(j)==1 ) THEN
          kk = Nsos(j)
          DO ii = 1, kk
            Wvd(ii, j) = S2(ii, j) + O2(ii, j)*0.5
          ENDDO
          DO k = 2, kk
            tmp = Wvd(k, j) - Wvd(k-1, j)
            IF ( ABS(tmp)<NEARZERO ) tmp = 1.0
            S24(k, j) = (O2(k, j)-O2(k-1, j))/tmp
            C24(k, j) = O2(k, j) - S24(k, j)*Wvd(k, j)
          ENDDO
        ELSEIF ( Weir_gate_flag==1 ) THEN
          IF ( Lake_hru(j)==0 ) THEN
            PRINT *, 'ERROR, lake_type = 4 or 5 and lake_hru for lake:', j, ' specified as 0'
            Inputerror_flag = 1
            CYCLE
          ENDIF
          Gwr_type(Lake_hru(j)) = 2
          Basin_lake_stor = Basin_lake_stor + Lake_vol(j)*12.0D0
        ENDIF
      ENDDO
      Basin_lake_stor = Basin_lake_stor*Basin_area_inv

      !DEALLOCATE ( Lake_init, Lake_vol_init )
      !DEALLOCATE ( Lake_din1, Lake_qro )
      !IF ( Mxnsos>0 ) DEALLOCATE ( O2, S2 )

      END FUNCTION lakerteinit

!***********************************************************************
!     Computes lake routing common values
!***********************************************************************
      INTEGER FUNCTION lakerterun()
      USE PRMS_LAKE_ROUTE
      USE PRMS_MODULE, ONLY: Nlake, Cascade_flag
      USE PRMS_BASIN, ONLY: Hru_area, Hru_route_order, Active_hrus, Lake_hru_id, Hru_type
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Hru_actet
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Hortonian_lakes
      USE PRMS_SOILZONE, ONLY: Upslope_dunnianflow, Upslope_interflow
      USE PRMS_GWFLOW, ONLY: Lake_seepage, Gw_seep_lakein
      IMPLICIT NONE
! Local Variables
      INTEGER :: jj, k, lakeid
      REAL :: tocfs
!***********************************************************************
      lakerterun = 0

      Lake_inflow = 0.0D0
      Lake_outflow = 0.0D0
      Lake_lateral_inflow = 0.0D0
      Lake_stream_in = 0.0D0
      Lake_precip = 0.0D0
      IF ( Cascade_flag==1 ) THEN
        Lake_sroff = 0.0D0
        Lake_interflow = 0.0D0
      ENDIF
      Lake_seep_in = 0.0D0
      Lake_evap = 0.0D0
      Lake_2gw = 0.0D0

      ! shouldn't have snowpack, all precipitation should be added directly to lake
      ! units of lake_inflow = cfs
      DO jj = 1, Active_hrus
        k = Hru_route_order(jj)
        IF ( Hru_type(k)/=2 ) CYCLE
        tocfs = Hru_area(k)*Cfs_conv
        lakeid = Lake_hru_id(k)
        Lake_precip(lakeid) = Lake_precip(lakeid) + tocfs*Hru_ppt(k)
        IF ( Cascade_flag==1 ) THEN
          Lake_sroff(lakeid) = Lake_sroff(lakeid) + tocfs*(Hortonian_lakes(k)+Upslope_dunnianflow(k))
          Lake_interflow(lakeid) = Lake_interflow(lakeid) + tocfs*Upslope_interflow(k)
        ENDIF
        Lake_seep_in(lakeid) = Lake_seep_in(lakeid) + tocfs*Gw_seep_lakein(lakeid)
        Lake_evap(lakeid) = Lake_evap(lakeid) + tocfs*Hru_actet(k)
        Lake_2gw(lakeid) = Lake_2gw(lakeid) + tocfs*Lake_seepage(lakeid)
      ENDDO
      DO lakeid = 1, Nlake
        Lake_inflow(lakeid) = Lake_precip(lakeid) + Lake_seep_in(lakeid)
        IF ( Cascade_flag==1 ) THEN
          Lake_lateral_inflow(lakeid) = Lake_sroff(lakeid) + Lake_interflow(lakeid)
          Lake_inflow(lakeid) = Lake_inflow(lakeid) + Lake_lateral_inflow(lakeid)
        ENDIF
        Lake_outflow(lakeid) = Lake_evap(lakeid) + Lake_2gw(lakeid)
      ENDDO

      END FUNCTION lakerterun

!     ***********************************
!     * Route Lake
!     ***********************************
      SUBROUTINE route_lake(Lakeid, Lake_type, Lake_inflow, Lake_outflow, Lake_outcfs, Obsout_lake, &
     &                      Din1, Nsos, Lake_sto, Lake_coef, Lake_vol, Lake_invol, Lake_outvol, Lake_area, Elevlake)
      USE PRMS_LAKE_ROUTE, ONLY: Wvd, S24, C24, &
     &    Elev_outflow, Ngate, Nstage, Ngate2, Nstage2, Ngate3, Nstage3, Ngate4, Nstage4, &
     &    Tbl_gate, Tbl_stage, Rate_table, Tbl_gate2, Tbl_stage2, Rate_table2, Tbl_gate3, Tbl_stage3, Rate_table3, &
     &    Tbl_gate4, Tbl_stage4, Rate_table4, Ratetbl_lake, Weir_coef, Weir_len
      USE PRMS_MODULE, ONLY: Nratetbl
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO
      USE PRMS_OBS, ONLY: Gate_ht, Streamflow_cfs
      USE PRMS_FLOWVARS, ONLY: Basin_lake_stor
      USE PRMS_ROUTING, ONLY: Cfs2acft
      IMPLICIT NONE
! Functions
      INTRINSIC EXP
      EXTERNAL table_comp
! Arguments
      INTEGER, INTENT(IN) :: Lakeid, Lake_type, Obsout_lake, Nsos
      REAL, INTENT(IN) :: Din1, Lake_coef
      REAL, INTENT(INOUT) :: Elevlake
      DOUBLE PRECISION, INTENT(IN) :: Lake_inflow, Lake_outflow, Lake_area
      DOUBLE PRECISION, INTENT(INOUT) :: Lake_outcfs, Lake_sto, Lake_vol, Lake_invol, Lake_outvol
! Local Variables
      INTEGER :: n, jjj, i
      REAL :: xkt, c2, elevold, q1, q3, head, new_elevlake, head2
      DOUBLE PRECISION :: avin, s2o2, q2, lake_out, diff_vol, lake_out1
!***********************************************************************
      ! q2 = lake out in cfs
      q2 = 0.0D0
!   Compute outflow using Puls routing method
      IF ( Lake_type==1 ) THEN
        !rsr, why half of current in and last in???
        avin = (Lake_inflow+Din1)*0.5D0
        s2o2 = Lake_sto - (Lake_outflow+Lake_outcfs)*0.5D0
        IF ( s2o2<DNEARZERO ) THEN
          PRINT *, 'WARNING: lake storage for Puls routing < 0 lake:', Lakeid, s2o2, ' set to 0'
          PRINT *, 'Probably need to increase lake_init'
          s2o2 = 0.0D0
        ENDIF
        s2o2 = s2o2 + avin
        n = Nsos
        DO jjj = 2, Nsos
          IF ( s2o2<Wvd(jjj, Lakeid) ) THEN
            n = jjj
            EXIT
          ENDIF
        ENDDO
        q2 = S24(n, Lakeid)*s2o2 + C24(n, Lakeid)

        IF ( q2<DNEARZERO ) q2 = 0.0D0
        Lake_sto = s2o2 - q2*0.5D0
        IF ( Lake_sto<DNEARZERO ) THEN
          PRINT *, 'WARNING: lake storage for Puls routing < 0 lake:', Lakeid, Lake_sto, ' set to 0'
          PRINT *, 'Probably need to increase lake_init'
          q2 = s2o2 + Lake_outcfs*0.5D0
          Lake_sto = 0.0D0
        ENDIF

!   Compute outflow using linear reservoir method
      ELSEIF ( Lake_type==2 ) THEN
        !rsr, why half of current in and last in???
        avin = (Lake_inflow+Din1-Lake_outflow)*0.5D0
        xkt = Lake_coef
        c2 = 1.0 - EXP(-xkt)
        q2 = (avin*(1.0D0-(c2/xkt))) + Lake_sto*c2
        IF ( q2<DNEARZERO ) q2 = 0.0D0
        Lake_sto = Lake_sto + avin - q2
        IF ( Lake_sto<DNEARZERO ) THEN
          PRINT *, 'WARNING, lake storage for linear routing < 0 lake:', Lakeid, Lake_sto, ' set to 0'
          PRINT *, 'Probably need to increase lake_init'
        ENDIF

!   Compute using flow through reservoir
      ELSEIF ( Lake_type==3 ) THEN
        q2 = Lake_inflow - Lake_outflow

!   Set outflow as a measured flow
      ELSEIF ( Lake_type==6 ) THEN
        q2 = Streamflow_cfs(Obsout_lake)
        IF ( q2<0.0D0 ) THEN
          PRINT *, 'Observed runoff value specified <0 as outflow from lake:', lakeid, ' value:', q2
          PRINT *, '     runoff id:', Obsout_lake, ' outflow set to 0.0'
          q2 = 0.0D0
        ENDIF

      ELSE ! 4 or 5; broad-crested weir or gate opening
        elevold = Elevlake

        ! units lake_invol = acft
        Lake_invol = Lake_inflow*Cfs2acft

        ! units lake_out = acft
        lake_out = (Lake_outflow/12.0D0)*Lake_area
        diff_vol = Lake_invol - lake_out
        q1 = 0.0
        q3 = 0.0

!   Compute using lake surface elevation and broad crested weir
        IF ( Lake_type==4 ) THEN
          head = elevold - Elev_outflow(Lakeid)
          IF ( head>NEARZERO ) q1 = (head**1.5) * Weir_coef(Lakeid) * Weir_len(Lakeid)
          lake_out1 = q1*Cfs2acft

          ! new_elevlake has units of feet
          new_elevlake = elevold + (diff_vol-lake_out1)/Lake_area

          head2 = (new_elevlake+elevold)*0.5 - Elev_outflow(Lakeid)
          IF ( head2>NEARZERO ) q3 = (head2**1.5) * Weir_coef(Lakeid) * Weir_len(Lakeid)

!  Compute using a rating table of lake surface elevation & gate opening
        ELSE ! type = 5
          DO i = 1, Nratetbl
            IF ( Lakeid==Ratetbl_lake(i) ) THEN
              IF ( i==1 ) THEN
                CALL table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage, &
     &                          Rate_table, elevold, Gate_ht(i), q1, Lake_area)
              ELSEIF ( i==2 ) THEN
                CALL table_comp(Ngate2, Nstage2, Tbl_gate2, Tbl_stage2, &
     &                          Rate_table2, elevold, Gate_ht(i), q1, Lake_area)
              ELSEIF ( i==3 ) THEN
                CALL table_comp(Ngate3, Nstage3, Tbl_gate3, Tbl_stage3, &
     &                          Rate_table3, elevold, Gate_ht(i), q1, Lake_area)
              ELSEIF ( i==4 ) THEN
                CALL table_comp(Ngate4, Nstage4, Tbl_gate4, Tbl_stage4, &
     &                          Rate_table4, elevold, Gate_ht(i), q1, Lake_area)
              ENDIF
            ENDIF
          ENDDO

          lake_out1 = q1*Cfs2acft

          ! new_elevlake has units of feet
          new_elevlake = elevold + (diff_vol-lake_out1)/Lake_area

          DO i = 1, Nratetbl
            IF ( Lakeid==Ratetbl_lake(i) ) THEN
              IF ( i==1 ) THEN
                CALL table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage, Rate_table, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area)
              ELSEIF ( i==2 ) THEN
                CALL table_comp(Ngate2, Nstage2, Tbl_gate2, Tbl_stage2, Rate_table2, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area)
              ELSEIF ( i==3 ) THEN
                CALL table_comp(Ngate3, Nstage3, Tbl_gate3, Tbl_stage3, Rate_table3, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area)
              ELSEIF ( i==4 ) THEN
                CALL table_comp(Ngate4, Nstage4, Tbl_gate4, Tbl_stage4, Rate_table4, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area)
              ENDIF
            ENDIF
          ENDDO
        ENDIF

        q2 = (q1+q3)*0.5
!       !sanity check, rsr
        IF ( q2<0.0D0 ) PRINT *, 'q2<0', q2, ' lake:', Lakeid

        Lake_outvol = q2*Cfs2acft + lake_out

        ! adjust lake storage
        Lake_vol = Lake_vol - Lake_outvol
        IF ( Lake_vol<DNEARZERO ) THEN
          PRINT *, 'Lake storage issue, lake_vol<0:', Lake_vol, ' lake:', Lakeid
          IF ( q2*Cfs2acft>Lake_vol ) THEN
            PRINT *, 'stream flow out reduced and storage set to 0'
            q2 = q2 - Lake_vol/Cfs2acft
            Lake_outvol = q2*Cfs2acft + lake_out
            Lake_vol = 0.0D0
          ELSE
            STOP 'ERROR, negative storage > available streamflow out'
          ENDIF
        ENDIF

        ! adjust lake elevation with stream and lateral inflows
        ! and streamflow, GWR, and evaporation outflows
        Elevlake = Elevlake + (Lake_invol-Lake_outvol)/Lake_area
        Basin_lake_stor = Basin_lake_stor + Lake_vol*12.0D0
      ENDIF

      Lake_outcfs = q2

      END SUBROUTINE route_lake

!=====================================================================
!    Rating table computation
!=====================================================================
      SUBROUTINE table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage, Rate_table, Elevlake, Gate_ht, Q2, Lake_area)
      USE PRMS_ROUTING, ONLY: Cfs2acft
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ngate, Nstage
      REAL, INTENT(IN) :: Tbl_gate(Ngate), Tbl_stage(Nstage), Rate_table(Nstage, Ngate), Gate_ht, Elevlake
      DOUBLE PRECISION, INTENT(IN) :: Lake_area
      REAL, INTENT(OUT) :: Q2
! Local Variables
      INTEGER m, mm, stg1, stg2, gate1, gate2
      REAL :: diff_q_stg1, diff_q_stg2, ratiog, ratios, q_stg1, q_stg2, diffq
!***********************************************************************
      IF ( Elevlake<Tbl_stage(Nstage) ) THEN
        Q2 = 0.0

      ELSEIF ( Elevlake>Tbl_stage(1) ) THEN
        ! lake elevation is > maximum stage, spill all water
        Q2 = (Elevlake-Tbl_stage(1))*Lake_area/Cfs2acft
        PRINT *, 'WARNING, lake elevation > maximum stage in rating table all water above rating table spills'
        PRINT *, 'Lake elevation:', Elevlake, ' Rating table stage:', Tbl_stage(1), ' discharge to stream:', Q2
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
          diff_q_stg2 = Rate_table(stg2, gate2) - Rate_table(stg2, gate1)
          diff_q_stg1 = Rate_table(stg1, gate2) - Rate_table(stg1, gate1)

 !rsr, possible divide by 0.0???
          ratiog = (Gate_ht-Tbl_gate(gate1))/(Tbl_gate(gate2)-Tbl_gate(gate1))
          q_stg2 = (ratiog*diff_q_stg2) + Rate_table(stg2, gate1)
          q_stg1 = (ratiog*diff_q_stg1) + Rate_table(stg1, gate1)

 !rsr, possible divide by 0.0???
          ratios = (Elevlake-Tbl_stage(stg2))/(Tbl_stage(stg1)-Tbl_stage(stg2))
          diffq = q_stg1 - q_stg2
          Q2 = q_stg2 + (ratios*diffq)
        ENDIF
      ENDIF

      END SUBROUTINE table_comp

!***********************************************************************
!     write or read restart file
!***********************************************************************
      SUBROUTINE lake_route_restart(In_out)
      USE PRMS_LAKE_ROUTE
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Cascade_flag
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Function
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=10) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Din1
        WRITE ( Restart_outunit ) Lake_stream_in
        WRITE ( Restart_outunit ) Lake_precip
        WRITE ( Restart_outunit ) Lake_outcfs
        WRITE ( Restart_outunit ) Lake_outcms
        WRITE ( Restart_outunit ) Lake_outvol
        WRITE ( Restart_outunit ) Lake_invol
        WRITE ( Restart_outunit ) Lake_inflow
        WRITE ( Restart_outunit ) Lake_outflow
        WRITE ( Restart_outunit ) Lake_lateral_inflow
        WRITE ( Restart_outunit ) Lake_vol
        WRITE ( Restart_outunit ) Lake_sto
        WRITE ( Restart_outunit ) Lake_evap
        WRITE ( Restart_outunit ) Lake_2gw
        WRITE ( Restart_outunit ) Lake_seep_in
        IF ( Cascade_flag==1 ) THEN
          WRITE ( Restart_outunit ) Lake_sroff
          WRITE ( Restart_outunit ) Lake_interflow
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Din1
        READ ( Restart_inunit ) Lake_stream_in
        READ ( Restart_inunit ) Lake_precip
        READ ( Restart_inunit ) Lake_outcfs
        READ ( Restart_inunit ) Lake_outcms
        READ ( Restart_inunit ) Lake_outvol
        READ ( Restart_inunit ) Lake_invol
        READ ( Restart_inunit ) Lake_inflow
        READ ( Restart_inunit ) Lake_outflow
        READ ( Restart_inunit ) Lake_lateral_inflow
        READ ( Restart_inunit ) Lake_vol
        READ ( Restart_inunit ) Lake_sto
        READ ( Restart_inunit ) Lake_evap
        READ ( Restart_inunit ) Lake_2gw
        READ ( Restart_inunit ) Lake_seep_in
        IF ( Cascade_flag==1 ) THEN
          READ ( Restart_inunit ) Lake_sroff
          READ ( Restart_inunit ) Lake_interflow
        ENDIF
      ENDIF
      END SUBROUTINE lake_route_restart
