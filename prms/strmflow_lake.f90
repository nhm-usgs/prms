!***********************************************************************
! Routes water between segments and lakes in the stream network
! strmflow_in_out is used to route water in the stream segments (no routing)
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
      INTEGER, SAVE :: Obs_flag, Secondoutflow_flag, Noarea_flag
      CHARACTER(LEN=13), SAVE :: MODNAME
      INTEGER, SAVE :: Mxnsos, Ngate, Nstage, Ngate2, Nstage2, Ngate3, Nstage3, Ngate4, Nstage4, Nsfres
      INTEGER, SAVE :: Linear_flag, Weir_flag, Puls_flag, Weir_rate_flag, Puls_lin_flag, Gate_flag
      INTEGER, SAVE, ALLOCATABLE :: Lake_id(:)
      DOUBLE PRECISION, SAVE :: Cfs2acft, Flow_out
      REAL, SAVE, ALLOCATABLE :: C24(:,:), S24(:,:), Wvd(:,:)
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Din1(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_outcfs(:), Lake_outcms(:), Lake_outvol(:), Lake_invol(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_vol(:), Lake_sto(:), Lake_inflow(:), Lake_outflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_stream_in(:), Lake_lateral_inflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_precip(:), Lake_sroff(:), Lake_interflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_seep_in(:), Lake_evap(:), Lake_2gw(:), Lake_outq2(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Nsos(:), Ratetbl_lake(:)
      REAL, SAVE, ALLOCATABLE :: Lake_qro(:), Lake_din1(:), Lake_init(:), Lake_coef(:), O2(:,:), S2(:,:)
      INTEGER, SAVE, ALLOCATABLE :: Obsout_lake(:), Lake_out2(:)
      REAL, SAVE, ALLOCATABLE :: Rate_table(:,:), Rate_table2(:,:), Rate_table3(:,:), Rate_table4(:,:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage(:), Tbl_gate(:), Tbl_stage2(:), Tbl_gate2(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage3(:), Tbl_gate3(:), Tbl_stage4(:), Tbl_gate4(:)
      REAL, SAVE, ALLOCATABLE :: Lake_vol_init(:), Weir_coef(:), Weir_len(:)
      REAL, SAVE, ALLOCATABLE :: Elev_outflow(:), Elevlake_init(:)
      REAL, SAVE, ALLOCATABLE :: Lake_out2_a(:), Lake_out2_b(:)
      END MODULE PRMS_STRMFLOW_LAKE

!***********************************************************************
!     Main daily stream flow with lakes routine
!***********************************************************************
      INTEGER FUNCTION strmflow_lake()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: strmlkdecl, strmlkinit, strmlkrun, strmlksetdims
      EXTERNAL :: strmflow_lake_restart
!***********************************************************************
      strmflow_lake = 0

      IF ( Process(:3)=='run' ) THEN
        strmflow_lake = strmlkrun()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        strmflow_lake = strmlksetdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        strmflow_lake = strmlkdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL strmflow_lake_restart(1)
        ELSE
          strmflow_lake = strmlkinit()
        ENDIF
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL strmflow_lake_restart(0)
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
      strmlksetdims = 0

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

      IF ( decldim('nsfres', 0, MAXDIM, 'Number of storage-detention reservoirs and lakes connected to the stream network')/=0 ) &
     &     CALL read_error(7, 'nsfres')

      END FUNCTION strmlksetdims

!***********************************************************************
!     Get parameters and allocate arrays
!   Declared Parameters
!     tosegment, hru_segment, obsin_segment
!     lake_type, lake_init, lake_qro, lake_din1, lake_coef, o2, s2, nsos, hru_area, lake_hru
!     tbl_stage, tbl_gate, lake_vol_init, rate_table, weir_coef, weir_len, elev_outflow, elevlake_init
!     lake_out2, lake_out2_a, lake_out2_b
!***********************************************************************
      INTEGER FUNCTION strmlkdecl()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Model, Nsegment, Nlake, Nratetbl, Cascade_flag, Strmflow_flag, Inputerror_flag
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar, getdim
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_strmflow_lake
!***********************************************************************
      strmlkdecl = 0

      Version_strmflow_lake = '$Id: strmflow_lake.f90 5660 2013-04-30 20:14:42Z rsregan $'
      CALL print_module(Version_strmflow_lake, 'Streamflow Routing        ', 90)
      MODNAME = 'strmflow_lake'

      IF ( Nsegment<1 ) THEN
        IF ( Model==99 ) THEN
          Nsegment = 1
        ELSE
          PRINT *, 'ERROR, strmflowlake module requires nsegment > 0, specified as:', Nsegment
          STOP
        ENDIF
      ENDIF

      Nsfres = getdim('nsfres')
      IF ( Nsfres==-1 ) CALL read_error(7, 'nsfres')
      IF ( Nsfres>0 .AND. Nlake==0 ) THEN
        PRINT *, 'ERROR, dimension nsfres has been changed to nlake.'
        PRINT *, '       All references to nsfres must be changed to nlake in your Parameter File'
        PRINT *, 'The following parameter names have been changed'
        PRINT *, '   old name        new name'
        PRINT *, 'sfres_type       lake_type'
        PRINT *, 'sfres_out2       lake_out2'
        PRINT *, 'sfres_out2_a     lake_out2_a'
        PRINT *, 'sfres_out2_b     lake_out2_b'
        PRINT *, 'ratetbl_sfres    ratetbl_lake'
        PRINT *, 'sfres_qro        lake_qro'
        PRINT *, 'sfres_din1       lake_din1'
        PRINT *, 'sfres_init       lake_init'
        PRINT *, 'sfres_coef       lake_coef'
        PRINT *, 'sfres_vol_init   lake_vol_init'
        PRINT *, 'elevsurf_init    elevlake_init'
        PRINT *, 'sfres_hru        lake_hru'
        PRINT *, 'seg_res_id       seg_lake_id'
        PRINT *, 'sfres_seep_elev  lake_seep_elev'
        PRINT *, 'Data File: sfres_elev changed to lake_elev'
        PRINT *, ' '
        Inputerror_flag = 1
      ELSEIF ( Nlake<1 ) THEN
        IF ( Model==99 ) THEN
          Nlake = 1
        ELSE
          PRINT *, 'ERROR, strmflowlake module requires nlake > 0, specified as:', Nlake
          STOP
        ENDIF
        Inputerror_flag = 1
      ENDIF

! redeclare dimensions for Project Chief to know these should default to 1
      IF ( Nlake>0 .OR. Model==99 ) THEN
        IF ( Nlake<1 ) Nlake = 1 ! for Model=99

! Dimension for Puls routing
        Mxnsos = getdim('mxnsos')
        IF ( Mxnsos==-1 ) CALL read_error(1, 'mxnsos')
        IF ( Model==99 .AND. Mxnsos<1 ) Mxnsos = 1

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
            PRINT *, 'ERROR, Muskingum Lake module allows maximum of 4 rating tables'
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
            IF ( Nratetbl==0 ) Nratetbl = 4
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
          ALLOCATE ( Ratetbl_lake(Nratetbl) )
          ALLOCATE ( Rate_table(Nstage,Ngate), Tbl_stage(Nstage), Tbl_gate(Ngate) )
          IF ( Nratetbl>1 .OR. Model==99 ) THEN
            IF ( Model==99 ) THEN
              IF ( Nstage2<1 ) Nstage2 = 1
              IF ( Ngate2<1 ) Ngate2 = 1
            ELSE
              IF ( Nstage2<1 .OR. Ngate2<1 ) STOP 'ERROR, nratetbl>1 and nstage2 or ngate2 = 0'
            ENDIF
            ALLOCATE ( Rate_table2(Nstage2,Ngate2) )
            ALLOCATE ( Tbl_stage2(Nstage2), Tbl_gate2(Ngate2) )
          ENDIF
          IF ( Nratetbl>2 .OR. Model==99 ) THEN
            IF ( Model==99 ) THEN
              IF ( Nstage3<1 ) Nstage3 = 1
              IF ( Ngate3<1 ) Ngate3 = 1
            ELSE
              IF ( Nstage3<1 .OR. Ngate3<1 ) STOP 'ERROR, nratetbl>2 and nstage3 or ngate3 = 0'
            ENDIF
            ALLOCATE ( Rate_table3(Nstage3,Ngate3) )
            ALLOCATE ( Tbl_stage3(Nstage3), Tbl_gate3(Ngate3) )
          ENDIF
          IF ( Nratetbl>3 .OR. Model==99 ) THEN
            IF ( Model==99 ) THEN
              IF ( Nstage4<1 ) Nstage4 = 1
              IF ( Ngate4<1 ) Ngate4 = 1
            ELSE
              IF ( Nstage4<1 .OR. Ngate4<1 ) STOP 'ERROR, nratetbl>3 and nstage4 or ngate4 = 0'
            ENDIF
            ALLOCATE ( Rate_table4(Nstage4,Ngate4) )
            ALLOCATE ( Tbl_stage4(Nstage4), Tbl_gate4(Ngate4) )
          ENDIF
        ENDIF

        ! Lake declared variables
        ALLOCATE ( Lake_inflow(Nlake) )
        IF ( declvar(MODNAME, 'lake_inflow', 'nlake', Nlake, 'double', &
     &       'Total inflow to each lake HRU', &
     &       'cfs', Lake_inflow)/=0 ) CALL read_error(3, 'lake_inflow')

        ALLOCATE ( Lake_outflow(Nlake) )
        IF ( declvar(MODNAME, 'lake_outflow', 'nlake', Nlake, 'double', &
     &       'Evaporation and seepage from each lake HRU', &
     &       'cfs', Lake_outflow)/=0 ) CALL read_error(3, 'lake_outflow')

        ALLOCATE ( Lake_lateral_inflow(Nsegment) )
        IF ( declvar(MODNAME, 'lake_lateral_inflow', 'nlake', Nlake, 'double', &
     &       'Lateral inflow to each lake HRU', &
     &       'cfs', Lake_lateral_inflow)/=0 ) CALL read_error(3, 'lake_lateral_inflow')

        ALLOCATE ( Lake_outcfs(Nlake) )
        IF ( declvar(MODNAME, 'lake_outcfs', 'nlake', Nlake, 'double', &
     &       'Streamflow leaving each lake, includes in second outlet flow', &
     &       'cfs', Lake_outcfs)/=0 ) CALL read_error(3, 'lake_outcfs')

        ALLOCATE ( Lake_outcms(Nlake) )
        IF ( declvar(MODNAME, 'lake_outcms', 'nlake', Nlake, 'double', &
     &       'Streamflow leaving each lake, includes in second outlet flow', &
     &       'cms', Lake_outcms)/=0 ) CALL read_error(3, 'lake_outcms')

! Declared Variables for Puls or linear routing
        ALLOCATE ( Lake_sto(Nlake) )
        IF ( declvar(MODNAME, 'lake_sto', 'nlake', Nlake, 'double', &
     &       'Storage in each lake using Puls or linear storage routing', &
     &       'cfs-days', Lake_sto)/=0 ) CALL read_error(3, 'lake_sto')

        ALLOCATE ( Din1(Nlake) )
        IF ( declvar(MODNAME, 'din1', 'nlake', Nlake, 'real', &
     &     'Inflow from the previous time step to each lake using Puls or linear storage routing', &
     &     'cfs', Din1)/=0 ) CALL read_error(3, 'din1')

        ALLOCATE ( Lake_stream_in(Nlake) )
        IF ( declvar(MODNAME, 'lake_stream_in', 'nlake', Nlake, 'double', &
     &       'Total streamflow into each lake', &
     &       'cfs', Lake_stream_in)/=0 ) CALL read_error(3, 'lake_stream_in')

        ALLOCATE ( Lake_precip(Nlake) )
        IF ( declvar(MODNAME, 'lake_precip', 'nlake', Nlake, 'double', &
     &     'Total precipitation into each lake', &
     &     'cfs', Lake_precip)/=0 ) CALL read_error(3, 'lake_precip')

        IF ( Cascade_flag==1 ) THEN
          ALLOCATE ( Lake_sroff(Nlake) )
          IF ( declvar(MODNAME, 'lake_sroff', 'nlake', Nlake, 'double', &
     &         'Total surface runoff into each lake', &
     &         'cfs', Lake_sroff)/=0 ) CALL read_error(3, 'lake_sroff')
          ALLOCATE ( Lake_interflow(Nlake) )
          IF ( declvar(MODNAME, 'lake_interflow', 'nlake', Nlake,'double', &
     &         'Total interflow into each lake', &
     &         'cfs', Lake_interflow)/=0 ) CALL read_error(3, 'lake_interflow')
        ENDIF

        ALLOCATE ( Lake_seep_in(Nlake) )
        IF ( declvar(MODNAME, 'lake_seep_in', 'nlake', Nlake, 'double', &
     &       'Total interflow into each lake', &
     &       'cfs', Lake_seep_in)/=0 ) CALL read_error(3, 'lake_seep_in')

        ALLOCATE ( Lake_evap(Nlake) )
        IF ( declvar(MODNAME, 'lake_evap', 'nlake', Nlake, 'double', &
     &       'Total evaporation from each lake', &
     &       'cfs', Lake_evap)/=0 ) CALL read_error(3, 'lake_evap')

        ALLOCATE ( Lake_2gw(Nlake) )
        IF ( declvar(MODNAME, 'lake_2gw', 'nlake', Nlake, 'double', &
     &       'Total seepage from each lake', &
     &       'cfs', Lake_2gw)/=0 ) CALL read_error(3, 'lake_2gw')

! Declared Variables for broad-crested weir or gate opening routing
        ALLOCATE ( Lake_vol(Nlake) )
        IF ( declvar(MODNAME, 'lake_vol', 'nlake', Nlake, 'double', &
     &       'Storage in each lake using broad-crested weir or gate opening routing', &
     &       'acre-feet', Lake_vol)/=0 ) CALL read_error(3, 'lake_vol')

        ALLOCATE ( Lake_invol(Nlake) )
        IF ( declvar(MODNAME, 'lake_invol', 'nlake', Nlake, 'double', &
     &       'Inflow to each lake using broad-crested weir or gate opening routing', &
     &       'acre-feet', Lake_invol)/=0 ) CALL read_error(3, 'lake_invol')

        ALLOCATE ( Lake_outvol(Nlake) )
        IF ( declvar(MODNAME, 'lake_outvol', 'nlake', Nlake, 'double', &
     &       'Outflow from each lake using broad-crested weir or gate opening routing', &
     &       'acre-inches', Lake_outvol)/=0 ) CALL read_error(3, 'lake_outvol')

! Declared Variables for lakes with a second outlet and gate opening routing
        ALLOCATE ( Lake_outq2(Nlake) )
        IF ( declvar(MODNAME, 'lake_outq2', 'nlake', Nlake, 'double', &
     &       'Streamflow from second outlet for each lake with a second outlet', &
     &       'cfs', Lake_outq2)/=0 ) CALL read_error(3, 'lake_outq2')

        ALLOCATE ( Lake_id(Nsegment), Lake_qro(Nlake) )
        ALLOCATE ( Lake_coef(Nlake), Elev_outflow(Nlake), Weir_coef(Nlake), Weir_len(Nlake) )
        IF ( Mxnsos>0 ) THEN
          ALLOCATE ( O2(Mxnsos, Nlake), S2(Mxnsos, Nlake), Nsos(Nlake) )
          ALLOCATE ( Wvd(Mxnsos, Nlake), S24(Mxnsos, Nlake), C24(Mxnsos, Nlake) )
        ENDIF
        ALLOCATE ( Lake_out2(Nlake), Lake_out2_a(Nlake), Lake_out2_b(Nlake), Obsout_lake(Nlake) )
      ENDIF

      IF ( Timestep/=0 ) RETURN

      IF ( Nlake>0 ) THEN
        IF ( declparam(MODNAME, 'lake_qro', 'nlake', 'real', &
     &       '0.1', '0.0', '1.0E7', &
     &       'Initial daily mean outflow from each lake', &
     &       'Initial daily mean outflow from each lake', &
     &       'cfs')/=0 ) CALL read_error(1, 'lake_qro')

! Declared Parameters for Puls or linear routing
        ALLOCATE ( Lake_init(Nlake) )
        IF ( declparam(MODNAME, 'lake_init', 'nlake', 'real', &
     &       '0.0', '0.0', '2.0E6', &
     &       'Initial storage in each lake', &
     &       'Initial storage in each lake using Puls or linear storage routing', &
     &       'cfs-days')/=0 ) CALL read_error(1, 'lake_init')
        ALLOCATE ( Lake_din1(Nlake) )
        IF ( declparam(MODNAME, 'lake_din1', 'nlake', 'real', &
     &       '0.1', '0.0', '1.0E7', &
     &       'Initial inflow to each lake', &
     &       'Initial inflow to each lake using Puls or linear storage routing', &
     &       'cfs')/=0 ) CALL read_error(1, 'lake_din1')

! Declared Parameters for linear routing
        IF ( declparam(MODNAME, 'lake_coef', 'nlake', 'real', &
     &       '0.1', '0.0001', '1.0', &
     &       'Linear lake routing coefficient', &
     &       'Coefficient in equation to route storage to streamflow for each lake using linear routing', &
     &       '1/day')/=0 ) CALL read_error(1, 'lake_coef')

! Declared Parameters for Puls routing
        IF ( Mxnsos>0 ) THEN
          IF ( declparam(MODNAME, 'o2', 'mxnsos,nlake', 'real', &
     &         '0.0', '0.0', '100000.0', &
     &         'Outflow values in outflow/storage tables for Puls routing', &
     &         'Outflow values in outflow/storage tables for each lake using Puls routing', &
     &         'cfs')/=0 ) CALL read_error(1, 'o2')
          IF ( declparam(MODNAME, 's2', 'mxnsos,nlake', 'real', &
     &         '0.0', '0.0', '100000.0', &
     &         'Storage values in outflow/storage tables for Puls routing', &
     &         'Storage values in outflow/storage table for each lake using Puls routing', &
     &         'cfs-days')/=0 ) CALL read_error(1, 's2')
          IF ( declparam(MODNAME, 'nsos', 'nlake', 'integer', &
     &         '0', '0', '10', &
     &         'Number of storage/outflow values in table for Puls routing', &
     &         'Number of storage/outflow values in table for each lake using Puls routing', &
     &         'none')/=0 ) CALL read_error(1, 'nsos')
        ENDIF

! Declared Parameters for broad-crested weir or gate opening routing
        ALLOCATE ( Elevlake_init(Nlake) )
        IF ( declparam(MODNAME, 'elevlake_init', 'nlake', 'real', &
     &       '100.0', '-10000.0', '10000.0', &
     &       'Initial lake surface elevation', &
     &       'Initial lake surface elevation for each lake using broad-crested weir or gate opening routing', &
     &       'feet')/=0 ) CALL read_error(1, 'elevlake_init')

        ALLOCATE ( Lake_vol_init(Nlake) )
        IF ( declparam(MODNAME, 'lake_vol_init', 'nlake', 'real', &
     &       '100.0', '0.0', '10000.0', &
     &       'Initial lake volume', &
     &       'Initial lake volume for each lake using broad-crested weir or gate opening routing', &
     &       'acre-feet')/=0 ) CALL read_error(1, 'lake_vol_init')

! Declared Parameters for broad-crested weir routing
        IF ( declparam(MODNAME, 'weir_coef', 'nlake', 'real', &
     &       '2.7', '2.0', '3.0', &
     &       'Broad-crested weir coefficent', &
     &       'Coefficient for each lake using broad-crested weir equation', &
     &       'none')/=0 ) CALL read_error(1, 'weir_coef')
        IF ( declparam(MODNAME, 'weir_len', 'nlake', 'real', &
     &       '5.0', '1.0', '1000.0', &
     &       'Broad-crested weir length', &
     &       'Weir length for each lake using broad-crested weir equation', &
     &       'feet')/=0 ) CALL read_error(1, 'weir_len')
        IF ( declparam(MODNAME, 'elev_outflow', 'nlake', 'real', &
     &       '100.0', '-10000.0', '10000.0', &
     &       'Elevation of the main outflow point', &
     &       'Elevation of the main outflow point for each lake using broad-crested weir routing', &
     &       'feet')/=0 ) CALL read_error(1, 'elev_outflow')

! Declared Parameters for gate opening routing
        IF ( Nratetbl>0 ) THEN
          IF ( declparam(MODNAME, 'ratetbl_lake', 'nratetbl', 'integer', &
     &         '1', 'bounded', 'nlake', &
     &         'Index of lake associated with each rating table', &
     &         'Index of lake associated with each rating table for'// &
     &         ' each lake using gate opening routing', &
     &         'none')/=0 ) CALL read_error(1, 'ratetbl_lake')
          IF ( declparam(MODNAME, 'rate_table', 'nstage,ngate', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Rating table 1 with stage (rows) and gate opening (cols)', &
     &         'Rating table with stage (rows) and gate opening'// &
     &         ' (cols) for the first lake using gate opening routing', &
     &         'cfs')/=0 ) CALL read_error(1, 'rate_table')
          IF ( declparam(MODNAME, 'tbl_stage', 'nstage', 'real', &
     &         '1.0', '0.0', '10000.0', &
     &         'Stage values for each row of rating table 1', &
     &         'Stage values for each row of the rating table for the'// &
     &         ' first lake using gate opening routing', &
     &         'feet')/=0 ) CALL read_error(1, 'tbl_stage')
          IF ( declparam(MODNAME, 'tbl_gate', 'ngate', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Gate openings for each column of rating table 1', &
     &         'Gate openings for each column of the rating table'// &
     &         ' for the first lake using gate opening routing', &
     &         'inches')/=0 ) CALL read_error(1, 'tbl_gate')
        ENDIF

        IF ( Nratetbl>1 ) THEN
          IF ( declparam(MODNAME, 'rate_table2', 'nstage2,ngate2', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Rating table 2 with stage (rows) and gate opening (cols)', &
     &         'Rating table with stage (rows) and gate opening'// &
     &         ' (cols) for the second lake using gate opening routing', &
     &         'cfs')/=0 ) CALL read_error(1, 'rate_table2')
          IF ( declparam(MODNAME, 'tbl_stage2', 'nstage2', 'real', &
     &         '1.0', '0.0', '10000.0', &
     &         'Stage values for each row of rating table 2', &
     &         'Stage values for each row of the rating table for the'// &
     &         ' second lake using gate opening routing', &
     &         'feet')/=0 ) CALL read_error(1, 'tbl_stage2')
          IF ( declparam(MODNAME, 'tbl_gate2', 'ngate2', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Gate openings for each column of rating table 2', &
     &         'Gate openings for each column of the rating table'// &
     &         ' for the second lake using gate opening routing', &
     &         'inches')/=0 ) CALL read_error(1, 'tbl_gate2')
        ENDIF

        IF ( Nratetbl>2 ) THEN
          IF ( declparam(MODNAME, 'rate_table3', 'nstage3,ngate3', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Rating table 3 with stage (rows) and gate opening (cols)', &
     &         'Rating table with stage (rows) and gate opening'// &
     &         ' (cols) for the third lake using gate opening routing', &
     &         'cfs')/=0 ) CALL read_error(1, 'rate_table3')
          IF ( declparam(MODNAME, 'tbl_stage3', 'nstage3', 'real', &
     &         '1.0', '0.0', '10000.0', &
     &         'Stage values for each row of rating table 3', &
     &         'Stage values for each row of the rating table for the'// &
     &         ' third lake using gate opening routing', &
     &         'feet')/=0 ) CALL read_error(1, 'tbl_stage3')
          IF ( declparam(MODNAME, 'tbl_gate3', 'ngate3', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Gate openings for each column of rating table 3', &
     &         'Gate openings for each column of the rating table'// &
     &         ' for the third lake using gate opening routing', &
     &         'inches')/=0 ) CALL read_error(1, 'tbl_gate3')
        ENDIF

        IF ( Nratetbl>3 ) THEN
          IF ( declparam(MODNAME, 'rate_table4', 'nstage4,ngate4', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Rating table 4 with stage (rows) and gate opening (cols)', &
     &         'Rating table with stage (rows) and gate opening'// &
     &         ' (cols) for the fourth lake using gate opening routing', &
     &         'cfs')/=0 ) CALL read_error(1, 'rate_table4')
          IF ( declparam(MODNAME, 'tbl_stage4', 'nstage4', 'real', &
     &         '1.0', '0.0', '10000.0', &
     &         'Stage values for each row of rating table 4', &
     &         'Stage values for each row of the rating table for the'// &
     &         ' fourth lake using gate opening routing', &
     &         'feet')/=0 ) CALL read_error(1, 'tbl_stage4')
          IF ( declparam(MODNAME, 'tbl_gate4', 'ngate4', 'real', &
     &         '5.0', '1.0', '1000.0', &
     &         'Gate openings for each column of rating table 4', &
     &         'Gate openings for each column of the rating table'// &
     &         ' for the fourth lake using gate opening routing', &
     &         'inches')/=0 ) CALL read_error(1, 'tbl_gate4')
        ENDIF

! Declared Parameters for lakes with lake outflow set to measured streamflow
        IF ( declparam(MODNAME, 'obsout_lake', 'nlake', 'integer', &
     &       '0', 'bounded', 'nobs', &
     &       'Index of streamflow measurement station that specifies outflow from a lake', &
     &       'Index of streamflow measurement station that specifies outflow from each lake (lake_type=6)', &
     &       'none')/=0 ) CALL read_error(1, 'obsout_lake')

! Declared Parameters for lakes with a second outlet and gate opening routing
        IF ( declparam(MODNAME, 'lake_out2', 'nlake', 'integer', &
     &       '0', '0', '1', &
     &       'Switch to specify a second outlet from a lake', &
     &       'Switch to specify a second outlet from each lake (0=no; 1=yes)', &
     &       'none')/=0 ) CALL read_error(1, 'lake_out2')
        IF ( declparam(MODNAME, 'lake_out2_a', 'nlake', 'real', &
     &       '1.0', '0.0', '10000.0', &
     &       'Outflow coefficient A for each lake with second outlet', &
     &       'Coefficient A in outflow equation for each lake with a second outlet', &
     &       'cfs/ft')/=0 ) CALL read_error(1, 'lake_out2_a')
        IF ( declparam(MODNAME, 'lake_out2_b', 'nlake', 'real', &
     &       '100.0', '0.0', '10000.0', &
     &       'Outflow coefficient A for each lake with second outlet', &
     &       'Coefficient B in outflow equation for each lake with a second outlet', &
     &       'cfs')/=0 ) CALL read_error(1, 'lake_out2_b')
      ENDIF

      END FUNCTION strmlkdecl

!***********************************************************************
!     strmlkinit - Initialize strmlake module - get parameter values,
!                  compute initial values
!***********************************************************************
      INTEGER FUNCTION strmlkinit()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Inputerror_flag, Parameter_check_flag, Nlake, Nratetbl, Nsegment, Cascade_flag
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO, Segment_hruarea, Active_hrus, Hru_route_order, &
     &    CFS2CMS_CONV, Basin_area_inv, Gwr_type, Lake_hru, Lake_hru_id, Segment_type, Hru_segment, Lake_type
      USE PRMS_FLOWVARS, ONLY: Basin_lake_stor, Elevlake
      USE PRMS_OBS, ONLY: Nobs
      IMPLICIT NONE
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, ierr, j, kk, ii
      REAL :: tmp
!***********************************************************************
      strmlkinit = 0

      IF ( Nlake>0 ) THEN
        Noarea_flag = 0
        Lake_id = 0
        DO i = 1, Nsegment
          IF ( Segment_hruarea(i)<DNEARZERO ) Noarea_flag = 1
          DO kk = 1, Active_hrus
            j = Hru_route_order(kk)
            IF ( Hru_segment(j)==i ) THEN
              IF ( Segment_type(i)==2 ) Lake_id(i) = Lake_hru_id(j)
            ENDIF
          ENDDO
        ENDDO
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
        Lake_outq2 = 0.0D0
        Lake_stream_in = 0.0D0
        Lake_lateral_inflow = 0.0D0
        IF ( Cascade_flag==1 ) THEN
          Lake_sroff = 0.0D0
          Lake_interflow = 0.0D0
        ENDIF
      ENDIF

      Linear_flag = 0
      Weir_flag = 0
      Puls_flag = 0
      Weir_rate_flag = 0
      Puls_lin_flag = 0
      Obs_flag = 0
      Gate_flag = 0
      IF ( Nlake>0 ) THEN
        IF ( getparam(MODNAME, 'lake_qro', Nlake, 'real', Lake_qro)/=0 ) CALL read_error(2, 'lake_qro')
        DO j = 1, Nlake
          Lake_outcfs(j) = Lake_qro(j)
          Lake_outcms(j) = Lake_qro(j)*CFS2CMS_CONV
          IF ( Lake_type(j)==1 .OR. Lake_type(j)==2 ) THEN
            Puls_lin_flag = 1
            IF ( Lake_type(j)==2 ) Linear_flag = 1
            IF ( Lake_type(j)==1 ) Puls_flag = 1
          ELSEIF ( Lake_type(j)==4 .OR. Lake_type(j)==5 ) THEN
            Weir_rate_flag = 1
            IF ( Lake_type(j)==4 ) Weir_flag = 1
            IF ( Lake_type(j)==5 ) Gate_flag = 1
          ELSEIF ( Lake_type(j)==6 ) THEN
            Obs_flag = 1
          ENDIF
        ENDDO

        secondoutflow_flag = 0
        IF ( Gate_flag==1 ) THEN
          IF ( Nratetbl<1 ) STOP 'ERROR, nratetbl = 0 and gate opening routing requested'
          IF ( getparam(MODNAME, 'rate_table', Nstage*Ngate, 'real', Rate_table)/=0 ) CALL read_error(2, 'rate_table')
          IF ( getparam(MODNAME, 'tbl_stage', Nstage, 'real', Tbl_stage)/=0 ) CALL read_error(2, 'tbl_stage')
          IF ( getparam(MODNAME, 'tbl_gate', Ngate, 'real', Tbl_gate)/=0 ) CALL read_error(2, 'tbl_gate')
          IF ( getparam(MODNAME, 'ratetbl_lake', Nratetbl, 'integer', Ratetbl_lake)/=0 ) CALL read_error(2, 'ratetbl_lake')
          IF ( getparam(MODNAME, 'lake_out2', Nlake, 'integer', Lake_out2)/=0  ) CALL read_error(2, 'lake_out2')
          DO j = 1, Nlake
            IF ( Lake_out2(j)==1 ) secondoutflow_flag = 1
          ENDDO

          IF ( Nratetbl>1 ) THEN
            IF ( getparam(MODNAME, 'rate_table2', Nstage2*Ngate2, 'real', Rate_table2)/=0 ) CALL read_error(2, 'rate_table2')
            IF ( getparam(MODNAME, 'tbl_stage2', Nstage2, 'real', Tbl_stage2)/=0 ) CALL read_error(2, 'tbl_stage2')
            IF ( getparam(MODNAME, 'tbl_gate2', Ngate2, 'real', Tbl_gate2)/=0 ) CALL read_error(2, 'tbl_gate2')
          ENDIF
          IF ( Nratetbl>2 ) THEN
            IF ( getparam(MODNAME, 'rate_table3',Nstage3*Ngate3, 'real', Rate_table3)/=0 ) CALL read_error(2, 'rate_table3')
            IF ( getparam(MODNAME, 'tbl_stage3', Nstage3, 'real', Tbl_stage3)/=0 ) CALL read_error(2, 'tbl_stage3')
            IF ( getparam(MODNAME, 'tbl_gate3', Ngate3, 'real', Tbl_gate3)/=0 ) CALL read_error(2, 'tbl_gate3')
          ENDIF
          IF ( Nratetbl>3 ) THEN
            IF ( getparam(MODNAME, 'rate_table4', Nstage4*Ngate4, 'real', Rate_table4)/=0 ) CALL read_error(2, 'rate_table4')
            IF ( getparam(MODNAME, 'tbl_stage4', Nstage4, 'real', Tbl_stage4)/=0 ) CALL read_error(2, 'tbl_stage4')
            IF ( getparam(MODNAME, 'tbl_gate4', Ngate4, 'real', Tbl_gate4)/=0 ) CALL read_error(2, 'tbl_gate4')
          ENDIF
        ELSE
          DEALLOCATE ( Lake_out2 )
        ENDIF

        IF ( Puls_lin_flag==1 ) THEN
          IF ( getparam(MODNAME, 'lake_init', Nlake, 'real', Lake_init)/=0 ) CALL read_error(2, 'lake_init')
          IF ( getparam(MODNAME, 'lake_din1', Nlake, 'real', Lake_din1)/=0 ) CALL read_error(2, 'lake_din1')
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

        IF ( Weir_rate_flag==1 ) THEN
          IF ( getparam(MODNAME, 'elevlake_init', Nlake, 'real', Elevlake_init)/=0 ) CALL read_error(2, 'elevlake_init')
          IF ( getparam(MODNAME, 'lake_vol_init', Nlake, 'real', Lake_vol_init)/=0 ) CALL read_error(2, 'lake_vol_init')
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

        IF ( secondoutflow_flag==1 ) THEN
          IF ( getparam(MODNAME, 'lake_out2_a', Nlake, 'real', Lake_out2_a)/=0 ) CALL read_error(2, 'lake_out2_a')
          IF ( getparam(MODNAME, 'lake_out2_b', Nlake, 'real', Lake_out2_b)/=0 ) CALL read_error(2, 'lake_out2_b')
        ELSE
          DEALLOCATE ( Lake_out2_a, Lake_out2_b )
        ENDIF

        Basin_lake_stor = 0.0D0
        Din1 = 0.0
        Elevlake = 0.0
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
            ELSEIF ( Lake_type(j)==2 ) THEN
              IF ( Lake_coef(j)<0.0001 ) THEN
                PRINT *, 'lake_coef must be at least 0.0001'
                IF ( Parameter_check_flag==1 ) THEN
                  PRINT *, 'ERROR, for lake:', j, Lake_coef(j)
                  ierr = 1
                ELSE
                  PRINT *, 'WARNING, for lake:', j, Lake_coef(j), ' set to 0.0001'
                  Lake_coef(j) = 0.0001
                ENDIF
              ENDIF
            ENDIF
          ELSEIF ( Lake_type(j)==4 .OR. Lake_type(j)==5 ) THEN
            IF ( Lake_vol_init(j)<0.0 ) THEN
              PRINT *, 'ERROR, lake_vol_init < 0.0 for lake:', j, Lake_vol_init(j)
              ierr = 1
            ENDIF
            IF ( Lake_hru(j)==0 ) THEN
              PRINT *, 'ERROR, lake_type = 4 or 5 and lake_hru for lake:', j, ' specified as 0'
              ierr = 1
            ENDIF
!            IF ( Lake_type(j)==4 ) THEN
!              IF ( Elev_outflow(j)<0.0 ) THEN
!                PRINT *, 'ERROR, elev_outflow < 0.0 for lake:', j, Elev_outflow(j)
!                ierr = 1
!              ENDIF
!            ENDIF
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
          IF ( Lake_type(j)==1 .OR. Lake_type(j)==2 ) THEN
! stoin, a local variable was removed, unused
!            Stoin(j) = (Lake_init(j)*23.76)*Basin_area_inv
            Lake_sto(j) = Lake_init(j)
            Din1(j) = Lake_din1(j)
            IF ( Lake_type(j)==1 ) THEN
              kk = Nsos(j)
              DO ii = 1, kk
                Wvd(ii, j) = S2(ii, j) + O2(ii, j)*0.5
              ENDDO
              DO ii = 2, kk
                tmp = Wvd(ii, j) - Wvd(ii-1, j)
                IF ( ABS(tmp)<NEARZERO ) tmp = 1.0
                S24(ii, j) = (O2(ii, j)-O2(ii-1, j))/tmp
                C24(ii, j) = O2(ii, j) - S24(ii, j)*Wvd(ii, j)
              ENDDO
            ENDIF
          ELSEIF ( Lake_type(j)==4 .OR. Lake_type(j)==5 ) THEN
            IF ( Lake_hru(j)==0 ) THEN
              PRINT *, 'ERROR, lake_type = 4 or 5 and lake_hru for lake:', j, ' specified as 0'
              Inputerror_flag = 1
              CYCLE
            ENDIF
            Gwr_type(Lake_hru(j)) = 2
            Elevlake(j) = Elevlake_init(j)
            Lake_vol(j) = Lake_vol_init(j)
            Basin_lake_stor = Basin_lake_stor + Lake_vol(j)*12.0D0
          ENDIF
        ENDDO
        Basin_lake_stor = Basin_lake_stor*Basin_area_inv

        DEALLOCATE ( Lake_init, Elevlake_init, Lake_vol_init, Lake_din1, Lake_qro )
        IF ( Mxnsos>0 ) DEALLOCATE ( O2, S2 )
      ENDIF

      END FUNCTION strmlkinit

!***********************************************************************
!     strmlkrun - Computes basin streamflow and on-channel reservoir
!                 storage and outflows
!***********************************************************************
      INTEGER FUNCTION strmlkrun()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Nsegment, Nsegmentp1, Nlake, Cascade_flag
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Hru_area, Basin_area_inv, Hru_route_order, Active_hrus, &
     &    DNEARZERO, Lake_hru_id, Hru_type, Segment_hruarea, &
     &    Hru_segment, Tosegment, Order, Obsin_segment, Segment_type, Lake_type
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Ssres_flow, Sroff, Hru_outflow, &
     &    Basin_cms, Basin_gwflow_cfs, Basin_ssflow_cfs, Basin_stflow_out, Basin_cfs, &
     &    Basin_stflow_in, Basin_sroff_cfs, Seginc_gwflow, Seginc_sroff, Seg_lateral_inflow, &
     &    Seginc_swrad, Seginc_ssflow, Hru_actet, Basin_lake_stor, Basin_2ndstflow, &
     &    Seg_inflow, Seg_outflow, Seg_upstream_inflow
      USE PRMS_OBS, ONLY: Cfs_conv, Timestep_seconds, Streamflow_cfs
      USE PRMS_SRUNOFF, ONLY: Basin_sroff, Hortonian_lakes
      USE PRMS_SOILZONE, ONLY: Upslope_dunnianflow, Upslope_interflow
      USE PRMS_GWFLOW, ONLY: Gwres_flow, Basin_gwflow, Lake_seepage, Gw_seep_lakein
      IMPLICIT NONE
! Functions
      EXTERNAL route_lake
! Local Variables
      INTEGER :: i, j, jj, lakeid, iorder, toseg
      DOUBLE PRECISION :: area_fac, tocfs, last_stor
!***********************************************************************
      strmlkrun = 0

      Seginc_gwflow = 0.0
      Seginc_ssflow = 0.0
      Seginc_sroff = 0.0
      Seginc_swrad = 0.0
      Seg_lateral_inflow = 0.0D0
      DO i = 1, Nsegment
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          tocfs = Hru_area(j)*Cfs_conv
          Hru_outflow(j) = (Sroff(j) + Ssres_flow(j) + Gwres_flow(j))*tocfs
          IF ( Hru_segment(j)==i ) THEN
            Seg_lateral_inflow(i) = Seg_lateral_inflow(i) + Hru_outflow(j)
            Seginc_sroff(i) = Seginc_sroff(i) + Sroff(j)*tocfs
            Seginc_ssflow(i) = Seginc_ssflow(i) + Ssres_flow(j)*tocfs
            Seginc_gwflow(i) = Seginc_gwflow(i) + Gwres_flow(j)*tocfs
            Seginc_swrad(i) = Seginc_swrad(i) + Swrad(j)*Hru_area(j)
          ENDIF
        ENDDO
      ENDDO

! Divide solar radiation by sum of HRU area to get avarage
      DO i = 1, Nsegment
        IF ( Segment_hruarea(i)>DNEARZERO ) Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
      ENDDO

! If there are no HRUs associated with a segment, then figure out some
! other way to get the solar radiation, the following is not great
      IF ( Noarea_flag==1 ) THEN
        DO i = 1, Nsegment
          IF ( Segment_hruarea(i)<DNEARZERO ) THEN
            IF ( Tosegment(i)<Nsegmentp1 ) THEN
              Seginc_swrad(i) = Seginc_swrad(Tosegment(i))
            ELSEIF ( i>1 ) THEN
              Seginc_swrad(i) = Seginc_swrad(i-1)
            ELSE
              Seginc_swrad(i) = Seginc_swrad(i+1)
            ENDIF
          ENDIF
        ENDDO
      ENDIF

      Cfs2acft = Timestep_seconds/43560.0D0

      IF ( Nlake>0 ) THEN
        Lake_precip = 0.0D0
        Lake_seep_in = 0.0D0
        Lake_evap = 0.0D0
        Lake_2gw = 0.0D0
        Lake_inflow = 0.0D0
        Lake_outflow = 0.0D0
        Lake_stream_in = 0.0D0
        last_stor = Basin_lake_stor
        Basin_lake_stor = 0.0D0
        Basin_2ndstflow = 0.0D0
        Lake_outq2 = 0.0D0
        Lake_lateral_inflow = 0.0D0
        IF ( Cascade_flag==1 ) THEN
          Lake_sroff = 0.0D0
          Lake_interflow = 0.0D0
        ENDIF
        ! shouldn't have snowpack, all precipitation should be added directly to lake
        ! units of lake_inflow = cfs
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          IF ( Hru_type(j)/=2 ) CYCLE
          tocfs = Hru_area(j)*Cfs_conv
          lakeid = Lake_hru_id(j)
          Lake_precip(lakeid) = Lake_precip(lakeid) + tocfs*Hru_ppt(j)
          IF ( Cascade_flag==1 ) THEN
            Lake_sroff(lakeid) = Lake_sroff(lakeid) + &
     &                           tocfs*(Hortonian_lakes(j)+Upslope_dunnianflow(j))
            Lake_interflow(lakeid) = Lake_interflow(lakeid) + tocfs*Upslope_interflow(j)
          ENDIF
          Lake_seep_in(lakeid) = Lake_seep_in(lakeid) + tocfs*Gw_seep_lakein(j)
          Lake_evap(lakeid) = Lake_evap(lakeid) + tocfs*Hru_actet(j)
          Lake_2gw(lakeid) = Lake_2gw(lakeid) + tocfs*Lake_seepage(j)
        ENDDO
        DO lakeid = 1, Nlake
          Lake_inflow(lakeid) = Lake_precip(lakeid) + Lake_seep_in(lakeid)
          IF ( Cascade_flag==1 ) THEN
            Lake_lateral_inflow(lakeid) = Lake_sroff(lakeid) + Lake_interflow(lakeid)
            Lake_inflow(lakeid) = Lake_inflow(lakeid) + Lake_lateral_inflow(lakeid)
          ENDIF
          Lake_outflow(lakeid) = Lake_evap(lakeid) + Lake_2gw(lakeid)
        ENDDO
      ENDIF
      Seg_inflow = 0.0
      Seg_outflow = 0.0

      Seg_upstream_inflow = 0.0D0
      Flow_out = 0.0D0
      DO i = 1, Nsegment
        iorder = Order(i)
        toseg = Tosegment(iorder)
! add up flows to stream segments
        IF ( Obsin_segment(iorder)>0 ) Seg_upstream_inflow(iorder) = Streamflow_cfs(Obsin_segment(iorder))
        Seg_inflow(iorder) = Seg_upstream_inflow(iorder) + Seg_lateral_inflow(iorder)
        Seg_outflow(iorder) = Seg_inflow(iorder)
! Flow_out is the total flow out of the basin, which allows for multiple outlets
        IF ( toseg==Nsegmentp1 ) THEN ! lake cannot be an outlet to a basin
          Flow_out = Flow_out + DBLE( Seg_outflow(iorder) )
          CYCLE
        ELSE
          Seg_upstream_inflow(toseg) = Seg_upstream_inflow(toseg) + Seg_outflow(iorder)
        ENDIF ! compute lake routing
        IF ( Nlake>0 ) THEN
          IF ( Segment_type(iorder)==2 ) THEN
            lakeid = Lake_id(iorder)
            Lake_inflow(lakeid) = Lake_inflow(lakeid) + Seg_inflow(iorder)
            CALL route_lake(lakeid, Lake_type(lakeid), Lake_inflow(lakeid), Lake_outflow(lakeid), &
      &                     Lake_outcfs(lakeid), Obsout_lake(lakeid))
            Lake_outcms(lakeid) = Lake_outcfs(lakeid)*CFS2CMS_CONV
            Seg_outflow(iorder) = Lake_outcfs(lakeid)
            Lake_stream_in(lakeid) = Seg_inflow(iorder)
          ENDIF
        ENDIF
      ENDDO   ! end segment and lake routing loop

      area_fac = Cfs_conv/Basin_area_inv
      Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow
      Basin_cfs = Flow_out
      Basin_stflow_out = Basin_cfs / area_fac
      Basin_cms = Basin_cfs*CFS2CMS_CONV
      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac
      Basin_lake_stor = Basin_lake_stor*Basin_area_inv
      Basin_2ndstflow = Basin_2ndstflow*Basin_area_inv

      END FUNCTION strmlkrun

!     ***********************************
!     * Route Lake
!     ***********************************
      SUBROUTINE route_lake(Lakeid, Lake_type, Lake_inflow, Lake_outflow, Lake_outcfs, Obsout_lake)
      USE PRMS_STRMFLOW_LAKE, ONLY: Cfs2acft, Din1, Lake_sto, Nsos, Wvd, S24, C24, Lake_coef, Lake_vol, &
     &    Lake_invol, Elev_outflow, Ngate, Nstage, Ngate2, Nstage2, Ngate3, Nstage3, Ngate4, Nstage4, &
     &    Tbl_gate, Tbl_stage, Rate_table, Tbl_gate2, Tbl_stage2, Rate_table2, Tbl_gate3, Tbl_stage3, Rate_table3, &
     &    Tbl_gate4, Tbl_stage4, Rate_table4, Ratetbl_lake, Weir_coef, Weir_len, Lake_outvol, Secondoutflow_flag, &
     &    Lake_out2, Lake_out2_a, Lake_out2_b, Lake_outq2
      USE PRMS_MODULE, ONLY: Nratetbl
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO, Lake_area
      USE PRMS_OBS, ONLY: Gate_ht, Streamflow_cfs
      USE PRMS_FLOWVARS, ONLY: Basin_lake_stor, Elevlake, Basin_2ndstflow
      IMPLICIT NONE
! Functions
      INTRINSIC EXP
      EXTERNAL table_comp
! Arguments
      INTEGER, INTENT(IN) :: Lakeid, Lake_type, Obsout_lake
      DOUBLE PRECISION, INTENT(IN) :: Lake_inflow, Lake_outflow
      DOUBLE PRECISION, INTENT(INOUT) :: Lake_outcfs
! Local Variables
      INTEGER :: n, jjj, i
      REAL :: xkt, c2, elevold, q1, q3, head, new_elevlake, head2, scnd_cfs1, scnd_cfs2
      DOUBLE PRECISION :: avin, s2o2, q2, old_lake_vol, lake_out, diff_vol, lake_out1
!***********************************************************************
      ! q2 = lake out in cfs
      q2 = 0.0D0
!   Compute outflow using Puls routing method
      IF ( Lake_type==1 ) THEN
        !rsr, why half of current in and last in???
        avin = (Lake_inflow+Din1(Lakeid))*0.5D0
        s2o2 = Lake_sto(Lakeid) - (Lake_outflow+Lake_outcfs)*0.5D0
        IF ( s2o2<DNEARZERO ) THEN
          PRINT *, 'Warning: lake storage for Puls routing < 0 lake:', Lakeid, s2o2, ' set to 0'
          PRINT *, 'Probably need to increase lake_init'
          s2o2 = 0.0D0
        ENDIF
        s2o2 = s2o2 + avin
        n = Nsos(Lakeid)
        DO jjj = 2, Nsos(Lakeid)
          IF ( s2o2<Wvd(jjj, Lakeid) ) THEN
            n = jjj
            EXIT
          ENDIF
        ENDDO
        q2 = S24(n, Lakeid)*s2o2 + C24(n, Lakeid)

        IF ( q2<DNEARZERO ) q2 = 0.0D0
        Lake_sto(Lakeid) = s2o2 - q2*0.5D0
        IF ( Lake_sto(Lakeid)<DNEARZERO ) THEN
          PRINT *, 'Warning: lake storage for Puls routing < 0 lake:', Lakeid, Lake_sto(Lakeid), ' set to 0'
          PRINT *, 'Probably need to increase lake_init'
          q2 = s2o2 + (Lake_outcfs)*0.5D0
          Lake_sto(Lakeid) = 0.0D0
        ENDIF

!   Compute outflow using linear reservoir method
      ELSEIF ( Lake_type==2 ) THEN
        !rsr, why half of current in and last in???
        avin = (Lake_inflow+Din1(Lakeid)-Lake_outflow)*0.5D0
        xkt = Lake_coef(Lakeid)
        c2 = 1.0 - EXP(-xkt)
        q2 = (avin*(1.0D0-(c2/xkt))) + Lake_sto(Lakeid)*c2
        IF ( q2<DNEARZERO ) q2 = 0.0D0
        Lake_sto(Lakeid) = Lake_sto(Lakeid) + avin - q2
        IF ( Lake_sto(Lakeid)<DNEARZERO ) THEN
          PRINT *, 'Warning, lake storage for linear routing < 0 lake:', Lakeid, Lake_sto(Lakeid), ' set to 0'
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
        elevold = Elevlake(Lakeid)
        old_lake_vol = Lake_vol(Lakeid)

        ! units lake_invol = acft
        Lake_invol(Lakeid) = Lake_inflow*Cfs2acft

        ! units lake_out = acft
        lake_out = (Lake_outflow/12.0D0)*Lake_area(Lakeid)
        diff_vol = Lake_invol(Lakeid) - lake_out
        q1 = 0.0
        q3 = 0.0

!   Compute using lake surface elevation and broad crested weir
        IF ( Lake_type==4 ) THEN
          head = elevold - Elev_outflow(Lakeid)
          IF ( head>NEARZERO ) q1 = (head**1.5) * Weir_coef(Lakeid) * Weir_len(Lakeid)
          lake_out1 = q1*Cfs2acft

          ! new_elevlake has units of feet
          new_elevlake = elevold + (diff_vol-lake_out1)/Lake_area(Lakeid)

          head2 = (new_elevlake+elevold)*0.5 - Elev_outflow(Lakeid)
          IF ( head2>NEARZERO ) q3 = (head2**1.5) * Weir_coef(Lakeid) * Weir_len(Lakeid)

!  Compute using a rating table of lake surface elevation & gate opening
        ELSE ! type = 5
          DO i = 1, Nratetbl
            IF ( Lakeid==Ratetbl_lake(i) ) THEN
              IF ( i==1 ) THEN
                CALL table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage, &
     &                          Rate_table, elevold, Gate_ht(i), q1, Lake_area(Lakeid))
              ELSEIF ( i==2 ) THEN
                CALL table_comp(Ngate2, Nstage2, Tbl_gate2, Tbl_stage2, &
     &                          Rate_table2, elevold, Gate_ht(i), q1, Lake_area(Lakeid))
              ELSEIF ( i==3 ) THEN
                CALL table_comp(Ngate3, Nstage3, Tbl_gate3, Tbl_stage3, &
     &                          Rate_table3, elevold, Gate_ht(i), q1, Lake_area(Lakeid))
              ELSEIF ( i==4 ) THEN
                CALL table_comp(Ngate4, Nstage4, Tbl_gate4, Tbl_stage4, &
     &                          Rate_table4, elevold, Gate_ht(i), q1, Lake_area(Lakeid))
              ENDIF
            ENDIF
          ENDDO
          IF ( Secondoutflow_flag==1 ) THEN
!  if lake has a second outlet then outflow in cfs is computed by
!           Q = (Lake_out2_a * Elevlake) - Lake_out2_b
!               (as per Rob Dudley email 7 Sep 2006)
            IF ( Lake_out2(Lakeid)==1 ) THEN
              scnd_cfs1 = (Lake_out2_a(Lakeid)*elevold) - Lake_out2_b(Lakeid)
              IF ( scnd_cfs1<DNEARZERO ) scnd_cfs1 = 0.0D0
            ELSE
              scnd_cfs1 = 0.0D0
            ENDIF

            lake_out1 = (q1 + scnd_cfs1)*Cfs2acft
          ENDIF

          ! new_elevlake has units of feet
          new_elevlake = elevold + (diff_vol-lake_out1)/Lake_area(Lakeid)

          DO i = 1, Nratetbl
            IF ( Lakeid==Ratetbl_lake(i) ) THEN
              IF ( i==1 ) THEN
                CALL table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage, Rate_table, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area(Lakeid))
              ELSEIF ( i==2 ) THEN
                CALL table_comp(Ngate2, Nstage2, Tbl_gate2, Tbl_stage2, Rate_table2, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area(Lakeid))
              ELSEIF ( i==3 ) THEN
                CALL table_comp(Ngate3, Nstage3, Tbl_gate3, Tbl_stage3, Rate_table3, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area(Lakeid))
              ELSEIF ( i==4 ) THEN
                CALL table_comp(Ngate4, Nstage4, Tbl_gate4, Tbl_stage4, Rate_table4, &
     &                          new_elevlake, Gate_ht(i), q3, Lake_area(Lakeid))
              ENDIF
            ENDIF
          ENDDO

          IF ( Secondoutflow_flag==1 ) THEN
            IF ( Lake_out2(lakeid)==1 ) THEN
              scnd_cfs2 = (Lake_out2_a(Lakeid)*new_elevlake) - Lake_out2_b(Lakeid)
              IF ( scnd_cfs2<DNEARZERO ) scnd_cfs2 = 0.0D0
            ELSE
              scnd_cfs2 = 0.0D0
            ENDIF

            Lake_outq2(Lakeid) = (scnd_cfs1+scnd_cfs2)*0.5D0
            Basin_2ndstflow = Basin_2ndstflow + Lake_outq2(Lakeid)*Cfs2acft*12.0D0
          ENDIF
        ENDIF

        q2 = (q1+q3)*0.5
!       !sanity check, rsr
        IF ( q2<0.0D0 ) PRINT *, 'q2<0', q2, ' lake:', Lakeid

        Lake_outvol(Lakeid) = q2*Cfs2acft + lake_out + Lake_outq2(Lakeid)

        ! adjust lake storage
        Lake_vol(Lakeid) = Lake_vol(Lakeid) - Lake_outvol(Lakeid)
        IF ( Lake_vol(Lakeid)<DNEARZERO ) THEN
          PRINT *, 'Lake storage issue, lake_vol<0:', Lake_vol(Lakeid), ' lake:', Lakeid
          IF ( q2*Cfs2acft>Lake_vol(Lakeid) ) THEN
            PRINT *, 'stream flow out reduced and storage set to 0'
            q2 = q2 - Lake_vol(Lakeid)/Cfs2acft
            Lake_outvol(Lakeid) = q2*Cfs2acft + lake_out
            Lake_vol(Lakeid) = 0.0D0
          ELSE
            STOP 'ERROR, negative storage > available streamflow out'
          ENDIF
        ENDIF

        ! adjust lake elevation with stream and lateral inflows
        ! and streamflow, second outlet, GWR, and evaporation outflows
        Elevlake(Lakeid) = Elevlake(Lakeid) + (Lake_invol(Lakeid)-Lake_outvol(Lakeid))/Lake_area(Lakeid)
        Basin_lake_stor = Basin_lake_stor + Lake_vol(Lakeid)*12.0D0
      ENDIF

      Lake_outcfs = q2

      END SUBROUTINE route_lake

!=====================================================================
!    Rating table computation
!=====================================================================
      SUBROUTINE table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage, Rate_table, Elevlake, Gate_ht, Q2, Lake_area)
      USE PRMS_STRMFLOW_LAKE, ONLY: Cfs2acft
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
        PRINT*,'Warning, lake elevation > maximum stage in rating table all water above rating table spills'
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
!     strmflow_lake_restart - write or read restart file
!***********************************************************************
      SUBROUTINE strmflow_lake_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Nratetbl, Nlake, Cascade_flag
      USE PRMS_STRMFLOW_LAKE
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Function
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Flow_out, Obs_flag, Secondoutflow_flag, Noarea_flag, Linear_flag, &
     &                            Weir_flag, Puls_flag, Weir_rate_flag, Puls_lin_flag, Gate_flag
        IF ( Nlake>0 ) THEN
          WRITE ( Restart_outunit ) Lake_id
          WRITE ( Restart_outunit ) Lake_outcfs
          WRITE ( Restart_outunit ) Lake_outcms
          WRITE ( Restart_outunit ) Lake_inflow
          WRITE ( Restart_outunit ) Lake_outflow
          WRITE ( Restart_outunit ) Lake_stream_in
          WRITE ( Restart_outunit ) Lake_lateral_inflow
          WRITE ( Restart_outunit ) Lake_precip
          WRITE ( Restart_outunit ) Lake_seep_in
          WRITE ( Restart_outunit ) Lake_evap
          WRITE ( Restart_outunit ) Lake_2gw
        ENDIF
        IF ( Linear_flag==1 ) WRITE ( Restart_outunit ) Lake_coef
        IF ( Puls_flag==1 ) THEN
          WRITE ( Restart_outunit ) C24
          WRITE ( Restart_outunit ) S24
          WRITE ( Restart_outunit ) Wvd
          WRITE ( Restart_outunit ) Nsos
        ENDIF
        IF ( Puls_lin_flag==1 ) THEN
          WRITE ( Restart_outunit ) Din1
          WRITE ( Restart_outunit ) Lake_sto
          IF ( Mxnsos>0 ) WRITE ( Restart_outunit ) Nsos
        ENDIF
        IF ( Weir_rate_flag==1 ) THEN
          WRITE ( Restart_outunit ) Lake_invol
          WRITE ( Restart_outunit ) Lake_outvol
          WRITE ( Restart_outunit ) Lake_vol
        ENDIF
        IF ( Cascade_flag==1 ) THEN
          WRITE ( Restart_outunit ) Lake_sroff
          WRITE ( Restart_outunit ) Lake_interflow
        ENDIF
        IF ( Secondoutflow_flag==1 ) THEN
          WRITE ( Restart_outunit ) Lake_outq2
          WRITE ( Restart_outunit ) Lake_out2
          WRITE ( Restart_outunit ) Lake_out2_a
          WRITE ( Restart_outunit ) Lake_out2_b
        ENDIF
        IF ( Gate_flag==1 ) THEN
          IF ( Nratetbl>0 ) THEN
            WRITE ( Restart_outunit ) Ratetbl_lake
            WRITE ( Restart_outunit ) Rate_table
            WRITE ( Restart_outunit ) Tbl_stage
            WRITE ( Restart_outunit ) Tbl_gate
          ENDIF
          IF ( Nratetbl>1 ) THEN
            WRITE ( Restart_outunit ) Rate_table2
            WRITE ( Restart_outunit ) Tbl_stage2
            WRITE ( Restart_outunit ) Tbl_gate2
          ENDIF
          IF ( Nratetbl>2 ) THEN
            WRITE ( Restart_outunit ) Rate_table3
            WRITE ( Restart_outunit ) Tbl_stage3
            WRITE ( Restart_outunit ) Tbl_gate3
          ENDIF
          IF ( Nratetbl>3 ) THEN
            WRITE ( Restart_outunit ) Rate_table4
            WRITE ( Restart_outunit ) Tbl_stage4
            WRITE ( Restart_outunit ) Tbl_gate4
          ENDIF
        ENDIF
        IF ( Obs_flag==1 ) WRITE ( Restart_outunit, * )  Obsout_lake
        IF ( Weir_flag==1 ) THEN
          WRITE ( Restart_outunit ) Weir_coef
          WRITE ( Restart_outunit ) Weir_len
          WRITE ( Restart_outunit ) Elev_outflow
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Flow_out, Obs_flag, Secondoutflow_flag, Noarea_flag, Linear_flag, &
     &                          Weir_flag, Puls_flag, Weir_rate_flag, Puls_lin_flag, Gate_flag
        IF ( Nlake>0 ) THEN
          READ ( Restart_inunit ) Lake_id
          READ ( Restart_inunit ) Lake_outcfs
          READ ( Restart_inunit ) Lake_outcms
          READ ( Restart_inunit ) Lake_inflow
          READ ( Restart_inunit ) Lake_outflow
          READ ( Restart_inunit ) Lake_stream_in
          READ ( Restart_inunit ) Lake_lateral_inflow
          READ ( Restart_inunit ) Lake_precip
          READ ( Restart_inunit ) Lake_seep_in
          READ ( Restart_inunit ) Lake_evap
          READ ( Restart_inunit ) Lake_2gw
        ENDIF
        IF ( Linear_flag==1 ) READ ( Restart_inunit ) Lake_coef
        IF ( Puls_flag==1 ) THEN
          READ ( Restart_inunit ) C24
          READ ( Restart_inunit ) S24
          READ ( Restart_inunit ) Wvd
          READ ( Restart_inunit ) Nsos
        ENDIF
        IF ( Puls_lin_flag==1 ) THEN
          READ ( Restart_inunit ) Din1
          READ ( Restart_inunit ) Lake_sto
          IF ( Mxnsos>0 ) READ ( Restart_inunit ) Nsos
        ENDIF
        IF ( Weir_rate_flag==1 ) THEN
          READ ( Restart_inunit ) Lake_invol
          READ ( Restart_inunit ) Lake_outvol
          READ ( Restart_inunit ) Lake_vol
        ENDIF
        IF ( Cascade_flag==1 ) THEN
          READ ( Restart_inunit ) Lake_sroff
          READ ( Restart_inunit ) Lake_interflow
        ENDIF
        IF ( Secondoutflow_flag==1 ) THEN
          READ ( Restart_inunit ) Lake_outq2
          READ ( Restart_inunit ) Lake_out2
          READ ( Restart_inunit ) Lake_out2_a
          READ ( Restart_inunit ) Lake_out2_b
        ENDIF
        IF ( Gate_flag==1 ) THEN
          IF ( Nratetbl>0 ) THEN
            READ ( Restart_inunit ) Ratetbl_lake
            READ ( Restart_inunit ) Rate_table
            READ ( Restart_inunit ) Tbl_stage
            READ ( Restart_inunit ) Tbl_gate
          ENDIF
          IF ( Nratetbl>1 ) THEN
            READ ( Restart_inunit ) Rate_table2
            READ ( Restart_inunit ) Tbl_stage2
            READ ( Restart_inunit ) Tbl_gate2
          ENDIF
          IF ( Nratetbl>2 ) THEN
            READ ( Restart_inunit ) Rate_table3
            READ ( Restart_inunit ) Tbl_stage3
            READ ( Restart_inunit ) Tbl_gate3
          ENDIF
          IF ( Nratetbl>3 ) THEN
            READ ( Restart_inunit ) Rate_table4
            READ ( Restart_inunit ) Tbl_stage4
            READ ( Restart_inunit ) Tbl_gate4
          ENDIF
        ENDIF
        IF ( Obs_flag==1 ) READ ( Restart_inunit, * )  Obsout_lake
        IF ( Weir_flag==1 ) THEN
          READ ( Restart_inunit ) Weir_coef
          READ ( Restart_inunit ) Weir_len
          READ ( Restart_inunit ) Elev_outflow
        ENDIF
      ENDIF
      END SUBROUTINE strmflow_lake_restart
