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
      INTEGER, SAVE :: Nhrup1, Mxnsos, Num, Ngate, Nstage, Nratetbl
      INTEGER, SAVE :: Ngate2, Nstage2, Ngate3, Nstage3, Ngate4, Nstage4
      REAL, SAVE, ALLOCATABLE :: C24(:,:), S24(:,:),Wvd(:,:)
      REAL, SAVE, ALLOCATABLE :: Q_down(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sfres_stor, Basin_2ndstflow
      REAL, SAVE, ALLOCATABLE :: Sfres_sto(:), Din1(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_outq(:), Sfres_outcms(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_vol(:), Q_segment(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_outvol(:), Sfres_outq2(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_invol(:), Elevsurf(:)
!   Declared Parameters
      !rsr, no need for hru_sfres and sfres_gw
      INTEGER, SAVE, ALLOCATABLE :: Sfres_type(:), Nsos(:)
      INTEGER, SAVE, ALLOCATABLE :: Sfres_out2(:)
      INTEGER, SAVE, ALLOCATABLE :: Ratetbl_sfres(:), Obsout_lake(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_qro(:), Sfres_din1(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_init(:), Sfres_coef(:)
      REAL, SAVE, ALLOCATABLE :: O2(:,:), S2(:,:)
      REAL, SAVE, ALLOCATABLE :: Rate_table(:,:), Rate_table2(:,:)
      REAL, SAVE, ALLOCATABLE :: Rate_table3(:,:), Rate_table4(:,:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage(:), Tbl_gate(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage2(:), Tbl_gate2(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage3(:), Tbl_gate3(:)
      REAL, SAVE, ALLOCATABLE :: Tbl_stage4(:), Tbl_gate4(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_vol_init(:)
      REAL, SAVE, ALLOCATABLE :: Weir_coef(:), Weir_len(:)
      REAL, SAVE, ALLOCATABLE :: Elev_outflow(:), Elevsurf_init(:)
      REAL, SAVE, ALLOCATABLE :: Sfres_out2_a(:), Sfres_out2_b(:)
! Local Variables
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='strmflow_lake')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Streamflow Routing')
      END MODULE PRMS_STRMFLOW_LAKE

!***********************************************************************
!     Main daily and storm stream flow routine
!***********************************************************************
      INTEGER FUNCTION strmflow_lake()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: strmlkdecl, strmlkinit, strmlkrun
!***********************************************************************
      strmflow_lake = 0

      IF ( Process(:3)=='run' ) THEN
        strmflow_lake = strmlkrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        strmflow_lake = strmlkdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        strmflow_lake = strmlkinit()
      ENDIF

      END FUNCTION strmflow_lake

!***********************************************************************
!    strmlkdecl - set up parameters for streamflow and surface reservoir
!                 flow computations
!   Declared Parameters
!     sfres_type, sfres_init, sfres_qro, sfres_din1
!     sfres_coef, o2, s2, nsos, hru_area, sfres_hru
!     sfres_out2, tbl_stage, tbl_gate, sfres_vol_init, rate_table
!     weir_coef, weir_len, elev_outflow, elevsurf_init, sfres_out2_a
!     sfres_out2_b
!***********************************************************************
      INTEGER FUNCTION strmlkdecl()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Model, Nhru, Nssr, Ngw, Nsegment, Nsfres,
     +    Print_debug, Version_strmflow_lake, Strmflow_lake_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar, getdim
      EXTERNAL read_error
!***********************************************************************
      strmlkdecl = 1

      Version_strmflow_lake =
     +'$Id: strmflow_lake.f 4006 2011-11-30 00:17:49Z rsregan $'
      Strmflow_lake_nc = INDEX( Version_strmflow_lake, ' $' ) + 1
      IF ( Print_debug>-1 ) THEN
        IF ( declmodule(MODNAME, PROCNAME,
     +       Version_strmflow_lake(:Strmflow_lake_nc))/=0 )
     +       STOP
      ENDIF

      Nhrup1 = Nhru + 1

      Nratetbl = getdim('nratetbl')
      IF ( Nratetbl.EQ.-1 ) CALL read_error(6, 'nratetbl')
      IF ( Nratetbl>4 ) THEN
        PRINT *, 'Lake module allows maximum of 4 rating tables'
        STOP
      ENDIF

      Ngate = getdim('ngate')
      IF ( Ngate.EQ.-1 ) CALL read_error(6, 'ngate')

      Nstage = getdim('nstage')
      IF ( Nstage.EQ.-1 ) CALL read_error(6, 'nstage')

      Ngate2 = getdim('ngate2')
      IF ( Ngate2.EQ.-1 ) CALL read_error(6, 'ngate2')

      Nstage2 = getdim('nstage2')
      IF ( Nstage2.EQ.-1 ) CALL read_error(6, 'nstage2')

      Ngate3 = getdim('ngate3')
      IF ( Ngate3.EQ.-1 ) CALL read_error(6, 'ngate3')

      Nstage3 = getdim('nstage3')
      IF ( Nstage3.EQ.-1 ) CALL read_error(6, 'nstage3')

      Ngate4 = getdim('ngate4')
      IF ( Ngate4.EQ.-1 ) CALL read_error(6, 'ngate4')

      Nstage4 = getdim('nstage4')
      IF ( Nstage4.EQ.-1 ) CALL read_error(6, 'nstage4')

! Declared Variables
      ALLOCATE (Q_segment(Nsegment))
      IF ( declvar(MODNAME, 'q_segment', 'nsegment', Nsegment,
     +     'real',
     +     'Outflow from stream segment',
     +     'cfs',
     +     Q_segment).NE.0 ) CALL read_error(3, 'q_segment')

      Mxnsos = getdim('mxnsos')
      IF ( Mxnsos.EQ.-1 ) CALL read_error(1, 'mxnsos')

      ALLOCATE (Sfres_sto(Nsfres))
      IF ( declvar(MODNAME, 'sfres_sto', 'nsfres', Nsfres, 'real',
     +     'Storage in each lake HRU using Puls or linear storage'//
     +     ' routing',
     +     'cfs-days',
     +     Sfres_sto).NE.0 ) CALL read_error(3, 'sfres_sto')

      ALLOCATE (Sfres_vol(Nsfres))
      IF ( declvar(MODNAME, 'sfres_vol', 'nsfres', Nsfres, 'real',
     +     'Storage for lake HRUs using broad-crested weir or gate'//
     +     ' opening routing',
     +     'acre-feet',
     +     Sfres_vol).NE.0 ) CALL read_error(3, 'sfres_vol')

      ALLOCATE (Sfres_outq(Nsfres))
      IF ( declvar(MODNAME, 'sfres_outq', 'nsfres', Nsfres, 'real',
     +     'Streamflow leaving each lake HRU',
     +     'cfs',
     +     Sfres_outq).NE.0 ) CALL read_error(3, 'sfres_outq')

      ALLOCATE (Sfres_outcms(Nsfres))
      IF ( declvar(MODNAME, 'sfres_outcms', 'nsfres', Nsfres, 'real',
     +     'Streamflow leaving each lake HRU',
     +     'cms',
     +     Sfres_outcms).NE.0 ) CALL read_error(3, 'sfres_outcms')

      ALLOCATE (Din1(Nsfres))
      IF ( declvar(MODNAME, 'din1', 'nsfres', Nsfres, 'real',
     +     'Storage reservoir inflow from the previous time step',
     +     'cfs',
     +     Din1).NE.0 ) CALL read_error(3, 'din1')

      ALLOCATE (Elevsurf(Nsfres))
      IF ( declvar(MODNAME, 'elevsurf', 'nsfres', Nsfres, 'real',
     +     'Elevation for lake HRUs using broad-crested weir or gate'//
     +     ' opening routing',
     +     'feet',
     +     Elevsurf)/=0 ) CALL read_error(3, 'elevsurf')

      ALLOCATE (Elevsurf_init(Nsfres))
      IF ( declparam(MODNAME, 'elevsurf_init', 'nsfres', 'real',
     +     '100.0', '0.0', '10000.0',
     +     'Initial lake surface elevation',
     +     'Initial lake surface elevation for lake HRUs using'//
     +     ' broad-crested weir or gate opening routing',
     +     'feet')/=0 ) CALL read_error(1, 'elevsurf_init')

      ALLOCATE (Sfres_invol(Nsfres))
      IF ( declvar(MODNAME, 'sfres_invol', 'nsfres', Nsfres, 'real',
     +     'Volume of inflow to each lake HRU using ',
     +     'acre-feet',
     +     Sfres_invol).NE.0 ) CALL read_error(3, 'sfres_invol')

      ALLOCATE (Sfres_outvol(Nsfres))
      IF ( declvar(MODNAME, 'sfres_outvol', 'nsfres', Nsfres, 'real',
     +     'Volume of lake outflow',
     +     'acre-inches',
     +     Sfres_outvol).NE.0 ) CALL read_error(3, 'sfres_outvol')

      IF ( declvar(MODNAME, 'basin_sfres_stor', 'one', 1, 'double',
     +     'Basin reservoir storage',
     +     'inches',
     +     Basin_sfres_stor).NE.0 )
     +     CALL read_error(3, 'basin_sfres_stor')

! Declared Parameters
      ALLOCATE (Sfres_type(Nsfres))
      IF ( declparam(MODNAME, 'sfres_type', 'nsfres', 'integer',
     +     '1', '1', '6',
     +     'Type of surface reservoir',
     +     'Type of surface reservoir (1=Puls routing;'//
     +     ' 2=Linear routing; 3=Flow through;'//
     +     ' 4=Broad crested weir; 5=Gate opening; 6=measured flow)',
     +     'none').NE.0 ) CALL read_error(1, 'sfres_type')

      ALLOCATE (Sfres_init(Nsfres))
      IF ( declparam(MODNAME, 'sfres_init', 'nsfres', 'real',
     +     '0.0', '0.0', '2.0E6',
     +     'Initial storage in each lake HRU',
     +     'Initial storage in each lake HRU using Puls or linear'//
     +     ' storage routing',
     +     'cfs-days').NE.0 ) CALL read_error(1, 'sfres_init')

      ALLOCATE (Sfres_qro(Nsfres))
      IF ( declparam(MODNAME, 'sfres_qro', 'nsfres', 'real',
     +     '0.1', '0.0', '1.0E7',
     +     'Initial daily mean outflow from each lake HRU',
     +     'Initial daily mean outflow from each lake HRU',
     +     'cfs').NE.0 ) CALL read_error(1, 'sfres_qro')

      ALLOCATE (Sfres_din1(Nsfres))
      IF ( declparam(MODNAME, 'sfres_din1', 'nsfres', 'real',
     +     '0.1', '0.0', '1.0E7',
     +     'Initial inflow to each lake HRU',
     +     'Initial inflow to each lake HRU HRU using Puls or linear'//
     +     ' storage routing',
     +     'cfs').NE.0 ) CALL read_error(1, 'sfres_din1')

      ALLOCATE (Sfres_coef(Nsfres))
      IF ( declparam(MODNAME, 'sfres_coef', 'nsfres', 'real',
     +     '0.1', '0.0001', '1.0',
     +     'Linear reservoir routing coefficient',
     +     'Coefficient in equation to route reservoir storage to'//
     +     ' streamflow for lake HRUs using Linear routing',
     +     '1/day').NE.0 ) CALL read_error(1, 'sfres_coef')

      IF ( Mxnsos>0 .OR. Model==99 ) THEN
        ALLOCATE (Wvd(Mxnsos, Nsfres), S24(Mxnsos, Nsfres))
        ALLOCATE (C24(Mxnsos, Nsfres))
        ALLOCATE (O2(Mxnsos, Nsfres))
        IF ( declparam(MODNAME, 'o2', 'mxnsos,nsfres', 'real',
     +      '0.0', '0.0', '100000.0',
     +      'Outflow values in outflow/storage tables for Puls routing',
     +      'Outflow values in outflow/storage tables for each HRUs'//
     +      ' using for Puls routing',
     +       'cfs').NE.0 ) CALL read_error(1, 'o2')

        ALLOCATE (S2(Mxnsos, Nsfres))
        IF ( declparam(MODNAME, 's2', 'mxnsos,nsfres', 'real',
     +      '0.0', '0.0', '100000.0',
     +      'Storage values in outflow/storage tables for Puls routing',
     +      'Storage values in outflow/storage table for each HRUs'//
     +      ' using for Puls routing',
     +      'cfs-days').NE.0 ) CALL read_error(1, 's2')

        ALLOCATE (Nsos(Nsfres))
        IF ( declparam(MODNAME, 'nsos', 'nsfres', 'integer',
     +     '0', '0', '10',
     +     'Number of storage/outflow values in table for Puls routing',
     +     'Number of storage/outflow values in table for each lake'//
     +     ' HRU using Puls routing',
     +     'none').NE.0 ) CALL read_error(1, 'nsos')
      ENDIF

      ALLOCATE (Weir_coef(Nsfres))
      IF ( declparam(MODNAME, 'weir_coef', 'nsfres', 'real',
     +    '2.7', '2.0', '3.0',
     +    'Broad-crested weir coefficent',
     +    'Coefficient for lake HRUs using broad-crested weir equation',
     +    'none').NE.0 ) CALL read_error(1, 'weir_coef')

      ALLOCATE (Weir_len(Nsfres))
      IF ( declparam(MODNAME, 'weir_len', 'nsfres', 'real',
     +    '5.0', '1.0', '1000.0',
     +    'Broad-crested weir length',
     +    'Weir length for lake HRUs using broad-crested weir equation',
     +    'feet').NE.0 ) CALL read_error(1, 'weir_len')

      IF ( Nratetbl>0 .OR. Model==99 ) THEN
        ALLOCATE (Ratetbl_sfres(Nratetbl))
        IF ( Nstage<1 .OR. Ngate<1 ) THEN
          PRINT *, 'ERROR, nratetbl>0 and nstage or ngate = 0'
          STOP
        ENDIF

        ALLOCATE (Sfres_outq2(Nsfres))
        IF ( declvar(MODNAME, 'sfres_outq2', 'nsfres', Nsfres,'real',
     +       'Streamflow leaving second outflow point for lake HRUs'//
     +       ' using gate opening routing',
     +       'cfs',
     +       Sfres_outq2).NE.0 ) CALL read_error(3, 'sfres_outq2')

        IF ( declvar(MODNAME, 'basin_2ndstflow', 'one', 1, 'double',
     +       'Streamflow from second output point for lake HRUs using'//
     +       ' gate opening routing',
     +       'inches',
     +       Basin_2ndstflow).NE.0 )
     +       CALL read_error(3, 'basin_2ndstflow')

        ALLOCATE (Elev_outflow(Nsfres))
        IF ( declparam(MODNAME, 'elev_outflow', 'nsfres', 'real',
     +       '100.0', '0.0', '10000.0',
     +       'Elevation of the main outflow point',
     +       'Elevation of the main outflow point in each lake HRU'//
     +       ' using gate opening routing',
     +       'feet').NE.0 ) CALL read_error(1, 'elev_outflow')

        ALLOCATE (Sfres_out2(Nsfres))
        IF ( declparam(MODNAME, 'sfres_out2', 'nsfres', 'integer',
     +       '0', '0', '1',
     +       'Switch to specify a second outflow point from a lake HRU',
     +       'Switch to specify a second outflow point from any lake'//
     +       ' HRUs using gate opening routing (0=no; 1=yes)',
     +       'none').NE.0 ) CALL read_error(1, 'sfres_out2')

        ALLOCATE (Sfres_out2_a(Nsfres))
        IF ( declparam(MODNAME, 'sfres_out2_a', 'nsfres', 'real',
     +       '1.0', '0.0', '10000.0',
     +       'Outflow coefficient A for lake HRUs with second outlet',
     +       'Coefficient A in outflow equation for any lake HRU'//
     +       ' using gate opening routing',
     +       'cfs/ft').NE.0 ) CALL read_error(1, 'sfres_out2_a')

        ALLOCATE (Sfres_out2_b(Nsfres))
        IF ( declparam(MODNAME, 'sfres_out2_b', 'nsfres', 'real',
     +       '100.0', '0.0', '10000.0',
     +       'Outflow coefficient A for lake HRUs with second outlet',
     +       'Coefficient B in outflow equation for any lake HRU'//
     +       ' using gate opening routing',
     +       'cfs').NE.0 ) CALL read_error(1, 'sfres_out2_b')

        IF ( declparam(MODNAME, 'ratetbl_sfres','nratetbl','integer',
     +       '1', 'bounded', 'nsfres',
     +       'Index of lake associated with each rating table',
     +       'Index of lake associated with each rating table for'//
     +       ' lake HRUs using gate opening routing',
     +       'none').NE.0 ) CALL read_error(1, 'ratetbl_sfres')

        ALLOCATE (Rate_table(Nstage,Ngate))
        IF ( declparam(MODNAME, 'rate_table', 'nstage,ngate', 'real',
     +       '5.0', '1.0', '1000.0',
     +       'Rating table with stage (rows) and gate opening (cols)',
     +       'Rating table with stage (rows) and gate opening (cols)'//
     +       ' for rating table 1 for lake HRUs using gate opening'//
     +       ' routing',
     +       'cfs').NE.0 ) CALL read_error(1, 'rate_table')

        ALLOCATE (Tbl_stage(Nstage))
        IF ( declparam(MODNAME, 'tbl_stage', 'nstage', 'real',
     +       '1.0', '0.0', '10000.0',
     +       'Stage values for each row for rating table 1',
     +       'Stage values for each row for rating table 1',
     +       'feet').NE.0 ) CALL read_error(1, 'tbl_stage')

        ALLOCATE (Tbl_gate(Ngate))
        IF ( declparam(MODNAME, 'tbl_gate', 'ngate', 'real',
     +       '5.0', '1.0', '1000.0',
     +       'Gate openings for each column for rating table 1',
     +       'Gate openings for each column for rating table 1'//
     +       ' for lake HRUs using gate opening routing',
     +       'inches').NE.0 ) CALL read_error(1, 'tbl_gate')

        IF ( Nratetbl>1 .OR. Model==99 ) THEN
          IF ( Nstage2<1 .OR. Ngate2<1 ) THEN
            PRINT *, 'ERROR, nratetbl>1 and nstage2 or ngate2 = 0'
            STOP
          ENDIF
          ALLOCATE (Rate_table2(Nstage2,Ngate2))
          IF ( declparam(MODNAME, 'rate_table2', 'nstage2,ngate2',
     +         'real', '5.0', '1.0', '1000.0',
     +       'Rating table 2 with stage (rows) and gate opening (cols)',
     +       'Rating table with stage (rows) and gate opening (cols)'//
     +       ' for rating table 2 for lake HRUs using gate opening'//
     +       ' routing',
     +         'cfs').NE.0 ) CALL read_error(1, 'rate_table2')

          ALLOCATE (Tbl_stage2(Nstage2))
          IF ( declparam(MODNAME, 'tbl_stage2', 'nstage2', 'real',
     +         '1.0', '0.0', '10000.0',
     +         'Stage values for each row for rating table 2',
     +         'Stage values for each row for rating table 2',
     +         'ft').NE.0 ) CALL read_error(1, 'tbl_stage2')

          ALLOCATE (Tbl_gate2(Ngate2))
          IF ( declparam(MODNAME, 'tbl_gate2', 'ngate2', 'real',
     +         '5.0', '1.0', '1000.0',
     +         'Gate openings for each column for rating table 2',
     +         'Gate openings for each column for rating table 2'//
     +         ' for lake HRUs using gate opening routing',
     +         'inches').NE.0 ) CALL read_error(1, 'tbl_gate2')
        ENDIF

        IF ( Nratetbl>2 .OR. Model==99 ) THEN
          IF ( Nstage3<1 .OR. Ngate3<1 ) THEN
            PRINT *, 'ERROR, nratetbl>2 and nstage3 or ngate3 = 0'
            STOP
          ENDIF
          ALLOCATE (Rate_table3(Nstage3,Ngate3))
          IF ( declparam(MODNAME, 'rate_table3', 'nstage3,ngate3',
     +         'real', '5.0', '1.0', '1000.0',
     +       'Rating table 3 with stage (rows) and gate opening (cols)',
     +       'Rating table with stage (rows) and gate opening (cols)'//
     +       ' for rating table 3 for lake HRUs using gate opening'//
     +       ' routing',
     +         'cfs').NE.0 ) CALL read_error(1, 'rate_table3')

          ALLOCATE (Tbl_stage3(Nstage3))
          IF ( declparam(MODNAME, 'tbl_stage3', 'nstage3', 'real',
     +         '1.0', '0.0', '10000.0',
     +         'Stage values for each row for each rating table 3',
     +         'Stage values for each row for each rating table 3',
     +         'ft').NE.0 ) CALL read_error(1, 'tbl_stage3')

          ALLOCATE (Tbl_gate3(Ngate3))
          IF ( declparam(MODNAME, 'tbl_gate3', 'ngate3', 'real',
     +         '5.0', '1.0', '1000.0',
     +         'Gate openings for each column for each rating table 3',
     +         'Gate openings for each column for rating table 3'//
     +         ' for lake HRUs using gate opening routing',
     +         'inches').NE.0 ) CALL read_error(1, 'tbl_gate3')
        ENDIF

        IF ( Nratetbl>3.OR. Model==99 ) THEN
          IF ( Nstage4<1 .OR. Ngate4<1 ) THEN
            PRINT *, 'ERROR, nratetbl>3 and nstage4 or ngate4 = 0'
            STOP
          ENDIF
          ALLOCATE (Rate_table4(Nstage4,Ngate4))
          IF ( declparam(MODNAME, 'rate_table4', 'nstage4,ngate4',
     +         'real', '5.0', '1.0', '1000.0',
     +         'Rating table 4 with stage (rows) and discharge (cols)',
     +       'Rating table with stage (rows) and gate opening (cols)'//
     +       ' for rating table 4 for lake HRUs using gate opening'//
     +       ' routing',
     +         'cfs').NE.0 ) CALL read_error(1, 'rate_table4')

          ALLOCATE (Tbl_stage4(Nstage4))
          IF ( declparam(MODNAME, 'tbl_stage4', 'nstage4', 'real',
     +         '1.0', '0.0', '10000.0',
     +         'Stage values for each row for each rating table 4',
     +         'Stage values for each row for each rating table 4',
     +         'ft').NE.0 ) CALL read_error(1, 'tbl_stage4')

          ALLOCATE (Tbl_gate4(Ngate4))
          IF ( declparam(MODNAME, 'tbl_gate4', 'ngate4', 'real',
     +         '5.0', '1.0', '1000.0',
     +         'Gate openings for each column for each rating table 4',
     +         'Gate openings for each column for rating table 4'//
     +         ' for lake HRUs using gate opening routing',
     +         'inches').NE.0 ) CALL read_error(1, 'tbl_gate4')
        ENDIF
      ENDIF

      ALLOCATE (Sfres_vol_init(Nsfres))
      IF ( declparam(MODNAME, 'sfres_vol_init', 'nsfres', 'real',
     +     '100.0', '0.0', '10000.0',
     +     'Initial lake volume',
     +     'Initial lake volume for lake HRUs using broad-crested'//
     +     ' weir or gate opening routing',
     +     'acre-feet').NE.0 ) CALL read_error(1, 'sfres_vol_init')

! Declared Parameters
      ALLOCATE ( Obsout_lake(Nsfres) )
      IF ( declparam(MODNAME, 'obsout_lake', 'nsfres',
     +     'integer',
     +     '0', 'bounded', 'nobs',
     +     'Index of measured streamflow station that'//
     +     ' specifies outflow from a lake',
     +     'Index of measured streamflow station that'//
     +     ' specifies outflow from a lake',
     +     'none').NE.0 ) CALL read_error(1, 'obsout_lake')

      strmlkdecl = 0
      END FUNCTION strmlkdecl

!***********************************************************************
!     strmlkinit - Initialize strmlake module - get parameter values,
!                  compute initial values
!***********************************************************************
      INTEGER FUNCTION strmlkinit()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Nsfres
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Basin_area_inv, NEARZERO
      USE PRMS_CASCADE, ONLY: Nwtrbdy
      USE PRMS_OBS, ONLY: Nobs
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: j, k, kk, weir_rate_flag, weir_flag
      INTEGER :: param_problem, obs_flag, linear_flag, puls_lin_flag
      REAL :: tmp
!***********************************************************************
      strmlkinit = 1

      ALLOCATE (Q_down(Nwtrbdy))

      IF ( Nratetbl>0 ) THEN
        Sfres_outq2 = 0.0

        IF ( getparam(MODNAME, 'sfres_out2', Nsfres, 'integer',
     +       Sfres_out2).NE.0  ) CALL read_error(2, 'sfres_out2')
        IF ( getparam(MODNAME, 'sfres_out2_a', Nsfres, 'real',
     +       Sfres_out2_a).NE.0 ) CALL read_error(2, 'sfres_out2_a')
        IF ( getparam(MODNAME, 'sfres_out2_b', Nsfres, 'real',
     +       Sfres_out2_b).NE.0 ) CALL read_error(2, 'sfres_out2_b')
        IF ( getparam(MODNAME, 'elev_outflow', Nsfres, 'real',
     +       Elev_outflow).NE.0 ) CALL read_error(2, 'elev_outflow')

        IF ( getparam(MODNAME, 'rate_table', Nstage*Ngate, 'real',
     +       Rate_table).NE.0 ) CALL read_error(2, 'rate_table')
        IF ( getparam(MODNAME, 'tbl_stage', Nstage, 'real',
     +       Tbl_stage).NE.0 ) CALL read_error(2, 'tbl_stage')
        IF ( getparam(MODNAME, 'tbl_gate', Ngate, 'real',
     +       Tbl_gate).NE.0 ) CALL read_error(2, 'tbl_gate')
        IF ( getparam(MODNAME, 'ratetbl_sfres', Nratetbl, 'integer',
     +       Ratetbl_sfres).NE.0 ) CALL read_error(2, 'ratetbl_sfres')

        IF ( Nratetbl>1 ) THEN
          IF ( getparam(MODNAME, 'rate_table2',Nstage2*Ngate2,'real',
     +         Rate_table2).NE.0 ) CALL read_error(2, 'rate_table2')
          IF ( getparam(MODNAME, 'tbl_stage2', Nstage2, 'real',
     +         Tbl_stage2).NE.0 ) CALL read_error(2, 'tbl_stage2')
          IF ( getparam(MODNAME, 'tbl_gate2', Ngate2, 'real',
     +         Tbl_gate2).NE.0 ) CALL read_error(2, 'tbl_gate2')
        ENDIF
        IF ( Nratetbl>2 ) THEN
          IF ( getparam(MODNAME, 'rate_table3',Nstage3*Ngate3,'real',
     +         Rate_table3).NE.0 ) CALL read_error(2, 'rate_table3')
          IF ( getparam(MODNAME, 'tbl_stage3', Nstage3, 'real',
     +         Tbl_stage3).NE.0 ) CALL read_error(2, 'tbl_stage3')
          IF ( getparam(MODNAME, 'tbl_gate3', Ngate3, 'real',
     +         Tbl_gate3).NE.0 ) CALL read_error(2, 'tbl_gate3')
        ENDIF
        IF ( Nratetbl>3 ) THEN
          IF ( getparam(MODNAME, 'rate_table4',Nstage4*Ngate4,'real',
     +         Rate_table4).NE.0 ) CALL read_error(2, 'rate_table4')
          IF ( getparam(MODNAME, 'tbl_stage4', Nstage4, 'real',
     +         Tbl_stage4).NE.0 ) CALL read_error(2, 'tbl_stage4')
          IF ( getparam(MODNAME, 'tbl_gate4', Ngate4, 'real',
     +         Tbl_gate4).NE.0 ) CALL read_error(2, 'tbl_gate4')
        ENDIF

      ENDIF

      IF ( getparam(MODNAME, 'sfres_type', Nsfres, 'integer',
     +     Sfres_type).NE.0 ) CALL read_error(2, 'sfres_type')

      puls_lin_flag = 0
      obs_flag = 0
      linear_flag = 0
      weir_rate_flag = 0
      weir_flag = 0
      DO j = 1, Nsfres
        IF ( Sfres_type(j)==1 .OR. Sfres_type(j)==2 ) THEN
          puls_lin_flag = 1
          IF ( Sfres_type(j)==2 ) linear_flag = 1
        ELSEIF ( Sfres_type(j)==6 ) THEN
          obs_flag = 1
        ELSEIF ( Sfres_type(j)==4 .OR. Sfres_type(j)==5 ) THEN
          weir_rate_flag = 1
          IF ( Sfres_type(j)==4 ) weir_flag = 1
        ENDIF
      ENDDO
      IF ( puls_lin_flag==1 ) THEN
        IF ( getparam(MODNAME, 'sfres_init', Nsfres, 'real',
     +       Sfres_init).NE.0 ) CALL read_error(2, 'sfres_init')
        IF ( getparam(MODNAME, 'sfres_din1', Nsfres, 'real',
     +       Sfres_din1).NE.0 ) CALL read_error(2, 'sfres_din1')
        IF ( Mxnsos>0 ) THEN
          IF ( getparam(MODNAME, 'o2', Mxnsos*Nsfres, 'real', O2)
     +         .NE.0 ) CALL read_error(2, 'o2')
          IF ( getparam(MODNAME, 's2', Mxnsos*Nsfres, 'real', S2)
     +         .NE.0 ) CALL read_error(2, 's2')
          IF ( getparam(MODNAME, 'nsos', Nsfres, 'integer', Nsos)
     +         .NE.0 ) CALL read_error(2, 'nsos')
        ENDIF
      ENDIF
      IF ( linear_flag==1 ) THEN
        IF ( getparam(MODNAME, 'sfres_coef', Nsfres, 'real',
     +       Sfres_coef).NE.0 ) CALL read_error(2, 'sfres_coef')
      ENDIF
      IF ( obs_flag==1 ) THEN
        IF ( getparam(MODNAME, 'obsout_lake', Nsfres, 'integer',
     +       Obsout_lake).NE.0 ) CALL read_error(2, 'obsout_lake')
      ENDIF
      IF ( weir_rate_flag==1 ) THEN
        IF ( getparam(MODNAME, 'elevsurf_init', Nsfres, 'real',
     +       Elevsurf_init).NE.0 ) CALL read_error(2, 'elevsurf_init')
        IF ( getparam(MODNAME, 'sfres_vol_init', Nsfres, 'real',
     +       Sfres_vol_init).NE.0 ) CALL read_error(2, 'sfres_vol_init')
      ENDIF
      IF ( weir_flag==1 ) THEN
        IF ( getparam(MODNAME, 'weir_coef', Nsfres, 'real',
     +       Weir_coef).NE.0 ) STOP
        IF ( getparam(MODNAME, 'weir_len', Nsfres, 'real',
     +       Weir_len).NE.0 ) STOP
      ENDIF

      IF ( getparam(MODNAME, 'sfres_qro', Nsfres, 'real',
     +     Sfres_qro).NE.0 ) CALL read_error(2, 'sfres_qro')

      Basin_sfres_stor = 0.0D0
      Din1 = 0.0
      Elevsurf = 0.0
      Sfres_sto = 0.0
      param_problem = 0
      Sfres_vol = 0.0
      DO j = 1, Nsfres
        Sfres_outq(j) = Sfres_qro(j)
        Sfres_outcms(j) = Sfres_qro(j)*CFS2CMS_CONV
        IF ( Sfres_type(j)==1 .OR. Sfres_type(j)==2 ) THEN
!rsr stoin, a local variable was removed, unused
!         Stoin(j) = (Sfres_init(j)*23.76)*Basin_area_inv
          Sfres_sto(j) = Sfres_init(j)
          Din1(j) = Sfres_din1(j)
          IF ( Sfres_type(j)==1 ) THEN
            kk = Nsos(j)
            IF ( kk>Mxnsos ) THEN
              PRINT *, 'ERROR, sfres_type = 1, but, nsos>mxnsos, lake:',
     +                 j, ' nsos:', kk, ' mxnsos:', Mxnsos
              STOP
            ENDIF
            IF ( kk<1 ) THEN
              PRINT *, 'ERROR, sfres_type = 1, but, nsos<1, lake:',
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
          ELSEIF ( Sfres_type(j)==2 ) THEN
            IF ( Sfres_coef(j)<NEARZERO ) THEN
              PRINT *, 'Warning, parameter sfres_coef<=0 for sfres:', j
              PRINT *, '         set to 0.1'
              Sfres_coef(j) = 0.1
            ENDIF
          ENDIF
        ELSEIF ( Sfres_type(j)==6 ) THEN
          IF ( Obsout_lake(j)==0 .OR. Obsout_lake(j)>Nobs ) THEN
            PRINT *, 'ERROR, invalid measured outflow for lake=', j
            param_problem = 1
          ENDIF
        ELSEIF ( Sfres_type(j)==4 .OR. Sfres_type(j)==5 ) THEN
          Elevsurf(j) = Elevsurf_init(j)
          IF ( Sfres_vol_init(j)<0.0 ) THEN
            param_problem = 1
            PRINT *, 'ERROR, parameter sfres_vol_init<0 for sfres:', j
          ENDIF
          Sfres_vol(j) = Sfres_vol_init(j)
          Basin_sfres_stor = Basin_sfres_stor + (Sfres_vol(j)*12.0)
          IF ( Sfres_type(j)==4 ) THEN
            IF ( Nratetbl>0 ) THEN
              IF ( Elev_outflow(j)<0.0 ) THEN
                param_problem = 1
                PRINT *, 'ERROR, parameter elev_outflow<0 for sfres:', j
              ENDIF
            ENDIF
          ENDIF
        ELSE
          PRINT *, 'ERROR, invalid sfres_type for lake=', j
          param_problem = 1
        ENDIF
      ENDDO
      IF ( param_problem>0 ) STOP
      Basin_sfres_stor = Basin_sfres_stor*Basin_area_inv

      Q_segment = 0.0
      Basin_2ndstflow = 0.0D0

      IF ( puls_lin_flag==1 ) DEALLOCATE ( Sfres_init )
      IF ( weir_rate_flag==1 )
     +     DEALLOCATE ( Elevsurf_init, Sfres_vol_init )
      IF ( Mxnsos>0 ) DEALLOCATE ( O2, S2 )

      strmlkinit = 0
      END FUNCTION strmlkinit

!***********************************************************************
!     strmlkrun - Computes basin streamflow and on-channel reservoir
!                 storage and outflows
!***********************************************************************
      INTEGER FUNCTION strmlkrun()
      USE PRMS_STRMFLOW_LAKE
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Nsfres
      USE PRMS_BASIN, ONLY: Basin_area_inv, Hru_area, Basin_cfs,
     +    Basin_cms, Basin_stflow, Basin_sroff_cfs, Basin_ssflow_cfs,
     +    Basin_gwflow_cfs, CFS2CMS_CONV, NEARZERO, Sfres_hru, DNEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Hru_actet, Basin_sroff,
     +    Strm_seg_in, Hortonian_lakes
      USE PRMS_CASCADE, ONLY: Wtrbdy_down, Wtrbdy_route_order, Nwtrbdy
      USE PRMS_OBS, ONLY: Cfs_conv, Timestep_seconds, Runoff, Gate_ht
      USE PRMS_SNOW, ONLY: Pkwater_equiv, Snowmelt
      USE PRMS_SOILZONE, ONLY: Lakein_sz
      USE PRMS_GWFLOW, ONLY: Basin_gwflow, Sfres_seepage, Gw_seep_lakein
      IMPLICIT NONE
      INTRINSIC EXP, DBLE
      EXTERNAL table_comp
! Local Variables
      INTEGER :: j, jj, k, n, jjj, i
      INTEGER :: iwtrdn, ihru, lakeid
      REAL :: new_elevsurf, elevold, head, q1, q2, q3, ppt
      REAL :: head2, avin, lakein, s2o2, xkt, c2
      DOUBLE PRECISION :: cfs2acft, area_fac, diff_vol
      REAL :: res_out2, res_out, res_out1, harea, res2gw
      REAL :: scnd_cfs1, scnd_cfs2, sfres_2ndflow
      DOUBLE PRECISION :: old_sfres_vol, last_stor
!***********************************************************************
      cfs2acft = Timestep_seconds/43560.0D0
      area_fac = Cfs_conv/Basin_area_inv

      Basin_2ndstflow = 0.0D0

      Q_down = 0.0
      Q_segment = 0.0

      last_stor = Basin_sfres_stor
      Basin_sfres_stor = 0.0D0

      DO j = 1, Nwtrbdy
        ppt = 0.0
        res2gw = 0.0

        k = Wtrbdy_route_order(j)
        iwtrdn = Wtrbdy_down(k)
        ! if a segment add lateral inflow plus upstream flow
        IF ( k<=Nsegment ) THEN
          ! ignore segments in lakes
          IF ( iwtrdn==-Nhrup1 ) CYCLE
          ! Strm_seg_in in cfs, q_down in cfs
          Q_segment(k) = Strm_seg_in(k) + Q_down(k)
          IF ( iwtrdn<0 ) THEN
            ! down water body is a lake
            ihru = ABS(iwtrdn)
            DO jj = 1, Nsfres
              IF ( ihru==Sfres_hru(jj) ) THEN
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
        ihru = Sfres_hru(lakeid)

        ppt = Hru_ppt(ihru)
        !rsr, sanity check, snowcomp shouldn't have snowpack
        !     as precip should be added directly to lake
        IF ( Pkwater_equiv(ihru)>DNEARZERO ) THEN
          ppt = Snowmelt(ihru)
          IF ( ppt>0.0 ) PRINT *, 'snowpack on lake HRU:', ihru
        ENDIF

        ! do lake computations
        harea = Hru_area(ihru)

        ! units of lakein = cfs
        lakein = Q_down(k) + harea*Cfs_conv*(Hortonian_lakes(ihru)+
     +           Lakein_sz(ihru)+ppt+Gw_seep_lakein(lakeid))

        ! q2 = lake out in cfs
        q2 = 0.0

!   Set outflow as a measured flow
        IF ( Sfres_type(lakeid)==6 ) THEN
          IF ( Obsout_lake(lakeid)>0 ) THEN
            q2 = Runoff(Obsout_lake(lakeid))
            IF ( q2<0.0 ) THEN
              PRINT *, 'Observed runoff value specified <0 as outflow',
     +                 ' from lake:', lakeid, ' value:', q2
              PRINT *, '     runoff id:', Obsout_lake(lakeid),
     +                 ' outflow set to 0.0'
              q2 = 0.0
            ENDIF
          ELSE
          ENDIF
        ENDIF

!   Compute outflow using Puls routing method
        IF ( Sfres_type(lakeid)==1 ) THEN
          !rsr, why half of current in and last in???
          avin = (lakein+Din1(lakeid))*0.5
          s2o2 = Sfres_sto(lakeid) - Sfres_outq(lakeid)*0.5
          IF ( s2o2<NEARZERO ) s2o2 = 0.0
          s2o2 = s2o2 + avin
          n = Nsos(lakeid)
          DO jjj = 2, Nsos(lakeid)
            IF ( s2o2.LT.Wvd(jjj, lakeid) ) THEN
              n = jjj
              EXIT
            ENDIF
          ENDDO
          q2 = S24(n, lakeid)*s2o2 + C24(n, lakeid)

          IF ( q2<NEARZERO ) q2 = 0.0
          Sfres_sto(lakeid) = s2o2 - q2*0.5
          IF ( Sfres_sto(lakeid)<NEARZERO ) THEN
            q2 = s2o2 + (Sfres_outq(lakeid)*0.5)
            Sfres_sto(lakeid) = 0.0
          ENDIF

!   Compute outflow using linear reservoir method
        ELSEIF ( Sfres_type(lakeid)==2 ) THEN
          !rsr, why half of current in and last in???
          avin = (lakein+Din1(lakeid))*0.5
          xkt = Sfres_coef(lakeid)
          c2 = 1.0 - EXP(-xkt)
          q2 = (avin*(1.0-(c2/xkt))) + Sfres_sto(lakeid)*c2
          IF ( q2<NEARZERO ) q2 = 0.0
          Sfres_sto(lakeid) = Sfres_sto(lakeid) + avin - q2

!   Compute using flow through reservoir
        ELSEIF ( Sfres_type(lakeid)==3 ) THEN
          q2 = lakein

        ELSEIF ( Sfres_type(lakeid)/=6 ) THEN ! 4 or 5
          elevold = Elevsurf(lakeid)
          old_sfres_vol = Sfres_vol(lakeid)

          ! units sfres_invol = acft
          Sfres_invol(lakeid) = lakein*cfs2acft

          res2gw = Sfres_seepage(lakeid)
          ! units res_out = acft
          res_out = ((res2gw+Hru_actet(ihru))/12.0)*harea

          q1 = 0.0
          q3 = 0.0

!   Compute using lake surface elevation and broad crested weir
          IF ( Sfres_type(lakeid)==4 ) THEN

            head = Elevsurf(lakeid) - Elev_outflow(lakeid)
            IF ( head>NEARZERO )
     +           q1 = (head**1.5) * Weir_coef(lakeid) * Weir_len(lakeid)
            res_out1 = q1*cfs2acft

            diff_vol = Sfres_invol(lakeid) - res_out1 - res_out
            ! new_elevsurv has units of feet
            new_elevsurf = Elevsurf(lakeid) + diff_vol/harea

            head2 = ((new_elevsurf + Elevsurf(lakeid)) * 0.5) - 
     +              Elev_outflow(lakeid)

            IF ( head2>NEARZERO )
     +          q3 = (head2**1.5) * Weir_coef(lakeid) * Weir_len(lakeid)

            q2 = (q1+q3)*0.5

!  Compute using a rating table of lake surface elevation & gate opening
          ELSE !rsr, type = 5

            DO i = 1, Nratetbl
              IF ( lakeid==Ratetbl_sfres(i) ) THEN
                IF ( i==1 ) THEN
                  CALL table_comp(Ngate, Nstage, Tbl_gate, Tbl_stage,
     +                 Rate_table, Elevsurf(lakeid), Gate_ht(i), q1)
                ELSEIF ( i==2 ) THEN
                  CALL table_comp(Ngate2, Nstage2, Tbl_gate2,
     +                 Tbl_stage2, Rate_table2, Elevsurf(lakeid),
     +                 Gate_ht(i), q1)
                ELSEIF ( i==3 ) THEN
                  CALL table_comp(Ngate3, Nstage3, Tbl_gate3,
     +                 Tbl_stage3, Rate_table3, Elevsurf(lakeid),
     +                 Gate_ht(i), q1)
                ELSEIF ( i==4 ) THEN
                  CALL table_comp(Ngate4, Nstage4, Tbl_gate4,
     +                 Tbl_stage4, Rate_table4, Elevsurf(lakeid),
     +                 Gate_ht(i), q1)
                ENDIF
              ENDIF
            ENDDO

!  if reservoir has a second outlet then outflow in cfs is computed by
!       Q = (Sfres_out2_a * Elevsurf) - Sfres_out2_b
!       (as per Rob Dudley email 7 Sep 2006)
            IF ( Sfres_out2(lakeid).GT.0.0 ) THEN
              scnd_cfs1 = (Sfres_out2_a(lakeid)*Elevsurf(lakeid)) -
     +                    Sfres_out2_b(lakeid)
              IF ( scnd_cfs1.LT.NEARZERO ) scnd_cfs1 = 0.0
            ELSE
              scnd_cfs1 = 0.0
            ENDIF

            res_out1 = (q1 + scnd_cfs1)*cfs2acft + res_out
            diff_vol = Sfres_invol(lakeid) - res_out1
            new_elevsurf = (diff_vol / harea) + Elevsurf(lakeid)

            DO i = 1, Nratetbl
              IF ( lakeid==Ratetbl_sfres(i) ) THEN
                IF ( i==1 ) THEN
                  CALL table_comp(Ngate, Nstage, Tbl_gate,
     +                 Tbl_stage, Rate_table, new_elevsurf,
     +                 Gate_ht(i), q3)
                ELSEIF ( i==2 ) THEN
                  CALL table_comp(Ngate2, Nstage2, Tbl_gate2,
     +                 Tbl_stage2, Rate_table2, new_elevsurf,
     +                 Gate_ht(i), q3)
                ELSEIF ( i==3 ) THEN
                  CALL table_comp(Ngate3, Nstage3, Tbl_gate3,
     +                 Tbl_stage3, Rate_table3, new_elevsurf,
     +                 Gate_ht(i), q3)
                ELSEIF ( i==4 ) THEN
                  CALL table_comp(Ngate4, Nstage4, Tbl_gate4,
     +                 Tbl_stage4, Rate_table4, new_elevsurf,
     +                 Gate_ht(i), q3)
                ENDIF
              ENDIF
            ENDDO

            IF ( Sfres_out2(lakeid)>NEARZERO ) THEN
              scnd_cfs2 = (Sfres_out2_a(lakeid)*new_elevsurf) -
     +                    Sfres_out2_b(lakeid)
              IF ( scnd_cfs2<0.0 ) scnd_cfs2 = 0.0
            ELSE
              scnd_cfs2 = 0.0
            ENDIF

            Sfres_outq2(lakeid) = (scnd_cfs1+scnd_cfs2)*0.5

            q2 = (q1 + q3)*0.5 + Sfres_outq2(lakeid)

            sfres_2ndflow = Sfres_outq2(lakeid)*cfs2acft*12.0D0
            Basin_2ndstflow = Basin_2ndstflow + sfres_2ndflow

          ENDIF

!         !sanity check, rsr
          if(q2<0.0) print *, 'q2<0', q2

          res_out2 = q2*cfs2acft + res_out
          Sfres_outvol(lakeid) = res_out2

          diff_vol = Sfres_invol(lakeid) - res_out2

          Sfres_vol(lakeid) = Sfres_vol(lakeid) + diff_vol
          Elevsurf(lakeid) = diff_vol/harea + Elevsurf(lakeid)
          Basin_sfres_stor = Basin_sfres_stor + Sfres_vol(lakeid)*12.0D0
        ENDIF

        !rsr, could have more than one stream coming into segment
        IF ( iwtrdn>0 ) Q_down(iwtrdn) = Q_down(iwtrdn) + q2
        Sfres_outq(lakeid) = q2
        Sfres_outcms(lakeid) = q2*CFS2CMS_CONV
        !rsr, do not count any segment in lake
!       Q_segment(j) = q2  ??????
!       Q_segment(j) = q2 + Strm_seg_in(j)

      ENDDO   ! end segment routing loop

      Basin_cfs = Q_segment(Nsegment) !rsr Nsegment must be last segment
      Basin_cms = Basin_cfs*CFS2CMS_CONV

      Basin_stflow = Basin_cfs/area_fac
      Basin_sfres_stor = Basin_sfres_stor*Basin_area_inv
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
     +           Rate_table, Elevsurf, Gate_ht, Q2)
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ngate, Nstage
      REAL, INTENT(IN) :: Tbl_gate(Ngate), Tbl_stage(Nstage)
      REAL, INTENT(IN) :: Rate_table(Nstage, Ngate), Gate_ht, Elevsurf
      REAL, INTENT(OUT) :: Q2
! Local Variables
      INTEGER m, mm, stg1, stg2, gate1, gate2
      REAL :: diff_q_stg1, diff_q_stg2, ratiog, ratios
      REAL :: q_stg1, q_stg2, diffq
!***********************************************************************
      IF ( Elevsurf.LT.Tbl_stage(Nstage) ) THEN
        Q2 = 0.0

      ELSEIF ( Gate_ht<NEARZERO ) THEN
        IF ( Elevsurf.LT.172.91 ) THEN
          Q2 = (7.7322*Elevsurf) - 1320.6
        ELSEIF ( Elevsurf<175.0 ) THEN
          Q2 = (15.754*Elevsurf) - 2707.2
        ELSE
          Q2 = (76.431*Elevsurf) - 13333.0
        ENDIF

        IF ( Q2<0.0 ) Q2 = 0.0

      ELSE

        stg2 = 1
        stg1 = 0
        IF ( Elevsurf.LE.Tbl_stage(1) ) THEN
          DO m = 1, Nstage
            IF ( Elevsurf.GT.Tbl_stage(m) ) THEN
              IF ( m.EQ.1 ) THEN
                stg2 = 1
                stg1 = 1
              ELSE
                stg2 = m
                stg1 = m - 1
              ENDIF
              EXIT
            ENDIF
          ENDDO
        ENDIF

        gate2 = Ngate
        gate1 = Ngate - 1
        IF ( Gate_ht.LE.Tbl_gate(Ngate) ) THEN
          DO mm = 1, Ngate
            IF ( Tbl_gate(mm).GT.Gate_ht ) THEN
              IF ( mm.EQ.1 ) THEN
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

        IF ( stg1.EQ.0 ) THEN
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
          ratios = (Elevsurf-Tbl_stage(stg2))
     +             /(Tbl_stage(stg1)-Tbl_stage(stg2))

          diffq = q_stg1 - q_stg2

          Q2 = q_stg2 + (ratios*diffq)

        ENDIF
      ENDIF

      END SUBROUTINE table_comp
