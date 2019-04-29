!***********************************************************************
! Sums inflow to and outflow from PRMS ground-water reservoirs; outflow
! can be routed to downslope ground-water reservoirs and stream
! segments
!
! Can be used for depression storage
!***********************************************************************
! Modified 7/1997 J. Vaccaro to set a minimum value for groundwater flow
! by reading in a minimum ground-water storage value for each groundwater
! reservoir, if this value is set=0, then standard PRMS routine module.
! A minimum may represent an injection well, intrabasin transfer,
! contribution from larger regional gw system, or past residual storage
! modified 10/1/2008 rsregan to include Vaccaro code
!***********************************************************************
      MODULE PRMS_GWFLOW
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: BALUNT
      CHARACTER(LEN=6), SAVE :: MODNAME
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwstor_minarea(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seepage_gwr(:)
      DOUBLE PRECISION, SAVE :: Basin_gw_upslope, Basin_farflow
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwin_dprst(:)
      INTEGER, SAVE :: Gwminarea_flag, Hru_strmout_flag
      DOUBLE PRECISION, SAVE :: Basin_dnflow, Basin_gwstor_minarea_wb
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_gwstor, Basin_gwflow, Basin_gwsink
      DOUBLE PRECISION, SAVE :: Basin_lake_seep, Basin_gwin
      REAL, SAVE, ALLOCATABLE :: Gwres_flow(:), Gwres_sink(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gw_upslope(:), Gwres_in(:)
      REAL, SAVE, ALLOCATABLE :: Hru_gw_cascadeflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gw_in_soil(:), Gw_in_ssr(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_seepage(:), Gw_seep_lakein(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwstor_minarea_wb(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_streamflow_out(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Ssr_gwres(:)
      REAL, SAVE, ALLOCATABLE :: Gwflow_coef(:), Gwsink_coef(:)
      REAL, SAVE, ALLOCATABLE :: Gwstor_init(:)
      REAL, SAVE, ALLOCATABLE :: Lake_seep_elev(:), Elevlake_init(:)
      REAL, SAVE, ALLOCATABLE :: Gw_seep_coef(:), Gwstor_min(:)
      END MODULE PRMS_GWFLOW

!***********************************************************************
!     Main gwflow routine
!***********************************************************************
      INTEGER FUNCTION gwflow()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gwflowdecl, gwflowinit, gwflowrun
!***********************************************************************
      gwflow = 0

      IF ( Process(:3)=='run' ) THEN
        gwflow = gwflowrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        gwflow = gwflowdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        gwflow = gwflowinit()
      ENDIF

      END FUNCTION gwflow

!***********************************************************************
!     gwflowdecl - set up parameters for groundwater computations
!   Declared Parameters
!     ssr_gwres, hru_gwres, gwstor_init, gwflow_coef, gwsink_coef
!     lake_seep_elev, elevlake_init, gw_seep_coef
!***********************************************************************
      INTEGER FUNCTION gwflowdecl()
      USE PRMS_GWFLOW
      USE PRMS_MODULE, ONLY: Nhru, Ngw, Nssr, Nlake, Strmflow_flag, Model, Strmflow_flag, Dprst_flag
      USE PRMS_CASCADE, ONLY: Cascadegw_flag
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: i, nc
      CHARACTER(LEN=80), SAVE :: Version_gwflow
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Groundwater Flow'
!***********************************************************************
      gwflowdecl = 0

      Version_gwflow = '$Id: gwflow.f90 5173 2013-01-03 00:07:12Z rsregan $'
      nc = INDEX( Version_gwflow, 'Z' )
      i = INDEX( Version_gwflow, '.f90' ) + 3
      IF ( declmodule(Version_gwflow(6:i), PROCNAME, Version_gwflow(i+2:nc))/=0 ) STOP
      MODNAME = 'gwflow'

! cascading variables and parameters
      IF ( Cascadegw_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Gw_upslope(Ngw) )
        IF ( declvar(MODNAME, 'gw_upslope', 'ngw', Ngw, 'double', &
             'Groundwater flow received from upslope GWRs for each GWR', &
             'acre-inches', Gw_upslope)/=0 ) CALL read_error(3, 'gw_upslope')
        ALLOCATE ( Hru_gw_cascadeflow(Ngw) )
        IF ( declvar(MODNAME, 'hru_gw_cascadeflow', 'ngw', Ngw, 'real', &
             'Cascading groundwater flow from each GWR', &
             'inches', Hru_gw_cascadeflow)/=0 ) CALL read_error(3, 'hru_gw_cascadeflow')
      ENDIF

      ALLOCATE ( Gwres_flow(Ngw) )
      IF ( declvar(MODNAME, 'gwres_flow', 'ngw', Ngw, 'real', &
           'Groundwater discharge from each GWR to the stream network', &
           'inches', Gwres_flow)/=0 ) CALL read_error(3, 'gwres_flow')

      ALLOCATE ( Gwres_in(Ngw) )
      IF ( declvar(MODNAME, 'gwres_in', 'ngw', Ngw, 'double', &
           'Total inflow to each GWR from associated capillary and gravity reservoirs', &
           'acre-inches', Gwres_in)/=0 ) CALL read_error(3, 'gwres_in')

      ALLOCATE ( Gwres_sink(Ngw) )
      IF ( declvar(MODNAME, 'gwres_sink', 'ngw', Ngw, 'real', &
           'Outflow from GWRs to the groundwater sink; water is'// &
           ' considered underflow or flow to deep aquifers and does'// &
           ' not flow to the stream network', &
           'inches', Gwres_sink)/=0 ) CALL read_error(3, 'gwres_sink')

      ALLOCATE ( Gw_in_soil(Ngw) )
      IF ( declvar(MODNAME, 'gw_in_soil', 'ngw', Ngw, 'double', &
           'Drainage from capillary reservoir excess water for each GWR', &
           'acre-inches', Gw_in_soil)/=0 ) CALL read_error(3, 'gw_in_soil')

      ALLOCATE ( Gw_in_ssr(Ngw) )
      IF ( declvar(MODNAME, 'gw_in_ssr', 'ngw', Ngw, 'double', &
           'Drainage from gravity reservoir excess water for each GWR', &
           'acre-inches', Gw_in_ssr)/=0 ) CALL read_error(3, 'gw_in_ssr')

      IF ( declvar(MODNAME, 'basin_gwstor', 'one', 1, 'double', &
           'Basin area-weighted average of storage in GWRs', &
           'inches', Basin_gwstor)/=0 ) CALL read_error(3, 'basin_gwstor')

      IF ( declvar(MODNAME, 'basin_gwin', 'one', 1, 'double', &
           'Basin area-weighted average of inflow to GWRs', &
           'inches', Basin_gwin)/=0 ) CALL read_error(3, 'basin_gwin')

      IF ( declvar(MODNAME, 'basin_gwflow', 'one', 1, 'double', &
           'Basin area-weighted average of groundwater flow to the stream network', &
           'inches', Basin_gwflow)/=0 ) CALL read_error(3, 'basin_gwflow')

      IF ( declvar(MODNAME, 'basin_gwsink', 'one', 1, 'double', &
           'Basin area-weighted average of GWR outflow to the groundwater sink', &
           'inches', Basin_gwsink)/=0 ) CALL read_error(3, 'basin_gwsink')

      ALLOCATE ( Ssr_gwres(Nssr) )
      IF ( Nssr/=Ngw .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'ssr_gwres', 'nssr', 'integer', &
             '1', 'bounded', 'ngw', &
             'Index of GWR to receive flow from associated gravity reservoirs', &
             'Index of the GWR that receives flow from each'// &
             ' associated subsurface or gravity reservoir (deprecated)', &
             'none')/=0 ) CALL read_error(1, 'ssr_gwres')
      ENDIF

      ALLOCATE ( Gwflow_coef(Ngw), Gwsink_coef(Ngw) )
      ALLOCATE ( Gwstor_init(Ngw) )
      IF ( declparam(MODNAME, 'gwstor_init', 'ngw', 'real', &
           '0.1', '0.0', '20.0', &
           'Initial storage in each GWR', &
           'Storage in each GWR at the beginning of a simulation', &
           'inches')/=0 ) CALL read_error(1, 'gwstor_init')
      IF ( declparam(MODNAME, 'gwflow_coef', 'ngw', 'real', &
           '0.015', '0.0', '1.0', &
           'Groundwater routing coefficient', &
           'Linear coefficient in the equation to compute groundwater'// &
           ' discharge for each GWR', &
           '1.0/day')/=0 ) CALL read_error(1, 'gwflow_coef')
      IF ( declparam(MODNAME, 'gwsink_coef', 'ngw', 'real', &
           '0.0', '0.0', '1.0', &
           'Groundwater sink coefficient', &
           'Linear coefficient in the equation to compute outflow'// &
           ' to the groundwater sink for each GWR', &
           '1.0/day')/=0 ) CALL read_error(1, 'gwsink_coef')

      IF ( Strmflow_flag==2 .OR. Model==99 ) THEN
        IF ( declvar(MODNAME, 'basin_lake_seep', 'one', 1, 'double', &
             'Basin area-weighted average of lake-bed seepage to GWRs', &
             'inches', Basin_lake_seep)/=0 ) CALL read_error(3, 'basin_lake_seep')
        ALLOCATE ( Lake_seepage(Ngw) )
        IF ( declvar(MODNAME, 'lake_seepage', 'ngw', Ngw, 'double', &
             'Lake-bed seepage from each lake HRU to the associated GWR', &
             'inches', Lake_seepage)/=0 ) CALL read_error(3, 'lake_seepage')
        ALLOCATE ( Gw_seep_lakein(Ngw) )
        IF ( declvar(MODNAME, 'gw_seep_lakein', 'ngw', Ngw, 'double', &
             'Groundwater discharge to any associated lake HRU for each GWR', &
             'inches', Gw_seep_lakein)/=0 ) CALL read_error(3, 'gw_seep_lakein')

        ALLOCATE ( Lake_seep_elev(Nlake), Elevlake_init(Nlake), Gw_seep_coef(Nlake) )
        IF ( declparam(MODNAME, 'lake_seep_elev', 'nlake', 'real', &
             '1.0', '0.0', '1000.0', &
             'Elevation over which lakebed seepage to the GWR occurs', &
             'Elevation over which lakebed seepage to the GWR occurs for lake HRUs', &
             'feet')/=0 ) CALL read_error(1, 'lake_seep_elev')
        IF ( declparam(MODNAME, 'elevlake_init', 'nlake', 'real', &
             '100.0', '0.0', '10000.0', &
             'Initial lake surface elevation', 'Initial lake surface elevation for lake HRUs', &
             'feet')/=0 ) CALL read_error(1, 'elevlake_init')
        IF ( declparam(MODNAME, 'gw_seep_coef', 'nlake', 'real', &
             '0.015', '0.0', '1.0', &
             'Linear coefficient to compute seepage and groundwater'// &
             ' discharge to and from associated lake HRUs', &
             'Linear coefficient in equation to compute lakebed'// &
             ' seepage to the GWR and groundwater discharge to lake HRUs', &
             '1.0/day')/=0 ) CALL read_error(1, 'gw_seep_coef')
      ENDIF

      ALLOCATE ( Gwstor_min(Ngw) )
      IF ( declparam(MODNAME, 'gwstor_min', 'ngw', 'real', &
           '0.0', '0.0', '5.0', &
           'Minimum storage in each GWR', &
           'Minimum storage in each GWR to ensure storage is greater'// &
           ' than specified value to account for inflow from deep'// &
           ' aquifers or injection wells with the water source'// &
           ' outside the basin', &
           'inches')/=0 ) CALL read_error(1, 'gwstor_min')
      ALLOCATE ( Gwstor_minarea_wb(Ngw) )
      IF ( declvar(MODNAME, 'gwstor_minarea_wb', 'ngw', Ngw, 'double', &
           'Storage added to each GWR when storage is less than gwstor_min', &
           'inches', Gwstor_minarea_wb)/=0 ) CALL read_error(3, 'gwstor_minarea_wb')

      Hru_strmout_flag = 0
      IF ( Nhru==Ngw .AND. Nssr==Ngw ) THEN
        Hru_strmout_flag = 1
        ALLOCATE ( Hru_streamflow_out(Nhru) )
        IF ( declvar(MODNAME, 'hru_streamflow_out', 'nhru', Nhru, 'double', &
             'Total flow to stream network from each HRU', &
             'cfs', Hru_streamflow_out)/=0 ) CALL read_error(3, 'Hru_streamflow_out')
      ENDIF

      ALLOCATE ( Gwstor_minarea(Ngw) )
      IF ( Strmflow_flag==2 ) ALLOCATE ( Seepage_gwr(Ngw) )
      IF ( Dprst_flag==1 ) ALLOCATE ( Gwin_dprst(Ngw) )

      END FUNCTION gwflowdecl

!***********************************************************************
!     gwflowinit - Initialize gwflow module - get parameter values,
!               compute initial values.
!***********************************************************************
      INTEGER FUNCTION gwflowinit()
      USE PRMS_GWFLOW
      USE PRMS_MODULE, ONLY: Nhru, Ngw, Nssr, Nlake, Dprst_flag, Strmflow_flag, Print_debug, Inputerror_flag
      USE PRMS_CASCADE, ONLY: Cascadegw_flag
      USE PRMS_BASIN, ONLY: Timestep, Gwres_area, Gwr_type, Lake_hru_id, &
          Basin_area_inv, Active_gwrs, Gwr_route_order
      USE PRMS_FLOWVARS, ONLY: Gwres_stor, Elevlake
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error, PRMS_open_module_file
      INTRINSIC ABS, DBLE
! Local Variables
      INTEGER :: i, j, jj, jjj
      DOUBLE PRECISION :: seepage
!***********************************************************************
      gwflowinit = 0

      IF ( Print_debug==1 ) THEN
        CALL PRMS_open_module_file(BALUNT, 'gwflow.wbal')
        WRITE ( BALUNT, 9001 )
      ENDIF

      IF ( Nssr/=Ngw ) THEN
        IF ( getparam(MODNAME, 'ssr_gwres', Nssr, 'integer', Ssr_gwres)/=0 ) CALL read_error(2, 'ssr_gwres')
      ELSE
        DO i = 1, Nssr
          Ssr_gwres(i) = i
        ENDDO
      ENDIF

      IF ( getparam(MODNAME, 'gwflow_coef', Ngw, 'real', Gwflow_coef)/=0 ) CALL read_error(2, 'gwflow_coef')
      IF ( getparam(MODNAME, 'gwsink_coef', Ngw, 'real', Gwsink_coef)/=0 ) CALL read_error(2, 'gwsink_coef')
      IF ( getparam(MODNAME, 'gwstor_min', Ngw, 'real', Gwstor_min)/=0 ) CALL read_error(2, 'gwstor_min')

      Gwminarea_flag = 0
      Gwstor_minarea_wb = 0.0D0
      DO i = 1, Ngw
        IF ( Gwstor_min(i)>0.0 ) THEN
          Gwminarea_flag = 1
          Gwstor_minarea(i) = DBLE( Gwstor_min(i)*Gwres_area(i) )
        ELSE
          Gwstor_minarea(i) = 0.0D0
        ENDIF
        IF ( Gwflow_coef(i)<0.0 ) THEN
          PRINT *, 'ERROR, gwflow_coef value < 0.0 or > 1.0 for HRU:', i, Gwflow_coef(i)
          Inputerror_flag = 1
        ELSEIF ( Gwflow_coef(i)>1.0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'WARNING, gwflow_coef value > 1.0 for HRU:', i, Gwflow_coef(i)
        ENDIF
        ! GWR's cannot be swales
        IF ( Gwr_type(i)==3 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, gwr_type = 3 for GWR:', i, ' set to 1', Gwr_type(i)
          Gwr_type(i) = 1
        ENDIF
      ENDDO
      IF ( Gwminarea_flag==0 ) DEALLOCATE ( Gwstor_min )

      IF ( Strmflow_flag==2 ) THEN
        Seepage_gwr = 0.0D0
        IF ( getparam(MODNAME, 'gw_seep_coef', Nlake, 'real', Gw_seep_coef)/=0 ) CALL read_error(2, 'gw_seep_coef')
        IF ( getparam(MODNAME, 'lake_seep_elev', Nlake, 'real', Lake_seep_elev)/=0 ) CALL read_error(2, 'lake_seep_elev')
        Lake_seepage = 0.0D0
        Gw_seep_lakein = 0.0D0
      ENDIF

      IF ( Dprst_flag==1 ) Gwin_dprst = 0.0D0
! do only once, so restart uses saved values
      IF ( Timestep==0 ) THEN
        IF ( Cascadegw_flag==1 ) THEN
          Gw_upslope = 0.0D0
          Hru_gw_cascadeflow = 0.0
        ENDIF
        Gwres_flow = 0.0
        Gwres_in = 0.0D0
        Gwres_sink = 0.0
        Gw_in_ssr = 0.0D0
        Gw_in_soil = 0.0D0
        IF ( getparam(MODNAME, 'gwstor_init', Ngw, 'real', Gwstor_init)/=0 ) CALL read_error(2, 'gwstor_init')
        DO i = 1, Ngw
          Gwres_stor(i)= DBLE( Gwstor_init(i) )
        ENDDO
        DEALLOCATE ( Gwstor_init )
        Basin_gwflow = 0.0D0
        Basin_gwsink = 0.0D0
        Basin_gwin = 0.0D0
        Basin_farflow = 0.0D0
        Basin_lake_seep = 0.0D0
        IF ( Strmflow_flag==2 ) THEN
          IF ( getparam(MODNAME, 'elevlake_init', Nlake, 'real', Elevlake_init)/=0 ) CALL read_error(2, 'elevlake_init')
          Elevlake = Elevlake_init
          DEALLOCATE ( Elevlake_init )
        ENDIF
        Basin_gw_upslope = 0.0D0
        Basin_dnflow = 0.0D0
        Basin_gwstor_minarea_wb = 0.0D0
        IF ( Hru_strmout_flag==1 ) Hru_streamflow_out = 0.0D0
      ENDIF

      IF ( Strmflow_flag==2 ) THEN
        Basin_lake_seep = 0.0D0
        DO i = 1, Active_gwrs
          j = Gwr_route_order(i)
          IF ( Gwr_type(j)==2 ) THEN
            jjj = Lake_hru_id(j)
            IF ( jjj==0 ) THEN
              PRINT *, 'ERROR, GWR specified as a lake but lake_hru_id value is not 2, HRU:', jjj
              Inputerror_flag = 1
              CYCLE
            ENDIF
            seepage = (Elevlake(jjj)-Lake_seep_elev(jjj))*12.0*Gw_seep_coef(jjj)
            IF ( seepage<0.0D0 ) THEN
              IF ( ABS(seepage)>Gwres_stor(j) ) seepage = -Gwres_stor(j)
              Gw_seep_lakein(j) = -seepage
            ELSE
              Lake_seepage(j) = seepage
            ENDIF
            Basin_lake_seep = Basin_lake_seep + seepage*Gwres_area(j)
            Gwres_stor(j) = Gwres_stor(j) + seepage
            Seepage_gwr(j) = seepage
          ENDIF
        ENDDO
        Basin_lake_seep = Basin_lake_seep*Basin_area_inv
      ENDIF

      Basin_gwstor = 0.0D0
      DO jj = 1, Active_gwrs
        j = Gwr_route_order(jj)
        Basin_gwstor = Basin_gwstor + Gwres_stor(j)*Gwres_area(j)
      ENDDO
      Basin_gwstor = Basin_gwstor*Basin_area_inv

 9001 FORMAT ('    Date     Water Bal last store  GWR store', &
              '   GW input    GW flow    GW sink    farflow GW upslope minarea_in   downflow')

      END FUNCTION gwflowinit

!***********************************************************************
!     gwflowrun - Computes groundwater flow to streamflow and to
!              groundwater sink
!***********************************************************************
      INTEGER FUNCTION gwflowrun()
      USE PRMS_GWFLOW
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Strmflow_flag, Dprst_flag, Nlake, Print_debug
      USE PRMS_BASIN, ONLY: Active_gwrs, Gwr_route_order, Gwr_type, &
          Gwres_area, Basin_area_inv, Active_hrus, Hru_route_order, &
          Hru_area, Ssres_area, Hru_gwres, Lake_hru_id
      USE PRMS_FLOWVARS, ONLY: Soil_to_gw, Ssr_to_gw, Sroff, Ssres_flow, Gwres_stor, Dprst_seep_hru, Elevlake
      USE PRMS_CASCADE, ONLY: Ncascade_gwr, Cascadegw_flag
      USE PRMS_OBS, ONLY: Nowtime, Cfs_conv
      IMPLICIT NONE
      EXTERNAL rungw_cascade, read_error
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, j, ii, jj, jjj
      REAL :: gwarea, dnflow, far_gwflow
      DOUBLE PRECISION :: gwin, gwstor, gwsink, seepage, gwflow
      DOUBLE PRECISION :: gwbal, gwstor_last
      DOUBLE PRECISION :: last_basin_gwstor, last_gwstor, gwup
!***********************************************************************
      gwflowrun = 0

      IF ( Cascadegw_flag==1 ) THEN
        Gw_upslope = 0.0D0
        Basin_dnflow = 0.0D0
        Basin_gw_upslope = 0.0D0
      ENDIF

      DO jj = 1, Active_gwrs
        j = Gwr_route_order(jj)
        Gw_in_soil(j) = 0.0D0
        Gwres_stor(j) = Gwres_stor(j)*Gwres_area(j)
      ENDDO

      IF ( Strmflow_flag==2 ) THEN
        ! elevlake from last timestep
        Lake_seepage = 0.0D0
        Gw_seep_lakein = 0.0D0
        Basin_lake_seep = 0.0D0
        DO jj = 1, Active_gwrs
          j = Gwr_route_order(jj)
          IF ( Gwr_type(j)==2 ) THEN
            gwarea = Gwres_area(j)
            ! seepage added to GWR
            jjj = Lake_hru_id(j)
            IF ( jjj>0 ) THEN
              !rsr, need seepage variable for WB
              seepage = (Elevlake(jjj)-Lake_seep_elev(jjj))*12.0*Gw_seep_coef(jjj)*gwarea
              IF ( seepage<0.0D0 ) THEN
                IF ( ABS(seepage)>Gwres_stor(j) ) seepage = -Gwres_stor(j)
                Gw_seep_lakein(j) = -seepage/gwarea
              ELSE
                Lake_seepage(j) = seepage/gwarea
              ENDIF
              Basin_lake_seep = Basin_lake_seep + seepage
              Gwres_stor(j) = Gwres_stor(j) + seepage
              Seepage_gwr(j) = seepage
            ENDIF
          ENDIF
        ENDDO
        Basin_lake_seep = Basin_lake_seep*Basin_area_inv
      ENDIF

!******Sum the inflows to each GWR to units of acre-inches
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        j = Hru_gwres(i)
        IF ( Gwr_type(j)==2 ) CYCLE
        !rsr, soil_to_gw is for whole HRU, not just perv
        Gw_in_soil(j) = Gw_in_soil(j) + Soil_to_gw(i)*Hru_area(i)
        IF ( Dprst_flag>0 ) Gwin_dprst(j) = Dprst_seep_hru(i)*Hru_area(i)
      ENDDO

      IF ( Ngw/=Nhru ) THEN
        Gw_in_ssr = 0.0D0
        DO i = 1, Nssr
          j = Ssr_gwres(i)
          Gw_in_ssr(j) = Gw_in_ssr(j) + Ssr_to_gw(i)*Ssres_area(i)
        ENDDO
      ELSE
        DO ii = 1, Active_gwrs
          i = Gwr_route_order(ii)
          IF ( Gwr_type(i)==2 ) CYCLE
          Gw_in_ssr(i) = Ssr_to_gw(i)*Ssres_area(i)
        ENDDO
      ENDIF

      Basin_gwstor_minarea_wb = 0.0D0
      Basin_gwflow = 0.0D0
      last_basin_gwstor = Basin_gwstor
      Basin_gwstor = 0.0D0
      Basin_gwsink = 0.0D0
      Basin_gwin = 0.0D0
      Basin_farflow = 0.0D0
      DO j = 1, Active_gwrs
        i = Gwr_route_order(j)
        IF ( Gwr_type(i)==2 .OR. Gwr_type(i)==0 ) CYCLE
        gwarea = Gwres_area(i)
        last_gwstor = Gwres_stor(i)
        gwstor = last_gwstor
        gwin = Gw_in_soil(i) + Gw_in_ssr(i)
        IF ( Strmflow_flag==2 ) gwin = gwin + Seepage_gwr(i)
        IF ( Cascadegw_flag==1 ) THEN
          gwin = gwin + Gw_upslope(i)
          Basin_gw_upslope = Basin_gw_upslope + Gw_upslope(i)
        ENDIF
        IF ( Dprst_flag==1 ) THEN
          !rsr, need basin variable for WB
          Gwin_dprst(i) = Dprst_seep_hru(i)*gwarea
          gwin = gwin + Gwin_dprst(i)
        ENDIF
        IF ( Gwminarea_flag==1 ) THEN
          ! check to be sure gwres_stor >= gwstor_minarea before computing outflows
          IF ( gwstor<Gwstor_minarea(i) ) THEN
            gwstor_last = gwstor
            gwstor = Gwstor_minarea(i)
            !rsr, keep track of change in storage for WB
            Gwstor_minarea_wb(i) = gwstor - gwstor_last
            gwin = gwin + Gwstor_minarea_wb(i)
!            PRINT *, 'Added gwstor based on gwstor_min', Gwstor_minarea_wb(i)/gwarea
            Basin_gwstor_minarea_wb = Basin_gwstor_minarea_wb + Gwstor_minarea_wb(i)
          ELSE
            Gwstor_minarea_wb(i) = 0.0D0
          ENDIF
        ENDIF
        gwstor = gwstor + gwin
        Basin_gwin = Basin_gwin + gwin

! Compute groundwater discharge
        gwflow = gwstor*Gwflow_coef(i)

! Reduce storage by outflow
        gwstor = gwstor - gwflow

        gwsink = 0.0D0
        IF ( Gwsink_coef(i)>0.0 ) THEN
          gwsink = gwstor*Gwsink_coef(i)
          gwstor = gwstor - gwsink
          IF ( gwstor<0.0D0 ) THEN
            gwsink = gwsink + gwstor
            gwstor = 0.0D0
          ENDIF
          Gwres_sink(i) = gwsink/gwarea
          Basin_gwsink = Basin_gwsink + gwsink
        ENDIF
        Basin_gwstor = Basin_gwstor + gwstor

        dnflow = 0.0
        Gwres_flow(i) = gwflow/gwarea
        far_gwflow = 0.0
        IF ( Cascadegw_flag==1 ) THEN
          IF ( Ncascade_gwr(i)>0 ) THEN
            CALL rungw_cascade(i, Ncascade_gwr(i), Gwres_flow(i), dnflow, far_gwflow)
            Hru_gw_cascadeflow(i) = dnflow + far_gwflow
            Basin_dnflow = Basin_dnflow + dnflow*gwarea
            Basin_farflow = Basin_farflow + far_gwflow*gwarea
          ENDIF
        ENDIF
        Basin_gwflow = Basin_gwflow + Gwres_flow(i)*gwarea

        IF ( Print_debug==1 ) THEN
          gwbal = (last_gwstor + gwin - gwstor - gwsink + Gwstor_minarea_wb(i))/gwarea &
                  - Gwres_flow(i) - dnflow - far_gwflow
          gwup = 0.0D0
          IF ( Cascadegw_flag==1 ) gwup = Gw_upslope(i)
          IF ( ABS(gwbal)>5.0D-4 ) THEN
            WRITE ( BALUNT, * ) 'GWR possible water balance issue', &
                    i, gwbal, last_gwstor, gwin, gwstor, Gwres_flow(i), &
                    gwsink, dnflow, Gw_in_soil(i), Gw_in_ssr(i), &
                    gwup, far_gwflow, gwarea, Gwstor_minarea_wb(i)/gwarea
            IF ( Dprst_flag==1 ) WRITE ( BALUNT, * ) 'Gwin_dprst', Gwin_dprst(i)
          ENDIF
        ENDIF

        ! leave gwin in inch-acres
        Gwres_in(i) = gwin
        Gwres_stor(i) = gwstor/gwarea
        ! Cfs_conv converts acre-inches per timestep to cfs
        IF ( Hru_strmout_flag==1 ) Hru_streamflow_out(i) = gwarea*Cfs_conv*(Sroff(i)+Gwres_flow(i)+Ssres_flow(i))
      ENDDO

      Basin_gwflow = Basin_gwflow*Basin_area_inv
      Basin_gwstor = Basin_gwstor*Basin_area_inv
      Basin_gwsink = Basin_gwsink*Basin_area_inv
      Basin_gwin = Basin_gwin*Basin_area_inv
      Basin_farflow = Basin_farflow*Basin_area_inv
      Basin_gw_upslope = Basin_gw_upslope*Basin_area_inv
      Basin_gwstor_minarea_wb = Basin_gwstor_minarea_wb*Basin_area_inv
      IF ( Strmflow_flag==2 ) Basin_lake_seep = Basin_lake_seep*Basin_area_inv

      ! not going to balance because gwstor under lakes is computed each time step, maybe
      IF ( Print_debug==1 ) THEN
        Basin_dnflow = Basin_dnflow*Basin_area_inv
        ! gwin includes upslope flow, farflow, gwin_dprst, gw_in_soil, gw_in_ssr
        gwbal = last_basin_gwstor - Basin_gwstor - Basin_gwsink &
                + Basin_gwin - Basin_gwflow - Basin_dnflow - Basin_farflow + Basin_gwstor_minarea_wb
        IF ( ABS(gwbal)>5.0D-4 ) WRITE ( BALUNT, * ) 'Possible GWR basin water balance issue'
        WRITE ( BALUNT, 9001 ) Nowtime(1), Nowtime(2), Nowtime(3), &
                gwbal, last_basin_gwstor, Basin_gwstor, Basin_gwin, &
                Basin_gwflow, Basin_gwsink, Basin_farflow, &
                Basin_gw_upslope, Basin_gwstor_minarea_wb, Basin_dnflow
 9001   FORMAT (I5, 2('/', I2.2), 12F11.4)
      ENDIF

      END FUNCTION gwflowrun

!***********************************************************************
!     Compute cascading GW flow
!***********************************************************************
      SUBROUTINE rungw_cascade(Igwr, Ncascade_gwr, Gwres_flow, Dnflow, Far_gwflow)
      USE PRMS_MODULE, ONLY: Nsegmentp1
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in, Strm_farfield
      USE PRMS_GWFLOW, ONLY: Gw_upslope
      USE PRMS_CASCADE, ONLY: Gwr_down, Gwr_down_frac, Cascade_gwr_area
! Cfs_conv converts acre-inches per timestep to cfs
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
      INTRINSIC IABS
! Arguments
      INTEGER, INTENT(IN) :: Igwr, Ncascade_gwr
      REAL, INTENT(INOUT) :: Gwres_flow, Dnflow, Far_gwflow
! Local variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_gwr
        j = Gwr_down(k, Igwr)
        ! Gwres_flow is in inches
! if gwr_down(k, Igwr) > 0, cascade contributes to a downslope GWR
        IF ( j>0 ) THEN
          Gw_upslope(j) = Gw_upslope(j) + Gwres_flow*Cascade_gwr_area(k, Igwr)
          Dnflow = Dnflow + Gwres_flow*Gwr_down_frac(k, Igwr)
! if gwr_down(k, Igwr) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          IF ( j/=Nsegmentp1 ) THEN
            Strm_seg_in(j) = Strm_seg_in(j) + Gwres_flow*Cascade_gwr_area(k, Igwr)*Cfs_conv
          ELSE
            Strm_farfield = Strm_farfield + Gwres_flow*Cascade_gwr_area(k, Igwr)*Cfs_conv
            Far_gwflow = Far_gwflow + Gwres_flow*Gwr_down_frac(k, Igwr)
          ENDIF
        ENDIF
      ENDDO

      ! gwres_flow reduced by cascading flow to HRUs or farfield
      Gwres_flow = Gwres_flow - Dnflow - Far_gwflow
      IF ( Gwres_flow<0.0 ) Gwres_flow = 0.0

      END SUBROUTINE rungw_cascade
