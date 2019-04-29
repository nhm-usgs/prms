!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
      MODULE PRMS_BASIN
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 5.0E-6, INCH2CM = 2.54
      DOUBLE PRECISION, PARAMETER :: DNEARZERO = 1.0D-10
      DOUBLE PRECISION, PARAMETER :: CFS2CMS_CONV = 0.028316847D0
      REAL, PARAMETER :: INCH2MM = 25.4, INCH2M = 0.0254
      REAL, PARAMETER :: MM2INCH = 1.0/INCH2MM
      REAL, PARAMETER :: FEET2METERS = 0.3048
      REAL, PARAMETER :: METERS2FEET = 1.0/FEET2METERS
      CHARACTER(LEN=5), SAVE :: MODNAME
      INTEGER, SAVE :: Numlakes, Timestep, Starttime(6), Endtime(6)
      INTEGER, SAVE :: Start_year, Start_month, Start_day
      INTEGER, SAVE :: End_year, End_month, End_day
      INTEGER, SAVE :: Active_hrus, Active_gwrs
      INTEGER, SAVE :: Hemisphere, Weir_gate_flag
      DOUBLE PRECISION, SAVE :: Cfs2inches, Land_area, Water_area
      DOUBLE PRECISION, SAVE :: Basin_area_inv, Basin_lat
      DOUBLE PRECISION, SAVE :: Totarea, Active_area
      REAL, SAVE, ALLOCATABLE :: Ssres_area(:)
      REAL, SAVE, ALLOCATABLE :: Hru_elev_feet(:), Hru_elev_meters(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_clos(:)
      INTEGER, SAVE, ALLOCATABLE :: Gwr_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Hru_route_order(:), Order(:)
      INTEGER, SAVE, ALLOCATABLE :: Gwr_route_order(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_area(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_hruarea(:)
      CHARACTER(LEN=80), SAVE :: Version_basin
!   Declared Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_frac_perv(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_frac_hru(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_area_max(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_frac_imperv(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_perv(:), Hru_imperv(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_area_open_max(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_area_clos_max(:)
!   Declared Parameters
      INTEGER, SAVE :: Elev_units
      INTEGER, SAVE, ALLOCATABLE :: Hru_type(:), Cov_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Hru_ssres(:)
      INTEGER, SAVE, ALLOCATABLE :: Lake_hru(:), Lake_hru_id(:), Lake_type(:) !not needed if no lakes
      INTEGER, SAVE, ALLOCATABLE :: Tosegment(:), Hru_segment(:), Obsin_segment(:), Segment_type(:)
      REAL, SAVE, ALLOCATABLE :: Hru_area(:), Hru_percent_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Hru_elev(:), Hru_lat(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_open(:), Op_flow_thres(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area(:), Dprst_depth_avg(:)
      END MODULE PRMS_BASIN

!***********************************************************************
!     Main basin routine
!***********************************************************************
      INTEGER FUNCTION basin()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basdecl, basinit
      EXTERNAL :: basrestart
!***********************************************************************
      basin = 0

      IF ( Process(:4)=='decl' ) THEN
        basin = basdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        basin = basinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL basrestart(0)
      ENDIF

      END FUNCTION basin

!***********************************************************************
!     basdecl - set up parameters
!   Declared Parameters
!     print_debug, hru_area, hru_percent_imperv, hru_type, hru_elev,
!     cov_type
!***********************************************************************
      INTEGER FUNCTION basdecl()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Model, Nhru, Ngw, Nssr, Nlake, Dprst_flag, Init_vars_from_file, Nsegment, Strmflow_flag, Soltab_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module, basrestart
!***********************************************************************
      basdecl = 0

      Version_basin = '$Id: basin.f90 5632 2013-04-25 18:28:20Z rsregan $'
      CALL print_module(Version_basin, 'Basin Definition          ', 90)
      MODNAME = 'basin'

! Declared Variables

      ALLOCATE ( Hru_imperv(Nhru) )
      IF ( declvar(MODNAME, 'hru_imperv', 'nhru', Nhru, 'double', &
     &     'Area of HRU that is impervious', &
     &     'acres', Hru_imperv)/=0 ) CALL read_error(3, 'hru_imperv')

      ALLOCATE ( Hru_perv(Nhru) )
      IF ( declvar(MODNAME, 'hru_perv', 'nhru', Nhru, 'double', &
     &     'Area of HRU that is pervious', &
     &     'acres', Hru_perv)/=0 ) CALL read_error(3, 'hru_perv')

      ALLOCATE ( Hru_frac_imperv(Nhru) )
      IF ( declvar(MODNAME, 'hru_frac_imperv', 'nhru', Nhru, 'double', &
     &     'Fraction of HRU that is impervious', &
     &     'decimal fraction', Hru_frac_imperv)/=0 ) CALL read_error(3, 'hru_frac_imperv')

      ALLOCATE ( Hru_frac_perv(Nhru) )
      IF ( declvar(MODNAME, 'hru_frac_perv', 'nhru', Nhru, 'double', &
     &     'Fraction of HRU that is pervious', &
     &     'decimal fraction', Hru_frac_perv)/=0 ) CALL read_error(3, 'hru_frac_perv')

      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Dprst_frac_hru(Nhru) )
        IF ( declvar(MODNAME, 'dprst_frac_hru', 'nhru', Nhru, 'double', &
     &       'Fraction of HRU that has surface-depression storage', &
     &       'decimal fraction', Dprst_frac_hru)/=0 ) CALL read_error(3, 'dprst_frac_hru')
        ALLOCATE ( Dprst_area_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_max', 'nhru', Nhru, 'double', &
     &       'Aggregate sum of surface depression areas of each HRU', &
     &       'acres', Dprst_area_max)/=0 ) CALL read_error(1, 'dprst_area_max')
        ALLOCATE ( Dprst_area_open_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_open_max', 'nhru', Nhru, 'double', &
     &       'Aggregate sum of open surface depression areas of each HRU', &
     &       'acres', Dprst_area_open_max)/=0 ) CALL read_error(1, 'dprst_area_open_max')
        ALLOCATE ( Dprst_area_clos_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_clos_max', 'nhru', Nhru, 'double', &
     &       'Aggregate sum of closed surface depression areas of each HRU', &
     &       'acres', Dprst_area_clos_max)/=0 ) CALL read_error(1, 'dprst_area_clos_max')

        ALLOCATE ( Dprst_area(Nhru), Dprst_frac_open(Nhru), Dprst_frac_clos(Nhru) )
        ALLOCATE ( Dprst_depth_avg(Nhru), Op_flow_thres(Nhru) )
        IF ( Init_vars_from_file==0 .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'dprst_area', 'nhru', 'real', &
     &         '0.0', '0.0', '1.0E9', &
     &         'Aggregate sum of surface depression areas of each HRU', &
     &         'Aggregate sum of surface depression areas of each HRU', &
     &         'acres')/=0 ) CALL read_error(1, 'dprst_area')
          IF ( declparam(MODNAME, 'dprst_frac_open', 'nhru', 'real', &
     &         '1.0', '0.0', '1.0', &
     &         'Fraction of open surface depression storage area within'// &
     &         ' an HRU that can generate surface runoff as a function of storage volume', &
     &         'Fraction of open surface depression storage area within'// &
     &         ' an HRU that can generate surface runoff as a function of storage volume', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_open')
          IF ( declparam(MODNAME, 'dprst_depth_avg', 'nhru', 'real', &
     &         '132.0', '0.0', '10000.0', &
     &         'Average depth of surface depressions at maximum storage capacity', &
     &         'Average depth of surface depressions at maximum storage capacity', &
     &         'inches')/=0 ) CALL read_error(1, 'dprst_depth_avg')
          IF ( declparam(MODNAME, 'op_flow_thres', 'nhru', 'real', &
     &         '1.0', '0.0', '1.0', &
     &         'Fraction of open depression storage above'// &
     &         ' which surface runoff occurs for each timestep', &
     &         'Fraction of open depression storage above'// &
     &         ' which surface runoff occurs; any water above'// &
     &         ' maximum open storage capacity spills as surface runoff', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'op_flow_thres')
        ENDIF
      ENDIF

      ! Allocate parameters and local module arrays
      IF ( Soltab_flag==1 .OR. Model==99 ) ALLOCATE ( Hru_lat(Nhru) )
      ALLOCATE ( Hru_area(Nhru), Hru_elev(Nhru), Hru_type(Nhru) )
      ALLOCATE ( Hru_percent_imperv(Nhru), Cov_type(Nhru) )
      IF ( (Nsegment>0.AND.Strmflow_flag>1) .OR. Model==99 ) THEN
        ALLOCATE ( Tosegment(Nsegment), Order(Nsegment) )
        ALLOCATE ( Hru_segment(Nhru), Segment_hruarea(Nsegment), Obsin_segment(Nsegment) )
        IF ( Strmflow_flag==2 .OR. Model==99 ) ALLOCATE ( Segment_type(Nsegment) )
      ENDIF
      IF ( Nlake>0 ) ALLOCATE ( Lake_hru_id(Nhru), Lake_area(Nlake), Lake_hru(Nlake), Lake_type(Nlake) )
      ALLOCATE ( Hru_route_order(Nhru), Gwr_route_order(Ngw), Gwr_type(Ngw), Hru_elev_feet(Nhru), Hru_elev_meters(Nhru) )
      ALLOCATE ( Ssres_area(Nssr), Hru_ssres(Nhru) )

      ! Declared Parameters
      IF ( Init_vars_from_file==0 .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'hru_area', 'nhru', 'real', &
     &       '1.0', '0.01', '1.0E9', &
     &       'HRU area', 'Area of each HRU', &
     &       'acres')/=0 ) CALL read_error(1, 'hru_area')
        IF ( declparam(MODNAME, 'elev_units', 'one', 'integer', &
     &       '0', '0', '1', &
     &       'Elevation units flag', &
     &       'Flag to indicate the units of the elevation values (0=feet; 1=meters)', &
     &       'none')/=0 ) CALL read_error(1, 'elev_units')
        IF ( declparam(MODNAME, 'hru_elev', 'nhru', 'real', &
     &       '0.0', '-1000.0', '30000.0', &
     &       'HRU mean elevation', 'Mean elevation for each HRU', &
     &       'elev_units')/=0 ) CALL read_error(1, 'hru_elev')
        IF ( Soltab_flag==1 .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'hru_lat', 'nhru', 'real', &
     &         '40.0', '-90.0', '90.0', &
     &         'HRU latitude', 'Latitude of each HRU', &
     &         'degrees')/=0 ) CALL read_error(1, 'hru_lat')
        ENDIF
        IF ( declparam(MODNAME, 'hru_percent_imperv', 'nhru', 'real', &
     &       '0.0', '0.0', '0.999', &
     &       'HRU percent impervious', 'Fraction of each HRU area that is impervious', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'Hru_percent_imperv')
        IF ( declparam(MODNAME, 'hru_type', 'nhru', 'integer', &
     &       '1', '0', '3', &
     &       'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake; 3=swale)', &
     &       'none')/=0 ) CALL read_error(1, 'hru_type')
        IF ( declparam(MODNAME, 'cov_type', 'nhru', 'integer', &
     &       '3', '0', '4', &
     &       'Cover type designation for HRU', &
     &       'Vegetation cover type for each HRU (0=bare soil; 1=grasses; 2=shrubs; 3=trees; 4=coniferous)', &
     &       'none')/=0 ) CALL read_error(1, 'cov_type')

        IF ( Nhru/=Nssr .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'hru_ssres', 'nhru', 'integer', &
     &         '1', 'bounded', 'nssr', &
     &         'Index of subsurface reservoir assigned to HRU', &
     &         'Index of subsurface reservoir receiving excess water from capillary reservoir (deprecated)', &
     &         'none')/=0 ) CALL read_error(1, 'hru_ssres')
        ENDIF

        IF ( (Nsegment>0.AND.Strmflow_flag==2) .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'segment_type', 'nsegment', 'integer', &
     &         '0', '0', '3', &
     &         'Segment type', &
     &         'Segment type (0=link; 1=diversion; 2=lake; 3=replace inflow)', &
     &         'none')/=0 ) CALL read_error(1, 'segment_type')
        ENDIF
        IF ( (Nsegment>0.AND.Strmflow_flag>1) .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'tosegment', 'nsegment', 'integer', &
     &         '0', 'bounded', 'nsegment', &
     &         'The index of the downstream segment', &
     &         'Index of downstream segment to which the segment'// &
     &         ' streamflow flows, for segments that do not flow to another segment enter 0', &
     &         'none')/=0 ) CALL read_error(1, 'tosegment')
          IF ( declparam(MODNAME, 'hru_segment', 'nhru', 'integer', &
     &         '0', 'bounded', 'nsegment', &
     &         'Segment index for HRU lateral inflows', &
     &         'Segment index to which an HRU contributes lateral flows'// &
     &         ' (surface runoff, interflow, and groundwater discharge)', &
     &         'none')/=0 ) CALL read_error(1, 'hru_segment')
          IF ( declparam(MODNAME, 'obsin_segment', 'nsegment', 'integer', &
     &         '0', 'bounded', 'nobs', &
     &         'Index of measured streamflow station that replaces inflow to a segment', &
     &         'Index of measured streamflow station that replaces inflow to a segment', &
     &         'none')/=0 ) CALL read_error(1, 'obsin_segment')
        ENDIF

        IF ( Nlake>0 .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'lake_hru_id', 'nhru', 'integer', &
     &         '0', 'bounded', 'nhru', &
     &         'Identification number of the lake associated with an HRU', &
     &         'Identification number of the lake associated with'// &
     &         ' an HRU; more than one HRU can be associated with each lake', &
     &         'none')/=0 ) CALL read_error(1, 'lake_hru_id')
          IF ( declparam(MODNAME, 'lake_hru', 'nlake', 'integer', &
     &         '0', 'bounded', 'nhru', &
     &         'Index of HRU for each lake HRU', &
     &         'Index of HRU for each lake HRU', &
     &         'none')/=0 ) CALL read_error(1, 'lake_hru')
          IF ( declparam(MODNAME, 'lake_type', 'nlake', 'integer', &
     &         '1', '1', '6', &
     &         'Type of lake', &
     &         'Type of lake (1=Puls routing; 2=Linear routing; 3=Flow through;'// &
     &         ' 4=Broad crested weir; 5=Gate opening; 6=measured flow)', &
     &         'none')/=0 ) CALL read_error(1, 'lake_type')
        ENDIF

        Timestep = 0
      ELSE
        CALL basrestart(1)
      ENDIF

      END FUNCTION basdecl

!**********************************************************************
!     basinit - check for validity of basin parameters
!               and compute reservoir areas
!**********************************************************************
      INTEGER FUNCTION basinit()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Nlake, Dprst_flag, Strmflow_flag, Print_debug, Inputerror_flag, Soltab_flag, &
     &    Nsegment, Nsegmentp1, Model
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL dattim, write_outfile
      INTRINSIC ABS, DBLE
! Local Variables
      CHARACTER(LEN=68) :: buffer
      INTEGER :: i, j, ierr, test, lval
      INTEGER, ALLOCATABLE :: x_off(:)
      DOUBLE PRECISION :: harea, tmp, basin_imperv, basin_perv, basin_dprst
!**********************************************************************
      basinit = 0

      CALL dattim('start', Starttime)
      Start_year = Starttime(1)
      Start_month = Starttime(2)
      Start_day = Starttime(3)
      CALL dattim('end', Endtime)
      End_year = Endtime(1)
      End_month = Endtime(2)
      End_day = Endtime(3)

      IF ( Timestep==0 ) THEN
        IF ( getparam(MODNAME, 'hru_area', Nhru, 'real', Hru_area)/=0 ) CALL read_error(2, 'hru_area')
        IF ( getparam(MODNAME, 'hru_elev', Nhru, 'real', Hru_elev)/=0 ) CALL read_error(2, 'hru_elev')
        IF ( Soltab_flag==1 ) THEN
          IF ( getparam(MODNAME, 'hru_lat', Nhru, 'real', Hru_lat)/=0 ) CALL read_error(2, 'hru_lat')
        ENDIF
        IF ( getparam(MODNAME, 'hru_type', Nhru, 'integer', Hru_type)/=0 ) CALL read_error(2, 'hru_type')
        IF ( getparam(MODNAME, 'cov_type', Nhru, 'integer', Cov_type)/=0 ) CALL read_error(2, 'cov_type')
        IF ( getparam(MODNAME, 'elev_units', 1, 'integer', Elev_units)/=0 ) CALL read_error(2, 'elev_units')
        IF ( Elev_units<0 .OR. Elev_units>1 ) THEN
          PRINT *, 'ERROR, invalid elev_units value:', Elev_units
          Inputerror_flag = 1
        ENDIF
        IF ( getparam(MODNAME, 'hru_percent_imperv', Nhru, 'real', Hru_percent_imperv)/=0 ) CALL read_error(2, 'hru_percent_imperv')
        IF ( Dprst_flag==1 ) THEN
          IF ( getparam(MODNAME, 'dprst_frac_open', Nhru, 'real', Dprst_frac_open)/=0 ) CALL read_error(2, 'dprst_frac_open')
          IF ( getparam(MODNAME, 'dprst_area', Nhru, 'real', Dprst_area)/=0 ) CALL read_error(2, 'dprst_area')
          IF ( getparam(MODNAME, 'dprst_depth_avg', Nhru, 'real', Dprst_depth_avg)/=0 ) CALL read_error(2, 'dprst_depth_avg')
          IF ( getparam(MODNAME, 'op_flow_thres', Nhru, 'real', Op_flow_thres)/=0 ) CALL read_error(2, 'op_flow_thres')
          Dprst_frac_clos = 0.0
          Dprst_area_max = 0.0D0
          Dprst_area_open_max = 0.0D0
          Dprst_area_clos_max = 0.0D0
          Dprst_frac_hru = 0.0D0
        ENDIF
        IF ( Strmflow_flag>1 ) THEN
          IF ( Strmflow_flag==2 ) THEN
            IF ( getparam(MODNAME, 'segment_type', Nsegment, 'integer', Segment_type)/=0 ) CALL read_error(2, 'segment_type')
          ENDIF
          IF ( getparam(MODNAME, 'tosegment', Nsegment, 'integer', Tosegment)/=0 ) CALL read_error(2, 'tosegment')
          IF ( getparam(MODNAME, 'hru_segment', Nhru, 'integer', Hru_segment)/=0 ) CALL read_error(2, 'hru_segment')
          IF ( getparam(MODNAME, 'obsin_segment', Nsegment, 'integer', Obsin_segment)/=0 ) CALL read_error(2, 'obsin_segment')
        ENDIF
        IF ( Nlake>0 ) THEN
          IF ( getparam(MODNAME, 'lake_hru_id', Nhru, 'integer', Lake_hru_id)/=0 ) CALL read_error(1, 'lake_hru_id')
          IF ( getparam(MODNAME, 'lake_hru', Nlake, 'real', Lake_hru)/=0 ) CALL read_error(2, 'lake_hru')
          IF ( getparam(MODNAME, 'lake_type', Nlake, 'integer', Lake_type)/=0 ) CALL read_error(2, 'lake_type')
          Lake_area = 0.0D0
        ENDIF

        Numlakes = 0
        Totarea = 0.0D0
        Land_area = 0.0D0
        Water_area = 0.0D0
        Active_area = 0.0D0
        Basin_lat = 0.0D0
        Hru_route_order = 0
        j = 0
        DO i = 1, Nhru
          ierr = 0
          harea = DBLE( Hru_area(i) )
          Totarea = Totarea + harea
          Hru_frac_imperv(i) = Hru_percent_imperv(i)
          IF ( Hru_type(i)==1 .OR. Hru_type(i)==3 ) THEN
            Land_area = Land_area + harea
            IF ( Hru_frac_imperv(i)>0.999 ) THEN
              PRINT *, 'ERROR, hru_percent_imperv value > 0.999 for HRU:', i, Hru_frac_imperv(i)
              ierr = 1
            ELSEIF ( Hru_frac_imperv(i)<0.0 ) THEN
              PRINT *, 'ERROR, hru_percent_imperv value < 0.0 for HRU:', i, Hru_frac_imperv(i)
              ierr = 1
            ENDIF
          ELSEIF ( Hru_type(i)/=0 .AND. Hru_type(i)/=2 ) THEN
            PRINT *, 'ERROR, invalid hru_type value for HRU:', i, Hru_type(i)
            ierr = 1
          ELSEIF ( Hru_type(i)==2 ) THEN
            Water_area = Water_area + harea
            Numlakes = Numlakes + 1
            IF ( Nlake<1 ) THEN
              PRINT *, 'ERROR, invalid hru_type = 2 value for HRU:', i, ' and nlake = 0'
              ierr = 1
            ELSEIF ( Lake_hru_id(i)>Nlake ) THEN
              PRINT *, 'ERROR, lake_hru_id > nlake for lake HRU:', i, ' lake_hru_id:', Lake_hru_id(i), ' nlake:', Nlake
              ierr = 1
            ELSEIF ( Lake_hru_id(i)==0 ) THEN
              PRINT *, 'ERROR, lake_hru_id = 0 for lake HRU:', i
              ierr = 1
            ELSE
              Lake_area(Lake_hru_id(i)) = Lake_area(Lake_hru_id(i)) + harea
            ENDIF
          ELSEIF ( Hru_type(i)==0 ) THEN
            CYCLE
          ENDIF
          IF ( Cov_type(i)<0 .OR. Cov_type(i)>4 ) THEN
            PRINT *, 'ERROR, invalid cov_type value for HRU:', i, Cov_type(i)
            ierr = 1
          ENDIF
          IF ( Hru_area(i)<NEARZERO ) THEN
            PRINT *, 'ERROR, hru_area value <= 0.0 for HRU:', i, Hru_area(i)
            ierr = 1
          ENDIF
          IF ( Hru_percent_imperv(i)<NEARZERO ) Hru_frac_imperv(i) = 0.0D0
          Hru_imperv(i) = Hru_frac_imperv(i)*harea
          IF ( Hru_imperv(i)<DNEARZERO ) Hru_imperv(i) = 0.0D0
          Hru_perv(i) = harea - Hru_imperv(i)
          IF ( Dprst_flag==1 ) THEN
            IF ( Dprst_area(i)<0.0 ) THEN
              PRINT *, 'ERROR, dprst_area specified < 0.0 for HRU:', i, Dprst_area(i)
              ierr = 1
            ENDIF
            IF ( Op_flow_thres(i)<0.0 ) THEN
              PRINT *, 'ERROR, op_flow_thres specified < 0.0 for HRU:', i, Op_flow_thres(i)
              ierr = 1
            ENDIF
          ENDIF
          IF ( ierr==1 ) THEN
            Inputerror_flag = 1
            CYCLE
          ENDIF
          j = j + 1
          Hru_route_order(j) = i
          IF ( Soltab_flag==1 ) Basin_lat = Basin_lat + Hru_lat(i)*harea
          IF ( Elev_units==0 ) THEN
            Hru_elev_feet(i) = Hru_elev(i)
            Hru_elev_meters(i) = Hru_elev_feet(i)*FEET2METERS
          ELSE
            Hru_elev_meters(i) = Hru_elev(i)
            Hru_elev_feet(i) = Hru_elev_feet(i)*METERS2FEET
          ENDIF
          IF ( Dprst_flag==1 ) THEN
            IF ( Dprst_area(i)>NEARZERO ) THEN
              Dprst_area_open_max(i) = Dprst_area(i)*Dprst_frac_open(i)
              Dprst_frac_clos(i) = 1.0 - Dprst_frac_open(i)
              ! accounting for possible round-off error
              IF ( ABS(Dprst_frac_clos(i))<NEARZERO ) THEN
                Dprst_frac_clos(i) = 0.0
                Dprst_area_open_max(i) = Dprst_area(i)
              ENDIF
              Dprst_area_clos_max(i) = Dprst_area(i) - Dprst_area_open_max(i)
            ENDIF
            IF ( Dprst_area_open_max(i)<DNEARZERO ) Dprst_area_open_max(i) = 0.0D0
            IF ( Dprst_area_clos_max(i)<DNEARZERO ) Dprst_area_clos_max(i) = 0.0D0
            Dprst_area_max(i) = Dprst_area_open_max(i) + Dprst_area_clos_max(i)
            Hru_perv(i) = Hru_perv(i) - Dprst_area_max(i)
            tmp = harea - Hru_perv(i) - Hru_imperv(i) - Dprst_area(i)
            IF ( ABS(tmp)>0.0 ) THEN
              IF ( Dprst_area_open_max(i)>0.0 ) THEN
                Dprst_area_open_max(i) = Dprst_area_open_max(i) + tmp
              ELSE
                Dprst_area_clos_max(i) = Dprst_area_clos_max(i) + tmp
              ENDIF
            ENDIF
            Dprst_frac_hru(i) = Dprst_area_max(i)/harea
            IF ( Hru_frac_imperv(i)+Dprst_frac_hru(i)>0.999 ) THEN
              tmp = 0.999 - Hru_frac_imperv(i)
              Dprst_area_max(i) = tmp*harea
            ENDIF
            Dprst_frac_hru(i) = Dprst_area_max(i)/harea
            Hru_perv(i) = harea - Hru_imperv(i) - Dprst_area_max(i)
            tmp = Hru_perv(i)/harea
            ! sanity check
            IF ( tmp<0.001) THEN
              PRINT *, 'ERROR, hru_imperv+dprst_area > 0.999*hru_area for HRU:', i
              PRINT *, '    hru_area:', Hru_area(i), ' hru_imperv:', Hru_imperv(i), ' dprst_area:', Dprst_area(i)
              PRINT *, '    (hru_imperv+dprst_area)/hru_area =', (Hru_imperv(i)+Dprst_area(i))/Hru_area(i)
              Inputerror_flag = 1
              CYCLE
            ENDIF
          ENDIF
          Hru_frac_perv(i) = Hru_perv(i)/harea
          IF ( Hru_frac_perv(i)>0.9999 ) THEN
            Hru_frac_perv(i) = 1.0D0
            Hru_perv(i) = harea
            Hru_imperv(i) = 0.0D0
            Hru_frac_imperv(i) = 0.0D0
          ENDIF
        ENDDO
        Active_hrus = j
        Active_area = Land_area + Water_area
        IF ( Active_area<DNEARZERO ) THEN
          PRINT *, 'ERROR, active area <= 0.0', Active_area
          Inputerror_flag = 1
        ENDIF

        IF ( Numlakes/=Nlake ) THEN
          PRINT *, 'ERROR, number of lakes specified in hru_type'
          PRINT *, 'does not equal dimension nlake:', Nlake, ', numlakes:', Numlakes
          PRINT *, '**Correct or add nlake to the Parameter File**'
          Inputerror_flag = 1
        ENDIF
        Weir_gate_flag = 0
        IF ( Nlake>0 ) THEN
          DO i = 1, Nlake
            j = Lake_hru(i)
            IF ( j>0 ) THEN
              IF ( Lake_hru_id(j)==0 ) THEN
                Lake_hru_id(j) = i
              ELSEIF ( Lake_hru_id(j)/=i ) THEN
                PRINT *, 'ERROR, parameter values for lake_hru and lake_hru_id in conflict for Lake:', i, ', HRU:', j
                Inputerror_flag = 1
                CYCLE
              ENDIF
            ELSE
              PRINT *, 'ERROR, lake_hru=0 for lake:', i
              Inputerror_flag = 1
              CYCLE
            ENDIF
            IF ( Hru_type(j)/=2 ) THEN
              PRINT *, 'ERROR, HRU:', j, ' specifed to be a lake by lake_hru but hru_type not equal 2'
              Inputerror_flag = 1
            ENDIF
          ENDDO
        ENDIF

        IF ( Strmflow_flag>1 ) THEN
          ! Begin the loops for ordering segments
          Segment_hruarea = 0.0D0
          DO j = 1, Nsegment
            IF ( Tosegment(j)>Nsegment ) THEN
              PRINT *, 'ERROR, tosegment value (', Tosegment(j), ') > nsegment (', j, ')'
              Inputerror_flag = 1
              CYCLE
            ENDIF
            IF ( Tosegment(j)==0 ) Tosegment(j) = Nsegmentp1
            DO i = 1, Nhru
              IF ( Hru_segment(i)==i ) Segment_hruarea(j) = Segment_hruarea(j) + Hru_area(i)
            ENDDO
          ENDDO

          ALLOCATE ( x_off(Nsegment) )
          x_off = 0
          Order = 0
          lval = 0
          DO WHILE ( lval<Nsegment )
            DO i = 1, Nsegment
!     If segment "i" has not been crossed out consider it, else continue
              IF ( x_off(i)==1 ) CYCLE
!     Test to see if segment "i" is the to segment from other segments
              test = 1
              DO j = 1, Nsegment
                IF ( Tosegment(j)==i ) THEN
!     If segment "i" is a to segment, test to see if the originaing
!     segment has been crossed off the list.  if all have been, then
!     put the segment in as an ordered segment
                  IF ( x_off(j)==0 ) THEN
                    test = 0
                    EXIT
                  ENDIF
                ENDIF
              ENDDO
              IF ( test==1 ) THEN
                lval = lval + 1
                Order(lval) = i
                x_off(i) = 1
              ENDIF
            ENDDO
          ENDDO
!          IF ( Print_debug>-1 ) THEN
!            PRINT *, 'Stream Network Routing Order:'
!            PRINT '(10I5)', Order
!            PRINT *, 'tosegment:'
!            PRINT '(10I5)', Tosegment
!          ENDIF
          DEALLOCATE ( x_off )
        ENDIF

        IF ( Inputerror_flag==1 ) STOP

        IF ( Nssr==Nhru ) THEN
          DO i = 1, Nhru
            Ssres_area(i) = Hru_area(i)
            Hru_ssres(i) = i
          ENDDO
        ELSE
          IF ( getparam(MODNAME, 'hru_ssres', Nhru, 'integer', Hru_ssres)/=0 ) CALL read_error(2, 'hru_ssres')
          Ssres_area = 0.0
          DO i = 1, Nhru
            j = Hru_ssres(i)
            Ssres_area(j) = Ssres_area(j) + Hru_area(i)
          ENDDO
        ENDIF

        IF ( Ngw==Nhru ) THEN
          Active_gwrs = Active_hrus
          Gwr_route_order = Hru_route_order
          Gwr_type = Hru_type
          DO j = 1, Active_gwrs
            i = Gwr_route_order(j)
            IF ( Gwr_type(i)==3 .AND. Model/=0 ) THEN
              IF ( Print_debug>-1 ) PRINT *, 'Warning, GWRs cannot be swales for GWR:', i, ' set to 1'
              Gwr_type(i) = 1
            ENDIF
          ENDDO
        ELSE
          DO i = 1, Ngw
            Gwr_route_order(i) = i
          ENDDO
          Active_gwrs = Ngw
          Gwr_type = 1
        ENDIF
        IF ( Nlake>0 ) THEN
          DO i = 1, Nlake
            j = Lake_hru(i)
            IF ( Lake_type(i)==4 .OR. Lake_type(i)==5 ) THEN
              Weir_gate_flag = 1
              IF ( j==0 .AND. Model/=0 ) THEN
                PRINT *, 'ERROR, lake_type = 4 or 5 and lake_hru for GWR:', j, ' specified as 0'
                Inputerror_flag = 1
              ENDIF
              Gwr_type(j) = 2
            ENDIF
          ENDDO
        ENDIF

        Basin_area_inv = 1.0D0/Active_area
        Cfs2inches = Basin_area_inv*12.0D0*86400.0D0/43560.0D0
        Basin_lat = Basin_lat*Basin_area_inv
        ! used in solrad modules to winter/summer radiation adjustment
        IF ( Basin_lat>0.0D0 ) THEN
          Hemisphere = 0 ! Northern
        ELSE
          Hemisphere = 1 ! Southern
        ENDIF
      ENDIF

      basin_perv = 0.0D0
      basin_imperv = 0.0D0
      basin_dprst = 0.0D0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        basin_perv = basin_perv + Hru_perv(i)
        basin_imperv = basin_imperv + Hru_imperv(i)
        IF ( Dprst_flag==1 ) basin_dprst = basin_dprst + Dprst_area_max(i)
      ENDDO
      basin_perv = basin_perv*Basin_area_inv
      basin_imperv = basin_imperv*Basin_area_inv
      IF ( Dprst_flag==1 ) basin_dprst = basin_dprst*Basin_area_inv

      IF ( Print_debug==2 ) THEN
        PRINT *, ' HRU     Area'
        PRINT ('(I4, F14.5)'), (i, Hru_area(i), i=1, Nhru)
        PRINT *, 'Sum of HRU areas      = ', totarea
        PRINT *, 'Active basin area     = ', active_area
        PRINT *, 'Impervious basin area = ', basin_imperv
        PRINT *, 'Pervious basin area   = ', basin_perv
        IF ( Dprst_flag==1 ) PRINT *, 'Depression storage basin area =', basin_dprst
        PRINT *, ' '
      ENDIF

!     print out start and end times
      CALL write_outfile('  ')
      CALL write_outfile(' Surface Water and Energy Budgets Simulated by PRMS Version 3.0.'//Version_basin(16:30))
      WRITE (buffer, 9002) ' Start time: ', Starttime
      CALL write_outfile(buffer(:32))
      WRITE (buffer, 9002) ' End time:   ', Endtime
      CALL write_outfile(buffer(:32))
      WRITE (buffer, 9003) ' Sum of HRU areas:', totarea, ' Active basin area:', active_area
      CALL write_outfile(buffer(:61))
      WRITE (buffer, 9003) ' Impervious basin area:', basin_imperv, ' Pervious basin area:', basin_perv
      CALL write_outfile(buffer)
      IF ( Dprst_flag==1 ) THEN
        WRITE (buffer, 9003) ' Depression storage basin area:', basin_dprst
        CALL write_outfile(buffer(:45))
      ENDIF
      CALL write_outfile(' ')

 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
 9003 FORMAT (A, F12.2, A, F12.2)

      END FUNCTION basinit

!***********************************************************************
!     basrestart - write or read basin restart file
!***********************************************************************
      SUBROUTINE basrestart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Dprst_flag, Nlake, Strmflow_flag, Soltab_flag
      USE PRMS_BASIN
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=5) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Timestep, Elev_units, Basin_lat, Water_area, Land_area, Totarea, &
     &          Active_area, Active_hrus, Active_gwrs, Numlakes, Basin_area_inv, Cfs2inches, Hemisphere, Weir_gate_flag
        WRITE ( Restart_outunit ) Hru_frac_perv
        WRITE ( Restart_outunit ) Hru_frac_imperv
        WRITE ( Restart_outunit ) Hru_perv
        WRITE ( Restart_outunit ) Hru_imperv
        IF ( Dprst_flag==1 ) THEN
          WRITE ( Restart_outunit ) Dprst_frac_hru
          WRITE ( Restart_outunit ) Dprst_area_max
          WRITE ( Restart_outunit ) Dprst_area_open_max
          WRITE ( Restart_outunit ) Dprst_area_clos_max
          WRITE ( Restart_outunit ) Dprst_frac_open
          WRITE ( Restart_outunit ) Dprst_frac_clos
          WRITE ( Restart_outunit ) Dprst_area
          WRITE ( Restart_outunit ) Dprst_depth_avg
        ENDIF
        WRITE ( Restart_outunit ) Hru_type
        WRITE ( Restart_outunit ) Gwr_type
        WRITE ( Restart_outunit ) Cov_type
        WRITE ( Restart_outunit ) Hru_area
        WRITE ( Restart_outunit ) Hru_percent_imperv
        WRITE ( Restart_outunit ) Hru_elev
        WRITE ( Restart_outunit ) Hru_elev_feet
        WRITE ( Restart_outunit ) Hru_elev_meters
        IF ( Soltab_flag==1 ) WRITE ( Restart_outunit ) Hru_lat
        WRITE ( Restart_outunit ) Hru_route_order
        WRITE ( Restart_outunit ) Gwr_route_order
        WRITE ( Restart_outunit ) Ssres_area
        WRITE ( Restart_outunit ) Hru_ssres
        IF ( Strmflow_flag>1 ) THEN
          IF ( Strmflow_flag==2 ) WRITE ( Restart_outunit ) Segment_type
          WRITE ( Restart_outunit ) Tosegment
          WRITE ( Restart_outunit ) Hru_segment
          WRITE ( Restart_outunit ) Segment_hruarea
          WRITE ( Restart_outunit ) Obsin_segment
          WRITE ( Restart_outunit ) Order
        ENDIF
        IF ( Nlake>0 ) THEN
          WRITE ( Restart_outunit ) Lake_hru_id
          WRITE ( Restart_outunit ) Lake_hru
          WRITE ( Restart_outunit ) Lake_type
          WRITE ( Restart_outunit ) Lake_area
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Timestep, Elev_units, Basin_lat, Water_area, Land_area, Totarea, &
     &         Active_area, Active_hrus, Active_gwrs, Numlakes, Basin_area_inv, Cfs2inches, Hemisphere, Weir_gate_flag
        READ ( Restart_inunit ) Hru_frac_perv
        READ ( Restart_inunit ) Hru_frac_imperv
        READ ( Restart_inunit ) Hru_perv
        READ ( Restart_inunit ) Hru_imperv
        IF ( Dprst_flag==1 ) THEN
          READ ( Restart_inunit ) Dprst_frac_hru
          READ ( Restart_inunit ) Dprst_area_max
          READ ( Restart_inunit ) Dprst_area_open_max
          READ ( Restart_inunit ) Dprst_area_clos_max
          READ ( Restart_inunit ) Dprst_frac_open
          READ ( Restart_inunit ) Dprst_frac_clos
          READ ( Restart_inunit ) Dprst_area
          READ ( Restart_inunit ) Dprst_depth_avg 
        ENDIF
        READ ( Restart_inunit ) Hru_type
        READ ( Restart_inunit ) Gwr_type
        READ ( Restart_inunit ) Cov_type
        READ ( Restart_inunit ) Hru_area
        READ ( Restart_inunit ) Hru_percent_imperv
        READ ( Restart_inunit ) Hru_elev
        READ ( Restart_inunit ) Hru_elev_feet
        READ ( Restart_inunit ) Hru_elev_meters
        IF ( Soltab_flag==1 ) READ ( Restart_inunit ) Hru_lat
        READ ( Restart_inunit ) Hru_route_order
        READ ( Restart_inunit ) Gwr_route_order
        READ ( Restart_inunit ) Ssres_area
        READ ( Restart_inunit ) Hru_ssres
        IF ( Strmflow_flag>1 ) THEN
          IF ( Strmflow_flag==2 ) READ ( Restart_inunit ) Segment_type
          READ ( Restart_inunit ) Tosegment
          READ ( Restart_inunit ) Hru_segment
          READ ( Restart_inunit ) Segment_hruarea
          READ ( Restart_inunit ) Obsin_segment
          READ ( Restart_inunit ) Order
        ENDIF
        IF ( Nlake>0 ) THEN
          READ ( Restart_inunit ) Lake_hru_id
          READ ( Restart_inunit ) Lake_hru
          READ ( Restart_inunit ) Lake_type
          READ ( Restart_inunit ) Lake_area
        ENDIF
      ENDIF
      END SUBROUTINE basrestart
