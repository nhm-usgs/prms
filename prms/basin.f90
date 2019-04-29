!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
      MODULE PRMS_BASIN
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 1.0E-6, INCH2CM = 2.54, IGNOREPPT = 1.0E-5
      REAL, PARAMETER :: CLOSEZERO = 1.0E-09, SMALLPARAM = 1.0E-4
      DOUBLE PRECISION, PARAMETER :: DNEARZERO = 1.0D-10, FT2_PER_ACRE = 43560.0D0
      DOUBLE PRECISION, PARAMETER :: CFS2CMS_CONV = 0.028316847D0
      REAL, PARAMETER :: INCH2MM = 25.4, INCH2M = 0.0254
      REAL, PARAMETER :: MM2INCH = 1.0/INCH2MM
      REAL, PARAMETER :: FEET2METERS = 0.3048
      REAL, PARAMETER :: METERS2FEET = 1.0/FEET2METERS
      CHARACTER(LEN=5), SAVE :: MODNAME
      INTEGER, SAVE :: Numlake_hrus, Active_hrus, Active_gwrs
      INTEGER, SAVE :: Hemisphere, Dprst_clos_flag, Hru_order_flag
      DOUBLE PRECISION, SAVE :: Land_area, Water_area
      DOUBLE PRECISION, SAVE :: Basin_area_inv, Basin_lat, Totarea, Active_area
      REAL, SAVE, ALLOCATABLE :: Ssres_area(:), Hru_elev_feet(:), Hru_elev_meters(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_clos(:)
      INTEGER, SAVE, ALLOCATABLE :: Gwr_type(:), Hru_route_order(:), Gwr_route_order(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_area(:)
      INTEGER, SAVE :: Noarea_flag, Weir_gate_flag
      INTEGER, SAVE, ALLOCATABLE :: Segment_order(:), Segment_up(:)
      REAL, SAVE, ALLOCATABLE :: Segment_hruarea(:)
      CHARACTER(LEN=80), SAVE :: Version_basin
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Hru_frac_perv(:), Hru_frac_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Hru_frac_dprst(:), Dprst_area_max(:)
      REAL, SAVE, ALLOCATABLE :: Hru_perv(:), Hru_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open_max(:), Dprst_area_clos_max(:)
!   Declared Parameters
      INTEGER, SAVE :: Elev_units
      INTEGER, SAVE, ALLOCATABLE :: Hru_type(:), Cov_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Hru_ssres(:), Tosegment(:), Hru_segment(:), Obsin_segment(:)
      INTEGER, SAVE, ALLOCATABLE :: Lake_hru(:), Lake_hru_id(:) !not needed if no lakes
      REAL, SAVE :: Basin_area
      REAL, SAVE, ALLOCATABLE :: Hru_area(:), Hru_percent_imperv(:), Hru_elev(:), Hru_lat(:)
      REAL, SAVE, ALLOCATABLE :: Covden_sum(:), Covden_win(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_open(:), Dprst_area(:)
      END MODULE PRMS_BASIN

!***********************************************************************
!     Main basin routine
!***********************************************************************
      INTEGER FUNCTION basin()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basdecl, basinit
!***********************************************************************
      basin = 0

      IF ( Process(:4)=='decl' ) THEN
        basin = basdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        basin = basinit()
      ENDIF

      END FUNCTION basin

!***********************************************************************
!     basdecl - set up parameters
!   Declared Parameters
!     print_debug, hru_area, hru_percent_imperv, hru_type, hru_elev,
!     cov_type, hru_lat, dprst_frac_open, dprst_area, basin_area
!     hru_ssres, lake_hru, lake_hru_id, tosegment, hru_segment, obsin_segment
!***********************************************************************
      INTEGER FUNCTION basdecl()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Model, Nhru, Nlake, Dprst_flag, Nsegment, Stream_order_flag, Et_flag, Precip_flag, &
     &    Nssr, Soltab_flag, Lake_route_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
!***********************************************************************
      basdecl = 0

      Version_basin = '$Id: basin.f90 7231 2015-03-09 20:17:09Z rsregan $'
      CALL print_module(Version_basin, 'Basin Definition            ', 90)
      MODNAME = 'basin'

! Declared Variables
      ALLOCATE ( Hru_imperv(Nhru) )
      IF ( declvar(MODNAME, 'hru_imperv', 'nhru', Nhru, 'real', &
     &     'Area of HRU that is impervious', &
     &     'acres', Hru_imperv)/=0 ) CALL read_error(3, 'hru_imperv')

      ALLOCATE ( Hru_perv(Nhru) )
      IF ( declvar(MODNAME, 'hru_perv', 'nhru', Nhru, 'real', &
     &     'Area of HRU that is pervious', &
     &     'acres', Hru_perv)/=0 ) CALL read_error(3, 'hru_perv')

      ALLOCATE ( Hru_frac_imperv(Nhru) )
      IF ( declvar(MODNAME, 'hru_frac_imperv', 'nhru', Nhru, 'real', &
     &     'Fraction of HRU that is impervious', &
     &     'decimal fraction', Hru_frac_imperv)/=0 ) CALL read_error(3, 'hru_frac_imperv')

      ALLOCATE ( Hru_frac_perv(Nhru) )
      IF ( declvar(MODNAME, 'hru_frac_perv', 'nhru', Nhru, 'real', &
     &     'Fraction of HRU that is pervious', &
     &     'decimal fraction', Hru_frac_perv)/=0 ) CALL read_error(3, 'hru_frac_perv')

      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Hru_frac_dprst(Nhru) )
        IF ( declvar(MODNAME, 'hru_frac_dprst', 'nhru', Nhru, 'real', &
     &       'Fraction of HRU that has surface-depression storage', &
     &       'decimal fraction', Hru_frac_dprst)/=0 ) CALL read_error(3, 'hru_frac_dprst')

        ALLOCATE ( Dprst_area_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_max', 'nhru', Nhru, 'real', &
     &       'Aggregate sum of surface-depression storage areas of each HRU', &
     &       'acres', Dprst_area_max)/=0 ) CALL read_error(1, 'dprst_area_max')

        ALLOCATE ( Dprst_area_open_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_open_max', 'nhru', Nhru, 'real', &
     &       'Aggregate sum of open surface-depression storage areas of each HRU', &
     &       'acres', Dprst_area_open_max)/=0 ) CALL read_error(1, 'dprst_area_open_max')

        ALLOCATE ( Dprst_area_clos_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_clos_max', 'nhru', Nhru, 'real', &
     &       'Aggregate sum of closed surface-depression storage areas of each HRU', &
     &       'acres', Dprst_area_clos_max)/=0 ) CALL read_error(1, 'dprst_area_clos_max')

        ALLOCATE ( Dprst_area(Nhru) )
        IF ( declparam(MODNAME, 'dprst_area', 'nhru', 'real', &
     &       '0.0', '0.0', '1.0E9', &
     &       'Aggregate sum of surface-depression storage areas of each HRU', &
     &       'Aggregate sum of surface-depression storage areas of each HRU', &
     &       'acres')/=0 ) CALL read_error(1, 'dprst_area')

        ALLOCATE ( Dprst_frac_open(Nhru), Dprst_frac_clos(Nhru) )
        IF ( declparam(MODNAME, 'dprst_frac_open', 'nhru', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Fraction of open surface-depression storage area within'// &
     &       ' an HRU that can generate surface runoff as a function of storage volume', &
     &       'Fraction of open surface-depression storage area within'// &
     &       ' an HRU that can generate surface runoff as a function of storage volume', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_open')
      ENDIF

      ! local arrays
      ALLOCATE ( Hru_route_order(Nhru), Gwr_route_order(Nhru), Gwr_type(Nhru) )
      IF ( Et_flag==5 ) ALLOCATE ( Hru_elev_feet(Nhru) )
      IF ( Precip_flag==5 ) ALLOCATE ( Hru_elev_meters(Nhru) )

      ! Declared Parameters
      IF ( declparam(MODNAME, 'basin_area', 'one', 'real', &
     &     '0.0', '0.0', '1.0E9', &
     &     'Area of basin', 'Area of basin', &
     &     'acres')/=0 ) CALL read_error(1, 'basin_area')

      ALLOCATE ( Hru_area(Nhru) )
      IF ( declparam(MODNAME, 'hru_area', 'nhru', 'real', &
     &     '1.0', '0.01', '1.0E9', &
     &     'HRU area', 'Area of each HRU', &
     &     'acres')/=0 ) CALL read_error(1, 'hru_area')

      IF ( declparam(MODNAME, 'elev_units', 'one', 'integer', &
     &     '0', '0', '1', &
     &     'Elevation units flag', &
     &     'Flag to indicate the units of the elevation values (0=feet; 1=meters)', &
     &     'none')/=0 ) CALL read_error(1, 'elev_units')

      ALLOCATE ( Hru_elev(Nhru) )
      IF ( declparam(MODNAME, 'hru_elev', 'nhru', 'real', &
     &     '0.0', '-1000.0', '30000.0', &
     &     'HRU mean elevation', 'Mean elevation for each HRU', &
     &     'elev_units')/=0 ) CALL read_error(1, 'hru_elev')

      IF ( Soltab_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Hru_lat(Nhru) )
        IF ( declparam(MODNAME, 'hru_lat', 'nhru', 'real', &
     &       '40.0', '-90.0', '90.0', &
     &       'HRU latitude', 'Latitude of each HRU', &
     &       'angular degrees')/=0 ) CALL read_error(1, 'hru_lat')
      ENDIF

      ALLOCATE ( Hru_percent_imperv(Nhru) )
      IF ( declparam(MODNAME, 'hru_percent_imperv', 'nhru', 'real', &
     &     '0.0', '0.0', '0.999', &
     &     'HRU percent impervious', 'Fraction of each HRU area that is impervious', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'hru_percent_imperv')

      ALLOCATE ( Hru_type(Nhru) )
      IF ( declparam(MODNAME, 'hru_type', 'nhru', 'integer', &
     &     '1', '0', '3', &
     &     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake; 3=swale)', &
     &     'none')/=0 ) CALL read_error(1, 'hru_type')

      ALLOCATE ( Cov_type(Nhru) )
      IF ( declparam(MODNAME, 'cov_type', 'nhru', 'integer', &
     &     '3', '0', '4', &
     &     'Cover type designation for HRU', &
     &     'Vegetation cover type for each HRU (0=bare soil;'// &
     &     ' 1=grasses; 2=shrubs; 3=trees; 4=coniferous)', &
     &     'none')/=0 ) CALL read_error(1, 'cov_type')

      ALLOCATE ( Covden_sum(Nhru) )
      IF ( declparam(MODNAME, 'covden_sum', 'nhru', 'real', &
     &     '0.5', '0.0', '1.0', &
     &     'Summer vegetation cover density for major vegetation type', &
     &     'Summer vegetation cover density for the major vegetation type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'covden_sum')

      ALLOCATE ( Covden_win(Nhru) )
      IF ( declparam(MODNAME, 'covden_win', 'nhru', 'real', &
     &     '0.5', '0.0', '1.0', &
     &     'Winter vegetation cover density for major vegetation type', &
     &     'Winter vegetation cover density for the major vegetation type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'covden_win')

      ALLOCATE ( Ssres_area(Nssr), Hru_ssres(Nhru) )
      IF ( Nhru/=Nssr .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'hru_ssres', 'nhru', 'integer', &
     &       '1', 'bounded', 'nssr', &
     &       'Index of subsurface reservoir assigned to HRU', &
     &       'Index of subsurface reservoir receiving excess water from capillary reservoir', &
     &       'none')/=0 ) CALL read_error(1, 'hru_ssres')
      ENDIF

      IF ( Stream_order_flag==1 .OR. Model==99 ) THEN
        ! local arrays
        ALLOCATE ( Segment_order(Nsegment), Segment_up(Nsegment), Segment_hruarea(Nsegment) )
        ! parameters
        ALLOCATE ( Tosegment(Nsegment) )
        IF ( declparam(MODNAME, 'tosegment', 'nsegment', 'integer', &
     &       '0', 'bounded', 'nsegment', &
     &       'The index of the downstream segment', &
     &       'Index of downstream segment to which the segment'// &
     &       ' streamflow flows, for segments that do not flow to another segment enter 0', &
     &       'none')/=0 ) CALL read_error(1, 'tosegment')

        ALLOCATE ( Hru_segment(Nhru) )
        IF ( declparam(MODNAME, 'hru_segment', 'nhru', 'integer', &
     &       '0', 'bounded', 'nsegment', &
     &       'Segment index for HRU lateral inflows', &
     &       'Segment index to which an HRU contributes lateral flows'// &
     &       ' (surface runoff, interflow, and groundwater discharge)', &
     &       'none')/=0 ) CALL read_error(1, 'hru_segment')

        ALLOCATE ( Obsin_segment(Nsegment) )
        IF ( declparam(MODNAME, 'obsin_segment', 'nsegment', 'integer', &
     &       '0', 'bounded', 'nobs', &
     &       'Index of measured streamflow station that replaces inflow to a segment', &
     &       'Index of measured streamflow station that replaces inflow to a segment', &
     &       'none')/=0 ) CALL read_error(1, 'obsin_segment')
      ENDIF

      IF ( Nlake>0 .OR. Model==99 ) THEN
        ! Local array
        ALLOCATE ( Lake_area(Nlake) )
        ! parameters
        ALLOCATE ( Lake_hru_id(Nhru) )
        IF ( declparam(MODNAME, 'lake_hru_id', 'nhru', 'integer', &
     &       '0', 'bounded', 'nlake', &
     &       'Identification number of the lake associated with an HRU', &
     &       'Identification number of the lake associated with an HRU;'// &
     &       ' more than one HRU can be associated with each lake', &
     &       'none')/=0 ) CALL read_error(1, 'lake_hru_id')
        IF ( Lake_route_flag==1 .AND. Model/=0 ) THEN
          ALLOCATE ( Lake_hru(Nlake) )
          IF ( declparam(MODNAME, 'lake_hru', 'nlake', 'integer', &
     &         '0', 'bounded', 'nhru', &
     &         'Index of HRU for each lake HRU', &
     &         'Index of HRU for each lake HRU', &
     &         'none')/=0 ) CALL read_error(1, 'lake_hru')
        ENDIF
      ENDIF

      END FUNCTION basdecl

!**********************************************************************
!     basinit - check for validity of basin parameters
!               and compute reservoir areas
!**********************************************************************
      INTEGER FUNCTION basinit()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Nsegment, Cascade_flag, Dprst_flag, &
     &    Print_debug, Inputerror_flag, Model, PRMS_VERSION, Parameter_check_flag, Starttime, Endtime, &
     &    Stream_order_flag, Nssr, Ngw, Et_flag, Precip_flag, Lake_route_flag, Soltab_flag
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL write_outfile, checkint_param_limits
      INTRINSIC ABS
! Local Variables
      CHARACTER(LEN=68) :: buffer
      INTEGER :: i, j, k, test, lval
      INTEGER, ALLOCATABLE :: x_off(:)
      REAL :: harea, tmp
      DOUBLE PRECISION :: basin_imperv, basin_perv, basin_dprst
!**********************************************************************
      basinit = 0

      IF ( getparam(MODNAME, 'hru_area', Nhru, 'real', Hru_area)/=0 ) CALL read_error(2, 'hru_area')
      IF ( getparam(MODNAME, 'basin_area', 1, 'real', Basin_area)/=0 ) CALL read_error(2, 'basin_area')
      IF ( getparam(MODNAME, 'hru_elev', Nhru, 'real', Hru_elev)/=0 ) CALL read_error(2, 'hru_elev')
      IF ( Soltab_flag==1 ) THEN
        IF ( getparam(MODNAME, 'hru_lat', Nhru, 'real', Hru_lat)/=0 ) CALL read_error(2, 'hru_lat')
      ENDIF
      IF ( getparam(MODNAME, 'hru_type', Nhru, 'integer', Hru_type)/=0 ) CALL read_error(2, 'hru_type')
      IF ( getparam(MODNAME, 'cov_type', Nhru, 'integer', Cov_type)/=0 ) CALL read_error(2, 'cov_type')
      IF ( getparam(MODNAME, 'covden_sum', Nhru, 'real', Covden_sum)/=0 ) CALL read_error(2, 'covden_sum')
      IF ( getparam(MODNAME, 'covden_win', Nhru, 'real', Covden_win)/=0 ) CALL read_error(2, 'covden_win')
      IF ( getparam(MODNAME, 'elev_units', 1, 'integer', Elev_units)/=0 ) CALL read_error(2, 'elev_units')
      CALL checkint_param_limits(1, 'elev_units', Elev_units, 0, 1, Inputerror_flag)
      IF ( getparam(MODNAME, 'hru_percent_imperv', Nhru, 'real', Hru_percent_imperv)/=0 ) CALL read_error(2, 'hru_percent_imperv')

      IF ( Dprst_flag==1 ) THEN
        IF ( getparam(MODNAME, 'dprst_frac_open', Nhru, 'real', Dprst_frac_open)/=0 ) CALL read_error(2, 'dprst_frac_open')
        IF ( getparam(MODNAME, 'dprst_area', Nhru, 'real', Dprst_area)/=0 ) CALL read_error(2, 'dprst_area')
      ENDIF

      IF ( Stream_order_flag==1 ) THEN
        IF ( getparam(MODNAME, 'tosegment', Nsegment, 'integer', Tosegment)/=0 ) CALL read_error(2, 'tosegment')
        IF ( getparam(MODNAME, 'hru_segment', Nhru, 'integer', Hru_segment)/=0 ) CALL read_error(2, 'hru_segment')
        IF ( getparam(MODNAME, 'obsin_segment', Nsegment, 'integer', Obsin_segment)/=0 ) CALL read_error(2, 'obsin_segment')
      ENDIF

      IF ( Nlake>0 ) THEN
        IF ( getparam(MODNAME, 'lake_hru_id', Nhru, 'integer', Lake_hru_id)/=0 ) CALL read_error(1, 'lake_hru_id')
        Lake_area = 0.0D0
        IF ( Lake_route_flag==1 .AND. Model/=0 ) THEN
          IF ( getparam(MODNAME, 'lake_hru', Nlake, 'real', Lake_hru)/=0 ) CALL read_error(2, 'lake_hru')
        ENDIF
      ENDIF

      Numlake_hrus = 0
      Totarea = 0.0D0
      Land_area = 0.0D0
      Water_area = 0.0D0
      Active_area = 0.0D0
      Basin_lat = 0.0D0
      Hru_route_order = 0
      j = 0
      DO i = 1, Nhru
        harea = Hru_area(i)
        Totarea = Totarea + harea
        IF ( harea<SMALLPARAM ) THEN
          PRINT *, 'ERROR, hru_area <=', SMALLPARAM, ' for HRU:', i, ', value:', Hru_area(i)
          Inputerror_flag = 1
        ENDIF

        IF ( Hru_type(i)==0 .OR. Hru_type(i)==2 ) THEN ! inactive or lakes
          Hru_frac_imperv(i) = 0.0
          Hru_frac_perv(i) = 1.0
          Hru_imperv(i) = 0.0
          Hru_perv(i) = harea
          IF ( Dprst_flag==1 ) THEN
            Dprst_frac_clos(i) = 0.0
            Dprst_area_open_max(i) = 0.0
            Dprst_area_clos_max(i) = 0.0
            Hru_frac_dprst(i) = 0.0
            Dprst_area_max(i) = Dprst_area(i)
          ENDIF
          IF ( Hru_type(i)==0 ) CYCLE
          Water_area = Water_area + harea
          Numlake_hrus = Numlake_hrus + 1
          IF ( Nlake<1 ) THEN
            PRINT *, 'ERROR, invalid hru_type value = 2 for HRU:', i, ' and nlake value = 0'
            Inputerror_flag = 1
          ELSEIF ( Lake_hru_id(i)>Nlake ) THEN
            PRINT *, 'ERROR, lake_hru_id > nlake for lake HRU:', i, ' lake_hru_id value:', Lake_hru_id(i), ' nlake value:', Nlake
            Inputerror_flag = 1
          ELSEIF ( Lake_hru_id(i)==0 ) THEN
            PRINT *, 'ERROR, lake_hru_id value = 0 for lake HRU:', i
            Inputerror_flag = 1
          ELSE
            Lake_area(Lake_hru_id(i)) = Lake_area(Lake_hru_id(i)) + harea
          ENDIF
        ELSEIF ( Hru_type(i)==1 .OR. Hru_type(i)==3 ) THEN ! swale or land
          Land_area = Land_area + harea
        ELSE
          CALL checkint_param_limits(i, 'hru_type', Hru_type(i), 0, 3, Inputerror_flag)
        ENDIF

        IF ( Stream_order_flag==1 .AND. Cascade_flag==0 ) THEN
          IF ( Hru_segment(i)==0 ) THEN
            IF ( Print_debug>-1 ) PRINT *, 'WARNING, HRU:', i, ' is not associated with a segment'
          ENDIF
        ENDIF

        IF ( Soltab_flag==1 ) Basin_lat = Basin_lat + Hru_lat(i)*harea
        IF ( Elev_units==0 ) THEN
          IF ( Et_flag==5 ) Hru_elev_feet(i) = Hru_elev(i)
          IF ( Precip_flag==5 ) Hru_elev_meters(i) = Hru_elev(i)*FEET2METERS
        ELSE
          IF ( Precip_flag==5 ) Hru_elev_meters(i) = Hru_elev(i)
          IF ( Et_flag==5 ) Hru_elev_feet(i) = Hru_elev(i)*METERS2FEET
        ENDIF
        j = j + 1
        Hru_route_order(j) = i

        IF ( Hru_type(i)==2 ) CYCLE ! lake

        IF ( Hru_percent_imperv(i)>0.999 ) THEN
          PRINT *, 'ERROR, hru_percent_imperv > 0.999 for HRU:', i, ' value:', Hru_percent_imperv(i)
          Inputerror_flag = 1
        ELSEIF ( Hru_percent_imperv(i)<SMALLPARAM ) THEN
          IF ( ABS(Hru_percent_imperv(i))>0.0 ) THEN
            IF ( Print_debug>-1 ) PRINT 9001, 'hru_percent_imperv', SMALLPARAM, i, Hru_percent_imperv(i)
            Hru_percent_imperv(i) = 0.0
          ENDIF
        ENDIF
        Hru_frac_imperv(i) = Hru_percent_imperv(i)
        Hru_imperv(i) = Hru_frac_imperv(i)*harea
        Hru_perv(i) = harea - Hru_imperv(i)

        IF ( Dprst_flag==1 ) THEN
          IF ( Dprst_area(i)<SMALLPARAM ) THEN
            IF ( ABS(Dprst_area(i))>0.0 ) THEN
              PRINT 9001, 'dprst_area', SMALLPARAM, i, Dprst_area(i)
              Dprst_area(i) = 0.0
            ENDIF
          ELSEIF ( Dprst_area(i)>harea ) THEN
            PRINT *, 'ERROR, dprst_area > hru_area for HRU:', i, ', value:', Dprst_area(i)
            PRINT *, '       hru_area:', harea
            Inputerror_flag = 1
          ELSEIF ( Dprst_area(i)>0.999*harea ) THEN
            PRINT *, 'ERROR, dprst_area > 0.999*hru_area for HRU:', i, ', value:', Dprst_area(i)
            PRINT *, '       hru_area:', harea, '; fraction', 0.999*harea
            Inputerror_flag = 1
          ENDIF
          Dprst_area_max(i) = Dprst_area(i)
          Hru_frac_dprst(i) = Dprst_area_max(i)/harea
          ! ignore very small depression storage
          IF ( Hru_frac_dprst(i)<SMALLPARAM ) THEN
            IF ( ABS(Hru_frac_dprst(i))>0.0 ) PRINT 9001, 'dprst fraction', SMALLPARAM, i, Hru_frac_dprst(i)
            Hru_frac_dprst(i) = 0.0
            Dprst_area_max(i) = 0.0
            Dprst_frac_open(i) = 1.0
          ENDIF
          Dprst_area_open_max(i) = Dprst_area_max(i)*Dprst_frac_open(i)
          Dprst_frac_clos(i) = 1.0 - Dprst_frac_open(i)
          ! accounting for possible round-off error
          IF ( Dprst_frac_clos(i)<SMALLPARAM ) THEN
            Dprst_frac_clos(i) = 0.0
            Dprst_area_open_max(i) = Dprst_area_max(i)
            Dprst_frac_open(i) = 1.0
            Dprst_area_clos_max(i) = 0.0
          ELSE
            Dprst_area_clos_max(i) = Dprst_area_max(i) - Dprst_area_open_max(i)
          ENDIF
          Dprst_area_max(i) = Dprst_area_open_max(i) + Dprst_area_clos_max(i)
          Hru_perv(i) = Hru_perv(i) - Dprst_area_max(i)
          tmp = Hru_perv(i)/harea
          ! sanity check
          IF ( tmp<0.001) THEN
            PRINT *, 'ERROR, hru_imperv+dprst_area > 0.999*hru_area for HRU:', i
            PRINT *, '       hru_area:', Hru_area(i), ' hru_imperv:', Hru_imperv(i), ' dprst_area:', Dprst_area_max(i)
            PRINT *, '       (hru_imperv+dprst_area)/hru_area =', (Hru_imperv(i)+Dprst_area_max(i))/Hru_area(i)
            Inputerror_flag = 1
          ENDIF
          IF ( Hru_frac_imperv(i)+Hru_frac_dprst(i)>0.999 ) THEN
            tmp = 0.999 - Hru_frac_imperv(i)
            Dprst_area_max(i) = tmp*harea
            Hru_frac_dprst(i) = tmp
            Dprst_area_open_max(i) = Dprst_area_max(i)*Dprst_frac_open(i)
            Dprst_area_clos_max(i) = Dprst_area_max(i) - Dprst_area_open_max(i)
          ENDIF
          Dprst_frac_open(i) = Dprst_area_max(i)/Dprst_area_open_max(i)
          Hru_perv(i) = harea - Hru_imperv(i) - Dprst_area_max(i)
        ENDIF

        Hru_frac_perv(i) = Hru_perv(i)/harea
        IF ( Hru_frac_perv(i)>0.9999 ) THEN
          Hru_frac_perv(i) = 1.0
          Hru_perv(i) = harea
          Hru_imperv(i) = 0.0
          Hru_frac_imperv(i) = 0.0
          IF ( Dprst_flag==1 ) THEN
            Dprst_frac_clos(i) = 0.0
            Dprst_area_max(i) = 0.0
            Dprst_area_open_max(i) = 0.0
            Dprst_frac_open(i) = 1.0
            Dprst_area_clos_max(i) = 0.0
            Hru_frac_dprst(i) = 0.0
          ENDIF
        ENDIF

      ENDDO

      Active_hrus = j
      Active_area = Land_area + Water_area
      IF ( Active_area<SMALLPARAM ) THEN
        PRINT *, 'ERROR, active area <', SMALLPARAM, ' value: ', Active_area
        Inputerror_flag = 1
      ENDIF

      Weir_gate_flag = 0
! ????????? need to fix for lakes with multiple HRUs and PRMS lake routing ????????
      IF ( Model==99 ) Numlake_hrus = Nlake
      IF ( Nlake<1 .AND. Numlake_hrus>0 ) THEN
        PRINT *, 'ERROR, dimension nlake=0 and number of specified lake HRUs equals', Numlake_hrus
        Inputerror_flag = 1
      ENDIF
      IF ( Nlake>0 ) THEN
        DO i = 1, Active_hrus
          j = Hru_route_order(i)
          IF ( Lake_hru_id(j)>0 .AND. Hru_type(j)/=2 ) THEN
            PRINT *, 'ERROR, HRU:', j, ' specifed to be a lake by lake_hru_id but hru_type not equal 2'
            Inputerror_flag = 1
          ENDIF
        ENDDO
        DO i = 1, Nlake
          IF ( Lake_area(i)<DNEARZERO ) THEN
            PRINT *, 'ERROR, Lake:', i, ' has 0 area, thus no value of lake_hru_id is associated with the lake'
            Inputerror_flag = 1
          ENDIF
        ENDDO
        IF ( Lake_route_flag==1 .AND. Model/=0 ) THEN
          IF ( Numlake_hrus/=Nlake ) THEN
            PRINT *, 'ERROR, number of lake HRUs specified in hru_type'
            PRINT *, 'does not equal dimension nlake:', Nlake, ', number of lake HRUs:', Numlake_hrus
            PRINT *, 'For PRMS lake routing each lake must be a single HRU'
            Inputerror_flag = 1
          ENDIF
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
      ENDIF

      IF ( Stream_order_flag==1 ) THEN
        ! Begin the loops for ordering segments
        Segment_hruarea = 0.0
        DO j = 1, Nsegment
          IF ( Tosegment(j)>Nsegment ) THEN
            PRINT *, 'ERROR, tosegment value (', Tosegment(j), ') > nsegment for segment:', j
            Inputerror_flag = 1
          ELSEIF ( Tosegment(j)<0 ) THEN
            PRINT *, 'ERROR, tosegment value (', Tosegment(j), ') < 0 for segment:', j
            Inputerror_flag = 1
          ELSEIF ( Tosegment(j)==j ) THEN
            PRINT *, 'ERROR, tosegment value (', Tosegment(j), ') equals itself for segment:', j
            Inputerror_flag = 1
          ELSE
            DO k = 1, Active_hrus
              i = Hru_route_order(k)
              IF ( Hru_segment(i)==j ) Segment_hruarea(j) = Segment_hruarea(j) + Hru_area(i)
            ENDDO
          ENDIF
        ENDDO

        Noarea_flag = 0
        ! load segment_up with last stream segment that flows into a segment
        Segment_up = 0
        DO i = 1, Nsegment
          IF ( Segment_hruarea(i)<NEARZERO ) THEN
            WRITE ( buffer, '(I10)' ) i
            CALL write_outfile(' No HRUs are associated with segment:'//buffer(:10))
            Noarea_flag = 1
          ENDIF
          IF ( Tosegment(i)>0 ) Segment_up(Tosegment(i)) = i
        ENDDO
        IF ( Parameter_check_flag>0 ) THEN
          DO i = 1, Nsegment
            IF ( Segment_up(i)==0 .AND. Tosegment(i)==0 ) THEN
              IF ( Print_debug>-1 ) PRINT *, 'WARNING, no other segment flows into segment:',  &
     &                                       i, ' and tosegment=0'
            ENDIF
          ENDDO
        ENDIF

        ! Begin the loops for ordering segments
        ALLOCATE ( x_off(Nsegment) )
        x_off = 0
        Segment_order = 0
        lval = 0
        DO WHILE ( lval<Nsegment )
          DO i = 1, Nsegment
!     If segment "i" has not been crossed out consider it, else continue
            IF ( x_off(i)==1 ) CYCLE
!     Test to see if segment "i" is the to segment from other segments
            test = 1
            DO j = 1, Nsegment
              IF ( Tosegment(j)==i ) THEN
!     If segment "i" is a to segment, test to see if the originating
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
              Segment_order(lval) = i
              x_off(i) = 1
            ENDIF
          ENDDO
        ENDDO
!        IF ( Print_debug>-1 ) THEN
!          PRINT *, 'Stream Network Routing Order:'
!          PRINT '(10I5)', Segment_order
!          PRINT *, 'tosegment:'
!          PRINT '(10I5)', Tosegment
!        ENDIF
        DEALLOCATE ( x_off )
      ENDIF

      IF ( Nssr==Nhru ) THEN
        DO i = 1, Nhru
          Ssres_area(i) = Hru_area(i)
          Hru_ssres(i) = i
        ENDDO
      ELSE
        IF ( getparam(MODNAME, 'hru_ssres', Nhru, 'integer', Hru_ssres) &
     &       /=0 ) CALL read_error(2, 'hru_ssres')
        Ssres_area = 0.0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          j = Hru_ssres(i)
          Ssres_area(j) = Ssres_area(j) + Hru_area(i)
        ENDDO
      ENDIF

      IF ( Ngw==Nhru ) THEN
        Active_gwrs = Active_hrus
        Gwr_route_order = Hru_route_order
        Gwr_type = Hru_type
        IF ( Model/=0 ) THEN
          DO j = 1, Active_gwrs
            i = Gwr_route_order(j)
            IF ( Gwr_type(i)==3 ) THEN
              IF ( Print_debug>-1 ) PRINT *, 'WARNING, GWRs cannot be swales for GWR:', i, ' set to 1'
              Gwr_type(i) = 1
            ENDIF
          ENDDO
        ENDIF
      ELSE
        DO i = 1, Ngw
          Gwr_route_order(i) = i
        ENDDO
        Active_gwrs = Ngw
        Gwr_type = 1
      ENDIF

      Basin_area_inv = 1.0D0/Active_area
      Basin_lat = Basin_lat*Basin_area_inv
      ! used in solrad modules to winter/summer radiation adjustment
      IF ( Basin_lat>0.0D0 ) THEN
        Hemisphere = 0 ! Northern
      ELSE
        Hemisphere = 1 ! Southern
      ENDIF

      Hru_order_flag = 0
      IF ( Active_hrus==Nhru .AND. Cascade_flag==0 ) Hru_order_flag = 1

      Dprst_clos_flag = 0
      basin_perv = 0.0D0
      basin_imperv = 0.0D0
      basin_dprst = 0.0D0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        basin_perv = basin_perv + Hru_perv(i)
        basin_imperv = basin_imperv + Hru_imperv(i)
        IF ( Dprst_flag==1 ) THEN
          basin_dprst = basin_dprst + Dprst_area_max(i)
          IF ( Dprst_area_clos_max(i)>NEARZERO ) Dprst_clos_flag = 1
        ENDIF
      ENDDO
      basin_perv = basin_perv*Basin_area_inv
      basin_imperv = basin_imperv*Basin_area_inv
      IF ( Dprst_flag==1 ) basin_dprst = basin_dprst*Basin_area_inv

      IF ( Print_debug==2 ) THEN
        PRINT *, ' HRU     Area'
        PRINT ('(I7, F14.5)'), (i, Hru_area(i), i=1, Nhru)
        PRINT *, 'Sum of HRU areas      = ', totarea
        PRINT *, 'Active basin area     = ', active_area
        PRINT *, 'Fraction impervious   = ', basin_imperv
        PRINT *, 'Fraction pervious     = ', basin_perv
        IF ( Dprst_flag==1 ) PRINT *, 'Fraction depression storage =', basin_dprst
        PRINT *, ' '
      ENDIF

!     print out start and end times
      CALL write_outfile('  ')
      CALL write_outfile(' Surface Water and Energy Budgets Simulated by '//PRMS_VERSION)
      WRITE (buffer, 9002) ' Start time: ', Starttime
      CALL write_outfile(buffer(:32))
      WRITE (buffer, 9002) ' End time:   ', Endtime
      CALL write_outfile(buffer(:32))
      WRITE (buffer, 9003) ' Sum of HRU areas:', totarea, '; Active basin area:', active_area
      CALL write_outfile(buffer(:62))
      WRITE (buffer, 9004) ' Fraction impervious:', basin_imperv, '; Fraction pervious:   ', basin_perv
      CALL write_outfile(buffer(:62))
      IF ( Dprst_flag==1 ) THEN
        WRITE (buffer, 9004) ' Fraction depression storage:', basin_dprst
        CALL write_outfile(buffer(:45))
      ENDIF
      CALL write_outfile(' ')

 9001 FORMAT (/, 'WARNING, ABS(', A, ') > 0.0 and <', F7.4, ' for HRU:', I7, /, 9X, 'value:', F10.7, ' set to 0.0')
 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
 9003 FORMAT (2(A,F12.2))
 9004 FORMAT (2(A,F9.4))

      END FUNCTION basinit
