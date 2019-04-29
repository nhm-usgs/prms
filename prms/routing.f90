!***********************************************************************
! Defines stream and lake routing parameters and variables
!***********************************************************************
      MODULE PRMS_ROUTING
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=7), SAVE :: MODNAME
      DOUBLE PRECISION, SAVE :: Cfs2acft
      DOUBLE PRECISION, SAVE :: Segment_area
      CHARACTER(LEN=80), SAVE :: Version_routing
      !CHARACTER(LEN=32), SAVE :: Outfmt
!   Declared Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seginc_ssflow(:), Seginc_sroff(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seginc_gwflow(:), Seginc_swrad(:), Seginc_potet(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_outflow(:), Seg_ssflow(:), Seg_sroff(:), Seg_gwflow(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Segment_type(:)
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
        IF ( Init_vars_from_file==1 ) CALL routing_restart(1)
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
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Model, Strmflow_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
!***********************************************************************
      routingdecl = 0

      Version_routing = 'routing.f90 2016-03-25 21:00:49Z'
      CALL print_module(Version_routing, 'Routing Initialization      ', 90)
      MODNAME = 'routing'

! Declared Variables
      ALLOCATE ( Hru_outflow(Nhru) )
      IF ( declvar(MODNAME, 'hru_outflow', 'nhru', Nhru, 'double', &
     &     'Total flow leaving each HRU', &
     &     'cfs', Hru_outflow)/=0 ) CALL read_error(3, 'hru_outflow')

      IF ( Strmflow_flag==2 .OR. Model==99 ) THEN
        ALLOCATE ( Segment_type(Nsegment) )
        IF ( declparam(MODNAME, 'segment_type', 'nsegment', 'integer', &
     &       '0', '0', '3', &
     &       'Segment type', &
     &       'Segment type (0=segment; 1=diversion; 2=lake; 3=replace inflow)', &
     &       'none')/=0 ) CALL read_error(1, 'segment_type')
      ENDIF


      ALLOCATE ( Seginc_potet(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_potet', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average potential ET for each segment'// &
     &     ' from HRUs contributing flow to the segment', &
     &     'inches', Seginc_potet)/=0 ) CALL read_error(3, 'seginc_potet')

      ALLOCATE ( Seginc_swrad(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_swrad', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average solar radiation for each segment'// &
     &     ' from HRUs contributing flow to the segment', &
     &     'Langleys', Seginc_swrad)/=0 ) CALL read_error(3, 'seginc_swrad')

      ALLOCATE ( Seginc_ssflow(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_ssflow', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average interflow for each segment from'// &
     &     ' HRUs contributing flow to the segment', &
     &     'cfs', Seginc_ssflow)/=0 ) CALL read_error(3, 'seginc_ssflow')

      ALLOCATE ( Seginc_gwflow(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_gwflow', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average groundwater discharge for each'// &
     &     ' segment from HRUs contributing flow to the segment', &
     &     'cfs', Seginc_gwflow)/=0 ) CALL read_error(3, 'seginc_gwflow')

      ALLOCATE ( Seginc_sroff(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_sroff', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average surface runoff for each'// &
     &     ' segment from HRUs contributing flow to the segment', &
     &     'cfs', Seginc_sroff)/=0 ) CALL read_error(3, 'seginc_sroff')

      ALLOCATE ( Seg_ssflow(Nsegment) )
      IF ( declvar(MODNAME, 'seg_ssflow', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average interflow for each segment from'// &
     &     ' HRUs contributing flow to the segment and upstream HRUs', &
     &     'inches', Seg_ssflow)/=0 ) CALL read_error(3, 'seg_ssflow')

      ALLOCATE ( Seg_gwflow(Nsegment) )
      IF ( declvar(MODNAME, 'seg_gwflow', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average groundwater discharge for each segment from'// &
     &     ' HRUs contributing flow to the segment and upstream HRUs', &
     &     'inches', Seg_gwflow)/=0 ) CALL read_error(3, 'seg_gwflow')

      ALLOCATE ( Seg_sroff(Nsegment) )
      IF ( declvar(MODNAME, 'seg_sroff', 'nsegment', Nsegment, 'double', &
     &     'Area-weighted average surface runoff for each segment from'// &
     &     ' HRUs contributing flow to the segment and upstream HRUs', &
     &     'inches', Seg_sroff)/=0 ) CALL read_error(3, 'seg_sroff')

      END FUNCTION routingdecl

!**********************************************************************
!     routinginit - check for validity of parameters
!**********************************************************************
      INTEGER FUNCTION routinginit()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nsegment, Init_vars_from_file, Strmflow_flag
      USE PRMS_SET_TIME, ONLY: Timestep_seconds
      USE PRMS_BASIN, ONLY: FT2_PER_ACRE, Segment_hruarea
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: read_error
! Local Variable
      INTEGER :: i
!**********************************************************************
      routinginit = 0

      IF ( Init_vars_from_file==0 ) THEN
        Seginc_potet = 0.0D0
        Seginc_gwflow = 0.0D0
        Seginc_ssflow = 0.0D0
        Seginc_sroff = 0.0D0
        Seginc_swrad = 0.0D0
        Seg_gwflow = 0.0D0
        Seg_ssflow = 0.0D0
        Seg_sroff = 0.0D0
        Hru_outflow = 0.0D0
      ENDIF

      Cfs2acft = Timestep_seconds/FT2_PER_ACRE

      IF ( Strmflow_flag==2 ) THEN
        IF ( getparam(MODNAME, 'segment_type', Nsegment, 'integer', Segment_type)/=0 ) CALL read_error(2, 'segment_type')
      ENDIF

      Segment_area = 0.0D0
      DO i = 1, Nsegment
        Segment_area = Segment_area + Segment_hruarea(i)
      ENDDO
!      IF ( Active_area/=Segment_area ) PRINT *, 'Not all area in model domain included with segments, basin area =', &
!     &                                          Active_area, ' segment area = ', Segment_area

      END FUNCTION routinginit

!***********************************************************************
!     route_run - Computes segment flow states and fluxes
!***********************************************************************
      INTEGER FUNCTION route_run()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nsegment, Cascade_flag
      USE PRMS_BASIN, ONLY: Hru_area, Hru_route_order, Active_hrus, NEARZERO, &
     &    Hru_segment, Segment_hruarea, Tosegment, Noarea_flag, FT2_PER_ACRE
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Potet
      USE PRMS_SET_TIME, ONLY: Timestep_seconds, Cfs_conv
      USE PRMS_FLOWVARS, ONLY: Ssres_flow, Sroff, Seg_lateral_inflow !, Seg_outflow
      USE PRMS_GWFLOW, ONLY: Gwres_flow
      USE PRMS_SRUNOFF, ONLY: Strm_seg_in
      IMPLICIT NONE
      INTRINSIC DBLE
! Local Variables
      INTEGER :: i, j, jj
      DOUBLE PRECISION :: tocfs
!***********************************************************************
      route_run = 0

      Cfs2acft = Timestep_seconds/FT2_PER_ACRE

      ! add hru_ppt, hru_actet
      Seginc_gwflow = 0.0D0
      Seginc_ssflow = 0.0D0
      Seginc_sroff = 0.0D0
      Seginc_swrad = 0.0D0
      Seginc_potet = 0.0D0
      Seg_gwflow = 0.0D0
      Seg_sroff = 0.0D0
      Seg_ssflow = 0.0D0
      IF ( Cascade_flag==0 ) THEN
        Seg_lateral_inflow = 0.0D0
      ELSE
        Seg_lateral_inflow = Strm_seg_in
      ENDIF

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        tocfs = DBLE( Hru_area(j) )*Cfs_conv
        Hru_outflow(j) = DBLE( (Sroff(j) + Ssres_flow(j) + Gwres_flow(j)) )*tocfs
        i = Hru_segment(j)
        IF ( i>0 ) THEN
          Seg_gwflow(i) = Seg_gwflow(i) + Gwres_flow(j)
          Seg_sroff(i) = Seg_sroff(i) + Sroff(j)
          Seg_ssflow(i) = Seg_ssflow(i) + Ssres_flow(j)
          IF ( Cascade_flag==0 ) Seg_lateral_inflow(i) = Seg_lateral_inflow(i) + Hru_outflow(j)
          Seginc_sroff(i) = Seginc_sroff(i) + DBLE( Sroff(j) )*tocfs
          Seginc_ssflow(i) = Seginc_ssflow(i) + DBLE( Ssres_flow(j) )*tocfs
          Seginc_gwflow(i) = Seginc_gwflow(i) + DBLE( Gwres_flow(j) )*tocfs
          Seginc_swrad(i) = Seginc_swrad(i) + DBLE( Swrad(j)*Hru_area(j) )
          Seginc_potet(i) = Seginc_potet(i) + DBLE( Potet(j)*Hru_area(j) )
        ENDIF
      ENDDO

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
          IF ( Segment_hruarea(i)>NEARZERO ) THEN
            Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
            Seginc_potet(i) = Seginc_potet(i)/Segment_hruarea(i)
          ELSEIF ( Tosegment(i)>0 ) THEN
            Seginc_swrad(i) = Seginc_swrad(Tosegment(i))
            Seginc_potet(i) = Seginc_potet(Tosegment(i))
          ELSEIF ( i>1 ) THEN ! set to next segment id
            Seginc_swrad(i) = Seginc_swrad(i-1)
            Seginc_potet(i) = Seginc_potet(i-1)
          ELSE ! assume at least 2 segments
            Seginc_swrad(i) = Seginc_swrad(i+1)
            Seginc_potet(i) = Seginc_potet(i+1)
          ENDIF
        ENDDO
      ENDIF

      END FUNCTION route_run

!***********************************************************************
!     routing_restart - write or read restart file
!***********************************************************************
      SUBROUTINE routing_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
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
        WRITE ( Restart_outunit ) Cfs2acft
        WRITE ( Restart_outunit ) Seginc_ssflow
        WRITE ( Restart_outunit ) Seginc_sroff
        WRITE ( Restart_outunit ) Seginc_gwflow
        WRITE ( Restart_outunit ) Seginc_swrad
        WRITE ( Restart_outunit ) Seginc_potet
        WRITE ( Restart_outunit ) Hru_outflow
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Cfs2acft
        READ ( Restart_inunit ) Seginc_ssflow
        READ ( Restart_inunit ) Seginc_sroff
        READ ( Restart_inunit ) Seginc_gwflow
        READ ( Restart_inunit ) Seginc_swrad
        READ ( Restart_inunit ) Seginc_potet
        READ ( Restart_inunit ) Hru_outflow
      ENDIF
      END SUBROUTINE routing_restart
