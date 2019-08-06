!***********************************************************************
! Defines stream and lake routing parameters and variables
!***********************************************************************
      MODULE PRMS_MIZUROUTE
      IMPLICIT NONE
!   Local Variables
! index for printing (set to negative to supress printing
      integer,parameter     :: ixPrint = -9999     ! index for printing
! useful constants
      logical,parameter     :: doKWTroute=.True.  !.True. if switch off will do KWT
      double precision,parameter         :: verySmall=tiny(1.0D0)  ! a very small number
! general guff
      integer,parameter     :: strLen=256          ! length of character string
      integer               :: ierr                ! error code
      character(len=strLen)      :: cmessage            ! error message of downwind routine
      integer               :: iTime               ! loop through time
      character(len=strLen)      :: str                 ! miscellaneous string
! define stream segment information
      integer,target        :: nSeg                ! number of all the stream segments
      integer,pointer       :: nSegRoute           ! number of stream segments to be routed
      integer               :: nUpstream           ! number of reaches upstream of each stream segment
      integer               :: iSeg                ! index of stream segment
      integer               :: jSeg                ! index of stream segment
      integer               :: iSegOut             ! index of outlet stream segment
      integer               :: iSelect(1)          ! index of desired stream segment (iSegOut) from the minloc operation
      integer               :: iSegDesire          ! index of desired stream segment -- de-vectorized version of iSelect(1)
      integer               :: iUps                ! index of upstream stream segment added by NM
      integer               :: iStart              ! start index of the ragged array
      integer,dimension(1)  :: iDesire             ! index of stream segment with maximum upstream area (vector)
      integer                :: ixDesire            ! index of stream segment with maximum upstream area (scalar)
! define stream network information
      integer,allocatable    :: REACHIDGV(:)
      integer,allocatable    :: RCHIXLIST(:)
      integer                :: nTotal              ! total number of upstream segments for all stream segments
      integer                :: iRchStart
      integer                :: iRchStart1
      integer,target         :: nRchCount
      integer                :: nRchCount1
      integer                :: iUpRchStart
      integer                :: nUpRchCount
      integer,allocatable    :: upStrmRchList(:)
! define metadata from model output file
      integer                :: iRch                ! index in reach structures
! interpolate simulated runoff data to the basins
      integer               :: ibas                ! index of the basins
      integer               :: iHRU                ! index of the HRUs associated to the basin
      integer               :: nDrain              ! number of HRUs that drain into a given stream segment
      integer               :: ix                  ! index of the HRU assigned to a given basin
! route delaied runoff through river network with St.Venant UH
      integer               :: nUH_DATA_MAX        ! maximum number of elements in the UH data among all the upstreamfs for a segment
! compute total instantaneous runoff upstream of each reach
      integer,allocatable   :: iUpstream(:)        ! indices for all reaches upstream
      double precision,allocatable       :: qUpstream(:)        ! streamflow for all reaches upstream
! route kinematic waves through the river network
      integer, parameter    :: nens=1              ! number of ensemble members
      integer               :: iens                ! index of ensemble member
      double precision, save :: T0               ! start of the time step (seconds)
      double precision      :: T1               ! end of the time step (seconds)
      integer               :: LAKEFLAG            ! >0 if processing lakes
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Outflow_ts(:)
      CHARACTER(LEN=9), SAVE :: MODNAME
      END MODULE PRMS_MIZUROUTE

!***********************************************************************
!     Main mizuroute routine
!***********************************************************************
      INTEGER FUNCTION mizuroute()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: mizuroute_decl, mizuroute_init, mizuroute_run
      EXTERNAL :: mizuroute_restart
!***********************************************************************
      mizuroute = 0

      IF ( Process(:3)=='run' ) THEN
        mizuroute  = mizuroute_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        mizuroute  = mizuroute_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file>0 ) CALL mizuroute_restart(1)
        mizuroute = mizuroute_init()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL mizuroute_restart(0)
      ENDIF

      END FUNCTION mizuroute

!***********************************************************************
!     mizuroute_decl - Declare parameters and variables and allocate arrays
!   Declared Parameters
!     tosegment, hru_segment, obsin_segment, K_coef, x_coef
!***********************************************************************
      INTEGER FUNCTION mizuroute_decl()
      USE PRMS_MIZUROUTE
      USE PRMS_MODULE, ONLY: Nsegment
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_mizuroute
!***********************************************************************
      mizuroute_decl = 0

      Version_mizuroute = 'mizuroute.f90 2017-10-06 11:04:00Z'
      CALL print_module(Version_mizuroute, 'Streamflow Routing          ', 90)
      MODNAME = 'mizuroute'

      ALLOCATE ( Outflow_ts(Nsegment) )

      END FUNCTION mizuroute_decl

!***********************************************************************
!    mizuroute_init - Get and check parameter values and initialize variables
!***********************************************************************
      INTEGER FUNCTION mizuroute_init()
      USE PRMS_MIZUROUTE
      USE PRMS_MODULE, ONLY: Nsegment, Init_vars_from_file
      USE PRMS_BASIN, ONLY: Basin_area_inv, FT2_PER_ACRE, FEET2METERS
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_ROUTING, ONLY: Basin_segment_storage, Tosegment, Segment_hruarea, &
     &    Segment_order, Obsin_segment, Obsout_segment, Seg_length, Seg_slope
! mizuroute specific modules
      USE nrtype                                    ! variable types, etc.
      USE reachparam                                ! reach parameters
      USE reachstate                                ! reach states
      USE reach_flux                                ! fluxes in each reach
      USE nrutil,only:arth                          ! use to build vectors with regular increments
      USE lake_param                                ! lake parameters
      USE lakes_flux                                ! fluxes in each lake
! ****
      USE kwt_route,only:reachorder              ! define the processing order for the stream segments
      IMPLICIT NONE
! Functions
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparam
! Local Variables
      INTEGER :: i, j, k, jj, toseg, iorder, reachStart(Nsegment), reachCount(Nsegment)
      INTEGER :: upReachStart(Nsegment), upReachCount(Nsegment),reachListMat(Nsegment,Nsegment)
      INTEGER :: reachList(Nsegment*Nsegment),upReachIndex(Nsegment*Nsegment), seg_id(Nsegment)
      INTEGER :: upReachIndMat(Nsegment,Nsegment), ilake
      DOUBLE PRECISION :: totalArea(Nsegment)
!***********************************************************************
      mizuroute_init = 0

      IF ( Init_vars_from_file==0 ) THEN
        Outflow_ts = 0.0D0
      ENDIF

      !Seg_outflow will have been initialized to Segment_flow_init in PRMS_ROUTING
      Basin_segment_storage = 0.0D0
      DO i = 1, Nsegment
        Basin_segment_storage = Basin_segment_storage + Seg_outflow(i)
      ENDDO
      Basin_segment_storage = Basin_segment_storage*Basin_area_inv/Cfs_conv

      DO i = 1, Nsegment
        IF ( Tosegment(i)==0 ) iSegOut = i
        seg_id(i) = i
      ENDDO

      T0 = 0.D0
      nSeg = Nsegment

! Calculate network topology information...
      reachStart = 0
      reachCount = 1
      upReachStart = -9999
      upReachCount = 0
      reachListMat = 0
      reachList = 0
      upReachIndMat = 0
      upReachIndex = 0
      DO i = 1, Nsegment
        iorder = Segment_order(i)
        toseg = Tosegment(iorder)
        reachListMat(iorder,iorder) = 1
        IF ( toseg>0 ) THEN
          reachListMat(toseg,1:Nsegment) = reachListMat(toseg,1:Nsegment)+reachListMat(iorder,1:Nsegment)
          upReachIndMat(toseg,iorder) = 1
        ENDIF
      ENDDO
      !Note, size of upReachIndex sUps = SUM(upReachIndex)
      !Note, size of reachList sAll = SUM(reachListMat)
      DO i = 1, Nsegment
        reachCount(i) = SUM(reachListMat(i,1:Nsegment))
        upReachCount(i) = SUM(upReachIndMat(i,1:Nsegment))
      ENDDO
      DO i = 1, Nsegment
        reachStart(i) = SUM(reachCount(1:i)) - reachCount(i) + 1
        IF ( upReachCount(i)>0 ) upReachStart(i) = SUM(upReachCount(1:i)) - upReachCount(i) + 1
        j = reachStart(i)
        jj = upReachStart(i)
        DO k = 1,Nsegment
          IF (reachListMat(i,k) == 1) THEN
            reachList(j) = k
            j = j+1
          ENDIF
          IF (jj>0 .AND. upReachIndMat(i,k) == 1) THEN
            upReachIndex(jj) = k
            jj = jj+1
          ENDIF
        ENDDO
        iRchStart = reachStart(i)
        nRchCount = reachCount(i)
        totalArea(i) = DBLE(SUM(Segment_hruarea(reachList(iRchStart:(iRchStart+nRchCount-1)))))
        totalArea(i) = totalArea(i)*FT2_PER_ACRE*(FEET2METERS**2.)
      ENDDO

! Read global reach id, allocate
      allocate(REACHIDGV(Nsegment), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for REACHIDGV')
      REACHIDGV = seg_id
      if ( iSegOut /= -9999 ) then
        !print*, 'Outlet segment = ', iSegOut
! Identify index of the desired stream segment from reachID vector (dimension size: Nsegment)
        iSelect = minloc(abs(REACHIDGV - iSegOut))
        iSegDesire = iSelect(1)  ! de-vectorize the desired stream segment
        if(REACHIDGV(iSegDesire) /= iSegOut)&
          call handle_err(20,'unable to find desired stream segment')

! Start index and the count for lagged array - all the upstream segments, immediate upstream segment, immediate upstream HRUs
        iRchStart = reachStart(iSegDesire)
        nRchCount = reachCount(iSegDesire)
        !print*,'iRchStart   = ',iRchStart
        !print*,'Number of upstream segment from outlet segment (nRchCount): ',nRchCount

! Read reach list of index from global segments (all the upstream reachs for each segment)
        allocate(upStrmRchList(nRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for upStrmRchList')
        upStrmRchList = reachList(iRchStart:(iRchStart+nRchCount-1))

! Reach upstream segment and associated HRU infor from non-ragged vector
        allocate(NETOPO(nRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO')
        allocate(RPARAM(nRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for RPARAM')

! Create REACH index for local segments
        NETOPO(:)%REACHIX=arth(1,1,nRchCount)
        do iSeg=1,nRchCount
          ! Reach reach topology and parameters (integer)
          NETOPO(iSeg)%REACHID = seg_id(upStrmRchList(iSeg))
          NETOPO(iSeg)%DREACHK = Tosegment(upStrmRchList(iSeg))
          ! Reach reach topology and parameters (double precision precision)
          RPARAM(iSeg)%R_SLOPE = DBLE(Seg_slope(upStrmRchList(iSeg)))
          RPARAM(iSeg)%RLENGTH = DBLE(Seg_length(upStrmRchList(iSeg)))
          RPARAM(iSeg)%TOTAREA = totalArea(upStrmRchList(iSeg))
        enddo

        ! Recompute downstream segment index as local segment list, NETOPO(:)%REACHID
        do iSeg=1,nRchCount
          ! Assign downstream segment ID = 0 at desired outlet segment
          if (NETOPO(iSeg)%REACHID == iSegOut) then
            NETOPO(iSeg)%DREACHK = 0
          else
            ! Identify the index of the desired stream segment from reachID vector (dimension size: Nsegment)
            iSelect = minloc(abs(NETOPO(:)%REACHID - NETOPO(iSeg)%DREACHK))
            NETOPO(iSeg)%DREACHI = iSelect(1)  ! de-vectorize the desired stream segment
            if (NETOPO(NETOPO(iSeg)%DREACHI)%REACHID /= NETOPO(iSeg)%DREACHK) then
              !print*,'iSeg = ', iSeg
              !print*,'NETOPO(iSeg)%DREACHK = ', NETOPO(iSeg)%DREACHK
              !print*,'NETOPO(NETOPO(iSeg)%DREACHI)%REACHID = ', NETOPO(NETOPO(iSeg)%DREACHI)%REACHID
              call handle_err(20,'unable to find desired downstream segment')
            endif
          endif
        enddo

! Reach upstream segment and associated HRU infor from ragged vector
        nTotal=0
        do iSeg=1,nRchCount
          ! sAll dimension
          iRchStart1 = reachStart(upStrmRchList(iSeg))
          nRchCount1 = reachCount(upStrmRchList(iSeg))
          allocate(NETOPO(iSeg)%RCHLIST(nRchCount1), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%RCHLIST')
          allocate(RCHIXLIST(nRchCount1), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for RCHIXLIST(nRchCount)')
          RCHIXLIST = reachList(iRchStart1:(iRchStart1+nRchCount1-1))

 ! Recompute all the upstream segment indices as local segment list = NETOPO(:)%REACHID
          nTotal = nTotal + nRchCount1
          do jSeg=1,nRchCount1
            ! Identify the index of the desired stream segment from reachID vector (dimension size: Nsegment)
            iSelect = minloc( abs( NETOPO(:)%REACHID - REACHIDGV(RCHIXLIST(jSeg)) ) )
            NETOPO(iSeg)%RCHLIST(jSeg) = iSelect(1)  ! de-vectorize the desired stream segment
          enddo
          !print*,'NETOPO(iSeg)%RCHLIST(:) = ',NETOPO(iSeg)%RCHLIST(:)
          deallocate(RCHIXLIST, stat=ierr)

          ! sUps dimension
          iUpRchStart = upReachStart(upStrmRchList(iSeg))
          nUpRchCount = upReachCount(upStrmRchList(iSeg))
          allocate(NETOPO(iSeg)%UREACHI(nUpRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%UREACHI')
          allocate(NETOPO(iSeg)%UREACHK(nUpRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%UREACHK')
          allocate(NETOPO(iSeg)%goodBas(nUpRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%goodBas')
          if (nUpRchCount > 0) then

            NETOPO(iSeg)%UREACHK(:) = seg_id(upReachIndex(iUpRchStart:(iUpRchStart+nUpRchCount-1)))
            do jSeg=1,nUpRchCount
              ! Identify the index of the desired stream segment from reachID vector (dimension size: Nsegment)
              iSelect = minloc(abs(NETOPO(:)%REACHID - NETOPO(iSeg)%UREACHK(jSeg)))
              NETOPO(iSeg)%UREACHI(jSeg) = iSelect(1)  ! de-vectorize the desired stream segment
              ! check that we identify the correct upstream reach
              if (NETOPO(NETOPO(iSeg)%UREACHI(jSeg))%REACHID /= NETOPO(iSeg)%UREACHK(jSeg)) then
                !print*,'iSeg = ', iSeg
                !print*,'NETOPO(iSeg)%UREACHK(jSeg) = ', NETOPO(iSeg)%UREACHK(jSeg)
                !print*,'NETOPO(NETOPO(iSeg)%UREACHI(jSeg))%REACHID = ', NETOPO(NETOPO(iSeg)%UREACHI(jSeg))%REACHID
                call handle_err(20,'unable to find desired immediate upstream segment')
              endif

              ! check that the upstream reach has a basin area > 0
              if(RPARAM(NETOPO(iSeg)%UREACHI(jSeg))%TOTAREA > verySmall)then
               NETOPO(iSeg)%goodBas(jSeg) = .true.
              else
               NETOPO(iSeg)%goodBas(jSeg) = .false.
              endif

            enddo ! looping through the immediate upstream reaches
          endif  ! if not a headwater
         enddo  ! looping through the stream segments within the model domain
        nSegRoute => nRchCount

      else ! if the entire river network routing is selected
        !print*, 'Route all the segments included in network topology'
        ! Populate sSeg dimensioned variable
        allocate(NETOPO(nSeg), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO')
        allocate(RPARAM(nSeg), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for RPARAM')
        do iSeg=1,Nsegment
          ! Reach reach topology and parameters (integer)
          NETOPO(iSeg)%REACHIX = iSeg
          NETOPO(iSeg)%REACHID = seg_id(iSeg) != to iSeg
          NETOPO(iSeg)%DREACHI = Tosegment(iSeg)
          NETOPO(iSeg)%DREACHK = Tosegment(iSeg)
          ! Reach reach topology and parameters (double precision precision)
          RPARAM(iSeg)%R_SLOPE = DBLE(Seg_slope(iSeg))
          RPARAM(iSeg)%RLENGTH = DBLE(Seg_length(iSeg))
          RPARAM(iSeg)%TOTAREA = totalArea(iSeg)
       enddo
        ! Populate sAll dimensioned variable
        ! NETOPO%RCHLIST - upstream reach list
        nTotal=0
        do iSeg=1,Nsegment
          iRchStart1 = reachStart(iSeg)
          nRchCount1 = reachCount(iSeg)
          allocate(NETOPO(iSeg)%UPSLENG(nRchCount1), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%UPSLENG')
          allocate(NETOPO(iSeg)%RCHLIST(nRchCount1), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%RCHLIST')
          NETOPO(iSeg)%RCHLIST(:) = reachList(iRchStart1:(iRchStart1+nRchCount1-1))
          nTotal = nTotal + nRchCount1
        enddo
        ! Populate sUps dimensioned variable
        ! NETOPO%UREACHI - Immediate upstream reach index list
        ! NETOPO%UREACHK - Immediate upstream reach ID list
        do iSeg=1,Nsegment
          iUpRchStart = upReachStart(iSeg)
          nUpRchCount = upReachCount(iSeg)
          allocate(NETOPO(iSeg)%UREACHI(nUpRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%UREACHI')
          allocate(NETOPO(iSeg)%UREACHK(nUpRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%UREACHK')
          allocate(NETOPO(iSeg)%goodBas(nUpRchCount), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for NETOPO%goodBas')
          if (nUpRchCount > 0) then
            NETOPO(iSeg)%UREACHK(:) = seg_id(upReachIndex(iUpRchStart:(iUpRchStart+nUpRchCount-1)))
            NETOPO(iSeg)%UREACHI(:) = upReachIndex(iUpRchStart:(iUpRchStart+nUpRchCount-1))
            do jSeg=1,nUpRchCount
              ! check that we identify the correct upstream reach
              if (NETOPO(NETOPO(iSeg)%UREACHI(jSeg))%REACHID /= NETOPO(iSeg)%UREACHK(jSeg)) then
                !print*,'iSeg = ', iSeg
                !print*,'NETOPO(iSeg)%UREACHK(jSeg) = ', NETOPO(iSeg)%UREACHK(jSeg)
                !print*,'NETOPO(NETOPO(iSeg)%UREACHI(jSeg))%REACHID = ', NETOPO(NETOPO(iSeg)%UREACHI(jSeg))%REACHID
                call handle_err(20,'unable to find desired immediate upstream segment')
              endif
              ! check that the upstream reach has a basin area > 0
              if(RPARAM(NETOPO(iSeg)%UREACHI(jSeg))%TOTAREA > verySmall)then
               NETOPO(iSeg)%goodBas(jSeg) = .true.
              else
               NETOPO(iSeg)%goodBas(jSeg) = .false.
              endif
            enddo ! looping through the immediate upstream reaches
          endif  ! if not a headwater
        enddo
        nSegRoute => nSeg
      endif ! outlet segment choice

      if (doKWTroute) then
        ! define processing order of the reaches
        call reachorder(nSegRoute, ierr, cmessage); call handle_err(ierr, cmessage)
      end if

      ! identify the stream segment with the largest upstream area
      iDesire = maxLoc(RPARAM(:)%TOTAREA)
      ixDesire= iDesire(1)
      !print*, 'maximum upstream area = ', RPARAM(ixDesire)%TOTAREA, size(NETOPO(ixDesire)%RCHLIST)

      ! set the downstream index of the outlet reach to negative (the outlet reach does not flow into anything)
      NETOPO(ixDesire)%DREACHI = -9999

      ! allocate space for the simulated runoff at reaches
      allocate(RCHFLX(nens,nSegRoute), KROUTE(nens,nSegRoute), stat=ierr)
      if(ierr/=0) call handle_err(ierr,'problem allocating space for simulated runoff at the basins')


       ! setup streamflow replacement at segments if needed
       ! using as fake lakes for now, FIX?
       ! **************************************************
      ilake = 0
      DO i = iSeg, nSegRoute
        IF ( Obsin_segment(NETOPO(iSeg)%REACHID)>0 ) ilake = ilake+1
        IF ( Obsout_segment(NETOPO(iSeg)%REACHID)>0 ) ilake = ilake+1
      ENDDO
      NETOPO(:)%LAKE_IX = 0
      NETOPO(:)%LAKINLT = .FALSE.
      allocate(LKTOPO(ilake), stat=ierr); if(ierr/=0) call handle_err(ierr,'problem allocating space for LKTOPO')
      LKTOPO(:)%DREACHI = 0
      ! allocate space for the simulated flux at lakes
      allocate(LAKFLX(nens,ilake), stat=ierr)
      if(ierr/=0) call handle_err(ierr,'problem allocating space for simulated fluxes at the lakes')
      LAKFLX(:,:)%LAKE_Q = 0.D0
      IF (ilake>0) THEN
        LAKEFLAG = 1
        ilake = 0
        DO i = 1, nSegRoute
          IF ( Obsin_segment(NETOPO(iSeg)%REACHID)>0 ) THEN !like reach is outlet of lake with known 'lake' flow coming into reach
            ilake= ilake+1
            NETOPO(iSeg)%LAKE_IX = ilake
            LKTOPO(ilake)%DREACHI = NETOPO(iSeg)%REACHIX
          ENDIF
          IF ( Obsout_segment(NETOPO(iSeg)%REACHID)>0 ) THEN !like reach a lake with known 'lake' flow coming out reach and not routed in the reach
            ilake= ilake+1
            NETOPO(iSeg)%LAKE_IX = ilake
            LKTOPO(ilake)%DREACHI = NETOPO(iSeg)%REACHIX
            RPARAM(iSeg)%RLENGTH = 0.D0 ! FIX: NOT SURE I CAN HAVE 0 LENGTH
          ENDIF
        ENDDO
      ENDIF
      ! initialize the routed elements
      RCHFLX(:,:)%BASIN_QR(1) = 0.D0

      END FUNCTION mizuroute_init

!***********************************************************************
!     mizuroute_run - Compute routing summary values
!***********************************************************************
      INTEGER FUNCTION mizuroute_run()
      USE PRMS_MIZUROUTE
      USE PRMS_MODULE, ONLY: Nsegment, Ripst_flag, Glacier_flag
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Basin_area_inv, METERS2FEET, Active_hrus, Hru_route_order, &
     &    Basin_gl_cfs, Basin_gl_ice_cfs
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_cms, Basin_gwflow_cfs, Basin_ssflow_cfs, &
     &    Basin_stflow_out, Basin_cfs, Basin_stflow_in, Basin_sroff_cfs, Seg_inflow, Seg_outflow, &
     &    Seg_upstream_inflow, Seg_lateral_inflow, Flow_out
      USE PRMS_OBS, ONLY: Streamflow_cfs
      USE PRMS_SET_TIME, ONLY: Cfs_conv, Timestep_seconds
      USE PRMS_ROUTING, ONLY: Mann_n, Seg_Width, Obsin_segment, Tosegment, Obsout_segment, &
     &    Segment_delta_flow, Segment_type, Basin_segment_storage, Flow_in_great_lakes, &
     &    Flow_to_ocean, Flow_to_great_lakes, Flow_out_region, Flow_out_NHM, Flow_terminus, &
     &    Flow_to_lakes, Flow_replacement, Flow_in_region, Flow_in_nation, Flow_headwater, &
     &    Stage_ts, Stage_ante, Seg_bankflow, Seg_slope, Basin_bankflow, Bankst_seep_rate, &
     &    Ripst_areafr_max, Bankfinite_hru, Basin_bankst_head, Seg_ripflow, Basin_ripflow, &
     &    Basin_bankst_seep_rate, Basin_bankflow, Basin_bankst_seep, Basin_bankst_vol, &
     &    Hru_segment, Seg_length, Basin_ripst_area, Basin_ripst_seep, Basin_ripst_evap, &
     &    Basin_ripst_vol, Bankst_seep_rate
      USE PRMS_GLACR, ONLY: Basin_gl_top_melt, Basin_gl_ice_melt
      USE PRMS_SRUNOFF, ONLY: Basin_sroff
      USE PRMS_GWFLOW, ONLY: Basin_gwflow
! mizuroute specific modules
      USE nrtype                                    ! variable types, etc.
      USE reachparam                                ! reach parameters
      USE reachstate                                ! reach states
      USE reach_flux                                ! fluxes in each reach
      USE lake_param                                ! lake parameters
      USE lakes_flux                                ! fluxes in each lake
      USE kwt_route,only:qroute_rch          ! route kinematic waves through the river network

      IMPLICIT NONE
! Functions
      INTRINSIC MOD
      EXTERNAL comp_bank_storage, drain_the_swamp
! Local Variables
      INTEGER :: i, j, segtype, ilake, toseg
      DOUBLE PRECISION :: area_fac, segout
!***********************************************************************
      mizuroute_run = 0

! compute the time-delay histogram (to route runoff within basins)
      allocate(FRAC_FUTURE(1), stat=ierr)
      FRAC_FUTURE(1) = 0.d0
      !call qtimedelay(dt, fshape, tscale, ierr, cmessage)
      !call handle_err(ierr, cmessage)

! *****
! Prepare for the routing simulations...
! *******************************************
! allocate space for the simulated runoff at the HRUs

      ! initialize the time-delay histogram
      do iens=1,nens
       do ibas=1,nSegRoute
        ! allocate space for the delayed runoff
        allocate(RCHFLX(iens,ibas)%QFUTURE(size(FRAC_FUTURE)), stat=ierr)
        call handle_err(ierr, 'problem allocating space for QFUTURE element')
        ! initialize to zeroes
        RCHFLX(iens,ibas)%QFUTURE(:) = 0.D0
       end do
      end do


      ! define flags
      !LAKEFLAG=0  ! no lakes in the river network, but putting in fake ones to add in observed streamflow
      ! FIX: HOW DO WE DEAL WITH LAKES HERE, USE MUSKINGUM??

      ! define time
      T1 = T0+Timestep_seconds

! *****
! Perform the routing...
! **************************
      iTime=1

      ! loop through ensemble members
      do iens=1,nens
       ! Interpolate simulated runoff to local basins...
       do ibas=1,nSegRoute
        RCHFLX(iens,ibas)%BASIN_QI = DBLE(Seg_lateral_inflow(NETOPO(ibas)%REACHID)*CFS2CMS_CONV)
       end do  ! (looping through basins)
       !print*,'RCHFLX(iens,:)%BASIN_QI = ',RCHFLX(iens,:)%BASIN_QI!

       ! *****
       ! FIX ZERO OUT Delay runoff within local basins... IS THIS RIGHT??
       ! ****************************************
       ! route streamflow through the basin
       do ibas=1,nSegRoute ! place a fraction of runoff in future time steps
         RCHFLX(iens,ibas)%QFUTURE(1) = RCHFLX(iens,ibas)%BASIN_QI
         ! save the routed runoff
         RCHFLX(iens,ibas)%BASIN_QR(0) = RCHFLX(iens,ibas)%BASIN_QR(1)  ! (save the runoff from the previous time step) !CUT?
         RCHFLX(iens,ibas)%BASIN_QR(1) = RCHFLX(iens,ibas)%QFUTURE(1)
         RCHFLX(iens,ibas)%QFUTURE(1)    = 0.D0
       end do  ! (looping through basins)

       ! *****
       ! Replace streamflow at segments if needed
       ! water-use removed/added in routing module
       ! FIX DEAL WITH GAINING STREAMS will happen in depression storage type module
       ! **************************************************
       ilake = 0
       DO iSeg = 1, nSegRoute
         IF ( Obsin_segment(NETOPO(iSeg)%REACHID)>0 ) THEN !like reach is outlet of lake with known 'lake' flow coming into reach
           ilake= ilake+1
           LAKFLX(iens,ilake)%LAKE_Q = DBLE((Streamflow_cfs(Obsin_segment(NETOPO(iSeg)%REACHID))+Seg_lateral_inflow(NETOPO(iSeg)%REACHID))*CFS2CMS_CONV)
         ENDIF
         IF ( Obsout_segment(NETOPO(iSeg)%REACHID)>0 ) THEN !like reach is inlet of lake with known 'lake' flow coming into reach
           ilake= ilake+1
           LAKFLX(iens,ilake)%LAKE_Q = DBLE(Streamflow_cfs(Obsout_segment(NETOPO(iSeg)%REACHID))*CFS2CMS_CONV)
         ENDIF
       ENDDO

       ! *****
       ! Compute total instantaneous runoff from all upstream basins...
       ! *******************************************************************
       ! compute the sum of all upstream runoff at each point in the river network
       do iSeg=1,nSegRoute
         ! identify how many reaches are upstream
         nUpstream = size(NETOPO(iSeg)%RCHLIST)
         ! allocate space for upstream vectors
         allocate(iUpstream(nUpstream), qUpstream(nUpstream), stat=ierr)
         if(ierr/=0) call handle_err(ierr,'problem allocating vectors for all upstream basins')
         ! get indices for all reaches upstream
         iUpstream = NETOPO(iSeg)%RCHLIST(1:nUpstream)
         ! get streamflow for all reaches upstream
         qUpstream = RCHFLX(iens,iUpstream(1:nUpstream))%BASIN_QR(1)
         ! get mean streamflow
         RCHFLX(IENS,iSeg)%UPSTREAM_QI = sum(qUpstream)
         ! test
         if(NETOPO(iSeg)%REACHID == ixPrint)then
          print*, 'ixUpstream = ', NETOPO(iUpstream(1:nUpstream))%REACHIX
          print*, 'idUpstream = ', NETOPO(iUpstream(1:nUpstream))%REACHID
          print*, 'qUpstream = ', qUpstream
         endif
         ! deallocate space for upstream vectors
         deallocate(iUpstream,qUpstream, stat=ierr)
         if(ierr/=0) call handle_err(ierr,'problem deallocating vectors for all upstream basins')
       end do  ! looping through stream segments

       ! *****
       ! Route streamflow through the river network...
       ! **************************************************
       Seg_inflow = 0.0D0
       Seg_outflow = 0.0D0
       Seg_upstream_inflow = 0.0D0
       IF ( Ripst_flag==1 ) Stage_ante =Stage_ts
       if (doKWTroute) then
         RPARAM(:)%R_WIDTH =  DBLE(Seg_width) ! channel width (m)
         RPARAM(:)%R_MAN_N =  DBLE(Mann_n)  ! Manning's "n" paramater (unitless)

         ! route streamflow through the river network
         do iSeg=1,nSegRoute
           ! identify reach to process
           irch = NETOPO(iSeg)%RHORDER
           !print*, 'irch, ixDesire = ', irch, ixDesire
           ! route kinematic waves through the river network
           CALL QROUTE_RCH(IENS,irch,    & ! input: array indices
                           ixDesire,     & ! input: index of the outlet reach
                           T0,T1,        & ! input: start and end of the time step
                           LAKEFLAG,     & ! input: flag if lakes are to be processed
                           ierr,cmessage)  ! output: error control
           call handle_err(ierr,cmessage)
           !if(iRch==5) pause 'finished stream segment'
         end do  ! (looping through stream segments)
         Seg_outflow(NETOPO(:)%REACHID)= RCHFLX(iens,:)%REACH_Q/CFS2CMS_CONV

         DO iSeg=1,nSegRoute
           irch = NETOPO(iSeg)%RHORDER
           toseg = Tosegment(irch)
           IF ( toseg>0 ) Seg_upstream_inflow(toseg) = Seg_upstream_inflow(toseg) + Seg_outflow(irch)
         ENDDO
         Seg_inflow(NETOPO(:)%REACHID) = Seg_lateral_inflow(NETOPO(:)%REACHID) + &
     &                                     Seg_upstream_inflow(NETOPO(:)%REACHID)


         Outflow_ts(NETOPO(:)%REACHID)= RCHFLX(iens,:)%REACH_Q/CFS2CMS_CONV
       end if
      end do  ! (looping through ensemble members)

      ! for stage estimate
      IF ( Ripst_flag==1 ) THEN
        Basin_bankst_seep = 0.D0
        Basin_bankst_seep_rate = 0.0D0
        Basin_bankst_head = 0.0D0
        Basin_bankst_vol = 0.0D0
        Basin_ripst_area = 0.0D0
        Basin_ripst_seep = 0.0D0
        Basin_ripst_evap = 0.0D0
        Basin_ripst_vol = 0.0D0
        Bankst_seep_rate = 0.0 !collect by segment that HRUs go to
        Seg_bankflow = 0.0D0 !collect by segment that HRUs go to
        DO i = 1, Nsegment
          Stage_ts(i) = ( Seg_outflow(i)*CFS2CMS_CONV &
     &                     *Mann_n(i)/( Seg_width(i)*SQRT(Seg_slope(i)) ))**(5./3.)
          IF (Stage_ts(i)>250.) Stage_ts(i) = 250.
        ENDDO
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Hru_segment(i)>0 .AND. (Bankfinite_hru(i)==0 .OR. Ripst_areafr_max(i)>0.0)) &
     &      CALL comp_bank_storage(i)
!           ******Compute the bank storage component
!           transfers water between separate bank storage and stream depending on seepage
        ENDDO
        Basin_bankst_seep = Basin_bankst_seep*Basin_area_inv
        Basin_bankst_head = Basin_bankst_head*Basin_area_inv
        Basin_bankst_vol = Basin_bankst_vol*Basin_area_inv
        DO i = 1, Nsegment
          Basin_bankst_seep_rate = Basin_bankst_seep_rate + Bankst_seep_rate(i) &
     &                        *Seg_length(i)/SUM(Seg_length) !m2/day per stream ft length
          Seg_outflow(i) = Seg_outflow(i)+Seg_bankflow(i)
          IF (Seg_bankflow(i) < 0.0) THEN ! only could go negative because of bankflow if is negative
            IF (Seg_outflow(i) < 0.0) THEN ! took out more than streamflow, this could also be a water_use problem
              Seg_bankflow(i)  = Seg_bankflow(i) - Seg_outflow(i)
              Seg_outflow(i) = 0.0
            ENDIF
          ENDIF
        ENDDO
        Bankst_seep_rate = 0.0 !collect by segment that HRUs go to
        Seg_ripflow = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF  ( Hru_segment(i)>0 .AND. Ripst_areafr_max(i)>0.0) &
     &      CALL drain_the_swamp(i)
!           ******Compute the overbank riparian storage component
!           transfers water between separate riparian storage and stream depending on seepage
        ENDDO
        Basin_ripst_seep = Basin_ripst_seep*Basin_area_inv
        Basin_ripst_evap = Basin_ripst_evap*Basin_area_inv
        Basin_ripst_vol = Basin_ripst_vol*Basin_area_inv
        DO i = 1, Nsegment
          Seg_outflow(i) = Seg_outflow(i)+Seg_ripflow(i) ! cannot go negative by design
          Stage_ts(i) = ( Seg_outflow(i)*CFS2CMS_CONV &
     &                     *Mann_n(i)/( Seg_width(i)*SQRT(Seg_slope(i)) ))**(5./3.)
          IF (Stage_ts(i)>250.) Stage_ts(i) = 250.
        ENDDO
      ENDIF

      T0 = T1

      Basin_segment_storage = 0.0D0
      Basin_bankflow = 0.0D0
      Basin_ripflow = 0.0D0
      Flow_out = 0.0D0
      Flow_to_lakes = 0.0D0
      Flow_to_ocean = 0.0D0
      Flow_to_great_lakes = 0.0D0
      Flow_out_region = 0.0D0
      Flow_out_NHM = 0.0D0
      Flow_in_region = 0.0D0
      Flow_terminus = 0.0D0
      Flow_in_nation = 0.0D0
      Flow_headwater = 0.0D0
      Flow_in_great_lakes = 0.0D0
      Flow_replacement = 0.0D0
      DO i = 1, Nsegment
        segtype = Segment_type(i)
        segout = Seg_outflow(i)
! Flow_out is the total flow out of the basin, which allows for multiple outlets
! includes closed basins (tosegment=0)
        IF ( segtype==1 ) THEN
          Flow_headwater = Flow_headwater + segout
        ELSEIF ( segtype==2 ) THEN
          Flow_to_lakes = Flow_to_lakes + segout
        ELSEIF ( segtype==3 ) THEN
          Flow_replacement = Flow_replacement + segout
        ELSEIF ( segtype==4 ) THEN
          Flow_in_nation = Flow_in_nation + segout
        ELSEIF ( segtype==5 ) THEN
          Flow_out_NHM = Flow_out_NHM + segout
        ELSEIF ( segtype==6 ) THEN
          Flow_in_region = Flow_in_region + segout
        ELSEIF ( segtype==7 ) THEN
          Flow_out_region = Flow_out_region + segout
        ELSEIF ( segtype==8 ) THEN
          Flow_to_ocean = Flow_to_ocean + segout
        ELSEIF ( segtype==9 ) THEN
          Flow_terminus = Flow_terminus + segout
        ELSEIF ( segtype==10 ) THEN
          Flow_in_great_lakes = Flow_in_great_lakes + segout
        ELSEIF ( segtype==11 ) THEN
          Flow_to_great_lakes = Flow_to_great_lakes + segout
        ENDIF
        IF ( Tosegment(i)==0 ) Flow_out = Flow_out + segout

        Segment_delta_flow(i) = Segment_delta_flow(i) + Seg_inflow(i) - segout
!        IF ( Segment_delta_flow(i) < 0.0D0 ) PRINT *, 'negative delta flow', Segment_delta_flow(i)
        Basin_segment_storage = Basin_segment_storage + Segment_delta_flow(i)
        IF ( Ripst_flag==1 ) THEN
          Basin_bankflow = Basin_bankflow + Seg_bankflow(i)
          Basin_ripflow = Basin_ripflow + Seg_ripflow(i)
        ENDIF
      ENDDO

      area_fac = Cfs_conv/Basin_area_inv
      Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow ! not equal to basin_stflow_out if replacement flows
      Basin_cfs = Flow_out
      Basin_stflow_out = Basin_cfs / area_fac
      Basin_cms = Basin_cfs*CFS2CMS_CONV
      IF ( Glacier_flag==1 ) THEN
        Basin_stflow_in = Basin_stflow_in + Basin_gl_top_melt
        Basin_gl_ice_cfs = Basin_gl_ice_melt*area_fac
        Basin_gl_cfs = Basin_gl_top_melt*area_fac
      ENDIF
      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac
      Basin_segment_storage = Basin_segment_storage/area_fac
      IF ( Ripst_flag==1 ) THEN
        Basin_bankflow = Basin_bankflow/area_fac
        Basin_ripflow = Basin_ripflow/area_fac
      ENDIF

      END FUNCTION mizuroute_run

!***********************************************************************
      subroutine handle_err(err,message)
      ! handle error codes
      implicit none
      integer,intent(in)::err             ! error code
      character(*),intent(in)::message         ! error message
      if(err/=0)then
       print*,'FATAL ERROR: '//trim(message)
       stop
      endif
      end subroutine handle_err

!***********************************************************************
!     mizuroute_restart - write or read restart file
!***********************************************************************
      SUBROUTINE mizuroute_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_MIZUROUTE
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Function
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=9) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Outflow_ts
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Outflow_ts
      ENDIF
      END SUBROUTINE mizuroute_restart
