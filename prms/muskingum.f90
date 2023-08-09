!***********************************************************************
! Routes water between segments in the system using Muskingum routing
!
!   The Muskingum equation is described in 'Hydrology for Engineers', 3rd ed.
!   by Linsley, R.K, Kohler, M.A., and Paulhus, J.L.H., 1982 p. 275 and in
!   'Water in Environmental Planning' by Dunne, T., and Leopold, L.B. 1978
!   p. 357.
!
!   Note that the Muskingum equation assumes a linear relation of storage
!   to the inflow/outflow relation and therefore the relation is the same
!   throughout the range of the hydrograph.  The route_time parameter in
!   the fixroute module is replaced by two new parameters, K_coef and
!   x_coef, which are described below:
!
!   The Muskingum method is based on the equation: S = K[xI + (1 - x)O]
!     where S is storage, K is the storage coefficient, x is a coefficient
!     between 0 and .5, I is inflow, and O is outflow.
!
!   Solving for the outflow at day 2,O2; and knowing the inflow at day 1,
!   I1; the inflow at day 2,I2; and the outflow at day 1, O1; the storage
!   equation can be written as follows:
!
!        O2 = czero*I2 + cone*I1 + ctwo*O1
!
!     where czero = -((Kx - 12)    / (K - Kx + 12))
!           cone  =  (Kx + 12)     / (K - Kx + 12)
!           ctwo  =  (K - Kx - 12) / (K - Kx + 12)
!
!     assuming a time step of one day and K is in units of hours
!
!   This module is based on the "musroute.f" module, plus:
!
!   1. This module uses an internal routing time step of one hour.
!      The old muskingum module ran on the same daily time step as
!      the rest of PRMS. The problem with this is that there is no
!      ability to distinguish where the flood wave (front of the flow
!      change) within the segment. For example, if there is a series
!      of 4 1-day long segments, a flood wave will make it to the bottom
!      of these in 1 day. If the same system is modeled as 1 4-day long
!      segment, it will take 4 days.
!
!   2. The X parameter has been removed as a specified input and is now computed. To
!      my knowledge, no modeler had ever set this to anything other than the default
!      value (0.2) anyway. Always using the default value can lead to problems
!      with the C coffecients which can result in mass balance problems or negative
!      flow values.
!      To solve this problem, I assume that the C coefficients must
!      always be between 0 and 1. By setting the C coefficients equal to 0 and 1,
!      various limits on the time step (ts), X, and K can be determined. There are
!      two of these limits which are of interest:
!
!      When C0 = 0:
!             ts
!        K = -----
!             2X
!
!      When C2 = 0:
!            ts
!       K = -----
!           2(1-X)
!
!      Determining a value of K half way between these two limits (by averaging)
!      and solving for X using the quadratic formula results in:
!
!            1-sqrt(1-(ts/K))
!       X = ------------------
!                  2
!
!       So when ts is fixed at one hour and K is fixed as the average (or expected)
!       travel time corresponding to the segment (for each segment in the stream
!       network), a value of X can be computed (for each segment in the stream
!       network) which will result in both conservation of mass and non-negative
!       flows. Another benefit is that only one input parameter (K) needs to be
!       input to the module.
!
!   3. If the travel time of a segment is less than or equal to the routing
!      time step (one hour), then the outflow of the segment is set to the
!      value of the inflow.
!
!   4. Simulate dead storage in a reservoir reach segment_type 2 that is filled by channel losses.
!      Storage is reduce at the segments potential evaporation rate and a user-defined percentage of the volume.
! Assumes daily timestep.
!   New parameters:  dead_volume (ac-ft)
!                    reach_area (ac-ft)
!   New variable     dead_storage (ac-ft)
!
!   5. Simulate channel losses in a reservoir reach--segment_type 2
!      Losses are assumed to be lost first to dead storage and then to deep
!      groundwater. Use monthlyq_pot.f module to get listings of the amount of channel loss.
!   New parameters:  deep_sink_pct         percentage of flow that is lost
!                    channel_sink_pct      decimal percent of dead storage
!                                          lost daily to deep groundwater
!                    channel_sink_thrshld  maximum flow in cfs that 
!                                          channel_sink_pct is applied to
!   New variable:    channel_loss
!
!***********************************************************************
      MODULE PRMS_MUSKINGUM
      IMPLICIT NONE
      character(len=*), parameter :: MODDESC = 'Streamflow Routing'
      character(len=14), parameter :: MODNAME = 'muskingum_mann'
      character(len=*), parameter :: Version_muskingum = '2022-06-07'
!   Local Variables
      DOUBLE PRECISION, PARAMETER :: ONE_24TH = 1.0D0 / 24.0D0
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Currinsum(:), Pastin(:), Pastout(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Outflow_ts(:), Inflow_ts(:)
      INTEGER, SAVE, ALLOCATABLE :: Nratetable(:)
      REAL, SAVE, ALLOCATABLE :: Czero_t(:, :), Cone_t(:, :), Ctwo_t(:, :)
      INTEGER, SAVE :: Nmusktable, Nbankval
      REAL, SAVE, ALLOCATABLE :: Past_deadstr(:), Vol_loss(:)
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Dead_storage(:), Channel_loss(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Table_segment(:), Segment_table_on_off(:)
      REAL, SAVE, ALLOCATABLE :: Ratetbl_disch(:, :), K_coef_table(:, :), X_coef_table(:, :)
      REAL, SAVE, ALLOCATABLE :: Dead_vol(:), Reach_area(:), Deep_sink_pct(:)
      REAL, SAVE, ALLOCATABLE :: Channel_sink_thrshld(:), Channel_sink_pct(:)
      END MODULE PRMS_MUSKINGUM

!***********************************************************************
!     Main muskingum routine
!***********************************************************************
      INTEGER FUNCTION muskingum()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, SETDIMENS, ACTIVE, OFF, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: muskingum_decl, muskingum_init, muskingum_run
      EXTERNAL :: muskingum_restart, muskingum_dims
!***********************************************************************
      muskingum = 0

      IF ( Process_flag==RUN ) THEN
        muskingum = muskingum_run()
      ELSEIF ( Process_flag==SETDIMENS ) THEN
        CALL muskingum_dims()
      ELSEIF ( Process_flag==DECL ) THEN
        muskingum = muskingum_decl()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL muskingum_restart(READ_INIT)
        muskingum = muskingum_init()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL muskingum_restart(SAVE_INIT)
      ENDIF

      END FUNCTION muskingum

!***********************************************************************
!     Declares musroute module specific dimensions
!***********************************************************************
      SUBROUTINE muskingum_dims()
      USE PRMS_MODULE, ONLY: MAXDIM
      use prms_utils, only: read_error
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
!***********************************************************************
      IF ( decldim('nmusktable', 0, MAXDIM, 'Number of Muskingum rating tables')/=0 ) CALL read_error(7, 'nmusktable')
      IF ( decldim('nbankval', 2, MAXDIM, 'Number of values in Muskingum rating tables')/=0 ) CALL read_error(7, 'nbankval')
      END SUBROUTINE muskingum_dims

!***********************************************************************
!     muskingum_decl - Declare parameters and variables and allocate arrays
!   Declared Parameters
!     tosegment, hru_segment, obsin_segment, K_coef, x_coef
!***********************************************************************
      INTEGER FUNCTION muskingum_decl()
      USE PRMS_CONSTANTS, ONLY: strmflow_muskingum_module, DOCUMENTATION, ACTIVE
      USE PRMS_MODULE, ONLY: Nsegment, Strmflow_flag, Model, channel_loss_flag
      USE PRMS_MUSKINGUM
      use prms_utils, only: print_module, read_error
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getdim, declparam, declvar
!***********************************************************************
      muskingum_decl = 0

      IF ( Strmflow_flag==strmflow_muskingum_module ) THEN
        CALL print_module(MODDESC, MODNAME(:9), Version_muskingum)
      ELSE ! muskingum_mann
        CALL print_module(MODDESC, MODNAME, Version_muskingum)
      ENDIF

      Nmusktable = getdim('nmusktable')
      IF ( Nmusktable==-1 ) CALL read_error(6, 'nmusktable')
      IF ( Model==DOCUMENTATION .AND. Nmusktable==0 ) Nmusktable = 1
      Nbankval = getdim('nbankvaltable')
      IF ( Nbankval==-1 ) CALL read_error(6, 'nbankval')
      IF ( Model==DOCUMENTATION .AND. Nbankval==0 ) Nbankval = 1
      IF ( Nmusktable>0 ) THEN
        ALLOCATE ( Nratetable(Nmusktable), Czero_t(Nbankval,Nmusktable) )
        ALLOCATE ( Cone_t(Nbankval,Nmusktable), Ctwo_t(Nbankval,Nmusktable) )

        ALLOCATE ( K_coef_table(Nbankval,Nmusktable) )
        IF ( declparam(MODNAME, 'k_coef_table', 'nbankval,nsegment', 'real', &
     &       '1.0', '0.01', '240.0', &
     &       'Muskingum storage coefficient for each rating table', &
     &       'Travel time of flood wave from one segment to the next downstream segment,'// &
     &       ' called the Muskingum storage coefficient; values must be greater than 0.0', &
     &       'hours')/=0 ) CALL read_error(1, 'k_coef_table')
        ALLOCATE ( X_coef_table(Nbankval,Nmusktable) )
        IF ( declparam(MODNAME, 'x_coef_table', 'nbankval,nmusktable', 'real', &
     &       '0.2', '0.0', '0.5', &
     &       'Routing weighting factor for each rating table', &
     &       'The amount of attenuation of the flow wave, called the Muskingum routing weighting factor;'// &
     &       ' enter 0.0 for reservoirs, diversions, and segment(s) flowing out of the basin', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'x_coef_table')
        ALLOCATE ( Ratetbl_disch(Nbankval,Nmusktable) )
        IF ( declparam(MODNAME, 'ratetbl_disch', 'nbankval,nmusktable', 'real', &
     &       '-999.0',  '0.0', '240.0', &
     &       'Flow rate corresponding to the musroute rating table', &
     &       'Flow rate corresponding to the musroute rating table', &
     &       'runoff_units')/=0 ) CALL read_error(1, 'ratetbl_disch')
        ALLOCATE ( Table_segment(Nsegment) )
        IF ( declparam(MODNAME, 'table_segment', 'nsegment', 'integer', &
     &       '0', '0', 'nmusktable', &
     &       'Rating table number for Maskingum flow routing', &
     &       'Rating table number for Maskingum flow routing', &
     &       'none')/=0 ) CALL read_error(1, 'table_segment')
        ALLOCATE ( Segment_table_on_off(Nsegment) )
        IF ( declparam(MODNAME, 'segment_table_on_off', 'nsegment', 'integer', &
     &       '0', '0', '1', &
     &       'Use or not use Muskingum flow rating table (0=no; 1=yes)', &
     &       'Use or not use Muskingum flow rating table (0=no; 1=yes)', &
     &       'none')/=0 ) CALL read_error(1, 'segment_table_on_off')
      ENDIF

! New parameters for dead storage simulation
      IF ( channel_loss_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Past_deadstr(Nsegment), Vol_loss(Nsegment) )
! New variable for dead storage simulation
        ALLOCATE ( Dead_storage(Nsegment) )
        IF ( declvar(MODNAME, 'dead_storage', 'nsegment', Nsegment, 'real', &
     &       'The current amount of water in dead storage', &
     &       'acre-feet', Dead_storage)/=0 ) CALL read_error(3, 'dead_storage')
! New variable for channel loss simulation
        ALLOCATE ( Channel_loss(Nsegment) )
        IF ( declvar(MODNAME, 'channel_loss', 'nsegment', Nsegment, 'real', &
     &       'Amount of water loss to deep groundwater from a segment', &
     &       'cfs', Channel_loss)/=0 ) CALL read_error(3, 'channel_loss')
        ALLOCATE ( Dead_vol(Nsegment) )
        IF ( declparam(MODNAME, 'dead_vol', 'nsegment', 'real', &
     &       '0.0', '0.0', '1E+07', &
     &       'Dead storage in acre-feet for reservoir segments', &
     &       'Volume in a reach that needs to be filled before flow leaves the segment. Volume is reduced by evaporation', &
     &       'acre-feet')/=0 ) CALL read_error(1, 'dead_vol')
        ALLOCATE ( Reach_area(Nsegment) )
        IF ( declparam(MODNAME, 'reach_area', 'nsegment', 'real', &
     &       '0.0', '0.0', '1E+07', &
     &       'Reach area in acres of dead storage', &
     &       'Used to calculate evaporation to reduce dead-storage volume', &
     &       'acres')/=0 ) CALL read_error(1, 'reach_area')
! New parameters for channel loss simulation
        ALLOCATE ( Deep_sink_pct(Nsegment) )
        IF ( declparam(MODNAME, 'deep_sink_pct', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Portion of flow in segment that is lost', &
     &       'Segment discharge is reduced by this decimal percentage for reservoir type segments', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'deep_sink_pct')
        ALLOCATE ( Channel_sink_pct(Nsegment) )
        IF ( declparam(MODNAME, 'channel_sink_pct', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Portion of flow in channel that is lost', &
     &       'Segment discharge is reduced by this decimal percentage and routed to dead storage only if it is not filled', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'channel_sink_pct')
        ALLOCATE ( Channel_sink_thrshld(Nsegment) )
        IF ( declparam(MODNAME, 'channel_sink_thrshld', 'nsegment', 'real', &
     &       '10.0', '0.0', '1E+07', &
     &       'The maximum discharge that channel_sink_pct is applied', &
     &       'The maximum channel loss in reservoir segment types', &
     &       'cfs')/=0 ) CALL read_error(1, 'channel_sink_thrshld')
      ENDIF

      ALLOCATE ( Currinsum(Nsegment) )
      ALLOCATE ( Pastin(Nsegment), Pastout(Nsegment) )
      ALLOCATE ( Outflow_ts(Nsegment), Inflow_ts(Nsegment) )

      END FUNCTION muskingum_decl

!***********************************************************************
!    muskingum_init - Get and check parameter values and initialize variables
!***********************************************************************
      INTEGER FUNCTION muskingum_init()
      USE PRMS_CONSTANTS, ONLY: NEARZERO, ACTIVE
      USE PRMS_MODULE, ONLY: Nsegment, Init_vars_from_file, channel_loss_flag
      USE PRMS_MUSKINGUM
      USE PRMS_BASIN, ONLY: Basin_area_inv
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_ROUTING, ONLY: Basin_segment_storage
      use prms_utils, only: read_error
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getdim, getparam
! Local Variables
      INTEGER :: i, j
      REAL :: k, d, kx, tss
!***********************************************************************
      muskingum_init = 0

      !Seg_outflow will have been initialized to Segment_flow_init in PRMS_ROUTING
      IF ( Init_vars_from_file==0 ) Outflow_ts = 0.0D0

      IF ( Nmusktable>1 ) THEN
        IF ( getparam(MODNAME, 'K_coef_table', Nbankval*Nmusktable, 'real', K_coef_table)/=0 ) CALL read_error(2, 'K_coef_table')
        IF ( getparam(MODNAME, 'x_coef_table', Nbankval*Nmusktable, 'real', X_coef_table)/=0 ) CALL read_error(2, 'x_coef_table')
        IF ( getparam(MODNAME, 'ratetbl_disch',  Nbankval*Nmusktable, 'real', Ratetbl_disch)/=0 ) &
             CALL read_error(2, 'ratetbl_disch')
        IF ( getparam(MODNAME, 'table_segment', Nsegment, 'integer', Table_segment)/=0 ) CALL read_error(2, 'table_segment')
        IF ( getparam(MODNAME, 'segment_table_on_off', Nsegment, 'integer', Segment_table_on_off)/=0 ) &
             CALL read_error(2, 'segment_table_on_off')
        Czero_t = 0.0
        Cone_t = 0.0
        Ctwo_t = 0.0
        Nratetable = 0
        DO j = Nmusktable, 1, -1
          DO i = 1, Nbankval
            IF ( K_coef_table(i,j)>0.0 ) THEN
              Nratetable(j) = i
              EXIT
            ENDIF
          ENDDO
        ENDDO
        tss = 12.0
        DO j = 1, Nmusktable
          DO i = 1, Nratetable(j)
            k = K_coef_table(i,j)
            kx = k*X_coef_table(i, j)
            k = k - kx
            d = k + tss
            IF ( ABS(d)<NEARZERO ) THEN
              PRINT *, 'WARNING in muskingum, computed value d = 0.0, set to:', NEARZERO
              d = NEARZERO
            ENDIF
            Czero_t(i,j) = -1.0*((kx - tss)/d)
            Cone_t(i,j) = (kx + tss)/d
            Ctwo_t(i,j) = (k - tss)/d

! SHORT travel time
            IF ( Ctwo_t(i,j)<0.0 ) THEN
              Cone_t(i,j) = Cone_t(i,j) + Ctwo_t(i,j)
              Ctwo_t(i,j) = 0.0
            ENDIF

! LONG travel time
            IF ( Czero_t(i,j)<0.0 ) THEN
              Cone_t(i,j) = Cone_t(i,j) + Czero_t(i,j)
              Czero_t(i,j) = 0.0
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      Basin_segment_storage = 0.0D0
      DO i = 1, Nsegment
        Basin_segment_storage = Basin_segment_storage + Seg_outflow(i)
      ENDDO
      Basin_segment_storage = Basin_segment_storage*Basin_area_inv/Cfs_conv

      IF ( channel_loss_flag==ACTIVE ) THEN
        Past_deadstr = Dead_storage
! following parameters for dead storage simulation
        IF ( getparam(MODNAME, 'dead_vol', Nsegment, 'real', Dead_vol)/=0 ) CALL read_error(2, 'dead_vol')
        IF ( getparam(MODNAME, 'reach_area', Nsegment, 'real', Reach_area)/=0 ) CALL read_error(2, 'reach_area')
! following parameters for channel loss simulation
        IF ( getparam(MODNAME, 'deep_sink_pct', Nsegment, 'real', Deep_sink_pct)/=0 ) CALL read_error(2, 'deep_sink_pct')
        IF ( getparam(MODNAME, 'channel_sink_pct', Nsegment, 'real', Channel_sink_pct)/=0 ) &
     &       CALL read_error(2, 'channel_sink_pct')
        IF ( getparam(MODNAME, 'channel_sink_thrshld', Nsegment, 'real', Channel_sink_thrshld)/=0 ) &
     &       CALL read_error(2, 'channel_sink_thrshld')
      ENDIF

      END FUNCTION muskingum_init

!***********************************************************************
!     muskingum_run - Compute routing summary values
!***********************************************************************
      INTEGER FUNCTION muskingum_run()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, CFS2CMS_CONV, OUTFLOW_SEGMENT, ERROR_streamflow, INCHES_PER_FOOT
      USE PRMS_MODULE, ONLY: Nsegment, Glacier_flag, channel_loss_flag
      USE PRMS_MUSKINGUM
      USE PRMS_BASIN, ONLY: Basin_area_inv, Basin_gl_cfs, Basin_gl_ice_cfs
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_cms, Basin_gwflow_cfs, Basin_ssflow_cfs, &
     &    Basin_stflow_out, Basin_cfs, Basin_stflow_in, Basin_sroff_cfs, Seg_inflow, Seg_outflow, &
     &    Seg_upstream_inflow, Seg_lateral_inflow, Flow_out, Basin_sroff
      USE PRMS_OBS, ONLY: Streamflow_cfs
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_ROUTING, ONLY: Use_transfer_segment, Segment_delta_flow, Basin_segment_storage, Seginc_potet, &
     &    Obsin_segment, Segment_order, Tosegment, C0, C1, C2, Ts, Ts_i, Obsout_segment, Cfs2acft, &
     &    Flow_to_ocean, Flow_to_great_lakes, Flow_out_region, Flow_out_NHM, Segment_type, Flow_terminus, &
     &    Flow_to_lakes, Flow_replacement, Flow_in_region, Flow_in_nation, Flow_headwater, Flow_in_great_lakes
      USE PRMS_GLACR, ONLY: Basin_gl_top_melt, Basin_gl_ice_melt
      USE PRMS_GWFLOW, ONLY: Basin_gwflow
      use prms_utils, only: error_stop
      IMPLICIT NONE
! Functions
      INTRINSIC :: MOD, SNGL
! Local Variables
      INTEGER :: i, j, iorder, toseg, imod, tspd, segtype
      REAL :: czero, cone, ctwo, avail_stor, temp
      DOUBLE PRECISION :: area_fac, segout, currin
!***********************************************************************
      muskingum_run = 0

!     SET yesterdays inflows and outflows into temp (past arrays)
!     values may be 0.0 as intial, > 0.0 for runtime and dynamic
!     initial condtions. Then set outlfow and inflow for this time
!     step to 0.0

      IF ( channel_loss_flag==1 ) THEN
        DO i = 1, Nsegment
          IF ( Segment_type(i)==1 ) THEN
            Past_deadstr(i) = Dead_storage(i)
            Vol_loss(i) = Reach_area(i)*SNGL( Seginc_potet(i)/INCHES_PER_FOOT )
          ENDIF
        ENDDO
      ENDIF

!
!     upstream_inflow and outflow will vary by hour
!     lateral_inflow and everything else will vary by day
!
!     Compute surface runoff, ssflow, and gwflow going to each segment
!     This is todays "seg_inflow" before additional water is routed to
!     a new (if any is routed)
!
!     For each HRU if the lateral flow for this HRU goes to the
!     segment being evaluated (segment i) then sum flows
!
!     Do these calculations once for the current day, before the hourly
!     routing starts.
!
!       Out2   =      In2*C0    +        In1*C1    +          Out1*C2
!     Seg_outflow = Seg_inflow*Czero + Pastinflow*Cone + Pastoutflow*Ctwo
!       C0, C1, and C2: initialized in the "init" part of this module
!
      Pastin = Seg_inflow
      Pastout = Seg_outflow
      Seg_inflow = 0.0D0
      Seg_outflow = 0.0D0
      Inflow_ts = 0.0D0
      Currinsum = 0.0D0

! 24 hourly timesteps per day
      DO j = 1, 24

        Seg_upstream_inflow = 0.0D0
        DO i = 1, Nsegment
          iorder = Segment_order(i)

! current inflow to the segment is the time weighted average of the outflow
! of the upstream segments plus the lateral HRU inflow plus any gains.
          currin = Seg_lateral_inflow(iorder) !note, this routes to inlet
          IF ( Obsin_segment(iorder)>0 ) Seg_upstream_inflow(iorder) = Streamflow_cfs(Obsin_segment(iorder))
          currin = currin + Seg_upstream_inflow(iorder)
          Seg_inflow(iorder) = Seg_inflow(iorder) + currin

!  First, compute potential channel loss then use channel loss to add to 
!  dead storage volume, if dead storage is full there is no channel loss or
!  it is reduced. 
!  Second, reduce dead storage by evaporation (vol_loss) and deep sink 
!  (deep_sink_pct * dead storage)
!  Third, compute Segment_inflow  (Segment_inflow - actual channel loss)

          IF ( channel_loss_flag==ACTIVE ) THEN
            !IF ( Segment_type(iorder)/=2 ) THEN ! not a lake segment
              ! channel sink and deep sink calcs
              IF ( Seg_inflow(iorder)>Channel_sink_thrshld(iorder) ) THEN
                Channel_loss(iorder) = Channel_sink_thrshld(iorder)*Channel_sink_pct(iorder)
              ELSE
                Channel_loss(iorder) = Seg_inflow(iorder)*Channel_sink_pct(iorder)
              ENDIF
              Dead_storage(iorder) = Past_deadstr(iorder) + Channel_loss(iorder)*Cfs2acft
              avail_stor = Dead_vol(iorder) - Dead_storage(iorder)
              IF ( avail_stor<0.0 ) THEN
                Channel_loss(iorder) = Channel_loss(iorder) + avail_stor/Cfs2acft
                Dead_storage(iorder) = Dead_vol(iorder)
              ENDIF
              Dead_storage(iorder) = Dead_storage(iorder) + Channel_loss(iorder)*Cfs2acft - Vol_loss(iorder) &
     &                               - Deep_sink_pct(iorder)*Dead_storage(iorder)
              IF ( Dead_storage(iorder)<0.0 ) Dead_storage(iorder) = 0.0
              Seg_inflow(iorder) = Seg_inflow(iorder) - Channel_loss(iorder)
              IF ( Seg_inflow(iorder)<0.0 ) THEN
                PRINT *, 'Channel loss problem, segment_inflow<0.0', Seg_inflow(iorder), ' value set to 0.0'
                temp = Seg_inflow(iorder)
                Channel_loss(iorder) = Channel_loss(iorder) + temp
                Dead_storage(iorder) = Dead_storage(iorder) - temp*Cfs2acft
                Vol_loss(iorder) = Vol_loss(iorder) + temp*Cfs2acft
                Seg_inflow(iorder) = 0.0
              ENDIF
            !ENDIF
          ENDIF

          Inflow_ts(iorder) = Inflow_ts(iorder) + currin
          Currinsum(iorder) = Currinsum(iorder) + Seg_upstream_inflow(iorder)

          ! Check to see if this segment is to be routed on this time step
          tspd = Ts_i(iorder)
          imod = MOD( j, tspd )
          IF ( imod==0 ) THEN
            Inflow_ts(iorder) = (Inflow_ts(iorder) / Ts(iorder))
! Compute routed streamflow
            IF ( Ts_i(iorder)>0 ) THEN
              IF ( Segment_table_on_off(i)==0 ) THEN
                czero = C0(iorder)
                cone = C1(iorder)
                ctwo = C2(iorder)
              ELSE
                ! look up flow in rating table to get Czero, Cone, Ctwo
                CALL RATING_LOOKUP_musroute(iorder, Table_segment(iorder), Outflow_ts(iorder), czero, cone, ctwo)
              ENDIF
! Muskingum routing equation
              Outflow_ts(iorder) = Inflow_ts(iorder)*czero + Pastin(iorder)*cone + Outflow_ts(iorder)*ctwo
            ELSE
! If travel time (K_coef paremter) is less than or equal to
! time step (one hour), then the outflow is equal to the inflow
! Outflow_ts is the value from last hour
              Outflow_ts(iorder) = Inflow_ts(iorder)
            ENDIF

            ! pastin is equal to the Inflow_ts on the previous routed timestep
            Pastin(iorder) = Inflow_ts(iorder)

! because the upstream inflow from streams is used, reset it to zero so new average
! can be computed next routing timestep.
            Inflow_ts(iorder) = 0.0D0
          ENDIF

          IF ( Obsout_segment(iorder)>0 ) Outflow_ts(iorder) = Streamflow_cfs(Obsout_segment(iorder))

          ! water-use removed/added in routing module
          ! check for negative flow
          IF ( Outflow_ts(iorder)<0.0 ) THEN
            IF ( Use_transfer_segment==1 ) THEN
              PRINT *, 'ERROR, transfer(s) from stream segment:', iorder, ' causes outflow to be negative'
              PRINT *, '       outflow =', Outflow_ts(iorder), ' must fix water-use stream segment transfer file'
            ELSE
              PRINT *, 'ERROR, outflow from segment:', iorder, ' is negative:', Outflow_ts(iorder)
              PRINT *, '       routing parameters may be invalid'
            ENDIF
            CALL error_stop('negative streamflow in muskingum', ERROR_streamflow)
          ENDIF

          ! Seg_outflow (the mean daily flow rate for each segment) will be the average of the hourly values.
          Seg_outflow(iorder) = Seg_outflow(iorder) + Outflow_ts(iorder)
          ! pastout is equal to the Inflow_ts on the previous routed timestep
          Pastout(iorder) = Outflow_ts(iorder)

! Add current timestep's flow rate to sum the upstream flow rates.
! This can be thought of as a volume because it is a volumetric rate
! (cubic feet per second) over a time step of an hour. Down below when
! this value is used, it will be divided by the number of hours in the
! segment's simulation time step, giving the mean flow rate over that
! period of time.
          toseg = Tosegment(iorder)
          IF ( toseg>0 ) Seg_upstream_inflow(toseg) = Seg_upstream_inflow(toseg) + Outflow_ts(iorder)

        ENDDO ! segment

      ENDDO  ! timestep

      Basin_segment_storage = 0.0D0
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
        Seg_outflow(i) = Seg_outflow(i) * ONE_24TH
        segout = Seg_outflow(i)
        segtype = Segment_type(i)
        Seg_inflow(i) = Seg_inflow(i) * ONE_24TH
        Seg_upstream_inflow(i) = Currinsum(i) * ONE_24TH
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
        IF ( Tosegment(i)==OUTFLOW_SEGMENT ) Flow_out = Flow_out + segout
        Segment_delta_flow(i) = Segment_delta_flow(i) + Seg_inflow(i) - segout
!        IF ( Segment_delta_flow(i) < 0.0D0 ) PRINT *, 'negative delta flow', Segment_delta_flow(i)
        Basin_segment_storage = Basin_segment_storage + Segment_delta_flow(i)
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

      END FUNCTION muskingum_run

!***********************************************************************
      ! look up flow in rating table to get Czero, Cone, Ctwo
!***********************************************************************
      SUBROUTINE RATING_LOOKUP_musroute(Iseg, Table_segment, Inflow, C0, C1, C2)
      USE PRMS_MUSKINGUM, ONLY: Nratetable, Czero_t, Cone_t, Ctwo_t, Ratetbl_disch
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Iseg, Table_segment
      DOUBLE PRECISION, INTENT(IN) :: Inflow
      REAL, INTENT(OUT) :: C0, C1, C2
      ! Function
      INTRINSIC MOD
      ! Local Variables
      INTEGER i, n
      REAL :: fac
!***********************************************************************
      n = Nratetable(Table_segment)
      IF ( Inflow>Ratetbl_disch(1,Table_segment) ) THEN
        IF ( Inflow<Ratetbl_disch(n,Table_segment) ) THEN
          DO i = n-1, 1 !rsr??? is linear interpolation valid???
            IF ( Inflow<Ratetbl_disch(i,Table_segment) ) CYCLE
            fac = (Inflow-Ratetbl_disch(i,Table_segment))/(Ratetbl_disch(i+1,Table_segment)-Ratetbl_disch(i,Table_segment))
            C0 = Czero_t(i, Table_segment) + Czero_t(i, Table_segment)*fac
            C1 = Cone_t(i, Table_segment) + Cone_t(i, Table_segment)*fac
            C2 = Ctwo_t(i, Table_segment) + Ctwo_t(i, Table_segment)*fac
            EXIT
          ENDDO
        ELSE
          PRINT *, 'flow > rating table for segment', Iseg, '; rating table:', Table_segment, ' using last value'
          C0 = Czero_t(n, Table_segment)
          C1 = Cone_t(n, Table_segment)
          C2 = Ctwo_t(n, Table_segment)
        ENDIF
      ELSE
        PRINT *, 'flow < rating table for segment', Iseg, '; rating table:', Table_segment, ' using first value'
        C0 = Czero_t(1, Table_segment)
        C1 = Cone_t(1, Table_segment)
        C2 = Ctwo_t(1, Table_segment)
      ENDIF

      END SUBROUTINE RATING_LOOKUP_musroute

!***********************************************************************
!     muskingum_restart - write or read restart file
!***********************************************************************
      SUBROUTINE muskingum_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, OFF, ACTIVE
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, text_restart_flag, channel_loss_flag
      USE PRMS_MUSKINGUM
      use prms_utils, only: check_restart
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Local Variable
      CHARACTER(LEN=14) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        IF ( text_restart_flag==OFF ) THEN
          WRITE ( Restart_outunit ) MODNAME
          WRITE ( Restart_outunit ) Outflow_ts
          IF ( channel_loss_flag>ACTIVE ) WRITE ( Restart_outunit ) Dead_storage
        ELSE
          WRITE ( Restart_outunit, * ) MODNAME
          WRITE ( Restart_outunit, * ) Outflow_ts
          IF ( channel_loss_flag>ACTIVE ) WRITE ( Restart_outunit, * ) Dead_storage
        ENDIF
      ELSE
        IF ( text_restart_flag==OFF ) THEN
          READ ( Restart_inunit ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit ) Outflow_ts
          IF ( channel_loss_flag==ACTIVE ) READ ( Restart_inunit ) Dead_storage
        ELSE
          READ ( Restart_inunit, * ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit, * ) Outflow_ts
          IF ( channel_loss_flag==ACTIVE ) READ ( Restart_inunit, * ) Dead_storage
        ENDIF
      ENDIF
      END SUBROUTINE muskingum_restart
