!***********************************************************************
! Routes water between segments and lakes in the stream network
!
! gwflow goes to GWR instead of to the lake unless specified as
! going to stream segment associated with the lake, which would be a
! problem, thus gw_upslope usually goes to GWR under the lake,
! but is included in strm_seg_in if gwflow is associated with a stream
! segment, set in gwflow, 06/15/2009
!***********************************************************************
!     Main daily stream flow with lakes routine
!***********************************************************************
      INTEGER FUNCTION strmflow_lake()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: strmlkrun, strmlkdecl
!***********************************************************************
      strmflow_lake = 0

      IF ( Process(:3)=='run' ) THEN
        strmflow_lake = strmlkrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        strmflow_lake = strmlkdecl()
      ENDIF
      END FUNCTION strmflow_lake

!***********************************************************************
!     Declare module
!***********************************************************************
      INTEGER FUNCTION strmlkdecl()
      IMPLICIT NONE
! Functions
      EXTERNAL print_module
! Local Variables
!      CHARACTER(LEN=13), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_strmflow_lake
!***********************************************************************
      strmlkdecl = 0

      Version_strmflow_lake = 'strmflow_lake.f90 2015-01-29 20:36:25Z'
      CALL print_module(Version_strmflow_lake, 'Streamflow Routing          ', 90)
!      MODNAME = 'strmflow_lake'

      END FUNCTION strmlkdecl

!***********************************************************************
!     strmlkrun - Computes basin streamflow and on-channel reservoir
!                 storage and outflows
!***********************************************************************
      INTEGER FUNCTION strmlkrun()
      USE PRMS_MODULE, ONLY: Nsegment, Nlake
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Basin_area_inv, Lake_area, Segment_order, Tosegment, Obsin_segment
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_cms, Basin_gwflow_cfs, Basin_ssflow_cfs, &
     &    Basin_stflow_out, Basin_cfs, Basin_stflow_in, Basin_sroff_cfs, Flow_out, &
     &    Seg_lateral_inflow, Basin_lake_stor, Seg_inflow, Seg_outflow, Seg_upstream_inflow
      USE PRMS_LAKE_ROUTE, ONLY: Lake_id, Lake_inflow, Lake_type, Lake_outflow, Lake_outcfs, Obsout_lake, &
     &    Din1, Nsos, Lake_sto, Lake_coef, Lake_vol, Lake_invol, Lake_outvol, Lake_outcms, Lake_stream_in
      USE PRMS_ROUTING, ONLY: Segment_type
      USE PRMS_OBS, ONLY: Streamflow_cfs
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Basin_sroff
      USE PRMS_GWFLOW, ONLY: Basin_gwflow, Elevlake
      IMPLICIT NONE
! Functions
      EXTERNAL route_lake
! Local Variables
      INTEGER :: i, lakeid, iorder, toseg
      REAL :: area_fac
      !DOUBLE PRECISION :: last_stor
!***********************************************************************
      strmlkrun = 0

      !last_stor = Basin_lake_stor
      Basin_lake_stor = 0.0D0
      Seg_inflow = 0.0D0
      Seg_outflow = 0.0D0
      Seg_upstream_inflow = 0.0D0
      Flow_out = 0.0D0
      DO i = 1, Nsegment
        iorder = Segment_order(i)
        toseg = Tosegment(iorder)
! add up flows to stream segments
        IF ( Obsin_segment(iorder)>0 ) Seg_upstream_inflow(iorder) = Streamflow_cfs(Obsin_segment(iorder))
        Seg_inflow(iorder) = Seg_upstream_inflow(iorder) + Seg_lateral_inflow(iorder)
        Seg_outflow(iorder) = Seg_inflow(iorder)
! Flow_out is the total flow out of the basin, which allows for multiple outlets
        IF ( toseg==0 ) THEN ! lake cannot be an outlet to a basin
          Flow_out = Flow_out + Seg_outflow(iorder)
          CYCLE
        ELSE
          Seg_upstream_inflow(toseg) = Seg_upstream_inflow(toseg) + Seg_outflow(iorder)
        ENDIF ! compute lake routing
        IF ( Nlake>0 ) THEN
          IF ( Segment_type(iorder)==2 ) THEN
            lakeid = Lake_id(iorder)
            Lake_inflow(lakeid) = Lake_inflow(lakeid) + Seg_inflow(iorder)
            CALL route_lake(lakeid, Lake_type(lakeid), Lake_inflow(lakeid), Lake_outflow(lakeid), &
      &                     Lake_outcfs(lakeid), Obsout_lake(lakeid), Din1(lakeid), Nsos(lakeid), Lake_sto(lakeid), &
      &                     Lake_coef(lakeid), Lake_vol(lakeid), Lake_invol(lakeid), Lake_outvol(lakeid), &
      &                     Lake_area(lakeid), Elevlake(lakeid))
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

      END FUNCTION strmlkrun
