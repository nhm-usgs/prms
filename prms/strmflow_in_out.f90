!***********************************************************************
! Routes water between segments in the system using inflow equals outflow
!***********************************************************************
      MODULE PRMS_STRMFLOW_IN_OUT
        IMPLICIT NONE
!   Local Variables
        DOUBLE PRECISION, SAVE :: Flow_out
        CHARACTER(LEN=17), SAVE :: MODNAME
      END MODULE PRMS_STRMFLOW_IN_OUT

!***********************************************************************
!     Main musroute routine
!***********************************************************************
      INTEGER FUNCTION strmflow_in_out()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: strminout_decl, strminout_run
      EXTERNAL :: strmflow_in_out_restart
!***********************************************************************
      strmflow_in_out = 0

      IF ( Process(:3)=='run' ) THEN
        strmflow_in_out  = strminout_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        strmflow_in_out  = strminout_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) CALL strmflow_in_out_restart(1)
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL strmflow_in_out_restart(0)
      ENDIF

      END FUNCTION strmflow_in_out

!***********************************************************************
!     strminout_decl - set up fixed routing parameters
!   Declared Parameters: tosegment, hru_segment
!***********************************************************************
      INTEGER FUNCTION strminout_decl()
      USE PRMS_STRMFLOW_IN_OUT
      USE PRMS_MODULE, ONLY: Nsegment, Model
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_strmflow_in_out
!***********************************************************************
      strminout_decl = 0

      Version_strmflow_in_out = '$Id: strmflow_in_out.f90 5532 2013-03-25 21:49:54Z rsregan $'
      CALL print_module(Version_strmflow_in_out, 'Streamflow Routing        ', 90)
      MODNAME = 'strmflow_in_out'

      IF ( Nsegment<1 ) THEN
        IF ( Model==99 ) THEN
          Nsegment = 1
        ELSE
          PRINT *, 'ERROR, strmflow_in_out module requires nsegment > 0'
          PRINT *, 'nsegment specified as:', Nsegment
          STOP
        ENDIF
      ENDIF

      END FUNCTION strminout_decl

!***********************************************************************
!     strmflwinout_run - Compute flows/fluxes going to and out of each segment
!***********************************************************************
      INTEGER FUNCTION strminout_run()
      USE PRMS_STRMFLOW_IN_OUT
      USE PRMS_MODULE, ONLY: Nsegment, Nsegmentp1
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Hru_area, Basin_area_inv, Hru_route_order, Active_hrus, DNEARZERO, &
     &    Order, Tosegment, Segment_hruarea, Obsin_segment, Hru_segment
      USE PRMS_CLIMATEVARS, ONLY: Swrad
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Ssres_flow, Sroff, Hru_outflow, &
     &    Basin_cms, Basin_gwflow_cfs, Basin_ssflow_cfs, Basin_stflow_out, Basin_stflow_in, Basin_cfs, Basin_sroff_cfs, &
     &    Seginc_gwflow, Seginc_ssflow, Seginc_sroff, Seginc_swrad, Seg_lateral_inflow, Seg_inflow, Seg_outflow, Seg_upstream_inflow
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Cfs_conv, Nowtime, Streamflow_cfs
      USE PRMS_SRUNOFF, ONLY: Basin_sroff
      USE PRMS_GWFLOW, ONLY: Gwres_flow, Basin_gwflow
      IMPLICIT NONE
      INTRINSIC DBLE
! Local Variables
      INTEGER :: i, j, jj, iorder, toseg
      DOUBLE PRECISION :: area_fac, tocfs
!***********************************************************************
      strminout_run = 0

      Seginc_gwflow = 0.0
      Seginc_ssflow = 0.0
      Seginc_sroff = 0.0
      Seginc_swrad = 0.0
      Seg_lateral_inflow = 0.0
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

      DO i = 1, Nsegment
        IF ( Segment_hruarea(i)>DNEARZERO ) THEN
          Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
        ELSE ! IF ( Segment_hruarea(i)<DNEARZERO ) THEN
          IF ( Tosegment(i)>0 .AND. Tosegment(i)<Nsegmentp1 ) THEN
            Seginc_swrad(i) = Seginc_swrad(Tosegment(i))
          ELSEIF ( i>1 ) THEN
            Seginc_swrad(i) = Seginc_swrad(i-1)
          ENDIF
        ENDIF
      ENDDO

      Seg_inflow = 0.0
      Seg_outflow = 0.0
      Seg_upstream_inflow = 0.0
      Flow_out = 0.0D0
      DO i = 1, Nsegment
        iorder = Order(i)
        toseg = Tosegment(iorder)
        IF ( Obsin_segment(iorder)>0 ) Seg_upstream_inflow(iorder) = Streamflow_cfs(Obsin_segment(iorder))
        Seg_inflow(iorder) = Seg_upstream_inflow(iorder) + Seg_lateral_inflow(iorder)
        Seg_outflow(iorder) = Seg_inflow(iorder)
        IF ( toseg==Nsegmentp1 ) THEN
          Flow_out = Flow_out + DBLE( Seg_outflow(iorder) )
          CYCLE
        ELSE
          Seg_upstream_inflow(toseg) = Seg_upstream_inflow(toseg) + Seg_outflow(iorder)
        ENDIF
      ENDDO

      Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow
!      Basin_cfs = Segment_cfs(Final_segment)
      Basin_cfs = Flow_out
      area_fac = Cfs_conv/Basin_area_inv
      Basin_stflow_out = Basin_cfs/area_fac
      Basin_cms = Basin_cfs*CFS2CMS_CONV

      area_fac = Cfs_conv/Basin_area_inv
      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac

      END FUNCTION strminout_run

!***********************************************************************
!     strmflow_in_out_restart - write or read strmflow_in_out restart file
!***********************************************************************
      SUBROUTINE strmflow_in_out_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_STRMFLOW_IN_OUT
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=17) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Flow_out
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Flow_out
      ENDIF
      END SUBROUTINE strmflow_in_out_restart
