!***********************************************************************
! Computes daily streamflow as the sum of surface runoff,
! shallow-subsurface flow (interflow), and ground-water flow
!***********************************************************************
      INTEGER FUNCTION strmflow()
      USE PRMS_MODULE, ONLY: Process, Print_debug, Version_strmflow, Strmflow_nc
      USE PRMS_BASIN, ONLY: Basin_area_inv, CFS2CMS_CONV, Basin_cfs, &
          Basin_cms, Basin_stflow, Basin_sroff_cfs, Basin_ssflow_cfs, Basin_gwflow_cfs
      USE PRMS_GWFLOW, ONLY: Basin_gwflow
      USE PRMS_FLOWVARS, ONLY: Basin_sroff, Basin_ssflow
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule
! Local Variables
      DOUBLE PRECISION :: area_fac
!***********************************************************************
      strmflow = 1

      IF ( Process(:3)=='run' ) THEN
!   Compute daily flow.
        area_fac = Cfs_conv/Basin_area_inv
        Basin_stflow = Basin_sroff + Basin_gwflow + Basin_ssflow
        Basin_cfs = Basin_stflow*area_fac
        Basin_cms = Basin_cfs*CFS2CMS_CONV
        Basin_sroff_cfs = Basin_sroff*area_fac
        Basin_ssflow_cfs = Basin_ssflow*area_fac
        Basin_gwflow_cfs = Basin_gwflow*area_fac

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_strmflow = '$Id: strmflow.f90 3805 2011-10-25 17:13:04Z rsregan $'
        Strmflow_nc = INDEX( Version_strmflow, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_strmflow(:Strmflow_nc))/=0 ) STOP
        ENDIF
      ENDIF

      strmflow = 0
      END FUNCTION strmflow

