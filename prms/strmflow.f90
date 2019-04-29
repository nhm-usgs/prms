!***********************************************************************
! Computes daily streamflow as the sum of surface runoff,
! shallow-subsurface flow (interflow), and ground-water flow
!***********************************************************************
      INTEGER FUNCTION strmflow()
      USE PRMS_MODULE, ONLY: Process
      USE PRMS_BASIN, ONLY: Basin_area_inv, CFS2CMS_CONV
      USE PRMS_GWFLOW, ONLY: Basin_gwflow
      USE PRMS_FLOWVARS, ONLY: Basin_sroff, Basin_ssflow, Basin_cfs, &
          Basin_cms, Basin_stflow_in, Basin_sroff_cfs, Basin_ssflow_cfs, &
          Basin_gwflow_cfs, Basin_stflow_out
      USE PRMS_OBS, ONLY: Cfs_conv
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule
! Local Variables
      INTEGER :: i, nc
      DOUBLE PRECISION :: area_fac
      CHARACTER(LEN=8), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_strmflow
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Streamflow Routing'
!***********************************************************************
      strmflow = 0

      IF ( Process(:3)=='run' ) THEN
!   Compute daily flow.
        area_fac = Cfs_conv/Basin_area_inv
! markstro out equals in because there is no routing
        Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow
        Basin_stflow_out = Basin_stflow_in
        Basin_cfs = Basin_stflow_in*area_fac
        Basin_cms = Basin_cfs*CFS2CMS_CONV
        Basin_sroff_cfs = Basin_sroff*area_fac
        Basin_ssflow_cfs = Basin_ssflow*area_fac
        Basin_gwflow_cfs = Basin_gwflow*area_fac

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_strmflow = '$Id: strmflow.f90 5169 2012-12-28 23:51:03Z rsregan $'
        nc = INDEX( Version_strmflow, 'Z' )
        i = INDEX ( Version_strmflow, '.f90' ) + 3
        IF ( declmodule(Version_strmflow(6:i), PROCNAME, Version_strmflow(i+2:nc))/=0 ) STOP
        MODNAME = 'strmflow'
      ENDIF

      END FUNCTION strmflow
