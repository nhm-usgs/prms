!***********************************************************************
! Computes cloud cover fraction
! Declared Parameters: ccov_slope, ccov_intcp
!***********************************************************************
      MODULE PRMS_CLOUD_COVER
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=11), SAVE :: MODNAME
        ! Declared Variable
        DOUBLE PRECISION, SAVE :: Basin_cloud_cover
        REAL, SAVE, ALLOCATABLE :: Cloud_cover_hru(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Ccov_slope(:, :), Ccov_intcp(:, :)
      END MODULE PRMS_CLOUD_COVER
!***********************************************************************
      INTEGER FUNCTION cloud_cover()
      USE PRMS_CLOUD_COVER
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file, Init_vars_from_file
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Tmax_hru, Tmin_hru
      USE PRMS_SET_TIME, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam, declvar
      EXTERNAL read_error, cloud_cover_restart, print_module
! Local Variables
      INTEGER :: j, jj
      REAL :: ccov
      CHARACTER(LEN=80), SAVE :: Version_cloud_cover
!***********************************************************************
      cloud_cover = 0

      IF ( Process(:3)=='run' ) THEN
        Basin_cloud_cover = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          ccov = Ccov_slope(j, Nowmonth)*(Tmax_hru(j)-Tmin_hru(j)) + Ccov_intcp(j, Nowmonth)
          IF ( ccov<NEARZERO ) THEN
            ccov = 0.0
          ELSEIF ( ccov>1.0 ) THEN
            ccov = 1.0
          ENDIF
          Cloud_cover_hru(j) = ccov
          Basin_cloud_cover = Basin_cloud_cover + ccov*Hru_area(j)
        ENDDO
        Basin_cloud_cover = Basin_cloud_cover*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_cloud_cover = '$Id: cloud_cover.f90 7184 2015-01-29 20:47:29Z rsregan $'
        CALL print_module(Version_cloud_cover, 'Cloud Cover                 ', 90)
        MODNAME = 'cloud_cover'

        ALLOCATE ( Cloud_cover_hru(Nhru) )
        IF ( declvar(MODNAME, 'cloud_cover_hru', 'nhru', Nhru, 'real', &
     &       'Cloud cover proportion of each HRU', &
     &       'decimal fraction', Cloud_cover_hru)/=0 ) CALL read_error(3, 'cloud_cover_hru')

        IF ( declvar(MODNAME, 'basin_cloud_cover', 'one', 1, 'double', &
     &       'Basin area-weighted average cloud cover proportion', &
     &       'decimal fraction', Basin_cloud_cover)/=0 ) CALL read_error(3, 'basin_cloud_cover')

        ! Declare Parameters
        ALLOCATE ( Ccov_slope(Nhru,12) )
        IF ( declparam(MODNAME, 'ccov_slope', 'nhru,nmonths', 'real', &
     &       '-0.13', '-0.5', '-0.01', &
     &       'Slope in temperature cloud cover relationship', &
     &       'Monthly (January to December) coefficient in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_slope')

        ALLOCATE ( Ccov_intcp(Nhru,12) )
        IF ( declparam(MODNAME, 'ccov_intcp', 'nhru,nmonths', 'real', &
     &       '1.83', '0.0', '5.0', &
     &       'Intercept in temperature cloud cover relationship', &
     &       'Monthly (January to December) intercept in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_intcp')

      ELSEIF ( Process(:4)=='init' ) THEN
! Get parameters
        IF ( Init_vars_from_file==1 ) THEN
          CALL cloud_cover_restart(1)
        ELSE
          Cloud_cover_hru = 0.0
          Basin_cloud_cover = 0.0D0
        ENDIF

        IF ( getparam(MODNAME, 'ccov_slope', Nhru*12, 'real', Ccov_slope)/=0 ) CALL read_error(2, 'ccov_slope')
        IF ( getparam(MODNAME, 'ccov_intcp', Nhru*12, 'real', Ccov_intcp)/=0 ) CALL read_error(2, 'ccov_intcp')

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL cloud_cover_restart(0)
      ENDIF

      END FUNCTION cloud_cover

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE cloud_cover_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_CLOUD_COVER
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_cloud_cover
        WRITE ( Restart_outunit ) Cloud_cover_hru
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_cloud_cover
        READ ( Restart_inunit ) Cloud_cover_hru
      ENDIF
      END SUBROUTINE cloud_cover_restart
