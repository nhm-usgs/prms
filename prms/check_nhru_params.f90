!***********************************************************************
! checks values of basin wide parameters
!***********************************************************************
      MODULE PRMS_CHECK_NHRU_PARAMS
      IMPLICIT NONE
!   Local Variables
      !CHARACTER(LEN=17), SAVE :: MODNAME
      INTEGER, SAVE :: Solsta_flag
      END MODULE PRMS_CHECK_NHRU_PARAMS

!***********************************************************************
!     Main check_nhru_params routine
!***********************************************************************
      SUBROUTINE check_nhru_params()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      EXTERNAL :: check_nhru_params_init
!***********************************************************************
      IF ( Process(:4)=='init' ) CALL check_nhru_params_init()
      END SUBROUTINE check_nhru_params

!***********************************************************************
!     check_nhru_params_init - Check some basin-wide parameters and
!                              compute some basin variable
!***********************************************************************
      SUBROUTINE check_nhru_params_init()
      USE PRMS_CHECK_NHRU_PARAMS
      USE PRMS_MODULE, ONLY: Temp_flag, Ntemp, Nsol, Nevap, Parameter_check_flag, &
     &    Print_debug, Inputerror_flag
      USE PRMS_BASIN, ONLY: Hru_type, Active_hrus, Hru_route_order, SMALLPARAM, &
     &    Cov_type, Covden_win, Covden_sum
      USE PRMS_CLIMATEVARS, ONLY: Hru_tsta, Hru_solsta, Hru_pansta, Use_pandata
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_rechr_max, Sat_threshold
      IMPLICIT NONE
! Functions
      EXTERNAL :: checkdim_param_limits, check_param_zero, check_param_value
! Local variables
      INTEGER :: i, j, check_tsta, ierr
!***********************************************************************
      check_tsta = 0
      IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==4 ) check_tsta = 1

      Solsta_flag = 0
      ! Sanity checks for parameters
      DO j = 1, Active_hrus
        i = Hru_route_order(j)

        IF ( check_tsta==1 ) CALL checkdim_param_limits(i, 'hru_tsta', 'ntemp', Hru_tsta(i), 1, Ntemp, Inputerror_flag)

        IF ( Nsol>0 ) THEN
          ierr = 0
          CALL checkdim_param_limits(i, 'hru_solsta', 'nsol', Hru_solsta(i), 0, Nsol, ierr)
          IF ( ierr==0 ) THEN
            IF ( Hru_solsta(i)>0 ) Solsta_flag = 1
          ELSE
            Inputerror_flag = 1
          ENDIF
        ENDIF

        IF ( Use_pandata==1 ) CALL checkdim_param_limits(i, 'hru_pansta', 'nevap', Hru_pansta(i), 1, Nevap, Inputerror_flag)

        CALL check_param_value(i, 'covden_win', Covden_win(i), Inputerror_flag)
        CALL check_param_value(i, 'covden_sum', Covden_sum(i), Inputerror_flag)
        CALL checkint_param_limits(i, 'cov_type', Cov_type(i), 0, 4, Inputerror_flag)

        IF ( Hru_type(i)==2 ) THEN
          IF ( Cov_type(i)/=0 ) THEN
            IF ( Parameter_check_flag>0 ) THEN
              PRINT *, 'ERROR, cov_type value must be 0 for lake HRU:', i, Cov_type(i)
              Inputerror_flag = 1
            ELSE
              IF ( Print_debug>-1 ) PRINT *,  'WARNING, cov_type must be 0 for lakes, reset from:', Cov_type(i), ' to 0 for HRU:', i
              Cov_type(i) = 0
            ENDIF
          ENDIF
          CYCLE
        ENDIF

        CALL check_param_zero(i, 'sat_threshold', Sat_threshold(i), ierr)
        IF ( Soil_moist_max(i)<SMALLPARAM ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT *, 'ERROR, soil_moist_max value <', SMALLPARAM, ' for HRU:', i, ', value:', Soil_moist_max(i)
            Inputerror_flag = 1
          ELSE
            PRINT 9001, 'soil_moist_max', SMALLPARAM, i, Soil_moist_max(i), SMALLPARAM
            Soil_moist_max(i) = SMALLPARAM
          ENDIF
        ENDIF
        IF ( Soil_rechr_max(i)<SMALLPARAM ) THEN
          IF ( Parameter_check_flag>0 ) THEN
            PRINT *, 'ERROR, soil_rechr_max value <', SMALLPARAM, ' for HRU:', i, ', value:', Soil_rechr_max(i)
            Inputerror_flag = 1
          ELSE
            PRINT 9001, 'soil_rechr_max', SMALLPARAM, i, Soil_rechr_max(i), SMALLPARAM
            Soil_rechr_max(i) = SMALLPARAM
          ENDIF
        ENDIF

      ENDDO

 9001 FORMAT (/, 'WARNING, ', A, ' <', F7.4, ' for HRU:', I7, /, 9X, 'value:', F10.7, ' set to', F7.4)

      END SUBROUTINE check_nhru_params_init
