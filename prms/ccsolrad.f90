!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a relation between solar radiation and cloud
! cover.
! Declared Parameters
!     ccov_slope, ccov_intcp, radj_sppt, radj_wppt, basin_solsta
!     crad_coef, crad_exp, radmax, ppt_rad_adj, rad_conv, hru_solsta
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 is
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_CCSOLRAD
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=8), SAVE :: MODNAME
        INTEGER, SAVE :: Observed_flag
        ! Declared Parameters
        REAL, SAVE :: Crad_coef, Crad_exp
        REAL, SAVE :: Ccov_slope(12), Ccov_intcp(12)
      END MODULE PRMS_CCSOLRAD
!***********************************************************************
      INTEGER FUNCTION ccsolrad()
      USE PRMS_CCSOLRAD
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru, Nsol, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, NEARZERO, Hemisphere, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Solrad_tmin, Solrad_tmax, &
     &    Ppt_rad_adj, Rad_conv, Hru_solsta, Basin_horad, Radmax, &
     &    Basin_potsw, Basin_solsta, Radj_sppt, Radj_wppt, Orad, Basin_obs_ppt
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl
      USE PRMS_OBS, ONLY: Solrad, Nowtime, Jday, Nowmonth
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, ccsolrad_restart
! Local Variables
      INTEGER :: j, jj, k
      REAL :: pptadj, ccov, radadj
      CHARACTER(LEN=80), SAVE :: Version_ccsolrad
!***********************************************************************
      ccsolrad = 0

      IF ( Process(:3)=='run' ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)

        Orad = -999.0
        IF ( Observed_flag==1 ) Orad = Solrad(Basin_solsta)*Rad_conv

        IF ( Orad<0.0 .OR. Orad>10000.0 ) THEN
          ccov = Ccov_slope(Nowmonth)*(Solrad_tmax-Solrad_tmin) + Ccov_intcp(Nowmonth)
          IF ( ccov<NEARZERO ) THEN
            ccov = 0.0
          ELSEIF ( ccov>1.0 ) THEN
            ccov = 1.0
          ENDIF

          IF ( Basin_obs_ppt<=Ppt_rad_adj(Nowmonth) ) THEN
            pptadj = 1.0
          ELSE
            pptadj = Radj_sppt
            IF ( Hemisphere==0 ) THEN ! Northern Hemisphere
              IF ( Jday<79 .OR. Jday>265 ) pptadj = Radj_wppt ! Winter
            ELSE ! Southern Hemisphere
              IF ( Jday>79 .OR. Jday<265 ) pptadj = Radj_wppt ! Winter
            ENDIF
          ENDIF

          radadj = Crad_coef + (1.0-Crad_coef)*((1.0-ccov)**Crad_exp)
          IF ( radadj>Radmax ) radadj = Radmax
          radadj = radadj*pptadj
          Orad = radadj*Basin_horad
        ENDIF

        Basin_potsw = 0.0D0
        IF ( Nsol==0 ) THEN
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            Swrad(j) = Soltab_potsw(Jday, j)/Basin_horad*Orad/Hru_cossl(j)
            Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
          ENDDO
        ELSE
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            k = Hru_solsta(j)
            IF ( k>0 ) THEN
              IF ( Solrad(k)<0.0 .OR. Solrad(k)>10000.0 ) THEN
                IF ( Print_debug>-1 ) THEN
                  PRINT *, 'Warning, measured solar radiation missing for HRU:', j, ' Solar radiation station:', k
                  PRINT *, ' Value computed; date:', Nowtime
                ENDIF
              ELSE
                Swrad(j) = Solrad(k)*Rad_conv
                Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
                CYCLE
              ENDIF
            ENDIF
            Swrad(j) = Soltab_potsw(Jday, j)/Basin_horad*Orad/Hru_cossl(j)
            Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
          ENDDO
        ENDIF
        Basin_potsw = Basin_potsw*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_ccsolrad = '$Id: ccsolrad.f90 5592 2013-04-23 18:26:23Z rsregan $'
        CALL print_module(Version_ccsolrad, 'Solar Radiation           ', 90)
        MODNAME = 'ccsolrad'

! Declare Parameters

        IF ( Timestep/=0 ) RETURN

        IF ( declparam(MODNAME, 'ccov_slope', 'nmonths', 'real', &
     &       '-0.13', '-0.5', '-0.01', &
     &       'Slope in temperature cloud cover relationship', &
     &       'Monthly (January to December) coefficient in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_slope')
        IF ( declparam(MODNAME, 'ccov_intcp', 'nmonths', 'real', &
     &       '1.83', '0.0', '5.0', &
     &       'Intercept in temperature cloud cover relationship', &
     &       'Monthly (January to December) intercept in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_intcp')
        IF ( declparam(MODNAME, 'crad_coef', 'one', 'real', &
     &       '0.4', '0.1', '0.7', &
     &       'Coefficient in cloud cover-solar radiation relationship', &
     &       'Coefficient(B) in Thompson(1976) equation,' // &
     &       ' Solar radiation = B + (1.0-B)*(1.0-cloudcover)**P' // &
     &       ' Varies by region, contour map of values in reference', &
     &       'none')/=0 ) CALL read_error(1, 'crad_coef')
        IF ( declparam(MODNAME, 'crad_exp', 'one', 'real', &
     &       '0.61', '0.2', '0.8', &
     &       'Exponent in cloud cover-solar radiation relationship', &
     &       'Exponent(P) in Thompson(1976) equation: Solar radiation = B + (1.0-B)*(1.0-cloudcover)**P', &
     &       'none')/=0 ) CALL read_error(1, 'crad_exp')

      ELSEIF ( Process(:4)=='init' ) THEN
! Get parameters
        IF ( Timestep/=0 ) THEN
          CALL ccsolrad_restart(1)
          RETURN
        ENDIF

        IF ( getparam(MODNAME, 'ccov_slope', 12, 'real', Ccov_slope)/=0 ) CALL read_error(2, 'ccov_slope')
        IF ( getparam(MODNAME, 'ccov_intcp', 12, 'real', Ccov_intcp)/=0 ) CALL read_error(2, 'ccov_intcp')
        IF ( getparam(MODNAME, 'crad_coef', 1, 'real', Crad_coef)/=0 ) CALL read_error(2, 'crad_coef')
        IF ( getparam(MODNAME, 'crad_exp', 1, 'real', Crad_exp)/=0 ) CALL read_error(2, 'crad_exp')
        Observed_flag = 0
        IF ( Nsol>0 .AND. Basin_solsta>0 ) Observed_flag = 1

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL ccsolrad_restart(0)
      ENDIF

      END FUNCTION ccsolrad

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE ccsolrad_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_CCSOLRAD
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=8) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Observed_flag
        WRITE ( Restart_outunit ) Ccov_slope
        WRITE ( Restart_outunit ) Ccov_intcp
        WRITE ( Restart_outunit ) Crad_coef
        WRITE ( Restart_outunit ) Crad_exp
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Observed_flag
        READ ( Restart_inunit ) Ccov_slope
        READ ( Restart_inunit ) Ccov_intcp
        READ ( Restart_inunit ) Crad_coef
        READ ( Restart_inunit ) Crad_exp
      ENDIF
      END SUBROUTINE ccsolrad_restart

