!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a relation between solar radiation and cloud cover.
! Declared Parameters
!     ccov_slope, ccov_intcp, radj_sppt, radj_wppt, basin_solsta
!     crad_coef, crad_exp, radmax, ppt_rad_adj, rad_conv, hru_solsta
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 in
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_CCSOLRAD
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE :: Observed_flag
        CHARACTER(LEN=8), SAVE :: MODNAME
        ! Declared Parameters
        REAL, SAVE :: Crad_coef, Crad_exp, Radmax, Radj_sppt, Radj_wppt
        REAL, SAVE :: Ccov_slope(12), Ccov_intcp(12), Ppt_rad_adj(12)
      END MODULE PRMS_CCSOLRAD
!***********************************************************************
      INTEGER FUNCTION ccsolrad()
      USE PRMS_CCSOLRAD
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru, Nsol
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, &
     &    Basin_area_inv, NEARZERO, Hru_order_flag
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Solrad_tmin, Solrad_tmax, &
     &    Rad_conv, Hru_solsta, Basin_horad, Basin_potsw, Basin_solsta, Orad, Basin_obs_ppt
      USE PRMS_CHECK_NHRU_PARAMS, ONLY: Solsta_flag
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl
      USE PRMS_SET_TIME, ONLY: Jday, Nowmonth, Summer_flag
      USE PRMS_OBS, ONLY: Solrad
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL :: read_error, print_module, print_date
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
          ELSEIF ( Summer_flag==1 ) THEN
            pptadj = Radj_sppt
          ELSE
            pptadj = Radj_wppt ! Winter
          ENDIF

          radadj = Crad_coef + (1.0-Crad_coef)*((1.0-ccov)**Crad_exp)
          IF ( radadj>Radmax ) radadj = Radmax
          radadj = radadj*pptadj
          Orad = radadj*Basin_horad
        ENDIF

        Basin_potsw = 0.0D0
        IF ( Solsta_flag==0 ) THEN
          IF ( Hru_order_flag==1 ) THEN
            DO j = 1, Nhru
              Swrad(j) = Soltab_potsw(Jday, j)/Basin_horad*Orad/Hru_cossl(j)
              Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
            ENDDO
          ELSE
            DO jj = 1, Active_hrus
              j = Hru_route_order(jj)
              Swrad(j) = Soltab_potsw(Jday, j)/Basin_horad*Orad/Hru_cossl(j)
              Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
            ENDDO
          ENDIF
        ELSE
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            k = Hru_solsta(j)
            IF ( k>0 ) THEN
              IF ( Solrad(k)<0.0 .OR. Solrad(k)>10000.0 ) THEN
                IF ( Print_debug>-1 ) THEN
                  PRINT *, 'WARNING, measured solar radiation missing, HRU:', j, '; station:', k, '; value computed'
                  CALL print_date(1)
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
        Version_ccsolrad = '$Id: ccsolrad.f90 7115 2015-01-06 00:09:15Z rsregan $'
        CALL print_module(Version_ccsolrad, 'Solar Radiation Distribution', 90)
        MODNAME = 'ccsolrad'

! Declare Parameters
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
     &       'Coefficient(B) in Thompson(1976) equation;' // &
     &       ' varies by region, contour map of values in reference', &
     &       'none')/=0 ) CALL read_error(1, 'crad_coef')
        IF ( declparam(MODNAME, 'crad_exp', 'one', 'real', &
     &       '0.61', '0.2', '0.8', &
     &       'Exponent in cloud cover-solar radiation relationship', &
     &       'Exponent(P) in Thompson(1976) equation', &
     &       'none')/=0 ) CALL read_error(1, 'crad_exp')
        IF ( declparam(MODNAME, 'ppt_rad_adj', 'nmonths', 'real', &
     &       '0.02', '0.0', '0.5', &
     &       'Radiation reduced if basin precipitation above this value', &
     &       'Monthly minimum precipitation, if basin precipitation exceeds this value, radiation is'// &
     &       ' multiplied by radj_sppt or radj_wppt adjustment factor', &
     &       'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')
        IF ( declparam(MODNAME, 'radmax', 'one', 'real', &
     &       '0.8', '0.1', '1.0', &
     &       'Maximum fraction of potential solar radiation', &
     &       'Maximum fraction of the potential solar radiation'// &
     &       ' that may reach the ground due to haze, dust, smog, and so forth', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radmax')
        IF ( declparam(MODNAME, 'radj_sppt', 'one', 'real', &
     &       '0.44', '0.0', '1.0', &
     &       'Adjustment to solar radiation on precipitation day - summer', &
     &       'Adjustment factor for computed solar radiation for summer day with greater than'// &
     &       ' ppt_rad_adj inches of precipitation', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radj_sppt')
        IF ( declparam(MODNAME, 'radj_wppt', 'one', 'real', &
     &       '0.5', '0.0', '1.0', &
     &       'Adjustment to solar radiation on precipitation day - winter', &
     &       'Adjustment factor for computed solar radiation for winter day with greater than'// &
     &       ' ppt_rad_adj inches of precipitation', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radj_wppt')

      ELSEIF ( Process(:4)=='init' ) THEN
! Get parameters
        IF ( getparam(MODNAME, 'ccov_slope', 12, 'real', Ccov_slope)/=0 ) CALL read_error(2, 'ccov_slope')
        IF ( getparam(MODNAME, 'ccov_intcp', 12, 'real', Ccov_intcp)/=0 ) CALL read_error(2, 'ccov_intcp')
        IF ( getparam(MODNAME, 'crad_coef', 1, 'real', Crad_coef)/=0 ) CALL read_error(2, 'crad_coef')
        IF ( getparam(MODNAME, 'crad_exp', 1, 'real', Crad_exp)/=0 ) CALL read_error(2, 'crad_exp')
        IF ( getparam(MODNAME, 'ppt_rad_adj', 12, 'real', Ppt_rad_adj)/=0 ) CALL read_error(2, 'ppt_rad_adj')
        IF ( getparam(MODNAME, 'radmax', 1, 'real', Radmax)/=0 ) CALL read_error(2, 'radmax')
        IF ( getparam(MODNAME, 'radj_sppt', 1, 'real', Radj_sppt)/=0 ) CALL read_error(2, 'radj_sppt')
        IF ( getparam(MODNAME, 'radj_wppt', 1, 'real', Radj_wppt)/=0 ) CALL read_error(2, 'radj_wppt')

        Observed_flag = 0
        IF ( Nsol>0 .AND. Basin_solsta>0 ) Observed_flag = 1

      ENDIF

      END FUNCTION ccsolrad
