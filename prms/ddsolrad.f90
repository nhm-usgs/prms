!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a maximum temperature per degree-day relation;
! Declared Parameters
!     dday_slope, dday_intcp, radj_sppt, radj_wppt, basin_solsta
!     radadj_slope, radadj_intcp, radmax, ppt_rad_adj, rad_conv
!     tmax_index, tmax_allrain, hru_solsta
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 in
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_DDSOLRAD
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE :: Observed_flag
        CHARACTER(LEN=8), SAVE :: MODNAME
        ! Declared Parameters
        REAL, SAVE :: Radadj_slope, Radadj_intcp
        REAL, SAVE :: Radmax, Radj_sppt, Radj_wppt
        REAL, SAVE :: Dday_slope(12), Dday_intcp(12), Tmax_index(12), Ppt_rad_adj(12)
      END MODULE PRMS_DDSOLRAD

      INTEGER FUNCTION ddsolrad()
      USE PRMS_DDSOLRAD
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru, Nsol
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, &
     &    Basin_area_inv, Hru_order_flag
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Solrad_tmax, Tmax_allrain, &
     &    Rad_conv, Hru_solsta, Basin_horad, Basin_potsw, Basin_solsta, Orad, Basin_obs_ppt
      USE PRMS_CHECK_NHRU_PARAMS, ONLY: Solsta_flag
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl
      USE PRMS_SET_TIME, ONLY: Jday, Nowmonth, Summer_flag
      USE PRMS_OBS, ONLY: Solrad
      IMPLICIT NONE
! Functions
      INTRINSIC INT, FLOAT
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL :: read_error, print_module, print_date
! Local Variables
      INTEGER :: j, jj, k, kp, kp1
      REAL :: pptadj, radadj, dday, ddayi
! Save Variables
      CHARACTER(LEN=80), SAVE :: Version_ddsolrad
      REAL, SAVE, DIMENSION(26) :: solf
      DATA solf/.20, .35, .45, .51, .56, .59, .62, .64, .655, .67, .682, &
     &          .69, .70, .71, .715, .72, .722, .724, .726, .728, .73, &
     &          .734, .738, .742, .746, .75/
!***********************************************************************
      ddsolrad = 0

      IF ( Process(:3)=='run' ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)

        Orad = -999.0
        IF ( Observed_flag==1 ) Orad = Solrad(Basin_solsta)*Rad_conv

        ! set degree day and radiation adjustment limited by radmax
        IF ( Orad<0.0 .OR. Orad>10000.0 ) THEN
          dday = Dday_slope(Nowmonth)*Solrad_tmax + Dday_intcp(Nowmonth) + 1.0
          IF ( dday<1.0 ) dday = 1.0
          IF ( dday<26.0 ) THEN
            kp = INT(dday)
            ddayi = FLOAT(kp)
            kp1 = kp + 1
            radadj = solf(kp) + ((solf(kp1)-solf(kp))*(dday-ddayi))
            IF ( radadj>Radmax ) radadj = Radmax
          ELSE
            radadj = Radmax
          ENDIF

          ! Set precipitation adjument factor based on temperature
          ! and amount of precipitation
          pptadj = 1.0
          IF ( Basin_obs_ppt>Ppt_rad_adj(Nowmonth) ) THEN
            IF ( Solrad_tmax<Tmax_index(Nowmonth) ) THEN
              pptadj = Radj_sppt
              IF ( Solrad_tmax>=Tmax_allrain(Nowmonth) ) THEN
                IF ( Summer_flag==0 ) pptadj = Radj_wppt ! Winter
              ELSE
                pptadj = Radj_wppt
              ENDIF
            ELSE
              pptadj = Radadj_intcp + Radadj_slope*(Solrad_tmax-Tmax_index(Nowmonth))
              IF ( pptadj>1.0 ) pptadj = 1.0
            ENDIF
          ENDIF

          radadj = radadj*pptadj
          IF ( radadj<0.2 ) radadj = 0.2
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
        Version_ddsolrad = '$Id: ddsolrad.f90 6899 2014-10-29 15:57:54Z rsregan $'
        CALL print_module(Version_ddsolrad, 'Solar Radiation Distribution', 90)
        MODNAME = 'ddsolrad'

        ! Declare Parameters
        IF ( declparam(MODNAME, 'dday_slope', 'nmonths', 'real', &
     &       '0.4', '0.2', '0.9', &
     &       'Slope in temperature degree-day relationship', &
     &       'Monthly (January to December) slope in degree-day equation', &
     &       'dday/temp_units')/=0 ) CALL read_error(1, 'dday_slope')
        IF ( declparam(MODNAME, 'dday_intcp', 'nmonths', 'real', &
     &       '-40.0', '-60.0', '10.0', &
     &       'Intercept in temperature degree-day relationship', &
     &       'Monthly (January to December) intercept in degree-day equation', &
     &       'dday')/=0 ) CALL read_error(1, 'dday_intcp')
        IF ( declparam(MODNAME, 'radadj_slope', 'one', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Slope in air temperature range adjustment to degree-day equation', &
     &       'Slope in air temperature range adjustment to degree-day equation', &
     &       'dday/temp_units')/=0 ) CALL read_error(1, 'radadj_slope')
        IF ( declparam(MODNAME, 'radadj_intcp', 'one', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Intercept in air temperature range adjustment to degree-day equation', &
     &       'Intercept in air temperature range adjustment to degree-day equation', &
     &       'dday')/=0 ) CALL read_error(1, 'radadj_intcp')
        IF ( declparam(MODNAME, 'tmax_index', 'nmonths', 'real', &
     &       '50.0', '-10.0', '110.0', &
     &       'Monthly index temperature', &
     &       'Monthly (January to December) index temperature used'// &
     &       ' to determine precipitation adjustments to solar radiation', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_index')
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
        IF ( getparam(MODNAME, 'dday_slope', 12, 'real', Dday_slope)/=0 ) CALL read_error(2, 'dday_slope')
        IF ( getparam(MODNAME, 'dday_intcp', 12, 'real', Dday_intcp)/=0 ) CALL read_error(2, 'dday_intcp')
        IF ( getparam(MODNAME, 'radadj_slope', 1, 'real', Radadj_slope)/=0 ) CALL read_error(2, 'radadj_slope')
        IF ( getparam(MODNAME, 'radadj_intcp', 1, 'real', Radadj_intcp)/=0 ) CALL read_error(2, 'radadj_intcp')
        IF ( getparam(MODNAME, 'tmax_index', 12, 'real', Tmax_index)/=0 ) CALL read_error(2, 'tmax_index')
        IF ( getparam(MODNAME, 'ppt_rad_adj', 12, 'real', Ppt_rad_adj)/=0 ) CALL read_error(2, 'ppt_rad_adj')
        IF ( getparam(MODNAME, 'radmax', 1, 'real', Radmax)/=0 ) CALL read_error(2, 'radmax')
        IF ( getparam(MODNAME, 'radj_sppt', 1, 'real', Radj_sppt)/=0 ) CALL read_error(2, 'radj_sppt')
        IF ( getparam(MODNAME, 'radj_wppt', 1, 'real', Radj_wppt)/=0 ) CALL read_error(2, 'radj_wppt')

        Observed_flag = 0
        IF ( Nsol>0 .AND. Basin_solsta>0 ) Observed_flag = 1

      ENDIF

      END FUNCTION ddsolrad
