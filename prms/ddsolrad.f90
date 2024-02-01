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
        character(len=*), parameter :: MODDESC = 'Solar Radiation Distribution'
        character(len=*), parameter :: MODNAME = 'ddsolrad'
        character(len=*), parameter :: Version_ddsolrad = '2024-01-31'
        INTEGER, SAVE :: Observed_flag
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Radadj_slope(:, :), Radadj_intcp(:, :), Radmax(:, :)
        REAL, SAVE, ALLOCATABLE :: Dday_slope(:, :), Dday_intcp(:, :), Tmax_index(:, :)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ppt_rad_adj(:, :)
        DOUBLE PRECISION, SAVE :: Rad_conv
      END MODULE PRMS_DDSOLRAD

      INTEGER FUNCTION ddsolrad()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, DEBUG_less, Nmonths, ACTIVE, OFF
      USE PRMS_MODULE, ONLY: Process_flag, Print_debug, Nhru, Nsol, Nowmonth, Nhru_nmonths
      USE PRMS_DDSOLRAD
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area_dble, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Tmax_hru, Basin_orad, Orad_hru, &
     &    Hru_solsta, Basin_horad, Basin_potsw, Basin_swrad, Basin_solsta, Orad, Hru_ppt, Tmax_allrain, &
     &    Solsta_flag, Radj_sppt, Radj_wppt
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl, Soltab_horad_potsw
      USE PRMS_SET_TIME, ONLY: Jday, Summer_flag
      USE PRMS_OBS, ONLY: Solrad
      IMPLICIT NONE
! Functions
      INTRINSIC :: INT, FLOAT, DBLE, SNGL
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL :: read_error, print_module, print_date
! Local Variables
      INTEGER :: j, jj, k, kp, kp1
      REAL :: pptadj, radadj, dday, ddayi, Rad_conv_sngl
      REAL, ALLOCATABLE :: Ppt_rad_adj_sngl(:, :)
      REAL, SAVE, DIMENSION(26) :: solf
      DATA solf/.20, .35, .45, .51, .56, .59, .62, .64, .655, .67, .682, &
     &          .69, .70, .71, .715, .72, .722, .724, .726, .728, .73, &
     &          .734, .738, .742, .746, .75/
!***********************************************************************
      ddsolrad = 0

      IF ( Process_flag==RUN ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)
        Basin_swrad = 0.0D0
        Basin_orad = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)

          ! set degree day and radiation adjustment limited by radmax
          dday = Dday_slope(j, Nowmonth)*Tmax_hru(j) + Dday_intcp(j, Nowmonth) + 1.0
          IF ( dday<1.0 ) dday = 1.0
          IF ( dday<26.0 ) THEN
            kp = INT(dday)
            ddayi = FLOAT(kp)
            kp1 = kp + 1
            radadj = solf(kp) + ((solf(kp1)-solf(kp))*(dday-ddayi))
            IF ( radadj>Radmax(j,Nowmonth) ) radadj = Radmax(j, Nowmonth)
          ELSE
            radadj = Radmax(j, Nowmonth)
          ENDIF

          ! Set precipitation adjument factor based on temperature
          ! and amount of precipitation
          pptadj = 1.0
          IF ( Hru_ppt(j) > Ppt_rad_adj(j,Nowmonth) ) THEN
            IF ( Tmax_hru(j) < DBLE( Tmax_index(j,Nowmonth) ) ) THEN
              pptadj = Radj_sppt(j)
              IF ( Tmax_hru(j)>=Tmax_allrain(j,Nowmonth) ) THEN
                IF ( Summer_flag==OFF ) pptadj = Radj_wppt(j) ! Winter
              ELSE
                pptadj = Radj_wppt(j)
              ENDIF
            ELSE
              pptadj = Radadj_intcp(j, Nowmonth) + &
     &                 Radadj_slope(j, Nowmonth)*SNGL( (Tmax_hru(j) ) -Tmax_index(j,Nowmonth))
              IF ( pptadj>1.0 ) pptadj = 1.0
            ENDIF
          ENDIF

          radadj = radadj*pptadj
          IF ( radadj<0.2 ) radadj = 0.2
          Orad_hru(j) = DBLE( radadj ) * Soltab_horad_potsw(Jday,j)
          Basin_orad = Basin_orad + Orad_hru(j)*Hru_area_dble(j)

          ! https://www.omnicalculator.com/physics/cloud-base
!         cloud base = (temperature - dew point) / 4.4 * 1000 + elevation, altitude of clouds
!In this formula, the temperature and dew point are expressed in degrees Fahrenheits and the elevation and cloud base altitude are expressed in feet. 
!Make sure to adjust the result afterwards if you're using the SI units!

          IF ( Solsta_flag==1 ) THEN
            k = Hru_solsta(j)
            IF ( k>0 ) THEN
              IF ( Solrad(k)<0.0D0 .OR. Solrad(k)>10000.0D0 ) THEN
                IF ( Print_debug>DEBUG_less ) THEN
                  PRINT *, 'WARNING, measured solar radiation missing, HRU:', j, '; station:', k, '; value computed'
                  CALL print_date(1)
                ENDIF
              ELSE
                Swrad(j) = Solrad(k)*Rad_conv
                Basin_swrad = Basin_swrad + Swrad(j)*Hru_area_dble(j)
                CYCLE
              ENDIF
            ENDIF
          ENDIF

! markstro
! in Alaska, there are HRUs on certain days when the sun never rises, so this equation doesn't work
! when soltab_potsw, soltab_horad_potsw, or orad_hru are 0.0

!          Swrad(j) = Soltab_potsw(Jday, j)/Soltab_horad_potsw(Jday, j)*Orad_hru(j)/Hru_cossl(j)
          if ( Soltab_potsw(jday, j) > 0.0D0 .and. Soltab_horad_potsw(jday, j) > 0.0D0 .and. Orad_hru(j) > 0.0D0 ) then
             Swrad(j) = Soltab_potsw(Jday, j)/Soltab_horad_potsw(Jday, j)*Orad_hru(j)/Hru_cossl(j)
          else
             Swrad(j) = 0.0D0
          endif
! markstro

          Basin_swrad = Basin_swrad + Swrad(j)*Hru_area_dble(j)
        ENDDO
        Basin_orad = Basin_orad*Basin_area_inv
        IF ( Observed_flag==1 ) THEN
          Orad = Solrad(Basin_solsta)*Rad_conv
        ELSE
          Orad = Basin_orad
        ENDIF
        Basin_swrad = Basin_swrad*Basin_area_inv
        Basin_potsw = Basin_swrad

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_ddsolrad)

        ! Declare Parameters
        ALLOCATE ( Dday_slope(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'dday_slope', 'nhru,nmonths', 'real', &
     &       '0.4', '0.1', '1.4', &
     &       'Slope in temperature degree-day relationship', &
     &       'Monthly (January to December) slope in degree-day equation for each HRU', &
     &       'dday/temp_units')/=0 ) CALL read_error(1, 'dday_slope')
        ALLOCATE ( Dday_intcp(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'dday_intcp', 'nhru,nmonths', 'real', &
     &       '-40.0', '-60.0', '10.0', &
     &       'Intercept in temperature degree-day relationship', &
     &       'Monthly (January to December) intercept in degree-day equation for each HRU', &
     &       'dday')/=0 ) CALL read_error(1, 'dday_intcp')
        ALLOCATE ( Radadj_slope(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'radadj_slope', 'nhru,nmonths', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Slope in air temperature range adjustment to degree-day equation', &
     &       'Monthly (January to December) slope in air temperature range adjustment to degree-day equation for each HRU', &
     &       'dday/temp_units')/=0 ) CALL read_error(1, 'radadj_slope')
        ALLOCATE ( Radadj_intcp(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'radadj_intcp', 'nhru,nmonths', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Intercept in air temperature range adjustment to degree-day equation', &
     &       'Monthly (January to December) intercept in air temperature range adjustment to degree-day equation for each HRU', &
     &       'dday')/=0 ) CALL read_error(1, 'radadj_intcp')
        ALLOCATE ( Tmax_index(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'tmax_index', 'nhru,nmonths', 'real', &
     &       '50.0', '-10.0', '110.0', &
     &       'Monthly index temperature', &
     &       'Monthly (January to December) index temperature used'// &
     &       ' to determine precipitation adjustments to solar radiation for each HRU', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_index')

        ALLOCATE ( Ppt_rad_adj(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'ppt_rad_adj', 'nhru,nmonths', 'real', &
     &       '0.02', '0.0', '0.5', &
     &       'Radiation reduced if HRU precipitation above this value', &
     &       'Monthly minimum precipitation, if HRU precipitation exceeds this value, radiation is'// &
     &       ' multiplied by radj_sppt or radj_wppt adjustment factor', &
     &       'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')
        ALLOCATE ( Radmax(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'radmax', 'nhru,nmonths', 'real', &
     &       '0.8', '0.1', '1.0', &
     &       'Maximum fraction of potential solar radiation', &
     &       'Monthly (January to December) maximum fraction of the potential solar radiation'// &
     &       ' that may reach the ground due to haze, dust, smog, and so forth, for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radmax')
        IF ( Nsol > 0 ) THEN
          IF ( declparam(MODNAME, 'rad_conv', 'one', 'real', &
     &         '1.0', '0.1', '100.0', &
     &         'Conversion factor to Langleys for measured radiation', &
     &         'Conversion factor to Langleys for measured solar radiation', &
     &         'Langleys/radiation units')/=0 ) CALL read_error(1, 'rad_conv')
        ENDIF

      ELSEIF ( Process_flag==INIT ) THEN
! Get parameters
        IF ( getparam(MODNAME, 'dday_slope', Nhru_nmonths, 'real', Dday_slope)/=0 ) CALL read_error(2, 'dday_slope')
        IF ( getparam(MODNAME, 'dday_intcp', Nhru_nmonths, 'real', Dday_intcp)/=0 ) CALL read_error(2, 'dday_intcp')
        IF ( getparam(MODNAME, 'radadj_slope', Nhru_nmonths, 'real', Radadj_slope)/=0 ) CALL read_error(2, 'radadj_slope')
        IF ( getparam(MODNAME, 'radadj_intcp', Nhru_nmonths, 'real', Radadj_intcp)/=0 ) CALL read_error(2, 'radadj_intcp')
        IF ( getparam(MODNAME, 'tmax_index', Nhru_nmonths, 'real', Tmax_index)/=0 ) CALL read_error(2, 'tmax_index')
        ALLOCATE ( Ppt_rad_adj_sngl(Nhru,Nmonths) )
        IF ( getparam(MODNAME, 'ppt_rad_adj', Nhru_nmonths, 'real', Ppt_rad_adj_sngl)/=0 ) &
     &       CALL read_error(2, 'ppt_rad_adj')
        Ppt_rad_adj = DBLE( Ppt_rad_adj_sngl )
        DEALLOCATE ( Ppt_rad_adj_sngl )
        IF ( getparam(MODNAME, 'radmax', Nhru_nmonths, 'real', Radmax)/=0 ) CALL read_error(2, 'radmax')
        IF ( Nsol > 0 ) THEN
          IF ( getparam(MODNAME, 'rad_conv', 1, 'real', Rad_conv_sngl)/=0 ) CALL read_error(2, 'rad_conv')
          Rad_conv = DBLE( Rad_conv_sngl )
        ENDIF
        Observed_flag = OFF
        IF ( Nsol>0 .AND. Basin_solsta>0 ) Observed_flag = ACTIVE

      ENDIF

      END FUNCTION ddsolrad
