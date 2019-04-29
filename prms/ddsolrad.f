!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a maximum temperature per degree-day relation;
! modification of ddsolrad_prms
! Declared Parameters
!     dday_slope, dday_intcp, radj_sppt, radj_wppt, basin_solsta
!     radadj_slope, radadj_intcp, radmax, ppt_rad_adj, rad_conv
!     tmax_index, tmax_allrain, hru_solsta
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 is
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      INTEGER FUNCTION ddsolrad()
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru,
     +    Version_ddsolrad, Ddsolrad_nc
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Nsol, Solrad_tmax,
     +    Ppt_rad_adj, Rad_conv, Hru_solsta, Basin_horad, Radmax,
     +    Basin_potsw, Basin_solsta, Radj_sppt, Radj_wppt, Orad,
     +    Basin_obs_ppt, Tmax_allrain
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl,
     +    Hemisphere
      USE PRMS_OBS, ONLY: Solrad, Nowtime, Jday, Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC INT, FLOAT, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Declared Parameters
      REAL, SAVE :: Radadj_slope, Radadj_intcp
      REAL, SAVE, ALLOCATABLE :: Dday_slope(:), Dday_intcp(:)
      REAL, SAVE, ALLOCATABLE :: Tmax_index(:)
! Local Variables
      INTEGER, SAVE :: observed_flag
      INTEGER :: j, jj, k, kp, kp1
      REAL :: pptadj, radadj, dday, ddayi
! Save Variables
      REAL, SAVE, DIMENSION(26) :: solf
      DATA solf/.20, .35, .45, .51, .56, .59, .62, .64, .655, .67, .682,
     +          .69, .70, .71, .715, .72, .722, .724, .726, .728, .73,
     +          .734, .738, .742, .746, .75/
      CHARACTER(LEN=8), PARAMETER :: MODNAME = 'ddsolrad'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Solar Radiation'
!***********************************************************************
      ddsolrad = 1

      IF ( Process(:3)=='run' ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)

        Orad = -999.0
        IF ( observed_flag==1 ) Orad = Solrad(Basin_solsta)*Rad_conv

        IF ( Orad<0.0 .OR. Orad>10000.0 ) THEN
          dday = Dday_slope(Nowmonth)*Solrad_tmax + Dday_intcp(Nowmonth)
     +           + 1.0
          IF ( dday<1.0 ) dday = 1.0
          IF ( dday<26.0 ) THEN
            kp = INT(dday)
            ddayi = FLOAT(kp)
            kp1 = kp + 1
            radadj = solf(kp) + ((solf(kp1)-solf(kp))*(dday-ddayi))
          ELSE
            radadj = Radmax
          ENDIF

          IF ( Basin_obs_ppt<=Ppt_rad_adj(Nowmonth) ) THEN
            pptadj = 1.0
          ELSEIF ( Solrad_tmax>=Tmax_index(Nowmonth) ) THEN
            pptadj = Radadj_intcp + Radadj_slope*
     +               (Solrad_tmax-Tmax_index(Nowmonth))
            IF ( pptadj>1.0 ) pptadj = 1.0
          ELSE
            pptadj = Radj_sppt
            IF ( Solrad_tmax>=Tmax_allrain(Nowmonth) ) THEN
              IF ( Hemisphere==0 ) THEN ! Northern Hemisphere
                IF ( Jday<79 .OR. Jday>265 ) pptadj = Radj_wppt ! Winter
              ELSE ! Southern Hemisphere
                IF ( Jday>79 .OR. Jday<265 ) pptadj = Radj_wppt ! Winter
              ENDIF
            ELSE
              pptadj = Radj_wppt
            ENDIF
          ENDIF

          radadj = radadj*pptadj
          IF ( radadj<0.2 ) radadj = 0.2
          Orad = radadj*Basin_horad
        ENDIF

        Basin_potsw = 0.0D0
        IF ( Nsol==0 ) THEN
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            Swrad(j) = Soltab_potsw(Jday, j)/Basin_horad*Orad
     +                 /Hru_cossl(j)
            Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
          ENDDO
        ELSE
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            k = Hru_solsta(j)
            IF ( k>0 ) THEN
              IF ( Solrad(k)<0.0 .OR. Solrad(k)>10000.0 ) THEN
                IF ( Print_debug>-1 ) THEN
                  PRINT *, 'Warning, measured solar radiation missing',
     +                     ' for HRU:', j, ' Solar radiation station:',k
                  PRINT *, ' Value computed; date:', Nowtime
                ENDIF
              ELSE
                Swrad(j) = Solrad(k)*Rad_conv
                Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
                CYCLE
              ENDIF
            ENDIF
            Swrad(j) = Soltab_potsw(Jday, j)/Basin_horad*Orad
     +                 /Hru_cossl(j)
            Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
          ENDDO
        ENDIF
        Basin_potsw = Basin_potsw*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_ddsolrad =
     +'$Id: ddsolrad.f 4233 2012-02-29 21:27:34Z rsregan $'
        Ddsolrad_nc = INDEX( Version_ddsolrad, 'Z' )
        k = INDEX( Version_ddsolrad, '.f' ) + 1
        IF ( declmodule(Version_ddsolrad(6:k), PROCNAME,
     +       Version_ddsolrad(k+2:Ddsolrad_nc))/=0 ) STOP

! Declare Parameters
        ALLOCATE ( Dday_slope(12) )
        IF ( declparam(MODNAME, 'dday_slope', 'nmonths', 'real',
     +       '0.4', '0.2', '0.7',
     +       'Slope in temperature degree-day relationship',
     +       'Monthly (January to December) slope in'//
     +       ' degree-day equation',
     +       'dday/temp_units')/=0 ) CALL read_error(1, 'dday_slope')
        ALLOCATE ( Dday_intcp(12) )
        IF ( declparam(MODNAME, 'dday_intcp', 'nmonths', 'real',
     +       '-10.0', '-60.0', '4.0',
     +       'Intercept in temperature degree-day relationship',
     +       'Monthly (January to December) intercept in'//
     +       ' degree-day equation',
     +       'dday')/=0 ) CALL read_error(1, 'dday_intcp')
        IF ( declparam(MODNAME, 'radadj_slope', 'one', 'real',
     +       '0.0', '0.0', '1.0',
     +       'Slope in air temperature range adjustment to solar'//
     +       ' radiation equation',
     +       'Slope in air temperature range adjustment to solar'//
     +       ' radiation equation',
     +       'dday/temp_units')/=0 ) CALL read_error(1, 'radadj_slope')
        IF ( declparam(MODNAME, 'radadj_intcp', 'one', 'real',
     +       '1.0', '0.0', '1.0',
     +       'Intercept in air temperature range adjustment to solar'//
     +       ' radiation equation',
     +       'Intercept in air temperature range adjustment to solar'//
     +       ' radiation equation',
     +       'dday')/=0 ) CALL read_error(1, 'radadj_intcp')
        ALLOCATE ( Tmax_index(12))
        IF ( declparam(MODNAME, 'tmax_index', 'nmonths', 'real',
     +       '50.0', '-10.0', '110.0',
     +       'Monthly index temperature',
     +       'Monthly (January to December) index temperature used'//
     +       ' to determine precipitation adjustments to solar'//
     +       ' radiation',
     +       'temp_units')/=0 ) CALL read_error(1, 'tmax_index')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'dday_slope', 12, 'real', Dday_slope)
     +       /=0 ) CALL read_error(2, 'dday_slope')
        IF ( getparam(MODNAME, 'dday_intcp', 12, 'real', Dday_intcp)
     +       /=0 ) CALL read_error(2, 'dday_intcp')
        IF ( getparam(MODNAME, 'radadj_slope', 1, 'real', Radadj_slope)
     +       /=0 ) CALL read_error(2, 'radadj_slope')
        IF ( getparam(MODNAME, 'radadj_intcp', 1, 'real', Radadj_intcp)
     +       /=0 ) CALL read_error(2, 'radadj_intcp')
        IF ( getparam(MODNAME, 'tmax_index', 12, 'real', Tmax_index)
     +       /=0 ) CALL read_error(2, 'tmax_index')

        observed_flag = 0
        IF ( Nsol>0 .AND. Basin_solsta>0 ) observed_flag = 1
      ENDIF

      ddsolrad = 0
      END FUNCTION ddsolrad

