!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a relation between solar radiation and cloud
! cover; modification of ccsolrad_prms
! Declared Parameters
!     ccov_slope, ccov_intcp, radj_sppt, radj_wppt, basin_solsta
!     crad_coef, crad_exp, radmax, ppt_rad_adj, rad_conv, hru_solsta
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 is
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      INTEGER FUNCTION ccsolrad()
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru,
     +    Version_ccsolrad, Ccsolrad_nc
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Nsol, Solrad_tmin, Solrad_tmax,
     +    Ppt_rad_adj, Rad_conv, Hru_solsta, Basin_horad, Radmax,
     +    Basin_potsw, Basin_solsta, Radj_sppt, Radj_wppt, Orad,
     +    Basin_obs_ppt
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl,
     +    Hemisphere
      USE PRMS_OBS, ONLY: Solrad, Nowtime, Jday, Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Declared Parameters
      REAL, SAVE :: Crad_coef, Crad_exp
      REAL, SAVE, ALLOCATABLE :: Ccov_slope(:), Ccov_intcp(:)
! Local Variables
      INTEGER, SAVE :: observed_flag
      INTEGER :: j, jj, k
      REAL :: pptadj, ccov, radadj
      
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='ccsolrad')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Solar Radiation')
      
!***********************************************************************
      ccsolrad = 1

      IF ( Process(:3)=='run' ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)

        Orad = -999.0
        IF ( observed_flag==1 ) Orad = Solrad(Basin_solsta)*Rad_conv

        IF ( Orad<0.0 .OR. Orad>10000.0 ) THEN
          ccov = Ccov_slope(Nowmonth)*(Solrad_tmax-Solrad_tmin)
     +           + Ccov_intcp(Nowmonth)
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
        Version_ccsolrad =
     +'$Id: ccsolrad.f 3794 2011-10-25 16:35:49Z rsregan $'
        Ccsolrad_nc = INDEX( Version_ccsolrad, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(MODNAME, PROCNAME,
     +                    Version_ccsolrad(:Ccsolrad_nc))/=0 ) STOP
        ENDIF

! Declare Parameters
        ALLOCATE ( Ccov_slope(12) )
        IF ( declparam(MODNAME, 'ccov_slope', 'nmonths', 'real',
     +       '-0.13', '-0.5', '-0.01',
     +       'Slope in temperature cloud cover relationship',
     +       'Monthly (January to December) coefficient in'//
     +       ' cloud-cover relationship',
     +       'none')/=0 ) CALL read_error(1, 'ccov_slope')
        ALLOCATE ( Ccov_intcp(12) )
        IF ( declparam(MODNAME, 'ccov_intcp', 'nmonths', 'real',
     +       '1.83', '0.0', '5.0',
     +       'Intercept in temperature cloud cover relationship',
     +       'Monthly (January to December) intercept in'//
     +       ' cloud-cover relationship',
     +       'none')/=0 ) CALL read_error(1, 'ccov_intcp')
        IF ( declparam(MODNAME, 'crad_coef', 'one', 'real',
     +       '0.4', '0.1', '0.7',
     +       'Coefficient in cloud cover-solar radiation relationship',
     +       'Coefficient(B) in Thompson(1976) equation,' //
     +       ' Solar radiation = B + (1.0-B)*(1.0-cloudcover)**P' //
     +       ' Varies by region, contour map of values in reference',
     +       'none')/=0 ) CALL read_error(1, 'crad_coef')
        IF ( declparam(MODNAME, 'crad_exp', 'one', 'real',
     +       '0.61', '0.2', '0.8',
     +       'Exponent in cloud cover-solar radiation relationship',
     +       'Exponent(P) in Thompson(1976) equation:' //
     +       ' Solar radiation = B + (1.0-B)*(1.0-cloudcover)**P',
     +       'none')/=0 ) CALL read_error(1, 'crad_exp')

      ELSEIF ( Process(:4)=='init' ) THEN
! Get parameters
        IF ( getparam(MODNAME, 'ccov_slope', 12, 'real', Ccov_slope)
     +       /=0 ) CALL read_error(2, 'ccov_slope')
        IF ( getparam(MODNAME, 'ccov_intcp', 12, 'real', Ccov_intcp)
     +       /=0 ) CALL read_error(2, 'ccov_intcp')
        IF ( getparam(MODNAME, 'crad_coef', 1, 'real', Crad_coef)
     +       /=0 ) CALL read_error(2, 'crad_coef')
        IF ( getparam(MODNAME, 'crad_exp', 1, 'real', Crad_exp)
     +       /=0 ) CALL read_error(2, 'crad_exp')

        observed_flag = 0
        IF ( Nsol>0 .AND. Basin_solsta>0 ) observed_flag = 1
      ENDIF

      ccsolrad = 0
      END FUNCTION ccsolrad

