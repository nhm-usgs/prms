!***********************************************************************
! Determines the form of precipitation and distributes it from one or
! more stations to each HRU using monthly correction factors to account
! for differences in altitude, spatial variation, topography, and
! measurement gage efficiency
!
! Needs variable "precip" in the DATA FILE
! Needs computed variables tmaxf and tminf set in the temperature module
!   Declared Parameters
!     tmax_allrain, tmax_allsnow, hru_psta, adjmix_rain
!     rain_adj, snow_adj, hru_area
!***********************************************************************
      INTEGER FUNCTION precip_1sta()
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Version_precip_1sta, Precip_1sta_nc
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_area, Hru_route_order, Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Prmx, Basin_ppt, &
          Basin_rain, Basin_snow, Hru_ppt, Hru_rain, Hru_snow, &
          Basin_obs_ppt, Tmaxf, Tminf, Tmax_allsnow_f, Tmax_allrain_f, &
          Adjmix_rain, Precip_units, Nrain
      USE PRMS_OBS, ONLY: Precip, Nowtime, Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL :: read_error, precip_form
! Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_psta(:)
      REAL, SAVE, ALLOCATABLE :: Rain_adj(:, :), Snow_adj(:, :)
! Local Variables
      INTEGER :: i, ii, ip
      REAL :: adjmix_mo, allrain_f_mo, ppt
      DOUBLE PRECISION :: sum_obs
      INTEGER, SAVE, ALLOCATABLE :: istack(:)
!***********************************************************************
      precip_1sta = 1

      IF ( Process(:3)=='run' ) THEN
        Basin_ppt = 0.0D0
        Basin_rain = 0.0D0
        Basin_snow = 0.0D0
        istack = 0
        sum_obs = 0.0D0
        adjmix_mo = Adjmix_rain(Nowmonth)
        allrain_f_mo = Tmax_allrain_f(Nowmonth)
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
!******Zero precipitation on HRU
          Hru_ppt(i) = 0.0
          Hru_rain(i) = 0.0
          Hru_snow(i) = 0.0
          Prmx(i) = 0.0
          Newsnow(i) = 0.0
          Pptmix(i) = 0
          ip = Hru_psta(i)
          ppt = Precip(ip)
          IF ( ppt<0.0 ) THEN
            IF ( istack(ip)==0 ) THEN
              PRINT 9002, ppt, ip, Nowtime
              istack(ip) = 1
            ENDIF
          ! ignore very small amounts of precipitation
          ELSEIF ( ppt>NEARZERO ) THEN
            IF ( Precip_units==1 ) ppt = ppt/25.4
            CALL precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), Tmaxf(i), &
                             Tminf(i), Pptmix(i), Newsnow(i), Prmx(i), &
                             allrain_f_mo, Rain_adj(i, Nowmonth), Snow_adj(i, Nowmonth), &
                             adjmix_mo, Hru_area(i), sum_obs)
          ENDIF
        ENDDO
        Basin_ppt = Basin_ppt*Basin_area_inv
        Basin_obs_ppt = sum_obs*Basin_area_inv
        Basin_rain = Basin_rain*Basin_area_inv
        Basin_snow = Basin_snow*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_precip_1sta = '$Id: precip_1sta.f90 3815 2011-10-25 18:33:55Z rsregan $'
        Precip_1sta_nc = INDEX( Version_precip_1sta, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_precip_1sta(:Precip_1sta_nc))/=0 ) STOP
        ENDIF

! Declare parameters
        ALLOCATE ( Hru_psta(Nhru) )
        IF ( declparam('precip', 'hru_psta', 'nhru', 'integer', &
             '1', 'bounded', 'nrain', &
             'Index of base precipitation station for HRU', &
             'Index of the base precipitation station used for lapse rate calculations for each HRU', &
             'none')/=0 ) CALL read_error(1, 'hru_psta')
        ALLOCATE ( Rain_adj(Nhru, 12) )
        IF ( declparam('precip', 'rain_adj', 'nhru,nmonths', 'real', &
             '1.0', '0.2', '5.0', &
             'Monthly rain adjustment factor for each HRU', &
             'Monthly (January to December) adjustment factor to measured precipitation on each HRU to account for'// &
             ' differences in elevation, etc.', &
             'decimal fraction')/=0 ) CALL read_error(1, 'rain_adj')
        ALLOCATE ( Snow_adj(Nhru, 12) )
        IF ( declparam('precip', 'snow_adj', 'nhru,nmonths', 'real', &
             '1.0', '0.2', '5.0', &
             'Monthly snow adjustment factor for each HRU', &
             'Monthly (January to December) adjustment factor to measured precipitation on each HRU to account for'// &
             ' differences in elevation, etc.', &
             'decimal fraction')/=0 ) CALL read_error(1, 'snow_adj')

! Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        ALLOCATE ( istack(Nrain) )
        IF ( getparam('precip', 'hru_psta', Nhru, 'integer', Hru_psta)/=0 ) CALL read_error(2, 'hru_psta')
        IF ( getparam('precip', 'rain_adj', Nhru*12, 'real', Rain_adj)/=0 ) CALL read_error(2, 'rain_adj')
        IF ( getparam('precip', 'snow_adj', Nhru*12, 'real', Snow_adj)/=0 ) CALL read_error(2, 'snow_adj')
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          IF ( Hru_psta(i)<1 .OR. Hru_psta(i)>Nrain ) THEN
            PRINT *, 'Warning, hru_psta=0 or hru_psta>nrain for HRU:', i, '; value set to 1'
            Hru_psta(i) = 1
          ENDIF
        ENDDO
      ENDIF

 9002 FORMAT ('Warning, bad precipitation value:', F10.3, &
              '; precip station:', I3, '; Time:', I5, 2('/', I2.2), I3, &
              2(':', I2.2), '; value set to 0.0')

      precip_1sta = 0
      END FUNCTION precip_1sta

