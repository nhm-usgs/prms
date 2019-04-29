!***********************************************************************
! Determines the form of precipitation and distributes it to each HRU
! using monthly lapse rates
! Declared Parameters
!     tmax_allrain, tmax_allsnow, hru_psta, adjmix_rain
!     padj_rn, padj_sn, precip_units
!     hru_plaps, psta_elev, pmn_mo, hru_elev
! Needs variable "precip" in the DATA FILE
! Needs computed variable tmax set in the temperature module
!***********************************************************************
      MODULE PRMS_PRECIP_LAPS
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE, ALLOCATABLE :: Istack(:)
        REAL, SAVE, ALLOCATABLE :: Rain_adj_lapse(:, :), Snow_adj_lapse(:, :)
        CHARACTER(LEN=11), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru_psta(:), Hru_plaps(:)
        REAL, SAVE, ALLOCATABLE :: Padj_rn(:, :), Padj_sn(:, :), Pmn_mo(:, :)
      END MODULE PRMS_PRECIP_LAPS

      INTEGER FUNCTION precip_laps()
      USE PRMS_PRECIP_LAPS
      USE PRMS_MODULE, ONLY: Process, Nhru, Nrain, Inputerror_flag, Parameter_check_flag
      USE PRMS_BASIN, ONLY: Hru_elev, Active_hrus, NEARZERO, &
     &    Hru_route_order, Hru_area, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Prmx, Basin_ppt, &
     &    Basin_rain, Basin_snow, Hru_ppt, Hru_rain, Hru_snow, &
     &    Basin_obs_ppt, Tmaxf, Tminf, Tmax_allrain_f, &
     &    Adjmix_rain, Precip_units, Psta_elev
      USE PRMS_OBS, ONLY: Precip, Nowtime, Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error, precip_form
! Local Variables
      INTEGER :: i, j, np1, np2, ip, ii, ierr, nc
      REAL elh_diff, elp_diff, pmo_diff, pmo_rate, adj_p, allrain_f_mo, adjmix_mo, ppt
      DOUBLE PRECISION :: sum_obs
      CHARACTER(LEN=80), SAVE :: Version_precip_laps
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Precipitation Distribution'
!***********************************************************************
      precip_laps = 0

      IF ( Process(:3)=='run' ) THEN
        Basin_ppt = 0.0D0
        Basin_rain = 0.0D0
        Basin_snow = 0.0D0
        Istack = 0
        allrain_f_mo = Tmax_allrain_f(Nowmonth)
        adjmix_mo = Adjmix_rain(Nowmonth)
        sum_obs = 0.0D0
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
!******Zero precipitation on HRU
          Hru_ppt(i) = 0.0
          Hru_rain(i) = 0.0
          Hru_snow(i) = 0.0
          Prmx(i) = 0.0
          Newsnow(i) = 0
          Pptmix(i) = 0
          ip = Hru_psta(i)
          ppt = Precip(ip)
          IF ( ppt<0.0 ) THEN
            IF ( Istack(ip)==0 ) THEN
              PRINT 9002, ppt, ip, Nowtime
              Istack(ip) = 1
            ENDIF
          ! ignore very small amounts of precipitation
          ELSEIF ( ppt>NEARZERO ) THEN
            IF ( Precip_units==1 ) ppt = ppt/25.4
            CALL precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), Tmaxf(i), &
     &                       Tminf(i), Pptmix(i), Newsnow(i), Prmx(i), &
     &                       allrain_f_mo, rain_adj_lapse(i, Nowmonth), &
     &                       snow_adj_lapse(i,Nowmonth), adjmix_mo, &
     &                       Hru_area(i), sum_obs)
          ENDIF
        ENDDO
        Basin_ppt = Basin_ppt*Basin_area_inv
        Basin_obs_ppt = sum_obs*Basin_area_inv
        Basin_rain = Basin_rain*Basin_area_inv
        Basin_snow = Basin_snow*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_precip_laps = '$Id: precip_laps.f90 5169 2012-12-28 23:51:03Z rsregan $'
        nc = INDEX( Version_precip_laps, 'Z' )
        i = INDEX ( Version_precip_laps, '.f90' ) + 3
        IF ( declmodule(Version_precip_laps(6:i), PROCNAME, Version_precip_laps(i+2:nc))/=0 ) STOP
        MODNAME = 'precip_laps'

! Declare parameters
        ALLOCATE ( Hru_psta(Nhru), Padj_rn(Nrain, 12), Padj_sn(Nrain, 12), Pmn_mo(Nrain, 12), Hru_plaps(Nhru) )
        ALLOCATE ( Istack(Nrain), Snow_adj_lapse(Nhru, 12), Rain_adj_lapse(Nhru, 12) )
        IF ( declparam(MODNAME, 'hru_psta', 'nhru', 'integer', &
     &       '1', 'bounded', 'nrain', &
     &       'Index of base precipitation station for HRU', &
     &       'Index of the base precipitation station used for lapse'// &
     &       ' rate calculations for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'hru_psta')
        IF ( declparam(MODNAME, 'padj_rn', 'nrain,nmonths', 'real', &
     &       '1.0', '-2.0', '10.0', &
     &       'Rain adjustment factor, by month for each precipitation station', &
     &       'Monthly (January to December) factor to adjust'// &
     &       ' precipitation lapse rate computed between station hru_psta'// &
     &       ' and station hru_plaps; positive factors are mutiplied'// &
     &       ' times the lapse rate and negative factors are made'// &
     &       ' positive and substituted for the computed lapse rate', &
     &       'precip_units')/=0 ) CALL read_error(1, 'padj_rn')
        IF ( declparam(MODNAME, 'padj_sn', 'nrain,nmonths', 'real', &
     &       '1.0', '-2.0', '10.0', &
     &       'Snow adjustment factor, by month for each precipitation station', &
     &       'Monthly (January to December) factor to adjust'// &
     &       ' precipitation lapse rate computed between station hru_psta'// &
     &       ' and station hru_plaps; positive factors are mutiplied'// &
     &       ' times the lapse rate and negative factors are made'// &
     &       ' positive and substituted for the computed lapse rate', &
     &       'precip_units')/=0 ) CALL read_error(1, 'padj_sn')
        IF ( declparam(MODNAME, 'pmn_mo', 'nrain,nmonths', 'real', &
     &       '1.0', '0.00001', '100.0', &
     &       'Mean monthly precipitation for each lapse precipitation station', &
     &       'Mean monthly (January to December) precipitation for'// &
     &       ' each lapse precipitation measurement station', &
     &       'precip_units')/=0 ) CALL read_error(1, 'pmn_mo')
        IF ( declparam(MODNAME, 'hru_plaps', 'nhru', 'integer', &
     &       '1', 'bounded', 'nrain', &
     &       'Index of precipitation station to lapse against hru_psta', &
     &       'Index of the lapse precipitation station used for lapse'// &
     &       ' rate calculations for each HRU using hru_psta', &
     &       'none')/=0 ) CALL read_error(1, 'hru_plaps')

! Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'hru_psta', Nhru, 'integer', Hru_psta)/=0 ) CALL read_error(2, 'hru_psta')
        IF ( getparam(MODNAME, 'padj_rn', Nrain*12, 'real', Padj_rn)/=0 ) CALL read_error(2, 'padj_rn')
        IF ( getparam(MODNAME, 'padj_sn', Nrain*12, 'real', Padj_sn)/=0 ) CALL read_error(2, 'padj_sn')
        IF ( getparam(MODNAME, 'hru_plaps', Nhru, 'integer', Hru_plaps)/=0 ) CALL read_error(2, 'hru_plaps')
        IF ( getparam(MODNAME, 'pmn_mo', Nrain*12, 'real', Pmn_mo)/=0 ) CALL read_error(2, 'pmn_mo')

        DO j = 1, 12
          DO i = 1, Nrain
            IF ( Pmn_mo(i,j)<0.00001 ) THEN
              PRINT *, 'pmn_mo needs to be at least 0.00001'
              IF ( Parameter_check_flag==1 ) THEN
                PRINT *, 'ERROR, HRU:', i, 'month:', j, ', pmn_mo:', Pmn_mo(i, j)
                Inputerror_flag = 1
              ELSE
                PRINT *, 'Warning, HRU:', i, 'month:', j, ', pmn_mo:', Pmn_mo(i, j), ') set to 0.00001'
                Pmn_mo(i, j) = 0.00001
              ENDIF
            ENDIF
          ENDDO
        ENDDO

        DO ii = 1, Active_hrus
          ierr = 0
          i = Hru_route_order(ii)
          IF ( Hru_psta(i)<1 .OR. Hru_psta(i)>Nrain ) THEN
            PRINT *, 'ERROR, hru_psta = 0 or > nrain for HRU:', i, Hru_psta(i)
            ierr = 1
          ENDIF
          IF ( Hru_plaps(i)<1 .OR. Hru_plaps(i)>Nrain ) THEN
            PRINT *, 'ERROR, hru_plaps = 0 or > nrain for HRU:', i, Hru_plaps(i)
            ierr = 1
          ENDIF
          IF ( ierr==1 ) THEN
            Inputerror_flag = 1
            CYCLE
          ENDIF

          np1 = Hru_psta(i)
          np2 = Hru_plaps(i)
          elp_diff = Psta_elev(np2) - Psta_elev(np1)
          IF ( ABS(elp_diff)<NEARZERO ) elp_diff = 1.0
          elh_diff = Hru_elev(i) - Psta_elev(np1)
          DO j = 1, 12
            pmo_diff = Pmn_mo(np2,j) - Pmn_mo(np1,j)
            pmo_rate = pmo_diff / elp_diff
            adj_p = (pmo_rate*elh_diff)/Pmn_mo(np1,j)
            IF ( Padj_sn(np1,j)>=0.0 ) THEN
              Snow_adj_lapse(i,j) = 1.0 + Padj_sn(np1,j)*adj_p
            ELSE 
              Snow_adj_lapse(i,j) = -Padj_sn(np1,j)
            ENDIF

            IF ( Padj_rn(np1,j)<0.0 ) THEN
              Rain_adj_lapse(i,j) = -Padj_rn(np1,j)
            ELSE
              Rain_adj_lapse(i,j) = 1.0 + Padj_rn(np1,j)*adj_p
            ENDIF
          ENDDO
        ENDDO
      ENDIF

 9002 FORMAT ('Warning, bad precipitation value:', F10.3, &
              '; precipitation station:', I3, '; Time:', I5, 2('/', I2.2), I3, &
              2(':', I2.2), '; value set to 0.0')

      END FUNCTION precip_laps
