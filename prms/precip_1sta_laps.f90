!***********************************************************************
! Determines the form of precipitation and distributes it to each HRU
! using monthly lapse rates (precip_1sta)
! or from one or more stations to each HRU using monthly correction
! factors to account for differences in altitude, spatial variation,
! topography, and measurement gage efficiency (precip_laps)
!
! Needs variable "precip" in the DATA FILE
! Needs computed variables tmaxf and tminf set in the temperature module
!***********************************************************************
      MODULE PRMS_PRECIP_1STA_LAPS
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE, ALLOCATABLE :: Istack(:)
        REAL, SAVE, ALLOCATABLE :: Rain_adj_lapse(:, :), Snow_adj_lapse(:, :)
        CHARACTER(LEN=11), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru_psta(:)
        ! Declared Parameters for precip_1sta
        ! Rain_adj(:, :), Snow_adj(:, :) = Rain_adj_lapse(:, :), Snow_adj_lapse(:, :)
        ! Declared Parameters for precip_laps
        INTEGER, SAVE, ALLOCATABLE :: Hru_plaps(:)
        REAL, SAVE, ALLOCATABLE :: Padj_rn(:, :), Padj_sn(:, :), Pmn_mo(:, :)
        ! parameters in climateflow or basin:
        !    both: tmax_allrain, tmax_allsnow, adjmix_rain, hru_area, hru_type
        !    precip_laps: hru_elev
      END MODULE PRMS_PRECIP_1STA_LAPS

      INTEGER FUNCTION precip_1sta_laps()
      USE PRMS_PRECIP_1STA_LAPS
      USE PRMS_MODULE, ONLY: Process, Nhru, Nrain, Inputerror_flag, Save_vars_to_file, Parameter_check_flag, Precip_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_area, Hru_route_order, Basin_area_inv, NEARZERO, Timestep, &
     &    Hru_elev
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Prmx, Basin_ppt, Adjmix_rain, Precip_units, &
     &    Basin_rain, Basin_snow, Hru_ppt, Hru_rain, Hru_snow, Basin_obs_ppt, Tmaxf, Tminf, Tmax_allrain_f, Tmax_allsnow_f
      USE PRMS_OBS, ONLY: Precip, Nowtime, Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC ABS
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL :: read_error, precip_form, print_module, precip_1sta_laps_restart, compute_precip_laps
! Local Variables
      INTEGER :: i, ii, ip, j
      REAL :: adjmix_mo, allrain_f_mo, ppt
      DOUBLE PRECISION :: sum_obs
      CHARACTER(LEN=80), SAVE :: Version_precip_1sta_laps
!***********************************************************************
      precip_1sta_laps = 0

      IF ( Process(:3)=='run' ) THEN
        Basin_ppt = 0.0D0
        Basin_rain = 0.0D0
        Basin_snow = 0.0D0
        Istack = 0
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
          Newsnow(i) = 0
          Pptmix(i) = 0
          ip = Hru_psta(i)
          ppt = Precip(ip)
          IF ( ppt<0.0 ) THEN
            IF ( Istack(ip)==0 ) THEN
              PRINT 9002, ppt, MODNAME, ip, Nowtime
              Istack(ip) = 1
            ENDIF
          ! ignore very small amounts of precipitation
          ELSEIF ( ppt>NEARZERO ) THEN
            IF ( Precip_units==1 ) ppt = ppt/25.4
            CALL precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), Tmaxf(i), &
     &                       Tminf(i), Pptmix(i), Newsnow(i), Prmx(i), &
     &                       allrain_f_mo, Rain_adj_lapse(i,Nowmonth), Snow_adj_lapse(i,Nowmonth), &
     &                       adjmix_mo, Hru_area(i), sum_obs, Tmax_allsnow_f)
          ENDIF
        ENDDO
        Basin_ppt = Basin_ppt*Basin_area_inv
        Basin_obs_ppt = sum_obs*Basin_area_inv
        Basin_rain = Basin_rain*Basin_area_inv
        Basin_snow = Basin_snow*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_precip_1sta_laps = '$Id: precip_1sta_laps.f90 5604 2013-04-23 18:44:41Z rsregan $'
        IF ( Precip_flag==1 ) THEN
          MODNAME = 'precip_1sta'
          Version_precip_1sta_laps = Version_precip_1sta_laps(:16)//Version_precip_1sta_laps(22:80)
        ELSE
          MODNAME = 'precip_laps'
          Version_precip_1sta_laps = Version_precip_1sta_laps(:11)//Version_precip_1sta_laps(17:80)
          ALLOCATE ( Padj_rn(Nrain, 12), Padj_sn(Nrain, 12), Pmn_mo(Nrain, 12), Hru_plaps(Nhru) )
        ENDIF
        CALL print_module(Version_precip_1sta_laps, 'Precipitation Distribution', 90)

        ALLOCATE ( Hru_psta(Nhru), Istack(Nrain), Snow_adj_lapse(Nhru, 12), Rain_adj_lapse(Nhru, 12) )
! Declare parameters
        IF ( Timestep/=0 ) RETURN

        IF ( declparam(MODNAME, 'hru_psta', 'nhru', 'integer', &
     &       '1', 'bounded', 'nrain', &
     &       'Index of base precipitation station for HRU', &
     &       'Index of the base precipitation station used for lapse rate calculations for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'hru_psta')
        IF ( Precip_flag==1 ) THEN
          IF ( declparam(MODNAME, 'rain_adj', 'nhru,nmonths', 'real', &
     &         '1.0', '0.2', '5.0', &
     &         'Monthly rain adjustment factor for each HRU', &
     &         'Monthly (January to December) adjustment factor to measured precipitation on each HRU to account for'// &
     &         ' differences in elevation, and so forth', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'rain_adj')
          IF ( declparam(MODNAME, 'snow_adj', 'nhru,nmonths', 'real', &
     &         '1.0', '0.2', '5.0', &
     &         'Monthly snow adjustment factor for each HRU', &
     &         'Monthly (January to December) adjustment factor to measured precipitation on each HRU to account for'// &
     &         ' differences in elevation, and so forth', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'snow_adj')
        ELSE ! precip_laps
          IF ( declparam(MODNAME, 'padj_rn', 'nrain,nmonths', 'real', &
     &         '1.0', '-2.0', '10.0', &
     &         'Rain adjustment factor, by month for each precipitation station', &
     &         'Monthly (January to December) factor to adjust precipitation lapse rate computed between station hru_psta'// &
     &         ' and station hru_plaps; positive factors are mutiplied times the lapse rate and negative factors are made'// &
     &         ' positive and substituted for the computed lapse rate', &
     &         'precip_units')/=0 ) CALL read_error(1, 'padj_rn')
          IF ( declparam(MODNAME, 'padj_sn', 'nrain,nmonths', 'real', &
     &         '1.0', '-2.0', '10.0', &
     &         'Snow adjustment factor, by month for each precipitation station', &
     &         'Monthly (January to December) factor to adjust precipitation lapse rate computed between station hru_psta'// &
     &         ' and station hru_plaps; positive factors are mutiplied times the lapse rate and negative factors are made'// &
     &         ' positive and substituted for the computed lapse rate', &
     &         'precip_units')/=0 ) CALL read_error(1, 'padj_sn')
          IF ( declparam(MODNAME, 'pmn_mo', 'nrain,nmonths', 'real', &
     &         '1.0', '0.00001', '100.0', &
     &         'Mean monthly precipitation for each lapse precipitation station', &
     &         'Mean monthly (January to December) precipitation for each lapse precipitation measurement station', &
     &         'precip_units')/=0 ) CALL read_error(1, 'pmn_mo')
          IF ( declparam(MODNAME, 'hru_plaps', 'nhru', 'integer', &
     &         '1', 'bounded', 'nrain', &
     &         'Index of precipitation station to lapse against hru_psta', &
     &         'Index of the lapse precipitation station used for lapse rate calculations for each HRU using hru_psta', &
     &         'none')/=0 ) CALL read_error(1, 'hru_plaps')
        ENDIF

! Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL precip_1sta_laps_restart(1)
          RETURN
        ENDIF

        IF ( getparam(MODNAME, 'hru_psta', Nhru, 'integer', Hru_psta)/=0 ) CALL read_error(2, 'hru_psta')
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          IF ( Hru_psta(i)<1 .OR. Hru_psta(i)>Nrain ) THEN
            PRINT *, 'ERROR, hru_psta = 0 or > nrain for HRU:', i, Hru_psta(i)
            Inputerror_flag = 1
          ENDIF
        ENDDO

        IF ( Precip_flag==1 ) THEN
          IF ( getparam(MODNAME, 'rain_adj', Nhru*12, 'real', Rain_adj_lapse)/=0 ) CALL read_error(2, 'rain_adj')
          IF ( getparam(MODNAME, 'snow_adj', Nhru*12, 'real', Snow_adj_lapse)/=0 ) CALL read_error(2, 'snow_adj')
        ELSE
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
            i = Hru_route_order(ii)
            IF ( Hru_plaps(i)<1 .OR. Hru_plaps(i)>Nrain ) THEN
              PRINT *, 'ERROR, hru_plaps = 0 or > nrain for HRU:', i, Hru_plaps(i)
              Inputerror_flag = 1
              CYCLE
            ENDIF

            CALL compute_precip_laps(i, Hru_plaps(i), Hru_psta(i), Hru_elev(i))
          ENDDO
        ENDIF

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL precip_1sta_laps_restart(0)
      ENDIF

 9002 FORMAT (/, 'WARNING: negative precipitation value:', F10.3, 'specified for module ', A, /, &
     &        'precipitation station:', I3, '; Time:', I5, 2('/', I2.2), I3, 2(':', I2.2), '; value set to 0.0')

      END FUNCTION precip_1sta_laps

!***********************************************************************
!     Compute lapse rate for an HRU
!***********************************************************************
      SUBROUTINE compute_precip_laps(Ihru, Hru_plaps, Hru_psta, Hru_elev)
      USE PRMS_PRECIP_1STA_LAPS, ONLY: Pmn_mo, Padj_sn, Padj_rn, Snow_adj_lapse, Rain_adj_lapse
      USE PRMS_CLIMATEVARS, ONLY: Psta_elev
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Hru_psta, Hru_plaps
      REAL, INTENT(IN) :: Hru_elev
! Functions
      INTRINSIC ABS
! Local Variables
      INTEGER :: j
      REAL :: elp_diff, elh_diff, pmo_diff, pmo_rate, adj_p
!***********************************************************************
      elp_diff = Psta_elev(Hru_plaps) - Psta_elev(Hru_psta)
      IF ( ABS(elp_diff)<NEARZERO ) elp_diff = 1.0
      elh_diff = Hru_elev - Psta_elev(Hru_psta)
      DO j = 1, 12
        pmo_diff = Pmn_mo(Hru_plaps, j) - Pmn_mo(Hru_psta, j)
        pmo_rate = pmo_diff / elp_diff
        adj_p = (pmo_rate*elh_diff)/Pmn_mo(Hru_psta, j)
        IF ( Padj_sn(Hru_psta, j)>=0.0 ) THEN
          Snow_adj_lapse(Ihru, j) = 1.0 + Padj_sn(Hru_psta, j)*adj_p
        ELSE 
          Snow_adj_lapse(Ihru, j) = -Padj_sn(Hru_psta, j)
        ENDIF
        IF ( Padj_rn(Hru_psta,j)<0.0 ) THEN
          Rain_adj_lapse(Ihru, j) = -Padj_rn(Hru_psta, j)
        ELSE
          Rain_adj_lapse(Ihru, j) = 1.0 + Padj_rn(Hru_psta, j)*adj_p
        ENDIF
      ENDDO
      END SUBROUTINE compute_precip_laps

!***********************************************************************
!     precip_1sta_laps_restart - write or read restart file
!***********************************************************************
      SUBROUTINE precip_1sta_laps_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Precip_flag
      USE PRMS_PRECIP_1STA_LAPS
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Rain_adj_lapse
        WRITE ( Restart_outunit ) Snow_adj_lapse
        WRITE ( Restart_outunit ) Hru_psta
        IF ( Precip_flag==2 ) THEN
          WRITE ( Restart_outunit ) Hru_plaps
          WRITE ( Restart_outunit ) Padj_rn
          WRITE ( Restart_outunit ) Padj_sn
          WRITE ( Restart_outunit ) Pmn_mo
        ENDIF
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Rain_adj_lapse
        READ ( Restart_inunit ) Snow_adj_lapse
        READ ( Restart_inunit ) Hru_psta
        IF ( Precip_flag==2 ) THEN
          READ ( Restart_inunit ) Hru_plaps
          READ ( Restart_inunit ) Padj_rn
          READ ( Restart_inunit ) Padj_sn
          READ ( Restart_inunit ) Pmn_mo
        ENDIF
      ENDIF
      END SUBROUTINE precip_1sta_laps_restart
