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
        ! Declared Variable
        DOUBLE PRECISION, SAVE :: Basin_radadj
        REAL, SAVE, ALLOCATABLE :: Cloud_radadj(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Crad_coef(:, :), Crad_exp(:, :), Radmax(:, :)
        REAL, SAVE, ALLOCATABLE :: Radj_sppt(:), Radj_wppt(:), Ppt_rad_adj(:, :)
      END MODULE PRMS_CCSOLRAD
!***********************************************************************
      INTEGER FUNCTION ccsolrad()
      USE PRMS_CCSOLRAD
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru, Nsol, Save_vars_to_file, &
     &    Init_vars_from_file
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, &
     &    Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Basin_orad, Orad_hru, &
     &    Rad_conv, Hru_solsta, Basin_horad, Basin_potsw, Basin_solsta, Orad, Hru_ppt
      USE PRMS_CHECK_NHRU_PARAMS, ONLY: Solsta_flag
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl, Soltab_horad_potsw
      USE PRMS_SET_TIME, ONLY: Jday, Nowmonth, Summer_flag
      USE PRMS_OBS, ONLY: Solrad
      USE PRMS_CLOUD_COVER, ONLY: Cloud_cover_hru
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam, declvar
      EXTERNAL :: read_error, print_module, print_date, ccsolrad_restart
! Local Variables
      INTEGER :: j, jj, k
      REAL :: pptadj, radadj
      CHARACTER(LEN=80), SAVE :: Version_ccsolrad
!***********************************************************************
      ccsolrad = 0

      IF ( Process(:3)=='run' ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)
        Basin_potsw = 0.0D0
        Basin_orad = 0.0D0
        Basin_radadj = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)

          ! determine radiation adjustment due to precipitation
          IF ( Hru_ppt(j)>Ppt_rad_adj(j,Nowmonth) ) THEN
            IF ( Summer_flag==1 ) THEN
              pptadj = Radj_sppt(j)
            ELSE
              pptadj = Radj_wppt(j) ! Winter
            ENDIF
          ELSE
            pptadj = 1.0
          ENDIF

          radadj = Crad_coef(j, Nowmonth) + &
     &             (1.0-Crad_coef(j,Nowmonth))*((1.0-Cloud_cover_hru(j))**Crad_exp(j,Nowmonth))
          IF ( radadj>Radmax(j,Nowmonth) ) radadj = Radmax(j, Nowmonth)
          Cloud_radadj(j) = radadj*pptadj
          Basin_radadj = Basin_radadj + Cloud_radadj(j)*Hru_area(j)

          Orad_hru(j) = Cloud_radadj(j)*Soltab_horad_potsw(Jday,j)
          Basin_orad = Basin_orad + Orad_hru(j)*Hru_area(j)
          IF ( Solsta_flag==1 ) THEN
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
          ENDIF
          Swrad(j) = Soltab_potsw(Jday, j)*Cloud_radadj(j)/Hru_cossl(j)
          Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
        ENDDO
        Basin_orad = Basin_orad*Basin_area_inv
        Basin_radadj = Basin_radadj*Basin_area_inv
        IF ( Observed_flag==1 ) THEN
          Orad = Solrad(Basin_solsta)*Rad_conv
        ELSE
          Orad = Basin_orad
        ENDIF
        Basin_potsw = Basin_potsw*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_ccsolrad = '$Id: ccsolrad_2d.f90 7214 2015-03-02 23:26:58Z rsregan $'
        CALL print_module(Version_ccsolrad, 'Solar Radiation Distribution', 90)
        MODNAME = 'ccsolrad'

        ALLOCATE ( Cloud_radadj(Nhru) )
        IF ( declvar(MODNAME, 'cloud_radadj', 'nhru', Nhru, 'real', &
     &       'Radiation adjustment for cloud cover of each HRU', &
     &       'decimal fraction', Cloud_radadj)/=0 ) CALL read_error(3, 'cloud_radadj')

        IF ( declvar(MODNAME, 'basin_radadj', 'one', 1, 'double', &
     &       'Basin area-weighted average radiation adjustment for cloud cover', &
     &       'decimal fraction', Basin_radadj)/=0 ) CALL read_error(3, 'basin_radadj')

        ! Declare Parameters
        ALLOCATE ( Crad_coef(Nhru,12) )
        IF ( declparam(MODNAME, 'crad_coef', 'nhru,nmonths', 'real', &
     &       '0.4', '0.1', '0.7', &
     &       'Coefficient in cloud cover-solar radiation relationship', &
     &       'Coefficient(B) in Thompson(1976) equation;' // &
     &       ' varies by region, contour map of values in reference', &
     &       'none')/=0 ) CALL read_error(1, 'crad_coef')
        ALLOCATE ( Crad_exp(Nhru,12) )
        IF ( declparam(MODNAME, 'crad_exp', 'nhru,nmonths', 'real', &
     &       '0.61', '0.2', '0.8', &
     &       'Exponent in cloud cover-solar radiation relationship', &
     &       'Exponent(P) in Thompson(1976) equation', &
     &       'none')/=0 ) CALL read_error(1, 'crad_exp')
        ALLOCATE ( Ppt_rad_adj(Nhru,12) )
        IF ( declparam(MODNAME, 'ppt_rad_adj', 'nhru,nmonths', 'real', &
     &       '0.02', '0.0', '0.5', &
     &       'Radiation reduced if HRU precipitation above this value', &
     &       'Monthly minimum precipitation, if HRU precipitation exceeds this value, radiation is'// &
     &       ' multiplied by radj_sppt or radj_wppt adjustment factor', &
     &       'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')
        ALLOCATE ( Radmax(Nhru,12) )
        IF ( declparam(MODNAME, 'radmax', 'nhru,nmonths', 'real', &
     &       '0.8', '0.1', '1.0', &
     &       'Maximum fraction of potential solar radiation', &
     &       'Maximum fraction of the potential solar radiation'// &
     &       ' that may reach the ground due to haze, dust, smog, and so forth', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radmax')
        ALLOCATE ( Radj_sppt(Nhru) )
        IF ( declparam(MODNAME, 'radj_sppt', 'nhru', 'real', &
     &       '0.44', '0.0', '1.0', &
     &       'Adjustment to solar radiation on precipitation day - summer', &
     &       'Adjustment factor for computed solar radiation for summer day with greater than'// &
     &       ' ppt_rad_adj inches of precipitation', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radj_sppt')
        ALLOCATE ( Radj_wppt(Nhru) )
        IF ( declparam(MODNAME, 'radj_wppt', 'nhru', 'real', &
     &       '0.5', '0.0', '1.0', &
     &       'Adjustment to solar radiation on precipitation day - winter', &
     &       'Adjustment factor for computed solar radiation for winter day with greater than'// &
     &       ' ppt_rad_adj inches of precipitation', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radj_wppt')

      ELSEIF ( Process(:4)=='init' ) THEN
! Get parameters
        IF ( getparam(MODNAME, 'crad_coef', Nhru*12, 'real', Crad_coef)/=0 ) CALL read_error(2, 'crad_coef')
        IF ( getparam(MODNAME, 'crad_exp', Nhru*12, 'real', Crad_exp)/=0 ) CALL read_error(2, 'crad_exp')
        IF ( getparam(MODNAME, 'ppt_rad_adj', Nhru*12, 'real', Ppt_rad_adj)/=0 ) CALL read_error(2, 'ppt_rad_adj')
        IF ( getparam(MODNAME, 'radmax', Nhru*12, 'real', Radmax)/=0 ) CALL read_error(2, 'radmax')
        IF ( getparam(MODNAME, 'radj_sppt', Nhru, 'real', Radj_sppt)/=0 ) CALL read_error(2, 'radj_sppt')
        IF ( getparam(MODNAME, 'radj_wppt', Nhru, 'real', Radj_wppt)/=0 ) CALL read_error(2, 'radj_wppt')

        IF ( Init_vars_from_file==1 ) THEN
          CALL ccsolrad_restart(1)
        ELSE
          Cloud_radadj = 0.0
          Basin_radadj = 0.0D0
        ENDIF

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
        WRITE ( Restart_outunit ) Basin_radadj
        WRITE ( Restart_outunit ) Cloud_radadj
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_radadj
        READ ( Restart_inunit ) Cloud_radadj
      ENDIF
      END SUBROUTINE ccsolrad_restart
