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
        character(len=*), parameter :: MODDESC = 'Solar Radiation Distribution'
        character(len=*), parameter :: MODNAME = 'ccsolrad'
        character(len=*), parameter :: Version_ccsolrad = '2024-01-31'
        INTEGER, SAVE :: Observed_flag
        ! Declared Variables
        DOUBLE PRECISION, SAVE :: Basin_radadj, Rad_conv
        REAL, SAVE, ALLOCATABLE :: Cloud_radadj(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Crad_coef(:, :), Crad_exp(:, :), Radmax(:, :)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ppt_rad_adj(:, :)
      END MODULE PRMS_CCSOLRAD
!***********************************************************************
      INTEGER FUNCTION ccsolrad()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, DEBUG_less, Nmonths, ACTIVE, OFF
      USE PRMS_MODULE, ONLY: Process_flag, Print_debug, Nhru, Nsol, Nowmonth, Nhru_nmonths
      USE PRMS_CCSOLRAD
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area_dble, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Basin_orad, Orad_hru, &
     &    Hru_solsta, Basin_horad, Basin_potsw, Basin_swrad, Basin_solsta, Orad, Hru_ppt, &
     &    Solsta_flag, Radj_sppt, Radj_wppt
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl, Soltab_horad_potsw
      USE PRMS_SET_TIME, ONLY: Jday, Summer_flag
      USE PRMS_OBS, ONLY: Solrad
      USE PRMS_CLOUD_COVER, ONLY: Basin_cloud_cover, Cloud_cover_hru
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE, SNGL
      INTEGER, EXTERNAL :: declparam, getparam, declvar
      EXTERNAL :: read_error, print_module, print_date
! Local Variables
      INTEGER :: j, jj, k
      REAL :: pptadj, radadj, Rad_conv_sngl
      REAL, ALLOCATABLE :: Ppt_rad_adj_sngl(:, :)
      DOUBLE PRECISION :: cloud_radadj_dble
!***********************************************************************
      ccsolrad = 0

      IF ( Process_flag==RUN ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)
        Basin_swrad = 0.0D0
        Basin_orad = 0.0D0
        Basin_radadj = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)

          ! determine radiation adjustment due to precipitation
          IF ( Hru_ppt(j)>Ppt_rad_adj(j,Nowmonth) ) THEN
            IF ( Summer_flag==ACTIVE ) THEN
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
          cloud_radadj_dble = DBLE( Cloud_radadj(j) )
          Basin_radadj = Basin_radadj + cloud_radadj_dble*Hru_area_dble(j)

          Orad_hru(j) = cloud_radadj_dble*Soltab_horad_potsw(Jday,j)
          Basin_orad = Basin_orad + Orad_hru(j)*Hru_area_dble(j)
          IF ( Solsta_flag==ACTIVE ) THEN
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
          if ( .NOT.(Soltab_potsw(jday, j))>0.0D0 .or. .NOT.(Hru_cossl(j)>0.0D0) ) then
            Swrad(j) = 0.0D0
          else
            Swrad(j) = Soltab_potsw(Jday, j)*cloud_radadj_dble/Hru_cossl(j)
          endif
          Basin_swrad = Basin_swrad + Swrad(j)*Hru_area_dble(j)
        ENDDO
        Basin_orad = Basin_orad*Basin_area_inv
        Basin_radadj = Basin_radadj*Basin_area_inv
        IF ( Observed_flag==ACTIVE ) THEN
          Orad = Solrad(Basin_solsta)*Rad_conv
        ELSE
          Orad = Basin_orad
        ENDIF
        Basin_swrad = Basin_swrad*Basin_area_inv
        Basin_potsw = Basin_swrad
        Basin_cloud_cover = Basin_cloud_cover*Basin_area_inv

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_ccsolrad)

        ALLOCATE ( Cloud_radadj(Nhru) )
        IF ( declvar(MODNAME, 'cloud_radadj', 'nhru', Nhru, 'real', &
     &       'Radiation adjustment for cloud cover of each HRU', &
     &       'decimal fraction', Cloud_radadj)/=0 ) CALL read_error(3, 'cloud_radadj')

        IF ( declvar(MODNAME, 'basin_radadj', 'one', 1, 'double', &
     &       'Basin area-weighted average radiation adjustment for cloud cover', &
     &       'decimal fraction', Basin_radadj)/=0 ) CALL read_error(3, 'basin_radadj')

        ! Declare Parameters
        ALLOCATE ( Crad_coef(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'crad_coef', 'nhru,nmonths', 'real', &
     &       '0.4', '0.1', '0.7', &
     &       'Coefficient in cloud cover-solar radiation relationship', &
     &       'Coefficient(B) in Thompson(1976) equation;' // &
     &       ' varies by region, contour map of values in reference', &
     &       'none')/=0 ) CALL read_error(1, 'crad_coef')
        ALLOCATE ( Crad_exp(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'crad_exp', 'nhru,nmonths', 'real', &
     &       '0.61', '0.2', '0.8', &
     &       'Exponent in cloud cover-solar radiation relationship', &
     &       'Exponent(P) in Thompson(1976) equation', &
     &       'none')/=0 ) CALL read_error(1, 'crad_exp')

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
        IF ( getparam(MODNAME, 'crad_coef', Nhru_nmonths, 'real', Crad_coef)/=0 ) CALL read_error(2, 'crad_coef')
        IF ( getparam(MODNAME, 'crad_exp', Nhru_nmonths, 'real', Crad_exp)/=0 ) CALL read_error(2, 'crad_exp')
        ALLOCATE ( Ppt_rad_adj_sngl(Nhru,Nmonths) )
        IF ( getparam(MODNAME, 'ppt_rad_adj', Nhru_nmonths, 'real', Ppt_rad_adj_sngl)/=0 ) &
     &       CALL read_error(2, 'ppt_rad_adj')
        Ppt_rad_adj = DBLE( Ppt_rad_adj_sngl )
        DEALLOCATE ( Ppt_rad_adj_sngl )
        IF ( getparam(MODNAME, 'radmax', Nhru_nmonths, 'real', Radmax)/=0 ) CALL read_error(2, 'radmax')
        Observed_flag = OFF
        IF ( Nsol > 0 ) THEN
          IF ( getparam(MODNAME, 'rad_conv', 1, 'real', Rad_conv_sngl)/=0 ) CALL read_error(2, 'rad_conv')
          Rad_conv = DBLE( Rad_conv_sngl )
          IF ( Basin_solsta>0 ) Observed_flag = ACTIVE
        ENDIF

        Cloud_radadj = 0.0

      ENDIF

      END FUNCTION ccsolrad
