!***********************************************************************
! DEPRECATED
! Superceded by ccsolrad
!
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a relation between solar radiation and cloud
! cover.
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 is
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_CCSOLRAD_RADPL
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=13), SAVE :: MODNAME
        REAL, SAVE, ALLOCATABLE :: Plrad(:)
        ! Declared Variables
        REAL, SAVE, ALLOCATABLE :: Radpl_potsw(:)
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru_radpl(:)
        REAL, SAVE :: Crad_coef, Crad_exp, Radj_sppt, Radj_wppt, Radmax
        REAL, SAVE :: Ccov_slope(12), Ccov_intcp(12), Ppt_rad_adj(12)
      END MODULE PRMS_CCSOLRAD_RADPL

!***********************************************************************
!     Main ccsolrad routine
!***********************************************************************
      INTEGER FUNCTION ccsolrad_prms()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: ccsoldecl, ccsolinit, ccsolrun
!***********************************************************************
      ccsolrad_prms = 0

      IF ( Process(:3)=='run' ) THEN
        ccsolrad_prms = ccsolrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        ccsolrad_prms = ccsoldecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        ccsolrad_prms = ccsolinit()
      ENDIF

      END FUNCTION ccsolrad_prms

!***********************************************************************
! ccsoldecl - set up parameters for actual solar radiation computations
!   Declared Parameters
!     ccov_slope, ccov_intcp, radj_sppt, radj_wppt, basin_solsta
!     crad_coef, crad_exp, radmax, ppt_rad_adj, rad_conv
!     hru_radpl, hru_area, hru_solsta
!***********************************************************************
      INTEGER FUNCTION ccsoldecl()
      USE PRMS_CCSOLRAD_RADPL
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_SOLTAB_RADPL, ONLY: Nradpl
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declparam, declvar
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_ccsolrad_prms
!***********************************************************************
      ccsoldecl = 1

      Version_ccsolrad_prms =
     +'$Id: ccsolrad_prms.f 6523 2014-06-18 22:42:29Z rsregan $'
      CALL print_module(Version_ccsolrad_prms,
     +                  'Solar Radiation             ', 77)
      MODNAME = 'ccsolrad_prms'

      ALLOCATE (Plrad(Nradpl))

! Declare Variables
      ALLOCATE (Radpl_potsw(Nradpl))
      IF ( declvar(MODNAME, 'radpl_potsw', 'nradpl', Nradpl, 'real',
     +     'Potential shortwave radiation for each radiation'//
     +     ' plane for each day',
     +     'langleys',
     +     Radpl_potsw).NE.0 ) RETURN

! Declare Parameters
      IF ( declparam(MODNAME, 'ccov_slope', 'nmonths', 'real',
     +     '-0.13', '-0.5', '-0.01',
     +     'Slope in temperature cloud cover relationship',
     +     'Monthly (January to December) coefficient in'//
     +     ' cloud-cover relationship',
     +     'none').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'ccov_intcp', 'nmonths', 'real',
     +     '1.83', '0.0', '5.0',
     +     'Intercept in temperature cloud cover relationship',
     +     'Monthly (January to December) intercept in'//
     +     ' cloud-cover relationship',
     +     'none').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'crad_coef', 'one', 'real',
     +     '0.4', '0.1', '0.7',
     +     'Coefficient in cloud cover-solar radiation relationship',
     +     'Coefficient(B) in Thompson(1976) equation',
     +     'none').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'crad_exp', 'one', 'real',
     +     '0.61', '0.2', '0.8',
     +     'Exponent in cloud cover-solar radiation relationship',
     +     'Exponent(P) in Thompson(1976) equation',
     +     'none').NE.0 ) RETURN

      ALLOCATE (Hru_radpl(Nhru))
      IF ( declparam(MODNAME, 'hru_radpl', 'nhru', 'integer',
     +     '1', 'bounded', 'nradpl',
     +     'Index of radiation plane for HRU',
     +     'Index of radiation plane used to compute solar'//
     +     ' radiation for each HRU',
     +     'none').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'radmax', 'one', 'real',
     +     '0.8', '0.1', '1.0',
     +     'Maximum fraction of potential solar radiation (decimal)',
     +     'Maximum fraction of the potential solar radiation that'//
     +    ' may reach the ground due to haze, dust, smog, and so forth',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'radj_sppt', 'one', 'real',
     +     '0.44', '0.0', '1.0',
     +    'Adjustment to solar radiation on precipitation day - summer',
     +     'Adjustment factor for computed solar radiation for summer'//
     +     ' day with greater than ppt_rad_adj inches of precipitation',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'radj_wppt', 'one', 'real',
     +     '0.5', '0.0', '1.0',
     +    'Adjustment to solar radiation on precipitation day - winter',
     +     'Adjustment factor for computed solar radiation for winter'//
     +     ' day with greater than ppt_rad_adj inches of precipitation',
     +     'decimal fraction').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'ppt_rad_adj', 'nmonths', 'real',
     +     '0.02', '0.0', '0.5',
     +     'Radiation reduced if basin precipitation above this value',
     +     'Monthly minimum precipitation, if basin precipitation'//
     +     ' exceeds this value, radiation is'//
     +     ' multiplied by radj_sppt or radj_wppt adjustment factor',
     +     'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')

      ccsoldecl = 0
      END FUNCTION ccsoldecl

!***********************************************************************
! ccsolinit - Initialize ccsolrad module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION ccsolinit()
      USE PRMS_CCSOLRAD_RADPL
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      ccsolinit = 1

      IF ( getparam(MODNAME, 'ccov_slope', 12, 'real', Ccov_slope)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'ccov_intcp', 12, 'real', Ccov_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'hru_radpl', Nhru, 'integer', Hru_radpl)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'crad_coef', 1, 'real', Crad_coef)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'crad_exp', 1, 'real', Crad_exp)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'radmax', 1, 'real', Radmax)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'radj_sppt', 1, 'real', Radj_sppt)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'radj_wppt', 1, 'real', Radj_wppt)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'ppt_rad_adj', 12, 'real', Ppt_rad_adj)
     +     .NE.0 ) RETURN

      Radpl_potsw = 0.0

      ccsolinit = 0
      END FUNCTION ccsolinit

!***********************************************************************
! ccsolrun - Computes actual solar radiation on horizontal surface,
!            then determines values for each HRU
!***********************************************************************
      INTEGER FUNCTION ccsolrun()
      USE PRMS_CCSOLRAD_RADPL
      USE PRMS_MODULE, ONLY: Nsol
      USE PRMS_SET_TIME, ONLY: Nowmonth, Jday
      USE PRMS_OBS, ONLY: Solrad
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order,
     +    Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin,
     +    Basin_obs_ppt, Basin_solsta, Orad, Basin_horad,
     +    Basin_potsw, Swrad, Rad_conv, Hru_solsta
      USE PRMS_SOLTAB_RADPL, ONLY: Nradpl, Hemisphere, Radpl_soltab,
     +    Radpl_cossl
      IMPLICIT NONE
! Local Variables
      INTEGER :: j, ir, jj, k
      REAL :: ccov, pptadj, radadj
!***********************************************************************
      ccsolrun = 0

!rsr using julian day as the soltab arrays are filled by julian day
      Basin_horad = Radpl_soltab(Jday, 1)

      Orad = -999.0
      IF ( Nsol.GT.0 ) Orad = Solrad(Basin_solsta)*Rad_conv

      IF ( Orad.LT.0.0 .OR. Orad.GT.10000.0 ) THEN

        ccov = Ccov_slope(Nowmonth)*(Solrad_tmax-Solrad_tmin)
     +         + Ccov_intcp(Nowmonth)
        IF ( ccov<NEARZERO ) THEN
          ccov = 0.0
        ELSEIF ( ccov.GT.1.0 ) THEN
          ccov = 1.0
        ENDIF

        pptadj = 1.0
        IF ( Basin_obs_ppt>Ppt_rad_adj(Nowmonth) ) THEN
          IF ( Hemisphere==0 ) THEN ! Northern Hemisphere
            IF ( Jday<79 .OR. Jday>265 ) THEN ! Equinox
              pptadj = Radj_wppt
            ELSE
              pptadj = Radj_sppt
            ENDIF
          ELSE ! Southern Hemisphere
            IF ( Jday>79 .AND. Jday<265 ) THEN ! Equinox
              pptadj = Radj_wppt
            ELSE
              pptadj = Radj_sppt
            ENDIF
          ENDIF
        ENDIF
        radadj = Crad_coef + (1.0-Crad_coef)*((1.0-ccov)**Crad_exp)
        IF ( radadj.GT.Radmax ) radadj = Radmax
        radadj = radadj*pptadj
        Orad = radadj*Basin_horad
      ENDIF

      DO j = 1, Nradpl
        Radpl_potsw(j) = Radpl_soltab(Jday, j)
        Plrad(j) = Radpl_potsw(j)/Basin_horad*Orad/Radpl_cossl(j)
      ENDDO

      Basin_potsw = 0.0D0
      IF ( Nsol==0 ) THEN
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          ir = Hru_radpl(j)
          Swrad(j) = Plrad(ir)
          Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
        ENDDO
      ELSE
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_solsta(j)
          IF ( k==0 .OR. k>Nsol ) THEN
            ir = Hru_radpl(j)
            Swrad(j) = Plrad(ir)
          ELSE
            Swrad(j) = Solrad(k)*Rad_conv
          ENDIF
          Basin_potsw = Basin_potsw + Swrad(j)*Hru_area(j)
        ENDDO
      ENDIF
      Basin_potsw = Basin_potsw*Basin_area_inv

      END FUNCTION ccsolrun
