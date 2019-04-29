!***********************************************************************
! DEPRECATED
! Superceded by ddsolrad
!
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a maximum temperature per degree-day relation.
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 is
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_DDSOLRAD_RADPL
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=13), SAVE :: MODNAME
        REAL, SAVE, ALLOCATABLE :: Plrad(:)
        ! Declared Variables
        REAL, SAVE, ALLOCATABLE :: Radpl_potsw(:)
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru_radpl(:)
        REAL, SAVE :: Radadj_slope, Radadj_intcp, Radmax
        REAL, SAVE :: Radj_sppt, Radj_wppt, Ppt_rad_adj(12)
        REAL, SAVE :: Dday_slope(12), Dday_intcp(12)
        REAL, SAVE :: Tmax_index(12), Tmax_allrain(12)
      END MODULE PRMS_DDSOLRAD_RADPL

!***********************************************************************
!     Main ddsolrad routine
!***********************************************************************
      INTEGER FUNCTION ddsolrad_prms()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: ddsoldecl, ddsolinit, ddsolrun
!***********************************************************************
      ddsolrad_prms = 0

      IF ( Process(:3)=='run' ) THEN
        ddsolrad_prms = ddsolrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        ddsolrad_prms = ddsoldecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        ddsolrad_prms = ddsolinit()
      ENDIF

      END FUNCTION ddsolrad_prms

!***********************************************************************
! ddsoldecl - set up parameters for actual solar radiation computations
!   Declared Parameters
!     dday_slope, dday_intcp, radj_sppt, radj_wppt, basin_solsta
!     radadj_slope, radadj_intcp, radmax, ppt_rad_adj, rad_conv
!     tmax_index, tmax_allrain, hru_radpl, hru_solsta
!***********************************************************************
      INTEGER FUNCTION ddsoldecl()
      USE PRMS_DDSOLRAD_RADPL
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_SOLTAB_RADPL, ONLY: Nradpl
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declparam, declvar
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_ddsolrad_prms
!***********************************************************************
      ddsoldecl = 1

      Version_ddsolrad_prms =
     +'$Id: ddsolrad_prms.f 6317 2014-03-28 21:46:37Z rsregan $'
      CALL print_module(Version_ddsolrad_prms,
     +                  'Solar Radiation             ', 77)
      MODNAME = 'ddsolrad_prms'

      ALLOCATE (Plrad(Nradpl))

! Declare Variables
      ALLOCATE (Radpl_potsw(Nradpl))
      IF ( declvar(MODNAME, 'radpl_potsw', 'nradpl', Nradpl, 'real',
     +     'Potential shortwave radiation for each radiation'//
     +     ' plane for each day',
     +     'langleys',
     +     Radpl_potsw).NE.0 ) RETURN

! Declare Parameters
      IF ( declparam(MODNAME, 'dday_slope', 'nmonths', 'real',
     +     '0.4', '0.2', '0.7',
     +     'Slope in temperature degree-day relationship',
     +     'Monthly (January to December) slope in'//
     +     ' degree-day equation',
     +     'dday/degree').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'dday_intcp', 'nmonths', 'real',
     +     '-10.0', '-60.0', '4.0',
     +     'Intercept in temperature degree-day relationship',
     +     'Monthly (January to December) intercept in'//
     +     ' degree-day equation',
     +     'dday').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'radadj_slope', 'one', 'real',
     +     '0.0', '0.0', '1.0',
     +     'Slope in air temperature range adjustment to solar'//
     +     ' radiation equation',
     +     'Slope in air temperature range adjustment to solar'//
     +     ' radiation equation',
     +     'dday/degree F').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'radadj_intcp', 'one', 'real',
     +     '1.0', '0.0', '1.0',
     +     'Intercept in air temperature range adjustment to solar'//
     +     ' radiation equation',
     +     'Intercept in air temperature range adjustment to solar'//
     +     ' radiation equation',
     +     'dday').NE.0 ) RETURN

      ALLOCATE (Hru_radpl(Nhru))
      IF ( declparam(MODNAME, 'hru_radpl', 'nhru', 'integer',
     +     '1', 'bounded', 'nradpl',
     +     'Index of radiation plane for HRU',
     +     'Index of radiation plane used to compute solar'//
     +     ' radiation for each HRU',
     +     'none').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'tmax_index', 'nmonths', 'real',
     +     '50.0', '-10.0', '110.0',
     +     'Monthly index temperature',
     +     'Monthly (January to December) index temperature used'//
     +     ' to determine precipitation adjustments to solar'//
     +     ' radiation',
     +     'degrees Fahrenheit').NE.0 ) RETURN

      IF ( declparam(MODNAME, 'tmax_allrain', 'nmonths', 'real',
     +     '38.0', '-8.0', '60.0',
     +     'Precipitation is rain if HRU max temperature >= this value',
     +     'Monthly (January to December) maximum air temperature'//
     +     ' when precipitation is assumed to be rain; if HRU air'//
     +     ' temperature is greater than or equal to this value,'//
     +     ' precipitation is rain',
     +     'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain')

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

      ddsoldecl = 0
      END FUNCTION ddsoldecl

!***********************************************************************
! ddsolinit - Initialize ddsolrad module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION ddsolinit()
      USE PRMS_DDSOLRAD_RADPL
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
!***********************************************************************
      ddsolinit = 1

      IF ( getparam(MODNAME, 'dday_slope', 12, 'real', Dday_slope)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'dday_intcp', 12, 'real', Dday_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'radadj_slope', 1, 'real', Radadj_slope)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'radadj_intcp', 1, 'real', Radadj_intcp)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'hru_radpl', Nhru, 'integer', Hru_radpl)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'tmax_index', 12, 'real', Tmax_index)
     +     .NE.0 ) RETURN

      IF ( getparam(MODNAME, 'tmax_allrain', 12, 'real', Tmax_allrain)
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

      ddsolinit = 0
      END FUNCTION ddsolinit

!***********************************************************************
! ddsolrun - Computes actual solar radiation on horizontal surface,
!            then determines values for each HRU.
!***********************************************************************
      INTEGER FUNCTION ddsolrun()
      USE PRMS_DDSOLRAD_RADPL
      USE PRMS_MODULE, ONLY: Nsol
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order,
     +    Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Orad,
     +    Basin_obs_ppt, Basin_solsta, Basin_horad, Basin_potsw, Swrad,
     +    Hru_solsta, Rad_conv
      USE PRMS_SOLTAB_RADPL, ONLY: Nradpl, Hemisphere, Radpl_soltab,
     +    Radpl_cossl
      USE PRMS_SET_TIME, ONLY: Jday, Nowmonth
      USE PRMS_OBS, ONLY: Solrad
      IMPLICIT NONE
      INTRINSIC INT
! Local Variables
      INTEGER :: j, kp, kp1, ir, jj, k
      REAL :: dday, pptadj, radadj, tdif, ddayi
! Save Variables
      REAL, SAVE, DIMENSION(26) :: solf
      DATA solf/.20, .35, .45, .51, .56, .59, .62, .64, .655, .67, .682,
     +          .69, .70, .71, .715, .72, .722, .724, .726, .728, .73,
     +          .734, .738, .742, .746, .75/
!***********************************************************************
      ddsolrun = 0

!rsr using julian day as the soltab arrays are filled by julian day
      Basin_horad = Radpl_soltab(Jday, 1)

      Orad = -999.0
      IF ( Nsol.GT.0 ) Orad = Solrad(Basin_solsta)*Rad_conv

      IF ( Orad.LT.0.0 .OR. Orad.GT.10000.0 ) THEN

        dday = (Dday_slope(Nowmonth)*Solrad_tmax) + Dday_intcp(Nowmonth)
     +         + 1.0
        IF ( dday.LT.1.0 ) dday = 1.0

        IF ( Basin_obs_ppt.LE.Ppt_rad_adj(Nowmonth) ) THEN
          pptadj = 1.0
        ELSEIF ( Solrad_tmax.GE.Tmax_index(Nowmonth) ) THEN
          tdif = Solrad_tmax - Tmax_index(Nowmonth)
          pptadj = Radadj_intcp + Radadj_slope*tdif
          IF ( pptadj.GT.1. ) pptadj = 1.0
        ELSE
          pptadj = Radj_wppt
          IF ( Solrad_tmax.GE.Tmax_allrain(Nowmonth) ) THEN
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
        ENDIF

        IF ( dday.LT.26.0 ) THEN
          kp = INT(dday)
          ddayi = kp
          kp1 = kp + 1
          radadj = solf(kp) + ((solf(kp1)-solf(kp))*(dday-ddayi))
        ELSE
          radadj = Radmax
        ENDIF
        radadj = radadj*pptadj
        IF ( radadj.LT.0.2 ) radadj = 0.2
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

      END FUNCTION ddsolrun
