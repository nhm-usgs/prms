!***********************************************************************
! Determines the form of precipitation and distributes precipitation
! and temperatures to each HRU based on measurements at stations with
! closest elevation or shortest distance to the respective HRU
!
! This is set up to work for Temperature in oF and Precipitation in inches,
! and elevation in meters!!!!
!
! Lauren Hay, November 2004
!***********************************************************************
      MODULE PRMS_IDE
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Temp_nsta, Rain_nsta
      CHARACTER(LEN=8), SAVE :: MODNAME
      INTEGER, SAVE, ALLOCATABLE :: Rain_nuse(:), Temp_nuse(:)
      REAL, SAVE :: Dalr
      DOUBLE PRECISION, SAVE :: Basin_centroid_x, Basin_centroid_y
      REAL, SAVE, ALLOCATABLE :: Temp_wght_elev(:), Prcp_wght_elev(:)
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Tmax_rain_sta(:), Tmin_rain_sta(:)
!   Declared Parameters
      REAL, SAVE :: Solrad_elev
      INTEGER, SAVE, ALLOCATABLE :: Tsta_nuse(:)
      INTEGER, SAVE, ALLOCATABLE :: Psta_nuse(:)
      REAL, SAVE, ALLOCATABLE :: Hru_x(:), Hru_y(:)
      REAL, SAVE, ALLOCATABLE :: Tsta_x(:), Tsta_y(:)
      REAL, SAVE, ALLOCATABLE :: Psta_x(:), Psta_y(:)
      INTEGER, SAVE :: Ndist_tsta, Ndist_psta
      REAL, SAVE :: Dist_exp
      REAL, SAVE, ALLOCATABLE :: Temp_wght_dist(:), Prcp_wght_dist(:)
      END MODULE PRMS_IDE

!***********************************************************************
!     Main ide_dist routine
!***********************************************************************
      INTEGER FUNCTION ide_dist()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: idedecl, ideinit, iderun
!***********************************************************************
      ide_dist = 0

      IF ( Process(:3)=='run' ) THEN
        ide_dist = iderun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        ide_dist = idedecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        ide_dist = ideinit()
      ENDIF

      END FUNCTION ide_dist

!***********************************************************************
!     idedecl - set up parameters for temperature computations
!***********************************************************************
      INTEGER FUNCTION idedecl()
      USE PRMS_IDE
      USE PRMS_MODULE, ONLY: Nhru, Ntemp, Nrain
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: n, nc
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Climate Distribuition'
      CHARACTER(LEN=80), SAVE :: Version_ide_dist
!***********************************************************************
      idedecl = 0

      Version_ide_dist =
     +'$Id: ide_dist.f 5203 2013-01-09 01:25:02Z rsregan $'
      nc = INDEX( Version_ide_dist, 'Z' )
      n = INDEX( Version_ide_dist, '.f' ) + 1
      IF ( declmodule(Version_ide_dist(6:n), PROCNAME,
     +                Version_ide_dist(n+2:nc))/=0 ) STOP
      MODNAME = 'ide_dist'

      ALLOCATE ( Tmax_rain_sta(Nrain) )
      IF ( declvar(MODNAME, 'tmax_rain_sta', 'nrain', Nrain, 'real',
     +     'Maximum temperature distributed to the precipitation'//
     +     ' measurement stations',
     +     'degrees F',
     +     Tmax_rain_sta)/=0 ) CALL read_error(3, 'tmax_rain_sta')

      ALLOCATE ( Tmin_rain_sta(Nrain) )
      IF ( declvar(MODNAME, 'tmin_rain_sta', 'nrain', Nrain, 'real',
     +     'Minimum temperature distributed to the precipitation'//
     +     ' measurement stations',
     +     'degrees F',
     +     Tmin_rain_sta)/=0 ) CALL read_error(3, 'tmin_rain_sta')

      ALLOCATE ( Hru_x(Nhru), Hru_y(Nhru), Tsta_x(Ntemp) )
      ALLOCATE ( Tsta_y(Ntemp), Psta_x(Nrain), Psta_y(Nrain) )
      ALLOCATE ( Temp_wght_dist(12), Temp_wght_elev(12) )
      ALLOCATE ( Temp_nuse(Ntemp), Rain_nuse(Nrain) )
      ALLOCATE ( Prcp_wght_dist(12), Prcp_wght_elev(12) )
! declare parameters
      IF ( declparam(MODNAME, 'hru_x', 'nhru', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X for each HRU (albers)',
     +     'Longitude (X) for each HRU in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'hru_x')

      IF ( declparam(MODNAME, 'hru_y', 'nhru', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y for each HRU (albers)',
     +     'Latitude (Y) for each HRU in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'hru_y')

      IF ( declparam(MODNAME, 'tsta_x', 'ntemp', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X for each temperature station (albers)',
     +     'Longitude (X) for each temperature measurement station in'//
     +     ' albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'tsta_x')

      IF ( declparam(MODNAME, 'tsta_y', 'ntemp', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y for each temperature station (albers)',
     +     'Latitude (Y) for each temperature measurement station in'//
     +     ' albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'tsta_y')

      IF ( declparam(MODNAME, 'psta_x', 'nrain', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'X for each precipitation station (albers)',
     +     'Longitude (X) for each precipitation measurement station'//
     +     ' in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'psta_x')

      IF ( declparam(MODNAME, 'psta_y', 'nrain', 'real',
     +     '0.0', '-1.0E7', '1.0E7',
     +     'Y for each precipitation station (albers)',
     +     'Latitude (Y) for each precipitation measurement station'//
     +     ' in albers projection',
     +     'meters')/=0 ) CALL read_error(1, 'psta_y')

      ALLOCATE ( Tsta_nuse(Ntemp) )
      IF ( declparam(MODNAME, 'tsta_nuse', 'ntemp', 'integer',
     +     '1', '0', '1',
     +     '0 = station not used; 1 = station used',
     +     'The subset of temperature measurement stations used in'//
     +     ' the distribution regression (0=station not used;'//
     +     ' 1=station used)',
     +     'none')/=0 ) CALL read_error(1, 'tsta_nuse')

      IF ( declparam(MODNAME, 'solrad_elev', 'one', 'real',
     +     '1000.0', '0.0', '10000.0',
     +     'Elevation of the solrad station used for DD curves',
     +     'Elevation of the solar radiation station used for'//
     +     ' degree-day curves',
     +     'meters')/=0 ) CALL read_error(1, 'solrad_elev')

      ALLOCATE ( Psta_nuse(Nrain) )
      IF ( declparam(MODNAME, 'psta_nuse', 'nrain', 'integer',
     +     '1', '0', '1',
     +     'The subset of precipitation stations used in the'//
     +     ' distribution regression (0=station not used;'//
     +     ' 1=station used)',
     +     'The subset of precipitation measurement stations used in'//
     +     ' the distribution regression (0=station not used;'//
     +     ' 1=station used)',
     +     'none')/=0 ) CALL read_error(1, 'psta_nuse')

! New parameters:
      IF ( declparam(MODNAME, 'temp_wght_dist', 'nmonths', 'real',
     +     '0.5', '0.0', '1.0',
     +     'Weighting function for inverse distance: temperature',
     +     'Monthly (January to December) temperature weighting'//
     +     ' function for inverse distance calculations',
     +     'none')/=0 ) CALL read_error(1, 'temp_wght_dist')

      IF ( declparam(MODNAME, 'prcp_wght_dist', 'nmonths', 'real',
     +     '0.5', '0.0', '1.0',
     +     'Weighting function for inverse distance: precipitation',
     +     'Monthly (January to December) precipitation weighting'//
     +     ' function for inverse distance calculations',
     +     'none')/=0 ) CALL read_error(1, 'prcp_wght_dist')

      IF ( declparam(MODNAME, 'dist_exp', 'one', 'real',
     +     '2.0', '0.0', '10.0',
     +     'Exponent for inverse distance calculations',
     +     'Exponent for inverse distance calculations',
     +     'none')/=0 ) CALL read_error(1, 'dist_exp')

      IF ( declparam(MODNAME, 'ndist_psta', 'one', 'integer',
     +     '3', '1', '100',
     +     'Number of stations for inverse distance calcs:'//
     +     ' precipitation',
     +     'Number of precipitation measurement stations for inverse'//
     +     ' distance calculations',
     +     'none')/=0 ) CALL read_error(1, 'ndist_psta')

      IF ( declparam(MODNAME, 'ndist_tsta', 'one', 'integer',
     +     '3', '1', '100',
     +     'Number of stations for inverse distance calcs: temperature',
     +     'Number of temperature measurement stations for inverse'//
     +     ' distance calculations',
     +     'none')/=0 ) CALL read_error(1, 'ndist_tsta')

      END FUNCTION idedecl

!***********************************************************************
!     ideinit - Initialize ide_dist module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION ideinit()
      USE PRMS_IDE
      USE PRMS_MODULE, ONLY: Nhru, Ntemp, Nrain
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Timestep,
     +    Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Tsta_elev_meters,
     +    Psta_elev_meters
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
! Local Variables
      INTEGER i, ii
!***********************************************************************
      ideinit = 0

! Initialize declared variables
      IF ( Timestep==0 ) THEN
        Tmax_rain_sta = 0.0
        Tmin_rain_sta = 0.0
      ENDIF

      IF ( getparam(MODNAME, 'solrad_elev', 1, 'real', Solrad_elev)
     +     /=0 ) CALL read_error(2, 'solrad_elev')

      IF ( getparam(MODNAME, 'hru_x', Nhru, 'real', Hru_x)
     +     /=0 ) CALL read_error(2, 'hru_x')

      IF ( getparam(MODNAME, 'hru_y', Nhru, 'real', Hru_y)
     +     /=0 ) CALL read_error(2, 'hru_y')

      IF ( getparam(MODNAME, 'tsta_x', Ntemp, 'real', Tsta_x)
     +     /=0 ) CALL read_error(2, 'tsta_x')

      IF ( getparam(MODNAME, 'tsta_y', Ntemp, 'real', Tsta_y)
     +     /=0 ) CALL read_error(2, 'tsta_y')

      IF ( getparam(MODNAME, 'psta_x', Nrain, 'real', Psta_x)
     +     /=0 ) CALL read_error(2, 'psta_x')

      IF ( getparam(MODNAME, 'psta_y', Nrain, 'real', Psta_y)
     +     /=0 ) CALL read_error(2, 'psta_y')

      IF ( getparam(MODNAME, 'tsta_nuse', Ntemp, 'integer',
     +     Tsta_nuse)/=0 ) CALL read_error(2, 'tsta_nuse')

      IF ( getparam(MODNAME, 'psta_nuse', Nrain, 'integer',
     +     Psta_nuse)/=0 ) CALL read_error(2, 'psta_nuse')

      IF ( getparam(MODNAME, 'temp_wght_dist', 12, 'real',
     +     Temp_wght_dist)/=0 ) CALL read_error(2, 'temp_wght_dist')

      IF ( getparam(MODNAME, 'prcp_wght_dist', 12, 'real',
     +     Prcp_wght_dist)/=0 ) CALL read_error(2, 'prcp_wght_dist')

      IF ( getparam(MODNAME, 'dist_exp', 1, 'real', Dist_exp)
     +     /=0 ) CALL read_error(2, 'dist_exp')

      IF ( getparam(MODNAME, 'ndist_psta', 1, 'integer',
     +     Ndist_psta)/=0 ) CALL read_error(2, 'ndist_psta')

      IF ( getparam(MODNAME, 'ndist_tsta', 1, 'integer',
     +     Ndist_tsta)/=0 ) CALL read_error(2, 'ndist_tsta')

! dry adiabatic lapse rate (DALR) when extrapolating
!       (DALR = 5.4oF/1000 meters)
!      Dalr = 0.0177
      Dalr = 5.4/1000.0
!
! Compute basin centroid
!
      Basin_centroid_x = 0.0D0
      Basin_centroid_y = 0.0D0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        Basin_centroid_x = Basin_centroid_x + (Hru_area(i)*Hru_x(i))
        Basin_centroid_y = Basin_centroid_y + (Hru_area(i)*Hru_y(i))
      ENDDO
      Basin_centroid_x = Basin_centroid_x*Basin_area_inv
      Basin_centroid_y = Basin_centroid_y*Basin_area_inv

      Temp_nsta = 0
      DO i = 1, Ntemp
        IF ( Tsta_nuse(i)==1 ) THEN
          Temp_nsta = Temp_nsta + 1
          Temp_nuse(Temp_nsta) = i
        ENDIF
      ENDDO

      Rain_nsta = 0
      DO i = 1, Nrain
        IF ( Psta_nuse(i)==1 ) THEN
          Rain_nsta = Rain_nsta + 1
          Rain_nuse(Rain_nsta) = i
        ENDIF
      ENDDO
      DEALLOCATE ( Psta_nuse, Tsta_nuse )

      DO i = 1, 12
        Temp_wght_elev(i) = 1.0 - Temp_wght_dist(i)
        Prcp_wght_elev(i) = 1.0 - Prcp_wght_dist(i)
      ENDDO

      END FUNCTION ideinit

!***********************************************************************
!     iderun - Temperature calculation
!               calculates daily max and min temperature
!               using data from available stations
!               Outputs a daily max and min Temperature by HRU elevation
!***********************************************************************
      INTEGER FUNCTION iderun()
      USE PRMS_IDE
      USE PRMS_CLIMATEVARS, ONLY: Adjmix_rain, Adjust_snow, Adjust_rain,
     +    Tmax_allrain
      USE PRMS_OBS, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: ide_temp_run, ide_rain_run
! Local variables
      INTEGER foo
!***********************************************************************
      iderun = 0

      foo = ide_temp_run(Temp_wght_dist(Nowmonth),
     +      Temp_wght_elev(Nowmonth))

      foo = ide_rain_run(Prcp_wght_dist(Nowmonth),
     +      Prcp_wght_elev(Nowmonth), Tmax_allrain(Nowmonth),
     +      Adjust_snow(Nowmonth), Adjust_rain(Nowmonth),
     +      Adjmix_rain(Nowmonth))

      END FUNCTION iderun

!***********************************************************************
!     ide_temp_run - Temperature calculation
!               calculates daily max and min temperature
!               using data from available stations
!               Outputs a daily max and min Temperature by HRU elevation
!***********************************************************************
      INTEGER FUNCTION ide_temp_run(Temp_wght_dist, Temp_wght_elev)
      USE PRMS_IDE, ONLY: Hru_x, Hru_y, Temp_nsta, Tsta_x,
     +    Tsta_y, Temp_nuse, Tmax_rain_sta, Dist_exp, Solrad_elev,
     +    Tmin_rain_sta, Psta_x, Psta_y, Basin_centroid_x,
     +    Basin_centroid_y, Ndist_tsta
      USE PRMS_MODULE, ONLY: Ntemp, Nrain
      USE PRMS_BASIN, ONLY: Hru_route_order, Active_hrus, Hru_area,
     +    Basin_area_inv, Hru_elev_meters
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp,
     +    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf,
     +    Tavgc, Temp_units, Tmax_adj, Tmin_adj, Tsta_elev_meters,
     +    Psta_elev_meters
      USE PRMS_OBS, ONLY: Tmax, Tmin
      IMPLICIT NONE
! Functions
      REAL, EXTERNAL :: c_to_f
      EXTERNAL :: compute_inv, compute_elv, temp_set
! Arguments
      REAL, INTENT(IN) :: Temp_wght_dist, Temp_wght_elev
! Local Variables
      INTEGER n, nn, itype
      REAL x, y, z, x1, tmax_hru, tmin_hru
      REAL centroid_x, centroid_y
      DOUBLE PRECISION dat_dist, dat_elev
!***********************************************************************
      ide_temp_run = 0

      Basin_tmax = 0.0D0
      Basin_tmin = 0.0D0
      Basin_temp = 0.0D0

!
! TEMPERATURE CALCULATIONS:
!       itype=1  -- maximum temperature
!       itype=-1 -- minimum temperature
!   !! only works in degrees F, fixed rsr 10/2007
!----------------
!----------------
      DO nn = 1, Active_hrus
        n = Hru_route_order(nn)
!----------------
!----------------
        itype = 1
        dat_elev = 0.0D0
        dat_dist = 0.0D0
        x = Hru_x(n)
        y = Hru_y(n)
        z = Hru_elev_meters(n)
        IF ( Temp_wght_dist.GT.0.0 )
     +       CALL compute_inv(Ntemp, Temp_nsta, Temp_nuse, Tsta_x, x,
     +       Tsta_y, y, Tmax, dat_dist, Ndist_tsta, Dist_exp)
        IF ( Temp_wght_elev.GT.0.0 )
     +       CALL compute_elv(Ntemp, Temp_nsta, Temp_nuse,
     +            Tsta_elev_meters, z, Tmax, dat_elev, itype)
        tmax_hru = (Temp_wght_dist*dat_dist) + (Temp_wght_elev*dat_elev)
!
!  Temperature adjustment by HRU
!
        tmax_hru = tmax_hru + Tmax_adj(n)

        itype = -1
        dat_elev = 0.0D0
        dat_dist = 0.0D0
        IF ( Temp_wght_dist.GT.0.0 )
     +       CALL compute_inv(Ntemp, Temp_nsta, Temp_nuse, Tsta_x, x,
     +       Tsta_y, y, Tmin, dat_dist, Ndist_tsta, Dist_exp)
        IF ( Temp_wght_elev.GT.0.0 )
     +       CALL compute_elv(Ntemp, Temp_nsta, Temp_nuse,
     +            Tsta_elev_meters, z, Tmin, dat_elev, itype)
        tmin_hru = (Temp_wght_dist*dat_dist) + (Temp_wght_elev*dat_elev)
!
!  Temperature adjustment by HRU
!
        tmin_hru = tmin_hru + Tmin_adj(n)
        IF ( Temp_units==1 ) THEN
          tmax_hru = c_to_f(tmax_hru)
          tmin_hru = c_to_f(tmin_hru)
        ENDIF
!
!  If max is less than min, switch
!
        IF ( tmax_hru<tmin_hru ) THEN
          x1 = tmax_hru
          tmax_hru = tmin_hru
          tmin_hru = x1
        ENDIF
!
!  Now sort out units.
!
        CALL temp_set(n, tmax_hru, tmin_hru, Tmaxf(n), Tminf(n),
     +       Tavgf(n), Tmaxc(n), Tminc(n), Tavgc(n), Hru_area(n))

!----------------
!----------------
      ENDDO
!----------------
!----------------
      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv
      Basin_temp = Basin_temp*Basin_area_inv
!
!  Compute maximum temperature at XY centroid of basin at the elevation of
!  the solrad station used to develop the DD solrad curves.
!
!     solrad_elev
!     basin_centroid_x
!     basin_centroid_y
      itype = 1
      dat_elev = 0.0D0
      dat_dist = 0.0D0
      centroid_x = Basin_centroid_x
      centroid_y = Basin_centroid_y
      IF ( Temp_wght_dist.GT.0.0 )
     +     CALL compute_inv(Ntemp, Temp_nsta, Temp_nuse, Tsta_x,
     +          centroid_x, Tsta_y, centroid_y, Tmax,
     +          dat_dist, Ndist_tsta, Dist_exp)
      IF ( Temp_wght_elev.GT.0.0 )
     +     CALL compute_elv(Ntemp, Temp_nsta, Temp_nuse,
     +          Tsta_elev_meters, Solrad_elev, Tmax, dat_elev, itype)
      Solrad_tmax = (Temp_wght_dist*dat_dist)
     +              + (Temp_wght_elev*dat_elev)
      IF ( Temp_wght_dist.GT.0.0 )
     +     CALL compute_inv(Ntemp, Temp_nsta, Temp_nuse, Tsta_x,
     +          centroid_x, Tsta_y, centroid_y, Tmin,
     +          dat_dist, Ndist_tsta, Dist_exp)
      IF ( Temp_wght_elev.GT.0.0 )
     +     CALL compute_elv(Ntemp, Temp_nsta, Temp_nuse,
     +          Tsta_elev_meters, Solrad_elev, Tmin, dat_elev, itype)
      Solrad_tmin = (Temp_wght_dist*dat_dist)
     +              + (Temp_wght_elev*dat_elev)
!
!  Compute temperatures at precipitation stations.
!
      DO n = 1, Nrain
        itype = 1
        dat_elev = 0.0D0
        dat_dist = 0.0D0
        x = Psta_x(n)
        y = Psta_y(n)
        z = Psta_elev_meters(n)
        IF ( Temp_wght_dist.GT.0.0 )
     +       CALL compute_inv(Ntemp, Temp_nsta, Temp_nuse, Tsta_x, x,
     +       Tsta_y, y, Tmax, dat_dist, Ndist_tsta, Dist_exp)
        IF ( Temp_wght_elev.GT.0.0 )
     +       CALL compute_elv(Ntemp, Temp_nsta, Temp_nuse,
     +            Tsta_elev_meters, z, Tmax, dat_elev, itype)
        Tmax_rain_sta(n) = (Temp_wght_dist*dat_dist)
     +                     + (Temp_wght_elev*dat_elev)
        itype = -1
        dat_elev = 0.0D0
        dat_dist = 0.0D0
        IF ( Temp_wght_dist.GT.0.0 )
     +       CALL compute_inv(Ntemp, Temp_nsta, Temp_nuse, Tsta_x, x,
     +            Tsta_y, y, Tmin, dat_dist, Ndist_tsta, Dist_exp)
        IF ( Temp_wght_elev.GT.0.0 )
     +       CALL compute_elv(Ntemp, Temp_nsta, Temp_nuse,
     +            Tsta_elev_meters, z, Tmin, dat_elev, itype)
        Tmin_rain_sta(n) = (Temp_wght_dist*dat_dist)
     +                     + (Temp_wght_elev*dat_elev)
        IF ( Temp_units==1 ) THEN
          Tmax_rain_sta(n) = c_to_f(Tmax_rain_sta(n))
          Tmin_rain_sta(n) = c_to_f(Tmin_rain_sta(n))
        ENDIF
      ENDDO

      END FUNCTION ide_temp_run

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION ide_rain_run(Prcp_wght_dist, Prcp_wght_elev,
     +                              Tmax_allrain, Adjust_snow,
     +                              Adjust_rain, Adjmix_rain)
      USE PRMS_IDE, ONLY: Hru_x, Hru_y, Psta_x, Psta_y,
     +    Rain_nuse, Rain_nsta, Tmax_rain_sta, Tmin_rain_sta,
     +    Ndist_psta, Dist_exp
      USE PRMS_MODULE, ONLY: Nrain
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Hru_elev_meters,
     +    Hru_route_order, Active_hrus, DNEARZERO, MM2INCH
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tminf, Newsnow, Pptmix,
     +    Prmx, Hru_ppt, Basin_ppt, Hru_rain, Hru_snow, Basin_rain,
     +    Basin_snow, Tmax_allsnow, Psta_elev_meters, Basin_obs_ppt,
     +    Precip_units
      USE PRMS_OBS, ONLY: Nowtime, Precip
      IMPLICIT NONE
! Functions
      INTRINSIC ABS
      EXTERNAL compute_inv, compute_elv, precip_form
! Arguments
      REAL, INTENT(IN) :: Prcp_wght_dist, Prcp_wght_elev, Tmax_allrain
      REAL, INTENT(IN) :: Adjust_snow, Adjust_rain, Adjmix_rain
! Local variables
      INTEGER i, j, err_chk, n, itype, allmissing, nn
      REAL x, y, z, ppt_sngl
      DOUBLE PRECISION dat_dist, sum_obs, dat_elev, ppt
!***********************************************************************
      ide_rain_run = 0
!
! add adjust_snow and adjust_rain here
!
      allmissing = 0
      DO j = 1, Rain_nsta
        i = Rain_nuse(j)
        IF ( Precip(i)>=0.0 ) THEN
 
          err_chk = 0
          IF ( Tmax_rain_sta(i)<=Tmax_allsnow ) THEN
            err_chk = 1
          ELSEIF ( Tmin_rain_sta(i)>Tmax_allsnow .OR.
     +             Tmax_rain_sta(i)>=Tmax_allrain ) THEN
            err_chk = 0
          ELSE
            err_chk = 1
          ENDIF

          IF ( err_chk==1 ) THEN
            Precip(i) = (Precip(i)*Adjust_snow) + Precip(i)
          ELSE
            Precip(i) = (Precip(i)*Adjust_rain) + Precip(i)
          ENDIF
          allmissing = 1
        ELSE !make sure negative precipitation values are < -99.0
          Precip(i) = -100.0
        ENDIF
 
      ENDDO
      IF ( allmissing==0 ) THEN
        PRINT *, 'ERROR, all precipitation stations have missing data',
     +           Nowtime
        STOP
      ENDIF

      Basin_ppt = 0.0D0
      Basin_rain = 0.0D0
      Basin_snow = 0.0D0

! PRECIPITATION CALCULATIONS
      itype = 2
!  precipitation in inches and elevation in meters!!!!!
!----------------
!----------------
      sum_obs = 0.0D0
      DO nn = 1, Active_hrus
        n = Hru_route_order(nn)
!----------------
!******Zero precipitation on HRU
!----------------
        Newsnow(n) = 0
        Pptmix(n) = 0
        Prmx(n) = 0.0
        Hru_rain(n) = 0.0
        Hru_snow(n) = 0.0
        Hru_ppt(n) = 0.0

! Calculate prcp at given HRU:
        dat_elev = 0.0D0
        dat_dist = 0.0D0
        x = Hru_x(n)
        y = Hru_y(n)
        z = Hru_elev_meters(n)
        IF ( Prcp_wght_dist>0.0 )
     +       CALL compute_inv(Nrain, Rain_nsta, Rain_nuse, Psta_x, x,
     +            Psta_y, y, Precip, dat_dist, Ndist_psta, Dist_exp)
        IF ( dat_dist<0.0D0 ) dat_dist = 0.0D0

        IF ( Prcp_wght_elev>0.0 )
     +       CALL compute_elv(Nrain, Rain_nsta, Rain_nuse,
     +            Psta_elev_meters, z, Precip, dat_elev, itype)
        IF ( dat_elev<0.0D0 ) dat_elev = 0.0D0

        ppt = (Prcp_wght_dist*dat_dist) + (Prcp_wght_elev*dat_elev)

        IF ( ppt>DNEARZERO ) THEN
          IF ( Precip_units==1 ) ppt = ppt*MM2INCH
          ppt_sngl = SNGL(ppt)
          CALL precip_form(ppt_sngl, Hru_ppt(n),Hru_rain(n),Hru_snow(n),
     +         Tmaxf(n), Tminf(n), Pptmix(n), Newsnow(n), Prmx(n),
     +         Tmax_allrain, 1.0, 1.0, Adjmix_rain, Hru_area(n),
     +         sum_obs)
        ENDIF
!----------------
!----------------
      ENDDO
!----------------
!----------------

      Basin_ppt = Basin_ppt*Basin_area_inv
      Basin_rain = Basin_rain*Basin_area_inv
      Basin_snow = Basin_snow*Basin_area_inv
      Basin_obs_ppt = sum_obs*Basin_area_inv

      END FUNCTION ide_rain_run

!***********************************************************************
! Calculates climate value based on inverse distance squared
!***********************************************************************
      SUBROUTINE compute_inv(Imax, Nsta, Nuse, Sta_x, X, Sta_y, Y, Dat,
     +                       Dat_dist, Ndist, Dist_exp)
      USE PRMS_OBS, ONLY: Nowtime
      IMPLICIT NONE
      INTRINSIC SQRT, FLOAT
      EXTERNAL SORT2I
! Arguments
      INTEGER, INTENT(IN) :: Imax, Ndist, Nsta
      INTEGER, DIMENSION(Imax), INTENT(IN) :: Nuse
      REAL, DIMENSION(Imax), INTENT(IN) :: Sta_x, Sta_y, Dat
      REAL, INTENT(IN) :: Dist_exp, X, Y
      DOUBLE PRECISION, INTENT(OUT) :: Dat_dist
! Local Variables
      INTEGER :: idist, jj, k, i, j, allmissing
      INTEGER, DIMENSION(Imax) :: ndist_sta, rb
      DOUBLE PRECISION :: sum, dist_mean
      DOUBLE PRECISION, DIMENSION(Imax) :: dist, wght_dist, ra
!***********************************************************************
!----------------
!----------------
! calculate the distance differences:
      allmissing = 0
      DO j = 1, Nsta
        i = Nuse(j)
        dist(i) = -9999.0D0
        IF ( Dat(i)>-95.0 ) THEN
          dist(i) = SQRT(((Sta_x(i)-X)**2+(Sta_y(i)-Y)**2))
          IF ( dist(i)<1.1D0 ) dist(i) = 1.1D0
          allmissing = 1
        ENDIF
      ENDDO
      IF ( allmissing==0 ) THEN
        PRINT *, 'ERROR, all temperature stations have missing data',
     +           Nowtime
        STOP
      ENDIF
!
! calculate weighting functions for ndist closest stations
!
      jj = 0
      DO j = 1, Nsta
        i = Nuse(j)
        IF ( dist(i)/=-9999.0D0 ) THEN
          jj = jj + 1
          ra(jj) = dist(i)
          rb(jj) = i
        ENDIF
      ENDDO
      IF ( jj>1 ) CALL SORT2I(Imax, jj, ra, rb)
      idist = Ndist
      IF ( Ndist>jj ) idist = jj
      sum = 0.0D0
      DO k = 1, idist
        j = rb(k)
        sum = sum + dist(j)
      ENDDO
      dist_mean = sum/FLOAT(idist)
      DO k = 1, idist
        j = rb(k)
        ndist_sta(k) = j
        wght_dist(k) = 1.0D0/((dist(j)/dist_mean)**Dist_exp)
      ENDDO
!
      sum = 0.0D0
      DO j = 1, idist
        sum = sum + wght_dist(j)
      ENDDO
      DO j = 1, idist
        wght_dist(j) = wght_dist(j)/sum
      ENDDO
!
      Dat_dist = 0.0D0
      DO j = 1, idist
        k = ndist_sta(j)
        Dat_dist = Dat_dist + (wght_dist(j)*Dat(k))
      ENDDO
!
      END SUBROUTINE compute_inv

!***********************************************************************
! Calculates climate value based on elevation trend in basin
! abs(Itype)=1 -- TEMPERATURE
! Itype=2 -- PRECIPITATION
!***********************************************************************
      SUBROUTINE compute_elv(Imax, Nsta, Nuse, Sta_z, Z, Data, Dat_elev,
     +                       Itype)
      USE PRMS_IDE, ONLY: Dalr
      IMPLICIT NONE
      INTRINSIC FLOAT, ABS
      EXTERNAL SORT2, SORT2I
! Arguments
      INTEGER, INTENT(IN) :: Imax, Itype, Nsta
      INTEGER, DIMENSION(Imax), INTENT(IN) :: Nuse
      REAL, INTENT(IN) :: Z
      REAL, DIMENSION(Imax), INTENT(IN) :: Data, Sta_z
      DOUBLE PRECISION, INTENT(OUT) :: Dat_elev
! Local Variables
      INTEGER nn, j1, j2, num, n, i, j
      INTEGER, DIMENSION(Imax) :: rb
      DOUBLE PRECISION, DIMENSION(Imax) :: dat0, elev, xmnelv, xmndat
      DOUBLE PRECISION :: slope, b
      DOUBLE PRECISION, DIMENSION(Imax) :: dat
      DOUBLE PRECISION :: sumdat, sumelv, sdev, ave, xmax, xmin
!***********************************************************************
!
! Temperature calculations
! NOTE: there is a problem with extreme values due to elevations greater or less than
!       those present in the station data. To avoid extreme temperature due to extrapolation
!       the slope will not be allowed to exceed the dry adiabatic lapse rate (DALR) when extrapolating
!       (DALR = 5.4oF/1000 meters)
!      dalr = 0.0177
!-----------------------------------------------------------------------------------------------------
      slope = -999.0D0
      b = -999.0D0
      ave = -999.0D0
!
! First get rid of missing data
      n = 0
      DO j = 1, Nsta
        i = Nuse(j)
        IF ( Data(i)>-95.0 ) THEN
          n = n + 1
          elev(n) = Sta_z(i)
          dat(n) = Data(i)
        ENDIF
      ENDDO
      num = n
!
!  get mean and standard deviation
      CALL moments(Imax, dat, num, ave, sdev)
!
! If there are 3 or less stations, return the mean value
      IF ( num<4 ) THEN
        Dat_elev = ave
        RETURN
      ENDIF
!
! If iTYPE=2 (PRECIPITAITON) calculate the stats without the 0 values
      IF ( Itype==2 ) THEN
        nn = 0
        DO n = 1, num
          IF ( dat(n)>0.0D0 ) THEN
            nn = nn + 1
            dat0(nn) = dat(n)
          ENDIF
        ENDDO
        IF ( nn<3 ) THEN
          Dat_elev = 0.0D0
          RETURN
        ENDIF
        CALL moments(Imax, dat0, nn, ave, sdev)
      ENDIF
!
! To get general elevation for the basin --
! remove daily points > or < then 2STD from mean
      xmin = ave - (2.0D0*sdev)
      IF ( Itype==2 ) xmin = 0.0D0
      xmax = ave + (2.0D0*sdev)
      DO i = 1, num
        IF ( dat(i)>xmax ) dat(i) = -99.0D0
        IF ( dat(i)<xmin ) dat(i) = -99.0D0
      ENDDO
!
      n = 0
      ave = 0.0D0
      DO i = 1, num
        IF ( dat(i)>-90.0D0 ) THEN
          n = n + 1
          dat(n) = dat(i)
          elev(n) = elev(i)
          ave = ave + dat(n)
        ENDIF
      ENDDO
      num = n
      ave = ave/FLOAT(num)
!
! If there are 3 or less stations now, return the mean value
      IF ( num<4 ) THEN
        Dat_elev = ave
        RETURN
      ENDIF
!
! sort the data by elevation
      CALL SORT2(Imax, num, elev, dat)
!
! get a 3 station moving average (increasing elevation)
      ave = 0.0D0
      nn = 0
      DO i = 1, num - 2
        sumelv = 0.0D0
        sumdat = 0.0D0
        n = 0
        DO j = i, i + 2
          n = n + 1
          sumelv = sumelv + elev(j)
          sumdat = sumdat + dat(j)
        ENDDO
        nn = nn + 1
        xmnelv(nn) = sumelv/FLOAT(n)
        xmndat(nn) = sumdat/FLOAT(n)
        ave = ave + xmndat(nn)
      ENDDO
      num = nn
      ave = ave/FLOAT(nn)
!
! If elevation to be estimated at is not within bounds of stations then
! you will be extrapolating, so use the slope from end to end:
!
      IF ( Z<xmnelv(1) .OR. Z>xmnelv(num) ) THEN
!
! Find lowest and highest 3-station mean
! and get climate estimate based on general elevation trend
!
        DO i = 1, num
          elev(i) = ABS(xmnelv(i)-Z)
          rb(i) = i
        ENDDO
        CALL SORT2I(Imax, num, elev, rb)
        j1 = rb(1)
        j2 = rb(num)
        slope = (xmndat(j2)-xmndat(j1))/(xmnelv(j2)-xmnelv(j1))
        b = xmndat(j1) - (slope*xmnelv(j1))
        IF ( ABS(Itype)==1 ) THEN
          IF ( ABS(slope)>Dalr ) THEN
            IF ( slope<0.0D0 ) slope = Dalr*(-1.0)
            IF ( slope>0.0D0 ) slope = Dalr
          ENDIF
        ENDIF
        Dat_elev = (slope*Z) + b
!
      ELSE
!
! Find 2 closest 3-station mean elevations
! and get climate estimate based on general elevation trend
!
        DO i = 1, num
          elev(i) = ABS(xmnelv(i)-Z)
          rb(i) = i
        ENDDO
        CALL SORT2I(Imax, num, elev, rb)
        j1 = rb(1)
        j2 = rb(2)
        slope = (xmndat(j2)-xmndat(j1))/(xmnelv(j2)-xmnelv(j1))
        b = xmndat(j1) - (slope*xmnelv(j1))
        Dat_elev = (slope*Z) + b
!
      ENDIF
!
!=============================================================
      END SUBROUTINE compute_elv
 
!***********************************************************************
!***********************************************************************
      SUBROUTINE SORT2(Imax, N, Ra, Rb)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Imax, N
      DOUBLE PRECISION, DIMENSION(Imax), INTENT(INOUT) :: Ra, Rb
! Local Variables
      INTEGER l, ir, i, j
      DOUBLE PRECISION rra, rrb
!***********************************************************************
      l = N/2 + 1
      ir = N
 100  IF ( l>1 ) THEN
        l = l - 1
        rra = Ra(l)
        rrb = Rb(l)
      ELSE
        rra = Ra(ir)
        rrb = Rb(ir)
        Ra(ir) = Ra(1)
        Rb(ir) = Rb(1)
        ir = ir - 1
        IF ( ir==1 ) THEN
          Ra(1) = rra
          Rb(1) = rrb
          RETURN
        ENDIF
      ENDIF
      i = l
      j = l + l
      DO
        IF ( j<=ir ) THEN
          IF ( j<ir ) THEN
            IF ( Ra(j)<Ra(j+1) ) j = j + 1
          ENDIF
          IF ( rra<Ra(j) ) THEN
            Ra(i) = Ra(j)
            Rb(i) = Rb(j)
            i = j
            j = j + j
          ELSE
            j = ir + 1
          ENDIF
          CYCLE
        ENDIF
        Ra(i) = rra
        Rb(i) = rrb
        GOTO 100
      ENDDO

      END SUBROUTINE SORT2

!***********************************************************************
! sort with second array integer
!***********************************************************************
      SUBROUTINE SORT2I(Imax, N, Ra, Rb)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Imax, N
      INTEGER, DIMENSION(Imax), INTENT(INOUT) :: Rb
      DOUBLE PRECISION, DIMENSION(Imax), INTENT(INOUT) :: Ra
! Local Variables
      INTEGER l, ir, i, j, rrb
      DOUBLE PRECISION rra
!***********************************************************************
      l = N/2 + 1
      ir = N
 100  IF ( l>1 ) THEN
        l = l - 1
        rra = Ra(l)
        rrb = Rb(l)
      ELSE
        rra = Ra(ir)
        rrb = Rb(ir)
        Ra(ir) = Ra(1)
        Rb(ir) = Rb(1)
        ir = ir - 1
        IF ( ir==1 ) THEN
          Ra(1) = rra
          Rb(1) = rrb
          RETURN
        ENDIF
      ENDIF
      i = l
      j = l + l
      DO
        IF ( j<=ir ) THEN
          IF ( j<ir ) THEN
            IF ( Ra(j)<Ra(j+1) ) j = j + 1
          ENDIF
          IF ( rra<Ra(j) ) THEN
            Ra(i) = Ra(j)
            Rb(i) = Rb(j)
            i = j
            j = j + j
          ELSE
            j = ir + 1
          ENDIF
          CYCLE
        ENDIF
        Ra(i) = rra
        Rb(i) = rrb
        GOTO 100
      ENDDO

      END SUBROUTINE SORT2I
!
!***********************************************************************
!***********************************************************************
      SUBROUTINE moments(Imax, Data, N, Ave, Sdev)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Imax, N
      DOUBLE PRECISION, DIMENSION(Imax), INTENT(IN) :: Data
      DOUBLE PRECISION, INTENT(OUT) :: Sdev, Ave
! Local Variables
      DOUBLE PRECISION :: var, s, p
      INTEGER :: j
!***********************************************************************
      Sdev = 0.0D0
      IF ( N<2 ) THEN
!        PRINT *, 'N must be at least 2'
!  markstro      STOP
         Ave = Data(1)
         RETURN
      ENDIF
      s = 0.0D0
      DO j = 1, N
        s = s + Data(j)
      ENDDO
      Ave = s/N
      var = 0.0D0
      DO j = 1, N
        s = Data(j) - Ave
        p = s*s
        var = var + p
      ENDDO
      var = var/(N-1)
      Sdev = SQRT(var)
      END SUBROUTINE moments
