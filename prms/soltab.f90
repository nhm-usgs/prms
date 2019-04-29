!***********************************************************************
! Compute potential solar radiation and sunlight hours for each HRU for
! each day of year; modification of soltab_prms
!
! References -- you *will* need these to figure out what is going on:
!   Swift, L.W., Jr., 1976, Algorithm for solar radiation on mountain
!   slopes: Water Resources Research, v. 12, no. 1, p. 108-112.
!
!   Lee, R., 1963, Evaluation of solar beam irradiation as a climatic parameter
!   of mountain watersheds, Colorado State University Hydrology Papers, 2,
!   50 pp.
!***********************************************************************
      MODULE PRMS_SOLTAB
        IMPLICIT NONE
!   Local Variables
        DOUBLE PRECISION, PARAMETER :: PI=3.1415926535898D0
        DOUBLE PRECISION, PARAMETER :: RADIANS=PI/180.0D0, TWOPI=2.0D0*PI
        DOUBLE PRECISION, PARAMETER :: PI_12=12.0D0/PI
! TWOPI ~ 6.2831853071786
! RADIANS ~ 0.017453292519943
! PI_12 ~ 3.8197186342055
        CHARACTER(LEN=6), SAVE :: MODNAME
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_cossl(:), Soltab_basinpotsw(:)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Soltab_sunhrs(:, :)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Soltab_potsw(:, :), Soltab_horad_potsw(:, :)
!   Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Hru_aspect(:), Hru_slope(:)
      END MODULE PRMS_SOLTAB

!***********************************************************************
!     Main soltab routine
!***********************************************************************
      INTEGER FUNCTION soltab()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: sthdecl, sthinit
      EXTERNAL :: soltab_restart
!***********************************************************************
      soltab = 0

      IF ( Process(:4)=='decl' ) THEN
        soltab = sthdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL soltab_restart(1)
        ELSE
          soltab = sthinit()
        ENDIF
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL soltab_restart(0)
      ENDIF

      END FUNCTION soltab

!***********************************************************************
!     sthdecl - set up parameters for solar radiation computations
!   Declared Parameters
!     hru_aspect, hru_lat, hru_slope
!***********************************************************************
      INTEGER FUNCTION sthdecl()
      USE PRMS_SOLTAB
      USE PRMS_MODULE, ONLY: Nhru
      USE PRMS_BASIN, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declparam
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_soltab
!***********************************************************************
      sthdecl = 0

      Version_soltab = '$Id: soltab.f90 5600 2013-04-23 18:36:27Z rsregan $'
      CALL print_module(Version_soltab, 'Potential Solar Radiation ', 90)
      MODNAME = 'soltab'

      ALLOCATE ( Soltab_potsw(366, Nhru), Soltab_sunhrs(366, Nhru))
      ALLOCATE ( Hru_cossl(Nhru), Soltab_basinpotsw(366) )
      ALLOCATE ( Soltab_horad_potsw(366, Nhru), Hru_slope(Nhru) )

      IF ( Timestep/=0 ) RETURN

!   Declared Parameters
      IF ( declparam(MODNAME, 'hru_slope', 'nhru', 'real', &
     &     '0.0', '0.0', '10.0', &
     &     'HRU slope', &
     &     'Slope of each HRU, specified as change in vertical length divided by change in horizontal length', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'hru_slope')
      ALLOCATE ( Hru_aspect(Nhru) )
      IF ( declparam(MODNAME, 'hru_aspect', 'nhru', 'real', &
     &     '0.0', '0.0', '360.0', &
     &     'HRU aspect', 'Aspect of each HRU', &
     &     'degrees')/=0 ) CALL read_error(1, 'hru_aspect')

      END FUNCTION sthdecl

!***********************************************************************
!     sthinit - Initialize soltab module - get parameter values,
!               compute soltab_potsw (potential shortwave radiation)
!               and soltab_sunhrs (hours between sunrise and sunset)
!               for each HRU for each day of the year.
!***********************************************************************
      INTEGER FUNCTION sthinit()
      USE PRMS_SOLTAB, ONLY: Hru_slope, Hru_aspect, RADIANS, Hru_slope, MODNAME, &
     &    Soltab_potsw, Soltab_sunhrs, Hru_cossl, Soltab_basinpotsw, Soltab_horad_potsw
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Inputerror_flag, Parameter_check_flag
      USE PRMS_BASIN, ONLY: Hru_type, Active_hrus, Hru_route_order, Basin_lat, Hru_lat
      IMPLICIT NONE
! Functions
      INTRINSIC SIN, COS, DBLE
!     INTRINSIC ASIN
      INTEGER, EXTERNAL :: getparam
      EXTERNAL compute_soltab, read_error, PRMS_open_module_file
! Local Variables
      CHARACTER(LEN=12) :: output_path
      INTEGER :: jd, j, n, file_unit, ierr, nn
      REAL :: lat
      DOUBLE PRECISION :: basin_cossl
      DOUBLE PRECISION, ALLOCATABLE :: basin_sunhrs(:)
      DOUBLE PRECISION, ALLOCATABLE :: e(:), dm(:)
      DOUBLE PRECISION :: y, y2, y3, jddbl
      DOUBLE PRECISION, PARAMETER :: ECCENTRICY = 0.01671D0
      DOUBLE PRECISION, PARAMETER :: DAYSYR = 365.242D0
      DOUBLE PRECISION, PARAMETER :: DEGDAY = 360.0D0/DAYSYR
      DOUBLE PRECISION, PARAMETER :: DEGDAYRAD = DEGDAY*RADIANS
! DEGDAY = 360 degrees/days in year
!***********************************************************************
      sthinit = 0

      ALLOCATE ( basin_sunhrs(366), e(366), dm(366) )

      IF ( getparam(MODNAME, 'hru_slope', Nhru, 'real', Hru_slope)/=0 ) CALL read_error(2, 'hru_slope')
      IF ( getparam(MODNAME, 'hru_aspect', Nhru, 'real', Hru_aspect)/=0 ) CALL read_error(2, 'hru_aspect')

      DO jd = 1, 366
        jddbl = DBLE(jd)
!rsr .0172 = 2PI/365 = RADIAN_YEAR = DEGDAYRAD
!rsr01/2006 commented out equations from Llowd W. Swift paper 2/1976
!       e(jd) = 1.0D0 - (0.0167D0*COS((jd-3)*0.0172D0))
        e(jd) = 1.0D0 - (ECCENTRICY*COS((jddbl-3.0D0)*DEGDAYRAD))
!       dm(jd) = 0.007D0 - (0.4067D0*COS((jd+10)*0.0172D0))
!       dm(jd) = ASIN(0.39785D0 * SIN( (278.9709D0+DEGDAY*jd)*RADIANS + 1.9163D0*RADIANS * SIN((356.6153D0+DEGDAY*jd)*RADIANS )) )
        ! hour = 12.0D0
!       y = DEGDAYRAD*(jddbl-1.0D0 +(hour-12.0D0)/24.0D0)
        y = DEGDAYRAD*(jddbl-1.0D0) ! assume noon
        y2 = 2.0D0*y
        y3 = 3.0D0*y
! dm = solar declination
        dm(jd) = 0.006918D0 - 0.399912D0*COS(y) + 0.070257D0*SIN(y) &
     &           - 0.006758D0*COS(y2) + 0.000907D0*SIN(y2) &
     &           - 0.002697D0*COS(y3) + 0.00148D0*SIN(y3)
      ENDDO

!   Module Variables
      Soltab_sunhrs = 0.0D0
      Soltab_potsw = 0.0D0
      Soltab_horad_potsw = 0.0D0
      DO nn = 1, Active_hrus
        n = Hru_route_order(nn)
        ierr = 0
        IF ( Hru_aspect(n)<0.0 ) THEN
          IF ( Parameter_check_flag==1 ) THEN
            PRINT *, 'ERROR, hru_aspect<0.0 for HRU:', n, ', hru_aspect:', Hru_aspect(n), ', hru_slope:', Hru_slope(n)
            ierr = 1
          ELSE
            PRINT *, 'Warning, hru_aspect<0.0', Hru_aspect(n), ' HRU:', n, ', hru_slope', Hru_slope(n)
            PRINT *, 'hru_aspect and hru_slope set to 0.0'
            Hru_aspect(n) = 0.0
            Hru_slope(n) = 0.0
          ENDIF
        ENDIF
        IF ( Hru_slope(n)<0.0 ) THEN
          PRINT *, 'ERROR, hru_slope<0.0 for HRU:', n, Hru_slope(n)
          ierr = 1
        ENDIF
        IF ( Hru_slope(n)>89.99 ) THEN
          PRINT *, 'ERROR, hru_slope>89.99 for HRU:', n, Hru_slope(n)
          ierr = 1
        ENDIF
        IF ( ierr==1 ) THEN
          Inputerror_flag = 1
          CYCLE
        ENDIF

        CALL compute_soltab(e, dm, 0.0, 0.0, Hru_lat(n), &
     &                      Hru_cossl(n), Soltab_horad_potsw(1, n), &
     &                      Soltab_sunhrs(1, n), Hru_type(n), n)
        CALL compute_soltab(e, dm, Hru_slope(n), Hru_aspect(n), &
     &                      Hru_lat(n), Hru_cossl(n), Soltab_potsw(1, n), &
     &                      Soltab_sunhrs(1, n), Hru_type(n), n)
      ENDDO

      lat = SNGL( Basin_lat )
      CALL compute_soltab(e, dm, 0.0, 0.0, lat, basin_cossl, &
     &                    Soltab_basinpotsw, basin_sunhrs, 0, 0)

      IF ( Print_debug==5 ) THEN
        output_path = 'soltab_debug'
        PRINT *, ''
        PRINT *, 'soltab debug data written to: ', output_path
        CALL PRMS_open_module_file(file_unit, output_path)
        DO n = 1, Nhru
          WRITE ( file_unit, * ) 'HRU:', n
          WRITE ( file_unit, * ) '***Soltab_sunhrs***'
          WRITE ( file_unit, '(13F8.3)' ) (Soltab_sunhrs(j,n), j=1,366)
          WRITE ( file_unit, * ) '***Soltab_potsw***'
          WRITE ( file_unit, '(13F8.3)' ) (Soltab_potsw(j,n), j=1,366)
        ENDDO
!       WRITE ( file_unit, * ) e, dm
        WRITE ( file_unit, * ) 2.0D0/(e(356)*e(356)), 2.0D0/(e(10)*e(10)), &
     &                         2.0D0/(e(23)*e(23)), 2.0D0/(e(38)*e(38)), &
     &                         2.0D0/(e(51)*e(51)), 2.0D0/(e(66)*e(66)), &
     &                         2.0D0/(e(80)*e(80)), 2.0D0/(e(94)*e(94)), &
     &                         2.0D0/(e(109)*e(109)), 2.0D0/(e(123)*e(123)), &
     &                         2.0D0/(e(138)*e(138)), 2.0D0/(e(152)*e(152)), &
     &                         2.0D0/(e(173)*e(173))
        WRITE ( file_unit, * ) dm(356), dm(10), dm(23), dm(38), dm(51), &
     &                         dm(66), dm(80), dm(94), dm(109), dm(123), &
     &                         dm(138), dm(152), dm(173)
        CLOSE ( file_unit) 
! from original soltab
!     data e/2.06699,2.06317,2.05582,2.04520,2.03243,2.01706,2.00080,
!    +1.98553,1.96990,1.95714,1.94689,1.94005,1.93616/

!     data dm/-.410152,-.383391,-.337430,-.27198,-.190532,-.09832,0.,
!    +.09832,.190532,.27198,.33743,.383391,.410152/

!     data jday/356,10,23,38,51,66,80,94,109,123,138,152,173/
      ENDIF

      DEALLOCATE ( basin_sunhrs, e, dm, Hru_aspect )

      END FUNCTION sthinit

!***********************************************************************
!  compute soltab_potsw (potential shortwave radiation)
!  and soltab_sunhrs (hours between sunrise and sunset)
!  for each HRU for each day of the year.
!***********************************************************************
      SUBROUTINE compute_soltab(E, Dm, Slope, Aspect, Latitude, Cossl, &
     &                          Soltab, Sunhrs, Hru_type, Id)
      USE PRMS_SOLTAB, ONLY: PI, TWOPI, RADIANS, PI_12
      USE PRMS_BASIN, ONLY: DNEARZERO
      IMPLICIT NONE
      EXTERNAL compute_t
!     Functions
      DOUBLE PRECISION, EXTERNAL :: func3
      INTRINSIC ASIN, SIN, COS, ATAN, ABS
!     Arguments
      INTEGER, INTENT(IN) :: Hru_type, Id
      DOUBLE PRECISION, INTENT(IN), DIMENSION(366) :: E, Dm
      REAL, INTENT(IN) :: Slope, Aspect, Latitude
      DOUBLE PRECISION, INTENT(OUT) :: Cossl
      DOUBLE PRECISION, INTENT(OUT), DIMENSION(366) :: Soltab
      DOUBLE PRECISION, INTENT(OUT), DIMENSION(366) :: Sunhrs
!     Local Variables
      INTEGER :: jd
      DOUBLE PRECISION :: a, x0, x1, x2, r0, r1, d1, t, sunh, solt
      DOUBLE PRECISION :: t0, t1, t2, t3, t6, t7, d, sl
!***********************************************************************
! from SWIFT (1976)
! x0, x1, x2 = l0, l1, l2
! sl = i

      sl = ATAN(Slope)
      Cossl = COS(sl)
      a = Aspect*RADIANS

! x0 latitude of HRU
      x0 = Latitude*RADIANS

! x1 latitude of equivalent slope
! This is equation 13 from Lee, 1963
      x1 = ASIN(Cossl*SIN(x0)+SIN(sl)*COS(x0)*COS(a))

! d1 is the denominator of equation 12, Lee, 1963
      d1 = Cossl*COS(x0) - SIN(sl)*SIN(x0)*COS(a)
      IF ( ABS(d1)<DNEARZERO ) d1 = DNEARZERO

! x2 is the difference in longitude between the location of
! the HRU and the equivalent horizontal surface expressed in angle hour
! This is equation 12 from Lee, 1963
      x2 = ATAN(SIN(sl)*SIN(a)/d1)
      IF ( d1<0.0D0 ) x2 = x2 + PI

! r0 is the minute solar constant cal/cm2/min
      r0 = 2.0D0
! r0 could be 1.95 (Drummond, et al 1968)
      DO jd = 1, 366
        d = Dm(jd)

! This is adjusted to express the variability of available insolation as
! a function of the earth-sun distance.  Lee, 1963, p 16.
! r1 is the hour solar constant cal/cm2/hour
! r0 is the minute solar constant cal/cm2/min
! 60.0D0 is minutes in an hour
! E is the obliquity of the ellipse of the earth's orbit around the sun. E
! is also called the radius vector of the sun (or earth) and is the ratio of
! the earth-sun distance on a day to the mean earth-sun distance.
! obliquity = ~23.439 (obliquity of sun)
        r1 = 60.0D0*r0/(E(jd)*E(jd))

!  compute_t is the sunrise equation.
!  t7 is the hour angle of sunset on the equivalent slope
!  t6 is the hour angle of sunrise on the equivalent slope
        CALL compute_t(x1, d, t)
        t7 = t - x2
        t6 = -t - x2

!  compute_t is the sunrise equation.
!  t1 is the hour angle of sunset on a hroizontal surface at the HRU
!  t0 is the hour angle of sunrise on a hroizontal surface at the HRU
        CALL compute_t(x0, d, t)
        t1 = t
        t0 = -t

! For HRUs that have an east or west direction component to their aspect, the
! longitude adjustment (moving the effective slope east or west) will cause either:
! (1) sunrise to be earlier than at the horizontal plane at the HRU
! (2) sunset to be later than at the horizontal plane at the HRU
! This is not possible. The if statements below check for this and adjust the
! sunrise/sunset angle hours on the equivalent slopes as necessary.
!
! t3 is the hour angle of sunrise on the slope at the HRU
! t2 is the hour angle of sunset on the slope at the HRU
        IF ( t7>t1 ) THEN
          t3 = t1
        ELSE
          t3 = t7
        ENDIF
        IF ( t6<t0 ) THEN
          t2 = t0
        ELSE
          t2 = t6
        ENDIF

        IF ( ABS(sl)<DNEARZERO ) THEN
!  solt is Swift's R4 (potential solar radiation on a sloping surface cal/cm2/day)
!  Swift, 1976, equation 6
          solt = func3(0.0D0, x0, t1, t0, r1, d)
!  sunh is the number of hours of direct sunlight (sunset minus sunrise) converted
!  from angle hours in radians to hours (24 hours in a day divided by 2 pi radians
!  in a day).
          sunh = (t1-t0)*PI_12
        ELSE
          IF ( t3<t2 ) THEN
            t2 = 0.0D0
            t3 = 0.0D0
          ENDIF
          t6 = t6 + TWOPI
          IF ( t6<t1 ) THEN
            solt = func3(x2, x1, t3, t2, r1, d) + func3(x2, x1, t1, t6, r1, d)
            sunh = (t3-t2+t1-t6)*PI_12
          ELSE
            t7 = t7 - TWOPI
            IF ( t7>t0 ) THEN
              solt = func3(x2, x1, t3, t2, r1, d) + func3(x2, x1, t7, t0, r1, d)
              sunh = (t3-t2+t7-t0)*PI_12
            ELSE
              solt = func3(x2, x1, t3, t2, r1, d)
              sunh = (t3-t2)*PI_12
            ENDIF
          ENDIF
        ENDIF
        IF ( solt<0.0D0 ) THEN
          PRINT *, 'Warning: solar table value for day:', jd, &
     &             ' computed as:', solt, ' set to', 0.0, &
     &             ' for HRU:', Id, ' hru_type:', Hru_type
          PRINT *, 'slope, aspect, latitude, cossl', Slope, Aspect, Latitude, Cossl
          solt = 0.0D0
          PRINT *, Slope, Aspect, Latitude, Cossl, sunh
          PRINT *, t0, t1, t2, t3, t6, t7, d
        ENDIF
        IF ( sunh<0.0D0 ) sunh = 0.0D0
        Sunhrs(jd) = sunh
        Soltab(jd) = solt

      ENDDO

      END SUBROUTINE compute_soltab

!***********************************************************************
!***********************************************************************
      SUBROUTINE compute_t(Lat, D, T)
      USE PRMS_SOLTAB, ONLY: PI
      IMPLICIT NONE
      INTRINSIC TAN, ACOS
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: Lat, D
      DOUBLE PRECISION, INTENT(OUT) :: T
! Local Variables
      DOUBLE PRECISION :: tx
!***********************************************************************

!  This is the sunrise equation
!  Lat is the latitude
!  D is the declination of the sun on a day
!  T is the angle hour from the local meridian (local solar noon) to the 
!  sunrise (negative) or sunset (positive).  The Earth rotates at the angular
!  speed of 15°/hour (2 pi / 24 hour in radians) and, therefore, T/15° (T*24/pi
!  in radians) gives the time of sunrise as the number of hours before the local
!  noon, or the time of sunset as the number of hours after the local noon.
!  Here the term local noon indicates the local time when the sun is exactly to
!  the south or north or exactly overhead.
      tx = -TAN(Lat)*TAN(D)
      IF ( tx<-1.0D0 ) THEN
        T = PI
!rsr bug fix, old code would set t=acos(0.0) for tx>1 12/05
      ELSEIF ( tx>1.0D0 ) THEN
        T = 0.0D0
      ELSE
        T = ACOS(tx)
      ENDIF

      END SUBROUTINE compute_t

!***********************************************************************
!***********************************************************************
      DOUBLE PRECISION FUNCTION func3(V, W, X, Y, R1, D)
      USE PRMS_SOLTAB, ONLY: PI_12
      IMPLICIT NONE
      INTRINSIC SIN, COS
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: V, W, X, Y, R1, D
!***********************************************************************
!  This is the radian angle version of FUNC3 (eqn 6) from Swift, 1976
!  or Lee, 1963 equation 5.
!  func3 (R4) is potential solar radiation on the surface cal/cm2/day
!  V (L2) latitude angle hour offset between actual and equivalent slope
!  W (L1) latitude of the equivalent slope
!  X (T3) hour angle of sunset on equivalent slope
!  Y (T2) hour angle of sunrise on equivalent slope
!  R1 solar constant for 60 minutes
!  D declination of sun
      func3 = R1*PI_12*(SIN(D)*SIN(W)*(X-Y) + COS(D)*COS(W)*(SIN(X+V)-SIN(Y+V)))

      END FUNCTION func3

!***********************************************************************
!     soltab_restart - write or read soltab restart file
!***********************************************************************
      SUBROUTINE soltab_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_SOLTAB
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=6) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Soltab_potsw
        WRITE ( Restart_outunit ) Soltab_sunhrs
        WRITE ( Restart_outunit ) Hru_cossl
        WRITE ( Restart_outunit ) Soltab_basinpotsw
        WRITE ( Restart_outunit ) Soltab_horad_potsw
        WRITE ( Restart_outunit ) Hru_slope
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Soltab_potsw
        READ ( Restart_inunit ) Soltab_sunhrs
        READ ( Restart_inunit ) Hru_cossl
        READ ( Restart_inunit ) Soltab_basinpotsw
        READ ( Restart_inunit ) Soltab_horad_potsw
        READ ( Restart_inunit ) Hru_slope
      ENDIF
      END SUBROUTINE soltab_restart
