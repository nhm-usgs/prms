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
module PRMS_SOLTAB
  use variableKind
  implicit none

  !   Local Variables
  character(len=*), parameter :: MODNAME = 'soltab'
  character(len=*), parameter :: VERSION = 'soltab.f90 2016-09-29 13:48:00Z'

  integer(r32), parameter :: DAYS_PER_YEAR = 366
  real(r64), parameter :: PI = 3.1415926535898D0
  real(r64), parameter :: RADIANS = PI / 180.0D0   ! RADIANS ~ 0.017453292519943
  real(r64), parameter :: TWOPI = 2.0D0 * PI       ! TWOPI ~ 6.2831853071786
  real(r64), parameter :: PI_12 = 12.0D0 / PI      ! PI_12 ~ 3.8197186342055

  private
  public :: Soltab

  type Soltab
    real(r64) :: solar_declination(366)
    real(r64) :: soltab_basinpotsw(366)
    real(r64), allocatable :: hru_cossl(:)
    real(r64), allocatable :: soltab_sunhrs(:, :)

    ! Declared Variables
    real(r64), allocatable :: soltab_potsw(:, :)
    real(r64), allocatable :: soltab_horad_potsw(:, :)

    contains
      procedure, private :: compute_t
      procedure, private :: compute_soltab
      procedure, nopass, private :: func3

  end type

  interface Soltab
    !! Soltab constructor
    module function constructor_Soltab(ctl_data, param_data, model_basin) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use PRMS_BASIN, only: Basin

      type(Soltab) :: this
        !! Soltab class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Parameters), intent(in) :: param_data
        !! Parameters
      class(Basin), intent(in) :: model_basin
    end function
  end interface

  contains
    !***********************************************************************
    ! Soltab constructor
    module function constructor_Soltab(ctl_data, param_data, model_basin) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use PRMS_BASIN, only: Basin
      use UTILS_PRMS, only: PRMS_open_module_file
      implicit none

      INTRINSIC SIN, COS, DBLE

      type(Soltab) :: this
      class(Control), intent(in) :: ctl_data
      class(Parameters), intent(in) :: param_data
      class(Basin), intent(in) :: model_basin

      ! Local Variables
      real(r64), parameter :: ECCENTRICY = 0.01671D0
      real(r64), parameter :: DAYSYR = 365.242D0
      ! 0.016723401  daily change -1.115E-09, eccen = 0.016723401 + (julhour-julhour(1966,1,0,18))+dmin/60)/24*-1.115E-09
      ! julday(1966,1,0.75 UT) = 2439126.25
      ! eccen = 0.01675104-0.00004180*T-0.000000126*T^2  T is julian centuries (days time from epoch, is GMT from Jan 0.0
      real(r64), parameter :: DEGDAY = 360.0D0 / DAYSYR
      real(r64), parameter :: DEGDAYRAD = DEGDAY * RADIANS ! about 0.00143356672
      ! DEGDAY = 360 degrees/days in year

      character(len=12) :: output_path
      integer(i32) :: jd
      integer(i32) :: j
      integer(i32) :: chru
      integer(i32) :: file_unit
      integer(i32) :: nn
      real(r32) :: lat
      real(r64) :: basin_cossl
      real(r64) :: basin_sunhrs(366)
      real(r64) :: obliquity(366)
      real(r64) :: y
      real(r64) :: y2
      real(r64) :: y3
      real(r64) :: jddbl

      ! ------------------------------------------------------------------------
      allocate(this%hru_cossl(ctl_data%nhru%values(1)))
      allocate(this%soltab_sunhrs(366, ctl_data%nhru%values(1)))
      allocate(this%soltab_potsw(366, ctl_data%nhru%values(1)))
      allocate(this%soltab_horad_potsw(366, ctl_data%nhru%values(1)))

      this%hru_cossl = 0.0D0
      this%soltab_sunhrs = 0.0D0
      this%soltab_potsw = 0.0D0
      this%soltab_horad_potsw = 0.0D0

      do jd = 1, 366
        jddbl = DBLE(jd)

        ! cosine of the solar zenith angle: http://www.cesm.ucar.edu/models/cesm1.0/cesm/cesmBbrowser/html_code/csm_share/shr_orb_mod.F90.html
        !    jday   ! Julian cal day (1.xx to 365.xx)
        !    lat    ! Centered latitude (radians)
        !    lon    ! Centered longitude (radians)
        !    declin ! Solar declination (radians)
        ! shr_orb_cosz = sin(lat)*sin(declin) - &
        !&              cos(lat)*cos(declin)*cos(jday*2.0_SHR_KIND_R8*pi + lon)

        ! Eccentricity from equation E-2 (Dingman, S. L., 1994, Physical Hydrology. Englewood Cliffs, NJ: Prentice Hall, 575 p.)
        ! eccentricity = (r_0/r)^2 = 1.00011+0.034221 cos⁡Γ+0.00128 sin⁡Γ+0.000719 cos⁡2Γ+0.000077 sin⁡2Γ
        ! Γ = (2π(J-1))/365 = DEGDAYRAD*(jddbl-1.0D0) = day angle
        ! dayangle = DEGDAYRAD*(jddbl-1.0D0)
        ! eccentricity = 1.00011D0 + 0.034221D0*COS(dayangle) + 0.00128D0*SIN(dayangle) + 0.000719D0*COS(2.0D0*dayangle) + 0.000077D0*SIN(2.0D0*dayangle)
        !rsr .0172 = 2PI/365 = RADIAN_YEAR = DEGDAYRAD
        !rsr01/2006 commented out equations from Llowd W. Swift paper 2/1976
        !       obliquity(jd) = 1.0D0 - (0.0167D0*COS((jd-3)*0.0172D0))
        obliquity(jd) = 1.0D0 - (ECCENTRICY * COS((jddbl - 3.0D0) * DEGDAYRAD))
        !       Solar_declination(jd) = 0.007D0 - (0.4067D0*COS((jd+10)*0.0172D0))
        !       Solar_declination(jd) = ASIN(0.39785D0 * SIN( (278.9709D0+DEGDAY*jd)*RADIANS + 1.9163D0*RADIANS * SIN((356.6153D0+DEGDAY*jd)*RADIANS )) )
        ! hour = 12.0D0
        !       y = DEGDAYRAD*(jddbl-1.0D0 +(hour-12.0D0)/24.0D0)
        y = DEGDAYRAD * (jddbl - 1.0D0) ! assume noon
        y2 = 2.0D0 * y
        y3 = 3.0D0 * y
        this%solar_declination(jd) = 0.006918D0 - 0.399912D0 * COS(y) + &
                                     0.070257D0 * SIN(y) - 0.006758D0 * COS(y2) + &
                                     0.000907D0 * SIN(y2) - 0.002697D0 * COS(y3) + &
                                     0.00148D0 * SIN(y3)
      enddo

      do nn = 1, model_basin%active_hrus
        chru = model_basin%hru_route_order(nn)
        ! OLD order
        ! Obliquity, Solar_declination, Slope, Aspect, Latitude, Cossl,
        ! Soltab, Sunhrs, Hru_type, Id
        ! NEW order
        ! Cossl, Soltab, Sunhrs, Obliquity, Solar_declination, Slope, Aspect,
        ! Latitude, Hru_type, Id
        call this%compute_soltab(this%hru_cossl(chru), this%soltab_horad_potsw(1, chru), &
                            this%soltab_sunhrs(1, chru), obliquity, &
                            this%solar_declination, 0.0, 0.0, &
                            param_data%hru_lat%values(chru), &
                            param_data%hru_type%values(chru), chru)

        call this%compute_soltab(this%hru_cossl(chru), this%soltab_potsw(1, chru), &
                            this%soltab_sunhrs(1, chru), obliquity, &
                            this%solar_declination, &
                            param_data%hru_slope%values(chru), &
                            param_data%hru_aspect%values(chru), &
                            param_data%hru_lat%values(chru), &
                            param_data%hru_type%values(chru), chru)
      enddo

      lat = SNGL(model_basin%basin_lat)
      call this%compute_soltab(basin_cossl, this%soltab_basinpotsw, basin_sunhrs, &
                          obliquity, this%solar_declination, 0.0, 0.0, lat, 0, 0)

      if (ctl_data%print_debug%values(1) == 5) then
        output_path = 'soltab_debug'
        print *, ''
        print *, 'soltab debug data written to: ', output_path
        call PRMS_open_module_file(file_unit, output_path)

        do chru = 1, ctl_data%nhru%values(1)
          write(file_unit, *) 'HRU:', chru
          write(file_unit, *) '***Soltab_sunhrs***'
          write(file_unit, '(13F8.3)') (this%soltab_sunhrs(j, chru), j = 1, 366)
          write(file_unit, *) '***Soltab_potsw***'
          write(file_unit, '(13F8.3)') (this%soltab_potsw(j, chru), j = 1, 366)
        enddo

        !       write ( file_unit, * ) obliquity, Solar_declination
        write(file_unit, *) 2.0D0 / (obliquity(356) * obliquity(356)), 2.0D0 / (obliquity(10) * obliquity(10)), &
                            2.0D0 / (obliquity(23) * obliquity(23)), 2.0D0 / (obliquity(38) * obliquity(38)), &
                            2.0D0 / (obliquity(51) * obliquity(51)), 2.0D0 / (obliquity(66) * obliquity(66)), &
                            2.0D0 / (obliquity(80) * obliquity(80)), 2.0D0 / (obliquity(94) * obliquity(94)), &
                            2.0D0 / (obliquity(109) * obliquity(109)), 2.0D0 / (obliquity(123) * obliquity(123)), &
                            2.0D0 / (obliquity(138) * obliquity(138)), 2.0D0 / (obliquity(152) * obliquity(152)), &
                            2.0D0 / (obliquity(173) * obliquity(173))
        write(file_unit, *) this%solar_declination(356), this%solar_declination(10), this%solar_declination(23), &
                            this%solar_declination(38), this%solar_declination(51), this%solar_declination(66), &
                            this%solar_declination(80), this%solar_declination(94), this%solar_declination(109), &
                            this%solar_declination(123), this%solar_declination(138), this%solar_declination(152), &
                            this%solar_declination(173)
        close(file_unit)
      endif
      ! deallocate (Hru_slope, Hru_aspect)
    end function


    !***********************************************************************
    !  compute soltab_potsw (potential shortwave radiation)
    !  and soltab_sunhrs (hours between sunrise and sunset)
    !  for each HRU for each day of the year.
    !***********************************************************************
    subroutine compute_soltab(this, Cossl, Soltab_daily, Sunhrs_daily, Obliquity, &
                              Solar_declination, Slope, Aspect, Latitude, &
                              Hru_type, Id)
      use prms_constants, only: DNEARZERO
      implicit none

      ! Functions
      INTRINSIC ASIN, SIN, COS, ATAN, ABS

      ! Arguments
      class(Soltab), intent(inout) :: this
      real(r64), intent(out) :: Cossl
      real(r64), intent(out), dimension(DAYS_PER_YEAR) :: Soltab_daily
      real(r64), intent(out), dimension(DAYS_PER_YEAR) :: Sunhrs_daily

      real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Obliquity
      real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Solar_declination
      real(r32), intent(in) :: Slope
      real(r32), intent(in) :: Aspect
      real(r32), intent(in) :: Latitude
      integer(i32), intent(in) :: Hru_type
      integer(i32), intent(in) :: Id

      ! Constants
      real(r64), parameter :: r0 = 2.0D0
        !! minute solar constant cal/cm2/min (r0 could also be 1.95 (Drummond, et al 1968))

      ! Local Variables
      integer(i32) :: jd
        !! General counter
      real(r64) :: a
        !! Aspect in radians
      real(r64) :: x0
        !! Latitude of HRU
      real(r64) :: x1
        !! Latitude of equivalent slope
      real(r64) :: x2
        !! The difference in longitude between the location of the HRU and the equivalent horizontal surface expressed in angle hour
      real(r64) :: r1
        !! The hour solar constant cal/cm2/hour
      real(r64) :: d1
        !! The denominator of equation 12 (Lee, 1963)
      real(r64) :: t
        !! The angle hour from the local meridian (local solar noon) to the sunrise (negative) or sunset (positive)
      real(r64) :: sunh
        !! Number of hours of direct sunlight (sunset minus sunrise)
      real(r64) :: solt
        !! Swift's R4 (potential solar radiation on a sloping surface cal/cm2/day)
      real(r64) :: t0
        !! The hour angle of sunrise on a hroizontal surface at the HRU
      real(r64) :: t1
        !! The hour angle of sunset on a horizontal surface at the HRU
      real(r64) :: t2
        !! The hour angle of sunset on the slope at the HRU
      real(r64) :: t3
        !! The hour angle of sunrise on the slope at the HRU
      real(r64) :: t6
        !! The hour angle of sunrise on the equivalent slope
      real(r64) :: t7
        !! The hour angle of sunset on the equivalent slope
      real(r64) :: d
        !! Solar declination for a day of the year
      real(r64) :: sl
        !! Arc-tangent of the slope

      !***********************************************************************
      ! from SWIFT (1976)
      ! x0, x1, x2 = l0, l1, l2
      ! sl = i

      sl = ATAN(Slope)
      Cossl = COS(sl)
      a = Aspect * RADIANS

      ! x0 latitude of HRU
      x0 = Latitude * RADIANS

      ! x1 latitude of equivalent slope
      ! This is equation 13 from Lee, 1963
      x1 = ASIN(Cossl * SIN(x0) + SIN(sl) * COS(x0) * COS(a))

      ! d1 is the denominator of equation 12, Lee, 1963
      d1 = Cossl * COS(x0) - SIN(sl) * SIN(x0) * COS(a)
      if (ABS(d1) < DNEARZERO) d1 = DNEARZERO

      ! x2 is the difference in longitude between the location of
      ! the HRU and the equivalent horizontal surface expressed in angle hour
      ! This is equation 12 from Lee, 1963
      x2 = ATAN(SIN(sl) * SIN(a) / d1)
      if (d1 < 0.0D0) x2 = x2 + PI

      do jd = 1, DAYS_PER_YEAR
        d = Solar_declination(jd)

        ! This is adjusted to express the variability of available insolation as
        ! a function of the earth-sun distance.  Lee, 1963, p 16.
        ! r1 is the hour solar constant cal/cm2/hour
        ! r0 is the minute solar constant cal/cm2/min
        ! 60.0D0 is minutes in an hour
        ! Obliquity is the obliquity of the ellipse of the earth's orbit around the sun. E
        ! is also called the radius vector of the sun (or earth) and is the ratio of
        ! the earth-sun distance on a day to the mean earth-sun distance.
        ! obliquity = ~23.439 (obliquity of sun)
        r1 = 60.0D0 * r0 / (Obliquity(jd) * Obliquity(jd))

        ! compute_t is the sunrise equation.
        ! t7 is the hour angle of sunset on the equivalent slope
        ! t6 is the hour angle of sunrise on the equivalent slope
        call this%compute_t(x1, d, t)
        t7 = t - x2
        t6 = -t - x2

        ! compute_t is the sunrise equation.
        ! t1 is the hour angle of sunset on a hroizontal surface at the HRU
        ! t0 is the hour angle of sunrise on a hroizontal surface at the HRU
        call this%compute_t(x0, d, t)
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
        if (t7 > t1) then
          t3 = t1
        else
          t3 = t7
        endif

        if (t6 < t0) then
          t2 = t0
        else
          t2 = t6
        endif

        if (ABS(sl) < DNEARZERO) then
          !  solt is Swift's R4 (potential solar radiation on a sloping surface cal/cm2/day)
          !  Swift, 1976, equation 6
          solt = func3(0.0D0, x0, t1, t0, r1, d)
          !  sunh is the number of hours of direct sunlight (sunset minus sunrise) converted
          !  from angle hours in radians to hours (24 hours in a day divided by 2 pi radians
          !  in a day).
          sunh = (t1 - t0) * PI_12
        else
          if (t3 < t2) then
            t2 = 0.0D0
            t3 = 0.0D0
          endif
          t6 = t6 + TWOPI

          if (t6 < t1) then
            solt = func3(x2, x1, t3, t2, r1, d) + func3(x2, x1, t1, t6, r1, d)
            sunh = (t3 - t2 + t1 - t6) * PI_12
          else
            t7 = t7 - TWOPI

            if (t7 > t0) then
              solt = func3(x2, x1, t3, t2, r1, d) + func3(x2, x1, t7, t0, r1, d)
              sunh = (t3 - t2 + t7 - t0) * PI_12
            else
              solt = func3(x2, x1, t3, t2, r1, d)
              sunh = (t3 - t2) * PI_12
            endif
          endif
        endif

        if (solt < 0.0D0) then
          print *, 'WARNING: solar table value for day:', jd, &
                   ' computed as:', solt, ' set to', 0.0, &
                   ' for HRU:', Id, ' hru_type:', Hru_type
          print *, 'slope, aspect, latitude, cossl', Slope, Aspect, &
                   Latitude, Cossl

          solt = 0.0D0
          print *, Slope, Aspect, Latitude, Cossl, sunh
          print *, t0, t1, t2, t3, t6, t7, d
        endif

        if (sunh < DNEARZERO) sunh = 0.0D0
        Sunhrs_daily(jd) = sunh
        Soltab_daily(jd) = solt
      enddo
    end subroutine compute_soltab

    !***********************************************************************
    !***********************************************************************
    subroutine compute_t(this, Lat, Solar_declination, T)
      implicit none

      INTRINSIC TAN, ACOS

      ! Arguments
      class(Soltab), intent(inout) :: this
      real(r64), intent(in) :: Lat
      real(r64), intent(in) :: Solar_declination
      real(r64), intent(out) :: T

      ! Local Variables
      real(r64) :: tx
      !***********************************************************************

      ! This is the sunrise equation
      ! Lat is the latitude
      ! Solar_declination is the declination of the sun on a day
      ! T is the angle hour from the local meridian (local solar noon) to the
      ! sunrise (negative) or sunset (positive).  The Earth rotates at the angular
      ! speed of 15 degrees/hour (2 pi / 24 hour in radians) and, therefore, T/15 degress (T*24/pi
      ! in radians) gives the time of sunrise as the number of hours before the local
      ! noon, or the time of sunset as the number of hours after the local noon.
      ! Here the term local noon indicates the local time when the sun is exactly to
      ! the south or north or exactly overhead.
      tx = -TAN(Lat) * TAN(Solar_declination)

      if (tx < -1.0D0) then
        T = PI
        !rsr bug fix, old code would set t=acos(0.0) for tx>1 12/05
      elseif (tx > 1.0D0) then
        T = 0.0D0
      else
        T = ACOS(tx)
      endif
    end subroutine compute_t

    !***********************************************************************
    !***********************************************************************
    ! This is the radian angle version of FUNC3 (eqn 6) from Swift, 1976
    ! or Lee, 1963 equation 5.
    ! func3 (R4) is potential solar radiation on the surface cal/cm2/day
    real(r64) function func3(V, W, X, Y, R1, Solar_declination)
      implicit none

      INTRINSIC SIN, COS

      ! Arguments
      real(r64), intent(in) :: V
        !! Latitude angle hour offset between actual and equivalent slope
      real(r64), intent(in) :: W
        !! Latitude of the equivalent slope
      real(r64), intent(in) :: X
        !! Hour angle of sunset on equivalent slope
      real(r64), intent(in) :: Y
        !! Hour angle of sunrise on equivalent slope
      real(r64), intent(in) :: R1
        !! Solar constant for 60 minutes
      real(r64), intent(in) :: Solar_declination
        !! Declination of sun

      !***********************************************************************
      func3 = R1 * PI_12 * (SIN(Solar_declination) * SIN(W) * (X - Y) + &
                            COS(Solar_declination) * COS(W) * (SIN(X + V) - &
                            SIN(Y + V)))
    end function func3
end module
