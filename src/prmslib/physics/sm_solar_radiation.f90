submodule (SOLAR_RADIATION) sm_solar_radiation
contains
  module subroutine init_SolarRadiation(this, ctl_data, model_basin, model_summary)
    use UTILS_PRMS, only: PRMS_open_module_file
    implicit none

    class(SolarRadiation), intent(inout) :: this
      !! SolarRadiation class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
      !! Model basin
    type(Summary), intent(inout) :: model_summary

    ! --------------------------------------------------------------------------
    ! Local Variables
    real(r64), parameter :: ECCENTRICY = 0.01671_dp
    real(r64), parameter :: DAYSYR = 365.242_dp
    ! 0.016723401  daily change -1.115E-09, eccen = 0.016723401 + (julhour-julhour(1966,1,0,18))+dmin/60)/24*-1.115E-09
    ! julday(1966,1,0.75 UT) = 2439126.25
    ! eccen = 0.01675104-0.00004180*T-0.000000126*T^2  T is julian centuries (days time from epoch, is GMT from Jan 0.0
    real(r64), parameter :: DEGDAY = 360.0_dp / DAYSYR
    real(r64), parameter :: DEGDAYRAD = DEGDAY * RADIANS ! about 0.00143356672

    integer(i32) :: i
    real(dp), parameter :: JD(366) = [(dble(i), i=1,366)]
    real(dp), parameter :: Y(366) = (JD - 1.0_dp) * DEGDAYRAD
    real(dp), parameter :: JD3(366) = (JD - 3.0_dp) * DEGDAYRAD
    real(dp), parameter :: Y2(366) = Y * 2.0_dp
    real(dp), parameter :: Y3(366) = Y * 3.0_dp
    real(dp), parameter :: obliquity(366) = 1.0_dp - (ECCENTRICY * cos(JD3))

    character(len=12) :: output_path
    integer(i32) :: j
    integer(i32) :: jj
    integer(i32) :: chru
    integer(i32) :: file_unit
    integer(i32) :: nn
    real(r64) :: basin_cossl
    real(r64) :: basin_sunhrs(366)

    ! ------------------------------------------------------------------------
    associate(outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &
              solrad_module => ctl_data%solrad_module%values(1), &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths, &
              hru_lat => model_basin%hru_lat, &
              hru_type => model_basin%hru_type, &
              hru_slope => model_basin%hru_slope, &
              hru_aspect => model_basin%hru_aspect, &
              active_hrus => model_basin%active_hrus, &
              active_mask => model_basin%active_mask, &
              basin_lat => model_basin%basin_lat, &
              hru_route_order => model_basin%hru_route_order)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Read dimensions
      this%nsol = param_hdl%get_dimension('nsol')

      ! Read parameters
      allocate(this%ppt_rad_adj(nhru, nmonths))
      call param_hdl%get_variable('ppt_rad_adj', this%ppt_rad_adj)

      allocate(this%radj_sppt(nhru))
      call param_hdl%get_variable('radj_sppt', this%radj_sppt)

      allocate(this%radj_wppt(nhru))
      call param_hdl%get_variable('radj_wppt', this%radj_wppt)

      allocate(this%radmax(nhru, nmonths))
      call param_hdl%get_variable('radmax', this%radmax)

      allocate(this%swrad(nhru))
      this%swrad = 0.0

      allocate(this%hru_cossl(nhru))
      allocate(this%soltab_sunhrs(366, nhru))
      allocate(this%soltab_potsw(366, nhru))
      allocate(this%soltab_horad_potsw(366, nhru))

      allocate(this%orad_hru(nhru))
      this%orad_hru = 0.0

      this%has_basin_obs_station = .false.
      this%has_hru_obs_station = .false.
      this%radiation_cv_factor = 1.0

      if (this%nsol > 0) then
        ! TODO: Hookup code to read the solar radiation data
        allocate(this%solrad(this%nsol))
        this%solrad = 0.0

        if (param_hdl%var_exists('hru_solsta')) then
        ! if (param_data%hru_solsta%exists()) then
          allocate(this%hru_solsta(this%nsol))
          call param_hdl%get_variable('hru_solsta', this%hru_solsta)

          ! If no radiation stations are available for the active HRUs
          ! then the sum of hru_solsta is 0
          this%has_hru_obs_station = sum(this%hru_solsta, mask=active_mask) > 0
        endif

        if (param_hdl%var_exists('basin_solsta')) then
        ! if (param_data%basin_solsta%exists()) then
          call param_hdl%get_variable('basin_solsta', this%basin_solsta)
          this%has_basin_obs_station = (this%basin_solsta > 0)
        endif

        if (param_hdl%var_exists('rad_conv')) then
        ! if (param_data%rad_conv%exists()) then
          call param_hdl%get_variable('rad_conv', this%rad_conv)
          this%radiation_cv_factor = this%rad_conv
        endif
      endif

      this%hru_cossl = 0.0
      this%soltab_sunhrs = 0.0
      this%soltab_potsw = 0.0
      this%soltab_horad_potsw = 0.0

      this%solar_declination = 0.006918_dp - &
                               0.399912_dp * COS(Y) + 0.070257_dp * SIN(Y) - &
                               0.006758_dp * COS(Y2) + 0.000907_dp * SIN(Y2) - &
                               0.002697_dp * COS(Y3) + 0.00148_dp * SIN(Y3)

      do nn=1, active_hrus
        chru = hru_route_order(nn)

        call this%compute_soltab(this%hru_cossl(chru), this%soltab_horad_potsw(:, chru), &
                                 this%soltab_sunhrs(:, chru), obliquity, &
                                 this%solar_declination, 0.0, 0.0, &
                                 hru_lat(chru), hru_type(chru), chru)

        call this%compute_soltab(this%hru_cossl(chru), this%soltab_potsw(:, chru), &
                                 this%soltab_sunhrs(:, chru), obliquity, &
                                 this%solar_declination, &
                                 hru_slope(chru), hru_aspect(chru), &
                                 hru_lat(chru), hru_type(chru), chru)
      enddo

      call this%compute_soltab(basin_cossl, this%soltab_basinpotsw, basin_sunhrs, &
                               obliquity, this%solar_declination, 0.0, 0.0, &
                               sngl(basin_lat), 0, 0)

      if (ctl_data%print_debug%value == 5) then
        output_path = 'soltab_debug'
        print *, ''
        print *, 'soltab debug data written to: ', output_path
        call PRMS_open_module_file(file_unit, output_path)

        do chru=1, nhru
          write(file_unit, *) 'HRU:', chru
          write(file_unit, *) '***Soltab_sunhrs***'
          write(file_unit, '(13F8.3)') (this%soltab_sunhrs(j, chru), j=1, 366)
          write(file_unit, *) '***Soltab_potsw***'
          write(file_unit, '(13F8.3)') (this%soltab_potsw(j, chru), j=1, 366)
        enddo

        !       write ( file_unit, * ) obliquity, Solar_declination
        write(file_unit, *) 2.0_dp / (obliquity(356) * obliquity(356)), 2.0_dp / (obliquity(10) * obliquity(10)), &
                            2.0_dp / (obliquity(23) * obliquity(23)), 2.0_dp / (obliquity(38) * obliquity(38)), &
                            2.0_dp / (obliquity(51) * obliquity(51)), 2.0_dp / (obliquity(66) * obliquity(66)), &
                            2.0_dp / (obliquity(80) * obliquity(80)), 2.0_dp / (obliquity(94) * obliquity(94)), &
                            2.0_dp / (obliquity(109) * obliquity(109)), 2.0_dp / (obliquity(123) * obliquity(123)), &
                            2.0_dp / (obliquity(138) * obliquity(138)), 2.0_dp / (obliquity(152) * obliquity(152)), &
                            2.0_dp / (obliquity(173) * obliquity(173))
        write(file_unit, *) this%solar_declination(356), this%solar_declination(10), this%solar_declination(23), &
                            this%solar_declination(38), this%solar_declination(51), this%solar_declination(66), &
                            this%solar_declination(80), this%solar_declination(94), this%solar_declination(109), &
                            this%solar_declination(123), this%solar_declination(138), this%solar_declination(152), &
                            this%solar_declination(173)
        close(file_unit)
      endif
      ! deallocate (Hru_slope, Hru_aspect)


      ! if (solrad_module%s == 'ccsolrad' .or. solrad_module%s == 'ddsolrad') then
      !   allocate(this%orad_hru(nhru))
      !   this%orad_hru = 0.0
      ! endif
      allocate(this%basin_horad)
      allocate(this%basin_orad)
      allocate(this%basin_potsw)
      allocate(this%basin_swrad)

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          ! TODO: This is where the daily basin values are linked based on
          !       what was requested in basinOutVar_names.
          select case(outVar_names%values(jj)%s)
            case('basin_horad')
              call model_summary%set_summary_var(jj, this%basin_horad)
            case('basin_orad')
              call model_summary%set_summary_var(jj, this%basin_orad)
            case('basin_potsw')
              call model_summary%set_summary_var(jj, this%basin_potsw)
            case('basin_swrad')
              call model_summary%set_summary_var(jj, this%basin_swrad)
            case('orad_hru')
              call model_summary%set_summary_var(jj, this%orad_hru)
            case('swrad')
              call model_summary%set_summary_var(jj, this%swrad)
            case default
              ! pass
          end select
        enddo
      endif
    end associate
  end subroutine

  module subroutine run_SolarRadiation(this, ctl_data, model_time, model_precip, model_basin, model_temp)
    class(SolarRadiation), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    class(Precipitation), intent(in) :: model_precip
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    print *, 'SolarRadiation%run() stub'
  end subroutine


  module subroutine compute_soltab(Cossl, Soltab_daily, Sunhrs_daily, Obliquity, &
                                   Solar_declination, Slope, Aspect, Latitude, &
                                   Hru_type, Id)
    use prms_constants, only: DNEARZERO
    implicit none

    ! Functions
    INTRINSIC ASIN, SIN, COS, ATAN, ABS

    ! Arguments
    real(r64), intent(out) :: Cossl
    real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Soltab_daily
    real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Sunhrs_daily

    real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Obliquity
    real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Solar_declination
    real(r32), intent(in) :: Slope
    real(r32), intent(in) :: Aspect
    real(r32), intent(in) :: Latitude
    integer(i32), intent(in) :: Hru_type
    integer(i32), intent(in) :: Id

    ! Constants
    real(r64), parameter :: r0 = 2.0_dp
      !! minute solar constant cal/cm2/min (r0 could also be 1.95 (Drummond, et al 1968))

    ! Local Variables
    real(r64) :: a
      !! Aspect in radians
    real(r64) :: x0
      !! Latitude of HRU
    real(r64) :: x1
      !! Latitude of equivalent slope
    real(r64) :: x2
      !! The difference in longitude between the location of the HRU and the equivalent horizontal surface expressed in angle hour
    real(r64), dimension(DAYS_PER_YEAR) :: r1
      !! The hour solar constant cal/cm2/hour
    real(r64) :: d1
      !! The denominator of equation 12 (Lee, 1963)
    real(r64), dimension(DAYS_PER_YEAR) :: t
      !! The angle hour from the local meridian (local solar noon) to the sunrise (negative) or sunset (positive)
    real(r64), dimension(DAYS_PER_YEAR) :: sunh
      !! Number of hours of direct sunlight (sunset minus sunrise)
    real(r64), dimension(DAYS_PER_YEAR) :: solt
      !! Swift's R4 (potential solar radiation on a sloping surface cal/cm2/day)
    real(r64), dimension(DAYS_PER_YEAR) :: t0
      !! The hour angle of sunrise on a hroizontal surface at the HRU
    real(r64), dimension(DAYS_PER_YEAR) :: t1
      !! The hour angle of sunset on a horizontal surface at the HRU
    real(r64), dimension(DAYS_PER_YEAR) :: t2
      !! The hour angle of sunset on the slope at the HRU
    real(r64), dimension(DAYS_PER_YEAR) :: t3
      !! The hour angle of sunrise on the slope at the HRU
    real(r64), dimension(DAYS_PER_YEAR) :: t6
      !! The hour angle of sunrise on the equivalent slope
    real(r64), dimension(DAYS_PER_YEAR) :: t7
      !! The hour angle of sunset on the equivalent slope
    ! real(r64), dimension(DAYS_PER_YEAR) :: d
      !! Solar declination for a day of the year
    real(r64) :: sl
      !! Arc-tangent of the slope

    real(r64) :: sl_sin, sl_cos, x0_sin, x0_cos, a_sin, a_cos
    !***********************************************************************
    ! from SWIFT (1976)
    ! x0, x1, x2 = l0, l1, l2
    ! sl = i

    sl = ATAN(Slope)
    sl_sin = sin(sl)
    sl_cos = cos(sl)

    Cossl = sl_cos
    ! Cossl = COS(sl)

    a = Aspect * RADIANS
    a_sin = sin(a)
    a_cos = cos(a)

    ! x0 latitude of HRU
    x0 = Latitude * RADIANS
    x0_sin = sin(x0)
    x0_cos = cos(x0)

    ! x1 latitude of equivalent slope
    ! This is equation 13 from Lee, 1963
    ! x1 = ASIN(Cossl * SIN(x0) + SIN(sl) * COS(x0) * COS(a))
    x1 = ASIN(Cossl * x0_sin + sl_sin * x0_cos * a_cos)

    ! d1 is the denominator of equation 12, Lee, 1963
    ! d1 = Cossl * COS(x0) - SIN(sl) * SIN(x0) * COS(a)
    d1 = Cossl * x0_cos - sl_sin * x0_sin * a_cos
    if (ABS(d1) < DNEARZERO) d1 = DNEARZERO

    ! x2 is the difference in longitude between the location of
    ! the HRU and the equivalent horizontal surface expressed in angle hour
    ! This is equation 12 from Lee, 1963
    ! x2 = ATAN(SIN(sl) * SIN(a) / d1)
    x2 = ATAN(sl_sin * a_sin / d1)
    if (d1 < 0.0_dp) x2 = x2 + PI

    r1 = (60.0_dp * r0) / (Obliquity**2)

    t = compute_t(x1, Solar_declination)
    t7 = t - x2
    t6 = -t - x2

    t = compute_t(x0, Solar_declination)
    t1 = t
    t0 = -t

    where (t7 > t1)
      t3 = t1
    else where
      t3 = t7
    end where

    where (t6 < t0)
      t2 = t0
    else where
      t2 = t6
    end where

    if (abs(sl) < DNEARZERO) then
      solt = func3(0.0_dp, x0, t1, t0, r1, Solar_declination)
      sunh = (t1 - t0) * PI_12
    else
      where (t3 < t2)
        t2 = 0.0_dp
        t3 = 0.0_dp
      end where
      t6 = t6 + TWOPI

      where (t6 < t1)
        solt = func3(x2, x1, t3, t2, r1, Solar_declination) + func3(x2, x1, t1, t6, r1, Solar_declination)
        sunh = (t3 - t2 + t1 - t6) * PI_12
      else where
        t7 = t7 - TWOPI

        where (t7 > t0)
          solt = func3(x2, x1, t3, t2, r1, Solar_declination) + func3(x2, x1, t7, t0, r1, Solar_declination)
          sunh = (t3 - t2 + t7 - t0) * PI_12
        else where
          solt = func3(x2, x1, t3, t2, r1, Solar_declination)
          sunh = (t3 - t2) * PI_12
        end where
      end where
    endif

    ! if (solt < 0.0D0) then
    !   print *, 'WARNING: solar table value for day:', jd, &
    !            ' computed as:', solt, ' set to', 0.0, &
    !            ' for HRU:', Id, ' hru_type:', Hru_type
    !   print *, 'slope, aspect, latitude, cossl', Slope, Aspect, &
    !            Latitude, Cossl
    !
    !   solt = 0.0D0
    !   print *, Slope, Aspect, Latitude, Cossl, sunh
    !   print *, t0, t1, t2, t3, t6, t7, d
    ! endif

    where (sunh < DNEARZERO) sunh = 0.0_dp
    Sunhrs_daily = sunh
    Soltab_daily = solt
  end subroutine




  !***********************************************************************
  !***********************************************************************
  pure elemental module function compute_t(Lat, Solar_declination) result(T)
    implicit none

    INTRINSIC TAN, ACOS

    ! Arguments
    real(r64) :: T
      !! Angle hour from the local meridian (local solar noon) to the sunrise(negative) or sunset(positive).
    real(r64), intent(in) :: Lat
      !! Latitude
    real(r64), intent(in) :: Solar_declination
      !! Declination of the sun on a day.


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
  end function

  !***********************************************************************
  !***********************************************************************
  ! This is the radian angle version of FUNC3 (eqn 6) from Swift, 1976
  ! or Lee, 1963 equation 5.
  ! func3 (R4) is potential solar radiation on the surface cal/cm2/day
  pure elemental module function func3(V, W, X, Y, R1, Solar_declination) result(res)
    implicit none

    INTRINSIC SIN, COS

    ! Arguments
    real(r64) :: res
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
    res = R1 * PI_12 * (SIN(Solar_declination) * SIN(W) * (X - Y) + &
                        COS(Solar_declination) * COS(W) * (SIN(X + V) - &
                        SIN(Y + V)))
  end function

end submodule
