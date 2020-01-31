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
    character(len=12) :: output_path
    integer(i32) :: j
    integer(i32) :: jj
    integer(i32) :: chru
    integer(i32) :: file_unit
    integer(i32) :: nn
    real(r64) :: basin_cossl
    real(r64) :: basin_sunhrs(DAYS_PER_YEAR)

    ! ------------------------------------------------------------------------
    associate(outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &

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
      allocate(this%soltab_sunhrs(DAYS_PER_YEAR, nhru))
      allocate(this%soltab_potsw(DAYS_PER_YEAR, nhru))
      allocate(this%soltab_horad_potsw(DAYS_PER_YEAR, nhru))

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
          allocate(this%hru_solsta(this%nsol))
          call param_hdl%get_variable('hru_solsta', this%hru_solsta)

          ! If no radiation stations are available for the active HRUs
          ! then the sum of hru_solsta is 0
          this%has_hru_obs_station = sum(this%hru_solsta, mask=active_mask) > 0
        endif

        if (param_hdl%var_exists('basin_solsta')) then
          call param_hdl%get_variable('basin_solsta', this%basin_solsta)
          this%has_basin_obs_station = (this%basin_solsta > 0)
        endif

        if (param_hdl%var_exists('rad_conv')) then
          call param_hdl%get_variable('rad_conv', this%rad_conv)
          this%radiation_cv_factor = this%rad_conv
        endif
      endif

      this%hru_cossl = 0.0
      this%soltab_sunhrs = 0.0
      this%soltab_potsw = 0.0
      this%soltab_horad_potsw = 0.0

      do nn=1, active_hrus
        chru = hru_route_order(nn)

        ! TODO: 2020-01-30 PAN: move the computation of hru_cossl into a separate function?
        ! Compute potential solar radiation on a horizontal plane for each day and hru
        call this%compute_soltab(this%hru_cossl(chru), this%soltab_horad_potsw(:, chru), &
                                 this%soltab_sunhrs(:, chru), &
                                 SOLAR_DECLINATION, 0.0, 0.0, &
                                 hru_lat(chru))

        ! Compute potential solar radiation for each day and hru
        call this%compute_soltab(this%hru_cossl(chru), this%soltab_potsw(:, chru), &
                                 this%soltab_sunhrs(:, chru), &
                                 SOLAR_DECLINATION, &
                                 hru_slope(chru), hru_aspect(chru), &
                                 hru_lat(chru))
      enddo

      ! Compute basin average potential solar radiation on a horizontal plane for each day
      call this%compute_soltab(basin_cossl, this%soltab_basinpotsw, basin_sunhrs, &
                               SOLAR_DECLINATION, 0.0, 0.0, &
                               sngl(basin_lat))

      if (ctl_data%print_debug%value == 5) then
        output_path = 'soltab_debug'
        print *, ''
        print *, 'soltab debug data written to: ', output_path
        call PRMS_open_module_file(file_unit, output_path)

        do chru=1, nhru
          write(file_unit, *) 'HRU:', chru
          write(file_unit, *) '***Soltab_sunhrs***'
          write(file_unit, '(13F8.3)') (this%soltab_sunhrs(j, chru), j=1, DAYS_PER_YEAR)
          write(file_unit, *) '***Soltab_potsw***'
          write(file_unit, '(12F9.3)') (this%soltab_potsw(j, chru), j=1, DAYS_PER_YEAR)
        enddo

        !       write ( file_unit, * ) obliquity, Solar_declination
        write(file_unit, *) '*** Obliquity ***'
        write(file_unit, '(13F18.14)') 2.0_dp / (OBLIQUITY(356) * OBLIQUITY(356)), 2.0_dp / (OBLIQUITY(10) * OBLIQUITY(10)), &
                            2.0_dp / (OBLIQUITY(23) * OBLIQUITY(23)), 2.0_dp / (OBLIQUITY(38) * OBLIQUITY(38)), &
                            2.0_dp / (OBLIQUITY(51) * OBLIQUITY(51)), 2.0_dp / (OBLIQUITY(66) * OBLIQUITY(66)), &
                            2.0_dp / (OBLIQUITY(80) * OBLIQUITY(80)), 2.0_dp / (OBLIQUITY(94) * OBLIQUITY(94)), &
                            2.0_dp / (OBLIQUITY(109) * OBLIQUITY(109)), 2.0_dp / (OBLIQUITY(123) * OBLIQUITY(123)), &
                            2.0_dp / (OBLIQUITY(138) * OBLIQUITY(138)), 2.0_dp / (OBLIQUITY(152) * OBLIQUITY(152)), &
                            2.0_dp / (OBLIQUITY(173) * OBLIQUITY(173))
        write(file_unit, *) '*** Solar Declination ***'
        write(file_unit, '(13F18.14)') SOLAR_DECLINATION(356), SOLAR_DECLINATION(10), SOLAR_DECLINATION(23), &
                            SOLAR_DECLINATION(38), SOLAR_DECLINATION(51), SOLAR_DECLINATION(66), &
                            SOLAR_DECLINATION(80), SOLAR_DECLINATION(94), SOLAR_DECLINATION(109), &
                            SOLAR_DECLINATION(123), SOLAR_DECLINATION(138), SOLAR_DECLINATION(152), &
                            SOLAR_DECLINATION(173)
        close(file_unit)
      endif
      ! deallocate (Hru_slope, Hru_aspect)

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          ! TODO: This is where the daily basin values are linked based on
          !       what was requested in basinOutVar_names.
          select case(outVar_names%values(jj)%s)
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


  module subroutine compute_soltab(Cossl, Soltab_daily, Sunhrs_daily, &
                                   Solar_declination, Slope, Aspect, Latitude)
    use prms_constants, only: DNEARZERO
    implicit none

    ! Functions
    INTRINSIC ASIN, SIN, COS, ATAN, ABS

    ! Arguments
    real(r64), intent(out) :: Cossl
    real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Soltab_daily
    real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Sunhrs_daily
    ! real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Obliquity
    real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Solar_declination
    real(r32), intent(in) :: Slope
    real(r32), intent(in) :: Aspect
    real(r32), intent(in) :: Latitude
    ! integer(i32), intent(in) :: Hru_type
    ! integer(i32), intent(in) :: Id

    ! Local Variables
    real(r64) :: a
      !! Aspect in radians
    real(r64) :: x0
      !! Latitude of HRU
    real(r64) :: x1
      !! Latitude of equivalent slope
    real(r64) :: x2
      !! The difference in longitude between the location of the HRU and the equivalent horizontal surface expressed in angle hour
    ! real(r64), dimension(DAYS_PER_YEAR) :: r1
      !! The hour solar constant cal/cm2/hour
    real(r64) :: d1
      !! The denominator of equation 12 (Lee, 1963)
    real(r64), dimension(DAYS_PER_YEAR) :: t
      !! The hour angle from the local meridian (local solar noon) to the sunrise (negative) or sunset (positive)
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
    ! x0, x1, x2 = L0, L1, L2
    ! sl = I

    ! Slope should be inclination of slope, degrees above horizontal
    sl = ATAN(Slope)
    sl_sin = sin(sl)
    sl_cos = cos(sl)

    Cossl = sl_cos
    ! Cossl = COS(sl)

    a = Aspect * RADIANS
    a_sin = sin(a)
    a_cos = cos(a)

    ! x0 - latitude of HRU
    x0 = Latitude * RADIANS
    x0_sin = sin(x0)
    x0_cos = cos(x0)

    ! x1 - latitude of equivalent slope
    ! This is equation 13 from Lee, 1963
    ! x1 = ASIN(Cossl * SIN(x0) + SIN(sl) * COS(x0) * COS(a))
    x1 = ASIN(sl_cos * x0_sin + sl_sin * x0_cos * a_cos)

    ! d1 - the denominator of equation 12, Lee, 1963
    ! d1 = Cossl * COS(x0) - SIN(sl) * SIN(x0) * COS(a)
    d1 = Cossl * x0_cos - sl_sin * x0_sin * a_cos
    if (ABS(d1) < DNEARZERO) d1 = DNEARZERO

    ! x2 - the difference in longitude between the location of
    ! the HRU and the equivalent horizontal surface expressed in angle hour
    ! This is equation 12 from Lee, 1963
    ! x2 = ATAN(SIN(sl) * SIN(a) / d1)
    x2 = ATAN(sl_sin * a_sin / d1)
    if (d1 < 0.0_dp) x2 = x2 + PI

    ! ! Solar constant for 60 minutes
    ! r1 = (60.0_dp * r0) / (Obliquity**2)

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
      solt = func3(0.0_dp, x0, t1, t0, R1, SOLAR_DECLINATION)
      sunh = (t1 - t0) * PI_12
    else
      where (t3 < t2)
        t2 = 0.0_dp
        t3 = 0.0_dp
      end where
      t6 = t6 + TWOPI

      where (t6 < t1)
        solt = func3(x2, x1, t3, t2, R1, SOLAR_DECLINATION) + func3(x2, x1, t1, t6, R1, SOLAR_DECLINATION)
        sunh = (t3 - t2 + t1 - t6) * PI_12
      else where
        t7 = t7 - TWOPI

        where (t7 > t0)
          solt = func3(x2, x1, t3, t2, R1, SOLAR_DECLINATION) + func3(x2, x1, t7, t0, R1, SOLAR_DECLINATION)
          sunh = (t3 - t2 + t7 - t0) * PI_12
        else where
          solt = func3(x2, x1, t3, t2, R1, SOLAR_DECLINATION)
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
  pure elemental module function compute_t(lat, solar_declination) result(res)
    implicit none

    INTRINSIC TAN, ACOS

    ! Arguments
    real(r64) :: res
      !! Angle hour from the local meridian (local solar noon) to the sunrise(negative) or sunset(positive).
    real(r64), intent(in) :: lat
      !! Latitude
    real(r64), intent(in) :: solar_declination
      !! Declination of the sun on a day.

    ! Local Variables
    real(r64) :: tx
    !***********************************************************************

    ! This is the sunrise equation
    !  lat - the latitude
    !  solar_declination - the declination of the sun on a day
    !  res - the angle hour from the local meridian (local solar noon) to the
    !        sunrise (negative) or sunset (positive).  The Earth rotates at the angular
    !        speed of 15 degrees/hour (2 pi / 24 hour in radians) and, therefore,
    !        res/15 degrees (res*24/pi in radians) gives the time of sunrise as the number
    !        of hours before the local noon, or the time of sunset as the number of
    !        hours after the local noon. Here the term local noon indicates the local
    !        time when the sun is exactly to the south or north or exactly overhead.
    tx = -TAN(lat) * TAN(solar_declination)

    if (tx < -1.0D0) then
      res = PI
      !rsr bug fix, old code would set t=acos(0.0) for tx>1 12/05
    elseif (tx > 1.0D0) then
      res = 0.0D0
    else
      res = ACOS(tx)
    endif
  end function

  !***********************************************************************
  !***********************************************************************
  ! This is the radian angle version of FUNC3 (eqn 6) from Swift, 1976
  ! or Lee, 1963 equation 5.
  ! func3 (R4) is potential solar radiation on the surface cal/cm2/day
  pure elemental module function func3(v, w, x, y, r1, solar_declination) result(res)
    implicit none

    INTRINSIC SIN, COS

    ! Arguments
    real(r64) :: res
    real(r64), intent(in) :: v
      !! Latitude angle hour offset between actual and equivalent slope
    real(r64), intent(in) :: w
      !! Latitude of the equivalent slope
    real(r64), intent(in) :: x
      !! Hour angle of sunset on equivalent slope
    real(r64), intent(in) :: y
      !! Hour angle of sunrise on equivalent slope
    real(r64), intent(in) :: r1
      !! Solar constant for 60 minutes
    real(r64), intent(in) :: solar_declination
      !! Declination of sun

    !***********************************************************************
    res = r1 * PI_12 * (SIN(solar_declination) * SIN(w) * (x - y) + &
                        COS(solar_declination) * COS(w) * (SIN(x + v) - SIN(y + v)))
  end function

end submodule
