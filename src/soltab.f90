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
    use kinds_mod, only: r4, r8, i4, i8
    implicit none

    !   Local Variables
    real(r8), parameter :: PI = 3.1415926535898D0
    real(r8), parameter :: RADIANS = PI / 180.0D0   ! RADIANS ~ 0.017453292519943
    real(r8), parameter :: TWOPI = 2.0D0 * PI       ! TWOPI ~ 6.2831853071786
    real(r8), parameter :: PI_12 = 12.0D0 / PI      ! PI_12 ~ 3.8197186342055

    character(len=6), save :: MODNAME
    real(r8), save :: Solar_declination(366), Soltab_basinpotsw(366)
    real(r8), save, allocatable :: Hru_cossl(:), Soltab_sunhrs(:, :)

    !   Declared Variables
    real(r8), save, allocatable :: Soltab_potsw(:, :), Soltab_horad_potsw(:, :)

    !   Declared Parameters
    real(r4), save, allocatable :: Hru_aspect(:), Hru_slope(:)

    private :: sthdecl, sthinit, compute_t, func3, compute_soltab
    public :: soltab

    contains
        !***********************************************************************
        !     Main soltab routine
        !***********************************************************************
        integer function soltab(dim_data, param_data, var_data)
            use PRMS_MODULE, only: Process
            use dimensions_mod, only: dimension_list
            use parameter_arr_mod, only: parameter_arr_t
            use variables_arr_mod, only: variables_arr_t
            implicit none

            type(dimension_list), intent(in) :: dim_data
            type(parameter_arr_t), intent(inout) :: param_data
            type(variables_arr_t), intent(inout) :: var_data

            !***********************************************************************
            soltab = 0

            if (Process == 'declare') then
                soltab = sthdecl(dim_data, param_data, var_data)
            elseif (Process == 'init') then
                soltab = sthinit(param_data)
            endif
        end function soltab

        !***********************************************************************
        !     sthdecl - set up parameters for solar radiation computations
        !   Declared Parameters
        !     hru_aspect, hru_lat, hru_slope
        !***********************************************************************
        integer function sthdecl(dim_data, param_data, var_data)
            use PRMS_MODULE, only:Nhru, print_module
            use UTILS_PRMS, only: read_error
            ! use parameter_mod, only: declparam
            ! use variables_mod, only: declvar_dble
            ! use PRMS_MMFAPI, only: declvar_dble  ! , declparam
            use dimensions_mod, only: dimension_list
            use parameter_arr_mod, only: parameter_arr_t
            use variables_arr_mod, only: variables_arr_t
            implicit none

            type(dimension_list), intent(in) :: dim_data
            type(parameter_arr_t), intent(inout) :: param_data
            type(variables_arr_t), intent(inout) :: var_data

            ! Functions
            INTRINSIC INDEX

            ! Local Variables
            character(len=80), save :: Version_soltab

            !***********************************************************************
            sthdecl = 0

            Version_soltab = 'soltab.f90 2016-09-29 13:48:00Z'
            call print_module(Version_soltab, 'Potential Solar Radiation   ', 90)
            MODNAME = 'soltab'

            allocate (Soltab_potsw(366, Nhru))
            call var_data%declvar_dble(MODNAME, 'soltab_potsw', 'ndays,nhru', 366 * Nhru, 'double', &
                    &     'Potential solar radiation for each Julian Day, for each HRU', &
                    &     'Langleys', Soltab_potsw)

            allocate (Soltab_horad_potsw(366, Nhru))
            call var_data%declvar_dble(MODNAME, 'soltab_horad_potsw', 'ndays,nhru', 366 * Nhru, 'double', &
                    &     'Potential solar radiation on a horizontal plane for each Julian Day, for each HRU', &
                    &     'Langleys', Soltab_horad_potsw)

            allocate (Hru_cossl(Nhru), Soltab_sunhrs(366, Nhru))

            !   Declared Parameters
            allocate (Hru_slope(Nhru))
            if (param_data%declparam(MODNAME, 'hru_slope', 'nhru', 'real', &
                    &     '0.0', '0.0', '10.0', &
                    &     'HRU slope', &
                    &     'Slope of each HRU, specified as change in vertical length divided by change in horizontal length', &
                    &     'decimal fraction', dim_data) /= 0) call read_error(1, 'hru_slope')

            allocate (Hru_aspect(Nhru))
            if (param_data%declparam(MODNAME, 'hru_aspect', 'nhru', 'real', &
                    &     '0.0', '0.0', '360.0', &
                    &     'HRU aspect', 'Aspect of each HRU', &
                    &     'angular degrees', dim_data) /= 0) call read_error(1, 'hru_aspect')

        end function sthdecl

        !***********************************************************************
        !     sthinit - Initialize soltab module - get parameter values,
        !               compute soltab_potsw (potential shortwave radiation)
        !               and soltab_sunhrs (hours between sunrise and sunset)
        !               for each HRU for each day of the year.
        !***********************************************************************
        integer function sthinit(param_data)
            use PRMS_MODULE, only:Nhru, Print_debug
            use PRMS_BASIN, only:Hru_type, Active_hrus, Hru_route_order, Basin_lat, Hru_lat
            use UTILS_PRMS, only: read_error, PRMS_open_module_file
            use parameter_arr_mod, only: parameter_arr_t
            ! use parameter_mod, only: getparam
            ! use PRMS_MMFAPI, only: getparam
            implicit none

            type(parameter_arr_t), intent(in) :: param_data

            ! Functions
            INTRINSIC SIN, COS, DBLE
            !     INTRINSIC ASIN

            ! Local Variables
            character(len=12) :: output_path
            integer(i4) :: jd, j, n, file_unit, nn
            real(r4) :: lat
            real(r8) :: basin_cossl
            real(r8) :: basin_sunhrs(366)
            real(r8) :: obliquity(366)
            real(r8) :: y, y2, y3, jddbl

            real(r8), parameter :: ECCENTRICY = 0.01671D0
            real(r8), parameter :: DAYSYR = 365.242D0
            ! 0.016723401  daily change -1.115E-09, eccen = 0.016723401 + (julhour-julhour(1966,1,0,18))+dmin/60)/24*-1.115E-09
            ! julday(1966,1,0.75 UT) = 2439126.25
            ! eccen = 0.01675104-0.00004180*T-0.000000126*T^2  T is julian centuries (days time from epoch, is GMT from Jan 0.0
            real(r8), parameter :: DEGDAY = 360.0D0 / DAYSYR
            real(r8), parameter :: DEGDAYRAD = DEGDAY * RADIANS ! about 0.00143356672
            ! DEGDAY = 360 degrees/days in year

            !***********************************************************************
            sthinit = 0

            if (param_data%getparam(MODNAME, 'hru_slope', Nhru, 'real', Hru_slope) /= 0) call read_error(2, 'hru_slope')
            if (param_data%getparam(MODNAME, 'hru_aspect', Nhru, 'real', Hru_aspect) /= 0) call read_error(2, 'hru_aspect')

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
                Solar_declination(jd) = 0.006918D0 - 0.399912D0 * COS(y) + 0.070257D0 * SIN(y) &
                                        - 0.006758D0 * COS(y2) + 0.000907D0 * SIN(y2) &
                                        - 0.002697D0 * COS(y3) + 0.00148D0 * SIN(y3)
            enddo

            !   Module Variables
            Soltab_sunhrs = 0.0D0
            Soltab_potsw = 0.0D0
            Soltab_horad_potsw = 0.0D0
            Hru_cossl = 0.0D0
            do nn = 1, Active_hrus
                n = Hru_route_order(nn)
                call compute_soltab(obliquity, Solar_declination, 0.0, 0.0, Hru_lat(n), &
                        &                      Hru_cossl(n), Soltab_horad_potsw(1, n), &
                        &                      Soltab_sunhrs(1, n), Hru_type(n), n)
                call compute_soltab(obliquity, Solar_declination, Hru_slope(n), Hru_aspect(n), &
                        &                      Hru_lat(n), Hru_cossl(n), Soltab_potsw(1, n), &
                        &                      Soltab_sunhrs(1, n), Hru_type(n), n)
            enddo

            lat = SNGL(Basin_lat)
            call compute_soltab(obliquity, Solar_declination, 0.0, 0.0, lat, basin_cossl, &
                    &                    Soltab_basinpotsw, basin_sunhrs, 0, 0)

            if (Print_debug == 5) then
                output_path = 'soltab_debug'
                print *, ''
                print *, 'soltab debug data written to: ', output_path
                call PRMS_open_module_file(file_unit, output_path)
                do n = 1, Nhru
                    write (file_unit, *) 'HRU:', n
                    write (file_unit, *) '***Soltab_sunhrs***'
                    write (file_unit, '(13F8.3)') (Soltab_sunhrs(j, n), j = 1, 366)
                    write (file_unit, *) '***Soltab_potsw***'
                    write (file_unit, '(13F8.3)') (Soltab_potsw(j, n), j = 1, 366)
                enddo
                !       write ( file_unit, * ) obliquity, Solar_declination
                write (file_unit, *) 2.0D0 / (obliquity(356) * obliquity(356)), 2.0D0 / (obliquity(10) * obliquity(10)), &
                        &                         2.0D0 / (obliquity(23) * obliquity(23)), 2.0D0 / (obliquity(38) * obliquity(38)), &
                        &                         2.0D0 / (obliquity(51) * obliquity(51)), 2.0D0 / (obliquity(66) * obliquity(66)), &
                        &                         2.0D0 / (obliquity(80) * obliquity(80)), 2.0D0 / (obliquity(94) * obliquity(94)), &
                        &                         2.0D0 / (obliquity(109) * obliquity(109)), 2.0D0 / (obliquity(123) * obliquity(123)), &
                        &                         2.0D0 / (obliquity(138) * obliquity(138)), 2.0D0 / (obliquity(152) * obliquity(152)), &
                        &                         2.0D0 / (obliquity(173) * obliquity(173))
                write (file_unit, *) Solar_declination(356), Solar_declination(10), Solar_declination(23), &
                        &                         Solar_declination(38), Solar_declination(51), Solar_declination(66), &
                        &                         Solar_declination(80), Solar_declination(94), Solar_declination(109), &
                        &                         Solar_declination(123), Solar_declination(138), Solar_declination(152), &
                        &                         Solar_declination(173)
                close (file_unit)
                ! from original soltab
                !     data obliquity/2.06699,2.06317,2.05582,2.04520,2.03243,2.01706,2.00080,
                !    +1.98553,1.96990,1.95714,1.94689,1.94005,1.93616/

                !     data Solar_declination/-.410152,-.383391,-.337430,-.27198,-.190532,-.09832,0.,
                !    +.09832,.190532,.27198,.33743,.383391,.410152/

                !     data jday/356,10,23,38,51,66,80,94,109,123,138,152,173/
            endif

            deallocate (Hru_slope, Hru_aspect)
        end function sthinit

        !***********************************************************************
        !  compute soltab_potsw (potential shortwave radiation)
        !  and soltab_sunhrs (hours between sunrise and sunset)
        !  for each HRU for each day of the year.
        !***********************************************************************
        subroutine compute_soltab(Obliquity, Solar_declination, Slope, Aspect, &
                &                          Latitude, Cossl, Soltab, Sunhrs, Hru_type, Id)
            use prms_constants, only: DNEARZERO
            implicit none

            ! Functions
            INTRINSIC ASIN, SIN, COS, ATAN, ABS

            ! Arguments
            integer(i4), intent(in) :: Hru_type, Id
            real(r8), intent(in), dimension(366) :: Obliquity, Solar_declination
            real(r4), intent(in) :: Slope, Aspect, Latitude
            real(r8), intent(out) :: Cossl
            real(r8), intent(out), dimension(366) :: Soltab, Sunhrs

            ! Local Variables
            integer(i4) :: jd
            real(r8) :: a, x0, x1, x2, r0, r1, d1, t, sunh, solt
            real(r8) :: t0, t1, t2, t3, t6, t7, d, sl

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

            ! r0 is the minute solar constant cal/cm2/min
            r0 = 2.0D0
            ! r0 could be 1.95 (Drummond, et al 1968)
            do jd = 1, 366
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

                !  compute_t is the sunrise equation.
                !  t7 is the hour angle of sunset on the equivalent slope
                !  t6 is the hour angle of sunrise on the equivalent slope
                call compute_t(x1, d, t)
                t7 = t - x2
                t6 = -t - x2

                !  compute_t is the sunrise equation.
                !  t1 is the hour angle of sunset on a hroizontal surface at the HRU
                !  t0 is the hour angle of sunrise on a hroizontal surface at the HRU
                call compute_t(x0, d, t)
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
                            &             ' computed as:', solt, ' set to', 0.0, &
                            &             ' for HRU:', Id, ' hru_type:', Hru_type
                    print *, 'slope, aspect, latitude, cossl', Slope, Aspect, Latitude, Cossl
                    solt = 0.0D0
                    print *, Slope, Aspect, Latitude, Cossl, sunh
                    print *, t0, t1, t2, t3, t6, t7, d
                endif
                if (sunh < DNEARZERO) sunh = 0.0D0
                Sunhrs(jd) = sunh
                Soltab(jd) = solt

            enddo

        end subroutine compute_soltab

        !***********************************************************************
        !***********************************************************************
        subroutine compute_t(Lat, Solar_declination, T)
            implicit none

            INTRINSIC TAN, ACOS

            ! Arguments
            real(r8), intent(in) :: Lat
            real(r8), intent(in) :: Solar_declination
            real(r8), intent(out) :: T

            ! Local Variables
            real(r8) :: tx
            !***********************************************************************

            !  This is the sunrise equation
            !  Lat is the latitude
            !  Solar_declination is the declination of the sun on a day
            !  T is the angle hour from the local meridian (local solar noon) to the
            !  sunrise (negative) or sunset (positive).  The Earth rotates at the angular
            !  speed of 15 degrees/hour (2 pi / 24 hour in radians) and, therefore, T/15 degress (T*24/pi
            !  in radians) gives the time of sunrise as the number of hours before the local
            !  noon, or the time of sunset as the number of hours after the local noon.
            !  Here the term local noon indicates the local time when the sun is exactly to
            !  the south or north or exactly overhead.
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
        real(r8) function func3(V, W, X, Y, R1, Solar_declination)
            implicit none

            INTRINSIC SIN, COS

            ! Arguments
            real(r8), intent(in) :: V
            real(r8), intent(in) :: W
            real(r8), intent(in) :: X
            real(r8), intent(in) :: Y
            real(r8), intent(in) :: R1
            real(r8), intent(in) :: Solar_declination
            !***********************************************************************
            !  This is the radian angle version of FUNC3 (eqn 6) from Swift, 1976
            !  or Lee, 1963 equation 5.
            !  func3 (R4) is potential solar radiation on the surface cal/cm2/day
            !  V (L2) latitude angle hour offset between actual and equivalent slope
            !  W (L1) latitude of the equivalent slope
            !  X (T3) hour angle of sunset on equivalent slope
            !  Y (T2) hour angle of sunrise on equivalent slope
            !  R1 solar constant for 60 minutes
            !  Solar_declination declination of sun
            func3 = R1 * PI_12 * (SIN(Solar_declination) * SIN(W) * (X - Y) + COS(Solar_declination) * COS(W) * (SIN(X + V) - SIN(Y + V)))
        end function func3

end module PRMS_SOLTAB
