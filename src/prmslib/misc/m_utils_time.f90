module UTILS_TIME
  use iso_fortran_env, only: output_unit
  use variableKind
  implicit none

  contains
    !***********************************************************************
    ! gregorian_to_julian
    ! computes the Julian Day given a Gregorian calendar date
    !***********************************************************************
    pure function gregorian_to_julian(year, month, day) result(julian_day)
      implicit none

      ! Arguments
      integer(i32) :: julian_day
      integer(i32), intent(in) :: year
      integer(i32), intent(in) :: month
      integer(i32), intent(in) :: day

      !***********************************************************************
      julian_day = day - 32075 + 1461 * (year + 4800 + (month - 14) / 12) / 4 + &
                      367 * (month - 2 - (month - 14) / 12 * 12) / 12 - &
                      3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
    end function


    !***********************************************************************
    ! julian_to_gregorian
    ! computes the Gregorian calendar date given the Julian Day
    !***********************************************************************
     function julian_to_gregorian(julian_day) result(res)
      implicit none

      ! Arguments
      integer(i32) :: res(6)
        !! Gregorian date ([1]=year, [2]=month, [3]=day)
      integer(i32), intent(in) :: julian_day
      ! integer(i32), intent(out) :: year
      ! integer(i32), intent(out) :: month
      ! integer(i32), intent(out) :: day

      ! Local Variables
      integer(i32) :: m
      integer(i32) :: n

      !***********************************************************************
      ! See: http://aa.usno.navy.mil/faq/docs/JD_Formula.php
      res = 0

      m = julian_day + 68569
      n = 4 * m / 146097
      m = m - (146097 * n + 3) / 4
      res(1) = 4000 * (m + 1) / 1461001

      m = m - 1461 * res(1) / 4 + 31
      res(2) = 80 * m / 2447
      res(3) = m - 2447 * res(2) / 80

      m = res(2) / 11
      res(2) = res(2) + 2 - 12 * m
      res(1) = 100 * (n - 49) + res(1) + m
    end function


    !***********************************************************************
    ! leap_day - is the year a leap year: (1=yes; 0=no)
    !***********************************************************************
    pure function leap_day(year) result(res)
      implicit none

      ! Arguments
      logical :: res
      integer(i32), intent(in) :: year

      ! Functions
      INTRINSIC MOD

      !***********************************************************************
      res = .false.

      ! Check if leapyear - Start by identifying all years not divisible by 4
      if (MOD(year, 4) == 0) then
          res = .true.
          if (MOD(year, 100) == 0) then
              if (MOD(year, 400) /= 0) res = .false.
          endif
      endif
    end function


    !***********************************************************************
    ! ordinal_date
    ! computes the ordinal day of the year given a Gregorian calendar date
    ! (Year, Month, Day) relative to:
    !   calendar (Jan 1),
    !   solar (12/22 in Northern; 6/21 in Southern) and
    !   water year (10/1 in Northern; 4/1 in Southern) start dates.
    ! The day of the year starts at noon of the Gregorian day and
    ! extends to noon the next Gregorian day.
    !
    ! 2017-10-30 PAN: moved here from utils_prms.f90
    !***********************************************************************
    function ordinal_date(datetime, Year_type, hemisphere) result(res)
      use prms_constants, only: YEAR, MONTH, DAY, NORTHERN, SOUTHERN
      implicit none

      ! Arguments
      integer(i32) :: res
      ! class(Time_t) :: this
      integer(i32), intent(in) :: datetime(6)
      character(len=*), intent(in) :: Year_type
        !! One of: "calendar", "solar", "water", "absolute"
      integer(i32), intent(in) :: hemisphere
        !! Hemisphere (0=North; 1=South)

      ! Local Variables
      integer(i32) :: reftime_year
      integer(i32) :: reftime_month
      integer(i32) :: reftime_day
      ! integer(i32) :: time_array(6)
      integer(i32) :: yr
      integer(i32) :: mo
      integer(i32) :: dy
      integer(i32) :: absolute_julday
      integer(i32) :: relative_julday
      logical :: found

      !***********************************************************************
      yr = datetime(YEAR)
      mo = datetime(MONTH)
      dy = datetime(DAY)

      found = .false.

      ! set reftime depending on type arg
      if (Year_type == 'solar') then
        found = .true.

        if (hemisphere == NORTHERN) then ! Northern
          if (mo == 12 .AND. dy > 21) then
            reftime_year = yr
          else
            reftime_year = yr - 1
          endif

          reftime_month = 12
          reftime_day = 21
        else ! Southern
          if (mo == 6 .AND. dy > 20) then
            reftime_year = yr;
          else
            reftime_year = yr - 1
          endif

          reftime_month = 6
          reftime_day = 20
        endif
      elseif (Year_type == 'water') then
        found = .true.

        if (hemisphere == NORTHERN) then ! Northern
          if (mo > 9) then
            reftime_year = yr
          else
            reftime_year = yr - 1
          endif

          reftime_month = 9
          reftime_day = 30
        else ! Southern
          if (mo > 3) then
            reftime_year = yr
          else
            reftime_year = yr - 1
          endif

          reftime_month = 3
          reftime_day = 31
        endif
      elseif (Year_type == 'spring') then
        found = .true.

        if (hemisphere == NORTHERN) then ! Northern
          if (mo > 3 .OR. (mo == 3 .AND. dy > 20)) then
            reftime_year = yr
          else
            reftime_year = yr - 1
          endif

          reftime_month = 3
          reftime_day = 20
        else ! Southern
          if (mo > 9 .OR. (mo == 9 .AND. dy > 22)) then
            reftime_year = yr
          else
            reftime_year = yr - 1
          endif

          reftime_month = 9
          reftime_day = 22
        endif
      elseif (Year_type == 'calendar') then
        found = .true.
        reftime_year = yr - 1
        reftime_month = 12
        reftime_day = 31
      endif

      if (.not. found) then
        print *, 'ERROR, invalid year type argument to compute Julian Day: ', Year_type
        STOP
      endif

      ! set actual Julian Day
      absolute_julday = gregorian_to_julian(yr, mo, dy)

      ! relative_julday = 0

      ! 2018-01-18 PAN: this conditional isn't needed
      ! if (Year_type == 'calendar') then
      !     relative_julday = gregorian_to_julian(reftime_year, reftime_month, reftime_day)
      ! else
      !     relative_julday = gregorian_to_julian(reftime_year, reftime_month, reftime_day)
      ! endif
      relative_julday = gregorian_to_julian(reftime_year, reftime_month, reftime_day)

      res = absolute_julday - relative_julday
    end function
end module