module time_mod
  use kinds_mod, only: r4, r8, i4, i8
  implicit none

  integer(i4), parameter :: DAYPMO(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  contains
    !***********************************************************************
    ! julday_in_year
    ! computes the Julian Day of a date
    !***********************************************************************
    integer function julday_in_year(Year, Month, Day)
      implicit none

      ! Arguments
      integer(i4), intent(in) :: Year
      integer(i4), intent(in) :: Month
      integer(i4), intent(in) :: Day

      ! Local Variables
      integer(i4) :: i

      !***********************************************************************
      julday_in_year = Day

      do i=1, Month - 1
          julday_in_year = julday_in_year + DAYPMO(i)
      enddo

      ! Add additional day if the day is in a leap year after February
      if (leap_day(Year) .and. Month > 2) julday_in_year = julday_in_year + 1
    end function julday_in_year

    !***********************************************************************
    ! leap_day - is the year a leap year: (1=yes; 0=no)
    !***********************************************************************
    function leap_day(Year)
      implicit none

      ! Arguments
      logical :: leap_day
      integer(i4), intent(in) :: Year

      ! Functions
      INTRINSIC MOD

      !***********************************************************************
      leap_day = .false.

      ! Check if leapyear - Start by identifying all years not divisible by 4
      if (MOD(Year, 4) == 0) then
        leap_day = .true.

        if (MOD(Year, 100) == 0) then
          if (MOD(Year, 400) /= 0) leap_day = .false.
        endif
      endif
    end function leap_day

    !***********************************************************************
    ! compute_gregorian
    ! computes the Gregorian calendar date given the Julian Day
    !***********************************************************************
    subroutine compute_gregorian(Julday, Year, Month, Day)
      implicit none

      ! Arguments
      integer(i4), intent(in) :: Julday
      integer(i4), intent(OUT) :: Year
      integer(i4), intent(OUT) :: Month
      integer(i4), intent(OUT) :: Day

      ! Functions
      ! INTRINSIC FLOOR, NINT

      ! Local Variables
      integer(i4) :: m
      integer(i4) :: n

      !***********************************************************************
      m = Julday + 68569
      n = 4 * m / 146097
      m = m - (146097 * n + 3) / 4
      Year = 4000 * (m + 1) / 1461001
      m = m - 1461 * Year / 4 + 31
      Month = 80 * m / 2447
      Day = m - 2447 * Month / 80
      m = Month / 11
      Month = Month + 2 - 12 * m
      Year = 100 * (n - 49) + Year + m
    end subroutine compute_gregorian

    !***********************************************************************
    ! compute_julday
    ! computes the Julian Day given a Gregorian calendar date
    !***********************************************************************
    integer function compute_julday(Year, Month, Day)
      implicit none

      ! Arguments
      integer(i4), intent(in) :: Year
      integer(i4), intent(in) :: Month
      integer(i4), intent(in) :: Day

      !***********************************************************************
      compute_julday = Day - 32075 + 1461 * (Year + 4800 + (Month - 14) / 12) / 4 &
                       + 367 * (Month - 2 - (Month - 14) / 12 * 12) &
                       / 12 - 3 * ((Year + 4900 + (Month - 14) / 12) / 100) / 4
    end function compute_julday
end module
