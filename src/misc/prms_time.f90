!***********************************************************************
! Sets PRMS time variables
!***********************************************************************
module PRMS_SET_TIME
    use kinds_mod, only: r4, r8, i4, i8
    implicit none

    !   Local Variables
    !      character(len=10), save :: MODNAME
    ! integer(i4), save :: Modays(12)
    integer(i4), save :: Yrdays
    integer(i4), save :: Summer_flag
    integer(i4), save :: Jday
    integer(i4), save :: Jsol
    integer(i4), save :: Julwater
    integer(i4), save :: Julian_day_absolute
    integer(i4), save :: Nowtime(6)
    integer(i4), save :: Nowday
    integer(i4), save :: Nowmonth
    integer(i4), save :: Nowyear
    integer(i4), save :: Nowhour
    integer(i4), save :: Nowminute
    real(r4), save :: Timestep_hours
    real(r4), save :: Timestep_days
    real(r4), save :: Timestep_minutes
    real(r8), save :: Cfs2inches
    real(r8), save :: Cfs_conv
    real(r8), save :: Timestep_seconds

    contains
        !***********************************************************************
        ! dattim - get start, end, or current date and time
        ! 2017-11-07 PAN: moved here from mmf_utils.f90
        !***********************************************************************
        subroutine dattim(String, Datetime)
            use PRMS_MODULE, only: Endtime, Starttime
            use time_mod, only: compute_gregorian
            implicit none

            ! Arguments
            character(len=*), intent(in) :: String
            integer(i4), intent(out) :: Datetime(6)

            !***********************************************************************
            Datetime = 0

            if (String == 'end') then
                Datetime = Endtime
            elseif (String == 'now') then
                call compute_gregorian(Julian_day_absolute, Nowyear, Nowmonth, Nowday)
                Datetime(1) = Nowyear
                Datetime(2) = Nowmonth
                Datetime(3) = Nowday
                ! Datetime = LIS function
            elseif (String == 'start') then
                Datetime = Starttime
            else
                STOP 'ERROR, invalid call to dattim'
            endif
        end subroutine dattim

        !***********************************************************************
        ! timestep_hours - time step increment in hours
        ! 2017-11-07 PAN: moved here from mmf_utils.f90
        !***********************************************************************
        real(r8) function deltim()
            implicit none

            !***********************************************************************
            !deltim = lisfunction() ! need to make routine to get time step increment
            deltim = 24.0D0
        end function deltim

        !***********************************************************************
        ! julian_day
        ! computes the Julian date given a Gregorian calendar date
        ! (Year, Month, Day) relative to: calendar (Jan 1),
        ! solar (12/22 in Northern; 6/21 in Southern) and
        ! water year (10/1 in Northern; 4/1 in Southern) start dates.
        ! The Julian day starts at noon of the Gregorian day and
        ! extends to noon the next Gregorian day.
        !
        ! 2017-10-30 PAN: moved here from utils_prms.f90
        !***********************************************************************
        integer function julian_day(Date_type, Year_type)
            use PRMS_MODULE, only: Starttime, Endtime
            use PRMS_BASIN, only: Hemisphere
            use time_mod, only: compute_julday
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Date_type ! "start", "end", "now"
            character(len=*), intent(in) :: Year_type ! "calendar", "solar", "water", "absolute"

            ! Local Variables
            integer(i4) :: reftime_year, reftime_month, reftime_day, time_array(6)
            integer(i4) :: year
            integer(i4) :: month
            integer(i4) :: day
            integer(i4) :: absolute_julday
            integer(i4) :: relative_julday
            logical :: found

            !***********************************************************************
            if (Date_type == 'end') then
                time_array = Endtime
            elseif (Date_type == 'now') then
                time_array = Nowtime
            elseif (Date_type == 'start') then
                time_array = Starttime
            else
                print *, 'ERROR, invalid argument to compute Julian Day: ', Date_type
                STOP
            endif

            year = time_array(1)
            month = time_array(2)
            day = time_array(3)

            found = .false.

            ! set reftime depending on type arg
            if (Year_type == 'solar') then
                found = .true.

                if (Hemisphere == 0) then ! Northern
                    if (month == 12 .AND. day > 21) then
                        reftime_year = year
                    else
                        reftime_year = year - 1
                    endif

                    reftime_month = 12
                    reftime_day = 21
                else ! Southern
                    if (month == 6 .AND. day > 20) then
                        reftime_year = year;
                    else
                        reftime_year = year - 1
                    endif

                    reftime_month = 6
                    reftime_day = 20
                endif
            elseif (Year_type == 'water') then
                found = .true.

                if (Hemisphere == 0) then ! Northern
                    if (month > 9) then
                        reftime_year = year
                    else
                        reftime_year = year - 1
                    endif

                    reftime_month = 9
                    reftime_day = 30
                else ! Southern
                    if (month > 3) then
                        reftime_year = year
                    else
                        reftime_year = year - 1
                    endif

                    reftime_month = 3
                    reftime_day = 31
                endif
            elseif (Year_type == 'spring') then
                found = .true.

                if (Hemisphere == 0) then ! Northern
                    if (month > 3 .OR. (month == 3 .AND. day > 20)) then
                        reftime_year = year
                    else
                        reftime_year = year - 1
                    endif

                    reftime_month = 3
                    reftime_day = 20
                else ! Southern
                    if (month > 9 .OR. (month == 9 .AND. day > 22)) then
                        reftime_year = year
                    else
                        reftime_year = year - 1
                    endif

                    reftime_month = 9
                    reftime_day = 22
                endif
            elseif (Year_type == 'calendar') then
                found = .true.
                reftime_year = year - 1
                reftime_month = 12
                reftime_day = 31
            endif

            if (.not. found) then
                print *, 'ERROR, invalid year type argument to compute Julian Day: ', Year_type
                STOP
            endif

            ! set actual Julian Day
            absolute_julday = compute_julday(year, month, day)

            ! relative_julday = 0

            ! 2018-01-18 PAN: this conditional isn't needed
            ! if (Year_type == 'calendar') then
            !     relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)
            ! else
            !     relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)
            ! endif
            relative_julday = compute_julday(reftime_year, reftime_month, reftime_day)

            julian_day = absolute_julday - relative_julday

        end function julian_day


        !***********************************************************************
        ! Print date
        ! 2017-10-30 PAN: moved here from utils_prms.f90
        !***********************************************************************
        subroutine print_date(Flag)
            implicit none

            ! Arguments
            integer(i4), intent(in) :: Flag

            !***********************************************************************
            if (Flag == 1) then
                print 9001, Nowyear, Nowmonth, Nowday, Nowhour, Nowminute
            elseif (Flag == 0) then
                print 9001, Nowyear, Nowmonth, Nowday
            else
                WRITE (Flag, 9001) Nowyear, Nowmonth, Nowday
            endif

            9001 FORMAT ('    Date: ', I4, 2('/', I2.2), I3.2, ':', I2.2, /)
        end subroutine print_date

        !***********************************************************************
        ! Return the last day of the given month
        !***********************************************************************
        integer function last_day_of_month(mon)
            use time_mod, only: daypmo, leap_day
            implicit none

            integer(i4), intent(in) :: mon

            ! ===============================
            Yrdays = 365
            last_day_of_month = daypmo(mon)

            if (leap_day(Nowyear)) then
                Yrdays = Yrdays + 1

                if (mon == 2) last_day_of_month = last_day_of_month + 1
            endif
        end function last_day_of_month


        !***********************************************************************
        !***********************************************************************
        integer function prms_time(hemisphere, basin_area_inv, var_data)
            use prms_constants, only: FT2_PER_ACRE, SECS_PER_DAY, SECS_PER_HOUR, HOUR_PER_DAY, MIN_PER_HOUR
            use time_mod, only: compute_julday
            use PRMS_MODULE, only : Process, Timestep, Starttime, print_module
            use PRMS_DATA_FILE, only: read_data_line
            use variables_arr_mod, only: variables_arr_t
            implicit none

            ! Arguments
            integer(i4), intent(in) :: hemisphere
            real(r8), intent(in) :: basin_area_inv
            type(variables_arr_t), intent(inout) :: var_data

            ! Functions
            INTRINSIC SNGL

            ! Local Variables
            integer(i4) :: startday
            real(r8) :: dt
            character(len=80), save :: Version_prms_time

            !*******************************************************************
            ! 2018-01-18 PAN: prms_time is never changed in this function
            prms_time = 0

            if (Process == 'run' .OR. Process == 'init') then

                if (Process == 'run') then
                    Timestep = Timestep + 1

                    call dattim('now', Nowtime)
                    Jday = julian_day('now', 'calendar')
                    Jsol = julian_day('now', 'solar')
                    Julwater = julian_day('now', 'water')
                    Julian_day_absolute = Julian_day_absolute + 1
                    call read_data_line(Nowtime, var_data)

                else ! initialize
                    Nowtime = Starttime
                    Jday = julian_day('start', 'calendar')
                    Jsol = julian_day('start', 'solar')
                    Julwater = julian_day('start', 'water')
                    startday = compute_julday(Starttime(1), Starttime(2), Starttime(3))
                    Julian_day_absolute = startday
                endif

                Nowyear = Nowtime(1)
                Nowmonth = Nowtime(2)
                Nowday = Nowtime(3)
                Nowhour = Nowtime(4)
                Nowminute = Nowtime(5)

                ! Summer is based on equinox:
                !   Julian days 79 to 265 for Northern hemisphere
                !   Julian day 265 to 79 in Southern hemisphere
                Summer_flag = 1 ! 1 = summer, 0 = winter
                if (hemisphere == 0) then ! Northern Hemisphere
                    if (Jday < 79 .OR. Jday > 265) Summer_flag = 0 ! Equinox
                else ! Southern Hemisphere
                    if (Jday > 79 .AND. Jday < 265) Summer_flag = 0 ! Equinox
                endif

                dt = deltim()
                Timestep_hours = SNGL(dt)
                Timestep_days = Timestep_hours / HOUR_PER_DAY
                Timestep_minutes = Timestep_hours * MIN_PER_HOUR
                Timestep_seconds = dt * SECS_PER_HOUR
                Cfs_conv = FT2_PER_ACRE / 12.0D0 / Timestep_seconds
                Cfs2inches = basin_area_inv * 12.0D0 * Timestep_seconds / FT2_PER_ACRE

                ! Check to see if in a daily or subdaily time step
                if (Timestep_hours > HOUR_PER_DAY) then
                    print *, 'ERROR, timestep > daily, fix Data File, timestep:', Timestep_hours
                    STOP
                elseif (Timestep_hours < HOUR_PER_DAY) then
                    print *, 'ERROR, timestep < daily for daily model, fix Data File', Timestep_hours
                    STOP
                endif

            elseif (Process == 'declare') then
                Version_prms_time = 'prms_time.f90 2017-07-06 14:16:00Z'
                call print_module(Version_prms_time, 'PRMS Set Time Variables     ', 90)
                !        MODNAME = 'prms_time'
                Timestep_seconds = SECS_PER_DAY
                Cfs_conv = FT2_PER_ACRE / 12.0D0 / Timestep_seconds
                Cfs2inches = basin_area_inv * 12.0D0 * Timestep_seconds / FT2_PER_ACRE
            endif
        end function prms_time

end module PRMS_SET_TIME
