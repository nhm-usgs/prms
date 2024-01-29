!***********************************************************************
! Computes maximum and minimum monthly temperatures for each HRU and
! writes the parameters tmax_month_mean_warm and tmin_month_mean_warm
! to a file
!***********************************************************************
      SUBROUTINE temp_monthly_mean()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ERROR_open_out, Nmonths
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Start_month, Start_day, End_year, End_month, End_day, Nowyear, Nowmonth
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Tminc, Tmaxc
      USE PRMS_SET_TIME, ONLY: Modays
      IMPLICIT NONE
      ! Functions
      EXTERNAL :: print_module, write_2d_double_array, write_integer_array, write_double_array
      EXTERNAL :: error_stop, PRMS_open_output_file
      INTRINSIC :: DBLE
      ! Local Variables
      character(len=*), parameter :: MODDESC = 'Monthly Temperature Distribution'
      character(len=*), parameter :: MODNAME = 'temp_monthly_mean'
      character(len=*), parameter :: Version_temp_monthly_mean = '2024-01-25'
      INTEGER, SAVE :: start, last_month, years_month(Nmonths), startmonth, endyear, endmonth, monthly_unit
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: tmin_mon(:), tmax_mon(:), warm_temp(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: tmax_month(:, :), tmin_month(:, :)
      INTEGER :: i, j, mo, ios
      INTEGER, ALLOCATABLE :: warm_month(:)
      CHARACTER(LEN=40), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, "2")'
      CHARACTER(LEN=46), PARAMETER :: fmt2 = '("####", /, A, /, "2", /, A, /, A, /, I8, "2")'

!***********************************************************************
      IF ( Process_flag == RUN ) THEN
        IF ( start == 0 ) THEN
          IF ( Nowmonth /= startmonth ) RETURN
          start = 1
        ENDIF

        ! check to be sure last month of simulation is a full month
        IF ( Nowyear > endyear ) RETURN
        IF ( Nowyear == endyear ) THEN
          IF ( Nowmonth > endmonth ) RETURN
        ENDIF

        IF ( Nowmonth == last_month ) THEN
          ! add up temperatures for current month
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            tmax_mon(i) = tmax_mon(i) + Tmaxc(i)
            tmin_mon(i) = tmin_mon(i) + Tminc(i)
          ENDDO
        ELSE
          ! add up average temperatures for last month
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            tmax_month(i, last_month) = tmax_month(i, last_month) + tmax_mon(i)/DBLE(Modays(last_month))
            tmin_month(i, last_month) = tmin_month(i, last_month) + tmin_mon(i)/DBLE(Modays(last_month))
            tmax_mon(i) = Tmaxc(i)
            tmin_mon(i) = Tminc(i)
          ENDDO
          years_month(last_month) = years_month(last_month) + 1
          last_month = Nowmonth
        ENDIF

      ELSEIF ( Process_flag == DECL ) THEN
        CALL print_module( MODDESC, MODNAME, Version_temp_monthly_mean )
        CALL PRMS_open_output_file( monthly_unit, MODNAME//'.out', MODNAME, 0, ios )
        IF ( ios /= 0 ) CALL error_stop( 'in '//MODNAME, ERROR_open_out )
        ALLOCATE ( tmax_month(Nhru, Nmonths), tmin_month(Nhru, Nmonths) )
        ALLOCATE ( tmax_mon(Nhru), tmin_mon(Nhru) )

      ELSEIF ( Process_flag == INIT ) THEN
        tmax_month = 0.0D0
        tmin_month = 0.0D0
        tmax_mon = 0.0D0
        tmin_mon = 0.0D0
        years_month = 0

        startmonth = Start_month
        IF ( Start_day /= 1 ) THEN
          startmonth = startmonth + 1
          IF ( startmonth > Nmonths ) startmonth = 1
          start = 0
        ELSE
          start = 1
        ENDIF
        last_month = startmonth

        endmonth = End_month
        endyear = End_year
        IF ( End_day /= Modays(endmonth) ) THEN
          endmonth = endmonth - 1
          IF ( endmonth == 0 ) THEN
            endyear = endyear - 1
            endmonth = 12
          ENDIF
        ENDIF

      ELSEIF ( Process_flag == CLEAN ) THEN
        ! find warmest month for each HRU
        ALLOCATE ( warm_month(Nhru), warm_temp(Nhru) )
        warm_temp = -100.0D0
        warm_month = 1
        DO mo = 1, Nmonths
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            tmin_month(i, mo) = tmin_month(i, mo) / DBLE( years_month(mo) )
            tmax_month(i, mo) = tmax_month(i, mo) / DBLE( years_month(mo) )
            IF ( tmax_month(i, mo) > warm_temp(i) ) THEN
              warm_temp(i) = tmax_month(i, mo)
              warm_month(i) = mo
            ENDIF
          ENDDO
        ENDDO

        CALL write_2d_double_array( 'tmax_month_mean_warm_C', 'nhru', Nhru, 'nmonths', Nmonths, tmax_month, monthly_unit )
        CALL write_2d_double_array( 'tmin_month_mean_warm_C', 'nhru', Nhru, 'nmonths', Nmonths, tmin_month, monthly_unit )

        tmax_month = 1.0D0
        CALL write_2d_double_array ('jh_coef_vapor', 'nhru', Nhru, 'nmonths', Nmonths, tmax_month, monthly_unit )

        CALL write_integer_array( 'warm_month', 'nhru', Nhru, warm_month, monthly_unit )
        CALL write_double_array( 'warm_temp', 'nhru', Nhru, warm_temp, monthly_unit )

        DEALLOCATE ( warm_month, warm_temp, tmax_month, tmin_month )
      ENDIF

      END SUBROUTINE temp_monthly_mean
