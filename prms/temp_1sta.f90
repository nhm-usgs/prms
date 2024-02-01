!***********************************************************************
! Distributes maximum, minimum, and average temperatures to each HRU
! using temperature data measured at one station and an estimated monthly lapse rate
!
! Variables needed from DATA FILE: tmax, tmin
!   Declared Parameters
!     tmax_lapse, tmin_lapse, tsta_elev, tmax_adj, tmin_adj, hru_type
!     hru_tsta, hru_elev, hru_area, temp_units, basin_tsta
!***********************************************************************
      MODULE PRMS_TEMP_1STA
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Temperature Distribution'
        character(len=*), parameter :: MODNAME = 'temp_1sta'
        character(len=*), parameter :: Version_temp = '2024-01-31'
        INTEGER, SAVE, ALLOCATABLE :: Tmax_cnt(:), Tmin_cnt(:), Nuse_tsta(:)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Elfac(:), Tmax_prev(:), Tmin_prev(:)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tcrn(:), Tcrx(:)
        DOUBLE PRECISION, SAVE :: Solrad_tmax_good, Solrad_tmin_good
        ! Declared Parameters
        INTEGER, SAVE :: Max_missing
        INTEGER, SAVE, ALLOCATABLE :: Hru_tsta(:)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tmax_lapse(:, :), Tmin_lapse(:, :)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tsta_elev(:)
      END MODULE PRMS_TEMP_1STA
!***********************************************************************
      INTEGER FUNCTION temp_1sta()
      USE PRMS_TEMP_1STA
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ACTIVE, OFF, &
          GLACIER, DEBUG_less, Nmonths, ERROR_temp, MINTEMP, MAXTEMP, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Ntemp, Print_debug, Init_vars_from_file, Save_vars_to_file, &
          Start_month, Glacier_flag, Nowmonth, Nowday, Nhru_nmonths, Parameter_check_flag, Inputerror_flag
      USE PRMS_BASIN, ONLY: Hru_elev_ts, Hru_area_dble, Active_hrus, Hru_route_order, Basin_area_inv, Hru_type, Hru_order_flag
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, Basin_tsta, &
          Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Tmax_allrain, Tmax_aspect_adjust, Tmin_aspect_adjust
      USE PRMS_OBS, ONLY: Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL :: read_error, temp_set, print_module, temp_1sta_restart, print_date, checkdim_bounded_limits
! Local Variables
      INTEGER :: j, k, jj, i, kk, kkk
      DOUBLE PRECISION :: tmx, tmn
      REAL, ALLOCATABLE :: Tsta_elev_sngl(:)
      REAL, ALLOCATABLE :: Tmax_lapse_sngl(:, :), Tmin_lapse_sngl(:, :)
!***********************************************************************
      temp_1sta = 0

      IF ( Process_flag == RUN ) THEN
        kk = 0
        kkk = 0
        DO i = 1, Ntemp
          IF ( Nuse_tsta(i)>0 ) THEN
            IF ( Tmax(i) < MINTEMP .OR. Tmax(i) > MAXTEMP ) THEN
              Tmax_cnt(i) = Tmax_cnt(i) + 1
              IF ( Tmax_cnt(i) < Max_missing ) THEN
                IF ( Print_debug > DEBUG_less ) THEN
                  PRINT 9001, 'tmax', Tmax(i), i, Tmax_prev(i)
                  CALL print_date(0)
                ENDIF
                Tmax(i) = Tmax_prev(i)
                kk = 1
              ELSE
                PRINT 9002, 'tmax', Tmax(i), i
                CALL print_date(0)
                ERROR STOP ERROR_temp
              ENDIF
            ELSE
              Tmax_prev(i) = Tmax(i)
              Tmax_cnt(i) = 0
            ENDIF
            IF ( Tmin(i) < MINTEMP .OR. Tmin(i) > MAXTEMP ) THEN
              Tmin_cnt(i) = Tmin_cnt(i) + 1
              IF ( Tmin_cnt(i) < Max_missing ) THEN
                IF ( Print_debug > DEBUG_less ) THEN
                  PRINT 9001, 'tmin', Tmin(i), i, Tmin_prev(i)
                  CALL print_date(0)
                ENDIF
                Tmin(i) = Tmin_prev(i)
                kkk = 1
              ELSE
                PRINT 9002, 'tmin', Tmin(i), i
                CALL print_date(0)
                ERROR STOP ERROR_temp
              ENDIF
            ELSE
              Tmin_prev(i) = Tmin(i)
              Tmin_cnt(i) = 0
            ENDIF
          ENDIF
        ENDDO
        ! if all values good, reset _cnt variable
        IF ( kk==0 ) Tmax_cnt = 0
        IF ( kkk==0 ) Tmin_cnt = 0

        Basin_tmax = 0.0D0
        Basin_tmin = 0.0D0
        Basin_temp = 0.0D0
        IF ( Hru_order_flag == OFF ) THEN
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            k = Hru_tsta(j)
            IF ( Nowday == 1 ) THEN
              IF ( Glacier_flag == 1 ) THEN
                ! Hru_elev_ts is the antecedent glacier elevation
                IF ( Hru_type(j)==GLACIER ) Elfac(j) = (Hru_elev_ts(j) - Tsta_elev(k)) / 1000.0D0
              ENDIF
              Tcrx(j) = Tmax_lapse(j, Nowmonth)*Elfac(j) - Tmax_aspect_adjust(j, Nowmonth)
              Tcrn(j) = Tmin_lapse(j, Nowmonth)*Elfac(j) - Tmin_aspect_adjust(j, Nowmonth)
            ENDIF
            tmx = Tmax(k) - Tcrx(j)
            tmn = Tmin(k) - Tcrn(j)
            CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j), Tmaxc(j), Tminc(j), Tavgc(j), Hru_area_dble(j))
          ENDDO
        ELSE
          DO j = 1, Nhru
            k = Hru_tsta(j)
            IF ( Nowday == 1 ) THEN
              IF ( Glacier_flag == 1 ) THEN
                ! Hru_elev_ts is the antecedent glacier elevation
                IF ( Hru_type(j)==GLACIER ) Elfac(j) = (Hru_elev_ts(j) - Tsta_elev(k)) / 1000.0D0
              ENDIF
              Tcrx(j) = Tmax_lapse(j, Nowmonth)*Elfac(j) - Tmax_aspect_adjust(j, Nowmonth)
              Tcrn(j) = Tmin_lapse(j, Nowmonth)*Elfac(j) - Tmin_aspect_adjust(j, Nowmonth)
            ENDIF
            tmx = Tmax(k) - Tcrx(j)
            tmn = Tmin(k) - Tcrn(j)
            CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j), Tmaxc(j), Tminc(j), Tavgc(j), Hru_area_dble(j))
          ENDDO
        ENDIF
        Basin_tmax = Basin_tmax * Basin_area_inv
        Basin_tmin = Basin_tmin * Basin_area_inv
        Basin_temp = Basin_temp * Basin_area_inv
        Solrad_tmax = Tmax(Basin_tsta)
        Solrad_tmin = Tmin(Basin_tsta)
        IF ( Solrad_tmax < MINTEMP .OR. Solrad_tmax > MAXTEMP ) THEN
          IF ( Print_debug > DEBUG_less ) THEN
            PRINT *, 'Bad temperature data to set solrad_tmax:', Solrad_tmax, ' using last valid value:', Solrad_tmax_good
            CALL print_date(0)
          ENDIF
          Solrad_tmax = Solrad_tmax_good
        ELSE
          Solrad_tmax_good = Solrad_tmax
        ENDIF
        IF ( Solrad_tmin<MINTEMP .OR. Solrad_tmin>MAXTEMP ) THEN
          IF ( Print_debug>DEBUG_less ) THEN
            PRINT *, 'Bad temperature data to set solrad_tmin:', Solrad_tmin, ' using last valid value:', Solrad_tmin_good
            CALL print_date(0)
          ENDIF
          Solrad_tmin = Solrad_tmin_good
        ELSE
          Solrad_tmin_good = Solrad_tmin
        ENDIF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_temp)

        ALLOCATE ( Elfac(Nhru), Nuse_tsta(Ntemp) )
        ALLOCATE ( Tmin_cnt(Ntemp), Tmax_cnt(Ntemp), Tmax_prev(Ntemp), Tmin_prev(Ntemp) )
        ALLOCATE ( Tcrn(Nhru), Tcrx(Nhru) )

        ALLOCATE ( Tmax_lapse(Nhru, Nmonths) )
        IF ( declparam(MODNAME, 'tmax_lapse', 'nhru,nmonths', 'real', &
             '3.0', '-20.0', '20.0', &
             'Monthly maximum temperature lapse rate for each HRU', &
             'Monthly (January to December) values representing the change in maximum air temperature per 1000 elev_units of'// &
             ' elevation change for each HRU', &
             'temp_units/elev_units')/=0 ) CALL read_error(1, 'tmax_lapse')
! 3 degC/ 1000 ft is adiabatic, or 9.8 degC/ 1000 m, or 5.4 degF/ 1000 ft, or 17.64 degF /1000 m

        ALLOCATE ( Tmin_lapse(Nhru, Nmonths) )
        IF ( declparam(MODNAME, 'tmin_lapse', 'nhru,nmonths', 'real', &
             '3.0', '-20.0', '20.0', &
             'Monthly minimum temperature lapse rate for each HRU', &
             'Monthly (January to December) values representing the change in minimum air temperture per 1000 elev_units of'// &
             ' elevation change for each HRU', &
             'temp_units/elev_units')/=0 ) CALL read_error(1, 'tmin_lapse')

        ALLOCATE ( Hru_tsta(Nhru) )
        IF ( declparam(MODNAME, 'hru_tsta', 'nhru', 'integer', &
             '0', 'bounded', 'ntemp', &
             'Index of base temperature station for each HRU', &
             'Index of the base temperature station used for lapse rate calculations', &
             'none') /= 0 ) CALL read_error(1, 'hru_tsta')

        ALLOCATE ( Tsta_elev(Ntemp) )
        IF ( declparam(MODNAME, 'tsta_elev', 'ntemp', 'real', &
             '0.0', '-300.0', '30000.0', &
             'Temperature station elevation', &
             'Elevation of each air-temperature-measurement station', &
             'elev_units') /= 0 ) CALL read_error(1, 'tsta_elev')

        IF ( declparam(MODNAME, 'max_missing', 'one', 'integer', &
             '3', '0', '10', &
             'Maximum number of consecutive missing values allowed for'// &
             ' any air-temperature-measurement station; 0=unlimited', &
             'Maximum number of consecutive missing values allowed for'// &
             ' any air-temperature-measurement station; missing value set'// &
             ' to last valid value; 0=unlimited', &
             'none')/=0 ) CALL read_error(1, 'max_missing')

      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL temp_1sta_restart(READ_INIT)

        ! Initialize variables, get parameter values, compute Elfac
        ALLOCATE ( Tmax_lapse_sngl(Nhru, Nmonths), Tmin_lapse_sngl(Nhru, Nmonths) )
        IF ( getparam(MODNAME, 'hru_tsta', Nhru, 'integer', Hru_tsta)/=0 ) CALL read_error(2, 'hru_tsta')
        IF ( Parameter_check_flag>0 ) &
             CALL checkdim_bounded_limits('hru_tsta', 'ntemp', Hru_tsta, Nhru, 0, Ntemp, Inputerror_flag)
        IF ( getparam(MODNAME, 'tmin_lapse', Nhru_nmonths, 'real', Tmin_lapse_sngl)/=0 ) CALL read_error(2, 'tmin_lapse')
        IF ( getparam(MODNAME, 'tmax_lapse', Nhru_nmonths, 'real', Tmax_lapse_sngl)/=0 ) CALL read_error(2, 'tmax_lapse')
        Tmax_lapse = DBLE( Tmax_lapse_sngl )
        Tmin_lapse = DBLE( Tmin_lapse_sngl )
        DEALLOCATE ( Tmax_lapse_sngl, Tmin_lapse_sngl )
        IF ( getparam(MODNAME, 'tsta_elev', Ntemp, 'real', Tsta_elev_sngl) /= 0 ) CALL read_error(2, 'tsta_elev')
        Tsta_elev = DBLE( Tsta_elev_sngl )
        DEALLOCATE ( Tsta_elev_sngl )
        IF ( getparam(MODNAME, 'max_missing', 1, 'integer', Max_missing)/=0 ) CALL read_error(2, 'max_missing')
        Max_missing = Max_missing + 1
        IF ( Inputerror_flag > 0 ) RETURN

        Nuse_tsta = 0
        Elfac = 0.0D0
        Tcrx = 0.0D0
        Tcrn = 0.0D0
        DO i = 1, Active_hrus
          j = Hru_route_order(i)
          k = Hru_tsta(j)
          Nuse_tsta(k) = 1
          ! Hru_elev_ts is the current elevation, either hru_elev or for restart Hru_elev_ts
          Elfac(j) = (Hru_elev_ts(j)-Tsta_elev(k)) / 1000.0D0
          Tcrx(j) = Tmax_lapse(j, Start_month)*Elfac(j) - Tmax_aspect_adjust(j, Start_month)
          Tcrn(j) = Tmin_lapse(j, Start_month)*Elfac(j) - Tmin_aspect_adjust(j, Start_month)
        ENDDO

        IF ( Init_vars_from_file==0 ) THEN
          Solrad_tmax_good = Solrad_tmax
          Solrad_tmin_good = Solrad_tmin
          Tmax_cnt = 0
          Tmin_cnt = 0

          DO i = 1, Ntemp
            Tmax_prev(i) = Tmax_allrain(1, Start_month)
          ENDDO
          Tmin_prev = Tmax_prev
        ENDIF

      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL temp_1sta_restart(SAVE_INIT)

      ENDIF

 9001 FORMAT ('WARNING, bad temperature, ', A, ':', F10.3, &
              '; temperature station: ', I0, /, 'Value set to last valid value:', F10.3)
 9002 FORMAT (/, 'ERROR, too many consecutive bad temperatures, ', A, ':', F10.3, /, &
              'temperature station: ', I0, /, &
              'Fix Data File or increase parameter max_missing' )

      END FUNCTION temp_1sta

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE temp_1sta_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, text_restart_flag
      USE PRMS_TEMP_1STA
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=9) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        IF ( text_restart_flag==OFF ) THEN
          WRITE ( Restart_outunit ) MODNAME
          WRITE ( Restart_outunit ) Solrad_tmax_good, Solrad_tmin_good
          WRITE ( Restart_outunit ) Tmax_cnt
          WRITE ( Restart_outunit ) Tmin_cnt
          WRITE ( Restart_outunit ) Tmax_prev
          WRITE ( Restart_outunit ) Tmin_prev
        ELSE
          WRITE ( Restart_outunit, * ) MODNAME
          WRITE ( Restart_outunit, * ) Solrad_tmax_good, Solrad_tmin_good
          WRITE ( Restart_outunit, * ) Tmax_cnt
          WRITE ( Restart_outunit, * ) Tmin_cnt
          WRITE ( Restart_outunit, * ) Tmax_prev
          WRITE ( Restart_outunit, * ) Tmin_prev
        ENDIF
      ELSE
        IF ( text_restart_flag==OFF ) THEN
          READ ( Restart_inunit ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit ) Solrad_tmax_good, Solrad_tmin_good
          READ ( Restart_inunit ) Tmax_cnt
          READ ( Restart_inunit ) Tmin_cnt
          READ ( Restart_inunit ) Tmax_prev
          READ ( Restart_inunit ) Tmin_prev
        ELSE
          READ ( Restart_inunit, * ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit, * ) Solrad_tmax_good, Solrad_tmin_good
          READ ( Restart_inunit, * ) Tmax_cnt
          READ ( Restart_inunit, * ) Tmin_cnt
          READ ( Restart_inunit, * ) Tmax_prev
          READ ( Restart_inunit, * ) Tmin_prev
        ENDIF
      ENDIF
      END SUBROUTINE temp_1sta_restart
