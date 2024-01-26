!***********************************************************************
! Sets PRMS time variables
!***********************************************************************
      MODULE PRMS_SET_TIME
        USE PRMS_CONSTANTS, ONLY: MONTHS_PER_YEAR
        IMPLICIT NONE
!   Local Variables
        character(len=*), parameter :: MODDESC = 'Timestep Control'
        character(len=*), parameter :: MODNAME = 'prms_time'
        character(len=*), parameter :: Version_prms_time = '2024-01-25'
        INTEGER, SAVE :: Modays(MONTHS_PER_YEAR), Yrdays, Summer_flag, Jday, Jsol, Julwater
        INTEGER, SAVE :: Nowtime(6), Nowhour, Nowminute, Julian_day_absolute
        DOUBLE PRECISION, save :: Timestep_hours, Timestep_days, Timestep_minutes, dt_save
        DOUBLE PRECISION, SAVE :: Cfs2inches, Cfs_conv, Timestep_seconds
        INTEGER, SAVE :: Storm_status, Storm_num, Newday, Storm_ts, Route_on
        INTEGER, SAVE :: Last_time, Last_day, Current_time
      END MODULE PRMS_SET_TIME

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION prms_time()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, YEAR, MONTH, DAY, HOUR, MINUTE, &
                                ACTIVE, OFF, NORTHERN, FT2_PER_ACRE, SECS_PER_HOUR, &
                                INCHES_PER_FOOT, SECS_PER_DAY, ERROR_time, &
                                CLEAN, DOCUMENTATION, SAVE_INIT, READ_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Timestep, Starttime, Nowyear, Nowmonth, Nowday, Dprst_flag, &
                             Storm_mode_flag, Storm_flag, Init_vars_from_file, Save_vars_to_file, Model, Hourly_flag
      USE PRMS_SET_TIME
      USE PRMS_BASIN, ONLY: Hemisphere, Basin_area_inv
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Ssres_stor, Basin_ssstor, &
                               Basin_soil_moist, Dprst_stor_hru, Hru_impervstor, Pkwater_equiv
      USE PRMS_IT0_VARS, ONLY: It0_soil_moist, It0_ssres_stor, &
                               It0_basin_ssstor, It0_basin_soil_moist, It0_dprst_stor_hru, &
                               It0_hru_impervstor, It0_pkwater_equiv
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: leap_day, julian_day, compute_julday, declvar
      DOUBLE PRECISION, EXTERNAL :: deltim
      EXTERNAL :: dattim, print_module, read_error, prms_time_restart
! Local Variables
      INTEGER :: startday
!***********************************************************************
      prms_time = 0

      IF ( Process_flag==RUN .OR. Process_flag==INIT ) THEN

        IF ( Process_flag==RUN ) THEN
          Timestep = Timestep + 1

          CALL dattim('now', Nowtime)
          Jday = julian_day('now', 'calendar')
          Jsol = julian_day('now', 'solar')
          Julwater = julian_day('now', 'water')
          Julian_day_absolute = Julian_day_absolute + 1
          It0_basin_soil_moist = Basin_soil_moist
          It0_basin_ssstor = Basin_ssstor
          It0_soil_moist = Soil_moist
          It0_ssres_stor = Ssres_stor
          It0_hru_impervstor = Hru_impervstor
          It0_pkwater_equiv = Pkwater_equiv
          IF ( Dprst_flag==ACTIVE ) It0_dprst_stor_hru = Dprst_stor_hru

        ELSE ! initialize
          IF ( Init_vars_from_file>OFF ) THEN
            CALL prms_time_restart(READ_INIT)
          ELSE
            Storm_status = 0
            Newday = 1
            Route_on = OFF
          ENDIF
          Modays(1) = 31
          Modays(3) = 31
          Modays(4) = 30
          Modays(5) = 31
          Modays(6) = 30
          Modays(7) = 31
          Modays(8) = 31
          Modays(9) = 30
          Modays(10) = 31
          Modays(11) = 30
          Modays(12) = 31

          Nowtime = Starttime
          Jday = julian_day('start', 'calendar')
          Jsol = julian_day('start', 'solar')
          Julwater = julian_day('start', 'water')
          startday = compute_julday(Starttime(YEAR), Starttime(MONTH), Starttime(DAY))
          Julian_day_absolute = startday
          Last_day = Nowtime(DAY)
          Last_time = Nowtime(YEAR)*10000000 + Jday*10000 + Nowtime(HOUR)*100 + Nowtime(MINUTE)
          Storm_num = 0
        ENDIF

        Nowyear = Nowtime(YEAR)
        Nowmonth = Nowtime(MONTH)
        Nowday = Nowtime(DAY)
        Nowhour = Nowtime(HOUR)
        Nowminute = Nowtime(MINUTE)

        Current_time = Nowyear*10000000 + Jday*10000 + Nowhour*100 + Nowminute
        IF ( Last_day /= Nowday ) THEN
          Newday = 1
          Last_day = Nowday
        ELSEIF ( Last_time /= Current_time ) THEN
          Newday = 0
        ENDIF
        Last_time = Current_time

        IF ( leap_day(Nowyear)==1 ) THEN
          Yrdays = 366
          Modays(2) = 29
        ELSE
          Yrdays = 365
          Modays(2) = 28
        ENDIF

        ! Summer is based on equinox:
        !   Julian days 79 to 265 for Northern hemisphere
        !   Julian day 265 to 79 in Southern hemisphere
        Summer_flag = ACTIVE ! 1 = summer, 0 = winter
        IF ( Hemisphere==NORTHERN ) THEN
          IF ( Jday<79 .OR. Jday>265 ) Summer_flag = OFF ! Equinox
        ELSE ! Southern Hemisphere
          IF ( Jday>79 .AND. Jday<265 ) Summer_flag = OFF ! Equinox
        ENDIF

        Timestep_hours = deltim()
        Timestep_days = Timestep_hours / 24.0D0
        Timestep_minutes = Timestep_hours * 60.0D0
        Timestep_seconds = Timestep_hours * 3600.0D0
        Cfs_conv = FT2_PER_ACRE/INCHES_PER_FOOT/Timestep_seconds
        Cfs2inches = Basin_area_inv*INCHES_PER_FOOT*Timestep_seconds/FT2_PER_ACRE

        ! Check to see if in a daily or subdaily time step
        Storm_flag = OFF
        IF ( Timestep_hours > 24.0D0 ) THEN
          PRINT *, 'ERROR, timestep > daily, fix Data File, timestep:', Timestep_hours
          ERROR STOP ERROR_time
        ELSEIF ( Timestep_hours < 24.0D0 ) THEN
          IF ( Storm_mode_flag == OFF ) THEN
            PRINT *, 'ERROR, timestep < daily and model_mode not specified as STORM or HOURLY', Timestep_hours
            ERROR STOP ERROR_time
          ELSEIF ( Hourly_flag == ACTIVE .AND. Timestep_hours /= 1.0D0 ) THEN
            PRINT *, 'ERROR, timestep not hourly and model_mode specified as HOURLY', Timestep_hours
            ERROR STOP ERROR_time
          ENDIF
          Storm_flag = ACTIVE
        ENDIF

        IF ( Storm_mode_flag == ACTIVE ) THEN ! Storm mode
          IF ( Storm_flag == ACTIVE ) THEN
            IF ( Timestep_hours /= dt_save ) THEN
              PRINT *, 'ERROR, timestep can not change within a storm, old:', dt_save, '; new:', Timestep_hours
              ERROR STOP ERROR_time
            ENDIF
            dt_save = Timestep_hours
            ! storm_status 1=new storm, 2=same storm, 3=storm ended
            IF ( Route_on==OFF ) THEN
              Storm_status = 1
              Storm_ts = 1
              Storm_num = Storm_num + 1
            ELSE
              Storm_status = 2
              IF ( Newday == 0 ) THEN
                Storm_ts = Storm_ts + 1
              ELSE
                Storm_ts = 1
              ENDIF
            ENDIF
            Route_on = ACTIVE
          ELSE
            IF ( Route_on == ACTIVE ) Storm_status = 3
            Route_on = OFF
          ENDIF
          IF ( Storm_status == ACTIVE ) Storm_num = Storm_num + 1
        ENDIF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_prms_time)
        Timestep_seconds = SECS_PER_DAY
        Cfs_conv = FT2_PER_ACRE/INCHES_PER_FOOT/Timestep_seconds
        Cfs2inches = Basin_area_inv*INCHES_PER_FOOT*Timestep_seconds/FT2_PER_ACRE
      ! Declared variables for Storm Mode
        IF ( Storm_mode_flag == ACTIVE .OR. Model == DOCUMENTATION ) THEN
          IF ( declvar(MODNAME, 'route_on', 'one', 1, 'integer', &
     &         'Kinematic routing switch (0=daily; 1=storm period)', &
     &         'none', Route_on) /= 0 ) CALL read_error( 8, 'route_on' )
          IF ( declvar(MODNAME, 'storm_status', 'one', 1, 'integer', &
     &         'Switch signifying storm status (0=not in storm;'// &
     &         ' 1=first time step of storm; 2=middle of storm; 3=storm end)', &
       &         'none', Storm_status) /= 0 ) CALL read_error( 8, 'storm_status' )
          dt_save = deltim()
        ENDIF
      ELSEIF ( Process_flag == CLEAN ) THEN
        IF ( Save_vars_to_file == ACTIVE ) CALL prms_time_restart( SAVE_INIT )
      ENDIF

      END FUNCTION prms_time

!***********************************************************************
!     write or read to restart file
!***********************************************************************
      SUBROUTINE prms_time_restart( In_out )
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_SET_TIME, ONLY: MODNAME, Storm_status, Newday, Route_on
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=10) :: module_name
!***********************************************************************
      IF ( In_out == SAVE_INIT ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Storm_status, Newday, Route_on
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Storm_status, Newday, Route_on
      ENDIF
      END SUBROUTINE prms_time_restart
