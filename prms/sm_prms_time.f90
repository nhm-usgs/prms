submodule(PRMS_SET_TIME) sm_prms_time

contains
!***********************************************************************
!***********************************************************************
  integer module function prms_time()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, MAX_DAYS_PER_YEAR, DAYS_PER_YEAR, &
          ACTIVE, OFF, NORTHERN, FT2_PER_ACRE, SECS_PER_HOUR, INCHES_PER_FOOT, SECS_PER_DAY, ERROR_time, &
          CLEAN, STORM, READ_INIT, SAVE_INIT, DOCUMENTATION
      use PRMS_MMFAPI, only: dattim, deltim
      USE PRMS_MODULE, ONLY: Process_flag, Timestep, Starttime, Nowyear, Nowmonth, Nowday, Dprst_flag, &
                             AG_flag, Ag_gravity_flag, Init_vars_from_file, Save_vars_to_file, Model, Start_day, Start_year
      USE PRMS_BASIN, ONLY: Hemisphere, Basin_area_inv
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Soil_rechr, Pkwater_equiv, Hru_intcpstor, &
                               Ssres_stor, Slow_stor, Pref_flow_stor, Basin_intcp_stor, Basin_ssstor, &
                               Basin_soil_moist, Basin_gwstor, Dprst_stor_hru, &
                               Ag_soil_moist, Ag_soil_rechr, Ag_gvr_stor, Imperv_stor, &
                               Dprst_vol_open, Dprst_vol_clos, Hru_impervstor, Basin_ag_gvr_stor, &
                               Basin_ag_soil_moist, Basin_ag_soil_rechr
      USE PRMS_IT0_VARS, ONLY: It0_soil_moist, It0_soil_rechr, It0_pkwater_equiv, &
                               It0_hru_intcpstor, It0_ssres_stor, It0_slow_stor, It0_pref_flow_stor, &
                               It0_basin_intcp_stor, It0_basin_ssstor, It0_basin_soil_moist, It0_basin_gwstor, &
                               It0_dprst_stor_hru, It0_ag_soil_moist, It0_ag_soil_rechr, It0_imperv_stor, It0_ag_gvr_stor, &
                               It0_hru_impervstor, It0_dprst_vol_open, It0_dprst_vol_clos, It0_basin_ag_gvr_stor, &
                               It0_basin_ag_soil_moist, It0_basin_ag_soil_rechr
      use prms_utils, only: leap_day, julian_day, compute_julday, print_module, read_error
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: SNGL
      EXTERNAL :: prms_time_restart
	  ! Declared Variables
	  INTEGER, SAVE :: Subdaily_status, Newday, Subdaily_flag
      ! Local Variables
      INTEGER :: startday
      DOUBLE PRECISION :: dt
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
          It0_basin_gwstor = Basin_gwstor
          It0_basin_intcp_stor = Basin_intcp_stor
          It0_soil_moist = Soil_moist
          It0_soil_rechr = Soil_rechr
          It0_ssres_stor = Ssres_stor
          It0_slow_stor = Slow_stor
          It0_pref_flow_stor = Pref_flow_stor
          It0_pkwater_equiv = Pkwater_equiv
          It0_imperv_stor = Imperv_stor
          It0_hru_impervstor = Hru_impervstor
          It0_hru_intcpstor = Hru_intcpstor
          IF ( Dprst_flag==ACTIVE ) THEN
            It0_dprst_vol_open = Dprst_vol_open
            It0_dprst_vol_clos = Dprst_vol_clos
            It0_dprst_stor_hru = Dprst_stor_hru
          ENDIF
          IF ( AG_flag==ACTIVE ) THEN
            It0_basin_ag_gvr_stor = Basin_ag_gvr_stor
            It0_basin_ag_soil_moist = Basin_ag_soil_moist
            It0_basin_ag_soil_rechr = Basin_ag_soil_rechr
            It0_ag_soil_moist = Ag_soil_moist
            It0_ag_soil_rechr = Ag_soil_rechr
            IF ( Ag_gravity_flag==ACTIVE ) It0_ag_gvr_stor = Ag_gvr_stor
          ENDIF

        ELSE ! initialize
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

          IF ( Init_vars_from_file > 0 ) THEN
            CALL prms_time_restart(READ_INIT)
          ELSE
            ! Storm variables
            Subdaily_flag = OFF
            Newday = ACTIVE
            Subdaily_status = 0
          ENDIF
          Previous_day = Start_day
          Last_time = Start_year*10000000 + Jday*10000 + Starttime(HOUR)*100 + Starttime(MINUTE)
          Subdaily_num = 0
        ENDIF

        Nowyear = Nowtime(YEAR)
        Nowmonth = Nowtime(MONTH)
        Nowday = Nowtime(DAY)
        Nowhour = Nowtime(HOUR)
        Nowminute = Nowtime(MINUTE)
        Nowsecond = Nowtime(SECOND)

        IF ( leap_day(Nowyear)==1 ) THEN
          Yrdays = MAX_DAYS_PER_YEAR
          Modays(2) = 29
        ELSE
          Yrdays = DAYS_PER_YEAR
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

        dt = deltim()
        Timestep_hours = SNGL( dt )
        Timestep_days = Timestep_hours/24.0
        Timestep_minutes = Timestep_hours*60.0
        Timestep_seconds = dt*SECS_PER_HOUR
        Cfs_conv = FT2_PER_ACRE/INCHES_PER_FOOT/Timestep_seconds
        Cfs2inches = Basin_area_inv*INCHES_PER_FOOT*Timestep_seconds/FT2_PER_ACRE

        IF ( Model == STORM ) THEN ! Storm mode
          Current_time = Nowyear*10000000 + Jday*10000 + Nowhour*100 + Nowminute
          IF ( Previous_day /= Nowday ) THEN
            Newday = ACTIVE
            Previous_day = Nowday
          ELSEIF ( Last_time /= Current_time ) THEN
            Newday = OFF
          ENDIF
          Last_time = Current_time

          Subdaily_status = 0
          IF ( Timestep_hours<23.999 ) THEN
            ! Subdaily_status 1=new storm, 2=same storm, 3=storm ended
            IF ( Subdaily_flag == OFF ) THEN
              Subdaily_status = 1
              Subdaily_flag = ACTIVE
              Subdaily_num = Subdaily_num + 1
            ELSE
              Subdaily_status = 2
            ENDIF
          ELSE
            IF ( Subdaily_flag == ACTIVE ) Subdaily_status = 3
            Subdaily_flag = OFF
          ENDIF
        ELSEIF ( Timestep_hours < 24.0 ) THEN
          PRINT *, 'ERROR, timestep < daily for daily model, fix Data File', Timestep_hours
          ERROR STOP ERROR_time
        ENDIF

        ! Check to see if in a daily or subdaily time step
        IF ( Timestep_hours>24.0 ) THEN
          PRINT *, 'ERROR, timestep > daily, fix Data File, timestep:', Timestep_hours
          ERROR STOP ERROR_time
!        ELSEIF ( Timestep_hours<24.0 ) THEN
!          PRINT *, 'ERROR, timestep < daily for daily model, fix Data File', Timestep_hours
!          ERROR STOP ERROR_time
        ENDIF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_prms_time)
        Timestep_seconds = SECS_PER_DAY
        Cfs_conv = FT2_PER_ACRE/INCHES_PER_FOOT/Timestep_seconds
        Cfs2inches = Basin_area_inv*INCHES_PER_FOOT*Timestep_seconds/FT2_PER_ACRE

        ! Declared variables for Storm Mode
        IF ( MODEL==STORM .OR. Model==DOCUMENTATION ) THEN
          IF ( declvar(MODNAME, 'subdaily_flag', 'one', 1, 'integer', &
     &         'Subdaily computation switch (0=daily; 1=storm period)', &
     &         'none', Subdaily_flag)/=0 ) CALL read_error(8, 'subdaily_flag')
          IF ( declvar(MODNAME, 'newday', 'one', 1, 'integer', &
     &         'Switch signifying the start of a new day for storm mode (0=no; 1=yes)', &
     &         'none', Newday)/=0 ) CALL read_error(8, 'newday')
          IF ( declvar(MODNAME, 'subdaily_status', 'one', 1, 'integer', &
     &         'Switch signifying timestep status (0=daily; 1=start subdaily; 2=continue subdaily; 3=end subdaily)', &
     &         'none', Subdaily_status)/=0 ) CALL read_error(8, 'subdaily_status')
        ENDIF

      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL prms_time_restart(SAVE_INIT)
      ENDIF

      END FUNCTION prms_time

!***********************************************************************
!     write or read to restart file
!***********************************************************************
      SUBROUTINE prms_time_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_SET_TIME
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=10) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Subdaily_status, Newday, Subdaily_flag
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Subdaily_status, Newday, Subdaily_flag
      ENDIF
      END SUBROUTINE prms_time_restart

end submodule
