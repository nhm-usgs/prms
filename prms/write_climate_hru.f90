!***********************************************************************
! Writes climate data (tmin, tmax, precip, potential solar radiation,
! and/or potential evapotranspieration) by HRU to files for use by
! the climate_hru module
!***********************************************************************
      INTEGER FUNCTION write_climate_hru()
      USE PRMS_MODULE, ONLY: Process, Nhru, Temp_flag, Precip_flag, &
     &    Solrad_flag, Et_flag, Transp_flag, Tmin_day, Tmax_day, Precip_day, Potet_day, Swrad_day, Transp_day
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tminf, Hru_ppt, Potet, Swrad, Orad, Transp_on
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: control_string, declmodule
      EXTERNAL :: read_error, PRMS_open_output_file
! Local Variables
      INTEGER, SAVE :: tmax_unit, tmin_unit, precip_unit, potet_unit, swrad_unit, transp_unit
      INTEGER :: i, ios, ierr, nc
      CHARACTER(LEN=32), SAVE :: fmt1, fmt2, fmt3
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Climate Distribuition'
      CHARACTER(LEN=80), SAVE :: Version_write_climate_hru
!***********************************************************************
      write_climate_hru = 0

!***Run Procedure***
      IF ( Process(:3)=='run' ) THEN
        IF ( Temp_flag<7 ) THEN
          WRITE ( tmax_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tmaxf(i), i=1,Nhru)
          WRITE ( tmin_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tminf(i), i=1,Nhru)
        ENDIF
        IF ( Precip_flag<7 ) WRITE ( precip_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Hru_ppt(i), i=1,Nhru)
        IF ( Et_flag/=7 .AND. Et_flag/=8 ) WRITE ( potet_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Potet(i), i=1,Nhru)
        IF ( Solrad_flag<7 ) WRITE ( swrad_unit, fmt2 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Swrad(i), i=1,Nhru), Orad
        IF ( Transp_flag/=3 ) WRITE ( transp_unit, fmt3 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Transp_on(i), i=1,Nhru)

!***Declare Procedure***
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_write_climate_hru = '$Id: write_climate_hru.f90 5169 2012-12-28 23:51:03Z rsregan $'
        nc = INDEX( Version_write_climate_hru, 'Z' )
        i = INDEX( Version_write_climate_hru, '.f90' ) + 3
        IF ( declmodule(Version_write_climate_hru(6:i), PROCNAME, Version_write_climate_hru(i+2:nc))/=0 ) STOP

!***Initialize Procedure***
      ELSEIF ( Process(:4)=='init' ) THEN
        fmt1 = ' '
        fmt2 = ' '
        fmt2 = ' '
        WRITE ( fmt1, 9003 ) Nhru
        WRITE ( fmt2, 9003 ) Nhru + 1
        WRITE ( fmt3, 9004 ) Nhru
        ierr = 0
        IF ( Temp_flag/=7 .AND. Temp_flag/=8 ) THEN
          IF ( control_string(Tmax_day, 'tmax_day')/=0 ) CALL read_error(5, 'tmax_day')
          CALL PRMS_open_output_file(tmax_unit, Tmax_day, 'tmax_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( tmax_unit, 9001 ) 'tmaxf', Nhru
          ENDIF

          IF ( control_string(Tmin_day, 'tmin_day')/=0 ) CALL read_error(5, 'tmin_day')
          CALL PRMS_open_output_file(tmin_unit, Tmin_day, 'tmin_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( tmin_unit, 9001 ) 'tminf', Nhru
          ENDIF
        ENDIF

        IF ( Precip_flag/=7 .AND. Precip_flag/=8 ) THEN
          IF ( control_string(Precip_day, 'precip_day')/=0 ) CALL read_error(5, 'precip_day')
          CALL PRMS_open_output_file(precip_unit, Precip_day, 'precip_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( precip_unit, 9001 ) 'precip', Nhru
          ENDIF
        ENDIF

        IF ( Et_flag/=7 .AND. Et_flag/=8 ) THEN
          IF ( control_string(Potet_day, 'potet_day')/=0 ) CALL read_error(5, 'potet_day')
          CALL PRMS_open_output_file(potet_unit, Potet_day, 'potet_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( potet_unit, 9001 ) 'potet', Nhru
          ENDIF
        ENDIF

        IF ( Transp_flag/=3 ) THEN
          IF ( control_string(Transp_day, 'transp_day')/=0 ) CALL read_error(5, 'transp_day')
          CALL PRMS_open_output_file(transp_unit, Transp_day, 'transp_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( transp_unit, 9001 ) 'transp', Nhru
          ENDIF
        ENDIF

        IF ( Solrad_flag/=7 .AND. Solrad_flag/=8 ) THEN
          IF ( control_string(Swrad_day, 'swrad_day')/=0 ) CALL read_error(5, 'swrad_day')
          CALL PRMS_open_output_file(swrad_unit, Swrad_day, 'swrad_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( swrad_unit, 9002 ) 'swrad', Nhru
            PRINT *, '******'
            PRINT *, '******WARNING****** BE SURE TO ADD orad_flag = 1 in Control File'
            PRINT *, '******'
          ENDIF
        ENDIF
        
        IF ( ierr==1 ) STOP

!***Clean-up Procedure***
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Temp_flag/=7 .AND. Temp_flag/=8 ) THEN
          CLOSE ( tmax_unit )
          CLOSE ( tmin_unit )
        ENDIF
        IF ( Precip_flag/=7 .AND. Precip_flag/=8 ) CLOSE ( precip_unit )
        IF ( Et_flag/=7 .AND. Et_flag/=8 ) CLOSE ( potet_unit )
        IF ( Solrad_flag/=7 .AND. Solrad_flag/=8 ) CLOSE ( swrad_unit )
        IF ( Transp_flag/=3 ) CLOSE ( transp_unit )

      ENDIF

 9001 FORMAT ( 'Generated by write_climate_hru module', /, A, I8, /, 40('#') )
 9002 FORMAT ( 'Generated by write_climate_hru module', /, A, I8, /, 'orad 1', /, 40('#') )
 9003 FORMAT ( '(I4,2I3,3I2,',I8,'E10.2)' )
 9004 FORMAT ( '(I4,2I3,3I2,',I8,'I3)' )

      END FUNCTION write_climate_hru
