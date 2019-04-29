!***********************************************************************
! Writes climate data (tmin, tmax, precip, potential solar radiation,
! and/or potential evapotranspieration) by HRU to files for use by
! the climate_hru module
!***********************************************************************
      INTEGER FUNCTION write_climate_hru()
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru, Temp_flag, Precip_flag, &
          Solrad_flag, Et_flag, Version_write_climate_hru, Write_climate_hru_nc, Transp_flag
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tminf, Hru_ppt, Potet, Swrad, Orad, Transp_on
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: control_string, get_ftnunit, declmodule
      EXTERNAL :: read_error
! Local Variables
      INTEGER, SAVE :: tmax_unit, tmin_unit, precip_unit, potet_unit, swrad_unit, transp_unit
      INTEGER :: i
      CHARACTER(LEN=128) :: tmin_day, tmax_day, precip_day, potet_day, swrad_day, transp_day
      CHARACTER(LEN=32), SAVE :: fmt1, fmt2
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='write_climate_hru')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Climate Distribuition')

!***********************************************************************
      write_climate_hru = 1

!***Run Procedure***
      IF ( Process(:3)=='run' ) THEN
        IF ( Temp_flag<7 ) THEN
          WRITE ( tmax_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tmaxf(i), i=1,Nhru)
          WRITE ( tmin_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tminf(i), i=1,Nhru)
        ENDIF
        IF ( Precip_flag<7 ) WRITE ( precip_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Hru_ppt(i), i=1,Nhru)
        IF ( Et_flag/=7 .AND. Et_flag/=8 ) WRITE ( potet_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Potet(i), i=1,Nhru)
        IF ( Solrad_flag<7 ) WRITE ( swrad_unit, fmt2 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Swrad(i), i=1,Nhru), Orad
        IF ( Transp_flag/=3 ) WRITE ( transp_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Transp_on(i), i=1,Nhru)

!***Declare Procedure***
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_write_climate_hru = '$Id: write_climate_hru.f90 4136 2012-01-27 17:46:18Z rsregan $'
        Write_climate_hru_nc = INDEX( Version_write_climate_hru, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(MODNAME, PROCNAME, Version_write_climate_hru(:Write_climate_hru_nc))/=0 ) STOP
        ENDIF

!***Initialize Procedure***
      ELSEIF ( Process(:4)=='init' ) THEN
        fmt1 = ' '
        fmt2 = ' '
        WRITE ( fmt1, 9003 ) Nhru
        WRITE ( fmt2, 9003 ) Nhru + 1
        IF ( Temp_flag/=7 .AND. Temp_flag/=8 ) THEN
          tmax_unit = get_ftnunit(640)
          IF ( control_string(tmax_day, 'tmax_day')/=0 ) CALL read_error(5, 'tmax_day')
          OPEN ( tmax_unit, FILE=tmax_day )
          WRITE ( tmax_unit, 9001 ) 'tmaxf', Nhru

          tmin_unit = get_ftnunit(tmax_unit)
          IF ( control_string(tmin_day, 'tmin_day')/=0 ) CALL read_error(5, 'tmin_day')
          OPEN ( tmin_unit, FILE=tmin_day )
          WRITE ( tmin_unit, 9001 ) 'tminf', Nhru
        ENDIF

        IF ( Precip_flag/=7 .AND. Precip_flag/=8 ) THEN
          precip_unit = get_ftnunit(643)
          IF ( control_string(precip_day, 'precip_day')/=0 ) CALL read_error(5, 'precip_day')
          OPEN ( precip_unit, FILE=precip_day )
          WRITE ( precip_unit, 9001 ) 'precip', Nhru
        ENDIF

        IF ( Et_flag/=7 .AND. Et_flag/=8 ) THEN
          potet_unit = get_ftnunit(644)
          IF ( control_string(potet_day, 'potet_day')/=0 ) CALL read_error(5, 'potet_day')
          OPEN ( potet_unit, FILE=potet_day )
          WRITE ( potet_unit, 9001 ) 'potet', Nhru
        ENDIF

        IF ( Transp_flag/=3 ) THEN
          transp_unit = get_ftnunit(646)
          IF ( control_string(transp_day, 'transp_day')/=0 ) CALL read_error(5, 'transp_day')
          OPEN ( transp_unit, FILE=transp_day )
          WRITE ( transp_unit, 9001 ) 'transp', Nhru
        ENDIF

        IF ( Solrad_flag/=7 .AND. Solrad_flag/=8 ) THEN
          swrad_unit = get_ftnunit(645)
          IF ( control_string(swrad_day, 'swrad_day')/=0 ) CALL read_error(5, 'swrad_day')
          OPEN ( swrad_unit, FILE=swrad_day )
          WRITE ( swrad_unit, 9002 ) 'swrad', Nhru
          PRINT *, '******'
          PRINT *, '******WARNING****** BE SURE TO ADD orad_flag = 1 in Control File'
          PRINT *, '******'
        ENDIF

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

      write_climate_hru = 0
      END FUNCTION write_climate_hru

