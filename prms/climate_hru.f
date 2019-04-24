!***********************************************************************
! Read and makes available climate data (tmin, tmax, precip, potential
! solar radiation, and/or potential evapotranspieration) by HRU from
! files pre-processed Data Files available for other PRMS modules
!***********************************************************************
      INTEGER FUNCTION climate_hru()
      USE PRMS_MODULE, ONLY: Process, Nhru, Nsub, Precip_flag,
     +    Print_debug, Solrad_flag, Et_flag, Temp_flag, Subbasin_flag,
     +    Version_climateflow, Climate_hru_nc
      USE PRMS_BASIN, ONLY: Starttime
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv, NEARZERO, MM2INCH
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp,
     +    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf,
     +    Tavgc, Hru_ppt, Hru_rain, Hru_snow, Prmx, Pptmix, Newsnow,
     +    Tmax_adj, Tmin_adj, Tmax_allrain_f, Adjmix_rain, Precip_units,
     +    Basin_ppt, Basin_potet, Potet, Basin_snow, Basin_rain,
     +    Basin_horad, Orad, Swrad, Basin_potsw, Basin_obs_ppt, Ntemp,
     +    Basin_tsta
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Jday, Nowtime,
     +    Tmax, Tmin
      USE PRMS_SOLTAB, ONLY: Soltab_basinpotsw, Hru_cossl, Soltab_potsw
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, control_integer
      INTEGER, EXTERNAL :: getparam, control_string, get_ftnunit
      EXTERNAL read_error, precip_form, temp_set
! Declared Parameters
      INTEGER, SAVE :: Adj_by_hru, Orad_flag
      INTEGER, SAVE, ALLOCATABLE :: Hru_subbasin(:)
      REAL, SAVE, ALLOCATABLE :: Rain_sub_adj(:, :), Snow_sub_adj(:, :)
      REAL, SAVE, ALLOCATABLE :: Rain_adj(:, :), Snow_adj(:, :)
! Local Variables
      INTEGER, SAVE :: precip_unit, tmax_unit, tmin_unit, et_unit
      INTEGER, SAVE :: swrad_unit
      INTEGER, SAVE, ALLOCATABLE :: istack(:)
      INTEGER :: year, month, day, hour, yr, mo, dy, i
      INTEGER :: hr, mn, sec, jj
      DOUBLE PRECISION :: sum_obs
      REAL :: rainadj, snowadj, tmax_hru, tmin_hru, ppt
      CHARACTER(LEN=4) :: dum
      CHARACTER(LEN=128) :: tmin_day, tmax_day, precip_day
      CHARACTER(LEN=128) :: potet_day, swrad_day
!***********************************************************************
      climate_hru = 1

      IF ( Process(:3)=='run' ) THEN
        IF ( Temp_flag==7 ) THEN
          READ ( tmax_unit, * ) yr, mo, dy, hr, mn, sec,
     +                          (Tmaxf(i), i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Tmaxf', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          READ ( tmin_unit, * ) yr, mo, dy, hr, mn, sec,
     +                          (Tminf(i), i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Tminf', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_tmax = 0.0D0
          Basin_tmin = 0.0D0
          Basin_temp = 0.0D0
        ENDIF

        IF ( Precip_flag==7 ) THEN
          READ ( precip_unit, * ) yr, mo, dy, hr, mn, sec,
     +                            (Hru_ppt(i), i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Hru_ppt', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_ppt = 0.0D0
          Basin_rain = 0.0D0
          Basin_snow = 0.0D0
          istack = 0
          sum_obs = 0.0D0
        ENDIF

        IF ( Et_flag==0 ) THEN
          READ ( et_unit, * ) yr, mo, dy, hr, mn, sec,
     +                        (Potet(i),i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Potet', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_potet = 0.0D0
        ENDIF

        IF ( Solrad_flag==0 ) THEN
          IF ( Orad_flag==0 ) THEN
            READ ( swrad_unit, * ) yr, mo, dy, hr, mn, sec,
     +                             (Swrad(i), i=1,Nhru)
          ELSEIF ( Orad_flag==1 ) THEN
            READ ( swrad_unit, * ) yr, mo, dy, hr, mn, sec,
     +                             (Swrad(i), i=1,Nhru), Orad
          ENDIF
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Swrad', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_potsw = 0.0D0
        ENDIF

        DO jj = 1, Active_hrus
          i = Hru_route_order(jj)
          IF ( Temp_flag==7 ) THEN
            tmax_hru = Tmaxf(i) + Tmax_adj(i)
            tmin_hru = Tminf(i) + Tmin_adj(i)
            CALL temp_set(i, tmax_hru, tmin_hru, Tmaxf(i), Tminf(i),
     +                    Tavgf(i), Tmaxc(i), Tminc(i), Tavgc(i),
     +                    Hru_area(i))
          ENDIF
          IF ( Et_flag==0 ) Basin_potet = Basin_potet
     +                                    + Potet(i)*Hru_area(i)
          IF ( Solrad_flag==0 ) Basin_potsw = Basin_potsw
     +                                        + Swrad(i)*Hru_area(i)

          IF ( Precip_flag==7 ) THEN
            IF ( Hru_ppt(i)<0.0 ) THEN
              IF ( istack(i).EQ.0 ) THEN
                PRINT 9002, Hru_ppt(i), i, Nowtime
                istack(i) = 1
              ENDIF
            ENDIF

!******Initialize HRU variables
            Pptmix(i) = 0
            Newsnow(i) = 0
            Prmx(i) = 0.0
            Hru_rain(i) = 0.0
            Hru_snow(i) = 0.0

            ! ignore very small amounts of precipitation
            IF ( Hru_ppt(i)<NEARZERO ) THEN
              Hru_ppt(i) = 0.0
              CYCLE
            ENDIF
            IF ( Precip_units==1 ) Hru_ppt(i) = Hru_ppt(i)*MM2INCH
            IF ( Adj_by_hru==0 ) THEN
              snowadj = Snow_sub_adj(Hru_subbasin(i), mo)
              rainadj = Rain_sub_adj(Hru_subbasin(i), mo)
            ELSE
              snowadj = Snow_adj(i, mo)
              rainadj = Rain_adj(i, mo)
            ENDIF
            ppt = Hru_ppt(i)
            CALL precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i),
     +                       Tmaxf(i), Tminf(i), Pptmix(i), Newsnow(i),
     +                       Prmx(i), Tmax_allrain_f(Nowmonth), rainadj,
     +                       snowadj, Adjmix_rain(Nowmonth),
     +                       Hru_area(i), sum_obs)
          ENDIF
        ENDDO

        IF ( Temp_flag==7 ) THEN
          Basin_tmax = Basin_tmax*Basin_area_inv
          Basin_tmin = Basin_tmin*Basin_area_inv
          Basin_temp = Basin_temp*Basin_area_inv
          IF ( Ntemp>0 ) THEN
            Solrad_tmax = Tmax(Basin_tsta)
            Solrad_tmin = Tmin(Basin_tsta)
          ELSE
            Solrad_tmax = Basin_tmax
            Solrad_tmin = Basin_tmin
          ENDIF
        ENDIF

        IF ( Precip_flag==7 ) THEN
          Basin_ppt = Basin_ppt*Basin_area_inv
          Basin_obs_ppt = sum_obs*Basin_area_inv
          Basin_rain = Basin_rain*Basin_area_inv
          Basin_snow = Basin_snow*Basin_area_inv
        ENDIF

        IF ( Et_flag==0 ) Basin_potet = Basin_potet*Basin_area_inv

        IF ( Solrad_flag==0 ) THEN
          Basin_horad = Soltab_basinpotsw(Jday)
          IF ( Orad_flag==0 ) Orad =
     +         (Swrad(1)*Hru_cossl(1)*Basin_horad)/Soltab_potsw(Jday,1)
          Basin_potsw = Basin_potsw*Basin_area_inv
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_climateflow =
     +'$Id: climate_hru.f 3816 2011-10-25 18:43:07Z rsregan $'
        Climate_hru_nc = INDEX( Version_climateflow, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_climateflow(:Climate_hru_nc))/=0 )STOP
        ENDIF

!   Declared Parameters
        IF ( Precip_flag==7 ) THEN
          IF ( declparam('climhru', 'adj_by_hru', 'one', 'integer',
     +         '1', '0', '1',
     +         'Adjust precipitation by HRU or subbasin'//
     +         ' (0=subbasin; 1=HRU)',
     +         'Flag to indicate whether to adjust precipitation and'//
     +         ' air temperature by HRU or subbasin'//
     +         ' (0=subbasin; 1=HRU)',
     +         'none')/=0 ) CALL read_error(1, 'adj_by_hru')

          IF ( Subbasin_flag==1 ) THEN
            ALLOCATE ( Hru_subbasin(Nhru) )
            IF ( declparam('climhru', 'hru_subbasin', 'nhru', 'integer',
     +           '0', 'bounded', 'nsub',
     +           'Index of subbasin assigned to each HRU',
     +           'Index of subbasin assigned to each HRU',
     +           'none')/=0 ) CALL read_error(1, 'hru_subbasin')
            ALLOCATE ( Rain_sub_adj(Nsub,12) )
            IF ( declparam('climhru', 'rain_sub_adj', 'nsub,nmonths',
     +           'real', '1.0', '0.0', '4.0',
     +           'Rain adjustment factor for each subbasin and month',
     +           'Monthly (January to December) rain adjustment'//
     +           ' factor to measured precipitation for each subbasin',
     +           'decimal fraction')/=0 )
     +           CALL read_error(1, 'rain_sub_adj')
            ALLOCATE ( Snow_sub_adj(Nsub,12) )
            IF ( declparam('climhru', 'snow_sub_adj', 'nsub,nmonths',
     +           'real', '1.0', '0.0', '4.0',
     +           'Snow adjustment factor for each subbasin and month',
     +           'Monthly (January to December) snow adjustment'//
     +           ' factor to measured precipitation for each subbasin',
     +           'decimal fraction')/=0 )
     +           CALL read_error(1, 'snow_sub_adj')
          ENDIF

          ALLOCATE ( Rain_adj(Nhru,12) )
          IF ( declparam('climhru', 'rain_adj', 'nhru,nmonths', 'real',
     +         '1.0', '0.2', '5.0',
     +         'Rain adjustment factor, by month for each HRU',
     +         'Monthly (January to December) adjustment factor to'//
     +         ' measured precipitation on each HRU to account for'//
     +         ' differences in elevation, etc.',
     +         'decimal fraction')/=0 ) CALL read_error(1, 'rain_adj')
          ALLOCATE ( Snow_adj(Nhru,12) )
          IF ( declparam('climhru', 'snow_adj', 'nhru,nmonths', 'real',
     +         '1.0', '0.2', '5.0',
     +         'Snow adjustment factor, by month for each HRU',
     +         'Monthly (January to December) adjustment factor to'//
     +         ' measured precipitation on each HRU to account for'//
     +         ' differences in elevation, etc.',
     +         'decimal fraction')/=0 ) CALL read_error(1, 'snow_adj')
        ENDIF

        IF ( Solrad_flag==0 ) THEN
          IF ( control_integer(Orad_flag, 'orad_flag')/=0 )
     +         Orad_flag = 0
        ENDIF

      ELSEIF ( Process(:4)=='init' ) THEN
        year = Starttime(1)
        month = Starttime(2)
        day = Starttime(3)
        hour = Starttime(4)

        IF ( Precip_flag==7 ) THEN
          ALLOCATE ( istack(Nhru) )
          IF ( getparam('climhru', 'adj_by_hru', 1, 'integer',
     +         Adj_by_hru)/=0 ) CALL read_error(2, 'adj_by_hru')

          IF ( Adj_by_hru==0 ) THEN
            IF ( getparam('climhru', 'hru_subbasin', Nhru, 'integer',
     +           Hru_subbasin)/=0 ) CALL read_error(2, 'hru_subbasin')
            IF ( getparam('climhru', 'rain_sub_adj', Nsub*12, 'real',
     +           Rain_sub_adj)/=0 ) CALL read_error(2, 'rain_sub_adj')
            IF ( getparam('climhru', 'snow_sub_adj', Nsub*12, 'real',
     +           Snow_sub_adj)/=0 ) CALL read_error(2, 'snow_sub_adj')
          ELSE
            IF ( getparam('climhru', 'rain_adj', Nhru*12, 'real',
     +           Rain_adj)/=0 ) CALL read_error(2, 'rain_adj')
            IF ( getparam('climhru', 'snow_adj', Nhru*12, 'real',
     +         Snow_adj)/=0 ) CALL read_error(2, 'snow_adj')
          ENDIF
          IF ( control_string(precip_day, 'precip_day')/=0 )
     +         CALL read_error(5, 'precip_day')
          precip_unit = get_ftnunit(220)
          OPEN ( precip_unit, FILE=precip_day, STATUS='OLD' )
! read to line before data starts in each file
          i = 0
          DO WHILE ( i==0 )
            READ ( precip_unit, FMT='(A4)' ) dum
            IF ( dum=='####' ) i = 1
          ENDDO
! find first value for simulation time period
          i = 0
          DO WHILE ( i==0 )
            READ ( precip_unit, * ) yr, mo, dy
            IF ( yr==year .AND. mo==month .AND. dy==day ) i = 1
          ENDDO
          BACKSPACE precip_unit
        ENDIF

        IF ( Temp_flag==7 ) THEN
          IF ( control_string(tmax_day, 'tmax_day')/=0 )
     +         CALL read_error(5, 'tmax_day')
          IF ( control_string(tmin_day, 'tmin_day')/=0 )
     +         CALL read_error(5, 'tmin_day')
          tmax_unit = get_ftnunit(221)
          tmin_unit = get_ftnunit(tmax_unit)
          OPEN ( tmax_unit, FILE=tmax_day, STATUS='OLD' )
          OPEN ( tmin_unit, FILE=tmin_day, STATUS='OLD' )
! read to line before data starts in each file
          i = 0
          DO WHILE ( i==0 )
            READ ( tmax_unit, FMT='(A4)' ) dum
            IF ( dum=='####' ) i = 1
          ENDDO
          i = 0
          DO WHILE ( i==0 )
            READ ( tmin_unit, FMT='(A4)' ) dum
            IF ( dum=='####' ) i = 1
          ENDDO
! find first value for simulation time period
          i = 0
          DO WHILE ( i==0 )
            READ ( tmax_unit, * ) yr, mo, dy
            IF ( yr==year .AND. mo==month .AND. dy==day ) i = 1
          ENDDO
          BACKSPACE tmax_unit
          i = 0
          DO WHILE ( i==0 )
            READ ( tmin_unit, * ) yr, mo, dy
            IF ( yr==year .AND. mo==month .AND. dy==day ) i = 1
          ENDDO
          BACKSPACE tmin_unit
        ENDIF

        IF ( Et_flag==0 ) THEN
          IF ( control_string(potet_day, 'potet_day')/=0 )
     +         CALL read_error(5, 'potet_day')
          et_unit = get_ftnunit(224)
          OPEN ( et_unit, FILE=potet_day, STATUS='OLD' )
          i = 0
          DO WHILE ( i==0 )
            READ ( et_unit, FMT='(A4)' ) dum
            IF ( dum=='####' ) i = 1
          ENDDO
          i = 0
          DO WHILE ( i==0 )
          READ ( et_unit, * ) yr, mo, dy
            IF ( yr==year .AND. mo==month .AND. dy==day ) i = 1
          ENDDO
          BACKSPACE et_unit
        ENDIF

        IF ( Solrad_flag==0 ) THEN
          IF ( control_string(swrad_day, 'swrad_day')/=0 )
     +         CALL read_error(5, 'swrad_day')
          swrad_unit = get_ftnunit(225)
          OPEN ( swrad_unit, FILE=swrad_day, STATUS='OLD' )
          i = 0
          DO WHILE ( i==0 )
            READ ( swrad_unit, FMT='(A4)' ) dum
            IF ( dum=='####' ) i = 1
          ENDDO
          i = 0
          DO WHILE ( i==0 )
            READ ( swrad_unit, * ) yr, mo, dy
            IF ( yr==year .AND. mo==month .AND. dy==day ) i = 1
          ENDDO
          BACKSPACE swrad_unit
        ENDIF
      ENDIF

 9001 FORMAT ( 'ERROR, problem reading daily HRU', A, ' file', /,
     +         'Timestep:', I5.4, 2('/',I2.2), /, 'File time:', I5.4,
     +         2('/',I2.2),/ )
 9002 FORMAT ( 'Warning, bad precipitation value:', F10.3,
     +         '; HRU:', I6, '; Time:', I5, 2('/', I2.2), I3,
     +         2(':', I2.2), '; value set to 0.0' )

      climate_hru = 0
      END FUNCTION climate_hru
