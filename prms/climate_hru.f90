!***********************************************************************
! Read and makes available climate data (tmin, tmax, precip, potential
! solar radiation, potential evapotranspieration) and/or transpiration
! on, by HRU from files pre-processed Data Files available for other
! PRMS modules
!***********************************************************************
      MODULE PRMS_CLIMATE_HRU
        ! Local Variables
        INTEGER, SAVE :: Precip_unit, Tmax_unit, Tmin_unit, Et_unit, Swrad_unit, Transp_unit
        CHARACTER(LEN=11), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE :: Adj_by_hru
        INTEGER, SAVE, ALLOCATABLE :: Hru_subbasin(:)
        REAL, SAVE, ALLOCATABLE :: Rain_sub_adj(:, :), Snow_sub_adj(:, :)
        REAL, SAVE, ALLOCATABLE :: Rain_cbh_adj(:, :), Snow_cbh_adj(:, :)
        REAL, SAVE, ALLOCATABLE :: Tmax_cbh_adj(:), Tmin_cbh_adj(:)
      END MODULE PRMS_CLIMATE_HRU

      INTEGER FUNCTION climate_hru()
      USE PRMS_CLIMATE_HRU
      USE PRMS_MODULE, ONLY: Process, Nhru, Nsub, Precip_flag, &
     &    Solrad_flag, Et_flag, Temp_flag, Subbasin_flag, Transp_flag, Orad_flag, &
     &    Tmin_day, Tmax_day, Precip_day, Potet_day, Swrad_day, Transp_day, Model
      USE PRMS_BASIN, ONLY: Start_year, Start_month, Start_day, &
     &    Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, NEARZERO, MM2INCH
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp, &
     &    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, &
     &    Tavgc, Hru_ppt, Hru_rain, Hru_snow, Prmx, Pptmix, Newsnow, &
     &    Precip_units, Tmax_allrain_f, Adjmix_rain, &
     &    Basin_ppt, Basin_potet, Potet, Basin_snow, Basin_rain, &
     &    Basin_horad, Orad, Swrad, Basin_potsw, Basin_obs_ppt, Transp_on, Basin_transp_on
      !USE PRMS_CLIMATEVARS, ONLY: Basin_tsta
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Jday, Tmax, Tmin
      USE PRMS_SOLTAB, ONLY: Soltab_basinpotsw, Hru_cossl, Soltab_potsw
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX, ISNAN
      INTEGER, EXTERNAL :: declmodule, declparam, getparam, control_string
      EXTERNAL read_error, precip_form, temp_set, find_header_end, find_current_time
! Local Variables
      INTEGER :: yr, mo, dy, i, hr, mn, sec, jj, ierr, istop, missing, nc
      INTEGER :: tmax_missing, tmin_missing, potet_missing, swrad_missing, ppt_missing, transp_missing
      DOUBLE PRECISION :: sum_obs
      REAL :: rainadj, snowadj, tmax_hru, tmin_hru, ppt, adjmix, allrain
      CHARACTER(LEN=80), SAVE :: Version_climate_hru
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Climate Distribuition'
!***********************************************************************
      climate_hru = 0

      IF ( Process(:3)=='run' ) THEN
        IF ( Temp_flag==7 ) THEN
          READ ( Tmax_unit, * ) yr, mo, dy, hr, mn, sec, (Tmaxf(i), i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Tmaxf', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          READ ( Tmin_unit, * ) yr, mo, dy, hr, mn, sec, (Tminf(i), i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Tminf', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_tmax = 0.0D0
          Basin_tmin = 0.0D0
          Basin_temp = 0.0D0
        ENDIF

        IF ( Precip_flag==7 ) THEN
          READ ( Precip_unit, * ) yr, mo, dy, hr, mn, sec, (Hru_ppt(i), i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Hru_ppt', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_ppt = 0.0D0
          Basin_rain = 0.0D0
          Basin_snow = 0.0D0
          sum_obs = 0.0D0
        ENDIF

        IF ( Et_flag==7 ) THEN
          READ ( Et_unit, * ) yr, mo, dy, hr, mn, sec, (Potet(i),i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Potet', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_potet = 0.0D0
        ENDIF

        IF ( Solrad_flag==7 ) THEN
          IF ( Orad_flag==0 ) THEN
            READ ( Swrad_unit, * ) yr, mo, dy, hr, mn, sec, (Swrad(i), i=1,Nhru)
          ELSE
            READ ( Swrad_unit, * ) yr, mo, dy, hr, mn, sec, (Swrad(i), i=1,Nhru), Orad
          ENDIF
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Swrad', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_potsw = 0.0D0
        ENDIF

        IF ( Transp_flag==3 ) THEN
          READ ( Transp_unit, * ) yr, mo, dy, hr, mn, sec, (Transp_on(i),i=1,Nhru)
          IF ( yr/=Nowyear .OR. mo/=Nowmonth .OR. Nowday/=dy ) THEN
            PRINT 9001, 'Transp_on', Nowyear, Nowmonth, Nowday, yr, mo, dy
            STOP
          ENDIF
          Basin_transp_on = 0
        ENDIF

        adjmix = Adjmix_rain(Nowmonth)
        allrain = Tmax_allrain_f(Nowmonth)
        tmax_missing = 0
        tmin_missing = 0
        potet_missing = 0
        swrad_missing = 0
        ppt_missing = 0
        transp_missing = 0
        missing = 0
        DO jj = 1, Active_hrus
          i = Hru_route_order(jj)

          IF ( Temp_flag==7 ) THEN
            IF ( Tmaxf(i)<-100 .OR. ISNAN(Tmaxf(i)) ) THEN
              tmax_missing = tmax_missing + 1
              missing = 1
            ENDIF
            IF ( Tminf(i)<-100 .OR. ISNAN(Tminf(i)) ) THEN
              tmin_missing = tmin_missing + 1
              missing = 1
            ENDIF
            IF ( missing==0 ) THEN
              tmax_hru = Tmaxf(i) + Tmax_cbh_adj(i)
              tmin_hru = Tminf(i) + Tmin_cbh_adj(i)
              CALL temp_set(i, tmax_hru, tmin_hru, Tmaxf(i), Tminf(i), &
     &                      Tavgf(i), Tmaxc(i), Tminc(i), Tavgc(i), Hru_area(i))
            ENDIF
          ENDIF

          IF ( Et_flag==7 ) THEN
            IF ( Potet(i)<0.0 .OR. ISNAN(Potet(i)) ) THEN
              potet_missing = potet_missing + 1
              missing = 1
            ELSE
              Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
            ENDIF
          ENDIF

          IF ( Solrad_flag==7 ) THEN
            IF ( Swrad(i)<0.0 .OR. ISNAN(Swrad(i)) ) THEN
              swrad_missing = swrad_missing + 1
              missing = 1
            ELSE
              Basin_potsw = Basin_potsw + Swrad(i)*Hru_area(i)
            ENDIF
          ENDIF

          IF ( Transp_flag==3 ) THEN
            IF ( Transp_on(i)<0 ) THEN
              transp_missing = transp_missing + 1
              missing = 1
            ELSE
              IF ( Transp_on(i)==1 ) Basin_transp_on = 1
            ENDIF
          ENDIF

          IF ( Precip_flag==7 ) THEN
            IF ( Hru_ppt(i)<0.0 .OR. ISNAN(Hru_ppt(i)) ) THEN
              ppt_missing = ppt_missing + 1
              missing = 1
              CYCLE
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
              snowadj = Snow_cbh_adj(i, mo)
              rainadj = Rain_cbh_adj(i, mo)
            ENDIF
            ppt = Hru_ppt(i)
            CALL precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), &
     &                       Tmaxf(i), Tminf(i), Pptmix(i), Newsnow(i), &
     &                       Prmx(i), allrain, rainadj, snowadj, adjmix, Hru_area(i), sum_obs)
          ENDIF
        ENDDO

        IF ( tmax_missing>0 ) PRINT *, 'ERROR,', tmax_missing, ' negative or NaN tmax CBH value(s) found'
        IF ( tmin_missing>0 ) PRINT *, 'ERROR,', tmin_missing, ' negative or NaN tmin CBH value(s) found'
        IF ( potet_missing>0 ) PRINT *, 'ERROR,', potet_missing, ' negative or NaN potet CBH value(s) found'
        IF ( swrad_missing>0 ) PRINT *, 'ERROR,', swrad_missing, ' negative or NaN swrad CBH value(s) found'
        IF ( ppt_missing>0 ) PRINT *, 'ERROR,', ppt_missing, ' negative or NaN precip CBH value(s) found'
        IF ( transp_missing>0 ) PRINT *, 'ERROR,', transp_missing, ' negative or NaN transp CBH value(s) found'
        IF ( missing==1 ) THEN
          PRINT '(A,I5,A,I2.2,A,I2.2)', 'Date:', Nowyear, '/', Nowmonth, '/', Nowday
          STOP
        ENDIF

        IF ( Temp_flag==7 ) THEN
          Basin_tmax = Basin_tmax*Basin_area_inv
          Basin_tmin = Basin_tmin*Basin_area_inv
          Basin_temp = Basin_temp*Basin_area_inv
          !IF ( Ntemp>0 ) THEN
          !  Solrad_tmax = Tmax(Basin_tsta)
          !  Solrad_tmin = Tmin(Basin_tsta)
          !ELSE
            Solrad_tmax = Basin_tmax
            Solrad_tmin = Basin_tmin
          !ENDIF
        ENDIF

        IF ( Precip_flag==7 ) THEN
          Basin_ppt = Basin_ppt*Basin_area_inv
          Basin_obs_ppt = sum_obs*Basin_area_inv
          Basin_rain = Basin_rain*Basin_area_inv
          Basin_snow = Basin_snow*Basin_area_inv
        ENDIF

        IF ( Et_flag==7 ) Basin_potet = Basin_potet*Basin_area_inv

        IF ( Solrad_flag==7 ) THEN
          Basin_horad = Soltab_basinpotsw(Jday)
          IF ( Orad_flag==0 ) Orad = (Swrad(1)*Hru_cossl(1)*Basin_horad)/Soltab_potsw(Jday,1)
          Basin_potsw = Basin_potsw*Basin_area_inv
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_climate_hru = '$Id: climate_hru.f90 5217 2013-01-14 21:02:30Z rsregan $'
        nc = INDEX( Version_climate_hru, 'Z' )
        i = INDEX( Version_climate_hru, '.f90' ) + 3
        IF ( declmodule(Version_climate_hru(6:i), PROCNAME, Version_climate_hru(i+2:nc))/=0 ) STOP
        MODNAME = 'climate_hru'

!   Declared Parameters
        IF ( Temp_flag==7 .OR. Model==99 ) ALLOCATE ( Tmax_cbh_adj(Nhru), Tmin_cbh_adj(Nhru) )
        IF ( Precip_flag==7 .OR. Model==99 ) THEN
          IF ( Subbasin_flag==1 .OR. Model==99 ) &
     &         ALLOCATE ( Hru_subbasin(Nhru), Rain_sub_adj(Nsub,12), Snow_sub_adj(Nsub,12) )
          ALLOCATE ( Rain_cbh_adj(Nhru,12), Snow_cbh_adj(Nhru,12) )
        ENDIF

        IF ( Temp_flag==7 .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'tmax_cbh_adj', 'nhru', 'real', &
     &         '0.0', '-10.0', '10.0', &
     &         'HRU maximum temperature adjustment', &
     &         'Adjustment to maximum temperature for each HRU, estimated based on slope and aspect', &
     &         'temp_units')/=0 ) CALL read_error(1, 'tmax_cbh_adj')
          IF ( declparam(MODNAME, 'tmin_cbh_adj', 'nhru', 'real', &
     &         '0.0', '-10.0', '10.0', &
     &         'HRU minimum temperature adjustment', &
     &         'Adjustment to minimum temperature for each HRU, estimated based on slope and aspect', &
     &         'temp_units')/=0 ) CALL read_error(1, 'tmin_cbh_adj')
        ENDIF

        IF ( Precip_flag==7 .OR. Model==99 ) THEN
          IF ( declparam(MODNAME, 'adj_by_hru', 'one', 'integer', &
     &         '1', '0', '1', &
     &         'Adjust precipitation by HRU or subbasin (0=subbasin; 1=HRU)', &
     &         'Flag to indicate whether to adjust precipitation and'// &
     &         ' air temperature by HRU or subbasin (0=subbasin; 1=HRU)', &
     &         'none')/=0 ) CALL read_error(1, 'adj_by_hru')

          IF ( Subbasin_flag==1 .OR. Model==99 ) THEN
            IF ( declparam(MODNAME, 'hru_subbasin', 'nhru', 'integer', &
     &           '0', 'bounded', 'nsub', &
     &           'Index of subbasin assigned to each HRU', &
     &           'Index of subbasin assigned to each HRU', &
     &           'none')/=0 ) CALL read_error(1, 'hru_subbasin')
            IF ( declparam(MODNAME, 'rain_sub_adj', 'nsub,nmonths', 'real', &
     &           '1.0', '0.0', '4.0', &
     &           'Rain adjustment factor for each subbasin and month', &
     &           'Monthly (January to December) adjustment factor to'// &
     &           ' measured precipitation determined to be rain for each subbasin', &
     &           'decimal fraction')/=0 ) CALL read_error(1, 'rain_sub_adj')
            IF ( declparam(MODNAME, 'snow_sub_adj', 'nsub,nmonths', &
     &           'real', '1.0', '0.0', '4.0', &
     &           'Snow adjustment factor for each subbasin and month', &
     &           'Monthly (January to December) adjustment factor to'// &
     &           ' measured precipitation determined to be snow for each subbasin', &
     &           'decimal fraction')/=0 ) CALL read_error(1, 'snow_sub_adj')
          ENDIF

          IF ( declparam(MODNAME, 'rain_cbh_adj', 'nhru,nmonths', 'real', &
     &         '1.0', '0.2', '5.0', &
     &         'Rain adjustment factor, by month for each HRU', &
     &         'Monthly (January to December) adjustment factor to'// &
     &         ' measured precipitation determined to be rain on'// &
     &         ' each HRU to account for differences in elevation, and so forth', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'rain_cbh_adj')
          IF ( declparam(MODNAME, 'snow_cbh_adj', 'nhru,nmonths', 'real', &
     &         '1.0', '0.2', '5.0', &
     &         'Snow adjustment factor, by month for each HRU', &
     &         'Monthly (January to December) adjustment factor to'// &
     &         ' measured precipitation determined to be snow on'// &
     &         ' each HRU to account for differences in elevation, and so forth', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'snow_cbh_adj')
        ENDIF

      ELSEIF ( Process(:4)=='init' ) THEN
        istop = 0
        IF ( Precip_flag==7 ) THEN
          IF ( getparam(MODNAME, 'adj_by_hru', 1, 'integer', Adj_by_hru)/=0 ) CALL read_error(2, 'adj_by_hru')
          IF ( Adj_by_hru==0 ) THEN
            IF ( Nsub==0 ) THEN
              PRINT *, 'ERROR, in climate_hru: adj_by_hru=0 and nsub=0'
              PRINT *, ' must have subbasins to adjust precipitation by subbasin'
              STOP
            ENDIF
            IF ( getparam(MODNAME, 'hru_subbasin', Nhru, 'integer', Hru_subbasin)/=0 ) CALL read_error(2, 'hru_subbasin')
            IF ( getparam(MODNAME, 'rain_sub_adj', Nsub*12, 'real', Rain_sub_adj)/=0 ) CALL read_error(2, 'rain_sub_adj')
            IF ( getparam(MODNAME, 'snow_sub_adj', Nsub*12, 'real', Snow_sub_adj)/=0 ) CALL read_error(2, 'snow_sub_adj')
          ELSE
            IF ( getparam(MODNAME, 'rain_cbh_adj', Nhru*12, 'real', Rain_cbh_adj)/=0 ) CALL read_error(2, 'rain_cbh_adj')
            IF ( getparam(MODNAME, 'snow_cbh_adj', Nhru*12, 'real', Snow_cbh_adj)/=0 ) CALL read_error(2, 'snow_cbh_adj')
          ENDIF
          IF ( control_string(Precip_day, 'precip_day')/=0 ) CALL read_error(5, 'precip_day')
          CALL find_header_end(Precip_unit, Precip_day, 'precip_day', ierr)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Precip_unit, Start_year, Start_month, Start_day, ierr)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, CBH File: ', Precip_day
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( Temp_flag==7 ) THEN
          IF ( getparam(MODNAME, 'tmax_cbh_adj', Nhru, 'real', Tmax_cbh_adj)/=0 ) CALL read_error(2, 'tmax_cbh_adj')
          IF ( getparam(MODNAME, 'tmin_cbh_adj', Nhru, 'real', Tmin_cbh_adj)/=0 ) CALL read_error(2, 'tmin_cbh_adj')
          IF ( control_string(Tmax_day, 'tmax_day')/=0 ) CALL read_error(5, 'tmax_day')
          IF ( control_string(Tmin_day, 'tmin_day')/=0 ) CALL read_error(5, 'tmin_day')
          CALL find_header_end(Tmax_unit, Tmax_day, 'tmax_day', ierr)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Tmax_unit, Start_year, Start_month, Start_day, ierr)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, CBH File: ', Tmax_day
              istop = 1
            ENDIF
          ENDIF
          CALL find_header_end(Tmin_unit, Tmin_day, 'tmin_day', ierr)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Tmin_unit, Start_year, Start_month, Start_day, ierr)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, CBH File: ', Tmin_day
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( Et_flag==7 ) THEN
          IF ( control_string(Potet_day, 'potet_day')/=0 ) CALL read_error(5, 'potet_day')
          CALL find_header_end(Et_unit, Potet_day, 'potet_day', ierr)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Et_unit, Start_year, Start_month, Start_day, ierr)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, CBH File: ', Potet_day
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( Transp_flag==3 ) THEN
          IF ( control_string(Transp_day, 'transp_day')/=0 ) CALL read_error(5, 'transp_day')
          CALL find_header_end(Transp_unit, Transp_day, 'transp_day', ierr)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Transp_unit, Start_year, Start_month, Start_day, ierr)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, CBH File: ', Transp_day
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( Solrad_flag==7 ) THEN
          IF ( control_string(Swrad_day, 'swrad_day')/=0 ) CALL read_error(5, 'swrad_day')
          CALL find_header_end(Swrad_unit, Swrad_day, 'swrad_day', ierr)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Swrad_unit, Start_year, Start_month, Start_day, ierr)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, CBH File: ', Swrad_day
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( istop==1 ) STOP 'ERROR in climate_hru'
      ENDIF

 9001 FORMAT ( 'ERROR, problem reading daily HRU: ', A, ' file', /, &
               'Timestep:', I5.4, 2('/',I2.2), /, 'File time:', I5.4, 2('/',I2.2),/ )

      END FUNCTION climate_hru
