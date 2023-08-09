!***********************************************************************
! Determines the form of precipitation and distributes precipitation
! and temperatures to each HRU based on measurements at stations with
! closest elevation or shortest distance to the respective HRU
! temp_nsta - number of temperature stations used
! temp_nuse (temp_nsta) - indicies of temperature stations used
! rain_nsta - number of precipitation stations used
! rain_nuse (rain_nsta) - indicies of precipitation stations used
!***********************************************************************
      MODULE PRMS_MM_DIST
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Temp & Precip Distribution'
        character(len=11) :: MODNAME
        character(len=*), parameter :: Version_mm_dist = '2022-04-05'
        INTEGER, SAVE :: Temp_nsta, Rain_nsta
        INTEGER, SAVE, ALLOCATABLE :: Temp_nuse(:), Rain_nuse(:)
        ! Declared Variables 
        INTEGER, SAVE :: Rain_flag
        REAL, SAVE, ALLOCATABLE :: Tmax_rain_sta(:), Tmin_rain_sta(:)
! Parameters
        INTEGER, SAVE, ALLOCATABLE :: Psta_freq_nuse(:), Tsta_nuse(:), Psta_nuse(:)
        REAL, SAVE :: Downscale_adj
        REAL, SAVE, ALLOCATABLE :: Hru_month_ppt(:,:), Hru_month_max(:,:), Hru_month_min(:,:)
        REAL, SAVE, ALLOCATABLE :: Rain_month_max(:,:), Rain_month_min(:,:), Tmax_allrain_sta(:,:), Tmax_allsnow_sta(:,:)
        REAL, SAVE, ALLOCATABLE :: Psta_month_ppt(:,:), Tsta_month_max(:,:), Tsta_month_min(:,:)
      END MODULE PRMS_MM_DIST

!***********************************************************************
      SUBROUTINE mm_dist()
      USE PRMS_CONSTANTS, ONLY: MONTHS_PER_YEAR, RUN, DECL, INIT, MONTHS_PER_YEAR
      USE PRMS_MODULE, ONLY: Nhru, Ntemp, Nrain, Process_flag 
      USE PRMS_MM_DIST
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam, declvar
      EXTERNAL :: read_error, print_module
! Local Variables
      INTEGER :: i, j
!*********************************************************************** 
      IF ( Process_flag==RUN ) THEN
        CALL mm_temp_run()
        CALL mm_rain_run()

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_mm_dist)

! allocate local variables
        ALLOCATE ( Temp_nuse(Ntemp), Rain_nuse(Nrain) )
! allocate parameters
        ALLOCATE ( Psta_freq_nuse(Nrain), Hru_month_max(Nhru,MONTHS_PER_YEAR), Hru_month_min(Nhru,MONTHS_PER_YEAR) )
        ALLOCATE ( Tsta_month_max(Ntemp,MONTHS_PER_YEAR), Tsta_month_min(Ntemp,MONTHS_PER_YEAR), Psta_month_ppt(Nrain,MONTHS_PER_YEAR) )
        ALLOCATE ( Rain_month_max(Nrain,MONTHS_PER_YEAR), Rain_month_min(Nrain,MONTHS_PER_YEAR), Hru_month_ppt(Nhru,MONTHS_PER_YEAR) )

! declare variables
        IF ( declvar(MODNAME, 'rain_flag', 'one', 1, 'integer', &
     &       'Flag to indicate if it is raining anywhere in the basin', 'none', rain_flag)/=0 ) CALL read_error(3, 'rain_flag')
      
        ALLOCATE ( Tmax_rain_sta(Nrain) )
        IF ( declvar(MODNAME, 'tmax_rain_sta', 'nrain', Nrain, 'real', &
     &       'Maximum temperature distributed to the precipitation stations', &
     &       'temp_units', Tmax_rain_sta)/=0 ) CALL read_error(3, 'tmax_rain_sta')

        ALLOCATE ( Tmin_rain_sta(Nrain) )
        IF ( declvar(MODNAME, 'tmin_rain_sta', 'nrain', Nrain, 'real', &
     &       'Minimum temperature distributed to the precipitation stations', &
     &       'temp_units', Tmin_rain_sta)/=0 ) CALL read_error(3, 'tmin_rain_sta')

! declare parameters
        ALLOCATE ( Tsta_nuse(Ntemp) )
        IF ( declparam (MODNAME, 'tsta_nuse', 'ntemp', 'integer', &
     &       '1', '0', '1', &
     &       '0 = station not used; 1 = station used', &
     &       'The subset of temperature measurement stations used in the distribution regression'// &
     &       ' (0=station not used; 1=station used)', &
     &       'none')/=0 ) CALL read_error(2, 'tsta_nuse')

        ALLOCATE ( Psta_nuse(Nrain) )
        IF ( declparam (MODNAME, 'psta_nuse', 'nrain', 'integer', &
     &       '1', '0', '1', &
     &       'The subset of precipitation stations used in the distribution regression (0=station not used; 1=station used)', &
     &       'The subset of precipitation measurement stations used in the distribution regression (0=station not used;'// &
     &       ' 1=station used)', &
     &       'none')/=0 ) CALL read_error(2, 'psta_nuse')

        IF ( declparam (MODNAME, 'psta_freq_nuse', 'nrain', 'integer', &
     &       '1', '0', '1', &
     &       'The subset of precipitation stations used to determine if there is distribution in the basin (0=station not used;'// &
     &       ' 1=station used)', &
     &       'The subset of precipitation measurement stations used to determine if there is precipitation in the basin'// &
     &       ' (0=station not used; 1=station used)', &
     &       'none')/=0 ) CALL read_error(2, 'psta_freq_nuse')

        IF ( declparam (MODNAME, 'tsta_month_max', 'ntemp,nmonths', 'real', &
     &       '0.0', '-100.0', '200.0', &
     &       'Average monthly maximum temperature at each station', &
     &       'Average monthly maximum temperature at each station', &
     &       'temp_units')/=0 ) CALL read_error(2, 'tsta_month_max')

        IF ( declparam (MODNAME, 'tsta_month_min', 'ntemp,nmonths', 'real', &
     &       '0.0', '-100.0', '200.0', &
     &       'Average monthly minimum temperature at each station', &
     &       'Average monthly minimum temperature at each station', &
     &       'temp_units')/=0 ) CALL read_error(2, 'tsta_month_min')

        IF ( declparam (MODNAME, 'psta_month_ppt', 'nrain,nmonths', 'real', &
     &       '0.0', '0.0', '200.0', &
     &       'Average monthly precipitation at each station', &
     &       'Average monthly precipitation at each station', &
     &       'precip_units')/=0 ) CALL read_error(2, 'psta_month_ppt')

        IF ( declparam (MODNAME, 'downscale_adj', 'one', 'real', &
     &       '0.01', '0.0', '1.0', &
     &       'Downscaling fraction error', 'Downscaling fraction error', &
     &       'decimal fraction')/=0 ) CALL read_error(2, 'downscale_adj')

        IF ( declparam(MODNAME, 'hru_month_max', 'nhru,nmonths', 'real', &
     &       '0.0', '-100.0', '200.0', &
     &       'Average monthly maximum temperature at each HRU', &
     &       'Average monthly maximum temperature at each HRU', &
     &       'temp_units')/=0 ) CALL read_error(2, 'hru_month_max')

        IF ( declparam(MODNAME, 'hru_month_min', 'nhru,nmonths', 'real', &
     &       '0.0', '-100.0', '200.0', &
     &       'Average monthly minimum temperature at each HRU', &
     &       'Average monthly minimum temperature at each HRU', &
     &       'temp_units')/=0 ) CALL read_error(2, 'hru_month_min')

        IF ( declparam(MODNAME, 'hru_month_ppt', 'nhru,nmonths', 'real', &
     &       '0.0', '0.0', '20.0', &
     &       'Average monthly precipitation at each HRU', &
     &       'Average monthly precipitation at each HRU', &
     &       'precip_units')/=0 ) CALL read_error(2, 'hru_month_ppt')

        IF ( declparam(MODNAME, 'rain_month_max', 'nrain,nmonths', &
     &       '0.0', '0.0', '20.0', &
     &       'Average monthly maximum precipitation at each rain station', &
     &       'Average monthly maximum precipitation at each rain station', &
     &       'precip_units')/=0 ) CALL read_error(2, 'rain_month_max')

        IF ( declparam(MODNAME, 'rain_month_min', 'nrain,nmonths', 'real', &
     &       '0.0', '0.0', '20.0', &
     &       'Average monthly minimum precipitation at each rain station', &
     &       'Average monthly minimum precipitation at each rain station', &
     &       'precip_units')/=0 ) CALL read_error(2, 'rain_month_min')

        ALLOCATE ( Tmax_allrain_sta(Nrain,MONTHS_PER_YEAR) )
        IF ( declparam(MODNAME, 'tmax_allrain_sta', 'nrain,nmonths', &
           'real', '38.0', '-8.0', '75.0', &
           'Precipitation is rain if HRU max temperature >= this value', &
           'Monthly (January to December) maximum air temperature when precipitation is assumed to be rain; if'// &
           ' precipitation measurement station air temperature is greater than or equal to this value, precipitation is rain', &
           'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain_sta')

      ALLOCATE ( Tmax_allsnow_sta(Nrain,MONTHS_PER_YEAR) )
      IF ( declparam(MODNAME, 'tmax_allsnow_sta', 'nrain,nmonths', &
           'real', '32.0', '-10.0', '40.0', &
           'Maximum temperature when precipitation is all snow', &
           'Monthly (January to December) maximum air temperature when precipitation is assumed to be snow; if'// &
           ' precipitation measurement station air temperature is less than or equal to this value, precipitation is snow', &
           'temp_units')/=0 ) CALL read_error(1, 'tmax_allsnow_sta')

      ELSEIF ( Process_flag==INIT ) THEN
        IF ( getparam (MODNAME, 'tsta_nuse', Ntemp, 'integer', Tsta_nuse)/=0 ) CALL read_error(2, 'tsta_nuse')
        IF ( getparam (MODNAME, 'psta_nuse', Nrain, 'integer', Psta_nuse)/=0 ) CALL read_error(2, 'psta_nuse')
        IF ( getparam (MODNAME, 'psta_freq_nuse', Nrain, 'integer', Psta_freq_nuse)/=0 ) CALL read_error(2, 'psta_freq_nuse')
        IF ( getparam (MODNAME, 'tsta_month_min', Ntemp*MONTHS_PER_YEAR, 'real', Tsta_month_min)/=0 ) CALL read_error(2, 'tsta_month_min')
        IF ( getparam (MODNAME, 'tsta_month_max', Ntemp*MONTHS_PER_YEAR, 'real', Tsta_month_max)/=0 ) CALL read_error(2, 'tsta_month_max')
        IF ( getparam (MODNAME, 'psta_month_ppt', Nrain*MONTHS_PER_YEAR, 'real', Psta_month_ppt)/=0 ) CALL read_error(2, 'psta_month_ppt')
        IF ( getparam (MODNAME, 'downscale_adj', 1, 'real', Downscale_adj)/=0 ) CALL read_error(2, 'downscale_adjor')
        IF ( getparam (MODNAME, 'hru_month_min', Nhru*MONTHS_PER_YEAR, 'real', Hru_month_min)/=0 ) CALL read_error(2, 'hru_month_min')
        IF ( getparam (MODNAME, 'hru_month_max', Nhru*MONTHS_PER_YEAR, 'real', Hru_month_max)/=0 ) CALL read_error(2, 'hru_month_max')
        IF ( getparam (MODNAME, 'hru_month_ppt', Nhru*MONTHS_PER_YEAR, 'real', Hru_month_ppt)/=0 ) CALL read_error(2, 'hru_month_ppt')
        IF ( getparam (MODNAME, 'rain_month_max', Nrain*MONTHS_PER_YEAR, 'real', Rain_month_max)/=0 ) CALL read_error(2, 'rain_month_max')
        IF ( getparam (MODNAME, 'rain_month_min', Nrain*MONTHS_PER_YEAR, 'real', Rain_month_min)/=0 ) CALL read_error(2, 'rain_month_min')
        IF ( getparam(MODNAME, 'tmax_allrain_sta', Nrain*MONTHS_PER_YEAR, 'real', &
             Tmax_allrain_sta)/=0 ) CALL read_error(2, 'tmax_allrain_sta')
        IF ( getparam(MODNAME, 'tmax_allsnow_sta', Nrain*MONTHS_PER_YEAR, 'real', &
             Tmax_allsnow_sta)/=0 ) CALL read_error(2, 'tmax_allsnow_sta')

        Temp_nsta = 0
        DO i = 1, Ntemp
          IF ( Tsta_nuse(i)==1 ) THEN
            Temp_nsta = Temp_nsta + 1
            Temp_nuse(Temp_nsta) = i
          ENDIF 
        ENDDO

        Rain_nsta = 0
        DO i = 1, Nrain
          IF ( Psta_nuse(i)==1 ) THEN
            Rain_nsta = Rain_nsta + 1
            Rain_nuse(Rain_nsta) = i
          ENDIF
        ENDDO

        DO j = 1, MONTHS_PER_YEAR
          DO i = 1, Nrain
            IF ( Psta_month_ppt(i,j)==0.0 ) THEN
              PRINT *, 'ERROR, psta_month_ppt cannot be 0.0, precip station:', i, '; month:', j
              STOP
            ENDIF
          ENDDO
        ENDDO

        DEALLOCATE ( Tsta_nuse, Psta_nuse )
      ENDIF
      END SUBROUTINE mm_dist


!***********************************************************************
! mm_temp_run - Temperature calculation
! calculates daily max and min temperature using data from available stations
! Outputs a daily max and min temperature by HRU elevation
!***********************************************************************
      SUBROUTINE mm_temp_run ()
      USE PRMS_MM_DIST
      USE PRMS_MODULE, ONLY: Nrain, Nowmonth
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_tmax, Basin_tmin, &
     &    Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Basin_tsta, Tmax_aspect_adjust, Tmin_aspect_adjust
      USE PRMS_OBS, ONLY: Tmax, Tmin
      IMPLICIT NONE
! Functions
      EXTERNAL temp_set
! Local variables
      INTEGER :: i, ii, j, k, ntmin, ntmax
      REAL :: x1, tmax_hru, tmin_hru
!*********************************************************************** 
      Solrad_tmax = Tmax(Basin_tsta)
      Solrad_tmin = Tmin(Basin_tsta)

      Basin_tmax = 0.0
      Basin_tmin = 0.0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        tmax_hru = 0.0
        tmin_hru = 0.0
        ntmax = 0
        ntmin = 0
        DO j = 1, Temp_nsta
          k = Temp_nuse(j)
          IF ( Tmax(k)>-99.0 ) THEN
            ntmax = ntmax + 1
            tmax_hru = tmax_hru + Tmax(k) + Hru_month_max(i, Nowmonth) - Tsta_month_max(k, Nowmonth)
          ENDIF
          IF ( Tmin(k)>-99.0 ) THEN
            ntmin = ntmin + 1
            tmin_hru = tmin_hru + Tmin(k) + Hru_month_min(i, Nowmonth) - Tsta_month_min(k, Nowmonth)
          ENDIF
        ENDDO

        IF ( ntmax==0 ) THEN
          tmax_hru = Hru_month_max(i, Nowmonth)
        ELSE
          tmax_hru = tmax_hru/ntmax
        ENDIF
        IF ( ntmin==0 ) THEN
          tmin_hru = Hru_month_min(i, Nowmonth)
        ELSE
          tmin_hru = tmin_hru/ntmin
        ENDIF

! Temperature adjustment by HRU
        tmax_hru = tmax_hru + Tmax_aspect_adjust(i, Nowmonth)
        tmin_hru = tmin_hru + Tmin_aspect_adjust(i, Nowmonth)

! IF max is less than min, switch
        IF ( tmax_hru<tmin_hru ) THEN
          x1 = tmax_hru
          tmax_hru = tmin_hru
          tmin_hru = x1
        ENDIF

! Now sort out units and calculate basin_tmax and basin_tmin.
        CALL temp_set(i, tmax_hru, tmin_hru, Tmaxf(i), Tminf(i), Tavgf(i), &
                      Tmaxc(i), Tminc(i), Tavgc(i), Hru_area(i))
      ENDDO
      Basin_tmax = Basin_tmax*Basin_area_inv
      Basin_tmin = Basin_tmin*Basin_area_inv

! Calculate temperatures at precipitation stations.
      DO i = 1, Nrain
        Tmax_rain_sta(i) = 0.0
        Tmin_rain_sta(i) = 0.0
        ntmax = 0
        ntmin = 0
        DO j = 1, Temp_nsta
          k = Temp_nuse(j)
          IF ( Tmax(k)>-99.0 ) THEN
            ntmax = ntmax + 1
            Tmax_rain_sta(i) = Tmax_rain_sta(i) + Tmax(k) + Rain_month_max(i, Nowmonth) - Tsta_month_max(k, Nowmonth)
          ENDIF
          IF ( Tmin(k)>-99.0 ) THEN
            ntmin = ntmin + 1
            Tmin_rain_sta(i) = Tmin_rain_sta(i) + Tmin(k) + Rain_month_min(i, Nowmonth) - Tsta_month_min(k, Nowmonth)
          ENDIF
        ENDDO

        IF ( ntmax==0 ) THEN
          Tmax_rain_sta(i) = Hru_month_max(i, Nowmonth)
        ELSE
          Tmax_rain_sta(i) = Tmax_rain_sta(i)/ntmax
        ENDIF
        IF ( ntmin==0 ) THEN
          Tmin_rain_sta(i) = Hru_month_min(i, Nowmonth)
        ELSE
          Tmin_rain_sta(i) = Tmin_rain_sta(i)/ntmin
        ENDIF

! If max is less than min, switch
        IF ( Tmax_rain_sta(i)<Tmin_rain_sta(i) ) THEN
          x1 = Tmax_rain_sta(i)
          Tmax_rain_sta(i) = Tmin_rain_sta(i)
          Tmin_rain_sta(i) = x1
        ENDIF
      ENDDO

      END SUBROUTINE mm_temp_run

! *******************************************************************
! mm_rain_run
! *******************************************************************
      SUBROUTINE mm_rain_run()
      USE PRMS_CONSTANTS, ONLY: NEARZERO, MM2INCH
      USE PRMS_MM_DIST
      USE PRMS_MODULE, ONLY: Nrain, Nowmonth
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tminf, Newsnow, Pptmix, Precip_units, &
     &    Prmx, Hru_rain, Hru_snow, Hru_ppt, Basin_rain, Basin_snow, &
     &    Basin_ppt, Adjmix_rain, Tmax_allrain_f, Tmax_allsnow_f
      USE PRMS_OBS, ONLY: Precip, Rain_day, Rain_code
      IMPLICIT NONE
! Functions
      INTRINSIC ABS
      EXTERNAL :: precip_form
! Local variables
      INTEGER :: i, ii, j, k, err_chk, nppt, nsta_used
      REAL :: ppt
      DOUBLE PRECISION :: sum_obs
!***********************************************************************
! Code to check the rain_code parameter to determine if it is raining in the basin.

      nsta_used = Nrain
      rain_flag = 0
      IF ( Rain_code(Nowmonth)==1 ) THEN
        nsta_used = Rain_nsta
        DO j = 1, Rain_nsta
          i = Rain_nuse(j)
          IF ( Precip(i)>0.0 ) rain_flag = 1
        ENDDO
      ELSEIF ( Rain_code(Nowmonth)==2 ) THEN
        DO i = 1, Nrain
          IF ( Precip(i)>0.0 ) rain_flag = 1
        ENDDO
      ELSEIF ( Rain_code(Nowmonth)==3 ) THEN
        rain_flag = 1
      ELSEIF ( Rain_code(Nowmonth)==4 ) THEN
        IF ( Rain_day==1 ) rain_flag = 1
      ELSEIF (Rain_code(Nowmonth)==5 ) THEN
        DO i = 1, Nrain
          IF ( Psta_freq_nuse(i)==1 ) THEN
            IF ( Precip(i)>0.0 ) rain_flag = 1
          ENDIF
        ENDDO
      ENDIF

! Add error here
      DO j = 1, nsta_used
        IF ( Rain_code(Nowmonth)==1 ) THEN
          i = Rain_nuse(j)
        ELSE
          i = j
        ENDIF
        IF ( Precip(i)<0.0 ) CYCLE
        err_chk = 0
        IF ( Tmax_rain_sta(i)<=Tmax_allsnow_sta(i,Nowmonth) ) THEN
          err_chk = 1
        ELSEIF ( Tmin_rain_sta(i)>Tmax_allsnow_sta(i,Nowmonth) .OR. Tmax_rain_sta(i)>=Tmax_allrain_sta(i,Nowmonth) ) THEN
          err_chk = 0
        ELSE
          err_chk = 1
        ENDIF
        IF ( err_chk==1 ) Precip(i) = Precip(i)*Downscale_adj + Precip(i)
      ENDDO

      Basin_ppt = 0.0
      Basin_rain = 0.0
      Basin_snow = 0.0
!******Initialize HRU variables
      Pptmix = 0
      Newsnow = 0
      Prmx = 0.0
      Hru_rain = 0.0
      Hru_snow = 0.0
      Hru_ppt = 0.0
      sum_obs = 0.0D0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Rain_day/=0 ) THEN
          nppt = 0
          DO j = 1, Rain_nsta
            k = Rain_nuse(j)
            IF ( Precip(k)>=0.0 ) THEN
              nppt = nppt + 1
              Hru_ppt(i) = Hru_ppt(i) + Precip(k)*Hru_month_ppt(i, Nowmonth)/Psta_month_ppt(k, Nowmonth)
            ENDIF
          ENDDO
          IF ( nppt>1 ) Hru_ppt(i) = Hru_ppt(i)/nppt
        ENDIF

!******Zero precipitation on HRU
        IF ( Hru_ppt(i)<NEARZERO ) THEN
          Hru_ppt(i) = 0.0
        ELSE
          IF ( Precip_units==1 ) ppt = Hru_ppt(i)*MM2INCH
          CALL precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), Tmaxf(i), &
     &                          Tminf(i), Pptmix(i), Newsnow(i), Prmx(i), &
     &                          Tmax_allrain_f(i,Nowmonth), 1.0, 1.0, &
     &                          Adjmix_rain(i,Nowmonth), Hru_area(i), sum_obs, Tmax_allsnow_f(i,Nowmonth))
        ENDIF
      ENDDO
      Basin_ppt = Basin_ppt*Basin_area_inv
      Basin_rain = Basin_rain*Basin_area_inv
      Basin_snow = Basin_snow*Basin_area_inv

      END SUBROUTINE mm_rain_run
