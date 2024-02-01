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
	    use PRMS_CONSTANTS, only: Nmonths
        implicit none
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Temp & Precip Distribution'
        character(len=14), parameter :: MODNAME = 'mm_dist'
        character(len=*), parameter :: Version_mm_dist = '2024-01-31'
        integer, save :: Temp_nsta, Rain_nsta
        integer, save, allocatable :: Temp_nuse(:), Rain_nuse(:)
! Declared Variables 
        integer, save :: Is_rain_day
        double precision, save, allocatable :: Tmax_rain_sta(:), Tmin_rain_sta(:)
! Parameters
        integer, save, allocatable :: Psta_freq_nuse(:), Tsta_nuse(:), Psta_nuse(:)
        double precision, save :: Downscale_adj
        double precision, save, allocatable :: Hru_month_ppt(:, :), Hru_month_max(:, :), Hru_month_min(:, :)
        double precision, save, allocatable :: Rain_month_max(:, :), Rain_month_min(:, :)
        double precision, save, allocatable :: Psta_month_ppt(:, :), Tsta_month_max(:, :), Tsta_month_min(:, :)
        double precision, save, allocatable :: Tmax_allsnow_sta(:, :), Tmax_allrain_sta(:, :)
      END MODULE PRMS_MM_DIST

!***********************************************************************
! Main routine
!***********************************************************************
      integer function mm_dist( )
      use PRMS_CONSTANTS, only: RUN, DECL, INIT
      use PRMS_MODULE, only: Process_flag
      implicit none
! functions
      external :: mmdecl, mminit, mmrun
!***********************************************************************
      mm_dist = 0

      if ( Process_flag == RUN ) then
        call mmrun()
      elseif ( Process_flag == DECL ) then
        call mmdecl()
      elseif ( Process_flag == INIT ) then
        call mminit()
      endif

      end function mm_dist 

!***********************************************************************
! mmdecl - set up parameters for computations
!***********************************************************************
      SUBROUTINE mmdecl( )
      use PRMS_MM_DIST
      use PRMS_MODULE, only: Nhru, Ntemp, Nrain
      implicit none
! functions
      integer, external :: declvar, declparam
      external :: read_error, print_module
!*********************************************************************** 
      call print_module( MODDESC, MODNAME, Version_mm_dist )

! allocate local variables
      ALLOCATE ( Temp_nuse(Ntemp), Rain_nuse(Nrain) )

! declare variables
      if ( declvar(MODNAME, 'is_rain_day', 'one', 1, 'integer', &
           'Flag to indicate if it is raining anywhere in the basin', 'none', Is_rain_day) /= 0 ) &
           call read_error( 3, 'is_rain_day' )

      ALLOCATE ( Tmax_rain_sta(Nrain) )
      if ( declvar(MODNAME, 'tmax_rain_sta', 'nrain', Nrain, 'double', &
           'Maximum temperature distributed to the precipitation stations', &
           'temp_units', Tmax_rain_sta) /= 0 ) CALL read_error( 3, 'tmax_rain_sta' )

      ALLOCATE ( Tmin_rain_sta(Nrain) )
      if ( declvar(MODNAME, 'tmin_rain_sta', 'nrain', Nrain, 'double', &
           'Minimum temperature distributed to the precipitation stations', &
           'temp_units', Tmin_rain_sta) /= 0 ) CALL read_error( 3, 'tmin_rain_sta' )

! declare parameters
      ALLOCATE ( Tsta_nuse(Ntemp) )
      if ( declparam (MODNAME, 'tsta_nuse', 'ntemp', 'integer', &
           '1', '0', '1', &
           '0 = station not used; 1 = station used', &
           'The subset of temperature measurement stations used in the distribution regression'// &
           ' (0=station not used; 1=station used)', &
           'none') /= 0 ) CALL read_error( 2, 'tsta_nuse' )

      ALLOCATE ( Psta_nuse(Nrain) )
      if ( declparam (MODNAME, 'psta_nuse', 'nrain', 'integer', &
           '1', '0', '1', &
           'The subset of precipitation stations used in the distribution regression (0=station not used; 1=station used)', &
           'The subset of precipitation measurement stations used in the distribution regression (0=station not used;'// &
           ' 1=station used)', &
           'none') /= 0 ) CALL read_error( 2, 'psta_nuse' )

      ALLOCATE ( Psta_freq_nuse(Nrain) )
      if ( declparam (MODNAME, 'psta_freq_nuse', 'nrain', 'integer', &
           '1', '0', '1', &
           'The subset of precipitation stations used to determine if there is distribution in the basin (0=station not used;'// &
           ' 1=station used)', &
           'The subset of precipitation measurement stations used to determine if there is precipitation in the basin'// &
           ' (0=station not used; 1=station used)', &
           'none') /= 0 ) CALL read_error( 2, 'psta_freq_nuse' )

      ALLOCATE ( Tsta_month_max(Ntemp, Nmonths) )
      if ( declparam (MODNAME, 'tsta_month_max', 'ntemp,nmonths', 'double', &
           '0.0', '-100.0', '200.0', &
           'Average monthly maximum temperature at each station', &
           'Average monthly maximum temperature at each station', &
           'temp_units') /= 0 ) CALL read_error( 2, 'tsta_month_max' )

      ALLOCATE ( Tsta_month_min(Ntemp, Nmonths) )
      if ( declparam (MODNAME, 'tsta_month_min', 'ntemp,nmonths', 'double', &
           '0.0', '-100.0', '200.0', &
           'Average monthly minimum temperature at each station', &
           'Average monthly minimum temperature at each station', &
           'temp_units') /= 0 ) CALL read_error( 2, 'tsta_month_min' )

      ALLOCATE ( Psta_month_ppt(Nrain, Nmonths) )
      if ( declparam (MODNAME, 'psta_month_ppt', 'nrain,nmonths', 'double', &
           '0.0', '0.0', '200.0', &
           'Average monthly precipitation at each station', &
           'Average monthly precipitation at each station', &
           'precip_units') /= 0 ) CALL read_error( 2, 'psta_month_ppt')

      if ( declparam (MODNAME, 'downscale_adj', 'one', 'double', &
           '0.01', '0.0', '1.0', &
           'Downscaling fraction adjustment', 'Downscaling fraction adjustment', &
           'decimal fraction') /= 0 ) CALL read_error( 2, 'downscale_adj' )

      ALLOCATE ( Hru_month_max(Nhru, Nmonths) )
      if ( declparam(MODNAME, 'hru_month_max', 'nhru,nmonths', 'double', &
           '0.0', '-100.0', '200.0', &
           'Average monthly maximum temperature at each HRU', &
           'Average monthly maximum temperature at each HRU', &
           'temp_units') /= 0 ) CALL read_error( 2, 'hru_month_max' )

      ALLOCATE ( Hru_month_min(Nhru, Nmonths) )
      if ( declparam(MODNAME, 'hru_month_min', 'nhru,nmonths', 'double', &
           '0.0', '-100.0', '200.0', &
           'Average monthly minimum temperature at each HRU', &
           'Average monthly minimum temperature at each HRU', &
           'temp_units') /= 0 ) CALL read_error( 2, 'hru_month_min' )

      ALLOCATE ( Hru_month_ppt(Nhru, Nmonths) )
      if ( declparam(MODNAME, 'hru_month_ppt', 'nhru,nmonths', 'double', &
           '0.0', '0.0', '20.0', &
           'Average monthly precipitation at each HRU', &
           'Average monthly precipitation at each HRU', &
           'precip_units') /= 0 ) CALL read_error( 2, 'hru_month_ppt' )

      ALLOCATE ( Rain_month_max(Nrain, Nmonths) )
      if ( declparam(MODNAME, 'rain_month_max', 'nrain,nmonths', &
           '0.0', '0.0', '20.0', &
           'Average monthly maximum precipitation at each rain station', &
           'Average monthly maximum precipitation at each rain station', &
           'precip_units') /= 0 ) CALL read_error( 2, 'rain_month_max' )

      ALLOCATE ( Rain_month_min(Nrain, Nmonths) )
      if ( declparam(MODNAME, 'rain_month_min', 'nrain,nmonths', 'double', &
           '0.0', '0.0', '20.0', &
           'Average monthly minimum precipitation at each rain station', &
           'Average monthly minimum precipitation at each rain station', &
           'precip_units') /= 0 ) CALL read_error( 2, 'rain_month_min' )

      ALLOCATE ( Tmax_allrain_sta(Nrain, Nmonths) )
      if ( declparam(MODNAME, 'tmax_allrain_sta', 'nrain,nmonths', &
           'double', '38.0', '-8.0', '45.0', &
           'Precipitation is rain if HRU max temperature >= this value', &
           'Monthly (January to December) maximum air temperature'// &
           ' when precipitation is assumed to be rain; if HRU air'// &
           ' temperature is greater than or equal to this value, precipitation is rain', &
           'temp_units') /= 0 ) CALL read_error( 1, 'tmax_allrain_sta' )

      ALLOCATE ( Tmax_allsnow_sta(Nrain, Nmonths) )
      if ( declparam(MODNAME, 'tmax_allsnow_sta', 'nrain,nmonths', &
           'double', '32.0', '-10.0', '40.0', &
           'Maximum temperature when precipitation is all snow', &
           'Maximum air temperature when precipitation is assumed'// &
           ' to be snow; if HRU air temperature is less than or'// &
           ' equal to this value, precipitation is snow', &
           'temp_units') /= 0 ) CALL read_error( 1, 'tmax_allsnow_sta' )

      END SUBROUTINE mmdecl

!***********************************************************************
! mminit - Initialize mm_dist module - get parameter values
!***********************************************************************
      SUBROUTINE mminit()
      use PRMS_MM_DIST
      use PRMS_MODULE, only: Ntemp, Nrain, Inputerror_flag, Nhru_nmonths
      implicit none
! functions
      integer, external :: getparam
      external read_error
! Local variables
      integer :: i, j
!***********************************************************************

      if ( getparam (MODNAME, 'tsta_nuse', Ntemp, 'integer', Tsta_nuse) /= 0 ) CALL read_error( 2, 'tsta_nuse' )
      if ( getparam (MODNAME, 'psta_nuse', Nrain, 'integer', Psta_nuse) /= 0 ) CALL read_error( 2, 'psta_nuse' )
      if ( getparam (MODNAME, 'psta_freq_nuse', Nrain, 'integer', Psta_freq_nuse) /= 0 ) CALL read_error( 2, 'psta_freq_nuse' )
      if ( getparam (MODNAME, 'tsta_month_min', Ntemp*Nmonths, 'double', Tsta_month_min) /= 0 ) &
           call read_error( 2, 'tsta_month_min' )
      if ( getparam (MODNAME, 'tsta_month_max', Ntemp*Nmonths, 'double', Tsta_month_max) /= 0 ) &
           call read_error( 2, 'tsta_month_max' )
      if ( getparam (MODNAME, 'psta_month_ppt', Nrain*Nmonths, 'double', Psta_month_ppt) /= 0 ) &
           call read_error( 2, 'psta_month_ppt' )
      if ( getparam (MODNAME, 'downscale_adj', 1, 'double', Downscale_adj) /= 0 ) CALL read_error( 2, 'downscale_adj' )
      if ( getparam (MODNAME, 'hru_month_min', Nhru_nmonths, 'double', Hru_month_min) /= 0 ) &
           call read_error( 2, 'hru_month_min' )
      if ( getparam (MODNAME, 'hru_month_max', Nhru_nmonths, 'double', Hru_month_max) /= 0 ) &
           call read_error( 2, 'hru_month_max' )
      if ( getparam (MODNAME, 'hru_month_ppt', Nhru_nmonths, 'double', Hru_month_ppt) /= 0 ) &
           call read_error( 2, 'hru_month_ppt' )
      if ( getparam (MODNAME, 'rain_month_max', Nrain*Nmonths, 'double', Rain_month_max) /= 0 ) &
           call read_error( 2, 'rain_month_max' )
      if ( getparam (MODNAME, 'rain_month_min', Nrain*Nmonths, 'double', Rain_month_min) /= 0 ) &
           call read_error( 2, 'rain_month_min' )
      if ( getparam (MODNAME, 'tmax_allrain_sta', Nrain*Nmonths, 'double', Tmax_allrain_sta) /= 0 ) &
           call read_error( 2, 'tmax_allrain_sta' )
      if ( getparam (MODNAME, 'tmax_allsnow_sta', Nrain*Nmonths, 'double', Tmax_allsnow_sta) /= 0 ) &
           call read_error( 2, 'tmax_allsnow_sta' )

      Temp_nsta = 0
      do i = 1, Ntemp
        if ( Tsta_nuse(i )== 1 ) then
          Temp_nsta = Temp_nsta + 1
          Temp_nuse(Temp_nsta) = i
        endif 
      enddo

      Rain_nsta = 0
      do i = 1, Nrain
        if ( Psta_nuse(i) == 1 ) then
          Rain_nsta = Rain_nsta + 1
          Rain_nuse(Rain_nsta) = i
        endif
      enddo

      do j = 1, Nmonths
        do i = 1, Nrain
          if ( Psta_month_ppt(i, j) == 0.0D0 ) then
            PRINT *, 'ERROR, psta_month_ppt cannot be 0.0, precip station:', i, '; month:', j
            Inputerror_flag = 1
          endif
        enddo
      enddo

      DEALLOCATE ( Tsta_nuse, Psta_nuse )

      END SUBROUTINE mminit

!***********************************************************************
! mmrun - Temperature calculation
!***********************************************************************
      SUBROUTINE mmrun( )
      implicit none
      external :: mm_temp_run, mm_rain_run
!*********************************************************************** 

      call mm_temp_run( )

      call mm_rain_run( )

      END SUBROUTINE mmrun 

!***********************************************************************
! mm_temp_run - Temperature calculation
! calculates daily max and min temperature using data from available stations
! Outputs a daily max and min temperature by HRU elevation
!***********************************************************************
      SUBROUTINE mm_temp_run( )
      use PRMS_MM_DIST
      use PRMS_MODULE, only: Nrain, Nowmonth
      use PRMS_BASIN, only: Hru_area_dble, Basin_area_inv, Active_hrus, Hru_route_order
      use PRMS_CLIMATEVARS, only: Solrad_tmax, Solrad_tmin, Basin_tmax, Basin_tmin, &
          Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Basin_tsta, &
          Tmax_aspect_adjust, Tmin_aspect_adjust ! these are read as tmax_adj and tmin_adj in climateflow
      use PRMS_OBS, only: Tmax, Tmin
      implicit none
! functions
      external :: temp_set
      intrinsic :: DBLE, SNGL
! Local variables
      integer :: i, ii, j, k, ntmin, ntmax
      double precision :: x1, tmax_hru, tmin_hru
!*********************************************************************** 
      Solrad_tmax = Tmax(Basin_tsta)
      Solrad_tmin = Tmin(Basin_tsta)

      Basin_tmax = 0.0D0
      Basin_tmin = 0.0D0
      do ii = 1, Active_hrus
        i = Hru_route_order(ii)
        tmax_hru = 0.0D0
        tmin_hru = 0.0D0
        ntmax = 0
        ntmin = 0
        do j = 1, Temp_nsta
          k = Temp_nuse(j)
          if ( Tmax(k) > -55.0D0 ) then
            ntmax = ntmax + 1
            tmax_hru = tmax_hru + Tmax(k) + Hru_month_max(i, Nowmonth) - Tsta_month_max(k, Nowmonth)
          endif
          if ( Tmin(k) > -55.0D0 ) then
            ntmin = ntmin + 1
            tmin_hru = tmin_hru + Tmin(k) + Hru_month_min(i, Nowmonth) - Tsta_month_min(k, Nowmonth)
          endif
        enddo

        if ( ntmax == 0 ) then
          tmax_hru = Hru_month_max(i, Nowmonth)
        else
          tmax_hru = tmax_hru / DBLE( ntmax )
        endif
        if ( ntmin == 0 ) then
          tmin_hru = Hru_month_min(i, Nowmonth)
        else
          tmin_hru = tmin_hru / DBLE( ntmin )
        endif

! Temperature adjustment by HRU
        tmax_hru = tmax_hru + Tmax_aspect_adjust(i, Nowmonth)
        tmin_hru = tmin_hru + Tmin_aspect_adjust(i, Nowmonth)

! IF max is less than min, switch
        if ( tmax_hru < tmin_hru ) then
          x1 = tmax_hru
          tmax_hru = tmin_hru
          tmin_hru = x1
        endif

! Now sort out units and calculate basin_tmax and basin_tmin.
        call temp_set( i, tmax_hru, tmin_hru, Tmaxf(i), Tminf(i), Tavgf(i), &
                      Tmaxc(i), Tminc(i), Tavgc(i), Hru_area_dble(i) )
      enddo
      Basin_tmax = Basin_tmax * Basin_area_inv
      Basin_tmin = Basin_tmin * Basin_area_inv

! Calculate temperatures at precipitation stations.
      do i = 1, Nrain
        Tmax_rain_sta(i) = 0.0D0
        Tmin_rain_sta(i) = 0.0D0
        ntmax = 0
        ntmin = 0
        do j = 1, Temp_nsta
          k = Temp_nuse(j)
          if ( Tmax(k) > -55.0D0 ) then
            ntmax = ntmax + 1
            Tmax_rain_sta(i) = Tmax_rain_sta(i) + Tmax(k) + Rain_month_max(i, Nowmonth) - Tsta_month_max(k, Nowmonth)
          endif
          if ( Tmin(k) > -55.0D0 ) then
            ntmin = ntmin + 1
            Tmin_rain_sta(i) = Tmin_rain_sta(i) + Tmin(k) + Rain_month_min(i, Nowmonth) - Tsta_month_min(k, Nowmonth)
          endif
        enddo

        if ( ntmax == 0 ) then
          Tmax_rain_sta(i) = Hru_month_max(i, Nowmonth)
        else
          Tmax_rain_sta(i) = Tmax_rain_sta(i) / DBLE(ntmin)
        endif
        if ( ntmin == 0 ) then
          Tmin_rain_sta(i) = Hru_month_min(i, Nowmonth)
        else
          Tmin_rain_sta(i) = Tmin_rain_sta(i) / DBLE(ntmin)
        endif

! If max is less than min, switch
        if ( Tmax_rain_sta(i) < Tmin_rain_sta(i) ) then
          x1 = DBLE( Tmax_rain_sta(i) )
          Tmax_rain_sta(i) = Tmin_rain_sta(i)
          Tmin_rain_sta(i) = SNGL( x1 )
        endif
      enddo

      END SUBROUTINE mm_temp_run

! *******************************************************************
! mm_rain_run
! *******************************************************************
      SUBROUTINE mm_rain_run( )
      use PRMS_CONSTANTS, only: MM, MM2INCH
      use PRMS_MM_DIST
      use PRMS_MODULE, only: Nrain, Nowmonth
      use PRMS_BASIN, only: Hru_area_dble, Basin_area_inv, Active_hrus, Hru_route_order
      use PRMS_CLIMATEVARS, only: Tmaxf, Tminf, Newsnow, Pptmix, Precip_units, &
          Prmx, Hru_rain, Hru_snow, Hru_ppt, Basin_rain, Basin_snow, &
          Basin_ppt, Adjmix_rain, Tmax_allrain_f, Tmax_allsnow_f, Ppt_zero_thresh
      use PRMS_OBS, only: Precip, Rain_day, Rain_code
      implicit none
! functions
      INTRINSIC ABS
      external :: precip_form
! Local variables
      integer :: i, ii, j, k, err_chk, nppt, nsta_used
      DOUBLE PRECISION :: sum_obs, ppt
!***********************************************************************
! Code to check the rain_code parameter to determine if it is raining in the basin.

      nsta_used = Nrain
      Is_rain_day = 0
      if ( Rain_code(Nowmonth) == 1 ) then
        nsta_used = Rain_nsta
        do j = 1, Rain_nsta
          i = Rain_nuse(j)
          if ( Precip(i) > 0.0D0 ) Is_rain_day = 1
        enddo
      elseif ( Rain_code(Nowmonth) == 2 ) then
        do i = 1, Nrain
          if ( Precip(i) > 0.0D0 ) Is_rain_day = 1
        enddo
      elseif ( Rain_code(Nowmonth) == 3 ) then
        Is_rain_day = 1
      elseif ( Rain_code(Nowmonth) == 4 ) then
        if ( Rain_day == 1 ) Is_rain_day = 1
      elseif (Rain_code(Nowmonth) == 5 ) then
        do i = 1, Nrain
          if ( Psta_freq_nuse(i) == 1 ) then
            if ( Precip(i) > 0.0D0 ) Is_rain_day = 1
          endif
        enddo
      endif

! Add error here
      do j = 1, nsta_used
        if ( Rain_code(Nowmonth)==1 ) then
          i = Rain_nuse(j)
        else
          i = j
        endif
        if ( Precip(i)<0.0D0 ) CYCLE
        err_chk = 0
        if ( Tmax_rain_sta(i)<=Tmax_allsnow_sta(i,Nowmonth) ) then
          err_chk = 1
        elseif ( Tmin_rain_sta(i)>Tmax_allsnow_sta(i,Nowmonth) .OR. Tmax_rain_sta(i)>=Tmax_allrain_sta(i,Nowmonth) ) then
          err_chk = 0
        else
          err_chk = 1
        endif
        if ( err_chk==1 ) Precip(i) = Precip(i)*Downscale_adj + Precip(i)
      enddo

      Basin_ppt = 0.0D0
      Basin_rain = 0.0D0
      Basin_snow = 0.0D0
!******Initialize HRU variables
      Pptmix = 0
      Newsnow = 0
      Prmx = 0.0D0
      Hru_rain = 0.0D0
      Hru_snow = 0.0D0
      Hru_ppt = 0.0D0
      sum_obs = 0.0D0
      do ii = 1, Active_hrus
        i = Hru_route_order(ii)
        if ( Is_rain_day /= 0 ) then
          nppt = 0
          do j = 1, Rain_nsta
            k = Rain_nuse(j)
            if ( Precip(k) >= 0.0D0 ) then
              nppt = nppt + 1
              Hru_ppt(i) = Hru_ppt(i) + Precip(k)*Hru_month_ppt(i, Nowmonth)/Psta_month_ppt(k, Nowmonth)
            endif
          enddo
          if ( nppt > 1 ) Hru_ppt(i) = Hru_ppt(i) / DBLE(nppt)
        endif

!******Zero precipitation on HRU
        if ( Hru_ppt(i) < Ppt_zero_thresh ) then
          Hru_ppt(i) = 0.0D0
        else
          if ( Precip_units == MM ) then
            ppt = Hru_ppt(i) * MM2INCH
          else
            ppt = Hru_ppt(i)
          endif
          call precip_form( ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), Tmaxf(i), &
                            Tminf(i), Pptmix(i), Newsnow(i), Prmx(i), &
                            Tmax_allrain_f(i,Nowmonth), 1.0, 1.0, &
                            Adjmix_rain(i,Nowmonth), Hru_area_dble(i), sum_obs, Tmax_allsnow_f(i,Nowmonth), i )
        endif
      enddo
      Basin_ppt = Basin_ppt * Basin_area_inv
      Basin_rain = Basin_rain * Basin_area_inv
      Basin_snow = Basin_snow * Basin_area_inv

      END SUBROUTINE mm_rain_run
