!***********************************************************************
! Distributes maximum and minimum temperatures to each HRU using
! temperature data measured at one station and an estimated monthly
! lapse rate
!
! Variables needed from DATA FILE: tmax, tmin
! Declared Parameters
!     tmax_lapse, tmin_lapse, tsta_elev, tmax_adj, tmin_adj
!     hru_tsta, hru_elev, hru_area, temp_units, basin_tsta
!***********************************************************************
      INTEGER FUNCTION temp_1sta()
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru, Version_temp_1sta, Temp_1sta_nc
      USE PRMS_BASIN, ONLY: Hru_elev, Starttime, Hru_area, &
          Active_hrus, Hru_route_order, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tmax_adj, Tmin_adj, Tsta_elev, Ntemp, &
          Hru_tsta, Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, &
          Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Basin_tsta
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error, temp_set
! Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Tmax_lapse(:), Tmin_lapse(:)
! Local Variables
      INTEGER :: j, k, jj
      REAL :: tmaxlaps_mo, tminlaps_mo, tmx, tmn
      REAL, SAVE, ALLOCATABLE :: tcrn(:), tcrx(:), elfac(:), tmax_prev(:), tmin_prev(:)
      REAL, SAVE :: solrad_tmax_good, solrad_tmin_good
!***********************************************************************
      temp_1sta = 1

      IF ( Process(:3)=='run' ) THEN
        Basin_tmax = 0.0D0
        Basin_tmin = 0.0D0
        Basin_temp = 0.0D0
        tmaxlaps_mo = Tmax_lapse(Nowmonth)
        tminlaps_mo = Tmin_lapse(Nowmonth)
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_tsta(j)
          IF ( Nowday==1 ) THEN
            tcrx(j) = tmaxlaps_mo*elfac(j) - Tmax_adj(j)
            tcrn(j) = tminlaps_mo*elfac(j) - Tmin_adj(j)
          ENDIF

          tmx = Tmax(k)
          IF ( tmx<-89.0 .OR. tmx>150.0 ) THEN
            PRINT 9001, 'tmax', tmx, k, Nowyear, Nowmonth, Nowday, tmax_prev(k)
            tmx = tmax_prev(k)
          ELSE
            tmax_prev(k) = tmx
          ENDIF

          tmn = Tmin(k)
          IF ( tmn<-50.0 .OR. tmn>150.0 ) THEN
            PRINT 9001, 'tmin', tmn, k, Nowyear, Nowmonth, Nowday, tmin_prev(k)
            tmn = tmin_prev(k)
          ELSE
            tmin_prev(k) = tmn
          ENDIF

          tmx = tmx - tcrx(j)
          tmn = tmn - tcrn(j)
          CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j), &
                        Tmaxc(j), Tminc(j), Tavgc(j), Hru_area(j))
        ENDDO
        Basin_tmax = Basin_tmax*Basin_area_inv
        Basin_tmin = Basin_tmin*Basin_area_inv
        Basin_temp = Basin_temp*Basin_area_inv
        Solrad_tmax = Tmax(Basin_tsta)
        Solrad_tmin = Tmin(Basin_tsta)
        IF ( Solrad_tmax<-89.0 .OR. Solrad_tmax>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmax:', Solrad_tmax, ' using last valid value'
          PRINT *, 'Value set to', solrad_tmax_good, ' Date:', Nowyear, Nowmonth, Nowday
          Solrad_tmax = solrad_tmax_good
        ELSE
          solrad_tmax_good = Solrad_tmax
        ENDIF
        IF ( Solrad_tmin<-89.0 .OR. Solrad_tmin>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmin:', Solrad_tmin, ' using last valid value'
          PRINT *, 'Value set to', solrad_tmin_good, ' Date:', Nowyear, Nowmonth, Nowday
          Solrad_tmin = solrad_tmin_good
        ELSE
          solrad_tmin_good = Solrad_tmin
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_temp_1sta = '$Id: temp_1sta.f90 3804 2011-10-25 17:12:19Z rsregan $'
        Temp_1sta_nc = INDEX( Version_temp_1sta, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_temp_1sta(:Temp_1sta_nc))/=0 ) STOP
        ENDIF

        ALLOCATE ( Tmax_lapse(12) )
        IF ( declparam('temp', 'tmax_lapse', 'nmonths', 'real', &
             '3.0', '-10.0', '10.0', &
             'Monthly maximum temperature lapse rate', &
             'Monthly (January to December) values representing the change in maximum air temperature per 1000 elev_units of'// &
             ' elevation change', &
             'degrees')/=0 ) CALL read_error(1, 'tmax_lapse')
        ALLOCATE ( Tmin_lapse(12) )
        IF ( declparam('temp', 'tmin_lapse', 'nmonths', 'real', &
             '3.0', '-10.0', '10.0', &
             'Monthly minimum temperature lapse rate', &
             'Monthly (January to December) values representing the change in minimum air temperture per 1000 elev_units of'// &
             ' elevation change', &
             'degrees')/=0 ) CALL read_error(1, 'tmin_lapse') 

      ELSEIF ( Process(:4)=='init' ) THEN
! Initialize variables, get parameter values, compute elfac
        IF ( getparam('temp', 'tmin_lapse', 12, 'real', Tmin_lapse)/=0 ) CALL read_error(2, 'tmin_lapse')
        IF ( getparam('temp', 'tmax_lapse', 12, 'real', Tmax_lapse)/=0 ) CALL read_error(2, 'tmax_lapse')

        ALLOCATE ( tcrn(Nhru), tcrx(Nhru), elfac(Nhru) )
        tmaxlaps_mo = Tmax_lapse(Starttime(2))
        tminlaps_mo = Tmin_lapse(Starttime(2))
        DO j = 1, Nhru
          k = Hru_tsta(j)
          elfac(j) = (Hru_elev(j) - Tsta_elev(k))/1000.0
          tcrx(j) = tmaxlaps_mo*elfac(j) - Tmax_adj(j)
          tcrn(j) = tminlaps_mo*elfac(j) - Tmin_adj(j)
        ENDDO
        ALLOCATE ( tmax_prev(Ntemp), tmin_prev(Ntemp) )
        tmax_prev = 0.0
        tmin_prev = 0.0
        solrad_tmax_good = 0.0
        solrad_tmin_good = 0.0
      ENDIF

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3, &
              '; temperature station:', I5, ' Date:', I5, 2('/', I2.2), /, &
              'Value set to last valid value:', F10.3)

      temp_1sta = 0
      END FUNCTION temp_1sta

