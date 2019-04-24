!***********************************************************************
! Distributes maximum, minimum, and average temperatures to each HRU by
! computing a daily lapse rate with temperature data measured at
! two stations
!
! Variables needed from DATA FILE: tmax, tmin
!***********************************************************************
      INTEGER FUNCTION temp_laps()
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Version_temp_laps, Temp_laps_nc
      USE PRMS_BASIN, ONLY: Hru_elev, Active_hrus, Hru_area, Hru_route_order, Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Tsta_elev, Hru_tsta, Basin_tsta, &
          Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, Basin_tmin, &
          Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Tmax_adj, Tmin_adj
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error, temp_set
! Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_tlaps(:)
! Local Variables
      INTEGER :: j, k, l, jj, ierr
      REAL :: tmx, tmn, tmxtsta, tmntsta, tmxlaps, tmnlaps, tdiff
      REAL, SAVE, ALLOCATABLE :: elfac(:), tmax_prev(:), tmin_prev(:)
      REAL, SAVE :: solrad_tmax_good, solrad_tmin_good
!***********************************************************************
      temp_laps = 1

      IF ( Process(:3)=='run' ) THEN
        Basin_tmax = 0.0D0
        Basin_tmin = 0.0D0
        Basin_temp = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_tsta(j)
          l = Hru_tlaps(j)

          tmxlaps = Tmax(l)
          IF ( tmxlaps<-89.0 .OR. tmxlaps>150.0 ) THEN
            PRINT 9001, 'tmax', tmxlaps, l, Nowyear, Nowmonth, Nowday, tmax_prev(l)
            tmxlaps = tmax_prev(l)
          ELSE
            tmax_prev(l) = tmxlaps
          ENDIF

          tmxtsta = Tmax(k)
          IF ( tmxtsta<-89.0 .OR. tmxtsta>150.0 ) THEN
            PRINT 9001, 'tmax', tmxtsta, k, Nowyear, Nowmonth, Nowday, tmax_prev(k)
            tmxtsta = tmax_prev(k)
          ELSE
            tmax_prev(k) = tmxtsta
          ENDIF

          tmnlaps = Tmin(l)
          IF ( tmnlaps<-89.0 .OR. tmnlaps>150.0 ) THEN
            PRINT 9001, 'tmin', tmnlaps, l, Nowyear, Nowmonth, Nowday, tmin_prev(l)
            tmnlaps = tmin_prev(l)
          ELSE
            tmin_prev(l) = tmnlaps
          ENDIF

          tmntsta = Tmin(k)
          IF ( tmntsta<-89.0 .OR. tmntsta>150.0 ) THEN
            PRINT 9001, 'tmin', tmntsta, k, Nowyear, Nowmonth, Nowday, tmin_prev(k)
            tmntsta = tmin_prev(k)
          ELSE
            tmin_prev(k) = tmntsta
          ENDIF

          tmx = tmxtsta + (tmxlaps - tmxtsta)*elfac(j) + Tmax_adj(j)
          tmn = tmntsta + (tmnlaps - tmntsta)*elfac(j) + Tmin_adj(j)
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
        Version_temp_laps = '$Id: temp_laps.f90 3815 2011-10-25 18:33:55Z rsregan $'
        Temp_laps_nc = INDEX( Version_temp_laps, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_temp_laps(:Temp_laps_nc))/=0 ) STOP
        ENDIF

        ALLOCATE ( Hru_tlaps(Nhru) )
        IF ( declparam('temp', 'hru_tlaps', 'nhru', 'integer', &
             '1', 'bounded', 'ntemp', &
             'Index of lapse temperature station for HRU', &
             'Index of the lapse temperature station used for lapse rate calculations', &
             'none')/=0 ) CALL read_error(1, 'hru_tlaps')

      ELSEIF ( Process(:4)=='init' ) THEN
!       Initialize variables, get parameter values, compute elfac
        IF ( getparam('temp', 'hru_tlaps', Nhru, 'integer', Hru_tlaps)/=0 ) CALL read_error(2, 'hru_tlaps') 

        ALLOCATE ( tmax_prev(Ntemp), tmin_prev(Ntemp), elfac(Nhru) )
        ierr = 0
        DO j = 1, Nhru
          IF ( Hru_tlaps(j)<1 .OR. Hru_tlaps(j)>Ntemp ) THEN
            PRINT *, 'ERROR, hru_tlaps=0 or hru_tlaps>ntemp, HRU:', j
            ierr = 1
          ENDIF
          k = Hru_tsta(j)
          l = Hru_tlaps(j)
          tdiff = Tsta_elev(l) - Tsta_elev(k)
          IF ( ABS(tdiff)<NEARZERO ) tdiff = 1.0
          elfac(j) = (Hru_elev(j)-Tsta_elev(k))/tdiff
        ENDDO
        IF ( ierr==1 ) STOP
        tmax_prev = 0.0
        tmin_prev = 0.0
        solrad_tmax_good = 0.0
        solrad_tmin_good = 0.0
      ENDIF

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3, &
              '; temperature station:', I5, ' Date:', I5, 2('/', I2.2), /, &
              'Value set to last valid value:', F10.3)

      temp_laps = 0
      END FUNCTION temp_laps

