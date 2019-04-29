!***********************************************************************
! Distributes maximum, minimum, and average temperatures to each HRU by
! computing a daily lapse rate with temperature data measured at
! two stations
!
! Variables needed from DATA FILE: tmax, tmin
!***********************************************************************
      INTEGER FUNCTION temp_laps()
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Version_temp_laps, Temp_laps_nc
      USE PRMS_BASIN, ONLY: Hru_elev, Active_hrus, Hru_area, Hru_route_order, Basin_area_inv, NEARZERO, Starttime
      USE PRMS_CLIMATEVARS, ONLY: Ntemp, Tsta_elev, Hru_tsta, Basin_tsta, &
          Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, Basin_tmin, &
          Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Tmax_adj, Tmin_adj, Tmax_allrain
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error, temp_set
! Declared Parameters
      INTEGER, SAVE :: Max_missing
      INTEGER, SAVE, ALLOCATABLE :: Hru_tlaps(:)
! Local Variables
      INTEGER :: j, k, l, jj, ierr, i, kk, kkk
      REAL :: tmx, tmn, tmxtsta, tmntsta, tmxlaps, tmnlaps, tdiff
      REAL, SAVE, ALLOCATABLE :: elfac(:), tmax_prev(:), tmin_prev(:)
      REAL, SAVE :: solrad_tmax_good, solrad_tmin_good
      INTEGER, SAVE, ALLOCATABLE :: tmax_cnt(:), tmin_cnt(:), nuse_tsta(:)
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='temp_laps')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Temperature Distribution')
!***********************************************************************
      temp_laps = 1

      IF ( Process(:3)=='run' ) THEN
        kk = 0
        kkk = 0
        DO i = 1, Ntemp
          k = 0
          IF ( nuse_tsta(i)>0 ) THEN
            IF ( Tmax(i)<-89.0 .OR. Tmax(i)>150.0 ) THEN
              tmax_cnt(i) = tmax_cnt(i) + 1
              IF ( tmax_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmax', tmx, i, Nowyear, Nowmonth, Nowday, tmax_prev(i)
                Tmax(i) = tmax_prev(i)
                k = 1
                kk = 1
              ELSE
                PRINT 9002, 'tmax', Tmax(i), i, Nowyear, Nowmonth, Nowday
                RETURN
              ENDIF
            ELSE
              tmax_prev(i) = Tmax(i)
              tmax_cnt(i) = 0
            ENDIF
            IF ( Tmin(i)<-89.0 .OR. Tmin(i)>150.0 ) THEN
              tmin_cnt(i) = tmin_cnt(i) + 1
              IF ( tmin_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmin', tmn, i, Nowyear, Nowmonth, Nowday, tmin_prev(i)
                Tmin(i) = tmin_prev(i)
                k = 1
                kkk = 1
              ELSE
                PRINT 9002, 'tmin', Tmin(i), i, Nowyear, Nowmonth, Nowday
                RETURN
              ENDIF
            ELSE
              tmin_prev(i) = Tmin(i)
              tmin_cnt(i) = 0
            ENDIF
          ENDIF
        ENDDO
        ! if all values good, reset _cnt variable
        IF ( kk==0 ) tmax_cnt = 0
        IF ( kkk==0 ) tmin_cnt = 0

        Basin_tmax = 0.0D0
        Basin_tmin = 0.0D0
        Basin_temp = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_tsta(j)
          l = Hru_tlaps(j)
          tmxlaps = Tmax(l)
          tmxtsta = Tmax(k)
          tmnlaps = Tmin(l)
          tmntsta = Tmin(k)
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
        Version_temp_laps = '$Id: temp_laps.f90 4116 2012-01-18 19:45:09Z rsregan $'
        Temp_laps_nc = INDEX( Version_temp_laps, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(MODNAME, PROCNAME, Version_temp_laps(:Temp_laps_nc))/=0 ) STOP
        ENDIF

        ALLOCATE ( Hru_tlaps(Nhru) )
        IF ( declparam(MODNAME, 'hru_tlaps', 'nhru', 'integer', &
             '1', 'bounded', 'ntemp', &
             'Index of lapse temperature station for HRU', &
             'Index of the lapse temperature station used for lapse rate calculations', &
             'none')/=0 ) CALL read_error(1, 'hru_tlaps')
        IF ( declparam(MODNAME, 'max_missing', 'one', 'integer', &
             '3', '0', '10', &
             'Maximum number of consecutive missing values allowed for'// &
             ' any measured air temperature station; 0 = unlimited', &
             'Maximum number of consecutive missing values allowed for'// &
             ' any measured air temperature station; missing value set'// &
             ' to last valid value; 0 = unlimited', 'none')/=0 ) CALL read_error(1, 'max_missing')

      ELSEIF ( Process(:4)=='init' ) THEN
!       Initialize variables, get parameter values, compute elfac
        IF ( getparam(MODNAME, 'hru_tlaps', Nhru, 'integer', Hru_tlaps)/=0 ) CALL read_error(2, 'hru_tlaps') 
        IF ( getparam(MODNAME, 'max_missing', 1, 'integer', Max_missing)/=0 ) CALL read_error(2, 'max_missing')
        IF ( Max_missing==0 ) Max_missing = 3
        Max_missing = Max_missing + 1

        ALLOCATE ( elfac(Nhru), nuse_tsta(Ntemp) )
        ierr = 0
        DO j = 1, Nhru
          IF ( Hru_tlaps(j)<1 .OR. Hru_tlaps(j)>Ntemp ) THEN
            PRINT *, 'ERROR, hru_tlaps=0 or hru_tlaps>ntemp, HRU:', j
            ierr = 1
          ENDIF
          k = Hru_tsta(j)
          l = Hru_tlaps(j)
          nuse_tsta(k) = 1
          tdiff = Tsta_elev(l) - Tsta_elev(k)
          IF ( ABS(tdiff)<NEARZERO ) tdiff = 1.0
          elfac(j) = (Hru_elev(j)-Tsta_elev(k))/tdiff
        ENDDO
        IF ( ierr==1 ) STOP
        solrad_tmax_good = 0.0
        solrad_tmin_good = 0.0

        ALLOCATE ( tmin_cnt(Ntemp), tmax_cnt(Ntemp), tmax_prev(Ntemp), tmin_prev(Ntemp) )
        tmax_cnt = 0
        tmin_cnt = 0
        tmax_prev = Tmax_allrain(Starttime(2))
        tmin_prev = tmax_prev
      ENDIF

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3, &
              '; temperature station:', I5, ' Date:', I5, 2('/', I2.2), /, &
              'Value set to last valid value:', F10.3)
 9002 FORMAT (/, 'ERROR, too many bad temperatures, ', A, ':', F10.3, &
              '; temperature station:', I3, ' Time:', I5, 2('/', I2.2))

      temp_laps = 0
      END FUNCTION temp_laps

