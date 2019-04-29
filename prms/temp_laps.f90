!***********************************************************************
! Distributes maximum, minimum, and average temperatures to each HRU by
! computing a daily lapse rate with temperature data measured at
! two stations
!
! Variables needed from DATA FILE: tmax, tmin
!***********************************************************************
      MODULE PRMS_TEMP_LAPS
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE, ALLOCATABLE :: Tmax_cnt(:), Tmin_cnt(:), Nuse_tsta(:)
        REAL, SAVE, ALLOCATABLE :: Elfac(:), Tmax_prev(:), Tmin_prev(:)
        REAL, SAVE :: Solrad_tmax_good, Solrad_tmin_good
        CHARACTER(LEN=9), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE :: Max_missing
        INTEGER, SAVE, ALLOCATABLE :: Hru_tlaps(:)
      END MODULE PRMS_TEMP_LAPS

      INTEGER FUNCTION temp_laps()
      USE PRMS_TEMP_LAPS
      USE PRMS_MODULE, ONLY: Process, Nhru, Ntemp, Inputerror_flag
      USE PRMS_BASIN, ONLY: Hru_elev, Active_hrus, Hru_area, Hru_route_order, Basin_area_inv, NEARZERO, Start_month
      USE PRMS_CLIMATEVARS, ONLY: Tsta_elev, Hru_tsta, Basin_tsta, &
          Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, Basin_tmin, &
          Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Tmax_adj, Tmin_adj, Tmax_allrain
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error, temp_set
! Local Variables
      INTEGER :: j, k, l, jj, i, kk, kkk, nc
      REAL :: tmx, tmn, tmxtsta, tmntsta, tmxlaps, tmnlaps, tdiff
      CHARACTER(LEN=80), SAVE :: Version_temp_laps
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Temperature Distribution'
!***********************************************************************
      temp_laps = 0

      IF ( Process(:3)=='run' ) THEN
        kk = 0
        kkk = 0
        DO i = 1, Ntemp
          k = 0
          IF ( Nuse_tsta(i)>0 ) THEN
            IF ( Tmax(i)<-99.0 .OR. Tmax(i)>150.0 ) THEN
              Tmax_cnt(i) = Tmax_cnt(i) + 1
              IF ( Tmax_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmax', Tmax(i), i, Nowyear, Nowmonth, Nowday, Tmax_prev(i)
                Tmax(i) = Tmax_prev(i)
                k = 1
                kk = 1
              ELSE
                PRINT 9002, 'tmax', Tmax(i), i, Nowyear, Nowmonth, Nowday
                RETURN
              ENDIF
            ELSE
              Tmax_prev(i) = Tmax(i)
              Tmax_cnt(i) = 0
            ENDIF
            IF ( Tmin(i)<-99.0 .OR. Tmin(i)>150.0 ) THEN
              Tmin_cnt(i) = Tmin_cnt(i) + 1
              IF ( Tmin_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmin', Tmin(i), i, Nowyear, Nowmonth, Nowday, Tmin_prev(i)
                Tmin(i) = Tmin_prev(i)
                k = 1
                kkk = 1
              ELSE
                PRINT 9002, 'tmin', Tmin(i), i, Nowyear, Nowmonth, Nowday
                RETURN
              ENDIF
            ELSE
              Tmin_prev(i) = Tmin(i)
              Tmin_cnt(i) = 0
            ENDIF
          ENDIF
        ENDDO
        ! if all values good, reset _cnt variable
        IF ( kk==0 ) Tmax_cnt = 0
        IF ( kkk==0 ) Tmin_cnt = 0

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
          tmx = tmxtsta + (tmxlaps - tmxtsta)*Elfac(j) + Tmax_adj(j)
          tmn = tmntsta + (tmnlaps - tmntsta)*Elfac(j) + Tmin_adj(j)
          CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j), &
                        Tmaxc(j), Tminc(j), Tavgc(j), Hru_area(j))
        ENDDO
        Basin_tmax = Basin_tmax*Basin_area_inv
        Basin_tmin = Basin_tmin*Basin_area_inv
        Basin_temp = Basin_temp*Basin_area_inv
        Solrad_tmax = Tmax(Basin_tsta)
        Solrad_tmin = Tmin(Basin_tsta)
        IF ( Solrad_tmax<-99.0 .OR. Solrad_tmax>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmax:', Solrad_tmax, ' using last valid value'
          PRINT *, 'Value set to', Solrad_tmax_good, ' Date:', Nowyear, Nowmonth, Nowday
          Solrad_tmax = Solrad_tmax_good
        ELSE
          Solrad_tmax_good = Solrad_tmax
        ENDIF
        IF ( Solrad_tmin<-99.0 .OR. Solrad_tmin>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmin:', Solrad_tmin, ' using last valid value'
          PRINT *, 'Value set to', Solrad_tmin_good, ' Date:', Nowyear, Nowmonth, Nowday
          Solrad_tmin = Solrad_tmin_good
        ELSE
          Solrad_tmin_good = Solrad_tmin
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_temp_laps = '$Id: temp_laps.f90 5169 2012-12-28 23:51:03Z rsregan $'
        nc = INDEX( Version_temp_laps, 'Z' )
        i = INDEX( Version_temp_laps, '.f90' ) + 3
        IF ( declmodule(Version_temp_laps(6:i), PROCNAME, Version_temp_laps(i+2:nc))/=0 ) STOP
        MODNAME = 'temp_laps'

        ALLOCATE ( Hru_tlaps(Nhru), Elfac(Nhru), Nuse_tsta(Ntemp) )
        ALLOCATE ( Tmin_cnt(Ntemp), Tmax_cnt(Ntemp), Tmax_prev(Ntemp), Tmin_prev(Ntemp) )
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
        ! Initialize variables, get parameter values, compute Elfac
        IF ( getparam(MODNAME, 'hru_tlaps', Nhru, 'integer', Hru_tlaps)/=0 ) CALL read_error(2, 'hru_tlaps') 
        IF ( getparam(MODNAME, 'max_missing', 1, 'integer', Max_missing)/=0 ) CALL read_error(2, 'max_missing')
        IF ( Max_missing==0 ) Max_missing = 3
        Max_missing = Max_missing + 1

        DO j = 1, Nhru
          IF ( Hru_tlaps(j)<1 .OR. Hru_tlaps(j)>Ntemp ) THEN
            PRINT *, 'ERROR, hru_tlaps = 0 or > ntemp, HRU:', j, ', hru_tlaps:', Hru_tlaps(j), &
      &              ', ntemp:', Ntemp
            Inputerror_flag = 1
            CYCLE
          ENDIF
          k = Hru_tsta(j)
          l = Hru_tlaps(j)
          Nuse_tsta(k) = 1
          tdiff = Tsta_elev(l) - Tsta_elev(k)
          IF ( ABS(tdiff)<NEARZERO ) tdiff = 1.0
          Elfac(j) = (Hru_elev(j)-Tsta_elev(k))/tdiff
        ENDDO
        Solrad_tmax_good = Solrad_tmax
        Solrad_tmin_good = Solrad_tmin

        Tmax_cnt = 0
        Tmin_cnt = 0
        DO i = 1, Ntemp
          Tmax_prev(i) = Tmax_allrain(Start_month)
          Tmin_prev(i) = Tmax_prev(i)
        ENDDO
      ENDIF

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3, &
              '; temperature station:', I5, ' Date:', I5, 2('/', I2.2), /, &
              'Value set to last valid value:', F10.3)
 9002 FORMAT (/, 'ERROR, too many bad temperatures, ', A, ':', F10.3, &
              '; temperature station:', I3, ' Time:', I5, 2('/', I2.2))

      END FUNCTION temp_laps
