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
      MODULE PRMS_TEMP_1STA
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE, ALLOCATABLE :: Tmax_cnt(:), Tmin_cnt(:), Nuse_tsta(:)
        REAL, SAVE, ALLOCATABLE :: Tcrn(:), Tcrx(:), Elfac(:), Tmax_prev(:), Tmin_prev(:)
        REAL, SAVE :: Solrad_tmax_good, Solrad_tmin_good
        CHARACTER(LEN=9), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE :: Max_missing
        REAL, SAVE, ALLOCATABLE :: Tmax_lapse(:), Tmin_lapse(:)
      END MODULE PRMS_TEMP_1STA

      INTEGER FUNCTION temp_1sta()
      USE PRMS_TEMP_1STA
      USE PRMS_MODULE, ONLY: Process, Nhru, Ntemp
      USE PRMS_BASIN, ONLY: Hru_elev, Start_month, Hru_area, &
     &    Active_hrus, Hru_route_order, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tmax_adj, Tmin_adj, Tsta_elev, &
     &    Hru_tsta, Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, &
     &    Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Basin_tsta, Tmax_allrain
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error, temp_set
! Local Variables
      INTEGER :: j, k, jj, i, kk, kkk, nc
      REAL :: tmaxlaps_mo, tminlaps_mo, tmx, tmn
      CHARACTER(LEN=80), SAVE :: Version_temp_1sta
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Temperature Distribution'
!***********************************************************************
      temp_1sta = 0

      IF ( Process(:3)=='run' ) THEN
        kk = 0
        kkk = 0
        DO i = 1, Ntemp
          k = 0
          IF ( nuse_tsta(i)>0 ) THEN
            IF ( Tmax(i)<-99.0 .OR. Tmax(i)>150.0 ) THEN
              tmax_cnt(i) = tmax_cnt(i) + 1
              IF ( tmax_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmax', Tmax(i), i, Nowyear, Nowmonth, Nowday, tmax_prev(i)
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
            IF ( Tmin(i)<-99.0 .OR. Tmin(i)>150.0 ) THEN
              tmin_cnt(i) = tmin_cnt(i) + 1
              IF ( tmin_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmin', Tmin(i), i, Nowyear, Nowmonth, Nowday, tmin_prev(i)
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
          tmn = Tmin(k)
          tmx = tmx - tcrx(j)
          tmn = tmn - tcrn(j)
          CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j), &
     &                  Tmaxc(j), Tminc(j), Tavgc(j), Hru_area(j))
        ENDDO
        Basin_tmax = Basin_tmax*Basin_area_inv
        Basin_tmin = Basin_tmin*Basin_area_inv
        Basin_temp = Basin_temp*Basin_area_inv
        Solrad_tmax = Tmax(Basin_tsta)
        Solrad_tmin = Tmin(Basin_tsta)
        IF ( Solrad_tmax<-99.0 .OR. Solrad_tmax>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmax:', Solrad_tmax, ' using last valid value'
          PRINT *, 'Value set to', solrad_tmax_good, ' Date:', Nowyear, Nowmonth, Nowday
          Solrad_tmax = Solrad_tmax_good
        ELSE
          Solrad_tmax_good = Solrad_tmax
        ENDIF
        IF ( Solrad_tmin<-99.0 .OR. Solrad_tmin>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmin:', Solrad_tmin, ' using last valid value'
          PRINT *, 'Value set to', solrad_tmin_good, ' Date:', Nowyear, Nowmonth, Nowday
          Solrad_tmin = Solrad_tmin_good
        ELSE
          Solrad_tmin_good = Solrad_tmin
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_temp_1sta = '$Id: temp_1sta.f90 5220 2013-01-14 21:33:43Z rsregan $'
        nc = INDEX( Version_temp_1sta, 'Z' )
        i = INDEX( Version_temp_1sta, '.f90' ) + 3
        IF ( declmodule(Version_temp_1sta(6:i), PROCNAME, Version_temp_1sta(i+2:nc))/=0 ) STOP
        MODNAME = 'temp_1sta'

        ALLOCATE ( Tmax_lapse(12), Tmin_lapse(12) )
        IF ( declparam(MODNAME, 'tmax_lapse', 'nmonths', 'real', &
     &       '3.0', '-10.0', '10.0', &
     &       'Monthly maximum temperature lapse rate', &
     &       'Monthly (January to December) values representing the change in maximum air temperature per 1000 elev_units of'// &
     &       ' elevation change', &
     &       'degrees')/=0 ) CALL read_error(1, 'tmax_lapse')
        IF ( declparam(MODNAME, 'tmin_lapse', 'nmonths', 'real', &
     &       '3.0', '-10.0', '10.0', &
     &       'Monthly minimum temperature lapse rate', &
     &       'Monthly (January to December) values representing the change in minimum air temperture per 1000 elev_units of'// &
     &       ' elevation change', &
     &       'degrees')/=0 ) CALL read_error(1, 'tmin_lapse') 
        IF ( declparam(MODNAME, 'max_missing', 'one', 'integer', &
     &       '3', '0', '10', &
     &       'Maximum number of consecutive missing values allowed for'// &
     &       ' any measured air temperature station; 0 = unlimited', &
     &       'Maximum number of consecutive missing values allowed for'// &
     &       ' any measured air temperature station; missing value set'// &
     &       ' to last valid value; 0 = unlimited', 'none')/=0 ) CALL read_error(1, 'max_missing')

      ELSEIF ( Process(:4)=='init' ) THEN
        ALLOCATE ( Tcrn(Nhru), Tcrx(Nhru), Elfac(Nhru), Nuse_tsta(Ntemp) )
        ALLOCATE ( Tmin_cnt(Ntemp), Tmax_cnt(Ntemp), Tmax_prev(Ntemp), Tmin_prev(Ntemp) )
! Initialize variables, get parameter values, compute elfac
        IF ( getparam(MODNAME, 'tmin_lapse', 12, 'real', Tmin_lapse)/=0 ) CALL read_error(2, 'tmin_lapse')
        IF ( getparam(MODNAME, 'tmax_lapse', 12, 'real', Tmax_lapse)/=0 ) CALL read_error(2, 'tmax_lapse')
        IF ( getparam(MODNAME, 'max_missing', 1, 'integer', Max_missing)/=0 ) CALL read_error(2, 'max_missing')
        IF ( Max_missing==0 ) Max_missing = 3
        Max_missing = Max_missing + 1

        tmaxlaps_mo = Tmax_lapse(Start_month)
        tminlaps_mo = Tmin_lapse(Start_month)
        Nuse_tsta = 0
        DO j = 1, Nhru
          k = Hru_tsta(j)
          Nuse_tsta(k) = 1
          Elfac(j) = (Hru_elev(j) - Tsta_elev(k))/1000.0
          Tcrx(j) = tmaxlaps_mo*elfac(j) - Tmax_adj(j)
          Tcrn(j) = tminlaps_mo*elfac(j) - Tmin_adj(j)
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
     &        '; temperature station:', I5, ' Date:', I5, 2('/', I2.2), /, &
     &        'Value set to last valid value:', F10.3)
 9002 FORMAT (/, 'ERROR, too many bad temperatures, ', A, ':', F10.3, &
     &        '; temperature station:', I3, ' Time:', I5, 2('/', I2.2))

      END FUNCTION temp_1sta
