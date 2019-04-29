!***********************************************************************
! Distributes maximum, minimum, and average temperatures to each HRU
! using temperature data measured at one station and an estimated monthly
! lapse rate (temp_1sta) or by computing a daily lapse rate with
! temperature data measured at two stations (temp_laps)
!
! Variables needed from DATA FILE: tmax, tmin
!***********************************************************************
      MODULE PRMS_TEMP_1STA_LAPS
        IMPLICIT NONE
        ! Local Variables
        INTEGER, SAVE, ALLOCATABLE :: Tmax_cnt(:), Tmin_cnt(:), Nuse_tsta(:)
        REAL, SAVE, ALLOCATABLE :: Elfac(:), Tmax_prev(:), Tmin_prev(:)
        REAL, SAVE, ALLOCATABLE :: Tcrn(:), Tcrx(:) ! temp_1sta
        REAL, SAVE :: Solrad_tmax_good, Solrad_tmin_good
        CHARACTER(LEN=9), SAVE :: MODNAME
        ! Declared Parameters
        INTEGER, SAVE :: Max_missing
        ! Declared Parameters for temp_1sta
        REAL, SAVE :: Tmax_lapse(12), Tmin_lapse(12)
        ! Declared Parameters for temp_laps
        INTEGER, SAVE, ALLOCATABLE :: Hru_tlaps(:)
        ! Declared Parameters from climateflow and basin
        !     both: hru_tsta, hru_area, temp_units, basin_tsta, tsta_elev,
        !           hru_elev, hru_type, tmax_adj, tmin_adj
      END MODULE PRMS_TEMP_1STA_LAPS

      INTEGER FUNCTION temp_1sta_laps()
      USE PRMS_TEMP_1STA_LAPS
      USE PRMS_MODULE, ONLY: Process, Nhru, Ntemp, Save_vars_to_file, Inputerror_flag, Temp_flag
      USE PRMS_BASIN, ONLY: Hru_elev, Active_hrus, Hru_area, Hru_route_order, Basin_area_inv, &
     &    NEARZERO, Start_month, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Tsta_elev, Hru_tsta, Basin_tsta, &
     &    Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, Basin_tmin, &
     &    Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Tmax_adj, Tmin_adj, Tmax_allrain
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, temp_set, print_module, temp_1sta_laps_restart, compute_temp_laps
! Local Variables
      INTEGER :: j, k, l, jj, i, kk, kkk
      REAL :: tmx, tmn, tmxtsta, tmntsta, tmxlaps, tmnlaps, tmaxlaps_mo, tminlaps_mo
      CHARACTER(LEN=80), SAVE :: Version_temp_1sta_laps
!***********************************************************************
      temp_1sta_laps = 0

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
        IF ( Temp_flag==1 ) THEN
          tmaxlaps_mo = Tmax_lapse(Nowmonth)
          tminlaps_mo = Tmin_lapse(Nowmonth)
        ENDIF
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          k = Hru_tsta(j)
          tmxtsta = Tmax(k)
          tmntsta = Tmin(k)
          IF ( Temp_flag==1 ) THEN
            IF ( Nowday==1 ) THEN
              Tcrx(j) = tmaxlaps_mo*Elfac(j) - Tmax_adj(j)
              Tcrn(j) = tminlaps_mo*Elfac(j) - Tmin_adj(j)
            ENDIF
            tmx = tmxtsta - Tcrx(j)
            tmn = tmntsta - Tcrn(j)
          ELSE
            l = Hru_tlaps(j)
            tmxlaps = Tmax(l)
            tmnlaps = Tmin(l)
            tmx = tmxtsta + (tmxlaps - tmxtsta)*Elfac(j) + Tmax_adj(j)
            tmn = tmntsta + (tmnlaps - tmntsta)*Elfac(j) + Tmin_adj(j)
          ENDIF
          CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j), Tmaxc(j), Tminc(j), Tavgc(j), Hru_area(j))
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
        Version_temp_1sta_laps = '$Id: temp_1sta_laps.f90 5593 2013-04-23 18:28:48Z rsregan $'
        IF ( Temp_flag==1 ) THEN
          MODNAME = 'temp_1sta'
          Version_temp_1sta_laps = Version_temp_1sta_laps(:14)//Version_temp_1sta_laps(20:80)
          ALLOCATE ( Tcrn(Nhru), Tcrx(Nhru) )
        ELSE
          MODNAME = 'temp_laps'
          Version_temp_1sta_laps = Version_temp_1sta_laps(:9)//Version_temp_1sta_laps(15:80)
          ALLOCATE ( Hru_tlaps(Nhru) )
        ENDIF

        ALLOCATE ( Elfac(Nhru), Nuse_tsta(Ntemp) )
        ALLOCATE ( Tmin_cnt(Ntemp), Tmax_cnt(Ntemp), Tmax_prev(Ntemp), Tmin_prev(Ntemp) )

        IF ( Timestep/=0 ) RETURN

        IF ( Temp_flag==1 ) THEN
          IF ( declparam(MODNAME, 'tmax_lapse', 'nmonths', 'real', &
     &         '3.0', '-10.0', '10.0', &
     &         'Monthly maximum temperature lapse rate', &
     &         'Monthly (January to December) values representing the change in maximum air temperature per 1000 elev_units of'// &
     &         ' elevation change', &
     &         'degrees')/=0 ) CALL read_error(1, 'tmax_lapse')
          IF ( declparam(MODNAME, 'tmin_lapse', 'nmonths', 'real', &
     &         '3.0', '-10.0', '10.0', &
     &         'Monthly minimum temperature lapse rate', &
     &         'Monthly (January to December) values representing the change in minimum air temperture per 1000 elev_units of'// &
     &         ' elevation change', &
     &         'degrees')/=0 ) CALL read_error(1, 'tmin_lapse')
        ELSE
          IF ( declparam(MODNAME, 'hru_tlaps', 'nhru', 'integer', &
     &         '1', 'bounded', 'ntemp', &
     &         'Index of lapse temperature station for HRU', &
     &         'Index of the lapse temperature station used for lapse rate calculations', &
     &         'none')/=0 ) CALL read_error(1, 'hru_tlaps')
        ENDIF
        IF ( declparam(MODNAME, 'max_missing', 'one', 'integer', &
     &       '3', '0', '10', &
     &       'Maximum number of consecutive missing values allowed for any measured air temperature station; 0 = unlimited', &
     &       'Maximum number of consecutive missing values allowed for'// &
     &       ' any measured air temperature station; missing value set to last valid value; 0 = unlimited', &
     &       'none')/=0 ) CALL read_error(1, 'max_missing')


      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL temp_1sta_laps_restart(1)
          RETURN
        ENDIF

        ! Initialize variables, get parameter values, compute Elfac
        IF ( Temp_flag==1 ) THEN
          IF ( getparam(MODNAME, 'tmin_lapse', 12, 'real', Tmin_lapse)/=0 ) CALL read_error(2, 'tmin_lapse')
          IF ( getparam(MODNAME, 'tmax_lapse', 12, 'real', Tmax_lapse)/=0 ) CALL read_error(2, 'tmax_lapse')
        ELSE
          IF ( getparam(MODNAME, 'hru_tlaps', Nhru, 'integer', Hru_tlaps)/=0 ) CALL read_error(2, 'hru_tlaps') 
        ENDIF
        IF ( getparam(MODNAME, 'max_missing', 1, 'integer', Max_missing)/=0 ) CALL read_error(2, 'max_missing')
        IF ( Max_missing==0 ) Max_missing = 3
        Max_missing = Max_missing + 1

        Nuse_tsta = 0
        Elfac = 0.0
        IF ( Temp_flag==1 ) THEN
          tmaxlaps_mo = Tmax_lapse(Start_month)
          tminlaps_mo = Tmin_lapse(Start_month)
          Tcrx = 0.0
          Tcrn = 0.0
        ENDIF
        DO i = 1, Active_hrus
          j = Hru_route_order(i)
          k = Hru_tsta(j)
          Nuse_tsta(k) = 1
          IF ( Temp_flag==1 ) THEN
            Elfac(j) = (Hru_elev(j) - Tsta_elev(k))/1000.0
            Tcrx(j) = tmaxlaps_mo*Elfac(j) - Tmax_adj(j)
            Tcrn(j) = tminlaps_mo*Elfac(j) - Tmin_adj(j)
          ELSE
            IF ( Hru_tlaps(j)<1 .OR. Hru_tlaps(j)>Ntemp ) THEN
              PRINT *, 'ERROR, hru_tlaps = 0 or > ntemp, HRU:', j, ', hru_tlaps:', Hru_tlaps(j), ', ntemp:', Ntemp
              Inputerror_flag = 1
              CYCLE
            ENDIF
            l = Hru_tlaps(j)
            CALL compute_temp_laps(Elfac(j), Hru_elev(j), Tsta_elev(l), Tsta_elev(k))
          ENDIF
        ENDDO
        Solrad_tmax_good = Solrad_tmax
        Solrad_tmin_good = Solrad_tmin

        Tmax_cnt = 0
        Tmin_cnt = 0
        DO i = 1, Ntemp
          Tmax_prev(i) = Tmax_allrain(Start_month)
          Tmin_prev(i) = Tmax_prev(i)
        ENDDO

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL temp_1sta_laps_restart(0)
      ENDIF

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3, &
     &        '; temperature station:', I5, ' Date:', I5, 2('/', I2.2), /, 'Value set to last valid value:', F10.3)
 9002 FORMAT (/, 'ERROR, too many bad temperatures, ', A, ':', F10.3, '; temperature station:', I3, ' Time:', I5, 2('/', I2.2))

      END FUNCTION temp_1sta_laps

!***********************************************************************
!     Compute lapse rate for an HRU
!***********************************************************************
      SUBROUTINE compute_temp_laps(Elfac, Hru_elev, Tsta_elev_laps, Tsta_elev_base)
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Hru_elev, Tsta_elev_laps, Tsta_elev_base
      REAL, INTENT(OUT) :: Elfac
! Functions
      INTRINSIC ABS
! Local Variables
      REAL :: tdiff
!***********************************************************************
      tdiff = Tsta_elev_laps - Tsta_elev_base
      IF ( ABS(tdiff)<NEARZERO ) tdiff = 1.0
      Elfac = (Hru_elev-Tsta_elev_base)/tdiff
      END SUBROUTINE compute_temp_laps

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE temp_1sta_laps_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Temp_flag
      USE PRMS_TEMP_1STA_LAPS
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=9) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Solrad_tmax_good, Solrad_tmin_good, Max_missing
        IF ( Temp_flag==1 ) THEN
          WRITE ( Restart_outunit ) Tmax_lapse
          WRITE ( Restart_outunit ) Tmin_lapse
          WRITE ( Restart_outunit ) Tcrx
          WRITE ( Restart_outunit ) Tcrn
        ELSE
          WRITE ( Restart_outunit ) Hru_tlaps
        ENDIF
        WRITE ( Restart_outunit ) Tmax_cnt
        WRITE ( Restart_outunit ) Tmin_cnt
        WRITE ( Restart_outunit ) Elfac
        WRITE ( Restart_outunit ) Tmax_prev
        WRITE ( Restart_outunit ) Tmin_prev
        WRITE ( Restart_outunit ) Nuse_tsta
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Solrad_tmax_good, Solrad_tmin_good, Max_missing
        IF ( Temp_flag==1 ) THEN
          READ ( Restart_inunit ) Tmax_lapse
          READ ( Restart_inunit ) Tmin_lapse
          READ ( Restart_inunit ) Tcrx
          READ ( Restart_inunit ) Tcrn
        ELSE
          READ ( Restart_inunit ) Hru_tlaps
        ENDIF
        READ ( Restart_inunit ) Tmax_cnt
        READ ( Restart_inunit ) Tmin_cnt
        READ ( Restart_inunit ) Elfac
        READ ( Restart_inunit ) Tmax_prev
        READ ( Restart_inunit ) Tmin_prev
        READ ( Restart_inunit ) Nuse_tsta
      ENDIF
      END SUBROUTINE temp_1sta_laps_restart
