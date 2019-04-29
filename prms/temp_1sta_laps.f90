!***********************************************************************
! Distributes maximum, minimum, and average temperatures to each HRU
! using temperature data measured at one station and an estimated monthly
! lapse rate (temp_1sta) or by computing a daily lapse rate based on 
! elevations with temperature data measured at two stations (temp_laps)
!
! Variables needed from DATA FILE: tmax, tmin
! Declared Parameters
!     tsta_elev, tmax_adj, tmin_adj, hru_type
!     tmax_adj = tmax_aspect_adjust; tmin_adj = tmin_aspect_adjust
!     hru_tsta, hru_elev, hru_area, temp_units, basin_tsta
! Declared Parameters for temp_1sta
!     tmax_lapse, tmin_lapse
! Declared Parameters for temp_laps
!     hru_tlaps
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
        REAL, SAVE :: Tmax_lapse(12), Tmin_lapse(12)
        INTEGER, SAVE, ALLOCATABLE :: Hru_tlaps(:)
      END MODULE PRMS_TEMP_1STA_LAPS

      INTEGER FUNCTION temp_1sta_laps()
      USE PRMS_TEMP_1STA_LAPS
      USE PRMS_MODULE, ONLY: Process, Nhru, Ntemp, Save_vars_to_file, &
     &    Inputerror_flag, Temp_flag, Init_vars_from_file, Model, Start_month
      USE PRMS_BASIN, ONLY: Hru_elev, Hru_area, &
     &    Active_hrus, Hru_route_order, Basin_area_inv, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Tmax_aspect_adjust, Tmin_aspect_adjust, Tsta_elev, &
     &    Hru_tsta, Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, &
     &    Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Basin_tsta, Tmax_allrain
      USE PRMS_SET_TIME, ONLY: Nowmonth, Nowday
      USE PRMS_OBS, ONLY: Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX, ABS
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, temp_set, print_module, temp_1sta_laps_restart, print_date, checkdim_param_limits
! Local Variables
      INTEGER :: j, k, jj, i, kk, kkk, l, ierr
      REAL :: tmaxlaps_mo, tminlaps_mo, tmx, tmn, tdiff
      CHARACTER(LEN=80), SAVE :: Version_temp
!***********************************************************************
      temp_1sta_laps = 0

      IF ( Process(:3)=='run' ) THEN
        kk = 0
        kkk = 0
        DO i = 1, Ntemp
          IF ( Nuse_tsta(i)>0 ) THEN
            IF ( Tmax(i)<-99.0 .OR. Tmax(i)>150.0 ) THEN
              Tmax_cnt(i) = Tmax_cnt(i) + 1
              IF ( Tmax_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmax', Tmax(i), i, Tmax_prev(i)
                CALL print_date(0)
                Tmax(i) = Tmax_prev(i)
                kk = 1
              ELSE
                PRINT 9002, 'tmax', Tmax(i), i
                CALL print_date(0)
                STOP
              ENDIF
            ELSE
              Tmax_prev(i) = Tmax(i)
              Tmax_cnt(i) = 0
            ENDIF
            IF ( Tmin(i)<-99.0 .OR. Tmin(i)>150.0 ) THEN
              Tmin_cnt(i) = Tmin_cnt(i) + 1
              IF ( Tmin_cnt(i)<Max_missing ) THEN
                PRINT 9001, 'tmin', Tmin(i), i, Tmin_prev(i)
                CALL print_date(0)
                Tmin(i) = Tmin_prev(i)
                kkk = 1
              ELSE
                PRINT 9002, 'tmin', Tmin(i), i
                CALL print_date(0)
                STOP
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
          IF ( Temp_flag==1 ) THEN
            IF ( Nowday==1 ) THEN
              Tcrx(j) = tmaxlaps_mo*Elfac(j) - Tmax_aspect_adjust(j)
              Tcrn(j) = tminlaps_mo*Elfac(j) - Tmin_aspect_adjust(j)
            ENDIF
            tmx = Tmax(k) - Tcrx(j)
            tmn = Tmin(k) - Tcrn(j)
          ELSE
            l = Hru_tlaps(j)
            tmx = Tmax(k) + (Tmax(l) - Tmax(k))*Elfac(j) + Tmax_aspect_adjust(j)
            tmn = Tmin(k) + (Tmin(l) - Tmin(k))*Elfac(j) + Tmin_aspect_adjust(j)
          ENDIF
          CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j), &
     &                  Tmaxc(j), Tminc(j), Tavgc(j), Hru_area(j))
        ENDDO
        Basin_tmax = Basin_tmax*Basin_area_inv
        Basin_tmin = Basin_tmin*Basin_area_inv
        Basin_temp = Basin_temp*Basin_area_inv
        Solrad_tmax = Tmax(Basin_tsta)
        Solrad_tmin = Tmin(Basin_tsta)
        IF ( Solrad_tmax<-99.0 .OR. Solrad_tmax>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmax:', Solrad_tmax, ' using last valid value:', Solrad_tmax_good
          CALL print_date(0)
          Solrad_tmax = Solrad_tmax_good
        ELSE
          Solrad_tmax_good = Solrad_tmax
        ENDIF
        IF ( Solrad_tmin<-99.0 .OR. Solrad_tmin>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmin:', Solrad_tmin, ' using last valid value:', Solrad_tmin_good
          CALL print_date(0)
          Solrad_tmin = Solrad_tmin_good
        ELSE
          Solrad_tmin_good = Solrad_tmin
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_temp = '$Id: temp_1sta_laps.f90 7115 2015-01-06 00:09:15Z rsregan $'
        IF ( Temp_flag==1 ) THEN
          MODNAME = 'temp_1sta'
          Version_temp = Version_temp(:14)//Version_temp(20:80)
        ELSE
          MODNAME = 'temp_laps'
          Version_temp = Version_temp(:9)//Version_temp(15:80)
        ENDIF
        CALL print_module(Version_temp, 'Temperature Distribution    ', 90)

        ALLOCATE ( Elfac(Nhru), Nuse_tsta(Ntemp) )
        ALLOCATE ( Tmin_cnt(Ntemp), Tmax_cnt(Ntemp), Tmax_prev(Ntemp), Tmin_prev(Ntemp) )

        IF ( Temp_flag==1 .OR. Model==99 ) THEN
          ALLOCATE ( Tcrn(Nhru), Tcrx(Nhru) )
          IF ( declparam(MODNAME, 'tmax_lapse', 'nmonths', 'real', &
     &         '3.0', '-10.0', '10.0', &
     &         'Monthly maximum temperature lapse rate', &
     &         'Monthly (January to December) values representing the change in maximum air temperature per 1000 elev_units of'// &
     &         ' elevation change', &
     &         'temp_units/elev_units')/=0 ) CALL read_error(1, 'tmax_lapse')

          IF ( declparam(MODNAME, 'tmin_lapse', 'nmonths', 'real', &
     &         '3.0', '-10.0', '10.0', &
     &         'Monthly minimum temperature lapse rate', &
     &         'Monthly (January to December) values representing the change in minimum air temperture per 1000 elev_units of'// &
     &         ' elevation change', &
     &         'temp_units/elev_units')/=0 ) CALL read_error(1, 'tmin_lapse')
        ENDIF

        IF ( Temp_flag==2 .OR. Model==99 ) THEN
          ALLOCATE ( Hru_tlaps(Nhru) )
          IF ( declparam(MODNAME, 'hru_tlaps', 'nhru', 'integer', &
     &         '1', 'bounded', 'ntemp', &
     &         'Index of lapse temperature station for HRU', &
     &         'Index of the lapse temperature station used for lapse rate calculations', &
     &         'none')/=0 ) CALL read_error(1, 'hru_tlaps')
        ENDIF

        IF ( declparam(MODNAME, 'max_missing', 'one', 'integer', &
     &       '3', '0', '10', &
     &       'Maximum number of consecutive missing values allowed for'// &
     &       ' any air-temperature-measurement station; 0=unlimited', &
     &       'Maximum number of consecutive missing values allowed for'// &
     &       ' any air-temperature-measurement station; missing value set'// &
     &       ' to last valid value; 0=unlimited', &
     &       'none')/=0 ) CALL read_error(1, 'max_missing')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL temp_1sta_laps_restart(1)

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
            Elfac(j) = (Hru_elev(j)-Tsta_elev(k))/1000.0
            Tcrx(j) = tmaxlaps_mo*Elfac(j) - Tmax_aspect_adjust(j)
            Tcrn(j) = tminlaps_mo*Elfac(j) - Tmin_aspect_adjust(j)
          ELSE
            ierr = 0
            CALL checkdim_param_limits(j, 'hru_tlaps', 'ntemp', Hru_tlaps(j), 1, Ntemp, ierr)
            IF ( ierr==0 ) THEN
              tdiff = Tsta_elev(Hru_tlaps(j)) - Tsta_elev(k)
              IF ( ABS(tdiff)<NEARZERO ) tdiff = 1.0
              Elfac(j) = (Hru_elev(j)-Tsta_elev(k))/tdiff
            ELSE
              Inputerror_flag = 1
            ENDIF
          ENDIF
        ENDDO

        IF ( Init_vars_from_file==0 ) THEN
          Solrad_tmax_good = Solrad_tmax
          Solrad_tmin_good = Solrad_tmin
          Tmax_cnt = 0
          Tmin_cnt = 0
          DO i = 1, Ntemp
            Tmax_prev(i) = Tmax_allrain(Start_month)
          ENDDO
          Tmin_prev = Tmax_prev
        ENDIF

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL temp_1sta_laps_restart(0)

      ENDIF

 9001 FORMAT ('WARNING, bad temperature, ', A, ':', F10.3, &
     &        '; temperature station:', I5, /, 'Value set to last valid value:', F10.3)
 9002 FORMAT (/, 'ERROR, too many consecutive bad temperatures, ', A, ':', F10.3, /, &
     &        'temperature station:', I3, /, &
     &        'Fix Data File or increase parameter max_missing' )

      END FUNCTION temp_1sta_laps

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE temp_1sta_laps_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
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
        WRITE ( Restart_outunit ) Solrad_tmax_good, Solrad_tmin_good
        WRITE ( Restart_outunit ) Tmax_cnt
        WRITE ( Restart_outunit ) Tmin_cnt
        WRITE ( Restart_outunit ) Tmax_prev
        WRITE ( Restart_outunit ) Tmin_prev
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Solrad_tmax_good, Solrad_tmin_good
        READ ( Restart_inunit ) Tmax_cnt
        READ ( Restart_inunit ) Tmin_cnt
        READ ( Restart_inunit ) Tmax_prev
        READ ( Restart_inunit ) Tmin_prev
      ENDIF
      END SUBROUTINE temp_1sta_laps_restart
