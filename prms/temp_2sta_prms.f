!***********************************************************************
! DEPRECATED
! This module has been superceded by temp_laps (which does the
! same thing)
!
! Distributes maximum and minimum temperatures to each HRU by computing
! a daily lapse rate with temperature data measured at two stations
!
! Variables needed from DATA FILE: tmax, tmin
! Declared Parameters
!     lo_index, hi_index, tsta_elev, tmax_adj, tmin_adj
!     hru_tsta, hru_elev, hru_area, basin_tsta
!***********************************************************************
      INTEGER FUNCTION temp_2sta_prms()
      USE PRMS_MODULE, ONLY: Process, Nhru, Ntemp
      USE PRMS_BASIN, ONLY: Hru_elev, Basin_area_inv, Hru_area, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Tsta_elev, Hru_tsta,
     +    Solrad_tmax, Solrad_tmin, Basin_temp, Basin_tmax, Basin_tmin,
     +    Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, Tavgc, Tmax_adj, Tmin_adj,
     +    Basin_tsta
      USE PRMS_OBS, ONLY: Nowtime, Nowyear, Nowmonth, Nowday, Tmax, Tmin
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, INDEX
      INTEGER, EXTERNAL :: declparam, getparam
      REAL, EXTERNAL :: c_to_f, f_to_c
      EXTERNAL read_error, temp_set, print_module
! Declared Parameters
      INTEGER, SAVE :: Lo_index, Hi_index
! Local Variables
      INTEGER :: j, k
      REAL :: eldif, elcor, tminsta
      REAL :: tmx, tmn, tcrx, tcrn, tmxsta, tmnsta, thi, tlo
      REAL, SAVE :: solrad_tmax_good, solrad_tmin_good
      REAL, SAVE, ALLOCATABLE :: elfac(:)
      CHARACTER(LEN=14), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_temp_2sta_prms
!***********************************************************************
      temp_2sta_prms = 0

      IF ( Process(:3)=='run' ) THEN
        thi = Tmax(Hi_index)
        IF ( thi<-99.0 .OR. thi>150.0 ) PRINT 9001, 'tmax', thi,
     +       Hi_index, Nowtime
        tlo = Tmax(Lo_index)
        IF ( tlo<-99.0 .OR. tlo>150.0 ) PRINT 9001, 'tmax', tlo,
     +       Lo_index, Nowtime
        tmxsta = thi - tlo
        thi = Tmin(Hi_index)
        IF ( thi<-99.0 .OR. thi>150.0 ) PRINT 9001, 'tmin', thi,
     +       Hi_index, Nowtime
        tlo = Tmin(Lo_index)
        IF ( tlo<-99.0 .OR. tlo>150.0 ) PRINT 9001, 'tmin', tlo,
     +       Lo_index, Nowtime
        tmnsta = thi - tlo

        Basin_tmax = 0.0D0
        Basin_tmin = 0.0D0
        Basin_temp = 0.0D0
        DO j = 1, Nhru
          k = Hru_tsta(j)
          tcrx = tmxsta*elfac(j) - Tmax_adj(j)
          tcrn = tmnsta*elfac(j) - Tmin_adj(j)
          tmx = Tmax(k) - tcrx
          tmn = Tmin(k) - tcrn
          CALL temp_set(j, tmx, tmn, Tmaxf(j), Tminf(j), Tavgf(j),
     +                  Tmaxc(j), Tminc(j), Tavgc(j), Hru_area(j))
        ENDDO
        Basin_tmax = Basin_tmax*Basin_area_inv
        Basin_tmin = Basin_tmin*Basin_area_inv
        Basin_temp = Basin_temp*Basin_area_inv
        Solrad_tmax = Tmax(Basin_tsta)
        Solrad_tmin = Tmin(Basin_tsta)
        IF ( Solrad_tmax<-50.0 .OR. Solrad_tmax>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmax:',
     +             Solrad_tmax, ' using last valid value'
          PRINT *, 'Value set to', solrad_tmax_good, ' Date:',
     +             Nowyear, Nowmonth, Nowday
          Solrad_tmax = solrad_tmax_good
        ELSE
          solrad_tmin_good = Solrad_tmin
        ENDIF
        IF ( Solrad_tmin<-50.0 .OR. Solrad_tmin>150.0 ) THEN
          PRINT *, 'Bad temperature data to set solrad_tmin:',
     +             Solrad_tmin, ' using last valid value'
          PRINT *, 'Value set to', solrad_tmin_good, ' Date:',
     +             Nowyear, Nowmonth, Nowday
          Solrad_tmin = solrad_tmin_good
        ELSE
          solrad_tmin_good = Solrad_tmin
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_temp_2sta_prms =
     +'$Id: temp_2sta_prms.f 5532 2013-03-25 21:49:54Z rsregan $'
        CALL print_module(Version_temp_2sta_prms,
     +                    'Temperature Distribution  ', 77)
        MODNAME = 'temp_2sta_prms'

        IF ( declparam(MODNAME, 'lo_index', 'one', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'Low elevation temperature station index',
     +       'Index of lower temperature station for daily lapse'//
     +       ' rate computations',
     +       'none')/=0 ) CALL read_error(1, 'lo_index')
        IF ( declparam(MODNAME, 'hi_index', 'one', 'integer',
     +       '1', 'bounded', 'ntemp',
     +       'High elevation temperature station index',
     +       'Index of upper temperature station for daily lapse'//
     +       ' rate computations',
     +       'none')/=0 ) CALL read_error(1, 'hi_index')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'lo_index', 1, 'integer', Lo_index)
     +       /=0 ) CALL read_error(2, 'lo_index')
        IF ( getparam(MODNAME, 'hi_index', 1, 'integer', Hi_index)
     +       /=0 ) CALL read_error(2, 'hi_index')

        ALLOCATE ( elfac(Nhru) )
        tminsta = Tsta_elev(Lo_index)
        eldif = (Tsta_elev(Hi_index)-tminsta)/1000.0
        IF ( ABS(eldif)<NEARZERO ) eldif = 1.0
        DO j = 1, Nhru
          IF ( Hru_tsta(j)<1 .OR. Hru_tsta(j)>Ntemp ) THEN
            PRINT *, 'ERROR, hru_tsta=0 or hru_tsta>ntemp, HRU:', j
            STOP
          ENDIF
          elcor = (Hru_elev(j)-tminsta)/1000.0
          elfac(j) = elcor/eldif
        ENDDO
      ENDIF
      solrad_tmax_good = 0.0
      solrad_tmin_good = 0.0

 9001 FORMAT ('Warning, bad temperature, ', A, ':', F10.3,
     +        '; temperature station:', I3, ' Time:', I5, 2('/', I2.2),
     +        I3, 2(':', I2.2), I5)

      END FUNCTION temp_2sta_prms
