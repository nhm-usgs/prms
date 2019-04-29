!***********************************************************************
! Determines the form of precipitation and distributes it to each HRU
! using an inverse distance weighting scheme
!
! Needs variable "precip" in the DATA FILE
! Needs observed variable tmax read in the temperature module
!
! MODIFIED 7/1998 ,JJ VACCARO, INTERPOLATES PRECIPITATION BASED ON DISTANCE
!   SQUARED USING ALL STATIONS IDENTIFIED FOR BASIN, MAY GIVE MORE
! PRECIPITATION IN LOWLANDS, BUT SMOOTHS OUT DISTRIBUTION, USES MONTHLY MEANS
! FOR 61-90 NORMAL PERIOD OF HRUS AND STATIONS FOR ADJUSTMENT
! RSR, added adjust to rain and snow, rather than using rain adjust to
!      adjust total precip
!***********************************************************************
      MODULE PRMS_PRECIP_DIST2
        IMPLICIT NONE
!   Local Variables
        INTEGER, SAVE, ALLOCATABLE :: N_psta(:), Nuse_psta(:, :)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dist2(:, :)
        CHARACTER(LEN=12), SAVE :: MODNAME
        CHARACTER(LEN=26), PARAMETER :: PROCNAME =
     +                                  'Precipitation Distribution'
!   Declared Parameters
        INTEGER, SAVE :: Max_psta
        REAL, SAVE :: Dist_max, Maxday_prec
        REAL, SAVE, ALLOCATABLE :: Rain_mon(:, :), Snow_mon(:, :)
        REAL, SAVE, ALLOCATABLE :: Psta_mon(:, :)
        REAL, SAVE, ALLOCATABLE :: Psta_xlong(:), Psta_ylat(:)
        REAL, SAVE, ALLOCATABLE :: Hru_ylat(:), Hru_xlong(:)
      END MODULE PRMS_PRECIP_DIST2

!***********************************************************************
!     Main precipitation routine
!***********************************************************************
      INTEGER FUNCTION precip_dist2()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: pptdist2decl, pptdist2init, pptdist2run
!***********************************************************************
      precip_dist2 = 0

      IF ( Process(:3)=='run' ) THEN
        precip_dist2 = pptdist2run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        precip_dist2 = pptdist2decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        precip_dist2 = pptdist2init()
      ENDIF

      END FUNCTION precip_dist2

!***********************************************************************
!     pptdist2decl - set up parameters for precipitation computations
!   Declared Parameters
!     tmax_allrain, tmax_allsnow, adjmix_rain, adjust_snow
!     rain_mon, snow_mon, precip_units
!     hru_area, temp_units, maxmon_prec, dist2, psta_xlong, psta_ylong
!     hru_ylat, hru_xlong, max_psta, dist_max, maxday_prec
!***********************************************************************
      INTEGER FUNCTION pptdist2decl()
      USE PRMS_PRECIP_DIST2
      USE PRMS_MODULE, ONLY: Model, Nhru, Nrain
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: n, nc
      CHARACTER(LEN=80), SAVE :: Version_precip_dist2
!***********************************************************************
      pptdist2decl = 0

      Version_precip_dist2 =
     +'$Id: precip_dist2.f 5169 2012-12-28 23:51:03Z rsregan $'
      nc = INDEX( Version_precip_dist2, 'Z' )
      n = INDEX( Version_precip_dist2, '.f' ) + 1
      IF ( declmodule(Version_precip_dist2(6:n), PROCNAME,
     +     Version_precip_dist2(n+2:nc))/=0 ) STOP
      MODNAME = 'precip_dist2'

      IF ( Nrain<2 .AND. Model/=99 ) THEN
        PRINT *, 'ERROR, precip_dist2 requires at least 2 precip',
     +           ' stations'
        STOP
      ENDIF

! declare parameters
      ALLOCATE ( Rain_mon(Nhru, 12), Snow_mon(Nhru, 12) )
      ALLOCATE ( Psta_mon(Nrain, 12) )
      ALLOCATE ( N_psta(Nhru), Dist2(Nhru, Nrain) )

      IF ( declparam(MODNAME, 'dist_max', 'one', 'real',
     +     '1.0E9', '0.0', '1.0E9',
     +     'Maximum distance from HRU to include a climate station',
     +     'Maximum distance from an HRU to a measurement station for'//
     +     ' use in calcuations',
     +     'feet')/=0 ) CALL read_error(1, 'dist_max')

      IF ( declparam(MODNAME, 'max_psta', 'one', 'integer',
     +     '50', '2', '50',
     +     'Maximum number of precipitation stations to distribute to'//
     +     ' an HRU',
     +     'Maximum number of precipitation stations to distribute to'//
     +     ' an HRU',
     +     'none')/=0 ) CALL read_error(1, 'max_psta')

      IF ( declparam(MODNAME, 'maxday_prec', 'one', 'real',
     +     '15.0', '0.0', '20.0',
     +     'Maximum daily precipitation for any weather site',
     +     'Maximum measured precipitation value above which'//
     +     ' precipitation is assumed to be in error',
     +     'precip_units')/=0 ) CALL read_error(1, 'maxday_prec')

      IF ( declparam(MODNAME, 'rain_mon', 'nhru,nmonths', 'real',
     +     '1.0', '0.0', '50.0',
     +     'Rain adjustment factor, by month for each HRU',
     +     'Monthly (January to December) factor to rain on each HRU'//
     +     ' to adjust precipitation distributed to each HRU to'//
     +     ' account for differences in elevation, and so forth',
     +     'precip_units')/=0 ) CALL read_error(1, 'rain_mon')

      IF ( declparam(MODNAME, 'snow_mon', 'nhru,nmonths', 'real',
     +     '1.0', '0.0', '50.0',
     +     'Rain adjustment factor, by month for each HRU',
     +     'Monthly (January to December) factor to snow on each HRU'//
     +     ' to adjust precipitation distributed to each HRU to'//
     +     ' account for differences in elevation, and so forth',
     +     'precip_units')/=0 ) CALL read_error(1, 'snow_mon')

      IF ( declparam(MODNAME, 'psta_mon', 'nrain,nmonths', 'real',
     +     '1.0', '0.00001', '50.0',
     +     'Monthly precipitation for each of the nrain precipitation'//
     +     ' stations',
     +     'Monthly (January to December) factor to precipitation'//
     +     ' at each measured station to adjust precipitation'//
     +     ' distributed to each HRU to'//
     +     ' account for differences in elevation, and so forth',
     +     'precip_units')/=0 ) CALL read_error(1, 'psta_mon')

      ALLOCATE ( Psta_xlong(Nrain) )
      IF ( declparam(MODNAME, 'psta_xlong', 'nrain', 'real',
     +     '0.0', '-1.0E9', '1.0E9',
     +     'Precipitation station longitude, State Plane',
     +     'Longitude of each precipitation measurement station,'//
     +     ' State Plane Coordinate System',
     +     'feet')/=0 ) CALL read_error(1, 'psta_xlong')

      ALLOCATE ( Psta_ylat(Nrain) )
      IF ( declparam(MODNAME, 'psta_ylat', 'nrain', 'real',
     +     '0.0', '-1.0E9', '1.0E9',
     +     'Precipitation station latitude, State Plane',
     +     'Latitude of each precipitation measurement station, State'//
     +     ' Plane Coordinate System',
     +     'feet')/=0 ) CALL read_error(1, 'psta_ylat')

      ALLOCATE ( Hru_ylat(Nhru) )
      IF ( declparam(MODNAME, 'hru_ylat', 'nhru', 'real',
     +     '0.0', '-1.0E9', '1.0E9',
     +     'HRU latitude of centroid, State Plane',
     +     'Latitude of each HRU for the centroid, State Plane'//
     +     ' Coordinate System',
     +     'feet')/=0 ) CALL read_error(1, 'hru_ylat')

      ALLOCATE ( Hru_xlong(Nhru) )
      IF ( declparam(MODNAME, 'hru_xlong', 'nhru', 'real',
     +     '0.0', '-1.0E9', '1.0E9',
     +     'HRU longitude of centroid, State Plane',
     +     'Longitude of each HRU for the centroid, State Plane'//
     +     ' Coordinate System',
     +     'feet')/=0 ) CALL read_error(1, 'hru_xlong')

      END FUNCTION pptdist2decl

!***********************************************************************
!     pptdist2init - Initialize precipitation module - get parameter values
!***********************************************************************
      INTEGER FUNCTION pptdist2init()
      USE PRMS_PRECIP_DIST2
      USE PRMS_MODULE, ONLY: Nhru, Nrain, Inputerror_flag,
     +    Parameter_check_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, DNEARZERO
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
      INTRINSIC SQRT, ABS
! Local Variables
      INTEGER :: i, k, j, n, kk, kkbig, jj
      DOUBLE PRECISION :: distx, disty, distance, big_dist, dist
      DOUBLE PRECISION, ALLOCATABLE :: nuse_psta_dist(:, :)
!***********************************************************************
      pptdist2init = 0

! NEW PARAMETERS
      IF ( getparam(MODNAME, 'maxday_prec', 1, 'real', Maxday_prec)
     +     /=0 ) CALL read_error(2, 'maxday_prec')

      IF ( getparam(MODNAME, 'dist_max', 1, 'real', Dist_max)
     +     /=0 ) CALL read_error(2, 'dist_max')

      IF ( getparam(MODNAME, 'max_psta', 1, 'real', Max_psta)
     +     /=0 ) CALL read_error(2, 'max_psta')
      IF ( Max_psta==50 ) Max_psta = Nrain

      IF ( getparam(MODNAME, 'rain_mon', Nhru*12, 'real', Rain_mon)
     +     /=0 ) CALL read_error(2, 'rain_mon')

      IF ( getparam(MODNAME, 'snow_mon', Nhru*12, 'real', Snow_mon)
     +     /=0 ) CALL read_error(2, 'snow_mon')

      IF ( getparam(MODNAME, 'psta_mon', Nrain*12, 'real', Psta_mon)
     +     /=0 ) CALL read_error(2, 'psta_mon')

      IF ( getparam(MODNAME, 'psta_xlong', Nrain, 'real', Psta_xlong)
     +     /=0 ) CALL read_error(2, 'psta_xlong')

      IF ( getparam(MODNAME, 'psta_ylat', Nrain, 'real', Psta_ylat)
     +     /=0 ) CALL read_error(2, 'psta_ylat')

      IF ( getparam(MODNAME, 'hru_xlong', Nhru, 'real', Hru_xlong)
     +     /=0 ) CALL read_error(2, 'hru_xlong')

      IF ( getparam(MODNAME, 'hru_ylat', Nhru, 'real', Hru_ylat)
     +     /=0 ) CALL read_error(2, 'hru_ylat')

! CALCULATE DISTANCE FROM EACH HRU TO EACH NRAIN GAGE,
! AS AN INVERSE FUNCTION, THEN SQUARE IT

      N_psta = 0
      ALLOCATE (Nuse_psta(Max_psta,Nhru), nuse_psta_dist(Max_psta,Nhru))
      Nuse_psta = 0
      nuse_psta_dist = 0.0D0

      DO jj = 1, Active_hrus
        i = Hru_route_order(jj)
        DO k = 1, Nrain
          distx = (Hru_xlong(i)-Psta_xlong(k))**2
          disty = (Hru_ylat(i)-Psta_ylat(k))**2
          distance = SQRT(distx+disty)
          IF ( ABS(distance)<DNEARZERO ) distance = 1.0D0
          dist = 1.0D0/(distance/5280.0D0)
          Dist2(i, k) = dist*dist
          IF ( distance<Dist_max ) THEN
            n = N_psta(i)
            IF ( n<Max_psta ) THEN
              n = n + 1
              Nuse_psta(n, i) = k
              nuse_psta_dist(n, i) = distance
              N_psta(i) = n
            ELSE ! have max_psta, don't use biggest distance
              big_dist = 0.0D0
              kkbig = 1
              DO kk = 1, Max_psta
                IF ( big_dist<nuse_psta_dist(kk,i) ) THEN
                  big_dist = nuse_psta_dist(kk, i)
                  kkbig = kk
                ENDIF
              ENDDO
              IF ( distance<big_dist ) THEN ! if equal use first one
                Nuse_psta(kkbig, i) = k
                nuse_psta_dist(kkbig, i) = distance
              ENDIF
            ENDIF
          ENDIF

          DO j = 1, 12
            IF ( Psta_mon(k,j)<0.00001 ) THEN
              PRINT *, 'psta_mon needs to be at least 0.00001'
              IF ( Parameter_check_flag==1 ) THEN
                PRINT *, 'ERROR, HRU:', k, 'month:', j, ', psta_mon:',
     +                   Psta_mon(k, j)
                Inputerror_flag = 1
              ELSE
                PRINT *, 'Warning, HRU:', k, 'month:', j, ', psta_mon:',
     +                   Psta_mon(k, j), ') set to 0.00001'
                Psta_mon(k, j) = 0.00001
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE ( nuse_psta_dist, Psta_xlong, Psta_ylat )
      DEALLOCATE ( Hru_xlong, Hru_ylat )

      END FUNCTION pptdist2init

!***********************************************************************
!     pptdist2run - Computes precipitation form (rain, snow or mix) and
!                   depth for each HRU, and basin weighted avg. precip
!***********************************************************************
      INTEGER FUNCTION pptdist2run()
      USE PRMS_PRECIP_DIST2
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area,
     +    Basin_area_inv, NEARZERO, DNEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Prmx, Basin_ppt,
     +    Basin_rain, Basin_snow, Hru_ppt, Hru_rain, Hru_snow,
     +    Basin_obs_ppt, Tmaxf, Tminf, Tmax_allsnow_f, Tmax_allrain_f,
     +    Adjmix_rain, Precip_units
      USE PRMS_OBS, ONLY: Precip, Nowtime, Nowmonth
      IMPLICIT NONE
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, iform, k, j, kk, allmissing
      REAL :: tdiff, allrain_mo, pcor, prec, adjmix_mo, ppt_sngl
      DOUBLE PRECISION :: sum_obs, sumdist, sump, ppt
!***********************************************************************
      pptdist2run = 0

      Basin_ppt = 0.0D0
      Basin_rain = 0.0D0
      Basin_snow = 0.0D0
      sum_obs = 0.0D0
      allrain_mo = Tmax_allrain_f(Nowmonth)
      adjmix_mo = Adjmix_rain(Nowmonth)
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        Hru_ppt(i) = 0.0
        Hru_rain(i) = 0.0
        Hru_snow(i) = 0.0
        Prmx(i) = 0.0
        Newsnow(i) = 0
        Pptmix(i) = 0

        ! determine form of precip

!******IF maximum temperature is below or equal to the base temperature
!******for snow then precipitation is all snow
        IF ( Tmaxf(i)<=Tmax_allsnow_f ) THEN
          ! precipitation is all snow
          iform = 1

!******If measured temperature data are not available then
!******precipitation is all rain.
!******If minimum temperature is above base temperature for snow or
!******maximum temperature is above all_rain temperature then
!******precipitation is all rain
! MODIFIED BELOW (10/99, JJV SO THAT ASSUMING ALWAYS TEMPERATURE DATA FOR A DAY
! FOR AT LEAST ONE SITE
        ELSEIF ( Tminf(i)>Tmax_allsnow_f .OR. Tmaxf(i)>=allrain_mo )THEN
          iform = 2
!******Otherwise precipitation is a mixture of rain and snow
        ELSE
          iform = 0
        ENDIF

! Determine if any precipitation on HRU=i, if not start next HRU

        ppt = 0.0D0
        sumdist = 0.0D0
        sump = 0.0D0
        allmissing = 0
        DO kk = 1, N_psta(i)
          k = Nuse_psta(kk, i)

!   Make sure stations precipitation is not negative or missing

!???rsr, pcor should only be used for portion of precipitation that is rain
          IF ( Precip(k)>=0.0 .AND. Precip(k)<=Maxday_prec ) THEN
!     +         Precip(k)<Maxmon_prec(Nowmonth) ) THEN
            allmissing = 1
            !rsr, if all rain use rain adjustment
            IF ( iform==2 ) THEN
              pcor = Rain_mon(i, Nowmonth)/Psta_mon(k, Nowmonth)
            !rsr, if all snow or mixed use snow adjustment
!           ELSEIF ( iform==1 .OR. iform==0 ) THEN
            ELSE
              pcor = Snow_mon(i, Nowmonth)/Psta_mon(k, Nowmonth)
            ENDIF
            sumdist = sumdist + Dist2(i, k)
            prec = Precip(k)*pcor
            sump = sump + prec
            ppt = ppt + prec*Dist2(i, k)
!          ELSE
!            PRINT *, 'bad precipitation value, HRU:', k, Precip(k), Nowtime
          ENDIF
        ENDDO
        IF ( allmissing==0 ) THEN
          PRINT *,'ERROR, all precipitation stations have missing data',
     +            Nowtime
          STOP
        ENDIF

        ! Ignore small amounts of preicipitation on an HRU
        IF ( ppt<DNEARZERO ) CYCLE

        IF ( sumdist>0.0D0 ) ppt = ppt/sumdist

        IF ( Precip_units==1 ) ppt = ppt/25.4D0
        ppt_sngl = SNGL(ppt)
        Hru_ppt(i) = ppt_sngl
        sum_obs = sum_obs + ppt*Hru_area(i)

        IF ( iform==2 ) THEN
          Hru_rain(i) = ppt_sngl
          Prmx(i) = 1.0

        ELSEIF ( iform==1 ) THEN
          Hru_snow(i) = ppt_sngl
          Newsnow(i) = 1

       ! precipitation is a mixture of rain and snow
        ELSE
          tdiff = Tmaxf(i) - Tminf(i)
          IF ( ABS(tdiff)<NEARZERO ) tdiff = 0.01
          Prmx(i) = ((Tmaxf(i)-Tmax_allsnow_f)/tdiff)*adjmix_mo

!******Unless mixture adjustment raises the proportion of rain to
!******greater than or equal to 1.0 in which case it all rain
!******If not, it is a rain/snow mixture
          IF ( Prmx(i)<1.0 ) THEN
            Pptmix(i) = 1
            Hru_rain(i) = Prmx(i)*ppt_sngl
            Hru_snow(i) = ppt_sngl - Hru_rain(i)
            Newsnow(i) = 1
          ELSE
            Hru_rain(i) = ppt_sngl
            Prmx(i) = 1.0
          ENDIF
        ENDIF

        Basin_ppt = Basin_ppt + ppt*Hru_area(i)
        Basin_rain = Basin_rain + Hru_rain(i)*Hru_area(i)
        Basin_snow = Basin_snow + Hru_snow(i)*Hru_area(i)

      ENDDO
      Basin_ppt = Basin_ppt*Basin_area_inv
      Basin_obs_ppt = sum_obs*Basin_area_inv
      Basin_rain = Basin_rain*Basin_area_inv
      Basin_snow = Basin_snow*Basin_area_inv

      END FUNCTION pptdist2run
