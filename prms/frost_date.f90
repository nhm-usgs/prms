!***********************************************************************
! This is a preprocess module.
! Determine the latest "killing" frost date in the spring and the
! earliest date in the fall.
! Declared Parameters: frost_temp
!***********************************************************************
      INTEGER FUNCTION frost_date()
      USE PRMS_MODULE, ONLY: Process, Nhru
      USE PRMS_BASIN, ONLY: Timestep, Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, Hemisphere
      USE PRMS_CLIMATEVARS, ONLY: Tmin_hru
      USE PRMS_OBS, ONLY: Jsol
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX, INT
      INTEGER, EXTERNAL :: declmodule, declparam, getparam, get_season
      EXTERNAL read_error, write_integer_param, PRMS_open_module_file
! Declared Parameters
      REAL, SAVE :: Frost_temp
! Local Variables
      ! fall_frost: The number of solar days after winter solstice of
      !             the first killing frost of the fall
      ! spring_frost: The number of solar days after winter solstice of
      !               the last killing frost of the spring
      INTEGER, SAVE, ALLOCATABLE :: fall_frost(:), spring_frost(:)
      ! basin_fall_frost: The basin average solar date of the first
      !                   killing frost of the fall
      ! basin_spring_frost: The basin average solar date of the last
      !                     killing frost of the fall
      DOUBLE PRECISION, SAVE :: basin_fall_frost, basin_spring_frost
      INTEGER, SAVE :: oldSeason, fallFrostCount, springFrostCount, fall1, spring1
      INTEGER, SAVE :: switchToSpringToday, switchToFallToday, Iunit
      INTEGER, SAVE, ALLOCATABLE :: fallFrostSum(:), springFrostSum(:)
      INTEGER, SAVE, ALLOCATABLE :: currentFallFrost(:), currentSpringFrost(:)
      INTEGER :: season, j, jj, basin_fall(1), basin_spring(1), nc
      CHARACTER(LEN=10), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_frost_date
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Transpiration Period'
!***********************************************************************
      frost_date = 0

      IF ( Process(:3)=='run' ) THEN
        season = get_season()

! Figure out if the season changes on this timestep. Putting this
! check here makes the blocks below easier to understand.
        IF ( oldSeason/=season ) THEN
          IF ( season==1 ) THEN
            switchToSpringToday = 1
          ELSE
            switchToFallToday = 1
          ENDIF
        ELSE
          switchToSpringToday = 0
          switchToFallToday = 0
        ENDIF
        oldSeason = season

! If this is the first timestep of fall, unset the CurrentFallFrost
! variable. Also since we are finished looking for spring frosts,
! add the CurrentSpringFrost dates to the spring_frost variable
! (average date of the spring frost for each HRU).
        IF ( switchToFallToday==1 ) THEN
          fallFrostCount = fallFrostCount + 1
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            currentFallFrost(j) = 0
            IF ( currentSpringFrost(j)==0 ) currentSpringFrost(j) = spring1
            springFrostSum(j) = springFrostSum(j) + currentSpringFrost(j)
          ENDDO

! If this is the first timestep of spring, unset the
! CurrentSpringFrost variable. Also since we are finished looking
! for fall frosts, add the CurrentFallFrost dates to the fall_frost
! variable (average date of the fall frost for each HRU).
        ELSEIF ( switchToSpringToday==1 ) THEN
          springFrostCount = springFrostCount + 1
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            currentSpringFrost(j) = 0
            IF ( currentFallFrost(j)==0 ) currentFallFrost(j) = fall1
            fallFrostSum(j) = fallFrostSum(j) + currentFallFrost(j)
          ENDDO
        ENDIF

! If the season is fall, look for the earliest fall frost.
! should probably look for x number of consecutive days or frosts before setting killing frost
        IF ( season==2 ) THEN
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            IF ( Tmin_hru(j)<=Frost_temp .AND. currentFallFrost(j)==0 ) currentFallFrost(j) = Jsol
          ENDDO
! This is the spring phase, look for the latest spring frost.
        ELSE
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            IF ( Tmin_hru(j)<=Frost_temp ) currentSpringFrost(j) = Jsol
          ENDDO
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_frost_date = '$Id: frost_date.f90 5169 2012-12-28 23:51:03Z rsregan $'
        nc = INDEX( Version_frost_date, 'Z' )
        j = INDEX( Version_frost_date, '.f90' ) + 3
        IF ( declmodule(Version_frost_date(6:j), PROCNAME, Version_frost_date(j+2:nc))/=0 ) STOP
        MODNAME = 'frost_date'

        IF ( declparam(MODNAME, 'frost_temp', 'one', 'real', &
             '28.0', '-10.0', '32.0', &
             'Temperature of killing frost', 'Temperature of killing frost', &
             'degrees')/=0 ) CALL read_error(1, 'frost_temp')

! Allocate arrays for local variables
        ALLOCATE ( fall_frost(Nhru), spring_frost(Nhru) )
        ALLOCATE ( fallFrostSum(Nhru), springFrostSum(Nhru) )
        ALLOCATE ( currentFallFrost(Nhru), currentSpringFrost(Nhru) )

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'frost_temp', 1, 'real', Frost_temp)/=0 ) CALL read_error(2, 'frost_temp')
        IF ( Timestep==0 ) THEN
          fall_frost = 0
          spring_frost = 0
          currentFallFrost = 0
          currentSpringFrost = 0
          fallFrostSum = 0
          springFrostSum = 0
        ENDIF
        fallFrostCount = 0
        springFrostCount = 0
        CALL PRMS_open_module_file(Iunit, 'frost_date.param')
        oldSeason = get_season()
        IF ( Hemisphere==0 ) THEN ! Northern Hemisphere
          spring1 = 0
          fall1 = 366
        ELSE
          spring1 = 366
          fall1 = 0
        ENDIF

      ELSEIF ( Process(:5)=='clean' ) THEN
        basin_fall_frost = 0.0D0
        basin_spring_frost = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          fall_frost(j) = fallFrostSum(j)/fallFrostCount
          spring_frost(j) = springFrostSum(j)/springFrostCount
          basin_fall_frost = basin_fall_frost + fall_frost(j)*Hru_area(j)
          basin_spring_frost = basin_spring_frost + spring_frost(j)*Hru_area(j)
        ENDDO
        basin_fall_frost = basin_fall_frost*Basin_area_inv
        basin_spring_frost = basin_spring_frost*Basin_area_inv

        CALL write_integer_param(Iunit, 'fall_frost', 'nhru', Nhru, fall_frost)
        CALL write_integer_param(Iunit, 'spring_frost', 'nhru', Nhru, spring_frost)
        basin_fall(1) = INT ( basin_fall_frost )
        basin_spring(1) = INT ( basin_spring_frost )
        CALL write_integer_param(Iunit, 'basin_fall_frost', 'one', 1, basin_fall)
        CALL write_integer_param(Iunit, 'basin_spring_frost', 'one', 1, basin_spring)
      ENDIF

      END FUNCTION frost_date

!*************************************************************
! Figure out if the current solar day is in "spring" or "fall"
!*************************************************************
      INTEGER FUNCTION get_season()
      USE PRMS_BASIN, ONLY: Hemisphere
      USE PRMS_OBS, ONLY: Jsol
!*************************************************************
      get_season = 2 ! default is fall frost
      IF ( Hemisphere==0 ) THEN ! Northern Hemisphere
        IF ( Jsol>0 .AND. Jsol<183 ) get_season = 1 ! This is the spring phase
      ELSE ! Southern Hemisphere
        IF ( Jsol>182 .AND. Jsol<367 ) get_season = 1 ! This is the spring phase
      ENDIF
      END FUNCTION get_season
