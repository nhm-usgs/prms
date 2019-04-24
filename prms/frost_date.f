!***********************************************************************
! This is a preprocess module.
! Determine the latest "killing" frost date in the spring and the
! earliest date in the fall.
! Declared Parameters: killing_temp
!***********************************************************************
      INTEGER FUNCTION frost_date()
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug,
     +    Version_frost_date, Frost_date_nc
      USE PRMS_BASIN, ONLY: Timestep, Active_hrus, Hru_route_order,
     +    Hru_area, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tmin_hru
      USE PRMS_OBS, ONLY: Jsol, Nowmonth, Nowday
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error, write_integer_array
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
      INTEGER, SAVE :: basin_fall_frost(1), basin_spring_frost(1)
      INTEGER, SAVE :: oldSeason, fallFrostCount, springFrostCount
      INTEGER, SAVE :: switchToSpringToday, switchToFallToday
      INTEGER, SAVE, ALLOCATABLE :: fallFrostSum(:), springFrostSum(:)
      INTEGER, SAVE, ALLOCATABLE :: currentFallFrost(:)
      INTEGER, SAVE, ALLOCATABLE :: currentSpringFrost(:)
      INTEGER :: season, j, jj
!***********************************************************************
      frost_date = 1

      IF ( Process(:3)=='run' ) THEN

! Figure out if the current solar day is in "spring" or "fall"
        IF ( Jsol>0 .AND. Jsol<183 ) THEN
          ! This is the spring phase
          season = 1
        ELSE
          ! This is the fall phase
          season = 2
        ENDIF

! Figure out if the season changes on this timestep. Putting this
! check here makes the blocks below easier to understand.
        IF ( oldSeason/=0 .AND. oldSeason/=season ) THEN
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
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            currentFallFrost(j) = 0
          ENDDO
          IF ( currentSpringFrost(Hru_route_order(1))>0 ) THEN
            springFrostCount = springFrostCount + 1
            DO jj = 1, Active_hrus
              j = Hru_route_order(jj)
! If the CurrentSpringFrost for an HRU is 0, that means
! that no spring frost was found. Set this to solar day 1
              IF ( currentSpringFrost(j)==0 ) currentSpringFrost(j) = 1
              springFrostSum(j) = springFrostSum(j) +
     +                            currentSpringFrost(j)
              spring_frost(j) = springFrostSum(j) / springFrostCount
!             PRINT *, "  Last spring frost for HRU ", j, " is on ",
!     +                   currentSpringFrost(j)
            ENDDO
          ENDIF
!         PRINT *, "Switching to fall. Jsol = ", Jsol

! If this is the first timestep of spring, unset the
! CurrentSpringFrost variable. Also since we are finished looking
! for fall frosts, add the CurrentFallFrost dates to the fall_frost
! variable (average date of the fall frost for each HRU).
        ELSEIF ( switchToSpringToday==1 ) THEN
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            currentSpringFrost(j) = 0
          ENDDO

          IF ( currentFallFrost(Hru_route_order(1))>0 ) THEN
            fallFrostCount = fallFrostCount + 1
            DO jj = 1, Active_hrus
              j = Hru_route_order(jj)
! If the CurrentFallFrost for an HRU is 0, that means
! that no fall frost was found. Set this to solar day 365
              IF ( currentFallFrost(j)==0 ) currentFallFrost(j) = 365
              fallFrostSum(j) = fallFrostSum(j) + currentFallFrost(j)
              fall_frost(j) = fallFrostSum(j) / fallFrostCount
!             PRINT *, "  First fall frost for HRU ", j, " is on ",
!     +                   currentFallFrost(j)
            ENDDO
          ENDIF
!         PRINT *, "Switching to spring. Jsol = ", Jsol
        ENDIF
      
! If the season is fall, look for the earliest fall frost. Once, it
! is found, don't bother looking for other fall frosts.
        IF ( season==2 ) THEN
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            IF ( Tmin_hru(j)<=Frost_temp .AND. currentFallFrost(j)==0)
     +           currentFallFrost(j) = Jsol
          ENDDO
        
! ELSE the season is spring, look for the latest spring frost.
        ELSE
          DO jj = 1, Active_hrus
            j = Hru_route_order(jj)
            IF ( Tmin_hru(j)<=Frost_temp ) currentSpringFrost(j) =Jsol
          ENDDO
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_frost_date =
     +'$Id: frost_date.f 3673 2011-10-05 00:40:23Z rsregan $'
        Frost_date_nc = INDEX( Version_frost_date, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_frost_date(:Frost_date_nc))/=0 ) STOP
        ENDIF
    
        IF ( declparam('frost', 'frost_temp', 'one', 'real',
     +       '28.0', '-10.0', '32.0',
     +       'Temperature of killing frost',
     +       'Temperature of killing frost',
     +       'degrees')/=0 ) CALL read_error(1, 'frost_temp')

! Allocate arrays for local variables
        ALLOCATE ( fall_frost(Nhru), spring_frost(Nhru) )
        ALLOCATE ( fallFrostSum(Nhru), springFrostSum(Nhru) )
        ALLOCATE ( currentFallFrost(Nhru), currentSpringFrost(Nhru) )

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam('frost', 'frost_temp', 1, 'real', Frost_temp)
     +       /=0 ) CALL read_error(2, 'frost_temp')
        IF ( Timestep==0 ) THEN
          fall_frost = 0
          spring_frost = 0
          currentFallFrost = 0
          currentSpringFrost = 0
          fallFrostSum = 0
          springFrostSum = 0
        ENDIF
        oldSeason = 0
        fallFrostCount = 0
        springFrostCount = 0

      ELSEIF ( Process(:5)=='clean' ) THEN
        basin_fall_frost = 0.0
        basin_spring_frost = 0.0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          basin_fall_frost(1) = basin_fall_frost(1)
     +                          + fall_frost(j)*Hru_area(j)
          basin_spring_frost(1) = basin_spring_frost(1)
     +                            + spring_frost(j)*Hru_area(j)
        ENDDO
        basin_fall_frost(1) = basin_fall_frost(1)*Basin_area_inv
        basin_spring_frost(1) = basin_spring_frost(1)*Basin_area_inv
      ENDIF

      CALL write_integer_array('fall_frost', 'nhru', Nhru, fall_frost)
      CALL write_integer_array('spring_frost', 'nhru', Nhru, fall_frost)
      CALL write_integer_array('basin_fall_frost', 'one', 1,
     +                         basin_fall_frost)
      CALL write_integer_array('basin_spring_frost', 'one', 1,
     +                         basin_spring_frost)

      frost_date = 0
      END FUNCTION frost_date


