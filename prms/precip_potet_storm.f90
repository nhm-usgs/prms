!***********************************************************************
! Adjusts precipitation and potential evapotranspiration storm event
!***********************************************************************
      MODULE PRMS_STORM_PRECIP
      IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Precipitation Distribution'
        character(len=*), parameter :: MODNAME = 'precip_storm'
        character(len=*), parameter :: Version_precip_potet_storm = '2022-04-05'
        ! Declared Parameter
        REAL, ALLOCATABLE :: Strain_adj(:, :)
      END MODULE PRMS_STORM_PRECIP

!***********************************************************************
      SUBROUTINE precip_storm()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, MONTHS_PER_YEAR
      USE PRMS_STORM_PRECIP
      USE PRMS_MODULE, ONLY: Nhru, Nowmonth, Process_flag
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Basin_ppt, Basin_rain, Basin_snow, Basin_potet, &
                                  Hru_ppt, Hru_rain, Hru_snow, Potet
      USE PRMS_SET_TIME, ONLY: Timestep_days
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL :: print_module, read_error
! Local Variables
      INTEGER :: i, ii
      REAL :: pcor
!***********************************************************************
       IF ( Process_flag==RUN ) THEN
          Basin_ppt = 0.0
          Basin_rain = 0.0
          Basin_snow = 0.0
          Basin_potet = 0.0
          DO ii = 1, Active_hrus
            i = Hru_route_order(ii)
            IF ( Hru_ppt(i) > 0.0 ) THEN
              pcor = Strain_adj(i, Nowmonth) * Timestep_days
              Hru_ppt(i) = Hru_ppt(i) * pcor
              Hru_rain(i) = Hru_rain(i) * pcor
              Hru_snow(i) = Hru_ppt(i) - Hru_rain(i)
              ! assume potet is zero if precip on HRU, may need to change
              Potet(i) = Potet(i) * pcor
              Basin_ppt = Basin_ppt + DBLE( Hru_ppt(i) * Hru_area(i) )
              Basin_rain = Basin_rain + DBLE( Hru_rain(i) * Hru_area(i) )
              Basin_snow = Basin_snow + DBLE( Hru_snow(i) * Hru_area(i) )
              Basin_potet = Basin_potet + DBLE( Potet(i) * Hru_area(i) )
            ENDIF
          ENDDO
          Basin_ppt = Basin_ppt * Basin_area_inv
          Basin_potet = Basin_potet * Basin_area_inv
          Basin_rain = Basin_rain * Basin_area_inv
          Basin_snow = Basin_snow * Basin_area_inv

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_precip_potet_storm)

! declare parameters

      ALLOCATE ( Strain_adj(Nhru, MONTHS_PER_YEAR) )
      IF ( declparam(MODNAME, 'strain_adj', 'nhru,nmonths', 'real', &
           '1.0', '0.2', '5.0', &
           'Storm rain adjustment factor, by month for each HRU', &
           'Monthly factor to adjust measured precipitation for each HRU to account for differences in topography,'// &
           ' for kinematic or storm routing', &
           'decimal fraction')/=0 ) CALL read_error(1, 'strain_adj')

      ELSEIF ( Process_flag==INIT ) THEN
        IF ( getparam(MODNAME, 'strain_adj', Nhru*MONTHS_PER_YEAR, 'real', Strain_adj)/=0 ) CALL read_error(2, 'strain_adj')
      ENDIF

      END SUBROUTINE precip_storm


