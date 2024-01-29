!***********************************************************************
! Adjust distributed rain and snow with scaling factors and set potet to zero
! if precipitation on an HRU for each storm event
!***********************************************************************
      MODULE PRMS_PRECIP_POTET_ADJ_STORM
      IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Precipitation Distribution'
        character(len=*), parameter :: MODNAME = 'precip_potet_adj_storm'
        character(len=*), parameter :: Version_precip_potet_storm = '2024-01-24'
        ! Declared Dimension
        INTEGER, SAVE :: Nstorm
        ! Declared Parameter
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Strain_adj(:, :), Stsnow_adj(:, :), Storm_scale_factor(:)
      END MODULE PRMS_PRECIP_POTET_ADJ_STORM

!***********************************************************************
      SUBROUTINE precip_potet_adj_storm()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, SETDIMENS, Nmonths, MAXDIM
      USE PRMS_PRECIP_POTET_ADJ_STORM
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Nowmonth, Nhru_nmonths
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area_dble, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Basin_ppt, Basin_rain, Basin_snow, Basin_potet, Hru_ppt, Hru_rain, Hru_snow, Potet
      USE PRMS_SET_TIME, ONLY: Timestep_days, Storm_num
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam, decldim, getdim
      EXTERNAL :: read_error, print_module
! Local Variables
      INTEGER :: i, ii
!***********************************************************************
       IF ( Process_flag == RUN ) THEN
          Basin_ppt = 0.0D0
          Basin_rain = 0.0D0
          Basin_snow = 0.0D0
          Basin_potet = 0.0D0
          DO ii = 1, Active_hrus
            i = Hru_route_order(ii)
            IF ( Hru_ppt(i) > 0.0D0 ) THEN
              Hru_rain(i) = Hru_rain(i) * Strain_adj(i, Nowmonth) * Storm_scale_factor(Storm_num)
              Hru_snow(i) = Hru_snow(i) * Stsnow_adj(i, Nowmonth) * Storm_scale_factor(Storm_num)
              Hru_ppt(i) = Hru_ppt(i) + Hru_snow(i)
              ! assume potet is zero if precip on HRU, may need to change
              Potet(i) = 0.0D0
            ELSE
              Potet(i) = Potet(i) * Timestep_days
            ENDIF
            Basin_ppt = Basin_ppt + Hru_ppt(i) * Hru_area_dble(i)
            Basin_rain = Basin_rain + Hru_rain(i) * Hru_area_dble(i)
            Basin_snow = Basin_snow + Hru_snow(i) * Hru_area_dble(i)
            Basin_potet = Basin_potet + Potet(i)*Hru_area_dble(i)
          ENDDO
          Basin_ppt = Basin_ppt * Basin_area_inv
          Basin_potet = Basin_potet * Basin_area_inv
          Basin_rain = Basin_rain * Basin_area_inv
          Basin_snow = Basin_snow * Basin_area_inv

      ELSEIF ( Process_flag==SETDIMENS ) THEN
        IF ( decldim('nstorm', 1, MAXDIM, 'Number of storms in the simulation time period') /= 0 ) &
		     CALL read_error( 7, 'nstorm' )
 
      ELSEIF ( Process_flag == DECL ) THEN
        CALL print_module( MODDESC, MODNAME, Version_precip_potet_storm )
        Nstorm = getdim( 'nstorm' )
        IF ( Nstorm == -1 ) CALL read_error( 6, 'nstorm' )
        IF ( Nstorm < 1 ) Nstorm = 1

        ALLOCATE ( Storm_scale_factor(Nstorm) )
        IF ( declparam(MODNAME, 'storm_scale_factor', 'nstorm', 'double', &
             '1.0', '0.0', '5.0', &
             'Adjustment factor for each storm', 'Adjustment factor for each storm', &
             'decimal fraction') /= 0 ) CALL read_error( 1, 'storm_scale_factor' )
        ALLOCATE ( Strain_adj(Nhru, Nmonths) )
        IF ( declparam(MODNAME, 'strain_adj', 'nhru,nmonths', 'double', &
             '1.0', '0.2', '5.0', &
             'Storm rain adjustment factor, by month for each HRU', &
             'Monthly factor to adjust measured precipitation to'// &
             ' each HRU to account for differences in elevation,'// &
             ' etc. This factor is for the rain gage used for storm flow routing', &
             'decimal fraction') /= 0 ) CALL read_error( 1, 'strain_adj' )
        ALLOCATE ( Stsnow_adj(Nhru, Nmonths) )
        IF ( declparam(MODNAME, 'stsnow_adj', 'nhru,nmonths', 'double', &
             '1.0', '0.2', '5.0', &
             'Storm snow adjustment factor, by month for each HRU', &
             'Monthly factor to adjust distributed snow for each HRU to account for differences storm patterns', &
             'decimal fraction') /= 0 ) CALL read_error( 1, 'stsnow_adj' )

      ELSEIF ( Process_flag==INIT ) THEN
        IF ( getparam(MODNAME, 'strain_adj', Nhru_nmonths, 'double', Strain_adj) /= 0 ) CALL read_error(2, 'strain_adj')
        IF ( getparam(MODNAME, 'stsnow_adj', Nhru_nmonths, 'double', Stsnow_adj) /= 0 ) CALL read_error(2, 'stsnow_adj')
      ENDIF

      END SUBROUTINE precip_potet_adj_storm
