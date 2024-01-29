!***********************************************************************
! Set potet to zero and adjust precipitation on HRUs for the timestep during storm mode
!   Declared Parameters
!     strain_adj, storm_scale_factor
!***********************************************************************
      MODULE STORM_PRECIP
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Adjust precipitation during storm'
      character(len=*), parameter :: MODNAME = 'precip_storm'
      character(len=*), parameter :: Version_precip_storm = '2024-01-02'
      integer :: Nstorm
!   Declared Parameters
      double precision, save, allocatable :: Strain_adj(:, :), Storm_scale_factor(:)
      END MODULE STORM_PRECIP

!***********************************************************************
!     Main precip_storm routine
!***********************************************************************
      SUBROUTINE precip_storm()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, MAXDIM, SETDIMENS, Nmonths
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Nowmonth, Nhru_nmonths
      USE STORM_PRECIP
	  USE PRMS_BASIN, ONLY: Hru_area_dble, Basin_area_inv, Active_hrus, Hru_route_order
	  USE PRMS_CLIMATEVARS, ONLY: Basin_ppt, Basin_rain, Basin_snow, Basin_potet, &
                                  Hru_ppt, Hru_rain, Hru_snow, Potet
      USE PRMS_SET_TIME, ONLY: Storm_num
      IMPLICIT NONE
! Functions
      integer, external :: declparam, getparam, getdim, decldim
	  external :: read_error, print_module
! Local Variables
      integer :: i, ii
      double precision :: pcor
!***********************************************************************
      IF ( Process_flag == RUN ) THEN
        Basin_ppt = 0.0D0
        Basin_rain = 0.0D0
        Basin_snow = 0.0D0
        Basin_potet = 0.0D0
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          IF ( Hru_ppt(i) > 0.0D0 ) THEN
            pcor = Strain_adj(i, Nowmonth) * Storm_scale_factor(Storm_num)
            Hru_ppt(i) = Hru_ppt(i) * pcor
            Hru_rain(i) = Hru_rain(i) * pcor
            Hru_snow(i) = Hru_ppt(i) - Hru_rain(i)
            ! assume potet is zero if precip on HRU, may need to change
            Potet(i) = 0.0D0
            Basin_ppt = Basin_ppt + Hru_ppt(i) * Hru_area_dble(i)
            Basin_rain = Basin_rain + Hru_rain(i) * Hru_area_dble(i)
            Basin_snow = Basin_snow + Hru_snow(i) * Hru_area_dble(i)
            Basin_potet = Basin_potet + Potet(i)*Hru_area_dble(i)
          ENDIF
        ENDDO
        Basin_ppt = Basin_ppt * Basin_area_inv
        Basin_potet = Basin_potet * Basin_area_inv
        Basin_rain = Basin_rain * Basin_area_inv
        Basin_snow = Basin_snow * Basin_area_inv

      ELSEIF ( Process_flag == SETDIMENS ) THEN
! declare parameters
        IF ( decldim('nstorm', 0, MAXDIM, &
             'Number of storms in the simulation time period' ) /= 0 ) CALL read_error( 7, 'nstorm' )

      ELSEIF ( Process_flag == DECL ) THEN
        CALL print_module( MODDESC, MODNAME, Version_precip_storm )

! declare parameters
        Nstorm = getdim('nstorm')  !! rsr move nstorm to call_modules
        IF ( Nstorm == -1 ) CALL read_error( 7, 'nstorm' )

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

      ELSEIF ( Process_flag == INIT ) THEN
        IF ( getparam(MODNAME, 'storm_scale_factor', Nstorm, 'double', &
             Storm_scale_factor) /= 0 ) CALL read_error( 2, 'storm_scale_factor' )
        IF ( getparam(MODNAME, 'strain_adj', Nhru_nmonths, 'double', &
             Strain_adj) /= 0 ) CALL read_error( 2, 'strain_adj' )
      ENDIF

      END SUBROUTINE precip_storm
