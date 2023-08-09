!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970) by computing linear coefficient
! based on satuaration vapor pressure computed using mean tmin and tmax
! for each month over the simulation time period
!     Potet = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
!***********************************************************************
      MODULE PRMS_POTET_JH_WARM
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
        character(len=*), parameter :: MODNAME = 'potet_jh_warm'
        character(len=*), parameter :: Version_potet = '2022-04-01'
        ! Local Variables
        REAL, ALLOCATABLE, SAVE :: coef_t_mean(:), temp_x_mean(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Jh_coef(:, :), Tmax_month_mean_warm_C(:), Tmin_month_mean_warm_C(:)
      END MODULE PRMS_POTET_JH_WARM
!***********************************************************************
!***********************************************************************
      SUBROUTINE potet_jh_warm()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, MONTHS_PER_YEAR, INCH2CM
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Nowmonth
      USE PRMS_POTET_JH_WARM
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order, Hru_elev_feet
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf, Swrad
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE
      INTEGER, EXTERNAL :: declparam, getparam
      REAL, EXTERNAL :: sat_vapor_press_poly
      EXTERNAL :: read_error, print_module
! Local Variables
      INTEGER :: i, j
      REAL :: elh, coef_h, elev_slope, satvapor, potet_tmp
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
!***********************************************************************
! 597.3 cal/gm at 0 C is the energy required to change the state of 
! water to vapor
! elh is the latent heat of vaporization (not including the *2.54)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          elh = (597.3-(0.5653*Tavgc(i)))*INCH2CM
          potet_tmp = Jh_coef(i, Nowmonth) * coef_t_mean(i) *(Tavgf(i)-temp_x_mean(i)) * Swrad(i) / elh
          IF ( potet_tmp<0.0 ) potet_tmp = 0.0
          Basin_potet = Basin_potet + DBLE( potet_tmp*Hru_area(i) )
          Potet(i) = potet_tmp
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

!******Declare parameters
      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_potet)

        ALLOCATE ( Jh_coef(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(MODNAME, 'jh_coef', 'nhru,nmonths', 'real', &
     &       '0.014', '-0.5', '1.5', &
     &       'Monthly air temperature coefficient for each HRU - Jensen-Haise', &
     &       'Monthly (January to December) air temperature coefficient used in Jensen-Haise potential ET computations'// &
     &       ' for each HRU', &
     &       'per degrees Fahrenheit')/=0 ) CALL read_error(1, 'jh_coef')

        ALLOCATE ( Tmax_month_mean_warm_C(Nhru) )
        IF ( declparam(MODNAME, 'tmax_month_mean_warm_C', 'nhru', 'real', &
             '24.0', '-10.0', '150.0', &
             'Mean maximum air temperature for simulation time period', &
             'Mean maximum air temperature for simulation time period', &
             'degrees Celsius')/=0 ) CALL read_error(1, 'tmax_month_mean_warm_C')

        ALLOCATE ( Tmin_month_mean_warm_C(Nhru) )
        IF ( declparam(MODNAME, 'tmin_month_mean_warm_C', 'nhru', 'real', &
             '24.0', '-10.0', '150.0', &
             'Mean minimum air temperature for simulation time period', &
             'Mean minimum air temperature for simulation time period', &
             'degrees Celsius')/=0 ) CALL read_error(1, 'tmin_month_mean_warm_C')

!******Get parameters
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( getparam(MODNAME, 'jh_coef', Nhru*MONTHS_PER_YEAR, 'real', Jh_coef)/=0 ) CALL read_error(2, 'jh_coef')

        IF ( getparam(MODNAME, 'tmax_month_mean_warm_C', Nhru, 'real', &
             Tmax_month_mean_warm_C)/=0 ) CALL read_error(2, 'tmax_month_mean_warm_C')

        IF ( getparam(MODNAME, 'tmin_month_mean_warm_C', Nhru, 'real', &
             Tmin_month_mean_warm_C)/=0 ) CALL read_error(2, 'tmin_month_mean_warm_C')

        ALLOCATE ( coef_t_mean(Nhru), temp_x_mean(Nhru) )
        DO i = 1, Nhru
          elev_slope = Hru_elev_feet(i) / 1000.0
          satvapor = sat_vapor_press_poly(Tmax_month_mean_warm_C(i)) - sat_vapor_press_poly(Tmin_month_mean_warm_C(i))
          coef_h = 50.0 / satvapor ! mb/mb
          ! degrees F
          temp_x_mean(i) = 27.5 - 0.25*satvapor - elev_slope
          ! degrees F
          coef_t_mean(i) = 1.0 / (68.0 - 3.6*elev_slope + 13.0*coef_h)
        ENDDO
        DEALLOCATE ( Tmax_month_mean_warm_C, Tmin_month_mean_warm_C )
      ENDIF

      END SUBROUTINE potet_jh_warm
