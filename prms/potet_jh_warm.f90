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
        character(len=*), parameter :: Version_potet = '2024-01-31'
        ! Local Variables
        double precision, allocatable, save :: coef_t_mean(:,:), temp_x_mean(:,:)
        ! Declared Parameters
        double precision, save, allocatable :: Jh_coef_warm(:, :)
        double precision, save, allocatable :: Tmax_month_mean_warm_C(:, :), Tmin_month_mean_warm_C(:, :)
      END MODULE PRMS_POTET_JH_WARM

      SUBROUTINE potet_jh_warm()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, INCH2CM, Nmonths
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Nowmonth, Nhru_nmonths
      USE PRMS_POTET_JH_WARM
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area_dble, Hru_route_order, Hru_elev_feet
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf, Swrad
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      double precision, external :: sat_vapor_press
      EXTERNAL :: read_error, print_module
! Local Variables
      INTEGER :: i, j
      double precision :: elh, coef_h, elev_slope, satvapor, temp, temp2
!***********************************************************************
      IF ( Process_flag == RUN ) THEN
!***********************************************************************
! 597.3 cal/gm at 0 C is the energy required to change the state of 
! water to vapor
! elh is the latent heat of vaporization (not including the *2.54)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          elh = ( 597.3D0-(0.5653D0*Tavgc(i)) ) * INCH2CM
          Potet(i) = ( Jh_coef_warm(i,Nowmonth)*coef_t_mean(i,Nowmonth) ) * ( (Tavgf(i)-temp_x_mean(i,Nowmonth)) * (Swrad(i)/elh) )
          IF ( Potet(i) < 0.0D0 ) Potet(i) = 0.0D0
          Basin_potet = Basin_potet + Potet(i) * Hru_area_dble(i)
        ENDDO
        Basin_potet = Basin_potet * Basin_area_inv

!******Declare parameters
      ELSEIF ( Process_flag == DECL ) THEN
        CALL print_module( MODDESC, MODNAME, Version_potet )

        ALLOCATE ( Jh_coef_warm(Nhru, Nmonths) )
        IF ( declparam(MODNAME, 'jh_coef_warm', 'nhru,nmonths', 'double', &
             '1.0', '0.1', '2.0', &
             'Potential ET monthly coefficient adjustment factor for each HRU', &
             'Potential ET monthly coefficient adjustment factor for each HRU', &
             'decimal fraction') /= 0 ) CALL read_error( 1, 'jh_coef_warm' )

        ALLOCATE ( Tmax_month_mean_warm_C(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'tmax_month_mean_warm_C', 'nhru,nmonths', 'double', &
             '24.0', '-10.0', '150.0', &
             'Mean maximum air temperature for simulation time period', &
             'Mean maximum air temperature for simulation time period', &
             'degrees Celsius') /= 0 ) CALL read_error( 1, 'tmax_month_mean_warm_C' )

        ALLOCATE ( Tmin_month_mean_warm_C(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'tmin_month_mean_warm_C', 'nhru,nmonth', 'double', &
             '24.0', '-10.0', '150.0', &
             'Mean minimum air temperature for simulation time period', &
             'Mean minimum air temperature for simulation time period', &
             'degrees Celsius') /= 0 ) CALL read_error( 1, 'tmin_month_mean_warm_C' )

!******Get parameters
      ELSEIF ( Process_flag == INIT ) THEN
        IF ( getparam(MODNAME, 'jh_coef_warm', Nhru_nmonths, 'double', Jh_coef_warm) /= 0 ) CALL read_error( 2, 'jh_coef_warm' )

        IF ( getparam(MODNAME, 'tmax_month_mean_warm_C', Nhru_nmonths, 'double', &
             Tmax_month_mean_warm_C) /= 0 ) CALL read_error( 2, 'tmax_month_mean_warm_C' )

        IF ( getparam(MODNAME, 'tmin_month_mean_warm_C', Nhru_nmonths, 'double', &
             Tmin_month_mean_warm_C) /= 0 ) CALL read_error( 2, 'tmin_month_mean_warm_C' )

        ALLOCATE ( coef_t_mean(Nhru,Nmonths), temp_x_mean(Nhru,Nmonths) )
        DO i = 1, Nhru
          elev_slope = Hru_elev_feet(i) / 1000.0D0
          DO j = 1, Nmonths
		    temp = Tmax_month_mean_warm_C(i,j)
            temp2 = Tmin_month_mean_warm_C(i,j)
            satvapor = sat_vapor_press(temp) - sat_vapor_press(temp2)
            coef_h = 50.0D0 / satvapor ! mb/mb
            ! degrees F
            temp_x_mean(i,j) = 27.5D0 - 0.25D0*satvapor - elev_slope
            ! degrees F
            coef_t_mean(i,j) = 1.0D0 / (68.0D0 - 3.6D0*elev_slope + 13.0D0*coef_h)
          ENDDO
        ENDDO
        DEALLOCATE ( Tmax_month_mean_warm_C, Tmin_month_mean_warm_C )
      ENDIF

      END SUBROUTINE potet_jh_warm
