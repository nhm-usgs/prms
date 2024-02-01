!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970)
!     Potet = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
!***********************************************************************
      MODULE PRMS_POTET_JH_MEAN
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
        character(len=*), parameter :: MODNAME = 'potet_jh_mean'
        character(len=*), parameter :: Version_potet = '2024-01-31'
        INTEGER, SAVE :: Last_year, Ndays
        DOUBLE PRECISION, ALLOCATABLE, SAVE :: Coef_t_mean(:, :), Temp_x_mean(:, :)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Tmax_month(:, :), Tmin_month(:, :)
        ! Declared Parameters
        DOUBLE PRECISION, ALLOCATABLE, SAVE :: Tmax_month_mean_warm_C(:, :), Tmin_month_mean_warm_C(:, :)
        DOUBLE PRECISION, SAVE, ALLOCATABLE :: Jh_coef_hru_month(:, :)
      END MODULE PRMS_POTET_JH_MEAN

      INTEGER FUNCTION potet_jh_mean()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, Nmonths, INCH2CM, ACTIVE, OFF, NEARZERO, READ_INIT, SAVE_INIT
      USE PRMS_POTET_JH_MEAN
      USE PRMS_MODULE, ONLY: Process_flag, Nhru, Nowyear, Nowmonth, Nhru_nmonths, &
     &    Save_vars_to_file, Init_vars_from_file, Start_year
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area_dble, Hru_route_order, &
     &    Hru_elev_feet
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf, Swrad, Tminc, Tmaxc
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, potet_jh_mean_restart, coef_t_jh
! Local Variables
      INTEGER :: i, j, k, iflg
      DOUBLE PRECISION :: elh, warm_month_temp
!***********************************************************************
      potet_jh_mean = 0

      IF ( Process_flag == RUN ) THEN
!***********************************************************************
!******Compute potential et for each HRU using Jensen-Haise formulation
! 597.3 cal/gm at 0 C is the energy required to change the state of 
! water to vapor
! elh is the latent heat of vaporization (not including the *2.54)

        iflg = 0
        ! may want running mean rather than every 10 years
        IF ( Nowyear-Last_year==10 ) iflg = 1
        Ndays = Ndays + 1
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( iflg==1 ) THEN
            warm_month_temp = -500.0D0
            DO k = 1, Nmonths
              Tmin_month(i, k) = Tmin_month(i, k) / DBLE( Ndays )
              Tmax_month(i, k) = Tmax_month(i, k)/ DBLE( Ndays )
              IF ( Tmin_month(i,k)+Tmax_month(i,k) > warm_month_temp ) THEN
                warm_month_temp = Tmin_month(i, k) + Tmax_month(i, k)
                Tmax_month_mean_warm_C(i, k) = Tmax_month(i, k)
                Tmin_month_mean_warm_C(i, k) = Tmin_month(i, k)
              ENDIF
              CALL coef_t_jh(Hru_elev_feet(i), Temp_x_mean(i,Nowmonth), Coef_t_mean(i,Nowmonth), &
                             Tmax_month_mean_warm_C(i,Nowmonth), Tmin_month_mean_warm_C(i,Nowmonth))
            ENDDO
          ENDIF
          elh = (597.3D0-(0.5653D0*Tavgc(i))) * INCH2CM
          Potet(i) = Jh_coef_hru_month(i, Nowmonth)*Coef_t_mean(i,Nowmonth)*(Tavgf(i)-Temp_x_mean(i,Nowmonth))*Swrad(i)/elh
          IF ( Potet(i)<0.0D0 ) Potet(i) = 0.0D0
          Basin_potet = Basin_potet + Potet(i)*Hru_area_dble(i)
          Tmin_month(i, Nowmonth) = Tmin_month(i, Nowmonth) + Tminc(i)
          Tmax_month(i, Nowmonth) = Tmax_month(i, Nowmonth) + Tmaxc(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
        IF ( iflg==1 ) THEN
          Last_year = Nowyear
          Ndays = 0
        ENDIF

!******Declare parameters
      ELSEIF ( Process_flag == DECL ) THEN
        CALL print_module( MODDESC, MODNAME, Version_potet )

        ALLOCATE ( Jh_coef_hru_month(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'jh_coef_hru_month', 'nhru,nmonths', 'double', &
             '0.014', '0.005', '0.06', &
             'Monthly Jensen-Haise air temperature coefficient', &
             'Monthly (January to December) Jensen-Haise potential ET air temperature coefficient for each HRU', &
             'per degrees Fahrenheit')/=0 ) CALL read_error(1, 'jh_coef_hru_month')

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

        ALLOCATE ( Coef_t_mean(Nhru, Nmonths), Temp_x_mean(Nhru, Nmonths) )
        ALLOCATE ( Tmax_month(Nhru, Nmonths), Tmin_month(Nhru, Nmonths) )

!******Get parameters
      ELSEIF ( Process_flag == INIT ) THEN
        IF ( getparam(MODNAME, 'jh_coef_hru_month', Nhru_nmonths, 'double', Jh_coef_hru_month)/=0 ) &
     &       CALL read_error(2, 'jh_coef_hru_month')
        IF ( getparam(MODNAME, 'tmax_month_mean_warm_C', Nhru_nmonths, 'double', &
     &       Tmax_month_mean_warm_C)/=0 ) CALL read_error(2, 'tmax_month_mean_warm_C')
        IF ( getparam(MODNAME, 'tmin_month_mean_warm_C', Nhru_nmonths, 'double', &
     &       Tmin_month_mean_warm_C)/=0 ) CALL read_error(2, 'tmin_month_mean_warm_C')

        Temp_x_mean = 0.0D0
        Coef_t_mean = 0.0D0
        DO j = 1, Active_hrus
          DO i = 1, Hru_route_order(j)
            DO k = 1, Nmonths
              CALL coef_t_jh(Hru_elev_feet(i), Temp_x_mean(i,k), Coef_t_mean(i,k), Tmax_month_mean_warm_C(i,k), &
                             Tmin_month_mean_warm_C(i,k))
            ENDDO
          ENDDO
        ENDDO

        Last_year = Start_year
        Ndays = 0

        IF ( Init_vars_from_file > OFF ) THEN
          CALL potet_jh_mean_restart(READ_INIT)
        ELSE
          Tmax_month = 0.0D0
          Tmin_month = 0.0D0
        ENDIF

      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL potet_jh_mean_restart(SAVE_INIT)
      ENDIF

      END FUNCTION potet_jh_mean

!***********************************************************************
!     Compute temperature coefficient
!***********************************************************************
      SUBROUTINE coef_t_jh(Hru_elev, Temp_x_mean, Coef_t_mean, Tmax_month_mean_warm_C, &
     &                     Tmin_month_mean_warm_C)
      IMPLICIT NONE
      ! Function
      DOUBLE PRECISION, EXTERNAL :: sat_vapor_press_poly
      ! Argument
      DOUBLE PRECISION, INTENT(IN) :: Hru_elev
      DOUBLE PRECISION, INTENT(OUT) :: Tmax_month_mean_warm_C, Tmin_month_mean_warm_C
      DOUBLE PRECISION, INTENT(OUT) :: Coef_t_mean, Temp_x_mean
      ! Local Variables
      DOUBLE PRECISION :: elev_slope, coef_h, satvapor
!***********************************************************************
      elev_slope = Hru_elev/1000.0D0
      satvapor = sat_vapor_press_poly(Tmax_month_mean_warm_C) &
     &           - sat_vapor_press_poly(Tmin_month_mean_warm_C)
      IF ( .not.(satvapor>0.0D0) ) satvapor = 1.0D0
      coef_h = 50.0D0/satvapor ! mb/mb
      ! degrees F
      Temp_x_mean = 27.5D0 - 0.25D0*satvapor - elev_slope
      ! degrees F
      Coef_t_mean = 1.0D0/(68.0D0 - 3.6D0*elev_slope + 13.0D0*coef_h)
      Tmin_month_mean_warm_C = 0.0D0  ! reset values for 10 year mean
      Tmax_month_mean_warm_C = 0.0D0
      END SUBROUTINE coef_t_jh

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_jh_mean_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, text_restart_flag
      USE PRMS_POTET_JH_MEAN
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        IF ( text_restart_flag==OFF ) THEN
          WRITE ( Restart_outunit ) MODNAME
          WRITE ( Restart_outunit ) Tmin_month
          WRITE ( Restart_outunit ) Tmax_month
        ELSE
          WRITE ( Restart_outunit, * ) MODNAME
          WRITE ( Restart_outunit, * ) Tmin_month
          WRITE ( Restart_outunit, * ) Tmax_month
        ENDIF
      ELSE
        IF ( text_restart_flag==OFF ) THEN
          READ ( Restart_inunit ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit ) Tmin_month
          READ ( Restart_inunit ) Tmax_month
        ELSE
          READ ( Restart_inunit, * ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit, * ) Tmin_month
          READ ( Restart_inunit, * ) Tmax_month
        ENDIF
      ENDIF
      END SUBROUTINE potet_jh_mean_restart
