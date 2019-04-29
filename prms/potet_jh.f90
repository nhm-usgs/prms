!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970)
!     Potet = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
!***********************************************************************
      MODULE PRMS_POTET_JH
        IMPLICIT NONE
        ! Local Variable
        CHARACTER(LEN=8), SAVE :: MODNAME
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Jh_coef_hru(:)
        REAL, SAVE :: Jh_coef(12)
      END MODULE PRMS_POTET_JH

      INTEGER FUNCTION potet_jh()
      USE PRMS_POTET_JH
      USE PRMS_MODULE, ONLY: Process, Nhru, Parameter_check_flag, Inputerror_flag
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf, Swrad
      USE PRMS_SET_TIME, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, check_param_limits
! Local Variables
      INTEGER :: i, j, k
      REAL :: elh, jhcoef_mon
      CHARACTER(LEN=80), SAVE :: Version_potet_jh
!***********************************************************************
      potet_jh = 0

      IF ( Process(:3)=='run' ) THEN
!***********************************************************************
! 597.3 cal/gm at 0 C is the energy required to change the state of 
! water to vapor
! elh is the latent heat of vaporization (not including the *2.54)
        jhcoef_mon = Jh_coef(Nowmonth)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          elh = (597.3-(0.5653*Tavgc(i)))*2.54
          Potet(i) = jhcoef_mon*(Tavgf(i)-Jh_coef_hru(i))*Swrad(i)/elh
          IF ( Potet(i)<NEARZERO ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

!******Declare parameters
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_jh = '$Id: potet_jh.f90 6835 2014-10-10 19:18:29Z rsregan $'
        CALL print_module(Version_potet_jh, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_jh'

        IF ( declparam(MODNAME, 'jh_coef', 'nmonths', 'real', &
     &       '0.014', '0.005', '0.1', &
     &       'Monthly air temperature coefficient - Jensen-Haise', &
     &       'Monthly (January to December) air temperature coefficient'// &
     &       ' used in Jensen-Haise potential ET computations', &
     &       'per degrees Fahrenheit')/=0 ) CALL read_error(1, 'jh_coef')

        ALLOCATE ( Jh_coef_hru(Nhru) )
        IF ( declparam(MODNAME, 'jh_coef_hru', 'nhru', 'real', &
     &       '13.0', '5.0', '25.0', &
     &       'HRU air temperature coefficient - Jensen-Haise', &
     &       'Air temperature coefficient used in Jensen-Haise potential ET computations for each HRU', &
     &       'degrees Fahrenheit')/=0 ) CALL read_error(1, 'jh_coef_hru')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'jh_coef', 12, 'real', Jh_coef)/=0 ) CALL read_error(2, 'jh_coef')
        IF ( getparam(MODNAME, 'jh_coef_hru', Nhru, 'real', Jh_coef_hru)/=0 ) CALL read_error(2, 'jh_coef_hru')
        IF ( Parameter_check_flag>0 ) THEN
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            CALL check_param_limits(i, 'jh_coef_hru', Jh_coef_hru(i), -50.0, 150.0, Inputerror_flag)
          ENDDO
          DO k = 1, 12
            CALL check_param_limits(k, 'jh_coef', Jh_coef(k), -1.0, 10.0, Inputerror_flag)
          ENDDO
        ENDIF

      ENDIF

      END FUNCTION potet_jh
