!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970)
!     Potet = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
! Used for potet_jh and potet_jh_hru
!***********************************************************************
      MODULE PRMS_POTET_JH
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=15), SAVE :: MODNAME
        ! Local or Parameter
        REAL, SAVE, ALLOCATABLE :: Jh_coef_hru_month(:, :)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Jh_coef_hru(:)
        REAL, SAVE :: Jh_coef(12)
      END MODULE PRMS_POTET_JH

      INTEGER FUNCTION potet_jh()
      USE PRMS_POTET_JH
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file, Et_flag
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf, Swrad
      USE PRMS_OBS, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getparam
      EXTERNAL read_error, print_module, potet_jh_restart
! Local Variables
      INTEGER :: i, j
      REAL :: elh
      CHARACTER(LEN=80), SAVE :: Version_potet_jh
!***********************************************************************
      potet_jh = 0

      IF ( Process(:3)=='run' ) THEN
!***********************************************************************
! 597.3 cal/gm at 0 C is the energy required to change the state of 
! water to vapor
! elh is the latent heat of vaporization (not including the *2.54)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          elh = (597.3-(0.5653*Tavgc(i)))*2.54
          Potet(i) = Jh_coef_hru_month(i, Nowmonth)*(Tavgf(i)-Jh_coef_hru(i))*Swrad(i)/elh
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

!******Declare parameters
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_jh = '$Id: potet_jh.f90 5660 2013-04-30 20:14:42Z rsregan $'
        IF ( Et_flag==1 ) THEN
          MODNAME = 'potet_jh'
        ELSE
          MODNAME = 'potet_jh_hru_mo'
          Version_potet_jh = Version_potet_jh(:13)//'_hru_mo'//Version_potet_jh(14:80)
        ENDIF
        CALL print_module(Version_potet_jh, 'Potential ET              ', 90)

        ALLOCATE ( Jh_coef_hru_month(Nhru,12), Jh_coef_hru(Nhru) )

        IF ( Timestep==0 ) THEN
          IF ( Et_flag==1 ) THEN
            IF ( declparam(MODNAME, 'jh_coef', 'nmonths', 'real', &
     &           '0.014', '0.005', '0.060', &
     &           'Monthly air temperature coefficient - Jensen-Haise', &
     &           'Monthly (January to December) air temperature coefficient used in Jensen-Haise potential ET computations', &
     &           'per degrees F')/=0 ) CALL read_error(1, 'jh_coef')
          ELSE
            IF ( declparam(MODNAME, 'jh_coef_hru_month', 'nhru,nmonths', 'real', &
     &           '0.014', '0.005', '0.060', &
     &           'Monthly Jensen-Haise air temperature coefficient', &
     &           'Monthly (January to December) Jensen-Haise potential ET air temperature coefficient for each HRU', &
     &           'per degrees F')/=0 ) CALL read_error(1, 'jh_coef_hru_month')
          ENDIF
          IF ( declparam(MODNAME, 'jh_coef_hru', 'nhru', 'real', &
     &         '13.0', '5.0', '25.0', &
     &         'HRU air temperature coefficient - Jensen-Haise', &
     &         'Air temperature coefficient used in Jensen-Haise potential ET computations for each HRU', &
     &         'degrees F')/=0 ) CALL read_error(1, 'jh_coef_hru')
        ENDIF

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL potet_jh_restart(1)
        ELSE
          IF ( Et_flag==1 ) THEN
            IF ( getparam(MODNAME, 'jh_coef', 12, 'real', Jh_coef)/=0 ) CALL read_error(2, 'jh_coef')
            DO j = 1, 12
              DO i = 1, Nhru
                Jh_coef_hru_month(i, j) = Jh_coef(j)
              ENDDO
            ENDDO
          ELSE
            IF ( getparam(MODNAME, 'jh_coef_hru_month', Nhru*12, 'real', Jh_coef_hru_month)/=0 ) &
     &           CALL read_error(2, 'jh_coef_hru_month')
          ENDIF
          IF ( getparam(MODNAME, 'jh_coef_hru', Nhru, 'real', Jh_coef_hru)/=0 ) CALL read_error(2, 'jh_coef_hru')
        ENDIF

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL potet_jh_restart(0)
      ENDIF

      END FUNCTION potet_jh

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_jh_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_POTET_JH
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=15) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Jh_coef_hru
        WRITE ( Restart_outunit ) Jh_coef_hru_month
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Jh_coef_hru
        READ ( Restart_inunit ) Jh_coef_hru_month
      ENDIF
      END SUBROUTINE potet_jh_restart
