!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1969)
!***********************************************************************
      INTEGER FUNCTION potet_jh()
      USE PRMS_MODULE, ONLY: Process, Nhru, Print_debug, Version_potet_jh, Potet_jh_nc
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf, Swrad
      USE PRMS_OBS, ONLY: Nowmonth
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, getparam
      EXTERNAL read_error
! Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Jh_coef_hru(:), Jh_coef(:)
! Local Variables
      INTEGER :: i, j
      REAL :: elh, jhcoef_mon
      
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='potet_jh')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Potential Evapotranspiration')
      
!***********************************************************************
      potet_jh = 1

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
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

!******Declare parameters
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_jh = '$Id: potet_jh.f90 4077 2012-01-05 23:46:06Z rsregan $'
        Potet_jh_nc = INDEX( Version_potet_jh, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(MODNAME, PROCNAME, Version_potet_jh(:Potet_jh_nc))/=0 ) STOP
        ENDIF

        ALLOCATE ( Jh_coef(12) )
        IF ( declparam(MODNAME, 'jh_coef', 'nmonths', 'real', &
             '0.014', '0.005', '0.060', &
             'Monthly air temp coefficient - Jensen-Haise', &
             'Monthly (January to December) air temperature coefficient used in Jensen-Haise potential ET computations', &
             'per degrees F')/=0 ) CALL read_error(1, 'jh_coef')
        ALLOCATE ( Jh_coef_hru(Nhru) )
        IF ( declparam(MODNAME, 'jh_coef_hru', 'nhru', 'real', &
             '13.0', '5.0', '20.0', &
             'HRU air temp coefficient - Jensen-Haise', &
             'Air temperature coefficient used in Jensen-Haise potential ET computations for each HRU', &
             'degrees F')/=0 ) CALL read_error(1, 'jh_coef_hru')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'jh_coef', 12, 'real', Jh_coef)/=0 ) CALL read_error(2, 'jh_coef')
        IF ( getparam(MODNAME, 'jh_coef_hru', Nhru, 'real', Jh_coef_hru)/=0 ) CALL read_error(2, 'jh_coef_hru')
      ENDIF

      potet_jh = 0
      END FUNCTION potet_jh
