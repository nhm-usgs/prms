!***********************************************************************
! Computes the potential evapotranspiration using the Hamon
! formulation (Hamon, 1961)
!***********************************************************************
      MODULE PRMS_POTET_HAMON
        IMPLICIT NONE
        CHARACTER(LEN=11), SAVE :: MODNAME
        ! Declared Parameters
        REAL, SAVE :: Hamon_coef(12)
      END MODULE PRMS_POTET_HAMON

      INTEGER FUNCTION potet_hamon()
      USE PRMS_POTET_HAMON, ONLY: MODNAME, Hamon_coef
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order, Basin_area_inv, Timestep
      USE PRMS_CLIMATEVARS, ONLY: Tavgc, Basin_potet, Potet
      USE PRMS_SOLTAB, ONLY: Soltab_sunhrs
      USE PRMS_OBS, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC EXP
      INTEGER, EXTERNAL :: getparam, declparam
      EXTERNAL read_error, print_module, potet_hamon_restart
! Local Variables
      INTEGER :: i, j
      REAL :: dyl, vpsat, vdsat, hamoncoef_mo
      CHARACTER(LEN=80), SAVE :: Version_potet_hamon
!***********************************************************************
      potet_hamon = 0

      IF ( Process(:3)=='run' ) THEN
!******Compute potential et for each HRU using Hamon formulation
        hamoncoef_mo = Hamon_coef(Nowmonth)
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
! Convert daylength from hours to 12 hour multiple (equal day and night period)
          dyl = Soltab_sunhrs(Jday, i)/12.0
          vpsat = 6.108*EXP(17.26939*Tavgc(i)/(Tavgc(i)+237.3))
          vdsat = 216.7*vpsat/(Tavgc(i)+273.3)
          Potet(i) = hamoncoef_mo*dyl*dyl*vdsat
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + Potet(i)*Hru_area(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet_hamon = '$Id: potet_hamon.f90 5603 2013-04-23 18:43:36Z rsregan $'
        CALL print_module(Version_potet_hamon, 'Potential ET              ', 90)
        MODNAME = 'potet_hamon'

        IF ( Timestep/=0 ) RETURN

        IF ( declparam(MODNAME, 'hamon_coef', 'nmonths', 'real', &
     &       '0.0055', '0.004', '0.008', &
     &       'Monthly air temperature coefficient - Hamon', &
     &       'Monthly (January to December) air temperature coefficient used in Hamon potential ET computations', &
     &       '????')/=0 ) CALL read_error(1, 'hamon_coef')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Timestep/=0 ) THEN
          CALL potet_hamon_restart(1)
          RETURN
        ENDIF
        IF ( getparam(MODNAME, 'hamon_coef', 12, 'real', Hamon_coef)/=0 ) CALL read_error(2, 'hamon_coef')

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL potet_hamon_restart(0)
      ENDIF

      END FUNCTION potet_hamon

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_hamon_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_POTET_HAMON
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Hamon_coef
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Hamon_coef
      ENDIF
      END SUBROUTINE potet_hamon_restart
