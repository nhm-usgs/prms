!***********************************************************************
! Computes the potential evapotranspiration using the Hamon
! formulation (Hamon, 1961); modification of potet_hamon_prms
!***********************************************************************
      INTEGER FUNCTION potet_hamon()
      USE PRMS_MODULE, ONLY: Process, Print_debug, Nhru, &
          Version_potet_hamon, Potet_hamon_nc
      USE PRMS_BASIN, ONLY: Hru_area, Active_hrus, Hru_route_order, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Tavgc, Basin_potet, Potet
      USE PRMS_SOLTAB, ONLY: Soltab_sunhrs
      USE PRMS_OBS, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, INDEX
      INTEGER, EXTERNAL :: getparam, declmodule, declparam
      EXTERNAL read_error
! Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Hamon_coef(:)
! Local Variables
      INTEGER :: i, j
      REAL :: dyl, vpsat, vdsat, hamoncoef_mo
!***********************************************************************
      potet_hamon = 1

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
        Version_potet_hamon = '$Id: potet_hamon.f90 3796 2011-10-25 16:42:38Z rsregan $'
        Potet_hamon_nc = INDEX( Version_potet_hamon, ' $' ) + 1
        IF ( Print_debug>-1 ) THEN
          IF ( declmodule(Version_potet_hamon(:Potet_hamon_nc))/=0 ) STOP
        ENDIF

        ALLOCATE ( Hamon_coef(12) )
        IF ( declparam('potet', 'hamon_coef', 'nmonths', 'real', &
             '0.0055', '0.004', '0.008', &
             'Monthly air temp coefficient - Hamon', &
             'Monthly (January to December) air temperature coefficient used in Hamon potential ET computations', &
             '????')/=0 ) CALL read_error(1, 'hamon_coef')

      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam('potet', 'hamon_coef', 12, 'real', Hamon_coef)/=0 ) CALL read_error(2, 'hamon_coef')
      ENDIF

      potet_hamon = 0
      END FUNCTION potet_hamon

