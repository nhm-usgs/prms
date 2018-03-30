module conversions_mod
  use kinds_mod, only: r4, r8, i4, i8
  implicit none

  contains
    !***********************************************************************
    ! Convert Fahrenheit to Celsius
    !***********************************************************************
    REAL FUNCTION f_to_c(Temp)
      implicit none
      
      ! Arguments
      REAL(r4), INTENT(IN) :: Temp
        !! Temperature in Fahrenheit

      !*******************************************************************
      f_to_c = (Temp - 32.0) / 1.8
    END FUNCTION f_to_c

    !***********************************************************************
    ! Convert Celsius to Fahrenheit
    !***********************************************************************
    REAL FUNCTION c_to_f(Temp)
      implicit none

      ! Arguments
      REAL(r4), INTENT(IN) :: Temp
        !! Temperature in Celsius

      !*******************************************************************
      c_to_f = Temp * 1.8 + 32.0
    END FUNCTION c_to_f

    !***********************************************************************
    ! Compute saturation vapor pressure over water in millibars
    ! 6th order Polynominal method (Flatau et. all., 1992) valid: -50 to 50C
    ! 1 kPa = 10 millibars
    ! Flatau, P.j., Walko, R.L., Cotton, W.R., 1992, Polynomial Fits to
    !   saturation vapor pressure: Jornal of Applied Meteorology, v. 31, p. 1507-1513
    !***********************************************************************
    REAL FUNCTION sat_vapor_press_poly(Tempc)
      IMPLICIT NONE

      ! Arguments
      REAL(r4), INTENT(IN) :: Tempc
        !! Temperature in degree Celsius

      !*******************************************************************
      sat_vapor_press_poly = 6.11176750 + 0.443986062 * Tempc + &
                             0.0143053301 * Tempc**2 + &
                             0.265027242E-03 * Tempc**3 + &
                             0.302246994E-05 * Tempc**4 + &
                             0.203886313E-07 * Tempc**5 + &
                             0.638780966E-10 * Tempc**6
      ! Mastin documentation for potet_dpm
      !      sat_vapor_press_poly = 23.38*exp(18.1-5303.3/(Tempc+273.0))
      ! Mastin documentation for param_leaf-loss.aml
      !      sat_vapor_press_poly = 6.1078*EXP(17.269*Tempc/(237.30D0+Tempc))
      ! Buck Research Manual (1996)
      !      sat_vapor_press_poly = 6.1121D0*EXP((18.678D0-Tempc/234.5D0)*Tempc/(257.14+Tempc))
      ! WMO 2008, CIMO Guide
      !      sat_vapor_press_poly = 6.112*EXP(17.62*Tempc/(243.12+Tempc))
      ! Irmak and others (2012), equation 12
      !      sat_vapor_press_poly = 0.6108*EXP(17.27*Tempc/(237.3+Tempc))
    END FUNCTION sat_vapor_press_poly

    !***********************************************************************
    ! Compute saturation vapor pressure over water
    ! Irmak and others (2012), equation 12
    !***********************************************************************
    REAL FUNCTION sat_vapor_press(Tempc)
      IMPLICIT NONE

      ! Arguments
      REAL(r4), INTENT(IN) :: Tempc
        !! Temperature in degree Celsius

      !*******************************************************************
      sat_vapor_press = 6.1078 * EXP((17.26939 * Tempc) / (237.3 + Tempc))
    END FUNCTION sat_vapor_press
end module
