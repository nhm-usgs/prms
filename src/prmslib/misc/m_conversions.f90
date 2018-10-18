module conversions_mod
  use prms_constants, only: dp
  use variableKind
  implicit none

  interface c_to_f
    pure elemental module function c_to_f_r32(temperature) result(res)
      real(r32) :: res
      real(r32), intent(in) :: temperature
    end function

    pure elemental module function c_to_f_r64(temperature) result(res)
      real(r64) :: res
      real(r64), intent(in) :: temperature
    end function
  end interface

  interface f_to_c
    pure elemental module function f_to_c_r32(temperature) result(res)
      real(r32) :: res
      real(r32), intent(in) :: temperature
        !! Temperature in Fahrenheit
    end function

    pure elemental module function f_to_c_r64(temperature) result(res)
      real(r64) :: res
      real(r64), intent(in) :: temperature
        !! Temperature in Fahrenheit
    end function
  end interface

  interface f_to_c_diff
    pure elemental module function f_to_c_diff_r32(temperature) result(res)
      real(r32) :: res
      real(r32), intent(in) :: temperature
        !! Temperature difference (relative temperature) in Fahrenheit
    end function

    pure elemental module function f_to_c_diff_r64(temperature) result(res)
      real(r64) :: res
      real(r64), intent(in) :: temperature
        !! Temperature difference (relative temperature) in Fahrenheit
    end function
  end interface

  interface sat_vapor_press
    pure elemental module function sat_vapor_press_r32(temp_c) result(res)
      real(r32) :: res
      real(r32), intent(in) :: temp_c
        !! Temperature in degree Celsius
    end function
  end interface

  contains
    !***********************************************************************
    ! Convert Fahrenheit to Celsius
    !***********************************************************************
    pure elemental module function f_to_c_r32(temperature) result(res)
      implicit none

      ! Arguments
      real(r32) :: res
      real(r32), intent(in) :: temperature
        !! Temperature in Fahrenheit

      !*******************************************************************
      res = (temperature - 32.0) / 1.8
    end function

    pure elemental module function f_to_c_r64(temperature) result(res)
      implicit none

      ! Arguments
      real(r64) :: res
      real(r64), intent(in) :: temperature
        !! Temperature in Fahrenheit

      !*******************************************************************
      res = (temperature - 32.0_dp) / 1.8_dp
    end function

    !***********************************************************************
    ! Convert Celsius to Fahrenheit
    !***********************************************************************
    pure elemental module function c_to_f_r32(temperature) result(res)
      implicit none

      ! Arguments
      real(r32) :: res
      real(r32), intent(in) :: temperature
        !! Temperature in Celsius

      !*******************************************************************
      res = temperature * 1.8 + 32.0
    end function

    pure elemental module function c_to_f_r64(temperature) result(res)
      implicit none

      ! Arguments
      real(r64) :: res
      real(r64), intent(in) :: temperature
        !! Temperature in Celsius

      !*******************************************************************
      res = temperature * 1.8_dp + 32.0_dp
    end function

    !***********************************************************************
    ! Compute saturation vapor pressure over water in millibars
    ! 6th order Polynominal method (Flatau et. all., 1992) valid: -50 to 50C
    ! 1 kPa = 10 millibars
    ! Flatau, P.j., Walko, R.L., Cotton, W.R., 1992, Polynomial Fits to
    !   saturation vapor pressure: Jornal of Applied Meteorology, v. 31, p. 1507-1513
    !***********************************************************************
    pure elemental function sat_vapor_press_poly(temp_c) result(res)
      implicit none

      ! Arguments
      real(r32) :: res
      real(r32), intent(in) :: temp_c
        !! Temperature in degree Celsius

      !*******************************************************************
      res = 6.11176750 + 0.443986062 * temp_c + 0.0143053301 * temp_c**2 + &
            0.265027242E-03 * temp_c**3 + 0.302246994E-05 * temp_c**4 + &
            0.203886313E-07 * temp_c**5 + 0.638780966E-10 * temp_c**6
      ! Mastin documentation for potet_dpm
      !      sat_vapor_press_poly = 23.38*exp(18.1-5303.3/(temp_c+273.0))
      ! Mastin documentation for param_leaf-loss.aml
      !      sat_vapor_press_poly = 6.1078*EXP(17.269*temp_c/(237.30D0+temp_c))
      ! Buck Research Manual (1996)
      !      sat_vapor_press_poly = 6.1121D0*EXP((18.678D0-temp_c/234.5D0)*temp_c/(257.14+temp_c))
      ! WMO 2008, CIMO Guide
      !      sat_vapor_press_poly = 6.112*EXP(17.62*temp_c/(243.12+temp_c))
      ! Irmak and others (2012), equation 12
      !      sat_vapor_press_poly = 0.6108*EXP(17.27*temp_c/(237.3+temp_c))
    end function

    !***********************************************************************
    ! Compute saturation vapor pressure over water
    ! Irmak and others (2012), equation 12
    !***********************************************************************
    pure elemental module function sat_vapor_press_r32(temp_c) result(res)
      implicit none

      ! Arguments
      real(r32) :: res
      real(r32), intent(in) :: temp_c
        !! Temperature in degree Celsius

      !*******************************************************************
      res = 6.1078 * EXP((17.26939 * temp_c) / (237.3 + temp_c))
    end function


    !***********************************************************************
    ! Convert temperature difference from Fahrenheit to Celsius
    !***********************************************************************
    pure elemental module function f_to_c_diff_r32(temperature) result(res)
      implicit none

      ! Arguments
      real(r32) :: res
      real(r32), intent(in) :: temperature
        !! Temperature difference (relative temperature) in Fahrenheit

      !*******************************************************************
      res = temperature / 1.8
    end function

    pure elemental module function f_to_c_diff_r64(temperature) result(res)
      implicit none

      ! Arguments
      real(r64) :: res
      real(r64), intent(in) :: temperature
        !! Temperature difference (relative temperature) in Fahrenheit

      !*******************************************************************
      res = temperature / 1.8_dp
    end function
end module
