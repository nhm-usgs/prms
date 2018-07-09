!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970)
!     Potential_ET = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
!***********************************************************************
module PRMS_POTET_JH
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_POTET, only: Potential_ET
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_TEMPERATURE, only: Temperature
  implicit none

  private
  public :: Potet_jh

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
  character(len=*), parameter :: MODNAME = 'potet_jh'
  character(len=*), parameter :: MODVERSION = '2016-05-10 15:48:00Z'

  type, extends(Potential_ET) :: Potet_jh
    ! WARNING: tavg_f will be removed once temp_unit is standardized to Celsius.
    real(r32), private, allocatable :: tavg_f(:)

    contains
      procedure, public :: run => run_Potet_jh
  end type

  interface Potet_jh
    !! Potet_jh constructor
    module function constructor_Potet_jh(ctl_data) result(this)
      type(Potet_jh) :: this
        !! Poteh_jh class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine run_Potet_jh(this, ctl_data, param_data, model_basin, model_time, climate, model_solrad, model_temp)
      class(Potet_jh), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Climateflow), intent(in) :: climate
      class(SolarRadiation), intent(in) :: model_solrad
      class(Temperature), intent(in) :: model_temp

    end subroutine
  end interface

end module
