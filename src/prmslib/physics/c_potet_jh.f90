!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970)
!     Potential_ET = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
!***********************************************************************
module PRMS_POTET_JH
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_POTET, only: Potential_ET
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Potet_jh

  character(len=*), parameter :: MODDESC = 'Potential ET - Jensen-Haise'
  character(len=*), parameter :: MODNAME = 'potet_jh'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:37:00Z'

  type, extends(Potential_ET) :: Potet_jh
    ! Parameters
    real(r32), pointer :: jh_coef(:, :) !rmcd changed to add access to bmi setter functions
      !! Monthly (January to December) air temperature coefficient used in Jensen-Haise potential ET computations for each HRU
    real(r32), pointer :: jh_coef_hru(:) !rmcd changed to add access to bmi setter functions
      !! Air temperature coefficient used in Jensen-Haise potential ET computations for each HRU

    ! WARNING: tavg_f will be removed once temp_unit is standardized to Celsius.
    ! real(r32), private, allocatable :: tavg_f(:)

    contains
      procedure, public :: init => init_Potet_jh
      procedure, public :: run => run_Potet_jh
      procedure, nopass, private :: calc_potet
  end type

  interface
    !! Potet_jh constructor
    module subroutine init_Potet_jh(this, ctl_data, model_basin, model_summary)
      class(Potet_jh), intent(inout) :: this
        !! Poteh_jh class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Potet_jh(this, ctl_data, model_basin, model_time, model_solrad, model_temp)
      class(Potet_jh), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad
      class(Temperature), intent(in) :: model_temp
    end subroutine
  end interface

  interface
    pure elemental module function calc_potet(tavg_c, tavg_f, jh_coef, jh_coef_hru, swrad) result(res)
      real(r32) :: res
      real(r32), intent(in) :: tavg_c
      real(r32), intent(in) :: tavg_f
      real(r32), intent(in) :: jh_coef
      real(r32), intent(in) :: jh_coef_hru
      real(r32), intent(in) :: swrad
    end function
  end interface
end module
