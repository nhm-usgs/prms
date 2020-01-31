!***********************************************************************
! Computes the potential evapotranspiration using the Penman-Monteith formulation
! according to Murray (1967), shown equation 13 in Irmak and others (2012)
! Irmak, Suat, Kabenge, Isa, Skaggs, K.E., and Mutiibwa, Denis, 2012
!   Trend and magnitude of changes in climate variables and reference
!   evapotranspiration over 116-yr period in the Platte River Basin,
!   central Nebraska-USA: Journal of Hydrology, V. 420-421, p. 228-244
!***********************************************************************

module PRMS_POTET_PM
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_POTET, only: Potential_ET
  use PRMS_TEMPERATURE, only: Temperature
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Potet_pm

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
  character(len=*), parameter :: MODNAME = 'potet_pm'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:37:00Z'

  type, extends(Potential_ET) :: Potet_pm
    integer(i32), private :: windspeed_funit
      !! Windspeed CBH file unit

    real(r32), pointer :: tempc_dewpt(:)
    real(r32), pointer :: vp_actual(:)
    real(r32), pointer :: vp_sat(:)
    real(r32), pointer :: vp_slope(:)
    real(r32), pointer :: lwrad_net(:)
    real(r32), pointer :: windspeed_hru(:)

    contains
      procedure, public :: init => init_Potet_pm
      procedure, public :: run => run_Potet_pm
  end type

  interface
    !! Potet_pm constructor
    module subroutine init_Potet_pm(this, ctl_data, model_basin, model_summary)
      class(Potet_pm), intent(inout) :: this
        !! Poteh_pm class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Potet_pm(this, ctl_data, model_basin, model_temp, model_time, model_solrad)
      class(Potet_pm), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad
    end subroutine
  end interface

end module
