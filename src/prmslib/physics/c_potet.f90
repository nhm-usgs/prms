module PRMS_POTET
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_SET_TIME, only: Time_t
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Potential_ET

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
  character(len=*), parameter :: MODNAME = 'potet'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:37:00Z'

  ! Potential Evapotranspiration class
  type, extends(ModelBase) :: Potential_ET
    ! Parameters

    ! NOTE: epan_coef is always used by intcp even though it is only required by the potet_pan module
    real(r32), pointer :: epan_coef(:, :)
      !! Monthly (January to December) evaporation pan coefficient for each HRU
    real(r32), pointer :: potet_sublim(:)
      !! Fraction of potential ET that is sublimated from snow in the canopy and snowpack for each HRU


    ! Other variables
    integer(i32), private :: humidity_funit
      !! Humidity CBH file unit

    ! Output variables
    real(r32), pointer :: humidity_hru(:)
      !! (moved from climate_hru)
    real(r32), pointer :: potet(:)

    ! For potet_pt, potet_pm, potet_pm_sta
    ! real(r32), allocatable :: tempc_dewpt(:)
    ! real(r32), allocatable :: vp_actual(:)
    ! real(r32), allocatable :: lwrad_net(:)
    ! real(r32), allocatable :: vp_slope(:)
    ! real(r32), allocatable :: vp_sat(:)

    contains
      procedure, public :: init => init_Potet
      procedure, public :: run => run_Potet
  end type

  interface
    !! Potential_ET constructor
    module subroutine init_Potet(this, ctl_data, model_basin, model_summary)
      class(Potential_ET), intent(inout) :: this
        !! Potential_ET class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Potet(this, ctl_data, model_basin, model_time, model_solrad, model_temp)
      class(Potential_ET), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad
      class(Temperature), intent(in) :: model_temp
    end subroutine
  end interface
end module
