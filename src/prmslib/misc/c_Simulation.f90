module Simulation_class
  use variableKind
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_GWFLOW, only: Gwflow
  use PRMS_INTCP, only: Interception
  use PRMS_MUSKINGUM, only: Muskingum
  use PRMS_OBS, only: Obs
  use PRMS_POTET_JH, only: Potet_jh
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_PRECIPITATION_HRU, only: Precipitation_hru
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SNOW, only: Snowcomp
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_SUMMARY, only: Summary
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_TEMPERATURE_HRU, only: Temperature_hru
  use PRMS_TRANSP_TINDEX, only: Transp_tindex
  use PRMS_WATER_BALANCE, only: WaterBalance
  use SOLAR_RADIATION_DEGDAY, only: Solrad_degday
  implicit none

  private
  public :: Simulation

  type :: Simulation
      type(Basin) :: model_basin
      type(Time_t) :: model_time
      type(Climateflow) :: climate
      type(Obs) :: model_obs

      class(Precipitation), allocatable :: model_precip
      class(Temperature), allocatable :: model_temp

      type(Solrad_degday) :: solrad
      type(Transp_tindex) :: transpiration
      type(Potet_jh) :: potet
      type(Interception) :: intcp
      type(Snowcomp) :: snow
      type(Srunoff) :: runoff
      type(Soilzone) :: soil
      type(Gwflow) :: groundwater
      ! ! type(Routing) :: model_route
      type(Muskingum) :: model_muskingum
      type(Summary) :: model_summary
      type(WaterBalance) :: model_waterbal
    contains
      procedure, public :: run => run_Simulation
      procedure, public :: cleanup => cleanup_Simulation
  end type

  interface Simulation
    !! Simulation constructor
    module function constructor_Simulation(ctl_data) result(this)
      type(Simulation) :: this
        !! Simulation class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine run_Simulation(this, ctl_data)
      class(Simulation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    module subroutine cleanup_Simulation(this, ctl_data)
      class(Simulation), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

end module
