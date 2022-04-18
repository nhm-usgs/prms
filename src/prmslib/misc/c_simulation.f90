module Simulation_class
  use variableKind
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_GWFLOW, only: Gwflow
  use PRMS_HUMIDITY, only: Humidity
  use PRMS_HUMIDITY_HRU, only: Humidity_hru
  use PRMS_HUMIDITY_PER, only: Humidity_per
  use PRMS_HUMIDITY_STA, only: Humidity_sta
  use PRMS_INTCP, only: Interception
  use PRMS_MUSKINGUM, only: Muskingum
  use PRMS_MUSKINGUM_MANN, only: Muskingum_mann
  use PRMS_OBS, only: Obs
  use PRMS_POTET, only: Potential_ET
  use PRMS_POTET_JH, only: Potet_jh
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_PRECIPITATION_HRU, only: Precipitation_hru
  ! use PRMS_RESTART, only: Restart
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SNOW, only: Snowcomp
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_STREAMFLOW, only: Streamflow
  use PRMS_STRMFLOW_IN_OUT, only: Strmflow_in_out
  use PRMS_SUMMARY, only: Summary
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_TEMPERATURE_HRU, only: Temperature_hru
  use PRMS_TRANSPIRATION, only: Transpiration
  use PRMS_TRANSP_TINDEX, only: Transp_tindex
  use PRMS_WATER_BALANCE, only: WaterBalance
  use SOLAR_RADIATION, only: SolarRadiation
  use SOLAR_RADIATION_DEGDAY, only: Solrad_degday
  use PRMS_WIND, only: Wind
  use PRMS_WIND_HRU, only: Wind_hru
  use PRMS_WIND_STA, only: Wind_sta
  implicit none

  private
  public :: Simulation

  type :: Simulation
      type(Basin) :: model_basin
      type(Time_t) :: model_time
      class(Climateflow), allocatable :: climate
      type(Obs) :: model_obs

      class(Humidity), allocatable :: model_humidity
      class(Wind), allocatable :: model_wind
      class(Precipitation), allocatable :: model_precip
      class(Temperature), allocatable :: model_temp

      class(SolarRadiation), allocatable :: solrad
      class(Transpiration), allocatable :: transpiration
      class(Potential_ET), allocatable :: potet
      class(Interception), allocatable :: intcp

      ! class(Restart), allocatable :: model_restart
      class(Snowcomp), allocatable :: snow
      class(Srunoff), allocatable :: runoff
      class(Soilzone), allocatable :: soil
      class(Gwflow), allocatable :: groundwater
      class(Streamflow), allocatable :: model_streamflow
      type(Summary) :: model_summary
      type(WaterBalance) :: model_waterbal
    contains
      procedure, public :: init => init_Simulation
      procedure, public :: run => run_Simulation
      procedure, public :: cleanup => cleanup_Simulation
  end type

  interface
    !! Simulation constructor
    module subroutine init_Simulation(this, ctl_data)
      class(Simulation), intent(inout) :: this
        !! Simulation class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end subroutine
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
