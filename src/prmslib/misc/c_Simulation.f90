module Simulation_class
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_FLOWVARS, only: Flowvars
  use PRMS_CLIMATE_HRU, only: Climate_HRU
  use PRMS_SOLTAB, only: Soltab
  use PRMS_DDSOLRAD, only: Ddsolrad
  use PRMS_TRANSP_TINDEX, only: Transp_tindex
  use PRMS_POTET_JH, only: Potet_jh
  use PRMS_INTCP, only: Interception
  use PRMS_SNOW, only: Snowcomp
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_GWFLOW, only: Gwflow
  use PRMS_ROUTING, only: Routing
  use PRMS_MUSKINGUM, only: Muskingum
  use PRMS_BASIN_SUMMARY, only: Basin_summary
  use PRMS_NHRU_SUMMARY, only: Nhru_summary
  use PRMS_SET_TIME, only: Time_t
  use PRMS_OBS, only: Obs
  implicit none

  private
  public :: Simulation

  type Simulation
      type(Basin) :: model_basin
      type(Climateflow) :: climate
      type(Flowvars) :: model_flow
      type(Soltab) :: solt
      type(Obs) :: model_obs
      type(Time_t) :: model_time

      type(Climate_HRU) :: climate_by_hru
      type(Ddsolrad) :: solrad
      type(Transp_tindex) :: transpiration
      type(Potet_jh) :: potet
      type(Interception) :: intcp
      type(Snowcomp) :: snow
      type(Srunoff) :: runoff
      type(Soilzone) :: soil
      type(Gwflow) :: groundwater
      type(Routing) :: model_route
      type(Muskingum) :: model_muskingum
      type(Nhru_summary) :: summary_by_hru
      type(Basin_summary) :: summary_by_basin
    contains
      procedure, public :: run => run_Simulation
      procedure, public :: cleanup => cleanup_Simulation
  end type

  interface Simulation
    !! Simulation constructor
    module function constructor_Simulation(ctl_data, param_data) result(this)
      type(Simulation) :: this
        !! Simulation class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine run_Simulation(this, ctl_data, param_data)
      class(Simulation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
    end subroutine
  end interface

  interface
    module subroutine cleanup_Simulation(this, ctl_data)
      implicit none

      class(Simulation), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

end module
