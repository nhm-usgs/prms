module PRMS_POTET_HRU
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  ! use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_POTET, only: Potential_ET
  use SOLAR_RADIATION, only: SolarRadiation
  implicit none

  private
  public :: Potet_hru

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration by HRU'
  character(len=*), parameter :: MODNAME = 'potet_hru'
  character(len=*), parameter :: MODVERSION = '2018-08-30 14:01:00Z'

  type, extends(Potential_ET) :: Potet_hru
    integer(i32), private :: et_funit
      !! Evapotranspiration CBH file unit
  end type

  interface Potet_hru
    !! Potet_hru constructor
    module function constructor_Potet_hru(ctl_data) result(this)
      type(Potet_hru) :: this
        !! Potet_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine run_Potet_hru(this, ctl_data, param_data, model_time, model_basin)
      class(Potet_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

end module
