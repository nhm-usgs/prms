module SOLAR_RADIATION_HRU
  use variableKind
  use SOLAR_RADIATION, only: SolarRadiation
  ! use prms_constants, only: dp
  ! use prms_constants, only: FAHRENHEIT, CELSIUS, INCHES, MM, FEET, METERS, &
  !                           FEET2METERS, METERS2FEET
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Solrad_hru

  character(len=*), parameter :: MODDESC = 'Solar radiation by HRU'
  character(len=*), parameter :: MODNAME = 'solrad_hru'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:20:00Z'

  type, extends(SolarRadiation) :: Solrad_hru
    integer(i32), private :: swrad_funit
      !! Solar radiation CBH file unit
  end type

  interface Solrad_hru
    !! Solrad_hru constructor
    module function constructor_Solrad_hru(ctl_data, param_data, model_basin, model_summary) result(this)
      type(Solrad_hru) :: this
        !! Solrad_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end function
  end interface

  interface
    module subroutine run_Solrad_hru(this, ctl_data, param_data, model_time, model_basin)
      class(Solrad_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface
end module
