module SOLAR_RADIATION_DEGDAY
  use variableKind
  use SOLAR_RADIATION, only: SolarRadiation
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  ! use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_OBS, only: Obs
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_SET_TIME, only: Time_t
  use PRMS_TEMPERATURE, only: Temperature
  implicit none

  private
  public :: Solrad_degday

  character(len=*), parameter :: MODDESC = 'Solar Radiation Distribution'
  character(len=*), parameter :: MODNAME = 'solrad_degday'
  character(len=*), parameter :: MODVERSION = '2018-07-02 14:04:00Z'

  real(r32), dimension(26), parameter :: SOLF = [.20, .35, .45, .51, .56, .59, &
                                                 .62, .64, .655, .67, .682, .69, &
                                                 .70, .71, .715, .72, .722, .724, &
                                                 .726, .728, .73, .734, .738, &
                                                 .742, .746, .75]

  type, extends(SolarRadiation) :: Solrad_degday

    contains
      procedure, public :: run => run_Solrad_degday
  end type

  interface Solrad_degday
    !! Solrad_degday constructor
    module function constructor_Solrad_degday(ctl_data, param_data, model_basin) result(this)
      type(Solrad_degday) :: this
        !! Solrad_degday class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
    end function
  end interface

  interface
    module subroutine run_Solrad_degday(this, ctl_data, param_data, model_time, model_obs, model_precip, model_basin, model_temp)
      class(Solrad_degday), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      type(Obs), intent(in) :: model_obs
      class(Precipitation), intent(in) :: model_precip
      ! type(Climateflow), intent(in) :: climate
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
    end subroutine
  end interface
end module
