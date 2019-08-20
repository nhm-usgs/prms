module SOLAR_RADIATION_DEGDAY
  use variableKind
  use SOLAR_RADIATION, only: SolarRadiation
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  ! use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_OBS, only: Obs
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_SET_TIME, only: Time_t
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Solrad_degday

  character(len=*), parameter :: MODDESC = 'Solar Radiation Distribution'
  character(len=*), parameter :: MODNAME = 'solrad_degday'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:20:00Z'

  real(r32), dimension(26), parameter :: SOLF = [.20, .35, .45, .51, .56, .59, &
                                                 .62, .64, .655, .67, .682, .69, &
                                                 .70, .71, .715, .72, .722, .724, &
                                                 .726, .728, .73, .734, .738, &
                                                 .742, .746, .75]

  type, extends(SolarRadiation) :: Solrad_degday
    ! Parameters
    real(r32), allocatable, private :: tmax_index(:, :)
      !! Monthly (January to December) index temperature used to determine precipitation adjustments to solar radiation for each HRU
    real(r32), allocatable, private :: dday_intcp(:, :)
      !! Monthly (January to December) intercept in degree-day equation for each HRU
    real(r32), allocatable, private :: dday_slope(:, :)
      !! Monthly (January to December) slope in degree-day equation for each HRU
    real(r32), allocatable, private :: radadj_intcp(:, :)
      !! Monthly (January to December) intercept in air temperature range adjustment to degree-day equation for each HRU
    real(r32), allocatable, private :: radadj_slope(:, :)
      !! Monthly (January to December) slope in air temperature range adjustment to degree-day equation for each HRU

    contains
      procedure, public :: init => init_Solrad_degday
      procedure, public :: run => run_Solrad_degday
  end type

  interface
    !! Solrad_degday constructor
    module subroutine init_Solrad_degday(this, ctl_data, model_basin, model_summary)
      class(Solrad_degday), intent(inout) :: this
        !! Solrad_degday class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Solrad_degday(this, ctl_data, model_time, model_precip, model_basin, model_temp)
      class(Solrad_degday), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      ! type(Obs), intent(in) :: model_obs
      class(Precipitation), intent(in) :: model_precip
      ! type(Climateflow), intent(in) :: climate
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
    end subroutine
  end interface
end module
