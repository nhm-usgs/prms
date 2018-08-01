!***********************************************************************
!     Output a set of declared basin variables as CSV file
!***********************************************************************
module PRMS_BASIN_SUMMARY
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                            MEAN_MONTHLY, MEAN_YEARLY, YEARLY, YEAR, MONTH, DAY
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_POTET, only: Potential_ET
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_TEMPERATURE, only: Temperature
  use SOLAR_RADIATION, only: SolarRadiation
  implicit none

  private
  public :: Basin_summary

  character(len=*), parameter :: MODDESC = 'Output Summary by Basin'
  character(len=*), parameter :: MODNAME = 'basin_summary'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:49:00Z'

  type Basin_summary

    ! Module Variables
    logical :: begin_results
    integer(i32) :: begyr
    integer(i32) :: lastyear
    integer(i32) :: dailyunit
    integer(i32) :: monthlyunit
    integer(i32) :: yearlyunit
    integer(i32) :: basin_var_type

    character(len=48) :: output_fmt
    character(len=48) :: output_fmt2
    character(len=48) :: output_fmt3

    integer(i32) :: daily_flag = 0
    integer(i32) :: monthly_flag = 0
    integer(i32) :: yeardays = 0

    real(r64) :: monthdays = 0.0
    real(r64), allocatable :: basin_var_daily(:)
    real(r64), allocatable :: basin_var_monthly(:)
    real(r64), allocatable :: basin_var_yearly(:)

    contains
      procedure, public :: run => run_Basin_summary
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type

  interface Basin_summary
    !! Basin_summary constructor
    module function constructor_Basin_summary(ctl_data, param_data) result(this)
      type(Basin_summary) :: this
        !! Basin_summary class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine run_Basin_summary(this, ctl_data, model_time, model_solrad, model_precip, model_potet, model_temp)
      class(Basin_summary), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      ! type(Climateflow), intent(in) :: climate
      class(SolarRadiation), intent(in) :: model_solrad
      class(Precipitation), intent(in) :: model_precip
      class(Potential_ET), intent(in) :: model_potet
      class(Temperature), intent(in) :: model_temp

    end subroutine
  end interface

  interface
    module function module_name() result(res)
      character(:), allocatable :: res
    end function
  end interface

  interface
    module function version() result(res)
      character(:), allocatable :: res
    end function
  end interface
end module
