!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
MODULE PRMS_NHRU_SUMMARY
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                            MEAN_MONTHLY, MEAN_YEARLY, YEARLY, YEAR, MONTH, DAY
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_INTCP, only: Interception
  use PRMS_POTET, only: Potential_ET
  use PRMS_SNOW, only: Snowcomp
  use PRMS_STREAMFLOW, only: Streamflow
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_TRANSPIRATION, only: Transpiration
  use SOLAR_RADIATION, only: SolarRadiation
  implicit none

  private
  public :: Nhru_summary

  character(len=*), parameter :: MODDESC = 'Output Summary by HRU'
  character(len=*), parameter :: MODNAME = 'nhru_summary'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:49:00Z'

  type Nhru_summary
    ! Module Variables
    logical :: begin_results
      !! Used to trigger processing in the run_Nhru_summary routine
    integer(i32) :: begyr
    integer(i32) :: lastyear
    integer(i32), allocatable :: dailyunit(:)
    real(r32), allocatable :: nhru_var_daily(:, :)
    real(r64), allocatable :: nhru_var_dble(:, :)

    character(len=48) :: output_fmt
    character(len=48) :: output_fmt2
    character(len=48) :: output_fmt3

    integer(i32) :: daily_flag
    integer(i32) :: double_vars = 0
    integer(i32) :: yeardays
    integer(i32) :: monthly_flag

    real(r64) :: monthdays
    integer(i32), allocatable :: monthlyunit(:)
    integer(i32), allocatable :: yearlyunit(:)
    real(r64), allocatable :: nhru_var_monthly(:, :)
    real(r64), allocatable :: nhru_var_yearly(:, :)

    contains
      procedure, public :: run => run_Nhru_summary
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type

  interface Nhru_summary
    !! Nhru_summary constructor
    module function constructor_Nhru_summary(ctl_data, param_data) result(this)
      type(Nhru_summary) :: this
        !! Nhru_summary class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine run_nhru_summary(this, ctl_data, model_time, model_basin, &
                                       climate, model_intcp, model_potet, model_snow, &
                                       model_solrad, model_streamflow, model_temp, &
                                       model_transp)
      class(Nhru_summary), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: climate
      class(Interception), intent(in) :: model_intcp
      class(Potential_ET), intent(in) :: model_potet
      type(Snowcomp), intent(in) :: model_snow
      class(SolarRadiation), intent(in) :: model_solrad
      class(Streamflow), intent(in) :: model_streamflow
      class(Temperature), intent(in) :: model_temp
      class(Transpiration), intent(in) :: model_transp
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
