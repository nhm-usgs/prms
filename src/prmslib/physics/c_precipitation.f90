module PRMS_PRECIPITATION
  use variableKind
  use ModelBase_class, only: ModelBase
  use prms_constants, only: dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_BASIN_SUMMARY_PTR, only: basin_summary_ptr
  use PRMS_NHRU_SUMMARY_PTR, only: Nhru_summary_ptr
  implicit none

  private
  public :: Precipitation

  character(len=*), parameter :: MODDESC = 'Precipitation distribution'
  character(len=*), parameter :: MODNAME = 'precipitation'
  character(len=*), parameter :: MODVERSION = '2018-10-10 15:55:00Z'

  type, extends(ModelBase) :: Precipitation
    logical :: has_hru_summary_vars

    ! Basin variables
    real(r64), pointer :: basin_obs_ppt
    real(r64), pointer :: basin_ppt
    real(r64), pointer :: basin_rain
    real(r64), pointer :: basin_snow

    real(r32), allocatable :: hru_ppt(:)
    real(r32), allocatable :: hru_rain(:)
    real(r32), allocatable :: hru_snow(:)
    real(r32), allocatable :: prmx(:)

    real(r32), allocatable :: tmax_allrain(:, :)
    real(r32), allocatable :: tmax_allrain_c(:, :)
    real(r32), allocatable :: tmax_allrain_f(:, :)
    real(r32), allocatable :: tmax_allsnow_c(:, :)
    real(r32), allocatable :: tmax_allsnow_f(:, :)

    integer(i32), allocatable :: newsnow(:)
    integer(i32), allocatable :: pptmix(:)

    contains
      procedure, public :: run => run_Precipitation
      procedure, public :: set_precipitation_form
      procedure, public :: set_nhru_summary_ptrs
  end type

  interface Precipitation
    !! Precipitation constructor
    module function constructor_Precipitation(ctl_data, param_data, basin_summary, nhru_summary) result(this)
      type(Precipitation) :: this
        !! Precipitation class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
      type(Basin_summary_ptr), intent(inout) :: basin_summary
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
        !! Summary by HRU module
    end function
  end interface

  interface
    module subroutine run_Precipitation(this, ctl_data, param_data, model_basin, model_temp, model_time, nhru_summary)
      class(Precipitation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Time_t), intent(in), optional :: model_time
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
    end subroutine
  end interface

  interface
    module subroutine set_precipitation_form(this, ctl_data, param_data, model_basin, model_temp, &
                                             month, rain_adj, snow_adj, rainmix_adj)
      class(Precipitation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      integer(i32), intent(in) :: month
      real(r32), optional, intent(in) :: rain_adj(:)
        !! Array of rain adjustments
      real(r32), optional, intent(in) :: snow_adj(:)
        !! Array of snow adjustments
      real(r32), optional, intent(in) :: rainmix_adj(:)
        !! Array of rain mixture adjustments
    end subroutine
  end interface

  interface
    module subroutine set_nhru_summary_ptrs(this, ctl_data, nhru_summary)
      class(Precipitation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
    end subroutine
  end interface
end module
