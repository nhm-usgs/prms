module PRMS_PRECIPITATION_HRU
  use variableKind
  use prms_constants, only: dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_BASIN_SUMMARY_PTR, only: basin_summary_ptr
  use PRMS_NHRU_SUMMARY_PTR, only: Nhru_summary_ptr
  implicit none

  private
  public :: Precipitation_hru

  character(len=*), parameter :: MODDESC = 'Precip distribution by HRU'
  character(len=*), parameter :: MODNAME = 'precipitation_hru'
  character(len=*), parameter :: MODVERSION = '2018-10-10 15:55:00Z'

  type, extends(Precipitation) :: Precipitation_hru
    integer(i32), private :: precip_funit
      !! Precipitation CBH file unit
    integer(i32), private :: precip_varid
    integer(i32), private :: precip_idx_offset

    logical, private :: has_netcdf_precip

    contains
      procedure, public :: run => run_Precipitation_hru
  end type

  interface Precipitation_hru
    !! Precipitation_hru constructor
    module function constructor_Precipitation_hru(ctl_data, param_data, basin_summary, nhru_summary) result(this)
      type(Precipitation_hru) :: this
        !! Precipitation_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
      type(Basin_summary_ptr), intent(inout) :: basin_summary
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
        !! Summary by HRU module
    end function
  end interface

  interface
    module subroutine run_Precipitation_hru(this, ctl_data, param_data, model_basin, model_temp, model_time, nhru_summary)
      class(Precipitation_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Time_t), intent(in), optional :: model_time
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
        !! Summary by HRU module
    end subroutine
  end interface


end module
