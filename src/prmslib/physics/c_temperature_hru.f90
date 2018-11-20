module PRMS_TEMPERATURE_HRU
  use variableKind
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
  public :: Temperature_hru

  character(len=*), parameter :: MODDESC = 'Temp distribution by HRU'
  character(len=*), parameter :: MODNAME = 'temp_hru'
  character(len=*), parameter :: MODVERSION = '2018-10-10 15:45:00Z'

  type, extends(Temperature) :: Temperature_hru
    integer(i32), private :: tmax_funit
      !! Maximum temperature CBH file unit
    integer(i32), private :: tmin_funit
      !! Minimum temperature CBH file unit

    integer(i32), private :: tmax_varid
    integer(i32), private :: tmin_varid
    integer(i32), private :: tmax_idx_offset
    integer(i32), private :: tmin_idx_offset

    logical, private :: has_netcdf_tmax
    logical, private :: has_netcdf_tmin

    contains
      procedure, public :: run => run_Temperature_hru
  end type

  interface Temperature_hru
    !! Temperature_hru constructor
    module function constructor_Temperature_hru(ctl_data, basin_summary, nhru_summary) result(this)
      type(Temperature_hru) :: this
        !! Temperature_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin_summary_ptr), intent(inout) :: basin_summary
        !! Basin summary
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
        !! Summary by HRU module
    end function
  end interface

  interface
    module subroutine run_Temperature_hru(this, ctl_data, param_data, model_basin, model_time, nhru_summary)
      class(Temperature_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
    end subroutine
  end interface
end module
