module PRMS_TEMPERATURE_HRU
  use variableKind
  use prms_constants, only: dp
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Temperature_hru

  character(len=*), parameter :: MODDESC = 'Temp distribution by HRU'
  character(len=*), parameter :: MODNAME = 'temp_hru'
  character(len=*), parameter :: MODVERSION = '2018-10-10 15:45:00Z'

  type, extends(Temperature) :: Temperature_hru
    ! Parameters
    real(r32), pointer :: tmax_cbh_adj(:, :) !rmcd changed to add access to bmi setter functions
    real(r32), pointer :: tmin_cbh_adj(:, :) !rmcd changed to add access to bmi setter functions

    ! Local variables
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
      procedure, public :: init => init_Temperature_hru
      procedure, public :: run => run_Temperature_hru
  end type

  interface
    !! Temperature_hru constructor
    module subroutine init_Temperature_hru(this, ctl_data, model_basin, model_summary)
      class(Temperature_hru), intent(inout) :: this
        !! Temperature_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Temperature_hru(this, ctl_data, model_basin, model_time, model_summary)
      class(Temperature_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in), optional :: model_time
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface
end module
