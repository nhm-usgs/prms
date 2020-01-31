module PRMS_PRECIPITATION_HRU
  use variableKind
  use prms_constants, only: dp
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Precipitation_hru

  character(len=*), parameter :: MODDESC = 'Precip distribution by HRU'
  character(len=*), parameter :: MODNAME = 'precipitation_hru'
  character(len=*), parameter :: MODVERSION = '2018-10-10 15:55:00Z'

  type, extends(Precipitation) :: Precipitation_hru
    ! Parameters for precipitation by HRU
    real(r32), pointer :: rain_cbh_adj(:, :) !rmcd changed to add access to bmi setter functions
    real(r32), pointer :: snow_cbh_adj(:, :) !rmcd changed to add access to bmi setter functions
    real(r32), pointer :: adjmix_rain(:, :) !rmcd changed to add access to bmi setter functions

    ! Other variables
    integer(i32), private :: precip_funit
      !! Precipitation CBH file unit
    integer(i32), private :: precip_varid
    integer(i32), private :: precip_idx_offset

    logical, private :: has_netcdf_precip

    contains
      procedure, public :: init => init_Precipitation_hru
      procedure, public :: run => run_Precipitation_hru
  end type

  interface
    !! Precipitation_hru constructor
    module subroutine init_Precipitation_hru(this, ctl_data, model_basin, model_temp, model_summary)
      class(Precipitation_hru), intent(inout) :: this
        !! Precipitation_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Summary), intent(inout) :: model_summary
        !! Summary by HRU module
    end subroutine
  end interface

  interface
    module subroutine run_Precipitation_hru(this, ctl_data, model_basin, model_temp, model_time, model_summary)
      class(Precipitation_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Time_t), intent(in), optional :: model_time
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface


end module
