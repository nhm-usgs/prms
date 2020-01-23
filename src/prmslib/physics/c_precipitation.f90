module PRMS_PRECIPITATION
  use variableKind
  use ModelBase_class, only: ModelBase
  use prms_constants, only: dp
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Precipitation

  character(len=*), parameter :: MODDESC = 'Precipitation distribution'
  character(len=*), parameter :: MODNAME = 'precipitation'
  character(len=*), parameter :: MODVERSION = '2018-10-10 15:55:00Z'

  type, extends(ModelBase) :: Precipitation
    ! Parameters for precipitation
    integer(i32) :: precip_units
    real(r32), pointer :: tmax_allsnow(:, :)
    real(r32), pointer :: tmax_allrain_offset(:, :)

    ! Output variables
    real(r64), allocatable :: basin_obs_ppt
      !! Basin area-weighted measured average precipitation, in inches
    real(r64), allocatable :: basin_ppt
      !! Basin area-weighted average precipitation, in inches
    real(r64), allocatable :: basin_rain
      !! Basin area-weighted average rainfall, in inches
    real(r64), allocatable :: basin_snow
      !! Basin area-weighted average snowfall, in inches

    real(r32), allocatable :: hru_ppt(:)
      !! Precipitation distributed to each HRU, in inches
    real(r32), allocatable :: hru_rain(:)
      !! Rain distributed to each HRU, in inches
    real(r32), allocatable :: hru_snow(:)
      !! Snow distributed to each HRU, in inches
    real(r32), allocatable :: prmx(:)
      !! Fraction of rain in a mixed precipitation event for each HRU

    ! integer(i32), allocatable :: newsnow(:)
    !   !! Flag to indicate if new snow fell on each HRU (0=no; 1=yes)
    ! integer(i32), allocatable :: pptmix(:)
    !   !! Flag to indicate if precipitation is a mixture of rain and snow for each HRU (0=no; 1=yes)

    ! Local variables
    real(r32), allocatable, private :: tmax_allrain(:, :)
    real(r32), allocatable, private :: tmax_allrain_c(:, :)
    real(r32), allocatable :: tmax_allrain_f(:, :)
    real(r32), allocatable :: tmax_allsnow_c(:, :)
    real(r32), allocatable, private :: tmax_allsnow_f(:, :)

    ! Other variables
    logical :: has_hru_summary_vars

    contains
      procedure, public :: init => init_Precipitation
      procedure, public :: run => run_Precipitation
      procedure, public :: set_precipitation_form
      procedure, public :: set_summary_ptrs
  end type

  interface
    !! Precipitation constructor
    module subroutine init_Precipitation(this, ctl_data, model_basin, model_temp, model_summary)
      class(Precipitation), intent(inout) :: this
        !! Precipitation class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Summary), intent(inout) :: model_summary
        !! Summary by HRU module
    end subroutine
  end interface

  ! interface Precipitation
  !   !! Precipitation constructor
  !   module function constructor_Precipitation(ctl_data, model_basin, model_temp, model_summary) result(this)
  !     type(Precipitation) :: this
  !       !! Precipitation class
  !     type(Control), intent(in) :: ctl_data
  !       !! Control file parameters
  !     type(Basin), intent(in) :: model_basin
  !     class(Temperature), intent(in) :: model_temp
  !     type(Summary), intent(inout) :: model_summary
  !       !! Summary by HRU module
  !   end function
  ! end interface

  interface
    module subroutine run_Precipitation(this, ctl_data, model_basin, model_temp, model_time, model_summary)
      class(Precipitation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Time_t), intent(in), optional :: model_time
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine set_precipitation_form(this, ctl_data, model_basin, model_temp, &
                                             month, rain_adj, snow_adj, rainmix_adj)
      class(Precipitation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      integer(i32), intent(in) :: month
      real(r32), optional, intent(in) :: rain_adj(:, :)
        !! Array of rain adjustments
      real(r32), optional, intent(in) :: snow_adj(:, :)
        !! Array of snow adjustments
      real(r32), optional, intent(in) :: rainmix_adj(:, :)
        !! Array of rain mixture adjustments
    end subroutine
  end interface

  interface
    module subroutine set_summary_ptrs(this, ctl_data, model_summary)
      class(Precipitation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface
end module
