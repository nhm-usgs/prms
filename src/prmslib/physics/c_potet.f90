module PRMS_POTET
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_BASIN_SUMMARY_PTR, only: basin_summary_ptr

  implicit none

  private
  public :: Potential_ET

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
  character(len=*), parameter :: MODNAME = 'potet'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:37:00Z'

  ! Potential Evapotranspiration class
  type, extends(ModelBase) :: Potential_ET
    integer(i32), private :: humidity_funit
      !! Humidity CBH file unit

    real(r64), pointer :: basin_humidity
      !! (moved from climateflow.f90)
    real(r64), pointer :: basin_potet

    real(r32), allocatable :: humidity_hru(:)
      !! (moved from climate_hru)
    real(r32), allocatable :: potet(:)

    ! For potet_pt, potet_pm, potet_pm_sta
    ! real(r32), allocatable :: tempc_dewpt(:)
    ! real(r32), allocatable :: vp_actual(:)
    ! real(r32), allocatable :: lwrad_net(:)
    ! real(r32), allocatable :: vp_slope(:)
    ! real(r32), allocatable :: vp_sat(:)

    contains
      procedure, public :: run_Potet
  end type

  interface Potential_ET
    !! Potential_ET constructor
    module function constructor_Potet(ctl_data, basin_summary) result(this)
      type(Potential_ET) :: this
        !! Potential_ET class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin_summary_ptr), intent(inout) :: basin_summary
        !! Basin summary
    end function
  end interface

  interface
    module subroutine run_Potet(this, ctl_data, param_data, model_basin)
      class(Potential_ET), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface
end module
