!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
module PRMS_BASIN
  use variableKind
  use prms_constants, only: dp, sp
  use iso_fortran_env, only: output_unit
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  implicit none

  private
  public :: Basin

  ! intrinsic :: EPSILON

  character(len=*), parameter :: MODDESC = 'Basin Definition'
  character(len=*), parameter :: MODNAME = 'basin'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:50:00Z'

  type :: Basin
    real(r64) :: active_area
    real(r64) :: basin_area_inv
    real(r64) :: basin_lat
    real(r64) :: land_area
    real(r64) :: total_area
    real(r64) :: water_area

    integer(i32) :: active_gwrs
    integer(i32) :: active_hrus
    integer(i32) :: dprst_clos_flag
    integer(i32) :: dprst_open_flag
    integer(i32) :: hemisphere
    integer(i32) :: numlake_hrus
    integer(i32) :: numlakes_check
    integer(i32) :: puls_lin_flag
    integer(i32) :: weir_gate_flag

    logical, allocatable :: active_mask(:)
      !! Logical mask of HRUs that have hru_type /= INACTIVE

    integer(i32), allocatable :: gwr_route_order(:)
    integer(i32), allocatable :: gwr_type(:)
    integer(i32), allocatable :: hru_route_order(:)

    real(r32), allocatable :: dprst_area_clos_max(:)
    real(r32), allocatable :: dprst_area_max(:)
    real(r32), allocatable :: dprst_area_open_max(:)
    real(r32), allocatable :: dprst_frac_clos(:)
    real(r32), allocatable :: hru_elev_feet(:)
    real(r32), allocatable :: hru_elev_meters(:)
    real(r32), allocatable :: hru_frac_perv(:)
    real(r32), allocatable :: hru_imperv(:)
    real(r32), allocatable :: hru_perv(:)

    real(r64), allocatable :: hru_area_dble(:)
    real(r64), allocatable :: lake_area(:)

    contains
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type

  interface Basin
    !! Basin constructor
    module function constructor_Basin(ctl_data, param_data) result(this)
      type(Basin) :: this
        !! Basin class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
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
