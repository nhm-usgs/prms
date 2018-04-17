!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
module PRMS_BASIN
  use variableKind
  use iso_fortran_env, only: output_unit
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  implicit none

  private
  public :: Basin

  intrinsic :: EPSILON

  character(len=*), parameter :: MODDESC = 'Basin Definition'
  character(len=*), parameter :: MODNAME = 'basin'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:50:00Z'

  type :: Basin
    real(r64) :: basin_area_inv
    real(r64) :: basin_lat = 0.0
    real(r64) :: active_area = 0.0

    integer(i32) :: active_hrus
    integer(i32) :: hemisphere
    logical, allocatable :: active_mask(:)
      !! Logical mask of HRUs that have hru_type /= INACTIVE
    integer(i32), allocatable :: hru_route_order(:)

    real(r32), allocatable :: hru_frac_perv(:)
    real(r32), allocatable :: hru_imperv(:)
    real(r32), allocatable :: hru_perv(:)

    real(r64) :: total_area = 0.0
    real(r64) :: land_area = 0.0

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
