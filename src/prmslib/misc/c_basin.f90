!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
module PRMS_BASIN
  use variableKind
  use prms_constants, only: dp, sp
  use iso_fortran_env, only: output_unit
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  implicit none

  private
  public :: Basin

  character(len=*), parameter :: MODDESC = 'Basin Definition'
  character(len=*), parameter :: MODNAME = 'basin'
  character(len=*), parameter :: MODVERSION = '2018-08-30 13:22:00Z'

  type, extends(ModelBase) :: Basin
    ! Dimensions
    integer(i32) :: nconsumed = 0
      !! TODO: does this belong here?
    integer(i32) :: nhru
      !! Number of HRUs in model
    integer(i32) :: nlake = 0
    integer(i32) :: nmonths
    integer(i32) :: nobs = 0
    integer(i32) :: nsegment
      !! Number of stream segments in model
    integer(i32) :: nwateruse = 0
      !! TODO: does this belong here?

    ! Parameters
    integer(i32), allocatable :: cov_type(:)
      !! Vegetation cover type for each HRU (0=bare soil; 1=grasses; 2=shrubs; 3=trees; 4=coniferous)
    real(r32), allocatable :: dprst_frac(:)
      !! Fraction of each HRU area that has surface depressions
    real(r32), allocatable :: hru_area(:)
    real(r32), allocatable :: hru_aspect(:)
    real(r32), allocatable :: hru_elev(:)
    real(r32), allocatable :: hru_lat(:)
    real(r32), allocatable :: hru_lon(:)
    real(r32), allocatable :: hru_percent_imperv(:)
    real(r32), allocatable :: hru_slope(:)
    integer(i32), allocatable :: hru_type(:)
    real(r32), allocatable :: hru_x(:)
    real(r32), allocatable :: hru_y(:)
    integer(i32), allocatable :: lake_hru_id(:)
    integer(i32), allocatable :: lake_type(:)
    integer(i32), allocatable :: nhm_id(:)
      !! NHM identification number for each HRU


    ! Local and computed variables
    real(r64) :: active_area
    real(r64) :: basin_area_inv
    real(r64) :: basin_lat
    real(r64) :: land_area
    real(r64) :: total_area
    real(r64) :: water_area

    integer(i32) :: active_gwrs
    integer(i32) :: active_hrus
    integer(i32) :: hemisphere

    ! TODO: 2018-09-11 PAN Only used by soilzone; could move there?
    integer(i32) :: numlake_hrus

    integer(i32), private :: numlakes_check
    integer(i32) :: puls_lin_flag
    integer(i32) :: weir_gate_flag

    logical, allocatable :: active_mask(:)
      !! Logical mask of HRUs that have hru_type /= INACTIVE

    integer(i32), allocatable :: gwr_route_order(:)
    integer(i32), allocatable :: gwr_type(:)
    integer(i32), allocatable :: hru_route_order(:)

    real(r32), allocatable :: dprst_area_max(:)
    real(r32), allocatable :: hru_frac_perv(:)
    real(r32), allocatable :: hru_area_imperv(:)
    real(r32), allocatable :: hru_area_perv(:)

    real(r64), allocatable :: hru_area_dble(:)

    ! TODO: 2018-09-11 PAN Possibly move lake_area to muskingum_lake class
    real(r64), allocatable :: lake_area(:)


  end type

  interface Basin
    !! Basin constructor
    module function constructor_Basin(ctl_data) result(this)
      type(Basin) :: this
        !! Basin class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

end module
