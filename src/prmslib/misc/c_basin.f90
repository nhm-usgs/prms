!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
module PRMS_BASIN
  use variableKind
  use prms_constants, only: dp, sp
  use iso_fortran_env, only: output_unit
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
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
      !! Number of hydrologic response units
    integer(i32) :: nlake = 0
      !! Number of lakes
    integer(i32) :: nmonths
    integer(i32) :: nobs = 0
    integer(i32) :: nsegment
      !! Number of stream-channel segments
    integer(i32) :: nsub = 0
      !! Number of internal subbasins
    integer(i32) :: nwateruse = 0
      !! TODO: does nwateruse belong here?

    ! Parameters
    integer(i32), pointer :: cov_type(:)
      !! Vegetation cover type for each HRU (0=bare soil; 1=grasses; 2=shrubs; 3=trees; 4=coniferous)
    real(r32), pointer :: hru_area(:)
      !! Area of each HRU [acres]
    real(r32), pointer :: hru_aspect(:)
      !! Aspect of each HRU [angular degrees]
    real(r32), pointer :: hru_elev(:)
      !! Mean elevation for each HRU [feet or meters]
    real(r32), pointer :: hru_lat(:)
      !! Latitude of each HRU [degrees north]
    real(r32), pointer :: hru_lon(:)
      !! Longitude of each HRU [degrees east]
    real(r32), pointer :: hru_slope(:)
      !! Slope of each HRU [decimal fraction]
    integer(i32), pointer :: hru_type(:)
    real(r32), pointer :: hru_x(:)
    real(r32), pointer :: hru_y(:)
    integer(i32), pointer :: lake_hru_id(:)
    integer(i32), pointer :: lake_type(:)
    integer(i32), pointer :: nhm_id(:)
      !! NHM identification number for each HRU
    integer(i32), pointer :: nhm_seg(:)
      !! NHM segment identification


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

    logical, pointer :: active_mask(:)
      !! Logical mask of HRUs that have hru_type /= INACTIVE

    integer(i32), pointer :: gwr_route_order(:)
    integer(i32), pointer :: gwr_type(:)
    integer(i32), pointer :: hru_route_order(:)

    ! real(r32), allocatable :: dprst_area_max(:)
    ! real(r32), allocatable :: hru_frac_perv(:)
    ! real(r32), allocatable :: hru_area_imperv(:)
    ! real(r32), allocatable :: hru_area_perv(:)

    real(r64), pointer :: hru_area_dble(:)

    ! TODO: 2018-09-11 PAN Possibly move lake_area to muskingum_lake class
    real(r64), pointer :: lake_area(:)

    integer(i32) :: covtype_unit
      !! File handle to dynamic cov_type parameter file
    integer(i32) :: dyn_output_unit
    integer(i32) :: next_dyn_covtype_date(3)
    integer(i32), pointer :: covtype_chgs(:)
    contains
      procedure, public :: run => run_Basin
      procedure, public :: cleanup => cleanup_Basin

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

  interface
    module subroutine run_Basin(this, ctl_data, model_time)
      class(Basin), intent(inout) :: this
        !! Basin class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Basin(this, ctl_data)
      class(Basin), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module
