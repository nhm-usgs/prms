module PRMS_CASCADE
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_GWFLOW, only: Gwflow
  ! use PRMS_CLIMATEVARS, only: Climateflow
  ! use PRMS_INTCP, only: Interception
  ! use PRMS_POTET, only: Potential_ET
  ! use PRMS_PRECIPITATION, only: Precipitation
  ! use PRMS_SET_TIME, only: Time_t
  ! use PRMS_SNOW, only: Snowcomp
  ! use PRMS_SRUNOFF, only: Srunoff
  ! use PRMS_TRANSPIRATION, only: Transpiration
  use PRMS_SUMMARY, only: Summary

  implicit none

  private
  public :: Cascade

  character(len=*), parameter :: MODDESC = 'Cascading Flow'
  character(len=*), parameter :: MODNAME = 'cascade'
  character(len=*), parameter :: MODVERSION = '2021-05-03 13:08:00Z'

  type, extends(ModelBase) :: Cascade
    ! Dimensions
    integer(i32) :: ncascade
    integer(i32) :: ncascdgw
    integer(i32) :: ndown

    ! Parameters
    integer(i32) :: cascade_flg
      !! Flag to indicate cascade type (0=allow many to many; 1=force one to one)
    integer(i32) :: circle_switch
      !! Switch to check for circles (0=no check; 1=check)

    integer(i32), pointer :: gw_down_id(:)
      !! Index number of the downslope GWR to which the upslope GWR contributes flow
    integer(i32), pointer :: gw_strmseg_down_id(:)
      !! Index number of the stream segment that cascade area contributes flow
    integer(i32), pointer :: gw_up_id(:)
      !! Index of GWR containing cascade area
    integer(i32), pointer :: hru_down_id(:)
      !! Index number of the downslope HRU to which the upslope HRU contributes flow
    integer(i32), pointer :: hru_segment(:)
      !! Segment index to which an HRU contributes lateral flows (surface runoff, interflow, and groundwater discharge)
    integer(i32), pointer :: hru_strmseg_down_id(:)
      !! Index number of the stream segment that cascade area contributes flow
    integer(i32), pointer :: hru_up_id(:)
      !! Index of HRU containing cascade area

    real(r32) :: cascade_tol
      !! Cascade area below which a cascade link is ignored

    real(r32), pointer :: gw_pct_up(:)
      !! Fraction of GWR area used to compute flow contributed to a downslope GWR or stream segment for cascade area
    real(r32), pointer:: hru_pct_up(:)
      !! Fraction of HRU area used to compute flow contributed to a downslope HRU or stream segment for cascade area

    ! Computed Variables
    integer(i32), allocatable :: gwr_down(:, :)
      !! Indices of the downslope GWRs to which the cascade area routes flow
    integer(i32), allocatable :: hru_down(:, :)
      !! Indices of the downslope HRUs or stream segments to which the cascade area routes flow
    integer(i32), allocatable :: ncascade_gwr(:)
    integer(i32), allocatable :: ncascade_hru(:)

    real(r32), allocatable :: cascade_area(:, :)
      !! Cascade area within an HRU
    real(r32), allocatable :: cascade_gwr_area(:, :)
      !! Cascade area within an GWR
    real(r32), allocatable :: gwr_down_frac(:, :)
      !! Fraction of GWR area used to compute flow routed to a downslope cascade area or stream segment from each cascade area of an GWR
    real(r32), allocatable :: hru_down_frac(:, :)
      !! Fraction of HRU area used to compute flow routed to a downslope HRU or stream segment
    real(r32), allocatable :: hru_down_fracwt(:, :)
      !! HRU area fraction, area weighted by downslope HRU area, used to compute flow routed to a downslope HRU or stream segment


    ! real, save, allocatable :: Gwr_down_fracwt(:, :)
    ! gwr_down_fracwt: GWR area fraction, area weighted by downslope GWR
    !                  area, used to compute flow routed to a downslope
    !                  GWR or stream segment.

    ! Local Variables
    integer(i32) :: igworder
    integer(i32) :: iorder
    integer(i32) :: MSGUNT
    integer(i32) :: ndown

    contains
      procedure, public :: init => init_Cascade
      procedure, public :: cleanup => cleanup_Cascade

      procedure, private :: check_path
      procedure, private :: init_cascade_second
      procedure, private :: initgw_cascade
      procedure, private :: order_gwrs
      procedure, private :: order_hrus
      procedure, private :: up_tree

  end type

  interface
    !! Cascade constructor
    module subroutine init_Cascade(this, ctl_data, model_basin, model_gw, model_summary)
      class(Cascade), intent(inout) :: this
        !! Cascade class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Gwflow), intent(in) :: model_gw
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine cleanup_Cascade(this, ctl_data)
      class(Cascade) :: this
        !! Cascade class
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    subroutine init_cascade_second(this, ctl_data, model_basin, iret)
      class(Cascade), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      integer(i32), intent(out) :: iret
    end subroutine
  end interface


  ! =========================================================
  ! =========================================================
  ! Remaining functions/subroutines to convert:
  !   check_path()
  !   initgw_cascade()
  !   order_hrus()
  !   order_gwrs()
  !   up_tree()
  ! =========================================================
  ! =========================================================

end module