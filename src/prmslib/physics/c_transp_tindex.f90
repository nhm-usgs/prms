!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
MODULE PRMS_TRANSP_TINDEX
  use variableKind
  use prms_constants, only: YEAR, MONTH, DAY
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_SET_TIME, only: Time_t
  use PRMS_TRANSPIRATION, only: Transpiration
  use PRMS_TEMPERATURE, only: Temperature
  implicit none

  private
  public :: transp_tindex

  character(len=*), PARAMETER :: MODDESC = 'Transpiration Distribution'
  character(len=*), PARAMETER :: MODNAME = 'transp_tindex'
  character(len=*), PARAMETER :: MODVERSION = '2018-08-30 13:54:00Z'

  type, extends(Transpiration) :: Transp_tindex
    ! Parameters
    integer(i32), allocatable, private :: transp_beg(:)
      !! Month to begin summing maximum air temperature for each HRU; when sum is greater than or equal to transp_tmax, transpiration begins
    integer(i32), allocatable, private :: transp_end(:)
      !! Month to stop transpiration computations; transpiration is computed thru end of previous month
    real(r32), allocatable, private :: transp_tmax(:)
      !! Temperature index to determine the specific date of the start of the transpiration period;’ the maximum air temperature for each HRU is summed starting with the first day of month transp_beg; when the sum exceeds this index, transpiration begins


    ! Local Variables
    ! integer(i32), allocatable :: transp_check(:)
    logical, allocatable, private :: transp_check(:)

    integer(i32), allocatable, private :: transp_beg_restart(:)
    integer(i32), allocatable, private :: transp_end_restart(:)

    real(r32), allocatable, private :: tmax_sum(:)
    real(r32), allocatable, private :: transp_tmax_c(:)
    real(r32), allocatable, private :: transp_tmax_restart(:)

    contains
      procedure, public :: init => init_Transp_tindex
      procedure, public :: cleanup => cleanup_Transp_tindex
      procedure, public :: run => run_Transp_tindex

  end type

  interface
    !! Transp_tindex constructor
    module subroutine init_Transp_tindex(this, ctl_data, model_basin, model_temp)
      class(Transp_tindex), intent(inout) :: this
        !! Transp_tindex class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Model basin information
      class(Temperature), intent(in) :: model_temp
    end subroutine
  end interface

  interface
    module subroutine cleanup_Transp_tindex(this, ctl_data)
      class(Transp_tindex), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    module subroutine run_Transp_tindex(this, ctl_data, model_time, model_basin, model_temp)
      class(Transp_tindex), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
    end subroutine
  end interface

end MODULE
