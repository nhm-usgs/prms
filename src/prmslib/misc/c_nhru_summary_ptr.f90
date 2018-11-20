!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
MODULE PRMS_NHRU_SUMMARY_PTR
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                            MEAN_MONTHLY, MEAN_YEARLY, YEARLY, YEAR, MONTH, DAY
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  implicit none

  private
  public :: Nhru_summary_ptr

  character(len=*), parameter :: MODDESC = 'Output Summary by HRU'
  character(len=*), parameter :: MODNAME = 'nhru_summary_ptr'
  character(len=*), parameter :: MODVERSION = '2018-11-07 15:14:00Z'


  type :: var_ptrs
    real(r64), pointer, dimension(:) :: ptr_r64 => null()
    real(r32), pointer, dimension(:) :: ptr_r32 => null()
  end type

  type, extends(ModelBase) :: Nhru_summary_ptr
    ! Module Variables
    logical :: begin_results
      !! Used to trigger processing in the run_Nhru_summary routine
    integer(i32) :: begyr
    integer(i32) :: daily_flag
    integer(i32) :: lastyear
    integer(i32) :: monthly_flag
    integer(i32) :: yeardays

    real(r64) :: monthdays

    integer(i32), allocatable :: dailyunit(:)
    integer(i32), allocatable :: monthlyunit(:)
    integer(i32), allocatable :: yearlyunit(:)

    character(len=48) :: output_fmt
    character(len=48) :: output_fmt2
    character(len=48) :: output_fmt3

    type(var_ptrs), allocatable :: nhru_var_daily(:)

    real(r64), allocatable :: nhru_var_monthly(:, :)
    real(r64), allocatable :: nhru_var_yearly(:, :)

    contains
      procedure, private:: set_nhru_var_r32
      procedure, private:: set_nhru_var_r64
      procedure, public :: run => run_nhru_summary_ptr
      generic, public :: set_nhru_var => set_nhru_var_r32, set_nhru_var_r64

  end type

  interface Nhru_summary_ptr
    !! Nhru_summary constructor
    module function constructor_Nhru_summary_ptr(ctl_data, param_data) result(this)
      type(Nhru_summary_ptr) :: this
        !! Nhru_summary_ptr class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine run_nhru_summary_ptr(this, ctl_data, model_time, model_basin)
      class(Nhru_summary_ptr), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface set_nhru_var
    module subroutine set_nhru_var_r32(this, idx, var)
      class(Nhru_summary_ptr), intent(inout) :: this
      integer(i32), intent(in) :: idx
      real(r32), target, intent(in) :: var(:)
    end subroutine

    module subroutine set_nhru_var_r64(this, idx, var)
      class(Nhru_summary_ptr), intent(inout) :: this
      integer(i32), intent(in) :: idx
      real(r64), target, intent(in) :: var(:)
    end subroutine
  end interface

end module
