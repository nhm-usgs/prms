!***********************************************************************
!     Output a set of declared basin variables as CSV file
!***********************************************************************
module PRMS_BASIN_SUMMARY_PTR
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                            MEAN_MONTHLY, MEAN_YEARLY, YEARLY, YEAR, MONTH, DAY
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only: Time_t
  implicit none

  private
  public :: Basin_summary_ptr

  character(len=*), parameter :: MODDESC = 'Output Summary by Basin'
  character(len=*), parameter :: MODNAME = 'basin_summary_ptr'
  character(len=*), parameter :: MODVERSION = '2018-08-30 15:12:00Z'

  type :: var_ptrs
    real(r64), pointer :: ptr => null()
  end type

  type, extends(ModelBase) :: Basin_summary_ptr

    ! Module Variables
    logical :: begin_results
    integer(i32) :: begyr
    integer(i32) :: lastyear
    integer(i32) :: dailyunit
    integer(i32) :: monthlyunit
    integer(i32) :: yearlyunit
    integer(i32) :: basin_var_type

    character(len=48) :: output_fmt
    character(len=48) :: output_fmt2
    character(len=48) :: output_fmt3

    integer(i32) :: daily_flag = 0
    integer(i32) :: monthly_flag = 0
    integer(i32) :: yeardays = 0

    real(r64) :: monthdays = 0.0
    type(var_ptrs), allocatable :: basin_var_daily(:)
    ! real(r64), allocatable :: basin_var_daily(:)
    real(r64), allocatable :: basin_var_monthly(:)
    real(r64), allocatable :: basin_var_yearly(:)

    contains
      procedure, public :: run => run_Basin_summary_ptr
      procedure, public :: set_basin_var

  end type

  interface Basin_summary_ptr
    !! Basin_summary constructor
    module function constructor_Basin_summary_ptr(ctl_data, param_data) result(this)
      type(Basin_summary_ptr) :: this
        !! Basin_summary class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine run_Basin_summary_ptr(this, ctl_data, model_time)
      class(Basin_summary_ptr), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  interface
    module subroutine set_basin_var(this, idx, var)
      class(Basin_summary_ptr), intent(inout) :: this
      integer(i32), intent(in) :: idx
      real(r64), target, intent(in) :: var
    end subroutine
  end interface
end module
