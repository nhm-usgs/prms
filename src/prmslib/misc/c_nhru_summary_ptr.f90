!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
module PRMS_NHRU_SUMMARY_PTR
use variableKind
use iso_fortran_env, only: output_unit
use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                          MEAN_MONTHLY, MEAN_YEARLY, YEARLY, YEAR, MONTH, DAY
use ModelBase_class, only: ModelBase
use Control_class, only: Control
use PRMS_SET_TIME, only: Time_t
use PRMS_BASIN, only: Basin
implicit none

private
public :: Nhru_summary_ptr

character(len=*), parameter :: MODDESC = 'Output Summary by HRU'
character(len=*), parameter :: MODNAME = 'nhru_summary_ptr'
character(len=*), parameter :: MODVERSION = '2019-02-11 17:06:00Z'


type :: var_ptrs
  real(r64), pointer, dimension(:) :: ptr_r64 => null()
  real(r32), pointer, dimension(:) :: ptr_r32 => null()
  integer(i32), pointer, dimension(:) :: ptr_i32 => null()
  real(r64), pointer :: scalar_r64 => null()
end type

type :: var_arrays
  real(r64), allocatable :: arr_r64(:)
  real(r32), allocatable :: arr_r32(:)
  integer(i32), allocatable :: arr_i32(:)
end type

type, extends(ModelBase) :: Nhru_summary_ptr
  ! Module Variables
  logical :: begin_results
    !! Used to trigger processing in the run_Summary routine
  logical, private :: is_daily_freq
  logical, private :: is_monthly_freq
  logical, private :: is_yearly_freq
  integer(i32) :: begyr
  integer(i32) :: daily_flag
  integer(i32) :: prioryear
    !! Year from prior timestep of model
  integer(i32) :: monthly_flag
  integer(i32), private :: time_index_monthly
    !! Time index for writing monthly values to netcdf files
  integer(i32), private :: time_index_yearly
    !! Time index for writing yearly values to netcdf files

  integer(i32) :: yeardays
    !! Number of days in the current year. Year starts on start_month and start_day.

  real(r64) :: monthdays

  integer(i32), private :: nhru_file_hdl
  integer(i32), allocatable, private :: nhru_outvar_id(:)
  integer(i32), allocatable, private :: nhru_outvar_size(:)
    !! Size of each output variable array
  integer(i32), private :: time_varid

  type(var_ptrs), allocatable :: nhru_var_daily(:)
  ! type(var_ptrs), allocatable :: nsegment_var_daily(:)

  type(var_arrays), allocatable :: nhru_var_summary(:)
  ! real(r64), allocatable :: nsegment_var_summary(:, :)
  ! real(r64), allocatable :: nhru_var_monthly(:, :)
  ! real(r64), allocatable :: nhru_var_yearly(:, :)

  contains
    procedure, private :: set_nhru_var_r32
    procedure, private :: set_nhru_var_r64
    procedure, private :: set_nhru_var_i32
    procedure, private :: set_var_r64_0D
    procedure, public :: cleanup => cleanup_Nhru_summary_ptr
    procedure, public :: run => run_Nhru_summary_ptr
    generic, public :: set_nhru_var => set_nhru_var_r32, set_nhru_var_r64, &
                                       set_nhru_var_i32, &
                                       set_var_r64_0D

    procedure, private :: create_netcdf
    procedure, nopass, private :: err_check
    procedure, private :: reset_summary_vars

    generic, private :: write_netcdf => write_netcdf_i32_0d, write_netcdf_i32_1d, &
                                        write_netcdf_r32_0d, write_netcdf_r32_1d, &
                                        write_netcdf_r64_0d, write_netcdf_r64_1d, &
                                        write_netcdf_var_arr, write_netcdf_var_ptr
    procedure, private :: write_netcdf_i32_0d
    procedure, private :: write_netcdf_i32_1d
    procedure, private :: write_netcdf_r32_0d
    procedure, private :: write_netcdf_r32_1d
    procedure, private :: write_netcdf_r64_0d
    procedure, private :: write_netcdf_r64_1d
    procedure, private :: write_netcdf_var_arr
    procedure, private :: write_netcdf_var_ptr

end type

interface Nhru_summary_ptr
  !! Nhru_summary_ptr constructor
  module function constructor_Nhru_summary_ptr(ctl_data, model_basin, model_time) result(this)
    type(Nhru_summary_ptr) :: this
      !! Nhru_summary_ptr class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
  end function
end interface

interface
  module subroutine run_Nhru_summary_ptr(this, ctl_data, model_time, model_basin)
    class(Nhru_summary_ptr), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
  end subroutine
end interface

interface
  module subroutine create_netcdf(this, ctl_data, model_basin, model_time, filename)
    class(Nhru_summary_ptr), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    character(len=*), intent(in) :: filename
  end subroutine
end interface

interface
  module subroutine err_check(status)
    integer(i32), intent(in) :: status
      !! The status returned by a netcdf call
  end subroutine
end interface

interface
  module subroutine reset_summary_vars(this)
    class(Nhru_summary_ptr), intent(inout) :: this
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

  module subroutine set_nhru_var_i32(this, idx, var)
    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    integer(i32), target, intent(in) :: var(:)
  end subroutine

  module subroutine set_var_r64_0D(this, idx, var)
    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var
  end subroutine
end interface

interface write_netcdf
  module subroutine write_netcdf_i32_0d(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    integer(i32), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_i32_1d(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    integer(i32), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r32_0d(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r32), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r32_1d(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r32), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r64_0d(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r64), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r64_1d(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r64), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_var_ptr(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    type(var_ptrs), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_var_arr(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    type(var_arrays), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine
end interface

interface
  module subroutine cleanup_Nhru_summary_ptr(this)
    class(Nhru_summary_ptr), intent(in) :: this
  end subroutine
end interface

end module
