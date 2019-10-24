!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
module PRMS_SUMMARY
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
public :: Summary

character(len=*), parameter :: MODDESC = 'Output Summary'
character(len=*), parameter :: MODNAME = 'summary'
character(len=*), parameter :: MODVERSION = '2019-02-27 17:06:00Z'


type :: var_ptrs
  real(r64), pointer, dimension(:) :: ptr_r64 => null()
  real(r32), pointer, dimension(:) :: ptr_r32 => null()
  integer(i32), pointer, dimension(:) :: ptr_i32 => null()
  logical, pointer, dimension(:) :: ptr_logical => null()
  real(r64), pointer :: scalar_r64 => null()
end type

type :: var_arrays
  real(r64), allocatable :: arr_r64(:)
  real(r32), allocatable :: arr_r32(:)
  integer(i32), allocatable :: arr_i32(:)
end type

type, extends(ModelBase) :: Summary
  ! Module Variables
  logical :: begin_results
    !! Used to trigger processing in the run_Summary routine
  integer(i32) :: begyr

  integer(i32), private :: file_hdl
    !! File handle to open netcdf output file
  integer(i32), allocatable, private :: outvar_id(:)
    !! netcdf variable ids for each output variable
  integer(i32), allocatable, private :: outvar_size(:)
    !! Size of each output variable array
  integer(i32), private :: time_varid
    !! NetCDF variable id for time variable

  type(var_ptrs), allocatable :: var_daily(:)
    !! Daily values for output variables - single timestep

  contains
    procedure, private :: set_summary_var_r32
    procedure, private :: set_summary_var_r64
    procedure, private :: set_summary_var_i32
    procedure, private :: set_summary_var_r64_0d
    procedure, private :: set_summary_var_logical_1d
    procedure, public :: cleanup => cleanup_Summary
    procedure, public :: run => run_Summary
    generic, public :: set_summary_var => set_summary_var_r32, set_summary_var_r64, &
                                       set_summary_var_i32, &
                                       set_summary_var_r64_0d, &
                                       set_summary_var_logical_1d

    procedure, private :: create_netcdf
    procedure, nopass, private :: err_check

    procedure, nopass, private :: chunk_shape_2d
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

interface Summary
  !! Summary constructor
  module function constructor_Summary(ctl_data, model_basin, model_time) result(this)
    type(Summary) :: this
      !! Summary class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
  end function
end interface

interface
  module subroutine run_Summary(this, ctl_data, model_time, model_basin)
    class(Summary), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
  end subroutine
end interface

interface
  module function chunk_shape_2d(dims, val_size, chunk_size) result(res)
    integer(i32) :: res(2)
    integer(i32), intent(in) :: dims(2)
    integer(i32), intent(in) :: val_size
    integer(i32), intent(in) :: chunk_size
  end function
end interface

interface
  module subroutine create_netcdf(this, ctl_data, model_basin, model_time, filename)
    class(Summary), intent(inout) :: this
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
    class(Summary), intent(inout) :: this
  end subroutine
end interface

interface set_summary_var
  module subroutine set_summary_var_r32(this, idx, var)
    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r32), target, intent(in) :: var(:)
  end subroutine

  module subroutine set_summary_var_r64(this, idx, var)
    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var(:)
  end subroutine

  module subroutine set_summary_var_i32(this, idx, var)
    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    integer(i32), target, intent(in) :: var(:)
  end subroutine

  module subroutine set_summary_var_r64_0d(this, idx, var)
    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var
  end subroutine

  module subroutine set_summary_var_logical_1d(this, idx, var)
    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    logical, target, intent(in) :: var(:)
  end subroutine
end interface

interface write_netcdf
  module subroutine write_netcdf_i32_0d(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    integer(i32), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_i32_1d(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    integer(i32), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r32_0d(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r32), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r32_1d(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r32), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r64_0d(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r64), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_r64_1d(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r64), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_var_ptr(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    type(var_ptrs), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine

  module subroutine write_netcdf_var_arr(this, ncid, varid, data, start, ocount)
    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    type(var_arrays), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)
  end subroutine
end interface

interface
  module subroutine cleanup_Summary(this)
    class(Summary), intent(in) :: this
  end subroutine
end interface

end module
