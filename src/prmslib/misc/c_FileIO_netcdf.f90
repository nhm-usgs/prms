module PRMS_FILE_IO_NETCDF
  use variableKind
  use iso_fortran_env, only: output_unit
  use PRMS_FILE_IO, only: FileIO
  implicit none

  private
  public :: FileIO_netcdf

  type, extends(FileIO) :: FileIO_netcdf
    ! character(len=:), allocatable :: override_dir
    ! type(list) :: override_list


    contains
      procedure, public, pass(this) :: open => open_netcdf
      procedure, public, pass(this) :: close => close_netcdf
      procedure, public, pass(this) :: dim_exists
      generic, public :: get_variable => get_variable_i32_scalar, &
                                         get_variable_i32_1d, get_variable_i32_2d, &
                                         get_variable_i64_2d, &
                                         get_variable_r32_scalar, &
                                         get_variable_r32_1d, get_variable_r32_2d, &
                                         get_variable_r64_1d, get_variable_r64_2d
      procedure, public, pass(this) :: get_dimension
      procedure, public, pass(this) :: var_exists

      procedure, private, nopass :: err_check
      procedure, private :: get_variable_i32_scalar
      procedure, private :: get_variable_i32_1d
      procedure, private :: get_variable_i32_2d
      procedure, private :: get_variable_i64_2d
      procedure, private :: get_variable_r32_scalar
      procedure, private :: get_variable_r32_1d
      procedure, private :: get_variable_r32_2d
      procedure, private :: get_variable_r64_1d
      procedure, private :: get_variable_r64_2d
  end type

  interface FileIO_netcdf
    module function constructor_FileIO_netcdf(filename) result(this)
      type(FileIO_netcdf) :: this
      character(len=*), intent(in) :: filename
    end function
  end interface

  interface get_variable
    module subroutine get_variable_i32_scalar(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i32), intent(inout) :: var_data
    end subroutine

    module subroutine get_variable_i32_1d(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i32), pointer, intent(inout) :: var_data(:)
    end subroutine

    module subroutine get_variable_i32_2d(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i32), pointer, intent(inout) :: var_data(:, :)
    end subroutine

    module subroutine get_variable_i64_2d(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i64), pointer, intent(inout) :: var_data(:, :)
    end subroutine

    module subroutine get_variable_r32_scalar(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r32), intent(inout) :: var_data
    end subroutine

    module subroutine get_variable_r32_1d(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r32), pointer, intent(inout) :: var_data(:)
    end subroutine

    module subroutine get_variable_r32_2d(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r32), pointer, intent(inout) :: var_data(:, :)
    end subroutine

    module subroutine get_variable_r64_1d(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r64), pointer, intent(inout) :: var_data(:)
    end subroutine

    module subroutine get_variable_r64_2d(this, name, var_data)
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r64), pointer, intent(inout) :: var_data(:, :)
    end subroutine
  end interface


  interface
    module subroutine open_netcdf(this, filename)
      class(FileIO_netcdf), intent(inout) :: this
      character(len=*), intent(in) :: filename
    end subroutine
  end interface


  interface
    module subroutine close_netcdf(this)
      class(FileIO_netcdf), intent(inout) :: this
    end subroutine
  end interface


  interface
    module function dim_exists(this, name) result(res)
      logical :: res
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
        !! Name of dimension
    end function
  end interface


  interface
    module function var_exists(this, name) result(res)
      logical :: res
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
        !! Name of variable
    end function
  end interface


  interface
    module function get_dimension(this, name) result(res)
      integer(i32) :: res
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
        !! Name of the dimension
    end function
  end interface


  interface
    module subroutine err_check(status)
      integer(r32), intent(in) :: status
        !! The status returned by a netcdf call
    end subroutine
  end interface
end module
