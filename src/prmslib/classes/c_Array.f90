module Array_class
  !!
  !!
  use iso_fortran_env, only: output_unit
  use variableKind, only: i32
  use Abc_class, only: Abc
  use m_allocate, only: allocate
  use m_deallocate, only: deallocate
  use m_strings, only: str
  implicit none

  private

  public :: Array

  type, extends(Abc) :: Array
    integer(i32), allocatable :: dims(:)
      !! Size(s) that values should represent (e.g. if it represents 2D data then dims might be [10, 15])

    contains
      generic, public :: allocate => allocate_Array_i1_, allocate_Array_i1D_
        !! Allocate the memory inside the class
      procedure, private, pass(this) :: allocate_Array_i1_ => allocate_Array_i1
      procedure, private, pass(this) :: allocate_Array_i1D_ => allocate_Array_i1D

      procedure, public, pass(this) :: deallocate => deallocate_Array
        !! Deallocate the memory inside the class
      procedure, public, pass(this) :: print => print_Array
        !! Print the class to the screen
      procedure, public, pass(this) :: read => read_Array
        !! Read the class from file
      procedure, public, pass(this) :: size => size_Array
  end type

  interface Array
    !! Overloaded interface to instantiate the class
    module procedure :: constructor_Array_i1, constructor_Array_i1D
  end interface

  contains
    !====================================================================!
    function constructor_Array_i1(N) result(this)
      type(Array) :: this
        !! Array class
      integer(i32), intent(in) :: N
        !! Size in 1D of the class

      call this%allocate(N)
    end function

    !====================================================================!
    function constructor_Array_i1D(dims) result(this)
      type(Array) :: this
        !! Array class
      integer(i32), intent(in) :: dims(:)
        !! The shape of the Array

      call this%allocate(dims)
    end function

    !====================================================================!
    subroutine allocate_Array_i1(this, N)
      class(Array), intent(inout) :: this
        !! Array class
      integer(i32), intent(in) :: N
        !! Allocate the dims array to 1D of length one.

      call allocate(this%dims, 1)
      this%dims(1) = N
    end subroutine

    !====================================================================!
    subroutine allocate_Array_i1D(this, dims)
      class(Array), intent(inout) :: this
        !! Array class
      integer(i32), intent(in) :: dims(:)
        !! Allocate the dims array to size of the given dims.

      call allocate(this%dims, size(dims))
      this%dims = dims
    end subroutine

    !====================================================================!
    subroutine deallocate_Array(this)
      class(Array), intent(inout) :: this
        !! Array class

      call deallocate(this%dims)
    end subroutine

    !====================================================================!
    subroutine print_Array(this, delim)
      class(Array), intent(in) :: this
        !! Array class
      character(len=*), intent(in), optional :: delim
        !! Delimiter between values

      write(output_unit, '(a)') str(this%dims, delim)
    end subroutine

    !====================================================================!
    function size_Array(this) result(res)
        !! Get the size of the list of strings
      class(Array), intent(in) :: this
        !! Array Class
      integer(i32) :: res
        !! Size of the list

      res = size(this%dims)
    end function
end module
