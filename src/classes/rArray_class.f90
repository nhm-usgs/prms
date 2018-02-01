module rArray_class
    !!
    !!
    !!
    !!
    !!
    !!
    !!
    !!
    !!
    !!
    !!
    !!

    use iso_fortran_env, only: output_unit
    use variableKind, only: r32, i32
    use Abc_class, only: Abc
    use m_allocate, only: allocate
    use m_strings, only: str
    
    implicit none

    private

    public :: rArray

    type, extends(Abc) :: rArray
        !! 1D array of reals that can represent multiple dimensional data
        real(r32), allocatable :: values(:)
          !! The values of the array
        integer(i32), allocatable :: dims(:)
          !! Size(s) that values should represent. e.g. If values represents 2D data, dims might be [10, 15]

    contains
        generic, public :: allocate => allocate_rArray_i1_, allocate_rArray_i1D_
          !! Allocate the memory inside the class
        procedure, private, pass(this) :: allocate_rArray_i1_ => allocate_rArray_i1
        procedure, private, pass(this) :: allocate_rArray_i1D_ => allocate_rArray_i1D

        procedure, public, pass(this) :: deallocate => deallocate_rArray
          !! Deallocate the memory inside the class
        procedure, public, pass(this) :: print => print_rArray
          !! Print the class to the screen

    end type

    interface rArray
      !! Overloaded interface to instantiate the class.
        module procedure :: constructor_rArray_i1, constructor_rArray_i1D
    end interface

contains
    !====================================================================!
    function constructor_rArray_i1(N) result(this)
        type(rArray) :: this
          !! rArray class
        integer(i32), intent(in) :: N
          !! Size in 1D of the class
          
        call this%allocate(N)
    end function
    !====================================================================!
    !====================================================================!
    function constructor_rArray_i1D(dims) result(this)
        type(rArray) :: this
          !! rArray class
        integer(i32), intent(in) :: dims(:)
          !! The shape of the rArray

        call this%allocate(dims)
    end function
    !====================================================================!

    !====================================================================!
    subroutine allocate_rArray_i1(this, N)
        class(rArray), intent(inout) :: this
          !! rArray class
        integer(i32), intent(in) :: N
          !! Allocate the values to size N, and set the dims array to 1D of length one.
    
        call allocate(this%values, N)
        call allocate(this%dims, 1)
        this%dims(1) = N
    end subroutine
    !====================================================================!
    !====================================================================!
    subroutine allocate_rArray_i1D(this, dims)
        class(rArray), intent(inout) :: this
          !! rArray class
        integer(i32), intent(in) :: dims(:)
          !! Allocate the values to the same shape defined by dims.
        call allocate(this%values, product(dims))
        call allocate(this%dims, size(dims))
        this%dims = dims
    end subroutine
    !====================================================================!


    !====================================================================!
    subroutine deallocate_rArray(this)
        class(rArray), intent(inout) :: this
          !! rArray class
        call deallocate(this%values)
        call deallocate(this%dims)
    end subroutine
    !====================================================================!


    !====================================================================!
    subroutine print_rArray(this)
        class(rArray) :: this

        write(output_unit, '(a)') str(this%values)
    end subroutine
    !====================================================================!
end module
