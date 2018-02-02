module iArray_class
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
    use variableKind, only: i32
    use m_errors, only: fErr
    use Abc_class, only: Abc
    use m_allocate, only: allocate
    use m_deallocate, only: deallocate
    use m_strings, only: str
    
    implicit none

    private

    public :: iArray

    type, extends(Abc) :: iArray
        !! 1D array of reals that can represent multiple dimensional data
        integer(i32), allocatable :: values(:)
          !! The values of the array
        integer(i32), allocatable :: dims(:)
          !! Size(s) that values should represent. e.g. If values represents 2D data, dims might be [10, 15]

    contains
        generic, public :: allocate => allocate_iArray_i1_, allocate_iArray_i1D_
          !! Allocate the memory inside the class
        procedure, private, pass(this) :: allocate_iArray_i1_ => allocate_iArray_i1
        procedure, private, pass(this) :: allocate_iArray_i1D_ => allocate_iArray_i1D

        procedure, public, pass(this) :: deallocate => deallocate_iArray
          !! Deallocate the memory inside the class
        procedure, public, pass(this) :: print => print_iArray
          !! Print the class to the screen
        procedure, public, pass(this) :: read => read_iArray
          !! Read the class from file
        procedure, public, pass(this) :: size => size_iArray

    end type

    interface iArray
      !! Overloaded interface to instantiate the class.
        module procedure :: constructor_iArray_i1, constructor_iArray_i1D
    end interface

contains
    !====================================================================!
    function constructor_iArray_i1(N) result(this)
        type(iArray) :: this
          !! iArray class
        integer(i32), intent(in) :: N
          !! Size in 1D of the class
          
        call this%allocate(N)
    end function
    !====================================================================!
    !====================================================================!
    function constructor_iArray_i1D(dims) result(this)
        type(iArray) :: this
          !! iArray class
        integer(i32), intent(in) :: dims(:)
          !! The shape of the iArray

        call this%allocate(dims)
    end function
    !====================================================================!

    !====================================================================!
    subroutine allocate_iArray_i1(this, N)
        class(iArray), intent(inout) :: this
          !! iArray class
        integer(i32), intent(in) :: N
          !! Allocate the values to size N, and set the dims array to 1D of length one.
    
        call allocate(this%values, N)
        call allocate(this%dims, 1)
        this%dims(1) = N
    end subroutine
    !====================================================================!
    !====================================================================!
    subroutine allocate_iArray_i1D(this, dims)
        class(iArray), intent(inout) :: this
          !! iArray class
        integer(i32), intent(in) :: dims(:)
          !! Allocate the values to the same shape defined by dims.
        call allocate(this%values, product(dims))
        call allocate(this%dims, size(dims))
        this%dims = dims
    end subroutine
    !====================================================================!


    !====================================================================!
    subroutine deallocate_iArray(this)
        class(iArray), intent(inout) :: this
          !! iArray class
        call deallocate(this%values)
        call deallocate(this%dims)
    end subroutine
    !====================================================================!


    !====================================================================!
    subroutine print_iArray(this, delim)
        class(iArray), intent(in) :: this
          !! iArray class
        character(len=*), intent(in), optional :: delim
          !! Delimiter between values

        write(output_unit, '(a)') str(this%values, delim)
    end subroutine
    !====================================================================!

    !====================================================================!
    subroutine read_iArray(this, iUnit, fName)
        class(iArray), intent(inout) :: this
          !! iArray Class
        integer(i32), intent(in) :: iUnit
          !! Unit number to read from
        character(len=*), intent(in) :: fName
          !! Name of the file that was opened

        integer(i32) :: i, istat
        integer(i32) :: N

        read(iUnit, *) N
        read(iUnit, *)

        call this%allocate(N)

        do i = 1, N
            read(iUnit, *, iostat=istat) this%values(i)
            call fErr(istat, fName, 2)
        enddo
    end subroutine
    !====================================================================!

    !====================================================================!
    function size_iArray(this) result(res)
      !! Get the size of the list of strings
        class(iArray), intent(in) :: this
          !! sArray Class
        integer(i32) :: res
          !! Size of the list
    
        res = size(this%values)
    end function
    !====================================================================!
end module
