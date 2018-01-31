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
    use variableKind
    use Abc_class, only: Abc
    use m_strings, only: str
    implicit none

    private

    public :: iArray

    type, extends(Abc) :: iArray
        private
        !! 1D array of reals that can represent multiple dimensional data
        integer(i32), allocatable :: values(:)
          !! The values of the array
        integer(i32), allocatable :: dims(:)
          !! Size(s) that values should represent. e.g. If values represents 2D data, dims might be [10, 15]

    contains
        procedure, public, pass(this) :: print => print_iArray

    end type

    interface iArray
        module procedure :: constructor_iArray
    end interface

contains
    !====================================================================!
    function constructor_iArray(values) result(this)
        type(iArray) :: this

        integer(i32), intent(in) :: values(:)

    end function
    !====================================================================!
    !====================================================================!
    subroutine print_iArray(this)
        class(iArray) :: this

        write(output_unit, '(a)') str(this%values)
    end subroutine
    !====================================================================!
end module
