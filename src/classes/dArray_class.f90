module dArray_class
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

    public :: dArray

    type, extends(Abc) :: dArray
        private
        !! 1D array of reals that can represent multiple dimensional data
        real(r64), allocatable :: values(:)
          !! The values of the array
        integer(i32), allocatable :: dims(:)
          !! Size(s) that values should represent. e.g. If values represents 2D data, dims might be [10, 15]

    contains
        procedure, public, pass(this) :: print => print_dArray

    end type

    interface dArray
        module procedure :: constructor_dArray
    end interface

contains
    !====================================================================!
    function constructor_dArray(values) result(this)
        type(dArray) :: this

        real(r64), intent(in) :: values(:)

    end function
    !====================================================================!
    !====================================================================!
    subroutine print_dArray(this)
        class(dArray) :: this

        write(output_unit, '(a)') str(this%values)
    end subroutine
    !====================================================================!
end module
