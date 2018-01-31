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
    use variableKind
    use Abc_class, only: Abc
    use m_strings, only: str
    implicit none

    private

    public :: rArray

    type, extends(Abc) :: rArray
        private
        !! 1D array of reals that can represent multiple dimensional data
        real(r32), allocatable :: values(:)
          !! The values of the array
        integer(i32), allocatable :: dims(:) 
          !! Size(s) that values should represent. e.g. If values represents 2D data, dims might be [10, 15]

    contains
        procedure, pass(this), public :: print => print_rArray
    end type

    interface rArray
        module procedure :: constructor_rArray
    end interface

contains
    !====================================================================!
    function constructor_rArray(values) result(this)
        type(rArray) :: this

        real(r32), intent(in) :: values(:)

    end function
    !====================================================================!
    !====================================================================!
    subroutine print_rArray(this)
        class(rArray) :: this

        write(output_unit, '(a)') str(this%values)
    end subroutine
    !====================================================================!
end module
