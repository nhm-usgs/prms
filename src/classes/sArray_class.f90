module sArray_class
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
    use String_class, only: String
    use m_strings, only: str
    implicit none

    private

    public :: sArray

    type, extends(Abc) :: sArray
        private
        !! 1D array of reals that can represent multiple dimensional data
        type(String), allocatable :: values(:)
          !! The values of the array

    contains
        procedure, public, pass(this) :: print => print_sArray

    end type

    interface sArray
        module procedure :: constructor_sArray
    end interface

contains
    !====================================================================!
    function constructor_sArray(values) result(this)
        type(sArray) :: this

        type(String), intent(in) :: values(:)
    end function
    !====================================================================!
    !====================================================================!
    subroutine print_sArray(this)
        class(sArray) :: this

        write(output_unit, *) this%values
        ! write(output_unit, '(a)') str(this%values)
    end subroutine
    !====================================================================!
end module
