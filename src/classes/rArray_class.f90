module rArray_class
    use Abc_class, only: Abc
    use variableKind
    implicit none

    type, extends(Abc) :: rArray
        !! 1D array of reals
        real(r32), allocatable :: values(:)
        integer(i32), allocatable :: dims(:) ! dimension size(s) from source data

    contains
        private
        procedure, pass(this), public :: print => print_1D
    end type rArray

    interface rArray
        module procedure constructor
    end interface rArray

contains
    function constructor(values)
        type(rArray) :: constructor

        real(r32), intent(in) :: values(:)

    end function constructor

    subroutine print_1D(this)
        class(rArray) :: this

        print *, this%values
    end subroutine print_1D
end module rArray_class
