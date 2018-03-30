module sVariable_class
    !!# Variable Class (array of strings)
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
    use variableKind
    use String_class, only: String
    use sArray_class, only: sArray


    implicit none

    private

    public :: sVariable

    type, extends(sArray) :: sVariable
        ! real(r32) :: min_value
        ! real(r32) :: max_value
        type(String) :: default_value

        type(sArray), allocatable :: dim_names
        type(sArray), allocatable :: module_names
    end type

    ! interface sVariable
    !     module procedure constructor
    ! end interface
end module
