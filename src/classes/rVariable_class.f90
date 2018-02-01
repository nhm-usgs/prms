module rVariable_class
    !!# Variable Class (single precision real)
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
    use sArray_class, only: sArray
    use rArray_class, only: rArray


    implicit none

    private

    public :: rVariable

    type, extends(rArray) :: rVariable
        real(r32) :: min_value
        real(r32) :: max_value
        real(r32) :: default_value

        type(sArray) :: dim_names
        type(sArray) :: module_names
    end type

    ! interface rVariable
    !     module procedure constructor
    ! end interface
end module
