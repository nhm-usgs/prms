module dVariable_class
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
    use dArray_class, only: dArray


    implicit none

    private

    public :: dVariable

    type, extends(dArray) :: dVariable
        real(r64) :: min_value
        real(r64) :: max_value
        real(r64) :: default_value

        ! type(str_arr_type), allocatable :: dim_names(:)
        ! type(str_arr_type), allocatable :: module_names(:)
    end type

    ! interface dVariable
    !     module procedure constructor
    ! end interface
end module
