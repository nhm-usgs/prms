module iVariable_class
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
    use iArray_class, only: iArray


    implicit none

    private

    public :: iVariable

    type, extends(iArray) :: iVariable
        integer(i32) :: min_value
        integer(i32) :: max_value
        integer(i32) :: default_value

        ! type(str_arr_type), allocatable :: dim_names(:)
        ! type(str_arr_type), allocatable :: module_names(:)
    end type

    ! interface iVariable
    !     module procedure constructor
    ! end interface
end module
