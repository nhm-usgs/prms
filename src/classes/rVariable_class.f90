module rVariable_class
    use rArray_class, only: rArray
    use variableKind
    implicit none

    type, extends(rArray) :: rVariable
        real(r32) :: min_value
        real(r32) :: max_value
        real(r32) :: default_value

        ! type(str_arr_type), allocatable :: dim_names(:)
        ! type(str_arr_type), allocatable :: module_names(:)
    end type rVariable

    ! interface rVariable
    !     module procedure constructor
    ! end interface rVariable
end module rVariable_class
