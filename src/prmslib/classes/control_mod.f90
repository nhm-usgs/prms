module control_mod
    use kinds_mod, only: r4, r8, i4, i8
    use arr_mod, only: array_1D_t
    implicit none

    private
    public :: control_t

    ! ************************************************************************
    ! Control parameter type
    type :: control_t
        ! private
        character(len=:), allocatable :: key   ! name of control entry
        integer(i4) :: datatype     ! data type of the elements (1=integer, 2=real, 3=double, 4=string)
        type(array_1D_t) :: value

        type(control_t), pointer :: next => null()   ! pointer to next control entry
        type(control_t), pointer :: prev => null()   ! pointer to previous control entry
    contains
        private
        procedure, public :: destroy  => destroy_node_data  ! deallocate value
        ! procedure, public :: get_data => get_dim_size       ! get data from a node
        procedure, public :: print
    end type control_t

    interface control_t
        module procedure constructor
    end interface control_t

    contains

        ! ************************************************************************
        ! control_t procedures
        ! ************************************************************************
        function constructor(name, datatype, value)
            ! Declaring the constructor as class(control_t) causes gfortran to
            ! segfault when compiling the test program with this type
            type(control_t) :: constructor
            character(len=*), intent(in) :: name
            integer(i4), intent(in) :: datatype
            class(*), intent(in) :: value(:)

            ! *******************************
            constructor%key = name
            constructor%datatype = datatype
            constructor%value = value
        end function constructor

        ! function length(this)
        !     ! Return the number of elements in the value array
        !     implicit none
        !
        !     class(control_t), intent(in) :: this
        !     integer :: length
        !
        !     length = length(this%value)
        ! end function length


        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! Get the data from a node
        ! subroutine get_dim_size(this, value)
        !     implicit none
        !
        !     class(control_t), intent(in) :: this
        !     integer, intent(out) :: value
        !
        !     value = this%value
        ! end subroutine get_dim_size

        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! Destroy the data in the node.
        pure elemental subroutine destroy_node_data(this)
            implicit none

            class(control_t), intent(inout) :: this

            if (allocated(this%key)) deallocate(this%key)
        end subroutine destroy_node_data


        subroutine print(this)
            class(control_t), intent(in) :: this

            print *, '**** Control variable ****'
            print *, 'Name: ', this%key
            print *, 'Number of elements: ', this%value%length()
            print *, 'Datatype: ', this%datatype
            print *, '--- value ---'
            call this%value%print()
        end subroutine print
end module control_mod


module control_ll_mod
    use kinds_mod, only: r4, r8, i4, i8
    use control_mod

    implicit none

    ! ************************************************************************
    ! Linked-list of control parameters
    type, public :: control_list
        private

        type(control_t), pointer :: head => null() ! pointer to first entry
        type(control_t), pointer :: tail => null() ! pointer to last entry
        integer(i4) :: count   ! number of entries in the list

    contains
        procedure, private :: set_ctl_param
        ! procedure, private :: set_ctl_param_scalar
        procedure, public :: exists       ! tests if control parameter exists in list
        procedure, public :: length       ! returns the number of defined control parameters


        procedure, public :: traverse                ! traverse the list and return each key & value
        procedure, public :: remove => remove_by_key ! remove item from the list, given the key
        procedure, public :: destroy => destroy_list

        procedure, private :: get_data_str
        procedure, private :: get_data_str_scalar
        procedure, private :: get_data_int
        procedure, private :: get_data_int_scalar
        procedure, private :: get_data_real
        procedure, private :: get_data_real_scalar
        procedure, private :: get_data_dbl
        procedure, private :: get_data_dbl_scalar
        procedure, private :: get_data_char_scalar

        ! procedures that operate on dimensions
        procedure, public :: remove_by_pointer ! remove node from list, given pointer to it
        procedure, public :: get_node          ! get a pointer to a node in the list

        ! get value of control entry in the list
        generic, public :: get_data => get_data_str, get_data_int, get_data_real, &
                                       get_data_dbl, get_data_int_scalar, &
                                       get_data_real_scalar, get_data_dbl_scalar, &
                                       get_data_str_scalar, get_data_char_scalar
        generic, public :: set => set_ctl_param ! , set_ctl_param_scalar
        procedure, public :: traverse_list     ! traverse each node of the list

        procedure :: keys_equal     ! for testing key string equality
    end type control_list

    interface control_list
        procedure :: init_list
    end interface

    abstract interface
        subroutine iterator_func(this, done)
            ! internal function for traversing all control parameters in a list
            import :: control_t
            implicit none

            type(control_t), pointer  :: this
            logical, intent(out) :: done    ! set to true to stop traversing
        end subroutine iterator_func

        subroutine key_iterator(key, value, datatype, done)
            ! for traversing all keys in a list
            use kinds_mod, only: i4
            use arr_mod, only: array_1D_t
            implicit none

            character(len=*), intent(in) :: key
            type(array_1D_t), intent(in) :: value
            integer(i4), intent(in) :: datatype
            logical, intent(out) :: done  ! set to true to stop traversing
        end subroutine key_iterator
    end interface

contains
    ! ************************************************************************
    ! control_list procedures
    ! ************************************************************************
    function init_list()
        implicit none

        ! initialize the control parameter list
        type(control_list) :: init_list

        init_list%count = 0
    end function init_list


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Check if a given control parameter exists in the list
    function exists(this, key)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key

        ! Private variables
        logical :: exists

        exists = .false.

        ! traverse the list
        call this%traverse_list(key_search)

        contains
            subroutine key_search(ptr, done)
                ! Search for the key
                implicit none

                type(control_t), pointer  :: ptr
                logical, intent(out) :: done

                exists = this%keys_equal(ptr%key, key)
                done = exists
            end subroutine key_search
    end function exists

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Return the number of control parameters in the list
    function length(this)
        implicit none

        class(control_list), intent(in) :: this
        integer(i4) :: length

        length = this%count
    end function length


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine traverse_list(this, iterator)
        implicit none

        class(control_list), intent(in) :: this
        procedure(iterator_func) :: iterator  ! function to call for each control parameter

        ! Private variables
        type(control_t), pointer :: ptr
        logical :: done

        done = .false.
        ptr => this%head

        do
            if (associated(ptr)) then
               call iterator(ptr, done)
               if (done) exit

               ptr => ptr%next
            else
               exit ! done
            end if
        end do
    end subroutine traverse_list

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! traverse list from head to tail, calling the
    ! iterator function for each key.
    subroutine traverse(this, iterator)
        implicit none

        class(control_list), intent(inout) :: this
        procedure(key_iterator)  :: iterator  ! the function to call for each node.

        call this%traverse_list(key_iterator_wrapper)

        contains
            subroutine key_iterator_wrapper(this, done)
                ! for calling the user-specified key_iterator function.
                implicit none

                type(control_t), pointer  :: this
                logical, intent(out) :: done ! set to true to stop traversing

                call iterator(this%key, this%value, this%datatype, done)
            end subroutine key_iterator_wrapper
    end subroutine traverse


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Remove a control parameter from the list (given the key).
    subroutine remove_by_key(this, key)
        implicit none

        class(control_list), intent(inout) :: this
        character(len=*), intent(in) :: key

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)
        call this%remove_by_pointer(ptr)
    end subroutine remove_by_key

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Remove a control parameter from the list given a pointer.
    subroutine remove_by_pointer(this, ptr)
        implicit none

        class(control_list), intent(inout) :: this
        type(control_t), pointer :: ptr   ! the item to remove

        logical :: has_next, has_prev

        if (associated(ptr)) then
            call ptr%destroy()  ! destroy the data

            has_next = associated(ptr%next)
            has_prev = associated(ptr%prev)

            if (has_next .and. has_prev) then ! neither first nor last in a list
                ptr%prev%next => ptr%next
                ptr%next%prev => ptr%prev
            elseif (has_next .and. .not. has_prev) then  ! first one in a list
                this%head => ptr%next
                this%head%prev => null()
            elseif (has_prev .and. .not. has_next) then  ! last one in a list
                this%tail => ptr%prev
                this%tail%next => null()
            elseif (.not. has_prev .and. .not. has_next) then  ! only one in the list
                this%head => null()
                this%tail => null()
            end if

            deallocate(ptr)
            nullify(ptr)

            this%count = this%count - 1
        end if
    end subroutine remove_by_pointer

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Returns a pointer to the control parameter data stored in the list.
    subroutine get_data_str(this, key, value, missing_stop)
        use data_mod, only: str_arr_type
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        type(str_arr_type), allocatable, intent(inout) :: value(:)
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            if (allocated(value)) deallocate(value)
            allocate(value(ptr%value%length()))
            value = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_str

    subroutine get_data_int(this, key, val, missing_stop)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(i4), allocatable, intent(inout) :: val(:)
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            if (allocated(val)) deallocate(val)
            allocate(val(ptr%value%length()))
            val = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_int

    subroutine get_data_int_scalar(this, key, value, missing_stop)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(i4), intent(inout) :: value
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            value = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_int_scalar

    subroutine get_data_real(this, key, val, missing_stop)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        real(r4), allocatable, intent(inout) :: val(:)
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            if (allocated(val)) deallocate(val)
            allocate(val(ptr%value%length()))
            val = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_real

    subroutine get_data_real_scalar(this, key, value, missing_stop)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        real(r4), intent(out) :: value
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            value = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_real_scalar

    subroutine get_data_dbl(this, key, val, missing_stop)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        real(r8), allocatable, intent(inout) :: val(:)
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            if (allocated(val)) deallocate(val)
            allocate(val(ptr%value%length()))
            val = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_dbl

    subroutine get_data_dbl_scalar(this, key, value, missing_stop)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        real(r8), intent(out) :: value
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            value = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_dbl_scalar

    subroutine get_data_str_scalar(this, key, value, missing_stop)
        use data_mod, only: str_arr_type
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        type(str_arr_type), allocatable, intent(inout) :: value
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            ! if (allocated(value)) deallocate(value)
            ! allocate(value(ptr%value%length()))
            value = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_str_scalar

    subroutine get_data_char_scalar(this, key, value, missing_stop)
        use data_mod, only: str_arr_type
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: key
        character(:), allocatable, intent(inout) :: value
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            ! if (allocated(value)) deallocate(value)
            ! allocate(value(ptr%value%length()))
            value = ptr%value
        else
            if (present(missing_stop) .and. missing_stop) then
                write (*, *) 'ERROR: Control parameter, ', key, ' does not exist'
                stop
            end if

            return
        endif
    end subroutine get_data_char_scalar

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Returns a pointer to a control parameter (node) in a list.
    subroutine get_node(this, key, ptr_node, missing_stop)
        implicit none

        class(control_list),intent(in) :: this
        character(len=*), intent(in) :: key
        type(control_t), pointer, intent(out) :: ptr_node
        logical, optional, intent(in) :: missing_stop

        type(control_t), pointer :: ptr

        nullify(ptr_node)

        ptr => this%head
        do
            if (associated(ptr)) then
                if (this%keys_equal(ptr%key, key)) then
                    ptr_node => ptr
                    return
                end if
                ptr => ptr%next
            else
                return ! not found
            end if
        end do
    end subroutine get_node

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Returns true if the two keys are equal.
    pure function keys_equal(this, k1, k2)
        implicit none

        class(control_list), intent(in) :: this
        character(len=*), intent(in) :: k1
        character(len=*), intent(in) :: k2

        logical :: keys_equal

        keys_equal = .false.
        keys_equal = k1 == k2
    end function keys_equal


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Add a control parameter to the list
    subroutine set_ctl_param(this, key, value, datatype)
        implicit none

        class(control_list), intent(inout) :: this
        character(len=*), intent(in) :: key
        class(*), intent(in) :: value(:)
        integer(i4), intent(in) :: datatype

        ! Private variables
        type(control_t), pointer :: ptr

        ! NOTE: If an item with the same key is already
        !       in the list it is removed and replaced with
        !       the new value(s)

        if (len_trim(key) < 1) error stop 'Error: key must be nonblank.'

        call this%get_node(key, ptr)
        if (associated(ptr)) then
            ! If the control parameter already exists then remove it
            call this%remove_by_pointer(ptr)
        end if

        ! Add the control parameter
        if (associated(this%tail)) then
            allocate(this%tail%next)  ! insert new item at the end
            ptr => this%tail%next
            ptr%prev => this%tail
        else
            allocate(this%head)  ! first item in the list
            ptr => this%head
        end if

        this%tail => ptr
        this%count = this%count + 1

        ptr%key = key
        ptr%value = value
        ptr%datatype = datatype
    end subroutine set_ctl_param

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Add a control parameter to the list
    ! subroutine set_ctl_param_scalar(this, key, value, datatype)
    !     implicit none
    !
    !     class(control_list), intent(inout) :: this
    !     character(len=*), intent(in) :: key
    !     class(*), intent(in) :: value
    !     integer, intent(in) :: datatype
    !
    !     ! Private variables
    !     type(control_t), pointer :: ptr
    !
    !     ! NOTE: If an item with the same key is already
    !     !       in the list it is removed and replaced with
    !     !       the new value(s)
    !
    !     if (len_trim(key) < 1) error stop 'Error: key must be nonblank.'
    !
    !     call this%get_node(key, ptr)
    !     if (associated(ptr)) then
    !         ! If the control parameter already exists then remove it
    !         call this%remove_by_pointer(ptr)
    !     end if
    !
    !     ! Add the control parameter
    !     if (associated(this%tail)) then
    !         allocate(this%tail%next)  ! insert new item at the end
    !         ptr => this%tail%next
    !         ptr%prev => this%tail
    !     else
    !         allocate(this%head)  ! first item in the list
    !         ptr => this%head
    !     end if
    !
    !     this%tail => ptr
    !     this%count = this%count + 1
    !
    !     ptr%key = key
    !     ptr%value = [value]
    !     ptr%datatype = datatype
    ! end subroutine set_ctl_param_scalar

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Wrapper for destroy_list()
    pure elemental subroutine list_finalizer(this)
        implicit none

        type(control_list), intent(inout) :: this

        call this%destroy()
    end subroutine list_finalizer


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Destroy the list (traverses from head to tail)
    pure elemental subroutine destroy_list(this)
        implicit none

        class(control_list), intent(inout) :: this

        this%count = 0

        if (associated(this%head)) call destroy_node(this%head)

        nullify(this%head)
        nullify(this%tail)
    end subroutine destroy_list


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! destroy the node (and subsequent ones in the list).
    pure recursive subroutine destroy_node(this)
        implicit none

        type(control_t), pointer :: this

        if (associated(this)) then
            call this%destroy()
            call destroy_node(this%next)
            nullify(this%prev)

            deallocate(this)
            nullify(this)
        end if
    end subroutine destroy_node
end module control_ll_mod
