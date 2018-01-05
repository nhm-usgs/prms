
! Linked list for dimensions is based on code from:
! https://github.com/jacobwilliams/flist/blob/master/src/linked_list_module.f90

module dimensions_mod
    use kinds_mod, only: r4, r8, i4, i8
    implicit none

    ! ************************************************************************
    ! Individual dimension type
    TYPE dimension_t
        character(len=:), allocatable :: key
        character(len=:), allocatable :: description
        integer(i4) :: default
        integer(i4) :: maximum
        integer(i4) :: value
        type(dimension_t), pointer :: next => null()   ! pointer to next dimension entry
        type(dimension_t), pointer :: prev => null()   ! pointer to previous dimension entry
    contains
        procedure, public :: destroy  => destroy_node_data  ! deallocate value
        procedure, public :: get_data => get_dim_size       ! get data from a node

    END TYPE dimension_t

    interface dimension_t
        module procedure constructor
    end interface dimension_t


    ! ************************************************************************
    ! Linked-list of dimensions
    type, public :: dimension_list
        private

        type(dimension_t), pointer :: head => null() ! pointer to first dimension entry
        type(dimension_t), pointer :: tail => null() ! pointer to last dimension entry
        integer(i4) :: count   ! number of entries in the list

    contains
        procedure, public :: set_dimension
        procedure, public :: exists     ! tests if dimension name exists in list
        procedure, public :: size       ! returns the number of defined dimensions


        procedure, public :: traverse                ! traverse the list and return each key & value
        procedure, public :: remove => remove_by_key ! remove item from the list, given the key
        procedure, public :: destroy => destroy_list

        ! procedures that operate on dimensions
        procedure, public :: remove_by_pointer ! remove node from list, given pointer to it
        procedure, public :: get_node          ! get a pointer to a node in the list
        procedure, public :: get_data          ! get value of dimension in the list
        procedure, public :: traverse_list     ! traverse each node of the list

        procedure :: keys_equal     ! for testing key string equality
    end type dimension_list


    interface dimension_list
        procedure :: init_list
    end interface

    abstract interface
        subroutine iterator_func(this, done)
            ! internal function for traversing all dimension nodes in a list
            import :: dimension_t
            implicit none

            type(dimension_t), pointer  :: this
            logical, intent(out) :: done    ! set to true to stop traversing
        end subroutine iterator_func

        subroutine key_iterator(key, value, done)
            ! for traversing all keys in a list
            use kinds_mod, only: r4, r8, i4, i8
            implicit none

            character(len=*), intent(in) :: key
            integer(i4), intent(in) :: value
            logical, intent(out) :: done  ! set to true to stop traversing
        end subroutine key_iterator
    end interface

contains

    ! ************************************************************************
    ! dimension_t procedures
    ! ************************************************************************
    function constructor(key, description, def_value, max_value, value)
        type(dimension_t) :: constructor
        character(len=:), allocatable, intent(in) :: key
        character(len=:), allocatable, intent(in) :: description
        integer(i4), intent(in) :: def_value
        integer(i4), intent(in) :: max_value
        integer(i4), intent(in) :: value

        constructor%key = key
        constructor%description = description
        constructor%default = def_value
        constructor%maximum = max_value
        constructor%value = value
    end function constructor

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Get the data from a node
    subroutine get_dim_size(this, value)
        implicit none

        class(dimension_t), intent(in) :: this
        integer(i4), intent(out) :: value

        value = this%value
    end subroutine get_dim_size

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Destroy the data in the node.
    pure elemental subroutine destroy_node_data(this)
        implicit none

        class(dimension_t), intent(inout) :: this

        if (allocated(this%key)) deallocate(this%key)
    end subroutine destroy_node_data



    ! ************************************************************************
    ! dimension_list procedures
    ! ************************************************************************
    function init_list()
        implicit none

        ! initialize the dimension list
        type(dimension_list) :: init_list

        init_list%count = 0
    end function init_list


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Check if a given dimension exists in the list
    function exists(this, key)
        implicit none

        class(dimension_list), intent(inout) :: this
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

                type(dimension_t), pointer  :: ptr
                logical, intent(out) :: done

                exists = this%keys_equal(ptr%key, key)
                done = exists
            end subroutine key_search
    end function exists

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Return the number of dimensions in the list
    function size(this)
        implicit none

        class(dimension_list), intent(in) :: this
        integer(i4) :: size

        size = this%count
    end function size


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine traverse_list(this, iterator)
        implicit none

        class(dimension_list), intent(inout) :: this
        procedure(iterator_func) :: iterator  ! function to call for each dimension node

        ! Private variables
        type(dimension_t), pointer :: ptr
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

        class(dimension_list), intent(inout) :: this
        procedure(key_iterator)  :: iterator  ! the function to call for each node.

        call this%traverse_list(key_iterator_wrapper)

        contains
            subroutine key_iterator_wrapper(this, done)
                ! for calling the user-specified key_iterator function.
                implicit none

                type(dimension_t), pointer  :: this
                logical, intent(out) :: done ! set to true to stop traversing

                call iterator(this%key, this%value, done)
            end subroutine key_iterator_wrapper
    end subroutine traverse


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Remove an item from the list (given the key).
    subroutine remove_by_key(this, key)
        implicit none

        class(dimension_list), intent(inout) :: this
        character(len=*), intent(in) :: key

        type(dimension_t), pointer :: ptr

        call this%get_node(key, ptr)
        call this%remove_by_pointer(ptr)
    end subroutine remove_by_key

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Remove an item from the list.
    subroutine remove_by_pointer(this, ptr)
        implicit none

        class(dimension_list), intent(inout) :: this
        type(dimension_t), pointer :: ptr   ! the item to remove

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
    !  Returns a pointer to the data stored in the list.
    subroutine get_data(this, key, value)
        implicit none

        class(dimension_list), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(i4), intent(out) :: value

        type(dimension_t), pointer :: ptr

        call this%get_node(key, ptr)

        if (associated(ptr)) then
            value = ptr%value
        else
            write (*,*) 'Dimension does not exist'
            value = -1
        endif
    end subroutine get_data

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Returns a pointer to a node in a list.
    subroutine get_node(this, key, ptr_node)
        implicit none

        class(dimension_list),intent(in) :: this
        character(len=*), intent(in) :: key
        type(dimension_t), pointer, intent(out) :: ptr_node

        type(dimension_t), pointer :: ptr

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

        class(dimension_list), intent(in) :: this
        character(len=*), intent(in) :: k1
        character(len=*), intent(in) :: k2

        logical :: keys_equal

        keys_equal = .false.
        keys_equal = k1 == k2
    end function keys_equal


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Add a dimension to the list
    ! NOTE: If an item with the same key is already
    !       in the list its value is replaced with
    !       the new value.
    subroutine set_dimension(this, key, value, description, default, maximum)
        implicit none

        class(dimension_list), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(i4), intent(in) :: value
        character(len=*), optional, intent(in) :: description
        integer(i4), optional, intent(in) :: default
        integer(i4), optional, intent(in) :: maximum

        ! Private variables
        type(dimension_t), pointer :: ptr

        if (len_trim(key) < 1) error stop 'Error: key must be nonblank.'

        call this%get_node(key, ptr)
        if (associated(ptr)) then
            ! If the dimension already exists then modify it
            ! ptr%key = key
            ptr%value = value
            if (present(description)) ptr%description = description
            if (present(default)) ptr%default = default
            if (present(maximum)) ptr%maximum = maximum
        else
            ! Otherwise add a new dimension
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
            if (present(description)) ptr%description = description
            if (present(default)) ptr%default = default
            if (present(maximum)) ptr%maximum = maximum
        end if
    end subroutine set_dimension


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Wrapper for destroy_list()
    pure elemental subroutine list_finalizer(this)
        implicit none

        type(dimension_list), intent(inout) :: this

        call this%destroy()
    end subroutine list_finalizer


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Destroy the list (traverses from head to tail)
    pure elemental subroutine destroy_list(this)
        implicit none

        class(dimension_list), intent(inout) :: this

        this%count = 0

        if (associated(this%head)) call destroy_node(this%head)

        nullify(this%head)
        nullify(this%tail)
    end subroutine destroy_list


    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! destroy the node (and subsequent ones in the list).
    pure recursive subroutine destroy_node(this)
        implicit none

        type(dimension_t), pointer :: this

        if (associated(this)) then
            call this%destroy()
            call destroy_node(this%next)
            nullify(this%prev)

            deallocate(this)
            nullify(this)
        end if
    end subroutine destroy_node
end module dimensions_mod
