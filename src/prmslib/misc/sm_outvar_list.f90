submodule (PRMS_OUTVAR_LIST) sm_outvar_list
contains
  module function contructor_outvar_list() result(this)
    implicit none
    type(outvar_list) :: this

    ! -------------------------------------------------------------------------
    this%count = 0
  end function

  module function exists(this, key) result(res)
    logical :: res
    class(outvar_list), intent(in) :: this
    character(len=*), intent(in) :: key

    ! -------------------------------------------------------------------------
    res = .false.

    ! traverse the list
    call this%traverse_list(key_search)

    contains
      subroutine key_search(ptr, done)
        type(outvar_node), pointer, intent(in) :: ptr
        logical, intent(out) :: done

        res = this%keys_equal(ptr%key, key)
        done = res
      end subroutine
  end function

  module function length(this) result(res)
    integer(i32) :: res
    class(outvar_list), intent(in) :: this

    ! -------------------------------------------------------------------------
    res = this%count
  end function

  module subroutine traverse_list(this, iterator)
    class(outvar_list), intent(in) :: this
    procedure(iterator_func) :: iterator

    ! Private variables
    type(outvar_node), pointer :: ptr
    logical :: done

    ! -------------------------------------------------------------------------
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
  end subroutine


  module subroutine traverse(this, iterator)
    class(outvar_list), intent(inout) :: this
    procedure(key_iterator) :: iterator

    ! -------------------------------------------------------------------------
    call this%traverse_list(key_iterator_wrapper)

    contains
      subroutine key_iterator_wrapper(this, done)
        type(outvar_node), pointer, intent(in) :: this
        logical, intent(out) :: done
          !! Stops traversing when true

        ! call iterator(this%key, this%value, this%datatype, done)
        call iterator(this%key, this%datatype, done)
      end subroutine
  end subroutine


  module subroutine remove_by_key(this, key)
    class(outvar_list), intent(inout) :: this
    character(len=*), intent(in) :: key

    ! Private variables
    type(outvar_node), pointer :: ptr

    ! -------------------------------------------------------------------------
    call this%get_node(key, ptr)
    call this%remove_by_pointer(ptr)
  end subroutine


  module subroutine remove_by_pointer(this, ptr)
    class(outvar_list), intent(inout) :: this
    type(outvar_node), pointer :: ptr

    ! Private variables
    logical :: has_next, has_prev

    ! -------------------------------------------------------------------------
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
  end subroutine


  module subroutine get_outvar_info(this, key, dimensions, datatype, description, units)
    class(outvar_list), intent(in) :: this
    character(len=*), intent(in) :: key
    character(len=:), allocatable, intent(out) :: dimensions
    integer(i32), intent(out) :: datatype
    ! character(len=:), allocatable, intent(inout) :: datatype
    character(len=:), allocatable, intent(out) :: description
    character(len=:), allocatable, intent(out) :: units

    ! Private variables
    type(outvar_node), pointer :: ptr_node

    ! -------------------------------------------------------------------------
    call this%get_node(key, ptr_node)

    if (associated(ptr_node)) then
      dimensions = ptr_node%dimensions
      datatype = ptr_node%datatype
      description = ptr_node%description
      units = ptr_node%units
    else
      write(error_unit, *) 'get_node() ERROR: ', key, ' is not a valid output variable.'
      stop
    end if
  end subroutine

  module subroutine get_node(this, key, ptr_node)
    class(outvar_list), intent(in) :: this
    character(len=*), intent(in) :: key
    type(outvar_node), pointer, intent(out) :: ptr_node

    ! Private variables
    type(outvar_node), pointer :: ptr

    ! -------------------------------------------------------------------------
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
  end subroutine


  pure module function keys_equal(this, k1, k2) result(res)
    logical :: res
    class(outvar_list), intent(in) :: this
    character(len=*), intent(in) :: k1
    character(len=*), intent(in) :: k2

    ! -------------------------------------------------------------------------
    res = .false.
    res = k1 == k2
  end function


  module subroutine set_outvar(this, key, dimensions, datatype, description, units)
    class(outvar_list), intent(inout) :: this
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: dimensions
    integer(i32), intent(in) :: datatype
    ! character(len=*), intent(in) :: datatype
    character(len=*), intent(in) :: description
    character(len=*), intent(in) :: units

    ! Private variables
    type(outvar_node), pointer :: ptr

    ! -------------------------------------------------------------------------
    ! NOTE: If an item with the same key is already
    !       in the list it is removed and replaced with
    !       the new value(s)
    if (len_trim(key) < 1) error stop 'Error: key must be nonblank.'

    call this%get_node(key, ptr)
    if (associated(ptr)) then
      ! If the output variable already exists then remove it
      call this%remove_by_pointer(ptr)
    end if

    ! Add the output variable
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
    ptr%dimensions = dimensions
    ptr%datatype = datatype
    ptr%description = description
    ptr%units = units
  end subroutine


  pure elemental module subroutine list_finalizer(this)
    type(outvar_list), intent(inout) :: this

    ! -------------------------------------------------------------------------
    call this%destroy()
  end subroutine


  pure elemental module subroutine destroy_list(this)
    class(outvar_list), intent(inout) :: this

    ! -------------------------------------------------------------------------
    this%count = 0

    if (associated(this%head)) call destroy_node(this%head)

    nullify(this%head)
    nullify(this%tail)
  end subroutine

  pure recursive module subroutine destroy_node(this)
    type(outvar_node), pointer, intent(inout) :: this

    ! -------------------------------------------------------------------------
    if (associated(this)) then
      call this%destroy()
      call destroy_node(this%next)
      nullify(this%prev)

      deallocate(this)
      nullify(this)
    end if
  end subroutine

end submodule