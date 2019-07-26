module PRMS_OUTVAR_LIST
  use PRMS_OUTVAR_NODE
  use variableKind
  use iso_fortran_env, only: output_unit, error_unit
  implicit none

  private
  public :: outvar_list

  type :: outvar_list
    private

    type(outvar_node), pointer :: head => null()
      !! Pointer to first output variable entry
    type(outvar_node), pointer :: tail => null()
      !! Pointer to last output variable entry
    integer(i32) :: count
      !! Number of entries in the list

    contains
      ! procedure, private :: set_ctl_param
      procedure, public :: exists       ! tests if key exists in list
      procedure, public :: length       ! returns the number of list entries

      procedure, public :: traverse  ! traverse the list and return each key & value
      procedure, public :: remove => remove_by_key ! remove item from the list, given the key
      procedure, public :: destroy => destroy_list

      procedure, private :: get_outvar_info
      procedure, private :: set_outvar

      ! procedures that operate on dimensions
      procedure, public :: remove_by_pointer ! remove node from list, given pointer to it
      procedure, public :: get_node          ! get a pointer to a node in the list

      ! get value of control entry in the list
      generic, public :: get => get_outvar_info
      generic, public :: set => set_outvar
      procedure, public :: traverse_list     ! traverse each node of the list

      procedure :: keys_equal     ! for testing key string equality
  end type

  interface outvar_list
    module function contructor_outvar_list() result(this)
      type(outvar_list) :: this
    end function
  end interface

  abstract interface
    subroutine iterator_func(this, done)
      import :: outvar_node
      ! internal function for traversing all control parameters in a list
      type(outvar_node), pointer, intent(in)  :: this
      logical, intent(out) :: done    ! set to true to stop traversing
    end subroutine

    subroutine key_iterator(key, datatype, done)
      use variableKind
      ! for traversing all keys in a list
      character(len=*), intent(in) :: key
      integer(i32), intent(in) :: datatype
      ! character(len=*), intent(in) :: datatype
      logical, intent(out) :: done  ! set to true to stop traversing
    end subroutine
  end interface

  interface
    module function exists(this, key) result(res)
      logical :: res
      class(outvar_list), intent(in) :: this
      character(len=*), intent(in) :: key

      ! contains
      !   module subroutine key_search(ptr, done)
      !     type(outvar_node), pointer, intent(in) :: ptr
      !     logical, intent(out) :: done
      !   end subroutine
    end function
  end interface

  interface
    module function length(this) result(res)
      integer(i32) :: res
      class(outvar_list), intent(in) :: this
    end function
  end interface

  interface
    module subroutine traverse_list(this, iterator)
      class(outvar_list), intent(in) :: this
      procedure(iterator_func) :: iterator
        !! Function to call for each item in list
    end subroutine
  end interface

  interface
    module subroutine traverse(this, iterator)
      class(outvar_list), intent(inout) :: this
      procedure(key_iterator) :: iterator
        !! The function to call for each list node

      ! contains
      !   module subroutine key_iterator_wrapper(this, done)
      !     type(outvar_list), pointer :: this
      !     logical, intent(out) :: done
      !       !! Stops traversing when true
      !   end subroutine
    end subroutine
  end interface

  interface
    module subroutine remove_by_key(this, key)
      class(outvar_list), intent(inout) :: this
      character(len=*), intent(in) :: key
    end subroutine
  end interface

  interface
    module subroutine remove_by_pointer(this, ptr)
      class(outvar_list), intent(inout) :: this
      type(outvar_node), pointer :: ptr
        !! The item to remove
    end subroutine
  end interface

  interface
    module subroutine get_outvar_info(this, key, dimensions, datatype, description, units)
      class(outvar_list), intent(in) :: this
      character(len=*), intent(in) :: key
      character(len=:), allocatable, intent(out) :: dimensions
      ! character(len=:), allocatable, intent(inout) :: datatype
      integer(i32), intent(out) :: datatype
      character(len=:), allocatable, intent(out) :: description
      character(len=:), allocatable, intent(out) :: units
    end subroutine
  end interface

  interface
    module subroutine get_node(this, key, ptr_node)
      class(outvar_list), intent(in) :: this
      character(len=*), intent(in) :: key
      type(outvar_node), pointer, intent(out) :: ptr_node
    end subroutine
  end interface

  interface
    pure module function keys_equal(this, k1, k2) result(res)
      logical :: res
      class(outvar_list), intent(in) :: this
      character(len=*), intent(in) :: k1
      character(len=*), intent(in) :: k2
    end function
  end interface

  interface
    module subroutine set_outvar(this, key, dimensions, datatype, description, units)
      class(outvar_list), intent(inout) :: this
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: dimensions
      ! character(len=*), intent(in) :: datatype
      integer(i32), intent(in) :: datatype
      character(len=*), intent(in) :: description
      character(len=*), intent(in) :: units
    end subroutine
  end interface

  interface
    pure elemental module subroutine list_finalizer(this)
      type(outvar_list), intent(inout) :: this
    end subroutine
  end interface

  interface
    pure elemental module subroutine destroy_list(this)
      class(outvar_list), intent(inout) :: this
    end subroutine
  end interface

  interface
    pure recursive module subroutine destroy_node(this)
      type(outvar_node), pointer, intent(inout) :: this
    end subroutine
  end interface


end module