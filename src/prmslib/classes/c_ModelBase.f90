module ModelBase_class
  !! Abstract model class

  implicit none

  private
  public :: ModelBase

  type, abstract :: ModelBase
    character(len=:), allocatable, private :: MODDESC
    character(len=:), allocatable, private :: MODNAME
    character(len=:), allocatable, private :: MODVERSION
    contains
      procedure, public :: module_description
        !! Return the description of the module
      procedure, public :: module_name
        !! Return the name of the module
      procedure, public :: version
        !! Return the version of the module
      procedure, public :: print_module_info
        !! Print out the module information
      procedure, public :: set_module_info
        !! Set the name, description, and version of the module
  end type

  interface
    module function module_description(this) result(res)
      character(:), allocatable :: res
      class(ModelBase), intent(in) :: this
    end function
  end interface

  interface
    module function module_name(this) result(res)
      character(:), allocatable :: res
      class(ModelBase), intent(in) :: this
    end function
  end interface

  interface
    module subroutine print_module_info(this)
      class(ModelBase), intent(in) :: this
    end subroutine
  end interface

  interface
    module function version(this) result(res)
      character(:), allocatable :: res
      class(ModelBase), intent(in) :: this
    end function
  end interface

  interface
    module subroutine set_module_info(this, name, desc, version)
      class(ModelBase), intent(inout) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: desc
      character(len=*), intent(in) :: version
    end subroutine
  end interface
end module
