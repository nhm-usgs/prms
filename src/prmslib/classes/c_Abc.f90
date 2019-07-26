module Abc_class
  !!# Abstract Base Class
  !!This class defines an abstract base class which is extended within this library
  !!
  !!The Abc class should never be instantiated.
  !!
  !!See [[rArray_Class]], [[rVariable_Class]]

  use Object_class, only: Object
  implicit none

  private

  public :: Abc

  type, abstract, extends(Object) :: Abc
    ! private
    !! Abstract base class
    character(len=:), allocatable :: name
      !! Name of variable that this class represents
    character(len=:), allocatable :: description
      !! Description of variable that this class represents
    character(len=:), allocatable :: units
      !! Units of variable that this class represents

    contains
      procedure(print_abc), pass(this), public, deferred :: print
  end type

  abstract interface
    subroutine print_abc(this, delim)
      import :: Abc

      class(Abc), intent(in) :: this

      character(len=*), intent(in), optional :: delim
    end subroutine
  end interface

end module
