module Abc_class
    use Object_class, only: Object
    implicit none

    type, abstract, extends(Object) :: Abc
        !! Abstract base class
        character(len=:), allocatable :: name
        character(len=:), allocatable :: description
        character(len=:), allocatable :: units

    contains
        private
            procedure(Iprint), pass(this), public, deferred :: print
    end type Abc

    abstract interface
        subroutine Iprint(this)
            import :: Abc

            class(Abc) :: this
        end subroutine Iprint
    end interface

end module Abc_class
