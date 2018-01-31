module Object_class
    !! Base Object class akin to Python's object.
    !! This class is never declared due to it being abstract, but is extended in further classes
    !! [[Abc_Class]], [[rArray_Class]], [[rVariable_Class]]
    implicit none

    private

    public :: Object

    type, abstract :: Object
        !! Base object
    end type

end module
