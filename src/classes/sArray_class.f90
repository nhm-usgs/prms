module sArray_class
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

    use iso_fortran_env, only: output_unit
    use variableKind, only: i32, cLen
    use Abc_class, only: Abc
    use m_allocate, only: allocate
    use m_deallocate, only: deallocate
    use m_errors, only: mErr, fErr
    use String_class, only: String
    use m_strings, only: str
    implicit none

    private

    public :: sArray

    type, extends(Abc) :: sArray
        private
        !! 1D array of reals that can represent multiple dimensional data
        type(String), allocatable :: values(:)
          !! The values of the array

    contains
        generic, public :: allocate => allocate_sArray_i1_
          !! Allocate the memory inside the class
        procedure, private, pass(this) :: allocate_sArray_i1_ => allocate_sArray_i1

        procedure, public, pass(this) :: deallocate => deallocate_sArray
          !! Deallocate the memory inside the class
        procedure, public, pass(this) :: print => print_sArray
          !! Print the class to the screen
        procedure, public, pass(this) :: size => size_sArray
          !! Get the size of class
        procedure, public, pass(this) :: read => read_sArray
          !! read the class from a file handle

    end type

    interface sArray
      !! Overloaded interface to instantiate the class.
        module procedure :: constructor_sArray_i1
    end interface

contains
    !====================================================================!
    function constructor_sArray_i1(N) result(this)
        type(sArray) :: this
          !! sArray class
        integer(i32), intent(in) :: N
          !! Size in 1D of the class

        call this%allocate(N)
    end function
    !====================================================================!

    !====================================================================!
    subroutine allocate_sArray_i1(this, N)
        class(sArray), intent(inout) :: this
          !! sArray class
        integer(i32), intent(in) :: N
          !! Allocate the values to size N, and set the dims array to 1D of length one.
    
        integer(i32) :: istat
          
        allocate(this%values(N), stat=istat)
        call mErr(istat, 'sArray%allocate(): values', 1)
        
    end subroutine
    !====================================================================!


    !====================================================================!
    subroutine deallocate_sArray(this)
        class(sArray), intent(inout) :: this
          !! sArray class

        integer(i32) :: istat

        deallocate(this%values, stat=istat)
        call mErr(istat, 'sArray%deallocate(): values', 1)

    end subroutine
    !====================================================================!


    !====================================================================!
    subroutine print_sArray(this, delim)
        class(sArray), intent(in) :: this
          !! sArray class
        character(len=*), intent(in), optional :: delim
          !! Delimiter between values

        integer(i32) :: i

        do i = 1, size(this%values)
            write(output_unit, '(DT)') this%values(i)
        enddo
        
    end subroutine
    !====================================================================!

    !====================================================================!
    subroutine read_sArray(this, iUnit, fName)
        class(sArray), intent(inout) :: this
          !! sArray Class
        integer(i32), intent(in) :: iUnit
          !! Unit number to read from
        character(len=*), intent(in) :: fName
          !! Name of the file that was opened

        integer(i32) :: i, istat
        integer(i32) :: N

        read(iUnit, *) N
        read(iUnit, *) i

        call this%allocate(N)

        do i = 1, N
            call this%values(i)%read(iUnit, fName)
        enddo

    end subroutine
    !====================================================================!

    !====================================================================!
    function size_sArray(this) result(res)
      !! Get the size of the list of strings
        class(sArray), intent(in) :: this
          !! sArray Class
        integer(i32) :: res
          !! Size of the list
    
        res = size(this%values)
    end function
    !====================================================================!
end module
