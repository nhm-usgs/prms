module rArray_class
    !!
    !!
    !!

    use iso_fortran_env, only: output_unit
    use prms_constants, only: MAXFILE_LENGTH
    use variableKind, only: r32, i32
    use m_errors, only: fErr
    use Abc_class, only: Abc
    use m_allocate, only: allocate
    use m_deallocate, only: deallocate
    use m_strings, only: str

    implicit none

    private

    public :: rArray

    type, extends(Abc) :: rArray
      !! 1D array of reals that can represent multiple dimensional data
      real(r32), allocatable :: values(:)
        !! The values of the array
      integer(i32), allocatable :: dims(:)
        !! Size(s) that values should represent. e.g. If values represents 2D data, dims might be [10, 15]

    contains
      generic, public :: allocate => allocate_rArray_i1_, allocate_rArray_i1D_
        !! Allocate the memory inside the class
      procedure, private, pass(this) :: allocate_rArray_i1_ => allocate_rArray_i1
      procedure, private, pass(this) :: allocate_rArray_i1D_ => allocate_rArray_i1D

      procedure, public, pass(this) :: deallocate => deallocate_rArray
        !! Deallocate the memory inside the class
      procedure, public, pass(this) :: exists => exists_rArray
        !! Returns true if values array is allocated
      procedure, public, pass(this) :: print => print_rArray
        !! Print the class to the screen
      procedure, public, pass(this) :: read => read_rArray
        !! Read the class from a file
      procedure, public, pass(this) :: size => size_rArray

      ! procedure, private, pass(rhs) :: get_real_0D
      ! procedure, private, pass(rhs) :: get_real_1D
      ! procedure, private, pass(rhs) :: get_real_2D
      ! ! procedure, pass(this), public :: rval => fget_t_real
      !
      ! generic, public :: assignment(=) => get_real_0D, get_real_1D, get_real_2D
    end type

    interface rArray
      !! Overloaded interface to instantiate the class.
        module procedure :: constructor_rArray_i1, constructor_rArray_i1D
    end interface

contains
    !====================================================================!
    function constructor_rArray_i1(N) result(this)
        type(rArray) :: this
          !! rArray class
        integer(i32), intent(in) :: N
          !! Size in 1D of the class

        call this%allocate(N)
    end function
    !====================================================================!
    !====================================================================!
    function constructor_rArray_i1D(dims) result(this)
        type(rArray) :: this
          !! rArray class
        integer(i32), intent(in) :: dims(:)
          !! The shape of the rArray

        call this%allocate(dims)
        this%values = 0.0
    end function
    !====================================================================!

    !====================================================================!
    subroutine allocate_rArray_i1(this, N)
        class(rArray), intent(inout) :: this
          !! rArray class
        integer(i32), intent(in) :: N
          !! Allocate the values to size N, and set the dims array to 1D of length one.

        call allocate(this%values, N)
        call allocate(this%dims, 1)
        this%dims(1) = N
    end subroutine
    !====================================================================!
    !====================================================================!
    subroutine allocate_rArray_i1D(this, dims)
        class(rArray), intent(inout) :: this
          !! rArray class
        integer(i32), intent(in) :: dims(:)
          !! Allocate the values to the same shape defined by dims.
        call allocate(this%values, product(dims))
        call allocate(this%dims, size(dims))
        this%dims = dims
    end subroutine
    !====================================================================!


    !====================================================================!
    subroutine deallocate_rArray(this)
        class(rArray), intent(inout) :: this
          !! rArray class
        call deallocate(this%values)
        call deallocate(this%dims)
    end subroutine
    !====================================================================!

    !====================================================================!
    function exists_rArray(this) result(res)
      logical :: res
      class(rArray), intent(in) :: this

      res = .false.
      if (allocated(this%values)) then
        res = .true.
      endif
    end function
    !====================================================================!


    !====================================================================!
    ! subroutine get_real_0D(lhs, rhs)
    !   real(r32), allocatable, intent(inout) :: lhs
    !   class(rArray), intent(in) :: rhs
    !
    !   real, allocatable :: rtmp
    !
    !   if (.not. allocated(rtmp)) allocate(rtmp)
    !
    !   rtmp = rhs%values(1)
    !
    !   call move_alloc(rtmp, lhs)
    !   ! lhs = rhs%values(1)
    ! end subroutine
    !
    ! subroutine get_real_1D(lhs, rhs)
    !   real(r32), allocatable, intent(inout) :: lhs(:)
    !   class(rArray), intent(in) :: rhs
    !
    !   lhs = rhs%values
    ! end subroutine
    !
    ! subroutine get_real_2D(lhs, rhs)
    !   real(r32), allocatable, intent(inout) :: lhs(:, :)
    !   class(rArray), intent(in) :: rhs
    !
    !   lhs = reshape(rhs%values, shape(lhs))
    ! end subroutine


    !====================================================================!
    subroutine print_rArray(this, delim)
        class(rArray), intent(in) :: this
          !! rArray class
        character(len=*), intent(in), optional :: delim
          !! Delimiter between values

        write(output_unit, *) this%name, ": ", str(this%values, delim), " :dims= ", str(this%dims, delim)
        ! write(output_unit, '(a)') str(this%values, delim)
    end subroutine
    !====================================================================!

    !====================================================================!
    subroutine read_rArray(this, iUnit) !, fName)
        class(rArray), intent(inout) :: this
          !! rArray Class
        integer(i32), intent(in) :: iUnit
          !! Unit number to read from
        ! character(len=*), intent(in) :: fName
          ! Name of the file that was opened

        integer(i32) :: ii
        integer(i32) :: istat
        integer(i32) :: N ! number of values
        character(len=MAXFILE_LENGTH) :: filename

        read(iUnit, *) N
        read(iUnit, *)    ! Skip the datatype

        call this%allocate(N)

        do ii = 1, N
            read(iUnit, *, iostat=istat) this%values(ii)

            if (istat /= 0) then
              inquire(UNIT=iUnit, NAME=filename)
              write(output_unit, *) "ERROR: Reading from file: " // trim(filename)
              close(iUnit)
              stop
            endif
            ! call fErr(istat, fName, 2)
        enddo
    end subroutine
    !====================================================================!

    !====================================================================!
    function size_rArray(this) result(res)
      !! Get the size of the list of strings
        class(rArray), intent(in) :: this
          !! sArray Class
        integer(i32) :: res
          !! Size of the list

        res = size(this%values)
    end function
    !====================================================================!
end module
