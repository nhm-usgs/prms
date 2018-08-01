module iScalar_class
  !!
  !!
  !!

  use iso_fortran_env, only: output_unit
  use prms_constants, only: MAXFILE_LENGTH
  use variableKind, only: i32
  use m_errors, only: fErr, IO_READ
  use Abc_class, only: Abc
  use m_strings, only: str
  implicit none

  private
  public :: iScalar

  type, extends(Abc) :: iScalar
      !! 1D array of reals that can represent multiple dimensional data
    integer(i32) :: value
      !! The scalar value

    contains
      procedure, public, pass(this) :: print => print_iScalar
        !! Print the class to the screen
      procedure, public, pass(this) :: read => read_iScalar
        !! Read the class from file

      ! procedure, private, pass(rhs) :: get_int
      ! generic, public :: assignment(=) => get_int

    end type

  interface iScalar
      !! Overloaded interface to instantiate the class.
    module procedure :: constructor_iScalar
  end interface

  contains
    !====================================================================!
    function constructor_iScalar(value) result(this)
      type(iScalar) :: this
        !! iScalar class
      integer(i32), optional, intent(in) :: value
        !! Value to assign

      if (present(value)) then
        this%value = value
      else
        this%value = 0
      endif
    end function
    !====================================================================!

    !====================================================================!
    subroutine print_iScalar(this, delim)
      class(iScalar), intent(in) :: this
        !! iScalar class
      character(len=*), intent(in), optional :: delim
        !! Delimiter between values

      ! write(output_unit, *) str(this%values, delim), " :dims= ", str(this%dims, delim)
      write(output_unit, *) str(this%value)
    end subroutine
    !====================================================================!

    !====================================================================!
    subroutine read_iScalar(this, iUnit)
      class(iScalar), intent(inout) :: this
        !! iScalar Class
      integer(i32), intent(in) :: iUnit
        !! Unit number to read from

      ! integer(i32) :: ii  ! counter
      integer(i32) :: istat  ! iostat result from read
      integer(i32) :: numvals  ! Number of values to read
      character(len=MAXFILE_LENGTH) :: filename

      ! Read number of values the parameter should have and allocate
      read(iUnit, *) numvals
      read(iUnit, *)

      if (numvals > 1) then
        write(output_unit, *) "ERROR: Variable ", this%name, ", is not a scalar."
        close(iUnit)
        stop
      endif

      read(iUnit, *, iostat=istat) this%value

      if (istat /= 0) then
        inquire(UNIT=iUnit, NAME=filename)
        write(output_unit, 9005) "ERROR: IOSTAT=", istat, "Reading from file:", trim(filename)
        ! write(output_unit, *) "ERROR: IOSTAT=" // istat // "Reading from file: " // trim(filename)
        close(iUnit)
        stop
      endif

      9005 format(a, 1x, i6, 1x, a, 1x, a)
    end subroutine
    !====================================================================!


    ! subroutine get_int(lhs, rhs)
    !   integer(i32), intent(inout) :: lhs
    !   class(iScalar), intent(in) :: rhs
    !
    !   lhs = int(rhs%value, kind=4)
    ! end subroutine

end module
