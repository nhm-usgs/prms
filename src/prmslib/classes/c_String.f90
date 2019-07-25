module String_class
  use iso_fortran_env, only: output_unit
  use prms_constants, only: MAXFILE_LENGTH
  use variableKind
  use m_strings, only: str
  use m_errors, only: fErr
  implicit none

  private

  type, public :: String
    !! A container for an arbitrary length string.  Fortran does not allow an allocatable array of variable
    !! length strings, so this container must be used instead.
    character(len=:), allocatable :: s

  contains
    ! NOTE: ifort does not like 'generic, public' for UDIO
    procedure, public :: read => read_String
    !! Read the class from a file
    generic :: write(formatted) => write_str
    !! UDIO formatted write
    procedure, private :: write_str => write_String
  end type

contains
  ! !====================================================================!
  ! pure function is_equal(this, other)
  !     ! Returns true if the two keys are equal.
  !     implicit none

  !     class(String), intent(in) :: this
  !     character(len=*), intent(in) :: other
  !     ! character(len=*), intent(in) :: k2

  !     logical :: is_equal

  !     is_equal = (other == this%s)
  ! end function
  !====================================================================!

  !====================================================================!
  subroutine read_String(this, iUnit)
    class(String), intent(inout) ::this
    integer(i32), intent(in) :: iUnit

    ! Private variables
    character(len=1024) :: buffer
    integer(i32) :: istat
    character(len=MAXFILE_LENGTH) :: filename

    read(iUnit, '(a)', iostat=istat) buffer

    if (istat /= 0) then
      inquire(UNIT=iUnit, NAME=filename)
      write(output_unit, 9005) "ERROR: IOSTAT=", istat, "Reading from file:", trim(filename)
      ! write(output_unit, *) "ERROR: IOSTAT=" // istat // "Reading from file: " // trim(filename)
      close(iUnit)
      stop
    endif

    9005 format(a, 1x, i6, 1x, a, 1x, a)

    ! call fErr(istat, fName, 2)

    this%s = trim(buffer)
  end subroutine

  ! !====================================================================!
  ! subroutine read_String(dtv, unit, iotype, v_list, iostat, iomsg)
  !     class(String), intent(inout) ::dtv
  !     integer(i32), intent(in) :: unit
  !     character(len=*), intent(in) :: iotype
  !     integer(i32), intent(in) :: v_list(:)
  !     integer(i32), intent(out) :: iostat
  !     character(len=*), intent(inout) :: iomsg

  !     ! Private variables
  !     character(len=1024) :: buffer

  !     select case(iotype)
  !         case('LISTDIRECTED', 'DT')
  !             read(unit, *, IOSTAT=iostat, IOMSG=iomsg) buffer
  !             dtv%s = trim(buffer)
  !         case default
  !             ! Error
  !             iostat = 1
  !             iomsg = 'read_String: Unsupported iotype'
  !     end select
  ! end subroutine
  !====================================================================!

  !====================================================================!
  subroutine write_String(dtv, unit, iotype, v_list, iostat, iomsg)
    class(String), intent(in) :: dtv
    integer(i32), intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer(i32), intent(in) :: v_list(:)
    integer(i32), intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg

    select case(iotype)
    case('LISTDIRECTED', 'DT')
      write(unit, '(a)', IOSTAT=iostat, IOMSG=iomsg) str(dtv%s)
    case default
      ! Error
      iostat = 1
      iomsg = 'write_String: Unsupported iotype'
    end select
  end subroutine
  !====================================================================!

end module
