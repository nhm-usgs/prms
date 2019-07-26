module sVariable_class
  !!# Variable Class (array of strings)
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
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH
  use String_class, only: String
  use sArray_class, only: sArray
  implicit none

  private
  public :: sVariable

  type, extends(sArray) :: sVariable
    ! real(r32) :: min_value
    ! real(r32) :: max_value
    type(String) :: default_value

    type(sArray) :: dim_names
    type(sArray) :: module_names

    contains
      procedure, public, pass(this) :: read => read_sVariable
        !! Read the class from a file
  end type

  ! interface sVariable
  !     module procedure constructor
  ! end interface
  contains
    !====================================================================!
    subroutine read_sVariable(this, iUnit, has_datatype)
      class(sVariable), intent(inout) :: this
        !! sVariable Class
      integer(i32), intent(in) :: iUnit
        !! Unit number to read from
      logical, optional, intent(in) :: has_datatype

      integer(i32) :: ii
      integer(i32) :: istat
      integer(i32) :: N ! number of values
      ! character(len=MAXFILE_LENGTH) :: filename

      ! Read the dimension names
      call this%dim_names%read(Iunit, has_datatype=.false.)

      read(iUnit, *) N
      read(iUnit, *)    ! Skip the datatype

      call this%allocate(N)

      do ii = 1, N
        call this%values(ii)%read(iUnit)
      enddo
    end subroutine
end module
