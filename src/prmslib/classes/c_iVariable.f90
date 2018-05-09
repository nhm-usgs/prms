module iVariable_class
    !!# Variable Class (integer)
    !!
    !!
    use iso_fortran_env, only: output_unit
    use variableKind
    use prms_constants, only: MAXFILE_LENGTH
    use sArray_class, only: sArray
    use iArray_class, only: iArray
    implicit none

    private

    public :: iVariable

    type, extends(iArray) :: iVariable
        integer(i32) :: min_value
        integer(i32) :: max_value
        ! integer(i32) :: default_value

        type(sArray) :: dim_names
        type(sArray) :: module_names

    contains
        procedure, public, pass(this) :: read => read_iVariable
          !! Read the class from a file
    end type

contains
  !====================================================================!
  subroutine read_iVariable(this, iUnit)
      class(iVariable), intent(inout) :: this
        !! iArray Class
      integer(i32), intent(in) :: iUnit
        !! Unit number to read from

      integer(i32) :: ii
      integer(i32) :: istat
      integer(i32) :: N ! number of values
      character(len=MAXFILE_LENGTH) :: filename

      ! Read the dimension names
      call this%dim_names%read(Iunit, has_datatype=.false.)

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

end module
