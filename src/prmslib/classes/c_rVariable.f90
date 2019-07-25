module rVariable_class
    !!# Variable Class (single precision real)
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
    use sArray_class, only: sArray
    use rArray_class, only: rArray


    implicit none

    private

    public :: rVariable

    type, extends(rArray) :: rVariable
        real(r32) :: min_value
        real(r32) :: max_value
        real(r32) :: default_value

        type(sArray) :: dim_names
        type(sArray) :: module_names

    contains
        procedure, public, pass(this) :: read => read_rVariable
          !! Read the class from a file

        ! procedure, private, pass(rhs) :: get_real_0D
        ! procedure, private, pass(rhs) :: get_real_1D
        ! procedure, private, pass(rhs) :: get_real_2D
        ! procedure, pass(this), public :: rval => fget_t_real

        ! generic, public :: assignment(=) => get_real_0D, get_real_1D, get_real_2D
    end type

contains
  !====================================================================!
  subroutine read_rVariable(this, iUnit)
      class(rVariable), intent(inout) :: this
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

  !====================================================================!
  ! subroutine get_real_0D(lhs, rhs)
  !   real(r32), intent(inout) :: lhs
  !   class(rVariable), intent(in) :: rhs
  !
  !   lhs = rhs%values(1)
  ! end subroutine
  !
  ! subroutine get_real_1D(lhs, rhs)
  !   real(r32), intent(inout) :: lhs(:)
  !   class(rVariable), intent(in) :: rhs
  !
  !   if (rhs%size() == 1) then
  !     lhs = reshape(rhs%values(:), shape(lhs), PAD=rhs%values(:))
  !   else
  !     lhs = rhs%values(:)
  !   endif
  ! end subroutine
  !
  ! subroutine get_real_2D(lhs, rhs)
  !   real(r32), intent(inout) :: lhs(:, :)
  !   class(rVariable), intent(in) :: rhs
  !
  !   lhs = reshape(rhs%values(:), shape(lhs), PAD=rhs%values(:))
  ! end subroutine

end module
