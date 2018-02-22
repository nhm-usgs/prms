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
    use variableKind
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
        ! procedure, public :: read
    end type

    ! interface rVariable
    !     module procedure constructor
    ! end interface
contains
    ! subroutine read(this, iunit)
    !     use iso_fortran_env
    !     implicit none
    !
    !     class(rVariable), intent(in) :: this
    !     integer(i32), intent(in) :: iunit
    !
    !     integer(r32) :: ios
    !     character(len=256) :: buf
    !     ! character(len=16) :: string
    !     character(len=32) :: paramstring
    !     character(len=12) :: dim_string(2)
    !     ! integer(i32) :: iunit
    !         ! File IO unit for opened parameter file
    !
    !     ! integer(i32) :: ios
    !     integer(i32) :: num_dims
    !     integer(i32) :: i
    !     integer(i32) :: num
    !     integer(i32) :: inum
    !
    !
    !     ! ~~~~~~~~~~~~~~~~~~~~
    !     ! Number of dimensions
    !     read (iunit, *, IOSTAT=ios) num_dims
    !     if (ios /= 0) call read_error(11, 'invalid number of dimensions: ' // trim(paramstring))
    !     if (num_dims > 2) call read_error(11, 'number of dimensions > 3: ' // trim(paramstring))
    !
    !     ! ~~~~~~~~~~~~~~~~~~~~
    !     ! Dimension names
    !     num = 1
    !     do i = 1, num_dims
    !         read (iunit, '(A)', IOSTAT = ios) dim_string(i)
    !         if (ios /= 0) call read_error(11, 'invalid dimension for parameter: ' // trim(paramstring))
    !
    !         ! inum = getdim(dim_string(i))
    !         ! call dim_data%get_data(dim_string(i), inum)
    !         if (inum == IOSTAT_END) call read_error(11, TRIM(dim_string(i)))
    !         num = num * inum
    !     enddo
    !
    ! end subroutine
end module
