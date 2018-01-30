module m_str_arr
    use m_kinds, only: r4, r8, i4, i8
    implicit none

    private

    type, public :: str_arr_type
        character(len=:), allocatable :: str

    contains
        procedure, private :: read_str
        procedure, private :: write_str
        procedure :: is_equal
        ! NOTE: ifort does not like 'generic, public' for UDIO
        generic :: read(formatted) => read_str      ! UDIO formatted read
        generic :: write(formatted) => write_str    ! UDIO formatted write
    end type str_arr_type

contains
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Returns true if the two keys are equal.
    pure function is_equal(this, k1)
        implicit none

        class(str_arr_type), intent(in) :: this
        character(len=*), intent(in) :: k1
        ! character(len=*), intent(in) :: k2

        logical :: is_equal

        is_equal = .false.
        is_equal = k1 == this%str
    end function is_equal

    
    subroutine read_str(dtv, unit, iotype, v_list, iostat, iomsg)
        class(str_arr_type), intent(inout) ::dtv
        integer(i4), intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer(i4), intent(in) :: v_list(:)
        integer(i4), intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        ! Private variables
        ! character(len=1024) :: buffer

        if (iotype == 'LISTDIRECTED') then
            read(unit, *, IOSTAT=iostat, IOMSG=iomsg) dtv%str
        else
            ! Error
            iostat = 1
            iomsg = 'read_str: Unsupported iotype'
        endif
    end subroutine read_str

    subroutine write_str(dtv, unit, iotype, v_list, iostat, iomsg)
        class(str_arr_type), intent(in) :: dtv
        integer(i4), intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer(i4), intent(in) :: v_list(:)
        integer(i4), intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        ! character(len=80) :: buffer

        if (iotype == 'LISTDIRECTED') then
            write(unit, '(A, 1X)', IOSTAT=iostat, IOMSG=iomsg) dtv%str
        else
            ! Error
            iostat = 1
            iomsg = 'write_str: Unsupported iotype'
        endif
    end subroutine write_str


end module m_str_arr
