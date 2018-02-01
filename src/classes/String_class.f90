module String_class
    use variableKind
    use m_strings, only: str
    implicit none

    private

    type, public :: String
        character(len=:), allocatable :: s

    contains
        procedure :: is_equal

        ! NOTE: ifort does not like 'generic, public' for UDIO
        generic :: read(formatted) => read_str
            !! UDIO formatted read
        generic :: write(formatted) => write_str
            !! UDIO formatted write
        procedure, private :: read_str
        procedure, private :: write_str
    end type

contains
    !====================================================================!
    pure function is_equal(this, k1)
        ! Returns true if the two keys are equal.
        implicit none

        class(String), intent(in) :: this
        character(len=*), intent(in) :: k1
        ! character(len=*), intent(in) :: k2

        logical :: is_equal

        is_equal = .false.
        is_equal = k1 == this%s
    end function is_equal

    !====================================================================!
    subroutine read_str(dtv, unit, iotype, v_list, iostat, iomsg)
        class(String), intent(inout) ::dtv
        integer(i32), intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer(i32), intent(in) :: v_list(:)
        integer(i32), intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        ! Private variables
        ! character(len=1024) :: buffer

        if (iotype == 'LISTDIRECTED') then
            read(unit, *, IOSTAT=iostat, IOMSG=iomsg) dtv%s
        else
            ! Error
            iostat = 1
            iomsg = 'read_str: Unsupported iotype'
        endif
    end subroutine

    !====================================================================!
    subroutine write_str(dtv, unit, iotype, v_list, iostat, iomsg)
        class(String), intent(in) :: dtv
        integer(i32), intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer(i32), intent(in) :: v_list(:)
        integer(i32), intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        ! character(len=80) :: buffer

        if (iotype == 'LISTDIRECTED') then
            write(unit, '(a)', IOSTAT=iostat, IOMSG=iomsg) str(dtv%s)
        else
            ! Error
            iostat = 1
            iomsg = 'write_str: Unsupported iotype'
        endif
    end subroutine

end module
