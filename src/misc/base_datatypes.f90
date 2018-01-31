module kinds_mod
    use, intrinsic :: iso_fortran_env, only: r4 => real32, r8 => real64, &
                                             i4 => int32, i8 => int64

    private
    public :: r4, r8, i4, i8
end module kinds_mod

module data_mod
    use kinds_mod, only: r4, r8, i4, i8
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

    type, abstract, public :: data_t
        private

        contains
            private
                ! procedure(Iset), pass(this), deferred :: set
                procedure(Iprint), pass(this), public, deferred :: print
                ! generic, public :: assignment(=) => set_t, get_int, get_real, get_dbl
    end type data_t

    abstract interface
        subroutine Iprint(this)
            import :: data_t

            class(data_t) :: this
        end subroutine Iprint

        ! subroutine Iset(this, newval)
        !     import :: data_t
        !     class(data_t), intent(inout) :: this
        !
        !     ! How do you define this so the implementation of the
        !     ! abstract interface could allow this parameter to
        !     ! be either array or scalar?
        !     class(*), intent(in) :: newval(:)
        ! end subroutine Iset

    end interface

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

end module data_mod


module arr_mod
    use kinds_mod, only: r4, r8, i4, i8
    use data_mod, only: data_t, str_arr_type
    implicit none

    private

    type, extends(data_t), public :: array_1D_t
        private
        class(*), public, allocatable :: values(:)

        contains
            private
            procedure, pass(this), public :: print => print_1D
            procedure, public :: exists
            procedure, pass(this) :: set_t
            procedure, pass(rhs) :: get_int
            procedure, pass(rhs) :: get_real
            procedure, pass(rhs) :: get_dbl
            procedure, pass(rhs) :: get_str
            procedure, pass(rhs) :: get_int_scalar
            procedure, pass(rhs) :: get_real_scalar
            procedure, pass(rhs) :: get_dbl_scalar
            procedure, pass(rhs) :: get_str_scalar
            procedure, pass(rhs) :: get_char_scalar

            generic, public :: assignment(=) => set_t, get_int, get_real, &
                                                get_dbl, get_str, &
                                                get_int_scalar, get_real_scalar, &
                                                get_dbl_scalar, get_str_scalar, &
                                                get_char_scalar

            procedure, pass(this), public :: ival => fget_t_int
            procedure, pass(this), public :: rval => fget_t_real
            procedure, pass(this), public :: dval => fget_t_dbl
            procedure, pass(this), public :: strval => fget_t_str
            procedure, pass(this), public :: ival_scalar => fget_t_int_scalar
            procedure, pass(this), public :: rval_scalar => fget_t_real_scalar
            procedure, pass(this), public :: dval_scalar => fget_t_dbl_scalar
            procedure, pass(this), public :: strval_scalar => fget_t_str_scalar
            procedure, pass(this), public :: charval_scalar => fget_t_char_scalar

            procedure, pass(this), public :: length
    end type array_1D_t

    contains
        function exists(this, key)
            implicit none

            class(array_1D_t), intent(inout) :: this
            ! type(str_arr_type), intent(in) :: key
            character(len=*), intent(in) :: key

            ! Private variables
            logical :: exists
            integer(i4) :: ii

            exists = .false.

            select type(v => this%values)
                type is (integer)
                    print *, v
                type is (real)
                    print *, v
                type is (real(8))
                    print *, v
                class is (str_arr_type)
                    ! print *, v
                    do ii = 1, size(v)
                        exists = v(ii)%is_equal(key)
                        if (exists) exit
                        ! print *, v(ii)%str
                    enddo
                class default
            end select
        end function exists

        subroutine get_int(lhs, rhs)
            integer(i4), allocatable, intent(out) :: lhs(:)
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%ival()
        end subroutine get_int

        subroutine get_real(lhs, rhs)
            real(r4), allocatable, intent(out) :: lhs(:)
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%rval()
        end subroutine get_real

        subroutine get_dbl(lhs, rhs)
            real(r8), allocatable, intent(out) :: lhs(:)
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%dval()
        end subroutine get_dbl

        subroutine get_str(lhs, rhs)
            type(str_arr_type), allocatable, intent(out) :: lhs(:)
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%strval()
        end subroutine get_str

        subroutine get_int_scalar(lhs, rhs)
            integer(i4), intent(out) :: lhs
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%ival_scalar()
        end subroutine get_int_scalar

        subroutine get_real_scalar(lhs, rhs)
            real(r4), intent(out) :: lhs
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%rval_scalar()
        end subroutine get_real_scalar

        subroutine get_dbl_scalar(lhs, rhs)
            real(r8), intent(out) :: lhs
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%dval_scalar()
        end subroutine get_dbl_scalar

        subroutine get_str_scalar(lhs, rhs)
            type(str_arr_type), allocatable, intent(out) :: lhs
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%strval_scalar()
        end subroutine get_str_scalar

        subroutine get_char_scalar(lhs, rhs)
            character(:), allocatable, intent(inout) :: lhs
            class(array_1D_t), intent(in) :: rhs

            lhs = rhs%charval_scalar()
        end subroutine get_char_scalar

        subroutine set_t(this, newval)
            class(array_1D_t), intent(inout) :: this
            class(*), intent(in) :: newval(:)

            if (allocated(this%values)) deallocate(this%values)

            select type (newval)
                type is (integer)
                    allocate(this%values, source=newval)
                type is (real)
                    allocate(this%values, source=newval)
                type is (real(8))
                    allocate(this%values, source=newval)
                class is (str_arr_type)
                    allocate(this%values, source=newval)
                class default
                    print *, '*** ERROR *** type not found!'
            end select
        end subroutine set_t

        subroutine print_1D(this)
            class(array_1D_t) :: this

            select type(v => this%values)
                type is (integer)
                    print *, v
                type is (real)
                    print *, v
                type is (real(8))
                    print *, v
                class is (str_arr_type)
                    print *, v
                    ! do ii = 1, size(v)
                    !     print *, v(ii)%str
                    ! enddo
                class default
            end select
        end subroutine print_1D

        function fget_t_int(this) result(ival)
            class(array_1D_t), intent(in) :: this
            integer(i4), allocatable :: ival(:)

            if (.not. allocated(ival)) allocate(ival(size(this%values)))

            select type (dat => this%values)
                type is (integer)
                    ival = dat
                type is (real)
                    ival = int(dat)
                type is (real(8))
                    ival = int(dat)
                class default
            end select
        end function fget_t_int


        function fget_t_real(this) result(rval)
            class(array_1D_t), intent(in) :: this
            real(r4), allocatable :: rval(:)

            if (.not. allocated(rval)) allocate(rval(size(this%values)))

            select type (dat => this%values)
                type is (integer)
                    rval = real(dat)
                type is (real)
                    rval = dat
                type is (real(8))
                    rval = real(dat)
                class default
            end select
        end function fget_t_real

        function fget_t_dbl(this) result(dval)
            class(array_1D_t), intent(in) :: this
            real(r8), allocatable :: dval(:)

            if (.not. allocated(dval)) allocate(dval(size(this%values)))

            select type (dat => this%values)
                type is (integer)
                    dval = dble(dat)
                type is (real)
                    dval = dble(dat)
                type is (real(8))
                    dval = dat
                class default
            end select
        end function fget_t_dbl

        function fget_t_str(this) result(strval)
            class(array_1D_t), intent(in) :: this
            type(str_arr_type), allocatable :: strval(:)

            if (.not. allocated(strval)) allocate(strval(size(this%values)))

            select type (dat => this%values)
                type is (integer)
                    print *, 'ERROR: String to integer not implemented!'
                    ! strval = dble(dat)
                type is (real)
                    print *, 'ERROR: String to real not implemented!'
                    ! strval = dble(dat)
                type is (real(8))
                    print *, 'ERROR: String to double not implemented!'
                    ! strval = dat
                type is (str_arr_type)
                    strval = dat
                class default
            end select
        end function fget_t_str

        function fget_t_int_scalar(this) result(ival_scalar)
            class(array_1D_t), intent(in) :: this
            integer(i4) :: ival_scalar

            select type (dat => this%values)
                type is (integer)
                    ival_scalar = dat(1)
                type is (real)
                    ival_scalar = int(dat(1))
                type is (real(8))
                    ival_scalar = int(dat(1))
                class default
            end select
        end function fget_t_int_scalar

        function fget_t_real_scalar(this) result(rval_scalar)
            class(array_1D_t), intent(in) :: this
            real(r4) :: rval_scalar

            select type (dat => this%values)
                type is (integer)
                    rval_scalar = real(dat(1))
                type is (real)
                    rval_scalar = dat(1)
                type is (real(8))
                    rval_scalar = real(dat(1))
                class default
            end select
        end function fget_t_real_scalar

        function fget_t_dbl_scalar(this) result(dval_scalar)
            class(array_1D_t), intent(in) :: this
            real(r8), allocatable :: dval_scalar

            select type (dat => this%values)
                type is (integer)
                    dval_scalar = dble(dat(1))
                type is (real)
                    dval_scalar = dble(dat(1))
                type is (real(8))
                    dval_scalar = dat(1)
                class default
            end select
        end function fget_t_dbl_scalar

        function fget_t_str_scalar(this) result(strval_scalar)
            class(array_1D_t), intent(in) :: this
            type(str_arr_type), allocatable :: strval_scalar

            ! if (.not. allocated(strval)) allocate(strval(size(this%values)))

            select type (dat => this%values)
                type is (integer)
                    print *, 'ERROR: String to integer not implemented!'
                    ! strval = dble(dat)
                type is (real)
                    print *, 'ERROR: String to real not implemented!'
                    ! strval = dble(dat)
                type is (real(8))
                    print *, 'ERROR: String to double not implemented!'
                    ! strval = dat
                type is (str_arr_type)
                    strval_scalar = dat(1)
                class default
            end select
        end function fget_t_str_scalar

        function fget_t_char_scalar(this) result(charval_scalar)
            class(array_1D_t), intent(in) :: this
            character(:), allocatable :: charval_scalar

            ! if (.not. allocated(strval)) allocate(strval(size(this%values)))

            select type (dat => this%values)
                type is (integer)
                    print *, 'ERROR: String to integer not implemented!'
                    ! strval = dble(dat)
                type is (real)
                    print *, 'ERROR: String to real not implemented!'
                    ! strval = dble(dat)
                type is (real(8))
                    print *, 'ERROR: String to double not implemented!'
                    ! strval = dat
                type is (str_arr_type)
                    charval_scalar = dat(1)%str
                class default
            end select
        end function fget_t_char_scalar

        function length(this)
            ! Return the number of elements in the value array
            implicit none

            class(array_1D_t), intent(in) :: this
            integer(i4) :: length

            length = size(this%values)
        end function length
end module arr_mod
