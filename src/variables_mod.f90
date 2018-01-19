MODULE variables_mod
    use kinds_mod, only: r4, r8, i4, i8
    use data_mod, only: str_arr_type
    implicit none

    !    private
    type PRMS_variable
        character(len=:), allocatable :: variable_name
        character(len=:), allocatable :: description
        integer(i4) :: numvals
        integer(i4) :: data_flag
        integer(i4) :: decl_flag
        integer(i4) :: get_flag
        ! integer(i4) :: var_name_nchars
        integer(i4) :: id_num
        character(len=:), allocatable :: data_type, dimen_names, module_name, units
        integer(i4), pointer :: values_int(:)
        real(r4), pointer :: values_real(:)
        real(r8), pointer :: values_dble(:)
    end type PRMS_variable
end MODULE variables_mod



module variables_arr_mod
    use kinds_mod, only: r4, r8, i4, i8
    use variables_mod, only: PRMS_variable
    implicit none

    type variables_arr_t
        type(PRMS_variable), allocatable :: Variable_data(:)
        integer(i4) :: Num_variables
    contains
        private
        procedure :: declvar
        procedure :: declvar_dble_0D
        procedure :: declvar_dble_1D
        procedure :: declvar_dble_2D
        procedure :: declvar_int_0D
        procedure :: declvar_int_1D
        procedure :: declvar_real_0D
        procedure :: declvar_real_1D
        procedure :: find_variable
        procedure :: getvar
        procedure, public :: getvarnvals
        procedure, public :: getvarsize
        procedure, public :: getvartype
        procedure, public :: getvar_dble
        procedure, public :: getvar_id
        procedure, public :: getvar_real
        generic, public :: declvar_dble => declvar_dble_0D, declvar_dble_1D, declvar_dble_2D
        generic, public :: declvar_int => declvar_int_0D, declvar_int_1D
        generic, public :: declvar_real => declvar_real_0D, declvar_real_1D
    end type variables_arr_t

    interface variables_arr_t
        procedure :: init
    end interface variables_arr_t

contains
    function init()
        use prms_constants, only: MAXVARIABLES
        implicit none
        type(variables_arr_t) :: init

        integer(i4) :: ii

        allocate(init%Variable_data(MAXVARIABLES))

        init%Num_variables = 0
    end function init


    !***********************************************************************
    ! declvar - set up memory for variables
    !***********************************************************************
    subroutine declvar(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)
        use UTILS_PRMS, only: set_data_type  ! ,numchars
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer, intent(in) :: Numvalues

        ! Local Variables
        integer(i4) type_flag

        this%Num_variables = this%Num_variables + 1
        if (this%Num_variables > 400) STOP 'PRMS ERROR, maximum number of declared variables (400) exceeded'

        this%Variable_data(this%Num_variables)%get_flag = 0
        this%Variable_data(this%Num_variables)%decl_flag = 1
        this%Variable_data(this%Num_variables)%variable_name = Varname
        ! this%Variable_data(this%Num_variables)%var_name_nchars = numchars(Varname)
        this%Variable_data(this%Num_variables)%description = Desc
        this%Variable_data(this%Num_variables)%units = Units
        this%Variable_data(this%Num_variables)%dimen_names = Dimenname
        this%Variable_data(this%Num_variables)%module_name = Modname
        this%Variable_data(this%Num_variables)%numvals = Numvalues
        this%Variable_data(this%Num_variables)%data_type = Data_type
        this%Variable_data(this%Num_variables)%id_num = this%Num_variables
        call set_data_type(Data_type, type_flag)

        if (type_flag < 1 .OR. type_flag > 3) then
            print *, 'ERROR, data type not implemented: ', Data_type, ' Variable: ', Varname
                    ! &           Varname(:this%Variable_data(this%Num_variables)%var_name_nchars)
            STOP
        endif

        this%Variable_data(this%Num_variables)%data_flag = type_flag
    end subroutine declvar

    !***********************************************************************
    ! declvar_dble_0D - set up memory for double precision variables
    ! 2017-11-08 PAN: This doesn't work correctly using a scalar target
    !                 for an array pointer
    !***********************************************************************
    subroutine declvar_dble_0D(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer(i4), intent(in) :: Numvalues
        real(r8), target :: Values

        !***********************************************************************
        call this%declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

        allocate (this%Variable_data(this%Num_variables)%values_dble(Numvalues))
        this%Variable_data(this%Num_variables)%values_dble(1) = Values
    end subroutine declvar_dble_0D

    !***********************************************************************
    ! declvar_dble_1D - set up memory for double precision variables
    !***********************************************************************
    subroutine declvar_dble_1D(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer(i4), intent(in) :: Numvalues
        real(r8), TARGET :: Values(*)

        !***********************************************************************
        call this%declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

        allocate (this%Variable_data(this%Num_variables)%values_dble(Numvalues))
        this%Variable_data(this%Num_variables)%values_dble => Values(:Numvalues)
    end subroutine declvar_dble_1D

    !***********************************************************************
    ! declvar_dble_2D - set up memory for double precision variables
    !***********************************************************************
    subroutine declvar_dble_2D(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer(i4), intent(in) :: Numvalues
        real(r8), TARGET, contiguous :: Values(:, :)

        !***********************************************************************
        call this%declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

        allocate (this%Variable_data(this%Num_variables)%values_dble(Numvalues))
        this%Variable_data(this%Num_variables)%values_dble(1:Numvalues) => Values(:, :)
    end subroutine declvar_dble_2D

    !***********************************************************************
    ! declvar_int_0D - set up memory for integer variables
    !***********************************************************************
    subroutine declvar_int_0D(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer(i4), intent(in) :: Numvalues
        integer(i4), target :: Values

        !***********************************************************************
        call this%declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

        allocate (this%Variable_data(this%Num_variables)%values_int(Numvalues))
        this%Variable_data(this%Num_variables)%values_int(1) = Values
    end subroutine declvar_int_0D

    !***********************************************************************
    ! declvar_int - set up memory for integer variables
    !***********************************************************************
    subroutine declvar_int_1D(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer(i4), intent(in) :: Numvalues
        integer(i4), TARGET :: Values(*)

        !***********************************************************************
        call this%declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)
        allocate (this%Variable_data(this%Num_variables)%values_int(Numvalues))
        this%Variable_data(this%Num_variables)%values_int => Values(:Numvalues)
    end subroutine declvar_int_1D

    !***********************************************************************
    ! declvar_real - set up memory for real variables
    !***********************************************************************
    subroutine declvar_real_0D(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer(i4), intent(in) :: Numvalues
        real(r4), TARGET :: Values

        !***********************************************************************
        call this%declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

        allocate (this%Variable_data(this%Num_variables)%values_real(Numvalues))
        this%Variable_data(this%Num_variables)%values_real(1) = Values
    end subroutine declvar_real_0D

    !***********************************************************************
    ! declvar_real_1D - set up memory for real variables
    !***********************************************************************
    subroutine declvar_real_1D(this, Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Data_type
        character(len=*), intent(in) :: Desc
        character(len=*), intent(in) :: Units
        integer(i4), intent(in) :: Numvalues
        real(r4), TARGET :: Values(*)

        !***********************************************************************
        call this%declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)
        allocate (this%Variable_data(this%Num_variables)%values_real(Numvalues))
        this%Variable_data(this%Num_variables)%values_real => Values(:Numvalues)
    end subroutine declvar_real_1D

    !***********************************************************************
    ! find_variable - find variable in data structure
    !***********************************************************************
    integer function find_variable(this, Modname, Varname, Numvalues, Data_type)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: found, i, ierr

        !***********************************************************************
        ierr = 0
        found = 0
        find_variable = 1
        do i = 1, this%Num_variables
            if (Varname == this%Variable_data(i)%variable_name) then
                found = 1
                if (this%Variable_data(i)%numvals /= Numvalues) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Variable: ', Varname, &
                            &               ' number of values in getvar does not match declared number of values'
                endif
                if (this%Variable_data(i)%data_type /= Data_type) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Variable: ', Varname, &
                            ' data type does in getvar not match declared data type'
                endif
                find_variable = i
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR in: ', Modname, ', Variable: ', Varname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP
    end function find_variable

    !***********************************************************************
    ! getvar - get variable values
    !***********************************************************************
    integer function getvar(this, Modname, Varname, Numvalues, Data_type, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname   ! Module name
        character(len=*), intent(in) :: Varname     ! Variable name
        character(len=*), intent(in) :: Data_type   ! Datatype (integer, real, double)
        integer(i4), intent(in) :: Numvalues        ! Number of expected values

        ! values could be any data type
        real(r4), intent(out) :: Values(Numvalues)

        ! Local Variables
        integer(i4) :: var_id, var_type
        integer(i4), allocatable :: itemp(:)
        real(r4), allocatable :: temp(:)
        real(r8), allocatable :: dtemp(:)

        !***********************************************************************
        var_id = this%find_variable(Modname, Varname, Numvalues, Data_type)
        var_type = this%Variable_data(var_id)%data_flag

        if (var_type == 1) then
            allocate (itemp(Numvalues))
            itemp = this%Variable_data(var_id)%values_int
            Values = transfer(itemp, Values)
            deallocate (itemp)
        elseif (var_type == 2) then
            allocate (temp(Numvalues))
            temp = this%Variable_data(var_id)%values_real
            Values = transfer(temp, Values)
            deallocate (temp)
        elseif (var_type == 3) then
            allocate (dtemp(Numvalues))
            dtemp = this%Variable_data(var_id)%values_dble
            Values = transfer(dtemp, Values)
            deallocate (dtemp)
        endif

        getvar = 0
    end function getvar

    !***********************************************************************
    ! getvarnvals - get variable number of values
    !***********************************************************************
    integer function getvarnvals(this, Varname)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Varname

        ! Local Variables
        integer(i4) :: i

        !***********************************************************************
        getvarnvals = 1
        do i = 1, this%Num_variables
            if (Varname == this%Variable_data(i)%variable_name) then
                getvarnvals = this%Variable_data(i)%numvals
                return
            endif
        enddo
        print *, 'ERROR in: getvarnvals, Variable: ', Varname, ' not declared'
        STOP
    end function getvarnvals

    !***********************************************************************
    ! getvarsize - return the number of values for a parameter
    !***********************************************************************
    integer function getvarsize(this, Varname)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Varname

        ! Local Variables
        integer(i4) :: found, i

        !***********************************************************************
        found = 0
        do i = 1, this%Num_variables
            if (Varname == this%Variable_data(i)%variable_name) then
                found = i
                getvarsize = this%Variable_data(i)%numvals
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR, Variable: ', Varname, ' not declared'
            STOP
        endif

    end function getvarsize

    !***********************************************************************
    ! getvartype - get variable type
    !***********************************************************************
    integer function getvartype(this, Varname)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Varname

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer :: i

        !***********************************************************************
        getvartype = 1
        do i = 1, this%Num_variables
            if (Varname == this%Variable_data(i)%variable_name) then
                getvartype = this%Variable_data(i)%data_flag
                ! getvartype = getvartype
                return
            endif
        enddo
        print *, 'ERROR variable: ', Varname, ' not available'
        STOP
    end function getvartype

    !***********************************************************************
    ! getvar_dble - get double precision variable values
    !***********************************************************************
    subroutine getvar_dble(this, Modname, Varname, Numvalues, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        integer(i4), intent(in) :: Numvalues
        real(r8), intent(out) :: Values(Numvalues)

        ! Local Variables
        integer(i4) :: var_id

        !***********************************************************************
        var_id = this%find_variable(Modname, Varname, Numvalues, 'double')
        Values = this%Variable_data(var_id)%values_dble
    end subroutine getvar_dble

    !***********************************************************************
    ! getvar_id - get variable index
    !***********************************************************************
    integer function getvar_id(this, Varname)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Varname

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: i

        !***********************************************************************
        getvar_id = 1
        do i = 1, this%Num_variables
            if (Varname == this%Variable_data(i)%variable_name) then
                getvar_id = this%Variable_data(i)%id_num
                return
            endif
        enddo
        print *, 'ERROR variable: ', Varname, ' not available'
        STOP
    end function getvar_id

    !***********************************************************************
    ! getvar_real - get single precision variable values
    !***********************************************************************
    subroutine getvar_real(this, Modname, Varname, Numvalues, Values)
        implicit none

        ! Arguments
        class(variables_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Varname
        integer(i4), intent(in) :: Numvalues
        real(r4), intent(out) :: Values(Numvalues)

        ! Local Variables
        integer(i4) :: var_id

        !***********************************************************************
        var_id = this%find_variable(Modname, Varname, Numvalues, 'real')
        Values = this%Variable_data(var_id)%values_real
    end subroutine getvar_real
end module variables_arr_mod
