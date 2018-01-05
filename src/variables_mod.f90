MODULE variables_mod
    use kinds_mod, only: r4, r8, i4, i8
    use data_mod, only: str_arr_type
    implicit none

    !    private
    !    public

    integer(i4), save :: Num_variables

    type PRMS_variable
        character(len=:), allocatable :: variable_name
        character(len=:), allocatable :: description
        integer(i4) :: numvals, data_flag, decl_flag, get_flag, var_name_nchars, id_num
        character(len=:), allocatable :: data_type, dimen_names, module_name, units
        integer(i4), pointer :: values_int(:)
        real(r4), pointer :: values_real(:)
        real(r8), pointer :: values_dble(:)
    end type PRMS_variable
    type (PRMS_variable), save, allocatable :: Variable_data(:)

    interface declvar_dble
        module procedure declvar_dble_0D
        module procedure declvar_dble_1D
        module procedure declvar_dble_2D
    end interface declvar_dble

    interface declvar_int
        module procedure declvar_int_0D
        module procedure declvar_int_1D
    end interface declvar_int

    interface declvar_real
        module procedure declvar_real_0D
        module procedure declvar_real_1D
    end interface declvar_real

    contains
        !***********************************************************************
        ! declvar - set up memory for variables
        !***********************************************************************
        subroutine declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)
            use UTILS_PRMS, only: numchars, set_data_type
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer, intent(in) :: Numvalues

            ! Local Variables
            integer(i4) type_flag
            integer(i4), save :: init
            DATA init/0/

            !***********************************************************************
            if (init == 0) then
                init = 1
                Num_variables = 0
                allocate (Variable_data(400)) ! don't know how many, need to read var_name file
            endif
            ! need to declare parameters first, but don't know how many, know how many in Parameter File
            Num_variables = Num_variables + 1
            if (Num_variables > 400) STOP 'PRMS ERROR, maximum number of declared variables (400) exceeded'

            Variable_data(Num_variables)%get_flag = 0
            Variable_data(Num_variables)%decl_flag = 1
            Variable_data(Num_variables)%variable_name = Varname
            Variable_data(Num_variables)%var_name_nchars = numchars(Varname)
            Variable_data(Num_variables)%description = Desc
            Variable_data(Num_variables)%units = Units
            Variable_data(Num_variables)%dimen_names = Dimenname
            Variable_data(Num_variables)%module_name = Modname
            Variable_data(Num_variables)%numvals = Numvalues
            Variable_data(Num_variables)%data_type = Data_type
            Variable_data(Num_variables)%id_num = Num_variables
            call set_data_type(Data_type, type_flag)

            if (type_flag < 1 .OR. type_flag > 3) then
                print *, 'ERROR, data type not implemented: ', Data_type, ' Variable: ', &
                        &           Varname(:Variable_data(Num_variables)%var_name_nchars)
                STOP
            endif
            Variable_data(Num_variables)%data_flag = type_flag
        end subroutine declvar

        !***********************************************************************
        ! declvar_dble_0D - set up memory for double precision variables
        ! 2017-11-08 PAN: This doesn't work correctly using a scalar target
        !                 for an array pointer
        !***********************************************************************
        subroutine declvar_dble_0D(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer(i4), intent(in) :: Numvalues
            real(r8), target :: Values

            !***********************************************************************
            call declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

            allocate (Variable_data(Num_variables)%values_dble(Numvalues))
            Variable_data(Num_variables)%values_dble(1) = Values
        end subroutine declvar_dble_0D

        !***********************************************************************
        ! declvar_dble_1D - set up memory for double precision variables
        !***********************************************************************
        subroutine declvar_dble_1D(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
            implicit none

            ! Arguments
            character(len = *), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer(i4), intent(in) :: Numvalues
            real(r8), TARGET :: Values(*)

            !***********************************************************************
            call declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

            allocate (Variable_data(Num_variables)%values_dble(Numvalues))
            Variable_data(Num_variables)%values_dble => Values(:Numvalues)
        end subroutine declvar_dble_1D

        !***********************************************************************
        ! declvar_dble_2D - set up memory for double precision variables
        !***********************************************************************
        subroutine declvar_dble_2D(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer(i4), intent(in) :: Numvalues
            real(r8), TARGET, contiguous :: Values(:, :)

            !***********************************************************************
            call declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

            allocate (Variable_data(Num_variables)%values_dble(Numvalues))
            Variable_data(Num_variables)%values_dble(1:Numvalues) => Values(:, :)
        end subroutine declvar_dble_2D

        !***********************************************************************
        ! declvar_int_0D - set up memory for integer variables
        !***********************************************************************
        subroutine declvar_int_0D(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer(i4), intent(in) :: Numvalues
            integer(i4), target :: Values

            !***********************************************************************
            call declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

            allocate (Variable_data(Num_variables)%values_int(Numvalues))
            Variable_data(Num_variables)%values_int(1) = Values
        end subroutine declvar_int_0D

        !***********************************************************************
        ! declvar_int - set up memory for integer variables
        !***********************************************************************
        subroutine declvar_int_1D(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer(i4), intent(in) :: Numvalues
            integer(i4), TARGET :: Values(*)

            !***********************************************************************
            call declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)
            allocate (Variable_data(Num_variables)%values_int(Numvalues))
            Variable_data(Num_variables)%values_int => Values(:Numvalues)
        end subroutine declvar_int_1D

        !***********************************************************************
        ! declvar_real - set up memory for real variables
        !***********************************************************************
        subroutine declvar_real_0D(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer(i4), intent(in) :: Numvalues
            real(r4), TARGET :: Values

            !***********************************************************************
            call declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)

            allocate (Variable_data(Num_variables)%values_real(Numvalues))
            Variable_data(Num_variables)%values_real(1) = Values
        end subroutine declvar_real_0D

        !***********************************************************************
        ! declvar_real_1D - set up memory for real variables
        !***********************************************************************
        subroutine declvar_real_1D(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname, Varname, Dimenname, Data_type, Desc, Units
            integer(i4), intent(in) :: Numvalues
            real(r4), TARGET :: Values(*)

            !***********************************************************************
            call declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)
            allocate (Variable_data(Num_variables)%values_real(Numvalues))
            Variable_data(Num_variables)%values_real => Values(:Numvalues)
        end subroutine declvar_real_1D

        !***********************************************************************
        ! find_variable - find variable in data structure
        !***********************************************************************
        integer function find_variable(Modname, Varname, Numvalues, Data_type)
            implicit none

            ! Arguments
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
            do i = 1, Num_variables
                if (Varname == Variable_data(i)%variable_name) then
                    found = 1
                    if (Variable_data(i)%numvals /= Numvalues) then
                        ierr = 1
                        print *, 'ERROR in: ', Modname, ', Variable: ', Varname, &
                                &               ' number of values in getvar does not match declared number of values'
                    endif
                    if (Variable_data(i)%data_type /= Data_type) then
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
        integer function getvar(Modname, Varname, Numvalues, Data_type, Values)
            implicit none

            ! Arguments
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
            var_id = find_variable(Modname, Varname, Numvalues, Data_type)
            var_type = Variable_data(var_id)%data_flag

            if (var_type == 1) then
                allocate (itemp(Numvalues))
                itemp = Variable_data(var_id)%values_int
                Values = transfer(itemp, Values)
                deallocate (itemp)
            elseif (var_type == 2) then
                allocate (temp(Numvalues))
                temp = Variable_data(var_id)%values_real
                Values = transfer(temp, Values)
                deallocate (temp)
            elseif (var_type == 3) then
                allocate (dtemp(Numvalues))
                dtemp = Variable_data(var_id)%values_dble
                Values = transfer(dtemp, Values)
                deallocate (dtemp)
            endif

            getvar = 0
        end function getvar

        !***********************************************************************
        ! getvarnvals - get variable number of values
        !***********************************************************************
        integer function getvarnvals(Varname)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Varname

            ! Local Variables
            integer(i4) :: i

            !***********************************************************************
            getvarnvals = 1
            do i = 1, Num_variables
                if (Varname == Variable_data(i)%variable_name) then
                    getvarnvals = Variable_data(i)%numvals
                    return
                endif
            enddo
            print *, 'ERROR in: getvarnvals, Variable: ', Varname, ' not declared'
            STOP
        end function getvarnvals

        !***********************************************************************
        ! getvarsize - return the number of values for a parameter
        !***********************************************************************
        integer function getvarsize(Varname)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Varname

            ! Local Variables
            integer(i4) :: found, i

            !***********************************************************************
            found = 0
            do i = 1, Num_variables
                if (Varname == Variable_data(i)%variable_name) then
                    found = i
                    getvarsize = Variable_data(i)%numvals
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
        integer function getvartype(Varname)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Varname

            ! Functions
            ! INTRINSIC TRIM

            ! Local Variables
            integer :: i

            !***********************************************************************
            getvartype = 1
            do i = 1, Num_variables
                if (Varname == Variable_data(i)%variable_name) then
                    getvartype = Variable_data(i)%data_flag
                    getvartype = getvartype
                    return
                endif
            enddo
            print *, 'ERROR variable: ', Varname, ' not available'
            STOP
        end function getvartype

        !***********************************************************************
        ! getvar_dble - get double precision variable values
        !***********************************************************************
        subroutine getvar_dble(Modname, Varname, Numvalues, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname
            character(len=*), intent(in) :: Varname
            integer(i4), intent(in) :: Numvalues
            real(r8), intent(out) :: Values(Numvalues)

            ! Local Variables
            integer(i4) :: var_id

            !***********************************************************************
            var_id = find_variable(Modname, Varname, Numvalues, 'double')
            Values = Variable_data(var_id)%values_dble
        end subroutine getvar_dble

        !***********************************************************************
        ! getvar_id - get variable index
        !***********************************************************************
        integer function getvar_id(Varname)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Varname

            ! Functions
            ! INTRINSIC TRIM

            ! Local Variables
            integer(i4) :: i

            !***********************************************************************
            getvar_id = 1
            do i = 1, Num_variables
                if (Varname == Variable_data(i)%variable_name) then
                    getvar_id = Variable_data(i)%id_num
                    return
                endif
            enddo
            print *, 'ERROR variable: ', Varname, ' not available'
            STOP
        end function getvar_id

        !***********************************************************************
        ! getvar_real - get single precision variable values
        !***********************************************************************
        subroutine getvar_real(Modname, Varname, Numvalues, Values)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Modname
            character(len=*), intent(in) :: Varname
            integer(i4), intent(in) :: Numvalues
            real(r4), intent(out) :: Values(Numvalues)

            ! Local Variables
            integer(i4) :: var_id

            !***********************************************************************
            var_id = find_variable(Modname, Varname, Numvalues, 'real')
            Values = Variable_data(var_id)%values_real
        end subroutine getvar_real

end MODULE variables_mod
