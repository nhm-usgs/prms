module parameter_mod
    use kinds_mod, only: r4, r8, i4, i8
    ! use data_mod, only: str_arr_type
    implicit none

    ! integer(i4), save :: Num_parameters  ! Total_parameters

    type PRMS_parameter
        character(len=:), allocatable :: param_name
        character(len=:), allocatable :: short_description, long_description
        integer(i4) :: numvals, data_flag, decl_flag, read_flag, nchars
        ! integer :: id_num   ! what is this?
        integer(i4) :: default_int, maximum_int, minimum_int, num_dimens
        character(len=:), allocatable :: max_value, min_value, def_value, data_type
        character(len=:), allocatable :: dimen_names, module_name, units
        real(r4), pointer :: values(:)
        integer(i4), pointer :: int_values(:)
        real(r4) :: maximum, minimum, default_real
    end type PRMS_parameter
end module parameter_mod


module parameter_arr_mod
    use kinds_mod, only: r4, r8, i4, i8
    use parameter_mod, only: PRMS_parameter
    implicit none

    type parameter_arr_t
        type(PRMS_parameter), allocatable :: Parameter_data(:)
        integer(i4) :: Num_parameters

    contains
        private
        procedure :: check_parameters_declared
        procedure, public :: declparam
        procedure :: getparamstring
        procedure :: getparam_dbl
        procedure :: getparam_int_0D
        procedure :: getparam_int_1D
        procedure :: getparam_real_0D
        procedure :: getparam_real_1D
        procedure :: getparam_real_2D
        generic, public :: getparam => getparam_dbl, getparam_int_0D, getparam_int_1D, &
                                       getparam_real_0D, getparam_real_1D, getparam_real_2D
        procedure, public :: setparam
    end type parameter_arr_t

    interface parameter_arr_t
        procedure :: init
    end interface parameter_arr_t
contains
    function init()
        use prms_constants, only: MAXPARAMETERS
        implicit none

        ! initialize the array of parameters
        type(parameter_arr_t) :: init

        ! Local Variables
        integer(i4) :: i

        !***********************************************************************
        ! allocate and store parameter data
        !***********************************************************************
        ! Allocate and initialize parameter data base
        ! DANGER, DANGER, hard coded maximum number of parameters, DANGER, DANGER
        !***********************************************************************
        allocate (init%Parameter_data(MAXPARAMETERS)) ! allow for extra parameters being expected
        do i = 1, MAXPARAMETERS
            init%Parameter_data(i)%param_name = ' '
            init%Parameter_data(i)%short_description = ' '
            init%Parameter_data(i)%long_description = ' '
            init%Parameter_data(i)%numvals = 0
            init%Parameter_data(i)%data_flag = 0
            init%Parameter_data(i)%decl_flag = 0
            init%Parameter_data(i)%read_flag = 0
            init%Parameter_data(i)%nchars = 0
            ! Parameter_data(i)%id_num = 0
            init%Parameter_data(i)%max_value = ' '
            init%Parameter_data(i)%min_value = ' '
            init%Parameter_data(i)%def_value = ' '
            init%Parameter_data(i)%data_type = ' '
            init%Parameter_data(i)%module_name = ' '
            init%Parameter_data(i)%units = ' '
            init%Parameter_data(i)%dimen_names = ' '
            init%Parameter_data(i)%maximum = 0.0
            init%Parameter_data(i)%minimum = 0.0
            init%Parameter_data(i)%default_real = 0.0
            init%Parameter_data(i)%maximum_int = 0
            init%Parameter_data(i)%minimum_int = 0
            init%Parameter_data(i)%default_int = 0
            init%Parameter_data(i)%num_dimens = 0
        enddo

        init%Num_parameters = 0
    end function init


    !***********************************************************************
    ! check_parameters_declared - check for parameters being declared more than once
    !***********************************************************************
    subroutine check_parameters_declared(this, Parmname, Modname, Iret)
        use UTILS_PRMS, only: numchars
        USE PRMS_MODULE, only: Print_debug
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Parmname
        character(len=*), intent(in) :: Modname
        integer(i4), intent(out) :: Iret

        ! Local Variables
        integer(i4) :: i, nchars

        !***********************************************************************
        Iret = 0
        nchars = numchars(Parmname)

        do i = 1, this%Num_parameters
            if (nchars == this%Parameter_data(i)%nchars) then
                if (Parmname(:nchars) == this%Parameter_data(i)%param_name(:nchars)) then
                    if (this%Parameter_data(i)%decl_flag == 1) then
                        if (Print_debug > -1) then
                            print *, 'Parameter: ', Parmname, ' declared more than once'
                            print *, 'First declared by module: ', this%Parameter_data(this%Num_parameters)%module_name
                            print *, 'Also declared by module: ', Modname
                            print *, 'Model uses values based on first declare'
                        endif
                        Iret = 1
                    endif

                    EXIT
                endif
            endif
        enddo
    end subroutine check_parameters_declared

    !***********************************************************************
    ! declparam - set up memory for parameters
    !***********************************************************************
    integer function declparam(this, Modname, Paramname, Dimenname, Datatype, &
            &                  Defvalue, Minvalue, Maxvalue, Descshort, &
                               Desclong, Units, dim_data)
        use prms_constants, only: MAXPARAMETERS, MAXCONTROL_LENGTH
        use UTILS_PRMS, only: numchars, read_error, set_data_type
        use dimensions_mod, only: dimension_list
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(inout) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Dimenname
        character(len=*), intent(in) :: Datatype
        character(len=*), intent(in) :: Defvalue
        character(len=*), intent(in) :: Minvalue
        character(len=*), intent(in) :: Maxvalue
        character(len=*), intent(in) :: Descshort
        character(len=*), intent(in) :: Desclong
        character(len=*), intent(in) :: Units
        type(dimension_list), intent(in) :: dim_data

        ! INTRINSIC
        INTRINSIC INDEX, TRIM

        ! Local Variables
        integer(i4) :: comma, ndimen, nval, nvals, nvals2, declared, numvalues, type_flag, iset
        !    character(len = :), allocatable :: dimen1, dimen2
        character(len=MAXCONTROL_LENGTH) dimen1, dimen2

        !***********************************************************************
        !!!!!!!!!!!! check to see if already in data structure
        ! doesn't check to see if declared the same, uses first values
        call this%check_parameters_declared(Paramname, Modname, declared)
        if (declared == 1) return   ! 2017-10-30 PAN: Warning: control reaches end of non-void function

        ! current value of Num_parameters is the number that have been declared
        this%Num_parameters = this%Num_parameters + 1
        if (this%Num_parameters > MAXPARAMETERS) STOP 'ERROR, hard-coded number of parameters exceeded, report to developers'

        this%Parameter_data(this%Num_parameters)%module_name = Modname
        this%Parameter_data(this%Num_parameters)%param_name = Paramname
        this%Parameter_data(this%Num_parameters)%dimen_names = Dimenname
        this%Parameter_data(this%Num_parameters)%data_type = Datatype
        this%Parameter_data(this%Num_parameters)%def_value = Defvalue
        this%Parameter_data(this%Num_parameters)%min_value = Minvalue
        this%Parameter_data(this%Num_parameters)%max_value = Maxvalue
        this%Parameter_data(this%Num_parameters)%short_description = Descshort
        this%Parameter_data(this%Num_parameters)%long_description = Desclong
        this%Parameter_data(this%Num_parameters)%units = Units

        this%Parameter_data(this%Num_parameters)%decl_flag = 1
        this%Parameter_data(this%Num_parameters)%nchars = numchars(Paramname)
        ! Parameter_data(Num_parameters)%id_num = Num_dimensions

        call set_data_type(Datatype, type_flag)
        if (type_flag < 1 .OR. type_flag > 2) call read_error(16, Paramname // ': data type not implemented: ' // Datatype)
        this%Parameter_data(this%Num_parameters)%data_flag = type_flag

        ! get dimension number of values
        dimen2 = ' '
        ndimen = numchars(Dimenname)
        comma = INDEX(Dimenname, ',')

        if (comma == 0) then
            dimen1 = Dimenname(:ndimen)
            this%Parameter_data(this%Num_parameters)%num_dimens = 1
        else
            dimen1 = Dimenname(:(comma - 1))
            dimen2 = Dimenname((comma + 1):ndimen)
            this%Parameter_data(this%Num_parameters)%num_dimens = 2
        endif

        call dim_data%get_data(trim(dimen1), numvalues)
        ! numvalues = getdim(TRIM(dimen1))
        if (numvalues == -1) call read_error(11, TRIM(dimen1))
        if (comma > 0) then
            call dim_data%get_data(trim(dimen2), nvals2)
            ! nvals2 = getdim(TRIM(dimen2))

            if (nvals2 == -1) call read_error(11, TRIM(dimen2))
            numvalues = numvalues * nvals2
        endif
        this%Parameter_data(this%Num_parameters)%numvals = numvalues

        ! could add string and double
        if (type_flag == 1) then
            read (Defvalue, *) this%Parameter_data(this%Num_parameters)%default_int
            allocate (this%Parameter_data(this%Num_parameters)%int_values(numvalues))
            this%Parameter_data(this%Num_parameters)%int_values = this%Parameter_data(this%Num_parameters)%default_int
        elseif (type_flag == 2) then
            read (Defvalue, *) this%Parameter_data(this%Num_parameters)%default_real
            allocate (this%Parameter_data(this%Num_parameters)%values(numvalues))
            this%Parameter_data(this%Num_parameters)%values = this%Parameter_data(this%Num_parameters)%default_real
        endif

        iset = 0
        nval = len(Minvalue)
        if (nval > 6) then
            if (Minvalue(:7) == 'bounded') iset = 1
        endif

        if (iset == 1) then
            if (type_flag == 1) then  ! bounded parameters should all be integer
                call dim_data%get_data(Maxvalue, nvals)
                ! nvals = getdim(Maxvalue)

                if (nvals == -1) call read_error(11, Maxvalue)
                this%Parameter_data(this%Num_parameters)%maximum_int = nvals
                this%Parameter_data(this%Num_parameters)%minimum_int = this%Parameter_data(this%Num_parameters)%default_int
            else
                STOP 'ERROR, bounded parameter not real type'
            endif
        else
            if (type_flag == 1) then
                read (Maxvalue, *) this%Parameter_data(this%Num_parameters)%maximum_int
                read (Minvalue, *) this%Parameter_data(this%Num_parameters)%minimum_int
            else
                read (Maxvalue, *) this%Parameter_data(this%Num_parameters)%maximum
                read (Minvalue, *) this%Parameter_data(this%Num_parameters)%minimum
            endif
        endif

        declparam = 0
    end function declparam

    !***********************************************************************
    ! getparamstring
    ! control parameters are read and verified this
    ! function checks to be sure a required parameter has a value (read or default)
    !***********************************************************************
    integer function getparamstring(this, Paramname, Numvalues, Data_type, String)
        use UTILS_PRMS, only: set_data_type
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Paramname, Data_type
        integer(i4), intent(in) :: Numvalues
        character(len=*), intent(out) :: String

        ! Functions
        INTRINSIC INDEX

        ! Local Variables
        integer(i4) nchars, nchars_param, type_flag, num_values, i, j
        character(len = 16) :: dimenname

        !***********************************************************************
        String = ' '
        ! Modname
        nchars_param = INDEX(Paramname, ' ') - 1
        ! Paramname(:nchars_param)
        nchars = INDEX(Dimenname, ' ') - 1
        num_values = -2
        if (num_values /= Numvalues) then
            print *, 'ERROR, number of values does not equal values for the dimension'
            print *, '       parameter: ', Dimenname(:nchars), ' dimension value:', num_values
            print *, '       dimension: ', Paramname(:nchars_param), ' number of values:', Numvalues
            STOP
        endif
        nchars = INDEX(Data_type, ' ') - 1
        ! Data_type(:nchars)
        call set_data_type(Data_type, type_flag)

        do j = 1, this%Num_parameters
            do i = 1, Numvalues
                if (type_flag == 1) then
                elseif (type_flag == 2) then
                elseif (type_flag == 3) then
                elseif (type_flag == 4) then
                endif
            enddo
            EXIT
        enddo

        getparamstring = 0
    end function getparamstring


    !***********************************************************************
    ! getparam_dbl - get parameter values for double precision datatype
    !***********************************************************************
    integer function getparam_dbl(this, Modname, Paramname, Numvalues, Data_type, Values)
        ! USE PRMS_MODULE, only: Parameter_check_flag
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues

        ! values could be any data type
        real(r8), intent(out) :: Values(Numvalues)

        ! Functions
        INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: type_flag, found, param_id, i, ierr

        !***********************************************************************
        Values = 0.0
        ierr = 0
        found = 0

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = 1
                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif
                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP

        type_flag = this%Parameter_data(param_id)%data_flag

        if (type_flag == 3) then
            Values = this%Parameter_data(param_id)%values
        else
            print *, 'Paramname: ', Paramname, '  type: ', type_flag
            stop 'getparam_dbl called with wrong datatype'
        endif

        getparam_dbl = 0
    end function getparam_dbl



    !***********************************************************************
    ! getparam_int_0D - get parameter values of datatype integer
    !***********************************************************************
    integer function getparam_int_0D(this, Modname, Paramname, Numvalues, Data_type, Values)
        ! USE PRMS_MODULE, only: Parameter_check_flag
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues
        integer(i4), intent(out) :: Values

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: type_flag, found, param_id, i, ierr

        !***********************************************************************
        Values = 0.0
        ierr = 0
        found = 0

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = 1

                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                             ' number of values in getparam does not match declared number of values'
                endif

                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                             ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP

        type_flag = this%Parameter_data(param_id)%data_flag

        if (type_flag == 1) then
            Values = this%Parameter_data(param_id)%int_values(1)
        else
            print *, 'Paramname: ', Paramname, '  type: ', type_flag
            stop 'getparam_int called with wrong datatype'
        end if

        getparam_int_0D = 0
    end function getparam_int_0D

    !***********************************************************************
    ! getparam_int_1D - get parameter values of datatype integer
    !***********************************************************************
    integer function getparam_int_1D(this, Modname, Paramname, Numvalues, Data_type, Values)
        ! USE PRMS_MODULE, only: Parameter_check_flag
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues
        integer(i4), intent(out) :: Values(Numvalues)

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: type_flag, found, param_id, i, ierr

        !***********************************************************************
        Values = 0.0
        ierr = 0
        found = 0

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = 1

                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' number of values in getparam does not match declared number of values'
                endif

                if (Data_type /= this%Parameter_data(i)%data_type) then
                ! if (TRIM(Parameter_data(i)%data_type) /= Data_type) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP

        type_flag = this%Parameter_data(param_id)%data_flag

        if (type_flag == 1) then
            Values = this%Parameter_data(param_id)%int_values
        else
            print *, 'Paramname: ', Paramname, '  type: ', type_flag
            stop 'getparam_int called with wrong datatype'
        end if

        getparam_int_1D = 0
    end function getparam_int_1D

    !***********************************************************************
    ! getparam_real_1D - get parameter values for real datatype
    !***********************************************************************
    integer function getparam_real_0D(this, Modname, Paramname, Numvalues, Data_type, Values)
        USE PRMS_MODULE, only: Parameter_check_flag
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues

        ! values could be any data type
        real(r4), intent(out) :: Values

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: type_flag, found, param_id, i, ierr

        !***********************************************************************
        Values = 0.0
        ierr = 0
        found = 0

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = 1
                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif
                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP

        type_flag = this%Parameter_data(param_id)%data_flag

        if (type_flag == 2) then
            if (Parameter_check_flag == 1) then
                do i = 1, Numvalues
                    if (this%Parameter_data(param_id)%values(i) > this%Parameter_data(param_id)%maximum) then
                        print *, 'WARNING, value > maximum value for parameter: ', Paramname, '; index:', param_id
                        print *, '         value:', this%Parameter_data(param_id)%values(i), '; maximum value:', &
                                this%Parameter_data(param_id)%maximum
                    endif
                    if (this%Parameter_data(param_id)%values(i) < this%Parameter_data(param_id)%minimum) then
                        print *, 'WARNING, value < minimum value for parameter: ', Paramname, '; index:', param_id
                        print *, '         value:', this%Parameter_data(param_id)%values(i), '; minimum value:', &
                                this%Parameter_data(param_id)%minimum
                    endif
                enddo
            endif
            Values = this%Parameter_data(param_id)%values(1)
        else
            print *, 'Paramname: ', Paramname, '  type: ', type_flag
            stop 'getparam_real called with wrong datatype'
        endif

        getparam_real_0D = 0
    end function getparam_real_0D

    !***********************************************************************
    ! getparam_real_1D - get parameter values for real datatype
    !***********************************************************************
    integer function getparam_real_1D(this, Modname, Paramname, Numvalues, Data_type, Values)
        USE PRMS_MODULE, only: Parameter_check_flag
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues

        ! values could be any data type
        real(r4), intent(out) :: Values(Numvalues)

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: type_flag, found, param_id, i, ierr

        !***********************************************************************
        Values = 0.0
        ierr = 0
        found = 0

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = 1
                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif
                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP

        type_flag = this%Parameter_data(param_id)%data_flag

        if (type_flag == 2) then
            if (Parameter_check_flag == 1) then
                do i = 1, Numvalues
                    if (this%Parameter_data(param_id)%values(i) > this%Parameter_data(param_id)%maximum) then
                        print *, 'WARNING, value > maximum value for parameter: ', Paramname, '; index:', param_id
                        print *, '         value:', this%Parameter_data(param_id)%values(i), '; maximum value:', &
                                this%Parameter_data(param_id)%maximum
                    endif
                    if (this%Parameter_data(param_id)%values(i) < this%Parameter_data(param_id)%minimum) then
                        print *, 'WARNING, value < minimum value for parameter: ', Paramname, '; index:', param_id
                        print *, '         value:', this%Parameter_data(param_id)%values(i), '; minimum value:', &
                                this%Parameter_data(param_id)%minimum
                    endif
                enddo
            endif
            Values = this%Parameter_data(param_id)%values
        else
            print *, 'Paramname: ', Paramname, '  type: ', type_flag
            stop 'getparam_real called with wrong datatype'
        endif

        getparam_real_1D = 0
    end function getparam_real_1D

    !***********************************************************************
    ! getparam_real - get parameter values for real datatype
    !***********************************************************************
    integer function getparam_real_2D(this, Modname, Paramname, Numvalues, Data_type, Values)
        USE PRMS_MODULE, only: Parameter_check_flag
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Modname
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues

        ! values could be any data type
        real(r4), intent(out) :: Values(:,:)

        ! Functions
        ! INTRINSIC TRIM

        ! Local Variables
        integer(i4) :: type_flag, found, param_id, i, ierr
        integer(i4) :: shp(2)   ! Array to hold shape information for the Values array

        !***********************************************************************
        shp = shape(Values)
        Values = 0.0
        ierr = 0
        found = 0

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = 1
                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif
                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = 1
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP

        type_flag = this%Parameter_data(param_id)%data_flag

        if (type_flag == 2) then
            if (Parameter_check_flag == 1) then
                do i = 1, Numvalues
                    if (this%Parameter_data(param_id)%values(i) > this%Parameter_data(param_id)%maximum) then
                        print *, 'WARNING, value > maximum value for parameter: ', Paramname, '; index:', param_id
                        print *, '         value:', this%Parameter_data(param_id)%values(i), '; maximum value:', &
                                this%Parameter_data(param_id)%maximum
                    endif
                    if (this%Parameter_data(param_id)%values(i) < this%Parameter_data(param_id)%minimum) then
                        print *, 'WARNING, value < minimum value for parameter: ', Paramname, '; index:', param_id
                        print *, '         value:', this%Parameter_data(param_id)%values(i), '; minimum value:', &
                                this%Parameter_data(param_id)%minimum
                    endif
                enddo
            endif
            ! Reshape the 1D Parameter_data array to 2D
            Values = reshape(this%Parameter_data(param_id)%values, shp)
        else
            print *, 'Paramname: ', Paramname, '  type: ', type_flag
            stop 'getparam_real called with wrong datatype'
        endif

        getparam_real_2D = 0
    end function getparam_real_2D


    !***********************************************************************
    ! setparam - set real or integer parameter values read from Parameter File
    !***********************************************************************
    subroutine setparam(this, Paramname, Numvalues, Data_type, Num_dims, Dim_string, Values, Ivalues)
        USE PRMS_MODULE, only: Nhru
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(inout) :: this
        integer(i4), intent(in) :: Numvalues
        integer(i4), intent(in) :: Data_type
        integer(i4), intent(in) :: Num_dims
        integer(i4), intent(in) :: Ivalues(*)
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Dim_string(Num_dims)
        real(r4), intent(in) :: Values(*)
        ! NOTE: 20171229 PAN: above line was originally (r8)

        ! Functions
        INTRINSIC TRIM, INDEX

        ! Local Variables
        integer(i4) :: found, i, ii, j, k, ierr, iflg, comma, nvals
        character(len=:), allocatable :: dimen1
        ! character(len = MAXCONTROL_LENGTH) dimen1

        !***********************************************************************
        ierr = 0
        found = 0
        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = i

                if (this%Parameter_data(i)%data_flag /= Data_type) then
                    ierr = 1
                    print *, 'ERROR, Parameter: ', Paramname, ' data type does not match declared data type'
                endif

                if (this%Parameter_data(i)%numvals == Numvalues) then
                    if (Data_type == 2) then
                        do j = 1, Numvalues
                            this%Parameter_data(found)%values(j) = Values(j)
                        enddo
                    else
                        do j = 1, Numvalues
                            this%Parameter_data(found)%int_values(j) = Ivalues(j)
                        enddo
                    endif
                else ! check for flexible dimension
                    if (Numvalues == 1) then ! set all values to single value
                        if (Data_type == 2) then
                            do j = 1, this%Parameter_data(found)%numvals
                                this%Parameter_data(found)%values(j) = Values(1)
                            enddo
                        else
                            do j = 1, this%Parameter_data(found)%numvals
                                this%Parameter_data(found)%int_values(j) = Ivalues(1)
                            enddo
                        endif
                    else
                        nvals = this%Parameter_data(found)%numvals / 12
                        if (nvals * 12 /= this%Parameter_data(found)%numvals) then
                            iflg = 0
                            if (Num_dims == 1 .AND. TRIM(Dim_string(1)) == 'nmonths') iflg = 1

                            if (Num_dims == 2) then
                                if (TRIM(Dim_string(2)) == 'nmonths') iflg = 1
                            endif

                            if (iflg == 1) then
                                print *, 'ERROR, parameter not evenly divisible by 12'
                                print *, '       number of parameter values expected:', this%Parameter_data(i)%numvals
                                print *, '       number of parameter values specified:', Numvalues
                                STOP
                            endif
                        endif

                        comma = INDEX(this%Parameter_data(found)%dimen_names, ',')
                        if (comma == 0) then
                            dimen1 = TRIM(this%Parameter_data(found)%dimen_names)
                        else
                            dimen1 = this%Parameter_data(found)%dimen_names(:(comma - 1))
                        endif

                        ! DANGER, messy if's
                        iflg = 0
                        if (Numvalues == 12 .AND. Nhru /= 12 .AND. Num_dims == 1 .AND. TRIM(Dim_string(1)) == 'nmonths') iflg = 2 ! set monthly
                        if (Numvalues == Nhru .AND. Num_dims == 1 .AND. TRIM(Dim_string(1)) /= 'nmonths') iflg = 3 ! set nhru, nmonths

                        k = 0
                        if (iflg == 3) then ! 12 sets of nhru values
                            do j = 1, 12
                                do ii = 1, nvals
                                    k = k + 1
                                    if (Data_type == 2) then
                                        this%Parameter_data(found)%values(k) = Values(ii)
                                    else
                                        this%Parameter_data(found)%int_values(k) = Ivalues(ii)
                                    endif
                                enddo
                            enddo
                        elseif (iflg == 2) then ! dim sets of 12
                            do j = 1, 12
                                do ii = 1, nvals
                                    k = k + 1
                                    if (Data_type == 2) then
                                        this%Parameter_data(found)%values(k) = Values(j)
                                    else
                                        this%Parameter_data(found)%int_values(k) = Ivalues(j)
                                    endif
                                enddo
                            enddo
                        else
                            !                print *, '??? not sure this can happen'
                            !                do ii = 1, nvals
                            !                  do j = 1, 12
                            !                    k = k + 1
                            !                    if ( Data_type==2 ) then
                            !                      Parameter_data(found)%values(k) = Values(ii)
                            !                    else
                            !                      Parameter_data(found)%int_values(k) = Ivalues(ii)
                            !                    endif
                            !                  enddo
                            !                enddo
                            !!!!!! add parameter expansion !!!!!!!!!! for nsub
                            ierr = 1
                            print *, 'ERROR, Parameter: ', Paramname, &
                                    ' number of values in getparam does not match declared number of values'
                        endif
                    endif
                endif
                EXIT
            endif
        enddo

        if (found == 0) then
            print *, 'ERROR, Parameter: ', Paramname, ' not declared'
            ierr = 1
        endif
        if (ierr == 1) STOP
    end subroutine setparam

end module parameter_arr_mod
