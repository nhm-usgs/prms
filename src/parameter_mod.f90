module parameter_mod
    use kinds_mod, only: r4, r8, i4, i8
    ! use data_mod, only: str_arr_type
    implicit none

    type PRMS_parameter
        character(len=:), allocatable :: param_name
        character(len=:), allocatable :: short_description
        character(len=:), allocatable :: long_description
        integer(i4) :: numvals
        integer(i4) :: data_flag
        integer(i4) :: decl_flag
        integer(i4) :: read_flag
        ! integer :: id_num   ! what is this?

        integer(i4) :: num_dimens
        character(len=:), allocatable :: data_type
        character(len=:), allocatable :: dimen_names
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: units

        ! Used for string parameters
        character(len=:), allocatable :: max_value
        character(len=:), allocatable :: min_value
        character(len=:), allocatable :: def_value

        ! Used for integer parameters
        integer(i4), pointer :: int_values(:)
        integer(i4) :: default_int
        integer(i4) :: maximum_int
        integer(i4) :: minimum_int

        ! Used for real parameters
        real(r4), pointer :: values(:)
        real(r4) :: default_real
        real(r4) :: maximum
        real(r4) :: minimum
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
        procedure :: is_declared
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
            ! init%Parameter_data(i)%nchars = 0
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
    ! is_declared - check for parameters being declared more than once
    !***********************************************************************
    logical function is_declared(this, Parmname, Modname)
        USE PRMS_MODULE, only: Print_debug
        implicit none

        ! Arguments
        class(parameter_arr_t), intent(in) :: this
        character(len=*), intent(in) :: Parmname
        character(len=*), intent(in) :: Modname

        ! Local Variables
        integer(i4) :: i

        !***********************************************************************
        is_declared = .false.

        do i = 1, this%Num_parameters
            if (Parmname == this%Parameter_data(i)%param_name) then
                if (this%Parameter_data(i)%decl_flag == 1) then
                    if (Print_debug > -1) then
                        print *, 'Parameter: ', Parmname, ' declared more than once'
                        print *, 'First declared by module: ', this%Parameter_data(i)%module_name
                        print *, 'Also declared by module: ', Modname
                        print *, 'Model uses values based on first declare'
                    endif

                    is_declared = .true.
                endif

                EXIT
            endif
        enddo
    end function is_declared

    !***********************************************************************
    ! declparam - set up memory for parameters
    !***********************************************************************
    integer function declparam(this, Modname, Paramname, Dimenname, Datatype, &
                               Defvalue, Minvalue, Maxvalue, Descshort, &
                               Desclong, Units, dim_data)
        use prms_constants, only: MAXPARAMETERS, MAXCONTROL_LENGTH
        use UTILS_PRMS, only: set_data_type
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
        integer(i4) :: comma
        integer(i4) :: nvals
        integer(i4) :: nvals2
        integer(i4) :: numvalues
        integer(i4) :: type_flag
        integer(i4) :: iset
        integer(i4) :: cidx     ! copy of current index for new parameter

        !    character(len = :), allocatable :: dimen1, dimen2
        character(len=MAXCONTROL_LENGTH) dimen1, dimen2

        !***********************************************************************
        !!!!!!!!!!!! check to see if already in data structure
        ! doesn't check to see if declared the same, uses first values
        if (this%is_declared(Paramname, Modname)) return

        ! current value of Num_parameters is the number that have been declared
        this%Num_parameters = this%Num_parameters + 1
        cidx = this%Num_parameters

        if (cidx > MAXPARAMETERS) STOP 'ERROR, hard-coded number of parameters exceeded, report to developers'

        this%Parameter_data(cidx)%module_name = Modname
        this%Parameter_data(cidx)%param_name = Paramname
        this%Parameter_data(cidx)%dimen_names = Dimenname
        this%Parameter_data(cidx)%data_type = Datatype
        this%Parameter_data(cidx)%def_value = Defvalue
        this%Parameter_data(cidx)%min_value = Minvalue
        this%Parameter_data(cidx)%max_value = Maxvalue
        this%Parameter_data(cidx)%short_description = Descshort
        this%Parameter_data(cidx)%long_description = Desclong
        this%Parameter_data(cidx)%units = Units

        this%Parameter_data(cidx)%decl_flag = 1
        ! this%Parameter_data(this%Num_parameters)%nchars = numchars(Paramname)
        ! Parameter_data(Num_parameters)%id_num = Num_dimensions

        call set_data_type(Datatype, type_flag)

        if (.not. ANY([1, 2]==type_flag)) then
            print *, 'ERROR: ', Paramname, '; datatype, ', Datatype, ', not implemented.'
            STOP
        endif

        this%Parameter_data(cidx)%data_flag = type_flag

        ! get dimension number of values
        dimen2 = ' '
        ! ndimen = numchars(Dimenname)
        comma = INDEX(Dimenname, ',')

        if (comma == 0) then
            dimen1 = Dimenname
            this%Parameter_data(cidx)%num_dimens = 1
        else
            dimen1 = Dimenname(:(comma - 1))
            dimen2 = Dimenname((comma + 1):)
            this%Parameter_data(cidx)%num_dimens = 2
        endif

        call dim_data%get_data(trim(dimen1), numvalues, missing_stop=.true.)

        if (comma > 0) then
            call dim_data%get_data(trim(dimen2), nvals2, missing_stop=.true.)
            numvalues = numvalues * nvals2
        endif
        this%Parameter_data(cidx)%numvals = numvalues

        ! could add string and double
        if (type_flag == 1) then
            read (Defvalue, *) this%Parameter_data(cidx)%default_int
            allocate (this%Parameter_data(cidx)%int_values(numvalues))
            this%Parameter_data(cidx)%int_values = this%Parameter_data(cidx)%default_int
        elseif (type_flag == 2) then
            read (Defvalue, *) this%Parameter_data(cidx)%default_real
            allocate (this%Parameter_data(cidx)%values(numvalues))
            this%Parameter_data(cidx)%values = this%Parameter_data(cidx)%default_real
        endif

        iset = 0
        if (Minvalue == 'bounded') iset = 1

        if (iset == 1) then
            if (type_flag == 1) then  ! bounded parameters should all be integer
                call dim_data%get_data(Maxvalue, nvals, missing_stop=.true.)

                this%Parameter_data(cidx)%maximum_int = nvals
                this%Parameter_data(cidx)%minimum_int = this%Parameter_data(cidx)%default_int
            else
                print *, 'ERROR: Bounded parameter, ', Paramname, ', not real type'
                stop
            endif
        else
            if (type_flag == 1) then
                ! Integer datatype
                read (Maxvalue, *) this%Parameter_data(cidx)%maximum_int
                read (Minvalue, *) this%Parameter_data(cidx)%minimum_int
            else
                ! Real datatype
                read (Maxvalue, *) this%Parameter_data(cidx)%maximum
                read (Minvalue, *) this%Parameter_data(cidx)%minimum
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
        character(len=*), intent(in) :: Paramname
        character(len=*), intent(in) :: Data_type
        integer(i4), intent(in) :: Numvalues
        character(len=*), intent(out) :: String

        ! Functions
        INTRINSIC INDEX

        ! Local Variables
        integer(i4) :: nchars
        integer(i4) :: nchars_param
        integer(i4) :: type_flag
        integer(i4) :: num_values
        integer(i4) :: i
        integer(i4) :: j
        character(len=16) :: dimenname

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
        integer(i4) :: type_flag
        integer(i4) :: param_id
        integer(i4) :: i
        logical :: ierr     ! indicates an error condition has occurred
        logical :: found

        !***********************************************************************
        Values = 0.0
        ierr = .false.
        found = .false.

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = .true.

                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif

                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (.not. found) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = .true.
        endif

        if (ierr) STOP

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
        integer(i4) :: type_flag
        integer(i4) :: param_id
        integer(i4) :: i
        logical :: found
        logical :: ierr     ! indicates error condition

        !***********************************************************************
        Values = 0.0
        ierr = .false.
        found = .false.

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = .true.

                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                             ' number of values in getparam does not match declared number of values'
                endif

                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                             ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (.not. found) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = .true.
        endif
        if (ierr) STOP

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
        integer(i4) :: type_flag
        integer(i4) :: param_id
        integer(i4) :: i
        logical :: found
        logical :: ierr

        !***********************************************************************
        Values = 0.0
        ierr = .false.
        found = .false.

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = .true.

                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' number of values in getparam does not match declared number of values'
                endif

                if (Data_type /= this%Parameter_data(i)%data_type) then
                ! if (TRIM(Parameter_data(i)%data_type) /= Data_type) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (.not. found) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = .true.
        endif
        if (ierr) STOP

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
        integer(i4) :: type_flag
        integer(i4) :: param_id
        integer(i4) :: i
        logical :: found
        logical :: ierr

        !***********************************************************************
        Values = 0.0
        ierr = .false.
        found = .false.

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = .true.
                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif
                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (.not. found) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = .true.
        endif
        if (ierr) STOP

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
        integer(i4) :: type_flag
        integer(i4) :: param_id
        integer(i4) :: i
        logical :: found
        logical :: ierr

        !***********************************************************************
        Values = 0.0
        ierr = .false.
        found = .false.

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = .true.
                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif

                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (.not. found) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = .true.
        endif
        if (ierr) STOP

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
        integer(i4) :: type_flag
        integer(i4) :: param_id
        integer(i4) :: i
        integer(i4) :: shp(2)   ! Array to hold shape information for the Values array

        logical :: found
        logical :: ierr


        !***********************************************************************
        shp = shape(Values)
        Values = 0.0
        ierr = .false.
        found = .false.

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = .true.
                if (this%Parameter_data(i)%numvals /= Numvalues) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            &               ' number of values in getparam does not match declared number of values'
                endif

                if (this%Parameter_data(i)%data_type /= Data_type) then
                    ierr = .true.
                    print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
                            ' data type does in getparam not match declared data type'
                endif
                param_id = i
                EXIT
            endif
        enddo

        if (.not. found) then
            print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
            ierr = .true.
        endif
        if (ierr) STOP

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
        integer(i4) :: i
        integer(i4) :: ii
        integer(i4) :: j
        integer(i4) :: k
        integer(i4) :: iflg
        integer(i4) :: comma
        integer(i4) :: nvals
        character(len=:), allocatable :: dimen1
        integer(i4) :: found    ! used to indicate parameter was found, but also for the index of the found parameter
        logical :: ierr

        !***********************************************************************
        ierr = .false.
        found = 0

        do i = 1, this%Num_parameters
            if (Paramname == this%Parameter_data(i)%param_name) then
                found = i

                if (this%Parameter_data(i)%data_flag /= Data_type) then
                    ierr = .true.
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
                            ierr = .true.
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
            ierr = .true.
        endif
        if (ierr) STOP
    end subroutine setparam

end module parameter_arr_mod
