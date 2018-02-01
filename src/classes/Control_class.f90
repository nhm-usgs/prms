module Control_class
    use variableKind
    use rVariable_class, only: rVariable
    use iVariable_class, only: iVariable
    use sVariable_class, only: sVariable
    implicit none

    private

    public :: Control

    type Control
        type(iVariable) :: print_debug
        type(iVariable) :: parameter_check_flag
        type(iVariable) :: cbh_check_flag
        type(iVariable) :: cbh_binary_flag

        type(iVariable) :: save_vars_to_file
        type(iVariable) :: init_vars_from_file
        type(iVariable) :: prms_warmup

        type(sVariable) :: model_mode

        type(sVariable) :: precip_module
        type(sVariable) :: temp_module
        type(sVariable) :: solrad_module
        type(sVariable) :: et_module
        type(sVariable) :: transp_module

        type(sVariable) :: data_file
        type(sVariable) :: param_file
        type(sVariable) :: model_output_file
        type(sVariable) :: var_save_file
        type(sVariable) :: var_init_file

        type(sVariable) :: tmax_day
        type(sVariable) :: tmin_day
        type(sVariable) :: precip_day

        type(iVariable) :: start_time
        type(iVariable) :: end_time

        type(iVariable) :: basinOutON_OFF
        type(iVariable) :: basinOutVars
        type(iVariable) :: basinOut_freq
        type(sVariable) :: basinOutVar_names
        type(sVariable) :: basinOutBaseFileName

        type(iVariable) :: nhruOutON_OFF
        type(iVariable) :: nhruOutVars
        type(iVariable) :: nhruOut_freq
        type(sVariable) :: nhruOutVar_names
        type(sVariable) :: nhruOutBaseFileName

        character(len=:), allocatable, private :: Version_read_control_file
        character(len=:), allocatable, private :: control_filename
    end type

    interface Control
      !! Overloaded interface to instantiate the class.
        module procedure :: constructor_Control
    end interface

    interface
        module subroutine read_control_file(this)
            type(Control), intent(inout) :: this
        end subroutine
    end interface

contains
    !====================================================================!
    function constructor_Control(control_filename) result(this)
        type(Control) :: this
        character(len=:), allocatable, intent(in) :: control_filename

        this%control_filename = control_filename
    end function
    !====================================================================!
end module
