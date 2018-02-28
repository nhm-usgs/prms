module Control_class
  use variableKind
  use m_fileIO, only: openFile, closeFile
  use rArray_class, only: rArray
  use iArray_class, only: iArray
  use sArray_class, only: sArray
  implicit none

  private

  public :: Control

  type Control
    type(iArray) :: print_debug
    type(iArray) :: parameter_check_flag
    type(iArray) :: cbh_check_flag
    type(iArray) :: cbh_binary_flag

    type(iArray) :: save_vars_to_file
    type(iArray) :: init_vars_from_file
    type(iArray) :: prms_warmup

    type(sArray) :: model_mode

    type(sArray) :: precip_module
    type(sArray) :: temp_module
    type(sArray) :: solrad_module
    type(sArray) :: et_module
    type(sArray) :: transp_module

    type(sArray) :: data_file
    type(sArray) :: param_file
    type(sArray) :: model_output_file
    type(sArray) :: var_save_file
    type(sArray) :: var_init_file

    type(sArray) :: tmax_day
    type(sArray) :: tmin_day
    type(sArray) :: precip_day

    type(iArray) :: start_time
    type(iArray) :: end_time

    type(iArray) :: basinOutON_OFF
    type(iArray) :: basinOutVars
    type(iArray) :: basinOut_freq
    type(sArray) :: basinOutVar_names
    type(sArray) :: basinOutBaseFileName

    type(iArray) :: nhruOutON_OFF
    type(iArray) :: nhruOutVars
    type(iArray) :: nhruOut_freq
    type(sArray) :: nhruOutVar_names
    type(sArray) :: nhruOutBaseFileName

    character(len=:), allocatable, private :: Version_read_control_file
    character(len=:), allocatable, private :: control_filename

  contains
    procedure, public :: read => read_Control
  end type

  interface Control
    !! Overloaded interface to instantiate the class.
    module function constructor_Control(control_filename) result(this)
      type(Control) :: this
        !! Control Class
      character(len=*), intent(in) :: control_filename
        !! File name to read the control parameters from.
    end function
  end interface

  interface
    module subroutine read_Control(this)
      class(Control), intent(inout) :: this
        !! Control Class
    end subroutine
  end interface

end module
