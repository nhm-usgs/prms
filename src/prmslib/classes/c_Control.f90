module Control_class
  use iso_fortran_env, only: output_unit
  use variableKind
  use m_fileIO, only: openFile, closeFile
  use rArray_class, only: rArray
  use iArray_class, only: iArray
  use sArray_class, only: sArray
  use iScalar_class, only: iScalar
  use rScalar_class, only: rScalar
  use PRMS_FILE_IO_NETCDF, only: FileIO_netcdf
  use PRMS_OUTVAR_LIST, only: outvar_list
  implicit none

  private
  public :: Control

  character(len=*), parameter :: MODDESC = 'Control File'
  character(len=*), parameter :: MODNAME = 'Control_class'
  character(len=*), parameter :: MODVERSION = '2019-02-01 13:50:00Z'

  type Control
    ! Control variables
    type(iScalar) :: aniOutON_OFF
    type(sArray) :: aniOutVar_names
    type(sArray) :: ani_output_file
    type(sArray) :: basinOutBaseFileName
    type(iScalar) :: basinOutON_OFF
    type(sArray) :: basinOutVar_names
    type(iScalar) :: basinOutVars
    type(iScalar) :: basinOut_freq
    type(iScalar) :: cascade_flag
    type(iScalar) :: cascadegw_flag
    type(iScalar) :: cbh_binary_flag
    type(iScalar) :: cbh_check_flag
    type(sArray) :: covden_sum_dynamic
    type(sArray) :: covden_win_dynamic
    type(sArray) :: covtype_dynamic
    type(iScalar) :: csvON_OFF
    type(sArray) :: csv_output_file
    type(sArray) :: data_file
    type(iScalar) :: dispGraphsBuffSize
    type(sArray) :: dispVar_element
    type(sArray) :: dispVar_names
    type(sArray) :: dispVar_plot
    type(sArray) :: dprst_depth_dynamic
    type(iScalar) :: dprst_flag
    type(sArray) :: dprst_frac_dynamic
    type(iScalar) :: dprst_transferON_OFF
    type(sArray) :: dprst_transfer_file
    type(iScalar) :: dyn_covden_flag
    type(iScalar) :: dyn_covtype_flag
    type(iScalar) :: dyn_dprst_flag
    type(iScalar) :: dyn_fallfrost_flag
    type(iScalar) :: dyn_imperv_flag
    type(iScalar) :: dyn_intcp_flag
    type(iScalar) :: dyn_potet_flag
    type(iScalar) :: dyn_radtrncf_flag
    type(iScalar) :: dyn_snareathresh_flag
    type(iScalar) :: dyn_soil_flag
    type(iScalar) :: dyn_springfrost_flag
    type(iScalar) :: dyn_sro2dprst_imperv_flag
    type(iScalar) :: dyn_sro2dprst_perv_flag
    type(iScalar) :: dyn_transp_flag
    type(iScalar) :: dyn_transp_on_flag
    type(iArray) :: end_time
    type(sArray) :: et_module
    type(sArray) :: executable_desc
    type(sArray) :: executable_model
    type(iScalar) :: external_transferON_OFF
    type(sArray) :: external_transfer_file
    type(sArray) :: fallfrost_dynamic
    type(iScalar) :: gwr_swale_flag
    type(iScalar) :: gwr_transferON_OFF
    type(sArray) :: gwr_transfer_file
    type(sArray) :: humidity_day
    type(sArray) :: humidity_module
    type(sArray) :: imperv_frac_dynamic
    type(sArray) :: imperv_stor_dynamic
    type(iScalar) :: init_vars_from_file
    type(rScalar) :: initial_deltat
    type(iScalar) :: lake_transferON_OFF
    type(sArray) :: lake_transfer_file
    type(iScalar) :: mapOutON_OFF
    type(sArray) :: mapOutVar_names
    type(sArray) :: model_mode
    type(sArray) :: model_output_file
    type(iScalar) :: naniOutVars
    type(iScalar) :: ndispGraphs
    type(sArray) :: nhruOutBaseFileName
    type(iScalar) :: nhruOutON_OFF
    type(sArray) :: nhruOutVar_names
    type(iScalar) :: nhruOutVars
    type(iScalar) :: nhruOut_format
    type(iScalar) :: nhruOut_freq
    type(iScalar) :: nmapOutVars
    type(sArray) :: nsegmentOutBaseFileName
    type(iScalar) :: nsegmentOutON_OFF
    type(sArray) :: nsegmentOutVar_names
    type(iScalar) :: nsegmentOutVars
    type(iScalar) :: nsegmentOut_format
    type(iScalar) :: nsegmentOut_freq
    type(iScalar) :: nstatVars
    type(sArray) :: nsubOutBaseFileName
    type(iScalar) :: nsubOutON_OFF
    type(sArray) :: nsubOutVar_names
    type(iScalar) :: nsubOutVars
    type(iScalar) :: nsubOut_format
    type(iScalar) :: nsubOut_freq
    type(iScalar) :: orad_flag
    type(iScalar) :: outVarON_OFF
    type(sArray) :: outVar_base_filename
    type(sArray) :: outVar_names
    type(sArray) :: param_file
    type(iScalar) :: parameter_check_flag
    type(sArray) :: potet_day
    type(sArray) :: potetcoef_dynamic
    type(sArray) :: precip_day
    type(sArray) :: precip_module
    type(iScalar) :: print_debug
    type(iScalar) :: prms_warmup
    type(sArray) :: radtrncf_dynamic
    type(iScalar) :: save_vars_to_file
    type(iScalar) :: segment_transferON_OFF
    type(sArray) :: segment_transfer_file
    type(sArray) :: snareathresh_dynamic
    type(sArray) :: snow_intcp_dynamic
    type(sArray) :: soilmoist_dynamic
    type(sArray) :: soilrechr_dynamic
    type(sArray) :: soilzone_module
    type(sArray) :: solrad_module
    type(sArray) :: springfrost_dynamic
    type(sArray) :: srain_intcp_dynamic
    type(sArray) :: sro2dprst_imperv_dynamic
    type(sArray) :: sro2dprst_perv_dynamic
    type(sArray) :: srunoff_module
    type(iArray) :: start_time
    type(sArray) :: statVar_element
    type(sArray) :: statVar_names
    type(iScalar) :: statsON_OFF
    type(sArray) :: stats_output_file
    type(sArray) :: stat_var_file
    type(iScalar) :: stream_temp_flag
    type(iScalar) :: stream_temp_shade_flag
    type(sArray) :: strmflow_module
    type(iScalar) :: strmtemp_humidity_flag
    type(iScalar) :: subbasin_flag
    type(sArray) :: swrad_day
    type(sArray) :: temp_module
    type(sArray) :: tmax_day
    type(sArray) :: tmin_day
    type(sArray) :: transp_day
    type(sArray) :: transp_module
    type(sArray) :: transp_on_dynamic
    type(sArray) :: transpbeg_dynamic
    type(sArray) :: transpend_dynamic
    type(sArray) :: var_init_file
    type(sArray) :: var_save_file
    type(sArray) :: wind_module
    type(sArray) :: windspeed_day
    type(sArray) :: wrain_intcp_dynamic

    ! Non-control file variables
    integer(i32) :: model_output_unit
      !! File unit for opened model_output_file
    integer(i32) :: restart_output_unit
      !! File unit to write restart information to
    integer(i32) :: restart_input_unit
      !! File unit to read restart information from
    integer(i32) :: restart_out_hdl
    integer(i32) :: restart_in_hdl

    logical :: gsflow_mode = .false.
      !! Indicates true if model_mode == 'GSFLOW'

    type(outvar_list) :: output_variables
      !! List of all possible output variables for PRMS

    character(len=:), allocatable, private :: Version_read_control_file
    character(len=:), allocatable, private :: control_filename

    ! class(FileIO), allocatable :: param_file_hdl
    type(FileIO_netcdf) :: param_file_hdl
      !! Parameter file handle to opened file

    contains
      procedure, public :: init => init_Control
      procedure, public :: read => read_Control
      procedure, public :: cleanup => cleanup_control
      generic, public :: add_variable => add_variable_logical_1d, &
                                         add_variable_i32_1d, &
                                         add_variable_r32_1d, &
                                         add_variable_r64_1d
      generic, public :: read_restart_variable => read_restart_var_logical_1d, &
                                                  read_restart_var_i32_1d, &
                                                  read_restart_var_r32_1d, &
                                                  read_restart_var_r64_1d
      generic, public :: write_restart_variable => write_restart_var_logical_1d, &
                                                   write_restart_var_i32_0d, &
                                                   write_restart_var_i32_1d, &
                                                   write_restart_var_r32_0d, &
                                                   write_restart_var_r32_1d, &
                                                   write_restart_var_r64_1d

      procedure, private :: load_output_variables
      procedure, private :: open_model_output_file
      procedure, public :: add_dimension
      procedure, public :: add_time_dimension
      procedure, public :: create_restart_netcdf
      procedure, public :: open_restart_netcdf
      procedure, private :: add_variable_logical_1d
      procedure, private :: add_variable_i32_1d
      procedure, private :: add_variable_r32_1d
      procedure, private :: add_variable_r64_1d
      procedure, private :: read_restart_var_logical_1d
      ! procedure, private :: read_restart_var_i32_0d
      procedure, private :: read_restart_var_i32_1d
      ! procedure, private :: read_restart_var_r32_0d
      procedure, private :: read_restart_var_r32_1d
      procedure, private :: read_restart_var_r64_1d
      procedure, private :: write_restart_var_logical_1d
      procedure, private :: write_restart_var_i32_0d
      procedure, private :: write_restart_var_i32_1d
      procedure, private :: write_restart_var_r32_0d
      procedure, private :: write_restart_var_r32_1d
      procedure, private :: write_restart_var_r64_1d

      procedure, nopass, private :: err_check
      procedure, nopass, private :: timestamped_filename

  end type

  interface
    !! Overloaded interface to instantiate the class.
    module subroutine init_Control(this, control_filename)
      class(Control), intent(inout) :: this
        !! Control Class
      character(len=*), intent(in) :: control_filename
        !! File name to read the control parameters from.
    end subroutine
  end interface

  interface
    module subroutine read_Control(this)
      class(Control), intent(inout) :: this
        !! Control Class
    end subroutine
  end interface

  interface
    module subroutine load_output_variables(this)
      class(Control), intent(inout) :: this
    end subroutine
  end interface

  interface
    module function open_model_output_file(this)
      integer(i32) :: open_model_output_file
      class(Control), intent(inout) :: this
        !! Control class
    end function
  end interface

  ! interface
  !   module function open_var_save_file(this)
  !     !! Open the var_save_file (aka restart file)
  !     integer(i32) :: open_var_save_file
  !     class(Control), intent(inout) :: this
  !       !! Control class
  !   end function
  ! end interface

  interface
    module subroutine write_restart_var_logical_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      logical, intent(in) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine write_restart_var_i32_0d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer(i32), intent(in) :: data
    end subroutine
  end interface

  interface
    module subroutine write_restart_var_i32_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer(i32), intent(in) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine write_restart_var_r32_0d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real(r32), intent(in) :: data
    end subroutine
  end interface

  interface
    module subroutine write_restart_var_r32_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real(r32), intent(in) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine write_restart_var_r64_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real(r64), intent(in) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine add_dimension(this, dim_name, dim_size, dim_var_name, dim_var_longname, datatype, units)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: dim_name
      integer(i32), intent(in) :: dim_size
      character(len=*), intent(in) :: dim_var_name
      character(len=*), intent(in) :: dim_var_longname
      integer(i32), intent(in) :: datatype
      character(len=*), intent(in) :: units
    end subroutine
  end interface

  interface
    module subroutine add_time_dimension(this, dim_name, dim_size, dim_var_name, units, calendar)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: dim_name
      integer(i32), intent(in) :: dim_size
      character(len=*), intent(in) :: dim_var_name
      character(len=*), intent(in) :: units
      character(len=*), intent(in) :: calendar
    end subroutine
  end interface

  interface
    module subroutine add_variable_logical_1d(this, var_name, thevar, dim_name, units)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      logical, intent(in) :: thevar(:)
      character(len=*), intent(in) :: dim_name
      character(len=*), intent(in) :: units
    end subroutine
  end interface

  interface
    module subroutine add_variable_i32_1d(this, var_name, thevar, dim_name, units)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer(i32), intent(in) :: thevar(:)
      character(len=*), intent(in) :: dim_name
      character(len=*), intent(in) :: units
    end subroutine
  end interface

  interface
    module subroutine add_variable_r32_1d(this, var_name, thevar, dim_name, units)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real(r32), intent(in) :: thevar(:)
      character(len=*), intent(in) :: dim_name
      character(len=*), intent(in) :: units
    end subroutine
  end interface

  interface
    module subroutine add_variable_r64_1d(this, var_name, thevar, dim_name, units)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real(r64), intent(in) :: thevar(:)
      character(len=*), intent(in) :: dim_name
      character(len=*), intent(in) :: units
    end subroutine
  end interface


  interface
    module subroutine read_restart_var_logical_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      logical, pointer, intent(inout) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine read_restart_var_i32_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer(i32), pointer, intent(inout) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine read_restart_var_r32_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real(r32), pointer, intent(inout) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine read_restart_var_r64_1d(this, var_name, data)
      class(Control), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real(r64), pointer, intent(inout) :: data(:)
    end subroutine
  end interface

  interface
    module subroutine create_restart_netcdf(this)
      class(Control), intent(inout) :: this
    end subroutine
  end interface

  interface
    module subroutine err_check(status)
      integer(i32), intent(in) :: status
        !! The status returned by a netcdf call
    end subroutine
  end interface

  interface
    module subroutine open_restart_netcdf(this)
      class(Control), intent(inout) :: this
    end subroutine
  end interface

  interface
    module function timestamped_filename(prefix, timestamp) result(res)
      character(len=:), allocatable :: res
      character(len=*), intent(in) :: prefix
      integer(i32), intent(in) :: timestamp(6)
    end function
  end interface

  interface
    module subroutine cleanup_control(this)
      class(Control) :: this
        !! Srunoff class
    end subroutine
  end interface

end module
