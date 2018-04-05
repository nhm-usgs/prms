module Control_class
  use variableKind
  use m_fileIO, only: openFile, closeFile
  use rArray_class, only: rArray
  use iArray_class, only: iArray
  use sArray_class, only: sArray
  implicit none

  private
  public :: Control

  character(len=*), parameter :: MODDESC = 'Control File'
  character(len=*), parameter :: MODNAME = 'Control_class'
  character(len=*), parameter :: MODVERSION = '2018-04-05 13:50:00Z'

  type Control
    ! Allowed dimensions
    type(iArray) :: ncascade
    type(iArray) :: ncascdgw
    type(iArray) :: nconsumed
    type(iArray) :: ndays
    type(iArray) :: ndepl
    type(iArray) :: ndeplval
    type(iArray) :: nevap
    type(iArray) :: nexternal
    type(iArray) :: ngw
    type(iArray) :: ngwcell
    type(iArray) :: nhru
    type(iArray) :: nhrucell
    type(iArray) :: nhumid
    type(iArray) :: nlake
    type(iArray) :: nlakeelev
    type(iArray) :: nlapse
    type(iArray) :: nmonths
    type(iArray) :: nobs
    type(iArray) :: npoigages
    type(iArray) :: nrain
    type(iArray) :: nratetbl
    type(iArray) :: nsegment
    type(iArray) :: nsnow
    type(iArray) :: nsol
    type(iArray) :: nssr
    type(iArray) :: nsub
    type(iArray) :: ntemp
    type(iArray) :: nwateruse
    type(iArray) :: nwind
    type(iArray) :: one

    ! All other control file parameters
    type(iArray) :: aniOutON_OFF
    type(sArray) :: aniOutVar_names
    type(sArray) :: ani_output_file
    type(sArray) :: basinOutBaseFileName
    type(iArray) :: basinOutON_OFF
    type(sArray) :: basinOutVar_names
    type(iArray) :: basinOutVars
    type(iArray) :: basinOut_freq
    type(iArray) :: canopy_transferON_OFF
    type(sArray) :: capillary_module
    type(iArray) :: cascade_flag
    type(iArray) :: cascadegw_flag
    type(iArray) :: cbh_binary_flag
    type(iArray) :: cbh_check_flag
    type(iArray) :: consumed_transferON_OFF
    type(sArray) :: covden_sum_dynamic
    type(sArray) :: covden_win_dynamic
    type(sArray) :: covtype_dynamic
    type(sArray) :: creator_email
    type(iArray) :: csvON_OFF
    type(sArray) :: csv_output_file
    type(sArray) :: data_file
    type(iArray) :: dispGraphsBuffSize
    type(sArray) :: dprst_area_dynamic
    type(sArray) :: dprst_depth_dynamic
    type(iArray) :: dprst_flag
    type(sArray) :: dprst_frac_dynamic
    type(iArray) :: dprst_transferON_OFF
    type(iArray) :: dyn_covden_flag
    type(iArray) :: dyn_covtype_flag
    type(iArray) :: dyn_dprst_flag
    type(iArray) :: dyn_fallfrost_flag
    type(iArray) :: dyn_imperv_flag
    type(iArray) :: dyn_intcp_flag
    type(iArray) :: dyn_potet_flag
    type(iArray) :: dyn_radtrncf_flag
    type(iArray) :: dyn_snareathresh_flag
    type(iArray) :: dyn_soil_flag
    type(iArray) :: dyn_springfrost_flag
    type(iArray) :: dyn_sro2dprst_imperv_flag
    type(iArray) :: dyn_sro2dprst_perv_flag
    type(iArray) :: dyn_sro_to_dprst_flag
    type(iArray) :: dyn_sro_to_imperv_flag
    type(iArray) :: dyn_transp_flag
    type(iArray) :: dyn_transp_on_flag
    type(iArray) :: end_time
    type(sArray) :: et_module
    type(sArray) :: executable_desc
    type(sArray) :: executable_model
    type(iArray) :: external_transferON_OFF
    type(sArray) :: fallfrost_dynamic
    type(iArray) :: frozen_flag
    type(iArray) :: glacier_flag
    type(iArray) :: gsf_rpt
    type(sArray) :: gsflow_csv_file
    type(sArray) :: gsflow_output_file
    type(iArray) :: gwflow_cbh_flag
    type(iArray) :: gwr_swale_flag
    type(iArray) :: gwr_transferON_OFF
    type(sArray) :: gwres_flow_day
    type(iArray) :: humidity_cbh_flag
    type(sArray) :: humidity_day
    type(iArray) :: ignore_data_file_end
    type(sArray) :: imperv_frac_dynamic
    type(sArray) :: imperv_stor_dynamic
    type(iArray) :: init_vars_from_file
    type(rArray) :: initial_deltat
    type(sArray) :: jhcoef_dynamic
    type(iArray) :: lake_transferON_OFF
    type(iArray) :: mapOutON_OFF
    type(sArray) :: mapOutVar_names
    type(iArray) :: mbInit_flag
    type(sArray) :: model_mode
    type(sArray) :: model_output_file
    type(sArray) :: modflow_name
    type(iArray) :: modflow_time_zero
    type(iArray) :: musroute_flag
    type(iArray) :: naniOutVars
    type(iArray) :: ndispGraphs
    type(sArray) :: nhruOutBaseFileName
    type(iArray) :: nhruOutON_OFF
    type(sArray) :: nhruOutVar_names
    type(iArray) :: nhruOutVars
    type(iArray) :: nhruOut_freq
    type(iArray) :: nmapOutVars
    type(sArray) :: nsegmentOutBaseFileName
    type(iArray) :: nsegmentOutON_OFF
    type(iArray) :: nsegmentOutVars
    type(iArray) :: nsegmentOut_freq
    type(iArray) :: nstatVars
    type(sArray) :: nsubOutBaseFileName
    type(iArray) :: nsubOutON_OFF
    type(sArray) :: nsubOutVar_names
    type(iArray) :: nsubOutVars
    type(iArray) :: nsubOut_freq
    type(iArray) :: orad_flag
    type(sArray) :: param_file
    type(iArray) :: parameter_check_flag
    type(sArray) :: pk_depth_day
    type(sArray) :: pkwater_equiv_day
    type(sArray) :: potet_coef_dynamic
    type(sArray) :: potet_day
    type(sArray) :: precip_day
    type(sArray) :: precip_module
    type(iArray) :: print_debug
    type(iArray) :: prms_warmup
    type(sArray) :: radtrncf_dynamic
    type(iArray) :: rpt_days
    type(iArray) :: save_vars_to_file
    type(iArray) :: seg2hru_flag
    type(iArray) :: segmentOutON_OFF
    type(sArray) :: segmentOutVar_names
    type(iArray) :: segment_transferON_OFF
    type(iArray) :: snow_cbh_flag
    type(sArray) :: snow_evap_day
    type(sArray) :: snow_intcp_dynamic
    type(sArray) :: snowcov_area_day
    type(sArray) :: snowmelt_day
    type(sArray) :: soilmoist_dynamic
    type(sArray) :: soilrechr_dynamic
    type(sArray) :: soilzone_module
    type(iArray) :: soilzone_transferON_OFF
    type(sArray) :: solrad_module
    type(sArray) :: springfrost_dynamic
    type(sArray) :: srain_intcp_dynamic
    type(sArray) :: sro2dprst_imperv_dynamic
    type(sArray) :: sro2dprst_perv_dynamic
    type(sArray) :: srunoff_module
    type(iArray) :: start_time
    type(sArray) :: stat_var_file
    type(iArray) :: statsON_OFF
    type(sArray) :: stats_output_file
    type(iArray) :: stream_temp_flag
    type(iArray) :: stream_temp_shade_flag
    type(sArray) :: strmflow_module
    type(iArray) :: subbasin_flag
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
    type(iArray) :: windspeed_cbh_flag
    type(sArray) :: windspeed_day
    type(sArray) :: wrain_intcp_dynamic

    ! Non-control file variables
    integer(i32) :: model_output_unit
      !! File unit for opened model_output_file
    character(len=:), allocatable, private :: Version_read_control_file
    character(len=:), allocatable, private :: control_filename

  contains
    procedure, public :: read => read_Control
    procedure, private :: open_model_output_file
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

  interface
    module function open_model_output_file(this)
      integer(i32) :: open_model_output_file
      class(Control), intent(inout) :: this
        !! Control class
    end function
  end interface
end module
