module Parameters_class
  use variableKind
  use rVariable_class, only:rVariable
  use iVariable_class, only:iVariable
  use sVariable_class, only:sVariable
  use sArray_class, only:sArray
  use Control_class, only:Control

  private

  public :: Parameters

  type Parameters
    type(rVariable) :: K_coef
    type(rVariable) :: adjmix_rain
    type(rVariable) :: albset_rna
    type(rVariable) :: albset_rnm
    type(rVariable) :: albset_sna
    type(rVariable) :: albset_snm
    type(iVariable) :: basin_fall_frost
    type(iVariable) :: basin_spring_frost
    type(rVariable) :: carea_max
    type(rVariable) :: cecn_coef
    type(iVariable) :: cov_type
    type(rVariable) :: covden_sum
    type(rVariable) :: covden_win
    type(rVariable) :: dday_intcp
    type(rVariable) :: dday_slope
    type(rVariable) :: den_init
    type(rVariable) :: den_max
    type(rVariable) :: dprst_depth_avg
    type(rVariable) :: dprst_et_coef
    type(rVariable) :: dprst_flow_coef
    type(rVariable) :: dprst_frac
    type(rVariable) :: dprst_frac_init
    type(rVariable) :: dprst_frac_open
    type(rVariable) :: dprst_seep_rate_clos
    type(rVariable) :: dprst_seep_rate_open
    type(iVariable) :: elev_units
    type(rVariable) :: emis_noppt
    type(rVariable) :: epan_coef
    type(iVariable) :: fall_frost
    type(rVariable) :: fastcoef_lin
    type(rVariable) :: fastcoef_sq
    type(rVariable) :: freeh2o_cap
    type(rVariable) :: gwflow_coef
    type(rVariable) :: gwsink_coef
    type(rVariable) :: gwstor_init
    type(rVariable) :: gwstor_min
    type(rVariable) :: hru_area
    type(rVariable) :: hru_aspect
    type(iVariable) :: hru_deplcrv
    type(rVariable) :: hru_elev
    type(rVariable) :: hru_lat
    type(rVariable) :: hru_lon
    type(rVariable) :: hru_percent_imperv
    type(iVariable) :: hru_segment
    type(iVariable) :: hru_segment_nhm
    type(rVariable) :: hru_slope
    type(iVariable) :: hru_type
    type(rVariable) :: hru_x
    type(rVariable) :: hru_y
    type(rVariable) :: imperv_stor_max
    type(rVariable) :: jh_coef
    type(rVariable) :: jh_coef_hru
    type(iVariable) :: melt_force
    type(iVariable) :: melt_look
    type(iVariable) :: nhm_deplcrv
    type(iVariable) :: nhm_id
    type(iVariable) :: nhm_seg
    type(iVariable) :: obsin_segment
    type(rVariable) :: op_flow_thres
    type(iVariable) :: outlet_sta
    type(sVariable) :: poi_gage_id
    type(iVariable) :: poi_gage_segment
    type(iVariable) :: poi_type
    type(rVariable) :: potet_sublim
    type(rVariable) :: ppt_rad_adj
    type(iVariable) :: precip_units
    type(rVariable) :: pref_flow_den
    type(iVariable) :: print_freq
    type(iVariable) :: print_type
    type(rVariable) :: rad_trncf
    type(rVariable) :: radadj_intcp
    type(rVariable) :: radadj_slope
    type(rVariable) :: radj_sppt
    type(rVariable) :: radj_wppt
    type(rVariable) :: radmax
    type(rVariable) :: rain_cbh_adj
    type(iVariable) :: runoff_units
    type(rVariable) :: sat_threshold
    type(rVariable) :: segment_flow_init
    type(iVariable) :: segment_type
    type(rVariable) :: settle_const
    type(rVariable) :: slowcoef_lin
    type(rVariable) :: slowcoef_sq
    type(rVariable) :: smidx_coef
    type(rVariable) :: smidx_exp
    type(rVariable) :: snarea_curve
    type(rVariable) :: snarea_thresh
    type(rVariable) :: snow_cbh_adj
    type(rVariable) :: snow_intcp
    type(rVariable) :: snowinfil_max
    type(rVariable) :: snowpack_init
    type(rVariable) :: soil2gw_max
    type(rVariable) :: soil_moist_init
    type(rVariable) :: soil_moist_init_frac
    type(rVariable) :: soil_moist_max
    type(rVariable) :: soil_rechr_init
    type(rVariable) :: soil_rechr_init_frac
    type(rVariable) :: soil_rechr_max_frac
    type(iVariable) :: soil_type
    type(iVariable) :: spring_frost
    type(rVariable) :: srain_intcp
    type(rVariable) :: sro_to_dprst_imperv
    type(rVariable) :: sro_to_dprst_perv
    type(rVariable) :: ssr2gw_exp
    type(rVariable) :: ssr2gw_rate
    type(rVariable) :: ssstor_init
    type(rVariable) :: ssstor_init_frac
    type(iVariable) :: temp_units
    type(rVariable) :: tmax_allrain_offset
    type(rVariable) :: tmax_allsnow
    type(rVariable) :: tmax_cbh_adj
    type(rVariable) :: tmax_index
    type(rVariable) :: tmin_cbh_adj
    type(iVariable) :: tosegment
    type(iVariable) :: tosegment_nhm
    type(iVariable) :: transp_beg
    type(iVariable) :: transp_end
    type(rVariable) :: transp_tmax
    type(iVariable) :: tstorm_mo
    type(rVariable) :: va_clos_exp
    type(rVariable) :: va_open_exp
    type(rVariable) :: wrain_intcp
    type(rVariable) :: x_coef

    type(sArray), private :: parameter_filenames
  contains
    procedure, public :: read => read_Parameters
  end type

  interface Parameters
    !! Overloaded interface to instantiate the class.
    module function constructor_Parameters(Control_data) result(this)
      use Control_class, only:Control

      type(Parameters) :: this
        !! Parameter class
      class(Control), intent(in) :: Control_data
        !! Control class - contains information needed to open and read the parameter file
    end function
  end interface

  interface
    module subroutine read_parameters(this)
      class(Parameters), intent(inout) :: this
        !! Parameter class
    end subroutine
  end interface

end module
