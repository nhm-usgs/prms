module Parameters_class
  use variableKind
  use rVariable_class, only: rVariable
  use iVariable_class, only: iVariable
  use sVariable_class, only: sVariable
  use sArray_class, only: sArray
  use Control_class, only: Control

  private

  public :: Parameters

  type Parameters
    type(rVariable) :: K_coef
    type(rVariable) :: adjmix_rain
    type(rVariable) :: adjust_rain
    type(rVariable) :: adjust_snow
    type(rVariable) :: albset_rna
    type(rVariable) :: albset_rnm
    type(rVariable) :: albset_sna
    type(rVariable) :: albset_snm
    type(iVariable) :: basin_tsta
    type(rVariable) :: carea_max
    type(rVariable) :: carea_min
    type(rVariable) :: ccov_intcp
    type(rVariable) :: ccov_slope
    type(rVariable) :: cecn_coef
    type(iVariable) :: conv_flag
    type(iVariable) :: cov_type
    type(rVariable) :: covden_sum
    type(rVariable) :: covden_win
    type(rVariable) :: crad_coef
    type(rVariable) :: crad_exp
    type(rVariable) :: dday_intcp
    type(rVariable) :: dday_slope
    type(rVariable) :: den_init
    type(rVariable) :: den_max
    type(rVariable) :: dist_exp
    type(rVariable) :: dist_max
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
    type(rVariable) :: hamon_coef
    type(rVariable) :: hru_area
    type(rVariable) :: hru_aspect
    type(iVariable) :: hru_deplcrv
    type(rVariable) :: hru_elev
    type(rVariable) :: hru_lat
    type(rVariable) :: hru_lon
    type(iVariable) :: hru_pansta
    type(rVariable) :: hru_percent_imperv
    type(iVariable) :: hru_plaps
    type(iVariable) :: hru_psta
    type(iVariable) :: hru_segment
    type(rVariable) :: hru_slope
    type(iVariable) :: hru_tlaps
    type(iVariable) :: hru_tsta
    type(iVariable) :: hru_type
    type(rVariable) :: hru_x
    type(rVariable) :: hru_xlong
    type(rVariable) :: hru_y
    type(rVariable) :: hru_ylat
    type(rVariable) :: imperv_stor_max
    type(rVariable) :: jh_coef
    type(rVariable) :: jh_coef_hru
    type(rVariable) :: lapsemax_max
    type(rVariable) :: lapsemax_min
    type(rVariable) :: lapsemin_max
    type(rVariable) :: lapsemin_min
    type(rVariable) :: max_lapse
    type(iVariable) :: max_missing
    type(iVariable) :: max_psta
    type(iVariable) :: max_tsta
    type(rVariable) :: maxday_prec
    type(iVariable) :: melt_force
    type(iVariable) :: melt_look
    type(rVariable) :: min_lapse
    type(rVariable) :: monmax
    type(rVariable) :: monmin
    type(iVariable) :: ndist_psta
    type(iVariable) :: ndist_tsta
    type(iVariable) :: nhm_id
    type(iVariable) :: nhm_seg
    type(iVariable) :: obsin_segment
    type(iVariable) :: obsout_segment
    type(rVariable) :: op_flow_thres
    type(iVariable) :: outlet_sta
    type(rVariable) :: padj_rn
    type(rVariable) :: padj_sn
    type(iVariable) :: parent_gw
    type(iVariable) :: parent_hru
    type(iVariable) :: parent_poigages
    type(iVariable) :: parent_segment
    type(iVariable) :: parent_ssr
    type(rVariable) :: pmn_mo
    type(sVariable) :: poi_gage_id
    type(iVariable) :: poi_gage_segment
    type(iVariable) :: poi_type
    type(rVariable) :: potet_cbh_adj
    type(rVariable) :: potet_sublim
    type(rVariable) :: ppt_add
    type(rVariable) :: ppt_div
    type(rVariable) :: ppt_lapse
    type(rVariable) :: ppt_rad_adj
    type(rVariable) :: prcp_wght_dist
    type(iVariable) :: precip_units
    type(rVariable) :: pref_flow_den
    type(iVariable) :: print_freq
    type(iVariable) :: print_type
    type(rVariable) :: psta_elev
    type(iVariable) :: psta_freq_nuse
    type(rVariable) :: psta_mon
    type(rVariable) :: psta_month_ppt
    type(iVariable) :: psta_nuse
    type(rVariable) :: psta_x
    type(rVariable) :: psta_xlong
    type(rVariable) :: psta_y
    type(rVariable) :: psta_ylat
    type(rVariable) :: rad_trncf
    type(rVariable) :: radadj_intcp
    type(rVariable) :: radadj_slope
    type(rVariable) :: radj_sppt
    type(rVariable) :: radj_wppt
    type(rVariable) :: radmax
    type(rVariable) :: rain_adj
    type(rVariable) :: rain_cbh_adj
    type(iVariable) :: rain_code
    type(rVariable) :: rain_mon
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
    type(rVariable) :: snow_adj
    type(rVariable) :: snow_cbh_adj
    type(rVariable) :: snow_intcp
    type(rVariable) :: snow_mon
    type(rVariable) :: snowinfil_max
    type(rVariable) :: snowpack_init
    type(rVariable) :: soil2gw_max
    type(rVariable) :: soil_moist_init_frac
    type(rVariable) :: soil_moist_max
    type(rVariable) :: soil_rechr_init_frac
    type(rVariable) :: soil_rechr_max_frac
    type(iVariable) :: soil_type
    type(rVariable) :: solrad_elev
    type(iVariable) :: spring_frost
    type(rVariable) :: srain_intcp
    type(rVariable) :: sro_to_dprst_imperv
    type(rVariable) :: sro_to_dprst_perv
    type(rVariable) :: ssr2gw_exp
    type(rVariable) :: ssr2gw_rate
    type(rVariable) :: ssstor_init_frac
    type(iVariable) :: temp_units
    type(rVariable) :: temp_wght_dist
    type(rVariable) :: tmax_add
    type(rVariable) :: tmax_adj
    type(rVariable) :: tmax_allrain_dist
    type(rVariable) :: tmax_allrain_offset
    type(rVariable) :: tmax_allrain_sta
    type(rVariable) :: tmax_allsnow
    type(rVariable) :: tmax_allsnow_dist
    type(rVariable) :: tmax_allsnow_sta
    type(rVariable) :: tmax_cbh_adj
    type(rVariable) :: tmax_div
    type(rVariable) :: tmax_index
    type(rVariable) :: tmax_lapse
    type(rVariable) :: tmin_add
    type(rVariable) :: tmin_adj
    type(rVariable) :: tmin_cbh_adj
    type(rVariable) :: tmin_div
    type(rVariable) :: tmin_lapse
    type(iVariable) :: tosegment
    type(iVariable) :: tosegment_nhm
    type(iVariable) :: transp_beg
    type(iVariable) :: transp_end
    type(rVariable) :: transp_tmax
    type(rVariable) :: tsta_elev
    type(rVariable) :: tsta_month_max
    type(rVariable) :: tsta_month_min
    type(iVariable) :: tsta_nuse
    type(rVariable) :: tsta_x
    type(rVariable) :: tsta_xlong
    type(rVariable) :: tsta_y
    type(rVariable) :: tsta_ylat
    type(iVariable) :: tstorm_mo
    type(rVariable) :: va_clos_exp
    type(rVariable) :: va_open_exp
    type(rVariable) :: wrain_intcp
    type(rVariable) :: x_add
    type(rVariable) :: x_coef
    type(rVariable) :: x_div
    type(rVariable) :: y_add
    type(rVariable) :: y_div
    type(rVariable) :: z_add
    type(rVariable) :: z_div

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
