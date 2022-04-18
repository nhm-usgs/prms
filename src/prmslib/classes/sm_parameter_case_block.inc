select case(buf)
  case('K_coef')
    this%K_coef%name = last
    call this%K_coef%read(iUnit)
    line = line + this%K_coef%size() + this%K_coef%dim_names%size() + ENTRY_OFFSET
  case('Mann_n')
    this%Mann_n%name = last
    call this%Mann_n%read(iUnit)
    line = line + this%Mann_n%size() + this%Mann_n%dim_names%size() + ENTRY_OFFSET
  case('adjmix_rain')
    this%adjmix_rain%name = last
    call this%adjmix_rain%read(iUnit)
    line = line + this%adjmix_rain%size() + this%adjmix_rain%dim_names%size() + ENTRY_OFFSET
  case('albedo')
    this%albedo%name = last
    call this%albedo%read(iUnit)
    line = line + this%albedo%size() + this%albedo%dim_names%size() + ENTRY_OFFSET
  case('albset_rna')
    this%albset_rna%name = last
    call this%albset_rna%read(iUnit)
    line = line + this%albset_rna%size() + this%albset_rna%dim_names%size() + ENTRY_OFFSET
  case('albset_rnm')
    this%albset_rnm%name = last
    call this%albset_rnm%read(iUnit)
    line = line + this%albset_rnm%size() + this%albset_rnm%dim_names%size() + ENTRY_OFFSET
  case('albset_sna')
    this%albset_sna%name = last
    call this%albset_sna%read(iUnit)
    line = line + this%albset_sna%size() + this%albset_sna%dim_names%size() + ENTRY_OFFSET
  case('albset_snm')
    this%albset_snm%name = last
    call this%albset_snm%read(iUnit)
    line = line + this%albset_snm%size() + this%albset_snm%dim_names%size() + ENTRY_OFFSET
  case('alte')
    this%alte%name = last
    call this%alte%read(iUnit)
    line = line + this%alte%size() + this%alte%dim_names%size() + ENTRY_OFFSET
  case('altw')
    this%altw%name = last
    call this%altw%read(iUnit)
    line = line + this%altw%size() + this%altw%dim_names%size() + ENTRY_OFFSET
  case('azrh')
    this%azrh%name = last
    call this%azrh%read(iUnit)
    line = line + this%azrh%size() + this%azrh%dim_names%size() + ENTRY_OFFSET
  case('basin_solsta')
    this%basin_solsta%name = last
    call this%basin_solsta%read(iUnit)
    line = line + this%basin_solsta%size() + this%basin_solsta%dim_names%size() + ENTRY_OFFSET
  case('basin_tsta')
    this%basin_tsta%name = last
    call this%basin_tsta%read(iUnit)
    line = line + this%basin_tsta%size() + this%basin_tsta%dim_names%size() + ENTRY_OFFSET
  case('carea_max')
    this%carea_max%name = last
    call this%carea_max%read(iUnit)
    line = line + this%carea_max%size() + this%carea_max%dim_names%size() + ENTRY_OFFSET
  case('carea_min')
    this%carea_min%name = last
    call this%carea_min%read(iUnit)
    line = line + this%carea_min%size() + this%carea_min%dim_names%size() + ENTRY_OFFSET
  case('cascade_flg')
    this%cascade_flg%name = last
    call this%cascade_flg%read(iUnit)
    line = line + this%cascade_flg%size() + this%cascade_flg%dim_names%size() + ENTRY_OFFSET
  case('cascade_tol')
    this%cascade_tol%name = last
    call this%cascade_tol%read(iUnit)
    line = line + this%cascade_tol%size() + this%cascade_tol%dim_names%size() + ENTRY_OFFSET
  case('ccov_intcp')
    this%ccov_intcp%name = last
    call this%ccov_intcp%read(iUnit)
    line = line + this%ccov_intcp%size() + this%ccov_intcp%dim_names%size() + ENTRY_OFFSET
  case('ccov_slope')
    this%ccov_slope%name = last
    call this%ccov_slope%read(iUnit)
    line = line + this%ccov_slope%size() + this%ccov_slope%dim_names%size() + ENTRY_OFFSET
  case('cecn_coef')
    this%cecn_coef%name = last
    call this%cecn_coef%read(iUnit)
    line = line + this%cecn_coef%size() + this%cecn_coef%dim_names%size() + ENTRY_OFFSET
  case('circle_switch')
    this%circle_switch%name = last
    call this%circle_switch%read(iUnit)
    line = line + this%circle_switch%size() + this%circle_switch%dim_names%size() + ENTRY_OFFSET
  case('cov_type')
    this%cov_type%name = last
    call this%cov_type%read(iUnit)
    line = line + this%cov_type%size() + this%cov_type%dim_names%size() + ENTRY_OFFSET
  case('covden_sum')
    this%covden_sum%name = last
    call this%covden_sum%read(iUnit)
    line = line + this%covden_sum%size() + this%covden_sum%dim_names%size() + ENTRY_OFFSET
  case('covden_win')
    this%covden_win%name = last
    call this%covden_win%read(iUnit)
    line = line + this%covden_win%size() + this%covden_win%dim_names%size() + ENTRY_OFFSET
  case('crad_coef')
    this%crad_coef%name = last
    call this%crad_coef%read(iUnit)
    line = line + this%crad_coef%size() + this%crad_coef%dim_names%size() + ENTRY_OFFSET
  case('crad_exp')
    this%crad_exp%name = last
    call this%crad_exp%read(iUnit)
    line = line + this%crad_exp%size() + this%crad_exp%dim_names%size() + ENTRY_OFFSET
  case('crop_coef')
    this%crop_coef%name = last
    call this%crop_coef%read(iUnit)
    line = line + this%crop_coef%size() + this%crop_coef%dim_names%size() + ENTRY_OFFSET
  case('dday_intcp')
    this%dday_intcp%name = last
    call this%dday_intcp%read(iUnit)
    line = line + this%dday_intcp%size() + this%dday_intcp%dim_names%size() + ENTRY_OFFSET
  case('dday_slope')
    this%dday_slope%name = last
    call this%dday_slope%read(iUnit)
    line = line + this%dday_slope%size() + this%dday_slope%dim_names%size() + ENTRY_OFFSET
  case('den_init')
    this%den_init%name = last
    call this%den_init%read(iUnit)
    line = line + this%den_init%size() + this%den_init%dim_names%size() + ENTRY_OFFSET
  case('den_max')
    this%den_max%name = last
    call this%den_max%read(iUnit)
    line = line + this%den_max%size() + this%den_max%dim_names%size() + ENTRY_OFFSET
  case('dist_max')
    this%dist_max%name = last
    call this%dist_max%read(iUnit)
    line = line + this%dist_max%size() + this%dist_max%dim_names%size() + ENTRY_OFFSET
  case('dprst_area')
    this%dprst_area%name = last
    call this%dprst_area%read(iUnit)
    line = line + this%dprst_area%size() + this%dprst_area%dim_names%size() + ENTRY_OFFSET
  case('dprst_depth_avg')
    this%dprst_depth_avg%name = last
    call this%dprst_depth_avg%read(iUnit)
    line = line + this%dprst_depth_avg%size() + this%dprst_depth_avg%dim_names%size() + ENTRY_OFFSET
  case('dprst_et_coef')
    this%dprst_et_coef%name = last
    call this%dprst_et_coef%read(iUnit)
    line = line + this%dprst_et_coef%size() + this%dprst_et_coef%dim_names%size() + ENTRY_OFFSET
  case('dprst_flow_coef')
    this%dprst_flow_coef%name = last
    call this%dprst_flow_coef%read(iUnit)
    line = line + this%dprst_flow_coef%size() + this%dprst_flow_coef%dim_names%size() + ENTRY_OFFSET
  case('dprst_frac')
    this%dprst_frac%name = last
    call this%dprst_frac%read(iUnit)
    line = line + this%dprst_frac%size() + this%dprst_frac%dim_names%size() + ENTRY_OFFSET
  case('dprst_frac_hru')
    this%dprst_frac_hru%name = last
    call this%dprst_frac_hru%read(iUnit)
    line = line + this%dprst_frac_hru%size() + this%dprst_frac_hru%dim_names%size() + ENTRY_OFFSET
  case('dprst_frac_init')
    this%dprst_frac_init%name = last
    call this%dprst_frac_init%read(iUnit)
    line = line + this%dprst_frac_init%size() + this%dprst_frac_init%dim_names%size() + ENTRY_OFFSET
  case('dprst_frac_open')
    this%dprst_frac_open%name = last
    call this%dprst_frac_open%read(iUnit)
    line = line + this%dprst_frac_open%size() + this%dprst_frac_open%dim_names%size() + ENTRY_OFFSET
  case('dprst_seep_rate_clos')
    this%dprst_seep_rate_clos%name = last
    call this%dprst_seep_rate_clos%read(iUnit)
    line = line + this%dprst_seep_rate_clos%size() + this%dprst_seep_rate_clos%dim_names%size() + ENTRY_OFFSET
  case('dprst_seep_rate_open')
    this%dprst_seep_rate_open%name = last
    call this%dprst_seep_rate_open%read(iUnit)
    line = line + this%dprst_seep_rate_open%size() + this%dprst_seep_rate_open%dim_names%size() + ENTRY_OFFSET
  case('elev_outflow')
    this%elev_outflow%name = last
    call this%elev_outflow%read(iUnit)
    line = line + this%elev_outflow%size() + this%elev_outflow%dim_names%size() + ENTRY_OFFSET
  case('elev_units')
    this%elev_units%name = last
    call this%elev_units%read(iUnit)
    line = line + this%elev_units%size() + this%elev_units%dim_names%size() + ENTRY_OFFSET
  case('elevlake_init')
    this%elevlake_init%name = last
    call this%elevlake_init%read(iUnit)
    line = line + this%elevlake_init%size() + this%elevlake_init%dim_names%size() + ENTRY_OFFSET
  case('emis_noppt')
    this%emis_noppt%name = last
    call this%emis_noppt%read(iUnit)
    line = line + this%emis_noppt%size() + this%emis_noppt%dim_names%size() + ENTRY_OFFSET
  case('epan_coef')
    this%epan_coef%name = last
    call this%epan_coef%read(iUnit)
    line = line + this%epan_coef%size() + this%epan_coef%dim_names%size() + ENTRY_OFFSET
  case('fall_frost')
    this%fall_frost%name = last
    call this%fall_frost%read(iUnit)
    line = line + this%fall_frost%size() + this%fall_frost%dim_names%size() + ENTRY_OFFSET
  case('fastcoef_lin')
    this%fastcoef_lin%name = last
    call this%fastcoef_lin%read(iUnit)
    line = line + this%fastcoef_lin%size() + this%fastcoef_lin%dim_names%size() + ENTRY_OFFSET
  case('fastcoef_sq')
    this%fastcoef_sq%name = last
    call this%fastcoef_sq%read(iUnit)
    line = line + this%fastcoef_sq%size() + this%fastcoef_sq%dim_names%size() + ENTRY_OFFSET
  case('freeh2o_cap')
    this%freeh2o_cap%name = last
    call this%freeh2o_cap%read(iUnit)
    line = line + this%freeh2o_cap%size() + this%freeh2o_cap%dim_names%size() + ENTRY_OFFSET
  case('frost_temp')
    this%frost_temp%name = last
    call this%frost_temp%read(iUnit)
    line = line + this%frost_temp%size() + this%frost_temp%dim_names%size() + ENTRY_OFFSET
  case('gvr_cell_id')
    this%gvr_cell_id%name = last
    call this%gvr_cell_id%read(iUnit)
    line = line + this%gvr_cell_id%size() + this%gvr_cell_id%dim_names%size() + ENTRY_OFFSET
  case('gvr_cell_pct')
    this%gvr_cell_pct%name = last
    call this%gvr_cell_pct%read(iUnit)
    line = line + this%gvr_cell_pct%size() + this%gvr_cell_pct%dim_names%size() + ENTRY_OFFSET
  case('gvr_hru_id')
    this%gvr_hru_id%name = last
    call this%gvr_hru_id%read(iUnit)
    line = line + this%gvr_hru_id%size() + this%gvr_hru_id%dim_names%size() + ENTRY_OFFSET
  case('gw_down_id')
    this%gw_down_id%name = last
    call this%gw_down_id%read(iUnit)
    line = line + this%gw_down_id%size() + this%gw_down_id%dim_names%size() + ENTRY_OFFSET
  case('gw_init')
    this%gw_init%name = last
    call this%gw_init%read(iUnit)
    line = line + this%gw_init%size() + this%gw_init%dim_names%size() + ENTRY_OFFSET
  case('gw_pct_up')
    this%gw_pct_up%name = last
    call this%gw_pct_up%read(iUnit)
    line = line + this%gw_pct_up%size() + this%gw_pct_up%dim_names%size() + ENTRY_OFFSET
  case('gw_seep_coef')
    this%gw_seep_coef%name = last
    call this%gw_seep_coef%read(iUnit)
    line = line + this%gw_seep_coef%size() + this%gw_seep_coef%dim_names%size() + ENTRY_OFFSET
  case('gw_strmseg_down_id')
    this%gw_strmseg_down_id%name = last
    call this%gw_strmseg_down_id%read(iUnit)
    line = line + this%gw_strmseg_down_id%size() + this%gw_strmseg_down_id%dim_names%size() + ENTRY_OFFSET
  case('gw_tau')
    this%gw_tau%name = last
    call this%gw_tau%read(iUnit)
    line = line + this%gw_tau%size() + this%gw_tau%dim_names%size() + ENTRY_OFFSET
  case('gw_up_id')
    this%gw_up_id%name = last
    call this%gw_up_id%read(iUnit)
    line = line + this%gw_up_id%size() + this%gw_up_id%dim_names%size() + ENTRY_OFFSET
  case('gwflow_coef')
    this%gwflow_coef%name = last
    call this%gwflow_coef%read(iUnit)
    line = line + this%gwflow_coef%size() + this%gwflow_coef%dim_names%size() + ENTRY_OFFSET
  case('gwsink_coef')
    this%gwsink_coef%name = last
    call this%gwsink_coef%read(iUnit)
    line = line + this%gwsink_coef%size() + this%gwsink_coef%dim_names%size() + ENTRY_OFFSET
  case('gwstor_init')
    this%gwstor_init%name = last
    call this%gwstor_init%read(iUnit)
    line = line + this%gwstor_init%size() + this%gwstor_init%dim_names%size() + ENTRY_OFFSET
  case('gwstor_min')
    this%gwstor_min%name = last
    call this%gwstor_min%read(iUnit)
    line = line + this%gwstor_min%size() + this%gwstor_min%dim_names%size() + ENTRY_OFFSET
  case('hamon_coef')
    this%hamon_coef%name = last
    call this%hamon_coef%read(iUnit)
    line = line + this%hamon_coef%size() + this%hamon_coef%dim_names%size() + ENTRY_OFFSET
  case('hru_area')
    this%hru_area%name = last
    call this%hru_area%read(iUnit)
    line = line + this%hru_area%size() + this%hru_area%dim_names%size() + ENTRY_OFFSET
  case('hru_aspect')
    this%hru_aspect%name = last
    call this%hru_aspect%read(iUnit)
    line = line + this%hru_aspect%size() + this%hru_aspect%dim_names%size() + ENTRY_OFFSET
  case('hru_deplcrv')
    this%hru_deplcrv%name = last
    call this%hru_deplcrv%read(iUnit)
    line = line + this%hru_deplcrv%size() + this%hru_deplcrv%dim_names%size() + ENTRY_OFFSET
  case('hru_down_id')
    this%hru_down_id%name = last
    call this%hru_down_id%read(iUnit)
    line = line + this%hru_down_id%size() + this%hru_down_id%dim_names%size() + ENTRY_OFFSET
  case('hru_elev')
    this%hru_elev%name = last
    call this%hru_elev%read(iUnit)
    line = line + this%hru_elev%size() + this%hru_elev%dim_names%size() + ENTRY_OFFSET
  case('hru_humidity_sta')
    this%hru_humidity_sta%name = last
    call this%hru_humidity_sta%read(iUnit)
    line = line + this%hru_humidity_sta%size() + this%hru_humidity_sta%dim_names%size() + ENTRY_OFFSET
  case('hru_lat')
    this%hru_lat%name = last
    call this%hru_lat%read(iUnit)
    line = line + this%hru_lat%size() + this%hru_lat%dim_names%size() + ENTRY_OFFSET
  case('hru_lon')
    this%hru_lon%name = last
    call this%hru_lon%read(iUnit)
    line = line + this%hru_lon%size() + this%hru_lon%dim_names%size() + ENTRY_OFFSET
  case('hru_pansta')
    this%hru_pansta%name = last
    call this%hru_pansta%read(iUnit)
    line = line + this%hru_pansta%size() + this%hru_pansta%dim_names%size() + ENTRY_OFFSET
  case('hru_pct_up')
    this%hru_pct_up%name = last
    call this%hru_pct_up%read(iUnit)
    line = line + this%hru_pct_up%size() + this%hru_pct_up%dim_names%size() + ENTRY_OFFSET
  case('hru_percent_imperv')
    this%hru_percent_imperv%name = last
    call this%hru_percent_imperv%read(iUnit)
    line = line + this%hru_percent_imperv%size() + this%hru_percent_imperv%dim_names%size() + ENTRY_OFFSET
  case('hru_plaps')
    this%hru_plaps%name = last
    call this%hru_plaps%read(iUnit)
    line = line + this%hru_plaps%size() + this%hru_plaps%dim_names%size() + ENTRY_OFFSET
  case('hru_psta')
    this%hru_psta%name = last
    call this%hru_psta%read(iUnit)
    line = line + this%hru_psta%size() + this%hru_psta%dim_names%size() + ENTRY_OFFSET
  case('hru_segment')
    this%hru_segment%name = last
    call this%hru_segment%read(iUnit)
    line = line + this%hru_segment%size() + this%hru_segment%dim_names%size() + ENTRY_OFFSET
  case('hru_slope')
    this%hru_slope%name = last
    call this%hru_slope%read(iUnit)
    line = line + this%hru_slope%size() + this%hru_slope%dim_names%size() + ENTRY_OFFSET
  case('hru_solsta')
    this%hru_solsta%name = last
    call this%hru_solsta%read(iUnit)
    line = line + this%hru_solsta%size() + this%hru_solsta%dim_names%size() + ENTRY_OFFSET
  case('hru_strmseg_down_id')
    this%hru_strmseg_down_id%name = last
    call this%hru_strmseg_down_id%read(iUnit)
    line = line + this%hru_strmseg_down_id%size() + this%hru_strmseg_down_id%dim_names%size() + ENTRY_OFFSET
  case('hru_subbasin')
    this%hru_subbasin%name = last
    call this%hru_subbasin%read(iUnit)
    line = line + this%hru_subbasin%size() + this%hru_subbasin%dim_names%size() + ENTRY_OFFSET
  case('hru_tlaps')
    this%hru_tlaps%name = last
    call this%hru_tlaps%read(iUnit)
    line = line + this%hru_tlaps%size() + this%hru_tlaps%dim_names%size() + ENTRY_OFFSET
  case('hru_tsta')
    this%hru_tsta%name = last
    call this%hru_tsta%read(iUnit)
    line = line + this%hru_tsta%size() + this%hru_tsta%dim_names%size() + ENTRY_OFFSET
  case('hru_type')
    this%hru_type%name = last
    call this%hru_type%read(iUnit)
    line = line + this%hru_type%size() + this%hru_type%dim_names%size() + ENTRY_OFFSET
  case('hru_up_id')
    this%hru_up_id%name = last
    call this%hru_up_id%read(iUnit)
    line = line + this%hru_up_id%size() + this%hru_up_id%dim_names%size() + ENTRY_OFFSET
  case('hru_windspeed_sta')
    this%hru_windspeed_sta%name = last
    call this%hru_windspeed_sta%read(iUnit)
    line = line + this%hru_windspeed_sta%size() + this%hru_windspeed_sta%dim_names%size() + ENTRY_OFFSET
  case('hru_x')
    this%hru_x%name = last
    call this%hru_x%read(iUnit)
    line = line + this%hru_x%size() + this%hru_x%dim_names%size() + ENTRY_OFFSET
  case('hru_xlong')
    this%hru_xlong%name = last
    call this%hru_xlong%read(iUnit)
    line = line + this%hru_xlong%size() + this%hru_xlong%dim_names%size() + ENTRY_OFFSET
  case('hru_y')
    this%hru_y%name = last
    call this%hru_y%read(iUnit)
    line = line + this%hru_y%size() + this%hru_y%dim_names%size() + ENTRY_OFFSET
  case('hru_ylat')
    this%hru_ylat%name = last
    call this%hru_ylat%read(iUnit)
    line = line + this%hru_ylat%size() + this%hru_ylat%dim_names%size() + ENTRY_OFFSET
  case('hs_krs')
    this%hs_krs%name = last
    call this%hs_krs%read(iUnit)
    line = line + this%hs_krs%size() + this%hs_krs%dim_names%size() + ENTRY_OFFSET
  case('humidity_percent')
    this%humidity_percent%name = last
    call this%humidity_percent%read(iUnit)
    line = line + this%humidity_percent%size() + this%humidity_percent%dim_names%size() + ENTRY_OFFSET
  case('imperv_stor_max')
    this%imperv_stor_max%name = last
    call this%imperv_stor_max%read(iUnit)
    line = line + this%imperv_stor_max%size() + this%imperv_stor_max%dim_names%size() + ENTRY_OFFSET
  case('irr_type')
    this%irr_type%name = last
    call this%irr_type%read(iUnit)
    line = line + this%irr_type%size() + this%irr_type%dim_names%size() + ENTRY_OFFSET
  case('jh_coef')
    this%jh_coef%name = last
    call this%jh_coef%read(iUnit)
    line = line + this%jh_coef%size() + this%jh_coef%dim_names%size() + ENTRY_OFFSET
  case('jh_coef_hru')
    this%jh_coef_hru%name = last
    call this%jh_coef_hru%read(iUnit)
    line = line + this%jh_coef_hru%size() + this%jh_coef_hru%dim_names%size() + ENTRY_OFFSET
  case('lake_coef')
    this%lake_coef%name = last
    call this%lake_coef%read(iUnit)
    line = line + this%lake_coef%size() + this%lake_coef%dim_names%size() + ENTRY_OFFSET
  case('lake_din1')
    this%lake_din1%name = last
    call this%lake_din1%read(iUnit)
    line = line + this%lake_din1%size() + this%lake_din1%dim_names%size() + ENTRY_OFFSET
  case('lake_evap_adj')
    this%lake_evap_adj%name = last
    call this%lake_evap_adj%read(iUnit)
    line = line + this%lake_evap_adj%size() + this%lake_evap_adj%dim_names%size() + ENTRY_OFFSET
  case('lake_hru_id')
    this%lake_hru_id%name = last
    call this%lake_hru_id%read(iUnit)
    line = line + this%lake_hru_id%size() + this%lake_hru_id%dim_names%size() + ENTRY_OFFSET
  case('lake_init')
    this%lake_init%name = last
    call this%lake_init%read(iUnit)
    line = line + this%lake_init%size() + this%lake_init%dim_names%size() + ENTRY_OFFSET
  case('lake_out2')
    this%lake_out2%name = last
    call this%lake_out2%read(iUnit)
    line = line + this%lake_out2%size() + this%lake_out2%dim_names%size() + ENTRY_OFFSET
  case('lake_out2_a')
    this%lake_out2_a%name = last
    call this%lake_out2_a%read(iUnit)
    line = line + this%lake_out2_a%size() + this%lake_out2_a%dim_names%size() + ENTRY_OFFSET
  case('lake_out2_b')
    this%lake_out2_b%name = last
    call this%lake_out2_b%read(iUnit)
    line = line + this%lake_out2_b%size() + this%lake_out2_b%dim_names%size() + ENTRY_OFFSET
  case('lake_qro')
    this%lake_qro%name = last
    call this%lake_qro%read(iUnit)
    line = line + this%lake_qro%size() + this%lake_qro%dim_names%size() + ENTRY_OFFSET
  case('lake_seep_elev')
    this%lake_seep_elev%name = last
    call this%lake_seep_elev%read(iUnit)
    line = line + this%lake_seep_elev%size() + this%lake_seep_elev%dim_names%size() + ENTRY_OFFSET
  case('lake_segment_id')
    this%lake_segment_id%name = last
    call this%lake_segment_id%read(iUnit)
    line = line + this%lake_segment_id%size() + this%lake_segment_id%dim_names%size() + ENTRY_OFFSET
  case('lake_type')
    this%lake_type%name = last
    call this%lake_type%read(iUnit)
    line = line + this%lake_type%size() + this%lake_type%dim_names%size() + ENTRY_OFFSET
  case('lake_vol_init')
    this%lake_vol_init%name = last
    call this%lake_vol_init%read(iUnit)
    line = line + this%lake_vol_init%size() + this%lake_vol_init%dim_names%size() + ENTRY_OFFSET
  case('lapsemax_max')
    this%lapsemax_max%name = last
    call this%lapsemax_max%read(iUnit)
    line = line + this%lapsemax_max%size() + this%lapsemax_max%dim_names%size() + ENTRY_OFFSET
  case('lapsemax_min')
    this%lapsemax_min%name = last
    call this%lapsemax_min%read(iUnit)
    line = line + this%lapsemax_min%size() + this%lapsemax_min%dim_names%size() + ENTRY_OFFSET
  case('lapsemin_max')
    this%lapsemin_max%name = last
    call this%lapsemin_max%read(iUnit)
    line = line + this%lapsemin_max%size() + this%lapsemin_max%dim_names%size() + ENTRY_OFFSET
  case('lapsemin_min')
    this%lapsemin_min%name = last
    call this%lapsemin_min%read(iUnit)
    line = line + this%lapsemin_min%size() + this%lapsemin_min%dim_names%size() + ENTRY_OFFSET
  case('lat_temp_adj')
    this%lat_temp_adj%name = last
    call this%lat_temp_adj%read(iUnit)
    line = line + this%lat_temp_adj%size() + this%lat_temp_adj%dim_names%size() + ENTRY_OFFSET
  case('mann_n')
    this%mann_n%name = last
    call this%mann_n%read(iUnit)
    line = line + this%mann_n%size() + this%mann_n%dim_names%size() + ENTRY_OFFSET
  case('mapvars_freq')
    this%mapvars_freq%name = last
    call this%mapvars_freq%read(iUnit)
    line = line + this%mapvars_freq%size() + this%mapvars_freq%dim_names%size() + ENTRY_OFFSET
  case('mapvars_units')
    this%mapvars_units%name = last
    call this%mapvars_units%read(iUnit)
    line = line + this%mapvars_units%size() + this%mapvars_units%dim_names%size() + ENTRY_OFFSET
  case('max_missing')
    this%max_missing%name = last
    call this%max_missing%read(iUnit)
    line = line + this%max_missing%size() + this%max_missing%dim_names%size() + ENTRY_OFFSET
  case('max_psta')
    this%max_psta%name = last
    call this%max_psta%read(iUnit)
    line = line + this%max_psta%size() + this%max_psta%dim_names%size() + ENTRY_OFFSET
  case('max_tsta')
    this%max_tsta%name = last
    call this%max_tsta%read(iUnit)
    line = line + this%max_tsta%size() + this%max_tsta%dim_names%size() + ENTRY_OFFSET
  case('maxday_prec')
    this%maxday_prec%name = last
    call this%maxday_prec%read(iUnit)
    line = line + this%maxday_prec%size() + this%maxday_prec%dim_names%size() + ENTRY_OFFSET
  case('maxiter_sntemp')
    this%maxiter_sntemp%name = last
    call this%maxiter_sntemp%read(iUnit)
    line = line + this%maxiter_sntemp%size() + this%maxiter_sntemp%dim_names%size() + ENTRY_OFFSET
  case('melt_force')
    this%melt_force%name = last
    call this%melt_force%read(iUnit)
    line = line + this%melt_force%size() + this%melt_force%dim_names%size() + ENTRY_OFFSET
  case('melt_look')
    this%melt_look%name = last
    call this%melt_look%read(iUnit)
    line = line + this%melt_look%size() + this%melt_look%dim_names%size() + ENTRY_OFFSET
  case('melt_temp')
    this%melt_temp%name = last
    call this%melt_temp%read(iUnit)
    line = line + this%melt_temp%size() + this%melt_temp%dim_names%size() + ENTRY_OFFSET
  case('monmax')
    this%monmax%name = last
    call this%monmax%read(iUnit)
    line = line + this%monmax%size() + this%monmax%dim_names%size() + ENTRY_OFFSET
  case('monmin')
    this%monmin%name = last
    call this%monmin%read(iUnit)
    line = line + this%monmin%size() + this%monmin%dim_names%size() + ENTRY_OFFSET
  case('ncol')
    this%ncol%name = last
    call this%ncol%read(iUnit)
    line = line + this%ncol%size() + this%ncol%dim_names%size() + ENTRY_OFFSET
  case('nhm_id')
    this%nhm_id%name = last
    call this%nhm_id%read(iUnit)
    line = line + this%nhm_id%size() + this%nhm_id%dim_names%size() + ENTRY_OFFSET
  case('nhm_seg')
    this%nhm_seg%name = last
    call this%nhm_seg%read(iUnit)
    line = line + this%nhm_seg%size() + this%nhm_seg%dim_names%size() + ENTRY_OFFSET
  case('nsos')
    this%nsos%name = last
    call this%nsos%read(iUnit)
    line = line + this%nsos%size() + this%nsos%dim_names%size() + ENTRY_OFFSET
  case('o2')
    this%o2%name = last
    call this%o2%read(iUnit)
    line = line + this%o2%size() + this%o2%dim_names%size() + ENTRY_OFFSET
  case('obsin_segment')
    this%obsin_segment%name = last
    call this%obsin_segment%read(iUnit)
    line = line + this%obsin_segment%size() + this%obsin_segment%dim_names%size() + ENTRY_OFFSET
  case('obsout_lake')
    this%obsout_lake%name = last
    call this%obsout_lake%read(iUnit)
    line = line + this%obsout_lake%size() + this%obsout_lake%dim_names%size() + ENTRY_OFFSET
  case('obsout_segment')
    this%obsout_segment%name = last
    call this%obsout_segment%read(iUnit)
    line = line + this%obsout_segment%size() + this%obsout_segment%dim_names%size() + ENTRY_OFFSET
  case('op_flow_thres')
    this%op_flow_thres%name = last
    call this%op_flow_thres%read(iUnit)
    line = line + this%op_flow_thres%size() + this%op_flow_thres%dim_names%size() + ENTRY_OFFSET
  case('outlet_sta')
    this%outlet_sta%name = last
    call this%outlet_sta%read(iUnit)
    line = line + this%outlet_sta%size() + this%outlet_sta%dim_names%size() + ENTRY_OFFSET
  case('padj_rn')
    this%padj_rn%name = last
    call this%padj_rn%read(iUnit)
    line = line + this%padj_rn%size() + this%padj_rn%dim_names%size() + ENTRY_OFFSET
  case('padj_sn')
    this%padj_sn%name = last
    call this%padj_sn%read(iUnit)
    line = line + this%padj_sn%size() + this%padj_sn%dim_names%size() + ENTRY_OFFSET
  case('parent_gw')
    this%parent_gw%name = last
    call this%parent_gw%read(iUnit)
    line = line + this%parent_gw%size() + this%parent_gw%dim_names%size() + ENTRY_OFFSET
  case('parent_hru')
    this%parent_hru%name = last
    call this%parent_hru%read(iUnit)
    line = line + this%parent_hru%size() + this%parent_hru%dim_names%size() + ENTRY_OFFSET
  case('parent_poigages')
    this%parent_poigages%name = last
    call this%parent_poigages%read(iUnit)
    line = line + this%parent_poigages%size() + this%parent_poigages%dim_names%size() + ENTRY_OFFSET
  case('parent_segment')
    this%parent_segment%name = last
    call this%parent_segment%read(iUnit)
    line = line + this%parent_segment%size() + this%parent_segment%dim_names%size() + ENTRY_OFFSET
  case('parent_ssr')
    this%parent_ssr%name = last
    call this%parent_ssr%read(iUnit)
    line = line + this%parent_ssr%size() + this%parent_ssr%dim_names%size() + ENTRY_OFFSET
  case('pm_d_coef')
    this%pm_d_coef%name = last
    call this%pm_d_coef%read(iUnit)
    line = line + this%pm_d_coef%size() + this%pm_d_coef%dim_names%size() + ENTRY_OFFSET
  case('pm_n_coef')
    this%pm_n_coef%name = last
    call this%pm_n_coef%read(iUnit)
    line = line + this%pm_n_coef%size() + this%pm_n_coef%dim_names%size() + ENTRY_OFFSET
  case('pmn_mo')
    this%pmn_mo%name = last
    call this%pmn_mo%read(iUnit)
    line = line + this%pmn_mo%size() + this%pmn_mo%dim_names%size() + ENTRY_OFFSET
  case('poi_gage_id')
    this%poi_gage_id%name = last
    call this%poi_gage_id%read(iUnit)
    line = line + this%poi_gage_id%size() + this%poi_gage_id%dim_names%size() + ENTRY_OFFSET
  case('poi_gage_segment')
    this%poi_gage_segment%name = last
    call this%poi_gage_segment%read(iUnit)
    line = line + this%poi_gage_segment%size() + this%poi_gage_segment%dim_names%size() + ENTRY_OFFSET
  case('poi_type')
    this%poi_type%name = last
    call this%poi_type%read(iUnit)
    line = line + this%poi_type%size() + this%poi_type%dim_names%size() + ENTRY_OFFSET
  case('potet_cbh_adj')
    this%potet_cbh_adj%name = last
    call this%potet_cbh_adj%read(iUnit)
    line = line + this%potet_cbh_adj%size() + this%potet_cbh_adj%dim_names%size() + ENTRY_OFFSET
  case('potet_sublim')
    this%potet_sublim%name = last
    call this%potet_sublim%read(iUnit)
    line = line + this%potet_sublim%size() + this%potet_sublim%dim_names%size() + ENTRY_OFFSET
  case('ppt_rad_adj')
    this%ppt_rad_adj%name = last
    call this%ppt_rad_adj%read(iUnit)
    line = line + this%ppt_rad_adj%size() + this%ppt_rad_adj%dim_names%size() + ENTRY_OFFSET
  case('precip_units')
    this%precip_units%name = last
    call this%precip_units%read(iUnit)
    line = line + this%precip_units%size() + this%precip_units%dim_names%size() + ENTRY_OFFSET
  case('pref_flow_den')
    this%pref_flow_den%name = last
    call this%pref_flow_den%read(iUnit)
    line = line + this%pref_flow_den%size() + this%pref_flow_den%dim_names%size() + ENTRY_OFFSET
  case('print_freq')
    this%print_freq%name = last
    call this%print_freq%read(iUnit)
    line = line + this%print_freq%size() + this%print_freq%dim_names%size() + ENTRY_OFFSET
  case('print_type')
    this%print_type%name = last
    call this%print_type%read(iUnit)
    line = line + this%print_type%size() + this%print_type%dim_names%size() + ENTRY_OFFSET
  case('psta_elev')
    this%psta_elev%name = last
    call this%psta_elev%read(iUnit)
    line = line + this%psta_elev%size() + this%psta_elev%dim_names%size() + ENTRY_OFFSET
  case('psta_mon')
    this%psta_mon%name = last
    call this%psta_mon%read(iUnit)
    line = line + this%psta_mon%size() + this%psta_mon%dim_names%size() + ENTRY_OFFSET
  case('psta_xlong')
    this%psta_xlong%name = last
    call this%psta_xlong%read(iUnit)
    line = line + this%psta_xlong%size() + this%psta_xlong%dim_names%size() + ENTRY_OFFSET
  case('psta_ylat')
    this%psta_ylat%name = last
    call this%psta_ylat%read(iUnit)
    line = line + this%psta_ylat%size() + this%psta_ylat%dim_names%size() + ENTRY_OFFSET
  case('pt_alpha')
    this%pt_alpha%name = last
    call this%pt_alpha%read(iUnit)
    line = line + this%pt_alpha%size() + this%pt_alpha%dim_names%size() + ENTRY_OFFSET
  case('rad_conv')
    this%rad_conv%name = last
    call this%rad_conv%read(iUnit)
    line = line + this%rad_conv%size() + this%rad_conv%dim_names%size() + ENTRY_OFFSET
  case('rad_trncf')
    this%rad_trncf%name = last
    call this%rad_trncf%read(iUnit)
    line = line + this%rad_trncf%size() + this%rad_trncf%dim_names%size() + ENTRY_OFFSET
  case('radadj_intcp')
    this%radadj_intcp%name = last
    call this%radadj_intcp%read(iUnit)
    line = line + this%radadj_intcp%size() + this%radadj_intcp%dim_names%size() + ENTRY_OFFSET
  case('radadj_slope')
    this%radadj_slope%name = last
    call this%radadj_slope%read(iUnit)
    line = line + this%radadj_slope%size() + this%radadj_slope%dim_names%size() + ENTRY_OFFSET
  case('radj_sppt')
    this%radj_sppt%name = last
    call this%radj_sppt%read(iUnit)
    line = line + this%radj_sppt%size() + this%radj_sppt%dim_names%size() + ENTRY_OFFSET
  case('radj_wppt')
    this%radj_wppt%name = last
    call this%radj_wppt%read(iUnit)
    line = line + this%radj_wppt%size() + this%radj_wppt%dim_names%size() + ENTRY_OFFSET
  case('radmax')
    this%radmax%name = last
    call this%radmax%read(iUnit)
    line = line + this%radmax%size() + this%radmax%dim_names%size() + ENTRY_OFFSET
  case('rain_adj')
    this%rain_adj%name = last
    call this%rain_adj%read(iUnit)
    line = line + this%rain_adj%size() + this%rain_adj%dim_names%size() + ENTRY_OFFSET
  case('rain_cbh_adj')
    this%rain_cbh_adj%name = last
    call this%rain_cbh_adj%read(iUnit)
    line = line + this%rain_cbh_adj%size() + this%rain_cbh_adj%dim_names%size() + ENTRY_OFFSET
  case('rain_code')
    this%rain_code%name = last
    call this%rain_code%read(iUnit)
    line = line + this%rain_code%size() + this%rain_code%dim_names%size() + ENTRY_OFFSET
  case('rain_mon')
    this%rain_mon%name = last
    call this%rain_mon%read(iUnit)
    line = line + this%rain_mon%size() + this%rain_mon%dim_names%size() + ENTRY_OFFSET
  case('rate_table')
    this%rate_table%name = last
    call this%rate_table%read(iUnit)
    line = line + this%rate_table%size() + this%rate_table%dim_names%size() + ENTRY_OFFSET
  case('rate_table2')
    this%rate_table2%name = last
    call this%rate_table2%read(iUnit)
    line = line + this%rate_table2%size() + this%rate_table2%dim_names%size() + ENTRY_OFFSET
  case('rate_table3')
    this%rate_table3%name = last
    call this%rate_table3%read(iUnit)
    line = line + this%rate_table3%size() + this%rate_table3%dim_names%size() + ENTRY_OFFSET
  case('rate_table4')
    this%rate_table4%name = last
    call this%rate_table4%read(iUnit)
    line = line + this%rate_table4%size() + this%rate_table4%dim_names%size() + ENTRY_OFFSET
  case('ratetbl_lake')
    this%ratetbl_lake%name = last
    call this%ratetbl_lake%read(iUnit)
    line = line + this%ratetbl_lake%size() + this%ratetbl_lake%dim_names%size() + ENTRY_OFFSET
  case('runoff_units')
    this%runoff_units%name = last
    call this%runoff_units%read(iUnit)
    line = line + this%runoff_units%size() + this%runoff_units%dim_names%size() + ENTRY_OFFSET
  case('s2')
    this%s2%name = last
    call this%s2%read(iUnit)
    line = line + this%s2%size() + this%s2%dim_names%size() + ENTRY_OFFSET
  case('sat_threshold')
    this%sat_threshold%name = last
    call this%sat_threshold%read(iUnit)
    line = line + this%sat_threshold%size() + this%sat_threshold%dim_names%size() + ENTRY_OFFSET
  case('seg_depth')
    this%seg_depth%name = last
    call this%seg_depth%read(iUnit)
    line = line + this%seg_depth%size() + this%seg_depth%dim_names%size() + ENTRY_OFFSET
  case('seg_elev')
    this%seg_elev%name = last
    call this%seg_elev%read(iUnit)
    line = line + this%seg_elev%size() + this%seg_elev%dim_names%size() + ENTRY_OFFSET
  case('seg_humidity')
    this%seg_humidity%name = last
    call this%seg_humidity%read(iUnit)
    line = line + this%seg_humidity%size() + this%seg_humidity%dim_names%size() + ENTRY_OFFSET
  case('seg_humidity_sta')
    this%seg_humidity_sta%name = last
    call this%seg_humidity_sta%read(iUnit)
    line = line + this%seg_humidity_sta%size() + this%seg_humidity_sta%dim_names%size() + ENTRY_OFFSET
  case('seg_lat')
    this%seg_lat%name = last
    call this%seg_lat%read(iUnit)
    line = line + this%seg_lat%size() + this%seg_lat%dim_names%size() + ENTRY_OFFSET
  case('seg_length')
    this%seg_length%name = last
    call this%seg_length%read(iUnit)
    line = line + this%seg_length%size() + this%seg_length%dim_names%size() + ENTRY_OFFSET
  case('seg_slope')
    this%seg_slope%name = last
    call this%seg_slope%read(iUnit)
    line = line + this%seg_slope%size() + this%seg_slope%dim_names%size() + ENTRY_OFFSET
  case('seg_summer_humidity')
    this%seg_summer_humidity%name = last
    call this%seg_summer_humidity%read(iUnit)
    line = line + this%seg_summer_humidity%size() + this%seg_summer_humidity%dim_names%size() + ENTRY_OFFSET
  case('seg_winter_humidity')
    this%seg_winter_humidity%name = last
    call this%seg_winter_humidity%read(iUnit)
    line = line + this%seg_winter_humidity%size() + this%seg_winter_humidity%dim_names%size() + ENTRY_OFFSET
  case('segment_flow_init')
    this%segment_flow_init%name = last
    call this%segment_flow_init%read(iUnit)
    line = line + this%segment_flow_init%size() + this%segment_flow_init%dim_names%size() + ENTRY_OFFSET
  case('segment_type')
    this%segment_type%name = last
    call this%segment_type%read(iUnit)
    line = line + this%segment_type%size() + this%segment_type%dim_names%size() + ENTRY_OFFSET
  case('segshade_sum')
    this%segshade_sum%name = last
    call this%segshade_sum%read(iUnit)
    line = line + this%segshade_sum%size() + this%segshade_sum%dim_names%size() + ENTRY_OFFSET
  case('segshade_win')
    this%segshade_win%name = last
    call this%segshade_win%read(iUnit)
    line = line + this%segshade_win%size() + this%segshade_win%dim_names%size() + ENTRY_OFFSET
  case('settle_const')
    this%settle_const%name = last
    call this%settle_const%read(iUnit)
    line = line + this%settle_const%size() + this%settle_const%dim_names%size() + ENTRY_OFFSET
  case('slowcoef_lin')
    this%slowcoef_lin%name = last
    call this%slowcoef_lin%read(iUnit)
    line = line + this%slowcoef_lin%size() + this%slowcoef_lin%dim_names%size() + ENTRY_OFFSET
  case('slowcoef_sq')
    this%slowcoef_sq%name = last
    call this%slowcoef_sq%read(iUnit)
    line = line + this%slowcoef_sq%size() + this%slowcoef_sq%dim_names%size() + ENTRY_OFFSET
  case('smidx_coef')
    this%smidx_coef%name = last
    call this%smidx_coef%read(iUnit)
    line = line + this%smidx_coef%size() + this%smidx_coef%dim_names%size() + ENTRY_OFFSET
  case('smidx_exp')
    this%smidx_exp%name = last
    call this%smidx_exp%read(iUnit)
    line = line + this%smidx_exp%size() + this%smidx_exp%dim_names%size() + ENTRY_OFFSET
  case('snarea_curve')
    this%snarea_curve%name = last
    call this%snarea_curve%read(iUnit)
    line = line + this%snarea_curve%size() + this%snarea_curve%dim_names%size() + ENTRY_OFFSET
  case('snarea_thresh')
    this%snarea_thresh%name = last
    call this%snarea_thresh%read(iUnit)
    line = line + this%snarea_thresh%size() + this%snarea_thresh%dim_names%size() + ENTRY_OFFSET
  case('snow_adj')
    this%snow_adj%name = last
    call this%snow_adj%read(iUnit)
    line = line + this%snow_adj%size() + this%snow_adj%dim_names%size() + ENTRY_OFFSET
  case('snow_cbh_adj')
    this%snow_cbh_adj%name = last
    call this%snow_cbh_adj%read(iUnit)
    line = line + this%snow_cbh_adj%size() + this%snow_cbh_adj%dim_names%size() + ENTRY_OFFSET
  case('snow_intcp')
    this%snow_intcp%name = last
    call this%snow_intcp%read(iUnit)
    line = line + this%snow_intcp%size() + this%snow_intcp%dim_names%size() + ENTRY_OFFSET
  case('snow_mon')
    this%snow_mon%name = last
    call this%snow_mon%read(iUnit)
    line = line + this%snow_mon%size() + this%snow_mon%dim_names%size() + ENTRY_OFFSET
  case('snowinfil_max')
    this%snowinfil_max%name = last
    call this%snowinfil_max%read(iUnit)
    line = line + this%snowinfil_max%size() + this%snowinfil_max%dim_names%size() + ENTRY_OFFSET
  case('snowpack_init')
    this%snowpack_init%name = last
    call this%snowpack_init%read(iUnit)
    line = line + this%snowpack_init%size() + this%snowpack_init%dim_names%size() + ENTRY_OFFSET
  case('soil2gw_max')
    this%soil2gw_max%name = last
    call this%soil2gw_max%read(iUnit)
    line = line + this%soil2gw_max%size() + this%soil2gw_max%dim_names%size() + ENTRY_OFFSET
  case('soil_moist_init')
    this%soil_moist_init%name = last
    call this%soil_moist_init%read(iUnit)
    line = line + this%soil_moist_init%size() + this%soil_moist_init%dim_names%size() + ENTRY_OFFSET
  case('soil_moist_init_frac')
    this%soil_moist_init_frac%name = last
    call this%soil_moist_init_frac%read(iUnit)
    line = line + this%soil_moist_init_frac%size() + this%soil_moist_init_frac%dim_names%size() + ENTRY_OFFSET
  case('soil_moist_max')
    this%soil_moist_max%name = last
    call this%soil_moist_max%read(iUnit)
    line = line + this%soil_moist_max%size() + this%soil_moist_max%dim_names%size() + ENTRY_OFFSET
  case('soil_rechr_init')
    this%soil_rechr_init%name = last
    call this%soil_rechr_init%read(iUnit)
    line = line + this%soil_rechr_init%size() + this%soil_rechr_init%dim_names%size() + ENTRY_OFFSET
  case('soil_rechr_init_frac')
    this%soil_rechr_init_frac%name = last
    call this%soil_rechr_init_frac%read(iUnit)
    line = line + this%soil_rechr_init_frac%size() + this%soil_rechr_init_frac%dim_names%size() + ENTRY_OFFSET
  case('soil_rechr_max')
    this%soil_rechr_max%name = last
    call this%soil_rechr_max%read(iUnit)
    line = line + this%soil_rechr_max%size() + this%soil_rechr_max%dim_names%size() + ENTRY_OFFSET
  case('soil_rechr_max_frac')
    this%soil_rechr_max_frac%name = last
    call this%soil_rechr_max_frac%read(iUnit)
    line = line + this%soil_rechr_max_frac%size() + this%soil_rechr_max_frac%dim_names%size() + ENTRY_OFFSET
  case('soil_type')
    this%soil_type%name = last
    call this%soil_type%read(iUnit)
    line = line + this%soil_type%size() + this%soil_type%dim_names%size() + ENTRY_OFFSET
  case('spring_frost')
    this%spring_frost%name = last
    call this%spring_frost%read(iUnit)
    line = line + this%spring_frost%size() + this%spring_frost%dim_names%size() + ENTRY_OFFSET
  case('srain_intcp')
    this%srain_intcp%name = last
    call this%srain_intcp%read(iUnit)
    line = line + this%srain_intcp%size() + this%srain_intcp%dim_names%size() + ENTRY_OFFSET
  case('sro_to_dprst')
    this%sro_to_dprst%name = last
    call this%sro_to_dprst%read(iUnit)
    line = line + this%sro_to_dprst%size() + this%sro_to_dprst%dim_names%size() + ENTRY_OFFSET
  case('sro_to_dprst_imperv')
    this%sro_to_dprst_imperv%name = last
    call this%sro_to_dprst_imperv%read(iUnit)
    line = line + this%sro_to_dprst_imperv%size() + this%sro_to_dprst_imperv%dim_names%size() + ENTRY_OFFSET
  case('sro_to_dprst_perv')
    this%sro_to_dprst_perv%name = last
    call this%sro_to_dprst_perv%read(iUnit)
    line = line + this%sro_to_dprst_perv%size() + this%sro_to_dprst_perv%dim_names%size() + ENTRY_OFFSET
  case('ss_init')
    this%ss_init%name = last
    call this%ss_init%read(iUnit)
    line = line + this%ss_init%size() + this%ss_init%dim_names%size() + ENTRY_OFFSET
  case('ss_tau')
    this%ss_tau%name = last
    call this%ss_tau%read(iUnit)
    line = line + this%ss_tau%size() + this%ss_tau%dim_names%size() + ENTRY_OFFSET
  case('ssr2gw_exp')
    this%ssr2gw_exp%name = last
    call this%ssr2gw_exp%read(iUnit)
    line = line + this%ssr2gw_exp%size() + this%ssr2gw_exp%dim_names%size() + ENTRY_OFFSET
  case('ssr2gw_rate')
    this%ssr2gw_rate%name = last
    call this%ssr2gw_rate%read(iUnit)
    line = line + this%ssr2gw_rate%size() + this%ssr2gw_rate%dim_names%size() + ENTRY_OFFSET
  case('ssstor_init')
    this%ssstor_init%name = last
    call this%ssstor_init%read(iUnit)
    line = line + this%ssstor_init%size() + this%ssstor_init%dim_names%size() + ENTRY_OFFSET
  case('ssstor_init_frac')
    this%ssstor_init_frac%name = last
    call this%ssstor_init_frac%read(iUnit)
    line = line + this%ssstor_init_frac%size() + this%ssstor_init_frac%dim_names%size() + ENTRY_OFFSET
  case('subbasin_down')
    this%subbasin_down%name = last
    call this%subbasin_down%read(iUnit)
    line = line + this%subbasin_down%size() + this%subbasin_down%dim_names%size() + ENTRY_OFFSET
  case('tbl_gate')
    this%tbl_gate%name = last
    call this%tbl_gate%read(iUnit)
    line = line + this%tbl_gate%size() + this%tbl_gate%dim_names%size() + ENTRY_OFFSET
  case('tbl_gate2')
    this%tbl_gate2%name = last
    call this%tbl_gate2%read(iUnit)
    line = line + this%tbl_gate2%size() + this%tbl_gate2%dim_names%size() + ENTRY_OFFSET
  case('tbl_gate3')
    this%tbl_gate3%name = last
    call this%tbl_gate3%read(iUnit)
    line = line + this%tbl_gate3%size() + this%tbl_gate3%dim_names%size() + ENTRY_OFFSET
  case('tbl_gate4')
    this%tbl_gate4%name = last
    call this%tbl_gate4%read(iUnit)
    line = line + this%tbl_gate4%size() + this%tbl_gate4%dim_names%size() + ENTRY_OFFSET
  case('tbl_stage')
    this%tbl_stage%name = last
    call this%tbl_stage%read(iUnit)
    line = line + this%tbl_stage%size() + this%tbl_stage%dim_names%size() + ENTRY_OFFSET
  case('tbl_stage2')
    this%tbl_stage2%name = last
    call this%tbl_stage2%read(iUnit)
    line = line + this%tbl_stage2%size() + this%tbl_stage2%dim_names%size() + ENTRY_OFFSET
  case('tbl_stage3')
    this%tbl_stage3%name = last
    call this%tbl_stage3%read(iUnit)
    line = line + this%tbl_stage3%size() + this%tbl_stage3%dim_names%size() + ENTRY_OFFSET
  case('tbl_stage4')
    this%tbl_stage4%name = last
    call this%tbl_stage4%read(iUnit)
    line = line + this%tbl_stage4%size() + this%tbl_stage4%dim_names%size() + ENTRY_OFFSET
  case('temp_units')
    this%temp_units%name = last
    call this%temp_units%read(iUnit)
    line = line + this%temp_units%size() + this%temp_units%dim_names%size() + ENTRY_OFFSET
  case('tmax_adj')
    this%tmax_adj%name = last
    call this%tmax_adj%read(iUnit)
    line = line + this%tmax_adj%size() + this%tmax_adj%dim_names%size() + ENTRY_OFFSET
  case('tmax_allrain')
    this%tmax_allrain%name = last
    call this%tmax_allrain%read(iUnit)
    line = line + this%tmax_allrain%size() + this%tmax_allrain%dim_names%size() + ENTRY_OFFSET
  case('tmax_allrain_offset')
    this%tmax_allrain_offset%name = last
    call this%tmax_allrain_offset%read(iUnit)
    line = line + this%tmax_allrain_offset%size() + this%tmax_allrain_offset%dim_names%size() + ENTRY_OFFSET
  case('tmax_allsnow')
    this%tmax_allsnow%name = last
    call this%tmax_allsnow%read(iUnit)
    line = line + this%tmax_allsnow%size() + this%tmax_allsnow%dim_names%size() + ENTRY_OFFSET
  case('tmax_cbh_adj')
    this%tmax_cbh_adj%name = last
    call this%tmax_cbh_adj%read(iUnit)
    line = line + this%tmax_cbh_adj%size() + this%tmax_cbh_adj%dim_names%size() + ENTRY_OFFSET
  case('tmax_index')
    this%tmax_index%name = last
    call this%tmax_index%read(iUnit)
    line = line + this%tmax_index%size() + this%tmax_index%dim_names%size() + ENTRY_OFFSET
  case('tmax_lapse')
    this%tmax_lapse%name = last
    call this%tmax_lapse%read(iUnit)
    line = line + this%tmax_lapse%size() + this%tmax_lapse%dim_names%size() + ENTRY_OFFSET
  case('tmin_adj')
    this%tmin_adj%name = last
    call this%tmin_adj%read(iUnit)
    line = line + this%tmin_adj%size() + this%tmin_adj%dim_names%size() + ENTRY_OFFSET
  case('tmin_cbh_adj')
    this%tmin_cbh_adj%name = last
    call this%tmin_cbh_adj%read(iUnit)
    line = line + this%tmin_cbh_adj%size() + this%tmin_cbh_adj%dim_names%size() + ENTRY_OFFSET
  case('tmin_lapse')
    this%tmin_lapse%name = last
    call this%tmin_lapse%read(iUnit)
    line = line + this%tmin_lapse%size() + this%tmin_lapse%dim_names%size() + ENTRY_OFFSET
  case('tosegment')
    this%tosegment%name = last
    call this%tosegment%read(iUnit)
    line = line + this%tosegment%size() + this%tosegment%dim_names%size() + ENTRY_OFFSET
  case('tosegment_nhm')
    this%tosegment_nhm%name = last
    call this%tosegment_nhm%read(iUnit)
    line = line + this%tosegment_nhm%size() + this%tosegment_nhm%dim_names%size() + ENTRY_OFFSET
  case('transp_beg')
    this%transp_beg%name = last
    call this%transp_beg%read(iUnit)
    line = line + this%transp_beg%size() + this%transp_beg%dim_names%size() + ENTRY_OFFSET
  case('transp_end')
    this%transp_end%name = last
    call this%transp_end%read(iUnit)
    line = line + this%transp_end%size() + this%transp_end%dim_names%size() + ENTRY_OFFSET
  case('transp_tmax')
    this%transp_tmax%name = last
    call this%transp_tmax%read(iUnit)
    line = line + this%transp_tmax%size() + this%transp_tmax%dim_names%size() + ENTRY_OFFSET
  case('tsta_elev')
    this%tsta_elev%name = last
    call this%tsta_elev%read(iUnit)
    line = line + this%tsta_elev%size() + this%tsta_elev%dim_names%size() + ENTRY_OFFSET
  case('tsta_xlong')
    this%tsta_xlong%name = last
    call this%tsta_xlong%read(iUnit)
    line = line + this%tsta_xlong%size() + this%tsta_xlong%dim_names%size() + ENTRY_OFFSET
  case('tsta_ylat')
    this%tsta_ylat%name = last
    call this%tsta_ylat%read(iUnit)
    line = line + this%tsta_ylat%size() + this%tsta_ylat%dim_names%size() + ENTRY_OFFSET
  case('tstorm_mo')
    this%tstorm_mo%name = last
    call this%tstorm_mo%read(iUnit)
    line = line + this%tstorm_mo%size() + this%tstorm_mo%dim_names%size() + ENTRY_OFFSET
  case('va_clos_exp')
    this%va_clos_exp%name = last
    call this%va_clos_exp%read(iUnit)
    line = line + this%va_clos_exp%size() + this%va_clos_exp%dim_names%size() + ENTRY_OFFSET
  case('va_open_exp')
    this%va_open_exp%name = last
    call this%va_open_exp%read(iUnit)
    line = line + this%va_open_exp%size() + this%va_open_exp%dim_names%size() + ENTRY_OFFSET
  case('vce')
    this%vce%name = last
    call this%vce%read(iUnit)
    line = line + this%vce%size() + this%vce%dim_names%size() + ENTRY_OFFSET
  case('vcw')
    this%vcw%name = last
    call this%vcw%read(iUnit)
    line = line + this%vcw%size() + this%vcw%dim_names%size() + ENTRY_OFFSET
  case('vdemn')
    this%vdemn%name = last
    call this%vdemn%read(iUnit)
    line = line + this%vdemn%size() + this%vdemn%dim_names%size() + ENTRY_OFFSET
  case('vdemx')
    this%vdemx%name = last
    call this%vdemx%read(iUnit)
    line = line + this%vdemx%size() + this%vdemx%dim_names%size() + ENTRY_OFFSET
  case('vdwmn')
    this%vdwmn%name = last
    call this%vdwmn%read(iUnit)
    line = line + this%vdwmn%size() + this%vdwmn%dim_names%size() + ENTRY_OFFSET
  case('vdwmx')
    this%vdwmx%name = last
    call this%vdwmx%read(iUnit)
    line = line + this%vdwmx%size() + this%vdwmx%dim_names%size() + ENTRY_OFFSET
  case('vhe')
    this%vhe%name = last
    call this%vhe%read(iUnit)
    line = line + this%vhe%size() + this%vhe%dim_names%size() + ENTRY_OFFSET
  case('vhw')
    this%vhw%name = last
    call this%vhw%read(iUnit)
    line = line + this%vhw%size() + this%vhw%dim_names%size() + ENTRY_OFFSET
  case('voe')
    this%voe%name = last
    call this%voe%read(iUnit)
    line = line + this%voe%size() + this%voe%dim_names%size() + ENTRY_OFFSET
  case('vow')
    this%vow%name = last
    call this%vow%read(iUnit)
    line = line + this%vow%size() + this%vow%dim_names%size() + ENTRY_OFFSET
  case('weir_coef')
    this%weir_coef%name = last
    call this%weir_coef%read(iUnit)
    line = line + this%weir_coef%size() + this%weir_coef%dim_names%size() + ENTRY_OFFSET
  case('weir_len')
    this%weir_len%name = last
    call this%weir_len%read(iUnit)
    line = line + this%weir_len%size() + this%weir_len%dim_names%size() + ENTRY_OFFSET
  case('width_alpha')
    this%width_alpha%name = last
    call this%width_alpha%read(iUnit)
    line = line + this%width_alpha%size() + this%width_alpha%dim_names%size() + ENTRY_OFFSET
  case('width_flow')
    this%width_flow%name = last
    call this%width_flow%read(iUnit)
    line = line + this%width_flow%size() + this%width_flow%dim_names%size() + ENTRY_OFFSET
  case('width_m')
    this%width_m%name = last
    call this%width_m%read(iUnit)
    line = line + this%width_m%size() + this%width_m%dim_names%size() + ENTRY_OFFSET
  case('width_values')
    this%width_values%name = last
    call this%width_values%read(iUnit)
    line = line + this%width_values%size() + this%width_values%dim_names%size() + ENTRY_OFFSET
  case('wrain_intcp')
    this%wrain_intcp%name = last
    call this%wrain_intcp%read(iUnit)
    line = line + this%wrain_intcp%size() + this%wrain_intcp%dim_names%size() + ENTRY_OFFSET
  case('x_coef')
    this%x_coef%name = last
    call this%x_coef%read(iUnit)
    line = line + this%x_coef%size() + this%x_coef%dim_names%size() + ENTRY_OFFSET

  case default
    write(output_unit, *) 'WARNING: Skipping parameter, ', last

    ! Get the number of dimension names
    read(iUnit, *) N
    line = line + 1

    do ii=1, N
      read(iUnit, '(A)')  ! Skip the dimension names
      line = line + 1
    enddo

    read(iUnit, *) N  ! Get the number of entries for parameter
    line = line + 1
    read(iUnit, *)    ! Skip the datatype
    line = line + 1

    do ii=1, N
      read(iUnit, *)  ! Skip the parameter entries

      ! if (istat /= 0) then
      !   inquire(UNIT=iUnit, NAME=filename)
      !   write(output_unit, *) "ERROR: Reading from file: " // trim(filename)
      !   close(iUnit)
      !   stop
      ! endif

      line = line + 1
    enddo
end select
