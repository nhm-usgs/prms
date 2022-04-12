select case(buf)
  ! Control variables
  case('aniOutON_OFF')
    this%aniOutON_OFF%name = last
    call this%aniOutON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('aniOutVar_names')
    this%aniOutVar_names%name = last
    call this%aniOutVar_names%read(iUnit)
    line = line + this%aniOutVar_names%size() + ENTRY_OFFSET
  case('ani_output_file')
    this%ani_output_file%name = last
    call this%ani_output_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('basinOutBaseFileName')
    this%basinOutBaseFileName%name = last
    call this%basinOutBaseFileName%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('basinOutON_OFF')
    this%basinOutON_OFF%name = last
    call this%basinOutON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('basinOutVar_names')
    this%basinOutVar_names%name = last
    call this%basinOutVar_names%read(iUnit)
    line = line + this%basinOutVar_names%size() + ENTRY_OFFSET
  case('basinOutVars')
    this%basinOutVars%name = last
    call this%basinOutVars%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('basinOut_freq')
    this%basinOut_freq%name = last
    call this%basinOut_freq%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('cascade_flag')
    this%cascade_flag%name = last
    call this%cascade_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('cascadegw_flag')
    this%cascadegw_flag%name = last
    call this%cascadegw_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('cbh_binary_flag')
    this%cbh_binary_flag%name = last
    call this%cbh_binary_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('cbh_check_flag')
    this%cbh_check_flag%name = last
    call this%cbh_check_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('covden_sum_dynamic')
    this%covden_sum_dynamic%name = last
    call this%covden_sum_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('covden_win_dynamic')
    this%covden_win_dynamic%name = last
    call this%covden_win_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('covtype_dynamic')
    this%covtype_dynamic%name = last
    call this%covtype_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('csvON_OFF')
    this%csvON_OFF%name = last
    call this%csvON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('csv_output_file')
    this%csv_output_file%name = last
    call this%csv_output_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('data_file')
    this%data_file%name = last
    call this%data_file%read(iUnit)
    line = line + this%data_file%size() + ENTRY_OFFSET
  case('dispGraphsBuffSize')
    this%dispGraphsBuffSize%name = last
    call this%dispGraphsBuffSize%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dispVar_element')
    this%dispVar_element%name = last
    call this%dispVar_element%read(iUnit)
    line = line + this%dispVar_element%size() + ENTRY_OFFSET
  case('dispVar_names')
    this%dispVar_names%name = last
    call this%dispVar_names%read(iUnit)
    line = line + this%dispVar_names%size() + ENTRY_OFFSET
  case('dispVar_plot')
    this%dispVar_plot%name = last
    call this%dispVar_plot%read(iUnit)
    line = line + this%dispVar_plot%size() + ENTRY_OFFSET
  case('dprst_depth_dynamic')
    this%dprst_depth_dynamic%name = last
    call this%dprst_depth_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dprst_flag')
    this%dprst_flag%name = last
    call this%dprst_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dprst_frac_dynamic')
    this%dprst_frac_dynamic%name = last
    call this%dprst_frac_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dprst_transferON_OFF')
    this%dprst_transferON_OFF%name = last
    call this%dprst_transferON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dprst_transfer_file')
    this%dprst_transfer_file%name = last
    call this%dprst_transfer_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_covden_flag')
    this%dyn_covden_flag%name = last
    call this%dyn_covden_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_covtype_flag')
    this%dyn_covtype_flag%name = last
    call this%dyn_covtype_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_dprst_flag')
    this%dyn_dprst_flag%name = last
    call this%dyn_dprst_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_fallfrost_flag')
    this%dyn_fallfrost_flag%name = last
    call this%dyn_fallfrost_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_imperv_flag')
    this%dyn_imperv_flag%name = last
    call this%dyn_imperv_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_intcp_flag')
    this%dyn_intcp_flag%name = last
    call this%dyn_intcp_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_potet_flag')
    this%dyn_potet_flag%name = last
    call this%dyn_potet_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_radtrncf_flag')
    this%dyn_radtrncf_flag%name = last
    call this%dyn_radtrncf_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_snareathresh_flag')
    this%dyn_snareathresh_flag%name = last
    call this%dyn_snareathresh_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_soil_flag')
    this%dyn_soil_flag%name = last
    call this%dyn_soil_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_springfrost_flag')
    this%dyn_springfrost_flag%name = last
    call this%dyn_springfrost_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_sro2dprst_imperv_flag')
    this%dyn_sro2dprst_imperv_flag%name = last
    call this%dyn_sro2dprst_imperv_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_sro2dprst_perv_flag')
    this%dyn_sro2dprst_perv_flag%name = last
    call this%dyn_sro2dprst_perv_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_transp_flag')
    this%dyn_transp_flag%name = last
    call this%dyn_transp_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('dyn_transp_on_flag')
    this%dyn_transp_on_flag%name = last
    call this%dyn_transp_on_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('end_time')
    this%end_time%name = last
    call this%end_time%read(iUnit)
    line = line + this%end_time%size() + ENTRY_OFFSET
  case('et_module')
    this%et_module%name = last
    call this%et_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('executable_desc')
    this%executable_desc%name = last
    call this%executable_desc%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('executable_model')
    this%executable_model%name = last
    call this%executable_model%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('external_transferON_OFF')
    this%external_transferON_OFF%name = last
    call this%external_transferON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('external_transfer_file')
    this%external_transfer_file%name = last
    call this%external_transfer_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('fallfrost_dynamic')
    this%fallfrost_dynamic%name = last
    call this%fallfrost_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('gwr_swale_flag')
    this%gwr_swale_flag%name = last
    call this%gwr_swale_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('gwr_transferON_OFF')
    this%gwr_transferON_OFF%name = last
    call this%gwr_transferON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('gwr_transfer_file')
    this%gwr_transfer_file%name = last
    call this%gwr_transfer_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('humidity_day')
    this%humidity_day%name = last
    call this%humidity_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('humidity_module')
    this%humidity_module%name = last
    call this%humidity_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('imperv_frac_dynamic')
    this%imperv_frac_dynamic%name = last
    call this%imperv_frac_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('imperv_stor_dynamic')
    this%imperv_stor_dynamic%name = last
    call this%imperv_stor_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('init_vars_from_file')
    this%init_vars_from_file%name = last
    call this%init_vars_from_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('initial_deltat')
    this%initial_deltat%name = last
    call this%initial_deltat%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('lake_transferON_OFF')
    this%lake_transferON_OFF%name = last
    call this%lake_transferON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('lake_transfer_file')
    this%lake_transfer_file%name = last
    call this%lake_transfer_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('mapOutON_OFF')
    this%mapOutON_OFF%name = last
    call this%mapOutON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('mapOutVar_names')
    this%mapOutVar_names%name = last
    call this%mapOutVar_names%read(iUnit)
    line = line + this%mapOutVar_names%size() + ENTRY_OFFSET
  case('model_mode')
    this%model_mode%name = last
    call this%model_mode%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('model_output_file')
    this%model_output_file%name = last
    call this%model_output_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('naniOutVars')
    this%naniOutVars%name = last
    call this%naniOutVars%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('ndispGraphs')
    this%ndispGraphs%name = last
    call this%ndispGraphs%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nhruOutBaseFileName')
    this%nhruOutBaseFileName%name = last
    call this%nhruOutBaseFileName%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nhruOutON_OFF')
    this%nhruOutON_OFF%name = last
    call this%nhruOutON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nhruOutVar_names')
    this%nhruOutVar_names%name = last
    call this%nhruOutVar_names%read(iUnit)
    line = line + this%nhruOutVar_names%size() + ENTRY_OFFSET
  case('nhruOutVars')
    this%nhruOutVars%name = last
    call this%nhruOutVars%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nhruOut_format')
    this%nhruOut_format%name = last
    call this%nhruOut_format%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nhruOut_freq')
    this%nhruOut_freq%name = last
    call this%nhruOut_freq%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nmapOutVars')
    this%nmapOutVars%name = last
    call this%nmapOutVars%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsegmentOutBaseFileName')
    this%nsegmentOutBaseFileName%name = last
    call this%nsegmentOutBaseFileName%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsegmentOutON_OFF')
    this%nsegmentOutON_OFF%name = last
    call this%nsegmentOutON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsegmentOutVar_names')
    this%nsegmentOutVar_names%name = last
    call this%nsegmentOutVar_names%read(iUnit)
    line = line + this%nsegmentOutVar_names%size() + ENTRY_OFFSET
  case('nsegmentOutVars')
    this%nsegmentOutVars%name = last
    call this%nsegmentOutVars%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsegmentOut_format')
    this%nsegmentOut_format%name = last
    call this%nsegmentOut_format%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsegmentOut_freq')
    this%nsegmentOut_freq%name = last
    call this%nsegmentOut_freq%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nstatVars')
    this%nstatVars%name = last
    call this%nstatVars%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsubOutBaseFileName')
    this%nsubOutBaseFileName%name = last
    call this%nsubOutBaseFileName%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsubOutON_OFF')
    this%nsubOutON_OFF%name = last
    call this%nsubOutON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsubOutVar_names')
    this%nsubOutVar_names%name = last
    call this%nsubOutVar_names%read(iUnit)
    line = line + this%nsubOutVar_names%size() + ENTRY_OFFSET
  case('nsubOutVars')
    this%nsubOutVars%name = last
    call this%nsubOutVars%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsubOut_format')
    this%nsubOut_format%name = last
    call this%nsubOut_format%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('nsubOut_freq')
    this%nsubOut_freq%name = last
    call this%nsubOut_freq%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('orad_flag')
    this%orad_flag%name = last
    call this%orad_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('outVarON_OFF')
    this%outVarON_OFF%name = last
    call this%outVarON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('outVar_base_filename')
    this%outVar_base_filename%name = last
    call this%outVar_base_filename%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('outVar_names')
    this%outVar_names%name = last
    call this%outVar_names%read(iUnit)
    line = line + this%outVar_names%size() + ENTRY_OFFSET
  case('param_file')
    this%param_file%name = last
    call this%param_file%read(iUnit)
    line = line + this%param_file%size() + ENTRY_OFFSET
  case('parameter_check_flag')
    this%parameter_check_flag%name = last
    call this%parameter_check_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('potet_day')
    this%potet_day%name = last
    call this%potet_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('potetcoef_dynamic')
    this%potetcoef_dynamic%name = last
    call this%potetcoef_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('precip_day')
    this%precip_day%name = last
    call this%precip_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('precip_module')
    this%precip_module%name = last
    call this%precip_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('print_debug')
    this%print_debug%name = last
    call this%print_debug%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('prms_warmup')
    this%prms_warmup%name = last
    call this%prms_warmup%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('radtrncf_dynamic')
    this%radtrncf_dynamic%name = last
    call this%radtrncf_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('save_vars_to_file')
    this%save_vars_to_file%name = last
    call this%save_vars_to_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('segment_transferON_OFF')
    this%segment_transferON_OFF%name = last
    call this%segment_transferON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('segment_transfer_file')
    this%segment_transfer_file%name = last
    call this%segment_transfer_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('snareathresh_dynamic')
    this%snareathresh_dynamic%name = last
    call this%snareathresh_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('snow_intcp_dynamic')
    this%snow_intcp_dynamic%name = last
    call this%snow_intcp_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('soilmoist_dynamic')
    this%soilmoist_dynamic%name = last
    call this%soilmoist_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('soilrechr_dynamic')
    this%soilrechr_dynamic%name = last
    call this%soilrechr_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('soilzone_module')
    this%soilzone_module%name = last
    call this%soilzone_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('solrad_module')
    this%solrad_module%name = last
    call this%solrad_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('springfrost_dynamic')
    this%springfrost_dynamic%name = last
    call this%springfrost_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('srain_intcp_dynamic')
    this%srain_intcp_dynamic%name = last
    call this%srain_intcp_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('sro2dprst_imperv_dynamic')
    this%sro2dprst_imperv_dynamic%name = last
    call this%sro2dprst_imperv_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('sro2dprst_perv_dynamic')
    this%sro2dprst_perv_dynamic%name = last
    call this%sro2dprst_perv_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('srunoff_module')
    this%srunoff_module%name = last
    call this%srunoff_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('start_time')
    this%start_time%name = last
    call this%start_time%read(iUnit)
    line = line + this%start_time%size() + ENTRY_OFFSET
  case('statVar_element')
    this%statVar_element%name = last
    call this%statVar_element%read(iUnit)
    line = line + this%statVar_element%size() + ENTRY_OFFSET
  case('statVar_names')
    this%statVar_names%name = last
    call this%statVar_names%read(iUnit)
    line = line + this%statVar_names%size() + ENTRY_OFFSET
  case('statsON_OFF')
    this%statsON_OFF%name = last
    call this%statsON_OFF%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('stats_output_file')
    this%stats_output_file%name = last
    call this%stats_output_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('stat_var_file')
    this%stat_var_file%name = last
    call this%stat_var_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('stream_temp_flag')
    this%stream_temp_flag%name = last
    call this%stream_temp_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('stream_temp_shade_flag')
    this%stream_temp_shade_flag%name = last
    call this%stream_temp_shade_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('strmflow_module')
    this%strmflow_module%name = last
    call this%strmflow_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('strmtemp_humidity_flag')
    this%strmtemp_humidity_flag%name = last
    call this%strmtemp_humidity_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('subbasin_flag')
    this%subbasin_flag%name = last
    call this%subbasin_flag%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('swrad_day')
    this%swrad_day%name = last
    call this%swrad_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('temp_module')
    this%temp_module%name = last
    call this%temp_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('tmax_day')
    this%tmax_day%name = last
    call this%tmax_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('tmin_day')
    this%tmin_day%name = last
    call this%tmin_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('transp_day')
    this%transp_day%name = last
    call this%transp_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('transp_module')
    this%transp_module%name = last
    call this%transp_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('transp_on_dynamic')
    this%transp_on_dynamic%name = last
    call this%transp_on_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('transpbeg_dynamic')
    this%transpbeg_dynamic%name = last
    call this%transpbeg_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('transpend_dynamic')
    this%transpend_dynamic%name = last
    call this%transpend_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('var_init_file')
    this%var_init_file%name = last
    call this%var_init_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('var_save_file')
    this%var_save_file%name = last
    call this%var_save_file%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('wind_module')
    this%wind_module%name = last
    call this%wind_module%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('windspeed_day')
    this%windspeed_day%name = last
    call this%windspeed_day%read(iUnit)
    line = line + 1 + ENTRY_OFFSET
  case('wrain_intcp_dynamic')
    this%wrain_intcp_dynamic%name = last
    call this%wrain_intcp_dynamic%read(iUnit)
    line = line + 1 + ENTRY_OFFSET

  case default
    ! Skip to the next ####
end select
