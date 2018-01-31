module m_control
    use rVariable_class, only: rVariable
    use iVariable_class, only: iVariable
    implicit none



    type(iVariable) :: print_debug
    type(iVariable) :: parameter_check_flag
    type(iVariable) :: parameter_check_flag
    type(iVariable) :: cbh_check_flag
    type(iVariable) :: cbh_binary_flag
    
    type(iVariable) :: save_vars_to_file
    type(iVariable) :: init_vars_from_file
    type(iVariable) :: prms_warmup

    !
    ! model_mode
    !
    ! precip_module
    ! temp_module
    ! solrad_module
    ! et_module
    ! transp_module
    !
    ! data_file
    ! param_file
    ! model_output_file
    ! var_save_file
    ! var_init_file
    !
    ! tmax_day
    ! tmin_day
    ! precip_day

    type(iVariable) :: start_time
    type(iVariable) :: end_time

    type(iVariable) :: basinOutON_OFF
    type(iVariable) :: basinOutVars
    type(iVariable) :: basinOut_freq
    ! basinOutVar_names
    ! basinOutBaseFileName

    type(iVariable) :: nhruOutON_OFF
    type(iVariable) :: nhruOutVars
    type(iVariable) :: nhruOut_freq
    ! nhruOutVar_names
    ! nhruOutBaseFileName

end module
