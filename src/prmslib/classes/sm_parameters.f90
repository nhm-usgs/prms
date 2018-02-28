submodule (Parameters_class) sm_parameters

contains
  !====================================================================!
  module function constructor_Parameters(Control_data) result(this)
    type(Parameters) :: this
    class(Control), intent(in) :: Control_data

    this%parameter_filenames = Control_data%param_file
    call this%read()
  end function

  module subroutine read_parameters(this)
    use iso_fortran_env
    use variableKind
    use m_errors, only: eMsg
    use m_fileIO, only: openFile, closeFile
    use m_strings, only: compact, str
    use prms_constants, only: ENTRY_DELIMITER
    implicit none

    class(Parameters), intent(inout) :: this

    integer(i32) :: istat
      !! Contains the IOSTAT result from a read command
    integer(i32) :: iUnit
      !! Unit of the opened control file
    integer(i32) :: line
      !! Tracks the number of the last line read in the file
    character(len=cLen) :: buf
      !! Buffer for reading control file
    character(len=:), allocatable :: last
      !! Previous line read from file
    type(Abc), pointer :: ptr

    logical :: go

    integer(i32) :: k
    integer(i32) :: numfiles

    !***********************************************************************

    ! NOTE: Dimensions no longer stored in the parameter file. They
    !       are now stored in the control file.

    ! Read all parameters and verify
    numfiles = size(this%parameter_filenames%values)
    ! Read_parameters = 0

    do k = 1, numfiles
      ! Open parameter file
      call openFile(this%parameter_filenames%values(k)%s, iunit, stat='old', istat=istat)

      ! Read the Header line
      read(iUnit, 1) buf
      line = 1
      last = 'Header_1'

      read(iUnit, 1) buf
      line = line + 1
      last = 'Header_2'

      ! Read the next line - should be '####'
      read(iUnit, 1) buf
      call compact(buf)
      line = line + 1

      write(*, *) 'Working on' // this%parameter_filenames%values(k)%s

      go = .true.
      do while (go)
        ! read (iUnit, '(A)', IOSTAT=istat) buf
        ! line = line + 1
        ! if (istat == IOSTAT_END) EXIT ! found end of a Parameter File

        ! if (buf(:4) == '    ') CYCLE ! skip blank lines
        ! if (buf(:2) == '//') CYCLE ! skip comment lines

        if (buf(:4) == ENTRY_DELIMITER) then
          ! ~~~~~~~~~~~~~~~~~~
          ! Parameter name
          read(iUnit, '(A)', IOSTAT=istat) buf
          line = line + 1
          last = trim(buf)

          select case(buf)
            case('adjmix_rain')
              call this%adjmix_rain%read(iUnit)
              line = line + this%adjmix_rain%size() + this%adjmix_rain%dim_names%size() + 2
              call this%adjmix_rain%print()

            case('albset_rna')
              call this%albset_rna%read(iUnit)
              line = line + this%albset_rna%size() + 2
              call this%albset_rna%print()

            case('albset_rnm')
              call this%albset_rnm%read(iUnit)
              line = line + this%albset_rnm%size() + 2
              call this%albset_rnm%print()

            case('albset_sna')
              call this%albset_sna%read(iUnit)
              line = line + this%albset_sna%size() + 2
              call this%albset_sna%print()

            case('albset_snm')
              call this%albset_snm%read(iUnit)
              line = line + this%albset_snm%size() + 2
              call this%albset_snm%print()

            case('basin_fall_frost')
              call this%basin_fall_frost%read(iUnit)
              line = line + this%basin_fall_frost%size() + 2
              call this%basin_fall_frost%print()

            case('basin_spring_frost')
              call this%basin_spring_frost%read(iUnit)
              line = line + this%basin_spring_frost%size() + 2
              call this%basin_spring_frost%print()

            case('carea_max')
              call this%carea_max%read(iUnit)
              line = line + this%carea_max%size() + 2
              call this%carea_max%print()

            case('cecn_coef')
              call this%cecn_coef%read(iUnit)
              line = line + this%cecn_coef%size() + 2
              call this%cecn_coef%print()

            case('cov_type')
              call this%cov_type%read(iUnit)
              line = line + this%cov_type%size() + 2
              call this%cov_type%print()

            case('covden_sum')
              call this%covden_sum%read(iUnit)
              line = line + this%covden_sum%size() + 2
              call this%covden_sum%print()

            case('covden_win')
              call this%covden_win%read(iUnit)
              line = line + this%covden_win%size() + 2
              call this%covden_win%print()

            case('dday_intcp')
              call this%dday_intcp%read(iUnit)
              line = line + this%dday_intcp%size() + 2
              call this%dday_intcp%print()

            case('dday_slope')
              call this%dday_slope%read(iUnit)
              line = line + this%dday_slope%size() + 2
              call this%dday_slope%print()

            case('den_init')
              call this%den_init%read(iUnit)
              line = line + this%den_init%size() + 2
              call this%den_init%print()

            case('den_max')
              call this%den_max%read(iUnit)
              line = line + this%den_max%size() + 2
              call this%den_max%print()

            case('dprst_depth_avg')
              call this%dprst_depth_avg%read(iUnit)
              line = line + this%dprst_depth_avg%size() + 2
              call this%dprst_depth_avg%print()

            case('dprst_et_coef')
              call this%dprst_et_coef%read(iUnit)
              line = line + this%dprst_et_coef%size() + 2
              call this%dprst_et_coef%print()

            case('dprst_flow_coef')
              call this%dprst_flow_coef%read(iUnit)
              line = line + this%dprst_flow_coef%size() + 2
              call this%dprst_flow_coef%print()

            case('dprst_frac')
              call this%dprst_frac%read(iUnit)
              line = line + this%dprst_frac%size() + 2
              call this%dprst_frac%print()

            case('dprst_frac_init')
              call this%dprst_frac_init%read(iUnit)
              line = line + this%dprst_frac_init%size() + 2
              call this%dprst_frac_init%print()

            case('dprst_frac_open')
              call this%dprst_frac_open%read(iUnit)
              line = line + this%dprst_frac_open%size() + 2
              call this%dprst_frac_open%print()

            case('dprst_seep_rate_clos')
              call this%dprst_seep_rate_clos%read(iUnit)
              line = line + this%dprst_seep_rate_clos%size() + 2
              call this%dprst_seep_rate_clos%print()

            case('dprst_seep_rate_open')
              call this%dprst_seep_rate_open%read(iUnit)
              line = line + this%dprst_seep_rate_open%size() + 2
              call this%dprst_seep_rate_open%print()

            case('elev_units')
              ptr => this%elev_units
              call ptr%read(iUnit)
              line = line + ptr%size() + 2
              call ptr%print()
              ! call this%elev_units%read(iUnit)
              ! line = line + this%elev_units%size() + 2
              ! call this%elev_units%print()

            case('emis_noppt')
              call this%emis_noppt%read(iUnit)
              line = line + this%emis_noppt%size() + 2
              call this%emis_noppt%print()

            case('epan_coef')
              call this%epan_coef%read(iUnit)
              line = line + this%epan_coef%size() + 2
              call this%epan_coef%print()

            case('fall_frost')
              call this%fall_frost%read(iUnit)
              line = line + this%fall_frost%size() + 2
              call this%fall_frost%print()

            case('fastcoef_lin')
              call this%fastcoef_lin%read(iUnit)
              line = line + this%fastcoef_lin%size() + 2
              call this%fastcoef_lin%print()

            case('fastcoef_sq')
              call this%fastcoef_sq%read(iUnit)
              line = line + this%fastcoef_sq%size() + 2
              call this%fastcoef_sq%print()

            case('freeh2o_cap')
              call this%freeh2o_cap%read(iUnit)
              line = line + this%freeh2o_cap%size() + 2
              call this%freeh2o_cap%print()

            case('gwflow_coef')
              call this%gwflow_coef%read(iUnit)
              line = line + this%gwflow_coef%size() + 2
              call this%gwflow_coef%print()

            case('gwsink_coef')
              call this%gwsink_coef%read(iUnit)
              line = line + this%gwsink_coef%size() + 2
              call this%gwsink_coef%print()

            case('gwstor_init')
              call this%gwstor_init%read(iUnit)
              line = line + this%gwstor_init%size() + 2
              call this%gwstor_init%print()

            case('gwstor_min')
              call this%gwstor_min%read(iUnit)
              line = line + this%gwstor_min%size() + 2
              call this%gwstor_min%print()

            case('hru_area')
              call this%hru_area%read(iUnit)
              line = line + this%hru_area%size() + 2
              call this%hru_area%print()

            case('hru_aspect')
              call this%hru_aspect%read(iUnit)
              line = line + this%hru_aspect%size() + 2
              call this%hru_aspect%print()

            case('hru_deplcrv')
              call this%hru_deplcrv%read(iUnit)
              line = line + this%hru_deplcrv%size() + 2
              call this%hru_deplcrv%print()

            case('hru_elev')
              call this%hru_elev%read(iUnit)
              line = line + this%hru_elev%size() + 2
              call this%hru_elev%print()

            case('hru_lat')
              call this%hru_lat%read(iUnit)
              line = line + this%hru_lat%size() + 2
              call this%hru_lat%print()

            case('hru_lon')
              call this%hru_lon%read(iUnit)
              line = line + this%hru_lon%size() + 2
              call this%hru_lon%print()

            case('hru_percent_imperv')
              call this%hru_percent_imperv%read(iUnit)
              line = line + this%hru_percent_imperv%size() + 2
              call this%hru_percent_imperv%print()

            case('hru_segment')
              call this%hru_segment%read(iUnit)
              line = line + this%hru_segment%size() + 2
              call this%hru_segment%print()

            case('hru_segment_nhm')
              call this%hru_segment_nhm%read(iUnit)
              line = line + this%hru_segment_nhm%size() + 2
              call this%hru_segment_nhm%print()

            case('hru_slope')
              call this%hru_slope%read(iUnit)
              line = line + this%hru_slope%size() + 2
              call this%hru_slope%print()

            case('hru_type')
              call this%hru_type%read(iUnit)
              line = line + this%hru_type%size() + 2
              call this%hru_type%print()

            case('hru_x')
              call this%hru_x%read(iUnit)
              line = line + this%hru_x%size() + 2
              call this%hru_x%print()

            case('hru_y')
              call this%hru_y%read(iUnit)
              line = line + this%hru_y%size() + 2
              call this%hru_y%print()

            case('imperv_stor_max')
              call this%imperv_stor_max%read(iUnit)
              line = line + this%imperv_stor_max%size() + 2
              call this%imperv_stor_max%print()

            case('jh_coef')
              call this%jh_coef%read(iUnit)
              line = line + this%jh_coef%size() + 2
              call this%jh_coef%print()

            case('jh_coef_hru')
              call this%jh_coef_hru%read(iUnit)
              line = line + this%jh_coef_hru%size() + 2
              call this%jh_coef_hru%print()

            case('K_coef')
              call this%K_coef%read(iUnit)
              line = line + this%K_coef%size() + 2
              call this%K_coef%print()

            case('melt_force')
              call this%melt_force%read(iUnit)
              line = line + this%melt_force%size() + 2
              call this%melt_force%print()

            case('melt_look')
              call this%melt_look%read(iUnit)
              line = line + this%melt_look%size() + 2
              call this%melt_look%print()

            case('nhm_deplcrv')
              call this%nhm_deplcrv%read(iUnit)
              line = line + this%nhm_deplcrv%size() + 2
              call this%nhm_deplcrv%print()

            case('nhm_id')
              call this%nhm_id%read(iUnit)
              line = line + this%nhm_id%size() + 2
              call this%nhm_id%print()

            case('nhm_seg')
              call this%nhm_seg%read(iUnit)
              line = line + this%nhm_seg%size() + 2
              call this%nhm_seg%print()

            case('obsin_segment')
              call this%obsin_segment%read(iUnit)
              line = line + this%obsin_segment%size() + 2
              call this%obsin_segment%print()

            case('op_flow_thres')
              call this%op_flow_thres%read(iUnit)
              line = line + this%op_flow_thres%size() + 2
              call this%op_flow_thres%print()

            case('outlet_sta')
              call this%outlet_sta%read(iUnit)
              line = line + this%outlet_sta%size() + 2
              call this%outlet_sta%print()

            case('poi_gage_id')
              call this%poi_gage_id%read(iUnit)
              line = line + this%poi_gage_id%size() + 2
              call this%poi_gage_id%print()

            case('poi_gage_segment')
              call this%poi_gage_segment%read(iUnit)
              line = line + this%poi_gage_segment%size() + 2
              call this%poi_gage_segment%print()

            case('poi_type')
              call this%poi_type%read(iUnit)
              line = line + this%poi_type%size() + 2
              call this%poi_type%print()

            case('potet_sublim')
              call this%potet_sublim%read(iUnit)
              line = line + this%potet_sublim%size() + 2
              call this%potet_sublim%print()

            case('ppt_rad_adj')
              call this%ppt_rad_adj%read(iUnit)
              line = line + this%ppt_rad_adj%size() + 2
              call this%ppt_rad_adj%print()

            case('precip_units')
              call this%precip_units%read(iUnit)
              line = line + this%precip_units%size() + 2
              call this%precip_units%print()

            case('pref_flow_den')
              call this%pref_flow_den%read(iUnit)
              line = line + this%pref_flow_den%size() + 2
              call this%pref_flow_den%print()

            case('print_freq')
              call this%print_freq%read(iUnit)
              line = line + this%print_freq%size() + 2
              call this%print_freq%print()

            case('print_type')
              call this%print_type%read(iUnit)
              line = line + this%print_type%size() + 2
              call this%print_type%print()

            case('rad_trncf')
              call this%rad_trncf%read(iUnit)
              line = line + this%rad_trncf%size() + 2
              call this%rad_trncf%print()

            case('radadj_intcp')
              call this%radadj_intcp%read(iUnit)
              line = line + this%radadj_intcp%size() + 2
              call this%radadj_intcp%print()

            case('radadj_slope')
              call this%radadj_slope%read(iUnit)
              line = line + this%radadj_slope%size() + 2
              call this%radadj_slope%print()

            case('radj_sppt')
              call this%radj_sppt%read(iUnit)
              line = line + this%radj_sppt%size() + 2
              call this%radj_sppt%print()

            case('radj_wppt')
              call this%radj_wppt%read(iUnit)
              line = line + this%radj_wppt%size() + 2
              call this%radj_wppt%print()

            case('radmax')
              call this%radmax%read(iUnit)
              line = line + this%radmax%size() + 2
              call this%radmax%print()

            case('rain_cbh_adj')
              call this%rain_cbh_adj%read(iUnit)
              line = line + this%rain_cbh_adj%size() + 2
              call this%rain_cbh_adj%print()

            case('runoff_units')
              call this%runoff_units%read(iUnit)
              line = line + this%runoff_units%size() + 2
              call this%runoff_units%print()

            case('sat_threshold')
              call this%sat_threshold%read(iUnit)
              line = line + this%sat_threshold%size() + 2
              call this%sat_threshold%print()

            case('segment_flow_init')
              call this%segment_flow_init%read(iUnit)
              line = line + this%segment_flow_init%size() + 2
              call this%segment_flow_init%print()

            case('segment_type')
              call this%segment_type%read(iUnit)
              line = line + this%segment_type%size() + 2
              call this%segment_type%print()

            case('settle_const')
              call this%settle_const%read(iUnit)
              line = line + this%settle_const%size() + 2
              call this%settle_const%print()

            case('slowcoef_lin')
              call this%slowcoef_lin%read(iUnit)
              line = line + this%slowcoef_lin%size() + 2
              call this%slowcoef_lin%print()

            case('slowcoef_sq')
              call this%slowcoef_sq%read(iUnit)
              line = line + this%slowcoef_sq%size() + 2
              call this%slowcoef_sq%print()

            case('smidx_coef')
              call this%smidx_coef%read(iUnit)
              line = line + this%smidx_coef%size() + 2
              call this%smidx_coef%print()

            case('smidx_exp')
              call this%smidx_exp%read(iUnit)
              line = line + this%smidx_exp%size() + 2
              call this%smidx_exp%print()

            case('snarea_curve')
              call this%snarea_curve%read(iUnit)
              line = line + this%snarea_curve%size() + 2
              call this%snarea_curve%print()

            case('snarea_thresh')
              call this%snarea_thresh%read(iUnit)
              line = line + this%snarea_thresh%size() + 2
              call this%snarea_thresh%print()

            case('snow_cbh_adj')
              call this%snow_cbh_adj%read(iUnit)
              line = line + this%snow_cbh_adj%size() + 2
              call this%snow_cbh_adj%print()

            case('snow_intcp')
              call this%snow_intcp%read(iUnit)
              line = line + this%snow_intcp%size() + 2
              call this%snow_intcp%print()

            case('snowinfil_max')
              call this%snowinfil_max%read(iUnit)
              line = line + this%snowinfil_max%size() + 2
              call this%snowinfil_max%print()

            case('snowpack_init')
              call this%snowpack_init%read(iUnit)
              line = line + this%snowpack_init%size() + 2
              call this%snowpack_init%print()

            case('soil2gw_max')
              call this%soil2gw_max%read(iUnit)
              line = line + this%soil2gw_max%size() + 2
              call this%soil2gw_max%print()

            case('soil_moist_init')
              call this%soil_moist_init%read(iUnit)
              line = line + this%soil_moist_init%size() + 2
              call this%soil_moist_init%print()

            case('soil_rechr_init_frac')
              call this%soil_rechr_init_frac%read(iUnit)
              line = line + this%soil_rechr_init_frac%size() + 2
              call this%soil_rechr_init_frac%print()

            case('soil_rechr_max_frac')
              call this%soil_rechr_max_frac%read(iUnit)
              line = line + this%soil_rechr_max_frac%size() + 2
              call this%soil_rechr_max_frac%print()

            case('soil_type')
              call this%soil_type%read(iUnit)
              line = line + this%soil_type%size() + 2
              call this%soil_type%print()

            case('spring_frost')
              call this%spring_frost%read(iUnit)
              line = line + this%spring_frost%size() + 2
              call this%spring_frost%print()

            case('srain_intcp')
              call this%srain_intcp%read(iUnit)
              line = line + this%srain_intcp%size() + 2
              call this%srain_intcp%print()

            case('sro_to_dprst_imperv')
              call this%sro_to_dprst_imperv%read(iUnit)
              line = line + this%sro_to_dprst_imperv%size() + 2
              call this%sro_to_dprst_imperv%print()

            case('sro_to_dprst_perv')
              call this%sro_to_dprst_perv%read(iUnit)
              line = line + this%sro_to_dprst_perv%size() + 2
              call this%sro_to_dprst_perv%print()

            case('ssr2gw_exp')
              call this%ssr2gw_exp%read(iUnit)
              line = line + this%ssr2gw_exp%size() + 2
              call this%ssr2gw_exp%print()

            case('ssr2gw_rate')
              call this%ssr2gw_rate%read(iUnit)
              line = line + this%ssr2gw_rate%size() + 2
              call this%ssr2gw_rate%print()

            case('ssstor_init')
              call this%ssstor_init%read(iUnit)
              line = line + this%ssstor_init%size() + 2
              call this%ssstor_init%print()

            case('ssstor_init_frac')
              call this%ssstor_init_frac%read(iUnit)
              line = line + this%ssstor_init_frac%size() + 2
              call this%ssstor_init_frac%print()

            case('temp_units')
              call this%temp_units%read(iUnit)
              line = line + this%temp_units%size() + 2
              call this%temp_units%print()

            case('tmax_allrain_offset')
              call this%tmax_allrain_offset%read(iUnit)
              line = line + this%tmax_allrain_offset%size() + 2
              call this%tmax_allrain_offset%print()

            case('tmax_allsnow')
              call this%tmax_allsnow%read(iUnit)
              line = line + this%tmax_allsnow%size() + 2
              call this%tmax_allsnow%print()

            case('tmax_cbh_adj')
              call this%tmax_cbh_adj%read(iUnit)
              line = line + this%tmax_cbh_adj%size() + 2
              call this%tmax_cbh_adj%print()

            case('tmax_index')
              call this%tmax_index%read(iUnit)
              line = line + this%tmax_index%size() + 2
              call this%tmax_index%print()

            case('tmin_cbh_adj')
              call this%tmin_cbh_adj%read(iUnit)
              line = line + this%tmin_cbh_adj%size() + 2
              call this%tmin_cbh_adj%print()

            case('tosegment')
              call this%tosegment%read(iUnit)
              line = line + this%tosegment%size() + 2
              call this%tosegment%print()

            case('tosegment_nhm')
              call this%tosegment_nhm%read(iUnit)
              line = line + this%tosegment_nhm%size() + 2
              call this%tosegment_nhm%print()

            case('transp_beg')
              call this%transp_beg%read(iUnit)
              line = line + this%transp_beg%size() + 2
              call this%transp_beg%print()

            case('transp_end')
              call this%transp_end%read(iUnit)
              line = line + this%transp_end%size() + 2
              call this%transp_end%print()

            case('transp_tmax')
              call this%transp_tmax%read(iUnit)
              line = line + this%transp_tmax%size() + 2
              call this%transp_tmax%print()

            case('tstorm_mo')
              call this%tstorm_mo%read(iUnit)
              line = line + this%tstorm_mo%size() + 2
              call this%tstorm_mo%print()

            case('va_clos_exp')
              call this%va_clos_exp%read(iUnit)
              line = line + this%va_clos_exp%size() + 2
              call this%va_clos_exp%print()

            case('va_open_exp')
              call this%va_open_exp%read(iUnit)
              line = line + this%va_open_exp%size() + 2
              call this%va_open_exp%print()

            case('wrain_intcp')
              call this%wrain_intcp%read(iUnit)
              line = line + this%wrain_intcp%size() + 2
              call this%wrain_intcp%print()

            case('x_coef')
              call this%x_coef%read(iUnit)
              line = line + this%x_coef%size() + 2
              call this%x_coef%print()

            ! case('<>')
            !   call this%<>%read(iUnit)
            !   line = line + this%<>%size() + 2
            !   call this%<>%print()

            case default
              ! pass
          end select
        else
          ! Backup the line counter by one which will be where the problem occurred
          line = line - 1

          call eMsg("Could not read from file " // this%parameter_filenames%values(k)%s // &
                    " for entry " // last // " at line " // str(line))
        endif

        read(iUnit, 1, IOSTAT=istat) buf
        if (istat == IOSTAT_END) exit
        call compact(buf)
        line = line + 1
      enddo

      call closeFile(this%parameter_filenames%values(k)%s, iUnit, '', istat)
      1   format(a)
    enddo
  end subroutine
end submodule
