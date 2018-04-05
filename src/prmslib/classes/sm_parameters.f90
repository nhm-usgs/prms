submodule (Parameters_class) sm_parameters

contains
  !====================================================================!
  module function constructor_Parameters(Control_data) result(this)
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Parameters) :: this
    class(Control), intent(in) :: Control_data

    ! --------------------------------------------------------------------------
    associate(print_debug => Control_data%print_debug%values(1), &
              param_file => Control_data%param_file%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      this%parameter_filenames = Control_data%param_file
    end associate

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
    ! type(Abc), pointer :: ptr

    logical :: go

    integer(i32), parameter :: ENTRY_OFFSET = 2
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

      ! write(*, *) '-- reading' // this%parameter_filenames%values(k)%s

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
            case('K_coef')
              this%K_coef%name = last
              call this%K_coef%read(iUnit)
              line = line + this%K_coef%size() + this%K_coef%dim_names%size() + ENTRY_OFFSET
            case('adjmix_rain')
              this%adjmix_rain%name = last
              call this%adjmix_rain%read(iUnit)
              line = line + this%adjmix_rain%size() + this%adjmix_rain%dim_names%size() + ENTRY_OFFSET
            case('adjust_rain')
              this%adjust_rain%name = last
              call this%adjust_rain%read(iUnit)
              line = line + this%adjust_rain%size() + this%adjust_rain%dim_names%size() + ENTRY_OFFSET
            case('adjust_snow')
              this%adjust_snow%name = last
              call this%adjust_snow%read(iUnit)
              line = line + this%adjust_snow%size() + this%adjust_snow%dim_names%size() + ENTRY_OFFSET
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
            case('conv_flag')
              this%conv_flag%name = last
              call this%conv_flag%read(iUnit)
              line = line + this%conv_flag%size() + this%conv_flag%dim_names%size() + ENTRY_OFFSET
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
            case('dist_exp')
              this%dist_exp%name = last
              call this%dist_exp%read(iUnit)
              line = line + this%dist_exp%size() + this%dist_exp%dim_names%size() + ENTRY_OFFSET
            case('dist_max')
              this%dist_max%name = last
              call this%dist_max%read(iUnit)
              line = line + this%dist_max%size() + this%dist_max%dim_names%size() + ENTRY_OFFSET
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
            case('elev_units')
              this%elev_units%name = last
              call this%elev_units%read(iUnit)
              line = line + this%elev_units%size() + this%elev_units%dim_names%size() + ENTRY_OFFSET
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
            case('hru_elev')
              this%hru_elev%name = last
              call this%hru_elev%read(iUnit)
              line = line + this%hru_elev%size() + this%hru_elev%dim_names%size() + ENTRY_OFFSET
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
            case('imperv_stor_max')
              this%imperv_stor_max%name = last
              call this%imperv_stor_max%read(iUnit)
              line = line + this%imperv_stor_max%size() + this%imperv_stor_max%dim_names%size() + ENTRY_OFFSET
            case('jh_coef')
              this%jh_coef%name = last
              call this%jh_coef%read(iUnit)
              line = line + this%jh_coef%size() + this%jh_coef%dim_names%size() + ENTRY_OFFSET
            case('jh_coef_hru')
              this%jh_coef_hru%name = last
              call this%jh_coef_hru%read(iUnit)
              line = line + this%jh_coef_hru%size() + this%jh_coef_hru%dim_names%size() + ENTRY_OFFSET
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
            case('max_lapse')
              this%max_lapse%name = last
              call this%max_lapse%read(iUnit)
              line = line + this%max_lapse%size() + this%max_lapse%dim_names%size() + ENTRY_OFFSET
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
            case('melt_force')
              this%melt_force%name = last
              call this%melt_force%read(iUnit)
              line = line + this%melt_force%size() + this%melt_force%dim_names%size() + ENTRY_OFFSET
            case('melt_look')
              this%melt_look%name = last
              call this%melt_look%read(iUnit)
              line = line + this%melt_look%size() + this%melt_look%dim_names%size() + ENTRY_OFFSET
            case('min_lapse')
              this%min_lapse%name = last
              call this%min_lapse%read(iUnit)
              line = line + this%min_lapse%size() + this%min_lapse%dim_names%size() + ENTRY_OFFSET
            case('monmax')
              this%monmax%name = last
              call this%monmax%read(iUnit)
              line = line + this%monmax%size() + this%monmax%dim_names%size() + ENTRY_OFFSET
            case('monmin')
              this%monmin%name = last
              call this%monmin%read(iUnit)
              line = line + this%monmin%size() + this%monmin%dim_names%size() + ENTRY_OFFSET
            case('ndist_psta')
              this%ndist_psta%name = last
              call this%ndist_psta%read(iUnit)
              line = line + this%ndist_psta%size() + this%ndist_psta%dim_names%size() + ENTRY_OFFSET
            case('ndist_tsta')
              this%ndist_tsta%name = last
              call this%ndist_tsta%read(iUnit)
              line = line + this%ndist_tsta%size() + this%ndist_tsta%dim_names%size() + ENTRY_OFFSET
            case('nhm_id')
              this%nhm_id%name = last
              call this%nhm_id%read(iUnit)
              line = line + this%nhm_id%size() + this%nhm_id%dim_names%size() + ENTRY_OFFSET
            case('nhm_seg')
              this%nhm_seg%name = last
              call this%nhm_seg%read(iUnit)
              line = line + this%nhm_seg%size() + this%nhm_seg%dim_names%size() + ENTRY_OFFSET
            case('obsin_segment')
              this%obsin_segment%name = last
              call this%obsin_segment%read(iUnit)
              line = line + this%obsin_segment%size() + this%obsin_segment%dim_names%size() + ENTRY_OFFSET
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
            case('ppt_add')
              this%ppt_add%name = last
              call this%ppt_add%read(iUnit)
              line = line + this%ppt_add%size() + this%ppt_add%dim_names%size() + ENTRY_OFFSET
            case('ppt_div')
              this%ppt_div%name = last
              call this%ppt_div%read(iUnit)
              line = line + this%ppt_div%size() + this%ppt_div%dim_names%size() + ENTRY_OFFSET
            case('ppt_lapse')
              this%ppt_lapse%name = last
              call this%ppt_lapse%read(iUnit)
              line = line + this%ppt_lapse%size() + this%ppt_lapse%dim_names%size() + ENTRY_OFFSET
            case('ppt_rad_adj')
              this%ppt_rad_adj%name = last
              call this%ppt_rad_adj%read(iUnit)
              line = line + this%ppt_rad_adj%size() + this%ppt_rad_adj%dim_names%size() + ENTRY_OFFSET
            case('prcp_wght_dist')
              this%prcp_wght_dist%name = last
              call this%prcp_wght_dist%read(iUnit)
              line = line + this%prcp_wght_dist%size() + this%prcp_wght_dist%dim_names%size() + ENTRY_OFFSET
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
            case('psta_freq_nuse')
              this%psta_freq_nuse%name = last
              call this%psta_freq_nuse%read(iUnit)
              line = line + this%psta_freq_nuse%size() + this%psta_freq_nuse%dim_names%size() + ENTRY_OFFSET
            case('psta_mon')
              this%psta_mon%name = last
              call this%psta_mon%read(iUnit)
              line = line + this%psta_mon%size() + this%psta_mon%dim_names%size() + ENTRY_OFFSET
            case('psta_month_ppt')
              this%psta_month_ppt%name = last
              call this%psta_month_ppt%read(iUnit)
              line = line + this%psta_month_ppt%size() + this%psta_month_ppt%dim_names%size() + ENTRY_OFFSET
            case('psta_nuse')
              this%psta_nuse%name = last
              call this%psta_nuse%read(iUnit)
              line = line + this%psta_nuse%size() + this%psta_nuse%dim_names%size() + ENTRY_OFFSET
            case('psta_x')
              this%psta_x%name = last
              call this%psta_x%read(iUnit)
              line = line + this%psta_x%size() + this%psta_x%dim_names%size() + ENTRY_OFFSET
            case('psta_xlong')
              this%psta_xlong%name = last
              call this%psta_xlong%read(iUnit)
              line = line + this%psta_xlong%size() + this%psta_xlong%dim_names%size() + ENTRY_OFFSET
            case('psta_y')
              this%psta_y%name = last
              call this%psta_y%read(iUnit)
              line = line + this%psta_y%size() + this%psta_y%dim_names%size() + ENTRY_OFFSET
            case('psta_ylat')
              this%psta_ylat%name = last
              call this%psta_ylat%read(iUnit)
              line = line + this%psta_ylat%size() + this%psta_ylat%dim_names%size() + ENTRY_OFFSET
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
            case('runoff_units')
              this%runoff_units%name = last
              call this%runoff_units%read(iUnit)
              line = line + this%runoff_units%size() + this%runoff_units%dim_names%size() + ENTRY_OFFSET
            case('sat_threshold')
              this%sat_threshold%name = last
              call this%sat_threshold%read(iUnit)
              line = line + this%sat_threshold%size() + this%sat_threshold%dim_names%size() + ENTRY_OFFSET
            case('segment_flow_init')
              this%segment_flow_init%name = last
              call this%segment_flow_init%read(iUnit)
              line = line + this%segment_flow_init%size() + this%segment_flow_init%dim_names%size() + ENTRY_OFFSET
            case('segment_type')
              this%segment_type%name = last
              call this%segment_type%read(iUnit)
              line = line + this%segment_type%size() + this%segment_type%dim_names%size() + ENTRY_OFFSET
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
            case('soil_moist_init_frac')
              this%soil_moist_init_frac%name = last
              call this%soil_moist_init_frac%read(iUnit)
              line = line + this%soil_moist_init_frac%size() + this%soil_moist_init_frac%dim_names%size() + ENTRY_OFFSET
            case('soil_moist_max')
              this%soil_moist_max%name = last
              call this%soil_moist_max%read(iUnit)
              line = line + this%soil_moist_max%size() + this%soil_moist_max%dim_names%size() + ENTRY_OFFSET
            case('soil_rechr_init_frac')
              this%soil_rechr_init_frac%name = last
              call this%soil_rechr_init_frac%read(iUnit)
              line = line + this%soil_rechr_init_frac%size() + this%soil_rechr_init_frac%dim_names%size() + ENTRY_OFFSET
            case('soil_rechr_max_frac')
              this%soil_rechr_max_frac%name = last
              call this%soil_rechr_max_frac%read(iUnit)
              line = line + this%soil_rechr_max_frac%size() + this%soil_rechr_max_frac%dim_names%size() + ENTRY_OFFSET
            case('soil_type')
              this%soil_type%name = last
              call this%soil_type%read(iUnit)
              line = line + this%soil_type%size() + this%soil_type%dim_names%size() + ENTRY_OFFSET
            case('solrad_elev')
              this%solrad_elev%name = last
              call this%solrad_elev%read(iUnit)
              line = line + this%solrad_elev%size() + this%solrad_elev%dim_names%size() + ENTRY_OFFSET
            case('spring_frost')
              this%spring_frost%name = last
              call this%spring_frost%read(iUnit)
              line = line + this%spring_frost%size() + this%spring_frost%dim_names%size() + ENTRY_OFFSET
            case('srain_intcp')
              this%srain_intcp%name = last
              call this%srain_intcp%read(iUnit)
              line = line + this%srain_intcp%size() + this%srain_intcp%dim_names%size() + ENTRY_OFFSET
            case('sro_to_dprst_imperv')
              this%sro_to_dprst_imperv%name = last
              call this%sro_to_dprst_imperv%read(iUnit)
              line = line + this%sro_to_dprst_imperv%size() + this%sro_to_dprst_imperv%dim_names%size() + ENTRY_OFFSET
            case('sro_to_dprst_perv')
              this%sro_to_dprst_perv%name = last
              call this%sro_to_dprst_perv%read(iUnit)
              line = line + this%sro_to_dprst_perv%size() + this%sro_to_dprst_perv%dim_names%size() + ENTRY_OFFSET
            case('ssr2gw_exp')
              this%ssr2gw_exp%name = last
              call this%ssr2gw_exp%read(iUnit)
              line = line + this%ssr2gw_exp%size() + this%ssr2gw_exp%dim_names%size() + ENTRY_OFFSET
            case('ssr2gw_rate')
              this%ssr2gw_rate%name = last
              call this%ssr2gw_rate%read(iUnit)
              line = line + this%ssr2gw_rate%size() + this%ssr2gw_rate%dim_names%size() + ENTRY_OFFSET
            case('ssstor_init_frac')
              this%ssstor_init_frac%name = last
              call this%ssstor_init_frac%read(iUnit)
              line = line + this%ssstor_init_frac%size() + this%ssstor_init_frac%dim_names%size() + ENTRY_OFFSET
            case('temp_units')
              this%temp_units%name = last
              call this%temp_units%read(iUnit)
              line = line + this%temp_units%size() + this%temp_units%dim_names%size() + ENTRY_OFFSET
            case('temp_wght_dist')
              this%temp_wght_dist%name = last
              call this%temp_wght_dist%read(iUnit)
              line = line + this%temp_wght_dist%size() + this%temp_wght_dist%dim_names%size() + ENTRY_OFFSET
            case('tmax_add')
              this%tmax_add%name = last
              call this%tmax_add%read(iUnit)
              line = line + this%tmax_add%size() + this%tmax_add%dim_names%size() + ENTRY_OFFSET
            case('tmax_adj')
              this%tmax_adj%name = last
              call this%tmax_adj%read(iUnit)
              line = line + this%tmax_adj%size() + this%tmax_adj%dim_names%size() + ENTRY_OFFSET
            case('tmax_allrain_dist')
              this%tmax_allrain_dist%name = last
              call this%tmax_allrain_dist%read(iUnit)
              line = line + this%tmax_allrain_dist%size() + this%tmax_allrain_dist%dim_names%size() + ENTRY_OFFSET
            case('tmax_allrain_offset')
              this%tmax_allrain_offset%name = last
              call this%tmax_allrain_offset%read(iUnit)
              line = line + this%tmax_allrain_offset%size() + this%tmax_allrain_offset%dim_names%size() + ENTRY_OFFSET
            case('tmax_allrain_sta')
              this%tmax_allrain_sta%name = last
              call this%tmax_allrain_sta%read(iUnit)
              line = line + this%tmax_allrain_sta%size() + this%tmax_allrain_sta%dim_names%size() + ENTRY_OFFSET
            case('tmax_allsnow')
              this%tmax_allsnow%name = last
              call this%tmax_allsnow%read(iUnit)
              line = line + this%tmax_allsnow%size() + this%tmax_allsnow%dim_names%size() + ENTRY_OFFSET
            case('tmax_allsnow_dist')
              this%tmax_allsnow_dist%name = last
              call this%tmax_allsnow_dist%read(iUnit)
              line = line + this%tmax_allsnow_dist%size() + this%tmax_allsnow_dist%dim_names%size() + ENTRY_OFFSET
            case('tmax_allsnow_sta')
              this%tmax_allsnow_sta%name = last
              call this%tmax_allsnow_sta%read(iUnit)
              line = line + this%tmax_allsnow_sta%size() + this%tmax_allsnow_sta%dim_names%size() + ENTRY_OFFSET
            case('tmax_cbh_adj')
              this%tmax_cbh_adj%name = last
              call this%tmax_cbh_adj%read(iUnit)
              line = line + this%tmax_cbh_adj%size() + this%tmax_cbh_adj%dim_names%size() + ENTRY_OFFSET
            case('tmax_div')
              this%tmax_div%name = last
              call this%tmax_div%read(iUnit)
              line = line + this%tmax_div%size() + this%tmax_div%dim_names%size() + ENTRY_OFFSET
            case('tmax_index')
              this%tmax_index%name = last
              call this%tmax_index%read(iUnit)
              line = line + this%tmax_index%size() + this%tmax_index%dim_names%size() + ENTRY_OFFSET
            case('tmax_lapse')
              this%tmax_lapse%name = last
              call this%tmax_lapse%read(iUnit)
              line = line + this%tmax_lapse%size() + this%tmax_lapse%dim_names%size() + ENTRY_OFFSET
            case('tmin_add')
              this%tmin_add%name = last
              call this%tmin_add%read(iUnit)
              line = line + this%tmin_add%size() + this%tmin_add%dim_names%size() + ENTRY_OFFSET
            case('tmin_adj')
              this%tmin_adj%name = last
              call this%tmin_adj%read(iUnit)
              line = line + this%tmin_adj%size() + this%tmin_adj%dim_names%size() + ENTRY_OFFSET
            case('tmin_cbh_adj')
              this%tmin_cbh_adj%name = last
              call this%tmin_cbh_adj%read(iUnit)
              line = line + this%tmin_cbh_adj%size() + this%tmin_cbh_adj%dim_names%size() + ENTRY_OFFSET
            case('tmin_div')
              this%tmin_div%name = last
              call this%tmin_div%read(iUnit)
              line = line + this%tmin_div%size() + this%tmin_div%dim_names%size() + ENTRY_OFFSET
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
            case('tsta_month_max')
              this%tsta_month_max%name = last
              call this%tsta_month_max%read(iUnit)
              line = line + this%tsta_month_max%size() + this%tsta_month_max%dim_names%size() + ENTRY_OFFSET
            case('tsta_month_min')
              this%tsta_month_min%name = last
              call this%tsta_month_min%read(iUnit)
              line = line + this%tsta_month_min%size() + this%tsta_month_min%dim_names%size() + ENTRY_OFFSET
            case('tsta_nuse')
              this%tsta_nuse%name = last
              call this%tsta_nuse%read(iUnit)
              line = line + this%tsta_nuse%size() + this%tsta_nuse%dim_names%size() + ENTRY_OFFSET
            case('tsta_x')
              this%tsta_x%name = last
              call this%tsta_x%read(iUnit)
              line = line + this%tsta_x%size() + this%tsta_x%dim_names%size() + ENTRY_OFFSET
            case('tsta_xlong')
              this%tsta_xlong%name = last
              call this%tsta_xlong%read(iUnit)
              line = line + this%tsta_xlong%size() + this%tsta_xlong%dim_names%size() + ENTRY_OFFSET
            case('tsta_y')
              this%tsta_y%name = last
              call this%tsta_y%read(iUnit)
              line = line + this%tsta_y%size() + this%tsta_y%dim_names%size() + ENTRY_OFFSET
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
            case('wrain_intcp')
              this%wrain_intcp%name = last
              call this%wrain_intcp%read(iUnit)
              line = line + this%wrain_intcp%size() + this%wrain_intcp%dim_names%size() + ENTRY_OFFSET
            case('x_add')
              this%x_add%name = last
              call this%x_add%read(iUnit)
              line = line + this%x_add%size() + this%x_add%dim_names%size() + ENTRY_OFFSET
            case('x_coef')
              this%x_coef%name = last
              call this%x_coef%read(iUnit)
              line = line + this%x_coef%size() + this%x_coef%dim_names%size() + ENTRY_OFFSET
            case('x_div')
              this%x_div%name = last
              call this%x_div%read(iUnit)
              line = line + this%x_div%size() + this%x_div%dim_names%size() + ENTRY_OFFSET
            case('y_add')
              this%y_add%name = last
              call this%y_add%read(iUnit)
              line = line + this%y_add%size() + this%y_add%dim_names%size() + ENTRY_OFFSET
            case('y_div')
              this%y_div%name = last
              call this%y_div%read(iUnit)
              line = line + this%y_div%size() + this%y_div%dim_names%size() + ENTRY_OFFSET
            case('z_add')
              this%z_add%name = last
              call this%z_add%read(iUnit)
              line = line + this%z_add%size() + this%z_add%dim_names%size() + ENTRY_OFFSET
            case('z_div')
              this%z_div%name = last
              call this%z_div%read(iUnit)
              line = line + this%z_div%size() + this%z_div%dim_names%size() + ENTRY_OFFSET
            case default
              ! pass
          end select
        else
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
