submodule(PRMS_POTET) sm_potet
contains
  module subroutine init_Potet(this, ctl_data, model_basin, model_summary)
    use UTILS_CBH, only: find_current_time, find_header_end
    implicit none

    class(Potential_ET), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    ! Local variables
    ! integer(i32) :: ierr
    ! integer(i32) :: istop = 0
    integer(i32) :: jj

    ! Control
    ! nhru, cbh_binary_flag, et_module, humidity_day, stream_temp_flag,
    ! strmtemp_humidity_flag,

    ! --------------------------------------------------------------------------
    associate(outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              et_module => ctl_data%et_module%values(1), &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &

              ! NOTE: humidity_day needs a default value to be associated
              ! humidity_day => ctl_data%humidity_day%values(1), &
              start_time => ctl_data%start_time%values, &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! TODO: This belongs in the potet_pan module not potet parent
      ! allocate(this%epan_coef(nhru, nmonths))
      ! call param_hdl%get_variable('epan_coef', this%epan_coef)

      allocate(this%potet_sublim(nhru))
      call param_hdl%get_variable('potet_sublim', this%potet_sublim)

      allocate(this%potet(nhru))
      this%potet = 0.0

      ! TODO: Figure this out (pulled from climateflow)
      ! if (ctl_data%et_module%values(1)%s /= 'potet_pm' .and. &
      !     ctl_data%et_module%values(1)%s /= 'potet_pt') then
      !   ! ?anything needed?
      ! else
      !   ! This is confusing because humidity_percent appears to only be used
      !   ! by potet_pm and potet_pt. But it's forced to 1.0 in this case which
      !   ! overrides the parameter values.
      !   humidity_percent = 1.0
      ! endif

      ! if (et_module%s == 'potet_pt' .or. et_module%s == 'potet_pm' .or. &
      !     (stream_temp_flag == 1 .and. strmtemp_humidity_flag == 0)) then
      !   allocate(this%humidity_hru(nhru))
      !   this%humidity_hru = 0.0

      !   call find_header_end(nhru, this%humidity_funit, ierr, &
      !                        ctl_data%humidity_day%values(1)%s, &
      !                        'humidity_day', (cbh_binary_flag==1))

      !   if (ierr == 1) then
      !     istop = 1
      !   else
      !     ! TODO: Original code defaults ierr=2 which causes the humidity_cbh_flag
      !     !       to be reset and allows climate_flow to use humidity_percent
      !     !       from the parameter file instead. Need to figure out a good way
      !     !       to handle this.
      !     call find_current_time(ierr, this%humidity_funit, start_time, (cbh_binary_flag==1))
      !   endif
      ! endif

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          ! TODO: This is where the daily basin values are linked based on
          !       what was requested in basinOutVar_names.
          select case(outVar_names%values(jj)%s)
            case('potet')
              call model_summary%set_summary_var(jj, this%potet)
            case('humidity_hru')
              call model_summary%set_summary_var(jj, this%humidity_hru)
            case default
              ! pass
          end select
        enddo
      endif
    end associate
  end subroutine

  module subroutine run_Potet(this, ctl_data, model_basin, model_time, model_solrad, model_temp)
    class(Potential_ET), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    class(SolarRadiation), intent(in) :: model_solrad
    class(Temperature), intent(in) :: model_temp

    ! Local variables
    integer(i32) :: jj
    integer(i32) :: ios
    integer(i32) :: yr, mo, dy, hr, mn, sec

    ! --------------------------------------------------------------------------
    associate(et_module => ctl_data%et_module%values(1), &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value, &

              nhru => model_basin%nhru)

      ! WARNING: PAN - I think this will get moved to child classes
      ! Humidity
      if (et_module%s == 'potet_pt' .or. et_module%s == 'potet_pm' .or. &
          (stream_temp_flag == 1 .and. strmtemp_humidity_flag == 0)) then
        read(this%humidity_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%humidity_hru(jj), jj=1, nhru)
      endif
    end associate
  end subroutine


end submodule
