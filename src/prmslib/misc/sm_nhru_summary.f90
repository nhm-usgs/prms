submodule (PRMS_NHRU_SUMMARY) sm_nhru_summary
contains
  !***********************************************************************
  ! Climateflow constructor
  module function constructor_Nhru_summary(ctl_data, param_data) result(this)
    use prms_constants, only: MAXFILE_LENGTH
    use UTILS_PRMS, only: PRMS_open_output_file, print_module_info

    implicit none

    type(Nhru_summary) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Functions
    INTRINSIC CHAR

    integer(i32) :: ios
    ! integer(i32) :: ierr = 0
    ! integer(i32) :: size
    integer(i32) :: jj
    integer(i32) :: j
    character(len=MAXFILE_LENGTH) :: fileName

    ! ------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              end_time => ctl_data%end_time%values, &
              nhruOutON_OFF => ctl_data%nhruOutON_OFF%value, &
              nhruOut_freq => ctl_data%nhruOut_freq%value, &
              nhruOutVars => ctl_data%nhruOutVars%value, &
              nhm_id => param_data%nhm_id%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! NOTE: NhruOutON_OFF=2 is an undocumented feature
      !       Parameter nhm_id is needed for this.

      if (nhruOutVars == 0) then
        if (ctl_data%model_mode%values(1)%s /= 'DOCUMENTATION') then
          print *, 'ERROR, nhru_summary requested with nhruOutVars equal 0'
          STOP
          !          print *, 'no nhru_summary output is produced'
          !          NhruOutON_OFF = 0
          ! Inputerror_flag = 1
          return
        endif
      endif

      this%begin_results = .true.
      this%begyr = start_time(YEAR)

      if (ctl_data%prms_warmup%value > 0) this%begin_results = .false.

      this%begyr = this%begyr + ctl_data%prms_warmup%value
      this%lastyear = this%begyr

      write (this%output_fmt, 9001) nhru

      ! NOTE: removed checks for datatype and size

      if (this%double_vars == 1) then
        allocate(this%nhru_var_dble(nhru, nhruOutVars))
        this%nhru_var_dble = 0.0D0
      endif

      this%daily_flag = 0
      if (ANY([DAILY, DAILY_MONTHLY]==nhruOut_freq)) then
        this%daily_flag = 1
        allocate(this%dailyunit(nhruOutVars))
      endif

      this%monthly_flag = 0
      if (ANY([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY]==nhruOut_freq)) this%monthly_flag = 1

      if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        this%yeardays = 0
        allocate(this%nhru_var_yearly(nhru, nhruOutVars))
        this%nhru_var_yearly = 0.0D0

        allocate(this%yearlyunit(nhruOutVars))

        write(this%output_fmt3, 9003) nhru
      elseif (this%monthly_flag == 1) then
        ! this%monthdays = 0.0D0
        allocate(this%nhru_var_monthly(nhru, nhruOutVars))
        this%nhru_var_monthly = 0.0D0

        allocate(this%monthlyunit(nhruOutVars))
      endif

      write(this%output_fmt2, 9002) nhru
      allocate(this%nhru_var_daily(nhru, nhruOutVars))
      this%nhru_var_daily = 0.0

      do jj = 1, nhruOutVars
        if (this%daily_flag == 1) then
          fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                     ctl_data%nhruOutVar_names%values(jj)%s // '.csv'

          call PRMS_open_output_file(this%dailyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary'

          if (nhruOutON_OFF < 2) then
            write (this%dailyunit(jj), this%output_fmt2) (j, j=1, nhru)
          endif
        endif

        if (nhruOut_freq == MEAN_YEARLY) then
          fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                     ctl_data%nhruOutVar_names%values(jj)%s // '_meanyearly.csv'

          call PRMS_open_output_file(this%yearlyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary, mean yearly'

          if (nhruOutON_OFF < 2) write (this%yearlyunit(jj), this%output_fmt2) (j, j=1, nhru)
        elseif (nhruOut_freq == YEARLY) then
          fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                     ctl_data%nhruOutVar_names%values(jj)%s // '_yearly.csv'

          call PRMS_open_output_file(this%yearlyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary, yearly'

          write (this%yearlyunit(jj), this%output_fmt2) (j, j=1, nhru)
        elseif (this%monthly_flag == 1) then
          if (nhruOut_freq == MEAN_MONTHLY) then
            fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                       ctl_data%nhruOutVar_names%values(jj)%s // '_meanmonthly.csv'
          else
            fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                       ctl_data%nhruOutVar_names%values(jj)%s // '_monthly.csv'
          endif

          call PRMS_open_output_file(this%monthlyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary, monthly'

          if (NhruOutON_OFF < 2) write (this%monthlyunit(jj), this%output_fmt2) (j, j=1, nhru)
        endif
      enddo

      if (nhruOutON_OFF == 2) then
          do jj = 1, nhruOutVars
              if (this%daily_flag == 1) write (this%dailyunit(jj), this%output_fmt2) (nhm_id(j), j=1, nhru)

              if (nhruOut_freq == MEAN_YEARLY) then
                  write (this%yearlyunit(jj), this%output_fmt2) (nhm_id(j), j=1, nhru)
              elseif (this%monthly_flag == 1) then
                  write (this%monthlyunit(jj), this%output_fmt2) (nhm_id(j), j=1, nhru)
              endif
          enddo
      endif

      9001 FORMAT ('(I4, 2(''-'',I2.2),', I6, '('',''ES10.3))')
      9002 FORMAT ('("Date "', I6, '('',''I6))')
      9003 FORMAT ('(I4,', I6, '('',''ES10.3))')
    end associate
  end function



  !***********************************************************************
  !     Output set of declared variables in R compatible format
  !***********************************************************************
  module subroutine run_nhru_summary(this, ctl_data, model_time, model_basin, &
                                     climate, model_gw, model_intcp, model_potet, model_snow, &
                                     model_soil, model_solrad, model_srunoff, model_streamflow, model_temp, &
                                     model_transp)
    use conversions_mod, only: c_to_f
    implicit none

    class(Nhru_summary), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(in) :: climate
    type(Gwflow), intent(in) :: model_gw
    class(Interception), intent(in) :: model_intcp
    class(Potential_ET), intent(in) :: model_potet
    type(Snowcomp), intent(in) :: model_snow
    type(Soilzone), intent(in) :: model_soil
    class(SolarRadiation), intent(in) :: model_solrad
    type(Srunoff), intent(in) :: model_srunoff
    class(Streamflow), intent(in) :: model_streamflow
    class(Temperature), intent(in) :: model_temp
    class(Transpiration), intent(in) :: model_transp

    ! FUNCTIONS AND SUBROUTINES
    INTRINSIC SNGL, DBLE

    ! Local Variables
    integer(i32) :: j
    ! integer(i32) :: chru
    integer(i32) :: jj
    logical :: write_month
    logical :: write_year
    logical :: last_day
    logical :: last_day_of_simulation

    !***********************************************************************
    associate(curr_date => model_time%Nowtime, &
              curr_year => model_time%Nowtime(YEAR), &
              curr_month => model_time%Nowtime(MONTH), &
              curr_day => model_time%Nowtime(DAY), &
              st_date => ctl_data%start_time%values, &
              st_year => ctl_data%start_time%values(YEAR), &
              st_month => ctl_data%start_time%values(MONTH), &
              st_day => ctl_data%start_time%values(DAY), &
              en_date => ctl_data%end_time%values, &
              en_year => ctl_data%end_time%values(YEAR), &
              en_month => ctl_data%end_time%values(MONTH), &
              en_day => ctl_data%end_time%values(DAY), &
              nhruOutVars => ctl_data%nhruOutVars%value, &
              nhruOut_freq => ctl_data%nhruOut_freq%value, &
              nhruOutVar_names => ctl_data%nhruOutVar_names%values, &
              nhru => ctl_data%nhru%value, &
              active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order)

      if (.not. this%begin_results) then
        if (curr_year == this%begyr .and. curr_month == st_month .and. curr_day == st_day) then
          this%begin_results = .true.
        else
          RETURN
        endif
      endif

      !-----------------------------------------------------------------------
      do concurrent(jj=1: nhruOutVars)
      ! do jj = 1, nhruOutVars
        select case(nhruOutVar_names(jj)%s)
          !
          case('dprst_area_open')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_area_open
          case('dprst_evap_hru')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_evap_hru
          case('dprst_insroff_hru')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_insroff_hru
          case('dprst_seep_hru')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_seep_hru
          case('dprst_sroff_hru')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_sroff_hru
          case('dprst_stor_hru')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_stor_hru
          case('dprst_vol_clos')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_vol_clos
          case('dprst_vol_open')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_vol_open
          case('dprst_vol_open_frac')
            this%nhru_var_daily(:, jj) = model_srunoff%dprst_vol_open_frac
          case('freeh2o')
            this%nhru_var_daily(:, jj) = model_snow%freeh2o
          case('gw_in_soil')
            this%nhru_var_daily(:, jj) = model_gw%gw_in_soil
          case('gw_in_ssr')
            this%nhru_var_daily(:, jj) = model_gw%gw_in_ssr
          case('gwres_flow')
            this%nhru_var_daily(:, jj) = model_gw%gwres_flow
          case('gwres_in')
            this%nhru_var_daily(:, jj) = model_gw%gwres_in
          case('hru_actet')
            this%nhru_var_daily(:, jj) = model_soil%hru_actet
          case('hru_impervevap')
            this%nhru_var_daily(:, jj) = model_srunoff%hru_impervevap
          case('hru_impervstor')
            this%nhru_var_daily(:, jj) = model_srunoff%hru_impervstor
          case('hru_intcpevap')
            this%nhru_var_daily(:, jj) = model_intcp%hru_intcpevap
          case('hru_intcpstor')
            this%nhru_var_daily(:, jj) = model_intcp%hru_intcpstor
          case('hru_lateral_flow')
            this%nhru_var_daily(:, jj) = model_gw%hru_lateral_flow
          case('hru_outflow')
            this%nhru_var_daily(:, jj) = sngl(model_streamflow%hru_outflow)
          case('hru_ppt')
            this%nhru_var_daily(:, jj) = climate%hru_ppt
          case('hru_rain')
            this%nhru_var_daily(:, jj) = climate%hru_rain
          case('hru_snow')
            this%nhru_var_daily(:, jj) = climate%hru_snow
          case('hru_sroffi')
            this%nhru_var_daily(:, jj) = model_srunoff%hru_sroffi
          case('hru_sroffp')
            this%nhru_var_daily(:, jj) = model_srunoff%hru_sroffp
          case('hru_storage')
            this%nhru_var_daily(:, jj) = model_gw%hru_storage
          case('hru_streamflow_out')
            this%nhru_var_daily(:, jj) = model_gw%hru_streamflow_out
          case('net_ppt')
            this%nhru_var_daily(:, jj) = model_intcp%net_ppt
          case('net_rain')
            this%nhru_var_daily(:, jj) = model_intcp%net_rain
          case('net_snow')
            this%nhru_var_daily(:, jj) = model_intcp%net_snow
          case('perv_actet')
            this%nhru_var_daily(:, jj) = model_soil%perv_actet
          case('pkwater_equiv')
            this%nhru_var_daily(:, jj) = sngl(climate%pkwater_equiv)
          case('pk_def')
            this%nhru_var_daily(:, jj) = model_snow%pk_def
          case('pk_ice')
            this%nhru_var_daily(:, jj) = model_snow%pk_ice
          case('potet')
            this%nhru_var_daily(:, jj) = model_potet%potet
          case('pref_flow')
            this%nhru_var_daily(:, jj) = model_soil%pref_flow
          case('pref_flow_in')
            this%nhru_var_daily(:, jj) = model_soil%pref_flow_in
          case('pref_flow_infil')
            this%nhru_var_daily(:, jj) = model_soil%pref_flow_infil
          case('pref_flow_stor')
            this%nhru_var_daily(:, jj) = model_soil%pref_flow_stor
          case('prmx')
            this%nhru_var_daily(:, jj) = climate%prmx
          ! case('seg_inflow')
          !   this%nhru_var_daily(:, jj) = model_streamflow%seg_inflow
          ! case('seg_lateral_inflow')
          !   this%nhru_var_daily(:, jj) = model_streamflow%seg_lateral_inflow
          ! case('seg_outflow')
          !   this%nhru_var_daily(:, jj) = model_streamflow%seg_outflow
          ! case('seg_sroff')
          !   this%nhru_var_daily(:, jj) = model_streamflow%seg_sroff
          ! case('seg_ssflow')
          !   this%nhru_var_daily(:, jj) = model_streamflow%seg_ssflow
          ! case('seg_upstream_inflow')
          !   this%nhru_var_daily(:, jj) = model_streamflow%seg_upstream_inflow
          case('slow_flow')
            this%nhru_var_daily(:, jj) = model_soil%slow_flow
          case('slow_stor')
            this%nhru_var_daily(:, jj) = model_soil%slow_stor
          case('snow_evap')
            this%nhru_var_daily(:, jj) = model_snow%snow_evap
          case('snowcov_area')
            this%nhru_var_daily(:, jj) = model_snow%snowcov_area
          case('snowmelt')
            this%nhru_var_daily(:, jj) = model_snow%snowmelt
          case('soil_lower')
            this%nhru_var_daily(:, jj) = model_soil%soil_lower
          case('soil_moist_tot')
            this%nhru_var_daily(:, jj) = model_soil%soil_moist_tot
          case('soil_to_gw')
            this%nhru_var_daily(:, jj) = model_soil%soil_to_gw
          case('soil_to_ssr')
            this%nhru_var_daily(:, jj) = model_soil%soil_to_ssr
          case('swrad')
            this%nhru_var_daily(:, jj) = model_solrad%swrad
          case('tavg')
            this%nhru_var_daily(:, jj) = model_temp%tavg
          case('tmax')
            this%nhru_var_daily(:, jj) = model_temp%tmax
          case('tmin')
            this%nhru_var_daily(:, jj) = model_temp%tmin
          ! case('tavgc')
          !   this%nhru_var_daily(:, jj) = climate%tavgc
          case('tavgf')
            this%nhru_var_daily(:, jj) = model_temp%tavg_f
          case('tmaxf')
            this%nhru_var_daily(:, jj) = model_temp%tmax_f
          case('tminf')
            this%nhru_var_daily(:, jj) = c_to_f(model_temp%tmin)
          case('transp_on')
            this%nhru_var_daily(:, jj) = model_transp%transp_on
          ! case('tminc')
          !   this%nhru_var_daily(:, jj) = climate%tminc
          ! case('tminf')
          !   this%nhru_var_daily(:, jj) = climate%tminf
          ! case('tmax_hru')
          !   this%nhru_var_daily(:, jj) = climate%tmax_hru
          ! case('tmin_hru')
          !   this%nhru_var_daily(:, jj) = climate%tmin_hru
          case default
            ! pass
        end select
      end do

      write_month = .false.
      write_year = .false.

      last_day_of_simulation = all(curr_date .eq. en_date)

      if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        last_day = .false.

        if (last_day_of_simulation) then
        ! if (curr_year == en_year .and. curr_month == en_month .and. curr_day == en_day) then
          last_day = .true.
        endif

        if (this%lastyear /= curr_year .or. last_day) then
          if ((curr_month == st_month .and. curr_day == st_day) .or. last_day) then
            do jj=1, nhruOutVars
              if (nhruOut_freq == MEAN_YEARLY) then
                do concurrent (j=1:active_hrus)
                  this%nhru_var_yearly(hru_route_order(j), jj) = this%nhru_var_yearly(hru_route_order(j), jj) / this%yeardays
                end do
              endif

              write (this%yearlyunit(jj), this%output_fmt3) this%lastyear, (this%nhru_var_yearly(j, jj), j=1, nhru)
            enddo

            this%nhru_var_yearly = 0.0d0
            this%yeardays = 0
            this%lastyear = curr_year
          endif
        endif

        this%yeardays = this%yeardays + 1
      elseif (this%monthly_flag == 1) then
          ! check for last day of month and simulation
          if (curr_day == model_time%last_day_of_month(curr_month)) then
            write_month = .true.
          elseif (last_day_of_simulation) then
          ! elseif (curr_year == en_year .and. curr_month == en_month .and. curr_day == en_day) then
            write_month = .true.
          endif

          ! this%monthdays = this%monthdays + 1.0d0
      endif

      if (this%double_vars == 1) then
        do jj = 1, nhruOutVars
          ! TODO: figure out how to handle this
          ! if (Nhru_var_type(jj) == 3) then
          !   do j = 1, model_time%active_hrus
          !     chru = model_time%hru_route_order(j)
          !     this%nhru_var_daily(chru, jj) = SNGL(this%nhru_var_dble(chru, jj))
          !   enddo
          ! endif
        enddo
      endif

      if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        do jj = 1, nhruOutVars
          do concurrent (j=1:active_hrus)
            this%nhru_var_yearly(hru_route_order(j), jj) = this%nhru_var_yearly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j), jj))
          end do
        enddo
        RETURN
      endif

      if (this%monthly_flag == 1) then
        do jj = 1, nhruOutVars
          do concurrent (j=1:active_hrus)
            this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j), jj))

            if (write_month) then
              if (nhruOut_freq == MEAN_MONTHLY) then
                this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) / model_time%last_day_of_month(curr_month)
                ! this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) / this%monthdays
              endif
            endif
          end do
        enddo
      endif

      do jj = 1, nhruOutVars
        if (this%daily_flag == 1) then
          write (this%dailyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
                                                      (this%nhru_var_daily(j, jj), j = 1, nhru)
        endif

        if (write_month) then
          write (this%monthlyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
                                                        (this%nhru_var_monthly(j, jj), j = 1, nhru)
        endif
      enddo

      if (write_month) then
        ! this%monthdays = 0.0d0
        this%nhru_var_monthly = 0.0d0
      endif
    end associate
  end subroutine

  module function module_name() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODNAME
  end function

  module function version() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODVERSION
  end function
end submodule
