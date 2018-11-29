submodule (PRMS_NHRU_SUMMARY_PTR) sm_nhru_summary_ptr
contains
  !***********************************************************************
  ! Climateflow constructor
  module function constructor_Nhru_summary_ptr(ctl_data, param_data) result(this)
    use prms_constants, only: MAXFILE_LENGTH, dp
    use UTILS_PRMS, only: PRMS_open_output_file, print_module_info
    implicit none

    type(Nhru_summary_ptr) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Local variables
    integer(i32) :: ios
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

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      end if

      ! NOTE: NhruOutON_OFF=2 is an undocumented feature.
      !       Parameter nhm_id is needed for this.

      if (nhruOutVars == 0) then
        print *, 'ERROR, nhru_summary requested with nhruOutVars equal 0'
        STOP
        return
      end if

      this%begin_results = .true.
      this%begyr = start_time(YEAR)

      if (ctl_data%prms_warmup%value > 0) this%begin_results = .false.

      this%begyr = this%begyr + ctl_data%prms_warmup%value
      this%lastyear = this%begyr

      write (this%output_fmt, 9001) nhru

      this%daily_flag = 0
      if (ANY([DAILY, DAILY_MONTHLY]==nhruOut_freq)) then
        this%daily_flag = 1
        allocate(this%dailyunit(nhruOutVars))
      end if

      this%monthly_flag = 0
      if (ANY([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY]==nhruOut_freq)) then
        this%monthly_flag = 1
      end if

      if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        this%yeardays = 0
        allocate(this%nhru_var_yearly(nhru, nhruOutVars))
        this%nhru_var_yearly = 0.0_dp

        allocate(this%yearlyunit(nhruOutVars))

        write(this%output_fmt3, 9003) nhru
      elseif (this%monthly_flag == 1) then
        ! this%monthdays = 0.0D0
        allocate(this%nhru_var_monthly(nhru, nhruOutVars))
        this%nhru_var_monthly = 0.0_dp

        allocate(this%monthlyunit(nhruOutVars))
      end if

      write(this%output_fmt2, 9002) nhru
      ! allocate(this%nhru_var_daily(nhru, nhruOutVars))
      allocate(this%nhru_var_daily(nhruOutVars))
      ! this%nhru_var_daily = 0.0

      do jj = 1, nhruOutVars
        if (this%daily_flag == 1) then
          fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                     ctl_data%nhruOutVar_names%values(jj)%s // '.csv'

          call PRMS_open_output_file(this%dailyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary'

          if (nhruOutON_OFF < 2) then
            write (this%dailyunit(jj), this%output_fmt2) (j, j=1, nhru)
          end if
        end if

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
          end if

          call PRMS_open_output_file(this%monthlyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary, monthly'

          if (NhruOutON_OFF < 2) write (this%monthlyunit(jj), this%output_fmt2) (j, j=1, nhru)
        end if
      enddo

      if (nhruOutON_OFF == 2) then
          do jj = 1, nhruOutVars
              if (this%daily_flag == 1) write (this%dailyunit(jj), this%output_fmt2) (nhm_id(j), j=1, nhru)

              if (nhruOut_freq == MEAN_YEARLY) then
                  write (this%yearlyunit(jj), this%output_fmt2) (nhm_id(j), j=1, nhru)
              elseif (this%monthly_flag == 1) then
                  write (this%monthlyunit(jj), this%output_fmt2) (nhm_id(j), j=1, nhru)
              end if
          enddo
      end if

      9001 FORMAT ('(I4, 2(''-'',I2.2),', I6, '('',''ES10.3))')
      9002 FORMAT ('("Date "', I6, '('',''I6))')
      9003 FORMAT ('(I4,', I6, '('',''ES10.3))')
    end associate
  end function



  !***********************************************************************
  !     Output set of declared variables in R compatible format
  !***********************************************************************
  module subroutine run_nhru_summary_ptr(this, ctl_data, model_time, model_basin)
    use conversions_mod, only: c_to_f
    use prms_constants, only: dp
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin

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
        end if
      end if

      write_month = .false.
      write_year = .false.

      last_day_of_simulation = all(curr_date .eq. en_date)

      if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        last_day = .false.

        if (last_day_of_simulation) then
        ! if (curr_year == en_year .and. curr_month == en_month .and. curr_day == en_day) then
          last_day = .true.
        end if

        if (this%lastyear /= curr_year .or. last_day) then
          if ((curr_month == st_month .and. curr_day == st_day) .or. last_day) then
            do jj=1, nhruOutVars
              if (nhruOut_freq == MEAN_YEARLY) then
                do concurrent (j=1:active_hrus)
                  this%nhru_var_yearly(hru_route_order(j), jj) = this%nhru_var_yearly(hru_route_order(j), jj) / this%yeardays
                end do
              end if

              write (this%yearlyunit(jj), this%output_fmt3) this%lastyear, (this%nhru_var_yearly(j, jj), j=1, nhru)
            enddo

            this%nhru_var_yearly = 0.0d0
            this%yeardays = 0
            this%lastyear = curr_year
          end if
        end if

        this%yeardays = this%yeardays + 1
      elseif (this%monthly_flag == 1) then
          ! check for last day of month and simulation
          if (curr_day == model_time%last_day_of_month(curr_month)) then
            write_month = .true.
          elseif (last_day_of_simulation) then
          ! elseif (curr_year == en_year .and. curr_month == en_month .and. curr_day == en_day) then
            write_month = .true.
          end if

          ! this%monthdays = this%monthdays + 1.0d0
      end if

      if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        do jj = 1, nhruOutVars
          do concurrent (j=1:active_hrus)
            ! this%nhru_var_yearly(hru_route_order(j), jj) = this%nhru_var_yearly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j), jj))
            if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
              this%nhru_var_yearly(hru_route_order(j), jj) = this%nhru_var_yearly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j))%ptr_r32(jj))
            else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
              this%nhru_var_yearly(hru_route_order(j), jj) = this%nhru_var_yearly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j))%ptr_r64(jj))
            end if
          end do
        enddo
        RETURN
      end if

      if (this%monthly_flag == 1) then
        do jj = 1, nhruOutVars
          do concurrent (j=1:active_hrus)
            ! this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j), jj))
            if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
              this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j))%ptr_r32(jj))
            else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
              this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) + DBLE(this%nhru_var_daily(hru_route_order(j))%ptr_r64(jj))
            end if

            if (write_month) then
              if (nhruOut_freq == MEAN_MONTHLY) then
                this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) / model_time%last_day_of_month(curr_month)
                ! this%nhru_var_monthly(hru_route_order(j), jj) = this%nhru_var_monthly(hru_route_order(j), jj) / this%monthdays
              end if
            end if
          end do
        enddo
      end if

      do jj=1, nhruOutVars
        if (this%daily_flag == 1) then
          if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
            write (this%dailyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
                                                        (this%nhru_var_daily(jj)%ptr_r32(j), j=1, nhru)
          else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
            write (this%dailyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
                                                        (this%nhru_var_daily(jj)%ptr_r64(j), j=1, nhru)
          else
            write(*, *) MODNAME, 'No output array for variable index:', jj
          end if
          ! write (this%dailyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
          !                                             (this%nhru_var_daily(j, jj), j = 1, nhru)
        end if

        if (write_month) then
          write (this%monthlyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
                                                        (this%nhru_var_monthly(j, jj), j=1, nhru)
        end if
      enddo

      if (write_month) then
        ! this%monthdays = 0.0d0
        this%nhru_var_monthly = 0.0_dp
      end if
    end associate
  end subroutine


  module subroutine set_nhru_var_r64(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    this%nhru_var_daily(idx)%ptr_r64 => var
  end subroutine

  module subroutine set_nhru_var_r32(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r32), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    this%nhru_var_daily(idx)%ptr_r32 => var
  end subroutine
end submodule
