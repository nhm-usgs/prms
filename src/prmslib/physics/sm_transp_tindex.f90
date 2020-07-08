submodule (PRMS_TRANSP_TINDEX) sm_transp_tindex
contains
  !***********************************************************************
  ! Transp_tindex constructor
  module subroutine init_Transp_tindex(this, ctl_data, model_basin, model_temp, model_summary)
    use prms_constants, only: CELSIUS
    use conversions_mod, only: f_to_c
    use UTILS_PRMS, only: print_module_info
    implicit none

    class(Transp_tindex), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Summary), intent(inout) :: model_summary

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Transpiration%init(ctl_data, model_basin, model_temp, model_summary)

    associate(init_vars_from_file => ctl_data%init_vars_from_file%value, &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &
              save_vars_to_file => ctl_data%save_vars_to_file%value, &
              st_month => ctl_data%start_time%values(MONTH), &
              st_day => ctl_data%start_time%values(DAY), &

              nhru => model_basin%nhru, &
              active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order, &

              temp_units => model_temp%temp_units)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Parameters
      allocate(this%transp_beg(nhru))
      allocate(this%transp_end(nhru))
      allocate(this%transp_tmax(nhru))
      call param_hdl%get_variable('transp_tmax', this%transp_tmax)

      ! Other variables
      allocate(this%tmax_sum(nhru))
      allocate(this%transp_check(nhru))

      if (init_vars_from_file == 0) then
        ! Read parameter data
        call param_hdl%get_variable('transp_beg', this%transp_beg)
        call param_hdl%get_variable('transp_end', this%transp_end)

        ! Initialize internal variables
        this%tmax_sum = 0.0
        this%transp_check = .false.

        this%transp_on = init_transp_on(this%transp_beg, this%transp_end, st_month, st_day)
        this%transp_check = init_transp_check(this%transp_beg, st_month, st_day)
      else
        ! ~~~~~~~~~~~~~~~~~~~~~~~
        ! Initialize from restart
        call ctl_data%read_restart_variable('transp_beg', this%transp_beg)
        call ctl_data%read_restart_variable('transp_end', this%transp_end)
        call ctl_data%read_restart_variable('transp_check', this%transp_check)
        call ctl_data%read_restart_variable('tmax_sum', this%tmax_sum)
      end if

      if (save_vars_to_file == 1) then
        ! Create restart variables
        call ctl_data%add_variable('tmax_sum', this%tmax_sum, 'nhru', 'none')
        call ctl_data%add_variable('transp_beg', this%transp_beg, 'nhru', 'month')
        call ctl_data%add_variable('transp_end', this%transp_end, 'nhru', 'month')
        call ctl_data%add_variable('transp_check', this%transp_check, 'nhru', 'none')
        ! call ctl_data%add_variable('transp_tmax', this%transp_tmax, 'nhru', 'temp_units')
      end if

      ! NOTE: changed to use Celsius units by default
      ! NOTE: this will be unnecessary once parameter file units are standardized
      ! WARNING: 2019-11-01 PAN: There is no simple way to convert transp_tmax
      !                       from F to C because it represents a running total
      !                       of temperature. So if a transp_tmax in F is converted
      !                       to celsius then timing of transpiration turning
      !                       on will be incorrect.
      ! if (temp_units == CELSIUS) then
      !   this%transp_tmax_c = this%transp_tmax(:)
      ! else
      !   this%transp_tmax_c = f_to_c(this%transp_tmax)
      ! endif
    end associate
  end subroutine


  module subroutine run_Transp_tindex(this, ctl_data, model_time, model_basin, model_temp)
    use prms_constants, only: CELSIUS
    implicit none

    class(Transp_tindex), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    ! Local Variables
    ! integer(i32) :: chru
      !! Current HRU
    ! integer(i32) :: j
      !! Counter

    ! --------------------------------------------------------------------------
    ! Call parent run_Transpiration() procedure if needed
    ! call this%run_Transpiration(ctl_data)

    associate(active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order, &

              temp_units => model_temp%temp_units, &
              tmax => model_temp%tmax, &
              tmax_f => model_temp%tmax_f, &

              curr_month => model_time%Nowmonth, &
              curr_day => model_time%Nowday)

      if (temp_units == CELSIUS) then
        call update_transpiration(this%transp_on, this%transp_check, this%tmax_sum, &
                                  this%transp_beg, this%transp_end, this%transp_tmax, &
                                  tmax, 0.0, curr_month, curr_day)
      else
        call update_transpiration(this%transp_on, this%transp_check, this%tmax_sum, &
                                  this%transp_beg, this%transp_end, this%transp_tmax, &
                                  tmax_f, 32.0, curr_month, curr_day)
      end if

      ! write(*, 9008) '===========', curr_month, curr_day, '============================'
      ! write(*, 9010) this%transp_on
      ! write(*,*) '======================================='
      ! write(*, 9010) this%transp_check
      ! write(*,*) '======================================='
      ! write(*, 9009) this%tmax_sum
      ! write(*,*) '======================================='
      ! write(*, 9009) this%transp_tmax
      ! write(*,*) '======================================='
      ! write(*, 9009) tmax
      ! write(*,*) '======================================='
      ! write(*, 9009) tmax_f
      ! write(*,*) '+++++++++++++++++++++++++++++++++++++++'

      ! 9008 format(A, 2('-', I2.2), A)
      ! 9009 format(14F7.2)
      ! 9010 format(14L2)
    end associate
  end subroutine

  module subroutine cleanup_Transp_tindex(this, ctl_data)
    implicit none
    class(Transp_tindex), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! --------------------------------------------------------------------------
    ! Call parent cleanup first
    call this%Transpiration%cleanup(ctl_data)

    ! --------------------------------------------------------------------------
    associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
      if (save_vars_to_file == 1) then
        ! Write out this module's restart variables
        call ctl_data%write_restart_variable('tmax_sum', this%tmax_sum)
        call ctl_data%write_restart_variable('transp_beg', this%transp_beg)
        call ctl_data%write_restart_variable('transp_check', this%transp_check)
        call ctl_data%write_restart_variable('transp_end', this%transp_end)
        ! call ctl_data%write_restart_variable('transp_tmax', this%transp_tmax)
      end if
    end associate
  end subroutine

  pure elemental module function init_transp_check(tr_beg, month, day) result(res)
    implicit none

    logical :: res
    integer, intent(in) :: tr_beg
    integer, intent(in) :: month
    integer, intent(in) :: day

    res = .false.

    if (month == tr_beg .and. day <= 10) then
      res = .true.
    end if
  end function


  pure elemental module function init_transp_on(tr_beg, tr_end, month, day) result(res)
    implicit none

    logical :: res
    integer, intent(in) :: tr_beg
    integer, intent(in) :: tr_end
    integer, intent(in) :: month
    integer, intent(in) :: day

    res = .false.
    if (month == tr_beg) then
      if (day > 10) then
        ! print *, 'transp_on = 1 (st_day > 10)'
        res = .true.
      endif
    elseif (tr_end > tr_beg) then
      if (month > tr_beg .and. month < tr_end) then
        ! print *, 'transp_on = 1 (st_month > transp_beg && st_month < transp_end)'
        res = .true.
      endif
    else
      if (month > tr_beg .or. (month + 12) < (tr_end + 12)) then
        ! TODO: shouldn't the 2nd line of the conditional just be:
        !       st_month < transp_end(chr)
        ! print *, 'transp_on = 1 (st_month > trans_beg || st_mo+12 < transp_end+12)'
        res = .true.
      endif
    endif
  end function

  pure elemental module subroutine update_transpiration(tr_on, tr_check, tmax_sum, &
                                                        tr_beg, tr_end, tr_tmax, tmax, &
                                                        tmax_threshold, month, day)
    implicit none

    logical, intent(inout) :: tr_on
    logical, intent(inout) :: tr_check
    real(r32), intent(inout) :: tmax_sum
    integer(i32), intent(in) :: tr_beg
    integer(i32), intent(in) :: tr_end
    real(r32), intent(in) :: tr_tmax
    real(r32), intent(in) :: tmax
    real(r32), intent(in) :: tmax_threshold
    integer(i32), intent(in) :: month
    integer(i32), intent(in) :: day

    ! ****** Check for month to turn transp_check switch on or off
    ! ****** transpiration switch off (transp_on)
    if (day == 1) then
      if (month == tr_end) then
        ! At the end of the current transpiration period
        tr_on = .false.
        tr_check = .false.
        tmax_sum = 0.0
      endif

      !******check for month to turn transpiration switch (transp_check) on or off
      if (month == tr_beg) then
        tr_check = .true.
        tmax_sum = 0.0
      endif
    endif

    ! ****** If in checking period, then for each day sum the maximum
    ! ****** temperature until it's greater than temperature index parameter,
    ! ****** at which time, turn transpiration switch on, check switch off.
    ! ****** Freezing temperature assumed to be 0 Celsius or 32 Fahrenheit.
    if (tr_check) then
      if (tmax > tmax_threshold) then
        tmax_sum = tmax_sum + tmax
      end if

      if (tmax_sum > tr_tmax) then
        tr_on = .true.
        tr_check = .false.
        tmax_sum = 0.0
      endif
    endif
  end subroutine
end submodule
