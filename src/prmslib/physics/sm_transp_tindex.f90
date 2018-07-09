submodule (PRMS_TRANSP_TINDEX) sm_transp_tindex
contains
  !***********************************************************************
  ! Transp_tindex constructor
  module function constructor_Transp_tindex(ctl_data, param_data, model_basin) result(this)
    use prms_constants, only: CELSIUS
    use conversions_mod, only: f_to_c
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Transp_tindex) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin

    ! Local variables
    character(LEN=11) :: modname_rst
      !! Used to verify module name when reading from restart file
    integer(i32) :: ii
      !! counter
    integer(i32) :: chru
      !! Current HRU

    ! Control
    ! nhru, init_vars_from_file, print_debug, rst_unit, st_month, st_day

    ! Basin
    ! active_hrus, hru_route_order,

    ! Parameters
    ! temp_units, transp_beg, transp_end, transp_tmax,
    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    this%Transpiration = Transpiration(ctl_data)

    associate(nhru => ctl_data%nhru%value, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              print_debug => ctl_data%print_debug%value, &
              rst_unit => ctl_data%restart_output_unit, &
              st_month => ctl_data%start_time%values(MONTH), &
              st_day => ctl_data%start_time%values(DAY), &

              active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order, &

              temp_units => param_data%temp_units%values(1), &
              transp_beg => param_data%transp_beg%values, &
              transp_end => param_data%transp_end%values, &
              transp_tmax => param_data%transp_tmax%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      allocate(this%tmax_sum(nhru))
      allocate(this%transp_check(nhru))
      allocate(this%transp_tmax_c(nhru))

      ! NOTE: changed to use Celsius units by default
      ! NOTE: this will be unnecessary once parameter file is standardized
      if (temp_units == CELSIUS) then
        this%transp_tmax_c = transp_tmax(:)
      else
        this%transp_tmax_c = f_to_c(transp_tmax)
      endif

      if (init_vars_from_file == 1) then
        ! TODO: Incorporate the load from restart file stuff
        ! These are only allocated when initializing from restart file
        ! integer(i32), allocatable :: transp_beg_restart(:)
        ! integer(i32), allocatable :: transp_end_restart(:)
        ! real(r32), allocatable :: transp_tmax_restart(:)

        ! read(rst_unit) modname_rst
        ! call check_restart(MODNAME, modname_rst)
        ! read(rst_unit) this%transp_check
        ! read(rst_unit) this%tmax_sum
        ! read(rst_unit) Transp_beg_restart
        ! read(rst_unit) Transp_end_restart
        ! read(rst_unit) Transp_tmax_restart
      endif

      this%tmax_sum = 0.0
      this%transp_check = 0
      this%basin_transp_on = 0

      do ii=1, active_hrus
        chru = hru_route_order(ii)

        if (st_month == transp_beg(chru)) then
          if (st_day > 10) then
            this%transp_on(chru) = 1
          else
            this%transp_check(chru) = 1
          endif
        elseif (transp_end(chru) > transp_beg(chru)) then
          if (st_month > transp_beg(chru) .and. st_month < transp_end(chru)) then
            this%transp_on(chru) = chru
          endif
        else
          if (st_month > transp_beg(chru) .or. (st_month + 12) < (transp_end(chru) + 12)) then
            ! TODO: shouldn't the 2nd line of the conditional just be:
            !       st_month < transp_end(chr)
            this%transp_on(chru) = 1
          endif
        endif

        if (this%basin_transp_on == 0) then
          if (this%transp_on(chru) == 1) then
            this%basin_transp_on = 1
          endif
        endif
      enddo
    end associate
  end function


  module subroutine cleanup_Transp_tindex(this, ctl_data)
    implicit none
    class(Transp_tindex), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! --------------------------------------------------------------------------
    associate(rst_unit => ctl_data%restart_output_unit)

      write(rst_unit) MODNAME
      write(rst_unit) this%transp_check
      write(rst_unit) this%tmax_sum

      ! NOTE: Why save the following? It's already in the parameter file.
      ! write(rst_unit) transp_beg
      ! write(rst_unit) transp_end
      ! write(rst_unit) transp_tmax
    end associate
  end subroutine



  module subroutine run_Transp_tindex(this, ctl_data, param_data, model_time, model_basin, model_temp)
    implicit none

    class(Transp_tindex), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    ! Local Variables
    integer(i32) :: chru
      !! Current HRU
    integer(i32) :: j
      !! Counter

    ! Control

    ! Basin
    ! active_hrus, hru_route_order

    ! Parameters
    ! transp_beg, transp_end,

    ! Time_t
    ! curr_month (Nowmonth), curr_day (Nowday),

    ! --------------------------------------------------------------------------
    ! Call parent run_Transpiration() procedure if needed
    ! call this%run_Transpiration(ctl_data)

    associate(active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order, &

              tmax => model_temp%tmax, &

              curr_month => model_time%Nowmonth, &
              curr_day => model_time%Nowday, &

              transp_beg => param_data%transp_beg%values, &
              transp_end => param_data%transp_end%values)

      ! Set switch for active transpiration period
      this%basin_transp_on = 0

      do j = 1, active_hrus
        chru = hru_route_order(j)

        !******check for month to turn transp_check switch on or
        !******transpiration switch off (transp_on)
        if (curr_day == 1) then
          !******check for end of period
          if (curr_month == transp_end(chru)) then
            this%transp_on(chru) = 0
            this%transp_check(chru) = 0
            this%tmax_sum(chru) = 0.0
          endif

          !******check for month to turn transpiration switch (transp_check) on or off
          if (curr_month == transp_beg(chru)) then
            this%transp_check(chru) = 1
            this%tmax_sum(chru) = 0.0
          endif
        endif

        !****** If in checking period, then for each day sum the maximum
        !****** temperature until it's greater than temperature index parameter,
        !****** at which time, turn transpiration switch on, check switch off
        ! freezing temperature assumed to be 32 degrees Fahrenheit (0 Celsius)
        if (this%transp_check(chru) == 1) then
          if (tmax(chru) > 0.0) then
            this%tmax_sum(chru) = this%tmax_sum(chru) + tmax(chru)
          endif

          if (this%tmax_sum(chru) > this%transp_tmax_c(chru)) then
            this%transp_on(chru) = 1
            this%transp_check(chru) = 0
            this%tmax_sum(chru) = 0.0
          endif
        endif

        if (this%basin_transp_on == 0) then
          if (this%transp_on(chru) == 1) this%basin_transp_on = 1
        endif
      enddo
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
