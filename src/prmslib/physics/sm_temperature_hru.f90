submodule(PRMS_TEMPERATURE_HRU) sm_temperature_hru
contains

  module function constructor_Temperature_hru(ctl_data, model_basin, model_summary) result(this)
    use UTILS_CBH, only: find_current_time, find_header_end, open_netcdf_cbh_file, read_netcdf_cbh_file
    implicit none

    type(Temperature_hru) :: this
      !! Temperature_hru class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! Control
    ! nhru, cbh_binary_flag, print_debug, start_time, tmax_day, tmin_day,

    ! ------------------------------------------------------------------------
    ! Call the parent constructor first
    this%Temperature = Temperature(ctl_data, model_basin, model_summary)

    associate(cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              end_time => ctl_data%end_time%values, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              tmax_day => ctl_data%tmax_day%values(1), &
              tmin_day => ctl_data%tmin_day%values(1), &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Read the parameters for the temperature by HRU module
      allocate(this%tmax_cbh_adj(nhru, nmonths))
      call param_hdl%get_variable('tmax_cbh_adj', this%tmax_cbh_adj)
      ! write(*, *) this%tmax_cbh_adj(:,1)

      allocate(this%tmin_cbh_adj(nhru, nmonths))
      call param_hdl%get_variable('tmin_cbh_adj', this%tmin_cbh_adj)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Open netcdf or ascii-base cbh files
      if (tmax_day%s(index(tmax_day%s, '.')+1:) == 'nc') then
        ! Read a netcdf file
        call open_netcdf_cbh_file(this%tmax_funit, this%tmax_varid, this%tmax_idx_offset, &
                                  tmax_day%s, 'tmax', start_time, end_time, nhru)
        this%has_netcdf_tmax = .true.
      else
        ! Open and read tmax cbh file
        call find_header_end(nhru, this%tmax_funit, ierr, tmax_day%s, &
                             'tmax_day', (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%tmax_funit, start_time, (cbh_binary_flag==1))
        endif

        this%has_netcdf_tmax = .false.
      endif

      if (tmin_day%s(index(tmin_day%s, '.')+1:) == 'nc') then
        call open_netcdf_cbh_file(this%tmin_funit, this%tmin_varid, this%tmin_idx_offset, &
                                  tmin_day%s, 'tmin', start_time, end_time, nhru)
        this%has_netcdf_tmin = .true.
      else
        ! Open and read tmin cbh file
        call find_header_end(nhru, this%tmin_funit, ierr, tmin_day%s, &
                             'tmin_day', (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%tmin_funit, start_time, (cbh_binary_flag==1))
        endif

        this%has_netcdf_tmin = .false.
      endif

      if (istop == 1) STOP 'ERROR in temperature_hru'
    end associate
  end function


  module subroutine run_Temperature_hru(this, ctl_data, model_basin, model_time, model_summary)
    use conversions_mod, only: f_to_c, c_to_f
    use UTILS_CBH, only: read_netcdf_cbh_file
    implicit none

    class(Temperature_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time
    type(Summary), intent(inout) :: model_summary

    ! Local variables
    ! integer(i32) :: chru
    ! integer(i32) :: idx1D
    integer(i32) :: ios
    integer(i32) :: jj
    integer(i32) :: datetime(6)

    ! Control
    ! nhru, nmonths,

    ! Basin
    ! basin_area_inv

    ! Parameters
    ! hru_area,
    ! not associated: tmin_cbh_adj, tmax_cbh_adj

    ! Time_t
    ! curr_month (Nowmonth),

    ! --------------------------------------------------------------------------
    associate(basin_area_inv => model_basin%basin_area_inv, &
              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths, &
              hru_area => model_basin%hru_area, &

              ! tmax_cbh_adj => param_data%tmax_cbh_adj%values, &
              ! tmin_cbh_adj => param_data%tmin_cbh_adj%values, &

              curr_month => model_time%Nowmonth, &
              timestep => model_time%Timestep)

      ios = 0

      if (this%has_netcdf_tmax) then
        call read_netcdf_cbh_file(this%tmax_funit, this%tmax_varid, this%tmax_idx_offset, timestep, nhru, this%tmax)
      else
        read(this%tmax_funit, *, IOSTAT=ios) datetime, this%tmax
      endif

      if (this%has_netcdf_tmin) then
        call read_netcdf_cbh_file(this%tmin_funit, this%tmin_varid, this%tmin_idx_offset, timestep, nhru, this%tmin)
      else
        read(this%tmin_funit, *, IOSTAT=ios) datetime, this%tmin
      endif

      do jj=1, nhru
        this%tmax(jj) = this%tmax(jj) + this%tmax_cbh_adj(jj, curr_month)
        this%tmin(jj) = this%tmin(jj) + this%tmin_cbh_adj(jj, curr_month)
        ! idx1D = (curr_month - 1) * nhru + jj
        ! this%tmax(jj) = this%tmax(jj) + tmax_cbh_adj(idx1D)
        ! this%tmin(jj) = this%tmin(jj) + tmin_cbh_adj(idx1D)
      end do

      ! NOTE: Only used by solar_radiation_degday; remove once temperature units
      !       are standardized.
      this%tmax_f = real(this%tmax, r32)
      ! this%tmax_f = sngl(this%tmax)

      ! NOTE: Only used by potet_jh; remove once temperature units are standardized
      ! NOTE: Using individual sngl calls is the only way to get tavg_f to match
      !       PRMS5 output.
      this%tavg_f = real(this%tmax + this%tmin, r32) * 0.5
      ! this%tavg_f = (sngl(this%tmax) + sngl(this%tmin)) * 0.5

      ! WARNING: Assuming CBH in Fahrenheit; conversion to Celsius can be removed
      !          once parameter files are standardized on Celsius temp units.
      this%tmax = f_to_c(this%tmax)
      this%tmin = f_to_c(this%tmin)

      this%tavg = f_to_c(this%tavg_f)
      ! this%tavg = (this%tmax + this%tmin) * 0.5

      ! NOTE: This may be moved to parent class at some point.
      ! WARNING: Explicit conversion to Fahrenheit for testing.
      this%basin_temp = c_to_f(sum(dble(this%tavg * hru_area)) * basin_area_inv)
      this%basin_tmax = c_to_f(sum(dble(this%tmax * hru_area)) * basin_area_inv)
      this%basin_tmin = c_to_f(sum(dble(this%tmin * hru_area)) * basin_area_inv)

      ! If any temperature output variables are specified then we set pointers
      ! to the arrays for the nhru_summary module.
      if (this%has_hru_summary_vars) then
        call this%set_nhru_summary_ptrs(ctl_data, model_summary)
      end if
    end associate
  end subroutine

end submodule
