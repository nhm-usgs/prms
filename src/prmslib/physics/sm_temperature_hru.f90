submodule(PRMS_TEMPERATURE_HRU) sm_temperature_hru
contains

  module function constructor_Temperature_hru(ctl_data) result(this)
    use UTILS_CBH, only: find_current_time, find_header_end
    implicit none

    type(Temperature_hru) :: this
      !! Temperature_hru class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! Control
    ! nhru, cbh_binary_flag, print_debug, start_time, tmax_day, tmin_day,

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    this%Temperature = Temperature(ctl_data)

    associate(nhru => ctl_data%nhru%value, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              tmax_day => ctl_data%tmax_day%values(1), &
              tmin_day => ctl_data%tmin_day%values(1))

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Open and read tmax cbh file
      call find_header_end(nhru, this%tmax_funit, ierr, tmax_day%s, &
                           'tmax_day', (cbh_binary_flag==1))
      if (ierr == 1) then
        istop = 1
      else
        call find_current_time(ierr, this%tmax_funit, start_time, (cbh_binary_flag==1))
      endif

      ! Open and read tmin cbh file
      call find_header_end(nhru, this%tmin_funit, ierr, tmin_day%s, &
                           'tmin_day', (cbh_binary_flag==1))
      if (ierr == 1) then
        istop = 1
      else
        call find_current_time(ierr, this%tmin_funit, start_time, (cbh_binary_flag==1))
      endif

      if (istop == 1) STOP 'ERROR in climate_hru'
    end associate
  end function


  module subroutine run_Temperature_hru(this, ctl_data, param_data, model_basin, model_time)
    use conversions_mod, only: f_to_c, c_to_f
    use UTILS_PRMS, only: get_array
    implicit none

    class(Temperature_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time

    ! Local variables
    ! integer(i32) :: chru
    integer(i32) :: ios
    integer(i32) :: jj
    integer(i32) :: yr, mo, dy, hr, mn, sec
      !! junk vars to hold time info from files

    real(r32), pointer, contiguous :: tmax_adj_2d(:,:)
    real(r32), pointer, contiguous :: tmin_adj_2d(:,:)

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
    associate(nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &

              basin_area_inv => model_basin%basin_area_inv, &

              hru_area => param_data%hru_area%values, &

              curr_month => model_time%Nowmonth)

      ios = 0

      read(this%tmax_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%tmax(jj), jj=1, nhru)
      read(this%tmin_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%tmin(jj), jj=1, nhru)

      ! NOTE: This is dangerous because it circumvents the intent for param_data
      ! Get 2D access to 1D array
      tmax_adj_2d => get_array(param_data%tmax_cbh_adj%values, (/nhru, nmonths/))
      tmin_adj_2d => get_array(param_data%tmin_cbh_adj%values, (/nhru, nmonths/))

      ! Adjust the temperatures if needed
      this%tmax = this%tmax + tmax_adj_2d(:, curr_month)
      this%tmin = this%tmin + tmin_adj_2d(:, curr_month)

      ! NOTE: Only used by solar_radiation_degday; remove once temperature units
      !       are standardized.
      this%tmax_f = real(this%tmax, r32)
      ! this%tmax_f = sngl(this%tmax)

      ! NOTE: Only used by potet_jh; remove once temperature units are standardized
      ! NOTE: Using individual sngl calls is the only way to get tavg_f to match
      !       PRMS5 output.
      this%tavg_f = real(this%tmax + this%tmin, r32) * 0.5
      ! this%tavg_f = (sngl(this%tmax) + sngl(this%tmin)) * 0.5

      ! WARNING: Assuming CBH in Fahrenheit; conversion to Celsius can be remove
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
    end associate
  end subroutine

end submodule
