submodule(PRMS_POTET) sm_potet
contains
  module function constructor_Potet(ctl_data) result(this)
    use UTILS_CBH, only: find_current_time, find_header_end
    implicit none

    type(Potential_ET) :: this
      !! Potential_ET class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! Control
    ! nhru, cbh_binary_flag, et_module, humidity_day, stream_temp_flag,
    ! strmtemp_humidity_flag,

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              et_module => ctl_data%et_module%values(1), &

              ! NOTE: humidity_day needs a default value to be associated
              ! humidity_day => ctl_data%humidity_day%values(1), &
              start_time => ctl_data%start_time%values, &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value)

      allocate(this%potet(nhru))
      this%potet = 0.0

      if (et_module%s == 'potet_pt' .or. et_module%s == 'potet_pm' .or. &
          (stream_temp_flag == 1 .and. strmtemp_humidity_flag == 0)) then
        allocate(this%humidity_hru(nhru))
        this%humidity_hru = 0.0

        call find_header_end(nhru, this%humidity_funit, ierr, &
                             ctl_data%humidity_day%values(1)%s, &
                             'humidity_day', (cbh_binary_flag==1))

        if (ierr == 1) then
          istop = 1
        else
          ! TODO: Original code defaults ierr=2 which causes the humidity_cbh_flag
          !       to be reset and allows climate_flow to use humidity_percent
          !       from the parameter file instead. Need to figure out a good way
          !       to handle this.
          call find_current_time(ierr, this%humidity_funit, start_time, (cbh_binary_flag==1))
        endif
      endif
    end associate
  end function

  module subroutine run_Potet(this, ctl_data, param_data, model_basin)
    class(Potential_ET), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin

    ! Local variables
    integer(i32) :: jj
    integer(i32) :: ios
    integer(i32) :: yr, mo, dy, hr, mn, sec

    ! Control
    ! nhru, et_module, stream_temp_flag, strmtemp_humidity_flag,

    ! Basin
    ! basin_area_inv

    ! Parameters
    ! hru_area,

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              et_module => ctl_data%et_module%values(1), &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value, &
              basin_area_inv => model_basin%basin_area_inv, &
              hru_area => param_data%hru_area%values)

      ! Humidity
      if (et_module%s == 'potet_pt' .or. et_module%s == 'potet_pm' .or. &
          (stream_temp_flag == 1 .and. strmtemp_humidity_flag == 0)) then
        read(this%humidity_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%humidity_hru(jj), jj=1, nhru)

        this%basin_humidity = sum(dble(this%humidity_hru * hru_area)) * basin_area_inv
      endif
    end associate
  end subroutine


end submodule
