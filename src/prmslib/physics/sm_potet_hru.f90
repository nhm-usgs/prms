submodule(PRMS_POTET_HRU) sm_potet_hru
contains
  module subroutine init_Potet_hru(this, ctl_data, model_basin, model_summary)
    use UTILS_CBH, only: find_current_time, find_header_end
    implicit none

    class(Potet_hru), intent(inout) :: this
      !! Poteh_jh class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! Control
    ! nhru, cbh_binary_flag, potet_day, start_time

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Potential_ET%init(ctl_data, model_basin, model_summary)
    ! this%Potential_ET = Potential_ET(ctl_data, model_summary)

    associate(nhru => model_basin%nhru%value, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              print_debug => ctl_data%print_debug%value, &
              potet_day => ctl_data%potet_day%values(1), &
              start_time => ctl_data%start_time%values)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      call find_header_end(nhru, this%et_funit, ierr, potet_day%s, 'potet_day', &
                           (cbh_binary_flag==1))
      if (ierr == 1) then
        istop = 1
      else
        call find_current_time(ierr, this%et_funit, start_time, (cbh_binary_flag==1))
      endif
    end associate
  end subroutine

  module subroutine run_Potet_hru(this, ctl_data, model_time, model_basin)
    use prms_constants, only: dp
    use UTILS_CBH, only: find_current_time, find_header_end
    implicit none

    class(Potet_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin

    ! Local variables
    integer(i32) :: idx1D
    integer(i32) :: ios
    integer(i32) :: jj
      !! Counter
    integer(i32) :: yr, mo, dy, hr, mn, sec
      !! junk vars to hold time info from files
    real(r32), pointer, contiguous :: potet_adj_2d(:,:)

    ! Control
    ! nhru, nmonths

    ! Basin
    ! basin_area_inv

    ! Parameters
    ! hru_area, potet_cbh_adj (not included in associate)

    ! Time_t
    ! curr_month [Nowmonth]
    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              nmonths => ctl_data%nmonths%value, &

              basin_area_inv => model_basin%basin_area_inv, &

              hru_area => param_data%hru_area%values, &
              potet_cbh_adj => param_data%potet_cbh_adj%values, &

              curr_month => model_time%Nowmonth)

      read(this%et_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%potet(jj), jj=1, nhru)
      ! this%basin_potet = 0.0_dp

      ! FIXME: This is dangerous because it circumvents the intent for param_data
      ! potet_adj_2d => get_array(param_data%potet_cbh_adj%values, (/nhru, nmonths/))

      ! Potential_ET(i) = Potential_ET(i)*Potet_cbh_adj(i, Nowmonth)
      ! this%potet = this%potet * potet_adj_2d(:, curr_month)

      do jj=1, nhru
        idx1D = (curr_month - 1) * nhru + jj
        this%potet = this%potet * potet_cbh_adj(idx1D)
      end do

    end associate
  end subroutine

end submodule
