submodule(PRMS_TRANSP_HRU) sm_transp_hru
contains
  module function constructor_Transp_hru(ctl_data) result(this)
    use UTILS_CBH, only: find_header_end, find_current_time
    implicit none

    type(Transp_hru) :: this
      !! Transp_hru class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters

    ! Local variables
    integer(i32) :: ierr
    integer(i32) :: istop = 0

    ! Control
    ! nhru, cbh_binary_flag, start_time, transp_day,

    ! --------------------------------------------------------------------------
    ! Call the parent constructor first
    this%Transpiration = Transpiration(ctl_data)

    associate(nhru => ctl_data%nhru%value, &
              cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
              print_debug => ctl_data%print_debug%value, &
              transp_day => ctl_data%transp_day%values(1), &
              start_time => ctl_data%start_time%values)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      call find_header_end(nhru, this%transp_funit, ierr, &
                                ctl_data%transp_day%values(1)%s, 'transp_day', &
                                (cbh_binary_flag==1))
      if (ierr == 1) then
        istop = 1
      else
        call find_current_time(ierr, this%transp_funit, start_time, (cbh_binary_flag==1))
      endif
    end associate
  end function

  module subroutine run_Transp_hru(this, ctl_data)
    class(Transp_hru), intent(inout) :: this
    type(Control), intent(in) :: ctl_data

    ! Local variables
    integer(i32) :: ios
    integer(i32) :: jj
    integer(i32) :: yr, mo, dy, hr, mn, sec
      !! junk vars to hold time info from files

    ! Control
    ! nhru

    ! --------------------------------------------------------------------------
    ! Call parent run_Transpiration() procedure if needed
    ! call this%run_Transpiration(ctl_data)

    associate(nhru => ctl_data%nhru%value)

      read(this%transp_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%transp_on(jj), jj=1, nhru)

      if (any(this%transp_on==1)) then
        this%basin_transp_on = 1
      else
        this%basin_transp_on = 0
      endif
    end associate
  end subroutine

end submodule
