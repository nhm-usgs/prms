submodule(SOLAR_RADIATION_HRU) sm_solar_radiation_hru
  contains
    module function constructor_Solrad_hru(ctl_data, param_data, model_basin) result(this)
      use UTILS_CBH, only: find_current_time, find_header_end
      implicit none

      type(Solrad_hru) :: this
        !! Solrad_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin

      ! Local variables
      integer(i32) :: ierr
      integer(i32) :: istop = 0

      ! Control
      ! nhru, cbh_binary_flag, print_debug, start_time, swrad_day,

      ! ------------------------------------------------------------------------
      ! Call the parent constructor first
      this%SolarRadiation = SolarRadiation(ctl_data, param_data, model_basin)

      associate(nhru => ctl_data%nhru%value, &
                cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
                print_debug => ctl_data%print_debug%value, &
                start_time => ctl_data%start_time%values, &
                swrad_day => ctl_data%swrad_day%values(1))

        ! if (print_debug > -2) then
        !   ! Output module and version information
        !   call print_module_info(MODNAME, MODDESC, MODVERSION)
        ! endif

        ! Solar radiation
        call find_header_end(nhru, this%swrad_funit, ierr, swrad_day%s, 'swrad_day', &
                             (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%swrad_funit, start_time, (cbh_binary_flag==1))
        endif
      end associate
    end function


    module subroutine run_Solrad_hru(this, ctl_data, param_data, model_time, model_basin)
      class(Solrad_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin

      ! Local variables
      integer(i32) :: ios
      integer(i32) :: jj
        !! Counter
      integer(i32) :: yr, mo, dy, hr, mn, sec
        !! junk vars to hold time info from files

      ! Control
      ! nhru, orad_flag

      ! Parameter
      ! hru_area

      ! Basin
      ! basin_area_inv

      ! Time_t
      ! day_of_year
      ! ----------------------------------------------------------------------
      associate(day_of_year => model_time%day_of_year, &
                nhru => ctl_data%nhru%value, &
                orad_flag => ctl_data%orad_flag%value, &
                basin_area_inv => model_basin%basin_area_inv, &
                hru_area => param_data%hru_area%values)

        ! basin_horad = soltab_basinpotsw(day_of_year)  ! Calculated in cc/ddsolrad

        if (orad_flag == 0) then
          read(this%swrad_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%swrad(jj), jj=1, nhru)

          ! FIXME: Bad assumption using HRU 1
          orad = sngl((dble(this%swrad(1)) * this%hru_cossl(1) * this%soltab_basinpotsw(day_of_year)) / this%soltab_potsw(day_of_year, 1))
        else
          ! orad is specified as last column in swrad_day CBH file
          read(this%swrad_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%swrad(jj), jj=1, nhru), this%orad
        endif

        this%basin_swrad = sum(dble(this%swrad * hru_area)) * basin_area_inv
        this%basin_potsw = this%basin_swrad
      end associate
    end subroutine
end submodule
