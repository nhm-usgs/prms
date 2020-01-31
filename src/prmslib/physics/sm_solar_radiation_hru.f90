submodule(SOLAR_RADIATION_HRU) sm_solar_radiation_hru
  contains
    module subroutine init_Solrad_hru(this, ctl_data, model_basin, model_summary)
      use UTILS_CBH, only: find_current_time, find_header_end
      implicit none

      class(Solrad_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary

      ! Local variables
      integer(i32) :: ierr
      integer(i32) :: istop = 0

      ! ------------------------------------------------------------------------
      ! Call the parent constructor first
      call this%SolarRadiation%init(ctl_data, model_basin, model_summary)

      associate(nhru => ctl_data%nhru%value, &
                cbh_binary_flag => ctl_data%cbh_binary_flag%value, &
                print_debug => ctl_data%print_debug%value, &
                start_time => ctl_data%start_time%values, &
                swrad_day => ctl_data%swrad_day%values(1))

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! Solar radiation
        call find_header_end(nhru, this%swrad_funit, ierr, swrad_day%s, 'swrad_day', &
                             (cbh_binary_flag==1))
        if (ierr == 1) then
          istop = 1
        else
          call find_current_time(ierr, this%swrad_funit, start_time, (cbh_binary_flag==1))
        endif
      end associate
    end subroutine


    module subroutine run_Solrad_hru(this, ctl_data, model_time, model_basin)
      class(Solrad_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin

      ! Local variables
      integer(i32) :: ios
      integer(i32) :: jj
        !! Counter
      integer(i32) :: yr, mo, dy, hr, mn, sec
        !! junk vars to hold time info from files

      ! ----------------------------------------------------------------------
      associate(day_of_year => model_time%day_of_year, &
                nhru => ctl_data%nhru%value, &
                orad_flag => ctl_data%orad_flag%value)

        if (orad_flag == 0) then
          read(this%swrad_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%swrad(jj), jj=1, nhru)

          ! FIXME: Bad assumption using HRU 1
          orad = sngl((dble(this%swrad(1)) * this%hru_cossl(1) * this%soltab_basinpotsw(day_of_year)) / this%soltab_potsw(day_of_year, 1))
        else
          ! orad is specified as last column in swrad_day CBH file
          ! WARNING: PAN With orad removed from solar radiation the next line won't work
          read(this%swrad_funit, *, IOSTAT=ios) yr, mo, dy, hr, mn, sec, (this%swrad(jj), jj=1, nhru), this%orad
        endif
      end associate
    end subroutine
end submodule
