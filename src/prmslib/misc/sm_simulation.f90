submodule (Simulation_class) sm_simulation

  contains
    !***********************************************************************
    ! Simulation constructor
    module function constructor_Simulation(ctl_data, param_data) result(this)
      implicit none

      type(Simulation) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data

      ! ------------------------------------------------------------------------

      ! Initialize the simulation modules
      this%model_basin = Basin(ctl_data, param_data)
      this%model_time = Time_t(ctl_data, this%model_basin)

      if (ctl_data%basinOutON_OFF%value == 1) then
        this%summary_by_basin = Basin_summary_ptr(ctl_data, param_data)
      endif

      this%model_obs = Obs(ctl_data)

      allocate(Temperature_hru::this%model_temp)
      this%model_temp = Temperature_hru(ctl_data, this%summary_by_basin)

      allocate(Precipitation_hru::this%model_precip)
      this%model_precip = Precipitation_hru(ctl_data, param_data, this%summary_by_basin)

      this%climate = Climateflow(ctl_data, param_data)

      ! this%climate_by_hru = Climate_HRU(ctl_data)
      this%solrad = Solrad_degday(ctl_data, param_data, this%model_basin, this%summary_by_basin)
      this%transpiration = Transp_tindex(ctl_data, param_data, this%model_basin)
      this%potet = Potet_jh(ctl_data, this%summary_by_basin)
      this%intcp = Interception(ctl_data, this%transpiration, this%summary_by_basin)
      this%snow = Snowcomp(this%climate, ctl_data, param_data, this%model_basin, this%summary_by_basin)
      this%runoff = Srunoff(ctl_data, param_data, this%model_basin, this%summary_by_basin)
      this%soil = Soilzone(ctl_data, param_data, this%model_basin, this%climate, this%snow, this%summary_by_basin)
      this%groundwater = Gwflow(ctl_data, param_data, this%summary_by_basin, this%model_basin, this%climate, this%intcp, this%soil, this%runoff)
      this%model_muskingum = Muskingum(ctl_data, param_data, this%model_basin, this%model_time, this%summary_by_basin)

      if (ctl_data%nhruOutON_OFF%value > 0) then
        this%summary_by_hru = Nhru_summary(ctl_data, param_data)
      endif

      ! if (ctl_data%basinOutON_OFF%value == 1) then
      !   this%summary_by_basin = Basin_summary(ctl_data, param_data)
      ! endif

      if (ctl_data%print_debug%value == 1) then
        this%model_waterbal = WaterBalance(ctl_data, param_data, this%groundwater)
      endif
    end function

    module subroutine run_Simulation(this, ctl_data, param_data)
      implicit none

      class(Simulation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data

      ! ------------------------------------------------------------------------
      do
        if (.not. this%model_time%next(ctl_data, this%model_basin)) exit
        ! print *, this%model_time%Nowyear, this%model_time%Nowmonth, this%model_time%Nowday

        call this%model_temp%run(ctl_data, param_data, this%model_basin, this%model_time)
        ! print *, '1'
        call this%model_precip%run(ctl_data, param_data, this%model_basin, this%model_temp, this%model_time)
        ! call this%climate_by_hru%run(ctl_data, param_data, this%model_time, &
        !                              this%model_basin, this%potet, this%model_temp, &
        !                              this%climate)
        ! print *, '2'
        call this%solrad%run(ctl_data, param_data, this%model_time, this%model_obs, &
                             this%model_precip, this%model_basin, this%model_temp)

        ! print *, '3'
        call this%transpiration%run(ctl_data, param_data, this%model_time, &
                                    this%model_basin, this%model_temp)

        ! print *, '4'
        call this%potet%run(ctl_data, param_data, this%model_basin, this%model_time, &
                            this%solrad, this%model_temp)

        ! print *, '5'
        call this%intcp%run(ctl_data, param_data, this%model_basin, this%potet, &
                            this%model_precip, this%transpiration, this%climate, this%model_time)

        ! print *, '6'
        call this%snow%run(this%climate, ctl_data, param_data, this%model_time, &
                           this%model_basin, this%model_precip, this%model_temp, &
                           this%intcp, this%solrad, this%potet, this%transpiration)

        ! print *, '7'
        call this%runoff%run(ctl_data, param_data, this%model_basin, this%climate, &
                             this%potet, this%intcp, this%snow, this%model_time)

        ! print *, '8'
        call this%soil%run(ctl_data, param_data, this%model_basin, this%model_time, &
                           this%potet, this%model_precip, this%climate, this%intcp, &
                           this%snow, this%transpiration, this%runoff)

        ! print *, '9'
        call this%groundwater%run(ctl_data, param_data, this%model_basin, &
                                  this%climate, this%intcp, this%soil, this%runoff, &
                                  this%model_time)

        ! call this%model_route%run(ctl_data, param_data, this%model_basin, this%climate, this%groundwater, this%soil, this%runoff, this%model_time, this%solrad)

        ! print *, '10'
        call this%model_muskingum%run(ctl_data, param_data, this%model_basin, &
                                      this%potet, this%groundwater, this%soil, &
                                      this%runoff, this%model_time, this%solrad, &
                                      this%model_obs)

        ! ctl_data, param_data, model_basin, model_climate, groundwater, soil, runoff, &
        !   model_time, model_solrad, model_flow, model_obs

        if (ctl_data%basinOutON_OFF%value == 1) then
          call this%summary_by_basin%run(ctl_data, this%model_time)
        endif

        if (ctl_data%nhruOutON_OFF%value > 0) then
          call this%summary_by_hru%run(ctl_data, this%model_time, this%model_basin, &
                                       this%climate, this%groundwater, this%intcp, &
                                       this%model_precip, this%potet, this%snow, &
                                       this%soil, this%solrad, this%runoff, this%model_muskingum, &
                                       this%model_temp, this%transpiration)
        endif

        if (ctl_data%print_debug%value == 1) then
          call this%model_waterbal%run(ctl_data, param_data, this%model_basin, &
                                       this%climate, this%groundwater, this%intcp, &
                                       this%model_precip, this%snow, this%soil, &
                                       this%runoff, this%model_time)
        endif
      enddo
    end subroutine


    module subroutine cleanup_Simulation(this, ctl_data)
      implicit none

      class(Simulation), intent(in) :: this
      type(Control), intent(in) :: ctl_data

      ! ------------------------------------------------------------------------
      if (ctl_data%save_vars_to_file%value == 1) then
        ! Write the important model information to the restart file
        write(ctl_data%restart_output_unit) this%model_time%timestep, &
                                            ctl_data%nhru%value, &
                                            ctl_data%temp_module%values(1)%s, &
                                            ctl_data%model_mode%values(1)%s

        call this%climate%cleanup(ctl_data)
        call this%model_obs%cleanup(ctl_data)
        call this%transpiration%cleanup(ctl_data)
      endif
    end subroutine
end submodule
