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
      this%climate = Climateflow(ctl_data, param_data)
      this%solt = Soltab(ctl_data, param_data, this%model_basin)
      this%model_obs = Obs(ctl_data)
      this%model_time = Time_t(ctl_data, this%model_basin)
      this%climate_by_hru = Climate_HRU(ctl_data, param_data)
      this%solrad = Ddsolrad(ctl_data)
      this%transpiration = Transp_tindex(ctl_data, param_data, this%model_basin, this%climate)
      this%potet = Potet_jh(ctl_data)

      if (ctl_data%nhruOutON_OFF%values(1) > 0) then
        this%summary_by_hru = Nhru_summary(ctl_data, param_data)
      endif

      if (ctl_data%basinOutON_OFF%values(1) == 1) then
        this%summary_by_basin = Basin_summary(ctl_data, param_data)
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

        call this%climate_by_hru%run(ctl_data, param_data, this%model_time, &
                                     this%model_basin, this%climate)

        call this%solrad%run(ctl_data, param_data, this%model_time, this%solt, &
                             this%climate, this%model_basin)

        call this%transpiration%run(ctl_data, param_data, this%model_time, &
                                    this%model_basin, this%climate)

        call this%potet%run(ctl_data, param_data, this%model_basin, this%model_time, this%climate)

        if (ctl_data%basinOutON_OFF%values(1) == 1) then
          call this%summary_by_basin%run(ctl_data, this%model_time, this%climate)
        endif

        if (ctl_data%nhruOutON_OFF%values(1) > 0) then
          call this%summary_by_hru%run(ctl_data, this%model_time, this%model_basin, this%climate)
        endif
      enddo
    end subroutine


    module subroutine cleanup_Simulation(this, ctl_data)
      implicit none

      class(Simulation), intent(in) :: this
      type(Control), intent(in) :: ctl_data

      ! ------------------------------------------------------------------------
      if (ctl_data%save_vars_to_file%values(1) == 1) then
        ! Write the important model information to the restart file
        write(ctl_data%restart_output_unit) this%model_time%timestep, &
                                            ctl_data%nhru%values(1), &
                                            ctl_data%temp_module%values(1)%s, &
                                            ctl_data%model_mode%values(1)%s

        call this%climate%cleanup(ctl_data)
        call this%model_obs%cleanup(ctl_data)
        call this%transpiration%cleanup(ctl_data)
      endif
    end subroutine
end submodule
