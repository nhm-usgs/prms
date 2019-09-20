submodule (Simulation_class) sm_simulation

  contains
    !***********************************************************************
    ! Simulation constructor
    module function constructor_Simulation(ctl_data) result(this)
      use iso_fortran_env, only: output_unit
      implicit none

      type(Simulation) :: this
      type(Control), intent(in) :: ctl_data

      ! ------------------------------------------------------------------------

      ! Initialize the simulation modules
      ! this%model_basin = Basin(ctl_data)
      ! this%model_time = Time_t(ctl_data, this%model_basin%hemisphere)

      this%model_time = Time_t(ctl_data)
      this%model_basin = Basin(ctl_data)
      call this%model_time%set_hemisphere(this%model_basin%hemisphere)

      if (ctl_data%outVarON_OFF%value == 1) then
        this%model_summary = Summary(ctl_data, this%model_basin, this%model_time)
      end if

      ! if (ctl_data%basinOutON_OFF%value == 1) then
      !   this%summary_by_basin = Basin_summary_ptr(ctl_data)
      ! endif

      ! if (ctl_data%nhruOutON_OFF%value > 0) then
      !   this%summary_by_hru = Nhru_summary_ptr(ctl_data, this%model_basin, this%model_time)
      ! endif

      ! this%summary_crap = Summary(ctl_data, this%model_basin, this%model_time)

      ! TODO: Only used by streamflow_muskingum currently. Needs streamflow_cfs when using
      !       replacement flow (obsin_segment). The Obs stuff is not completed so replacement
      !       flows will crash the model.
      this%model_obs = Obs(ctl_data)

      allocate(Temperature_hru::this%model_temp)
      call this%model_temp%init(ctl_data, this%model_basin, this%model_summary)
      ! this%model_temp = Temperature_hru(ctl_data, this%model_basin, this%model_summary)

      allocate(Precipitation_hru::this%model_precip)
      call this%model_precip%init(ctl_data, this%model_basin, this%model_temp, this%model_summary)
      ! this%model_precip = Precipitation_hru(ctl_data, this%model_basin, this%model_temp, this%model_summary)

      call this%climate%init(ctl_data, this%model_basin, this%model_summary)
      ! this%climate = Climateflow(ctl_data, this%model_basin, this%model_summary)

      ! TODO: PAN - add logic for other solar radiation modules
      call this%solrad%init(ctl_data, this%model_basin, this%model_summary)
      ! this%solrad = Solrad_degday(ctl_data, this%model_basin, this%model_summary)

      ! TODO: PAN - add logic for other transpiration modules
      call this%transpiration%init(ctl_data, this%model_basin, this%model_temp)
      ! this%transpiration = Transp_tindex(ctl_data, this%model_basin, this%model_temp)

      ! TODO: PAN - add logic for other potential ET modules
      call this%potet%init(ctl_data, this%model_basin, this%model_summary)
      ! this%potet = Potet_jh(ctl_data, this%model_basin, this%model_summary)

      call this%intcp%init(ctl_data, this%model_basin, this%transpiration, this%model_summary)
      ! this%intcp = Interception(ctl_data, this%model_basin, this%transpiration, this%model_summary)

      call this%snow%init(ctl_data, this%model_basin, this%climate, this%model_summary)
      ! this%snow = Snowcomp(ctl_data, this%model_basin, this%climate, this%model_summary)

      call this%runoff%init(ctl_data, this%model_basin, this%model_summary)
      ! this%runoff = Srunoff(ctl_data, this%model_basin, this%model_summary)

      call this%soil%init(ctl_data, this%model_basin, this%climate, this%snow, this%runoff, this%model_summary)
      ! this%soil = Soilzone(ctl_data, this%model_basin, this%climate, this%snow, this%runoff, this%model_summary)

      call this%groundwater%init(ctl_data, this%model_basin, this%climate, this%intcp, this%soil, this%runoff, this%model_summary)
      ! this%groundwater = Gwflow(ctl_data, this%model_basin, this%climate, this%intcp, this%soil, this%runoff, this%model_summary)

      call this%model_streamflow%init(ctl_data, this%model_basin, this%model_time, this%model_summary)
      ! this%model_muskingum = Muskingum(ctl_data, this%model_basin, this%model_time, this%model_summary)

      if (ctl_data%print_debug%value == 1) then
        this%model_waterbal = WaterBalance(ctl_data, this%model_basin, this%groundwater)
      endif
    end function

    module subroutine run_Simulation(this, ctl_data)
      use iso_fortran_env, only: output_unit
      implicit none

      class(Simulation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data

      ! ------------------------------------------------------------------------
      do
        if (.not. this%model_time%next(ctl_data)) exit
        ! print *, this%model_time%Nowyear, this%model_time%Nowmonth, this%model_time%Nowday

        write(output_unit, 9008) 'TIME: ', this%model_time%Nowtime(1:3)
        9008 format(A, I4, 2('/', I2.2))

        call this%model_basin%run(ctl_data, this%model_time)

        call this%model_temp%run(ctl_data, this%model_basin, this%model_time, this%model_summary)
        ! print *, '1'
        call this%model_precip%run(ctl_data, this%model_basin, this%model_temp, this%model_time, this%model_summary)

        ! print *, '2'
        call this%solrad%run(ctl_data, this%model_time, &
                             this%model_precip, this%model_basin, this%model_temp)

        ! print *, '3'
        call this%transpiration%run(ctl_data, this%model_time, &
                                    this%model_basin, this%model_temp)

        ! print *, '4'
        call this%potet%run(ctl_data, this%model_basin, this%model_time, &
                            this%solrad, this%model_temp)

        ! print *, '5'
        call this%intcp%run(ctl_data, this%model_basin, this%potet, &
                            this%model_precip, this%transpiration, this%climate, this%model_time)

        ! print *, '6'
        ! ctl_data, model_basin, model_time, model_climate, model_precip, model_temp, intcp, model_solrad, model_potet, model_transp
        call this%snow%run(ctl_data, this%model_basin, this%model_time, this%climate, &
                           this%model_precip, this%model_temp, &
                           this%intcp, this%solrad, this%potet, this%transpiration)

        ! print *, '7'
        call this%runoff%run(ctl_data, this%model_basin, this%climate, &
                             this%potet, this%intcp, this%snow, this%model_time)

        ! print *, '8'
        call this%soil%run(ctl_data, this%model_basin, this%model_time, &
                           this%potet, this%model_precip, this%climate, this%intcp, &
                           this%snow, this%transpiration, this%runoff)

        ! print *, '9'
        call this%groundwater%run(ctl_data, this%model_basin, &
                                  this%climate, this%intcp, this%soil, this%runoff, &
                                  this%model_time)

        ! call this%model_route%run(ctl_data, param_data, this%model_basin, this%climate, this%groundwater, this%soil, this%runoff, this%model_time, this%solrad)

        ! print *, '10'
        call this%model_streamflow%run(ctl_data, this%model_basin, &
                                      this%potet, this%groundwater, this%soil, &
                                      this%runoff, this%model_time, this%solrad, &
                                      this%model_obs)

        if (ctl_data%outVarON_OFF%value == 1) then
          call this%model_summary%run(ctl_data, this%model_time, this%model_basin)
        end if

        if (ctl_data%print_debug%value == 1) then
          call this%model_waterbal%run(ctl_data, this%model_basin, &
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
      if (ctl_data%outVarON_OFF%value == 1) then
        call this%model_summary%cleanup()
      end if

      call this%runoff%cleanup()
      ! if (ctl_data%save_vars_to_file%value == 1) then
      !   ! Write the important model information to the restart file
      !   write(ctl_data%restart_output_unit) this%model_time%timestep, &
      !                                       ctl_data%nhru%value, &
      !                                       ctl_data%temp_module%values(1)%s, &
      !                                       ctl_data%model_mode%values(1)%s
      !
      !   call this%climate%cleanup(ctl_data)
      !   call this%model_obs%cleanup(ctl_data)
      !   call this%transpiration%cleanup(ctl_data)
      ! endif
    end subroutine
end submodule
