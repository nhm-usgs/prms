submodule (Simulation_class) sm_simulation

  contains
    !***********************************************************************
    ! Simulation constructor
    module subroutine init_Simulation(this, ctl_data)
      use iso_fortran_env, only: output_unit
      implicit none

      class(Simulation), intent(inout) :: this
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

      ! TODO: Only used by streamflow_muskingum currently. Needs streamflow_cfs when using
      !       replacement flow (obsin_segment). The Obs stuff is not completed so replacement
      !       flows will crash the model.
      this%model_obs = Obs(ctl_data)

      allocate(Temperature_hru::this%model_temp)
      call this%model_temp%init(ctl_data, this%model_basin, this%model_summary)

      allocate(Precipitation_hru::this%model_precip)
      call this%model_precip%init(ctl_data, this%model_basin, this%model_temp, this%model_summary)

      allocate(Climateflow::this%climate)
      call this%climate%init(ctl_data, this%model_basin, this%model_summary)

      ! TODO: PAN - add logic for other solar radiation modules
      allocate(Solrad_degday::this%solrad)
      call this%solrad%init(ctl_data, this%model_basin, this%model_summary)

      ! Example of accessing a variable in the instantiated class
      ! select type(solrad => this%solrad)
      !   type is (Solrad_degday)
      !     print *, sizeof(solrad%dday_intcp)
      ! end select

      ! TODO: PAN - add logic for other transpiration modules
      allocate(Transp_tindex::this%transpiration)
      call this%transpiration%init(ctl_data, this%model_basin, this%model_temp, this%model_summary)
      ! this%transpiration = Transp_tindex(ctl_data, this%model_basin, this%model_temp)

      ! Humidity
      if (allocated(ctl_data%humidity_module%values)) then
        select case(ctl_data%humidity_module%values(1)%s)
          case('humidity_hru')
            allocate(Humidity_hru::this%model_humidity)
          case('humidity_per')
            allocate(Humidity_per::this%model_humidity)
          case('humidity_sta')
            allocate(Humidity_sta::this%model_humidity)
          case default
            ! NOTE: This defaults humidity_hru to zero
            allocate(Humidity::this%model_humidity)
        end select
      else
        allocate(Humidity::this%model_humidity)
      end if

      call this%model_humidity%init(ctl_data, this%model_basin)

      ! Wind
      if (allocated(ctl_data%wind_module%values)) then
        select case(ctl_data%wind_module%values(1)%s)
          case('wind_hru')
            allocate(Wind_hru::this%model_wind)
          case('wind_sta')
            allocate(Wind_sta::this%model_wind)
          case default
            ! NOTE: This defaults humidity_hru to zero
            allocate(Wind::this%model_wind)
        end select
      else
        allocate(Wind::this%model_wind)
      end if

      call this%model_wind%init(ctl_data, this%model_basin)

      ! TODO: PAN - add logic for other potential ET modules
      allocate(Potet_jh::this%potet)
      call this%potet%init(ctl_data, this%model_basin, this%model_summary)
      ! this%potet = Potet_jh(ctl_data, this%model_basin, this%model_summary)

      allocate(Interception::this%intcp)
      call this%intcp%init(ctl_data, this%model_basin, this%transpiration, this%model_summary)
      ! this%intcp = Interception(ctl_data, this%model_basin, this%transpiration, this%model_summary)

      allocate(Snowcomp::this%snow)
      call this%snow%init(ctl_data, this%model_basin, this%climate, this%model_summary)
      ! this%snow = Snowcomp(ctl_data, this%model_basin, this%climate, this%model_summary)

      allocate(Srunoff::this%runoff)
      call this%runoff%init(ctl_data, this%model_basin, this%model_summary)
      ! this%runoff = Srunoff(ctl_data, this%model_basin, this%model_summary)

      allocate(Soilzone::this%soil)
      call this%soil%init(ctl_data, this%model_basin, this%climate, this%snow, this%runoff, this%model_summary)
      ! this%soil = Soilzone(ctl_data, this%model_basin, this%climate, this%snow, this%runoff, this%model_summary)

      allocate(Gwflow::this%groundwater)
      call this%groundwater%init(ctl_data, this%model_basin, this%climate, this%intcp, this%soil, this%runoff, this%model_summary)
      ! this%groundwater = Gwflow(ctl_data, this%model_basin, this%climate, this%intcp, this%soil, this%runoff, this%model_summary)

      select case(ctl_data%strmflow_module%values(1)%s)
        case('muskingum')
          allocate(Muskingum::this%model_streamflow)
        case('muskingum_mann')
          allocate(Muskingum_mann::this%model_streamflow)
        case('strmflow_in_out')
          allocate(Strmflow_in_out::this%model_streamflow)
        case default
          allocate(Streamflow::this%model_streamflow)
      end select

      if (allocated(this%model_streamflow)) then
        ! TODO: Should there be a default case?
        call this%model_streamflow%init(ctl_data, this%model_basin, this%model_time, this%model_summary)
      end if
      ! this%model_muskingum = Muskingum(ctl_data, this%model_basin, this%model_time, this%model_summary)

      if (ctl_data%print_debug%value == 1) then
        this%model_waterbal = WaterBalance(ctl_data, this%model_basin, this%groundwater)
      endif
    end subroutine

    module subroutine run_Simulation(this, ctl_data)
      use iso_fortran_env, only: output_unit
      implicit none

      class(Simulation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data

      ! ------------------------------------------------------------------------
      do
        if (.not. this%model_time%next()) exit
        ! print *, this%model_time%Nowyear, this%model_time%Nowmonth, this%model_time%Nowday

        ! write(output_unit, 9008) 'TIME: ', this%model_time%Nowtime(1:3)
        ! 9008 format(A, I4, 2('/', I2.2))

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

      call this%model_time%cleanup(ctl_data)
      call this%model_basin%cleanup(ctl_data)
      call this%climate%cleanup(ctl_data)
      call this%transpiration%cleanup(ctl_data)
      call this%intcp%cleanup(ctl_data)
      call this%snow%cleanup(ctl_data)
      call this%runoff%cleanup(ctl_data)
      call this%soil%cleanup(ctl_data)
      call this%groundwater%cleanup(ctl_data)

      if (allocated(this%model_streamflow)) then
        call this%model_streamflow%cleanup(ctl_data)
      end if

      call this%model_waterbal%cleanup()

      call ctl_data%cleanup()

    end subroutine
end submodule
