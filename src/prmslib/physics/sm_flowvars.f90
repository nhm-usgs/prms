submodule (PRMS_FLOWVARS) sm_flowvars

  contains
    !***********************************************************************
    ! Flowvars constructor
    module function constructor_Flowvars(ctl_data, param_data) result(this)
      implicit none

      type(Flowvars) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data

      ! ------------------------------------------------------------------------
      associate(nhru => ctl_data%nhru%value, &
                nlake => ctl_data%nlake%value, &
                nsegment => ctl_data%nsegment%value, &
                dprst_flag => ctl_data%dprst_flag%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                soil_rechr_init_frac => param_data%soil_rechr_init_frac%values, &
                soil_rechr_max_frac => param_data%soil_rechr_max_frac%values, &
                soil_moist_init_frac => param_data%soil_moist_init_frac%values, &
                soil_moist_max => param_data%soil_moist_max%values, &
                ssstor_init_frac => param_data%ssstor_init_frac%values, &
                sat_threshold => param_data%sat_threshold%values)

        ! Soilzone variables
        allocate(this%hru_actet(nhru))
        allocate(this%slow_flow(nhru))
        allocate(this%slow_stor(nhru))
        allocate(this%soil_moist(nhru))
        allocate(this%soil_rechr_max(nhru))
        allocate(this%soil_rechr(nhru))
        allocate(this%soil_to_gw(nhru))
        allocate(this%soil_to_ssr(nhru))
        allocate(this%ssr_to_gw(nhru))
        allocate(this%ssres_flow(nhru))
        allocate(this%ssres_in(nhru))
        allocate(this%ssres_stor(nhru))

        this%hru_actet = 0.0
        this%slow_flow = 0.0
        ! ?this%slow_stor
        this%soil_moist = soil_moist_init_frac * soil_moist_max
        this%soil_rechr_max = soil_rechr_max_frac * soil_moist_max
        this%soil_rechr = soil_rechr_init_frac * this%soil_rechr_max
        this%soil_to_gw = 0.0
        this%soil_to_ssr = 0.0
        this%ssr_to_gw = 0.0
        this%ssres_flow = 0.0
        this%ssres_in = 0.0
        this%ssres_stor = ssstor_init_frac * sat_threshold

        ! NOTE: could deallocate soil_moist_init_frac, soil_rechr_init_frac,
        !       and ssstor_init_frac

        ! gwflow variables
        allocate(this%gwres_stor(nhru))
        this%gwres_stor = 0.0_dp

        ! srunoff variables
        allocate(this%imperv_stor(nhru))
        allocate(this%infil(nhru))
        allocate(this%sroff(nhru))

        this%imperv_stor = 0.0
        this%infil = 0.0
        this%sroff = 0.0

        ! Streamflow
        if (ctl_data%strmflow_module%values(1)%s == 'muskingum' .or. &
            ctl_data%strmflow_module%values(1)%s == 'muskingum_lake' .or. &
            ctl_data%strmflow_module%values(1)%s == 'strmflow_in_out') then
          if (nsegment < 1) then
            write(output_unit, *) 'ERROR: streamflow and cascade routing require nsegment > 0, specified as:', nsegment
            stop
          endif

          allocate(this%seg_inflow(nsegment))
          allocate(this%seg_lateral_inflow(nsegment))
          allocate(this%seg_outflow(nsegment))
          allocate(this%seg_upstream_inflow(nsegment))

          this%seg_upstream_inflow = 0.0_dp
          this%seg_lateral_inflow = 0.0_dp
        endif

        ! Lakes
        if (nlake > 0) then
          allocate(this%lake_vol(nlake))
          this%lake_vol = 0.0_dp
        endif

        ! Depression storage
        if (dprst_flag == 1) then
          allocate(this%dprst_vol_open(nhru))
          allocate(this%dprst_vol_clos(nhru))

          this%dprst_vol_open = 0.0_dp
          this%dprst_vol_clos = 0.0_dp
        endif

        if (init_vars_from_file == 1) then
          ! TODO: Get the init from file stuff hooked up
        else
          this%flow_out = 0.0_dp

          if (ctl_data%strmflow_module%values(1)%s == 'muskingum' .or. &
              ctl_data%strmflow_module%values(1)%s == 'muskingum_lake' .or. &
              ctl_data%strmflow_module%values(1)%s == 'strmflow_in_out') then
            ! TODO: why is the conditional necessary?
            this%seg_inflow = 0.0_dp
            this%seg_outflow = 0.0_dp
          endif
        endif
      end associate
    end function


    module function module_name() result(res)
      implicit none

      character(:), allocatable :: res

      res = MODNAME
    end function


    module function version() result(res)
      implicit none

      character(:), allocatable :: res

      res = MODVERSION
    end function
end submodule
