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
                init_vars_from_file => ctl_data%init_vars_from_file%value, &

                soil_rechr_init_frac => param_data%soil_rechr_init_frac%values, &
                soil_rechr_max_frac => param_data%soil_rechr_max_frac%values, &
                soil_moist_init_frac => param_data%soil_moist_init_frac%values, &
                soil_moist_max => param_data%soil_moist_max%values)

        ! Soilzone variables
        allocate(this%soil_moist(nhru))
        allocate(this%soil_rechr_max(nhru))
        allocate(this%soil_rechr(nhru))

        this%soil_moist = soil_moist_init_frac * soil_moist_max
        this%soil_rechr_max = soil_rechr_max_frac * soil_moist_max
        this%soil_rechr = soil_rechr_init_frac * this%soil_rechr_max

        ! NOTE: could deallocate soil_moist_init_frac, soil_rechr_init_frac,
        !       and ssstor_init_frac
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
