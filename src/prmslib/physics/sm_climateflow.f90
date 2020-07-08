submodule (PRMS_CLIMATEVARS) sm_climateflow

contains
  !***********************************************************************
  ! Climateflow constructor
  module subroutine init_Climateflow(this, ctl_data, model_basin, model_summary)
    use iso_fortran_env, only: output_unit, error_unit
    use prms_constants, only: LAND, INACTIVE, LAKE, SWALE
    implicit none

    class(Climateflow), target, intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj

    ! ------------------------------------------------------------------------
    associate(init_vars_from_file => ctl_data%init_vars_from_file%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &
              save_vars_to_file => ctl_data%save_vars_to_file%value, &

              nhru => model_basin%nhru, &
              hru_type => model_basin%hru_type)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Parameters
      allocate(this%soil_moist_max(nhru))
      call param_hdl%get_variable('soil_moist_max', this%soil_moist_max)

      allocate(this%soil_rechr_max_frac(nhru))
      call param_hdl%get_variable('soil_rechr_max_frac', this%soil_rechr_max_frac)

      ! Soilzone variables
      allocate(this%soil_moist(nhru))
      allocate(this%soil_rechr(nhru))
      allocate(this%soil_rechr_max(nhru))

      this%soil_rechr_max = this%soil_rechr_max_frac * this%soil_moist_max

      if (any([0, 2, 5] == init_vars_from_file)) then
        ! Parameters soil_moist_init_frac and soil_rechr_init_frac are only needed if they
        ! are not initialized from a restart file
        allocate(this%soil_moist_init_frac(nhru))
        call param_hdl%get_variable('soil_moist_init_frac', this%soil_moist_init_frac)

        allocate(this%soil_rechr_init_frac(nhru))
        call param_hdl%get_variable('soil_rechr_init_frac', this%soil_rechr_init_frac)

        this%soil_moist = this%soil_moist_init_frac * this%soil_moist_max
        this%soil_rechr = this%soil_rechr_init_frac * this%soil_rechr_max

        deallocate(this%soil_moist_init_frac)
        deallocate(this%soil_rechr_init_frac)
      else
        ! ~~~~~~~~~~~~~~~~~~
        ! Init from restart
        call ctl_data%read_restart_variable('soil_moist', this%soil_moist)
        call ctl_data%read_restart_variable('soil_rechr', this%soil_rechr)
      end if

      where (hru_type == INACTIVE .or. hru_type == LAKE)
        this%soil_moist = 0.0
        this%soil_rechr = 0.0
      end where

      do jj=1, nhru
        if (any([LAND, SWALE] == hru_type(jj))) then
          if (this%soil_rechr_max(jj) > this%soil_moist_max(jj)) then
            write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_rechr_max > soil_moist_max (HRU=', jj, ')'
            this%soil_rechr_max(jj) = this%soil_moist_max(jj)
          end if

          if (this%soil_rechr(jj) > this%soil_rechr_max(jj)) then
            ! NOTE: PRMS5 has 'soil_rechr_init > soil_rechr_max'
            write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_rechr_init_frac > soil_rechr_max (HRU=', jj, ')'
            this%soil_rechr(jj) = this%soil_rechr_max(jj)
          end if

          if (this%soil_moist(jj) > this%soil_moist_max(jj)) then
            ! NOTE: PRMS5 has 'soil_moist_init > soil_moist_max'
            write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_moist_init_frac > soil_moist_max (HRU=', jj, ')'
            this%soil_moist(jj) = this%soil_moist_max(jj)
          end if

          if (this%soil_rechr(jj) > this%soil_moist(jj)) then
            ! NOTE: PRMS5 has 'soil_rechr_init > soil_moist_init'
            ! DEBUG: PAN - re-add output
            ! write(error_unit, 9012) MODNAME, '%init(): WARNING: soil_rechr_init_frac > soil_moist_init_frac (HRU=', jj, ')'
            this%soil_rechr(jj) = this%soil_moist(jj)
          end if
        end if
      end do

      9012 format(2X, A, A, I0, A)

      ! Snow
      allocate(this%pkwater_equiv(nhru))

      if (any([0, 2, 3] == init_vars_from_file)) then
        this%pkwater_equiv = 0.0_dp
        ! Additional initialization handled in snowcomp
      else
        ! ~~~~~~~~~~~~~~~~~~~~~~~~
        ! Initialize from restart
        call ctl_data%read_restart_variable('pkwater_equiv', this%pkwater_equiv)
      end if

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Output variables

      ! Connect any nhru_summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj=1, outVar_names%size()
          select case(outVar_names%values(jj)%s)
            case('pkwater_equiv')
              call model_summary%set_summary_var(jj, this%pkwater_equiv)
            case('soil_moist')
              call model_summary%set_summary_var(jj, this%soil_moist)
            case('soil_rechr')
              call model_summary%set_summary_var(jj, this%soil_rechr)
            case default
              ! pass
          end select
        enddo
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Restart variables
      if (save_vars_to_file == 1) then
        ! Create restart variables
        call ctl_data%add_variable('pkwater_equiv', this%pkwater_equiv, 'nhru', 'inches')
        call ctl_data%add_variable('soil_moist', this%soil_moist, 'nhru', 'inches')
        call ctl_data%add_variable('soil_rechr', this%soil_rechr, 'nhru', 'inches')
      end if
    end associate
  end subroutine


  module subroutine cleanup_Climateflow(this, ctl_data)
    use Control_class, only: Control
    implicit none

    class(Climateflow), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! ------------------------------------------------------------------------
    ! TODO: Update to reflect the full PRMS codebase
    associate(save_vars_to_file => ctl_data%save_vars_to_file%value)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Restart variables
      if (save_vars_to_file == 1) then
        ! Write out this module's restart variables
        call ctl_data%write_restart_variable('pkwater_equiv', this%pkwater_equiv)
        call ctl_data%write_restart_variable('soil_moist', this%soil_moist)
        call ctl_data%write_restart_variable('soil_rechr', this%soil_rechr)
      end if
    end associate
  end subroutine


end submodule
