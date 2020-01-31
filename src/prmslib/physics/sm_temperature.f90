submodule(PRMS_TEMPERATURE) sm_temperature
contains
  module subroutine init_Temperature(this, ctl_data, model_basin, model_summary)
    class(Temperature), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj

    ! --------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              param_hdl => ctl_data%param_file_hdl, &

              nhru => model_basin%nhru)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Read the parameters for the temperature module
      call param_hdl%get_variable('temp_units', this%temp_units)

      ! Setup class variables
      allocate(this%tavg(nhru))
      allocate(this%tmax(nhru))
      allocate(this%tmin(nhru))

      ! NOTE: Only used by potet_jh; remove once temperature units are standardized
      allocate(this%tavg_f(nhru))
      allocate(this%tmax_f(nhru))

      ! TODO: PAN - these variables don't appear to be used anymore
      ! if (ctl_data%temp_module%values(1)%s /= 'climate_hru' .and. &
      !     ctl_data%temp_module%values(1)%s /= 'temp_sta') then
      !   allocate(this%tsta_elev_feet(ntemp))
      !   allocate(this%tsta_elev_meters(ntemp))
      !
      !   if (elev_units == FEET) then
      !     this%tsta_elev_feet = tsta_elev
      !     this%tsta_elev_meters = tsta_elev * FEET2METERS
      !   else
      !     this%tsta_elev_meters = tsta_elev
      !     this%tsta_elev_feet = tsta_elev * METERS2FEET
      !   endif
      ! endif

      ! TODO: PAN - these look like copies of tmax_adj and tmin_adj
      !           - used by temp_1sta_laps and temp_dist2
      ! allocate(this%tmax_aspect_adjust(nhru, 12), this%tmin_aspect_adjust(nhru, 12))

      ! NOTE: It is possible the output variable arrays can be re-allocated
      !       for each timestep (e.g. when using netcdf files), so we have to
      !       connect the pointers for nhru_summary output at each timestep
      !       during the model run instead connecting them during the
      !       constructor phase.
      this%has_hru_summary_vars = .false.

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj=1, outVar_names%size()
          ! TODO: This is where the daily basin values are linked based on
          !       what was requested in basinOutVar_names.
          select case(outVar_names%values(jj)%s)
            case('tavg')
              this%has_hru_summary_vars = .true.
            case('tmax_hru')
              this%has_hru_summary_vars = .true.
            case('tmin_hru')
              this%has_hru_summary_vars = .true.
            case default
              ! pass
          end select
        end do
      end if
    end associate
  end subroutine

  module subroutine run_Temperature(this, ctl_data, model_basin, model_time, model_summary)
    implicit none

    class(Temperature), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time
    type(Summary), intent(inout) :: model_summary

    ! --------------------------------------------------------------------------
  end subroutine

  module subroutine set_nhru_summary_ptrs(this, ctl_data, model_summary)
    implicit none

    class(Temperature), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj

    ! --------------------------------------------------------------------------
    associate(outVar_names => ctl_data%outVar_names)

      ! Connect any nhru_summary variables that need to be output
      do jj=1, outVar_names%size()
        select case(outVar_names%values(jj)%s)
          case('tavg')
            call model_summary%set_summary_var(jj, this%tavg)
          case('tmax_hru')
            call model_summary%set_summary_var(jj, this%tmax)
          case('tmin_hru')
            call model_summary%set_summary_var(jj, this%tmin)
          case default
            ! pass
        end select
      enddo
    end associate
  end subroutine
end submodule
