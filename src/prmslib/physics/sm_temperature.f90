submodule(PRMS_TEMPERATURE) sm_temperature
contains
  module function constructor_Temperature(ctl_data, model_basin, basin_summary, nhru_summary) result(this)
    type(Temperature) :: this
      !! Temperature class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Basin_summary_ptr), intent(inout) :: basin_summary
    type(Nhru_summary_ptr), intent(inout) :: nhru_summary

    integer(i32) :: jj

    ! Control
    ! nhru,

    ! --------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              basinOutON_OFF => ctl_data%basinOutON_OFF%value, &
              basinOutVars => ctl_data%basinOutVars%value, &
              basinOutVar_names => ctl_data%basinOutVar_names%values, &
              nhruOutON_OFF => ctl_data%nhruOutON_OFF%value, &
              nhruOutVars => ctl_data%nhruOutVars%value, &
              nhruOutVar_names => ctl_data%nhruOutVar_names%values, &
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

      allocate(this%basin_temp)
      allocate(this%basin_tmax)
      allocate(this%basin_tmin)


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

      ! Connect any basin summary variables that need to be output
      if (basinOutON_OFF == 1) then
        do jj=1, basinOutVars
          ! TODO: This is where the daily basin values are linked based on
          !       what was requested in basinOutVar_names.
          select case(basinOutVar_names(jj)%s)
            case('basin_temp')
              call basin_summary%set_basin_var(jj, this%basin_temp)
            case('basin_tmax')
              call basin_summary%set_basin_var(jj, this%basin_tmax)
            case('basin_tmin')
              call basin_summary%set_basin_var(jj, this%basin_tmin)
            case default
              ! pass
          end select
        enddo
      endif

      !     ! case('tminc')
      !     !   this%nhru_var_daily(:, jj) = climate%tminc
      !     ! case('tminf')
      !     !   this%nhru_var_daily(:, jj) = climate%tminf
      !     ! case('tmax_hru')
      !     !   this%nhru_var_daily(:, jj) = climate%tmax_hru
      !     ! case('tmin_hru')
      !     !   this%nhru_var_daily(:, jj) = climate%tmin_hru
      !     ! case('tavgc')
      !     !   this%nhru_var_daily(:, jj) = climate%tavgc
      !
      !     case('tavgf')
      !       this%nhru_var_daily(:, jj) = model_temp%tavg_f
      !     case('tmaxf')
      !       this%nhru_var_daily(:, jj) = model_temp%tmax_f
      !     case('tminf')
      !       this%nhru_var_daily(:, jj) = c_to_f(model_temp%tmin)

      ! NOTE: It is possible the output variable arrays can be re-allocated
      !       for each timestep (e.g. when using netcdf files), so we have to
      !       connect the pointers for nhru_summary output at each timestep
      !       during the model run instead connecting them during the
      !       constructor phase.
      this%has_hru_summary_vars = .false.

      if (nhruOutON_OFF == 1) then
        do jj=1, nhruOutVars
          select case(nhruOutVar_names(jj)%s)
            case('tavg')
              this%has_hru_summary_vars = .true.
              exit
            case('tmax')
              this%has_hru_summary_vars = .true.
              exit
            case('tmin')
              this%has_hru_summary_vars = .true.
              exit
            case default
              ! pass
          end select
        end do
      end if
    end associate
  end function

  module subroutine run_Temperature(this, ctl_data, model_basin, model_time, nhru_summary)
    implicit none

    class(Temperature), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time
    type(Nhru_summary_ptr), intent(inout) :: nhru_summary

    ! --------------------------------------------------------------------------
  end subroutine

  module subroutine set_nhru_summary_ptrs(this, ctl_data, nhru_summary)
    implicit none

    class(Temperature), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Nhru_summary_ptr), intent(inout) :: nhru_summary

    integer(i32) :: jj

    ! --------------------------------------------------------------------------
    associate(nhruOutON_OFF => ctl_data%nhruOutON_OFF%value, &
              nhruOutVars => ctl_data%nhruOutVars%value, &
              nhruOutVar_names => ctl_data%nhruOutVar_names%values)

      ! Connect any nhru_summary variables that need to be output
      do jj=1, nhruOutVars
        select case(nhruOutVar_names(jj)%s)
          case('tavg')
            call nhru_summary%set_nhru_var(jj, this%tavg)
          case('tmax')
            call nhru_summary%set_nhru_var(jj, this%tmax)
          case('tmin')
            call nhru_summary%set_nhru_var(jj, this%tmin)
          case default
            ! pass
        end select
      enddo
    end associate
  end subroutine
end submodule
