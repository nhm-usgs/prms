submodule(PRMS_TEMPERATURE) sm_temperature
contains
  module function constructor_Temperature(ctl_data, basin_summary) result(this)
    type(Temperature) :: this
      !! Temperature class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin_summary_ptr), intent(inout) :: basin_summary

    integer(i32) :: jj


    ! Control
    ! nhru,

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              print_debug => ctl_data%print_debug%value, &
              basinOutON_OFF => ctl_data%basinOutON_OFF%value, &
              basinOutVars => ctl_data%basinOutVars%value, &
              basinOutVar_names => ctl_data%basinOutVar_names%values)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%tavg(nhru))
      allocate(this%tmax(nhru))
      allocate(this%tmin(nhru))

      ! NOTE: Only used by potet_jh; remove once temperature units are standardized
      allocate(this%tavg_f(nhru))
      allocate(this%tmax_f(nhru))

      allocate(this%basin_temp)
      allocate(this%basin_tmax)
      allocate(this%basin_tmin)

      ! Connect any basin summary variables that need to be output
      if (basinOutON_OFF == 1) then
        do jj = 1, basinOutVars
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
    end associate
  end function

  module subroutine run_Temperature(this, ctl_data, param_data, model_basin, model_time)
    class(Temperature), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in), optional :: model_time

    ! --------------------------------------------------------------------------
  end subroutine

end submodule
