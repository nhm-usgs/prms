submodule(PRMS_TRANSPIRATION) sm_transpiration
contains
  module subroutine init_Transpiration(this, ctl_data, model_basin, model_temp, model_summary)
    implicit none

    class(Transpiration), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj
    ! --------------------------------------------------------------------------
    associate(init_vars_from_file => ctl_data%init_vars_from_file%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              print_debug => ctl_data%print_debug%value, &
              save_vars_to_file => ctl_data%save_vars_to_file%value, &

              nhru => model_basin%nhru)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%transp_on(nhru))

      if (init_vars_from_file == 0) then
        this%transp_on = .false.
      else
        call ctl_data%read_restart_variable('transp_on', this%transp_on)
      end if

      if (save_vars_to_file == 1) then
        ! Create restart variables
        call ctl_data%add_variable('transp_on', this%transp_on, 'nhru', 'none')
      end if

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          select case(outVar_names%values(jj)%s)
            case('transp_on')
              call model_summary%set_summary_var(jj, this%transp_on)
            case default
              ! pass
          end select
        enddo
      endif
    end associate
  end subroutine

  module subroutine run_Transpiration(this, ctl_data, model_time, model_basin, model_temp)
    implicit none

    class(Transpiration), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin
    class(Temperature), intent(in) :: model_temp

    ! --------------------------------------------------------------------------
    print *, 'Transpiration%run() stub'
  end subroutine

  module subroutine cleanup_Transpiration(this, ctl_data)
    class(Transpiration), intent(in) :: this
      !! Transpiration class
    type(Control), intent(in) :: ctl_data

    ! --------------------------------------------------------------------------
    associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
      if (save_vars_to_file == 1) then
        ! Write out this module's restart variables
        call ctl_data%write_restart_variable('transp_on', this%transp_on)
      end if
    end associate
  end subroutine
end submodule
