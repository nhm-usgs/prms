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
    associate(print_debug => ctl_data%print_debug%value, &
              nhru => model_basin%nhru, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%transp_on(nhru))
      this%transp_on = .false.

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

end submodule
