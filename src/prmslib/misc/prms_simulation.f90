module PRMS_SIMULATION
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_CLIMATE_HRU, only: Climate_HRU
  use PRMS_SOLTAB, only: Soltab
  use PRMS_DDSOLRAD, only: Ddsolrad
  use PRMS_TRANSP_TINDEX, only: Transp_tindex
  use PRMS_POTET_JH, only: Potet_jh
  use PRMS_BASIN_SUMMARY, only: Basin_summary
  use PRMS_NHRU_SUMMARY, only: Nhru_summary
  use PRMS_SET_TIME, only: Time
  use PRMS_OBS, only: Obs
  implicit none

  private
  public :: Simulation

  type Simulation
      class(Basin), allocatable :: model_basin
      class(Climateflow), allocatable :: climate
      type(Soltab) :: solt
      type(Obs) :: model_obs
      type(Time) :: model_time

      type(Climate_HRU) :: climate_by_hru
      type(Ddsolrad) :: solrad
      class(Transp_tindex), allocatable :: transpiration
      type(Potet_jh) :: potet
      type(Nhru_summary) :: summary_by_hru
      type(Basin_summary) :: summary_by_basin
    contains
      procedure, public :: run => run_Simulation
  end type

  interface Simulation
    !! Simulation constructor
    module function constructor_Simulation(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters

      type(Simulation) :: this
        !! Simulation class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  contains
    !***********************************************************************
    ! Simulation constructor
    module function constructor_Simulation(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      implicit none

      type(Simulation) :: this
      class(Control), intent(in) :: ctl_data
      class(Parameters), intent(in) :: param_data

      ! ------------------------------------------------------------------------
      ! TODO: open ctl_data%model_output_file (use PRMS_output_unit)
      ! TODO: open var_init_file (use Restart_inunit) if init_vars_from_file
      ! TODO: open var_save_file (use Restart_outunit) if save_vars_to_file

      ! Initialize the simulation modules
      this%model_basin = Basin(ctl_data, param_data)
      this%climate = Climateflow(ctl_data, param_data)
      this%solt = Soltab(ctl_data, param_data, this%model_basin)
      this%model_obs = Obs(ctl_data)
      this%model_time = Time(ctl_data, this%model_basin)
      this%climate_by_hru = Climate_HRU(ctl_data, param_data)
      this%solrad = Ddsolrad(ctl_data)
      this%transpiration = Transp_tindex(ctl_data, param_data, this%model_basin, this%climate)
      this%potet = Potet_jh(ctl_data)

      if (ctl_data%nhruOutON_OFF%values(1) > 0) then
        this%summary_by_hru = Nhru_summary(ctl_data, param_data)
      endif

      if (ctl_data%basinOutON_OFF%values(1) == 1) then
        this%summary_by_basin = Basin_summary(ctl_data, param_data)
      endif
    end function



    subroutine run_Simulation(this, ctl_data, param_data)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      implicit none

      class(Simulation), intent(inout) :: this
      class(Control), intent(in) :: ctl_data
      class(Parameters), intent(in) :: param_data

      ! ------------------------------------------------------------------------
      do
        if (.not. this%model_time%next(ctl_data, this%model_basin)) exit

        call this%climate_by_hru%run(ctl_data, param_data, this%model_time, &
                                     this%model_basin, this%climate)

        call this%solrad%run(ctl_data, param_data, this%model_time, this%solt, &
                             this%climate, this%model_basin)

        call this%transpiration%run(ctl_data, param_data, this%model_time, this%model_basin, &
                                    this%climate)

        call this%potet%run(ctl_data, param_data, this%model_basin, this%model_time, this%climate)

        if (ctl_data%basinOutON_OFF%values(1) == 1) then
          call this%summary_by_basin%run(ctl_data, this%model_time, this%climate)
        endif

        if (ctl_data%nhruOutON_OFF%values(1) > 0) then
          call this%summary_by_hru%run(ctl_data, this%model_time, this%model_basin, this%climate)
        endif
      enddo
    end subroutine


    ! subroutine prms_header(this, ctl_data)
    !   ! use PRMS_MODULE, only: Print_debug, PRMS_output_unit, PRMS_VERSION, PRMS_versn, &
    !   !                        Version_read_control_file, Version_read_parameter_file, print_module
    !   ! use prms_constants, only: EQULS
    !   implicit none
    !
    !   class(Simulation), intent(in) :: this
    !   class(Control), intent(in) :: ctl_data
    !
    !   ! ------------------------------------------------------------------------
    !   associate(print_debug => ctl_data%print_debug%values(1), &
    !             model_output_unit => ctl_data%model_output_unit)
    !
    !     if (print_debug > -2) then
    !       ! print 10, PRMS_VERSION
    !       ! write (PRMS_output_unit, 10) PRMS_VERSION
    !       write(model_output_unit, )
    !       print 15
    !       print 9002
    !       write (PRMS_output_unit, 15)
    !       print 16, EQULS
    !       write (PRMS_output_unit, 16) EQULS
    !     endif
    !
    !     10  FORMAT(/, 15X, 'Precipitation-Runoff Modeling System (PRMS)', /, 23X, A)
    !     15  FORMAT(/, 8X, 'Process', 12X, 'Available Modules', /, 68('-'), /, &
    !                       '  Basin Definition: basin', /, &
    !                       '  Time Series Data: obs', /, &
    !                       '   Potet Solar Rad: soltab', /, &
    !                       '  Temperature Dist: climate_hru', /, &
    !                       '       Precip Dist: climate_hru', /, &
    !                       '    Solar Rad Dist: ddsolrad', /, &
    !                       'Transpiration Dist: transp_tindex', /, &
    !                       '    Output Summary: nhru_summary, basin_summary', /, 68('-'))
    !     16  FORMAT(//, 4X, 'Active modules listed in the order in which they are called', //, 8X, 'Process', 19X, &
    !                        'Module', 16X, 'Version Date', /, A)
    !
    !     call print_module(PRMS_versn, 'PRMS6 Computation Order     ', 90)
    !     call print_module(Version_read_control_file, 'Read Control File           ', 90)
    !     call print_module(Version_read_parameter_file, 'Read Parameter File         ', 90)
    !
    !     9002 FORMAT(//, 74('='), /, 'Please give careful consideration to fixing all ERROR and WARNING messages', &
    !             /, 74('='), /)
    !   end associate
    ! end subroutine
end module
