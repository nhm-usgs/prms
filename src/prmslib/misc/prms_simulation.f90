module PRMS_SIMULATION
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_CLIMATE_HRU, only: Climate_HRU
  use PRMS_SOLTAB, only: Soltab
  use PRMS_DDSOLRAD, only: ddsolrad
  use PRMS_TRANSP_TINDEX, only: Transp_tindex
  use PRMS_POTET_JH, only: run_potet_jh
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
      ! ddsolrad
      class(Transp_tindex), allocatable :: transpiration
      ! potet_jh
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

      ! Initialize the simulation modules
      print *, '-- Init model_basin'
      this%model_basin = Basin(ctl_data, param_data)

      print *, '-- Init climate'
      this%climate = Climateflow(ctl_data, param_data)

      print *, '-- Init solt'
      this%solt = Soltab(ctl_data, param_data, this%model_basin)

      print *, '-- Init obs'
      this%model_obs = Obs(ctl_data)

      print *, '-- Init model_time'
      this%model_time = Time(ctl_data, this%model_basin)

      print *, '-- Init climate_by_HRU'
      this%climate_by_hru = Climate_HRU(ctl_data, param_data)

      ! something with ddsolrad
      print *, '-- ddsolrad() -- no initialization required'

      print *, '-- Init transpiration'
      ! ctl_data, param_data, model_basin, climate
      this%transpiration = Transp_tindex(ctl_data, param_data, this%model_basin, this%climate)

      print *, '-- run_potet_jh() -- no initialization required'

      if (ctl_data%nhruOutON_OFF%values(1) > 0) then
        print *, '-- Init summary_by_hru'
        this%summary_by_hru = Nhru_summary(ctl_data, param_data)
      endif

      if (ctl_data%basinOutON_OFF%values(1) == 1) then
        print *, '-- Init summary_by_basin'
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

      logical :: crap
      integer(i32) :: ii

      ! ------------------------------------------------------------------------
      print *, '-- run_Simulation()'
      do
        if (.not. this%model_time%next(ctl_data, this%model_basin)) exit

        ! print *, '-- climate_by_hru.run()'
        ! ctl_data, param_data, the_basin, climate, model_time
        call this%climate_by_hru%run(ctl_data, param_data, this%model_time, &
                                     this%model_basin, this%climate)

        ! print *, '-- run ddsolrad'
        call ddsolrad(ctl_data, param_data, this%model_time, this%solt, this%climate, &
                      this%model_basin)

        ! print *, '-- transpiration.run()'
        ! ctl_data, model_time, model_basin, param_data, climate
        call this%transpiration%run(ctl_data, param_data, this%model_time, this%model_basin, &
                                    this%climate)

        ! print *, '-- run_potet_jh()'
        ! ctl_data, param_data, model_basin, model_time, climate
        call run_potet_jh(ctl_data, param_data, this%model_basin, this%model_time, this%climate)

        call this%summary_by_basin%run(ctl_data, this%model_time, this%climate)
        call this%summary_by_hru%run(ctl_data, this%model_time, this%model_basin, this%climate)
      enddo

      ! TODO: write the nhru_summary.run() routine
      ! if (ctl_data%nhruOutON_OFF%values(1) > 0) then
      !   print *, '-- summary_by_hru.run()'
      !   this%summary_by_hru.run()
      ! endif

      ! TODO: write the basin_summary.run() routine
      ! if (ctl_data%basinOutON_OFF%values(1) == 1) then
      !   print *, '-- summary_by_basin.run()'
      !   this%summary_by_basin.run()
      ! endif
    end subroutine

end module
