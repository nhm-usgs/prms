!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
module PRMS_OBS
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_SET_TIME, only: Time_t
  implicit none

  private
  public :: Obs

  character(len=*), parameter :: MODDESC = 'Time Series Data'
  character(len=*), parameter :: MODNAME = 'obs'
  character(len=*), parameter :: MODVERSION = '2018-08-30 13:38:00Z'

  type, extends(ModelBase) :: Obs
    ! Declared Variables
    real(r32), allocatable :: gate_ht(:)
      !! Lake module gate height
    ! real(r32), allocatable :: humidity(:)
    real(r32), allocatable :: lake_elev(:)
      !! Lake module lake elevation
    real(r32), allocatable :: pan_evap(:)
    ! real(r32), allocatable :: precip(:)
    real(r32), allocatable :: runoff(:)
    real(r32), allocatable :: snowdepth(:)
    ! real(r32), allocatable :: solrad(:)
    ! real(r32), allocatable :: tmax(:)
    ! real(r32), allocatable :: tmin(:)
    ! real(r32), allocatable :: wind_speed(:)

    real(r64), allocatable :: streamflow_cfs(:)
    real(r64), allocatable :: streamflow_cms(:)

    contains
      ! procedure, public :: run => run_Obs
      procedure, public :: cleanup => cleanup_Obs
        !! Final cleanup code after simulation
  end type


  interface Obs
    !! Obs constructor
    module function constructor_Obs(ctl_data) result(this)
      type(Obs) :: this
        !! Obs class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine cleanup_Obs(this, ctl_data)
      class(Obs), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  ! interface
  !   module subroutine run_Obs(this, ctl_data, param_data, model_time, model_basin)
  !     class(Obs), intent(inout) :: this
  !     type(Control), intent(in) :: ctl_data
  !     type(Parameters), intent(in) :: param_data
  !     type(Time_t), intent(in) :: model_time
  !     type(Basin), intent(in) :: model_basin
  !   end subroutine
  ! end interface

end module
