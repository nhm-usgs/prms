module PRMS_MUSKINGUM
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  ! use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_GWFLOW, only: Gwflow
  use PRMS_FLOWVARS, only: Flowvars
  ! use PRMS_INTCP, only: Interception
  use PRMS_OBS, only: Obs
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SOILZONE, only: Soilzone
  ! use PRMS_SNOW, only: Snowcomp
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_ROUTING, only: Routing
  implicit none

  private
  public :: Muskingum

  character(len=*), parameter :: MODDESC = 'Streamflow routing'
  character(len=*), parameter :: MODNAME = 'muskingum'
  character(len=*), parameter :: MODVERSION = '2018-06-25 18:42:00Z'

  type Muskingum
    ! Local Variables
    real(r64), allocatable :: currinsum(:)
    real(r64), allocatable :: inflow_ts(:)
    real(r64), allocatable :: outflow_ts(:)
    real(r64), allocatable :: pastin(:)
    real(r64), allocatable :: pastout(:)

    ! Declared Parameters
    ! real(r32), allocatable :: segment_flow_init(:)

    contains
      procedure, public :: run => run_Muskingum
      procedure, public :: cleanup => cleanup_Muskingum
  end type

  interface Muskingum
    !! Muskingum constructor
    module function constructor_Muskingum(ctl_data, param_data, model_basin, &
                                          model_flow, model_route, model_time) result(this)
      use prms_constants, only: dp
      implicit none

      type(Muskingum) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Flowvars), intent(inout) :: model_flow
      type(Routing), intent(inout) :: model_route
      type(Time_t), intent(in) :: model_time
    end function
  end interface

  interface
    module subroutine run_Muskingum(this, ctl_data, param_data, model_basin, &
                                    groundwater, soil, runoff, model_obs, &
                                    model_route, model_time, model_flow)
      use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH
      implicit none

      class(Muskingum) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Gwflow), intent(in) :: groundwater
        !! Groundwater variables
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Obs), intent(in) :: model_obs
      type(Routing), intent(inout) :: model_route
      type(Time_t), intent(in) :: model_time
      type(Flowvars), intent(inout) :: model_flow
    end subroutine
  end interface

  interface
    module subroutine cleanup_Muskingum(this)
      class(Muskingum) :: this
        !! Muskingum class
    end subroutine
  end interface
end module
