module PRMS_ROUTING
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_GWFLOW, only: Gwflow
  ! use PRMS_FLOWVARS, only: Flowvars
  ! use PRMS_INTCP, only: Interception
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SOILZONE, only: Soilzone
  ! use PRMS_SNOW, only: Snowcomp
  use PRMS_SRUNOFF, only: Srunoff
  implicit none

  private
  public :: Routing

  character(len=*), parameter :: MODDESC = 'Routing initialization'
  character(len=*), parameter :: MODNAME = 'routing'
  character(len=*), parameter :: MODVERSION = '2018-06-25 16:20:00Z'

  type Routing
      ! Local Variables
      integer(i32) :: noarea_flag
      integer(i32) :: use_transfer_segment

      real(r64) :: cfs2acft
      real(r64) :: segment_area

      integer(i32), allocatable :: segment_order(:)
      integer(i32), allocatable :: segment_up(:)
      integer(i32), allocatable :: ts_i(:)

      real(r32), allocatable :: c0(:)
      real(r32), allocatable :: c1(:)
      real(r32), allocatable :: c2(:)
      real(r32), allocatable :: ts(:)

      real(r64), allocatable :: segment_hruarea(:)

      ! Declared Variables
      real(r64) :: basin_segment_storage
      real(r64) :: flow_headwater
      real(r64) :: flow_in_great_lakes
      real(r64) :: flow_in_nation
      real(r64) :: flow_in_region
      real(r64) :: flow_out_NHM
      real(r64) :: flow_out_region
      real(r64) :: flow_replacement
      real(r64) :: flow_terminus
      real(r64) :: flow_to_great_lakes
      real(r64) :: flow_to_lakes
      real(r64) :: flow_to_ocean

      real(r64), allocatable :: hru_outflow(:)
      real(r64), allocatable :: seg_gwflow(:)
      real(r64), allocatable :: seg_lateral_inflow(:)
        !! (moved from flowvars) Lateral inflow entering lateral inflow entering a segment
      real(r64), allocatable :: seg_sroff(:)
      real(r64), allocatable :: seg_ssflow(:)
      real(r64), allocatable :: seginc_gwflow(:)
      real(r64), allocatable :: seginc_potet(:)
      real(r64), allocatable :: seginc_sroff(:)
      real(r64), allocatable :: seginc_ssflow(:)
      real(r64), allocatable :: seginc_swrad(:)
      real(r64), allocatable :: segment_delta_flow(:)

      ! Declared Parameters
      ! integer(i32), allocatable :: Segment_type(:), Tosegment(:), Hru_segment(:), Obsin_segment(:), Obsout_segment(:)
      ! real(r32), allocatable :: K_coef(:), X_coef(:)
    contains
      procedure, public :: run => run_Routing
      procedure, public :: cleanup => cleanup_Routing
  end type

  interface Routing
    !! Routing constructor
    module function constructor_Routing(ctl_data, param_data, model_basin, model_time) result(this)
      type(Routing) :: this
        !! Routing class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
    end function
  end interface

  interface
    module subroutine run_Routing(this, ctl_data, param_data, model_basin, &
                                  model_climate, groundwater, soil, runoff, model_time)
      use prms_constants, only: dp, NEARZERO
      implicit none

      class(Routing) :: this
        !! Routing class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Gwflow), intent(in) :: groundwater
        !! Groundwater variables
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Routing(this)
      class(Routing) :: this
        !! Routing class
    end subroutine
  end interface
end module
