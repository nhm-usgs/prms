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
      integer(i32), private :: noarea_flag
      integer(i32), public :: use_transfer_segment
        !! used by muskingum and muskingum_lake

      real(r64), public :: cfs2acft
        !! used by muskingum_lake
      real(r64), private :: segment_area


      integer(i32), public, allocatable :: segment_order(:)
        !! used by muskingum, muskingum_lake, stream_temp, strmflow_in_out
      integer(i32), public, allocatable :: segment_up(:)
        !! used by stream_temp
      integer(i32), public, allocatable :: ts_i(:)
        !! used by muskingum and muskingum_lake

      real(r32), public, allocatable :: c0(:)
        !! used by muskingum and muskingum_lake
      real(r32), public, allocatable :: c1(:)
        !! used by muskingum and muskingum_lake
      real(r32), public, allocatable :: c2(:)
        !! used by muskingum and muskingum_lake
      real(r32), allocatable :: ts(:)

      real(r64), private, allocatable :: segment_hruarea(:)

      ! Declared Variables
      real(r64), public :: basin_segment_storage
        !! basin_sum, muskingum, muskingum_lake
      real(r64), public :: flow_headwater
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_in_great_lakes
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_in_nation
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_in_region
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_out_NHM
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_out_region
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_replacement
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_terminus
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_to_great_lakes
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_to_lakes
        !! muskingum, muskingum_lake, strmflow_in_out
      real(r64), public :: flow_to_ocean
        !! muskingum, muskingum_lake, strmflow_in_out

      real(r64), private, allocatable :: hru_outflow(:)
      real(r64), private, allocatable :: seg_gwflow(:)
      real(r64), public, allocatable :: seg_lateral_inflow(:)
        !! (moved from flowvars) Lateral inflow entering lateral inflow entering a segment
        !! muskingum, muskingum_lake, strmflow_in_out, stream_temp
      real(r64), private, allocatable :: seg_sroff(:)
      real(r64), private, allocatable :: seg_ssflow(:)
      real(r64), public, allocatable :: seginc_gwflow(:)
        !! stream_temp
      real(r64), private, allocatable :: seginc_potet(:)
      real(r64), public, allocatable :: seginc_sroff(:)
        !! stream_temp
      real(r64), public, allocatable :: seginc_ssflow(:)
        !! stream_temp
      real(r64), public, allocatable :: seginc_swrad(:)
        !! stream_temp
      real(r64), public, allocatable :: segment_delta_flow(:)
        !! muskingum, muskingum_lake

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
