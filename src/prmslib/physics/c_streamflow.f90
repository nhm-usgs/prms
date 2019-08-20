module PRMS_STREAMFLOW

  use variableKind
  use iso_fortran_env, only: output_unit
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_GWFLOW, only: Gwflow
  use PRMS_OBS, only: Obs
  use PRMS_POTET, only: Potential_ET
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_SRUNOFF, only: Srunoff
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_SUMMARY, only: Summary

  implicit none

  private
  public :: Streamflow

  character(len=*), parameter :: MODDESC = 'Streamflow initialization'
  character(len=*), parameter :: MODNAME = 'streamflow'
  character(len=*), parameter :: MODVERSION = '2018-10-10 18:09:00Z'

  type, extends(ModelBase) :: Streamflow
      ! Parameters
      integer(i32), allocatable :: hru_segment(:)
        !! Segment index to which an HRU contributes lateral flows (surface runoff, interflow, and groundwater discharge)
      integer(i32), allocatable :: obsin_segment(:)
        !! Index of measured streamflow station that replaces inflow to a segment
      integer(i32), allocatable :: obsout_segment(:)
        !! Index of measured streamflow station that replaces outflow from a segment
      real(r32), allocatable :: segment_flow_init(:)
        !! Initial flow in each stream segment
      integer(i32), allocatable :: segment_type(:)
        !! Segment type (0=segment; 1= headwater; 2=lake; 3=replace inflow; 4=inbound to NHM; 5=outbound from NHM; 6=inbound to region; 7=outbound from region; 8=drains to ocean; 9=sink; 10=inbound from Great Lakes; 11=outbound to Great Lakes)
      integer(i32), allocatable :: tosegment(:)
        !! Index of downstream segment to which the segment streamflow flows; for segments that do not flow to another segment enter 0


      ! Local Variables
      integer(i32), private :: noarea_flag
      real(r64), private :: segment_area

      real(r64) :: flow_out
        !! Total flow out of model domain
      real(r64), allocatable :: hru_outflow(:)
      real(r64), allocatable :: seg_gwflow(:)
      real(r64), allocatable :: seg_sroff(:)
      real(r64), allocatable :: seg_ssflow(:)
      real(r64), private, allocatable :: seginc_potet(:)
      real(r64), private, allocatable :: segment_hruarea(:)

      ! Declared Variables
      integer(i32), public :: use_transfer_segment
        !! used by muskingum and muskingum_lake

      ! real(r64), public :: cfs2acft
        !! used by muskingum_lake

      integer(i32), public, allocatable :: segment_order(:)
        !! used by muskingum, muskingum_lake, stream_temp, strmflow_in_out
      integer(i32), public, allocatable :: segment_up(:)
        !! used by stream_temp

      ! NOTE: Specific to muskingum, muskingum_lake, and strmflow_in_out.
      !       It is required by stream_temp. Used in parent streamflow class.
      real(r64), public, allocatable :: seg_lateral_inflow(:)
        !! Lateral inflow entering lateral inflow entering a segment
      real(r64), allocatable :: seg_inflow(:)
        !! Total flow entering a segment
      real(r64), allocatable :: seg_outflow(:)
        !! Streamflow leaving a segment
      real(r64), allocatable :: seg_upstream_inflow(:)
        !! Sum of inflow from upstream segments

      real(r64), allocatable :: basin_cfs
        !! Streamflow leaving the basin through the stream network (cfs)
      real(r64), allocatable :: basin_cms
        !! Streamflow leaving the basin through the stream network (cms)
      real(r64), public, allocatable :: basin_segment_storage
      real(r64), allocatable :: basin_gwflow_cfs
        !! Basin area-weighted average of groundwater flow to the stream network
      real(r64), allocatable :: basin_sroff_cfs
        !! Basin area-weighted average surface runoff to the stream network
      real(r64), allocatable :: basin_ssflow_cfs
        !! Interflow leaving the basin through the stream network
      real(r64), allocatable :: basin_stflow_in
        !! Basin area-weighted average lateral flow entering the stream network
      real(r64), allocatable :: basin_stflow_out
        !! Basin area-weighted average streamflow leaving through the stream network
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

      real(r64), public, allocatable :: seginc_gwflow(:)
        !! stream_temp
      real(r64), public, allocatable :: seginc_sroff(:)
        !! stream_temp
      real(r64), public, allocatable :: seginc_ssflow(:)
        !! stream_temp
      real(r64), public, allocatable :: seginc_swrad(:)
        !! stream_temp
      real(r64), public, allocatable :: segment_delta_flow(:)
        !! muskingum, muskingum_lake

    contains
      procedure, public :: init => init_Streamflow
      procedure, public :: run => run_Streamflow
      procedure, public :: cleanup => cleanup_Streamflow
  end type

  interface
    !! Streamflow constructor
    module subroutine init_Streamflow(this, ctl_data, model_basin, model_time, model_summary)
      class(Streamflow), intent(inout) :: this
        !! Streamflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Streamflow(this, ctl_data, model_basin, &
                                    model_potet, groundwater, soil, runoff, &
                                    model_time, model_solrad, model_obs)
      class(Streamflow), intent(inout) :: this
        !! Streamflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Potential_ET), intent(in) :: model_potet
      type(Gwflow), intent(in) :: groundwater
        !! Groundwater variables
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad
      type(Obs), intent(in) :: model_obs
    end subroutine
    ! module subroutine run_Streamflow(this, ctl_data, model_basin, &
    !                                  model_potet, groundwater, soil, runoff, &
    !                                  model_time, model_solrad)
    !   use prms_constants, only: dp, NEARZERO
    !   implicit none

    !   class(Streamflow) :: this
    !     !! Streamflow class
    !   type(Control), intent(in) :: ctl_data
    !     !! Control file parameters
    !   type(Basin), intent(in) :: model_basin
    !     !! Basin variables
    !   class(Potential_ET), intent(in) :: model_potet
    !   type(Gwflow), intent(in) :: groundwater
    !     !! Groundwater variables
    !   type(Soilzone), intent(in) :: soil
    !   type(Srunoff), intent(in) :: runoff
    !   type(Time_t), intent(in) :: model_time
    !   class(SolarRadiation), intent(in) :: model_solrad
    ! end subroutine
  end interface

  interface
    module subroutine cleanup_Streamflow(this)
      class(Streamflow), intent(inout) :: this
        !! Streamflow class
    end subroutine
  end interface
end module
