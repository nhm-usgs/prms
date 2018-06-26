module PRMS_FLOWVARS
  ! Parameters and variables related to flows from soilzone, smbal, ssflow,
  ! srunoff_carea, srunoff_smidx
  use iso_fortran_env, only: output_unit
  use variableKind
  use prms_constants, only: dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  implicit none

  private
  public :: Flowvars

  character(len=*), parameter :: MODDESC = 'Common Flows'
  character(len=*), parameter :: MODNAME = 'flowvars'
  character(len=*), parameter :: MODVERSION = '2018-05-02 07:30:00Z'

  ! Variables related to flows from soilzone, smbal, ssflow, srunoff_carea, srunoff_smidx
  type Flowvars
    ! soilzone variables
    real(r32), allocatable :: hru_actet(:)
      !! Actual ET for each HRU
    real(r32), allocatable :: slow_flow(:)
      !! Interflow from gravity reservoir storage that flows to the stream network for each HRU
    real(r32), allocatable :: soil_moist(:)
      !! Storage of capillary reservoir for each HRU
    real(r32), allocatable :: soil_rechr(:)
      !! Storage for recharge zone (upper portion) of the capillary reservoir that is available for both evaporation and transpiration
    real(r32), allocatable :: soil_rechr_max(:)
      !! Maximum storage for soil recharge zone (upper portion of capillary reservoir where losses occur as both evporation and transpiration)
    real(r32), allocatable :: soil_to_gw(:)
      !! Portion of excess flow to the capillary reservoir that drains to the associated GWR for each HRU
    real(r32), allocatable :: soil_to_ssr(:)
      !! Portion of excess flow to the capillary reservoir that flows to the gravity reservoir for each HRU
    real(r32), allocatable :: ssr_to_gw(:)
      !! Drainage from the gravity-reservoir to the associated GWR for each HRU
    real(r32), allocatable :: ssres_in(:)
      !! Inflow to the gravity and preferential-flow reservoirs for each HRU

    ! lakes variables
    real(r64) :: basin_lake_stor

    ! streamflow variables
    real(r64) :: basin_cfs
      !! Streamflow leaving the basin through the stream network (cfs)
    real(r64) :: basin_cms
      !! Streamflow leaving the basin through the stream network (cms)
    real(r64) :: basin_gwflow_cfs
      !! Basin area-weighted average of groundwater flow to the stream network
    real(r64) :: basin_sroff_cfs
      !! Basin area-weighted average surface runoff to the stream network
    real(r64) :: basin_ssflow_cfs
      !! Interflow leaving the basin through the stream network
    real(r64) :: basin_stflow_in
      !! Basin area-weighted average lateral flow entering the stream network
    real(r64) :: basin_stflow_out
      !! Basin area-weighted average streamflow leaving through the stream network
    real(r64) :: flow_out
      !! Total flow out of model domain
    real(r64), allocatable :: seg_inflow(:)
      !! Total flow entering a segment
    real(r64), allocatable :: seg_outflow(:)
      !! Streamflow leaving a segment
    real(r64), allocatable :: seg_upstream_inflow(:)
      !! Sum of inflow from upstream segments

    contains
      ! procedure, public :: cleanup => cleanup_Flowvars
        !! Final code to execute after simulation
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type

  interface Flowvars
    !! Flowvars constructor
    module function constructor_Flowvars(ctl_data, param_data) result(this)
      type(Flowvars) :: this
        !! Flowvars class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  ! interface
  !   module subroutine cleanup_Flowvars(this, ctl_data)
  !     class(Flowvars), intent(in) :: this
  !     type(Control), intent(in) :: ctl_data
  !   end subroutine
  ! end interface

  interface
    module function module_name() result(res)
      character(:), allocatable :: res
    end function
  end interface

  interface
    module function version() result(res)
      character(:), allocatable :: res
    end function
  end interface

end module
