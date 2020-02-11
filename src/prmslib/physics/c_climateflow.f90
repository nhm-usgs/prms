!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
module PRMS_CLIMATEVARS
  use variableKind
  use prms_constants, only: dp
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Climateflow

  character(len=*), parameter :: MODDESC = 'Common States and Fluxes'
  character(len=*), parameter :: MODNAME = 'climateflow'
  character(len=*), parameter :: MODVERSION = '2018-08-30 13:40:00Z'

  type, extends(ModelBase) :: Climateflow
    ! Parameters
    ! These are parameters that have conflicts that prevent them from being
    ! placed in their correct module.
    real(r32), pointer :: soil_moist_init_frac(:)
      !! Initial fraction of available water in the capillary reservoir (fraction of soil_moist_max for each HRU
    real(r32), pointer :: soil_moist_max(:)
      !! Maximum available water holding capacity of capillary reservoir from land surface to rooting depth of the major vegetation type of each HRU
    real(r32), pointer :: soil_rechr_init_frac(:)
      !! Initial fraction of available water in the capillary reservoir where losses occur as both evaporation and transpiration (upper zone of capillary reservoir) for each HRU
    real(r32), pointer :: soil_rechr_max_frac(:)
      !! Fraction of the capillary reservoir water-holding capacity (soil_moist_max) where losses occur as both evaporation and transpiration (upper zone of capillary reservoir) for each HRU


    ! Other variables

    ! Variables related to flows from soilzone, smbal, ssflow, srunoff_carea, srunoff_smidx
    ! WARNING: soil_moist, soil_rechr, soil_rechr_max are depended on
    !          by BOTH Srunoff and Soilzone.
    !          soil_moist and soil_rechr supply antecedent conditions to Srunoff.
    real(r32), pointer :: soil_moist(:)
      !! Storage of capillary reservoir for each HRU [inches]
    real(r32), pointer :: soil_rechr(:)
      !! Storage for recharge zone (upper portion) of the capillary reservoir that is available for both evaporation and transpiration [inches]
    real(r32), pointer :: soil_rechr_max(:)
      !! Maximum storage for soil recharge zone (upper portion of capillary reservoir where losses occur as both evporation and transpiration) [inches]

    ! snow variables
    real(r64), pointer :: pkwater_equiv(:)
      !! Snowpack water equivalent on each HRU [inches]

    ! TODO: what are these two used for?
    ! real(r32) :: solrad_tmax
    ! real(r32) :: solrad_tmin

    ! NOTE: Why both units? When are these variables needed?
    ! real(r32), allocatable :: psta_elev_feet(:)
    ! real(r32), allocatable :: psta_elev_meters(:)
    ! real(r32), allocatable :: tsta_elev_feet(:)
    ! real(r32), allocatable :: tsta_elev_meters(:)

    ! NOTE: These are just extra copies of parameters tmax_adj and tmin_adj
    ! real(r32), allocatable :: tmax_aspect_adjust(:, :)
    ! real(r32), allocatable :: tmin_aspect_adjust(:, :)

    contains
      procedure, public :: init => init_Climateflow
      procedure, public :: cleanup => cleanup_Climateflow
        !! Final code to execute after simulation
  end type

  interface
    !! Climateflow constructor
    module subroutine init_Climateflow(this, ctl_data, model_basin, model_summary)
      class(Climateflow), target, intent(inout) :: this
        !! Climateflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine cleanup_Climateflow(this, ctl_data)
      class(Climateflow), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

end module
