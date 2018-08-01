!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
module PRMS_CLIMATEVARS
  use variableKind
  use prms_constants, only: dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  implicit none

  private
  public :: Climateflow

  character(len=*), parameter :: MODDESC = 'Common States and Fluxes'
  character(len=*), parameter :: MODNAME = 'climateflow'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:47:00Z'

  type Climateflow
    ! Variables related to flows from soilzone, smbal, ssflow, srunoff_carea, srunoff_smidx
    ! WARNING: soil_moist, soil_rechr, soil_rechr_max are depended on
    !          by BOTH Srunoff and Soilzone.
    !          soil_moist and soil_rechr supply antecedent conditions to Srunoff.
    real(r32), allocatable :: soil_moist(:)
      !! Storage of capillary reservoir for each HRU
    real(r32), allocatable :: soil_rechr(:)
      !! Storage for recharge zone (upper portion) of the capillary reservoir that is available for both evaporation and transpiration
    real(r32), allocatable :: soil_rechr_max(:)
      !! Maximum storage for soil recharge zone (upper portion of capillary reservoir where losses occur as both evporation and transpiration)

    ! lakes variables
    real(r64) :: basin_lake_stor
      !! Modified by soilzone and muskingum_lake

    ! snow variables
    real(r64), allocatable :: pkwater_equiv(:)
      !! Snowpack water equivalent on each HRU [inches]

    ! TODO: what are these two used for?
    ! real(r32) :: solrad_tmax
    ! real(r32) :: solrad_tmin

    ! NOTE: Why both units? When are these variables needed
    ! real(r32), allocatable :: psta_elev_feet(:)
    ! real(r32), allocatable :: psta_elev_meters(:)
    ! real(r32), allocatable :: tsta_elev_feet(:)
    ! real(r32), allocatable :: tsta_elev_meters(:)

    ! NOTE: These are just extra copies of parameters tmax_adj and tmin_adj
    ! real(r32), allocatable :: tmax_aspect_adjust(:, :)
    ! real(r32), allocatable :: tmin_aspect_adjust(:, :)



    contains
      procedure, public :: cleanup => cleanup_Climateflow
        !! Final code to execute after simulation
      ! procedure, public :: set_precipitation_form
      ! procedure, public :: set_temperature
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type

  interface Climateflow
    !! Climateflow constructor
    module function constructor_Climateflow(ctl_data, param_data) result(this)
      type(Climateflow) :: this
        !! Climateflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine cleanup_Climateflow(this, ctl_data)
      class(Climateflow), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

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
