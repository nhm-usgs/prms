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
