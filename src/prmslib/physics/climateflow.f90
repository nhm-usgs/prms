!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
module PRMS_CLIMATEVARS
  use variableKind
  implicit none

  private
  public :: Climateflow

  character(len=*), parameter :: MODDESC = 'Common States and Fluxes'
  character(len=*), parameter :: MODNAME = 'climateflow'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:47:00Z'

  type Climateflow
    real(r32) :: solrad_tmax(1)
    real(r32) :: solrad_tmin(1)

    real(r64) :: basin_horad(1)
    real(r64) :: basin_obs_ppt(1) = 0.0
    real(r64) :: basin_orad(1) = 0.0
      !! used when solrad_module = [ddsolrad, ccsolrad] or model==99
    real(r64) :: basin_potet(1) = 0.0
    real(r64) :: basin_ppt(1) = 0.0
    real(r64) :: basin_rain(1) = 0.0
    real(r64) :: basin_snow(1) = 0.0
    real(r64) :: basin_swrad(1) = 0.0
    real(r64) :: basin_temp(1) = 0.0
    real(r64) :: basin_tmax(1) = 0.0
    real(r64) :: basin_tmin(1) = 0.0

    integer(i32) :: basin_solsta = 0
    integer(i32) :: basin_transp_on = 0

    real(r32), allocatable :: hru_ppt(:)
    real(r32), allocatable :: hru_rain(:)
    real(r32), allocatable :: hru_snow(:)
    real(r32), allocatable :: orad_hru(:)
      !! used when solrad_module = [ddsolrad, ccsolrad] or model==99
    real(r32), allocatable :: potet(:)
    real(r32), allocatable :: prmx(:)
    real(r32), allocatable :: swrad(:)
    real(r32), allocatable :: tavgc(:)
    real(r32), allocatable :: tavgf(:)
    real(r32), allocatable :: tmax_hru(:)
    real(r32), allocatable :: tmaxc(:)
    real(r32), allocatable :: tmaxf(:)
    real(r32), allocatable :: tmin_hru(:)
    real(r32), allocatable :: tminc(:)
    real(r32), allocatable :: tminf(:)
    real(r32), allocatable :: tmax_allrain(:, :)
    real(r32), allocatable :: tmax_allrain_f(:, :)
    real(r32), allocatable :: tmax_allsnow_c(:, :)
    real(r32), allocatable :: tmax_allsnow_f(:, :)
    real(r32), allocatable :: tmax_aspect_adjust(:, :)
    real(r32), allocatable :: tmin_aspect_adjust(:, :)

    integer(i32), allocatable :: newsnow(:)
    integer(i32), allocatable :: pptmix(:)
    integer(i32), allocatable :: transp_on(:)

    contains
      procedure, public :: cleanup
        !! Final code to execute after simulation
      procedure, public :: precip_form
      procedure, public :: temp_set

      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type


  interface Climateflow
    !! Climateflow constructor
    module function constructor_Climateflow(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters

      type(Climateflow) :: this
        !! Climateflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine cleanup(this, ctl_data)
      use Control_class, only: Control

      class(Climateflow), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    module subroutine precip_form(this, ihru, month, hru_area, adjmix_rain, &
                                  rain_adj, snow_adj, precip, sum_obs)
      class(Climateflow), intent(inout) :: this
      integer(i32), intent(in) :: ihru
      integer(i32), intent(in) :: month
      real(r32), intent(in) :: hru_area
      real(r32), intent(in) :: adjmix_rain
      real(r32), intent(in) :: rain_adj
      real(r32), intent(in) :: snow_adj
      real(r32), intent(inout) :: precip
      real(r64), intent(inout) :: sum_obs
    end subroutine
  end interface

  interface
    module subroutine temp_set(this, param_data, ihru, hru_area, tmax, tmin)
      use Parameters_class, only: Parameters

      class(Climateflow), intent(inout) :: this
      type(Parameters), intent(in) :: param_data
      integer(i32), intent(in) :: ihru
      real(r32), intent(in) :: hru_area
      real(r32), intent(in) :: tmax
      real(r32), intent(in) :: tmin
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
