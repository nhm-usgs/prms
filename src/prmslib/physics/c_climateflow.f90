!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
module PRMS_CLIMATEVARS
  use variableKind
  use prms_constants, only: dp
  use prms_constants, only: FAHRENHEIT, CELSIUS, INCHES, MM, FEET, METERS, &
                            FEET2METERS, METERS2FEET
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
    real(r32) :: solrad_tmax
    real(r32) :: solrad_tmin

    ! Basin variables
    real(r64) :: basin_cloud_cover
    real(r64) :: basin_horad
    real(r64) :: basin_humidity
    real(r64) :: basin_obs_ppt
    real(r64) :: basin_orad
      !! used when solrad_module = [ddsolrad, ccsolrad] or model==99
    real(r64) :: basin_potet
    real(r64) :: basin_potsw
    real(r64) :: basin_ppt
    real(r64) :: basin_rain
    real(r64) :: basin_snow
    real(r64) :: basin_swrad
    real(r64) :: basin_temp
    real(r64) :: basin_tmax
    real(r64) :: basin_tmin

    integer(i32) :: basin_transp_on

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
    real(r32), allocatable :: tmaxc(:)
    real(r32), allocatable :: tmaxf(:)
    real(r32), allocatable :: tminc(:)
    real(r32), allocatable :: tminf(:)

    real(r32), allocatable :: tmax_hru(:)
      !! maximum temperature by hru in temp_units
    real(r32), allocatable :: tmin_hru(:)
      !! minimum temperature by hru in temp_units

    real(r32), allocatable :: tmax_allrain(:, :)
    real(r32), allocatable :: tmax_allrain_f(:, :)
    real(r32), allocatable :: tmax_allsnow_c(:, :)
    real(r32), allocatable :: tmax_allsnow_f(:, :)

    ! NOTE: Why both units? When are these variables needed
    real(r32), allocatable :: psta_elev_feet(:)
    real(r32), allocatable :: psta_elev_meters(:)
    real(r32), allocatable :: tsta_elev_feet(:)
    real(r32), allocatable :: tsta_elev_meters(:)

    ! NOTE: These are just extra copies of parameters tmax_adj and tmin_adj
    ! real(r32), allocatable :: tmax_aspect_adjust(:, :)
    ! real(r32), allocatable :: tmin_aspect_adjust(:, :)

    integer(i32), allocatable :: newsnow(:)
    integer(i32), allocatable :: pptmix(:)
    integer(i32), allocatable :: transp_on(:)

    ! snow variables
    real(r64), allocatable :: pkwater_equiv(:)
      !! Snowpack water equivalent on each HRU [inches]

    ! For potet_pt, potet_pm, potet_pm_sta
    real(r32), allocatable :: tempc_dewpt(:)
    real(r32), allocatable :: vp_actual(:)
    real(r32), allocatable :: lwrad_net(:)
    real(r32), allocatable :: vp_slope(:)
    real(r32), allocatable :: vp_sat(:)

    ! For stream temperature
    real(r32), allocatable :: cloud_cover_hru(:)

    ! For solar radiation
    real(r32) :: orad

    real(r32), allocatable, private :: tdiff_arr(:)
      !! Array containing differences b/t tmaxf and tminf

    contains
      procedure, public :: cleanup => cleanup_Climateflow
        !! Final code to execute after simulation
      procedure, public :: set_precipitation_form
      procedure, public :: set_temperature
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
    module subroutine set_precipitation_form(this, ctl_data, param_data, model_basin, &
                                             month, rain_adj, snow_adj, rainmix_adj)
      class(Climateflow), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      integer(i32), intent(in) :: month
      real(r32), optional, intent(in) :: rain_adj(:)
        !! Array of rain adjustments
      real(r32), optional, intent(in) :: snow_adj(:)
        !! Array of snow adjustments
      real(r32), optional, intent(in) :: rainmix_adj(:)
        !! Array of rain mixture adjustments
    end subroutine
  end interface


  interface
    module subroutine set_temperature(this, ctl_data, param_data, model_basin, tmin_adj, tmax_adj)
      class(Climateflow), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      real(r32), optional, intent(in) :: tmin_adj(:)
        !! Array of minimum temperature adjustments
      real(r32), optional, intent(in) :: tmax_adj(:)
        !! Array of maximum temperature adjustments
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
