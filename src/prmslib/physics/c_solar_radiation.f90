module SOLAR_RADIATION
  use variableKind
  use prms_constants, only: sp, dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use prms_constants, only: dp
  implicit none

  private
  public :: SolarRadiation

  character(len=*), parameter :: MODDESC = 'Potential Solar Radiation'
  character(len=*), parameter :: MODNAME = 'soltab'
  character(len=*), parameter :: MODVERSION = '2018-06-29 20:10:00Z'

  integer(r32), parameter :: DAYS_PER_YEAR = 366.0_sp
  real(r64), parameter :: PI = 3.1415926535898_dp
  real(r64), parameter :: RADIANS = PI / 180.0_dp   ! RADIANS ~ 0.017453292519943
  real(r64), parameter :: TWOPI = 2.0_dp * PI       ! TWOPI ~ 6.2831853071786
  real(r64), parameter :: PI_12 = 12.0_dp / PI      ! PI_12 ~ 3.8197186342055

  type SolarRadiation
    logical :: has_basin_obs_station
      !! When true has a main solar radiation station
    logical :: has_hru_obs_station
      !! When true has solar radiation stations available
    real(r32) :: radiation_cv_factor
      !! Conversion factor to Langleys for measured radiation. Defaults to 1.0, but can be overridden by parameter rad_conv

    real(r32), allocatable :: orad_hru(:)

    ! WARNING: tmax_f and tmin_f will be removed once ccov_slope and ccov_intcp
    !          are converted to match Celsius temp_unit.
    real(r32), allocatable :: tmax_f(:)
    real(r32), allocatable :: tmin_f(:)

    real(r64) :: solar_declination(366)
    real(r64) :: soltab_basinpotsw(366)
    real(r64), allocatable :: hru_cossl(:)
    real(r64), allocatable :: soltab_sunhrs(:, :)

    ! Declared Variables
    real(r32) :: orad

    real(r64) :: basin_horad
    real(r64) :: basin_orad
    real(r64) :: basin_potsw
    real(r64) :: basin_swrad

    ! real(r32), allocatable :: orad_hru(:)
    real(r32), allocatable :: swrad(:)

    real(r64), allocatable :: soltab_potsw(:, :)
    real(r64), allocatable :: soltab_horad_potsw(:, :)

    contains
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
      procedure, nopass, private :: compute_soltab
      procedure, nopass, private :: compute_t
      procedure, nopass, private :: func3
  end type

  interface SolarRadiation
    !! SolarRadiation constructor
    module function constructor_SolarRadiation(ctl_data, param_data, model_basin) result(this)
      type(SolarRadiation) :: this
        !! SolarRadiation class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Model basin
    end function
  end interface

  interface
    module subroutine compute_soltab(Cossl, Soltab_daily, Sunhrs_daily, Obliquity, &
                              Solar_declination, Slope, Aspect, Latitude, &
                              Hru_type, Id)
      real(r64), intent(out) :: Cossl
      real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Soltab_daily
      real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Sunhrs_daily

      real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Obliquity
      real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Solar_declination
      real(r32), intent(in) :: Slope
      real(r32), intent(in) :: Aspect
      real(r32), intent(in) :: Latitude
      integer(i32), intent(in) :: Hru_type
      integer(i32), intent(in) :: Id
    end subroutine
  end interface

  interface
    pure elemental module function compute_t(Lat, Solar_declination) result(T)
      real(r64) :: T
        !! Angle hour from the local meridian (local solar noon) to the sunrise(negative) or sunset(positive).
      real(r64), intent(in) :: Lat
        !! Latitude
      real(r64), intent(in) :: Solar_declination
        !! Declination of the sun on a day.
    end function
  end interface

  interface
    pure elemental module function func3(V, W, X, Y, R1, Solar_declination) result(res)
      real(r64) :: res
      real(r64), intent(in) :: V
        !! Latitude angle hour offset between actual and equivalent slope
      real(r64), intent(in) :: W
        !! Latitude of the equivalent slope
      real(r64), intent(in) :: X
        !! Hour angle of sunset on equivalent slope
      real(r64), intent(in) :: Y
        !! Hour angle of sunrise on equivalent slope
      real(r64), intent(in) :: R1
        !! Solar constant for 60 minutes
      real(r64), intent(in) :: Solar_declination
        !! Declination of sun
    end function
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
