module SOLAR_RADIATION
  use variableKind
  use prms_constants, only: sp, dp
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use prms_constants, only: dp
  use PRMS_SUMMARY, only: Summary
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_SET_TIME, only: Time_t
  use PRMS_TEMPERATURE, only: Temperature
  implicit none

  private
  public :: SolarRadiation

  character(len=*), parameter :: MODDESC = 'Potential Solar Radiation'
  character(len=*), parameter :: MODNAME = 'soltab'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:20:00Z'

  integer(i32), parameter :: DAYS_PER_YEAR = 366
  real(r64), parameter :: PI = 3.1415926535898_dp
  real(r64), parameter :: RADIANS = PI / 180.0_dp   ! RADIANS ~ 0.017453292519943
  real(r64), parameter :: TWOPI = 2.0_dp * PI       ! TWOPI ~ 6.2831853071786
  real(r64), parameter :: PI_12 = 12.0_dp / PI      ! PI_12 ~ 3.8197186342055

  real(r64), parameter :: ECCENTRICY = 0.01671_dp
  real(r64), parameter :: DAYSYR = 365.242_dp
  real(r64), parameter :: DEGDAY = 360.0_dp / DAYSYR
  real(r64), parameter :: DEGDAYRAD = DEGDAY * RADIANS ! about 0.00143356672

  integer(i32) :: i
  real(r64), parameter :: JD(DAYS_PER_YEAR) = [(dble(i), i=1,DAYS_PER_YEAR)]
  real(r64), parameter :: Y(DAYS_PER_YEAR) = (JD - 1.0_dp) * DEGDAYRAD
  real(r64), parameter :: JD3(DAYS_PER_YEAR) = (JD - 3.0_dp) * DEGDAYRAD
  real(r64), parameter :: Y2(DAYS_PER_YEAR) = Y * 2.0_dp
  real(r64), parameter :: Y3(DAYS_PER_YEAR) = Y * 3.0_dp
  real(r64), parameter :: OBLIQUITY(DAYS_PER_YEAR) = 1.0_dp - (ECCENTRICY * cos(JD3))
  real(r64), parameter :: SOLAR_DECLINATION(DAYS_PER_YEAR) = 0.006918_dp - &
                                                   0.399912_dp * COS(Y) + 0.070257_dp * SIN(Y) - &
                                                   0.006758_dp * COS(Y2) + 0.000907_dp * SIN(Y2) - &
                                                   0.002697_dp * COS(Y3) + 0.00148_dp * SIN(Y3)
  real(r64), parameter :: R0 = 2.0_dp
    !! solar constant cal/cm2/min (r0 could also be 1.95 (Drummond, et al 1968))
  real(r64), parameter :: R1(DAYS_PER_YEAR) = (60.0_dp * R0) / (OBLIQUITY**2)
    !! Solar constant for 60 minutes

  type, extends(ModelBase) :: SolarRadiation
    ! Dimensions
    integer(i32) :: nsol = 0
      !! Number of solar radiation stations

    ! Parameters
    integer(i32) :: basin_solsta
    integer(i32) :: rad_conv
      !! Conversion factor to langleys for measured solar radiation
    integer(i32), pointer :: hru_solsta(:)
    real(r32), pointer :: ppt_rad_adj(:, :)
      !! Monthly minimum precipitation, if HRU precipitation exceeds this value, radiation is multiplied by radj_sppt or radj_wppt precipitation adjustment factor
    real(r32), pointer :: radj_sppt(:)
    real(r32), pointer :: radj_wppt(:)
    real(r32), pointer :: radmax(:, :)
      !! Monthly (January to December maximum fraction of the potential solar radiation that may reach the ground due to haze, dust, smog, and so forth, for each HRU

    ! Time-series input variables
    real(r32), pointer :: solrad(:)
      !! Solar radiation at each measurement station

    ! Output variables
    real(r32), pointer :: orad_hru(:)
    real(r32), pointer :: swrad(:)

    ! Other variables
    logical :: has_basin_obs_station
      !! When true has a main solar radiation station
    logical :: has_hru_obs_station
      !! When true has solar radiation stations available
    real(r32) :: radiation_cv_factor
      !! Conversion factor to Langleys for measured radiation. Defaults to 1.0, but can be overridden by parameter rad_conv

    ! WARNING: tmax_f and tmin_f will be removed once ccov_slope and ccov_intcp
    !          are converted to match Celsius temp_unit.
    real(r32), pointer :: tmax_f(:)
    real(r32), pointer :: tmin_f(:)

    real(r64) :: soltab_basinpotsw(DAYS_PER_YEAR)
    real(r64), pointer :: hru_cossl(:)
    real(r64), pointer :: soltab_sunhrs(:, :)

    ! Declared Variables
    ! real(r32), allocatable :: orad_hru(:)
    real(r64), pointer :: soltab_potsw(:, :)
    real(r64), pointer :: soltab_horad_potsw(:, :)

    contains
      procedure, public :: init => init_SolarRadiation
      procedure, public :: run => run_SolarRadiation
      procedure, nopass, private :: compute_soltab
      procedure, nopass, private :: compute_t
      procedure, nopass, private :: func3
  end type

  interface
    !! SolarRadiation constructor
    module subroutine init_SolarRadiation(this, ctl_data, model_basin, model_summary)
      class(SolarRadiation), intent(inout) :: this
        !! SolarRadiation class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Model basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_SolarRadiation(this, ctl_data, model_time, model_precip, model_basin, model_temp)
      class(SolarRadiation), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      class(Precipitation), intent(in) :: model_precip
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
    end subroutine
  end interface

  interface
    module subroutine compute_soltab(Cossl, Soltab_daily, Sunhrs_daily, &
                              Solar_declination, Slope, Aspect, Latitude)
      real(r64), intent(out) :: Cossl
      real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Soltab_daily
      real(r64), intent(inout), dimension(DAYS_PER_YEAR) :: Sunhrs_daily

      ! real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Obliquity
      real(r64), intent(in), dimension(DAYS_PER_YEAR) :: Solar_declination
      real(r32), intent(in) :: Slope
      real(r32), intent(in) :: Aspect
      real(r32), intent(in) :: Latitude
      ! integer(i32), intent(in) :: Hru_type
      ! integer(i32), intent(in) :: Id
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

end module
