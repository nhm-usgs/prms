!***********************************************************************
! Compute potential solar radiation and sunlight hours for each HRU for
! each day of year; modification of soltab_prms
!
! References -- you *will* need these to figure out what is going on:
!   Swift, L.W., Jr., 1976, Algorithm for solar radiation on mountain
!   slopes: Water Resources Research, v. 12, no. 1, p. 108-112.
!
!   Lee, R., 1963, Evaluation of solar beam irradiation as a climatic parameter
!   of mountain watersheds, Colorado State University Hydrology Papers, 2,
!   50 pp.
!***********************************************************************
module PRMS_SOLTAB
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  implicit none

  private
  public :: Soltab

  character(len=*), parameter :: MODDESC = 'Potential Solar Radiation'
  character(len=*), parameter :: MODNAME = 'soltab'
  character(len=*), parameter :: MODVERSION = '2016-09-29 13:48:00Z'

  integer(r32), parameter :: DAYS_PER_YEAR = 366
  real(r64), parameter :: PI = 3.1415926535898D0
  real(r64), parameter :: RADIANS = PI / 180.0D0   ! RADIANS ~ 0.017453292519943
  real(r64), parameter :: TWOPI = 2.0D0 * PI       ! TWOPI ~ 6.2831853071786
  real(r64), parameter :: PI_12 = 12.0D0 / PI      ! PI_12 ~ 3.8197186342055

  type Soltab
    real(r64) :: solar_declination(366)
    real(r64) :: soltab_basinpotsw(366)
    real(r64), allocatable :: hru_cossl(:)
    real(r64), allocatable :: soltab_sunhrs(:, :)

    ! Declared Variables
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

  interface Soltab
    !! Soltab constructor
    module function constructor_Soltab(ctl_data, param_data, model_basin) result(this)
      ! use Control_class, only: Control
      ! use Parameters_class, only: Parameters
      ! use PRMS_BASIN, only: Basin

      type(Soltab) :: this
        !! Soltab class
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
      real(r64), intent(out), dimension(DAYS_PER_YEAR) :: Soltab_daily
      real(r64), intent(out), dimension(DAYS_PER_YEAR) :: Sunhrs_daily

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
    module subroutine compute_t(Lat, Solar_declination, T)
      real(r64), intent(in) :: Lat
        !! Latitude
      real(r64), intent(in) :: Solar_declination
        !! Declination of the sun on a day.
      real(r64), intent(out) :: T
        !! Angle hour from the local meridian (local solar noon) to the sunrise(negative) or sunset(positive).
    end subroutine
  end interface

  interface
    module function func3(V, W, X, Y, R1, Solar_declination) result(res)
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
