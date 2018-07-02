module PRMS_SNOW
  use variableKind
  use prms_constants, only: dp, NEARZERO, DNEARZERO, INCH2CM
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_SET_TIME, only : Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_INTCP, only: Interception
  use SOLAR_RADIATION, only: SolarRadiation
  implicit none

  private
  public :: Snowcomp

  character(len=*), parameter :: MODDESC = 'Snow Dynamics'
  character(len=*), parameter :: MODNAME = 'snowcomp'
  character(len=*), parameter :: MODVERSION = '2018-02-23 16:04:00Z'

  integer(i32), parameter :: MAXALB = 15

  real(r32), parameter :: ACUM_INIT(MAXALB) = [0.80, 0.77, 0.75, 0.72, 0.70, 0.69, 0.68, &
                                       0.67, 0.66, 0.65, 0.64, 0.63, 0.62, 0.61, 0.60]
  real(r32), parameter :: AMLT_INIT(MAXALB) = [0.72, 0.65, 0.60, 0.58, 0.56, 0.54, 0.52, &
                                       0.50, 0.48, 0.46, 0.44, 0.43, 0.42, 0.41, 0.40]

  type Snowcomp
    integer(i32), allocatable :: int_alb(:)
      !! Flag to indicate: 1) accumlation season curve; 2) use of the melt season curve [flag]
    real(r64) :: deninv
    real(r64) :: denmaxinv
    real(r64) :: settle_const_dble
    !     real(r32), SAVE :: Setden, Set1

    real(r32) :: acum(MAXALB)
    real(r32) :: amlt(MAXALB)
    real(r32), allocatable :: snowcov_areasv(:)
      !! Snow cover fraction when there is new snow and in melt phase [fraction]

    real(r64), allocatable :: scrv(:)
      !! Snowpack water equivalent plus a portion of new snow on each HRU
    real(r64), allocatable :: pss(:)
      !! Previous snowpack water equivalent plus new snow [inches]
    real(r64), allocatable :: pksv(:)
      !! Snowpack water equivalent when there is new snow and in melt phase [inches]
    real(r64), allocatable :: pst(:)
      !! While a snowpack exists, pst tracks the maximum snow water equivalent of that snowpack [inches]

    real(r32), allocatable :: salb(:)
      !! Days since last new snow to reset albedo for each HRU
    real(r32), allocatable :: slst(:)
      !! Days since last new snow for each HRU [days]

    !****************************************************************
    !   Declared Variables
    integer(i32), allocatable :: pptmix_nopack(:)
      !! Flag indicating that a mixed precipitation event has occurred with no snowpack present on an HRU [flag]
    integer(i32), allocatable :: lst(:)
      !! Flag indicating whether there was new snow that was insufficient to reset the albedo curve (1; albset_snm or albset_sna), otherwise (0) [flag]
    integer(i32), allocatable :: iasw(:)
      !! Flag indicating that snow covered area is interpolated between previous location on curve and maximum (1), or is on the defined curve
    integer(i32), allocatable :: iso(:)
      !! Flag to indicate if time is before (1) or after (2) the day to force melt season (melt_force)
    integer(i32), allocatable :: mso(:)
      !! Flag to indicate if time is before (1) or after (2) the first potnetial day for melt season (melt_look)
    integer(i32), allocatable :: lso(:)
      !! Counter for tracking the number of days the snowpack is at or above 0 degrees Celsius

    real(r64) :: basin_snowmelt
      !! Basin area-weighted average snowmelt
    real(r64) :: basin_pweqv
      !! Basin area-weighted average snowpack water equivalent
    real(r64) :: basin_tcal
      !! Basin area-weighted average net snowpack energy balance
    real(r64) :: basin_snowcov
      !! Basin area-weighted average snow-covered area
    real(r64) :: basin_snowevap
      !! Basin area-weighted average evaporation and sublimation from snowpack
    real(r64) :: basin_snowdepth
      !! Basin area-weighted average snow depth
    real(r64) :: basin_pk_precip
      !! Basin area-weighted average precipitation added to snowpack

    real(r32), allocatable :: snowmelt(:)
      !! Snowmelt from snowpack on each HRU [inches]
    real(r32), allocatable :: snow_evap(:)
      !! Evaporation and sublimation from snowpack on each HRU
    real(r32), allocatable :: albedo(:)
      !! Snow surface albedo or the fraction of radiation reflected from the snowpack surface for each HRU [fraction]
    real(r32), allocatable :: pk_temp(:)
      !! Temperature of the snowpack on each HRU [degree C]
    real(r32), allocatable :: pk_den(:)
      !! Density of the snowpack on each HRU [fraction of depth]
    real(r32), allocatable :: pk_def(:)
      !! Heat deficit, amount of heat necessary to make the snowpack isothermal at 0 degreees Celsius [cal/cm^2]
    ! real(r64), allocatable :: pkwater_equiv(:)
      !! Snowpack water equivalent on each HRU
    real(r32), allocatable :: pk_ice(:)
      !! Storage of frozen water in the snowpack on each HRU [inches]
    real(r32), allocatable :: freeh2o(:)
      !! Storage of free liquid water in the snowpack on each HRU [inches]
    real(r32), allocatable :: snowcov_area(:)
      !! Snow-covered area on each HRU prior to melt and sublimation unless snowpack is depleted [fraction]
    real(r32), allocatable :: tcal(:)
      !! Net snowpack energy balance on each HRU
    real(r32), allocatable :: snsv(:)
      !! Tracks the cumulative amount of new snow until there is enough to reset the albedo curve (albset_snm or albset_sna) [inches]
    real(r32), allocatable :: pk_precip(:)
      !! Precipitation added to snowpack for each HRU [inches]
    real(r32), allocatable :: frac_swe(:)
      !! Fraction of maximum snow-water equivalent (snarea_thresh) on each HRU
    real(r64), allocatable :: pk_depth(:)
      !! Depth of snowpack on each HRU [inches]
    real(r64), allocatable :: pkwater_ante(:)
      !! Antecedent snowpack water equivalent on each HRU
    real(r64), allocatable :: ai(:)
      !! Maximum snowpack for each HRU

    contains
      procedure, public :: run => run_Snowcomp
      ! procedure, nopass, public :: module_name
        !! Return the name of the module
      ! procedure, nopass, public :: version
        !! Return the version of the module
      procedure, private :: calin
      procedure, private :: caloss
      procedure, private :: ppt_to_pack
      procedure, nopass, private :: sca_deplcrv
      procedure, private :: snalbedo
      procedure, private :: snowbal
      procedure, private :: snowcov
      procedure, private :: snowevap
  end type


  interface Snowcomp
    !! Snowcomp constructor
    module function constructor_Snowcomp(model_climate, ctl_data, param_data, model_basin) result(this)
      type(Snowcomp) :: this
        !! Snowcomp class
      type(Climateflow), intent(inout) :: model_climate
        !! Climateflow
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Model basin
    end function
  end interface

  interface
    module subroutine run_Snowcomp(this, model_climate, ctl_data, param_data, model_time, model_basin, intcp, model_solrad)
        class(Snowcomp), intent(inout) :: this
          !! Snowcomp class
        type(Climateflow), intent(inout) :: model_climate
          !! Climate
        type(Control), intent(in) :: ctl_data
          !! Control file parameters
        type(Parameters), intent(in) :: param_data
          !! Parameters
        type(Time_t), intent(in) :: model_time
          !! Time
        type(Basin), intent(in) :: model_basin
          !! Basin
        type(Interception), intent(in) :: intcp
          !! Canopy interception
        class(SolarRadiation), intent(in) :: model_solrad
      end subroutine
  end interface

  interface
    module subroutine calin(this, model_climate, ctl_data, param_data, cal, chru)
      class(Snowcomp), intent(inout) :: this
      type(Climateflow), intent(inout) :: model_climate
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      real(r32), intent(in) :: cal
      integer(i32), intent(in) :: chru
    end subroutine
  end interface

  interface
    module subroutine caloss(this, model_climate, cal, chru)
      class(Snowcomp), intent(inout) :: this
      type(Climateflow), intent(inout) :: model_climate
      real(r32), intent(in) :: cal
      integer(i32), intent(in) :: chru
    end subroutine
  end interface

  interface
    module subroutine ppt_to_pack(this, model_climate, month, chru, ctl_data, param_data, intcp)
      class(Snowcomp), intent(inout) :: this
      type(Climateflow), intent(inout) :: model_climate
      integer(i32), intent(in) :: month
      integer(i32), intent(in) :: chru
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Interception), intent(in) :: intcp
    end subroutine
  end interface

  interface
    module pure function sca_deplcrv(snarea_curve, frac_swe) result(res)
      real(r32) :: res
        !! Snow covered area returned from function
      real(r32), intent(in) :: snarea_curve(11)
      real(r32), intent(in) :: frac_swe
    end function
  end interface

  interface
    module subroutine snalbedo(this, param_data, model_climate, intcp, chru)
      class(Snowcomp), intent(inout) :: this
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      type(Interception), intent(in) :: intcp
      integer(i32), intent(in) :: chru
    end subroutine
  end interface

  interface
    module subroutine snowbal(this, cal, model_climate, ctl_data, param_data, intcp, &
                              chru, month, niteda, cec, cst, esv, sw, temp, trd)
      class(Snowcomp), intent(inout) :: this
      real(r32), intent(out) :: cal
      type(Climateflow), intent(inout) :: model_climate
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Interception), intent(in) :: intcp
      integer(i32), intent(in) :: chru
      integer(i32), intent(in) :: month
      integer(i32), intent(in) :: niteda
      real(r32), intent(in) :: cec
      real(r32), intent(in) :: cst
      real(r32), intent(in) :: esv
      real(r32), intent(in) :: sw
      real(r32), intent(in) :: temp
      real(r32), intent(in) :: trd
    end subroutine
  end interface

  interface
    module subroutine snowcov(this, chru, ctl_data, param_data, model_climate, intcp)
      class(Snowcomp), intent(inout) :: this
      integer(i32), intent(in) :: chru
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      type(Interception), intent(in) :: intcp
    end subroutine
  end interface

  interface
    module subroutine snowevap(this, model_climate, chru, ctl_data, param_data, intcp)
      class(Snowcomp), intent(inout) :: this
            type(Climateflow), intent(inout) :: model_climate
      integer(i32), intent(in) :: chru
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Interception), intent(in) :: intcp
    end subroutine
  end interface
end module
