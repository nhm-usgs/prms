!***********************************************************************
! Computes volume of intercepted precipitation, evaporation from
! intercepted precipitation, and throughfall that reaches the soil or
! snowpack
!***********************************************************************
module PRMS_INTCP
  use variableKind
  use iso_fortran_env, only: output_unit, error_unit
  use ModelBase_class, only: ModelBase
  use prms_constants, only: dp
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_POTET, only: Potential_ET
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_TRANSPIRATION, only: Transpiration
  use PRMS_SUMMARY, only: Summary
  ! use PRMS_SNOW, only: Snowcomp
  implicit none

  private
  public :: Interception

  character(len=*), parameter :: MODDESC = 'Canopy Interception'
  character(len=*), parameter :: MODNAME = 'intcp'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:48:00Z'

  type, extends(ModelBase) :: Interception
    ! Dimensions

    ! Parameters
    real(r32), allocatable, private :: covden_sum(:)
      !! Summer vegetation cover density for the major vegetation type in each HRU
    real(r32), allocatable, private :: covden_win(:)
      !! Winter vegetation cover density for the major vegetation type in each HRU
    real(r32), allocatable :: snow_intcp(:)
      !! Snow interception storage capacity for the major vegetation type in each HRU
    real(r32), allocatable :: srain_intcp(:)
      !! Summer rain interception storage capacity for the major vegetation type in each HRU
    real(r32), allocatable :: wrain_intcp(:)
      !! Winter rain interception storage capacity for the major vegetation type in each HRU

    ! Local Variables
    real(r32), allocatable :: gain_inches(:)
    real(r32), allocatable :: intcp_changeover(:)
    real(r32), allocatable :: intcp_stor_ante(:)

    real(r64) :: last_intcp_stor
      !! Set by intcp, used by water_balance

    ! integer(i32) :: use_transfer_intcp
    logical :: use_transfer_intcp

    real(r64), allocatable :: basin_changeover

    ! Output variables
    real(r64), allocatable :: basin_hru_apply
    real(r64), allocatable :: basin_intcp_evap
    real(r64), allocatable :: basin_intcp_stor
    real(r64), allocatable :: basin_net_apply
    real(r64), allocatable :: basin_net_ppt
    real(r64), allocatable :: basin_net_rain
    real(r64), allocatable :: basin_net_snow

    real(r32), allocatable :: canopy_covden(:)
    real(r32), allocatable :: hru_intcpevap(:)
    real(r32), allocatable :: hru_intcpstor(:)
    real(r32), allocatable :: intcp_evap(:)
    real(r32), allocatable :: intcp_stor(:)
    real(r32), allocatable :: net_apply(:)
    real(r32), allocatable :: net_ppt(:)
    real(r32), allocatable :: net_rain(:)
    real(r32), allocatable :: net_snow(:)


    ! Local variables
    integer(i32), allocatable, private :: intcp_form(:)

    logical, allocatable, private :: intcp_on(:)
    logical, allocatable, private :: intcp_transp_on(:)

    ! integer(i32), allocatable, private :: intcp_on(:)
    ! integer(i32), allocatable, private :: intcp_transp_on(:)

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Dynamic parameter variables
    logical, private :: has_dynamic_params

    integer(i32), private :: dyn_output_unit
      !! File handle to open dynamic parameter log file

    integer(i32), private :: covden_sum_unit
    integer(i32), private :: next_dyn_covden_sum_date(3)
    real(r32), private, allocatable :: covden_sum_chgs(:)

    integer(i32), private :: covden_win_unit
    integer(i32), private :: next_dyn_covden_win_date(3)
    real(r32), private, allocatable :: covden_win_chgs(:)

    integer(i32), private :: snow_intcp_unit
    integer(i32), private :: next_dyn_snow_intcp_date(3)
    real(r32), private, allocatable :: snow_intcp_chgs(:)

    integer(i32), private :: srain_intcp_unit
    integer(i32), private :: next_dyn_srain_intcp_date(3)
    real(r32), private, allocatable :: srain_intcp_chgs(:)

    integer(i32), private :: wrain_intcp_unit
    integer(i32), private :: next_dyn_wrain_intcp_date(3)
    real(r32), private, allocatable :: wrain_intcp_chgs(:)


    ! integer(i32), private :: imperv_frac_unit
    ! integer(i32) :: next_dyn_imperv_frac_date(3)
    ! real(r32), allocatable :: imperv_frac_chgs(:)


    contains
      procedure, private :: init_Interception
      procedure, nopass, private :: intercept
      procedure, private :: read_dyn_params

      generic, public :: init => init_Interception
      procedure, public :: run => run_Interception
      procedure, public :: cleanup => cleanup_Interception
  end type

  ! interface Interception
  !   !! Intercept constructor
  !   module function constructor_Interception(ctl_data, model_basin, model_transp, model_summary) result(this)
  !     type(Interception) :: this
  !       !! Interception class
  !     type(Control), intent(in) :: ctl_data
  !       !! Control file parameters
  !     type(Basin), intent(in) :: model_basin
  !     class(Transpiration), intent(in) :: model_transp
  !     type(Summary), intent(inout) :: model_summary
  !   end function
  ! end interface

  interface
    !! Intercept constructor
    module subroutine init_Interception(this, ctl_data, model_basin, model_transp, model_summary)
      class(Interception) :: this
        !! Interception class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      class(Transpiration), intent(in) :: model_transp
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Interception(this, ctl_data, model_basin, &
                                       model_potet, model_precip, model_transp, model_climate, model_time)
      class(Interception) :: this
        !! Interception class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Potential_ET), intent(in) :: model_potet
      class(Precipitation), intent(inout) :: model_precip
      class(Transpiration), intent(in) :: model_transp
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Interception(this)
      class(Interception) :: this
        !! Interception class
    end subroutine
  end interface

  interface
    module subroutine intercept(net_precip, intcp_stor, cov, precip, stor_max)
      ! logical, intent(out) :: intcp_on
      real(r32), intent(out) :: net_precip
      real(r32), intent(inout) :: intcp_stor
      real(r32), intent(in) :: cov
      real(r32), intent(in) :: precip
      real(r32), intent(in) :: stor_max
    end subroutine
  end interface

  interface
    module subroutine read_dyn_params(this, ctl_data, model_basin, model_time)
      class(Interception), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface
end module
