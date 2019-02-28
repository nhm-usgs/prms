module PRMS_SRUNOFF
  use variableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_INTCP, only: Interception
  use PRMS_POTET, only: Potential_ET
  use PRMS_SNOW, only: Snowcomp
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Srunoff

  character(len=*), parameter :: MODDESC = 'Surface Runoff'
  character(len=*), parameter :: MODNAME = 'srunoff_smidx'
  character(len=*), parameter :: MODVERSION = '2018-10-10 17:23:00Z'

  type, extends(ModelBase) :: Srunoff
    ! Parameters
    real(r32), allocatable :: carea_max(:)
      !! Maximum possible area contributing to surface runoff expressed as a portion of the HRU area
    real(r32), allocatable :: carea_min(:)
      !! Minimum possible area contributing to surface runoff expressed as a portion of the area for each HRU
    real(r32), allocatable :: imperv_stor_max(:)
      !! Maximum impervious area retention storage for each HRU
    real(r32), allocatable :: smidx_coef(:)
      !! Coefficient in non-linear contributing area algorithm for each HRU
    real(r32), allocatable :: smidx_exp(:)
      !! Exponent in non-linear contributing area algorithm for each HRU
    real(r32), allocatable :: snowinfil_max(:)
      !! Maximum snow infiltration per day for each HRU

    ! NOTE: The following dprst_* parameters are only needed when dprst_flag = 1

    real(r32), allocatable :: dprst_depth_avg(:)
      !! Average depth of storage depressions at maximum storage capacity
    real(r32), allocatable :: dprst_et_coef(:)
      !! Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage
    real(r32), allocatable :: dprst_flow_coef(:)
      !! Coefficient in linear flow routing equation for open surface depressions for each HRU

    real(r32), allocatable :: dprst_frac_init(:)
      !! Fraction of maximum surface-depression storage that contains water at the start of a simulation
    real(r32), allocatable :: dprst_frac_open(:)
      !! Fraction of open surface-depression storage area within an HRU that can generate surface runoff as a function of storage volume
    real(r32), allocatable :: dprst_seep_rate_clos(:)
      !! Coefficient used in linear seepage flow equation for closed surface depressions for each HRU
    real(r32), allocatable :: dprst_seep_rate_open(:)
      !! Coefficient used in linear seepage flow equation for open surface depressions for each HRU
    real(r32), allocatable :: op_flow_thres(:)
      !! Fraction of open depression storage above which surface runoff occurs; any water above maximum open storage capacity spills as surface runoff
    real(r32), allocatable :: sro_to_dprst_imperv(:)
      !! Fraction of impervious surface runoff that flows into surface-depression storage; the remainder flows to a stream network for each HRU
    real(r32), allocatable :: sro_to_dprst_perv(:)
      !! Fraction of pervious surface runoff that flows into surface-depression storage; the remainder flows to a stream network for each HRU
    real(r32), allocatable :: va_clos_exp(:)
      !! Coefficient in the exponential equation relating maximum surface area to the fraction that closed depressions are full to compute current surface area for each HRU; 0.001 is an approximate rectangle; 1.0 is a triangle
    real(r32), allocatable :: va_open_exp(:)
      !! Coefficient in the exponential equation relating maximum surface area to the fraction that open depressions are full to compute current surface area for each HRU; 0.001 is an approximate rectangle; 1.0 is a triangle


    ! Local Variables
    logical :: has_closed_dprst
      !! NOTE: replaces dprst_clos_flag
    logical :: has_open_dprst
      !! NOTE: replaces dprst_open_flag

    real(r32), allocatable :: carea_dif(:)
    real(r64), allocatable :: imperv_stor_ante(:)

    real(r64) :: sri
    real(r64) :: srp

    logical :: use_sroff_transfer
    ! integer(i32) :: use_sroff_transfer

    ! Declared Variables
    real(r64), pointer :: basin_apply_sroff
    real(r64), pointer :: basin_contrib_fraction
    real(r64), pointer :: basin_hortonian
    real(r64), pointer :: basin_imperv_evap
    real(r64), pointer :: basin_imperv_stor
    real(r64), pointer :: basin_infil
    real(r64), pointer :: basin_sroff
    real(r64), pointer :: basin_sroffi
    real(r64), pointer :: basin_sroffp

    ! Used for cascades
    real(r64), pointer :: basin_hortonian_lakes
    real(r64), pointer :: basin_sroff_down
    real(r64), pointer :: basin_sroff_upslope

    real(r64), allocatable :: contrib_fraction(:)
    real(r64), allocatable :: hortonian_flow(:)
    real(r64), allocatable :: hortonian_lakes(:)
    real(r64), allocatable :: hru_hortn_cascflow(:)
    real(r64), allocatable :: hru_impervevap(:)
    real(r64), allocatable :: hru_impervstor(:)
    real(r64), allocatable :: hru_sroffi(:)
    real(r64), allocatable :: hru_sroffp(:)
    real(r64), allocatable :: imperv_evap(:)
    real(r64), allocatable :: imperv_stor(:)
      !! Storage on impervious area for each HRU
    real(r64), allocatable :: infil(:)
      !! Infiltration to the capillary and preferential-flow reservoirs from each HRU
    real(r64), allocatable :: sroff(:)
      !! Surface runoff to the stream network for each HRU
    real(r64), allocatable :: strm_seg_in(:)
    real(r64), allocatable :: upslope_hortonian(:)
      !! Used for cascades

    ! Declared Variables for Depression Storage
    real(r64), pointer :: basin_dprst_evap
    real(r64), pointer :: basin_dprst_seep
    real(r64), pointer :: basin_dprst_sroff
    real(r64), pointer :: basin_dprst_volcl
    real(r64), pointer :: basin_dprst_volop

    real(r64), allocatable :: dprst_in(:)
    real(r64), allocatable :: dprst_seep_hru(:)
    real(r64), allocatable :: dprst_sroff_hru(:)
    real(r64), allocatable :: dprst_stor_ante(:)
    real(r64), allocatable :: dprst_stor_hru(:)
    real(r64), allocatable :: dprst_vol_clos(:)
      !! (from flowvars) Storage volume in closed surface depressions for each HRU
    real(r64), allocatable :: dprst_vol_clos_max(:)
    real(r64), allocatable :: dprst_vol_open(:)
      !! (from flowvars) Storage volume in open surface depressions for each HRU
    real(r64), allocatable :: dprst_vol_open_max(:)
    real(r64), allocatable :: dprst_vol_thres_open(:)

    real(r64), allocatable :: dprst_area_clos(:)
    real(r64), allocatable :: dprst_area_clos_max(:)
      !! NOTE: pulled from basin.f90
    real(r64), allocatable :: dprst_area_open(:)
    real(r64), allocatable :: dprst_area_open_max(:)
      !! NOTE: pulled from basin.f90
    real(r64), allocatable :: dprst_evap_hru(:)
    real(r32), allocatable :: dprst_frac_clos(:)
      !! NOTE: pulled from basin.f90
    real(r64), allocatable :: dprst_insroff_hru(:)

    real(r32), allocatable :: dprst_vol_clos_frac(:)
    real(r32), allocatable :: dprst_vol_frac(:)
    real(r32), allocatable :: dprst_vol_open_frac(:)

! ! Declared Parameters
!       real(r32), allocatable :: Smidx_coef(:), Smidx_exp(:)
!       real(r32), allocatable :: Carea_min(:), Carea_max(:)
! ! Declared Parameters for Depression Storage
!       real(r32), allocatable :: Op_flow_thres(:), Sro_to_dprst_perv(:)
!       real(r32), allocatable :: Va_clos_exp(:), Va_open_exp(:)
!       real(r32), allocatable :: Dprst_flow_coef(:), Dprst_frac_init(:)
!       real(r32), allocatable :: Dprst_seep_rate_open(:), Dprst_seep_rate_clos(:)
!       real(r32), allocatable :: Dprst_depth_avg(:), Sro_to_dprst_imperv(:), Dprst_et_coef(:)
    contains
      procedure, public :: run => run_Srunoff
      procedure, public :: cleanup => cleanup_Srunoff
      procedure, private :: check_capacity
      procedure, private :: dprst_comp
      procedure, private :: dprst_init
      procedure, private :: compute_infil
      procedure, private :: imperv_et
      procedure, private :: perv_comp
  end type

  interface Srunoff
    !! Srunoff constructor
    module function constructor_Srunoff(ctl_data, model_basin, model_summary) result(this)
      type(Srunoff) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end function
  end interface

  interface
    module subroutine run_Srunoff(this, ctl_data, model_basin, &
                                  model_climate, model_potet, intcp, snow, &
                                  model_time)
      class(Srunoff), intent(inout) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      ! type(Flowvars), intent(in) :: model_flow
      class(Potential_ET), intent(in) :: model_potet
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  interface
    module subroutine cleanup_Srunoff(this)
      class(Srunoff) :: this
        !! Srunoff class
    end subroutine
  end interface

  interface
    module subroutine check_capacity(this, model_climate, idx)
      class(Srunoff), intent(inout) :: this
      type(Climateflow), intent(in) :: model_climate
      ! type(Flowvars), intent(in) :: model_flow
      integer(i32), intent(in) :: idx
    end subroutine
  end interface

  interface
    module subroutine dprst_init(this, ctl_data, model_basin)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine dprst_comp(this, ctl_data, model_basin, model_climate, model_potet, intcp, &
                                 snow, model_time, idx, avail_et)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: model_climate
      class(Potential_ET), intent(in) :: model_potet
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      type(Time_t), intent(in) :: model_time
      integer(i32), intent(in) :: idx
      real(r64), intent(inout) :: avail_et
    end subroutine
  end interface

  interface
    module subroutine imperv_et(this, model_basin, idx, potet, sca, avail_et)
      class(Srunoff), intent(inout) :: this
      type(Basin), intent(in) :: model_basin
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: sca
      real(r64), intent(in) :: avail_et
    end subroutine
  end interface

  interface
    module subroutine perv_comp(this, ctl_data, model_climate, idx, &
                                pptp, ptc, srp)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Climateflow), intent(in) :: model_climate
      ! type(Flowvars), intent(in) :: model_flow
      integer(i32), intent(in) :: idx
      real(r64), intent(in) :: pptp
      real(r64), intent(in) :: ptc
      ! real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: srp
    end subroutine
  end interface

  interface
    module subroutine compute_infil(this, ctl_data, model_basin, model_climate, &
                                    intcp, snow, idx)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: model_climate
      ! type(Flowvars), intent(in) :: model_flow
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      integer(i32), intent(in) :: idx
    end subroutine
  end interface
end module
