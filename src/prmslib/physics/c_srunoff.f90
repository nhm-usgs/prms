module PRMS_SRUNOFF
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_FLOWVARS, only: Flowvars
  use PRMS_INTCP, only: Interception
  use PRMS_SNOW, only: Snowcomp
  use PRMS_SET_TIME, only: Time_t
  implicit none

  private
  public :: Srunoff

  character(len=*), parameter :: MODDESC = 'Surface Runoff'
  character(len=*), parameter :: MODNAME = 'srunoff_smidx'
  character(len=*), parameter :: MODVERSION = '2018-06-18 13:46:00Z'

  type Srunoff
    ! Local Variables
    logical :: has_closed_dprst
      !! NOTE: replaces dprst_clos_flag
    logical :: has_open_dprst
      !! NOTE: replace dprst_open_flag
    ! integer(i32) :: ihru


    real(r32), allocatable :: carea_dif(:)
    real(r32), allocatable :: imperv_stor_ante(:)

    real(r32) :: hruarea
    real(r32) :: hruarea_imperv
    real(r32) :: imperv_frac
    real(r32) :: perv_frac
    real(r32) :: sri
    real(r32) :: srp

    real(r64) :: basin_apply_sroff
    real(r64) :: hruarea_dble

    integer(i32) :: use_sroff_transfer

    ! Declared Variables
    real(r64) :: basin_contrib_fraction
    real(r64) :: basin_hortonian
    real(r64) :: basin_hortonian_lakes
    real(r64) :: basin_imperv_evap
    real(r64) :: basin_imperv_stor
    real(r64) :: basin_infil
    real(r64) :: basin_sroff
    real(r64) :: basin_sroff_down
    real(r64) :: basin_sroff_upslope
    real(r64) :: basin_sroffi
    real(r64) :: basin_sroffp

    real(r32), allocatable :: contrib_fraction(:)
    real(r32), allocatable :: hortonian_flow(:)
    real(r32), allocatable :: hru_impervevap(:)
    real(r32), allocatable :: hru_impervstor(:)
    real(r32), allocatable :: hru_sroffi(:)
    real(r32), allocatable :: hru_sroffp(:)
    real(r32), allocatable :: imperv_evap(:)
    real(r32), allocatable :: imperv_stor(:)
      !! (from c_flowvars) Storage on impervious area for each HRU
    real(r32), allocatable :: infil(:)
      !! (from c_flowvars) Infiltration to the capillary and preferential-flow reservoirs from each HRU
    real(r32), allocatable :: sroff(:)
      !! (from c_flowvars) Surface runoff to the stream network for each HRU

    real(r64), allocatable :: hortonian_lakes(:)
    real(r64), allocatable :: hru_hortn_cascflow(:)
    real(r64), allocatable :: strm_seg_in(:)
    real(r64), allocatable :: upslope_hortonian(:)

    ! Declared Variables for Depression Storage
    real(r64) :: basin_dprst_evap
    real(r64) :: basin_dprst_seep
    real(r64) :: basin_dprst_sroff
    real(r64) :: basin_dprst_volcl
    real(r64) :: basin_dprst_volop

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

    real(r32), allocatable :: dprst_area_clos(:)
    real(r32), allocatable :: dprst_area_clos_max(:)
      !! NOTE: pulled from basin.f90
    real(r32), allocatable :: dprst_area_open(:)
    real(r32), allocatable :: dprst_area_open_max(:)
      !! NOTE: pulled from basin.f90
    real(r32), allocatable :: dprst_evap_hru(:)
    real(r32), allocatable :: dprst_frac_clos(:)
      !! NOTE: pulled from basin.f90
    real(r32), allocatable :: dprst_insroff_hru(:)
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
    module function constructor_Srunoff(ctl_data, param_data, model_basin) result(this)
      type(Srunoff) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
    end function
  end interface

  interface
    module subroutine run_Srunoff(this, ctl_data, param_data, model_basin, &
                                       model_climate, model_flow, intcp, snow)
      class(Srunoff) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Flowvars), intent(in) :: model_flow
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
    end subroutine
  end interface

  interface
    module subroutine cleanup_Srunoff(this)
      class(Srunoff) :: this
        !! Srunoff class
    end subroutine
  end interface

  interface
    module subroutine check_capacity(this, param_data, model_flow, idx)
      class(Srunoff), intent(inout) :: this
      type(Parameters), intent(in) :: param_data
      type(Flowvars), intent(in) :: model_flow
      integer(i32), intent(in) :: idx
    end subroutine
  end interface

  interface
    module subroutine dprst_init(this, ctl_data, param_data, model_basin)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine dprst_comp(this, ctl_data, param_data, model_climate, intcp, &
                                 snow, idx, avail_et)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      integer(i32), intent(in) :: idx
      real(r32), intent(inout) :: avail_et
    end subroutine
  end interface

  interface
    module subroutine imperv_et(this, idx, potet, sca, avail_et)
      class(Srunoff), intent(inout) :: this
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: sca
      real(r32), intent(in) :: avail_et
    end subroutine
  end interface

  interface
    module subroutine perv_comp(this, ctl_data, param_data, model_flow, idx, &
                                pptp, ptc, srp)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Flowvars), intent(in) :: model_flow
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: pptp
      real(r32), intent(in) :: ptc
      ! real(r32), intent(inout) :: infil
      real(r32), intent(inout) :: srp
    end subroutine
  end interface

  interface
    module subroutine compute_infil(this, ctl_data, param_data, model_climate, &
                                    model_flow, intcp, snow, idx)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Climateflow), intent(in) :: model_climate
      type(Flowvars), intent(in) :: model_flow
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
      integer(i32), intent(in) :: idx
    end subroutine
  end interface
end module
