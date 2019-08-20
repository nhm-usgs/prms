module PRMS_SRUNOFF
  use variableKind
  use iso_fortran_env, only: output_unit, error_unit
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
    real(r32), allocatable :: hru_percent_imperv(:)
      !! Fraction of each HRU area that is impervious
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
    real(r32), allocatable :: dprst_frac(:)
      !! Fraction of each HRU area that has surface depressions
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
    logical, private :: has_dynamic_params
    logical :: has_open_dprst
      !! NOTE: replaces dprst_open_flag

    real(r32), allocatable :: carea_dif(:)
    real(r32), allocatable :: imperv_stor_ante(:)

    ! 2019-05-28 PAN: moved as local to run()
    ! real(r64) :: sri
    ! real(r64) :: srp

    logical :: use_sroff_transfer
    ! integer(i32) :: use_sroff_transfer

    real(r64), allocatable :: basin_apply_sroff
    real(r32), allocatable :: hru_area_imperv(:)
    real(r32), allocatable :: hru_area_perv(:)


    ! Output variables
    real(r64), allocatable :: basin_contrib_fraction
    real(r64), allocatable :: basin_hortonian
    real(r64), allocatable :: basin_imperv_evap
    real(r64), allocatable :: basin_imperv_stor
    real(r64), allocatable :: basin_infil
    real(r64), allocatable :: basin_sroff
    real(r64), allocatable :: basin_sroffi
    real(r64), allocatable :: basin_sroffp

    real(r32), allocatable :: dprst_area_max(:)
    real(r32), allocatable :: hru_frac_perv(:)


    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    ! Cascades
    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    ! output variables
    real(r64), allocatable :: basin_hortonian_lakes
    real(r64), allocatable :: basin_sroff_down
    real(r64), allocatable :: basin_sroff_upslope

    real(r32), allocatable :: contrib_fraction(:)
    real(r32), allocatable :: hortonian_flow(:)
    real(r64), allocatable :: hortonian_lakes(:)
      !! r64 is correct
    real(r64), allocatable :: hru_hortn_cascflow(:)
      !! r64 is correct
    real(r32), allocatable :: hru_impervevap(:)
    real(r32), allocatable :: hru_impervstor(:)
    real(r32), allocatable :: hru_sroffi(:)
    real(r32), allocatable :: hru_sroffp(:)
    real(r32), allocatable :: imperv_evap(:)
    real(r32), allocatable :: imperv_stor(:)
      !! Storage on impervious area for each HRU
    real(r32), allocatable :: infil(:)
      !! Infiltration to the capillary and preferential-flow reservoirs from each HRU
    real(r32), allocatable :: sroff(:)
      !! Surface runoff to the stream network for each HRU
    real(r64), allocatable :: strm_seg_in(:)
      !! r64 is correct
    real(r64), allocatable :: upslope_hortonian(:)
      !! Used for cascades; r64 is correct

    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    ! Depression storage
    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    real(r32), allocatable :: dprst_frac_clos(:)
      !! NOTE: pulled from basin.f90
    real(r64), allocatable :: dprst_in(:)
    real(r64), allocatable :: dprst_stor_ante(:)
    real(r64), allocatable :: dprst_vol_clos_max(:)
    real(r64), allocatable :: dprst_vol_open_max(:)
    real(r64), allocatable :: dprst_vol_thres_open(:)

    ! Next two variables are only allocated if
    ! imperv_frac_flag == 1 or dprst_frac_flag == 1
    real(r32), allocatable :: soil_moist_chg(:)
    real(r32), allocatable :: soil_rechr_chg(:)

    logical :: srunoff_updated_soil

    ! Output variables
    real(r64), allocatable :: basin_dprst_evap
    real(r64), allocatable :: basin_dprst_seep
    real(r64), allocatable :: basin_dprst_sroff
    real(r64), allocatable :: basin_dprst_volcl
    real(r64), allocatable :: basin_dprst_volop

    real(r64), allocatable :: dprst_seep_hru(:)
      !! r64 is correct
    real(r64), allocatable :: dprst_sroff_hru(:)
      !! r64 is correct
    real(r64), allocatable :: dprst_stor_hru(:)
      !! r64 is correct
    real(r64), allocatable :: dprst_vol_clos(:)
      !! (from flowvars) Storage volume in closed surface depressions for each HRU
      !! r64 is correct
    real(r64), allocatable :: dprst_vol_open(:)
      !! (from flowvars) Storage volume in open surface depressions for each HRU
      !! r64 is correct
    real(r32), allocatable :: dprst_area_clos(:)
    real(r32), allocatable :: dprst_area_clos_max(:)
      !! NOTE: pulled from basin.f90
    real(r32), allocatable :: dprst_area_open(:)
    real(r32), allocatable :: dprst_area_open_max(:)
      !! NOTE: pulled from basin.f90
    real(r32), allocatable :: dprst_evap_hru(:)
    real(r32), allocatable :: dprst_insroff_hru(:)
    real(r32), allocatable :: dprst_vol_clos_frac(:)
    real(r32), allocatable :: dprst_vol_frac(:)
    real(r32), allocatable :: dprst_vol_open_frac(:)

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Dynamic parameter variables
    integer(i32) :: dyn_output_unit
      !! File handle to open dynamic parameter log file
    integer(i32), private :: imperv_frac_unit
    integer(i32) :: next_dyn_imperv_frac_date(3)
    real(r32), allocatable :: imperv_frac_chgs(:)

    integer(i32), private :: imperv_stor_unit
    integer(i32) :: next_dyn_imperv_stor_date(3)
    real(r32), allocatable :: imperv_stor_chgs(:)

    integer(i32), private :: dprst_frac_unit
    integer(i32) :: next_dyn_dprst_frac_date(3)
    real(r32), allocatable :: dprst_frac_chgs(:)

    integer(i32), private :: dprst_depth_unit
    integer(i32) :: next_dyn_dprst_depth_date(3)
    real(r32), allocatable :: dprst_depth_chgs(:)
    ! integer(i32) :: dyn_output_unit

    contains
      procedure, public :: init => init_Srunoff
      procedure, public :: run => run_Srunoff
      procedure, public :: cleanup => cleanup_Srunoff
      procedure, private :: check_capacity
      procedure, private :: dprst_comp
      procedure, private :: dprst_init
      procedure, private :: compute_infil
      procedure, private :: imperv_et
      procedure, private :: perv_comp
      procedure, private :: read_dyn_params
      procedure, nopass, private :: close_if_open
      procedure, nopass, private :: depression_surface_area
  end type

  interface
    !! Srunoff constructor
    module subroutine init_Srunoff(this, ctl_data, model_basin, model_summary)
      class(Srunoff), intent(inout) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
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

  ! interface
  !   module subroutine check_capacity(this, model_climate, idx, srp)
  !     class(Srunoff), intent(inout) :: this
  !     type(Climateflow), intent(in) :: model_climate
  !     integer(i32), intent(in) :: idx
  !     real(r64), intent(inout) :: srp
  !   end subroutine
  ! end interface

  interface
    module subroutine check_capacity(this, idx, soil_moist, soil_moist_max, &
                                     infil, srp)
      class(Srunoff), intent(in) :: this
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: srp
    end subroutine
  end interface

  ! interface
  !   module subroutine compute_infil(this, ctl_data, model_basin, model_climate, &
  !                                   intcp, snow, idx, sri, srp)
  !     class(Srunoff), intent(inout) :: this
  !     type(Control), intent(in) :: ctl_data
  !     type(Basin), intent(in) :: model_basin
  !     type(Climateflow), intent(in) :: model_climate
  !     type(Interception), intent(in) :: intcp
  !     type(Snowcomp), intent(in) :: snow
  !     integer(i32), intent(in) :: idx
  !     real(r64), intent(inout) :: sri
  !     real(r64), intent(inout) :: srp
  !   end subroutine
  ! end interface

  interface
    module subroutine compute_infil(this, ctl_data, idx, hru_type, net_ppt, net_rain, &
                                    net_snow, pkwater_equiv, pptmix_nopack, &
                                    snowmelt, soil_moist, soil_moist_max, soil_rechr, soil_rechr_max, &
                                    contrib_frac, imperv_stor, infil, sri, srp)
      class(Srunoff), intent(in) :: this
      type(Control), intent(in) :: ctl_data
      integer(i32), intent(in) :: idx
      integer(i32), intent(in) :: hru_type
      real(r32), intent(in) :: net_ppt
      real(r32), intent(in) :: net_rain
      real(r32), intent(in) :: net_snow
      real(r64), intent(in) :: pkwater_equiv
      logical, intent(in) :: pptmix_nopack
      real(r32), intent(in) :: snowmelt
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: soil_rechr
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(inout) :: contrib_frac
      real(r32), intent(inout) :: imperv_stor
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: sri
      real(r64), intent(inout) :: srp
    end subroutine
  end interface

  interface
    module subroutine close_if_open(unit)
        integer(i32), intent(in) :: unit

        logical :: is_opened
    end subroutine
  end interface

  interface
    elemental module function depression_surface_area(volume, volume_max, area_max, va_exp) result(res)
      real(r32) :: res
      real(r64), intent(in) :: volume
      real(r64), intent(in) :: volume_max
      real(r32), intent(in) :: area_max
      real(r32), intent(in) :: va_exp
    end function
  end interface

  interface
    module subroutine dprst_init(this, ctl_data, model_basin)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

  interface
    module subroutine dprst_comp(this, ctl_data, idx, model_basin, model_time, pkwater_equiv, &
                                 potet, net_rain, net_snow, &
                                 pptmix_nopack, snowmelt, snowcov_area, &
                                 avail_et, sri, srp)
                                !  dprst_insroff_hru, dprst_stor_hru, dprst_vol_clos, &
                                !  dprst_vol_clos_frac, dprst_vol_frac, dprst_vol_open, &
                                !  dprst_vol_open_frac, &
                                !  dprst_area_clos, dprst_area_open, dprst_evap_hru, &
                                !  dprst_seep_hru, dprst_sroff_hru)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      integer(i32), intent(in) :: idx
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      real(r64), intent(in) :: pkwater_equiv
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: net_rain
      real(r32), intent(in) :: net_snow
      logical, intent(in) :: pptmix_nopack
      real(r32), intent(in) :: snowmelt
      real(r32), intent(in) :: snowcov_area
      real(r32), intent(inout) :: avail_et
      real(r64), intent(inout) :: sri
      real(r64), intent(inout) :: srp
    end subroutine
  end interface

  ! interface
  !   module subroutine dprst_comp(this, ctl_data, model_basin, model_climate, model_potet, intcp, &
  !                                snow, model_time, idx, avail_et, sri, srp)
  !     class(Srunoff), intent(inout) :: this
  !     type(Control), intent(in) :: ctl_data
  !     type(Basin), intent(in) :: model_basin
  !     type(Climateflow), intent(in) :: model_climate
  !     class(Potential_ET), intent(in) :: model_potet
  !     type(Interception), intent(in) :: intcp
  !     type(Snowcomp), intent(in) :: snow
  !     type(Time_t), intent(in) :: model_time
  !     integer(i32), intent(in) :: idx
  !     real(r32), intent(inout) :: avail_et
  !     real(r64), intent(inout) :: sri
  !     real(r64), intent(inout) :: srp
  !   end subroutine
  ! end interface

  ! interface
  !   module subroutine imperv_et(this, idx, potet, sca, avail_et)
  !     class(Srunoff), intent(inout) :: this
  !     ! type(Basin), intent(in) :: model_basin
  !     integer(i32), intent(in) :: idx
  !     real(r32), intent(in) :: potet
  !     real(r32), intent(in) :: sca
  !     real(r32), intent(in) :: avail_et
  !   end subroutine
  ! end interface

  interface
    module subroutine imperv_et(this, idx, potet, sca, avail_et, imperv_evap, imperv_stor)
      class(Srunoff), intent(in) :: this
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: sca
      real(r32), intent(in) :: avail_et
      real(r32), intent(inout) :: imperv_evap
      real(r32), intent(inout) :: imperv_stor
    end subroutine
  end interface

  ! interface
  !   module subroutine perv_comp(this, ctl_data, model_climate, idx, &
  !                               pptp, ptc, srp)
  !     class(Srunoff), intent(inout) :: this
  !     type(Control), intent(in) :: ctl_data
  !     type(Climateflow), intent(in) :: model_climate
  !     integer(i32), intent(in) :: idx
  !     real(r32), intent(in) :: pptp
  !     real(r32), intent(in) :: ptc
  !     ! real(r32), intent(inout) :: infil
  !     real(r64), intent(inout) :: srp
  !   end subroutine
  ! end interface

  interface
    module subroutine perv_comp(this, ctl_data, idx, soil_moist, soil_rechr, soil_rechr_max, &
                                pptp, ptc, contrib_frac, infil, srp)
      class(Srunoff), intent(in) :: this
      type(Control), intent(in) :: ctl_data
      integer(i32), intent(in) :: idx
      real(r32), intent(in) :: pptp
      real(r32), intent(in) :: ptc
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: soil_rechr
      real(r32), intent(in) :: soil_rechr_max
      real(r32), intent(inout) :: contrib_frac
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: srp
    end subroutine
  end interface

  interface
    module subroutine read_dyn_params(this, ctl_data, model_basin, model_time, model_climate)
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Climateflow), intent(in) :: model_climate
    end subroutine
  end interface

end module
