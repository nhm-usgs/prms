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
    real(r32), pointer :: carea_max(:)
      !! Maximum possible area contributing to surface runoff expressed as a portion of the HRU area [decimal fraction]
    real(r32), pointer :: carea_min(:)
      !! Minimum possible area contributing to surface runoff expressed as a portion of the area for each HRU [decimal fraction]
    real(r32), pointer :: hru_percent_imperv(:)
      !! Fraction of each HRU area that is impervious [decimal fraction]
    real(r32), pointer :: imperv_stor_max(:)
      !! Maximum impervious area retention storage for each HRU [inches]
    real(r32), pointer :: smidx_coef(:)
      !! Coefficient in non-linear contributing area algorithm for each HRU [decimal fraction]
    real(r32), pointer :: smidx_exp(:)
      !! Exponent in non-linear contributing area algorithm for each HRU [1/inch]
    real(r32), pointer :: snowinfil_max(:)
      !! Maximum snow infiltration per day for each HRU [inches/day]

    ! NOTE: The following dprst_* parameters are only needed when dprst_flag = 1
    real(r32), pointer :: dprst_depth_avg(:)
      !! Average depth of storage depressions at maximum storage capacity [inches]
    real(r32), pointer :: dprst_et_coef(:)
      !! Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage [decimal fraction]
    real(r32), pointer :: dprst_flow_coef(:)
      !! Coefficient in linear flow routing equation for open surface depressions for each HRU [fraction/day]
    real(r32), pointer :: dprst_frac(:)
      !! Fraction of each HRU area that has surface depressions [decimal fraction]
    real(r32), pointer :: dprst_frac_init(:)
      !! Fraction of maximum surface-depression storage that contains water at the start of a simulation [decimal fraction]
    real(r32), pointer :: dprst_frac_open(:)
      !! Fraction of open surface-depression storage area within an HRU that can generate surface runoff as a function of storage volume [decimal fraction]
    real(r32), pointer :: dprst_seep_rate_clos(:)
      !! Coefficient used in linear seepage flow equation for closed surface depressions for each HRU [fraction/day]
    real(r32), pointer :: dprst_seep_rate_open(:)
      !! Coefficient used in linear seepage flow equation for open surface depressions for each HRU [fraction/day]

    real(r32), pointer :: op_flow_thres(:)
      !! Fraction of open depression storage above which surface runoff occurs; any water above maximum open storage capacity spills as surface runoff [decimal fraction]
    real(r32), pointer :: sro_to_dprst_imperv(:)
      !! Fraction of impervious surface runoff that flows into surface-depression storage; the remainder flows to a stream network for each HRU [decimal fraction]
    real(r32), pointer :: sro_to_dprst_perv(:)
      !! Fraction of pervious surface runoff that flows into surface-depression storage; the remainder flows to a stream network for each HRU [decimal fraction]
    real(r32), pointer :: va_clos_exp(:)
      !! Coefficient in the exponential equation relating maximum surface area to the fraction that closed depressions are full to compute current surface area for each HRU; 0.001 is an approximate rectangle; 1.0 is a triangle [none]
    real(r32), pointer :: va_open_exp(:)
      !! Coefficient in the exponential equation relating maximum surface area to the fraction that open depressions are full to compute current surface area for each HRU; 0.001 is an approximate rectangle; 1.0 is a triangle [none]


    ! Local Variables
    logical, private :: has_closed_dprst
      !! NOTE: replaces dprst_clos_flag
    logical, private :: has_dynamic_params
    logical, private :: has_open_dprst
      !! NOTE: replaces dprst_open_flag
    logical :: use_sroff_transfer

    real(r32), pointer :: carea_dif(:)
    real(r32), pointer :: imperv_stor_ante(:)

    real(r32), pointer :: hru_area_imperv(:)
      !! Area of HRU that is impervious [acres]
    real(r32), pointer :: hru_area_perv(:)
      !! Area of HRU that is pervious [acres]


    ! Output variables
    real(r32), pointer :: contrib_fraction(:)
      !! Contributing area of each HRU pervious area [decimal fraction]
    real(r32), pointer :: dprst_area_max(:)
      !! Aggregate sum of surface-depression storage areas of each HRU [acres]
    real(r32), pointer :: hru_frac_perv(:)
      !! Fraction of HRU that is pervious [decimal fraction]
    real(r32), pointer :: infil(:)
      !! Infiltration to the capillary and preferential-flow reservoirs from each HRU [inches]

    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    ! Cascades
    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    ! output variables
    real(r32), pointer :: hortonian_flow(:)
      !! Hortonian surface runoff reaching stream network for each HRU [inches]
    real(r32), pointer :: hru_impervevap(:)
      !!
    real(r32), pointer :: hru_impervstor(:)
      !! Storage on impervious area for each HRU [inches]
    real(r32), pointer :: hru_sroffi(:)
      !! Surface runoff from impervious areas for each HRU [inches]
    real(r32), pointer :: hru_sroffp(:)
      !! Surface runoff from pervious areas for each HRU [inches]
    real(r32), pointer :: imperv_evap(:)
      !!
    real(r32), pointer :: imperv_stor(:)
      !! Storage on impervious area for each HRU [inches]
    real(r32), pointer :: sroff(:)
      !! Surface runoff to the stream network for each HRU [inches]

    real(r64), pointer :: hortonian_lakes(:)
      !! Surface runoff to lakes for each HRU [inches]
      !! r64 is correct
    real(r64), pointer :: hru_hortn_cascflow(:)
      !! Cascading Hortonian surface runoff leaving each HRU [inches]
      !! r64 is correct
    real(r64), pointer :: strm_seg_in(:)
      !!
      !! r64 is correct
    real(r64), pointer :: upslope_hortonian(:) => null()
      !! Hortonian surface runoff received from upslope HRUs [inches]
      !! r64 is correct


    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    ! Depression storage
    ! ~~~~~~~~~~~~~~~~~~~~~~~~
    ! Internal variables
    logical :: srunoff_updated_soil

    real(r32), pointer :: dprst_frac_clos(:)
    real(r32), pointer :: soil_moist_chg(:)
      !! Allocated if imperv_frac_flag == 1 or dprst_frac_flag == 1
    real(r32), pointer :: soil_rechr_chg(:)
      !! Allocated if imperv_frac_flag == 1 or dprst_frac_flag == 1

    real(r64), pointer :: dprst_in(:)
    real(r64), pointer :: dprst_stor_ante(:)
    real(r64), pointer :: dprst_vol_clos_max(:)
    real(r64), pointer :: dprst_vol_open_max(:)
    real(r64), pointer :: dprst_vol_thres_open(:)

    real(r32), pointer :: dprst_area_clos(:)
      !! Surface area of closed surface depressions based on volume for each HRU [acres]
    real(r32), pointer :: dprst_area_clos_max(:)
      !! Aggregate sum of closed surface-depression storage areas of each HRU [acres]
    real(r32), pointer :: dprst_area_open(:)
      !! Surface area of open surface depressions based on volume for each HRU [acres]
    real(r32), pointer :: dprst_area_open_max(:)
      !! Aggregate sum of open surface-depression storage areas of each HRU [acres]
    real(r32), pointer :: dprst_evap_hru(:)
      !! Evaporation from surface-depression storage for each HRU [inches]
    real(r32), pointer :: dprst_insroff_hru(:)
      !! Surface runoff from pervious and impervious portions into surface depression storage for each HRU [inches]
    real(r32), pointer :: dprst_vol_clos_frac(:)
      !! Fraction of closed surface-depression storage of the maximum storage for each HRU [decimal fraction]
    real(r32), pointer :: dprst_vol_frac(:)
      !! Fraction of surface-depression storage of the maximum storage for each HRU [decimal fraction]
    real(r32), pointer :: dprst_vol_open_frac(:)
      !! Fraction of open surface-depression storage of the maximum storage for each HRU [decimal fraction]

    real(r64), pointer :: dprst_seep_hru(:)
      !! Seepage from surface-depression storage to associated GWR for each HRU [inches]
      !! r64 is correct
    real(r64), pointer :: dprst_sroff_hru(:)
      !! Surface runoff from open surface-depression storage for each HRU [inches]
      !! r64 is correct
    real(r64), pointer :: dprst_stor_hru(:)
      !! r64 is correct
    real(r64), pointer :: dprst_vol_clos(:)
      !! Storage volume in closed surface depressions for each HRU [acre-inches]
      !! r64 is correct
    real(r64), pointer :: dprst_vol_open(:)
      !! Storage volume in open surface depressions for each HRU [acre-inches]
      !! r64 is correct

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Dynamic parameter variables
    integer(i32) :: dyn_output_unit
      !! File handle to open dynamic parameter log file
    integer(i32), private :: imperv_frac_unit
    integer(i32) :: next_dyn_imperv_frac_date(3)
    real(r32), pointer :: imperv_frac_chgs(:)

    integer(i32), private :: imperv_stor_unit
    integer(i32) :: next_dyn_imperv_stor_date(3)
    real(r32), pointer :: imperv_stor_chgs(:)

    integer(i32), private :: dprst_frac_unit
    integer(i32) :: next_dyn_dprst_frac_date(3)
    real(r32), pointer :: dprst_frac_chgs(:)

    integer(i32), private :: dprst_depth_unit
    integer(i32) :: next_dyn_dprst_depth_date(3)
    real(r32), pointer :: dprst_depth_chgs(:)
    ! integer(i32) :: dyn_output_unit

    contains
      procedure, public :: init => init_Srunoff
      procedure, public :: run => run_Srunoff
      procedure, public :: cleanup => cleanup_Srunoff

      ! procedure, private :: compute_infil
      ! procedure, private :: dprst_comp
      procedure, private :: dprst_init
      procedure, private :: perv_comp
      procedure, private :: read_dyn_params

      procedure, nopass, private :: adjust_imperv_area
      procedure, nopass, private :: check_capacity
      procedure, nopass, private :: close_if_open
      procedure, nopass, private :: compute_contrib_fraction_smidx
      procedure, nopass, private :: compute_dprst_evaporation
      procedure, nopass, private :: compute_dprst_inflow
      procedure, nopass, private :: compute_dprst_insroff
      procedure, nopass, private :: compute_dprst_seepage
      procedure, nopass, private :: compute_infil2
      procedure, nopass, private :: compute_infil_srp
      procedure, nopass, private :: depression_surface_area
      procedure, nopass, private :: get_avail_water
      procedure, nopass, private :: imperv_et
      procedure, nopass, private :: update_dprst_open_sroff
      procedure, nopass, private :: update_dprst_storage
      procedure, nopass, private :: update_dprst_vol_fractions
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
      !! Compute surface runoff using contributing area computations using antecedent soil moisture.
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
    module subroutine cleanup_Srunoff(this, ctl_data)
      class(Srunoff), intent(in) :: this
        !! Srunoff class
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Private class procedures
  ! interface
  !   module subroutine dprst_comp(this, ctl_data, idx, model_basin, model_time, pkwater_equiv, &
  !                                potet, net_rain, net_snow, &
  !                                pptmix_nopack, snowmelt, snowcov_area)
  !                               !  dprst_insroff_hru, dprst_stor_hru, dprst_vol_clos, &
  !                               !  dprst_vol_clos_frac, dprst_vol_frac, dprst_vol_open, &
  !                               !  dprst_vol_open_frac, &
  !                               !  dprst_area_clos, dprst_area_open, dprst_evap_hru, &
  !                               !  dprst_seep_hru, dprst_sroff_hru)
  !     !! Compute depression storage area hydrology
  !     class(Srunoff), intent(inout) :: this
  !     type(Control), intent(in) :: ctl_data
  !     integer(i32), intent(in) :: idx
  !     type(Basin), intent(in) :: model_basin
  !     type(Time_t), intent(in) :: model_time
  !     real(r64), intent(in) :: pkwater_equiv
  !     real(r32), intent(in) :: potet
  !     real(r32), intent(in) :: net_rain
  !     real(r32), intent(in) :: net_snow
  !     logical, intent(in) :: pptmix_nopack
  !     real(r32), intent(in) :: snowmelt
  !     real(r32), intent(in) :: snowcov_area
  !     real(r32), intent(inout) :: avail_et
  !     real(r64), intent(inout) :: sri
  !     real(r64), intent(inout) :: srp
  !   end subroutine
  ! end interface

  interface
    module subroutine dprst_init(this, ctl_data, model_basin)
      !! Initialize depression storage area hydrology
      class(Srunoff), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

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

  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Private, nopass procedures
  interface
    pure elemental module subroutine adjust_imperv_area(hru_type, avail_water, area_imperv, imperv_stor_max, imperv_stor, imperv_runoff)
      integer(i32), intent(in) :: hru_type
      real(r32), intent(in) :: avail_water
      real(r32), intent(in) :: area_imperv
      real(r32), intent(in) :: imperv_stor_max
      real(r32), intent(inout) :: imperv_stor
      real(r64), intent(inout) :: imperv_runoff
    end subroutine
  end interface

  interface
    pure elemental module subroutine check_capacity(soil_moist, soil_moist_max, snowinfil_max, &
                                                    infil, srp)
      !! Fill soil to soil_moist_max, if more than capacity ten restrict infiltration by snowinfil_max, with excess added to runoff
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: soil_moist_max
      real(r32), intent(in) :: snowinfil_max
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: srp
    end subroutine
  end interface

  interface
    module subroutine close_if_open(unit)
        integer(i32), intent(in) :: unit
    end subroutine
  end interface

  interface
    pure elemental module function compute_contrib_fraction_smidx(carea_max, smidx_coef, smidx_exp, soil_moist, precip) result(res)
      real(r32) :: res
      real(r32), intent(in) :: carea_max
      real(r32), intent(in) :: smidx_coef
      real(r32), intent(in) :: smidx_exp
      real(r32), intent(in) :: soil_moist
      real(r32), intent(in) :: precip
    end function
  end interface

  interface
    pure elemental module subroutine compute_dprst_evaporation(et_coef, potet, snowcov_area, area_clos, &
                                                               area_open, area, avail_et, evaporation, &
                                                               vol_clos, vol_open)
      real(r32), intent(in) :: et_coef
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: snowcov_area
      real(r32), intent(in) :: area_clos
        !! Surface area of closed surface depressions based on volume [acres]
      ! real(r32), intent(in) :: area_max
      real(r32), intent(in) :: area_open
        !! Surface area of open surface depressions based on volume [acres]
      real(r64), intent(in) :: area
        !! HRU area [acres]
      real(r32), intent(inout) :: avail_et
      real(r32), intent(inout) :: evaporation
        !! Evaporation from surface-depression storage [inches]
      real(r64), intent(inout) :: vol_clos
        !! Storage volume in closed surface depression [acre-inches]
      real(r64), intent(inout) :: vol_open
        !! Storage volume in open surface depression [acre-inches]
    end subroutine
  end interface

  interface
    pure elemental module subroutine compute_dprst_inflow(area, area_clos_max, &
                                                          area_open_max, has_cascades, &
                                                          net_rain, net_snow, pkwater_equiv, &
                                                          pptmix_nopack, snowmelt, &
                                                          upslope_hortonian, &
                                                          dprst_in, vol_clos, vol_open)
      real(r64), intent(in) :: area
        !! HRU area [acres]
      real(r32), intent(in) :: area_clos_max
        ! Aggregate sum of closed surface-depression storage areas in HRU [acres]
      ! real(r32), intent(in) :: area_max
      real(r32), intent(in) :: area_open_max
        ! Aggregate sum of open surface-depression storage areas in HRU [acres]
      logical, intent(in) :: has_cascades
      real(r32), intent(in) :: net_rain
      real(r32), intent(in) :: net_snow
      real(r64), intent(in) :: pkwater_equiv
      logical, intent(in) :: pptmix_nopack
      real(r32), intent(in) :: snowmelt
      real(r64), intent(in) :: upslope_hortonian
      real(r64), intent(inout) :: dprst_in
      real(r64), intent(inout) :: vol_clos
        ! Storage volume in closed surface depression [acre-inches]
      real(r64), intent(inout) :: vol_open
        ! Storage volume in open surface depression [acre-inches]
    end subroutine
  end interface

  interface
    pure elemental module subroutine compute_dprst_insroff(area, area_clos_max, area_open_max, &
                                                           frac_clos, frac_open, frac_perv, &
                                                           percent_imperv, &
                                                           sro_to_dprst_imperv, sro_to_dprst_perv, &
                                                           va_clos_exp, va_open_exp,  &
                                                           vol_clos_max, vol_open_max, &
                                                           area_clos, area_open, insroff, sri, srp, vol_clos, vol_open)
      real(r64), intent(in) :: area
        !! HRU area [acres]
      real(r32), intent(in) :: area_clos_max
        ! Aggregate sum of closed surface-depression storage areas in HRU [acres]
      ! real(r32), intent(in) :: area_max
      real(r32), intent(in) :: area_open_max
        ! Aggregate sum of open surface-depression storage areas in HRU [acres]
      real(r32), intent(in) :: frac_clos
      real(r32), intent(in) :: frac_open
        !! Fraction of open surface-depression storage area that can generate surface runoff as a function of storage volume [decimal fraction]
      real(r32), intent(in) :: frac_perv
      real(r32), intent(in) :: percent_imperv
        !! Fraction of HRU area that is impervious [decimal fraction]
      real(r32), intent(in) :: sro_to_dprst_imperv
        !! Fraction of impervious surface runoff that flows into surface-depression storage [decimal fraction]
      real(r32), intent(in) :: sro_to_dprst_perv
        !! Fraction of pervious surface runoff that flows into surface-depression storage [decimal fraction]
      real(r32), intent(in) :: va_clos_exp
        !! Coefficient in the exponential equation relating maximum surface area to the fraction that closed depressions are full; 0.001 is an approximate rectangle; 1.0 is a triangle [none]
      real(r32), intent(in) :: va_open_exp
        !! Coefficient in the exponential equation relating maximum surface area to the fraction that open depressions are full; 0.001 is an approximate rectangle; 1.0 is a triangle [none]
      real(r64), intent(in) :: vol_clos_max
      real(r64), intent(in) :: vol_open_max
      real(r32), intent(inout) :: area_clos
        !! Surface area of closed surface depressions based on volume [acres]
      real(r32), intent(inout) :: area_open
        !! Surface area of open surface depressions based on volume [acres]
      real(r32), intent(inout) :: insroff
        !! Surface runoff from pervious and impervious portions into surface depression storage [inches]
      real(r64), intent(inout) :: sri
      real(r64), intent(inout) :: srp
      real(r64), intent(inout) :: vol_clos
        ! Storage volume in closed surface depression [acre-inches]
      real(r64), intent(inout) :: vol_open
        ! Storage volume in open surface depression [acre-inches]
    end subroutine
  end interface

  interface
    pure elemental module subroutine compute_dprst_seepage(seep_rate_clos, seep_rate_open, area_clos, &
                                                           area_clos_max, area, seepage, vol_clos, vol_open)
      real(r32), intent(in) :: seep_rate_clos
        !! Coefficient used in linear seepage flow equation for closed surface depression [fraction/day]
      real(r32), intent(in) :: seep_rate_open
        !! Coefficient used in linear seepage flow equation for open surface depression [fraction/day]
      real(r32), intent(in) :: area_clos
        !! Surface area of closed surface depressions based on volume [acres]
      real(r32), intent(in) :: area_clos_max
        !! Aggregate sum of closed surface-depression storage areas [acres]
      real(r64), intent(in) :: area
        !! HRU area [acres]
      ! real(r32), intent(in) :: area_max
      real(r64), intent(inout) :: seepage
        !! Seepage from surface-depression storage to associated GWR [inches]
      real(r64), intent(inout) :: vol_clos
        !! Storage volume in closed surface depression [acre-inches]
      real(r64), intent(inout) :: vol_open
        !! Storage volume in open surface depression [acre-inches]
    end subroutine
  end interface

  interface
    module subroutine compute_infil(this, ctl_data, idx, hru_type, net_ppt, net_rain, &
                                    net_snow, pkwater_equiv, pptmix_nopack, &
                                    snowmelt, soil_moist, soil_moist_max, soil_rechr, soil_rechr_max, &
                                    contrib_frac, imperv_stor, infil, sri, srp)
      !! Compute infiltration
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
    pure elemental module subroutine compute_infil2(has_cascades, hru_type, upslope_hortonian, carea_max, &
                                     smidx_coef, smidx_exp, snowinfil_max, net_ppt, net_rain, &
                                     net_snow, pkwater_equiv, pptmix_nopack, snowmelt, soil_moist, &
                                     soil_moist_max, soil_rechr, soil_rechr_max, contrib_frac, infil, srp)
      !! Compute infiltration
      logical, intent(in) :: has_cascades
      integer(i32), intent(in) :: hru_type
      real(r64), intent(in) :: upslope_hortonian
      real(r32), intent(in) :: carea_max
      real(r32), intent(in) :: smidx_coef
      real(r32), intent(in) :: smidx_exp
      real(r32), intent(in) :: snowinfil_max
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
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: srp
    end subroutine
  end interface

  interface
    pure elemental module subroutine compute_infil_srp(contrib_frac, precip, infil, perv_runoff)
      real(r32), intent(in) :: contrib_frac
      real(r32), intent(in) :: precip
      real(r32), intent(inout) :: infil
      real(r64), intent(inout) :: perv_runoff
    end subroutine
  end interface

  interface
    pure elemental module function depression_surface_area(volume, volume_max, area_max, va_exp) result(res)
      real(r32) :: res
      real(r64), intent(in) :: volume
      real(r64), intent(in) :: volume_max
      real(r32), intent(in) :: area_max
      real(r32), intent(in) :: va_exp
    end function
  end interface

  interface
    pure elemental module function get_avail_water(ctl_data, upslope_hortonian, &
                                                   net_rain, net_snow, snowmelt, &
                                                   pptmix_nopack, pkwater_equiv) result(res)
      real(r32) :: res
      type(Control), intent(in) :: ctl_data
      real(r64), intent(in) :: upslope_hortonian
      real(r32), intent(in) :: net_rain
      real(r32), intent(in) :: net_snow
      real(r32), intent(in) :: snowmelt
      logical, intent(in) :: pptmix_nopack
      real(r64), intent(in) :: pkwater_equiv
    end function
  end interface

  interface
    pure elemental module subroutine imperv_et(potet, sca, avail_et, percent_imperv, imperv_evap, imperv_stor)
      !! Compute evaporation from impervious area at potential ET rate up to available ET
      real(r32), intent(in) :: potet
      real(r32), intent(in) :: sca
      real(r32), intent(in) :: avail_et
      real(r32), intent(in) :: percent_imperv
      real(r32), intent(inout) :: imperv_evap
      real(r32), intent(inout) :: imperv_stor
    end subroutine
  end interface

  interface
    pure elemental module subroutine update_dprst_open_sroff(flow_coef, vol_thres_open, vol_open_max, &
                                                             area, sroff, vol_open)
      real(r32), intent(in) :: flow_coef
      real(r64), intent(in) :: vol_thres_open
      real(r64), intent(in) :: vol_open_max
      real(r64), intent(in) :: area
      ! real(r32), intent(in) :: area_max
      real(r64), intent(inout) :: sroff
      real(r64), intent(inout) :: vol_open
    end subroutine
  end interface

  interface
    pure elemental module function update_dprst_storage(vol_clos, vol_open, area) result(res)
      real(r64) :: res
        !! Depression storage
      real(r64), intent(in) :: vol_clos
        !! Storage volume in closed surface depression [acre-inch]
      real(r64), intent(in) :: vol_open
        !! Storage volume in open surface depression [acre-inch]
      real(r64), intent(in) :: area
        !! Area of the HRU
    end function
  end interface

  interface
    pure elemental module subroutine update_dprst_vol_fractions(vol_clos_max, vol_open_max, &
                                                                vol_clos, vol_open, &
                                                                vol_clos_frac, vol_open_frac, vol_frac)

      ! real(r32), intent(in) :: area_max
      real(r64), intent(in) :: vol_clos_max
      real(r64), intent(in) :: vol_open_max
      real(r64), intent(in) :: vol_clos
        !! Storage volume in closed surface depression [acre-inch]
      real(r64), intent(in) :: vol_open
        !! Storage volume in open surface depression [acre-inch]
      real(r32), intent(inout) :: vol_clos_frac
        !! Fraction of closed surface-depression storage of the maximum storage [fraction]
      real(r32), intent(inout) :: vol_open_frac
        !! Fraction of open surface-depression storage of the maximum storage [fraction]
      real(r32), intent(inout) :: vol_frac
    end subroutine
  end interface
end module
