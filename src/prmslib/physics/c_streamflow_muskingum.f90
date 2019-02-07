module PRMS_MUSKINGUM
  use variableKind
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_GWFLOW, only: Gwflow
  use PRMS_OBS, only: Obs
  use PRMS_POTET, only: Potential_ET
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_STREAMFLOW, only: Streamflow
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_BASIN_SUMMARY_PTR, only: basin_summary_ptr
  use PRMS_NHRU_SUMMARY_PTR, only: Nhru_summary_ptr
  implicit none

  private
  public :: Muskingum

  character(len=*), parameter :: MODDESC = 'Streamflow routing'
  character(len=*), parameter :: MODNAME = 'muskingum'
  character(len=*), parameter :: MODVERSION = '2018-10-10 18:09:00Z'

  type, extends(Streamflow) :: Muskingum
    ! Parameters
    real(r32), allocatable :: x_coef(:)
      !! The amount of attenuation of the flow wave, called the Muskingum routing weighting factor; enter 0.0 for reservoirs, diversions, and segment(s) flowing out of the basin
    real(r32), allocatable :: K_coef(:)
      !! Travel time of flood wave from one segment to the next downstream segment, called the Muskingum storage coefficient; enter 1.0 for reservoirs, diversions, and segment(s) flowing out of the basin

    ! Local Variables
    real(r64), allocatable :: currinsum(:)
    real(r64), allocatable :: inflow_ts(:)
    real(r64), allocatable :: outflow_ts(:)
    real(r64), allocatable :: pastin(:)
    real(r64), allocatable :: pastout(:)

    ! Declared variables

    ! NOTE: ts_i, c0, c1, c2, ts moved from Streamflow because they
    !       are specific to muskingum or muskingum_lake.
    !       The muskingum_lake class will also declare these variables.
    integer(i32), public, allocatable :: ts_i(:)
      !! used by muskingum and muskingum_lake

    real(r32), public, allocatable :: c0(:)
      !! used by muskingum and muskingum_lake
    real(r32), public, allocatable :: c1(:)
      !! used by muskingum and muskingum_lake
    real(r32), public, allocatable :: c2(:)
      !! used by muskingum and muskingum_lake
    real(r32), public, allocatable :: ts(:)
      !! used by muskingum and muskingum_lake

    contains
      procedure, public :: run => run_Muskingum
      procedure, public :: cleanup => cleanup_Muskingum
  end type

  interface Muskingum
    !! Muskingum constructor
    module function constructor_Muskingum(ctl_data, model_basin, &
                                          model_time, basin_summary, nhru_summary) result(this)
      use prms_constants, only: dp
      implicit none

      type(Muskingum) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Basin_summary_ptr), intent(inout) :: basin_summary
        !! Basin summary
      type(Nhru_summary_ptr), intent(inout) :: nhru_summary
    end function
  end interface


  interface
    module subroutine run_Muskingum(this, ctl_data, model_basin, &
                                    model_potet, groundwater, soil, runoff, &
                                    model_time, model_solrad, model_obs)
      use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH
      implicit none

      class(Muskingum), intent(inout) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Potential_ET), intent(in) :: model_potet
      type(Gwflow), intent(in) :: groundwater
        !! Groundwater variables
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad
      type(Obs), intent(in) :: model_obs
    end subroutine
  end interface

  interface
    module subroutine cleanup_Muskingum(this)
      class(Muskingum), intent(inout) :: this
        !! Muskingum class
    end subroutine
  end interface
end module
