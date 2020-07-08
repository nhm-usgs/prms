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
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Muskingum

  character(len=*), parameter :: MODDESC = 'Streamflow routing'
  character(len=*), parameter :: MODNAME = 'muskingum'
  character(len=*), parameter :: MODVERSION = '2018-10-10 18:09:00Z'

  type, extends(Streamflow) :: Muskingum
    ! Parameters
    real(r32), pointer, private :: x_coef(:)
      !! The amount of attenuation of the flow wave, called the Muskingum routing weighting factor; enter 0.0 for reservoirs, diversions, and segment(s) flowing out of the basin
    real(r32), pointer, private :: K_coef(:)
      !! Travel time of flood wave from one segment to the next downstream segment, called the Muskingum storage coefficient; enter 1.0 for reservoirs, diversions, and segment(s) flowing out of the basin

    ! Local Variables
    real(r64), allocatable, private :: currinsum(:)
    real(r64), allocatable, private :: inflow_ts(:)
    real(r64), pointer, private :: outflow_ts(:)
    real(r64), allocatable, private :: pastin(:)
    real(r64), allocatable, private :: pastout(:)

    ! Declared variables

    ! NOTE: ts_i, c0, c1, c2, ts moved from Streamflow because they
    !       are specific to muskingum or muskingum_lake.
    !       The muskingum_lake class will also declare these variables.
    integer(i32), allocatable, private :: ts_i(:)
      !! used by muskingum and muskingum_lake

    real(r64), allocatable, private :: c0(:)
      !! used by muskingum and muskingum_lake
    real(r64), allocatable, private :: c1(:)
      !! used by muskingum and muskingum_lake
    real(r64), allocatable, private :: c2(:)
      !! used by muskingum and muskingum_lake
    real(r64), allocatable, private :: ts(:)
      !! used by muskingum and muskingum_lake

    contains
      procedure, public :: init => init_Muskingum
      procedure, public :: run => run_Muskingum
      procedure, public :: cleanup => cleanup_Muskingum
  end type

  interface
    !! Muskingum constructor
    module subroutine init_Muskingum(this, ctl_data, model_basin, &
                                          model_time, model_summary)
      use prms_constants, only: dp
      implicit none

      class(Muskingum), intent(inout) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Summary), intent(inout) :: model_summary
    end subroutine
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
    module subroutine cleanup_Muskingum(this, ctl_data)
      class(Muskingum), intent(in) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module
