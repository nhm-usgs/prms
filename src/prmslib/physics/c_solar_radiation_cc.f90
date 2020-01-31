module SOLAR_RADIATION_CC
  ! ***********************************************************************
  ! Distributes solar radiation to each HRU and estimates missing solar
  ! radiation data using a relation between solar radiation and cloud cover.
  ! Declared Parameters
  !     ccov_slope, ccov_intcp, radj_sppt, radj_wppt, basin_solsta
  !     crad_coef, crad_exp, radmax, ppt_rad_adj, rad_conv, hru_solsta
  ! RSR: 03/31/2008
  ! RSR: Warning, summer is based on equinox of Julian days 79 to 265 in
  ! RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
  ! ***********************************************************************
  use variableKind
  use SOLAR_RADIATION, only: SolarRadiation
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_OBS, only: Obs
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_TEMPERATURE, only: Temperature
  use PRMS_SET_TIME, only: Time_t
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Solrad_cc

  character(len=*), parameter :: MODDESC = 'Solar Radiation Distribution'
  character(len=*), parameter :: MODNAME = 'solrad_cc'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:20:00Z'

  type, extends(SolarRadiation) :: Solrad_cc
    ! Declared Variables
    real(r32), pointer :: cloud_radadj(:)
    real(r32), pointer :: cloud_cover_hru(:)

    contains
      procedure, public :: init => init_Solrad_cc
      procedure, public :: run => run_Solrad_cc
  end type


  interface Solrad_cc
    !! Solrad_cc constructor
    module subroutine init_Solrad_cc(this, ctl_data, model_basin, model_temp, model_summary)
      class(Solrad_cc), intent(inout) :: this
        !! Solrad_cc class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      class(Temperature), intent(in) :: model_temp
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Solrad_cc(this, ctl_data, model_time, model_obs, model_precip, model_basin)
      class(Solrad_cc), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      type(Obs), intent(in) :: model_obs
      class(Precipitation), intent(in) :: model_precip
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface
end module
