module PRMS_POTET_HRU
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  use PRMS_BASIN, only: Basin
  use PRMS_POTET, only: Potential_ET
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_SUMMARY, only: Summary
  implicit none

  private
  public :: Potet_hru

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration by HRU'
  character(len=*), parameter :: MODNAME = 'potet_hru'
  character(len=*), parameter :: MODVERSION = '2018-10-10 16:37:00Z'

  type, extends(Potential_ET) :: Potet_hru
    integer(i32), private :: et_funit
      !! Evapotranspiration CBH file unit

    contains
      procedure, public :: init => init_Potet_hru
      procedure, public :: run => run_Potet_hru
  end type

  interface
    !! Potet_hru constructor
    module subroutine init_Potet_hru(this, ctl_data, model_basin, model_summary)
      class(Potet_hru), intent(inout) :: this
        !! Poteh_jh class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface

  interface
    module subroutine run_Potet_hru(this, ctl_data, model_time, model_basin)
      class(Potet_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
    end subroutine
  end interface

end module
