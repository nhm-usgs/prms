module PRMS_STRMFLOW_IN_OUT
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
  public :: Strmflow_in_out

  character(len=*), parameter :: MODDESC = 'Streamflow routing'
  character(len=*), parameter :: MODNAME = 'strmflow_in_out'
  character(len=*), parameter :: MODVERSION = '2020-08-14 17:37:00Z'

  type, extends(Streamflow) :: Strmflow_in_out
    ! Parameters

    ! Local variables

    ! Declared variables
    !   NO declared variables

    contains
      procedure, public :: init => init_Strmflow_in_out
      procedure, public :: run => run_Strmflow_in_out
      procedure, public :: cleanup => cleanup_Strmflow_in_out
  end type

  interface
    !! Muskingum constructor
    module subroutine init_Strmflow_in_out(this, ctl_data, model_basin, &
                                          model_time, model_summary)
      use prms_constants, only: dp
      implicit none

      class(Strmflow_in_out), intent(inout) :: this
        !! Strmflow_in_out class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Summary), intent(inout) :: model_summary
    end subroutine
  end interface


  interface
    module subroutine run_Strmflow_in_out(this, ctl_data, model_basin, &
                                    model_potet, groundwater, soil, runoff, &
                                    model_time, model_solrad, model_obs)
      use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH
      implicit none

      class(Strmflow_in_out), intent(inout) :: this
        !! Strmflow_in_out class
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
    module subroutine cleanup_Strmflow_in_out(this, ctl_data)
      class(Strmflow_in_out), intent(in) :: this
        !! Strmflow_in_out class
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface
end module