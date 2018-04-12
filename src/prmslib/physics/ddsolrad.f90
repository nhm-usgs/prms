!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a maximum temperature per degree-day relation;
! Declared Parameters
!     dday_slope, dday_intcp, radj_sppt, radj_wppt
!     radadj_slope, radadj_intcp, radmax, ppt_rad_adj, rad_conv
!     tmax_index, tmax_allrain
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 in
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
module PRMS_DDSOLRAD
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_SOLTAB, only: Soltab
  use PRMS_SET_TIME, only: Time_t
  implicit none

  private
  public :: Ddsolrad

  character(len=*), parameter :: MODDESC = 'Solar Radiation Distribution'
  character(len=*), parameter :: MODNAME = 'ddsolrad'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:50:00Z'

  real(r32), dimension(26), parameter :: SOLF = [.20, .35, .45, .51, .56, .59, &
                                                 .62, .64, .655, .67, .682, .69, &
                                                 .70, .71, .715, .72, .722, .724, &
                                                 .726, .728, .73, .734, .738, &
                                                 .742, .746, .75]

  type Ddsolrad
    contains
      procedure, public :: run => run_Ddsolrad
  end type

  interface Ddsolrad
    !! Ddsolrad constructor
    module function constructor_Ddsolrad(ctl_data) result(this)
      ! use Control_class, only: Control

      type(Ddsolrad) :: this
        !! Ddsolrad class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine run_Ddsolrad(this, ctl_data, param_data, model_time, solt, climate, model_basin)
      ! use Control_class, only: Control
      ! use Parameters_class, only: Parameters
      ! use PRMS_BASIN, only: Basin
      ! use PRMS_CLIMATEVARS, only: Climateflow
      ! use PRMS_SOLTAB, only: Soltab
      ! use PRMS_SET_TIME, only: Time_t

      class(Ddsolrad), intent(in) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      type(Soltab), intent(in) :: solt
      type(Climateflow), intent(inout) :: climate
      type(Basin), intent(inout) :: model_basin
    end subroutine
  end interface
end module
