module PRMS_POTET
  use variableKind
  use Control_class, only: Control
  use Parameters_class, only: Parameters

  implicit none

  private
  public :: Potential_ET

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
  character(len=*), parameter :: MODNAME = 'potet'
  character(len=*), parameter :: MODVERSION = '20188-07-03 12:36:00Z'

  ! Potential Evapotranspiration class
  type Potential_ET
    real(r64) :: basin_potet

    real(r32), allocatable :: potet(:)

    ! For potet_pt, potet_pm, potet_pm_sta
    ! real(r32), allocatable :: tempc_dewpt(:)
    ! real(r32), allocatable :: vp_actual(:)
    ! real(r32), allocatable :: lwrad_net(:)
    ! real(r32), allocatable :: vp_slope(:)
    ! real(r32), allocatable :: vp_sat(:)

    contains
      procedure, public :: run_Potet
  end type

  interface Potential_ET
    !! Potential_ET constructor
    module function constructor_Potet(ctl_data) result(this)
      type(Potential_ET) :: this
        !! Potential_ET class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine run_Potet(this, ctl_data, param_data)
      class(Potential_ET), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
    end subroutine
  end interface
end module
