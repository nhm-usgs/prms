module PRMS_TRANSPIRATION
  use variableKind
  use prms_constants, only: sp, dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use prms_constants, only: dp
  implicit none

  private
  public :: Transpiration

  character(len=*), parameter :: MODDESC = 'Transpiration'
  character(len=*), parameter :: MODNAME = 'transpiration'
  character(len=*), parameter :: MODVERSION = '2018-07-05 16:57:00Z'

  type Transpiration
    integer(i32) :: basin_transp_on

    integer(i32), allocatable :: transp_on(:)

  contains
    procedure, public :: run_Transpiration
  end type

  interface Transpiration
    !! Transpiration constructor
    module function constructor_Transpiration(ctl_data) result(this)
      type(Transpiration) :: this
        !! Transpiration class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine run_Transpiration(this, ctl_data)
      class(Transpiration), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

end module
