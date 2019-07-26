module PRMS_TRANSPIRATION
  use variableKind
  use prms_constants, only: sp, dp
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use prms_constants, only: dp
  implicit none

  private
  public :: Transpiration

  character(len=*), parameter :: MODDESC = 'Transpiration'
  character(len=*), parameter :: MODNAME = 'transpiration'
  character(len=*), parameter :: MODVERSION = '2018-08-30 13:51:00Z'

  type, extends(ModelBase) :: Transpiration
    ! integer(i32) :: basin_transp_on
    logical :: basin_transp_on

    logical, allocatable :: transp_on(:)
    ! integer(i32), allocatable :: transp_on(:)

  contains
    procedure, public :: run_Transpiration
  end type

  interface Transpiration
    !! Transpiration constructor
    module function constructor_Transpiration(ctl_data, model_basin) result(this)
      type(Transpiration) :: this
        !! Transpiration class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
    end function
  end interface

  interface
    module subroutine run_Transpiration(this, ctl_data)
      class(Transpiration), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

end module
