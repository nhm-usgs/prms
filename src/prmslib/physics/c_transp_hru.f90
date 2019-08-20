module PRMS_TRANSP_HRU
  use variableKind
  use prms_constants, only: sp, dp
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_TRANSPIRATION, only: Transpiration

  use prms_constants, only: dp
  implicit none

  private
  public :: Transp_hru

  character(len=*), parameter :: MODDESC = 'Transpiration by HRU'
  character(len=*), parameter :: MODNAME = 'transp_hru'
  character(len=*), parameter :: MODVERSION = '2018-08-30 13:55:00Z'

  type, extends(Transpiration) :: Transp_hru
    integer(i32), private :: transp_funit
      !! Transpiration CBH file unit

  contains
    procedure, public :: init => init_Transp_hru
    procedure, public :: run => run_Transp_hru
  end type

  interface
    !! Transp_hru constructor
    module subroutine init_Transp_hru(this, ctl_data)
      class(Transp_hru), intent(inout) :: this
        !! Transp_hru class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end subroutine
  end interface

  interface
    module subroutine run_Transp_hru(this, ctl_data)
      class(Transp_hru), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

end module
