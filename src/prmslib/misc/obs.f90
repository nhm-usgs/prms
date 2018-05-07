!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
module PRMS_OBS
  use variableKind
  use Control_class, only: Control
  implicit none

  private
  public :: Obs

  character(len=*), parameter :: MODDESC = 'Time Series Data'
  character(len=*), parameter :: MODNAME = 'obs'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:50:00Z'

  type Obs
    ! Declared Variables
    real(r32), allocatable :: runoff(:)
    real(r32), allocatable :: precip(:)
    real(r32), allocatable :: tmax(:)
    real(r32), allocatable :: tmin(:)
    real(r64), allocatable :: streamflow_cfs(:)
    real(r64), allocatable :: streamflow_cms(:)

    contains
      procedure, public :: cleanup
        !! Final cleanup code after simulation
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type


  interface Obs
    !! Obs constructor
    module function constructor_Obs(ctl_data) result(this)
      type(Obs) :: this
        !! Obs class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  interface
    module subroutine cleanup(this, ctl_data)
      class(Obs), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    module function module_name() result(res)
      character(:), allocatable :: res
    end function
  end interface

  interface
    module function version() result(res)
      character(:), allocatable :: res
    end function
  end interface

end module
