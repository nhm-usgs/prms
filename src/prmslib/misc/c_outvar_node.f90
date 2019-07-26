module PRMS_OUTVAR_NODE
  use variableKind
  use iso_fortran_env, only: output_unit
  implicit none

  private
  public :: outvar_node

  type :: outvar_node
    character(len=:), allocatable :: key
      !! Name of output variable
    character(len=:), allocatable :: dimensions
      !! Dimensions used by the output variable
    integer(i32) :: datatype
    ! character(len=:), allocatable :: datatype
      !! Datatype of the output variable; either real, double, or integer
    character(len=:), allocatable :: description
      !! Short description of the output variable
    character(len=:), allocatable :: units
      !! The units used for the output variable
    type(outvar_node), pointer :: next => null()
      !! Pointer to next output variable entry
    type(outvar_node), pointer :: prev => null()
      !! Pointer to previous output variable entry

    contains
      private
      procedure, public :: destroy  => destroy_node_data  ! deallocate value
      procedure, public :: print
  end type

  interface outvar_node
    module function constructor_outvar_node(name, dimensions, datatype, description, units) result(this)
      type(outvar_node) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: dimensions
      integer(i32), intent(in) :: datatype
      ! character(len=*), intent(in) :: datatype
      character(len=*), intent(in) :: description
      character(len=*), intent(in) :: units
    end function
  end interface

  interface
    pure elemental module subroutine destroy_node_data(this)
      class(outvar_node), intent(inout) :: this
    end subroutine
  end interface

  interface
    module subroutine print(this)
      class(outvar_node), intent(in) :: this
    end subroutine
  end interface


end module