module PRMS_FILE_IO
  use variableKind
  implicit none

  private
  public :: FileIO

  type, abstract :: FileIO
    !> Handle to an opened file
    integer(r32) :: file_hdl

    contains
      procedure(open_fileio), pass(this), public, deferred :: open
      procedure(close_fileio), pass(this), public, deferred :: close
  end type

  abstract interface
    subroutine open_fileio(this, filename)
      import :: FileIO

      class(FileIO), intent(inout) :: this
      character(len=*), intent(in) :: filename
    end subroutine
  end interface

  abstract interface
    subroutine close_fileio(this)
      import :: FileIO

      class(FileIO), intent(inout) :: this
    end subroutine
  end interface
end module
