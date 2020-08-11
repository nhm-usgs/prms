!***********************************************************************
! Class for working with PRMS restart files
!***********************************************************************
module PRMS_RESTART
use variableKind
use iso_fortran_env, only: output_unit
use prms_constants, only: MAXFILE_LENGTH
use ModelBase_class, only: ModelBase
use Control_class, only: Control
use PRMS_SET_TIME, only: Time_t
use PRMS_BASIN, only: Basin
implicit none

private
public :: Restart

character(len=*), parameter :: MODDESC = 'Restart module'
character(len=*), parameter :: MODNAME = 'Restart'
character(len=*), parameter :: MODVERSION = '2020-02-04 14:52:00Z'

type, extends(ModelBase) :: Restart
  integer(i32), private :: read_hdl
    !! File handle to open netcdf input file
  integer(i32), private :: write_hdl
    !! File handle to open netcdf output file
  integer(i32), private :: time_varid
    !! NetCDF variable id for time variable

  contains
    procedure, private :: init_Restart
    procedure, private :: add_variable_r64_1d
    procedure, private :: add_variable_r32_1d

    procedure, private :: create_netcdf
    ! procedure, private :: open_netcdf
    procedure, nopass, private :: err_check

    generic, public :: add_variable => add_variable_r64_1d, add_variable_r32_1d

    ! procedure, public :: read_variable
    generic, public :: init => init_Restart
    procedure, public :: cleanup => cleanup_Restart

end type

interface
  !! Restart constructor
  module subroutine init_Restart(this, ctl_data, model_basin, model_time)
    class(Restart) :: this
      !! Restart class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
  end subroutine
end interface


interface
  module subroutine add_variable_r64_1d(this, data, var_name, var_dim_name, var_desc, var_units)
    class(Restart), intent(inout) :: this
    real(r64), intent(in) :: data(:)
    character(len=*), intent(in) :: var_name
    character(len=*), intent(in) :: var_dim_name
    character(len=*), intent(in) :: var_desc
    character(len=*), intent(in) :: var_units
  end subroutine
end interface

interface
  module subroutine add_variable_r32_1d(this, data, var_name, var_dim_name, var_desc, var_units)
    class(Restart), intent(inout) :: this
    real(r32), intent(in) :: data(:)
    character(len=*), intent(in) :: var_name
    character(len=*), intent(in) :: var_dim_name
    character(len=*), intent(in) :: var_desc
    character(len=*), intent(in) :: var_units
  end subroutine
end interface

interface
  module subroutine create_netcdf(this, ctl_data, model_basin, model_time, filename)
    class(Restart), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    character(len=*), intent(in) :: filename
  end subroutine
end interface

interface
  module subroutine err_check(status)
    integer(i32), intent(in) :: status
      !! The status returned by a netcdf call
  end subroutine
end interface

interface
  module subroutine cleanup_Restart(this)
    class(Restart), intent(in) :: this
  end subroutine
end interface

end module