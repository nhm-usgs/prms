!***********************************************************************
! Defines globals
!***********************************************************************
module PRMS_MODULE
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH, MAXCONTROL_LENGTH
  ! use data_mod, only: str_arr_type
  implicit none

  character(len=*), PARAMETER :: MODNAME = 'prms6'
  character(len=*), PARAMETER :: PRMS_VERSION = 'Version 6.0.0 09/29/2017'

  ! character(len=:), allocatable, save :: Process   ! one of: setdims, declare, init, run, or clean
  ! character(len=:), allocatable, save :: PRMS_versn
  ! integer(i32), save :: Model
  ! integer(i32), save :: Number_timesteps
  ! integer(i32), save :: Nhru
  ! integer(i32), save :: Ntemp
  ! integer(i32), save :: Nrain
  ! integer(i32), save :: Nobs
  ! integer(i32), allocatable, save :: Starttime(:)  !> Simulation start date and time (YY MM DD hh mm ss)
  ! integer(i32), allocatable, save :: Endtime(:)    !> Simulation end date and time (YY MM DD hh mm ss)
  ! integer(i32), save :: Start_year
  ! integer(i32), save :: Start_month
  ! integer(i32), save :: Start_day
  ! integer(i32), save :: End_year
  ! integer(i32), save :: End_month
  ! integer(i32), save :: End_day

  ! integer(i32), save :: Inputerror_flag
  ! integer(i32), save :: Timestep
  ! integer(i32), save :: Prms_warmup
  integer(i32), save :: PRMS_output_unit
  integer(i32), save :: Restart_inunit
  integer(i32), save :: Restart_outunit
  ! integer(i32), save :: Elapsed_time_start(8)
  ! integer(i32), save :: Elapsed_time_end(8)
  ! integer(i32), save :: Elapsed_time_minutes
  ! character(len=:), allocatable, save :: Version_read_control_file
  ! character(len=:), allocatable, save :: Version_read_parameter_file
  ! real(r32), save :: Execution_time_start
  ! real(r32), save :: Execution_time_end
  ! real(r32), save :: Elapsed_time

  contains
    !***********************************************************************
    ! print_module
    ! print module version information to the screen
    !***********************************************************************
    subroutine print_module(Versn, Description, Ftntype)
      implicit none

      ! Arguments
      character(len=*), intent(in) :: Versn
      character(len=*), intent(in) :: Description
      integer(i32), intent(in) :: Ftntype

      ! Functions
      INTRINSIC INDEX, TRIM

      ! Local Variables
      integer(i32) :: nc
      integer(i32) :: n
      integer(i32) :: nb
      integer(i32) :: is
      character(len=28) :: blanks
      character(len=80) :: string

      !*******************************************************************
      ! if (Print_debug == -2) return

      nc = INDEX(Versn, 'Z') - 10
      n = INDEX(Versn, '.f') - 1
      if (n < 1) n = 1

      if (Ftntype == 90) then
        is = 5
      else
        is = 3
      endif

      blanks = ' '
      nb = 29 - (n + 3)
      string = Description // '   ' // Versn(:n) // blanks(:nb) // Versn(n + is:nc)
      print '(A)', TRIM(string)
      !      write ( Logunt, '(A)' ) TRIM( string )

      ! 20180329 PAN: Don't see a Model=2 in PRMS source
      ! if (Model /= 2) write (PRMS_output_unit, '(A)') TRIM(string)
      write(PRMS_output_unit, '(A)') TRIM(string)
    end subroutine print_module
end module
