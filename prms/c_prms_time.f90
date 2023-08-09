!***********************************************************************
! Sets PRMS time variables
!***********************************************************************
module PRMS_SET_TIME
  use PRMS_CONSTANTS, only: MONTHS_PER_YEAR
  implicit none

  !   Local Variables
  character(len=*), parameter :: MODDESC = 'Timestep Control'
  character(len=*), parameter :: MODNAME = 'prms_time'
  character(len=*), parameter :: Version_prms_time = '2022-04-21'

  integer, save :: Modays(MONTHS_PER_YEAR), Yrdays, Summer_flag, Jday, Jsol, Julwater
  integer, save :: Nowtime(6), Nowhour, Nowminute, Nowsecond, Julian_day_absolute
  real, save :: Timestep_hours, Timestep_days, Timestep_minutes
  double precision :: Cfs2inches
  double precision :: Cfs_conv
  double precision :: Timestep_seconds
  integer, save :: Previous_day, Last_time, Current_time, Subdaily_num
! Declared Variables
  integer, save :: Subdaily_status, Subdaily_flag, Newday

  interface
    integer module function prms_time()
    end function
  end interface
end module
