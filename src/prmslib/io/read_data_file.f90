!***********************************************************************
! Read PRMS Data File
!***********************************************************************
module PRMS_DATA_FILE
  use variableKind
  ! use data_mod, only: str_arr_type
  implicit none

  integer(i32), save :: Num_datafile_types, Num_datafile_columns, Datafile_unit
  type(str_arr_type), allocatable, save :: Data_varname(:)
  ! character(len=:), allocatable, save :: Data_varname(:)
  integer(i32), allocatable, save :: Data_varnum(:)
  real(r32), allocatable, save :: Data_line_values(:)

  private :: check_data_variables
  public :: read_data_line, read_prms_data_file

  contains








end module PRMS_DATA_FILE

!***********************************************************************
! read_data_file_line - Read next data line, check increment
!***********************************************************************
!        subroutine read_data_file_line(Iret)
!            use PRMS_MODULE, only: Start_year, Start_month, Start_day
!            use time_mod, only: compute_julday
!            use PRMS_SET_TIME, only: Nowyear, Nowmonth, Nowday
!            use UTILS_PRMS, only: read_error
!            implicit none
!
!            ! Arguments
!            integer, intent(OUT) :: Iret
!
!            ! Local Variables
!            integer last_julday, now_julday, hr, mn, sec, start, i
!            integer, save :: init
!            DATA init/1/
!
!            !***********************************************************************
!            Iret = 0
!
!            if (init == 1) then
!                Nowyear = Start_year
!                Nowmonth = Start_month
!                Nowday = Start_day
!            else
!                last_julday = compute_julday(Nowyear, Nowmonth, Nowday)
!            endif
!
!            read (Datafile_unit, *, IOSTAT = Iret) Nowyear, Nowmonth, Nowday, hr, mn, sec, &
!                    (Data_line_values(i), i = 1, Num_datafile_columns)
!
!            if (Iret == 0) then
!                if (init == 0) then
!                    now_julday = compute_julday(Nowyear, Nowmonth, Nowday)
!                    if (now_julday - last_julday /= 1) then
!                        print *, 'ERROR, Data File timestep not equal to 1 day on:', Nowyear, Nowmonth, Nowday
!                        print *, '       timestep =', now_julday - last_julday
!                        STOP
!                    endif
!                else
!                    init = 0
!                endif
!
!                start = 1
!                do i = 1, Num_datafile_types
!                    call check_data_variables(Data_varname(i), Data_varnum(i), Data_line_values(start), 1, Iret)
!                    start = start + Data_varnum(i)
!                enddo
!            else
!                call read_error(13, 'measured variables')
!            endif
!        end subroutine read_data_file_line
