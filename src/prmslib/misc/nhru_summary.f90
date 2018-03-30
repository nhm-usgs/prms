!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
MODULE PRMS_NHRU_SUMMARY
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                            MEAN_MONTHLY, MEAN_YEARLY, YEARLY
  implicit none

  character(len=*), parameter :: MODNAME = 'nhru_summary'
  character(len=*), parameter :: VERSION = 'nhru_summary.f90 2017-09-29 13:49:00Z'

  private ! :: nhru_summarydecl, nhru_summaryinit, nhru_summaryrun
  public :: Nhru_summary

  type Nhru_summary
    ! Module Variables
    integer(i32) :: begin_results = 1
    integer(i32) :: begyr
    integer(i32) :: lastyear
    integer(i32), allocatable :: dailyunit(:)
    integer(i32), allocatable :: nc_vars(:)
    integer(i32), allocatable :: nhru_var_type(:)
    real(r32), allocatable :: nhru_var_daily(:, :)
    real(r64), allocatable :: nhru_var_dble(:, :)

    character(len=48) :: output_fmt
    character(len=48) :: output_fmt2
    character(len=48) :: output_fmt3

    integer(i32) :: daily_flag
    integer(i32) :: double_vars = 0
    integer(i32) :: yeardays
    integer(i32) :: monthly_flag

    real(r64) :: monthdays
    integer(i32), allocatable :: monthlyunit(:)
    integer(i32), allocatable :: yearlyunit(:)
    real(r64), allocatable :: nhru_var_monthly(:, :)
    real(r64), allocatable :: nhru_var_yearly(:, :)

    integer(i32), private :: start_time(6)
      !! Local copy of ctl_data%start_time
    integer(i32), private :: end_time(6)
      !! Local copy of ctl_data%start_time

    ! Parameters
    ! integer(i32), allocatable :: Nhm_id(:)
    ! TODO: create run routine
  end type

  interface Nhru_summary
    !! Climateflow constructor
    module function constructor_Nhru_summary(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters

      type(Nhru_summary) :: this
        !! Nhru_summary class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  contains
    !***********************************************************************
    ! Climateflow constructor
    module function constructor_Nhru_summary(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use prms_constants, only: MAXFILE_LENGTH
      ! USE PRMS_MODULE, ONLY: Start_year
      use UTILS_PRMS, only: read_error, PRMS_open_output_file

      implicit none

      type(Nhru_summary) :: this
      class(Control), intent(in) :: ctl_data
      class(Parameters), intent(in) :: param_data

      ! Functions
      INTRINSIC CHAR

      ! Local Variables
      integer(i32) :: nhru_out_flag
        !! Local copy of nhruOutON_OFF
      integer(i32) :: nhru_out_freq
        !! Local copy of nhruOut_freq
      integer(i32) :: nhru_out_vars
        !! Local copy of nhruOutVars

      integer(i32) :: ios
      integer(i32) :: ierr = 0
      integer(i32) :: size
      integer(i32) :: jj
      integer(i32) :: j
      character(len=MAXFILE_LENGTH) :: fileName

      ! ------------------------------------------------------------------------
      ! Available parameters
      ! type(sArray) :: nhruOutBaseFileName
      ! type(iArray) :: nhruOutON_OFF
      ! type(sArray) :: nhruOutVar_names
      ! type(iArray) :: nhruOutVars
      ! type(iArray) :: nhruOut_freq

      this%start_time = ctl_data%start_time%values(:)
      this%end_time = ctl_data%end_time%values(:)

      ! NOTE: NhruOutON_OFF=2 is an undocumented feature
      !       Parameter nhm_id is needed for this.

      nhru_out_flag = ctl_data%nhruOutON_OFF%values(1)

      ! nhruOut_freq
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      nhru_out_freq = ctl_data%nhruOut_freq%values(1)

      ! nhruOutVars - use if present in control file
      nhru_out_vars = ctl_data%nhruOutVars%values(1)

      if (nhru_out_vars == 0) then
        if (ctl_data%model_mode%values(1)%s /= 'DOCUMENTATION') then
          print *, 'ERROR, nhru_summary requested with nhruOutVars equal 0'
          STOP
          !          print *, 'no nhru_summary output is produced'
          !          NhruOutON_OFF = 0
          ! Inputerror_flag = 1
          return
        endif
      else
        allocate(this%nhru_var_type(nhru_out_vars))
        allocate(this%nc_vars(nhru_out_vars))
      endif

      ! begin_results = 1
      this%begyr = this%start_time(1)

      if (ctl_data%prms_warmup%values(1) > 0) this%begin_results = 0

      this%begyr = this%begyr + ctl_data%prms_warmup%values(1)
      this%lastyear = this%begyr

      write (this%output_fmt, 9001) ctl_data%nhru%values(1)

      ! double_vars = 0
      ! ierr = 0

      do jj = 1, nhru_out_vars
        ! TODO: Not sure yet how to handle the variables and their datatypes
        ! this%nhru_var_type(jj) = var_data%getvartype(ctl_data%nhruOutVar_names%values(jj)%s)
        ! if (this%nhru_var_type(jj) == 3) double_vars = 1
        !
        ! if (this%nhru_var_type(jj) /= 2 .AND. this%nhru_var_type(jj) /= 3) then
        !   print *, 'ERROR, invalid nhru_summary variable:', ctl_data%nhruOutVar_names%values(jj)%s
        !   print *, '       only real or double variables allowed'
        !   ierr = 1
        ! endif
        !
        ! size = var_data%getvarsize(ctl_data%nhruOutVar_names%values(jj)%s)
        ! if (size /= ctl_data%nhru%values(1)) then
        !   print *, 'ERROR, invalid nhru_summary variable:', ctl_data%nhruOutVar_names%values(jj)%s
        !   print *, '       only variables dimensioned by nhru, nssr, or ngw allowed'
        !   ierr = 1
        ! endif
      enddo
      if (ierr == 1) STOP

      if (this%double_vars == 1) then
        allocate(this%nhru_var_dble(ctl_data%nhru%values(1), nhru_out_vars))
        this%nhru_var_dble = 0.0D0
      endif

      this%daily_flag = 0
      if (ANY([DAILY, DAILY_MONTHLY]==nhru_out_freq)) then
      ! if (nhru_out_freq == DAILY .OR. nhru_out_freq == DAILY_MONTHLY) then
        this%daily_flag = 1
        allocate(this%dailyunit(nhru_out_vars))
      endif

      this%monthly_flag = 0
      if (ANY([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY]==nhru_out_freq)) this%monthly_flag = 1
      ! if (nhru_out_freq == MONTHLY .OR. nhru_out_freq == DAILY_MONTHLY .OR. nhru_out_freq == MEAN_MONTHLY) this%monthly_flag = 1

      if (ANY([MEAN_YEARLY, YEARLY]==nhru_out_freq)) then
      ! if (nhru_out_freq == MEAN_YEARLY .or. nhru_out_freq == YEARLY) then
        this%yeardays = 0
        allocate(this%nhru_var_yearly(ctl_data%nhru%values(1), nhru_out_vars))
        this%nhru_var_yearly = 0.0D0

        allocate(this%yearlyunit(nhru_out_vars))

        write(this%output_fmt3, 9003) ctl_data%nhru%values(1)
      elseif (this%monthly_flag == 1) then
        this%monthdays = 0.0D0
        allocate(this%nhru_var_monthly(ctl_data%nhru%values(1), nhru_out_vars))
        this%nhru_var_monthly = 0.0D0

        allocate(this%monthlyunit(nhru_out_vars))
      endif

      write(this%output_fmt2, 9002) ctl_data%nhru%values(1)
      allocate(this%nhru_var_daily(ctl_data%nhru%values(1), nhru_out_vars))
      this%nhru_var_daily = 0.0

      do jj = 1, nhru_out_vars
        if (this%daily_flag == 1) then
          fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                     ctl_data%nhruOutVar_names%values(jj)%s // '.csv'

          call PRMS_open_output_file(this%dailyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary'

          if (nhru_out_flag < 2) then
            write (this%dailyunit(jj), this%output_fmt2) (j, j=1, ctl_data%nhru%values(1))
          endif
        endif

        if (nhru_out_freq == MEAN_YEARLY) then
          fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                     ctl_data%nhruOutVar_names%values(jj)%s // '_meanyearly.csv'

          call PRMS_open_output_file(this%yearlyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary, mean yearly'

          if (nhru_out_flag < 2) write (this%yearlyunit(jj), this%output_fmt2) (j, j=1, ctl_data%nhru%values(1))
        elseif (nhru_out_freq == YEARLY) then
          fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                     ctl_data%nhruOutVar_names%values(jj)%s // '_yearly.csv'

          call PRMS_open_output_file(this%yearlyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary, yearly'

          write (this%yearlyunit(jj), this%output_fmt2) (j, j=1, ctl_data%nhru%values(1))
        elseif (this%monthly_flag == 1) then
          if (nhru_out_freq == MEAN_MONTHLY) then
            fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                       ctl_data%nhruOutVar_names%values(jj)%s // '_meanmonthly.csv'
          else
            fileName = ctl_data%nhruOutBaseFileName%values(1)%s // &
                       ctl_data%nhruOutVar_names%values(jj)%s // '_monthly.csv'
          endif
          !print *, fileName
          call PRMS_open_output_file(this%monthlyunit(jj), fileName, 'xxx', 0, ios)
          if (ios /= 0) STOP 'in nhru_summary, monthly'

          if (nhru_out_flag < 2) write (this%monthlyunit(jj), this%output_fmt2) (j, j=1, ctl_data%nhru%values(1))
        endif
      enddo

      if (nhru_out_flag == 2) then
          do jj = 1, nhru_out_vars
              if (this%daily_flag == 1) write (this%dailyunit(jj), this%output_fmt2) (param_data%nhm_id%values(j), j=1, ctl_data%nhru%values(1))

              if (nhru_out_freq == MEAN_YEARLY) then
                  write (this%yearlyunit(jj), this%output_fmt2) (param_data%nhm_id%values(j), j=1, ctl_data%nhru%values(1))
              elseif (this%monthly_flag == 1) then
                  write (this%monthlyunit(jj), this%output_fmt2) (param_data%nhm_id%values(j), j=1, ctl_data%nhru%values(1))
              endif
          enddo
      endif

      9001 FORMAT ('(I4, 2(''-'',I2.2),', I6, '('',''ES10.3))')
      9002 FORMAT ('("Date "', I6, '('',''I6))')
      9003 FORMAT ('(I4,', I6, '('',''ES10.3))')

    end function



    !***********************************************************************
    !     Output set of declared variables in R compatible format
    !***********************************************************************
    ! subroutine run_nhru_summary(var_data)
    !   use PRMS_MODULE, only: Start_month, Start_day, End_year, End_month, End_day
    !                          ! NhruOutVars, NhruOut_freq, NhruOutVar_names
    !   ! use PRMS_BASIN, only: Active_hrus, Hru_route_order
    !   use PRMS_SET_TIME, only: Nowyear, Nowmonth, Nowday, last_day_of_month
    !   use UTILS_PRMS, only: read_error
    !   ! use variables_arr_mod, only: variables_arr_t
    !   implicit none
    !
    !   type(variables_arr_t), intent(in) :: var_data
    !
    !   ! FUNCTIONS AND SUBROUTINES
    !   INTRINSIC SNGL, DBLE
    !
    !   ! Local Variables
    !   integer(i32) :: j
    !   integer(i32) :: i
    !   integer(i32) :: jj
    !   integer(i32) :: write_month
    !   integer(i32) :: write_year
    !   integer(i32) :: last_day
    !
    !   !***********************************************************************
    !   if (Begin_results == 0) then
    !     if (Nowyear == Begyr .AND. Nowmonth == Start_month .AND. Nowday == Start_day) then
    !       Begin_results = 1
    !     else
    !       RETURN
    !     endif
    !   endif
    !
    !   !-----------------------------------------------------------------------
    !   ! need getvars for each variable (only can have short string)
    !   do jj = 1, NhruOutVars
    !     if (Nhru_var_type(jj) == 2) then
    !       call var_data%getvar_real(MODNAME, NhruOutVar_names(jj)%str, Nhru, Nhru_var_daily(1, jj))
    !     elseif (Nhru_var_type(jj) == 3) then  ! probably don't need double
    !       call var_data%getvar_dble(MODNAME, NhruOutVar_names(jj)%str, Nhru, Nhru_var_dble(1, jj))
    !     endif
    !   enddo
    !
    !   write_month = 0
    !   write_year = 0
    !   if (NhruOut_freq > 4) then
    !     last_day = 0
    !
    !     if (Nowyear == End_year .AND. Nowmonth == End_month .AND. Nowday == End_day) last_day = 1
    !
    !     if (Lastyear /= Nowyear .OR. last_day == 1) then
    !       if ((Nowmonth == Start_month .AND. Nowday == Start_day) .OR. last_day == 1) then
    !         do jj = 1, NhruOutVars
    !           if (NhruOut_freq == 5) then
    !             do j = 1, Active_hrus
    !               i = Hru_route_order(j)
    !               Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj) / Yeardays
    !             enddo
    !           endif
    !           write (Yearlyunit(jj), Output_fmt3) Lastyear, (Nhru_var_yearly(j, jj), j=1, Nhru)
    !         enddo
    !
    !         Nhru_var_yearly = 0.0D0
    !         Yeardays = 0
    !         Lastyear = Nowyear
    !       endif
    !     endif
    !     Yeardays = Yeardays + 1
    !   elseif (Monthly_flag == 1) then
    !       ! check for last day of month and simulation
    !       if (Nowday == last_day_of_month(Nowmonth)) then
    !         write_month = 1
    !       elseif (Nowyear == End_year) then
    !         if (Nowmonth == End_month) then
    !           if (Nowday == End_day) write_month = 1
    !         endif
    !       endif
    !       Monthdays = Monthdays + 1.0D0
    !   endif
    !
    !   if (Double_vars == 1) then
    !     do jj = 1, NhruOutVars
    !       if (Nhru_var_type(jj) == 3) then
    !         do j = 1, Active_hrus
    !           i = Hru_route_order(j)
    !           Nhru_var_daily(i, jj) = SNGL(Nhru_var_dble(i, jj))
    !         enddo
    !       endif
    !     enddo
    !   endif
    !
    !   if (NhruOut_freq > 4) then
    !     do jj = 1, NhruOutVars
    !       do j = 1, Active_hrus
    !         i = Hru_route_order(j)
    !         Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj) + DBLE(Nhru_var_daily(i, jj))
    !       enddo
    !     enddo
    !     RETURN
    !   endif
    !
    !   if (Monthly_flag == 1) then
    !     do jj = 1, NhruOutVars
    !       do j = 1, Active_hrus
    !         i = Hru_route_order(j)
    !         Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) + DBLE(Nhru_var_daily(i, jj))
    !
    !         if (write_month == 1) then
    !           if (NhruOut_freq == 4) Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) / Monthdays
    !         endif
    !       enddo
    !     enddo
    !   endif
    !
    !   do jj = 1, NhruOutVars
    !     if (Daily_flag == 1) write (Dailyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, &
    !             (Nhru_var_daily(j, jj), j = 1, Nhru)
    !     if (write_month == 1) write (Monthlyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, &
    !             (Nhru_var_monthly(j, jj), j = 1, Nhru)
    !   enddo
    !
    !   if (write_month == 1) then
    !     Monthdays = 0.0D0
    !     Nhru_var_monthly = 0.0D0
    !   endif
    ! end subroutine nhru_summaryrun
end module
