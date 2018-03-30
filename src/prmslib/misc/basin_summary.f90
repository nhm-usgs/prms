!***********************************************************************
!     Output a set of declared basin variables as CSV file
!***********************************************************************
module PRMS_BASIN_SUMMARY
    use variableKind
    use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                              MEAN_MONTHLY, MEAN_YEARLY, YEARLY
    implicit none

    character(len=*), parameter :: MODNAME = 'basin_summary'
    character(len=*), parameter :: VERSION = 'basin_summary.f90 2017-09-29 13:49:00Z'
    ! integer(i32) :: BasinOutVars
    ! integer(i32) :: BasinOut_freq
    ! ! character(len=:), save, allocatable :: BasinOutVar_names(:)
    ! type(str_arr_type), allocatable :: BasinOutVar_names(:)
    ! character(len=:), allocatable :: BasinOutBaseFileName
    private
    public :: Basin_summary

    type Basin_summary

      ! Module Variables
      logical :: begin_results
      integer(i32) :: begyr
      integer(i32) :: lastyear
      integer(i32) :: dailyunit
      integer(i32) :: monthlyunit
      integer(i32) :: yearlyunit
      integer(i32) :: basin_var_type
      integer(i32), allocatable :: nc_vars(:)
      character(len=48) :: output_fmt
      character(len=48) :: output_fmt2
      character(len=48) :: output_fmt3

      integer(i32) :: daily_flag
      integer(i32) :: yeardays
      integer(i32) :: monthly_flag

      integer(i32), private :: start_time(6)
        !! Local copy of ctl_data%start_time
      integer(i32), private :: end_time(6)
        !! Local copy of ctl_data%start_time
      real(r64) :: monthdays
      real(r64), allocatable :: basin_var_daily(:)
      real(r64), allocatable :: basin_var_monthly(:)
      real(r64), allocatable :: basin_var_yearly(:)
    end type

    interface Basin_summary
      !! Basin_summary constructor
      module function constructor_Basin_summary(ctl_data, param_data) result(this)
        use Control_class, only: Control
        use Parameters_class, only: Parameters

        type(Basin_summary) :: this
          !! Basin_summary class
        class(Control), intent(in) :: ctl_data
          !! Control file parameters
        class(Parameters), intent(in) :: param_data
          !! Parameters
      end function
    end interface

    contains
      !***********************************************************************
      ! Basin_summary constructor
      module function constructor_Basin_summary(ctl_data, param_data) result(this)
        use Control_class, only: Control
        use Parameters_class, only: Parameters
        ! use PRMS_MODULE, only: Start_year
        use UTILS_PRMS, only: PRMS_open_output_file
        implicit none

        type(Basin_summary) :: this
        class(Control), intent(in) :: ctl_data
        class(Parameters), intent(in) :: param_data

        ! Local variables
        integer(i32) :: basin_out_flag
          !! Local copy of basinOutON_OFF
        integer(i32) :: basin_out_freq
          !! Local copy of basinOut_freq
        integer(i32) :: basin_out_vars
          !! Local copy of basinOutVars

        integer(i32) :: ios
        integer(i32) :: ierr = 0
        integer(i32) :: size
        integer(i32) :: jj
        character(len=MAXFILE_LENGTH) :: fileName


        ! ----------------------------------------------------------------------
        this%start_time = ctl_data%start_time%values(:)
        this%end_time = ctl_data%end_time%values(:)

        basin_out_vars = ctl_data%basinOutVars%values(1)

        ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
        basin_out_freq = ctl_data%basinOut_freq%values(1)

        if (basin_out_vars == 0) then
            if (ctl_data%model_mode%values(1)%s /= 'DOCUMENTATION') then
                print *, 'ERROR, basin_summary requested with basinOutVars equal 0'
                ! Inputerror_flag = 1
                STOP
            endif
        else
            allocate(this%nc_vars(basin_out_vars))
        endif


        ! Initialize everything
        this%begin_results = .true.
        this%begyr = this%start_time(1)
        if (ctl_data%prms_warmup%values(1) > 0) this%begin_results = .false.

        this%begyr = this%begyr + ctl_data%prms_warmup%values(1)
        this%lastyear = this%begyr

        write (this%output_fmt, 9001) basin_out_vars

        ! ierr = 0
        ! TODO: convert this
        do jj = 1, basin_out_vars
            ! this%basin_var_type = var_data%getvartype(ctl_data%basinOutVar_names%values(jj)%s)
            !
            ! if (this%basin_var_type /= 3) then
            !     print *, 'ERROR, invalid basin_summary variable:', ctl_data%basinOutVar_names%values(jj)%s
            !     print *, '       only double variables allowed'
            !     ierr = 1
            ! endif
            !
            ! size = var_data%getvarsize(ctl_data%basinOutVar_names%values(jj)%s)
            !
            ! if (size /= 1) then
            !     print *, 'ERROR, invalid Basin_summary variable:', ctl_data%basinOutVar_names%values(jj)%s
            !     print *, '       only scalar variables are allowed'
            !     ierr = 1
            ! endif
        enddo
        if (ierr == 1) STOP

        allocate(this%basin_var_daily(basin_out_vars))
        this%basin_var_daily = 0.0D0

        this%daily_flag = 0
        if (ANY([DAILY, DAILY_MONTHLY]==basin_out_freq)) this%daily_flag = 1

        this%monthly_flag = 0
        if (ANY([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY]==basin_out_freq)) this%monthly_flag = 1

        if (ANY([MEAN_YEARLY, YEARLY]==basin_out_freq)) then
            this%yeardays = 0
            allocate(this%basin_var_yearly(basin_out_vars))
            this%basin_var_yearly = 0.0D0
            write(this%output_fmt3, 9003) basin_out_vars
        endif
        if (this%monthly_flag == 1) then
            this%monthdays = 0.0D0
            allocate(this%basin_var_monthly(basin_out_vars))
            this%basin_var_monthly = 0.0D0
        endif

        write(this%output_fmt2, 9002) basin_out_vars

        if (this%daily_flag == 1) then
            fileName = ctl_data%basinOutBaseFileName%values(1)%s // '.csv'

            call PRMS_open_output_file(this%dailyunit, fileName, 'xxx', 0, ios)
            if (ios /= 0) STOP 'in basin_summary, daily'

            write(this%dailyunit, this%output_fmt2) (ctl_data%basinOutVar_names%values(jj)%s, jj=1, basin_out_vars)
        endif

        if (basin_out_freq == MEAN_YEARLY) then
            fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_meanyearly.csv'
            call PRMS_open_output_file(this%yearlyunit, fileName, 'xxx', 0, ios)
            if (ios /= 0) STOP 'in basin_summary, mean yearly'

            write(this%yearlyunit, this%output_fmt2) (ctl_data%basinOutVar_names%values(jj)%s, jj=1, basin_out_vars)
        elseif (basin_out_freq == YEARLY) then
            fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_yearly.csv'
            call PRMS_open_output_file(this%yearlyunit, fileName, 'xxx', 0, ios)
            if (ios /= 0) STOP 'in basin_summary, yearly'

            write (this%yearlyunit, this%output_fmt2) (ctl_data%basinOutVar_names%values(jj)%s, jj=1, basin_out_vars)
        elseif (this%monthly_flag == 1) then
            if (basin_out_freq == MEAN_MONTHLY) then
                fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_meanmonthly.csv'
            else
                fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_monthly.csv'
            endif

            call PRMS_open_output_file(this%monthlyunit, fileName, 'xxx', 0, ios)
            if (ios /= 0) STOP 'in basin_summary, monthly'

            write (this%monthlyunit, this%output_fmt2) (ctl_data%basinOutVar_names%values(jj)%s, jj=1, basin_out_vars)
        endif

        9001 FORMAT ('(I4, 2(''-'',I2.2),', I6, '('',''ES10.3))')
        9002 FORMAT ('("Date"', I6, '('',''A))')
        9003 FORMAT ('(I4,', I6, '('',''ES10.3))')
      end function


      ! TODO: convert the summary run routine
      !***********************************************************************
      !     Output set of declared variables in CSV format
      !***********************************************************************
      ! subroutine run_Basin_summary(var_data)
      !   use PRMS_MODULE, only: Start_month, Start_day, End_year, End_month, End_day
      !   use PRMS_SET_TIME, only: Nowyear, Nowmonth, Nowday, last_day_of_month
      !   use variables_arr_mod, only: variables_arr_t
      !   implicit none
      !
      !   type(variables_arr_t), intent(in) :: var_data
      !
      !   ! FUNCTIONS AND SUBROUTINES
      !   INTRINSIC SNGL, DBLE
      !
      !   ! Local Variables
      !   integer(i32) :: jj
      !   integer(i32) :: write_month
      !   integer(i32) :: write_year
      !   integer(i32) :: last_day
      !
      !   !***********************************************************************
      !   if (.not. Begin_results) then
      !     if (Nowyear == Begyr .AND. Nowmonth == Start_month .AND. Nowday == Start_day) then
      !       Begin_results = .true.
      !     else
      !       RETURN
      !     endif
      !   endif
      !
      !   !-----------------------------------------------------------------------
      !   do jj = 1, BasinOutVars
      !     call var_data%getvar_dble(MODNAME, BasinOutVar_names(jj)%str, 1, Basin_var_daily(jj))
      !   enddo
      !
      !   write_month = 0
      !   write_year = 0
      !   if (BasinOut_freq > 4) then
      !     last_day = 0
      !     if (Nowyear == End_year .AND. Nowmonth == End_month .AND. Nowday == End_day) last_day = 1
      !
      !     if (Lastyear /= Nowyear .OR. last_day == 1) then
      !       if ((Nowmonth == Start_month .AND. Nowday == Start_day) .OR. last_day == 1) then
      !         do jj = 1, BasinOutVars
      !           if (BasinOut_freq == 5) Basin_var_yearly(jj) = Basin_var_yearly(jj) / Yeardays
      !         enddo
      !
      !         write (Yearlyunit, Output_fmt3) Lastyear, (Basin_var_yearly(jj), jj = 1, BasinOutVars)
      !         Basin_var_yearly = 0.0D0
      !         Yeardays = 0
      !         Lastyear = Nowyear
      !       endif
      !     endif
      !     Yeardays = Yeardays + 1
      !   elseif (Monthly_flag == 1) then
      !     ! check for last day of month and simulation
      !     if (Nowday == last_day_of_month(Nowmonth)) then
      !       write_month = 1
      !     elseif (Nowyear == End_year) then
      !       if (Nowmonth == End_month) then
      !         if (Nowday == End_day) write_month = 1
      !       endif
      !     endif
      !     Monthdays = Monthdays + 1.0D0
      !   endif
      !
      !   if (BasinOut_freq > 4) then
      !     do jj = 1, BasinOutVars
      !       Basin_var_yearly(jj) = Basin_var_yearly(jj) + Basin_var_daily(jj)
      !     enddo
      !     RETURN
      !   endif
      !
      !   if (Monthly_flag == 1) then
      !     do jj = 1, BasinOutVars
      !       Basin_var_monthly(jj) = Basin_var_monthly(jj) + Basin_var_daily(jj)
      !
      !       if (write_month == 1) then
      !         if (BasinOut_freq == 4) Basin_var_monthly(jj) = Basin_var_monthly(jj) / Monthdays
      !       endif
      !     enddo
      !   endif
      !
      !   if (Daily_flag == 1) write (Dailyunit, Output_fmt) Nowyear, Nowmonth, Nowday, (Basin_var_daily(jj), jj = 1, BasinOutVars)
      !
      !   if (write_month == 1) then
      !     write (Monthlyunit, Output_fmt) Nowyear, Nowmonth, Nowday, (Basin_var_monthly(jj), jj = 1, BasinOutVars)
      !     Monthdays = 0.0D0
      !     Basin_var_monthly = 0.0D0
      !   endif
      ! end subroutine basin_summaryrun
end module PRMS_BASIN_SUMMARY
