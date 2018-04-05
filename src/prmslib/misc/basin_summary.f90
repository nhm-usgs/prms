!***********************************************************************
!     Output a set of declared basin variables as CSV file
!***********************************************************************
module PRMS_BASIN_SUMMARY
    use variableKind
    use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                              MEAN_MONTHLY, MEAN_YEARLY, YEARLY, YEAR, MONTH, DAY
    implicit none

    private
    public :: Basin_summary

    character(len=*), parameter :: MODDESC = 'Output Summary by Basin'
    character(len=*), parameter :: MODNAME = 'basin_summary'
    character(len=*), parameter :: MODVERSION = '2017-09-29 13:49:00Z'

    type Basin_summary

      ! Module Variables
      logical :: begin_results
      integer(i32) :: begyr
      integer(i32) :: lastyear
      integer(i32) :: dailyunit
      integer(i32) :: monthlyunit
      integer(i32) :: yearlyunit
      integer(i32) :: basin_var_type
      ! integer(i32), allocatable :: nc_vars(:) ! We don't store number of characters in name anymore
      character(len=48) :: output_fmt
      character(len=48) :: output_fmt2
      character(len=48) :: output_fmt3

      integer(i32) :: daily_flag = 0
      integer(i32) :: monthly_flag = 0
      integer(i32) :: yeardays = 0

      ! integer(i32), private :: start_time(6)
        !! Local copy of ctl_data%start_time
      ! integer(i32), private :: end_time(6)
        !! Local copy of ctl_data%start_time
      real(r64) :: monthdays = 0.0
      real(r64), allocatable :: basin_var_daily(:)
      real(r64), allocatable :: basin_var_monthly(:)
      real(r64), allocatable :: basin_var_yearly(:)

      contains
        procedure, public :: run => run_Basin_summary
        procedure, nopass, public :: module_name
          !! Return the name of the module
        procedure, nopass, public :: version
          !! Return the version of the module
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
        use UTILS_PRMS, only: PRMS_open_output_file, print_module_info
        implicit none

        type(Basin_summary) :: this
        class(Control), intent(in) :: ctl_data
        class(Parameters), intent(in) :: param_data

        integer(i32) :: ios
        integer(i32) :: ierr = 0
        integer(i32) :: size
        integer(i32) :: jj
        character(len=MAXFILE_LENGTH) :: fileName

        ! ----------------------------------------------------------------------
        associate(print_debug => ctl_data%print_debug%values(1), &
                  start_time => ctl_data%start_time%values, &
                  end_time => ctl_data%end_time%values, &
                  basinOutVars => ctl_data%basinOutVars%values(1), &
                  basinOut_freq => ctl_data%basinOut_freq%values(1))

          if (print_debug > -2) then
            ! Output module and version information
            call print_module_info(MODNAME, MODDESC, MODVERSION)
          endif

          if (basinOutVars == 0) then
              if (ctl_data%model_mode%values(1)%s /= 'DOCUMENTATION') then
                  print *, 'ERROR, basin_summary requested with basinOutVars equal 0'
                  ! Inputerror_flag = 1
                  STOP
              endif
          endif

          ! Initialize everything
          this%begin_results = .true.
          this%begyr = start_time(1)
          if (ctl_data%prms_warmup%values(1) > 0) this%begin_results = .false.

          this%begyr = this%begyr + ctl_data%prms_warmup%values(1)
          this%lastyear = this%begyr

          write (this%output_fmt, 9001) basinOutVars

          ! The header for output - common to all frequencies
          write(this%output_fmt2, 9002) basinOutVars

          ! Always allocate and intialize the daily array
          allocate(this%basin_var_daily(basinOutVars))
          this%basin_var_daily = 0.0D0

          if (ANY([DAILY, DAILY_MONTHLY]==basinOut_freq)) then
            this%daily_flag = 1

            fileName = ctl_data%basinOutBaseFileName%values(1)%s // '.csv'

            call PRMS_open_output_file(this%dailyunit, fileName, 'xxx', 0, ios)
            if (ios /= 0) STOP 'in basin_summary, daily'

            write(this%dailyunit, this%output_fmt2) (ctl_data%basinOutVar_names%values(jj)%s, jj=1, basinOutVars)
          endif

          ! Allocate/intialize monthly array for month-based frequencies
          if (ANY([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY]==basinOut_freq)) then
            this%monthly_flag = 1

            allocate(this%basin_var_monthly(basinOutVars))
            this%basin_var_monthly = 0.0D0

            if (basinOut_freq == MEAN_MONTHLY) then
                fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_meanmonthly.csv'
            else
                fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_monthly.csv'
            endif

            call PRMS_open_output_file(this%monthlyunit, fileName, 'xxx', 0, ios)
            if (ios /= 0) STOP 'in basin_summary, monthly or meanmonthly'

            write (this%monthlyunit, this%output_fmt2) (ctl_data%basinOutVar_names%values(jj)%s, jj=1, basinOutVars)
          endif

          ! Allocate/initialize the yearly array for year-based frequencies
          if (ANY([MEAN_YEARLY, YEARLY]==basinOut_freq)) then
            allocate(this%basin_var_yearly(basinOutVars))
            this%basin_var_yearly = 0.0D0

            write(this%output_fmt3, 9003) basinOutVars

            if (basinOut_freq == MEAN_YEARLY) then
              fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_meanyearly.csv'
            elseif (basinOut_freq == YEARLY) then
              fileName = ctl_data%basinOutBaseFileName%values(1)%s // '_yearly.csv'
            endif

            call PRMS_open_output_file(this%yearlyunit, fileName, 'xxx', 0, ios)
            if (ios /= 0) STOP 'in basin_summary, mean_yearly or yearly'

            ! Write the header to the file
            write(this%yearlyunit, this%output_fmt2) (ctl_data%basinOutVar_names%values(jj)%s, jj=1, basinOutVars)
          endif

          9001 FORMAT ('(I4, 2(''-'',I2.2),', I6, '('',''ES10.3))')
          9002 FORMAT ('("Date"', I6, '('',''A))')
          9003 FORMAT ('(I4,', I6, '('',''ES10.3))')
        end associate
      end function


      ! TODO: convert the summary run routine
      !***********************************************************************
      !     Output set of declared variables in CSV format
      !***********************************************************************
      subroutine run_Basin_summary(this, ctl_data, model_time, climate)
        ! use PRMS_MODULE, only: Start_month, Start_day, End_year, End_month, End_day
        ! use PRMS_SET_TIME, only: Nowyear, Nowmonth, Nowday, last_day_of_month
        ! use variables_arr_mod, only: variables_arr_t
        use Control_class, only: Control
        use PRMS_SET_TIME, only: Time
        use PRMS_CLIMATEVARS, only: Climateflow
        implicit none

        class(Basin_summary), intent(inout) :: this
        type(Control), intent(in) :: ctl_data
        type(Time), intent(in) :: model_time
        type(Climateflow), intent(in) :: climate

        ! FUNCTIONS AND SUBROUTINES
        INTRINSIC SNGL, DBLE

        ! Local Variables
        integer(i32) :: jj
        logical :: write_month = .false.
        logical :: last_day = .false.

        !***********************************************************************
        associate(curr_year => model_time%Nowtime(YEAR), &
                  curr_month => model_time%Nowtime(MONTH), &
                  curr_day => model_time%Nowtime(DAY), &
                  st_year => ctl_data%start_time%values(YEAR), &
                  st_month => ctl_data%start_time%values(MONTH), &
                  st_day => ctl_data%start_time%values(DAY), &
                  en_year => ctl_data%end_time%values(YEAR), &
                  en_month => ctl_data%end_time%values(MONTH), &
                  en_day => ctl_data%end_time%values(DAY), &
                  basinOutVars => ctl_data%basinOutVars%values(1), &
                  basinOut_freq => ctl_data%basinOut_freq%values(1), &
                  basinOutVar_names => ctl_data%basinOutVar_names%values)

          if (.not. this%begin_results) then
            if (curr_year == this%begyr .and. curr_month == st_month .and. curr_day == st_day) then
              this%begin_results = .true.
            else
              RETURN
            endif
          endif

          !-----------------------------------------------------------------------
          do jj = 1, basinOutVars
            ! TODO: This is where the daily basin values are copied over based on
            !       what was requested in basinOutVar_names.
            ! basin_potet, basin_swrad, basin_temp, basin_ppt
            ! call var_data%getvar_dble(MODNAME, BasinOutVar_names(jj)%str, 1, Basin_var_daily(jj))
            select case(basinOutVar_names(jj)%s)

              case('basin_horad')
                this%basin_var_daily(jj) = climate%basin_horad(1)
              case('basin_obs_ppt')
                this%basin_var_daily(jj) = climate%basin_obs_ppt(1)
              case('basin_orad')
                this%basin_var_daily(jj) = climate%basin_orad(1)
              case('basin_potet')
                this%basin_var_daily(jj) = climate%basin_potet(1)
              case('basin_ppt')
                this%basin_var_daily(jj) = climate%basin_ppt(1)
              case('basin_rain')
                this%basin_var_daily(jj) = climate%basin_rain(1)
              case('basin_snow')
                this%basin_var_daily(jj) = climate%basin_snow(1)
              ! case('basin_solsta')
              !   this%basin_var_daily(jj) = climate%basin_solsta(1)
              case('basin_swrad')
                this%basin_var_daily(jj) = climate%basin_swrad(1)
              case('basin_temp')
                this%basin_var_daily(jj) = climate%basin_temp(1)
              case('basin_tmax')
                this%basin_var_daily(jj) = climate%basin_tmax(1)
              case('basin_tmin')
                this%basin_var_daily(jj) = climate%basin_tmin(1)
              ! case('basin_transp_on')
              !   this%basin_var_daily(jj) = climate%basin_transp_on(1)
              case default
                ! pass
            end select
          enddo

          if (ANY([MEAN_YEARLY, YEARLY]==basinOut_freq)) then
            if (curr_year == en_year .AND. curr_month == en_month .AND. curr_day == en_day) then
              last_day = .true.
            endif

            if (this%lastyear /= curr_year .or. last_day) then
              if ((curr_month == st_month .and. curr_day == st_day) .or. last_day) then
                do jj = 1, basinOutVars
                  if (basinOut_freq == MEAN_YEARLY) then
                    this%basin_var_yearly(jj) = this%basin_var_yearly(jj) / this%yeardays
                  endif
                enddo

                write (this%yearlyunit, this%output_fmt3) this%lastyear, (this%basin_var_yearly(jj), jj = 1, basinOutVars)
                this%basin_var_yearly = 0.0
                this%yeardays = 0
                this%lastyear = curr_year
              endif
            endif

            this%yeardays = this%yeardays + 1

            do jj = 1, basinOutVars
              this%basin_var_yearly(jj) = this%basin_var_yearly(jj) + this%basin_var_daily(jj)
            enddo
            RETURN
          elseif (this%monthly_flag == 1) then
            ! check for last day of month and simulation
            if (curr_day == model_time%last_day_of_month(curr_month)) then
              write_month = .true.
            elseif (curr_year == en_year .and. curr_month == en_month .and. curr_day == en_day) then
              write_month = .true.
            endif

            this%monthdays = this%monthdays + 1.0D0

            do jj = 1, basinOutVars
              this%basin_var_monthly(jj) = this%basin_var_monthly(jj) + this%basin_var_daily(jj)

              if (write_month) then
                if (basinOut_freq == MEAN_MONTHLY) then
                  this%basin_var_monthly(jj) = this%basin_var_monthly(jj) / this%monthdays
                endif
              endif
            enddo

            if (write_month) then
              write (this%monthlyunit, this%output_fmt) curr_year, curr_month, curr_day, (this%basin_var_monthly(jj), jj = 1, basinOutVars)
              this%monthdays = 0.0
              this%basin_var_monthly = 0.0
            endif
          elseif (this%daily_flag == 1) then
            write (this%dailyunit, this%output_fmt) curr_year, curr_month, curr_day, (this%basin_var_daily(jj), jj = 1, basinOutVars)
          endif
        end associate
      end subroutine

      function module_name()
        implicit none

        character(:), allocatable :: module_name
        module_name = MODNAME
      end function

      function version()
        implicit none

        character(:), allocatable :: version
        version = MODVERSION
      end function
end module PRMS_BASIN_SUMMARY
