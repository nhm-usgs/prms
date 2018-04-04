!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
MODULE PRMS_NHRU_SUMMARY
  use variableKind
  use prms_constants, only: MAXFILE_LENGTH, DAILY, DAILY_MONTHLY, MONTHLY, &
                            MEAN_MONTHLY, MEAN_YEARLY, YEARLY, YEAR, MONTH, DAY
  implicit none

  character(len=*), parameter :: MODNAME = 'nhru_summary'
  character(len=*), parameter :: VERSION = 'nhru_summary.f90 2017-09-29 13:49:00Z'

  private ! :: nhru_summarydecl, nhru_summaryinit, nhru_summaryrun
  public :: Nhru_summary

  type Nhru_summary
    ! Module Variables
    logical :: begin_results
      !! Used to trigger processing in the run_Nhru_summary routine
    integer(i32) :: begyr
    integer(i32) :: lastyear
    integer(i32), allocatable :: dailyunit(:)
    ! integer(i32), allocatable :: nc_vars(:)
    ! integer(i32), allocatable :: nhru_var_type(:)
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

    contains
      procedure, public :: run => run_Nhru_summary
  end type

  interface Nhru_summary
    !! Nhru_summary constructor
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
      endif

      this%begin_results = .true.
      this%begyr = this%start_time(1)

      if (ctl_data%prms_warmup%values(1) > 0) this%begin_results = .false.

      this%begyr = this%begyr + ctl_data%prms_warmup%values(1)
      this%lastyear = this%begyr

      write (this%output_fmt, 9001) ctl_data%nhru%values(1)

      ! NOTE: removed checks for datatype and size

      if (this%double_vars == 1) then
        allocate(this%nhru_var_dble(ctl_data%nhru%values(1), nhru_out_vars))
        this%nhru_var_dble = 0.0D0
      endif

      this%daily_flag = 0
      if (ANY([DAILY, DAILY_MONTHLY]==nhru_out_freq)) then
        this%daily_flag = 1
        allocate(this%dailyunit(nhru_out_vars))
      endif

      this%monthly_flag = 0
      if (ANY([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY]==nhru_out_freq)) this%monthly_flag = 1

      if (ANY([MEAN_YEARLY, YEARLY]==nhru_out_freq)) then
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
    subroutine run_nhru_summary(this, ctl_data, model_time, model_basin, climate)
      use Control_class, only: Control
      use PRMS_SET_TIME, only: Time
      use PRMS_BASIN, only: Basin
      use PRMS_CLIMATEVARS, only: Climateflow
      ! use PRMS_MODULE, only: Start_month, Start_day, End_year, End_month, End_day
                             ! NhruOutVars, NhruOut_freq, NhruOutVar_names
      ! use PRMS_BASIN, only: Active_hrus, Hru_route_order
      ! use PRMS_SET_TIME, only: Nowyear, Nowmonth, Nowday, last_day_of_month
      ! use UTILS_PRMS, only: read_error
      implicit none

      class(Nhru_summary), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Time), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(in) :: climate

      ! FUNCTIONS AND SUBROUTINES
      INTRINSIC SNGL, DBLE

      ! Local Variables
      integer(i32) :: j
      integer(i32) :: chru
      integer(i32) :: jj
      logical :: write_month
      logical :: write_year
      logical :: last_day

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
                nhruOutVars => ctl_data%nhruOutVars%values(1), &
                nhruOut_freq => ctl_data%nhruOut_freq%values(1), &
                nhruOutVar_names => ctl_data%nhruOutVar_names%values, &
                nhru => ctl_data%nhru%values(1))

        if (.not. this%begin_results) then
          if (curr_year == this%begyr .and. curr_month == st_month .and. curr_day == st_day) then
            this%begin_results = .true.
          else
            RETURN
          endif
        endif


        !-----------------------------------------------------------------------
        do jj = 1, nhruOutVars
          select case(nhruOutVar_names(jj)%s)

            case('hru_ppt')
              this%nhru_var_daily(:, jj) = climate%hru_ppt
            case('hru_rain')
              this%nhru_var_daily(:, jj) = climate%hru_rain
            case('hru_snow')
              this%nhru_var_daily(:, jj) = climate%hru_snow
            case('potet')
              this%nhru_var_daily(:, jj) = climate%potet
            case('prmx')
              this%nhru_var_daily(:, jj) = climate%prmx
            case('swrad')
              this%nhru_var_daily(:, jj) = climate%swrad
            case('tavgc')
              this%nhru_var_daily(:, jj) = climate%tavgc
            case('tavgf')
              this%nhru_var_daily(:, jj) = climate%tavgf
            case('tmaxc')
              this%nhru_var_daily(:, jj) = climate%tmaxc
            case('tmaxf')
              this%nhru_var_daily(:, jj) = climate%tmaxf
            case('tminc')
              this%nhru_var_daily(:, jj) = climate%tminc
            case('tminf')
              this%nhru_var_daily(:, jj) = climate%tminf
            case('tmax_hru')
              this%nhru_var_daily(:, jj) = climate%tmax_hru
            case('tmin_hru')
              this%nhru_var_daily(:, jj) = climate%tmin_hru
            case default
              ! pass
          end select
        enddo

        write_month = .false.
        write_year = .false.
        if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        ! if (NhruOut_freq > 4) then
          last_day = .false.

          if (curr_year == en_year .and. curr_month == en_month .and. curr_day == en_day) then
            last_day = .true.
          endif

          if (this%lastyear /= curr_year .or. last_day) then
            if ((curr_month == st_month .and. curr_day == st_day) .or. last_day) then
              do jj = 1, nhruOutVars
                if (nhruOut_freq == MEAN_YEARLY) then
                ! if (NhruOut_freq == 5) then
                  do j = 1, model_basin%active_hrus
                    chru = model_basin%hru_route_order(j)
                    this%nhru_var_yearly(chru, jj) = this%nhru_var_yearly(chru, jj) / this%yeardays
                  enddo
                endif
                write (this%yearlyunit(jj), this%output_fmt3) this%lastyear, (this%nhru_var_yearly(j, jj), j=1, nhru)
              enddo

              this%nhru_var_yearly = 0.0
              this%yeardays = 0
              this%lastyear = curr_year
            endif
          endif

          this%yeardays = this%yeardays + 1
        elseif (this%monthly_flag == 1) then
            ! check for last day of month and simulation
            if (curr_day == model_time%last_day_of_month(curr_month)) then
              write_month = .true.
            elseif (curr_year == en_year .and. curr_month == en_month .and. curr_day == en_day) then
              write_month = .true.
            endif

            this%monthdays = this%monthdays + 1.0
        endif

        if (this%double_vars == 1) then
          do jj = 1, nhruOutVars
            ! TODO: figure out how to handle this
            ! if (Nhru_var_type(jj) == 3) then
            !   do j = 1, model_time%active_hrus
            !     chru = model_time%hru_route_order(j)
            !     this%nhru_var_daily(chru, jj) = SNGL(this%nhru_var_dble(chru, jj))
            !   enddo
            ! endif
          enddo
        endif

        if (ANY([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        ! if (NhruOut_freq > 4) then
          do jj = 1, nhruOutVars
            do j = 1, model_basin%active_hrus
              chru = model_basin%hru_route_order(j)
              this%nhru_var_yearly(chru, jj) = this%nhru_var_yearly(chru, jj) + DBLE(this%nhru_var_daily(chru, jj))
            enddo
          enddo
          RETURN
        endif

        if (this%monthly_flag == 1) then
          do jj = 1, nhruOutVars
            do j = 1, model_basin%active_hrus
              chru = model_basin%hru_route_order(j)
              this%nhru_var_monthly(chru, jj) = this%nhru_var_monthly(chru, jj) + DBLE(this%nhru_var_daily(chru, jj))

              if (write_month) then
                if (nhruOut_freq == MEAN_MONTHLY) then
                  this%nhru_var_monthly(chru, jj) = this%nhru_var_monthly(chru, jj) / this%monthdays
                endif
              endif
            enddo
          enddo
        endif

        do jj = 1, nhruOutVars
          if (this%daily_flag == 1) then
            write (this%dailyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
                                                        (this%nhru_var_daily(j, jj), j = 1, nhru)
          endif

          if (write_month) then
            write (this%monthlyunit(jj), this%output_fmt) curr_year, curr_month, curr_day, &
                                                          (this%nhru_var_monthly(j, jj), j = 1, nhru)
          endif
        enddo

        if (write_month) then
          this%monthdays = 0.0
          this%nhru_var_monthly = 0.0
        endif
      end associate
    end subroutine
end module
