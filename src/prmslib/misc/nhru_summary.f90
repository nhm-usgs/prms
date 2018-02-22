!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
MODULE PRMS_NHRU_SUMMARY
    use kinds_mod, only: r4, r8, i4, i8
    USE prms_constants, ONLY:MAXFILE_LENGTH
    IMPLICIT NONE

    ! Module Variables
    INTEGER(i4) :: Begin_results
    INTEGER(i4) :: Begyr
    INTEGER(i4) :: Lastyear
    INTEGER(i4), ALLOCATABLE :: Dailyunit(:)
    INTEGER(i4), ALLOCATABLE :: Nc_vars(:)
    INTEGER(i4), ALLOCATABLE :: Nhru_var_type(:)
    REAL(r4), ALLOCATABLE :: Nhru_var_daily(:, :)
    real(r8), ALLOCATABLE :: Nhru_var_dble(:, :)

    CHARACTER(LEN=48) :: Output_fmt
    character(len=48) :: Output_fmt2
    character(len=48) :: Output_fmt3
    CHARACTER(LEN=12), SAVE :: MODNAME

    INTEGER(i4) :: Daily_flag
    INTEGER(i4) :: Double_vars
    INTEGER(i4) :: Yeardays
    INTEGER(i4) :: Monthly_flag

    real(r8) :: Monthdays
    INTEGER(i4), ALLOCATABLE :: Monthlyunit(:)
    INTEGER(i4), ALLOCATABLE :: Yearlyunit(:)
    real(r8), ALLOCATABLE :: Nhru_var_monthly(:, :)
    real(r8), ALLOCATABLE :: Nhru_var_yearly(:, :)

    ! Parameters
    INTEGER(i4), ALLOCATABLE :: Nhm_id(:)

    private :: nhru_summarydecl, nhru_summaryinit, nhru_summaryrun
    public :: nhru_summary

    contains
        !     ******************************************************************
        !     nhru results module
        !     ******************************************************************
        SUBROUTINE nhru_summary(dim_data, ctl_data, param_data, var_data)
            USE PRMS_MODULE, ONLY: Process
            use dimensions_mod, only: dimension_list
            use control_ll_mod, only: control_list
            use parameter_arr_mod, only: parameter_arr_t
            use variables_arr_mod, only: variables_arr_t
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data
            type(control_list), intent(in) :: ctl_data
            type(parameter_arr_t), intent(inout) :: param_data
            type(variables_arr_t), intent(in) :: var_data

            !*******************************************************************
            IF (Process == 'run') THEN
                CALL nhru_summaryrun(var_data)
            ELSEIF (Process == 'declare') THEN
                CALL nhru_summarydecl(dim_data, ctl_data, param_data)
            ELSEIF (Process == 'init') THEN
                CALL nhru_summaryinit(ctl_data, param_data, var_data)
            ENDIF
        END SUBROUTINE nhru_summary

        !***********************************************************************
        !     declare parameters and variables
        !***********************************************************************
        SUBROUTINE nhru_summarydecl(dim_data, ctl_data, param_data)
            USE PRMS_MODULE, ONLY: Model, Nhru, print_module, &  ! , Inputerror_flag, NhruOutON_OFF
                                   NhruOutVars, NhruOut_freq, NhruOutVar_names, NhruOutBaseFileName
            use UTILS_PRMS, only: read_error
            use dimensions_mod, only: dimension_list
            use control_ll_mod, only: control_list
            use parameter_arr_mod, only: parameter_arr_t

            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data
            type(control_list), intent(in) :: ctl_data
            type(parameter_arr_t), intent(inout) :: param_data

            ! Functions
            INTRINSIC CHAR

            ! Local Variables
            integer(i4) :: NhruOutON_OFF
            CHARACTER(LEN=:), allocatable, SAVE :: Version_nhru_summary

            !***********************************************************************
            Version_nhru_summary = 'nhru_summary.f90 2017-09-29 13:49:00Z'
            CALL print_module(Version_nhru_summary, 'Nhru Output Summary         ', 90)
            MODNAME = 'nhru_summary'

            call ctl_data%get_data('nhruOutON_OFF', NhruOutON_OFF)

            NhruOutVars = 0
            if (ctl_data%exists('nhruOutVars')) call ctl_data%get_data('nhruOutVars', NhruOutVars)

            NhruOut_freq = 0
            if (ctl_data%exists('nhruOut_freq')) call ctl_data%get_data('nhruOut_freq', NhruOut_freq)
            ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total

            IF (NhruOutVars == 0) THEN
                IF (Model /= 99) THEN
                    PRINT *, 'ERROR, nhru_summary requested with nhruOutVars equal 0'
                    STOP
                    !          PRINT *, 'no nhru_summary output is produced'
                    !          NhruOutON_OFF = 0
                    ! Inputerror_flag = 1
                    RETURN
                ENDIF
            ELSE
                ALLOCATE (NhruOutVar_names(NhruOutVars), Nhru_var_type(NhruOutVars), Nc_vars(NhruOutVars))

                call ctl_data%get_data('nhruOutVar_names', NhruOutVar_names, missing_stop=.true.)
                call ctl_data%get_data('nhruOutBaseFileName', NhruOutBaseFileName, missing_stop=.true.)
            ENDIF

            IF (NhruOutON_OFF == 2) THEN
                ALLOCATE (Nhm_id(Nhru))

                IF (param_data%declparam(MODNAME, 'nhm_id', 'nhru', 'integer', '1', '1', '9999999', &
                              'National Hydrologic Model HRU ID', 'National Hydrologic Model HRU ID', &
                              'none', dim_data) /= 0) CALL read_error(1, 'nhm_id')
            ENDIF
        END SUBROUTINE nhru_summarydecl

        !***********************************************************************
        !     Initialize module values
        !***********************************************************************
        SUBROUTINE nhru_summaryinit(ctl_data, param_data, var_data)
            use prms_constants, only: MAXFILE_LENGTH
            USE PRMS_MODULE, ONLY: Nhru, Start_year, Prms_warmup, &
                                   NhruOutVars, NhruOut_freq, NhruOutVar_names, &  ! , NhruOutON_OFF
                                   NhruOutBaseFileName
            use UTILS_PRMS, only: read_error, PRMS_open_output_file
            use control_ll_mod, only: control_list
            use parameter_arr_mod, only: parameter_arr_t
            use variables_arr_mod, only: variables_arr_t
            IMPLICIT NONE

            INTRINSIC ABS

            type(control_list), intent(in) :: ctl_data
            type(parameter_arr_t), intent(in) :: param_data
            type(variables_arr_t), intent(in) :: var_data

            ! Local Variables
            integer(i4) :: NhruOutON_OFF

            INTEGER(i4) :: ios
            INTEGER(i4) :: ierr
            INTEGER(i4) :: size
            INTEGER(i4) :: jj
            INTEGER(i4) :: j
            CHARACTER(LEN=MAXFILE_LENGTH) :: fileName

            !***********************************************************************
            call ctl_data%get_data('nhruOutON_OFF', NhruOutON_OFF)

            Begin_results = 1
            Begyr = Start_year

            IF (Prms_warmup > 0) Begin_results = 0

            Begyr = Begyr + Prms_warmup
            Lastyear = Begyr

            WRITE (Output_fmt, 9001) Nhru

            Double_vars = 0
            ierr = 0

            DO jj = 1, NhruOutVars
                Nhru_var_type(jj) = var_data%getvartype(NhruOutVar_names(jj)%str)
                IF (Nhru_var_type(jj) == 3) Double_vars = 1

                IF (Nhru_var_type(jj) /= 2 .AND. Nhru_var_type(jj) /= 3) THEN
                    PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)
                    PRINT *, '       only real or double variables allowed'
                    ierr = 1
                ENDIF

                size = var_data%getvarsize(NhruOutVar_names(jj)%str)
                IF (size /= Nhru) THEN
                    PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)
                    PRINT *, '       only variables dimensioned by nhru, nssr, or ngw allowed'
                    ierr = 1
                ENDIF
            ENDDO
            IF (ierr == 1) STOP

            IF (Double_vars == 1) THEN
                ALLOCATE (Nhru_var_dble(Nhru, NhruOutVars))
                Nhru_var_dble = 0.0D0
            ENDIF

            Daily_flag = 0
            IF (NhruOut_freq == 1 .OR. NhruOut_freq == 3) THEN
                Daily_flag = 1
                ALLOCATE (Dailyunit(NhruOutVars))
            ENDIF

            Monthly_flag = 0
            IF (NhruOut_freq == 2 .OR. NhruOut_freq == 3 .OR. NhruOut_freq == 4) Monthly_flag = 1

            IF (NhruOut_freq > 4) THEN
                Yeardays = 0
                ALLOCATE (Nhru_var_yearly(Nhru, NhruOutVars), Yearlyunit(NhruOutVars))
                Nhru_var_yearly = 0.0D0
                WRITE (Output_fmt3, 9003) Nhru
            ELSEIF (Monthly_flag == 1) THEN
                Monthdays = 0.0D0
                ALLOCATE (Nhru_var_monthly(Nhru, NhruOutVars), Monthlyunit(NhruOutVars))
                Nhru_var_monthly = 0.0D0
            ENDIF

            WRITE (Output_fmt2, 9002) Nhru
            ALLOCATE (Nhru_var_daily(Nhru, NhruOutVars))
            Nhru_var_daily = 0.0

            DO jj = 1, NhruOutVars
                IF (Daily_flag == 1) THEN
                    fileName = NhruOutBaseFileName // &
                            NhruOutVar_names(jj)%str // '.csv'
                    !print *, fileName
                    CALL PRMS_open_output_file(Dailyunit(jj), fileName, 'xxx', 0, ios)
                    IF (ios /= 0) STOP 'in nhru_summary'
                    IF (NhruOutON_OFF < 2) WRITE (Dailyunit(jj), Output_fmt2) (j, j = 1, Nhru)
                ENDIF

                IF (NhruOut_freq == 5) THEN
                    fileName = NhruOutBaseFileName // &
                            NhruOutVar_names(jj)%str // '_meanyearly.csv'
                    CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
                    IF (ios /= 0) STOP 'in nhru_summary, mean yearly'
                    IF (NhruOutON_OFF < 2) WRITE (Yearlyunit(jj), Output_fmt2) (j, j = 1, Nhru)
                ELSEIF (NhruOut_freq == 6) THEN
                    fileName = NhruOutBaseFileName // &
                            NhruOutVar_names(jj)%str // '_yearly.csv'
                    CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
                    IF (ios /= 0) STOP 'in nhru_summary, yearly'
                    WRITE (Yearlyunit(jj), Output_fmt2) (j, j = 1, Nhru)
                ELSEIF (Monthly_flag == 1) THEN
                    IF (NhruOut_freq == 4) THEN
                        fileName = NhruOutBaseFileName // &
                                NhruOutVar_names(jj)%str // &
                                &                 '_meanmonthly.csv'
                    ELSE
                        fileName = NhruOutBaseFileName // &
                                NhruOutVar_names(jj)%str // '_monthly.csv'
                    ENDIF
                    !print *, fileName
                    CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', 0, ios)
                    IF (ios /= 0) STOP 'in nhru_summary, monthly'
                    IF (NhruOutON_OFF < 2) WRITE (Monthlyunit(jj), Output_fmt2) (j, j = 1, Nhru)
                ENDIF
            ENDDO

            IF (NhruOutON_OFF == 2) THEN
                IF (param_data%getparam(MODNAME, 'nhm_id', Nhru, 'integer', Nhm_id) /= 0) CALL read_error(2, 'nhm_id')

                DO jj = 1, NhruOutVars
                    IF (Daily_flag == 1) WRITE (Dailyunit(jj), Output_fmt2) (Nhm_id(j), j = 1, Nhru)
                    IF (NhruOut_freq == 5) THEN
                        WRITE (Yearlyunit(jj), Output_fmt2) (Nhm_id(j), j = 1, Nhru)
                    ELSEIF (Monthly_flag == 1) THEN
                        WRITE (Monthlyunit(jj), Output_fmt2) (Nhm_id(j), j = 1, Nhru)
                    ENDIF
                ENDDO
            ENDIF

            9001 FORMAT ('(I4, 2(''-'',I2.2),', I6, '('',''ES10.3))')
            9002 FORMAT ('("Date "', I6, '('',''I6))')
            9003 FORMAT ('(I4,', I6, '('',''ES10.3))')

        END SUBROUTINE nhru_summaryinit

        !***********************************************************************
        !     Output set of declared variables in R compatible format
        !***********************************************************************
        SUBROUTINE nhru_summaryrun(var_data)
            USE PRMS_MODULE, ONLY: Nhru, Start_month, Start_day, End_year, End_month, End_day, &
                                   NhruOutVars, NhruOut_freq, NhruOutVar_names ! , NhruOutBaseFileName
            USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
            USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday, last_day_of_month   ! , Modays
            use UTILS_PRMS, only: read_error
            use variables_arr_mod, only: variables_arr_t
            IMPLICIT NONE

            type(variables_arr_t), intent(in) :: var_data

            ! FUNCTIONS AND SUBROUTINES
            INTRINSIC SNGL, DBLE

            ! Local Variables
            INTEGER(i4) :: j
            INTEGER(i4) :: i
            INTEGER(i4) :: jj
            INTEGER(i4) :: write_month
            INTEGER(i4) :: write_year
            INTEGER(i4) :: last_day

            !***********************************************************************
            IF (Begin_results == 0) THEN
                IF (Nowyear == Begyr .AND. Nowmonth == Start_month .AND. Nowday == Start_day) THEN
                    Begin_results = 1
                ELSE
                    RETURN
                ENDIF
            ENDIF

            !-----------------------------------------------------------------------
            ! need getvars for each variable (only can have short string)
            DO jj = 1, NhruOutVars
                IF (Nhru_var_type(jj) == 2) THEN
                    CALL var_data%getvar_real(MODNAME, NhruOutVar_names(jj)%str, Nhru, Nhru_var_daily(1, jj))
                ELSEIF (Nhru_var_type(jj) == 3) THEN  ! probably don't need double
                    CALL var_data%getvar_dble(MODNAME, NhruOutVar_names(jj)%str, Nhru, Nhru_var_dble(1, jj))
                ENDIF
            ENDDO

            write_month = 0
            write_year = 0
            IF (NhruOut_freq > 4) THEN
                last_day = 0

                IF (Nowyear == End_year .AND. Nowmonth == End_month .AND. Nowday == End_day) last_day = 1

                IF (Lastyear /= Nowyear .OR. last_day == 1) THEN
                    IF ((Nowmonth == Start_month .AND. Nowday == Start_day) .OR. last_day == 1) THEN
                        DO jj = 1, NhruOutVars
                            IF (NhruOut_freq == 5) THEN
                                DO j = 1, Active_hrus
                                    i = Hru_route_order(j)
                                    Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj) / Yeardays
                                ENDDO
                            ENDIF
                            WRITE (Yearlyunit(jj), Output_fmt3) Lastyear, (Nhru_var_yearly(j, jj), j=1, Nhru)
                        ENDDO

                        Nhru_var_yearly = 0.0D0
                        Yeardays = 0
                        Lastyear = Nowyear
                    ENDIF
                ENDIF
                Yeardays = Yeardays + 1
            ELSEIF (Monthly_flag == 1) THEN
                ! check for last day of month and simulation
                if (Nowday == last_day_of_month(Nowmonth)) then
                    write_month = 1
                ELSEIF (Nowyear == End_year) THEN
                    IF (Nowmonth == End_month) THEN
                        IF (Nowday == End_day) write_month = 1
                    ENDIF
                ENDIF
                Monthdays = Monthdays + 1.0D0
            ENDIF

            IF (Double_vars == 1) THEN
                DO jj = 1, NhruOutVars
                    IF (Nhru_var_type(jj) == 3) THEN
                        DO j = 1, Active_hrus
                            i = Hru_route_order(j)
                            Nhru_var_daily(i, jj) = SNGL(Nhru_var_dble(i, jj))
                        ENDDO
                    ENDIF
                ENDDO
            ENDIF

            IF (NhruOut_freq > 4) THEN
                DO jj = 1, NhruOutVars
                    DO j = 1, Active_hrus
                        i = Hru_route_order(j)
                        Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj) + DBLE(Nhru_var_daily(i, jj))
                    ENDDO
                ENDDO
                RETURN
            ENDIF

            IF (Monthly_flag == 1) THEN
                DO jj = 1, NhruOutVars
                    DO j = 1, Active_hrus
                        i = Hru_route_order(j)
                        Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) + DBLE(Nhru_var_daily(i, jj))
                        IF (write_month == 1) THEN
                            IF (NhruOut_freq == 4) Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) / Monthdays
                        ENDIF
                    ENDDO
                ENDDO
            ENDIF

            DO jj = 1, NhruOutVars
                IF (Daily_flag == 1) WRITE (Dailyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, &
                        (Nhru_var_daily(j, jj), j = 1, Nhru)
                IF (write_month == 1) WRITE (Monthlyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, &
                        (Nhru_var_monthly(j, jj), j = 1, Nhru)
            ENDDO

            IF (write_month == 1) THEN
                Monthdays = 0.0D0
                Nhru_var_monthly = 0.0D0
            ENDIF
        END SUBROUTINE nhru_summaryrun

END MODULE PRMS_NHRU_SUMMARY
