submodule (PRMS_NHRU_SUMMARY_PTR) sm_nhru_summary_ptr
use netcdf

contains
  !**************************************************************************
  ! Climateflow constructor
  module function constructor_Nhru_summary_ptr(ctl_data, model_basin, model_time) result(this)
    use prms_constants, only: MAXFILE_LENGTH, dp
    use UTILS_PRMS, only: PRMS_open_output_file, print_module_info
    implicit none

    type(Nhru_summary_ptr) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time

    ! Local variables
    ! integer(i32) :: ios
    ! integer(i32) :: jj
    ! integer(i32) :: j
    character(len=MAXFILE_LENGTH) :: fileName
    character(len=:), allocatable :: suffix

    ! ------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              end_time => ctl_data%end_time%values, &
              nhruOutON_OFF => ctl_data%nhruOutON_OFF%value, &
              nhruOut_freq => ctl_data%nhruOut_freq%value, &
              nhruOutVars => ctl_data%nhruOutVars%value, &

              nhru => model_basin%nhru, &
              nhm_id => model_basin%nhm_id)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      end if

      ! NOTE: NhruOutON_OFF=2 is an undocumented feature.
      !       Parameter nhm_id is needed for this.

      ! TODO: Handle nsegment and basin output
      if (nhruOutVars == 0) then
        print *, 'ERROR, Summary requested with nhruOutVars equal 0'
        STOP
        return
      end if

      this%begin_results = .true.
      this%begyr = start_time(YEAR)

      if (ctl_data%prms_warmup%value > 0) this%begin_results = .false.

      this%begyr = this%begyr + ctl_data%prms_warmup%value
      this%prioryear = this%begyr

      this%is_daily_freq = .false.
      this%is_monthly_freq = .false.
      this%is_yearly_freq = .false.

      ! NOTE: For now outputting both daily and monthly (DAILY_MONTHLY) is disabled
      ! if (any([DAILY, DAILY_MONTHLY]==nhruOut_freq)) then
      if (any([DAILY]==nhruOut_freq)) then
        this%is_daily_freq = .true.
        suffix = 'daily'
      ! else if (any([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY]==nhruOut_freq)) then
      else if (any([MONTHLY, MEAN_MONTHLY]==nhruOut_freq)) then
        this%is_monthly_freq = .true.
        this%time_index_monthly = 1

        if (nhruOut_freq == MEAN_MONTHLY) then
          suffix = 'meanmonthly'
        else
          suffix = 'monthly'
        end if
      else if (any([MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        this%is_yearly_freq = .true.
        this%time_index_yearly = 1
        this%yeardays = 0

        if (nhruOut_freq == MEAN_YEARLY) then
          suffix = 'meanyearly'
        else if (nhruOut_freq == YEARLY) then
          suffix = 'yearly'
        end if
      end if

      allocate(this%nhru_outvar_id(nhruOutVars))
      allocate(this%nhru_var_daily(nhruOutVars))

      if (any([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY, MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        write(*,*) '-- allocated nhru_var_summary'
        allocate(this%nhru_var_summary(nhru, nhruOutVars))
        this%nhru_var_summary = 0.0_dp
      end if

      fileName = ctl_data%nhruOutBaseFileName%values(1)%s // suffix // '.nc'

      ! Create the output netcdf file
      call this%create_netcdf(ctl_data, model_basin, model_time, fileName)
    end associate
  end function



  !***********************************************************************
  !     Output set of declared variables in R compatible format
  !***********************************************************************
  module subroutine run_Nhru_summary_ptr(this, ctl_data, model_time, model_basin)
    use conversions_mod, only: c_to_f
    use prms_constants, only: dp
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin

    ! Local Variables
    integer(i32) :: j
    integer(i32) :: jj
    integer(i32) :: chru
    logical :: write_month
    logical :: write_year
    logical :: last_day_of_simulation

    integer(i32) :: start(2)
    integer(i32) :: rcount(2)

    !***********************************************************************
    associate(curr_date => model_time%Nowtime, &
              curr_year => model_time%Nowtime(YEAR), &
              curr_month => model_time%Nowtime(MONTH), &
              curr_day => model_time%Nowtime(DAY), &
              days_since_start => model_time%days_since_start, &

              st_date => ctl_data%start_time%values, &
              st_year => ctl_data%start_time%values(YEAR), &
              st_month => ctl_data%start_time%values(MONTH), &
              st_day => ctl_data%start_time%values(DAY), &
              en_date => ctl_data%end_time%values, &
              en_year => ctl_data%end_time%values(YEAR), &
              en_month => ctl_data%end_time%values(MONTH), &
              en_day => ctl_data%end_time%values(DAY), &
              nhruOutVars => ctl_data%nhruOutVars%value, &
              nhruOut_freq => ctl_data%nhruOut_freq%value, &
              nhruOutVar_names => ctl_data%nhruOutVar_names%values, &

              nhru => model_basin%nhru, &
              active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order)

      if (.not. this%begin_results) then
        if (curr_year == this%begyr .and. curr_month == st_month .and. curr_day == st_day) then
          this%begin_results = .true.
        else
          return
        end if
      end if

      write_month = .false.
      write_year = .false.

      last_day_of_simulation = all(curr_date .eq. en_date)

      if (this%is_yearly_freq) then
        ! ===================================
        ! Yearly summary
        if (curr_year /= this%prioryear .or. last_day_of_simulation) then
          ! Check if a new year is starting; if so, write out the prior year and reset.
          if ((curr_month == st_month .and. curr_day == st_day) .or. last_day_of_simulation) then
            if (nhruOut_freq == MEAN_YEARLY) then
              ! Compute the mean summary values
              this%nhru_var_summary = this%nhru_var_summary / this%yeardays
            end if

            ! Write the yearly summary values
            rcount = (/ nhru, 1 /)
            start = (/ 1, this%time_index_yearly /)

            ! Write the timestep
            call this%err_check(nf90_put_var(this%nhru_file_hdl, this%time_varid, days_since_start, &
                                            start=[this%time_index_yearly]))

            ! Write the output variables
            do jj=1, nhruOutVars
              call this%err_check(nf90_put_var(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                              this%nhru_var_summary(:, jj), &
                                              start=start, count=rcount))
            end do

            this%time_index_yearly = this%time_index_yearly + 1
            this%nhru_var_summary = 0.0_dp
            this%yeardays = 0
            this%prioryear = curr_year
          end if
        end if

        this%yeardays = this%yeardays + 1

        ! Compute running totals
        do jj=1, nhruOutVars
          do concurrent (j=1:active_hrus)
            chru = hru_route_order(j)
            if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
              this%nhru_var_summary(chru, jj) = this%nhru_var_summary(chru, jj) + dble(this%nhru_var_daily(jj)%ptr_r32(chru))
            else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
              this%nhru_var_summary(chru, jj) = this%nhru_var_summary(chru, jj) + this%nhru_var_daily(jj)%ptr_r64(chru)
            end if
          end do
        enddo
      elseif (this%is_monthly_freq) then
        ! ===================================
        ! Monthly summary

        ! Check for last day of month and simulation
        if (curr_day == model_time%last_day_of_month(curr_month) .or. last_day_of_simulation) then
          write_month = .true.
        end if

        ! Compute running totals
        do jj=1, nhruOutVars
          do concurrent (j=1:active_hrus)
            chru = hru_route_order(j)
            if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
              this%nhru_var_summary(chru, jj) = this%nhru_var_summary(chru, jj) + dble(this%nhru_var_daily(jj)%ptr_r32(chru))
            else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
              this%nhru_var_summary(chru, jj) = this%nhru_var_summary(chru, jj) + this%nhru_var_daily(jj)%ptr_r64(chru)
            end if
          end do
        enddo

        if (write_month) then
          if (nhruOut_freq == MEAN_MONTHLY) then
            ! Compute the mean values for the month
            this%nhru_var_summary = this%nhru_var_summary / model_time%last_day_of_month(curr_month)
          end if

          ! Write out the monthly summary values
          rcount = (/ nhru, 1 /)
          start = (/ 1, this%time_index_monthly /)

          ! Write the timestep
          call this%err_check(nf90_put_var(this%nhru_file_hdl, this%time_varid, days_since_start, &
                                           start=[this%time_index_monthly]))

          ! Write the output variables
          do jj=1, nhruOutVars
            call this%err_check(nf90_put_var(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                             this%nhru_var_summary(:, jj), &
                                             start=start, count=rcount))
          end do

          this%time_index_monthly = this%time_index_monthly + 1
          this%nhru_var_summary = 0.0_dp
        end if
      end if

      if (this%is_daily_freq) then
        rcount = (/ nhru, 1 /)
        start = (/ 1, days_since_start+1 /)

        ! Write the timestep
        call this%err_check(nf90_put_var(this%nhru_file_hdl, this%time_varid, days_since_start, &
                                        start=[days_since_start+1]))
        do jj=1, nhruOutVars
          ! Write to netcdf file
          if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
            ! Floats
            call this%err_check(nf90_put_var(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                             this%nhru_var_daily(jj)%ptr_r32, &
                                             start=start, count=rcount))
          else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
            ! Doubles
            call this%err_check(nf90_put_var(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                             this%nhru_var_daily(jj)%ptr_r64, &
                                             start=start, count=rcount))
          else
            write(*, *) MODNAME, 'No output array for variable index:', jj
          end if
        end do
      end if
    end associate
  end subroutine


  module subroutine create_netcdf(this, ctl_data, model_basin, model_time, filename)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    character(len=*), intent(in) :: filename

    ! When we create netCDF files, variables and dimensions, we get back
    ! an ID for each one.
    integer(i32) :: dimids(2)
    integer(i32) :: nhru_dimid
    integer(i32) :: time_dimid
    integer(i32) :: nhm_id_varid

    integer(i32) :: jj
    character(len=:), allocatable :: outvar_dimensions
    integer(i32) :: outvar_datatype
    ! character(len=:), allocatable :: outvar_datatype
    character(len=:), allocatable :: outvar_desc
    character(len=:), allocatable :: outvar_name
    character(len=:), allocatable :: outvar_units

    character(len=:), allocatable :: days_since

    ! ------------------------------------------------------------------------
    associate(nhruOutVars => ctl_data%nhruOutVars%value, &
              output_variables => ctl_data%output_variables, &

              nhru => model_basin%nhru, &
              nhm_id => model_basin%nhm_id, &

              days_in_model => model_time%days_in_model, &
              months_in_model => model_time%months_in_model, &
              start_string => model_time%start_string, &
              years_in_model => model_time%years_in_model)

      ! Create netcdf file
      ! NOTE: using NF90_NETCDF4 with a fixed time dimension gives slightly
      !       slower writing but much smaller filesizes.
      !       Using NF90_CLOBBER (netcdf3) with an unlimited time dimension
      !       results in faster writing but larger file sizes (file sizes are
      !       still smaller than ASCII files).
      call this%err_check(nf90_create(filename, NF90_NETCDF4, this%nhru_file_hdl))

      ! Define the dimensions. NetCDF will hand back an ID for each.
      ! call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', NF90_UNLIMITED, time_dimid))
      if (this%is_daily_freq) then
        call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', days_in_model, time_dimid))
      else if (this%is_monthly_freq) then
        call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', months_in_model, time_dimid))
      else if (this%is_yearly_freq) then
        call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', years_in_model, time_dimid))
      end if

      call this%err_check(nf90_def_dim(this%nhru_file_hdl, "nhru", nhru, nhru_dimid))

      ! The dimids array is used to pass the IDs of the dimensions of the
      ! variables. Note that in fortran arrays are stored in column-major format.
      dimids = (/ nhru_dimid, time_dimid /)

      ! Define the variable for the time dimension
      days_since = 'days since ' // start_string // ' 00:00:00'
      call this%err_check(nf90_def_var(this%nhru_file_hdl, &
                                       'time', NF90_FLOAT, time_dimid, this%time_varid))
      call this%err_check(nf90_put_att(this%nhru_file_hdl, this%time_varid, &
                                       'long_name', 'time'))
      call this%err_check(nf90_put_att(this%nhru_file_hdl, this%time_varid, &
                                       'calendar', 'standard'))
      call this%err_check(nf90_put_att(this%nhru_file_hdl, this%time_varid, &
                                       'units', days_since))

      ! Always include nhm_id as a variable
      call this%err_check(nf90_def_var(this%nhru_file_hdl, &
                                       'nhm_id', NF90_INT, nhru_dimid, nhm_id_varid))
      call this%err_check(nf90_put_att(this%nhru_file_hdl, nhm_id_varid, &
                                       'long_name', 'NHM HRU id'))
      call this%err_check(nf90_put_att(this%nhru_file_hdl, nhm_id_varid, &
                                       'units', 'none'))

      ! Define the nhru-based output variables
      do jj = 1, nhruOutVars
        ! TODO: Figure out how to set the datatype correctly
        outvar_name = ctl_data%nhruOutVar_names%values(jj)%s

        ! Pull variable information from control class
        call output_variables%get(outvar_name, &
                                  outvar_dimensions, outvar_datatype, &
                                  outvar_desc, outvar_units)

        call this%err_check(nf90_def_var(this%nhru_file_hdl, outvar_name, &
                                         outvar_datatype, dimids, this%nhru_outvar_id(jj), &
                                         shuffle=.true., &
                                         deflate_level=5))

        ! Add attributes for each variable
        call this%err_check(nf90_put_att(this%nhru_file_hdl, &
                            this%nhru_outvar_id(jj), &
                            'description', outvar_desc))
        call this%err_check(nf90_put_att(this%nhru_file_hdl, &
                            this%nhru_outvar_id(jj), &
                            'units', outvar_units))
      end do

      ! Define global attributes
      ! TODO: Add global attributes for start and end date.

      ! End define mode. This tells netCDF we are done defining metadata.
      call this%err_check(nf90_enddef(this%nhru_file_hdl))

      ! Write the nhm_id values to the file
      call this%err_check(nf90_put_var(this%nhru_file_hdl, nhm_id_varid, nhm_id))

      ! Close the file. This frees up any internal netCDF resources
      ! associated with the file, and flushes any buffers.
      ! call this%err_check(nf90_close(this%nhru_file_hdl))
    end associate
  end subroutine


  module subroutine err_check(status)
    implicit none

    integer(i32), intent(in) :: status
      !! The status returned by a netcdf call

    ! ------------------------------------------------------------------------
    if (status /= nf90_noerr) then
      write(output_unit, *) trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine



  module subroutine set_nhru_var_r64(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - r64_outvar set'
    this%nhru_var_daily(idx)%ptr_r64 => var
  end subroutine

  module subroutine set_nhru_var_r32(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r32), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - r32_outvar set'
    this%nhru_var_daily(idx)%ptr_r32 => var
  end subroutine

  module subroutine set_nhru_var_i32(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    integer(i32), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - i32_outvar set'
    this%nhru_var_daily(idx)%ptr_i32 => var
  end subroutine

  module subroutine cleanup_Nhru_summary_ptr(this)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(in) :: this

    ! --------------------------------------------------------------------------
    ! Close the summary output file
    call this%err_check(nf90_close(this%nhru_file_hdl))
  end subroutine
end submodule
