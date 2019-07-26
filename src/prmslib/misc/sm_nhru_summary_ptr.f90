submodule (PRMS_NHRU_SUMMARY_PTR) sm_nhru_summary_ptr
use netcdf
use prms_constants, only: dp

contains
  !**************************************************************************
  ! Climateflow constructor
  module function constructor_Nhru_summary_ptr(ctl_data, model_basin, model_time) result(this)
    use prms_constants, only: MAXFILE_LENGTH
    use UTILS_PRMS, only: PRMS_open_output_file, print_module_info
    implicit none

    type(Nhru_summary_ptr) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time

    ! Local variables
    character(len=MAXFILE_LENGTH) :: fileName
    character(len=:), allocatable :: suffix

    ! ------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              end_time => ctl_data%end_time%values, &
              nhruOutON_OFF => ctl_data%nhruOutON_OFF%value, &
              nhruOut_freq => ctl_data%nhruOut_freq%value, &
              nhruOutVars => ctl_data%nhruOutVars%value)

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
      allocate(this%nhru_outvar_size(nhruOutVars))
      allocate(this%nhru_var_daily(nhruOutVars))

      if (any([MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY, MEAN_YEARLY, YEARLY]==nhruOut_freq)) then
        write(*,*) '-- allocated nhru_var_summary'
        allocate(this%nhru_var_summary(nhruOutVars))
        ! allocate(this%nhru_var_summary(nhru, nhruOutVars))
        ! this%nhru_var_summary = 0.0_dp
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
    ! use prms_constants, only: dp
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
              nsegment => model_basin%nsegment, &
              nsub => model_basin%nsub, &
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
              do jj=1, nhruOutVars
                if (allocated(this%nhru_var_summary(jj)%arr_i32)) then
                  ! NOTE: Is the mean of an integer ever needed?
                  this%nhru_var_summary(jj)%arr_i32 = this%nhru_var_summary(jj)%arr_i32 / this%yeardays
                else if (allocated(this%nhru_var_summary(jj)%arr_r32)) then
                  this%nhru_var_summary(jj)%arr_r32 = this%nhru_var_summary(jj)%arr_r32 / this%yeardays
                else if (allocated(this%nhru_var_summary(jj)%arr_r64)) then
                  this%nhru_var_summary(jj)%arr_r64 = this%nhru_var_summary(jj)%arr_r64 / this%yeardays
                end if
              end do
            end if

            ! Write the yearly summary values
            rcount = (/ nhru, 1 /)
            start = (/ 1, this%time_index_yearly /)

            ! Write the timestep
            call this%write_netcdf(this%nhru_file_hdl, this%time_varid, days_since_start, &
                                            start=[this%time_index_yearly])

            ! Write the output variables
            do jj=1, nhruOutVars
              call this%write_netcdf(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                     this%nhru_var_summary(jj), start=start, ocount=rcount)
            end do

            this%time_index_yearly = this%time_index_yearly + 1
            this%yeardays = 0
            this%prioryear = curr_year
            call this%reset_summary_vars()
          end if
        end if

        this%yeardays = this%yeardays + 1

        ! Compute yearly running totals
        do jj=1, nhruOutVars
          if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
            this%nhru_var_summary(jj)%arr_r32 = this%nhru_var_summary(jj)%arr_r32 + this%nhru_var_daily(jj)%ptr_r32
          else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
            this%nhru_var_summary(jj)%arr_r64 = this%nhru_var_summary(jj)%arr_r64 + this%nhru_var_daily(jj)%ptr_r64
          else if (associated(this%nhru_var_daily(jj)%ptr_i32)) then
            this%nhru_var_summary(jj)%arr_i32 = this%nhru_var_summary(jj)%arr_i32 + this%nhru_var_daily(jj)%ptr_i32
          end if
        enddo
      elseif (this%is_monthly_freq) then
        ! ===================================
        ! Monthly summary

        ! Check for last day of month and simulation
        if (curr_day == model_time%last_day_of_month(curr_month) .or. last_day_of_simulation) then
          write_month = .true.
        end if

        ! TODO: Could *_var_summary(nhru, nOutVars) be changed to
        !       *_var_summary(nOutVars) -> ptr_*
        !       If this worked then any sized dimension could be handled without
        !       conditionals for dimension name in the loop below.
        ! Compute running totals
        do jj=1, nhruOutVars
          if (associated(this%nhru_var_daily(jj)%ptr_r32)) then
            this%nhru_var_summary(jj)%arr_r32 = this%nhru_var_summary(jj)%arr_r32 + this%nhru_var_daily(jj)%ptr_r32
          else if (associated(this%nhru_var_daily(jj)%ptr_r64)) then
            this%nhru_var_summary(jj)%arr_r64 = this%nhru_var_summary(jj)%arr_r64 + this%nhru_var_daily(jj)%ptr_r64
          else if (associated(this%nhru_var_daily(jj)%ptr_i32)) then
            this%nhru_var_summary(jj)%arr_i32 = this%nhru_var_summary(jj)%arr_i32 + this%nhru_var_daily(jj)%ptr_i32
          end if
        enddo

        if (write_month) then
          if (nhruOut_freq == MEAN_MONTHLY) then
            ! Compute the mean values for the month
            do jj=1, nhruOutVars
              if (allocated(this%nhru_var_summary(jj)%arr_i32)) then
                this%nhru_var_summary(jj)%arr_i32 = this%nhru_var_summary(jj)%arr_i32 / model_time%last_day_of_month(curr_month)
              else if (allocated(this%nhru_var_summary(jj)%arr_r32)) then
                this%nhru_var_summary(jj)%arr_r32 = this%nhru_var_summary(jj)%arr_r32 / model_time%last_day_of_month(curr_month)
              else if (allocated(this%nhru_var_summary(jj)%arr_r64)) then
                this%nhru_var_summary(jj)%arr_r64 = this%nhru_var_summary(jj)%arr_r64 / model_time%last_day_of_month(curr_month)
              end if
            end do
          end if

          ! Write out the monthly summary values
          rcount = (/ nhru, 1 /)
          start = (/ 1, this%time_index_monthly /)

          ! Write the timestep
          call this%write_netcdf(this%nhru_file_hdl, this%time_varid, days_since_start, &
                                 start=[this%time_index_monthly])

          ! Write the output variables
          do jj=1, nhruOutVars
            call this%write_netcdf(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                   this%nhru_var_summary(jj), start=start, ocount=rcount)
          end do

          this%time_index_monthly = this%time_index_monthly + 1
          call this%reset_summary_vars()
        end if
      end if

      if (this%is_daily_freq) then
        ! rcount = (/ nhru, 1 /)
        start = (/ 1, days_since_start+1 /)

        ! Write the timestep
        call this%write_netcdf(this%nhru_file_hdl, this%time_varid, days_since_start, &
                               start=[days_since_start+1])
        do jj=1, nhruOutVars
          ! Write daily values to netcdf file
          if (this%nhru_outvar_size(jj) == 1) then
            call this%write_netcdf(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                   this%nhru_var_daily(jj), &
                                   start=[days_since_start+1])
          else
            rcount = (/ this%nhru_outvar_size(jj), 1 /)
            call this%write_netcdf(this%nhru_file_hdl, this%nhru_outvar_id(jj), &
                                   this%nhru_var_daily(jj), start=start, ocount=rcount)
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
    integer(i32) :: nsegment_dimid
    integer(i32) :: nsub_dimid
    integer(i32) :: time_dimid
    integer(i32) :: nhm_id_varid
    integer(i32) :: nhm_seg_varid
    integer(i32) :: ov_dimid
      !! dimid for current output variable being created

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
              nsegment => model_basin%nsegment, &
              nsub => model_basin%nsub, &
              nhm_id => model_basin%nhm_id, &
              nhm_seg => model_basin%nhm_seg, &

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
      ! write(*, *) 'Create output netcdf'
      call this%err_check(nf90_create(filename, NF90_NETCDF4, this%nhru_file_hdl))

      ! Define the dimensions. NetCDF will hand back an ID for each.
      ! call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', NF90_UNLIMITED, time_dimid))
      if (this%is_daily_freq) then
        ! write(*, *) '  add time dimension'
        call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', days_in_model, time_dimid))
      else if (this%is_monthly_freq) then
        call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', months_in_model, time_dimid))
      else if (this%is_yearly_freq) then
        call this%err_check(nf90_def_dim(this%nhru_file_hdl, 'time', years_in_model, time_dimid))
      end if

      ! TODO: Also add dimensions for nsegment and possibly nsub
      write(*, *) '  add nhru dimension'
      call this%err_check(nf90_def_dim(this%nhru_file_hdl, "nhru", nhru, nhru_dimid))
      write(*, *) '  add nsegment dimension'
      call this%err_check(nf90_def_dim(this%nhru_file_hdl, "nsegment", nsegment, nsegment_dimid))

      if (nsub > 0) then
        write(*, *) '  add nsub dimension'
        call this%err_check(nf90_def_dim(this%nhru_file_hdl, "nsub", nsub, nsub_dimid))
      end if

      ! The dimids array is used to pass the IDs of the dimensions of the
      ! variables. Note that in fortran arrays are stored in column-major format.
      ! dimids = (/ nhru_dimid, time_dimid /)

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

      ! Always include nhm_seg as a variable
      write(*, *) '  add nhm_seg dimension'
      call this%err_check(nf90_def_var(this%nhru_file_hdl, &
                                       'nhm_seg', NF90_INT, nsegment_dimid, nhm_seg_varid))
      call this%err_check(nf90_put_att(this%nhru_file_hdl, nhm_seg_varid, &
                                       'long_name', 'NHM segment id'))
      call this%err_check(nf90_put_att(this%nhru_file_hdl, nhm_seg_varid, &
                                       'units', 'none'))

      ! TODO: Add nsub-related variable

      ! Define the nhru-based output variables
      do jj = 1, nhruOutVars
        outvar_name = ctl_data%nhruOutVar_names%values(jj)%s

        ! Pull variable information from control class
        call output_variables%get(outvar_name, &
                                  outvar_dimensions, outvar_datatype, &
                                  outvar_desc, outvar_units)

        write(*, *) '  Create var: ', outvar_name, '    dims: ', outvar_dimensions



        ! TODO: Add variable with specific outvar_dimensions (e.g. nsegment)
        !       plus the time dimension. If the outvar_dimensions is 'one' then
        !       add the variable with just the time dimension.
        if (outvar_dimensions == 'one') then
          ! 1D - e.g. each timestep writes a scalar

          ! Save the array size for this variable to an array for later
          this%nhru_outvar_size(jj) = 1

          call this%err_check(nf90_def_var(this%nhru_file_hdl, outvar_name, &
                                           outvar_datatype, time_dimid, this%nhru_outvar_id(jj), &
                                           shuffle=.true., &
                                           deflate_level=5))
        else
          ! 2D output variable (e.g. time, nhru)
          ! Get the dimid for the outvar dimension
          call this%err_check(nf90_inq_dimid(this%nhru_file_hdl, outvar_dimensions, &
                                            ov_dimid))
          dimids = (/ ov_dimid, time_dimid /)

          ! Save the array size for this variable to an array for later
          call this%err_check(nf90_inquire_dimension(this%nhru_file_hdl, ov_dimid, &
                            len=this%nhru_outvar_size(jj)))

          call this%err_check(nf90_def_var(this%nhru_file_hdl, outvar_name, &
                                          outvar_datatype, dimids, this%nhru_outvar_id(jj), &
                                          shuffle=.true., &
                                          deflate_level=5))
        end if

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

      ! Write the nhm_seg values to the file
      call this%err_check(nf90_put_var(this%nhru_file_hdl, nhm_seg_varid, nhm_seg))

      ! TODO: Write the nhm_seg values to the file

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

  module subroutine reset_summary_vars(this)
    class(Nhru_summary_ptr), intent(inout) :: this

    integer(i32) :: jj

    ! --------------------------------------------------------------------------
    ! Reset all summary variables to zero
    if (allocated(this%nhru_var_summary)) then
      do jj=1, size(this%nhru_var_summary)
        if (allocated(this%nhru_var_summary(jj)%arr_i32)) then
          this%nhru_var_summary(jj)%arr_i32 = 0
        else if (allocated(this%nhru_var_summary(jj)%arr_r32)) then
          this%nhru_var_summary(jj)%arr_r32 = 0.0
        else if (allocated(this%nhru_var_summary(jj)%arr_r64)) then
          this%nhru_var_summary(jj)%arr_r64 = 0.0_dp
        end if
      end do
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

    if (allocated(this%nhru_var_summary)) then
      if (.not. allocated(this%nhru_var_summary(idx)%arr_r64)) then
        allocate(this%nhru_var_summary(idx)%arr_r64(size(var, 1)))
        this%nhru_var_summary(idx)%arr_r64 = 0.0_dp
      end if
    end if
  end subroutine

  module subroutine set_nhru_var_r32(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r32), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - r32_outvar set'
    this%nhru_var_daily(idx)%ptr_r32 => var

    if (allocated(this%nhru_var_summary)) then
      if (.not. allocated(this%nhru_var_summary(idx)%arr_r32)) then
        allocate(this%nhru_var_summary(idx)%arr_r32(size(var, 1)))
        this%nhru_var_summary(idx)%arr_r32 = 0.0
      end if
    end if
  end subroutine

  module subroutine set_nhru_var_i32(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    integer(i32), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - i32_outvar set'
    this%nhru_var_daily(idx)%ptr_i32 => var

    if (allocated(this%nhru_var_summary)) then
      if (.not. allocated(this%nhru_var_summary(idx)%arr_i32)) then
        allocate(this%nhru_var_summary(idx)%arr_i32(size(var, 1)))
        this%nhru_var_summary(idx)%arr_i32 = 0
      end if
    end if
  end subroutine

  module subroutine set_var_r64_0D(this, idx, var)
    implicit none

    class(Nhru_summary_ptr), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var

    ! --------------------------------------------------------------------------
    this%nhru_var_daily(idx)%scalar_r64 => var
  end subroutine

  module subroutine write_netcdf_i32_0d(this, ncid, varid, data, start, ocount)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    integer(r32), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    if (present(ocount)) then
      write(output_unit, *) MODNAME, '%write_netcdf_r32_0d() WARNING: Count not allowed for scalars.'
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    else
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    end if
  end subroutine

  module subroutine write_netcdf_i32_1d(this, ncid, varid, data, start, ocount)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    integer(i32), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    if (present(ocount)) then
      call this%err_check(nf90_put_var(ncid, varid, data, start=start, count=ocount))
    else
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    end if
  end subroutine

  module subroutine write_netcdf_r32_0d(this, ncid, varid, data, start, ocount)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r32), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    if (present(ocount)) then
      write(output_unit, *) MODNAME, '%write_netcdf_r32_0d() WARNING: Count not allowed for scalars.'
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    else
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    end if
  end subroutine

  module subroutine write_netcdf_r32_1d(this, ncid, varid, data, start, ocount)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r32), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    if (present(ocount)) then
      call this%err_check(nf90_put_var(ncid, varid, data, start=start, count=ocount))
    else
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    end if
  end subroutine

  module subroutine write_netcdf_r64_0d(this, ncid, varid, data, start, ocount)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r64), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    if (present(ocount)) then
      write(output_unit, *) MODNAME, '%write_netcdf_r32_0d() WARNING: Count not allowed for scalars.'
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    else
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    end if
  end subroutine

  module subroutine write_netcdf_r64_1d(this, ncid, varid, data, start, ocount)
    use netcdf
    implicit none

    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    real(r64), intent(in) :: data(:)
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    if (present(ocount)) then
      call this%err_check(nf90_put_var(ncid, varid, data, start=start, count=ocount))
    else
      call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    end if
  end subroutine

  module subroutine write_netcdf_var_arr(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    type(var_arrays), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    ! if (present(ocount)) then
    !   call this%err_check(nf90_put_var(ncid, varid, data, start=start, count=ocount))
    ! else
    !   call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    ! end if

    if (allocated(data%arr_r32)) then
      call this%err_check(nf90_put_var(ncid, varid, data%arr_r32, start=start, count=ocount))
    else if (allocated(data%arr_r64)) then
      call this%err_check(nf90_put_var(ncid, varid, data%arr_r64, start=start, count=ocount))
    else if (allocated(data%arr_i32)) then
      call this%err_check(nf90_put_var(ncid, varid, data%arr_i32, start=start, count=ocount))
    else
      write(*, *) MODNAME, '%run() No output array for variable id: ', varid
    end if
  end subroutine

  module subroutine write_netcdf_var_ptr(this, ncid, varid, data, start, ocount)
    class(Nhru_summary_ptr), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    type(var_ptrs), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    ! --------------------------------------------------------------------------
    ! if (present(ocount)) then
    !   call this%err_check(nf90_put_var(ncid, varid, data, start=start, count=ocount))
    ! else
    !   call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    ! end if

    if (associated(data%ptr_r32)) then
      call this%err_check(nf90_put_var(ncid, varid, data%ptr_r32, start=start, count=ocount))
    else if (associated(data%ptr_r64)) then
      call this%err_check(nf90_put_var(ncid, varid, data%ptr_r64, start=start, count=ocount))
    else if (associated(data%ptr_i32)) then
      call this%err_check(nf90_put_var(ncid, varid, data%ptr_i32, start=start, count=ocount))
    else if (associated(data%scalar_r64)) then
      call this%err_check(nf90_put_var(ncid, varid, data%scalar_r64, start=start))
    else
      write(*, *) MODNAME, '%run() No output array for variable id: ', varid
    end if
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
