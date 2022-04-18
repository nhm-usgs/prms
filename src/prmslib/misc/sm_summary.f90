submodule (PRMS_SUMMARY) sm_summary
use netcdf
use prms_constants, only: dp

contains
  !**************************************************************************
  ! Summary constructor
  module function constructor_Summary(ctl_data, model_basin, model_time) result(this)
    use prms_constants, only: MAXFILE_LENGTH
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Summary) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time

    ! Local variables
    integer(i32) :: noutvars
      !! Number of output variables in outVar_names
    character(len=MAXFILE_LENGTH) :: filename
    character(len=:), allocatable :: suffix

    ! ------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &
              start_time => ctl_data%start_time%values, &
              end_time => ctl_data%end_time%values, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              outVar_base_filename => ctl_data%outVar_base_filename%values(1), &
              prms_warmup => ctl_data%prms_warmup%value)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      end if

      ! NOTE: NhruOutON_OFF=2 is an undocumented feature.
      !       Parameter nhm_id is needed for this.
      noutvars = outVar_names%size()

      if (noutvars == 0) then
        print *, 'ERROR, Summary requested with no outVar_names'
        STOP
        return
      end if

      this%begin_results = .true.
      this%begyr = start_time(YEAR)

      if (prms_warmup > 0) this%begin_results = .false.

      this%begyr = this%begyr + prms_warmup
      ! this%prioryear = this%begyr

      ! Currently only output of daily values is supported
      suffix = 'daily'

      allocate(this%outvar_id(noutvars))
      allocate(this%outvar_size(noutvars))
      allocate(this%var_daily(noutvars))

      this%outvar_id = 0
      this%outvar_size = 0

      filename = outVar_base_filename%s // suffix // '.nc'

      ! Create the output netcdf file
      call this%create_netcdf(ctl_data, model_basin, model_time, filename)
    end associate
  end function



  !***********************************************************************
  !     Output set of declared variables in netcdf format
  !***********************************************************************
  module subroutine run_Summary(this, ctl_data, model_time, model_basin)
    use conversions_mod, only: c_to_f
    implicit none

    class(Summary), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Time_t), intent(in) :: model_time
    type(Basin), intent(in) :: model_basin

    ! Local Variables
    ! integer(i32) :: j
    integer(i32) :: jj
    ! integer(i32) :: chru
    integer(i32) :: noutvars
    ! logical :: write_month
    ! logical :: write_year
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
              outVar_names => ctl_data%outVar_names, &

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

      noutvars = outVar_names%size()
      last_day_of_simulation = all(curr_date .eq. en_date)
      start = (/ 1, days_since_start+1 /)

      ! Write the timestep
      call this%write_netcdf(this%file_hdl, this%time_varid, days_since_start, &
                              start=[days_since_start+1])
      do jj=1, noutvars
        ! Write daily values to netcdf file
        if (nhru == 1) then
          ! NOTE: hack for single-HRU runs
          rcount = (/ this%outvar_size(jj), 1 /)
          call this%write_netcdf(this%file_hdl, this%outvar_id(jj), &
                                 this%var_daily(jj), start=start, ocount=rcount)
        else if (this%outvar_size(jj) == 1) then
          call this%write_netcdf(this%file_hdl, this%outvar_id(jj), &
                                 this%var_daily(jj), start=[days_since_start+1])
        else
          rcount = (/ this%outvar_size(jj), 1 /)

          ! write(output_unit, *) 'start: ', start, '  ocount: ', rcount
          ! if (jj == 10) then
          !   write(output_unit, *) 'before write_netcdf call'
          !   write(output_unit, *) this%var_daily(jj)%ptr_r32
          ! end if

          call this%write_netcdf(this%file_hdl, this%outvar_id(jj), &
                                 this%var_daily(jj), start=start, ocount=rcount)
        end if
      end do
    end associate
  end subroutine

  module function chunk_shape_2d(dims, val_size, chunk_size) result(res)
    integer(i32) :: res(2)
    integer(i32), intent(in) :: dims(2)
    integer(i32), intent(in) :: val_size
      !! Size of each data value, in bytes
    integer(i32), intent(in) :: chunk_size
      !! Maximum chunksize desired, in bytes

    integer(i32) :: starting_chunk(2)
    integer(i32) :: starting_size
    integer(i32) :: cnk_size
    integer(i32) :: xx
    integer(i32) :: yy

    real(r32) :: chunk_percent
    real(r32) :: nvals_in_chunk
    real(r32) :: nvals_total

    ! -------------------------------------------------------------------------
    nvals_in_chunk = float(chunk_size / val_size)
    nvals_total = float(dims(1) * dims(2))
    chunk_percent = (nvals_in_chunk / nvals_total)**0.5

    starting_chunk = int(dims * chunk_percent)
    starting_size = product(starting_chunk)
    res = starting_chunk

    if (chunk_percent > 1.0) then
      res = (/ dims(1), dims(2) /)
    else
      do xx=0, 1
        do yy=0, 1
          cnk_size = int((starting_chunk(1) + xx) * (starting_chunk(2) + yy))

          if (cnk_size > starting_size .and. cnk_size <= nvals_in_chunk) then
            res = (/ starting_chunk(1) + xx, starting_chunk(2) + yy /)
          end if
        end do
      end do
    end if
  end function

  module subroutine create_netcdf(this, ctl_data, model_basin, model_time, filename)
    use netcdf
    implicit none

    class(Summary), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    character(len=*), intent(in) :: filename

    ! When we create netCDF files, variables and dimensions, we get back
    ! an ID for each one.
    integer(i32) :: cnk_sizes(2)
    integer(i32) :: dimids(2)
    integer(i32) :: nhru_dimid
    integer(i32) :: noutvars
      !! Number of output variables in outVar_names
    integer(i32) :: nsegment_dimid
    integer(i32) :: nsub_dimid
    integer(i32) :: time_dimid
    integer(i32) :: nhm_id_varid
    integer(i32) :: nhm_seg_varid
    integer(i32) :: ov_dimid
      !! dimid for current output variable being created

    integer(i32) :: cnk_per_domain
    integer(i32) :: var_cache_size
    integer(i32) :: var_cache_slots

    integer(i32) :: jj
    character(len=:), allocatable :: outvar_dimensions
    integer(i32) :: outvar_datatype
    ! character(len=:), allocatable :: outvar_datatype
    character(len=:), allocatable :: outvar_desc
    character(len=:), allocatable :: outvar_name
    character(len=:), allocatable :: outvar_units

    character(len=:), allocatable :: days_since

    ! ------------------------------------------------------------------------
    associate(outVar_names => ctl_data%outVar_names, &
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

      noutvars = outVar_names%size()

      ! Create netcdf file
      ! NOTE: using NF90_NETCDF4 with a fixed time dimension gives slightly
      !       slower writing but much smaller filesizes.
      !       Using NF90_CLOBBER (netcdf3) with an unlimited time dimension
      !       results in faster writing but larger file sizes (file sizes are
      !       still smaller than ASCII files).
      !       NF90_64BIT_OFFSET

      ! write(*, *) 'Create output netcdf'
      call this%err_check(nf90_create(filename, NF90_NETCDF4, this%file_hdl))
      ! call this%err_check(nf90_create(filename, NF90_NETCDF4, this%file_hdl, &
      !                     cache_size=8388608, cache_nelems=2111))
      ! call this%err_check(nf90_create(filename, NF90_NETCDF4, this%file_hdl, &
      !                     cache_nelems=2048, &
      !                     cache_size=134217728))
      ! call this%err_check(nf90_create(filename, NF90_64BIT_OFFSET, this%file_hdl))

      ! Define the dimensions. NetCDF will hand back an ID for each.
      ! call this%err_check(nf90_def_dim(this%file_hdl, 'time', NF90_UNLIMITED, time_dimid))
      call this%err_check(nf90_def_dim(this%file_hdl, 'time', days_in_model, time_dimid))

      ! write(*, *) '  add nhru dimension'
      call this%err_check(nf90_def_dim(this%file_hdl, "nhru", nhru, nhru_dimid))

      if (nsegment > 0) then
        ! write(*, *) '  add nsegment dimension'
        call this%err_check(nf90_def_dim(this%file_hdl, "nsegment", nsegment, nsegment_dimid))
      end if

      if (nsub > 0) then
        ! write(*, *) '  add nsub dimension'
        call this%err_check(nf90_def_dim(this%file_hdl, "nsub", nsub, nsub_dimid))
      end if

      ! The dimids array is used to pass the IDs of the dimensions of the
      ! variables. Note that in fortran arrays are stored in column-major format.
      ! dimids = (/ nhru_dimid, time_dimid /)

      ! Define the variable for the time dimension
      days_since = 'days since ' // start_string // ' 00:00:00'
      call this%err_check(nf90_def_var(this%file_hdl, &
                                       'time', NF90_FLOAT, time_dimid, this%time_varid))
      call this%err_check(nf90_put_att(this%file_hdl, this%time_varid, &
                                       'long_name', 'time'))
      call this%err_check(nf90_put_att(this%file_hdl, this%time_varid, &
                                       'calendar', 'standard'))
      call this%err_check(nf90_put_att(this%file_hdl, this%time_varid, &
                                       'units', days_since))

      ! Always include nhm_id as a variable
      call this%err_check(nf90_def_var(this%file_hdl, &
                                       'nhm_id', NF90_INT, nhru_dimid, nhm_id_varid))
      call this%err_check(nf90_put_att(this%file_hdl, nhm_id_varid, &
                                       'long_name', 'NHM HRU id'))
      call this%err_check(nf90_put_att(this%file_hdl, nhm_id_varid, &
                                       'units', 'none'))

      if (nsegment > 0) then
        ! Include nhm_seg as a variable if nsegment > 0
        ! write(*, *) '  add nhm_seg dimension'
        call this%err_check(nf90_def_var(this%file_hdl, &
                                        'nhm_seg', NF90_INT, nsegment_dimid, nhm_seg_varid))
        call this%err_check(nf90_put_att(this%file_hdl, nhm_seg_varid, &
                                        'long_name', 'NHM segment id'))
        call this%err_check(nf90_put_att(this%file_hdl, nhm_seg_varid, &
                                        'units', 'none'))
      end if

      ! TODO: Add nsub-related variable

      ! Define the output variables
      do jj = 1, noutvars
        outvar_name = outVar_names%values(jj)%s
        ! DEBUG: PAN
        ! print *, 'CREATE OUTVAR: ', outvar_name

        ! Pull variable information from control class
        call output_variables%get(outvar_name, &
                                  outvar_dimensions, outvar_datatype, &
                                  outvar_desc, outvar_units)

        ! write(*, *) '  Create var: ', outvar_name, '    dims: ', outvar_dimensions

        if (outvar_dimensions == 'one') then
          ! 1D - e.g. each timestep writes a scalar
          ! Save the array size for this variable to an array for later
          this%outvar_size(jj) = 1

          ! call this%err_check(nf90_def_var(this%file_hdl, outvar_name, &
          !                                  outvar_datatype, time_dimid, this%outvar_id(jj)))
          call this%err_check(nf90_def_var(this%file_hdl, outvar_name, &
                                           outvar_datatype, time_dimid, this%outvar_id(jj), &
                                           shuffle=.true., &
                                           deflate_level=1))
        else
          ! 2D output variable (e.g. time, nhru)
          ! Get the dimid for the outvar dimension
          call this%err_check(nf90_inq_dimid(this%file_hdl, outvar_dimensions, &
                                             ov_dimid))
          dimids = (/ ov_dimid, time_dimid /)

          ! Save the array size for this variable to an array for later
          call this%err_check(nf90_inquire_dimension(this%file_hdl, ov_dimid, &
                                                     len=this%outvar_size(jj)))

          ! DEBUG: PAN
          ! print *, '---- ', this%outvar_size(jj), days_in_model
          ! call this%err_check(nf90_def_var(this%file_hdl, outvar_name, &
          !                                  outvar_datatype, dimids, this%outvar_id(jj)))

          if (outvar_datatype == 4 .or. outvar_datatype == 5) then
            ! NC_INT or NC_FLOAT (4 byte)
            cnk_sizes = chunk_shape_2d((/this%outvar_size(jj), days_in_model/), val_size=4, chunk_size=1048576)
            cnk_per_domain = ceiling(real(nhru) / real(cnk_sizes(1)))

            ! Compute initial cache size
            var_cache_size = 2**exponent(real(cnk_per_domain * product(cnk_sizes) * 4))
          else if (outvar_datatype == 6) then
            ! NC_DOUBLE (8 byte)
            cnk_sizes = chunk_shape_2d((/this%outvar_size(jj), days_in_model/), val_size=8, chunk_size=1048576)

            cnk_per_domain = ceiling(real(nhru) / real(cnk_sizes(1)))

            ! Compute initial cache size
            var_cache_size = 2**exponent(real(cnk_per_domain * product(cnk_sizes) * 8))
          else
            write(output_unit, *) 'ERROR: Unsupported output variable type'
          end if

          ! WARNING: 2020-09-29 PAN - var_cache_size as define is currently in bytes,
          !                           but cache_size in nf90_def_var expects Megabytes.

          ! TODO: the number of slots should be a prime number to minimize hash table collisions
          var_cache_slots = cnk_per_domain * 10
          ! DEBUG: PAN
          ! write(output_unit, *) 'cnk_sizes: ', cnk_sizes, '  cnk_per_domain: ', cnk_per_domain, &
          !                       ' var_cache_size: ', var_cache_size, ' var_cache_slots: ', var_cache_slots
          call this%err_check(nf90_def_var(this%file_hdl, outvar_name, &
                                           outvar_datatype, dimids, this%outvar_id(jj), &
                                           shuffle=.true., &
                                           deflate_level=1, &
                                           chunksizes=cnk_sizes, &
                                           cache_size=var_cache_size, &
                                           cache_nelems=var_cache_slots))

          ! call this%err_check(nf90_def_var(this%file_hdl, outvar_name, &
          !                                  outvar_datatype, dimids, this%outvar_id(jj), &
          !                                  shuffle=.true., &
          !                                  deflate_level=1, &
          !                                  chunksizes=[517, 15], &
          !                                  cache_size=8388608, cache_nelems=2130))

          ! call this%err_check(nf90_def_var(this%file_hdl, outvar_name, &
          !                     outvar_datatype, dimids, this%outvar_id(jj), &
          !                     shuffle=.true., &
          !                     deflate_level=1))
        end if

        ! Add attributes for each variable
        call this%err_check(nf90_put_att(this%file_hdl, &
                                         this%outvar_id(jj), &
                                         'description', outvar_desc))
        call this%err_check(nf90_put_att(this%file_hdl, &
                                         this%outvar_id(jj), &
                                         'units', outvar_units))
      end do

      ! Define global attributes
      ! TODO: Add global attributes for start and end date.

      ! End define mode. This tells netCDF we are done defining metadata.
      call this%err_check(nf90_enddef(this%file_hdl))

      ! Write the nhm_id values to the file
      call this%err_check(nf90_put_var(this%file_hdl, nhm_id_varid, nhm_id))

      if (nsegment > 0) then
        ! Write the nhm_seg values to the file
        call this%err_check(nf90_put_var(this%file_hdl, nhm_seg_varid, model_basin%nhm_seg))
      end if

      ! Close the file. This frees up any internal netCDF resources
      ! associated with the file, and flushes any buffers.
      ! call this%err_check(nf90_close(this%file_hdl))
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


  module subroutine set_summary_var_r64(this, idx, var)
    implicit none

    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - r64_outvar set'
    this%var_daily(idx)%ptr_r64 => var
  end subroutine

  module subroutine set_summary_var_r32(this, idx, var)
    implicit none

    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r32), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - r32_outvar set'
    this%var_daily(idx)%ptr_r32 => var
  end subroutine

  module subroutine set_summary_var_i32(this, idx, var)
    implicit none

    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    integer(i32), target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    ! write(*,*) '  - i32_outvar set'
    this%var_daily(idx)%ptr_i32 => var
  end subroutine

  module subroutine set_summary_var_r64_0d(this, idx, var)
    implicit none

    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    real(r64), target, intent(in) :: var

    ! --------------------------------------------------------------------------
    this%var_daily(idx)%scalar_r64 => var
  end subroutine

  module subroutine set_summary_var_logical_1d(this, idx, var)
    implicit none

    class(Summary), intent(inout) :: this
    integer(i32), intent(in) :: idx
    logical, target, intent(in) :: var(:)

    ! --------------------------------------------------------------------------
    this%var_daily(idx)%ptr_logical => var
  end subroutine

  module subroutine write_netcdf_i32_0d(this, ncid, varid, data, start, ocount)
    use netcdf
    implicit none

    class(Summary), intent(in) :: this
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

    class(Summary), intent(in) :: this
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

    class(Summary), intent(in) :: this
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

    class(Summary), intent(in) :: this
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

    class(Summary), intent(in) :: this
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

    class(Summary), intent(in) :: this
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
    class(Summary), intent(in) :: this
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
    implicit none

    class(Summary), intent(in) :: this
    integer(i32), intent(in) :: ncid
    integer(i32), intent(in) :: varid
    type(var_ptrs), intent(in) :: data
    integer(i32), intent(in) :: start(:)
    integer(i32), optional, intent(in) :: ocount(:)

    integer :: on
    integer :: off
    integer, allocatable :: logical_out(:)
    ! --------------------------------------------------------------------------
    ! if (present(ocount)) then
    !   call this%err_check(nf90_put_var(ncid, varid, data, start=start, count=ocount))
    ! else
    !   call this%err_check(nf90_put_var(ncid, varid, data, start=start))
    ! end if

    ! Sample from netcdf_expanded.f90
    ! numDims                 = size(shape(values))
    ! localStart (:         ) = 1
    ! localCount (:numDims  ) = shape(values)

    on = 1
    off = 0

    if (associated(data%ptr_r32)) then
      call this%err_check(nf90_put_var(ncid, varid, data%ptr_r32, start=start, count=ocount))
    else if (associated(data%ptr_r64)) then
      call this%err_check(nf90_put_var(ncid, varid, data%ptr_r64, start=start, count=ocount))
    else if (associated(data%ptr_i32)) then
      call this%err_check(nf90_put_var(ncid, varid, data%ptr_i32, start=start, count=ocount))
    else if (associated(data%ptr_logical)) then
      allocate(logical_out(size(data%ptr_logical)))
      logical_out = merge(on, off, data%ptr_logical)
      call this%err_check(nf90_put_var(ncid, varid, logical_out, start=start, count=ocount))
      deallocate(logical_out)
    else if (associated(data%scalar_r64)) then
      call this%err_check(nf90_put_var(ncid, varid, data%scalar_r64, start=start))
    else
      write(*, *) MODNAME, '%run() No output array for variable id: ', varid
    end if
  end subroutine


  module subroutine cleanup_Summary(this)
    use netcdf
    implicit none

    class(Summary), intent(in) :: this

    ! --------------------------------------------------------------------------
    ! Close the summary output file
    call this%err_check(nf90_close(this%file_hdl))
  end subroutine
end submodule
