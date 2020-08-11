submodule (PRMS_RESTART) sm_restart
use netcdf
use prms_constants, only: dp

contains
  module subroutine init_Restart(this, ctl_data, model_basin, model_time)
    use prms_constants, only : YEAR, MONTH, DAY
    implicit none

    class(Restart) :: this
      !! Restart class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time

    character(len=26) :: filename
    ! ------------------------------------------------------------------------
    associate(start_time => ctl_data%start_time%values, &
              end_time => ctl_data%end_time%values, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              save_vars_to_file => ctl_data%save_vars_to_file%value)

      if (save_vars_to_file == 1) then
        write(filename, 9001) end_time(YEAR), end_time(MONTH), end_time(DAY)
        write(*, *) 'Output restart filename: ', filename

        call this%create_netcdf(ctl_data, model_basin, model_time, filename)
      end if

      9001 format('PRMS_RESTART_', I4, '-', I2.2, '-', I2.2, '.nc')
    end associate
  end subroutine


  module subroutine add_variable_r32_1d(this, data, var_name, var_dim_name, var_desc, var_units)
    class(Restart), intent(inout) :: this
    real(r32), intent(in) :: data(:)
    character(len=*), intent(in) :: var_name
    character(len=*), intent(in) :: var_dim_name
    character(len=*), intent(in) :: var_desc
    character(len=*), intent(in) :: var_units

    integer(i32) :: dimids(2)
    integer(i32) :: time_dim_id
    integer(i32) :: var_id
    integer(i32) :: var_dim_id
    integer(i32) :: var_size

    integer(i32) :: start(2)
    integer(i32) :: rcount(2)

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%write_hdl))

    ! Get the variable dimension id
    call this%err_check(nf90_inq_dimid(this%write_hdl, var_dim_name, var_dim_id))
    call this%err_check(nf90_inq_dimid(this%write_hdl, 'time', time_dim_id))
    dimids = (/ var_dim_id, time_dim_id /)

    ! Save the array size for this variable
    call this%err_check(nf90_inquire_dimension(this%write_hdl, var_dim_id, len=var_size))

    ! Add the variable to the netcdf file
    ! Datatypes: NC_FLOAT=5, NC_DOUBLE=6, NC_INT=4, NC_CHAR=2
    call this%err_check(nf90_def_var(this%write_hdl, var_name, 5, dimids, var_id))

    ! Add attributes for the variable
    call this%err_check(nf90_put_att(this%write_hdl, var_id, 'description', var_desc))
    call this%err_check(nf90_put_att(this%write_hdl, var_id, 'units', var_units))

    ! Exit define mode
    call this%err_check(nf90_enddef(this%write_hdl))

    ! Write the variable data
    start = (/ 1, 1 /)
    rcount = (/ var_size, 1 /)
    call this%err_check(nf90_put_var(this%write_hdl, var_id, data, start=start, count=rcount))

  end subroutine

  module subroutine add_variable_r64_1d(this, data, var_name, var_dim_name, var_desc, var_units)
    class(Restart), intent(inout) :: this
    real(r64), intent(in) :: data(:)
    character(len=*), intent(in) :: var_name
    character(len=*), intent(in) :: var_dim_name
    character(len=*), intent(in) :: var_desc
    character(len=*), intent(in) :: var_units

    integer(i32) :: dimids(2)
    integer(i32) :: time_dim_id
    integer(i32) :: var_id
    integer(i32) :: var_dim_id
    integer(i32) :: var_size

    integer(i32) :: start(2)
    integer(i32) :: rcount(2)

    ! ------------------------------------------------------------------------
    ! Enter define mode
    call this%err_check(nf90_redef(this%write_hdl))

    ! Get the variable dimension id
    call this%err_check(nf90_inq_dimid(this%write_hdl, var_dim_name, var_dim_id))
    call this%err_check(nf90_inq_dimid(this%write_hdl, 'time', time_dim_id))
    dimids = (/ var_dim_id, time_dim_id /)

    ! Save the array size for this variable
    call this%err_check(nf90_inquire_dimension(this%write_hdl, var_dim_id, len=var_size))

    ! Add the variable to the netcdf file
    ! Datatypes: NC_FLOAT=5, NC_DOUBLE=6, NC_INT=4, NC_CHAR=2
    call this%err_check(nf90_def_var(this%write_hdl, var_name, 5, dimids, var_id))

    ! Add attributes for the variable
    call this%err_check(nf90_put_att(this%write_hdl, var_id, 'description', var_desc))
    call this%err_check(nf90_put_att(this%write_hdl, var_id, 'units', var_units))

    ! Exit define mode
    call this%err_check(nf90_enddef(this%write_hdl))

    ! Write the variable data
    start = (/ 1, 1 /)
    rcount = (/ var_size, 1 /)
    call this%err_check(nf90_put_var(this%write_hdl, var_id, data, start=start, count=rcount))
  end subroutine

  module subroutine create_netcdf(this, ctl_data, model_basin, model_time, filename)
    use netcdf
    implicit none

    class(Restart), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    character(len=*), intent(in) :: filename

    ! Dimension IDs
    integer(i32) :: nhru_dimid
    integer(i32) :: nsegment_dimid
    integer(i32) :: time_dimid

    ! Variable IDs
    integer(i32) :: nhm_id_varid
    integer(i32) :: nhm_seg_varid

    character(len=:), allocatable :: days_since

    ! ------------------------------------------------------------------------
    associate(outVar_names => ctl_data%outVar_names, &
              output_variables => ctl_data%output_variables, &

              nhru => model_basin%nhru, &
              nsegment => model_basin%nsegment, &
              nhm_id => model_basin%nhm_id, &
              nhm_seg => model_basin%nhm_seg, &

              days_in_model => model_time%days_in_model, &
              months_in_model => model_time%months_in_model, &
              start_string => model_time%start_string, &
              years_in_model => model_time%years_in_model)

      ! Create the netcdf restart file
      call this%err_check(nf90_create(filename, NF90_64BIT_OFFSET, this%write_hdl))

      ! Define the dimensions. NetCDF will hand back an ID for each.
      ! Restart files have a single timestep.
      call this%err_check(nf90_def_dim(this%write_hdl, 'time', 1, time_dimid))

      ! Add dimensions
      call this%err_check(nf90_def_dim(this%write_hdl, "nhru", nhru, nhru_dimid))
      call this%err_check(nf90_def_dim(this%write_hdl, "nsegment", nsegment, nsegment_dimid))

      ! Define the variable for the time dimension
      days_since = 'days since ' // start_string // ' 00:00:00'
      call this%err_check(nf90_def_var(this%write_hdl, &
                                       'time', NF90_FLOAT, time_dimid, this%time_varid))
      call this%err_check(nf90_put_att(this%write_hdl, this%time_varid, &
                                       'long_name', 'time'))
      call this%err_check(nf90_put_att(this%write_hdl, this%time_varid, &
                                       'calendar', 'standard'))
      call this%err_check(nf90_put_att(this%write_hdl, this%time_varid, &
                                       'units', days_since))

      ! Always include nhm_id as a variable
      call this%err_check(nf90_def_var(this%write_hdl, &
                                       'nhm_id', NF90_INT, nhru_dimid, nhm_id_varid))
      call this%err_check(nf90_put_att(this%write_hdl, nhm_id_varid, &
                                       'long_name', 'NHM HRU id'))
      call this%err_check(nf90_put_att(this%write_hdl, nhm_id_varid, &
                                       'units', 'none'))

      ! Always include nhm_seg as a variable
      call this%err_check(nf90_def_var(this%write_hdl, &
                                       'nhm_seg', NF90_INT, nsegment_dimid, nhm_seg_varid))
      call this%err_check(nf90_put_att(this%write_hdl, nhm_seg_varid, &
                                       'long_name', 'NHM segment id'))
      call this%err_check(nf90_put_att(this%write_hdl, nhm_seg_varid, &
                                       'units', 'none'))

      ! End define mode. This tells netCDF we are done defining metadata.
      call this%err_check(nf90_enddef(this%write_hdl))

      ! TODO: These can probably go in the basin cleanup
      ! ! Write the nhm_id values to the file
      ! call this%err_check(nf90_put_var(this%write_hdl, nhm_id_varid, nhm_id))

      ! ! Write the nhm_seg values to the file
      ! call this%err_check(nf90_put_var(this%write_hdl, nhm_seg_varid, nhm_seg))

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

  module subroutine cleanup_Restart(this)
    class(Restart), intent(in) :: this

    ! Close the summary output file
    call this%err_check(nf90_close(this%write_hdl))
  end subroutine
end submodule