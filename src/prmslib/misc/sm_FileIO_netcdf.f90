submodule (PRMS_FILE_IO_NETCDF) sm_FileIO_netcdf
  use netcdf

  contains

    module function constructor_FileIO_netcdf(filename) result(this)
      implicit none

      type(FileIO_netcdf) :: this
      character(len=*), intent(in) :: filename

      ! ------------------------------------------------------------------------
      ! Open the netcdf parameter file
      call this%open(filename)
    end function


    module subroutine open_netcdf(this, filename)
      implicit none

      class(FileIO_netcdf), intent(inout) :: this
      character(len=*), intent(in) :: filename

      ! ------------------------------------------------------------------------
      ! Open netcdf file as read only
      ! print *, 'TRYING TO OPEN: ', filename
      this%file_hdl = 0
      call this%err_check(nf90_open(filename, NF90_NOWRITE, this%file_hdl))
    end subroutine


    module subroutine close_netcdf(this)
      implicit none

      class(FileIO_netcdf), intent(inout) :: this

      ! ------------------------------------------------------------------------
      ! Close the netcdf file
      call this%err_check(nf90_close(this%file_hdl))
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


    module function get_dimension(this, name) result(res)
      implicit none

      integer(i32) :: res
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name

      integer(i32) :: dimid
      ! logical :: status

      ! -----------------------------------------------------------------------
      ! status = this%dim_exists(name)
      ! call this%err_check(nf90_inq_dimid(this%file_hdl, name, dimid))

      ! if (status) then
      if (this%dim_exists(name)) then
        ! Return the size of the dimension
        call this%err_check(nf90_inq_dimid(this%file_hdl, name, dimid))
        call this%err_check(nf90_inquire_dimension(this%file_hdl, dimid, len=res))
      else
        write(*, *) '  Dimension: ', trim(name), ' does not exist, default to 0.'
        res = 0
      end if
    end function


    module function dim_exists(this, name) result(res)
      logical :: res
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
        !! Name of dimension

      integer(i32) :: dimid
      integer(i32) :: status

      ! ------------------------------------------------------------------------
      status = nf90_inq_dimid(this%file_hdl, name, dimid)

      if (status /= nf90_noerr) then
        if (status == nf90_ebaddim) then
          ! Dimension doesn't exist
          res = .false.
        else
          ! Catchall for other errors
          write(output_unit, *) 'WARNING: ', status, trim(nf90_strerror(status))
          res = .false.
        end if
      else
        res = .true.
      end if
    end function


    module function var_exists(this, name) result(res)
      logical :: res
      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
        !! Name of variable

      integer(i32) :: varid
      integer(i32) :: status

      ! ------------------------------------------------------------------------
      status = nf90_inq_varid(this%file_hdl, name, varid)

      if (status /= nf90_noerr) then
        if (status == nf90_enotvar) then
          ! Variable doesn't exist
          res = .false.
        else
          ! Catchall for other errors
          write(output_unit, *) 'WARNING: ', status, trim(nf90_strerror(status))
          res = .false.
        end if
      else
        res = .true.
      end if
    end function


    module subroutine get_variable_i32_scalar(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i32), intent(inout) :: var_data

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine


    module subroutine get_variable_i32_1d(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i32), pointer, intent(inout) :: var_data(:)

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine


    module subroutine get_variable_i32_2d(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i32), pointer, intent(inout) :: var_data(:, :)

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine


    module subroutine get_variable_i64_2d(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(i64), pointer, intent(inout) :: var_data(:, :)

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))
      write(*, *) 'Int var: ', name, ' ID: ', varid

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine


    module subroutine get_variable_r32_scalar(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r32), intent(inout) :: var_data

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine


    module subroutine get_variable_r32_1d(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r32), pointer, intent(inout) :: var_data(:)

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(*, *) '-- in get_variable_r32_1d'
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine


    module subroutine get_variable_r32_2d(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r32), pointer, intent(inout) :: var_data(:, :)

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file
      ! integer(i32) :: num_dims
      !   !! Number of dimensions for a variable
      ! integer(i32), allocatable :: dim_ids(:)
      !   !! ID number for each dimension for a variable
      ! integer(i32) :: ii
      ! character(len=10) :: dimname

      ! ------------------------------------------------------------------------
      ! write(*, *) '-- in get_variable_r32_2d'
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! call this%err_check(nf90_inquire_variable(this%file_hdl, varid=varid, ndims=num_dims))

      ! Verify netcdf dimension size and variable size match.
      ! TODO: If netcdf size is larger than the variable size it's an error.
      ! TODO: If netcdf size is smaller than the variable size it may need to
      ! be expanded after reading it.

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))

      ! if (num_dims == 1) then
      !   call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
      ! else if (num_dims == 2) then
      !   ! Get the dimids
      !   allocate(dim_ids(num_dims))
      !   call this%err_check(nf90_inquire_variable(this%file_hdl, varid=varid, dimids=dim_ids))
      !   write(*,*) dim_ids
      !
      !   do ii=1, num_dims
      !     call this%err_check(nf90_inquire_dimension(this%file_hdl, dim_ids(ii), name=dimname))
      !     write(*, *) 'dim: ', dim_ids(ii), dimname
      !   end do
      !   call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
      !       ! , start=[1, timestep+idx_offset], count=[nhru, 1]))
      ! end if
    end subroutine


    module subroutine get_variable_r64_1d(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r64), pointer, intent(inout) :: var_data(:)

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine


    module subroutine get_variable_r64_2d(this, name, var_data)
      implicit none

      class(FileIO_netcdf), intent(in) :: this
      character(len=*), intent(in) :: name
      real(r64), pointer, intent(inout) :: var_data(:, :)

      integer(i32) :: varid
        !! Variable ID in the CBH netcdf file

      ! ------------------------------------------------------------------------
      ! Get the varid of the data variable, based on its name.
      ! write(output_unit, *) 'Loading ', name
      call this%err_check(nf90_inq_varid(this%file_hdl, name, varid))

      ! Read the data
      call this%err_check(nf90_get_var(this%file_hdl, varid, var_data))
    end subroutine
end submodule
