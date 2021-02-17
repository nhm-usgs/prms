  module prms_nsegment_ncf
  use prms_module, only: MAXFILE_LENGTH
  implicit none
  integer, save, allocatable :: varid(:), ncid(:), nhm_varid(:), time_varid(:), nc_vars(:), nhm_seg(:), fort_type(:)
  integer, save :: start(2), count(2)
  integer, save :: nsegmentoutvars
  character(len=36), save, allocatable :: nsegmentoutvar_names(:)
  character(len=12), parameter :: MODNAME = 'nsegment_ncf'
  character(len=MAXFILE_LENGTH), save :: nsegmentoutbasefilename
  integer, save, allocatable :: int_vals(:)
  real, save, allocatable :: real_vals(:)
  double precision, save, allocatable :: double_vals(:)

  contains
      subroutine check(status)
      use netcdf
      implicit none
      integer, intent ( in) :: status

          if(status /= nf90_noerr) then
              print *, trim(nf90_strerror(status))
              stop "Stopped prms_nsegment_ncf"
          end if
      end subroutine check
  end module prms_nsegment_ncf

!***********************************************************************
  subroutine nsegment_ncf()
  use prms_module, only: process
  implicit none

      if (process(:3) == 'run') then
          call nsegment_ncf_run()
      elseif (process(:4) == 'decl') then
          call nsegment_ncf_decl()
      elseif (process(:4) == 'init') then
          call nsegment_ncf_init()
      elseif (process(:5) == 'clean') then
          call nsegment_ncf_clean()
      endif
  end subroutine nsegment_ncf

!***********************************************************************
  subroutine nsegment_ncf_decl()
  use prms_nsegment_ncf
  use prms_module, only: nsegment, model
  implicit none
  integer, external :: control_string_array, control_integer, control_string, declparam, numchars
  integer :: ii
  character(len=80), parameter :: version_nsegment_ncf = 'nsegment_ncf.f90 2020-04-28 12:57:00Z'

      call print_module(version_nsegment_ncf, 'nsegment ncf                    ', 90)

      if (control_integer(nsegmentoutvars, 'nsegmentOutVars') /= 0) then
          nsegmentoutvars = 0
          if (model/=99) call error_stop('ERROR, nsegment_ncf requested with nsegmentOutVars equal 0')
      else
         allocate(nsegmentoutvar_names(nsegmentoutvars), nc_vars(nsegmentoutvars), fort_type(nsegmentoutvars))
         allocate(varid(nsegmentoutvars), ncid(nsegmentoutvars), nhm_varid(nsegmentoutvars), time_varid(nsegmentoutvars))
         allocate(int_vals(nsegment), real_vals(nsegment), double_vals(nsegment))

         nsegmentoutvar_names = ' '
         do ii = 1, nsegmentoutvars
            if (control_string_array(nsegmentoutvar_names(ii), 'nsegmentOutVar_names', ii)/=0 ) then
                call read_error(5, 'nsegmentOutVar_names')
            endif
            nc_vars(ii) = numchars(nsegmentoutvar_names(ii))
         enddo

         if (control_string(nsegmentoutbasefilename, 'nsegmentOutBaseFileName')/=0 ) call read_error(5, 'nsegmentOutBaseFileName')
      endif

      allocate(nhm_seg(nsegment))
      if (declparam(MODNAME, 'nhm_seg', 'nsegment', 'integer', &
     &    '1', '1', '9999999', &
     &    'National Hydrologic Model HRU ID', 'National Hydrologic Model segment ID', &
     &    'none') /= 0 ) CALL read_error(1, 'nhm_seg')
  end subroutine nsegment_ncf_decl

!***********************************************************************
  subroutine nsegment_ncf_init()
  use prms_nsegment_ncf
  use prms_module, only: nsegment, start_year, start_month, start_day
  use netcdf
  implicit none
  integer :: dimids(2), nsegment_dimid, time_dimid, ii, ret
  character(len=MAXFILE_LENGTH) :: file_name, varunits, varhelp
  character(len=30) :: start_string
  integer, external :: control_string_array, getparam, numchars
  integer, external :: getvartype, getvar_units, getvar_help

      count = (/ nsegment, 1 /)
      start = (/ 1, 1 /)

      if (getparam(modname, 'nhm_seg', nsegment, 'integer', nhm_seg)/=0 ) call read_error(2, 'nhm_seg')
  
      write (start_string, 1001) start_year, start_month, start_day
 1001 format ('days since ', i0.4, '-', i0.2, '-', i0.2, ' 00:00:00')

      do ii = 1, nsegmentoutvars
          if (control_string_array(nsegmentoutvar_names(ii), 'nsegmentOutVar_names', ii)/=0 ) then
              call read_error(5, 'nsegmentOutVar_names')
          endif
          nc_vars(ii) = numchars(nsegmentoutvar_names(ii))

          file_name = nsegmentoutbasefilename(:numchars(nsegmentoutbasefilename))//nsegmentoutvar_names(ii)(:nc_vars(ii))//'.nc'
          call check(nf90_create(file_name, NF90_CLOBBER, ncid(ii)))

          call check(nf90_def_dim(ncid(ii), "nsegment", nsegment, nsegment_dimid))
          call check(nf90_def_dim(ncid(ii), "time", NF90_UNLIMITED, time_dimid))

          call check(nf90_def_var(ncid(ii), "nhmsegid", NF90_INT, nsegment_dimid, nhm_varid(ii)))
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "standard_name", "nhm_seg")) 
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "long_name", "segment ids to use with CONUS Geospatial Fabric")) 
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "units", "segment_id")) 

          call check(nf90_def_var(ncid(ii), "timestep", NF90_INT, time_dimid, time_varid(ii)))
          call check(nf90_put_att(ncid(ii), time_varid(ii), "standard_name", "time step")) 
          call check(nf90_put_att(ncid(ii), time_varid(ii), "long_name", "time step of simulated value")) 
          call check(nf90_put_att(ncid(ii), time_varid(ii), "units", start_string)) 

          dimids =  (/ nsegment_dimid, time_dimid /)

          ! Define the variable. unidata.ucar.edu/software/netcdf/docs-fortran/f90-variables.html
          ! NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE

          fort_type(ii) = getvartype(nsegmentoutvar_names(ii)(:nc_vars(ii)))
          if (fort_type(ii) == 1) then
              call check(nf90_def_var(ncid(ii), nsegmentoutvar_names(ii)(:nc_vars(ii)), NF90_INT, dimids, varid(ii)))
          elseif (fort_type(ii) == 2) then
              call check(nf90_def_var(ncid(ii), nsegmentoutvar_names(ii)(:nc_vars(ii)), NF90_FLOAT, dimids, varid(ii)))
          elseif (fort_type(ii) == 3) then
! here's a switcharoo: double precision values written into float to save space in the output files
              call check(nf90_def_var(ncid(ii), nsegmentoutvar_names(ii)(:nc_vars(ii)), NF90_FLOAT, dimids, varid(ii)))
          endif
          
          ret = getvar_units(varunits, nsegmentoutvar_names(ii)(:nc_vars(ii)))
          ret = getvar_help(varhelp, nsegmentoutvar_names(ii)(:nc_vars(ii)))

!          call check(nf90_put_att(ncid(ii), varid(ii), "standard_name", "get from declvar")) 
          call check(nf90_put_att(ncid(ii), varid(ii), "long_name", varhelp)) 
          call check(nf90_put_att(ncid(ii), varid(ii), "units", varunits)) 

          call check(nf90_enddef(ncid(ii)))

          call check(nf90_put_var(ncid(ii), nhm_varid(ii), nhm_seg))
      enddo
  end subroutine nsegment_ncf_init

!##########################################################################################################
  subroutine nsegment_ncf_run()
  use prms_nsegment_ncf
  use netcdf
  use prms_module, only: timestep, nsegment
  implicit none
  integer, external :: getvar
  integer :: ii
  real :: foo
  integer :: bar(1)

      do ii = 1, nsegmentoutvars
          if (fort_type(ii) == 1) then
              if (getvar(MODNAME, nsegmentoutvar_names(ii)(:nc_vars(ii)), nsegment, 'integer', int_vals) /=0 ) then
                  call read_error(4, nsegmentoutvar_names(ii)(:nc_vars(ii)))
              endif

              start(2) = timestep
              call check(nf90_put_var(ncid(ii), varid(ii), int_vals, start = start, count = count))
          elseif (fort_type(ii) == 2) then
              if (getvar(MODNAME, nsegmentoutvar_names(ii)(:nc_vars(ii)), nsegment, 'real', real_vals) /=0 ) then
                  call read_error(4, nsegmentoutvar_names(ii)(:nc_vars(ii)))
              endif

              start(2) = timestep
              call check(nf90_put_var(ncid(ii), varid(ii), real_vals, start = start, count = count))
          elseif (fort_type(ii) == 3) then
              if (getvar(MODNAME, nsegmentoutvar_names(ii)(:nc_vars(ii)), nsegment, 'double', double_vals) /=0 ) then
                  call read_error(4, nsegmentoutvar_names(ii)(:nc_vars(ii)))
              endif

              start(2) = timestep
              call check(nf90_put_var(ncid(ii), varid(ii), double_vals, start = start, count = count))
          endif

          foo = timestep - 1.0
          bar(1) = timestep
          call check(nf90_put_var(ncid(ii), time_varid(ii), foo, start = bar))
      enddo
  end subroutine nsegment_ncf_run

!##########################################################################################################
  subroutine nsegment_ncf_clean()
  use prms_nsegment_ncf
  use netcdf
  implicit none
  integer :: ii

      do ii = 1, nsegmentoutvars
          call check(nf90_close(ncid(ii)))
      enddo
  end subroutine nsegment_ncf_clean
