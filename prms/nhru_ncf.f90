  module prms_nhru_ncf
  use prms_module, only: MAXFILE_LENGTH
  implicit none
  integer, save, allocatable :: varid(:), ncid(:), nhm_varid(:), time_varid(:), nc_vars(:), nhm_id(:), fort_type(:)
  integer, save :: start(2), count(2)
  integer, save :: nhruoutvars
  character(len=36), save, allocatable :: nhruoutvar_names(:)
  character(len=12), parameter :: MODNAME = 'nhru_ncf'
  character(len=MAXFILE_LENGTH), save :: nhruoutbasefilename
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
              stop "Stopped prms_nhru_ncf"
          end if
      end subroutine check
  end module prms_nhru_ncf

!***********************************************************************
  subroutine nhru_ncf()
  use prms_module, only: process
  implicit none

      if (process(:3) == 'run') then
          call nhru_ncf_run()
      elseif (process(:4) == 'decl') then
          call nhru_ncf_decl()
      elseif (process(:4) == 'init') then
          call nhru_ncf_init()
      elseif (process(:5) == 'clean') then
          call nhru_ncf_clean()
      endif
  end subroutine nhru_ncf

!***********************************************************************
  subroutine nhru_ncf_decl()
  use prms_nhru_ncf
  use prms_module, only: nhru, model
  implicit none
  integer, external :: control_string_array, control_integer, control_string, declparam, numchars
  integer :: ii
  character(len=80), parameter :: version_nhru_ncf = 'nhru_ncf.f90 2020-04-28 12:57:00Z'

      call print_module(version_nhru_ncf, 'Nhru ncf                    ', 90)

      if (control_integer(nhruoutvars, 'nhruOutVars') /= 0) then
          nhruoutvars = 0
          if (model/=99) call error_stop('ERROR, nhru_ncf requested with nhruOutVars equal 0')
      else
         allocate(nhruoutvar_names(nhruoutvars), nc_vars(nhruoutvars), fort_type(nhruoutvars))
         allocate(varid(nhruoutvars), ncid(nhruoutvars), nhm_varid(nhruoutvars), time_varid(nhruoutvars))
         allocate(int_vals(nhru), real_vals(nhru), double_vals(nhru))

         nhruoutvar_names = ' '
         do ii = 1, nhruoutvars
            if (control_string_array(nhruoutvar_names(ii), 'nhruOutVar_names', ii)/=0 ) call read_error(5, 'nhruOutVar_names')
            nc_vars(ii) = numchars(nhruoutvar_names(ii))
         enddo

         if (control_string(nhruoutbasefilename, 'nhruOutBaseFileName')/=0 ) call read_error(5, 'nhruOutBaseFileName')
      endif

      allocate(nhm_id(nhru))
      if (declparam(MODNAME, 'nhm_id', 'nhru', 'integer', &
     &    '1', '1', '9999999', &
     &    'National Hydrologic Model HRU ID', 'National Hydrologic Model HRU ID', &
     &    'none') /= 0 ) CALL read_error(1, 'nhm_id')
  end subroutine nhru_ncf_decl

!***********************************************************************
  subroutine nhru_ncf_init()
  use prms_nhru_ncf
  use prms_module, only: nhru, start_year, start_month, start_day
  use netcdf
  implicit none
  integer :: dimids(2), nhru_dimid, time_dimid, ii
  character(len=MAXFILE_LENGTH) :: file_name
  character(len=30) :: start_string
  integer, external :: control_string_array, getparam, numchars
  integer, external :: getvartype

      count = (/ nhru, 1 /)
      start = (/ 1, 1 /)

      if (getparam(modname, 'nhm_id', Nhru, 'integer', nhm_id)/=0 ) call read_error(2, 'nhm_id')
  
      write (start_string, 1001) start_year, start_month, start_day
 1001 format ('days since ', i0.4, '-', i0.2, '-', i0.2, ' 00:00:00')

      do ii = 1, nhruoutvars
          if (control_string_array(nhruoutvar_names(ii), 'nhruOutVar_names', ii)/=0 ) call read_error(5, 'nhruOutVar_names')
          nc_vars(ii) = numchars(nhruoutvar_names(ii))

          file_name = nhruoutbasefilename(:numchars(nhruoutbasefilename))//nhruoutvar_names(ii)(:nc_vars(ii))//'.nc'
          call check(nf90_create(file_name, NF90_CLOBBER, ncid(ii)))

          call check(nf90_def_dim(ncid(ii), "nhru", nhru, nhru_dimid))
          call check(nf90_def_dim(ncid(ii), "time", NF90_UNLIMITED, time_dimid))

          call check(nf90_def_var(ncid(ii), "nhmid", NF90_INT, nhru_dimid, nhm_varid(ii)))
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "standard_name", "nhm_id")) 
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "long_name", "hru ids to use with CONUS Geospatial Fabric")) 
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "units", "hru_id")) 

          call check(nf90_def_var(ncid(ii), "timestep", NF90_INT, time_dimid, time_varid(ii)))
          call check(nf90_put_att(ncid(ii), time_varid(ii), "standard_name", "time step")) 
          call check(nf90_put_att(ncid(ii), time_varid(ii), "long_name", "time step of simulated value")) 
          call check(nf90_put_att(ncid(ii), time_varid(ii), "units", start_string)) 

          dimids =  (/ nhru_dimid, time_dimid /)

          ! Define the variable. unidata.ucar.edu/software/netcdf/docs-fortran/f90-variables.html
          ! NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE

          fort_type(ii) = getvartype(nhruoutvar_names(ii)(:nc_vars(ii)))
          if (fort_type(ii) == 1) then
              call check(nf90_def_var(ncid(ii), nhruoutvar_names(ii)(:nc_vars(ii)) , NF90_INT, dimids, varid(ii)))
          elseif (fort_type(ii) == 2) then
              call check(nf90_def_var(ncid(ii), nhruoutvar_names(ii)(:nc_vars(ii)) , NF90_FLOAT, dimids, varid(ii)))
          elseif (fort_type(ii) == 3) then
! here's a switch: double precision values written into float to save space in the output files
              call check(nf90_def_var(ncid(ii), nhruoutvar_names(ii)(:nc_vars(ii)) , NF90_FLOAT, dimids, varid(ii)))
          endif

          call check(nf90_put_att(ncid(ii), varid(ii), "standard_name", "get from declvar")) 
          call check(nf90_put_att(ncid(ii), varid(ii), "long_name", "get from declvar")) 
          call check(nf90_put_att(ncid(ii), varid(ii), "units", "get from declvar")) 

          call check(nf90_enddef(ncid(ii)))

          call check(nf90_put_var(ncid(ii), nhm_varid(ii), nhm_id))
      enddo
  end subroutine nhru_ncf_init

!##########################################################################################################
  subroutine nhru_ncf_run()
  use prms_nhru_ncf
  use netcdf
  use prms_module, only: timestep, nhru
  implicit none
  integer, external :: getvar
  integer :: ii
  real :: foo
  integer :: bar(1)

      do ii = 1, nhruoutvars
          if (fort_type(ii) == 1) then
              if (getvar(MODNAME, nhruoutvar_names(ii)(:nc_vars(ii)), nhru, 'integer', int_vals) /=0 ) then
                  call read_error(4, nhruoutvar_names(ii)(:nc_vars(ii)))
              endif

              start(2) = timestep
              call check(nf90_put_var(ncid(ii), varid(ii), int_vals, start = start, count = count))
          elseif (fort_type(ii) == 2) then
              if (getvar(MODNAME, nhruoutvar_names(ii)(:nc_vars(ii)), nhru, 'real', real_vals) /=0 ) then
                  call read_error(4, nhruoutvar_names(ii)(:nc_vars(ii)))
              endif

              start(2) = timestep
              call check(nf90_put_var(ncid(ii), varid(ii), real_vals, start = start, count = count))
          elseif (fort_type(ii) == 3) then
              if (getvar(MODNAME, nhruoutvar_names(ii)(:nc_vars(ii)), nhru, 'double', double_vals) /=0 ) then
                  call read_error(4, nhruoutvar_names(ii)(:nc_vars(ii)))
              endif

              start(2) = timestep
              call check(nf90_put_var(ncid(ii), varid(ii), double_vals, start = start, count = count))
          endif

          foo = timestep - 1.0
          bar(1) = timestep
          call check(nf90_put_var(ncid(ii), time_varid(ii), foo, start = bar))
      enddo
  end subroutine nhru_ncf_run

!##########################################################################################################
  subroutine nhru_ncf_clean()
  use prms_nhru_ncf
  use netcdf
  implicit none
  integer :: ii

      do ii = 1, nhruoutvars
          call check(nf90_close(ncid(ii)))
      enddo
  end subroutine nhru_ncf_clean
