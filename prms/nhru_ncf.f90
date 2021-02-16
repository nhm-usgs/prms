!     Output a set of declared variables by HRU for use with R
!***********************************************************************
      MODULE PRMS_NHRU_NCF
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
! We are writing 2D data, a 6 x 12 grid.
  integer, parameter :: NDIMS = 2
  integer, parameter :: NX = 6, NY = 12
  integer, save, allocatable :: varid(:), ncid(:)
!  integer, save :: nhm_varid, time_varid
  integer, save, allocatable :: nhm_varid(:), time_varid(:)
  integer, save :: nhm_ncid, time_ncid
  character (len = *), parameter :: UNITS = "units"
 
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nhru_var_type(:), Nhru_var_int(:, :)
      REAL, SAVE, ALLOCATABLE :: vals(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_dble(:, :)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2, Output_fmt3, Output_fmtint
      CHARACTER(LEN=48), SAVE :: Output_grid_fmt, Output_grid_fmtint, Output_date_fmt, Output_date_fmt3, Output_fmt3int
      CHARACTER(LEN=12), SAVE :: MODNAME
      INTEGER, SAVE :: Daily_flag, Double_vars, Yeardays, Monthly_flag, Integer_vars
      DOUBLE PRECISION, SAVE :: Monthdays
      INTEGER, SAVE, ALLOCATABLE :: Monthlyunit(:), Yearlyunit(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_monthly(:, :), Nhru_var_yearly(:, :)
! Paramters
      INTEGER, SAVE, ALLOCATABLE :: Nhm_id(:)
! Control Parameters
      INTEGER, SAVE :: nhruoutvars, NhruOut_freq, NhruOut_format, NhruOutNcol
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: nhruoutvar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: nhruoutbasefilename
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: foobar

  contains
      subroutine check(status)
      use netcdf
      integer, intent ( in) :: status

          if(status /= nf90_noerr) then
              print *, trim(nf90_strerror(status))
              stop "Stopped"
          end if
      end subroutine check
  END MODULE PRMS_NHRU_NCF

!     ******************************************************************
!     nhru results module
!     ******************************************************************
  SUBROUTINE nhru_ncf()
  USE PRMS_MODULE, ONLY: Process
  USE PRMS_NHRU_NCF
  IMPLICIT NONE
! Functions
  EXTERNAL :: nhru_ncf_decl, nhru_ncf_init, nhru_ncf_run, nhru_ncf_clean
! Local Variables
!***********************************************************************

      IF ( Process(:3)=='run' ) THEN
          CALL nhru_ncf_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
          CALL nhru_ncf_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
          CALL nhru_ncf_init()
      ELSEIF ( Process(:5)=='clean' ) THEN
          CALL nhru_ncf_clean()
      ENDIF

  END SUBROUTINE nhru_ncf

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
  SUBROUTINE nhru_ncf_decl()
  USE PRMS_NHRU_NCF
  USE PRMS_MODULE, ONLY: nhru, model
  IMPLICIT NONE

! Functions
  INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
  INTEGER, EXTERNAL :: numchars

! Local Variables
  INTEGER :: ii
  CHARACTER(LEN=80), SAVE :: version_nhru_ncf
!***********************************************************************

      version_nhru_ncf = 'nhru_ncf.f90 2020-04-28 12:57:00Z'
      CALL print_module(version_nhru_ncf, 'Nhru ncf                    ', 90)
      MODNAME = 'nhru_ncf'

      IF (control_integer(nhruoutvars, 'nhruOutVars')/=0) nhruoutvars = 0
 
      if (nhruoutvars .lt. 1) then
          IF (Model/=99) CALL error_stop('ERROR, nhru_ncf requested with nhruOutVars equal 0')
      else
         ALLOCATE(nhruoutvar_names(nhruoutvars), Nhru_var_type(nhruoutvars), Nc_vars(nhruoutvars))
         allocate(varid(nhruoutvars), ncid(nhruoutvars))
         allocate(nhm_varid(nhruoutvars), time_varid(nhruoutvars))
         allocate(vals(nhru))

         nhruoutvar_names = ' '
         DO ii = 1, nhruoutvars
            IF (control_string_array(nhruoutvar_names(ii), 'nhruOutVar_names', ii)/=0 ) CALL read_error(5, 'nhruOutVar_names')
            nc_vars(ii) = numchars(nhruoutvar_names(ii))
         ENDDO
         IF (control_string(nhruoutbasefilename, 'nhruOutBaseFileName')/=0 ) CALL read_error(5, 'nhruOutBaseFileName')
      endif

! Declared Parameters
      allocate(nhm_id(nhru))
      IF(declparam(MODNAME, 'nhm_id', 'nhru', 'integer', &
     &    '1', '1', '9999999', &
     &    'National Hydrologic Model HRU ID', 'National Hydrologic Model HRU ID', &
     &    'none') /= 0 ) CALL read_error(1, 'nhm_id')

  END SUBROUTINE nhru_ncf_decl

!***********************************************************************
!     Initialize module values
!***********************************************************************
  SUBROUTINE nhru_ncf_init()
  USE PRMS_NHRU_NCF
  USE PRMS_MODULE, ONLY: nhru
!      USE PRMS_MODULE, ONLY: Nhru, MAXFILE_LENGTH, Start_year, NhruOutON_OFF, Prms_warmup
  USE PRMS_MODULE, ONLY: start_year, start_month, start_day
!  USE PRMS_MODULE, ONLY: Nhru, Start_month, Start_day, End_year, End_month, End_day
!      INTRINSIC ABS
!      INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
!      EXTERNAL read_error, PRMS_open_output_file
!! Local Variables
!      INTEGER :: ios, ierr, size, dim, jj, j
!
!      integer :: status

  use netcdf
  IMPLICIT NONE

!  integer :: ncid, varid, dimids(NDIMS)
!  integer :: varid, dimids(NDIMS)
  integer :: nhru_dimid, time_dimid
  integer :: dimids(NDIMS)
  CHARACTER(LEN=MAXFILE_LENGTH) :: file_name
  CHARACTER(LEN=30) :: start_string


!  INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
  INTEGER, EXTERNAL :: control_string_array
!     INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
  INTEGER, EXTERNAL :: getparam
  INTEGER, EXTERNAL :: numchars

  integer :: ii

      if (getparam(modname, 'nhm_id', Nhru, 'integer', nhm_id)/=0 ) call read_error(2, 'nhm_id')
  
      write (start_string, 1001) start_year, start_month, start_day
 1001 format ('days since ', i0.4, '-', i0.2, '-', i0.2, ' 00:00:00')

      DO ii = 1, nhruoutvars
          IF (control_string_array(nhruoutvar_names(ii), 'nhruOutVar_names', ii)/=0 ) CALL read_error(5, 'nhruOutVar_names')
          nc_vars(ii) = numchars(nhruoutvar_names(ii))

          file_name = nhruoutbasefilename(:numchars(nhruoutbasefilename))//nhruoutvar_names(ii)(:nc_vars(ii))//'.nc'
          call check(nf90_create(file_name, NF90_CLOBBER, ncid(ii)))

          ! Define the dimensions. NetCDF will hand back an ID for each.
          call check(nf90_def_dim(ncid(ii), "nhru", nhru, nhru_dimid))
          call check(nf90_def_dim(ncid(ii), "time", NF90_UNLIMITED, time_dimid))

          call check(nf90_def_var(ncid(ii), "nhmid", NF90_INT, nhru_dimid, nhm_varid(ii)))
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "standard_name", "nhm_id")) 
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), "long_name", "hru ids to use with CONUS Geospatial Fabric")) 
          call check(nf90_put_att(ncid(ii), nhm_varid(ii), UNITS, "hru_id")) 

          call check(nf90_def_var(ncid(ii), "timestep", NF90_INT, time_dimid, time_varid(ii)))
          call check(nf90_put_att(ncid(ii), time_varid(ii), "standard_name", "time step")) 
          call check(nf90_put_att(ncid(ii), time_varid(ii), "long_name", "time step of simulated value")) 
          call check(nf90_put_att(ncid(ii), time_varid(ii), UNITS, start_string)) 
          print *, "init time_varid ", time_varid(ii)

          ! The dimids array is used to pass the IDs of the dimensions of
          ! the variables. Note that in fortran arrays are stored in
          ! column-major format.
          dimids =  (/ nhru_dimid, time_dimid /)

          ! Define the variable. unidata.ucar.edu/software/netcdf/docs-fortran/f90-variables.html
          ! NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE

          call check(nf90_def_var(ncid(ii), nhruoutvar_names(ii)(:nc_vars(ii)) , NF90_FLOAT, dimids, varid(ii)))
          call check(nf90_put_att(ncid(ii), varid(ii), "standard_name", "get from declvar")) 
          call check(nf90_put_att(ncid(ii), varid(ii), "long_name", "get from declvar")) 
          call check(nf90_put_att(ncid(ii), varid(ii), UNITS, "get from declvar")) 

          ! End define mode. This tells netCDF we are done defining metadata.
          call check(nf90_enddef(ncid(ii)))

          call check(nf90_put_var(ncid(ii), nhm_varid(ii), nhm_id))
      ENDDO

  END SUBROUTINE nhru_ncf_init

!***********************************************************************
!     Output set of declared variables in CSV format
!***********************************************************************
  SUBROUTINE nhru_ncf_run()
  USE PRMS_NHRU_NCF
  USE netcdf
!      USE PRMS_MODULE, ONLY: Nhru, Start_month, Start_day, End_year, End_month, End_day
!      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
!      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday, Modays
     
  USE PRMS_MODULE, ONLY: timestep, nhru
  USE PRMS_SET_TIME, ONLY: nowyear, nowmonth, nowday
  implicit none

!! FUNCTIONS AND SUBROUTINES
!      INTRINSIC SNGL, DBLE
  INTEGER, EXTERNAL :: getvar
!      EXTERNAL read_error
! Local Variables

  ! This is the data array we will write. It will just be filled with
  ! a progression of integers for this example.
!  integer :: data_out(18, 114958)

!      INTEGER :: j, i, jj, write_month, write_year, last_day
!  integer :: x, y, ii
  integer :: ii
  integer, save :: start(2), count(2)
  integer, save :: val(1)
!***********************************************************************
      count = (/ nhru, 1 /)
      start = (/ 1, 1 /)
      DO ii = 1, nhruoutvars
          print *, Nowyear, "-", Nowmonth, "-", Nowday, "   ", timestep

          IF (getvar(MODNAME, nhruoutvar_names(ii)(:nc_vars(ii)), Nhru, 'real', vals) /=0 ) then
             CALL read_error(4, nhruoutvar_names(ii)(:nc_vars(ii)))
          endif
          print *, "      ", nhruoutvar_names(ii)(:nc_vars(ii)), vals(1), vals(nhru)

          start(2) = timestep
          call check(nf90_put_var(ncid(ii), varid(ii), vals, start = start, count = count))
      ENDDO
 
  END SUBROUTINE nhru_ncf_run

!##########################################################################################################
  subroutine nhru_ncf_clean()
  USE PRMS_NHRU_NCF
  use netcdf
  integer :: ii

  ! Close the file. This frees up any internal netCDF resources
  ! associated with the file, and flushes any buffers.
      DO ii = 1, nhruoutvars
          call check(nf90_close(ncid(ii)))
      enddo
  end subroutine nhru_ncf_clean
