!     Output a set of declared variables by HRU for use with R
!***********************************************************************
      MODULE PRMS_NHRU_NCF
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
! We are writing 2D data, a 6 x 12 grid.
  integer, parameter :: NDIMS = 2
  integer, parameter :: NX = 6, NY = 12
  integer :: varid
 
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nhru_var_type(:), Nhru_var_int(:, :)
      REAL, SAVE, ALLOCATABLE :: nhru_var_daily(:, :)
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

      integer, save :: ncid

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
!      INTEGER :: i
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
      SUBROUTINE  nhru_ncf_decl()
      USE PRMS_NHRU_NCF
!      USE PRMS_MODULE, ONLY: Model, NhruOutON_OFF, Nhru
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
! Functions
!      INTRINSIC CHAR
      INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
 !     INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
      INTEGER, EXTERNAL :: numchars
!      EXTERNAL read_error, print_module, error_stop
! Local Variables
      INTEGER :: ii
      CHARACTER(LEN=80), SAVE :: version_nhru_ncf
!***********************************************************************
      version_nhru_ncf = 'nhru_ncf.f90 2020-04-28 12:57:00Z'
      CALL print_module(version_nhru_ncf, 'Nhru ncf                    ', 90)
      MODNAME = 'nhru_ncf'

!         print *, "   nhru_ncf_decl: open the ncf files?"
      IF (control_integer(nhruoutvars, 'nhruOutVars')/=0) nhruoutvars = 0
!         print *, "      nhruoutvars: ", nhruoutvars
 
      if (nhruoutvars .lt. 1) then
!         print *, "      nhruoutvars: nhruoutvars less than 1"
!marks        IF ( Model/=99 ) CALL error_stop('ERROR, nhru_ncf requested with nhruOutVars equal 0')
!marks      ELSE
      else
         ALLOCATE(nhruoutvar_names(nhruoutvars), Nhru_var_type(nhruoutvars), Nc_vars(nhruoutvars))
         nhruoutvar_names = ' '
         DO ii = 1, nhruoutvars
            IF (control_string_array(nhruoutvar_names(ii), 'nhruOutVar_names', ii)/=0 ) CALL read_error(5, 'nhruOutVar_names')
            nc_vars(ii) = numchars(nhruoutvar_names(ii))
         ENDDO
         IF (control_string(nhruoutbasefilename, 'nhruOutBaseFileName')/=0 ) CALL read_error(5, 'nhruOutBaseFileName')
         print *, "decl", nhruoutbasefilename(:numchars(nhruoutbasefilename))
      endif

      print *, "decl after if 0"

! Declared Parameters
!marks      IF ( NhruOutON_OFF==2 ) THEN
        allocate(nhm_id(nhru) )
        IF(declparam(MODNAME, 'nhm_id', 'nhru', 'integer', &
     &       '1', '1', '9999999', &
     &       'National Hydrologic Model HRU ID', 'National Hydrologic Model HRU ID', &
     &       'none') /= 0 ) CALL read_error(1, 'nhm_id')
!marks      ENDIF

      print *, "decl done"

      END SUBROUTINE nhru_ncf_decl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE nhru_ncf_init()
      USE PRMS_NHRU_NCF
!      use netcdf
!      USE PRMS_MODULE, ONLY: Nhru, MAXFILE_LENGTH, Start_year, NhruOutON_OFF, Prms_warmup
!      USE PRMS_MODULE, ONLY: Starttime
!      use netcdf
!      IMPLICIT NONE
!      INTRINSIC ABS
!      INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
!      EXTERNAL read_error, PRMS_open_output_file
!! Local Variables
!      INTEGER :: ios, ierr, size, dim, jj, j
!
!      integer :: status


  use netcdf
  implicit none

  ! When we create netCDF files, variables and dimensions, we get back
  ! an ID for each one.
!  integer :: ncid, varid, dimids(NDIMS)
!  integer :: varid, dimids(NDIMS)
  integer :: x_dimid, y_dimid
  integer :: dimids(NDIMS)
  CHARACTER(LEN=MAXFILE_LENGTH) :: file_name
!  INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
  INTEGER, EXTERNAL :: control_string_array
!     INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
  INTEGER, EXTERNAL :: numchars
  intrinsic trim


  ! Loop indexes, and error handling.
  integer :: ii

  print *, "init: before loop"
  print *, "init: nhruoutbasefilename = ", nhruoutbasefilename(:numchars(nhruoutbasefilename))
  DO ii = 1, nhruoutvars
      IF (control_string_array(nhruoutvar_names(ii), 'nhruOutVar_names', ii)/=0 ) CALL read_error(5, 'nhruOutVar_names')
      nc_vars(ii) = numchars(nhruoutvar_names(ii))
      file_name = nhruoutbasefilename(:numchars(nhruoutbasefilename))//nhruoutvar_names(ii)(:nc_vars(ii))//'.nc'

      ! Always check the return code of every netCDF function call. In
      ! this example program, wrapping netCDF calls with "call check()"
      ! makes sure that any return which is not equal to nf90_noerr (0)
      ! will print a netCDF error message and exit.
    
      ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
      ! overwrite this file, if it already exists.
      print *, "init: file_name = ", file_name
      call check( nf90_create(file_name, NF90_CLOBBER, ncid) )

      ! Define the dimensions. NetCDF will hand back an ID for each.
      call check( nf90_def_dim(ncid, "x", NX, x_dimid) )
      call check( nf90_def_dim(ncid, "y", NY, y_dimid) )

      ! The dimids array is used to pass the IDs of the dimensions of
      ! the variables. Note that in fortran arrays are stored in
      ! column-major format.
      dimids =  (/ y_dimid, x_dimid /)

      ! Define the variable. The type of the variable in this case is
      ! NF90_INT (4-byte integer).
      call check( nf90_def_var(ncid, "data", NF90_INT, dimids, varid) )

      ! End define mode. This tells netCDF we are done defining metadata.
      call check( nf90_enddef(ncid) )
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

      USE PRMS_MODULE, ONLY: Timestep
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday

!      IMPLICIT NONE
!! FUNCTIONS AND SUBROUTINES
!      INTRINSIC SNGL, DBLE
!      INTEGER, EXTERNAL :: getvar
!      EXTERNAL read_error
! Local Variables

  implicit none

  ! This is the data array we will write. It will just be filled with
  ! a progression of integers for this example.
  integer :: data_out(NY, NX)

!      INTEGER :: j, i, jj, write_month, write_year, last_day
   integer :: x, y
!***********************************************************************

         print *, Nowyear, "-", Nowmonth, "-", Nowday, "   ", timestep
  ! Create some pretend data. If this wasn't an example program, we
  ! would have some real data to write, for example, model output.
  if (timestep .eq. 1) then
      do x = 1, NX
         do y = 1, NY
            data_out(y, x) = (x - 1) * NY + (y - 1)
         end do
      end do

      call check( nf90_put_var(ncid, varid, data_out) )

  endif
 
!      DO jj = 1, nhruoutvars
!
!marks nc_vars is the number of characters in nhruoutvar_names
!
!          IF (getvar(MODNAME, nhruoutvar_names(jj)(:nc_vars(jj)), Nhru, 'real', nhru_var_daily(1, jj))/=0 ) then
!             CALL read_error(4, nhruoutvar_names(jj)(:nc_vars(jj)))
!          endif
!
!         print *, "      ", nhruoutvar_names(jj)(:nc_vars(jj)), nhru_var_daily(1,jj), nhru_var_daily(nhru,jj)
!
!
!
!      ENDDO

      END SUBROUTINE nhru_ncf_run

!##########################################################################################################
      subroutine nhru_ncf_clean()
      USE PRMS_NHRU_NCF
      use netcdf
!         integer :: status
!         character :: foobar
!
!
!
         print *, "   nhru_ncf_clean: close the ncf files"
  ! Close the file. This frees up any internal netCDF resources
  ! associated with the file, and flushes any buffers.
      call check( nf90_close(ncid) )

      print *, "*** SUCCESS writing example file simple_xy.nc! "
!
!  ! Close the file. This frees up any internal netCDF resources
!  ! associated with the file, and flushes any buffers.
!      DO jj = 1, nhruoutvars
!         status = nf90_close(ncid)
!
!         if(status /= nf90_noerr) then 
!            foobar = nf90_strerror(status)
!!            print *, "bullshit", foobar
!!              print *, ncid, "clean", foobar
!!            stop "Stopped"
!         end if
!      enddo
!              print *, ncid, "init", foobar
      end subroutine nhru_ncf_clean
