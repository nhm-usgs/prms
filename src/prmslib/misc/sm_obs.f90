submodule (PRMS_OBS) sm_obs
contains
  !***********************************************************************
  ! Obs constructor
  module function constructor_Obs(ctl_data) result(this)
    use prms_constants, only: dp
    implicit none

    type(Obs) :: this
    type(Control), intent(in) :: ctl_data

    ! Local variables
    ! character(LEN=11) :: modname_rst
      !! Used to verify module name when reading from restart file

    ! ------------------------------------------------------------------------
    ! associate(nevap => ctl_data%nevap%value, &
    !           nhumid => ctl_data%nhumid%value, &
    !           nlakeelev => ctl_data%nlakeelev%value, &
    !           nobs => ctl_data%nobs%value, &
    !           nrain => ctl_data%nrain%value, &
    !           nratetbl => ctl_data%nratetbl%value, &
    !           nsnow => ctl_data%nsnow%value, &
    !           nsol => ctl_data%nsol%value, &
    !           ntemp => ctl_data%ntemp%value, &
    !           nwind => ctl_data%nwind%value, &
    !           init_vars_from_file => ctl_data%init_vars_from_file%value, &
    !           rst_unit => ctl_data%restart_output_unit, &
    !           print_debug => ctl_data%print_debug%value)
    !
    !   call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)
    !
    !   if (print_debug > -2) then
    !     ! Output module and version information
    !     call this%print_module_info()
    !   endif
    !
    !   if (nobs > 0) then
    !     allocate(this%runoff(nobs))
    !     allocate(this%streamflow_cfs(nobs))
    !     allocate(this%streamflow_cms(nobs))
    !
    !     if (init_vars_from_file == 0) then
    !       this%runoff = 0.0
    !       this%streamflow_cfs = 0.0_dp
    !       this%streamflow_cms = 0.0_dp
    !     endif
    !   endif
    !
    !   if (nrain > 0) then
    !     allocate(this%precip(nrain))
    !
    !     if (init_vars_from_file == 0) then
    !       this%precip = 0.0
    !     endif
    !   endif
    !
    !   if (ntemp > 0) then
    !     allocate(this%tmin(ntemp))
    !     allocate(this%tmax(ntemp))
    !
    !     if (init_vars_from_file == 0) then
    !       this%tmax = 0.0
    !       this%tmin = 0.0
    !     endif
    !   endif
    !
    !   if (nsol > 0) then
    !     allocate(this%solrad(nsol))
    !     this%solrad = 0.0
    !   endif
    !
    !   if (nsnow > 0) then
    !     allocate(this%snowdepth(nsnow))
    !     this%snowdepth = 0.0
    !   endif
    !
    !   if (nevap > 0) then
    !     allocate(this%pan_evap(nevap))
    !     this%pan_evap = 0.0
    !   endif
    !
    !   if (nhumid > 0) then
    !     allocate(this%humidity(nhumid))
    !     this%humidity = 0.0
    !   endif
    !
    !   if (nwind > 0) then
    !     allocate(this%wind_speed(nwind))
    !     this%wind_speed = 0.0
    !   endif
    !
    !   ! Lake variables
    !   if (nratetbl > 0) then
    !     allocate(this%gate_ht(nratetbl))
    !     this%gate_ht = 0.0
    !   endif
    !
    !   if (nlakeelev > 0) then
    !     allocate(this%lake_elev(nlakeelev))
    !     this%lake_elev = 0.0
    !   endif
    !
    !   if (init_vars_from_file == 1) then
    !   !     read(rst_unit) modname_rst
    !   !     call check_restart(MODNAME, modname_rst)
    !   !     read(rst_unit) nrain_test, ntemp_test, nobs_test
    !   !     ierr = 0
    !   !
    !   !     call check_restart_dimen('nrain', nrain_test, nrain, ierr)
    !   !     call check_restart_dimen('ntemp', ntemp_test, ntemp, ierr)
    !   !     call check_restart_dimen('nobs', nobs_test, nobs, ierr)
    !   !     if (ierr == 1) STOP
    !   !
    !   !     if (nrain > 0) read (rst_unit) this%precip
    !   !
    !   !     if (ntemp > 0) then
    !   !       read(rst_unit) this%tmax
    !   !       read(rst_unit) this%tmin
    !   !     endif
    !   !
    !   !     if (nobs > 0) then
    !   !       read(rst_unit) this%runoff
    !   !       read(rst_unit) this%streamflow_cfs
    !   !       read(rst_unit) this%streamflow_cms
    !   !     endif
    !   endif
    ! end associate
  end function


  module subroutine cleanup_Obs(this, ctl_data)
    implicit none

    class(Obs), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! ------------------------------------------------------------------------
    ! associate(nrain => ctl_data%nrain%value, &
    !           ntemp => ctl_data%ntemp%value, &
    !           nobs => ctl_data%nobs%value, &
    !           rst_unit => ctl_data%restart_output_unit)
    !
    !   write(rst_unit) MODNAME
    !   write(rst_unit) nrain, ntemp, nobs
    !
    !   if (nrain > 0) then
    !     write(rst_unit) this%precip
    !   endif
    !
    !   if (ntemp > 0) then
    !     write(rst_unit) this%tmax
    !     write(rst_unit) this%tmin
    !   endif
    !
    !   if (nobs > 0) then
    !     write(rst_unit) this%runoff
    !     write(rst_unit) this%streamflow_cfs
    !     write(rst_unit) this%streamflow_cms
    !   endif
    ! end associate
  end subroutine


  ! module subroutine run_Obs(this, ctl_data, param_data, model_time, model_basin)
  !   use prms_constants, only: CFS2CMS_CONV
  !   implicit none
  !
  !   class(Obs), intent(inout) :: this
  !   type(Control), intent(in) :: ctl_data
  !   type(Parameters), intent(in) :: param_data
  !   type(Time_t), intent(in) :: model_time
  !   type(Basin), intent(in) :: model_basin
  !
  !   ! Local Variables
  !   ! integer(i32) :: i
  !
  !   ! Control
  !   ! nevap, nobs, nrain, nsnow, nsol, ntemp,
  !   ! precip_module
  !
  !   ! Parameter
  !   ! rain_code, runoff_units
  !
  !   ! Basin
  !
  !   ! Time_t
  !   ! Nowmonth
  !
  !   ! --------------------------------------------------------------------------
  !   ! TODO: Need to write a read_var routine before this can be used.
  !   ! if (nobs > 0) then
  !   !   if ( readvar(MODNAME, 'runoff')/=0 ) call read_error(9, 'runoff')
  !   !
  !   !   if (runoff_units == 1) then
  !   !     do i=1, nobs
  !   !       this%streamflow_cms(i) = dble(this%runoff(i))
  !   !       this%streamflow_cfs(i) = this%streamflow_cms(i) / CFS2CMS_CONV
  !   !     enddo
  !   !   else
  !   !     do i=1, nobs
  !   !       this%streamflow_cfs(i) = dble(this%runoff(i))
  !   !       this%streamflow_cms(i) = this%streamflow_cfs(i) * CFS2CMS_CONV
  !   !     enddo
  !   !   endif
  !   ! endif
  !   !
  !   ! if (nrain > 0) then
  !   !   if ( readvar(MODNAME, 'precip')/=0 ) call read_error(9, 'precip')
  !   ! endif
  !   !
  !   ! if (ntemp > 0) then
  !   !   if ( readvar(MODNAME, 'tmax')/=0 ) call read_error(9, 'tmax')
  !   !   if ( readvar(MODNAME, 'tmin')/=0 ) call read_error(9, 'tmin')
  !   ! endif
  !   !
  !   ! if (nsol > 0) then
  !   !   if ( readvar(MODNAME, 'solrad')/=0 ) call read_error(9, 'solrad')
  !   ! endif
  !   !
  !   ! if (nevap > 0) then
  !   !   if ( readvar(MODNAME, 'pan_evap')/=0 ) call read_error(9, 'pan_evap')
  !   ! endif
  !   !
  !   ! if (nsnow > 0) then
  !   !   if ( readvar(MODNAME, 'snowdepth')/=0 ) call read_error(9, 'snowdepth')
  !   ! endif
  !   !
  !   ! if (precip_module%s == 'xyz_dist') then
  !   !   if (rain_code(Nowmonth) == 4) then
  !   !     if ( readvar(MODNAME, 'rain_day')/=0 ) call read_error(9, 'rain_day')
  !   !   endif
  !   ! endif
  !   !
  !   ! if (nlakeelev > 0) then
  !   !   if ( readvar(MODNAME, 'lake_elev')/=0 ) call read_error(9, 'lake_elev')
  !   ! endif
  !   !
  !   ! if (nratetbl > 0) then
  !   !   if ( readvar(MODNAME, 'gate_ht')/=0 ) call read_error(9, 'gate_ht')
  !   ! endif
  !   !
  !   ! if (nhumid > 0) then
  !   !   if ( readvar(MODNAME, 'humidity')/=0 ) call read_error(9, 'humidity')
  !   ! endif
  !   !
  !   ! if (nwind > 0) then
  !   !   if ( readvar(MODNAME, 'wind_speed')/=0 ) call read_error(9, 'wind_speed')
  !   ! endif
  !
  ! end subroutine


  !***********************************************************************
  ! check_data_variables - Check data variables and dimensions
  !***********************************************************************
  ! subroutine check_data_variables(Varname, Numvalues, Values, Iflag, Iret)
  !     use prms_constants, only: CFS2CMS_CONV
  !     implicit none
  !
  !     ! Arguments
  !     character(len=*), intent(in) :: Varname
  !     integer(i32), intent(in) :: Numvalues
  !     integer(i32), intent(in) :: Iflag
  !     integer(i32), intent(out) :: Iret
  !     real(r32), intent(in) :: Values(Numvalues)
  !
  !     ! Local Variables
  !     integer(i32) :: ndim, i
  !
  !     !***********************************************************************
  !     Iret = 0
  !
  !     if (Varname == 'tmax') then
  !         if (Iflag == 0) then
  !             if (Numvalues /= ntemp) then
  !                 Iret = -1
  !                 ndim = ntemp
  !             endif
  !         else
  !             do i = 1, Numvalues
  !                 Tmax(i) = Values(i)
  !             enddo
  !         endif
  !     elseif (Varname == 'tmin') then
  !         if (Iflag == 0) then
  !             if (Numvalues /= ntemp) then
  !                 Iret = -1
  !                 ndim = ntemp
  !             endif
  !         else
  !             do i = 1, Numvalues
  !                 Tmin(i) = Values(i)
  !             enddo
  !         endif
  !     elseif (Varname == 'precip') then
  !         if (Iflag == 0) then
  !             if (Numvalues /= nrain) then
  !                 Iret = -1
  !                 ndim = nrain
  !             endif
  !         else
  !             do i = 1, Numvalues
  !                 Precip(i) = Values(i)
  !             enddo
  !         endif
  !     elseif (Varname == 'runoff') then
  !         if (Iflag == 0) then
  !             if (Numvalues /= nobs) then
  !                 Iret = -1
  !                 ndim = nobs
  !             endif
  !         else
  !             do i = 1, Numvalues
  !                 this%runoff(i) = Values(i)
  !             enddo
  !             if (ALL(runoff_units == 1)) then
  !                 do i = 1, nobs
  !                     this%streamflow_cms(i) = dble(this%runoff(i))
  !                     this%streamflow_cfs(i) = this%streamflow_cms(i) / CFS2CMS_CONV
  !                 enddo
  !             else
  !                 do i = 1, nobs
  !                     this%streamflow_cfs(i) = dble(this%runoff(i))
  !                     this%streamflow_cms(i) = this%streamflow_cfs(i) * CFS2CMS_CONV
  !                 enddo
  !             endif
  !         endif
  !     else
  !         print *, 'ERROR, data variable: ', Varname, ' is not valid'
  !         Iret = -2
  !     endif
  !     if (Iret == -1) then
  !         print *, 'ERROR, number of values for data variable: ', Varname
  !         print *, 'does not equal dimension value; data values:', Numvalues, ' dimension:', ndim
  !     endif
  ! end subroutine check_data_variables



  !***********************************************************************
  ! Read PRMS Data File line
  !***********************************************************************
  ! subroutine read_data_line(curr_time, var_data)
  !     use iso_fortran_env
  !     use variables_arr_mod, only: variables_arr_t
  !     use UTILS_PRMS, only: read_error
  !     implicit none
  !
  !     ! Arguments
  !     integer(i32), intent(in) :: curr_time(6)
  !     type(variables_arr_t), intent(inout) :: var_data
  !
  !     ! Functions
  !     INTRINSIC TRANSFER
  !
  !     ! Local Variables
  !     integer(i32) :: datatime(6)
  !     integer(i32) :: jj
  !     integer(i32) :: ios
  !     integer(i32) :: column_end
  !     integer(i32) :: column
  !     integer(i32) :: nvals
  !     integer(i32) :: var_id
  !
  !     !***********************************************************************
  !     read (Datafile_unit, *, IOSTAT=ios) datatime, (Data_line_values(jj), jj=1, Num_datafile_columns)
  !     if (ios /= 0) then
  !         print *, 'ERROR on:', curr_time(1), curr_time(2), curr_time(3)
  !
  !         if (ios /= 0) then
  !             if (ios == IOSTAT_END) call read_error(13, 'hit end of file')
  !             call read_error(13, 'invalid line')
  !         endif
  !     endif
  !
  !     if (datatime(1) /= curr_time(1) .OR. datatime(2) /= curr_time(2) .OR. datatime(3) /= curr_time(3)) then
  !         print *, 'ERROR on: ', curr_time(1), curr_time(2), curr_time(3), 'Data File date:', datatime(1), datatime(2), datatime(3)
  !         call read_error(13, 'data file date does not match time step date')
  !     endif
  !
  !     column_end = 0
  !     column = 1
  !
  !     do jj = 1, Num_datafile_types
  !         nvals = var_data%getvarnvals(Data_varname(jj)%str)
  !         var_id = var_data%getvar_id(Data_varname(jj)%str)
  !         column_end = column_end + nvals
  !
  !         var_data%Variable_data(var_id)%values_real = TRANSFER(Data_line_values(column:column_end), var_data%Variable_data(var_id)%values_real)
  !
  !         call check_data_variables(Data_varname(jj)%str, nvals, Data_line_values(column:column_end), 1, ios)
  !         if (ios /= 0) then
  !             print *, 'ERROR, Data File corrupted. Reading variable: ', Data_varname(jj)
  !             print *, 'Date:', curr_time(1), curr_time(2), curr_time(3)
  !             STOP
  !         endif
  !         column = column + nvals
  !     enddo
  ! end subroutine read_data_line



  ! subroutine read_prms_data_file(data_filename)
  !   use iso_fortran_env
  !   use prms_constants, only: EQULS, MAXFILE_LENGTH
  !   use fileio_mod, only: write_outfile
  !   use PRMS_MODULE, only: PRMS_output_unit, Print_debug, Starttime, Endtime, print_module
  !   use UTILS_PRMS, only: read_error, PRMS_open_input_file, find_current_time
  !   implicit none
  !
  !   character(:), allocatable, intent(in) :: data_filename
  !
  !   ! Functions
  !   INTRINSIC LEN_TRIM, TRIM
  !
  !   ! Local Variables
  !   character(len=MAXFILE_LENGTH) :: data_line, dmy
  !   character(len=80) :: line
  !   integer(i32) :: ierr, ios    ! , n, numchrs, length
  !   integer(i32) :: startyr, startmo, startdy, starthr, startmn, startsec
  !   integer(i32) :: endyr, endmo, enddy, endhr, endmn, endsec, num_vars
  !   real(r32), allocatable :: var(:)
  !   character(len=80), save :: Version_read_data_file
  !   integer(i32) :: vnum_tmp
  !   character(len=:), allocatable :: vline_tmp
  !   character(len=:), allocatable :: vname_tmp
  !   integer(i32) :: idx1 ! , idx2
  !
  !   !***********************************************************************
  !   Version_read_data_file = 'read_data_file.f90 2017-09-29 13:49:00Z'
  !   call print_module(Version_read_data_file, 'Read Data File              ', 90)
  !
  !   call PRMS_open_input_file(Datafile_unit, data_filename, 'data_file', 0, ios)
  !   if (ios /= 0) STOP
  !
  !   call write_outfile(' ')
  !   call write_outfile(EQULS)
  !   call write_outfile('Using PRMS Data File: ' // data_filename)
  !
  !   ! Echo Data File Header and comment lines
  !   read (Datafile_unit, FMT = '(A)', IOSTAT = ios) line
  !
  !   if (ios /= 0) call read_error(13, 'title')
  !   call write_outfile('Title: ' // TRIM(line))
  !   call write_outfile(EQULS)
  !   call write_outfile('Comment lines:')
  !   num_vars = 0
  !
  !   do
  !     ! Output each comment line
  !     read (Datafile_unit, FMT = '(A)', IOSTAT = ios) line
  !     if (ios == IOSTAT_END) call read_error(13, 'invalid Data File, end of file reached')
  !     if (ios /= 0) call read_error(13, 'comment')
  !     if (line(:4) == '    ') cycle
  !     if (line(:2) == '//') call write_outfile(TRIM(line))
  !     num_vars = num_vars + 1
  !     if (line(:4) == '####') EXIT
  !   enddo
  !
  !   if (line(:4) /= '####') STOP 'ERROR, invalid Data File, data section not found'
  !   call write_outfile(EQULS)
  !   call write_outfile('measured variables')
  !
  !   ! read variables and number
  !   rewind (Datafile_unit)
  !   read (Datafile_unit, FMT = '(A)') line ! skip first line
  !
  !   ! 2018-01-12 PAN: num_vars as computed results in an array of
  !   !                 strings that is larger than the actual number
  !   !                 of variables in the data file.
  !   allocate (Data_varname(num_vars), Data_varnum(num_vars))
  !   ! Data_varname = '                '
  !   Data_varnum = 0
  !   Num_datafile_columns = 0
  !   Num_datafile_types = 0
  !   ierr = 0
  !
  !   ! Get each type of variable (e.g. runoff) and how many
  !   ! entries are in the data section
  !   do
  !     read (Datafile_unit, FMT='(A)') line
  !     if (line(:4) == '    ' .OR. line(:2) == '//') cycle
  !     if (line(:4) == '####') EXIT
  !
  !     vline_tmp = trim(line)  ! create version of line without trailing spaces
  !
  !     ! Output the line to the outfile
  !     call write_outfile(vline_tmp)
  !
  !     idx1 = index(vline_tmp, ' ')    ! index to first space in line
  !
  !     vname_tmp = vline_tmp(:idx1-1)  ! name of variable
  !
  !     read(vline_tmp(idx1:), *) vnum_tmp  ! number of columns for the variable
  !
  !     if (vnum_tmp == 0) then
  !       if (Print_debug > -1) print *, 'Variable: ', vname_tmp, ' ignored as number of values = 0'
  !       cycle
  !     endif
  !
  !     Num_datafile_types = Num_datafile_types + 1
  !     Data_varname(Num_datafile_types)%str = vname_tmp
  !     Data_varnum(Num_datafile_types) = vnum_tmp
  !     Num_datafile_columns = Num_datafile_columns + vnum_tmp
  !
  !     allocate(var(vnum_tmp))
  !     call check_data_variables(Data_varname(Num_datafile_types)%str, vnum_tmp, var, 0, ios)
  !     deallocate(var)
  !     if (ios /= 0) ierr = ios
  !   enddo
  !
  !   if (ierr == 1) STOP
  !   call write_outfile(EQULS)
  !   allocate(Data_line_values(Num_datafile_columns))
  !
  !   read(Datafile_unit, *, IOSTAT=ios) startyr, startmo, startdy, starthr, startmn, startsec
  !   if (ios /= 0) call read_error(13, 'first data line')
  !
  !   do
  !     read (Datafile_unit, '(A)', IOSTAT=ios) dmy
  !     if (ios == -1) EXIT ! found end of file
  !     if (ios /= 0) call read_error(13, 'data line')
  !     data_line = dmy
  !   enddo
  !
  !   read(data_line, *, IOSTAT=ios) endyr, endmo, enddy, endhr, endmn, endsec
  !   write(PRMS_output_unit, 10) ' Data File', startyr, startmo, startdy, starthr, startmn, startsec, &
  !                                              endyr, endmo, enddy, endhr, endmn, endsec
  !   10   FORMAT (A, ' time period:', I5.4, 2('/', I2.2), I3.2, 2(':', I2.2), ' to', I5.4, 2('/', I2.2), I3.2, 2(':', I2.2))
  !   write(PRMS_output_unit, 10) 'Simulation', Starttime, Endtime
  !   call write_outfile(EQULS)
  !
  !   ! check start and end times, if not valid stop with print
  !   ierr = 0
  !   if (Starttime(1) < startyr) then
  !     ierr = 1
  !   elseif (Starttime(1) == startyr) then
  !     if (Starttime(2) < startmo) then
  !       ierr = 1
  !     elseif (Starttime(2) == startmo .AND. Starttime(3) < startdy) then
  !       ierr = 1
  !     endif
  !   endif
  !
  !   if (ierr == 1) STOP 'ERROR, simulation time begins before Data File'
  !
  !   ierr = 0
  !   if (Endtime(1) > endyr) then
  !     ierr = 1
  !   elseif (Endtime(1) == endyr) then
  !     if (Endtime(2) > endmo) then
  !       ierr = 1
  !     elseif (Endtime(2) == endmo .AND. Endtime(3) > enddy) then
  !       ierr = 1
  !     endif
  !   endif
  !
  !   if (ierr == 1) STOP 'ERROR, simulation end time exceeds Data File'
  !
  !   ! read to start of data
  !   rewind Datafile_unit
  !
  !   do
  !     read(Datafile_unit, FMT='(A)') data_line
  !     if (data_line(:4) == '####') EXIT
  !   enddo
  !
  !   call find_current_time(Datafile_unit, Starttime(1), Starttime(2), Starttime(3), ios, 0)
  !   if (ios /= 0) then
  !     print *, 'End of file or error reading Data File to find the first simulation time step'
  !     print *, 'Data File: ', data_filename
  !     STOP
  !   endif
  ! end subroutine read_prms_data_file

end submodule
