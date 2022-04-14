submodule (PRMS_BASIN) sm_basin
contains
  !***********************************************************************
  ! Basin constructor
  module function constructor_Basin(ctl_data) result(this)
    use prms_constants, only: INACTIVE, LAND, LAKE, SWALE, NORTHERN, SOUTHERN, &
                              BCWEIR, GATEOP, PULS, LINEAR
    use UTILS_PRMS, only: open_dyn_param_file, get_first_time, get_next_time
    implicit none

    type(Basin) :: this
    type(Control), intent(in) :: ctl_data

    ! Local variables
    ! real(r64) :: basin_perv = 0.0
    ! real(r64) :: basin_imperv = 0.0

    ! character(len=69) :: buffer
    integer(i32) :: chru
      !! Current HRU
    integer(i32) :: ierr
    integer(i32) :: ii
      !! General counter
    integer(i32) :: j
      !! General counter
    integer(i32) :: lakeid

    ! --------------------------------------------------------------------------
    associate(cascadegw_flag => ctl_data%cascadegw_flag%value, &
              covtype_dynamic => ctl_data%covtype_dynamic%values(1), &
              dyn_covtype_flag => ctl_data%dyn_covtype_flag%value, &
              et_module => ctl_data%et_module%values, &
              gsflow_mode => ctl_data%gsflow_mode, &
              precip_module => ctl_data%precip_module%values, &
              print_debug => ctl_data%print_debug%value, &
              ! model_mode => ctl_data%model_mode%values, &
              model_output_unit => ctl_data%model_output_unit, &
              save_vars_to_file => ctl_data%save_vars_to_file%value, &
              start_time => ctl_data%start_time%values, &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmflow_module => ctl_data%strmflow_module%values, &
              param_hdl => ctl_data%param_file_hdl)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Load dimensions
      this%nhru = param_hdl%get_dimension('nhru')
      this%nsegment = param_hdl%get_dimension('nsegment')
      this%nsub = param_hdl%get_dimension('nsub')
      this%nmonths = param_hdl%get_dimension('nmonths')
      ! TODO: additional dimensions nlake and nobs

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Load parameters
      allocate(this%cov_type(this%nhru))
      call param_hdl%get_variable('cov_type', this%cov_type)

      allocate(this%hru_area(this%nhru))
      call param_hdl%get_variable('hru_area', this%hru_area)

      allocate(this%hru_aspect(this%nhru))
      call param_hdl%get_variable('hru_aspect', this%hru_aspect)

      allocate(this%hru_lat(this%nhru))
      call param_hdl%get_variable('hru_lat', this%hru_lat)

      allocate(this%hru_slope(this%nhru))
      call param_hdl%get_variable('hru_slope', this%hru_slope)

      allocate(this%hru_type(this%nhru))
      call param_hdl%get_variable('hru_type', this%hru_type)

      allocate(this%nhm_id(this%nhru))
      call param_hdl%get_variable('nhm_id', this%nhm_id)

      if (this%nsegment > 0) then
        allocate(this%nhm_seg(this%nsegment))
        call param_hdl%get_variable('nhm_seg', this%nhm_seg)
      end if

      ! TODO: parameters that aren't coded yet
      ! hru_elev
      ! hru_lon
      ! hru_x
      ! hru_y
      ! lake_hru_id
      ! lake_type

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! If requested, open the covtype_dynamic file
      if (dyn_covtype_flag == 1) then
        write(output_unit, *) MODNAME, '%init() INFO: Dynamic cov_type file: ', covtype_dynamic%s
        call open_dyn_param_file(this%covtype_unit, ierr, covtype_dynamic%s, 'covtype_dynamic')
        if (ierr /= 0) then
          write(output_unit, *) MODNAME, '%init() ERROR opening dynamic cov_type parameter file.'
          stop
        end if

        this%next_dyn_covtype_date = get_first_time(this%covtype_unit, start_time(1:3))
        ! write(output_unit, *) MODNAME, '%init() INFO: Dynamic cov_type next avail time: ', this%next_dyn_covtype_date

        allocate(this%covtype_chgs(this%nhru))

        ! Open the output unit for summary information
        open(NEWUNIT=this%dyn_output_unit, STATUS='REPLACE', FILE='dyn_covtype.out')
      end if

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Allocate non-parameter arrays
      allocate(this%hru_area_dble(this%nhru))

      ! Create mask of only active land and swale HRUs
      allocate(this%active_mask(this%nhru))
      this%active_mask = .false.
      where (this%hru_type /= INACTIVE) this%active_mask = .true.

      ! Allocate the hru_route_order array for the number of active HRUs
      allocate(this%hru_route_order(count(this%active_mask)))

      ! if (model_mode(1)%s /= 'GSFLOW' .or. cascadegw_flag > 0) then
      if (.not. gsflow_mode .or. cascadegw_flag > 0) then
        allocate(this%gwr_route_order(this%nhru))
      endif

      if (this%nlake > 0) then
        allocate(this%lake_area(this%nlake))
        this%lake_area = 0.0_dp
      endif

      this%hru_area_dble = dble(this%hru_area)

      ! Populate hru_route_order with indices of active HRUs
      j = 0
      do chru=1, this%nhru
        if (this%active_mask(chru)) then
          j = j + 1
          this%hru_route_order(j) = chru
        endif
      enddo
      this%active_hrus = j

      if (.not. gsflow_mode .or. cascadegw_flag > 0) then
         this%active_gwrs = this%active_hrus

         ! WARNING: This modifies a parameter
         ! this%gwr_type = this%hru_type
         this%gwr_route_order = this%hru_route_order
      endif

      ! Total HRU area in the model
      this%total_area = sum(dble(this%hru_area))

      ! Total active HRU area in the model
      ! TODO: fix for lakes with multiple HRUs and PRMS lake routing
      ! NOTE: land_area currently includes lake HRUs
      this%land_area = sum(dble(this%hru_area), mask=this%active_mask)

      ! Total HRU area that is active in model.
      this%active_area = this%land_area
      this%basin_area_inv = 1.0_dp / this%active_area

      ! Compute rough center latitude of the model area
      this%basin_lat = sum(dble(this%hru_lat * this%hru_area), mask=this%active_mask) * this%basin_area_inv

      ! TODO: 2018-06-21 PAN - Hook up the lake stuff
      this%weir_gate_flag = 0
      this%puls_lin_flag = 0
      this%water_area = 0.0_dp

      this%numlakes_check = 0
      this%numlake_hrus = 0

      ! TODO: 2018-11-29 PAN - would this belong better in muskingum_lake?
      ! if (nlake > 0 .and. strmflow_module(1)%s == 'muskingum_lake' .and. model_mode(1)%s /= 'GSFLOW') then
      if (this%nlake > 0 .and. strmflow_module(1)%s == 'muskingum_lake' .and. .not. gsflow_mode) then
        if (any([BCWEIR, GATEOP]==this%lake_type)) then
          this%weir_gate_flag = 1
        endif

        if (any([PULS, LINEAR]==this%lake_type)) then
          this%puls_lin_flag = 1
        endif
      endif

      ! TODO: 2018-06-21 - more lake stuff to integrate
      do ii=1, this%nhru
        if (this%hru_type(ii) == LAKE) then
          this%numlake_hrus = this%numlake_hrus + 1
          this%water_area = this%water_area + this%hru_area_dble(ii)
          lakeid = this%lake_hru_id(ii)

          if (lakeid > 0) then
            this%lake_area(lakeid) = this%lake_area(lakeid) + this%hru_area_dble(ii)

            if (lakeid > this%numlakes_check) then
              this%numlakes_check = lakeid
            endif

          ! TODO: Hook this up
          ! else
          !   print *, 'ERROR, hru_type = 2 for HRU:', ii, ' and lake_hru_id = 0'
          !   cycle
          endif

          ! TODO: Hook this up
          ! if (nlake == 0) then
          !   print *, 'ERROR, hru_type = 2 for HRU:', ii, ' and dimension nlake = 0'
          !   cycle
          ! endif

          ! this%hru_frac_perv(ii) = 1.0
          ! this%hru_area_imperv(ii) = 0.0
          ! this%hru_area_perv(ii) = this%hru_area(ii)
        endif
      enddo

      ! Used in solrad modules to winter/summer radiation adjustment
      if (this%basin_lat > 0.0_dp) then
        this%hemisphere = NORTHERN
      else
        this%hemisphere = SOUTHERN
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Restart variables

      ! TODO: When starting from a restart file check that nhru and nseg from the
      !       restart file match what's read from the parameter file

      if (save_vars_to_file == 1) then
        call ctl_data%add_dimension(dim_name='nhru', dim_size=this%nhru, &
                                    dim_var_name='nhm_id', dim_var_longname='NHM ID', &
                                    datatype=4, units='none')
        call ctl_data%add_dimension(dim_name='nsegment', dim_size=this%nsegment, &
                                    dim_var_name='nhm_seg', dim_var_longname='NHM segment ID', &
                                    datatype=4, units='none')
      end if

      if (print_debug == 2) then
        write(output_unit, *) ' HRU     Area'
        write(output_unit, fmt='(i7, f14.5)') (j, this%hru_area(j), j=1, this%nhru)
        write(output_unit, 9001) 'Model domain area   =', this%total_area
        write(output_unit, 9001) 'Active basin area   =', this%active_area
        ! write(output_unit, 9001) 'Fraction impervious =', basin_imperv
        ! write(output_unit, 9001) 'Fraction pervious   =', basin_perv
        write(output_unit, *) ' '

        9001 format(a, 1x, f15.5)
      endif

      if (print_debug > -2) then
        write(model_output_unit, fmt='(1X)')
        write(model_output_unit, 9003) 'Model domain area:   ', this%total_area, '    Active basin area:', this%active_area

        ! write(model_output_unit, 9004) 'Fraction impervious:  ', basin_imperv, '    Fraction pervious: ', basin_perv
        write(model_output_unit, fmt='(/)')

        ! 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
        9003 FORMAT(2(a, f13.2))
        ! 9004 FORMAT(2(a, f12.5))
        ! 9005 FORMAT (A, F13.2, A, F13.4)
      endif
    end associate
  end function

  module subroutine run_Basin(this, ctl_data, model_time)
    use UTILS_PRMS, only: get_next_time, update_parameter, yr_mo_eq_dy_le
    implicit none

    class(Basin), intent(inout) :: this
      !! Basin class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Time_t), intent(in) :: model_time

    ! --------------------------------------------------------------------------
    associate(dyn_covtype_flag => ctl_data%dyn_covtype_flag%value, &
              curr_time => model_time%Nowtime)

      if (dyn_covtype_flag == 1) then
        if (yr_mo_eq_dy_le(this%next_dyn_covtype_date, curr_time(1:3))) then
          read(this%covtype_unit, *) this%next_dyn_covtype_date, this%covtype_chgs
          ! write(output_unit, 9008) MODNAME, '%run() INFO: covtype was updated. ', this%next_dyn_covtype_date
          ! TODO: some work
          ! update_parameter_i32(ctl_data, model_basin, model_time, dyn_output_unit, dyn_values, param_name, param)
          call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%covtype_chgs, 'cov_type', this%cov_type)
          this%next_dyn_covtype_date = get_next_time(this%covtype_unit)
        end if

        9008 format(A, A, I4, 2('/', I2.2))
      end if
    end associate
  end subroutine

  module subroutine cleanup_Basin(this, ctl_data)
    class(Basin), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! --------------------------------------------------------------------------
    associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
      if (save_vars_to_file == 1) then
        ! Write out this module's restart variables
        call ctl_data%write_restart_variable('nhm_id', this%nhm_id)
        call ctl_data%write_restart_variable('nhm_seg', this%nhm_seg)
      end if
    end associate

  end subroutine

end submodule
