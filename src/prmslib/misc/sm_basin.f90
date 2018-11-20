submodule (PRMS_BASIN) sm_basin
contains
  !***********************************************************************
  ! Basin constructor
  module function constructor_Basin(ctl_data, param_data) result(this)
    use prms_constants, only: INACTIVE, LAND, LAKE, SWALE, NORTHERN, SOUTHERN, &
                              BCWEIR, GATEOP, PULS, LINEAR
    implicit none

    type(Basin) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Local variables
    real(r64) :: basin_dprst = 0.0
    real(r64) :: basin_perv = 0.0
    real(r64) :: basin_imperv = 0.0

    ! character(len=69) :: buffer
    integer(i32) :: chru
      !! Current HRU
    integer(i32) :: ii
      !! General counter
    integer(i32) :: j
      !! General counter
    integer(i32) :: lakeid

    ! --------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              nlake => ctl_data%nlake%value, &
              cascadegw_flag => ctl_data%cascadegw_flag%value, &
              dprst_flag => ctl_data%dprst_flag%value, &
              et_module => ctl_data%et_module%values, &
              gsflow_mode => ctl_data%gsflow_mode, &
              precip_module => ctl_data%precip_module%values, &
              print_debug => ctl_data%print_debug%value, &
              ! model_mode => ctl_data%model_mode%values, &
              model_output_unit => ctl_data%model_output_unit, &
              stream_temp_flag => ctl_data%stream_temp_flag%value, &
              strmflow_module => ctl_data%strmflow_module%values, &

              dprst_frac => param_data%dprst_frac%values, &
              dprst_frac_open => param_data%dprst_frac_open%values, &
              hru_area => param_data%hru_area%values, &
              hru_lat => param_data%hru_lat%values, &
              hru_percent_imperv => param_data%hru_percent_imperv%values, &
              hru_type => param_data%hru_type%values, &
              lake_hru_id => param_data%lake_hru_id%values, &
              lake_type => param_data%lake_type%values)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      allocate(this%hru_area_dble(nhru))

      allocate(this%hru_frac_perv(nhru))
      allocate(this%hru_area_imperv(nhru))
      allocate(this%hru_area_perv(nhru))

      if (dprst_flag == 1) then
        allocate(this%dprst_area_max(nhru))
      endif

      ! Create mask of only active land and swale HRUs
      allocate(this%active_mask(nhru))
      this%active_mask = .false.
      where (hru_type /= INACTIVE) this%active_mask = .true.

      ! Allocate the hru_route_order array for the number of active HRUs
      allocate(this%hru_route_order(count(this%active_mask)))

      ! if (model_mode(1)%s /= 'GSFLOW' .or. cascadegw_flag > 0) then
      if (.not. gsflow_mode .or. cascadegw_flag > 0) then
        allocate(this%gwr_route_order(nhru))
      endif

      ! NOTE: It appears that hru_elev_feet is no longer used.
      ! NOTE: Elevation will be expected to use units meters.
      ! if (et_module(1)%s == 'potet_pm' .or. et_module(1)%s == 'potet_pm_sta' .or. &
      !     et_module(1)%s == 'potet_pt') then
      !   allocate(this%hru_elev_feet(nhru))
      !   allocate(this%hru_elev_meters(nhru))
      ! elseif (precip_module(1)%s == 'ide_dist' .or. stream_temp_flag == 1) then
      !   allocate(this%hru_elev_meters(nhru))
      ! endif

      if (nlake > 0) then
        allocate(this%lake_area(nlake))
        this%lake_area = 0.0_dp
      endif

      this%hru_area_dble = dble(hru_area)

      ! Populate hru_route_order with indices of active HRUs
      j = 0
      do chru=1, nhru
        if (this%active_mask(chru)) then
          j = j + 1
          this%hru_route_order(j) = chru
        endif
      enddo
      this%active_hrus = j

      ! if (model_mode(1)%s /= 'GSFLOW' .or. cascadegw_flag > 0) then
      if (.not. gsflow_mode .or. cascadegw_flag > 0) then
         this%active_gwrs = this%active_hrus

         ! WARNING: This modifies a parameter
         ! this%gwr_type = this%hru_type

         this%gwr_route_order = this%hru_route_order
      endif

      ! Total HRU area in the model
      this%total_area = sum(dble(hru_area))

      ! Total active HRU area in the model
      ! TODO: fix for lakes with multiple HRUs and PRMS lake routing
      ! NOTE: land_area currently includes lake HRUs
      this%land_area = sum(dble(hru_area), mask=this%active_mask)

      ! Total HRU area that is active in model.
      this%active_area = this%land_area
      this%basin_area_inv = 1.0_dp / this%active_area

      ! Compute rough center latitude of the model area
      this%basin_lat = sum(dble(hru_lat * hru_area), mask=this%active_mask) * this%basin_area_inv

      where (this%active_mask)
        this%hru_area_imperv = hru_percent_imperv * hru_area
        this%hru_area_perv = hru_area - this%hru_area_imperv
        this%hru_frac_perv = this%hru_area_perv / hru_area
      end where

      if (dprst_flag == 1) then
        this%dprst_area_max = dprst_frac * hru_area

        where (this%active_mask)
          this%hru_area_perv = this%hru_area_perv - this%dprst_area_max

          ! Recompute hru_frac_perv to reflect the depression storage area
          this%hru_frac_perv = this%hru_area_perv / hru_area
        end where

        basin_dprst = sum(dble(this%dprst_area_max))
      endif

      basin_perv = sum(dble(this%hru_area_perv)) * this%basin_area_inv
      basin_imperv = sum(dble(this%hru_area_imperv)) * this%basin_area_inv

      ! TODO: 2018-06-21 PAN - Hook up the lake stuff
      this%weir_gate_flag = 0
      this%puls_lin_flag = 0
      this%water_area = 0.0_dp

      this%numlakes_check = 0
      this%numlake_hrus = 0

      ! if (nlake > 0 .and. strmflow_module(1)%s == 'muskingum_lake' .and. model_mode(1)%s /= 'GSFLOW') then
      if (nlake > 0 .and. strmflow_module(1)%s == 'muskingum_lake' .and. .not. gsflow_mode) then
        if (any([BCWEIR, GATEOP]==lake_type)) then
          this%weir_gate_flag = 1
        endif

        if (any([PULS, LINEAR]==lake_type)) then
          this%puls_lin_flag = 1
        endif
      endif

      ! TODO: 2018-06-21 - more lake stuff to integrate
      do ii=1, nhru
        if (hru_type(ii) == LAKE) then
          this%numlake_hrus = this%numlake_hrus + 1
          this%water_area = this%water_area + this%hru_area_dble(ii)
          lakeid = lake_hru_id(ii)

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

          this%hru_frac_perv(ii) = 1.0
          this%hru_area_imperv(ii) = 0.0
          this%hru_area_perv(ii) = hru_area(ii)
        endif
      enddo

      ! Used in solrad modules to winter/summer radiation adjustment
      if (this%basin_lat > 0.0_dp) then
        this%hemisphere = NORTHERN
      else
        this%hemisphere = SOUTHERN
      endif

      if (print_debug == 2) then
        write(output_unit, *) ' HRU     Area'
        write(output_unit, fmt='(i7, f14.5)') (j, hru_area(j), j=1, nhru)
        write(output_unit, 9001) 'Model domain area   =', this%total_area
        write(output_unit, 9001) 'Active basin area   =', this%active_area
        write(output_unit, 9001) 'Fraction impervious =', basin_imperv
        write(output_unit, 9001) 'Fraction pervious   =', basin_perv
        write(output_unit, *) ' '

        9001 format(a, 1x, f15.5)
      endif

      if (print_debug > -2) then
        write(model_output_unit, fmt='(1X)')
        write(model_output_unit, 9003) 'Model domain area:   ', this%total_area, '    Active basin area:', this%active_area

        write(model_output_unit, 9004) 'Fraction impervious:  ', basin_imperv, '    Fraction pervious: ', basin_perv
        write(model_output_unit, fmt='(/)')

        ! 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
        9003 FORMAT(2(a, f13.2))
        9004 FORMAT(2(a, f12.5))
        ! 9005 FORMAT (A, F13.2, A, F13.4)
      endif
    end associate
  end function
end submodule
