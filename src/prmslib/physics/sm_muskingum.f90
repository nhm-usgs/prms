submodule (PRMS_MUSKINGUM) sm_muskingum
  contains
    module function constructor_Muskingum(ctl_data, param_data, model_basin, &
                                          model_flow, model_route, model_time) result(this)
      use prms_constants, only: dp
      implicit none

      type(Muskingum) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Flowvars), intent(inout) :: model_flow
      type(Routing), intent(inout) :: model_route
      type(Time_t), intent(in) :: model_time

      ! Local variables
      integer(i32) :: i

      ! Control
      ! nsegment, init_vars_from_file,

      ! Parameter
      ! segment_flow_init

      ! Basin
      ! basin_area_inv

      ! Flowvars
      ! seg_outflow

      ! Routing
      ! basin_segment_storage

      ! Time_t
      ! cfs_conv

      ! -----------------------------------------------------------------------
      associate(nsegment => ctl_data%nsegment%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                segment_flow_init => param_data%segment_flow_init%values, &
                basin_area_inv => model_basin%basin_area_inv, &
                seg_outflow => model_flow%seg_outflow, &
                basin_segment_storage => model_route%basin_segment_storage, &
                cfs_conv => model_time%cfs_conv)

        allocate(this%currinsum(nsegment))
        allocate(this%inflow_ts(nsegment))
        allocate(this%outflow_ts(nsegment))
        allocate(this%pastin(nsegment))
        allocate(this%pastout(nsegment))

        if (init_vars_from_file == 0 .or. init_vars_from_file == 2) then
          do i = 1, nsegment
            seg_outflow(i) = segment_flow_init(i)
          enddo

          ! deallocate (segment_flow_init)
        endif

        if (init_vars_from_file==0) then
          this%outflow_ts = 0.0_dp
        endif

        basin_segment_storage = 0.0_dp

        do i = 1, nsegment
          basin_segment_storage = basin_segment_storage + seg_outflow(i)
        enddo

        basin_segment_storage = basin_segment_storage * basin_area_inv / cfs_conv
      end associate
    end function

    module subroutine run_Muskingum(this, ctl_data, param_data, model_basin, &
                                    groundwater, soil, runoff, model_obs, &
                                    model_route, model_time, model_flow)
      use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH
      implicit none

      class(Muskingum) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Gwflow), intent(in) :: groundwater
        !! Groundwater variables
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Obs), intent(in) :: model_obs
      type(Routing), intent(inout) :: model_route
      type(Time_t), intent(in) :: model_time
      type(Flowvars), intent(inout) :: model_flow

      ! Local Variables
      integer(i32) :: i
      integer(i32) :: imod
      integer(i32) :: iorder
      integer(i32) :: j
      integer(i32) :: segtype
      integer(i32) :: toseg
      integer(i32) :: tspd

      real(r64) :: area_fac
      real(r64) :: currin
      real(r64) :: segout

      ! Control
      ! nsegment,

      ! Parameter
      ! obsin_segment, obsout_segment, segment_type, tosegment,

      ! Basin
      ! basin_area_inv

      ! Flowvars
      ! basin_cfs(RW), basin_cms(RW), basin_gwflow_cfs(RW), basin_sroff_cfs(RW),
      ! basin_ssflow_cfs(RW), basin_stflow_in(RW), basin_stflow_out(RW),
      ! flow_out(RW), seg_inflow, seg_outflow, seg_upstream_inflow(RW)

      ! Gwflow
      ! basin_gwflow

      ! Obs
      ! streamflow_cfs

      ! Routing
      ! basin_segment_storage(RW), c0, c1, c2, flow_headwater, flow_in_great_lakes,
      ! flow_in_nation, flow_in_region, flow_out_NHM, flow_out_region,
      ! flow_replacement, flow_terminus, flow_to_great_lakes, flow_to_lakes,
      ! flow_to_ocean, segment_delta_flow, segment_order, seg_lateral_inflow,
      ! ts, ts_i, use_transfer_segment,

      ! Soilzone
      ! basin_ssflow

      ! Srunoff
      ! basin_sroff,

      ! Time_t
      ! cfs_conv

      ! ------------------------------------------------------------------------

      ! SET yesterdays inflows and outflows into temp (past arrays)
      ! values may be 0.0 as intial, > 0.0 for runtime and dynamic
      ! initial condtions. Then set outlfow and inflow for this time
      ! step to 0.0
      !
      ! upstream_inflow and outflow will vary by hour
      ! lateral_inflow and everything else will vary by day
      !
      ! Compute surface runoff, ssflow, and gwflow going to each segment
      ! This is todays "seg_inflow" before additional water is routed to
      ! a new (if any is routed)
      !
      ! For each HRU if the lateral flow for this HRU goes to the
      ! segment being evaluated (segment i) then sum flows
      !
      ! Do these calculations once for the current day, before the hourly
      ! routing starts.
      !
      !   Out2   =      In2*c0    +        In1*c1    +          Out1*c2
      !   seg_outflow = seg_inflow*Czero + Pastinflow*Cone + Pastoutflow*Ctwo
      !     c0, c1, and c2: initialized in the "init" part of this module
      associate(nsegment => ctl_data%nsegment%value, &
                obsin_segment => param_data%obsin_segment%values, &
                ! FIXME: 2018-06-26 PAN - obsout_segment doesn't always exist in the
                !                         parameter file
                ! obsout_segment => param_data%obsout_segment%values, &
                segment_type => param_data%segment_type%values, &
                tosegment => param_data%tosegment%values, &
                basin_area_inv => model_basin%basin_area_inv, &
                basin_cfs => model_flow%basin_cfs, &
                basin_cms => model_flow%basin_cms, &
                basin_gwflow_cfs => model_flow%basin_gwflow_cfs, &
                basin_sroff_cfs => model_flow%basin_sroff_cfs, &
                basin_ssflow_cfs => model_flow%basin_ssflow_cfs, &
                basin_stflow_in => model_flow%basin_stflow_in, &
                basin_stflow_out => model_flow%basin_stflow_out, &
                flow_out => model_flow%flow_out, &
                seg_inflow => model_flow%seg_inflow, &
                seg_outflow => model_flow%seg_outflow, &
                seg_upstream_inflow => model_flow%seg_upstream_inflow, &
                basin_gwflow => groundwater%basin_gwflow, &
                streamflow_cfs => model_obs%streamflow_cfs, &
                basin_segment_storage => model_route%basin_segment_storage, &
                c0 => model_route%c0, &
                c1 => model_route%c1, &
                c2 => model_route%c2, &
                flow_headwater => model_route%flow_headwater, &
                flow_in_great_lakes => model_route%flow_in_great_lakes, &
                flow_in_nation => model_route%flow_in_nation, &
                flow_in_region => model_route%flow_in_region, &
                flow_out_NHM => model_route%flow_out_NHM, &
                flow_out_region => model_route%flow_out_region, &
                flow_replacement => model_route%flow_replacement, &
                flow_terminus => model_route%flow_terminus, &
                flow_to_great_lakes => model_route%flow_to_great_lakes, &
                flow_to_lakes => model_route%flow_to_lakes, &
                flow_to_ocean => model_route%flow_to_ocean, &
                segment_delta_flow => model_route%segment_delta_flow, &
                segment_order => model_route%segment_order, &
                seg_lateral_inflow => model_route%seg_lateral_inflow, &
                ts => model_route%ts, &
                ts_i => model_route%ts_i, &
                use_transfer_segment => model_route%use_transfer_segment, &
                basin_ssflow => soil%basin_ssflow, &
                basin_sroff => runoff%basin_sroff, &
                cfs_conv => model_time%cfs_conv)

        this%pastin = seg_inflow
        this%pastout = seg_outflow
        seg_inflow = 0.0_dp
        seg_outflow = 0.0_dp
        this%inflow_ts = 0.0_dp
        this%currinsum = 0.0_dp

        ! 24 hourly timesteps per day
        do j = 1, 24
          seg_upstream_inflow = 0.0_dp

          do i = 1, nsegment
            iorder = segment_order(i)

            ! current inflow to the segment is the time weighted average of the outflow
            ! of the upstream segments plus the lateral HRU inflow plus any gains.
            currin = seg_lateral_inflow(iorder)

            if (obsin_segment(iorder) > 0) then
              seg_upstream_inflow(iorder) = streamflow_cfs(obsin_segment(iorder))
            endif

            currin = currin + seg_upstream_inflow(iorder)
            seg_inflow(iorder) = seg_inflow(iorder) + currin
            this%inflow_ts(iorder) = this%inflow_ts(iorder) + currin
            this%currinsum(iorder) = this%currinsum(iorder) + seg_upstream_inflow(iorder)

            ! Check to see if this segment is to be routed on this time step
            tspd = ts_i(iorder)
            imod = mod(j, tspd)

            if (imod == 0) then
              this%inflow_ts(iorder) = (this%inflow_ts(iorder) / ts(iorder))
              ! Compute routed streamflow

              if (ts_i(iorder) > 0) then
                ! Muskingum routing equation
                this%outflow_ts(iorder) = this%inflow_ts(iorder) * c0(iorder) + &
                                          this%pastin(iorder) * c1(iorder) + &
                                          this%outflow_ts(iorder) * c2(iorder)
              else
                ! If travel time (K_coef parameter) is less than or equal to
                ! time step (one hour), then the outflow is equal to the inflow
                ! outflow_ts is the value from last hour
                this%outflow_ts(iorder) = this%inflow_ts(iorder)
              endif

              ! pastin is equal to the inflow_ts on the previous routed timestep
              this%pastin(iorder) = this%inflow_ts(iorder)

              ! Because the upstream inflow from streams is used, reset it to zero
              ! so new average can be computed next routing timestep.
              this%inflow_ts(iorder) = 0.0_dp
            endif

            ! FIXME: 2018-06-26 PAN - obsout_segment doesn't always exist in the
            !                         parameter file
            ! if (obsout_segment(iorder) > 0) then
            !   this%outflow_ts(iorder) = streamflow_cfs(obsout_segment(iorder))
            ! endif

            ! Water-use removed/added in routing module
            ! Check for negative flow
            if (this%outflow_ts(iorder) < 0.0) then
              if (use_transfer_segment == 1) then
                print *, 'ERROR, transfer(s) from stream segment:', iorder, ' causes outflow to be negative'
                print *, '       outflow =', this%outflow_ts(iorder), ' must fix water-use stream segment transfer file'
              else
                print *, 'ERROR, outflow from segment:', iorder, ' is negative:', this%outflow_ts(iorder)
                print *, '       routing parameters may be invalid'
              endif
              STOP
            endif

            ! seg_outflow (the mean daily flow rate for each segment) will be the average of the hourly values.
            seg_outflow(iorder) = seg_outflow(iorder) + this%outflow_ts(iorder)
            ! pastout is equal to the this%inflow_ts on the previous routed timestep
            this%pastout(iorder) = this%outflow_ts(iorder)

            ! Add current timestep's flow rate to sum the upstream flow rates.
            ! This can be thought of as a volume because it is a volumetric rate
            ! (cubic feet per second) over a time step of an hour. Down below when
            ! this value is used, it will be divided by the number of hours in the
            ! segment's simulation time step, giving the mean flow rate over that
            ! period of time.
            toseg = tosegment(iorder)

            if (toseg > 0) then
              seg_upstream_inflow(toseg) = seg_upstream_inflow(toseg) + this%outflow_ts(iorder)
            endif
          enddo  ! segment
        enddo  ! timestep

        basin_segment_storage = 0.0_dp
        flow_out = 0.0_dp
        flow_to_lakes = 0.0_dp
        flow_to_ocean = 0.0_dp
        flow_to_great_lakes = 0.0_dp
        flow_out_region = 0.0_dp
        flow_out_NHM = 0.0_dp
        flow_in_region = 0.0_dp
        flow_terminus = 0.0_dp
        flow_in_nation = 0.0_dp
        flow_headwater = 0.0_dp
        flow_in_great_lakes = 0.0_dp
        flow_replacement = 0.0_dp

        do i=1, nsegment
          seg_outflow(i) = seg_outflow(i) * ONE_24TH
          segout = seg_outflow(i)
          segtype = Segment_type(i)
          seg_inflow(i) = seg_inflow(i) * ONE_24TH
          seg_upstream_inflow(i) = this%currinsum(i) * ONE_24TH

          ! Flow_out is the total flow out of the basin, which allows for multiple
          ! outlets includes closed basins (tosegment=0)
          select case(segtype)
            case(1)
              flow_headwater = flow_headwater + segout
            case(2)
              flow_to_lakes = flow_to_lakes + segout
            case(3)
              flow_replacement = flow_replacement + segout
            case(4)
              flow_in_nation = flow_in_nation + segout
            case(5)
              flow_out_NHM = flow_out_NHM + segout
            case(6)
              flow_in_region = flow_in_region + segout
            case(7)
              flow_out_region = flow_out_region + segout
            case(8)
              flow_to_ocean = flow_to_ocean + segout
            case(9)
              flow_terminus = flow_terminus + segout
            case(10)
              flow_in_great_lakes = flow_in_great_lakes + segout
            case(11)
              flow_to_great_lakes = flow_to_great_lakes + segout
          end select

          ! if (segtype == 1) then
          !   flow_headwater = flow_headwater + segout
          ! elseif (segtype == 2) then
          !   flow_to_lakes = flow_to_lakes + segout
          ! elseif (segtype == 3) then
          !   flow_replacement = flow_replacement + segout
          ! elseif (segtype == 4) then
          !   flow_in_nation = flow_in_nation + segout
          ! elseif (segtype == 5) then
          !   flow_out_NHM = flow_out_NHM + segout
          ! elseif (segtype == 6) then
          !   flow_in_region = flow_in_region + segout
          ! elseif (segtype == 7) then
          !   flow_out_region = flow_out_region + segout
          ! elseif (segtype == 8) then
          !   flow_to_ocean = flow_to_ocean + segout
          ! elseif (segtype == 9) then
          !   flow_terminus = flow_terminus + segout
          ! elseif (segtype == 10) then
          !   flow_in_great_lakes = flow_in_great_lakes + segout
          ! elseif (segtype == 11) then
          !   flow_to_great_lakes = flow_to_great_lakes + segout
          ! endif

          if (tosegment(i) == 0) then
            flow_out = flow_out + segout
          endif

          segment_delta_flow(i) = segment_delta_flow(i) + seg_inflow(i) - segout
          basin_segment_storage = basin_segment_storage + segment_delta_flow(i)
        enddo

        area_fac = cfs_conv / basin_area_inv

        basin_stflow_in = basin_sroff + basin_gwflow + basin_ssflow ! not equal to basin_stflow_out if replacement flows
        basin_cfs = flow_out
        basin_stflow_out = basin_cfs / area_fac
        basin_cms = basin_cfs * CFS2CMS_CONV
        basin_sroff_cfs = basin_sroff * area_fac
        basin_ssflow_cfs = basin_ssflow * area_fac
        basin_gwflow_cfs = basin_gwflow * area_fac
        basin_segment_storage = basin_segment_storage / area_fac
      end associate
    end subroutine

    module subroutine cleanup_Muskingum(this)
      class(Muskingum) :: this
        !! Muskingum class
    end subroutine
end submodule
