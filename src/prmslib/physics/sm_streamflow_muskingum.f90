submodule (PRMS_MUSKINGUM) sm_muskingum
  contains
    module function constructor_Muskingum(ctl_data, param_data, model_basin, &
                                          model_time) result(this)
      use prms_constants, only: dp, NEARZERO
      implicit none

      type(Muskingum) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time

      ! Local variables
      integer(i32) :: i
      integer(i32) :: ierr

      real(r32) :: d
      real(r32) :: k
      real(r32) :: x
      real(r32) :: x_max

      ! Control
      ! nsegment, init_vars_from_file,

      ! Parameter
      ! segment_flow_init

      ! Basin
      ! basin_area_inv

      ! Time_t
      ! cfs_conv

      ! -----------------------------------------------------------------------
      ! Call the parent constructor first
      this%Streamflow = Streamflow(ctl_data, param_data, model_basin, model_time)

      associate(nsegment => ctl_data%nsegment%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                segment_flow_init => param_data%segment_flow_init%values, &
                basin_area_inv => model_basin%basin_area_inv, &
                ! seg_outflow => model_flow%seg_outflow, &
                cfs_conv => model_time%cfs_conv, &
                K_coef => param_data%K_coef%values, &
                x_coef => param_data%x_coef%values)

        allocate(this%c0(nsegment))
        allocate(this%c1(nsegment))
        allocate(this%c2(nsegment))
        allocate(this%ts(nsegment))
        allocate(this%ts_i(nsegment))

        ! Compute the three constants in the Muskingum routing equation based
        ! on the values of K_coef and a routing period of 1 hour. See the notes
        ! at the top of this file.
        this%c0 = 0.0
        this%c1 = 0.0
        this%c2 = 0.0

        ! Make sure K > 0
        this%ts = 1.0
        ierr = 0

        do i=1, nsegment
          ! WARNING: parameter data is read-only
          ! if (segment_type(i) == 2 .and. K_coef(i) < 24.0) then
          !   ! For lakes K_coef must be equal to 24
          !   K_coef(i) = 24.0
          ! endif

          k = K_coef(i)
          x = x_coef(i)

          ! Check the values of k and x to make sure that Muskingum routing is stable
          if (k < 1.0) then
            this%ts_i(i) = -1
          elseif (k < 2.0) then
            this%ts(i) = 1.0
            this%ts_i(i) = 1
          elseif (k < 3.0) then
            this%ts(i) = 2.0
            this%ts_i(i) = 2
          elseif (k < 4.0) then
            this%ts(i) = 3.0
            this%ts_i(i) = 3
          elseif (k < 6.0) then
            this%ts(i) = 4.0
            this%ts_i(i) = 4
          elseif (k < 8.0) then
            this%ts(i) = 6.0
            this%ts_i(i) = 6
          elseif (k < 12.0) then
            this%ts(i) = 8.0
            this%ts_i(i) = 8
          elseif (k < 24.0) then
            this%ts(i) = 12.0
            this%ts_i(i) = 12
          else
            this%ts(i) = 24.0
            this%ts_i(i) = 24
          endif

          ! x must be <= t/(2K) the C coefficents will be negative. Check for
          ! this for all segments with this%ts >= minimum this%ts (1 hour).
          if (this%ts(i) > 0.1) then
            x_max = this%ts(i) / (2.0 * k)

            if (x > x_max) then
              print *, 'ERROR, x_coef value is too large for stable routing for segment:', i, ' x_coef:', x
              print *, '       a maximum value of:', x_max, ' is suggested'
              ! Inputerror_flag = 1
              cycle
            endif
          endif

          d = k - (k * x) + (0.5 * this%ts(i))

          if (abs(d) < NEARZERO) then
            d = 0.0001
          endif

          this%c0(i) = (-(k * x) + (0.5 * this%ts(i))) / d
          this%c1(i) = ((k * x) + (0.5 * this%ts(i))) / d
          this%c2(i) = (k - (k * x) - (0.5 * this%ts(i))) / d

          ! the following code was in the original musroute, but, not in Linsley and others
          ! NOTE: rsr, 3/1/2016 - having < 0 coefficient can cause negative
          !                       flows as found by Jacob in GCPO headwater.
          !  if c2 is <= 0.0 then short travel time though reach (less daily
          !  flows), thus outflow is mainly = inflow w/ small influence of previous
          !  inflow. Therefore, keep c0 as is, and lower c1 by c2, set c2=0

          !  if c0 is <= 0.0 then long travel time through reach (greater than daily
          !  flows), thus mainly dependent on yesterdays flows.  Therefore, keep
          !  c2 as is, reduce c1 by c0 and set c0=0

          ! SHORT travel time
          if (this%c2(i) < 0.0) then
            this%c1(i) = this%c1(i) + this%c2(i)
            this%c2(i) = 0.0
          endif

          ! LONG travel time
          if (this%c0(i) < 0.0) then
            this%c1(i) = this%c1(i) + this%c0(i)
            this%c0(i) = 0.0
          endif

        enddo

        if (ierr == 1) print '(/,A,/)', '***Recommend that the Muskingum parameters be adjusted in the Parameter File'
        ! deallocate(K_coef)
        ! deallocate(x_coef)

        allocate(this%currinsum(nsegment))
        allocate(this%inflow_ts(nsegment))
        allocate(this%outflow_ts(nsegment))
        allocate(this%pastin(nsegment))
        allocate(this%pastout(nsegment))

        if (init_vars_from_file == 0 .or. init_vars_from_file == 2) then
          do i = 1, nsegment
            this%seg_outflow(i) = segment_flow_init(i)
          enddo

          ! deallocate (segment_flow_init)
        endif

        if (init_vars_from_file==0) then
          this%outflow_ts = 0.0_dp
        endif

        this%basin_segment_storage = 0.0_dp

        do i = 1, nsegment
          this%basin_segment_storage = this%basin_segment_storage + this%seg_outflow(i)
        enddo

        this%basin_segment_storage = this%basin_segment_storage * basin_area_inv / cfs_conv
      end associate
    end function

    module subroutine run_Muskingum(this, ctl_data, param_data, model_basin, &
                                    model_climate, model_potet, groundwater, soil, runoff, &
                                    model_time, model_solrad, model_obs)
      use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH
      implicit none

      class(Muskingum), intent(inout) :: this
        !! Muskingum class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      class(Potential_ET), intent(inout) :: model_potet
      type(Gwflow), intent(in) :: groundwater
        !! Groundwater variables
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad
      type(Obs), intent(in) :: model_obs

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

      ! Gwflow
      ! basin_gwflow

      ! Obs
      ! streamflow_cfs

      ! Soilzone
      ! basin_ssflow

      ! Srunoff
      ! basin_sroff

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

      ! Call parent class run routine first
      call this%run_Streamflow(ctl_data, param_data, model_basin, model_climate, &
                               model_potet, groundwater, soil, runoff, model_time, model_solrad)

      associate(nsegment => ctl_data%nsegment%value, &

                obsin_segment => param_data%obsin_segment%values, &
                ! FIXME: 2018-06-26 PAN - obsout_segment doesn't always exist in the
                !                         parameter file.
                ! obsout_segment => param_data%obsout_segment%values, &
                segment_type => param_data%segment_type%values, &
                tosegment => param_data%tosegment%values, &

                basin_area_inv => model_basin%basin_area_inv, &

                basin_gwflow => groundwater%basin_gwflow, &

                streamflow_cfs => model_obs%streamflow_cfs, &

                basin_ssflow => soil%basin_ssflow, &

                basin_sroff => runoff%basin_sroff, &

                cfs_conv => model_time%cfs_conv)

        this%pastin = this%seg_inflow
        this%pastout = this%seg_outflow
        this%seg_inflow = 0.0_dp
        this%seg_outflow = 0.0_dp
        this%inflow_ts = 0.0_dp
        this%currinsum = 0.0_dp

        ! 24 hourly timesteps per day
        do j = 1, 24
          this%seg_upstream_inflow = 0.0_dp

          do i = 1, nsegment
            iorder = this%segment_order(i)

            ! current inflow to the segment is the time weighted average of the outflow
            ! of the upstream segments plus the lateral HRU inflow plus any gains.
            currin = this%seg_lateral_inflow(iorder)

            if (obsin_segment(iorder) > 0) then
              this%seg_upstream_inflow(iorder) = streamflow_cfs(obsin_segment(iorder))
            endif

            currin = currin + this%seg_upstream_inflow(iorder)
            this%seg_inflow(iorder) = this%seg_inflow(iorder) + currin
            this%inflow_ts(iorder) = this%inflow_ts(iorder) + currin
            this%currinsum(iorder) = this%currinsum(iorder) + this%seg_upstream_inflow(iorder)

            ! Check to see if this segment is to be routed on this time step
            tspd = this%ts_i(iorder)
            imod = mod(j, tspd)

            if (imod == 0) then
              this%inflow_ts(iorder) = (this%inflow_ts(iorder) / this%ts(iorder))
              ! Compute routed streamflow

              if (this%ts_i(iorder) > 0) then
                ! Muskingum routing equation
                this%outflow_ts(iorder) = this%inflow_ts(iorder) * this%c0(iorder) + &
                                          this%pastin(iorder) * this%c1(iorder) + &
                                          this%outflow_ts(iorder) * this%c2(iorder)
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
              if (this%use_transfer_segment == 1) then
                print *, 'ERROR, transfer(s) from stream segment:', iorder, ' causes outflow to be negative'
                print *, '       outflow =', this%outflow_ts(iorder), ' must fix water-use stream segment transfer file'
              else
                print *, 'ERROR, outflow from segment:', iorder, ' is negative:', this%outflow_ts(iorder)
                print *, '       routing parameters may be invalid'
              endif
              STOP
            endif

            ! seg_outflow (the mean daily flow rate for each segment) will be the average of the hourly values.
            this%seg_outflow(iorder) = this%seg_outflow(iorder) + this%outflow_ts(iorder)
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
              this%seg_upstream_inflow(toseg) = this%seg_upstream_inflow(toseg) + this%outflow_ts(iorder)
            endif
          enddo  ! segment
        enddo  ! timestep

        this%flow_out = 0.0_dp
        this%basin_segment_storage = 0.0_dp
        this%flow_to_lakes = 0.0_dp
        this%flow_to_ocean = 0.0_dp
        this%flow_to_great_lakes = 0.0_dp
        this%flow_out_region = 0.0_dp
        this%flow_out_NHM = 0.0_dp
        this%flow_in_region = 0.0_dp
        this%flow_terminus = 0.0_dp
        this%flow_in_nation = 0.0_dp
        this%flow_headwater = 0.0_dp
        this%flow_in_great_lakes = 0.0_dp
        this%flow_replacement = 0.0_dp

        do i=1, nsegment
          this%seg_outflow(i) = this%seg_outflow(i) * ONE_24TH
          segout = this%seg_outflow(i)
          segtype = Segment_type(i)
          this%seg_inflow(i) = this%seg_inflow(i) * ONE_24TH
          this%seg_upstream_inflow(i) = this%currinsum(i) * ONE_24TH

          ! Flow_out is the total flow out of the basin, which allows for multiple
          ! outlets includes closed basins (tosegment=0)
          select case(segtype)
            case(1)
              this%flow_headwater = this%flow_headwater + segout
            case(2)
              this%flow_to_lakes = this%flow_to_lakes + segout
            case(3)
              this%flow_replacement = this%flow_replacement + segout
            case(4)
              this%flow_in_nation = this%flow_in_nation + segout
            case(5)
              this%flow_out_NHM = this%flow_out_NHM + segout
            case(6)
              this%flow_in_region = this%flow_in_region + segout
            case(7)
              this%flow_out_region = this%flow_out_region + segout
            case(8)
              this%flow_to_ocean = this%flow_to_ocean + segout
            case(9)
              this%flow_terminus = this%flow_terminus + segout
            case(10)
              this%flow_in_great_lakes = this%flow_in_great_lakes + segout
            case(11)
              this%flow_to_great_lakes = this%flow_to_great_lakes + segout
          end select

          if (tosegment(i) == 0) then
            this%flow_out = this%flow_out + segout
          endif

          this%segment_delta_flow(i) = this%segment_delta_flow(i) + this%seg_inflow(i) - segout
          this%basin_segment_storage = this%basin_segment_storage + this%segment_delta_flow(i)
        enddo

        area_fac = cfs_conv / basin_area_inv

        ! NOTE: Is this block repeated in the other child classes?
        this%basin_stflow_in = basin_sroff + basin_gwflow + basin_ssflow ! not equal to basin_stflow_out if replacement flows
        this%basin_cfs = this%flow_out
        this%basin_stflow_out = this%basin_cfs / area_fac
        this%basin_cms = this%basin_cfs * CFS2CMS_CONV
        this%basin_sroff_cfs = basin_sroff * area_fac
        this%basin_ssflow_cfs = basin_ssflow * area_fac
        this%basin_gwflow_cfs = basin_gwflow * area_fac
        this%basin_segment_storage = this%basin_segment_storage / area_fac
      end associate
    end subroutine

    module subroutine cleanup_Muskingum(this)
      class(Muskingum) :: this
        !! Muskingum class
    end subroutine
end submodule
