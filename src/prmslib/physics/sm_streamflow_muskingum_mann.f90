submodule (PRMS_MUSKINGUM_mann) sm_muskingum_mann
  contains
    module subroutine init_Muskingum_mann(this, ctl_data, model_basin, &
                                          model_time, model_summary)
      use, intrinsic :: iso_fortran_env, only: output_unit
      use prms_constants, only: dp, NEARZERO
      implicit none

      class(Muskingum_mann), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Summary), intent(inout) :: model_summary

      ! Local variables
      integer(i32) :: cseg
      integer(i32) :: ierr

      real(r32) :: d
      real(r32) :: k
      real(r32) :: x
      real(r32) :: x_max
      real(r32) :: velocity

      ! -----------------------------------------------------------------------
      ! Call the parent constructor first
      call this%Streamflow%init(ctl_data, model_basin, model_time, model_summary)

      associate(init_vars_from_file => ctl_data%init_vars_from_file%value, &
                param_hdl => ctl_data%param_file_hdl, &
                print_debug => ctl_data%print_debug%value, &
                save_vars_to_file => ctl_data%save_vars_to_file%value, &

                nsegment => model_basin%nsegment)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! Parameters
        allocate(this%x_coef(nsegment))
        call param_hdl%get_variable('x_coef', this%x_coef)

        allocate(this%mann_n(nsegment))
        call param_hdl%get_variable('mann_n', this%mann_n)

        allocate(this%seg_length(nsegment))
        call param_hdl%get_variable('seg_length', this%seg_length)

        allocate(this%seg_depth(nsegment))
        call param_hdl%get_variable('seg_depth', this%seg_depth)

        allocate(this%seg_slope(nsegment))
        call param_hdl%get_variable('seg_slope', this%seg_slope)

        ! Other variables
        allocate(this%K_coef(nsegment))

        allocate(this%c0(nsegment))
        allocate(this%c1(nsegment))
        allocate(this%c2(nsegment))
        allocate(this%ts(nsegment))
        allocate(this%ts_i(nsegment))

        ! Compute the three constants in the Muskingum routing equation based
        ! on the values of K_coef and a routing period of 1 hour. See the notes
        ! at the top of this file.
        this%c0 = 0.0_dp
        this%c1 = 0.0_dp
        this%c2 = 0.0_dp

        ! Make sure K > 0
        this%ts = 1.0_dp
        ierr = 0

        do cseg=1, nsegment
          velocity = (1.0 / this%mann_n(cseg)) * SQRT(this%seg_slope(cseg)) * this%seg_depth(cseg)**(2.0 / 3.0) ! simplify if say width>>depth
          this%K_coef(cseg) = this%seg_length(cseg) / (velocity * 60.0 * 60.0) !want in hours, length should include sloped length

          ! WARNING: parameter data is read-only
          ! if (segment_type(cseg) == 2 .and. K_coef(cseg) < 24.0) then
          !   write(*, *) 'WARNING, K_coef must be equal to 24.0 for lake segments'
          !   ! For lakes K_coef must be equal to 24
          !   K_coef(cseg) = 24.0
          ! endif

          ! NOTE: This block exists in 5.2.0
          ! Make compliant with old version of K_coef
          if (this%K_coef(cseg) < 0.01) this%K_coef(cseg) = 0.01
          if (this%K_coef(cseg) > 24.0) this%K_coef(cseg) = 24.0

          k = this%K_coef(cseg)
          x = this%x_coef(cseg)

          ! Check the values of k and x to make sure that Muskingum routing is stable
          if (k < 1.0) then
            this%ts_i(cseg) = -1
          elseif (k < 2.0) then
            this%ts(cseg) = 1.0
            this%ts_i(cseg) = 1
          elseif (k < 3.0) then
            this%ts(cseg) = 2.0
            this%ts_i(cseg) = 2
          elseif (k < 4.0) then
            this%ts(cseg) = 3.0
            this%ts_i(cseg) = 3
          elseif (k < 6.0) then
            this%ts(cseg) = 4.0
            this%ts_i(cseg) = 4
          elseif (k < 8.0) then
            this%ts(cseg) = 6.0
            this%ts_i(cseg) = 6
          elseif (k < 12.0) then
            this%ts(cseg) = 8.0
            this%ts_i(cseg) = 8
          elseif (k < 24.0) then
            this%ts(cseg) = 12.0
            this%ts_i(cseg) = 12
          else
            this%ts(cseg) = 24.0
            this%ts_i(cseg) = 24
          endif

          ! x must be <= t / (2K) or the C coefficents will be negative. Check for
          ! this for all segments with this%ts >= minimum this%ts (1 hour).
          if (this%ts(cseg) > 0.1) then
            x_max = this%ts(cseg) / (2.0 * k)

            if (x > x_max) then
              write(output_unit, *) 'ERROR, x_coef value is too large for stable routing for segment:', cseg, ' x_coef:', x
              write(output_unit, *) '       a maximum value of:', x_max, ' is suggested'
              ! Inputerror_flag = 1
              cycle
            endif
          endif

          d = k - (k * x) + (0.5 * this%ts(cseg))

          if (abs(d) < NEARZERO) then
            ! write(*, *) 'WARNING, segment ', cseg, ' computed value d <', NEARZERO, ', set to 0.0001'
            d = 0.0001
          endif

          this%c0(cseg) = (-(k * x) + (0.5 * this%ts(cseg))) / d
          this%c1(cseg) = ((k * x) + (0.5 * this%ts(cseg))) / d
          this%c2(cseg) = (k - (k * x) - (0.5 * this%ts(cseg))) / d

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
          if (this%c2(cseg) < 0.0_dp) then
            ! PRINT '(/,A)', 'WARNING, c2 < 0, set to 0, c1 set to c1 + c2'
            ! PRINT *, '        old c2:', C2(i), '; old c1:', C1(i), '; new c1:', C1(i) + C2(i)
            ! PRINT *, '        K_coef:', K_coef(i), '; x_coef:', x_coef(i)
            this%c1(cseg) = this%c1(cseg) + this%c2(cseg)
            this%c2(cseg) = 0.0_dp
          endif

          ! LONG travel time
          if (this%c0(cseg) < 0.0_dp) then
            ! PRINT '(/,A)', 'WARNING, c0 < 0, set to 0, c0 set to c1 + c0'
            ! PRINT *, '      old c0:', C0(i), 'old c1:', C1(i), 'new c1:', C1(i) + C0(i)
            ! PRINT *, '        K_coef:', K_coef(i), '; x_coef:', x_coef(i)
            this%c1(cseg) = this%c1(cseg) + this%c0(cseg)
            this%c0(cseg) = 0.0_dp
          endif
        enddo

        if (ierr == 1) then
          print '(/,A,/)', '***Recommend that the Muskingum parameters be adjusted in the Parameter File'
        endif
        deallocate(this%K_coef)
        deallocate(this%x_coef)

        allocate(this%currinsum(nsegment))
        allocate(this%inflow_ts(nsegment))
        allocate(this%outflow_ts(nsegment))
        allocate(this%pastin(nsegment))
        allocate(this%pastout(nsegment))

        if (init_vars_from_file==0) then
          this%outflow_ts = 0.0_dp
        else
          ! ~~~~~~~~~~~~~~~~~~~~~~~~
          ! Initialize from restart
          call ctl_data%read_restart_variable('outflow_ts', this%outflow_ts)
        endif

        if (save_vars_to_file == 1) then
          ! Create restart variables
          call ctl_data%add_variable('outflow_ts', this%outflow_ts, 'nsegment', 'cfs')
        end if

        ! DEBUG: PAN
        write(output_unit, 9002) '  Muskingum coefficient (min(c0), max(c0)): ', minval(this%c0), maxval(this%c0)
        write(output_unit, 9002) '  Muskingum coefficient (min(c1), max(c1)): ', minval(this%c1), maxval(this%c1)
        write(output_unit, 9002) '  Muskingum coefficient (min(c2), max(c2)): ', minval(this%c2), maxval(this%c2)
        write(output_unit, 9002) '  Muskingum coefficient (min(ts), max(ts)): ', minval(this%ts), maxval(this%ts)
        9002 format (A, 2F11.7)
      end associate
    end subroutine


    module subroutine run_Muskingum_mann(this, ctl_data, model_basin, &
                                    model_potet, groundwater, soil, runoff, &
                                    model_time, model_solrad, model_obs)
      use, intrinsic :: iso_fortran_env, only: output_unit
      use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH, DNEARZERO
      implicit none

      class(Muskingum_mann), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      class(Potential_ET), intent(in) :: model_potet
      type(Gwflow), intent(in) :: groundwater
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad
      type(Obs), intent(in) :: model_obs

      ! Local Variables
      integer(i32) :: cseg
      integer(i32) :: iorder
      integer(i32) :: j
      integer(i32) :: toseg

      real(r64) :: currin

      ! ------------------------------------------------------------------------

      ! SET yesterdays inflows and outflows into temp (past arrays)
      ! values may be 0.0 as initial, > 0.0 for runtime and dynamic initial
      ! condtions. Then set outflow and inflow for this time step to 0.0
      !
      ! upstream_inflow and outflow will vary by hour.
      ! lateral_inflow and everything else will vary by day.
      !
      ! Compute surface runoff, ssflow, and gwflow going to each segment
      ! This is todays "seg_inflow" before additional water is routed to
      ! a new (if any is routed).
      !
      ! For each HRU if the lateral flow for this HRU goes to the
      ! segment being evaluated (segment cseg) then sum flows.
      !
      ! Do these calculations once for the current day, before the hourly
      ! routing starts.
      !
      !   Out2        = In2*c0    +        In1*c1          + Out1*c2
      !   seg_outflow = seg_inflow*Czero + Pastinflow*Cone + Pastoutflow*Ctwo
      !     c0, c1, and c2: initialized in the "init" part of this module

      ! Call parent class run routine first
      ! call this%run_Streamflow(ctl_data, model_basin, model_potet, &
      call this%Streamflow%run(ctl_data, model_basin, model_potet, &
                               groundwater, soil, runoff, model_time, model_solrad, model_obs)

      associate(nsegment => model_basin%nsegment, &

                streamflow_cfs => model_obs%streamflow_cfs, &

                curr_time => model_time%Nowtime)

        this%pastin = this%seg_inflow
        this%pastout = this%seg_outflow
        this%seg_inflow = 0.0_dp
        this%seg_outflow = 0.0_dp
        this%inflow_ts = 0.0_dp
        this%currinsum = 0.0_dp

        ! 24 hourly timesteps per day
        do j = 1, 24
          this%seg_upstream_inflow = 0.0_dp

          do cseg = 1, nsegment
            iorder = this%segment_order(cseg)

            ! Current inflow to the segment is the time weighted average of the outflow
            ! of the upstream segments plus the lateral HRU inflow plus any gains.
            currin = this%seg_lateral_inflow(iorder)

            if (this%obsin_segment(iorder) > 0) then
              this%seg_upstream_inflow(iorder) = streamflow_cfs(this%obsin_segment(iorder))
            endif

            currin = currin + this%seg_upstream_inflow(iorder)
            this%seg_inflow(iorder) = this%seg_inflow(iorder) + currin
            this%inflow_ts(iorder) = this%inflow_ts(iorder) + currin
            this%currinsum(iorder) = this%currinsum(iorder) + this%seg_upstream_inflow(iorder)

            if (mod(j, this%ts_i(iorder)) == 0) then
              ! This segment is to be routed on this timestep
              this%inflow_ts(iorder) = (this%inflow_ts(iorder) / this%ts(iorder))
              ! Compute routed streamflow

              if (this%ts_i(iorder) > 0) then
                ! NOTE: PAN - outflow_ts can get small enough to result in denormalized
                !             values. Kick out a warning and reset to zero if this
                !             happens.
                ! if (exponent(this%outflow_ts(iorder)) < 0 .and. exponent(this%outflow_ts(iorder)) - exponent(tiny(0.0_dp)) <= 1) then
                if (exponent(this%outflow_ts(iorder)) - exponent(tiny(0.0_dp)) <= 1) then
                  write(output_unit, 9016) MODNAME, '%run() WARNING: outflow_ts(', iorder, ') =', this%outflow_ts(iorder), ' is too small, resetting to zero. ', curr_time(1:3)
                  9016 format(A, A, I0.1, A, es11.3e3, A, I4, 2('/', I2.2))
                  this%outflow_ts(iorder) = 0.0_dp
                  ! write(output_unit, *) '  outflow_ts(iorder): ', iorder, this%outflow_ts(iorder), exponent(this%outflow_ts(iorder)), range(0.0_dp)
                end if

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

            ! TODO: still needes to be hooked up correctly
            ! if (allocated(this%obsout_segment)) then
            !   if (this%obsout_segment(iorder) > 0) then
            !     this%outflow_ts(iorder) = streamflow_cfs(this%obsout_segment(iorder))
            !   endif
            ! end if

            ! Water-use removed/added in routing module
            ! Check for negative flow
            if (this%outflow_ts(iorder) < 0.0) then
              if (this%use_transfer_segment == 1) then
                write(output_unit, *) 'ERROR, transfer(s) from stream segment:', iorder, ' causes outflow to be negative'
                write(output_unit, *) '       outflow =', this%outflow_ts(iorder), ' must fix water-use stream segment transfer file'
              else
                write(output_unit, *) 'ERROR, outflow from segment:', iorder, ' is negative:', this%outflow_ts(iorder)
                write(output_unit, *) '       routing parameters may be invalid'
              endif
              STOP
            endif

            ! seg_outflow (the mean daily flow rate for each segment) will be
            ! the average of the hourly values.
            this%seg_outflow(iorder) = this%seg_outflow(iorder) + this%outflow_ts(iorder)

            ! NOTE: 2019-04-02 PAN - seg_outflow is reset to zero if it gets outside
            !       of the normal range for real64 numbers. Otherwise we can end
            !       up with denormal numbers.
            if (exponent(this%seg_outflow(iorder)) - exponent(tiny(0.0_dp)) <= 4) then
              write(output_unit, 9017) MODNAME, '%run() WARNING: seg_outflow(', iorder, ') =', this%seg_outflow(iorder), ' is too small, resetting to zero. ', curr_time(1:3)
              9017 format(A, A, I0.1, A, es11.3e3, A, I4, 2('/', I2.2))

              this%seg_outflow(iorder) = 0.0_dp
            end if

            ! pastout is equal to the this%inflow_ts on the previous routed timestep
            this%pastout(iorder) = this%outflow_ts(iorder)

            ! Add current timestep's flow rate to sum the upstream flow rates.
            ! This can be thought of as a volume because it is a volumetric rate
            ! (cubic feet per second) over a time step of an hour. Down below when
            ! this value is used, it will be divided by the number of hours in the
            ! segment's simulation time step, giving the mean flow rate over that
            ! period of time.
            toseg = this%tosegment(iorder)

            if (toseg > 0) then
              this%seg_upstream_inflow(toseg) = this%seg_upstream_inflow(toseg) + this%outflow_ts(iorder)
            endif
          enddo  ! segment
        enddo  ! timestep

        this%seg_outflow = this%seg_outflow * ONE_24TH
        this%seg_inflow = this%seg_inflow * ONE_24TH
        this%seg_upstream_inflow = this%currinsum * ONE_24TH

        ! write(*, *) this%seg_outflow(124)
        this%flow_headwater = sum(this%seg_outflow, mask=(this%segment_type == 1))
        this%flow_to_lakes = sum(this%seg_outflow, mask=(this%segment_type == 2))
        this%flow_replacement = sum(this%seg_outflow, mask=(this%segment_type == 3))
        this%flow_in_nation = sum(this%seg_outflow, mask=(this%segment_type == 4))
        this%flow_out_NHM = sum(this%seg_outflow, mask=(this%segment_type == 5))
        this%flow_in_region = sum(this%seg_outflow, mask=(this%segment_type == 6))
        this%flow_out_region = sum(this%seg_outflow, mask=(this%segment_type == 7))
        this%flow_to_ocean = sum(this%seg_outflow, mask=(this%segment_type == 8))
        this%flow_terminus = sum(this%seg_outflow, mask=(this%segment_type == 9))
        this%flow_in_great_lakes = sum(this%seg_outflow, mask=(this%segment_type == 10))
        this%flow_to_great_lakes = sum(this%seg_outflow, mask=(this%segment_type == 11))

        this%flow_out = sum(this%seg_outflow, mask=(this%tosegment == 0))
        this%segment_delta_flow = this%segment_delta_flow + sum(this%seg_inflow - this%seg_outflow)
      end associate
    end subroutine

    module subroutine cleanup_Muskingum_mann(this, ctl_data)
      class(Muskingum_mann), intent(in) :: this
        !! Muskingum_mann class
      type(Control), intent(in) :: ctl_data

      associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
        if (save_vars_to_file == 1) then
          call ctl_data%write_restart_variable('outflow_ts', this%outflow_ts)
        end if
      end associate

      ! Call the parent cleanup
      call this%Streamflow%cleanup(ctl_data)

    end subroutine
end submodule
