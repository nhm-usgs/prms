submodule (PRMS_STREAMFLOW) sm_streamflow
  contains
    module subroutine init_Streamflow(this, ctl_data, model_basin, model_time, model_summary)
      use prms_constants, only: dp, DNEARZERO, FT2_PER_ACRE, NEARZERO
      implicit none

      class(Streamflow), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time
      type(Summary), intent(inout) :: model_summary

      ! Local Variables
      integer(i32) :: cseg
      integer(i32) :: i
      integer(i32) :: iseg
      integer(i32) :: isegerr
      integer(i32) :: j
      integer(i32) :: jj
      integer(i32) :: lval
      integer(i32) :: test
      integer(i32) :: toseg

      integer(i32), allocatable :: x_off(:)

      ! character(len=10) :: buffer
      ! TODO: water_use_flag,

      ! ------------------------------------------------------------------------
      associate(cascade_flag => ctl_data%cascade_flag%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                outVarON_OFF => ctl_data%outVarON_OFF%value, &
                outVar_names => ctl_data%outVar_names, &
                param_hdl => ctl_data%param_file_hdl, &
                print_debug => ctl_data%print_debug%value, &
                save_vars_to_file => ctl_data%save_vars_to_file%value, &
                segment_transferON_OFF => ctl_data%segment_transferON_OFF%value, &
                strmflow_module => ctl_data%strmflow_module%values, &

                nhru => model_basin%nhru, &
                nsegment => model_basin%nsegment, &
                active_hrus => model_basin%active_hrus, &
                hru_area_dble => model_basin%hru_area_dble, &
                hru_route_order => model_basin%hru_route_order, &

                Timestep_seconds => model_time%Timestep_seconds)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! hru_outflow is needed even for non-routed models (e.g. single-HRU)
        allocate(this%hru_outflow(nhru))
        this%hru_outflow = 0.0_dp

        if (nsegment > 0) then
          ! Parameters
          allocate(this%hru_segment(nhru))
          call param_hdl%get_variable('hru_segment', this%hru_segment)

          allocate(this%obsin_segment(nsegment))
          call param_hdl%get_variable('obsin_segment', this%obsin_segment)

          if (param_hdl%var_exists('obsout_segment')) then
            allocate(this%obsout_segment(nsegment))
            call param_hdl%get_variable('obsout_segment', this%obsout_segment)
          end if

          allocate(this%segment_flow_init(nsegment))
          call param_hdl%get_variable('segment_flow_init', this%segment_flow_init)

          allocate(this%segment_type(nsegment))
          call param_hdl%get_variable('segment_type', this%segment_type)

          allocate(this%tosegment(nsegment))
          call param_hdl%get_variable('tosegment', this%tosegment)

          ! Other variables
          allocate(this%seg_lateral_inflow(nsegment))
          this%seg_lateral_inflow = 0.0_dp

          allocate(this%seg_inflow(nsegment))
          allocate(this%seg_outflow(nsegment))

          if (any([0, 2] == init_vars_from_file)) then
            this%seg_outflow = this%segment_flow_init

            do cseg = 1, nsegment
              if (this%tosegment(cseg) > 0) then
                this%seg_inflow(this%tosegment(cseg)) = this%seg_outflow(cseg)
              end if
            end do
          else
            ! ~~~~~~~~~~~~~~~~~~~~~~~~
            ! Initialize from restart
            call ctl_data%read_restart_variable('seg_inflow', this%seg_inflow)
            call ctl_data%read_restart_variable('seg_outflow', this%seg_outflow)
          endif

          deallocate(this%segment_flow_init)

          if (cascade_flag == 0) then
            allocate(this%seg_gwflow(nsegment))
            this%seg_gwflow = 0.0_dp
            allocate(this%seg_sroff(nsegment))
            this%seg_sroff = 0.0_dp
            allocate(this%seg_ssflow(nsegment))
            this%seg_ssflow = 0.0_dp
            allocate(this%seginc_gwflow(nsegment))
            this%seginc_gwflow = 0.0_dp
            allocate(this%seginc_potet(nsegment))
            this%seginc_potet = 0.0_dp
            allocate(this%seginc_sroff(nsegment))
            this%seginc_sroff = 0.0_dp
            allocate(this%seginc_ssflow(nsegment))
            this%seginc_ssflow = 0.0_dp
            allocate(this%seginc_swrad(nsegment))
            this%seginc_swrad = 0.0_dp
          endif

          allocate(this%segment_delta_flow(nsegment))
          allocate(this%segment_hruarea(nsegment))
          allocate(this%segment_order(nsegment))
          allocate(this%segment_up(nsegment))

          ! Streamflow
          ! if (ctl_data%strmflow_module%values(1)%s == 'muskingum' .or. &
          !     ctl_data%strmflow_module%values(1)%s == 'muskingum_lake' .or. &
          !     ctl_data%strmflow_module%values(1)%s == 'strmflow_in_out') then
          !   if (nsegment < 1) then
          !     write(output_unit, *) 'ERROR: streamflow and cascade routing require nsegment > 0, specified as:', nsegment
          !     stop
          !   endif

            allocate(this%seg_upstream_inflow(nsegment))
            this%seg_upstream_inflow = 0.0_dp
          ! endif

          ! Now initialize everything

          ! TODO: Uncomment once water_use_flag is added
          ! this%use_transfer_segment = 0
          ! if (water_use_flag == 1 .and. segment_transferON_OFF == 1) then
          !   this%use_transfer_segment = 1
          ! endif

          if (init_vars_from_file == 0) then
            this%segment_delta_flow = 0.0_dp
          else
            ! ~~~~~~~~~~~~~~~~~~~~~~~~
            ! Initialize from restart
            call ctl_data%read_restart_variable('segment_delta_flow', this%segment_delta_flow)
          endif

          this%flow_out = 0.0_dp
          this%flow_headwater = 0.0_dp
          this%flow_in_great_lakes = 0.0_dp
          this%flow_in_nation = 0.0_dp
          this%flow_in_region = 0.0_dp
          this%flow_out_NHM = 0.0_dp
          this%flow_out_region = 0.0_dp
          this%flow_replacement = 0.0_dp
          this%flow_terminus = 0.0_dp
          this%flow_to_great_lakes = 0.0_dp
          this%flow_to_lakes = 0.0_dp
          this%flow_to_ocean = 0.0_dp

          ! NOTE: This belongs in muskingum_lake
          ! this%cfs2acft = Timestep_seconds / FT2_PER_ACRE

          ! WARNING: parameter data is read-only
          ! do i=1, nsegment
          !   segment_type(i) = mod(segment_type(i), 100)
          ! enddo

          ! If cascades are active then ignore hru_segment
          if (cascade_flag == 0) then
            this%segment_hruarea = 0.0_dp

            do j=1, active_hrus
              i = hru_route_order(j)
              iseg = this%hru_segment(i)

              if (iseg > 0) then
                this%segment_hruarea(iseg) = this%segment_hruarea(iseg) + hru_area_dble(i)
              endif
            enddo

            this%segment_area = sum(this%segment_hruarea)
            this%noarea_flag = any(this%segment_hruarea < DNEARZERO)
          endif

          ! DEBUG: PAN
          ! print *, 'segment_hruarea'
          ! print *, this%segment_hruarea

          isegerr = 0
          this%segment_up = 0

          ! Begin the loops for ordering segments
          do j=1, nsegment
            toseg = this%tosegment(j)

            if (toseg == j) then
              print *, 'ERROR, tosegment value (', toseg, ') equals itself for segment:', j
              isegerr = 1
            elseif (toseg > 0) then
              ! Load segment_up with last stream segment that flows into a segment
              this%segment_up(toseg) = j
            endif
          enddo

          if (isegerr == 1) then
            ! TODO: How to handle error condition?
            ! Inputerror_flag = 1
            return
          endif

          ! Begin the loops for ordering segments
          allocate(x_off(nsegment))
          x_off = 0
          this%segment_order = 0
          lval = 0

          do while (lval < nsegment)
            do i=1, nsegment
              ! If segment "i" has not been crossed out consider it, else continue
              if (x_off(i) /= 1) then
                ! Test to see if segment "i" is the to-segment from other segments
                test = 1

                do j=1, nsegment
                  if (this%tosegment(j) == i) then
                    ! If segment "i" is a to-segment, test to see if the originating
                    ! segment has been crossed off the list.  If all have been, then
                    ! put the segment in as an ordered segment.
                    if (x_off(j) == 0) then
                      test = 0
                      exit
                    end if
                  end if
                end do

                if (test == 1) then
                  lval = lval + 1
                  this%segment_order(lval) = i
                  x_off(i) = 1
                end if
              end if
            end do
          end do

          deallocate(x_off)

          if (save_vars_to_file == 1) then
            ! Create restart variables
            ! call ctl_data%add_variable('flow_out', this%flow_out, 'one', 'cfs')
            call ctl_data%add_variable('seg_inflow', this%seg_inflow, 'nsegment', 'cfs')
            call ctl_data%add_variable('seg_outflow', this%seg_outflow, 'nsegment', 'cfs')
            call ctl_data%add_variable('segment_delta_flow', this%segment_delta_flow, 'nsegment', 'cfs')
          end if
        end if

        ! Connect summary variables that need to be output
        if (outVarON_OFF == 1) then
          do jj = 1, outVar_names%size()
            select case(outVar_names%values(jj)%s)
              case('hru_outflow')
                call model_summary%set_summary_var(jj, this%hru_outflow)
              case default
                ! pass
            end select

            ! Clunky way to avoid segment-related output variables for non-routed models
            if (nsegment > 0) then
              select case(outVar_names%values(jj)%s)
                case('seg_gwflow')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seg_gwflow)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('seg_inflow')
                  call model_summary%set_summary_var(jj, this%seg_inflow)
                case('seg_lateral_inflow')
                  call model_summary%set_summary_var(jj, this%seg_lateral_inflow)
                case('seg_outflow')
                  call model_summary%set_summary_var(jj, this%seg_outflow)
                case('seg_sroff')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seg_sroff)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('seg_ssflow')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seg_ssflow)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('seg_upstream_inflow')
                  call model_summary%set_summary_var(jj, this%seg_upstream_inflow)
                case('seginc_gwflow')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seginc_gwflow)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('seginc_potet')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seginc_potet)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('seginc_sroff')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seginc_sroff)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('seginc_ssflow')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seginc_ssflow)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('seginc_swrad')
                  if (cascade_flag == 0) then
                    call model_summary%set_summary_var(jj, this%seginc_swrad)
                  else
                    write(output_unit, *) MODNAME, "%constructor() WARNING:", outVar_names%values(jj)%s, " is not available when cascade module is active"
                  end if
                case('segment_delta_flow')
                  call model_summary%set_summary_var(jj, this%segment_delta_flow)
                case default
                  ! pass
              end select
            end if
          enddo
        endif
      end associate
    end subroutine

    module subroutine run_Streamflow(this, ctl_data, model_basin, &
                                    model_potet, groundwater, soil, runoff, &
                                    model_time, model_solrad, model_obs)
      use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH, DNEARZERO, NEARZERO
      implicit none

      class(Streamflow), intent(inout) :: this
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
      logical :: found
      integer(i32) :: cseg
        !! Current segment for loops
      integer(i32) :: cseg_us
        !! Used for searching upstream segments
      integer(i32) :: j
      integer(i32) :: jj
      real(r64) :: tocfs

      !***********************************************************************
      associate(cascade_flag => ctl_data%cascade_flag%value, &

                nsegment => model_basin%nsegment, &
                active_hrus => model_basin%active_hrus, &
                hru_area => model_basin%hru_area, &
                hru_route_order => model_basin%hru_route_order, &

                potet => model_potet%potet, &

                swrad => model_solrad%swrad, &

                gwres_flow => groundwater%gwres_flow, &

                ssres_flow => soil%ssres_flow, &

                sroff => runoff%sroff, &
                strm_seg_in => runoff%strm_seg_in, &

                cfs_conv => model_time%cfs_conv, &
                Timestep_seconds => model_time%Timestep_seconds)
                ! segment_gain => wateruse%segment_gain, &
                ! segment_transfer => wateruse%segment_transfer)

        ! NOTE: This belongs in muskingum_lake
        ! this%cfs2acft = Timestep_seconds / FT2_PER_ACRE

        ! hru_outflow is always computed whether streamflow is routed or not
        do jj = 1, active_hrus
          j = hru_route_order(jj)
          tocfs = dble(hru_area(j)) * cfs_conv
          this%hru_outflow(j) = dble(sroff(j) + ssres_flow(j) + gwres_flow(j)) * tocfs
        end do

        if (nsegment > 0) then
          ! seg variables are not computed if cascades are active as hru_segment is ignored
          if (cascade_flag == 0) then
            ! Add hru_ppt, hru_actet
            this%seginc_gwflow = 0.0_dp
            this%seginc_ssflow = 0.0_dp
            this%seginc_sroff = 0.0_dp
            this%seginc_swrad = 0.0_dp
            this%seginc_potet = 0.0_dp
            this%seg_gwflow = 0.0_dp
            this%seg_sroff = 0.0_dp
            this%seg_ssflow = 0.0_dp
            this%seg_lateral_inflow = 0.0_dp
          else
            this%seg_lateral_inflow = strm_seg_in
          endif

          do jj = 1, active_hrus
            j = hru_route_order(jj)
            tocfs = dble(hru_area(j)) * cfs_conv
            ! this%hru_outflow(j) = dble(sroff(j) + ssres_flow(j) + gwres_flow(j)) * tocfs

            if (cascade_flag == 0) then
              cseg = this%hru_segment(j)

              if (cseg > 0) then
                this%seg_gwflow(cseg) = this%seg_gwflow(cseg) + gwres_flow(j)
                this%seg_sroff(cseg) = this%seg_sroff(cseg) + sroff(j)
                this%seg_ssflow(cseg) = this%seg_ssflow(cseg) + ssres_flow(j)
                this%seg_lateral_inflow(cseg) = this%seg_lateral_inflow(cseg) + this%hru_outflow(j)
                this%seginc_sroff(cseg) = this%seginc_sroff(cseg) + dble(sroff(j)) * tocfs
                this%seginc_ssflow(cseg) = this%seginc_ssflow(cseg) + dble(ssres_flow(j)) * tocfs
                this%seginc_gwflow(cseg) = this%seginc_gwflow(cseg) + dble(gwres_flow(j)) * tocfs
                this%seginc_swrad(cseg) = this%seginc_swrad(cseg) + dble(swrad(j) * hru_area(j))
                this%seginc_potet(cseg) = this%seginc_potet(cseg) + dble(potet(j) * hru_area(j))
              endif
            endif
          enddo

          ! TODO: Uncomment once water_use module is converted
          ! if (this%use_transfer_segment == 1) then
          !   do i=1, nsegment
          !     this%seg_lateral_inflow(i) = this%seg_lateral_inflow(i) + dble(segment_gain(i) - segment_transfer(i))
          !   enddo
          ! endif

          if (cascade_flag == 1) return

          ! TODO: 2022-04-14 PAN - incorporate changes by Markstrom (route: 730-795)
          ! Divide solar radiation and PET by sum of HRU area to get average
          if (.not. this%noarea_flag) then
            do cseg=1, nsegment
              ! print *, cseg, ': ', this%seginc_swrad(cseg), this%seginc_potet(cseg), this%segment_hruarea(cseg)
              if (this%segment_hruarea(cseg) > DNEARZERO) then
                this%seginc_swrad(cseg) = this%seginc_swrad(cseg) / this%segment_hruarea(cseg)
                this%seginc_potet(cseg) = this%seginc_potet(cseg) / this%segment_hruarea(cseg)
              end if
            enddo
          else
            ! If there are no HRUs associated with a segment, then figure out some
            ! other way to get the solar radiation, the following is not great.
            do cseg = 1, nsegment
              ! This reworked by markstrom
              if (this%segment_hruarea(cseg) > NEARZERO) then
                this%seginc_swrad(cseg) = this%seginc_swrad(cseg) / this%segment_hruarea(cseg)
                this%seginc_potet(cseg) = this%seginc_potet(cseg) / this%segment_hruarea(cseg)
              else
                ! Segment does not have any HRUs, check upstream segments.
                cseg_us = cseg
                found = .false.
                do
                  if (this%segment_hruarea(cseg_us) <= NEARZERO) then

                    ! Hit the headwater segment without finding any HRUs (i.e. sources of streamflow)
                    if (this%segment_up(cseg_us) == 0) then
                      found = .false.
                      exit
                    end if

                    ! There is an upstream segment, check that segment for HRUs
                    cseg_us = this%segment_up(cseg_us)
                  else
                    ! This segment has HRUs so there will be swrad and potet
                    this%seginc_swrad(cseg) = this%seginc_swrad(cseg_us) / this%segment_hruarea(cseg_us)
                    this%seginc_potet(cseg) = this%seginc_potet(cseg_us) / this%segment_hruarea(cseg_us)
                    found = .true.
                    exit
                  end if
                end do

                if (.not. found) then
                  ! Segment does not have any upstream segments with HRUs, check downstream segments.
                  cseg_us = cseg
                  found = .false.
                  do
                    if (this%segment_hruarea(cseg_us) <= NEARZERO) then
                      ! Hit the terminal segment without finding any HRUs (i.e. sources of streamflow)
                      if (this%tosegment(cseg_us) .eq. 0) then
                        found = .false.
                        exit
                      end if

                      ! There is a downstream segment, check that segment for HRUs
                      cseg_us = this%tosegment(cseg_us)
                    else
                      ! This segment has HRUs so there will be swrad and potet
                      this%seginc_swrad(cseg) = this%seginc_swrad(cseg_us) / this%segment_hruarea(cseg_us)
                      this%seginc_potet(cseg) = this%seginc_potet(cseg_us) / this%segment_hruarea(cseg_us)
                      found = .true.
                      exit
                    end if
                  end do

                  if (.not. found) then
                    ! write(*,*) "route_run: no upstream or downstream HRU found for segment ", cseg
                    ! write(*,*) "    no values for seginc_swrad and seginc_potet"
                    this%seginc_swrad(cseg) = -99.9
                    this%seginc_potet(cseg) = -99.9
                  end if
                end if
              end if
            end do
          end if
          !   do i=1, nsegment
          !     if (this%segment_hruarea(i) > DNEARZERO) then
          !       this%seginc_swrad(i) = this%seginc_swrad(i) / this%segment_hruarea(i)
          !       this%seginc_potet(i) = this%seginc_potet(i) / this%segment_hruarea(i)
          !     elseif (this%tosegment(i) > 0) then
          !       this%seginc_swrad(i) = this%seginc_swrad(this%tosegment(i))
          !       this%seginc_potet(i) = this%seginc_potet(this%tosegment(i))
          !     elseif (i > 1) then
          !       ! Set to next segment id
          !       this%seginc_swrad(i) = this%seginc_swrad(i-1)
          !       this%seginc_potet(i) = this%seginc_potet(i-1)
          !     else
          !       ! Assume at least 2 segments
          !       this%seginc_swrad(i) = this%seginc_swrad(i+1)
          !       this%seginc_potet(i) = this%seginc_potet(i+1)
          !     endif
          !   enddo
          ! endif
        end if
      end associate
    end subroutine

    module subroutine cleanup_Streamflow(this, ctl_data)
      class(Streamflow), intent(in) :: this
        !! Streamflow class
      type(Control), intent(in) :: ctl_data

      associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
        if (save_vars_to_file == 1) then
          ! Write out this module's restart variables
          ! call ctl_data%write_restart_variable('flow_out', this%flow_out)
          if (associated(this%seg_inflow)) then
            call ctl_data%write_restart_variable('seg_inflow', this%seg_inflow)
          end if

          if (associated(this%seg_outflow)) then
            call ctl_data%write_restart_variable('seg_outflow', this%seg_outflow)
          end if

          if (associated(this%segment_delta_flow)) then
            call ctl_data%write_restart_variable('segment_delta_flow', this%segment_delta_flow)
          end if
        end if
      end associate
    end subroutine
end submodule
