submodule (PRMS_STREAMFLOW) sm_streamflow
  contains
    module function constructor_Streamflow(ctl_data, param_data, model_basin, model_time) result(this)
      use prms_constants, only: dp, DNEARZERO, FT2_PER_ACRE, NEARZERO
      implicit none

      type(Streamflow) :: this
        !! Streamflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameter data
      type(Basin), intent(in) :: model_basin
      type(Time_t), intent(in) :: model_time

      ! Local Variables
      integer(i32) :: i

      integer(i32) :: iseg
      integer(i32) :: isegerr
      integer(i32) :: j
      integer(i32) :: lval
      integer(i32) :: test
      integer(i32) :: toseg

      integer(i32), allocatable :: x_off(:)

      ! character(len=10) :: buffer

      ! Control
      ! nhru, nsegment
      ! cascade_flag, init_vars_from_file, segment_transferON_OFF, strmflow_module,
      !
      ! TODO: water_use_flag,

      ! Parameter
      ! hru_segment, K_coef, obsin_segment, segment_type, tosegment, x_coef

      ! Basin
      ! active_hrus, hru_area_dble, hru_route_order,

      ! Time_t
      ! Timestep_seconds

      ! ------------------------------------------------------------------------
      associate(nhru => ctl_data%nhru%value, &
                nsegment => ctl_data%nsegment%value, &
                cascade_flag => ctl_data%cascade_flag%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                print_debug => ctl_data%print_debug%value, &
                segment_transferON_OFF => ctl_data%segment_transferON_OFF%value, &
                strmflow_module => ctl_data%strmflow_module%values, &

                hru_segment => param_data%hru_segment%values, &
                ! K_coef => param_data%K_coef%values, &
                obsin_segment => param_data%obsin_segment%values, &
                segment_type => param_data%segment_type%values, &
                tosegment => param_data%tosegment%values, &
                ! x_coef => param_data%x_coef%values, &

                active_hrus => model_basin%active_hrus, &
                hru_area_dble => model_basin%hru_area_dble, &
                hru_route_order => model_basin%hru_route_order, &

                Timestep_seconds => model_time%Timestep_seconds)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        allocate(this%hru_outflow(nhru))

        allocate(this%seg_lateral_inflow(nsegment))
        this%seg_lateral_inflow = 0.0_dp

        allocate(this%seg_inflow(nsegment))
        allocate(this%seg_outflow(nsegment))

        if (cascade_flag == 0) then
          allocate(this%seg_gwflow(nsegment))
          allocate(this%seg_sroff(nsegment))
          allocate(this%seg_ssflow(nsegment))
          allocate(this%seginc_gwflow(nsegment))
          allocate(this%seginc_potet(nsegment))
          allocate(this%seginc_sroff(nsegment))
          allocate(this%seginc_ssflow(nsegment))
          allocate(this%seginc_swrad(nsegment))
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
          this%basin_segment_storage = 0.0_dp
          this%segment_delta_flow = 0.0_dp
        endif

        if (cascade_flag == 0) then
          this%seg_gwflow = 0.0_dp
          this%seg_sroff = 0.0_dp
          this%seg_ssflow = 0.0_dp
          this%seginc_gwflow = 0.0_dp
          this%seginc_potet = 0.0_dp
          this%seginc_sroff = 0.0_dp
          this%seginc_ssflow = 0.0_dp
          this%seginc_swrad = 0.0_dp
        endif

        this%seg_inflow = 0.0_dp
        this%seg_outflow = 0.0_dp

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
        this%hru_outflow = 0.0_dp

        ! NOTE: This belongs in muskingum_lake
        ! this%cfs2acft = Timestep_seconds / FT2_PER_ACRE

        ! WARNING: parameter data is read-only
        ! do i=1, nsegment
        !   segment_type(i) = mod(segment_type(i), 100)
        ! enddo

        ! If cascades are active then ignore hru_segment
        this%noarea_flag = 0

        if (cascade_flag == 0) then
          ! if ( getparam(MODNAME, 'hru_segment', Nhru, 'integer', hru_segment)/=0 ) CALL read_error(2, 'hru_segment')
          this%segment_hruarea = 0.0_dp

          do j=1, active_hrus
            i = hru_route_order(j)
            iseg = hru_segment(i)

            if (iseg > 0) then
              this%segment_hruarea(iseg) = this%segment_hruarea(iseg) + hru_area_dble(i)
            endif
          enddo

          this%segment_area = 0.0_dp

          do j=1, nsegment
            this%segment_area = this%segment_area + this%segment_hruarea(j)

            if (this%segment_hruarea(j) < DNEARZERO) then
              this%noarea_flag = 1
            endif
          enddo
        endif

        isegerr = 0
        this%segment_up = 0

        ! Begin the loops for ordering segments
        do j=1, nsegment
          iseg = obsin_segment(j)
          toseg = tosegment(j)

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
            if (x_off(i) == 1) cycle

            ! Test to see if segment "i" is the to segment from other segments
            test = 1
            do j=1, nsegment
              if (tosegment(j) == i) then
                ! If segment "i" is a to segment, test to see if the originating
                ! segment has been crossed off the list.  If all have been, then
                ! put the segment in as an ordered segment.
                if (x_off(j) == 0) then
                  test = 0
                  exit
                endif
              endif
            enddo

            if (test == 1) then
              lval = lval + 1
              this%segment_order(lval) = i
              x_off(i) = 1
            endif
          enddo
        enddo

        deallocate(x_off)

      end associate
    end function


    module subroutine run_Streamflow(this, ctl_data, param_data, model_basin, &
                                  model_potet, groundwater, soil, runoff, &
                                  model_time, model_solrad)
      use prms_constants, only: dp, FT2_PER_ACRE, NEARZERO
      implicit none

      class(Streamflow) :: this
        !! Streamflow class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Potential_ET), intent(in) :: model_potet
      type(Gwflow), intent(in) :: groundwater
        !! Groundwater variables
      type(Soilzone), intent(in) :: soil
      type(Srunoff), intent(in) :: runoff
      type(Time_t), intent(in) :: model_time
      class(SolarRadiation), intent(in) :: model_solrad

      ! Local Variables
      integer(i32) :: i
      integer(i32) :: j
      integer(i32) :: jj
      real(r64) :: tocfs

      ! Control
      ! nsegment,
      ! cascade_flag,

      ! Parameter
      ! hru_area, hru_segment, tosegment,

      ! Basin
      ! active_hrus, hru_route_order,

      ! Gwflow
      ! gwres_flow

      ! Potential_ET
      ! potet,

      ! Soilzone
      ! ssres_flow

      ! SolarRadiation
      ! swrad

      ! Srunoff
      ! sroff, strm_seg_in,

      ! Time_t
      ! cfs_conv, Timestep_seconds,

      ! Water use
      ! segment_gain, segment_transfer

      !***********************************************************************
      associate(nsegment => ctl_data%nsegment%value, &
                cascade_flag => ctl_data%cascade_flag%value, &

                hru_area => param_data%hru_area%values, &
                hru_segment => param_data%hru_segment%values, &
                tosegment => param_data%tosegment%values, &

                active_hrus => model_basin%active_hrus, &
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
          this%hru_outflow(j) = dble(sroff(j) + ssres_flow(j) + gwres_flow(j)) * tocfs

          if (cascade_flag == 0) then
            i = hru_segment(j)

            if (i > 0) then
              this%seg_gwflow(i) = this%seg_gwflow(i) + gwres_flow(j)
              this%seg_sroff(i) = this%seg_sroff(i) + sroff(j)
              this%seg_ssflow(i) = this%seg_ssflow(i) + ssres_flow(j)
              this%seg_lateral_inflow(i) = this%seg_lateral_inflow(i) + this%hru_outflow(j)
              this%seginc_sroff(i) = this%seginc_sroff(i) + dble(sroff(j)) * tocfs
              this%seginc_ssflow(i) = this%seginc_ssflow(i) + dble(ssres_flow(j)) * tocfs
              this%seginc_gwflow(i) = this%seginc_gwflow(i) + dble(gwres_flow(j)) * tocfs
              this%seginc_swrad(i) = this%seginc_swrad(i) + dble(swrad(j) * hru_area(j))
              this%seginc_potet(i) = this%seginc_potet(i) + dble(potet(j) * hru_area(j))
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

        ! Divide solar radiation and PET by sum of HRU area to get avarage
        if (this%noarea_flag == 0) then
          do i=1, nsegment
            this%seginc_swrad(i) = this%seginc_swrad(i) / this%segment_hruarea(i)
            this%seginc_potet(i) = this%seginc_potet(i) / this%segment_hruarea(i)
          enddo
        else
          ! If there are no HRUs associated with a segment, then figure out some
          ! other way to get the solar radiation, the following is not great.
          do i=1, nsegment
            if (this%segment_hruarea(i) > NEARZERO) then
              this%seginc_swrad(i) = this%seginc_swrad(i) / this%segment_hruarea(i)
              this%seginc_potet(i) = this%seginc_potet(i) / this%segment_hruarea(i)
            elseif (tosegment(i) > 0) then
              this%seginc_swrad(i) = this%seginc_swrad(tosegment(i))
              this%seginc_potet(i) = this%seginc_potet(tosegment(i))
            elseif (i > 1) then
              ! Set to next segment id
              this%seginc_swrad(i) = this%seginc_swrad(i-1)
              this%seginc_potet(i) = this%seginc_potet(i-1)
            else
              ! Assume at least 2 segments
              this%seginc_swrad(i) = this%seginc_swrad(i+1)
              this%seginc_potet(i) = this%seginc_potet(i+1)
            endif
          enddo
        endif
      end associate
    end subroutine

    module subroutine cleanup_Streamflow(this)
      class(Streamflow) :: this
        !! Streamflow class
    end subroutine
end submodule
