submodule (PRMS_STRMFLOW_IN_OUT) sm_streamflow_in_out
contains
  module subroutine init_Strmflow_in_out(this, ctl_data, model_basin, &
                                        model_time, model_summary)
    use prms_constants, only: dp
    implicit none

    class(Strmflow_in_out), intent(inout) :: this
      !! Strmflow_in_out class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time
    type(Summary), intent(inout) :: model_summary

    ! -----------------------------------------------------------------------
    ! Call the parent constructor first
    call this%Streamflow%init(ctl_data, model_basin, model_time, model_summary)

  end subroutine

  module subroutine run_Strmflow_in_out(this, ctl_data, model_basin, &
                                  model_potet, groundwater, soil, runoff, &
                                  model_time, model_solrad, model_obs)
    use prms_constants, only: dp, CFS2CMS_CONV, ONE_24TH
    implicit none

    class(Strmflow_in_out), intent(inout) :: this
      !! Strmflow_in_out class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
      !! Basin variables
    class(Potential_ET), intent(in) :: model_potet
    type(Gwflow), intent(in) :: groundwater
      !! Groundwater variables
    type(Soilzone), intent(in) :: soil
    type(Srunoff), intent(in) :: runoff
    type(Time_t), intent(in) :: model_time
    class(SolarRadiation), intent(in) :: model_solrad
    type(Obs), intent(in) :: model_obs

    ! Local Variables
    integer(i32) :: cseg
    integer(i32) :: iorder
    integer(i32) :: toseg

    ! Call parent class run routine first
    call this%Streamflow%run(ctl_data, model_basin, model_potet, &
                             groundwater, soil, runoff, model_time, model_solrad, model_obs)

    associate(print_debug => ctl_data%print_debug%value, &
              nsegment => model_basin%nsegment, &
              streamflow_cfs => model_obs%streamflow_cfs)

      this%seg_inflow = 0.0_dp
      this%seg_outflow = 0.0_dp
      this%seg_upstream_inflow = 0.0_dp

      do cseg = 1, nsegment
        iorder = this%segment_order(cseg)

        if (this%obsin_segment(iorder) > 0) then
          write(*,*) 'obsin_segment: ', iorder, this%obsin_segment(iorder)
          this%seg_upstream_inflow(iorder) = streamflow_cfs(this%obsin_segment(iorder))
        end if

        this%seg_inflow(iorder) = this%seg_upstream_inflow(iorder) + this%seg_lateral_inflow(iorder)

        if (this%obsout_segment(iorder) > 0) then
          this%seg_outflow(iorder) = streamflow_cfs(this%obsout_segment(iorder))
        else
          this%seg_outflow(iorder) = this%seg_inflow(iorder)
        end if

        if (this%seg_outflow(iorder) < 0.0) then
          if (print_debug > -1) then
            print *, 'WARNING, negative flow from segment:', iorder, ' flow:', this%seg_outflow(iorder)
            print *, '         likely a water-use specification or replacement flow issue'
          end if
        end if

        toseg = this%tosegment(iorder)

        if (toseg /= 0) then
          this%seg_upstream_inflow(toseg) = this%seg_upstream_inflow(toseg) + this%seg_outflow(iorder)
        end if
      enddo

      ! Flow_out is the total flow out of the basin, which allows for multiple outlets
      ! includes closed basins (tosegment=0)
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
    end associate

  end subroutine

  module subroutine cleanup_Strmflow_in_out(this, ctl_data)
    class(Strmflow_in_out), intent(in) :: this
      !! Strmflow_in_out class
    type(Control), intent(in) :: ctl_data

    ! Call the parent cleanup
    call this%Streamflow%cleanup(ctl_data)
  end subroutine
end submodule
