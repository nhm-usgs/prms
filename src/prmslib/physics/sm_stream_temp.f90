submodule (PRMS_STRMTEMP) sm_stream_temp
  contains
    !! StreamTemp constructor
    module subroutine init_StreamTemp(this, ctl_data, model_basin, model_streamflow) result(this)
      use prms_constants, only: dp, NEARZERO
      implicit none

      class(StreamTemp) :: this
        !! StreamTemp class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Streamflow), intent(in) :: model_streamflow

      ! Local variables
      integer(i32) :: i
      integer(i32) :: ierr
      integer(i32) :: ii
      integer(i32) :: iseg
      integer(i32) :: j
      integer(i32) :: k
      integer(i32) :: this_seg

      real(r32) :: alrs
      real(r32) :: cos_d
      real(r32) :: decl
      real(r32) :: sinhro
      real(r32) :: tan_d
      real(r32) :: tano
      real(r32) :: tanod
      real(r32) :: temp

      ! ------------------------------------------------------------------------
      associate(nsegment => ctl_data%nsegment%value, &
                init_vars_from_file => ctl_data%init_vars_from_file%value, &
                print_debug => ctl_data%print_debug%value, &
                stream_temp_shade_flag => ctl_data%stream_temp_shade_flag%value, &

                active_hrus => model_basin%active_hrus, &
                hru_route_order => model_basin%hru_route_order, &                

                ! seg_elev => param_data%seg_elev%values, &
                ! seg_lat => param_data%seg_lat%values, &

                azrh => model_streamflow%azrh%values, &
                hru_segment => model_streamflow%hru_segment%values, &
                seg_length => model_streamflow%seg_length%values, &
                tosegment => model_streamflow%tosegment%values, &
                segment_order => model_streamflow%segment_order, &
                segment_up => model_streamflow%segment_up)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! TODO: Allocate the parameters
        
        allocate(this%gw_silo(nsegment, 365))
        allocate(this%gw_sum(nsegment))
        allocate(this%hru_area_sum(nsegment))
        allocate(this%press(nsegment))
        allocate(this%seg_carea_inv(nsegment))
        allocate(this%seg_ccov(nsegment))
        allocate(this%seg_close(nsegment))
        allocate(this%seg_daylight(nsegment))
        allocate(this%seg_hru_count(nsegment))
        allocate(this%seg_humid(nsegment))
        allocate(this%seg_melt(nsegment))
        allocate(this%seg_potet(nsegment))
        allocate(this%seg_rain(nsegment))
        allocate(this%seg_shade(nsegment))
        allocate(this%seg_tave_air(nsegment))
        allocate(this%seg_tave_gw(nsegment))
        allocate(this%seg_tave_lat(nsegment))
        allocate(this%seg_tave_sroff(nsegment))
        allocate(this%seg_tave_ss(nsegment))
        allocate(this%seg_tave_upstream(nsegment))
        allocate(this%seg_tave_water(nsegment)) ! previous ??
        allocate(this%seg_width(nsegment))
        allocate(this%ss_silo(nsegment, 365))
        allocate(this%ss_sum(nsegment))

        ! FIXME: seg_lat is a parameter which is read-only
        ! Convert latitude from degrees to radians
        ! seg_lat = seg_lat * DEG_TO_RAD

        ! FIXME: seg_length is a parameter which is read-only
        ! Convert stream length in meters to km
        ! seg_length = seg_length / 1000.0

        ! Initialize declared variables
        this%seg_ccov = 0.0
        this%seg_humid = 0.0
        this%seg_potet = 0.0_dp
        this%seg_tave_air = 0.0
        this%seg_tave_gw = 0.0
        this%seg_tave_sroff = 0.0
        this%seg_tave_ss = 0.0
        this%seg_tave_upstream = 0.0
        this%seg_width = 0.0

        if (init_vars_from_file == 0) then
          this%gw_silo = 0.0
          this%gw_sum = 0.0
          this%seg_tave_water = 0.0
          this%ss_silo = 0.0
          this%ss_sum = 0.0

          ! These are set to zero because they will be incremented to 1 down in the run function
          this%gw_index = 0
          this%ss_index = 0
        endif

        this%seg_daylight = 12.0

        if (stream_temp_shade_flag == 0) then
          allocate(this%cos_lat_decl(366, nsegment))
          allocate(this%cos_seg_lat(nsegment))
          allocate(this%horizontal_hour_angle(366, nsegment))
          allocate(this%level_sunset_azimuth(366, nsegment))
          allocate(this%local_sunrise_hour_angle(366, nsegment))
          allocate(this%local_sunset_hour_angle(366, nsegment))
          allocate(this%max_solar_altitude(366, nsegment))
          allocate(this%shade_jday(nsegment, 366))
          allocate(this%sin_alrs(366, nsegment))
          allocate(this%sin_declination(366, nsegment))
          allocate(this%sin_lat_decl(366, nsegment))
          allocate(this%sin_seg_lat(nsegment))
          allocate(this%svi_jday(nsegment, 366))
          allocate(this%total_shade(366, nsegment))

          this%shade_jday = 0.0
          this%svi_jday = 0.0

          ! FIXME: seg_lat is a parameter which is read-only
          ! seg_lat = 0.0
        endif

        ! Figure out how many HRUs are connected to each segment
        this%seg_hru_count = 0
        do k=1, active_hrus
          j = hru_route_order(k)
          i = hru_segment(j)

          if (i == 0) cycle

          this%seg_hru_count(i) = this%seg_hru_count(i) + 1
        enddo

        ! Find segments that are too short and print them out as they are found
        do i=1, nsegment
          if (seg_length(i) < NEARZERO) then
            PRINT *, 'ERROR, seg_length too small for segment:', i, ', value:', seg_length(i)
            ierr = 1
          endif
        enddo

        ! Exit if there are any segments that are too short
        if (ierr == 1) then
          stop
          ! Inputerror_flag = ierr
          ! RETURN
        endif

        this%seg_close = segment_up ! assign upstream values
        do j=1, nsegment
          ! Set values based on routing order for segments without associated HRUs
          i = segment_order(j)

          ! If a segment does not have any HRUs, need to find the closest one for elevation and latitude info
          ! NOTE: seg_close variable can go upstream, downstream, or offstream looking for the "closest" segment with
          ! an HRU. This is not approprite to use in a situation where computed values are going to be taken from
          ! the closest HRU (i.e. flow).
          !
          ! This does work for NHM network (most comprehensive test).
          if (this%seg_hru_count(i) == 0) then
            if (segment_up(i) == 0) then
              if (tosegment(i) > 0) then
                ! Assign downstream values
                ! FIXME: don't have a value yet, need to fix
                this%seg_close(i) = tosegment(i)
              else
                ! No upstream or downstream segment
                if (j > 1) then
                  ! Set to previous segment id
                  this%seg_close(i) = segment_order(j-1)
                else
                  ! Assume at least 2 segments
                  this%seg_close(i) = segment_order(j+1)
                endif
              endif
            endif

            if (seg_elev(this%seg_close(i)) == 30000.0) then
              ! Need different segment
              iseg = -1

              do k=j+1, nsegment
                ! Find first segment with valid values
                ii = segment_order(k)
                if (this%seg_hru_count(ii) > 0) then
                  this%seg_close(i) = ii
                  exit
                endif
              enddo

              if (iseg == -1) then
                if (j > 1) then
                  ! Set to previous segment id
                  this%seg_close(i) = segment_order(j-1)
                else
                  ! This is a problem, shouldn't happen
                  STOP 'ERROR, segments do not have associated HRUs'
                  ! this%seg_close(i) = segment_order(1) ! set to first segment id
                endif
              endif
            endif
          endif

          ! Compute atmospheric pressure based on segment elevation.
          this%press(i) = 1013.0 - (0.1055 * seg_elev(i))

          if (stream_temp_shade_flag == 0) then
            ! LATITUDE TRIGONOMETRIC PARAMETERS
            this%cos_seg_lat(i) = cos(seg_lat(i)) ! coso

            if (this%cos_seg_lat(i) < NEARZERO) this%cos_seg_lat(i) = NEARZERO

            this%sin_seg_lat(i) = sin(seg_lat(i)) ! sino
            tano = this%sin_seg_lat(i) / this%cos_seg_lat(i)

            do k=1, 366
              ! DECLINATION TRIGONOMETRIC PARAMETERS
              decl = 0.40928 * cos(((2.0 * PI) / 365.25) * (172.0 - k))
              cos_d = cos(decl)
              this%sin_declination(k, i) = sin(decl) ! sin_d

              if (cos_d < NEARZERO) cos_d = NEARZERO
              tan_d = this%sin_declination(k, i) / cos_d

              ! JOINT LATITUDE & DECLINATION TRIGONOMETRIC PARAMETERS
              this%cos_lat_decl(k, i) = this%cos_seg_lat(i) * cos_d ! cosod
              this%sin_lat_decl(k, i) = this%sin_seg_lat(i) * this%sin_declination(k, i) ! sinod
              tanod = tano * tan_d

              if (abs(tanod) > 1.0) tanod = sign(1.0, tanod)

              ! LEVEL-PLAIN SUNRISE/SET HOUR ANGLE
              this%horizontal_hour_angle(k, i) = acos(-tanod) ! hrso
              sinhro = sin(this%horizontal_hour_angle(k, i))

              ! LEVEL-PLAIN SOLAR AZIMUTH
              temp = -this%sin_declination(k, i) / this%cos_seg_lat(i)

              if (abs(temp) > 1.0) temp = sign(1.0, temp)

              this%level_sunset_azimuth(k, i) = acos(temp) ! azso

              ! MAXIMUM POSSIBLE SOLAR ALTITUDE
              this%max_solar_altitude(k, i) = asin(this%sin_lat_decl(k,i) + this%cos_lat_decl(k,i) ) ! alsmx

              ! TOTAL POTENTIAL SHADE ON LEVEL-PLAIN ! totsh
              this%total_shade(k, i) = 2.0 * ((this%horizontal_hour_angle(k, i) * &
                                               this%sin_lat_decl(k, i)) + &
                                               (sinhro * this%cos_lat_decl(k, i)))

              if (this%total_shade(k, i) < NEARZERO) this%total_shade(k, i) = NEARZERO


              if (azrh(i) <= (-this%level_sunset_azimuth(k, i))) then
                ! CHECK FOR REACH AZIMUTH LESS THAN SUNRISE
                alrs = 0.0
              elseif (azrh(i) >= this%level_sunset_azimuth(k, i)) then
                ! CHECK FOR REACH AZIMUTH GREATER THAN SUNSET
                alrs = 0.0
              elseif (azrh(i) == 0.0) then
                ! REACH AZIMUTH IS BETWEEN SUNRISE & SUNSET
                alrs = this%max_solar_altitude(k, i)
              else
                alrs = this%solalt(param_data, this%cos_seg_lat(i), this%sin_seg_lat(i), this%sin_declination(k,i), azrh(i), 0.0, this%max_solar_altitude(k,i))
                this%sin_alrs(k, i) = sin(alrs)
                ! END REACH & SOLAR AZIMUTH CHECK
              endif
            enddo
          endif
        enddo

        ! There may be headwater segments that do not have any HRUs and do not have any upstream segments to produce
        ! streamflow. These segments will never have any streamflow, and consequently never be able to simulate
        ! stream temperature. This block finds these and sets the stream temperature value to -99.9. Subsequent code
        ! should be able to check if the temperature value is less than -99.0 and know that it doesn't need to do
        ! any stream temperature calculation because there will never be any water in the segment.
        !
        ! This code is similar to the code above that computes latitude and elevation, but is different because it
        ! must always look upstream because the downstream computations will not have been done when the current
        ! segment is being calculated.
        this%seg_tave_water = 0.0

        do j=1, nsegment
          this_seg = segment_order(j)

          ! Check if this segment has any HRUs, keep moving up stream if not.
          do
            if (this%seg_hru_count(this_seg) == 0) then
              ! Hit the headwater segment without finding any HRUs (i.e. sources of streamflow)
              ! Set the stream temp to -99.9 for this segment because there will never be any flow in this segment
              if (segment_up(this_seg) == 0) then
                this%seg_tave_water(segment_order(j)) = -99.9
                exit
              endif

              ! There is an upstream segment, check that segment for HRUs
              this_seg = segment_up(this_seg)
            else
              ! This segment has HRUs so there will be no streamflow
              exit
            endif
          enddo
        enddo

        ! TODO: Add output variables
        ! dlit
        ! seg_ccov
        ! seg_humid
        ! seg_maxtemp
        ! seg_melt
        ! seg_potet
        ! seg_rain
        ! seg_shade
        ! seg_width
        ! t_ground
        ! t_gw
        ! t_roff
        ! t_ss
        ! temp_avg
        ! upstrm_temp


      end associate
    end subroutine


    module subroutine run_StreamTemp(this, ctl_data, model_basin, model_precip, model_temp, model_potet, &
                                     model_obs, model_streamflow, snow, model_solrad, model_time)
      use prms_constants, only: CFS2CMS_CONV, dp, NEARZERO
      implicit none

      class(StreamTemp) :: this
        !! StreamTemp class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      class(Precipitation), intent(in) :: model_precip
      class(Temperature), intent(in) :: model_temp
      class(Potential_ET), intent(in) :: model_potet
      type(Obs), intent(in) :: model_obs
      class(Streamflow), intent(in) :: model_streamflow
      type(Snowcomp), intent(in) :: snow
      type(SolarRadiation), intent(in) :: model_solrad
      type(Time_t), intent(in) :: model_time

      ! Local Variables
      integer(i32) :: i
      integer(i32) :: idx1D
      integer(i32) :: iseg
      integer(i32) :: j
      integer(i32) :: k

      real(r32) :: ak1
      real(r32) :: ak2
      real(r32) :: ccov
      real(r32) :: fs
      real(r32) :: harea
      real(r32) :: svi
      real(r32) :: t_o
      real(r32) :: te
      real(r32) :: up_temp

      real(r64) :: qlat

      ! Control
      ! nsegment, strmtemp_humidity_flag, stream_temp_shade_flag,

      ! Parameter
      ! gw_tau, hru_area, hru_segment, lat_temp_adj, segshade_sum, segshade_win,
      ! seg_humidity, seg_humidity_sta, seg_length, ss_tau, tosegment, width_alpha,
      ! width_m,

      ! Basin
      ! active_hrus, hru_route_order,

      ! Precipitation
      ! hru_rain

      ! Climateflow
      ! seg_outflow,

      ! Obs
      ! humidity

      ! Potential_ET
      ! potet, humidity_hru

      ! Streamflow
      ! seginc_swrad, segment_order,

      ! Snowcomp
      ! snowmelt,

      ! SolarRadiation
      ! hru_cossl, soltab_potsw, swrad,

      ! Time_t
      ! day_of_year, Nowday, Nowmonth, Nowyear, Summer_flag

      ! ------------------------------------------------------------------------
      associate(nsegment => ctl_data%nsegment%value, &
                stream_temp_shade_flag => ctl_data%stream_temp_shade_flag%value, &
                strmtemp_humidity_flag => ctl_data%strmtemp_humidity_flag%value, &

                ! gw_tau => param_data%gw_tau%values, &
                ! lat_temp_adj => param_data%lat_temp_adj%values, &
                ! segshade_sum => param_data%segshade_sum%values, &
                ! segshade_win => param_data%segshade_win%values, &
                ! seg_humidity => param_data%seg_humidity%values, &
                ! seg_humidity_sta => param_data%seg_humidity_sta%values, &
                seg_length => param_data%seg_length%values, &
                ! ss_tau => param_data%ss_tau%values, &
                
                ! width_alpha => param_data%width_alpha%values, &
                ! width_m => param_data%width_m%values, &

                active_hrus => model_basin%active_hrus, &
                hru_area => model_basin%hru_area%values, &
                hru_route_order => model_basin%hru_route_order, &

                potet => model_potet%potet, &
                humidity_hru => model_potet%humidity_hru, &

                hru_rain => model_precip%hru_rain, &

                tavg => model_temp%tavg, &

                humidity => model_obs%humidity, &

                hru_segment => model_streamflow%hru_segment%values, &
                seg_outflow => model_streamflow%seg_outflow, &
                seginc_swrad => model_streamflow%seginc_swrad, &
                segment_order => model_streamflow%segment_order, &
                tosegment => model_streamflow%tosegment%values, &

                snowmelt => snow%snowmelt, &

                hru_cossl => model_solrad%hru_cossl, &
                soltab_potsw => model_solrad%soltab_potsw, &
                swrad => model_solrad%swrad, &

                day_of_year => model_time%day_of_year, &
                Nowday => model_time%Nowday, &
                Nowmonth => model_time%Nowmonth, &
                Nowyear => model_time%Nowyear, &
                Summer_flag => model_time%Summer_flag)

        this%seg_tave_air = 0.0

        ! humidity info comes from parameter file when strmtemp_humidity_flag==1
        ! Otherwise it comes as daily values per HRU from CBH. Code for this is
        ! down in the HRU loop.
        if (strmtemp_humidity_flag == 1) then
          do i=1, nsegment
            idx1D = (Nowmonth - 1) * nsegment + i
            this%seg_humid(i) = seg_humidity(idx1D)
            ! this%seg_humid(i) = seg_humidity(i, Nowmonth)
          enddo
        elseif (strmtemp_humidity_flag == 2) then
          ! use station data
          do i=1, nsegment
            this%seg_humid(i) = humidity(seg_humidity_sta(i))
          enddo
        else
          this%seg_humid = 0.0
        endif

        this%seg_potet = 0.0_dp
        this%seg_ccov = 0.0
        this%seg_melt = 0.0
        this%seg_rain = 0.0
        this%hru_area_sum = 0.0

        ! Compute segment lateral inflow temperatures and segment meteorological values
        do k=1, active_hrus
          j = hru_route_order(k)
          ccov = 1.0 - (swrad(j) / sngl(soltab_potsw(day_of_year, j)) * sngl(hru_cossl(j)))

          if (ccov < NEARZERO) then
            ccov = 0.0
          elseif (ccov > 1.0) then
            ccov = 1.0
          endif

          harea = hru_area(j)
          i = hru_segment(j)
          if (i == 0) cycle

          ! Compute temperature of surface runoff here for HRU and stream segments
          this%seg_tave_air(i) = this%seg_tave_air(i) + (tavg(j)) * harea
          this%hru_area_sum(i) = this%hru_area_sum(i) + harea

          ! Compute segment humidity if info is specified in CBH as timeseries by HRU
          if (strmtemp_humidity_flag == 0) then
            this%seg_humid(i) = this%seg_humid(i) + Humidity_hru(j) * harea
          endif

          ! Figure out the contributions of the HRUs to each segment for these drivers.
          this%seg_ccov(i) = this%seg_ccov(i) + ccov * harea
          this%seg_potet(i) = this%seg_potet(i) + dble(potet(j) * harea)
          this%seg_melt(i) = this%seg_melt(i) + snowmelt(j) * harea
          this%seg_rain(i) = this%seg_rain(i) + hru_rain(j) * harea
        enddo

        do j=1, nsegment
          i = segment_order(j)
          if (this%seg_hru_count(i) > 0) then
            ! carea = this%seg_carea_inv(i)
            this%seg_ccov(i) = this%seg_ccov(i) / this%hru_area_sum(i)
            this%seg_potet(i) = this%seg_potet(i) / dble(this%hru_area_sum(i))
            this%seg_tave_air(i) = this%seg_tave_air(i) / this%hru_area_sum(i)
            this%seg_melt(i) = this%seg_melt(i) / this%hru_area_sum(i)
            this%seg_rain(i) = this%seg_rain(i) / this%hru_area_sum(i)

            if (strmtemp_humidity_flag == 0) then
              this%seg_humid(i) = this%seg_humid(i) / this%hru_area_sum(i)

              ! DANGER potential hack here: Should CBH humidity data be converted to decimal fraction in
              ! the CBH file? Probably so. For now, convert it here.
              ! humidity coming from CBH is in percent, not decimal fraction
              this%seg_humid(i) = this%seg_humid(i) * 0.01
            endif
          else
            ! This block for segments that don't have contributing HRUs
            iseg = this%seg_close(i) ! doesn't work if upstream segment
            this%seg_tave_air(i) = this%seg_tave_air(iseg)
            this%seg_ccov(i) = this%seg_ccov(iseg)
            this%seg_potet(i) = this%seg_potet(iseg)
            this%seg_melt(i) = this%seg_melt(iseg)
            this%seg_rain(i) = this%seg_rain(iseg)

            if (strmtemp_humidity_flag == 0) then
              this%seg_humid(i) = this%seg_humid(iseg) * this%seg_carea_inv(iseg) ! ??
              ! DANGER humidity coming from CBH is in percent, not decimal fraction
              ! Same as comment in above block
              this%seg_humid(i) = this%seg_humid(i) * 0.01
            endif
          endif
        enddo

        ! Compute the running averages for groundwater and subsurface temperatures.
        if (this%gw_index >= gw_tau(i)) then
          this%gw_index = 1
        else
          this%gw_index = this%gw_index + 1
        endif

        if (this%ss_index >= ss_tau(i)) then
          this%ss_index = 1
        else
          this%ss_index = this%ss_index + 1
        endif

        ! Mark all of the upstream segment temperatures as not having been computed yet.
        ! If the value is something other than -100.0, then I know that it has been computed.
        ! Trying to get at the differece between computed bad values and segments that have not been
        ! computed yet.
        this%seg_tave_upstream(i) = -100.0

        ! Big do loop
        do j=1, nsegment
          i = segment_order(j)

          ! Compute the 1D index given a 2D argument; used for certain parameters
          idx1D = (Nowmonth - 1) * nsegment + i

          ! !! LOOP BREAKS HERE !!

          ! If the seg_tave_water value has been set to -99.9 (in init), then this is a segment that will
          ! never have streamflow because it does not have any HRUs connected to it and none of the
          ! upstream segments (if there are any) have HRUs connected. Because there can never be any
          ! flow, the temperature calculation will always fail, so don't bother with it.
          if (this%seg_tave_water(i) < -99.0) then
            cycle
          endif

          ! !! LOOP BREAKS HERE !!

          ! If the seginc_swrad value has been set to -99.9 (route_run), then this segment will
          ! never have solar radiation because it does not have any HRUs connected to it and none of the
          ! upstream or downstream segments have HRUs connected.
          if (seginc_swrad(i) < -99.0) then
            this%seg_tave_water(i) = -99.9
            cycle
          endif

          ! GW moving average
          this%gw_sum(i) = this%gw_sum(i) - this%gw_silo(i, this%gw_index)
          this%gw_silo(i, this%gw_index) = this%seg_tave_air(i)
          this%gw_sum(i) = this%gw_sum(i) + this%gw_silo(i, this%gw_index)
          this%seg_tave_gw(i) = this%gw_sum(i) / gw_tau(i)

          ! SS moving average
          this%ss_sum(i) = this%ss_sum(i) - this%ss_silo(i, this%ss_index)
          this%ss_silo(i, this%ss_index) = this%seg_tave_air(i)
          this%ss_sum(i) = this%ss_sum(i) + this%ss_silo(i, this%ss_index)
          this%seg_tave_ss(i) = this%ss_sum(i) / ss_tau(i)

          ! Find upstream intitial inflow temperature for segment i
          ! i is the current segment
          ! k is the upstream segment
          fs = 0.0
          up_temp = 0.0

          do k = 1, nsegment
            if (tosegment(k) == i) then
              if (this%seg_tave_water(k) > -1.0) then
                up_temp = up_temp + (this%seg_tave_water(k) * sngl(seg_outflow(k)))
                fs = fs + sngl(seg_outflow(k))
              endif
            endif
          enddo

          ! Finish computing this%seg_tave_upstream
          if (fs > NEARZERO) then
            this%seg_tave_upstream(i) = up_temp / fs
          else
            ! -98.9 is the code for no flow on this timestep
            this%seg_tave_upstream(i) = -98.9
          endif

          ! debug
          if (this%seg_tave_upstream(i) > 100.0) then
            write(*,*) "upstream_temp: i = ", i, " this%seg_tave_upstream = ", this%seg_tave_upstream(i), " fs = ", &
                       fs, " seg_tave_water = ", this%seg_tave_water(i), " troff = " , this%seg_tave_air(i), " up_temp = ", up_temp
          endif

          ! Compute flow-dependent water-in-segment width value
          if (seg_outflow(i) > NEARZERO) then
            this%seg_width(i) = width_alpha(i) * sngl(seg_outflow(i))**width_m(i)
          else
            this%seg_width(i) = 0.0

            if (this%seg_tave_water(i) > -99.0) then
              ! This segment has upstream HRUs somewhere, but the current day's flow is zero
              this%seg_tave_water(i) = -98.9
            endif
          endif

          ! Compute the shade on the segment. Either set by value in the parameter file or computed
          if (stream_temp_shade_flag == 1) then
            if (Summer_flag == 0) then
              this%seg_shade(i) = segshade_win(i)
            else
              this%seg_shade(i) = segshade_sum(i)
            endif

            ! svi = RIPARIAN VEGETATION SHADE
            svi = 0.0
          else
            call this%shday(param_data, model_time, i, this%seg_shade(i), svi)
          endif

          ! Start working towards the computation of the equilibrium temperature
          qlat = 0.0_dp
          this%seg_tave_lat(i) = 0.0
          ak1 = 0.0
          ak2 = 0.0

          ! Inputs: this%seg_tave_gw, this%seg_tave_air, this%seg_tave_ss, this%seg_tave_upstream, this%seg_melt, this%seg_rain
          ! Outputs: qlat (in CMS), this%seg_tave_lat
          call this%lat_inflow(param_data, model_streamflow, qlat, this%seg_tave_lat(i), &
                               i, this%seg_tave_gw(i), this%seg_tave_air(i), &
                               this%seg_tave_ss(i), this%seg_melt(i), this%seg_rain(i))


          ! This code does not handle thermodynamics of ice, so temperatures below 0 are not allowed.
          ! The question is when to set temperatures below 0 to 0. If, after computing the running averages
          ! and mixing the different sources of lateral flow, the temperature is less than 0, set the lateral
          ! flow temperature to 0 here.
          if (this%seg_tave_lat(i) < NEARZERO) then
            this%seg_tave_lat(i) = 0.0
          endif

          ! Compute t_o
          ! t_o is the temperature of the water at the beginning of the time step (this is To in equation 32)
          if (this%seg_tave_water(i) < -99.0) then
            ! No flow in this segment and there never will be becuase there are no upstream HRUs.
            t_o = this%seg_tave_water(i)
          elseif (this%seg_tave_water(i) < -98.0) then
            ! No flow in this segment on this time step, but could be on future time step
            t_o = this%seg_tave_water(i)
          elseif ((fs <= NEARZERO) .and. (qlat <= NEARZERO)) then
            ! If there is no flow, set the temperature to -98.9
            ! -99.9 means that the segment never has any flow (determined up in init).
            ! -98.9 means that this a segment that could have flow, but doesn't
            this%seg_tave_water(i) = -98.9
            t_o = this%seg_tave_water(i)
          elseif (fs <= NEARZERO) then
            ! if this is true, then there is no flow from upstream, but there is lateral inflow
            t_o = this%seg_tave_lat(i) + lat_temp_adj(idx1D)
            ! t_o = this%seg_tave_lat(i) + lat_temp_adj(i, Nowmonth)
          elseif (qlat <= NEARZERO) then
            ! if this is true, then there is no lateral flow, but there is flow from upstream
            t_o = this%seg_tave_upstream(i)
          else
            ! if this is true, then there is both lateral flow and flow from upstream
            !  qlat is in CMS so fs needs to be converted
            t_o = sngl((this%seg_tave_upstream(i) * fs * CFS2CMS_CONV) + &
                        (sngl(qlat) * (this%seg_tave_lat(i) + lat_temp_adj(idx1D)))) / &
                        sngl((fs * CFS2CMS_CONV) + sngl(qlat))
            ! t_o = sngl((this%seg_tave_upstream(i) * fs * CFS2CMS_CONV) + &
            !             (sngl(qlat) * (this%seg_tave_lat(i) + lat_temp_adj(i, Nowmonth)))) / &
            !             sngl((fs * CFS2CMS_CONV) + sngl(qlat))
          endif

          ! debug
          if (t_o /= t_o) then
            write(*,*) "t_o is Nan, this%seg_tave_upstream = ", this%seg_tave_upstream(i), " fs = ", fs, &
                       " qlat = ", qlat, " this%seg_tave_lat = ", this%seg_tave_lat(i), " lat_temp_adj = ", lat_temp_adj(idx1D)
            ! write(*,*) "t_o is Nan, this%seg_tave_upstream = ", this%seg_tave_upstream(i), " fs = ", fs, &
            !            " qlat = ", qlat, " this%seg_tave_lat = ", this%seg_tave_lat(i), " lat_temp_adj = ", lat_temp_adj(i, Nowmonth)
            continue
          endif

          ! debug
          if (t_o > 100.0) then
            write(*,*) "this is the place: t_o = ", t_o, " ted = ", te, " seg_id = ", i
            write(*,*) "   this%seg_tave_upstream = ", this%seg_tave_upstream(i), " fs = ", fs, &
                           " qlat = ", qlat, " this%seg_tave_lat = ", this%seg_tave_lat(i), " lat_temp_adj = ", lat_temp_adj(idx1D)
            ! write(*,*) "   this%seg_tave_upstream = ", this%seg_tave_upstream(i), " fs = ", fs, &
            !                " qlat = ", qlat, " this%seg_tave_lat = ", this%seg_tave_lat(i), " lat_temp_adj = ", lat_temp_adj(i, Nowmonth)
            write(*,*) "   width = ", this%seg_width(i), Nowyear, Nowmonth, Nowday
            continue
            exit
          endif

          ! Need a good value of t_o
          if (t_o > -98.0) then
            ! This block computes the value for seg_tave_water

            ! Compute the equilibrium temerature
            ! Out: te, ak1, ak2
            ! In: this%seg_shade, svi, i, t_o
            call this%equilb(param_data, model_streamflow, te, ak1, ak2, &
                             this%seg_shade(i), svi, i, t_o)

            ! Compute the daily mean water temperature
            ! In: t_o, qlat, this%seg_tave_lat(i), te, ak1, ak2, i, seg_width, seg_length
            this%seg_tave_water(i) = twavg(fs, t_o, qlat, this%seg_tave_lat(i), te, ak1, ak2, this%seg_width(i), seg_length(i))
          else
            ! bad t_o value
            this%seg_tave_water(i) = -98.9
          endif
        enddo
      end associate
    end subroutine

    module subroutine cleanup_StreamTemp(this, ctl_data)
      class(StreamTemp), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine

    !*******************************************************************************
    !    "equilb"
    !*******************************************************************************
    module subroutine equilb (this, param_data, model_streamflow, ted, ak1d, &
                              ak2d, sh, svi, seg_id, t_o)
      ! PURPOSE:
      !   1. Determine the average daily equilibrium water temperature parameters
      !   2. Determine the maximum daily equilibrium water temperature parameters
      use prms_constants, only: CFS2CMS_CONV, NEARZERO
      implicit none

      ! Arguments:
      class(StreamTemp), intent(in) :: this
      type(Parameters), intent(in) :: param_data
      class(Streamflow), intent(in) :: model_streamflow
      real(r32), intent(out) :: ted
      real(r32), intent(out) :: ak1d
      real(r32), intent(out) :: ak2d
      real(r32), intent(in) :: sh
      real(r32), intent(in) :: svi
      integer(i32), intent(in) :: seg_id
      real(r32), intent(in) :: t_o

      ! Local Variables:  !RSR, maybe declare energy balance fluxes
      real(r32), parameter :: AKZ = 1.65
      real(r32), parameter :: A = 5.40E-8
      real(r32), parameter :: RAD_CONVERT = 41840.0 / 86400.0

      real(r32) :: b
      real(r32) :: bow_coeff
      real(r32) :: c
      real(r32) :: d
      real(r32) :: del_ht
      real(r32) :: delt
      real(r32) :: evap
      real(r32) :: foo
      real(r32) :: hf
      real(r32) :: hnet
      real(r32) :: hs
      real(r32) :: ltnt_ht
      real(r32) :: q_init
      real(r32) :: sw_power
      real(r32) :: vp_sat
      real(r64) :: ha
      real(r64) :: hv
      real(r64) :: taabs

      ! Parameter
      ! albedo, seg_slope

      ! Climateflow
      ! seg_inflow

      ! Streamflow
      ! seginc_swrad

      ! ************************************************************************
      associate(albedo => param_data%albedo%values(1), &
                seg_slope => param_data%seg_slope%values, &
                seg_inflow => model_streamflow%seg_inflow, &
                seginc_swrad => model_streamflow%seginc_swrad)

        taabs = dble(t_o + ZERO_C)
        vp_sat = 6.108 * exp(17.26939 * t_o / (t_o + 237.3))

        !  Convert units and set up parameters
        q_init = sngl(seg_inflow(seg_id) * CFS2CMS_CONV)
        if (q_init < NEARZERO) q_init = NEARZERO

        ! sw_power should be in watts / m2
        ! seginc_swrad is in langleys / day
        ! Used to use RAD_CONVERT, the conversion I'm using now is a slightly
        ! different number.
        sw_power = 11.63 / 24.0 * sngl(seginc_swrad(seg_id))

        del_ht = 2.36E06   ! could multiple by 10E6 for this and other terms later to reduce round-off
        ltnt_ht = 2495.0E06

        ! If humidity is 1.0, there is a divide by zero below.
        if (this%seg_humid(seg_id) > 0.99) then
            foo = 0.99
        else
            foo = this%seg_humid(seg_id)
        endif

        bow_coeff = (0.00061 * this%press(seg_id)) / (vp_sat * (1.0 - foo))
        evap = sngl( this%seg_potet(seg_id) * MPS_CONVERT )

        ! Heat flux components
        ! document - ha = (1-rl)(1-sh)(1+0.17Cl**2)(0.61+0.05*sqrt(vp_sat)*stefan(Ta+273.16)**4

        ha = ( (3.354939D-8 + 2.74995D-9 * dble(sqrt(this%seg_humid(seg_id) * vp_sat))) * dble((1.0 - sh) &
            * (1.0 + (0.17*(this%seg_ccov(seg_id)**2)))) ) * (taabs**4)

        ! hf is heat from stream friction. See eqn. 14.  q_init is in CMS
        hf = 9805.0 * (q_init/this%seg_width(seg_id)) * seg_slope(seg_id)
        hs = (1.0 - sh) * sw_power * (1.0 - albedo)
        hv = 5.24D-8 * dble(svi) * (taabs**4)

        ! Stefan-Boltzmann constant = 5.670373D-08; emissivity of water = 0.9526, times each other: 5.4016D-08
        ! hw = water-emitted longwave radiation
        ! hw = 5.4016D-08 * (taabs**4)  hw is include in other computations

        ! Determine equilibirium coefficients
        b = bow_coeff * evap * (ltnt_ht + (del_ht * t_o)) + AKZ - (del_ht * evap)
        c = bow_coeff * del_ht * evap
        d = (sngl(ha + hv) + hf + hs) + (ltnt_ht * evap * ((bow_coeff * t_o) - 1.0) + (this%seg_tave_gw(seg_id) * AKZ))

        ! Determine equilibrium temperature & 1st order thermal exchange coef.
        ted = t_o

        call this%teak1(param_data, A, b, c, d, ted, ak1d)

        ! Determine 2nd order thermal exchange coefficient
        hnet = (A * ((t_o + ZERO_C)**4)) + (b * t_o) - (c * (t_o**2.0)) - d
        delt = t_o - ted

        if ( abs(delt) < NEARZERO) then
          ak2d = 0.0
        else
          ak2d = ((delt * ak1d) - hnet) / (delt**2)
        endif
      end associate
    end subroutine



    !*********************************************************************************
    ! Compute the flow-weighted average temperature and a total sum of lateral inflows
    !*********************************************************************************
    module subroutine lat_inflow(param_data, model_streamflow, qlat, tl_avg, id, tave_gw, tave_air, tave_ss, melt, rain)
      use prms_constants, only: dp, CFS2CMS_CONV, NEARZERO
      implicit none

      ! Arguments
      type(Parameters), intent(in) :: param_data
      class(Streamflow), intent(in) :: model_streamflow
      real(r32), intent(out) :: tl_avg
      real(r64), intent(out) :: qlat
      integer(i32), intent(in) :: id
      real(r32), intent(in) :: tave_gw
      real(r32), intent(in) :: tave_air
      real(r32), intent(in) :: tave_ss
      real(r32), intent(in) :: melt
      real(r32), intent(in) :: rain

      ! Local Variables
      real(r32) :: melt_wt
      real(r32) :: rain_wt
      real(r32) :: troff
      real(r32) :: tss
      real(r32) :: weight_gw
      real(r32) :: weight_roff
      real(r32) :: weight_ss

      ! Parameter
      ! melt_temp

      ! Streamflow
      ! seg_lateral_inflow, seginc_gwflow, seginc_sroff, seginc_ssflow

      !***************************************************************************
      associate(melt_temp => param_data%melt_temp%values(1), &
                seg_lateral_inflow => model_streamflow%seg_lateral_inflow, &
                seginc_gwflow => model_streamflow%seginc_gwflow, &
                seginc_sroff => model_streamflow%seginc_sroff, &
                seginc_ssflow => model_streamflow%seginc_ssflow)

        qlat = seg_lateral_inflow(id) * CFS2CMS_CONV
        tl_avg = 0.0

        if (qlat > 0.0_dp) then
          ! Weights do not include water-use if active, not sure it works for cascades
          weight_roff = sngl((seginc_sroff(id) / qlat) * CFS2CMS_CONV)
          weight_ss = sngl((seginc_ssflow(id) / qlat) * CFS2CMS_CONV)
          weight_gw = sngl((seginc_gwflow(id) / qlat) * CFS2CMS_CONV)
        else
          weight_roff = 0.0
          weight_ss = 0.0
          weight_gw = 0.0
        endif

        if (melt > 0.0) then
          melt_wt = melt / (melt + rain)

          if (melt_wt < 0.0) melt_wt = 0.0
          if (melt_wt > 1.0) melt_wt = 1.0

          rain_wt = 1.0 - melt_wt

          if (rain == 0.0) then
            troff = melt_temp
            tss = melt_temp
          else
            troff = melt_temp * melt_wt + tave_air * rain_wt
            tss = melt_temp * melt_wt + tave_ss * rain_wt
          endif
        else
          troff = tave_air
          tss = tave_ss
        endif

        tl_avg = weight_roff * troff + weight_ss * tss + weight_gw * tave_gw
      end associate
    end subroutine


    !***********************************************************************
    module function rprnvg (this, param_data, model_time, hrsr, hrrs, hrss, sino, coso, sin_d, cosod, sinod, seg_id) result(res)
      ! Compute the riparian vegetation shade segment between the
      ! two hour angles hrsr & hrss.
      use prms_constants, only: NEARZERO
      implicit none

      ! Arguments
      real(r32) :: res
      class(StreamTemp), intent(in) :: this
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      real(r32), intent(in) :: hrsr
      real(r32), intent(in) :: hrrs
      real(r32), intent(in) :: hrss
      real(r32), intent(in) :: sino
      real(r32), intent(in) :: coso
      real(r32), intent(in) :: sin_d
      real(r32), intent(in) :: cosod
      real(r32), intent(in) :: sinod
      integer(i32), intent(in):: seg_id

      ! Local Variables
      integer(i32) :: ii

      real(r32) :: als
      real(r32) :: azs
      real(r32) :: bs
      real(r32) :: cosals
      real(r32) :: coshrs
      real(r32) :: delhsr
      real(r32) :: delhss
      real(r32) :: hrs
      real(r32) :: sinals
      real(r32) :: sinhrs
      real(r32) :: svri
      real(r32) :: svsi
      real(r32) :: temp
      real(r32) :: vco

      ! Constants
      integer(i32), parameter :: NBHS = 15
      real(r64), dimension(NBHS), parameter :: EPSLON = [.006003741, .031363304, .075896109, &
                                                         .137791135, .214513914, .302924330, &
                                                         .399402954, .500000000, .600597047, &
                                                         .697075674, .785486087, .862208866, &
                                                         .924103292, .968636696, .993996259]
      real(r64), dimension(NBHS), parameter :: WEIGHT = [.015376621, .035183024, .053579610, &
                                                         .069785339, .083134603, .093080500, &
                                                         .099215743, .101289120, .099215743, &
                                                         .093080500, .083134603, .069785339, &
                                                         .053579610, .035183024, .015376621]

      ! Parameter
      ! azrh, vce, vcw, vdemn, vdemx, vdwmn, vdwmx, vhe, vhw, voe, vow

      ! Time_t
      ! Summer_flag

      !*************************************************************************
      associate(azrh => param_data%azrh%values, &
                vce => param_data%vce%values, &
                vcw => param_data%vcw%values, &
                vdemn => param_data%vdemn%values, &
                vdemx => param_data%vdemx%values, &
                vdwmn => param_data%vdwmn%values, &
                vdwmx => param_data%vdwmx%values, &
                vhe => param_data%vhe%values, &
                vhw => param_data%vhw%values, &
                voe => param_data%voe%values, &
                vow => param_data%vow%values, &
                Summer_flag => model_time%Summer_flag)

        ! Determine seasonal shade

        ! Check for no sunrise
        if (hrsr == hrss) then
          svri = 0.0
          svsi = 0.0
        else
          ! Vegetative shade between sunrise & reach hour angles
          svri = 0.0
          if (hrsr < hrrs) then
            vco = (vce(seg_id) / 2.0) - voe(seg_id)

            ! Determine sunrise side hour angle increment parameters
            delhsr = hrrs - hrsr

            ! Perform numerical integration
            do ii=1, NBHS
              ! Current solar hour angle
              hrs = sngl(hrsr + (EPSLON(ii) * delhsr))
              coshrs = cos(hrs)
              sinhrs = sin(hrs)

              ! Current solar altitude
              temp = sinod + (cosod * coshrs)
              if (temp > 1.0) temp = 1.0

              als = asin(temp)
              cosals = cos(als)
              sinals = sin(als)
              if (sinals == 0.0) sinals = NEARZERO

              ! Current solar azimuth
              temp = ((sino * sinals) - sin_d) / (coso * cosals)
              if (abs(temp) > 1.0) temp = sign(1.0, temp)

              azs = acos(temp)
              if (azs < 0.0) azs = HALF_PI - azs
              if (hrs < 0.0) azs = -azs

              ! Determine amount of stream width shaded
              bs = ((vhe(seg_id) * (cosals/sinals)) * abs(sin(azs - azrh(seg_id)))) + vco
              if (bs < 0.0) bs = 0.0
              if (bs > this%seg_width(seg_id)) bs = this%seg_width(seg_id)

              ! Increment sunrise side vegetative shade
              if (Summer_flag == 1) then
                ! Put back spring and autumn
                svri = svri + sngl(vdemx(seg_id) * bs * sinals * WEIGHT(ii))
              else
                svri = svri + sngl(vdemn(seg_id) * bs * sinals * WEIGHT(ii))
              endif
            enddo

            svri = svri * delhsr
          endif

          ! Vegetative shade between reach & sunset hour angles
          svsi = 0.0
          if (hrss > hrrs) then
            vco = (vcw(seg_id) / 2.0 ) - vow(seg_id)

            ! Determine sunset side hour angle increment parameters
            delhss = hrss - hrrs

            ! Perform numerical integration
            do ii=1, Nbhs
              ! Current solar hour angle
              hrs = sngl(hrrs + (EPSLON(ii) * delhss))
              coshrs = cos(hrs)
              sinhrs = sin(hrs)

              ! Current solar altitude
              temp = sinod + (cosod * coshrs)
              if (temp > 1.0) temp = 1.0

              als = asin(temp)
              cosals = cos(als)

              sinals = sin(als)
              if (sinals == 0.0) sinals = NEARZERO

              ! Current solar azimuth
              temp = ((sino * sinals) - sin_d) / (coso * cosals)
              if (abs(temp) > 1.0) temp = sign(1.0, temp)

              azs = acos(temp)
              if (azs < 0.0) azs = HALF_PI - azs

              if (hrs < 0.0) azs = -azs

              ! Determine amount of stream width shaded
              bs = ((vhw(seg_id) * (cosals/sinals)) * abs(sin(azs - azrh(seg_id)))) + vco
              if (bs < 0.0) bs = 0.0
              if (bs > this%seg_width(seg_id)) bs = this%seg_width(seg_id)

              ! Increment sunset side vegetative shade
              if (Summer_flag == 1) then
                ! Fix for seasons
                svsi = sngl(svsi + (vdwmx(seg_id) * bs * sinals * WEIGHT(ii)))
              else
                svsi = sngl(svsi + (vdwmn(seg_id) * bs * sinals * WEIGHT(ii)))
              endif
            enddo

            svsi = svsi * delhss
          endif
        endif

        ! Combine sunrise/set vegetative shade values
        res = svri + svsi
      end associate
    end function




    ! Daily shade
    module subroutine shday(this, param_data, model_time, seg_id, shade, svi)
      ! This subprogram is to calculate the total daily shade for a given reach.
      ! Both topographic and riparian vegetation shade is included.

      ! VARIABLE NAME LIST
      !      als    = CURRENT SOLAR ALTITUDE
      !      Alrs   = SOLAR ALTITUDE WHEN SOLAR & REACH AZIMUTHS ARE EQUAL
      !      Alsmx  = MAXIMUM POSSIBLE SOLAR ALTITUDE
      !      Alsr   = LOCAL SUNRISE SOLAR ALTITUDE ! rsr, not used
      !      Alss   = LOCAL SUNSET SOLAR ALTITUDE ! rsr, not used
      !      alt    = CURRENT TOPOGRAPHIC ALTITUDE
      !      alte   = EAST SIDE MAXIMUM TOPOGRAPHIC ALTITUDE
      !      Altmx  = CURRENT MAXIMUM TOPOGRAPHIC ALTITUDE LIMIT
      !      Altop  = CURRENT TOPOGRAPHIC ALTITUDE
      !      altw   = WEST SIDE MAXIMUM TOPOGRAPHIC ALTITUDE
      !      azrh   = STREAM REACH AZIMUTH
      !      azs    = CURRENT SOLAR AZIMUTH
      !      Azsr   = LOCAL SUNRISE SOLAR AZIMUTH ! rsr, not used
      !      Azss   = LOCAL SUNSET SOLAR AZIMUTH ! rsr, not used
      !      Azso   = LEVEL-PLAIN SUNSET AZIMUTH
      !      Bavg   = AVERAGE STREAM WIDTH
      !      Bs     = SHADED PART OF STREAM WIDTH
      !      Cosas  = cos(AS)
      !      Cosd   = cos(DECL)
      !      Coshs  = cos(HS)
      !      coso   = cos(XLAT)
      !      cosod  = cos(XLAT)*cos(DECL)
      !      Dayrad = CONVERSION RATIO FOR JULIAN DAYS TO RADIANS
      !      Decl   = CURRENT SOLAR DECLINATION
      !      Delhsr = SUNRISE SIDE HOUR ANGLE INCREMENT
      !      Delhss = SUNSET SIDE HOUR ANGLE INCREMENT
      !      hrrs   = REACH HOUR ANGLE WHEN SOLAR & REACH AZIMUTHS ARE EQUAL
      !      hrs    = CURRENT SOLAR HOUR ANGLE
      !      hrsr   = LOCAL SUNRISE SOLAR HOUR ANGLE
      !      hrss   = LOCAL SUNSET SOLAR HOUR ANGLE
      !      Hrso   = LEVEL-PLAIN SUNRISE/SET SOLAR HOUR ANGLE
      !      Nbhs   = NUMBER OF SUNRISE/SET HOUR ANGLE INCREMENTS
      !      Shday  = TOTAL DAILY SHADE
      !      Sinal  = sin(Al)
      !      Sinar  = sin(Ar)
      !      sin_d   = sin(DECL)
      !      Sinhsr = sin(hrsr)
      !      Sinhss = sin(hrss)
      !      Sinhro = sin(Hrso)
      !      sino   = sin(XLAT)
      !      sinod  = sin(XLAT)*sin(DECL)
      !      Snflag = SOLAR NOON LIMIT FLAG
      !      Sti    = TOPOGRAPHIC SHADE
      !      svi    = RIPARIAN VEGETATION SHADE
      !      Svri   = SUNRISE VEGETATIVE SHADE
      !      Svsi   = SUNSET VEGETATIVE SHADE
      !      Tanasr = TAN(Alsr)
      !      Tanass = TAN(Alss)
      !      Tanalt = TAN(alt)
      !      Tano   = TAN(XLAT)
      !      Tanod  = TAN(XLAT)*TAN(DECL)
      !      Totsh  = LEVEL-PLAIN TOTAL SHADE POTENTIAL
      !      Tolrn  = CONVERGENCE TOLERANCE CRITERIA
      !      Flgrs  = SUNRISE FLAG; TRUE if SUNRISE, FALSE if SUNSET
      !      Flgst  = SUNSET FLAG; TRUE if SUNSET, FALSE if SUNRISE
      !      Vc = CROWN DIAMETER, CURRENT VEGETATION
      !      vce    = CROWN DIAMETER, EAST SIDE VEGETATION
      !      Vco    = CURRENT VEGETATION OVERHANG
      !      vcw    = CROWN DIAMETER, WEST SIDE VEGETATION
      !      Vd     = DENSITY, CURRENT VEGETATION
      !      Vde    = DENSITY, EAST SIDE VEGETATION
      !      Vdw    = DENSITY, WEST SIDE VEGETATION
      !      Vh     = HEIGHT, CURRENT VEGETATION
      !      vhe    = HEIGHT, EAST SIDE VEGETATION
      !      vhw    = HEIGHT, WEST SIDE VEGETATION
      !      Vo     = OFFSET, CURRENT VEGETATION
      !      voe    = OFFSET, EAST SIDE VEGETATION
      !      vow    = OFFSET, WEST SIDE VEGETATION
      use prms_constants, only: CFS2CMS_CONV
      implicit none

      ! Arguments
      class(StreamTemp), intent(inout) :: this
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      integer(i32), intent(in) :: seg_id
      real(r32), intent(out):: shade
      real(r32), intent(out):: svi
        !! riparian vegetation shade

      ! Local variables
      real(r32) :: almn
      real(r32) :: almx
        !! Current maximum topographic altitude limit
      real(r32) :: als
        !! Current solar altitude
      real(r32) :: alsmx
        !! Caximum possible solar altitude
      real(r32) :: altmx
        !! Current maximum topographic altitude limit
      real(r32) :: altop(3)
        !! Current topographic altitude
      real(r32) :: azmn
      real(r32) :: azmx
      real(r32) :: azs
        !! Current solar azimuth
      real(r32) :: azso
        !! Level-plain sunset azimuth
      real(r32) :: aztop(3)
      real(r32) :: coso
        !! cos(xlat)
      real(r32) :: cosod
        !! cos(xlat) * cos(decl)
      real(r32) :: hrrh
      real(r32) :: hrrs
        !! Reach hour angle when solar & reach azimuths are equal
      real(r32) :: hrs
        !! Current solar hour angle
      real(r32) :: hrso
        !! Level-plain sunrise/set solar hour angle
      real(r32) :: hrsr
        !! Local sunrise solar hour angle
      real(r32) :: hrss
        !! Local sunset solar hour angle
      real(r32) :: sin_d
        !! sin(decl)
      real(r32) :: sino
        !! sin(hrso)
      real(r32) :: sinod
        !! sin(xlat) * sin(decl)
      real(r32) :: sti
        !! Yopographic shade
      real(r32) :: temp
      real(r32) :: totsh
        !! Level-plain total shade potential

      real(r32), parameter :: RADTOHOUR = 24.0 / (2.0 * PI)

      ! Parameter
      ! alte, altw, azrh,

      ! Time_t
      ! day_of_year,

      !*************************************************************************
      associate(alte => param_data%alte%values, &
                altw => param_data%altw%values, &
                azrh => param_data%azrh%values, &
                day_of_year => model_time%day_of_year)

        ! Latitude trigonometric parameters
        coso = this%cos_seg_lat(seg_id)
        sino = this%sin_seg_lat(seg_id)
        sin_d = this%sin_declination(day_of_year, seg_id)
        sinod = this%sin_lat_decl(day_of_year, seg_id)
        cosod = this%cos_lat_decl(day_of_year, seg_id)

        ! Initialize local sunrise/set solar parameters
        hrsr = 0.0
        hrss = 0.0

        ! Maximum possible solar altitude
        alsmx = this%max_solar_altitude(day_of_year, seg_id)

        ! Level-plain sunrise/set hour angle
        hrso = this%horizontal_hour_angle(day_of_year, seg_id)

        ! Level-plain solar azimuth
        azso = this%level_sunset_azimuth(day_of_year, seg_id)

        ! Total potential shade on level-plain
        totsh = this%total_shade(day_of_year, seg_id)

        if (azrh(seg_id) <= (-azso)) then
          ! Check for reach azimuth less than sunrise
          hrrs = -hrso
        elseif (azrh(seg_id) >= azso) then
          ! Check for reach azimuth greater than sunset
          hrrs = hrso
        elseif (azrh(seg_id) == 0.0) then
          ! Reach azimuth is between sunrise & sunset
          hrrs = 0.0
        else
          temp = (this%sin_alrs(day_of_year, seg_id) - sinod) / cosod

          if (abs(temp) > 1.0) temp = sign(1.0, temp)

          hrrs = sign(acos(temp), azrh(seg_id))
        endif

        if ((alte(seg_id) == 0.0 ) .and. (altw(seg_id) == 0.0)) then
          ! Check if level-plain
          ! azsr = -azso
          hrsr = -hrso
          ! azss = azso
          hrss = hrso
          sti = 0.0
          svi = (this%rprnvg(param_data, model_time, hrsr, hrrs, hrss, sino, coso, sin_d, cosod, sinod, seg_id)) / (this%seg_width(seg_id) * totsh)
        else
          ! Initialize shade values
          ! Insert starting topographic azimuth values between level plain sunrise and sunset
          aztop = 0.0

          ! Determine sunrise hour angle.
          altop = 0.0
          if (-azso <= azrh(seg_id)) then
            altop(1) = alte(seg_id)
            aztop(1) = azso * (alte(seg_id) / HALF_PI) - azso
          else
            altop(1) = altw(seg_id)
            aztop(1) = azso * (altw(seg_id) / HALF_PI) - azso
          endif

          if (altop(1) == 0.0) then
            ! Level plain
            hrsr = -hrso
          else
            ! NOT
            ! Look for solution between limits of level plain sunrise and noon
            azmn = -azso
            azmx = 0.0
            azs = aztop(1)
            altmx = altop(1)
            almn = 0.0
            almx = 1.5708
            als = this%solalt(param_data, coso, sino, sin_d, azs, almn, almx)

            call this%snr_sst(param_data, coso, sino, sin_d, altmx, almn, almx, azmn, azmx, azs, als, hrs, seg_id)
            ! azsr = azs
            ! alsr = als
            hrsr = hrs
            ! altr = altmx
          endif

          ! Determine sunset hour angle.
          if (azso <= azrh(seg_id))then
            altop(2) = alte(seg_id)
            aztop(2) = azso - azso * (alte(seg_id) / HALF_PI)
          else
            altop(2) = altw(seg_id)
            aztop(2) = azso - azso * (altw(seg_id) / HALF_PI)
          endif

          if (altop(2) == 0.0) then
            ! Level plain
            hrss = hrso
          else
            ! NOT
            ! Look for solution between limits of noon and level plain sunset
            azmn = 0.0
            azmx = azso
            azs = aztop(2)
            altmx = altop(2)
            almn = 0.0
            almx = 1.5708
            als = this%solalt(param_data, coso, sino, sin_d, azs, almn, almx)

            call this%snr_sst(param_data, coso, sino, sin_d, altmx, almn, almx, azmn, azmx, azs, als, hrs, seg_id)
            ! azss = azs
            ! alss = als
            hrss = hrs
            ! alts = altmx
          endif

          ! Solve for shade increments this segment
          if (hrrs < hrsr) then
            hrrh = hrsr
          elseif (hrrs > hrss) then
            hrrh = hrss
          else
            hrrh = hrrs
          endif

          this%seg_daylight(seg_id) = (hrss - hrsr) * RADTOHOUR
          sti = 1.0 - ((((hrss - hrsr) * sinod) + ((sin(hrss) - sin(hrsr)) * cosod)) / (totsh))
          svi = ((this%rprnvg(param_data, model_time, hrsr, hrrh, hrss, sino, coso, sin_d, cosod, sinod, seg_id)) / (this%seg_width(seg_id) * totsh))

          ! End sunrise/sunset calculation
        endif

        ! Check for roundoff errors
        if (sti < 0.0) sti = 0.0
        if (sti > 1.0) sti = 1.0
        if (svi < 0.0) svi = 0.0
        if (svi > 1.0) svi = 1.0

        ! Record total shade
        shade = sti + svi
      end associate
    end subroutine


    !**********************************************************************************************************
    !    "snr_sst"
    module subroutine snr_sst (this, param_data, coso, sino, sin_d, alt, almn, almx, &
                               azmn, azmx, azs, als, hrs, seg_id)
      ! Determines the local solar sunrise/set azimuth, altitude, and hour angle
      use prms_constants, only: NEARZERO
      implicit none

      ! Arguments
      class(StreamTemp), intent(in) :: this
      type(Parameters), intent(in) :: param_data
      real(r32), intent(in) :: coso
      real(r32), intent(in) :: sino
      real(r32), intent(in) :: sin_d
      real(r32), intent(in) :: alt
      real(r32), intent(in) :: almn
      real(r32), intent(in) :: almx
      real(r32), intent(in) :: azmn
      real(r32), intent(in) :: azmx
      real(r32), intent(inout) :: azs
      real(r32), intent(inout) :: als
      real(r32), intent(out) :: hrs
      integer(i32), intent(in) :: seg_id

      ! Local Variables
      integer(i32) :: count

      real(r32) :: cosals
      real(r32) :: cosazr
      real(r32) :: cosazs
      real(r32) :: delals
      real(r32) :: delazs
      real(r32) :: f
      real(r32) :: fals
      real(r32) :: fazs
      real(r32) :: g
      real(r32) :: gals
      real(r32) :: gazs
      real(r32) :: sinals
      real(r32) :: sinazr
      real(r32) :: sinazs
      real(r32) :: tanals
      real(r32) :: tanalt
      real(r32) :: tano
      real(r32) :: temp
      real(r32) :: xjacob

      ! Parameter
      ! azrh, maxiter_sntemp

      ! ************************************************************************
      associate(azrh => param_data%azrh%values, &
                maxiter_sntemp => param_data%maxiter_sntemp%values(1))

        ! Trig function for local altitude
        tanalt = TAN(alt)
        tano = sino / coso
        f = 999999.0  ! rsr, these need values
        delazs = 9999999.0
        g = 99999999.0
        delals = 99999999.0

        ! Begin newton-raphson solution
        do count=1, maxiter_sntemp
          if (abs(delazs) < NEARZERO) exit
          if (abs(delals) < NEARZERO) exit
          if (abs(f) < NEARZERO) exit
          if (abs(g) < NEARZERO) exit

          cosazs = cos(azs)
          sinazs = sin(azs)

          sinazr = abs(sin(azs - azrh(seg_id)))
          if ((((azs - azrh(seg_id)) <= 0.0) .and. ((azs - azrh(seg_id)) <= -PI)) .or. &
              (((azs - azrh(seg_id)) > 0.0 ) .and. ((azs - azrh(seg_id)) <= PI))) then
            cosazr = cos(azs - azrh(seg_id))
          else
            cosazr = -cos(azs - azrh(seg_id))
          endif

          cosals = cos(als)
          if (cosals < NEARZERO) cosals = NEARZERO

          sinals = sin(als)
          tanals = sinals / cosals

          ! Functions of azs & als
          f = cosazs- (((sino * sinals) - sin_d) / (coso * cosals))
          g = tanals - (tanalt * sinazr)

          ! First partials derivatives of f & g
          fazs = -sinazs
          fals = ((tanals * (sin_d / coso)) - (tano / cosals)) / cosals
          gazs = -tanalt * cosazr
          gals = 1.0 / (cosals * cosals)

          ! Jacobian
          xjacob = (fals * gazs) - (fazs * gals)

          ! Delta corrections
          delazs = ((f * gals) - (g * fals)) / xjacob
          delals = ((g * fazs) - (f * gazs)) / xjacob

          ! New values of azs & als
          azs = azs + delazs
          als = als + delals

          ! Check for limits
          if (azs < (azmn + NEARZERO)) azs = (azmn + NEARZERO)
          if (azs > (azmx - NEARZERO)) azs = (azmx - NEARZERO)
          if (als < (almn + NEARZERO)) als = (almn + NEARZERO)
          if (als > (almx - NEARZERO)) als = (almx - NEARZERO)
        enddo

        ! Ensure azimuth remains between -PI & PI
        if (azs < (-PI)) then
          azs = azs + PI
        elseif (azs > PI) then
          azs = azs - PI
        endif

        ! Determine local sunrise/set hour angle
        sinals = sin(als)
        temp = (sinals - (sino * sin_d)) / (coso * cos(asin(sin_d)))
        if (abs(temp) > 1.0) temp = sign(1.0, temp)

        hrs = sign(acos(temp), azs)
      end associate
    end subroutine


    ! **************************************************************************
    ! solalt
    module function solalt(this, param_data, coso, sino, sin_d, az, almn, almx) result(res)
      ! This subprogram is used to determine the solar altitude when the
      ! trigonometric parameters for latitude, declination, and azimuth
      ! are given.
      use prms_constants, only: NEARZERO
      implicit none

      ! Arguments
      real(r32) :: res
      class(StreamTemp), intent(in) :: this
      type(Parameters), intent(in) :: param_data
      real(r32), intent(in):: coso
        !! cos(xlat)
      real(r32), intent(in):: sino
        !! sin(xlat)
      real(r32), intent(in):: sin_d
        !! sin(decl)
      real(r32), intent(in):: az
        !! Solar azimuth
      real(r32), intent(in):: almn
      real(r32), intent(in):: almx

      ! Local Variables
      real(r32) :: a
      real(r32) :: al
        !! Trial solar altitude
      real(r32) :: alold
      real(r32) :: b
      real(r32) :: cosal
        !! cos(al)
      real(r32) :: cosaz
        !! cos(az)
      real(r32) :: delal
        !! Incremental correction to al
      real(r32) :: fal
        !! Function of al
      real(r32) :: fpal
        !! First derivative of fal
      real(r32) :: fppal
        !! Second derivative of fal
      real(r32) :: sinal
      real(r32) :: temp

      integer(i32) :: ii

      ! Parameter
      ! maxiter_sntemp,

      ! ************************************************************************
      associate(maxiter_sntemp => param_data%maxiter_sntemp%values(1))

        ! Check cos(az) equal to 0
        if (abs(abs(az) - HALF_PI) < NEARZERO) then
          temp = abs(sin_d / sino)

          if (temp > 1.0) temp = 1.0

          al = asin(temp)
        else
          ! Determine solar altitude function coefficients
          cosaz = cos(az)
          a = sino / (cosaz * coso)
          b = sin_d / (cosaz * coso)

          ! Initialize
          al = (almn + almx) / 2.0
          ii = 0
          fal = cos(al) - (a * sin(al)) + b
          delal = fal / (-sin(al) - (a * cos(al)))

          ! Begin newton second-order solution
          do ii=1, maxiter_sntemp
            if (abs(fal) < NEARZERO) exit
            if (abs(delal) < NEARZERO) exit

            alold = al
            cosal = cos(al)
            sinal = sin(al)
            fal =  cosal - (a * sinal) + b
            fpal = -sinal - (a * cosal)

            if (ii <= 3) then
              delal = fal / fpal
            else
              fppal = b - fal
              delal = (2.0 * fal * fpal) / ((2.0 * fpal * fpal) - (fal * fppal))
            endif

            al = al - delal

            if (al < almn) al = (alold + almn) / 2.0
            if (al > almx) al = (alold + almx) / 2.0
          enddo
        endif

        ! Solution obtained
        res = al
      end associate
    end function


    !**********************************************************************************
    !    "teak1"
    !**********************************************************************************
    module subroutine teak1(this, param_data, A, B, C, D, Teq, Ak1c)
      ! PURPOSE:
      !   1. To determine the equilibrium water temperature from the energy balance
      !      equation by iterating newton's method.
      !   2. To determine the 1st thermal exchange coefficient.
      implicit none

      ! Arguments
      class(StreamTemp), intent(in) :: this
      type(Parameters), intent(in) :: param_data
      real(r32), intent(in) :: A
      real(r32), intent(in) :: B
      real(r32), intent(in) :: C
      real(r32), intent(in) :: D
      real(r32), intent(inout) :: Teq
      real(r32), intent(out) :: Ak1c

      ! Local variables
      real(r32) :: teabs
      real(r32) :: fte
      real(r32) :: fpte
      real(r32) :: delte

      integer(i32) :: ii

      ! Solution convergence tolerance
      real(r32), parameter :: TOLRN = 1.0E-4

      ! Parameter
      ! maxiter_sntemp

      !**********************************************************************************
      associate(maxiter_sntemp => param_data%maxiter_sntemp%values(1))

        fte = 99999.0 ! rsr, fte was not set
        delte = 99999.0 ! rsr, delte was not set
        ii = 0

        ! Begin newton iteration solution for te
        do ii=1, maxiter_sntemp
           if (abs(fte) < TOLRN) exit
           if (abs(delte) < TOLRN) exit

           teabs = Teq + ZERO_C
           fte = (A * (teabs**4.0)) + (B * Teq) - (C * (Teq**2.0)) - D
           fpte = (4.0 * A * (teabs**3.0)) + B - (2.0 * C * Teq)
           delte = fte / fpte
           Teq = Teq - delte
        enddo

        ! Determine 1st thermal exchange coefficient
        Ak1c = (4.0 * A * ((Teq + ZERO_C)**3.0)) + B - (2.0 * C * Teq)
      end associate
    end subroutine



    !***********************************************************************************************
    module function twavg(qup, t0, qlat, tl_avg, te, ak1, ak2, width, length) result(res)
      ! PURPOSE:
      !   1. TO PREDICT THE AVERAGE DAILY WATER TEMPERATURE USING A SECOND-ORDER
      !      CLOSED-FORM SOLUTION TO THE STEADY-STATE HEAT TRANSPORT EQUATION.
      use prms_constants, only: CFS2CMS_CONV, NEARZERO
      implicit none

      ! Arguments
      real(r32) :: res
      real(r32), intent(in) :: qup
      real(r32), intent(in) :: t0
      real(r64), intent(in) :: qlat
      real(r32), intent(in) :: tl_avg
      real(r32), intent(in) :: te
      real(r32), intent(in) :: ak1
      real(r32), intent(in) :: ak2
      real(r32), intent(in) :: width
      real(r32), intent(in) :: length

      ! Local Variables
      real(r32) :: b
      real(r32) :: delt
      real(r32) :: denom
      real(r32) :: q_init
      real(r32) :: ql
      real(r32) :: r
      real(r32) :: rexp
      real(r32) :: tep
      real(r32) :: tw

      !***************************************************************************************************
      ! Determine equation parameters
      q_init = sngl(qup  * CFS2CMS_CONV)
      ql = sngl(qlat)

      ! This is confused logic coment out here and compute the terms as needed below
      !   b = (ql / Seg_length) + ((ak1 * this%seg_width) / 4182.0E03)
      !   if ( b < NEARZERO ) b = NEARZERO ! rsr, don't know what value this should be to avoid divide by 0
      !   r = 1.0 + (ql / q_init)
      !   if ( r < NEARZERO ) r = NEARZERO

      if (ql <= NEARZERO) then
        ! Zero lateral flow
        tep = te
        b = (ak1 * width) / 4182.0E03
        rexp = -1.0 * (b * length) / q_init
        r = exp(rexp)
      elseif (ql < 0.0) then
        ! Losing stream
        ! No such thing as losing streams in PRMS
        write(*,*) "twavg: losing stream!!! Should be no such thing in PRMS!"
        tep  = te
        b = (ql / length) + ((ak1 * width) / 4182.0E03)
        rexp = (ql - (b * length)) / ql
        r = 1.0 + (ql / q_init)
        r = r**rexp
      elseif (ql > NEARZERO .and. q_init <= NEARZERO) then
        ! This is a headwaters (i.e. no streamflow from above, but lateral flow from HRUs.
        ! Treat the lateral flow as upstream flow to avoid divide by zero.
        tep = te
        b = (ak1 * width) / 4182.0E03
        ! rexp = -1.0*(b * length) / q_init
        rexp = -1.0 * (b * length) / ql
        r = exp(rexp)
      else
        ! Gaining stream (ie both ql and q_init have > zero values)
        b = (ql / length) + ((ak1 * width) / 4182.0E03)
        tep = (((ql / length) * tl_avg) + (((ak1 * width) / 4182.0E03) * te)) / b

        ! Shouldn't need to do this because ql will always be greater than 0 if in here.
        if (ql > 0.0) then
          rexp = -b / (ql / length)
        else
          rexp = 0.0
        endif

        ! DANGER -- replaced this potential divide by zero with the logic below
        !          r = 1.0 + (ql / q_init)
        if (q_init < NEARZERO) then
          r = 2.0
        else
          r = 1.0 + (ql / q_init)
        endif

        r = r**rexp
        ! End lateral flow term logic
      endif

      ! Determine water temperature
      delt  = tep - t0
      denom = (1.0 + (ak2 / ak1) * delt * (1.0 - r))

      if (abs(denom) < NEARZERO) denom = sign(NEARZERO, denom)
      tw = tep - (delt * r / denom)

      if (tw < 0.0) tw = 0.0
      res = tw
    end function


end submodule
