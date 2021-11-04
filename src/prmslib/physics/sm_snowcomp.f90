submodule (PRMS_SNOW) sm_snowcomp


contains
  ! Snowcomp constructor
  module subroutine init_Snowcomp(this, ctl_data, model_basin, model_climate, model_summary)
    use prms_constants, only: dp
    implicit none

    class(Snowcomp), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(inout) :: model_climate
    type(Summary), intent(inout) :: model_summary

    ! Local Variables
    integer(i32) :: chru
    ! integer(i32) :: idx1D
    integer(i32) :: j
    integer(i32) :: jj
    integer(i32) :: sd

    ! real(r32), allocatable :: snarea_curve_2d(:, :)
    ! real(r32), pointer, contiguous :: snarea_curve_2d(:, :)

    ! -------------------------------------------------------------------------
    associate(init_vars_from_file => ctl_data%init_vars_from_file%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &
              save_vars_to_file => ctl_data%save_vars_to_file%value, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths, &
              active_hrus => model_basin%active_hrus, &
              active_mask => model_basin%active_mask, &
              hru_route_order => model_basin%hru_route_order, &

              pkwater_equiv => model_climate%pkwater_equiv)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Dimensions
      this%ndeplval = param_hdl%get_dimension('ndeplval')

      ! Parameters
      call param_hdl%get_variable('albset_rna', this%albset_rna)
      call param_hdl%get_variable('albset_rnm', this%albset_rnm)
      call param_hdl%get_variable('albset_sna', this%albset_sna)
      call param_hdl%get_variable('albset_snm', this%albset_snm)
      call param_hdl%get_variable('den_init', this%den_init)
      call param_hdl%get_variable('den_max', this%den_max)
      call param_hdl%get_variable('settle_const', this%settle_const)

      allocate(this%emis_noppt(nhru))
      call param_hdl%get_variable('emis_noppt', this%emis_noppt)

      allocate(this%freeh2o_cap(nhru))
      call param_hdl%get_variable('freeh2o_cap', this%freeh2o_cap)

      allocate(this%hru_deplcrv(nhru))
      call param_hdl%get_variable('hru_deplcrv', this%hru_deplcrv)

      allocate(this%melt_force(nhru))
      call param_hdl%get_variable('melt_force', this%melt_force)

      allocate(this%melt_look(nhru))
      call param_hdl%get_variable('melt_look', this%melt_look)

      allocate(this%rad_trncf(nhru))
      call param_hdl%get_variable('rad_trncf', this%rad_trncf)

      allocate(this%snarea_curve(this%ndeplval))
      call param_hdl%get_variable('snarea_curve', this%snarea_curve)

      allocate(this%snarea_thresh(nhru))
      call param_hdl%get_variable('snarea_thresh', this%snarea_thresh)

      if (any([0, 2, 3] == init_vars_from_file)) then
        ! This isn't needed when snowcomp is initialized from a restart file
        allocate(this%snowpack_init(nhru))
        call param_hdl%get_variable('snowpack_init', this%snowpack_init)
      end if

      allocate(this%cecn_coef(nhru, nmonths))
      call param_hdl%get_variable('cecn_coef', this%cecn_coef)

      allocate(this%tstorm_mo(nhru, nmonths))
      call param_hdl%get_variable('tstorm_mo', this%tstorm_mo)


      ! TODO: Allocated internal arrays/variables
      allocate(this%ai(nhru))
      allocate(this%albedo(nhru))
      allocate(this%frac_swe(nhru))
      allocate(this%freeh2o(nhru))
      allocate(this%iasw(nhru))
      allocate(this%int_alb(nhru))
      allocate(this%iso(nhru))
      allocate(this%lso(nhru))
      allocate(this%lst(nhru))
      allocate(this%mso(nhru))
      allocate(this%pk_def(nhru))
      allocate(this%pk_den(nhru))
      allocate(this%pk_depth(nhru))
      allocate(this%pk_ice(nhru))
      allocate(this%pk_precip(nhru))
      allocate(this%pk_temp(nhru))
      allocate(this%pksv(nhru))
      allocate(this%pkwater_ante(nhru))
      allocate(this%pptmix_nopack(nhru))
      allocate(this%pss(nhru))
      allocate(this%pst(nhru))
      allocate(this%salb(nhru))
      allocate(this%scrv(nhru))
      allocate(this%slst(nhru))
      allocate(this%snow_evap(nhru))
      allocate(this%snowcov_area(nhru))
      allocate(this%snowcov_areasv(nhru))
      allocate(this%snowmelt(nhru))
      allocate(this%snsv(nhru))
      allocate(this%tcal(nhru))

      ! NOTE: 2019-10-31 PAN: moved from precipitation
      allocate(this%newsnow(nhru))
      allocate(this%pptmix(nhru))
      this%pptmix = 0
      this%newsnow = 0

      ! FIXME: This may not get the correct 2D index because of how the
      !        snarea_curve is stored.
      ! NOTE: pan - this does appear to work.
      ! snarea_curve_2d => get_array(param_data%snarea_curve%values, (/11, nhru/))

      this%ai = 0.0_dp
      this%frac_swe = 0.0
      this%freeh2o = 0.0
      this%pk_den = 0.0
      this%pk_depth = 0.0_dp
      this%pk_ice = 0.0
      this%pk_precip = 0.0
      this%pptmix_nopack = .false.
      this%snow_evap = 0.0
      this%snowcov_area = 0.0
      this%snowmelt = 0.0
      this%tcal = 0.0

      this%acum = ACUM_INIT
      this%amlt = AMLT_INIT

      ! NOTE: If parameters settle_const, den_init and/or den_max aren't in the
      !       parameter file this will crash here.
      this%deninv = 1.0_dp / dble(this%den_init)
      this%denmaxinv = 1.0_dp / dble(this%den_max)

      sd = this%ndeplval / 11
      allocate(this%snarea_curve_2d(11, sd))
      this%snarea_curve_2d = reshape(this%snarea_curve, (/11, sd/))

      ! this%settle_const_dble = dble(settle_const)

      ! TODO: Hookup the read from restart file code
      ! if ( Init_vars_from_file>0 ) call snowcomp_restart(1)

      if (any([0, 2, 3] == init_vars_from_file)) then
        this%albedo = 0.0
        this%iasw = .false.
        this%int_alb = 1
        this%iso = 1
        this%lso = 0
        this%lst = .false.
        this%mso = 1
        this%pk_def = 0.0
        this%pk_temp = 0.0
        this%pksv = 0.0_dp
        this%salb = 0.0
        this%scrv = 0.0_dp
        this%slst = 0.0
        this%snowcov_areasv = 0.0
        this%snsv = 0.0

        pkwater_equiv = dble(this%snowpack_init)

        where (pkwater_equiv > 0.0_dp)
          this%pk_depth = pkwater_equiv * this%deninv
          this%pk_den = sngl(pkwater_equiv / this%pk_depth)
          this%pk_ice = sngl(pkwater_equiv)
          this%freeh2o = this%pk_ice * this%freeh2o_cap
          this%ai = pkwater_equiv ! [inches]

          where (this%ai > this%snarea_thresh)
            this%ai = dble(this%snarea_thresh)
          end where

          this%frac_swe = sngl(pkwater_equiv / this%ai)
        end where

        do concurrent (j = 1:active_hrus)
          chru = hru_route_order(j)
          if (pkwater_equiv(chru) > 0.0_dp) then
            this%snowcov_area(chru) = this%sca_deplcrv(this%snarea_curve_2d(1:11, this%hru_deplcrv(chru)), &
                                                       this%frac_swe(chru))
          end if
        end do

        ! do j=1, active_hrus
        !   chru = hru_route_order(j)
        !   ! pkwater_equiv(chru) = dble(this%snowpack_init(chru))

        !   if (pkwater_equiv(chru) > 0.0_dp) then
        !     this%pk_depth(chru) = pkwater_equiv(chru) * this%deninv
        !     this%pk_den(chru) = sngl(pkwater_equiv(chru) / this%pk_depth(chru))
        !     this%pk_ice(chru) = sngl(pkwater_equiv(chru))
        !     this%freeh2o(chru) = this%pk_ice(chru) * this%freeh2o_cap(chru)
        !     this%ai(chru) = pkwater_equiv(chru) ! [inches]

        !     if (this%ai(chru) > this%snarea_thresh(chru)) then
        !       this%ai(chru) = dble(this%snarea_thresh(chru)) ! [inches]
        !     endif

        !     this%frac_swe(chru) = sngl(pkwater_equiv(chru) / this%ai(chru)) ! [fraction]

        !     this%snowcov_area(chru) = this%sca_deplcrv(this%snarea_curve_2d(1:11, this%hru_deplcrv(chru)), this%frac_swe(chru))
        !   endif
        ! enddo

        ! NOTE: can deallocate parameter snowpack_init at this point
        deallocate(this%snowpack_init)

        this%pkwater_ante = pkwater_equiv
        this%pss = pkwater_equiv
        this%pst = pkwater_equiv
      else
        ! ~~~~~~~~~~~~~~~~~~~~~~~~
        ! Initialize from restart
        call ctl_data%read_restart_variable('albedo', this%albedo)
        call ctl_data%read_restart_variable('freeh2o', this%freeh2o)
        call ctl_data%read_restart_variable('iasw', this%iasw)
        call ctl_data%read_restart_variable('int_alb', this%int_alb)
        call ctl_data%read_restart_variable('iso', this%iso)
        call ctl_data%read_restart_variable('lso', this%lso)
        call ctl_data%read_restart_variable('lst', this%lst)
        call ctl_data%read_restart_variable('mso', this%mso)
        call ctl_data%read_restart_variable('pk_def', this%pk_def)
        call ctl_data%read_restart_variable('pk_depth', this%pk_depth)
        call ctl_data%read_restart_variable('pk_den', this%pk_den)
        call ctl_data%read_restart_variable('pk_ice', this%pk_ice)
        call ctl_data%read_restart_variable('pk_temp', this%pk_temp)
        call ctl_data%read_restart_variable('pksv', this%pksv)
        call ctl_data%read_restart_variable('pss', this%pss)
        call ctl_data%read_restart_variable('pst', this%pst)
        call ctl_data%read_restart_variable('salb', this%salb)
        call ctl_data%read_restart_variable('scrv', this%scrv)
        call ctl_data%read_restart_variable('slst', this%slst)
        call ctl_data%read_restart_variable('snowcov_area', this%snowcov_area)
        call ctl_data%read_restart_variable('snowcov_areasv', this%snowcov_areasv)
        call ctl_data%read_restart_variable('snsv', this%snsv)
      endif

      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          ! TODO: This is where the daily basin values are linked based on
          !       what was requested in basinOutVar_names.
          select case(outVar_names%values(jj)%s)
            case('ai')
              call model_summary%set_summary_var(jj, this%ai)
            case('albedo')
              call model_summary%set_summary_var(jj, this%albedo)
            case('frac_swe')
              call model_summary%set_summary_var(jj, this%frac_swe)
            case('freeh2o')
              call model_summary%set_summary_var(jj, this%freeh2o)
            case('pk_def')
              call model_summary%set_summary_var(jj, this%pk_def)
            case('pk_depth')
              call model_summary%set_summary_var(jj, this%pk_depth)
            case('pk_ice')
              call model_summary%set_summary_var(jj, this%pk_ice)
            case('pk_precip')
              call model_summary%set_summary_var(jj, this%pk_precip)
            case('pk_temp')
              call model_summary%set_summary_var(jj, this%pk_temp)
            case('pst')
              call model_summary%set_summary_var(jj, this%pst)
            case('snow_evap')
              call model_summary%set_summary_var(jj, this%snow_evap)
            case('snowcov_area')
              call model_summary%set_summary_var(jj, this%snowcov_area)
            case('snowmelt')
              call model_summary%set_summary_var(jj, this%snowmelt)
            case('tcal')
              call model_summary%set_summary_var(jj, this%tcal)
            case('pptmix')    ! from precipitation
              call model_summary%set_summary_var(jj, this%pptmix)
            case('newsnow')   ! from precipitation
              call model_summary%set_summary_var(jj, this%newsnow)
            case default
              ! pass
          end select
        enddo
      endif

      if (save_vars_to_file == 1) then
        ! Create restart variables
        ! call ctl_data%add_variable('deninv', this%et_type, 'nhru', 'none')
        ! call ctl_data%add_variable('denmaxinv', this%gravity_stor_res, 'nhru', 'none')
        call ctl_data%add_variable('albedo', this%albedo, 'nhru', 'decimal fraction')
        call ctl_data%add_variable('freeh2o', this%freeh2o, 'nhru', 'inches')
        call ctl_data%add_variable('iasw', this%iasw, 'nhru', 'none')
        call ctl_data%add_variable('int_alb', this%int_alb, 'nhru', 'none')
        call ctl_data%add_variable('iso', this%iso, 'nhru', 'none')
        call ctl_data%add_variable('lso', this%lso, 'nhru', 'number of iterations')
        call ctl_data%add_variable('lst', this%lst, 'nhru', 'none')
        call ctl_data%add_variable('mso', this%mso, 'nhru', 'none')
        call ctl_data%add_variable('pk_def', this%pk_def, 'nhru', 'Langleys')
        call ctl_data%add_variable('pk_den', this%pk_den, 'nhru', 'gm/cm3')
        call ctl_data%add_variable('pk_depth', this%pk_depth, 'nhru', 'inches')
        call ctl_data%add_variable('pk_ice', this%pk_ice, 'nhru', 'inches')
        call ctl_data%add_variable('pk_temp', this%pk_temp, 'nhru', 'degreeC')
        call ctl_data%add_variable('pksv', this%pksv, 'nhru', 'inches')
        ! call ctl_data%add_variable('pkwater_ante', this%pkwater_ante, 'nhru', 'inches')
        call ctl_data%add_variable('pss', this%pss, 'nhru', 'inches')
        call ctl_data%add_variable('pst', this%pst, 'nhru', 'inches')
        call ctl_data%add_variable('salb', this%salb, 'nhru', 'days')
        call ctl_data%add_variable('scrv', this%scrv, 'nhru', '')
        call ctl_data%add_variable('slst', this%slst, 'nhru', 'days')
        call ctl_data%add_variable('snowcov_area', this%snowcov_area, 'nhru', 'decimal fraction')
        call ctl_data%add_variable('snowcov_areasv', this%snowcov_areasv, 'nhru', 'decimal fraction')
        call ctl_data%add_variable('snsv', this%snsv, 'nhru', 'inches')
      end if
    end associate
  end subroutine


  !***********************************************************************
  ! Run daily snow estimates
  !***********************************************************************
  module subroutine run_Snowcomp(this, ctl_data, model_basin, model_time, &
                                 model_climate, model_precip, model_temp, &
                                 intcp, model_solrad, model_potet, model_transp)
    use prms_constants, only: dp, LAKE
    use UTILS_PRMS, only: get_array
    implicit none

    class(Snowcomp), intent(inout) :: this
      !! Snowcomp class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Basin), intent(in) :: model_basin
      !! Basin
    type(Time_t), intent(in) :: model_time
      !! Time
    type(Climateflow), intent(inout) :: model_climate
      !! Climate
    class(Precipitation), intent(in) :: model_precip
    class(Temperature), intent(in) :: model_temp
    type(Interception), intent(in) :: intcp
      !! Canopy interception
    class(SolarRadiation), intent(in) :: model_solrad
    class(Potential_ET), intent(in) :: model_potet
    class(Transpiration), intent(in) :: model_transp

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: idx1D
    integer(i32) :: j
    integer(i32) :: niteda

    real(r32) :: cals
    real(r32) :: cec
    real(r32) :: cst
    real(r32) :: effk
    real(r32) :: emis
    real(r32) :: esv
    real(r32) :: sw
    real(r32) :: swn
    real(r32) :: trd

    real(r64) :: dpt1
    real(r64) :: dpt_before_settle
    real(r64) :: temp

    !***********************************************************************
    associate(print_debug => ctl_data%print_debug%value, &

              curr_month => model_time%Nowmonth, &
              day_of_year => model_time%day_of_year, &
              day_of_water_year => model_time%day_of_water_year, &

              nhru => model_basin%nhru, &
              nmonths => model_basin%nmonths, &
              active_hrus => model_basin%active_hrus, &
              active_mask => model_basin%active_mask, &
              cov_type => model_basin%cov_type, &
              hru_type => model_basin%hru_type, &
              hru_route_order => model_basin%hru_route_order, &

              orad_hru => model_solrad%orad_hru, &
              soltab_horad_potsw => model_solrad%soltab_horad_potsw, &
              swrad => model_solrad%swrad, &

              pkwater_equiv => model_climate%pkwater_equiv, &

              hru_ppt => model_precip%hru_ppt, &
              prmx => model_precip%prmx, &

              tavg => model_temp%tavg, &
              tmax => model_temp%tmax, &
              tmin => model_temp%tmin, &

              transp_on => model_transp%transp_on, &

              net_ppt => intcp%net_ppt, &
              net_rain => intcp%net_rain, &
              net_snow => intcp%net_snow)

      cals = 0.0  ! initialize

      ! Calculate the ratio of measured radiation to potential radiation
      ! (used as a cumulative indicator of cloud cover)
      ! trd = orad / sngl(basin_horad)  ! [dimensionless ratio]

      ! By default, the precipitation added to snowpack, snowmelt,
      ! and snow evaporation are 0.
      ! this%pk_precip = 0.0  ! [inches]
      ! this%snowmelt = 0.0  ! [inches]
      ! this%snow_evap = 0.0  ! [inches]
      ! this%frac_swe = 0.0
      ! this%ai = 0.0_dp
      ! this%tcal = 0.0

      this%newsnow = 0
      this%pptmix = 0

      where (net_snow > 0.0)
        this%newsnow = 1
      end where

      where (net_rain > 0.0 .and. net_snow > 0.0)
        this%pptmix = 1
      end where

      ! By default, there has not been a mixed event without a snowpack
      this%pptmix_nopack = .false.  ! [flag]

      ! Keep track of the pack water equivalent before it is changed
      ! by precipitation during this time step.
      ! this%pkwater_ante = pkwater_equiv

      ! Loop through all the active HRUs, in routing order
      do j=1, active_hrus
        chru = hru_route_order(j)

        ! Skip the HRU if it is a lake
        if (hru_type(chru) == LAKE) cycle

        trd = orad_hru(chru) / soltab_horad_potsw(day_of_year, chru)
        ! NOTE: 2021-08-27 PAN - below is a change suggested by Steve Regan
        ! orad_local = SNGL( (DBLE(Swrad(i))*Hru_cossl(i)*Soltab_horad_potsw(Jday,i))/Soltab_potsw(Jday,i) )
        ! trd = orad_local/SNGL(Soltab_horad_potsw(Jday,i)) ! [dimensionless ratio]


        ! 2D index to 1D
        idx1D = (curr_month - 1) * nhru + chru

        ! If it's the first julian day of the water year, several
        ! variables need to be reset:
        ! - reset the previous snow water eqivalent plus new snow to 0
        ! - reset flags to indicate it is not melt season or potetential melt season
        ! - reset the counter for the number of days a snowpack is at 0 deg Celsius
        ! TODO: rsr, do we want to reset all HRUs, what about Southern Hemisphere
        if (day_of_water_year == 1) then
          this%pss(chru) = 0.0_dp  ! [inches]
          this%iso(chru) = 1  ! [flag]
          this%mso(chru) = 1  ! [flag]
          this%lso(chru) = 0  ! [counter]
        endif

        ! HRU SET-UP - SET DEFAULT VALUES AND/OR BASE CONDITIONS FOR THIS TIME PERIOD
        !**************************************************************
        ! Keep track of the pack water equivalent before it is changed
        ! by precipitation during this time step.
        ! TODO: PAN: Why can't this be set outside of the loop?
        this%pkwater_ante(chru) = pkwater_equiv(chru)

        ! By default, the precipitation added to snowpack, snowmelt,
        ! and snow evaporation are 0.
        this%pk_precip(chru) = 0.0  ! [inches]
        this%snowmelt(chru) = 0.0  ! [inches]
        this%snow_evap(chru) = 0.0  ! [inches]
        this%frac_swe(chru) = 0.0
        this%ai(chru) = 0.0_dp
        this%tcal(chru) = 0.0

        ! If the day of the water year is beyond the forced melt day indicated by
        ! the parameter, then set the flag indicating melt season
        ! TODO: rsr, need to rethink this at some point
        if (day_of_year == this%melt_force(chru)) then
          this%iso(chru) = 2  ! [flag]
        endif

        ! If the day of the water year is beyond the first day to look for melt
        ! season indicated by the parameter, then set the flag indicating to watch
        ! for melt season.
        ! TODO: rsr, need to rethink this at some point
        if (day_of_year == this%melt_look(chru)) then
          this%mso(chru) = 2  ! [flag]
        endif

        if (pkwater_equiv(chru) < DNEARZERO) then
          ! No existing snowpack
          if (this%newsnow(chru) == 0) then
            ! Skip the HRU if there is no snowpack and no new snow
            ! Reset to be sure it is zero if snowpack melted on last timestep.
            this%snowcov_area(chru) = 0.0
            cycle
          elseif (this%newsnow(chru) == 1) then
            ! We have new snow; the initial snow-covered area is complete (1)
            this%snowcov_area(chru) = 1.0  ! [fraction of area]
          endif
        endif

        ! HRU STEP 1 - DEAL WITH PRECIPITATION AND ITS EFFECT ON THE WATER
        !              CONTENT AND HEAT CONTENT OF SNOW PACK
        !***********************************************************************

              ! if (chru == 5) then
              !   if (curr_month == 3 .and. model_time%Nowyear == 1986) then
              !     print *, '^', pkwater_equiv(chru)
              !   endif
              ! endif

        ! If there is net precipitation on an existing snowpack, OR if there is
        ! any net snow, add the incoming water (or ice) and heat (or heat deficit)
        ! to the snowpack.
        ! WARNING: pan - wouldn't this be pkwater_equiv > DNEARZERO?
        if ((pkwater_equiv(chru) > 0.0_dp .and. net_ppt(chru) > 0.0) .or. net_snow(chru) > 0.0) then
          ! DEBUG:
          ! write(*,*) 'pkwater_equiv, net_ppt, net_snow', pkwater_equiv(chru), net_ppt(chru), net_snow(chru), this%pptmix(chru), this%newsnow(chru)
          ! write(*,*) 'pst, ai', this%pst(chru), this%ai(chru)
          call this%ppt_to_pack(model_climate, model_precip, curr_month, chru, &
                                ctl_data, intcp, model_temp)
          ! write(*,*) 'pst, ai, pkwater_equiv', this%pst(chru), this%ai(chru), pkwater_equiv(chru)
        endif

        if (pkwater_equiv(chru) > 0.0_dp) then
          ! There is still a snowpack

          ! HRU STEP 2 - CALCULATE THE NEW SNOW COVERED AREA
          !**********************************************************
          ! Compute snow-covered area from depletion curve
          call this%snowcov(chru, ctl_data, model_basin, model_climate, intcp, model_precip)

          ! HRU STEP 3 - COMPUTE THE NEW ALBEDO
          !**********************************************************
          ! Compute albedo if there is any snowpack
          call this%snalbedo(intcp, model_precip, chru)

          ! HRU STEP 4 - DETERMINE RADIATION FLUXES AND SNOWPACK
          !              STATES NECESSARY FOR ENERGY BALANCE
          !**********************************************************
          ! Set the emissivity of the air to the emissivity when there is no precipitation
          emis = this%emis_noppt(chru)  ! [fraction of radiation]

          ! Could use equation from Swinbank 63 using Temp, a is -13.638, b is 6.148
          ! Temperature is halfway between the minimum and average temperature for the day
          ! temp = (tminc(chru) + tavgc(chru)) * 0.5
          ! emis = ((temp + 273.16)**(Emis_coefb - 4.0)) * (10.0**(Emis_coefa + 1.0)) / 5.670373E−8 ! /by Stefan Boltzmann in SI units

          ! If there is any precipitation in the HRU, reset the emissivity to 1
          if (hru_ppt(chru) > 0.0) emis = 1.0  ! [fraction of radiation]

          ! Save the current value of emissivity
          esv = emis  ! [fraction of radiation]

          ! The incoming shortwave radiation is the HRU radiation adjusted by the
          ! albedo (some is reflected back into the atmoshphere) and the
          ! transmission coefficient (some is intercepted by the winter vegetative canopy)
          swn = swrad(chru) * (1.0 - this%albedo(chru)) * this%rad_trncf(chru)  ! [cal/cm^2] or [Langleys]

          ! Set the convection-condensation for a half-day interval
          cec = this%cecn_coef(chru, curr_month) * 0.5  ! [cal/(cm^2 degC)] or [Langleys/degC]

          ! If the land cover is trees, reduce the convection-condensation
          ! parameter by half
          if (cov_type(chru) > 2) then
            ! RSR: cov_type==4 is valid for trees (coniferous)
            cec = cec * 0.5  ! [cal/(cm^2 degC)] or [Langleys/degC]
          endif

          ! Calculate the new snow depth (Riley et al. 1973)
          ! RSR: the following 3 lines of code were developed by Rob Payn, 7/10/2013
          ! The snow depth depends on the previous snow pack water equivalent plus
          ! the current net snow.
          this%pss(chru) = this%pss(chru) + dble(net_snow(chru))  ! [inches]
          dpt_before_settle = this%pk_depth(chru) + dble(net_snow(chru)) * this%deninv
          dpt1 = dpt_before_settle + dble(this%settle_const) * ((this%pss(chru) * this%denmaxinv) - dpt_before_settle)

          ! RAPCOMMENT - CHANGED TO THE APPROPRIATE FINITE DIFFERENCE APPROXIMATION OF SNOW DEPTH
          this%pk_depth(chru) = dpt1  ! [inches]

          ! Calculate the snowpack density
          if (dpt1 > 0.0_dp) then
            this%pk_den(chru) = sngl(pkwater_equiv(chru) / dpt1)
          else
            this%pk_den(chru) = 0.0  ! [inch water equiv / inch depth]
          endif

          ! The effective thermal conductivity is approximated (empirically)
          ! as 0.0077 times (snowpack density)^2 [cal / (sec g degC)] Therefore,
          ! the effective conductivity term (inside the square root) in the
          ! equation for conductive heat exchange can be calculated as follows:
          !   (0.0077 * pk_den^2) / (pk_den * 0.5)
          ! where 0.5 is the specific heat of ice [cal / (g degC)]
          ! this simplifies to the following
          effk = 0.0154 * this%pk_den(chru)  ! [unitless]

          ! 13751 is the number of seconds in 12 hours over pi
          ! So for a half day, to calculate the conductive heat exchange per cm
          ! of snow per cm^2 area per degree temperature difference is the following
          ! In effect, multiplying cst times the temperature gradient gives the
          ! heatexchange by heat conducted (calories) per square cm of snowpack
          cst = this%pk_den(chru) * (SQRT(effk * 13751.0))  ! [cal/(cm^2 degC)] or [Langleys / degC]

          ! Check whether to force spring melt
          ! Spring melt is forced if time is before the melt-force day and after
          ! the melt-look day (parameters). If between these dates, the spring melt
          ! applies if the snowpack temperature is above or equal to 0 for more
          ! than 4 cycles of the snorun function.
          if (this%iso(chru) == 1) then
            ! Before the first melt-force day
            if (this%mso(chru) == 2) then
              ! After the first melt-look day

              ! Melt season is determined by the number of days the snowpack is
              ! above 0 degrees C. The first time that the snowpack is isothermal
              ! at 0 degrees C for more than 4 days is the beginning of snowmelt season.
              ! 2 options below (if-then, else)
              if (this%pk_temp(chru) >= 0.0) then
                ! (1) The snowpack temperature is 0 degrees
                ! Increment the number of days that the snowpack has been
                ! isothermal at 0 degrees C
                this%lso(chru) = this%lso(chru) + 1  ! [days]

                ! If the snowpack temperature has been 0 or greater for more than 4 cycles
                if (this%lso(chru) > 4) then
                  ! Set the melt-force flag and reset counter
                  this%iso(chru) = 2  ! [flag]
                  this%lso(chru) = 0  ! [days]
                endif
              else
                ! (2) The snowpack temperature is less than 0 degrees
                ! Reset the counter for days snowpack temperature is above 0
                this%lso(chru) = 0  ! [days]
              endif
            endif
          endif

          ! Compute energy balance for night period
          ! niteda is a flag indicating nighttime (1) or daytime (2)
          ! Set the flag indicating night time
          niteda = 1  ! [flag]

          ! No shortwave (solar) radiation at night.
          sw = 0.0  ! [cal / cm^2] or [Langleys]

          ! Temperature is halfway between the minimum and average temperature
          ! for the day.
          temp = (tmin(chru) + tavg(chru)) * 0.5

          ! Calculate the night time energy balance
          call this%snowbal(cals, model_climate, ctl_data, intcp, model_precip, &
                            chru, curr_month, niteda, cec, cst, esv, sw, sngl(temp), trd)

          ! Track total heat flux from both night and day periods
          this%tcal(chru) = cals  ! [cal/cm^2] or [Langleys]

          ! Compute energy balance for day period (if the snowpack still exists)
          if (pkwater_equiv(chru) > 0.0_dp) then
            ! Set the flag indicating daytime
            niteda = 2  ! [flag]

            ! Set shortwave radiation as calculated earlier
            sw = swn  ! [cal/cm^2] or [Langleys]

            ! Temperature is halfway between the maximum and average temperature
            ! for the day.
            temp = (tmax(chru) + tavg(chru)) * 0.5  ! [degrees C]

            call this%snowbal(cals, model_climate, ctl_data, intcp, model_precip, &
                              chru, curr_month, niteda, cec, cst, esv, sw, sngl(temp), trd)

            ! Track total heat flux from both night and day periods
            this%tcal(chru) = this%tcal(chru) + cals  ! [cal/cm^2] or [Langleys]
          endif

          !  HRU STEP 5 - CALCULATE SNOWPACK LOSS TO EVAPORATION
          !********************************************************

          ! Compute snow evaporation (if there is still a snowpack)
          ! Some of the calculated evaporation can come from interception rather
          ! than the snowpack.  Therefore, the effects of interception must be evaluated.

          if (pkwater_equiv(chru) > 0.0_dp) then
            ! Snow can evaporate when transpiration is not occuring or when
            ! transpiration is occuring with cover types of bare soil or grass.
            ! if (transp_on(chru) == 0 .or. (transp_on(chru) == 1 .and. cov_type(chru) < 2)) then
            if (.not. transp_on(chru) .or. (transp_on(chru) .and. cov_type(chru) < 2)) then
              call this%snowevap(model_climate, chru, ctl_data, intcp, model_potet)
            end if
          else if (pkwater_equiv(chru) < 0.0_dp) then
            if (print_debug > -1) then
              if (pkwater_equiv(chru) < -DNEARZERO) then
                print *, 'snowpack issue 3, negative pkwater_equiv, HRU:', chru, ' value:', pkwater_equiv(chru)
              end if
            end if

            pkwater_equiv(chru) = 0.0_dp  ! just to be sure negative values are ignored
          end if

          !  HRU CLEAN-UP - ADJUST FINAL HRU SNOWPACK STATES AND
          !                 INCREMENT THE BASIN TOTALS
          !*********************************************************

          ! Final state of the snowpack depends on whether it still exists after
          ! all the processing above.
          ! 2 options below (if-then, else)

          if (pkwater_equiv(chru) > 0.0_dp) then
            ! (1) Snow pack still exists
            ! Snowpack still exists
            if (this%pk_den(chru) > 0.0) then
              this%pk_depth(chru) = pkwater_equiv(chru) / dble(this%pk_den(chru))
            else
              this%pk_den(chru) = this%den_max
              this%pk_depth(chru) = pkwater_equiv(chru) * this%denmaxinv
            end if
            this%pss(chru) = pkwater_equiv(chru)

            ! If it is during the melt period and snowfall was insufficient to
            ! reset albedo, then reduce the cumulative new snow by the amount
            ! melted during the period (but don't let it be negative).
            ! if (this%lst(chru) > 0) then
            if (this%lst(chru)) then
              this%snsv(chru) = this%snsv(chru) - this%snowmelt(chru)

              if (this%snsv(chru) < 0.0) then
                this%snsv(chru) = 0.0
              end if
            end if
          end if
        end if

        ! LAST check to clear out all arrays if packwater is gone
        if (pkwater_equiv(chru) <= 0.0_dp) then
          if (print_debug > -1) then
            if (pkwater_equiv(chru) < -DNEARZERO) then
              print *, 'Snowpack problem, pkwater_equiv negative, HRU:', chru, ' value:', pkwater_equiv(chru)
            end if
          end if

          pkwater_equiv(chru) = 0.0_dp ! just to be sure negative values are ignored

          ! Snowpack has been completely depleted, reset all states to no-snowpack values
          this%pk_depth(chru) = 0.0_dp
          this%pss(chru) = 0.0_dp
          this%snsv(chru) = 0.0
          this%lst(chru) = .false.
          this%pst(chru) = 0.0_dp
          this%iasw(chru) = .false.
          this%albedo(chru) = 0.0
          this%pk_den(chru) = 0.0
          this%snowcov_area(chru) = 0.0
          this%pk_def(chru) = 0.0
          this%pk_temp(chru) = 0.0
          this%pk_ice(chru) = 0.0
          this%freeh2o(chru) = 0.0
          this%snowcov_areasv(chru) = 0.0  ! rsr, not in original code
          this%ai(chru) = 0.0_dp
          this%frac_swe(chru) = 0.0
        endif
      enddo

      if (print_debug == 9) then
        print 9001, day_of_year, (transp_on(chru), chru=1, nhru)
        print 9001, day_of_year, (net_snow(chru), chru=1, nhru)
        print 9001, day_of_year, (this%snowmelt(chru), chru=1, nhru)
      endif

      9001 FORMAT (I5, 177F6.3)
    end associate
  end subroutine


  module subroutine cleanup_Snowcomp(this, ctl_data)
    class(Snowcomp) :: this
      !! Snowcomp class
    type(Control), intent(in) :: ctl_data

    ! --------------------------------------------------------------------------
    associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
      if (save_vars_to_file == 1) then
        ! Write out this module's restart variables
        call ctl_data%write_restart_variable('albedo', this%albedo)
        call ctl_data%write_restart_variable('freeh2o', this%freeh2o)
        call ctl_data%write_restart_variable('iasw', this%iasw)
        call ctl_data%write_restart_variable('int_alb', this%int_alb)
        call ctl_data%write_restart_variable('iso', this%iso)
        call ctl_data%write_restart_variable('lso', this%lso)
        call ctl_data%write_restart_variable('lst', this%lst)
        call ctl_data%write_restart_variable('mso', this%mso)
        call ctl_data%write_restart_variable('pk_def', this%pk_def)
        call ctl_data%write_restart_variable('pk_den', this%pk_den)
        call ctl_data%write_restart_variable('pk_depth', this%pk_depth)
        call ctl_data%write_restart_variable('pk_ice', this%pk_ice)
        call ctl_data%write_restart_variable('pk_temp', this%pk_temp)
        call ctl_data%write_restart_variable('pksv', this%pksv)
        call ctl_data%write_restart_variable('pkwater_ante', this%pkwater_ante)
        call ctl_data%write_restart_variable('pss', this%pss)
        call ctl_data%write_restart_variable('pst', this%pst)
        call ctl_data%write_restart_variable('salb', this%salb)
        call ctl_data%write_restart_variable('scrv', this%scrv)
        call ctl_data%write_restart_variable('slst', this%slst)
        call ctl_data%write_restart_variable('snowcov_area', this%snowcov_area)
        call ctl_data%write_restart_variable('snowcov_areasv', this%snowcov_areasv)
        call ctl_data%write_restart_variable('snsv', this%snsv)
      end if
    end associate
  end subroutine

  !***********************************************************************
  !      Subroutine to compute changes in snowpack when a net gain in
  !        heat energy has occurred.
  !***********************************************************************
  module subroutine calin(this, model_climate, ctl_data, cal, chru)
    implicit none

    ! Arguments
    class(Snowcomp), intent(inout) :: this
    type(Climateflow), intent(inout) :: model_climate
    type(Control), intent(in) :: ctl_data
    real(r32), intent(in) :: cal
    integer(i32), intent(in) :: chru

    ! Local Variables
    real(r32) :: apk_ice
      !! Pack-ice per area [inches]
    real(r32) :: apmlt
      !! Actual potential snowmelt [inches]
    real(r32) :: dif
      !! Difference between incoming calories and calories needed to bring the pack to isothermal at 0 [cal/cm^2]
    real(r32) :: pmlt
      !! Potential amount of snowmelt from excess heat in rain [inches]
    real(r32) :: pwcap
      !! Capacity of the snowpack to hold free water [inches]
    real(r64) :: dif_dble
      !! 64-bit version of dif [cal/cm^2]

    ! this
    ! iasw(RW), pk_def(RW), pk_den(RW), pk_depth(RW), pk_ice(RW), pk_temp(RW),
    ! pss(RW), pst(RW), snowmelt(RW),

    ! climate
    ! pkwater_equiv(RW),
    ! --------------------------------------------------------------------------
    associate(pkwater_equiv => model_climate%pkwater_equiv(chru), &

              print_debug => ctl_data%print_debug%value, &

              freeh2o_cap => this%freeh2o_cap(chru), &
              freeh2o => this%freeh2o(chru), &
              iasw => this%iasw(chru), &
              pk_def => this%pk_def(chru), &
              pk_den => this%pk_den(chru), &
              pk_depth => this%pk_depth(chru), &
              pk_ice => this%pk_ice(chru), &
              pk_temp => this%pk_temp(chru), &
              pss => this%pss(chru), &
              pst => this%pst(chru), &
              snowcov_area => this%snowcov_area(chru), &
              snowmelt => this%snowmelt(chru))

      ! Calculate the difference between the incoming calories and the calories
      ! needed to bring the pack to isothermal at 0 (heat deficit).
      dif = cal - pk_def  ! [cal/cm^2]

      ! The way incoming heat is handled depends on whether there is not enough,
      ! just enough, or more than enough heat to overcome the heat deficit of the snowpack.
      ! 3 choices below (if-then, elseif, else)

      ! (1) Not enough heat to overcome heat deficit...
      if (dif < 0.0) then
        ! Reduce the heat deficit by the amount of incoming calories and adjust to
        ! the new temperature based on new heat deficit.
        pk_def = pk_def - cal  ! [cal/cm^2]
        pk_temp = -pk_def / sngl(pkwater_equiv * 1.27_dp)  ! [degrees C]

      ! (3) More than enough heat to overcome heat deficit (melt ice)...
      elseif (dif > 0.0) then
        ! Calculate the potential amount of snowmelt from excess heat in rain it
        ! takes 203.2 calories / (in cm^2) to melt snow (latent heat of fusion).
        ! Convert from 1 cm depth over 1 square cm to
        ! 1 inch depth over 1 square cm 80.0*(INCH2CM = 2.54 cm/in) = 203.2
        pmlt = dif / 203.2  ! [inches]

        ! Actual snowmelt can only come from snow covered area, so to calculate
        ! the actual potential snowmelt, the potential snowmelt from snowcovered
        ! area must be re-normalized to HRU area (rather than snowcover area).
        ! In effect, the potential snowmelt per area is reduced by the fraction of
        ! the watershed that is actually covered by snow.
        apmlt = pmlt * snowcov_area  ! [inches]

        ! Set the heat deficit and temperature of the remaining snowpack to 0.
        pk_def = 0.0  ! [cal/cm^2]
        pk_temp = 0.0  ! [degrees C]

        ! The only pack ice that is melted is in the snow covered area, so the
        ! pack ice needs to be re-normalized to the snowcovered area (rather than
        ! HRU area). In effect, the pack ice per area is increased by the fraction
        ! of the watershed that is actually covered by snow.
        if (snowcov_area > 0.0) then
          apk_ice = pk_ice / snowcov_area  ! [inches]
        else
          ! print *, 'snowcov_area really small, melt all ice', snowcov_area, ' pmlt:', pmlt, ' dif:', dif, ' pk_ice:', pk_ice
          apk_ice = 0.0
        endif

        ! If snow is melting, the heat is handled based on whether all or only
        ! part of the pack ice melts.
        ! 2 options below (if-then, else)

        ! (3.1) Heat applied to snow covered area is sufficient to melt all the
        !       ice in that snow pack.
        if (pmlt > apk_ice) then
          ! All pack water equivalent becomes meltwater.
          snowmelt = snowmelt + sngl(pkwater_equiv)  ! [inches]
          pkwater_equiv = 0.0_dp  ! [inches]
          ! iasw = 0  ! [flag]
          iasw = .false.  ! [flag]

          ! Set all snowpack states to 0.
          ! snowcov_area = 0.0  ! [fraction of area] ! shouldn't be changed with melt
          pk_def = 0.0  ! [cal / cm^2]
          pk_temp = 0.0  ! [degreees C]
          pk_ice = 0.0  ! [inches]
          freeh2o = 0.0  ! [inches]
          pk_depth = 0.0_dp  ! [inches]
          pss = 0.0_dp  ! [inches]
          pst = 0.0_dp  ! [inches]
          pk_den = 0.0  ! [fraction of depth]

        ! (3.2) Heat only melts part of the ice in the snow pack.
        else
          ! Remove actual melt from frozen water and add melt to free water.
          pk_ice = pk_ice - apmlt  ! [inches]
          freeh2o = freeh2o + apmlt  ! [inches]

          ! Calculate the capacity of the snowpack to hold free water according to
          ! its current level of frozen water.
          pwcap = freeh2o_cap * pk_ice  ! [inches]

          ! Calculate the amount of free water in excess of the capacity to hold
          ! free water.
          dif_dble = dble(freeh2o - pwcap)  ! [inches]

          ! If there is more free water than the snowpack can hold, then there is
          ! going to be melt...
          if (dif_dble > 0.0_dp) then
            if (dif_dble > pkwater_equiv) dif_dble = pkwater_equiv
            ! total packwater decreases by the excess and a new depth is calculated
            ! based on density.
            pkwater_equiv = pkwater_equiv - dif_dble  ! [inches]

            ! Free water is at the current capacity.
            freeh2o = pwcap  ! [inches]
            if (pk_den > 0.0) then
              pk_depth = pkwater_equiv / dble(pk_den)  ! [inches]
              ! RAPCOMMENT - Added the conditional statement to make sure there is
              !              no division by zero (this can happen if there is a
              !              mixed event on no existing snowpack because a pack
              !              density has not been calculated, yet).
            else
              !rsr, this should not happen, remove later
              if (print_debug > -1) then
                print *, 'snow density problem', pk_depth, pk_den, pss, pkwater_equiv
                ! call print_date(1)
              endif

              pk_den = this%den_max
              pk_depth = pkwater_equiv * this%denmaxinv  ! [inches]
            endif

            ! Snowmelt increases by the excess free water.
            snowmelt = snowmelt + sngl(dif_dble)  ! [inches]

            ! Reset the previous-snowpack-plus-new-snow to the current pack water equivalent.
            pss = pkwater_equiv  ! [inches]
          endif
        endif
        ! (2) Just enough heat to overcome heat deficit
      else ! if ( dif==0.0 ) then ! rsr 1/27/2016 why not set all snow states to 0 ???
        ! Set temperature and heat deficit to zero.
        pk_temp = 0.0  ! [degrees C]
        pk_def = 0.0  ! [cal/cm^2]
      endif
    end associate
  end subroutine


  !***********************************************************************
  ! Subroutine to compute change in snowpack when a net loss in
  ! heat energy has occurred.
  !***********************************************************************
  module subroutine caloss(this, model_climate, cal, chru)
    ! caloss(Cal, Pkwater_equiv, Pk_def, Pk_temp, Pk_ice, Freeh2o)
    implicit none

    ! Arguments
    class(Snowcomp), intent(inout) :: this
    type(Climateflow), intent(inout) :: model_climate
    real(r32), intent(in) :: cal
    integer(i32), intent(in) :: chru

    ! Local Variables
    real(r32) :: calnd
      !! Total amount of heat per area that can be released by free water freezing [cal/cm^2]
    real(r32) :: dif
      !! Difference between heat in free water and the heat that can be absorbed by new snow without melting [cal/cm^2]

    ! this
    ! freeh2o(RW), pk_def(RW), pk_ice(RW), pk_temp(RW)

    ! climate
    ! pkwater_equiv(RW)
    ! --------------------------------------------------------------------------
    associate(freeh2o => this%freeh2o(chru), &
              pk_def => this%pk_def(chru), &
              pk_ice => this%pk_ice(chru), &
              pk_temp => this%pk_temp(chru), &

              pkwater_equiv => model_climate%pkwater_equiv(chru))

      ! Loss of heat is handled differently if there is liquid water in the
      ! snowpack or not.
      ! 2 options below (if-then, else)
      if (freeh2o < CLOSEZERO) then
        ! (1) No free water exists in pack

        ! Heat deficit increases because snow is colder than pack
        ! (minus a negative number = plus).
        pk_def = pk_def - cal  ! [cal/cm^2]
      else
        ! (2) Free water exists in pack

        ! Calculate the total amount of heat per area that can be released by
        ! free water freezing.
        calnd = freeh2o * 203.2  ! [cal/cm^2]

        ! Determine the difference between heat in free water and the heat that
        ! can be absorbed by new snow (without melting). Remember that cal is a
        ! negative number.
        dif = cal + calnd  ! [cal/cm^2]

        ! The effect of freezing water depends on whether all or only part of
        ! the liquid water freezes.
        ! 2 options below (if-then, else)
        if (dif > 0.0) then
          ! (2) Only part of free water freezes

          ! The calories absorbed by the new snow freezes some of the free water
          ! (increase in ice, decrease in free water).
          pk_ice = pk_ice + (-cal / 203.2)  ! [inches]
          freeh2o   = freeh2o - (-cal / 203.2)  ! [inches]
          return
        else ! if ( dif<=0.0 ) then
          ! (1) All free water freezes

          ! If all the water freezes, then the remaining heat that can be
          ! absorbed by new snow (that which is not provided by freezing free
          ! water) becomes the new pack heat deficit.
          if (dif < 0.0) pk_def = -dif  ! [cal/cm^2]

          ! Free pack water becomes ice.
          pk_ice = pk_ice + freeh2o  ! [inches]
          freeh2o = 0.0  ! [inches]
        endif
      endif

      if (pkwater_equiv > 0.0_dp) then
        ! There is still a snowpack, calculate the new temperature.
        pk_temp = -pk_def / sngl(pkwater_equiv * 1.27_dp)  ! [degrees C]
      elseif (pkwater_equiv < 0.0_dp) then
        ! if ( pkwater_equiv<-DNEARZERO ) &
        !   print *, 'snowpack issue 4, negative pkwater_equiv', pkwater_equiv
        pkwater_equiv = 0.0_dp
      endif
    end associate
  end subroutine


  !***********************************************************************
  ! Subroutine to add rain and/or snow to snowpack
  !***********************************************************************
  module subroutine ppt_to_pack(this, model_climate, model_precip, month, chru, ctl_data, intcp, model_temp)
    implicit none

    class(Snowcomp), intent(inout) :: this
    type(Climateflow), intent(inout) :: model_climate
    class(Precipitation), intent(in) :: model_precip
    integer(i32), intent(in) :: month
    integer(i32), intent(in) :: chru
    type(Control), intent(in) :: ctl_data
    type(Interception), intent(in) :: intcp
    class(Temperature), intent(in) :: model_temp

    ! Local Variables
    real(r32) :: caln
      !! Liquid to solid state latent heat [cal/(in cm^2)]
    real(r32) :: calpr
      !! Heat per area added by the rain [cal/cm^2]
    real(r32) :: calps
      !! Amount of heat new snow will absorb if warming it to 0C [cal/cm^2]
    real(r32) :: pndz
      !! Amount of rain needed to bring the snowpack to isothermal at 0 [inches]
    real(r32) :: train
      !! Rain temperature [degree C]
    real(r32) :: tsnow
      !! Snow temperature [degree C]

    ! this
    ! freeh2o(RW), pk_def(RW), pk_ice(RW), pptmix_nopack(RW), pk_precip(RW), pk_temp(RW),

    !***********************************************************************
    associate(pkwater_equiv => model_climate%pkwater_equiv, &

              ! pptmix => model_precip%pptmix, &
              tmax_allsnow => model_precip%tmax_allsnow_c, &

              net_rain => intcp%net_rain, &
              net_snow => intcp%net_snow, &

              tavg => model_temp%tavg, &
              tmax => model_temp%tmax, &
              tmin => model_temp%tmin)

      ! Added by PAN 2018-07-16
      caln = 0.0
      calpr = 0.0
      calps = 0.0
      pndz = 0.0

      ! The temperature of precipitation will be different if it is mixed or
      ! all rain or snow 2 options below (if-then, else).

      ! If there is any snow, the snow temperature is the average temperature.
      tsnow = tavg(chru)  ! [degrees C]

      if (this%pptmix(chru) == 1) then
        ! (1) If precipitation is mixed...

        ! If there is any rain, the rain temperature is halfway between the maximum
        ! temperature and the allsnow temperature.
        train = (tmax(chru) + tmax_allsnow(chru, month)) * 0.5  ! [degrees C]

        ! Temperatures will differ, depending on the presence of existing snowpack.
        if (pkwater_equiv(chru) > 0.0_dp) then
          ! If there is a snowpack, snow temperature is halfway between the minimum
          ! daily temperature and maximum temperature for which all precipitation is snow.
          tsnow = (tmin(chru) + tmax_allsnow(chru, month)) * 0.5  ! [degrees C]
        elseif (pkwater_equiv(chru) < 0.0_dp) then
          ! If no existing snowpack, snow temperature is the average temperature for the day.
          pkwater_equiv(chru) = 0.0_dp  ! To be sure negative snowpack is ignored
        endif
      else
        ! (2) If precipitation is all snow or all rain...

        ! If there is any rain, the rain temperature is the average temperature.
        train = (tavg(chru))  ! [degrees C]

        if (train < CLOSEZERO) then
          ! If average temperature is close to freezing, the rain temperature is
          ! halfway between the maximum daily temperature and maximum temperature
          ! for which all precipitation is snow.
          train = (tmax(chru) + tmax_allsnow(chru, month)) * 0.5 ! [degrees C]
        endif
      endif

      if (train < 0.0) train = 0.0  ! [degrees C] ! train can't be < 0
      if (tsnow > 0.0) tsnow = 0.0  ! [degrees C] ! tsnow can't be > 0

      ! Leavesley comments...
      ! If snowpack already exists, add rain first, then add snow.  If no
      ! antecedent snowpack, rain is already taken care of, so start snowpack with
      ! snow.  This subroutine assumes that in a mixed event, the rain will be
      ! first and turn to snow as the temperature drops.

      ! Rain can only add to the snowpack if a previous snowpack exists, so rain
      ! or a mixed event is processed differently when a snowpack exists.
      ! 2 options below (if-then, elseif)
      if (pkwater_equiv(chru) > 0.0_dp) then
        ! (1) There is net rain on an existing snowpack...
        if (net_rain(chru) > 0.0) then
          ! Add rain water to pack (rain on snow) and increment the precipitation
          ! on the snowpack by the rain water.
          pkwater_equiv(chru) = pkwater_equiv(chru) + dble(net_rain(chru))  ! [inches]
          this%pk_precip(chru) = this%pk_precip(chru) + net_rain(chru)  ! [inches]

          ! Incoming rain water carries heat that must be added to the snowpack.
          ! This heat could both warm the snowpack and melt snow. Handling of this
          ! heat depends on the current thermal condition of the snowpack.
          ! 2 options below (if-then, else)

          ! (1.1) If the snowpack is colder than freezing it has a heat deficit
          ! (requires heat to be brought to isothermal at 0 degC)...
          if (this%pk_def(chru) > 0.0) then
            ! Calculate the number of calories given up per inch of rain when
            ! cooling it from the current rain temperature to 0 deg C and then
            ! freezing it (liquid to solid state latent heat).
            ! This calculation assumes a volume of an inch of rain over a square cm of area
            ! 80 cal come from freezing 1 cm3 at 0 C (latent heat of fusion is 80 cal/cm^3),
            ! 1 cal from cooling 1cm3 for every degree C (specific heat of water is 1 cal/(cm^3 degC)),
            ! convert from 1 cm depth over 1 square cm to 1 inch depth over 1 square cm (INCH2CM = 2.54 cm/in)
            caln = (80.0 + train) * INCH2CM  ! [cal / (in cm^2)]

            ! Calculate the amount of rain in inches (at the current rain temperature)
            ! needed to bring the snowpack to isothermal at 0.
            pndz = this%pk_def(chru) / caln  ! [inches]

            ! The effect of rain on the snowpack depends on if there is not enough,
            ! enough, or more than enough heat in the rain to bring the snowpack
            ! to isothermal at 0 degC or not 3 options below (if-then, elseif, else).

            ! (1.1.1) Exactly enough rain to bring pack to isothermal...
            if (ABS(net_rain(chru) - pndz) < CLOSEZERO) then
              ! Heat deficit and temperature of the snowpack go to 0.
              this%pk_def(chru) = 0.0  ! [cal/cm^2]
              this%pk_temp(chru) = 0.0  ! [degrees C]

              ! In the process of giving up its heat, all the net rain freezes and
              ! becomes pack ice.
              this%pk_ice(chru) = this%pk_ice(chru) + net_rain(chru)  ! [inches]

            ! (1.1.2) Rain not sufficient to bring pack to isothermal...
            elseif (net_rain(chru) < pndz) then
              ! The snowpack heat deficit decreases by the heat provided by rain
              ! and a new snowpack temperature is calculated.
              ! 1.27 is the specific heat of ice (0.5 cal/(cm^3 degC))
              ! times the conversion of cm to inches (2.54 cm/in)
              this%pk_def(chru) = this%pk_def(chru) - (caln * net_rain(chru))  ! [cal/(in cm^3)]
              this%pk_temp(chru) = -this%pk_def(chru) / sngl(pkwater_equiv(chru) * 1.27_dp)

              ! All the net rain freezes and becomes pack ice
              this%pk_ice(chru) = this%pk_ice(chru) + net_rain(chru)

            ! (1.1.3) Rain in excess of amount required to bring pack to isothermal...
            else
              ! Heat deficit and temperature of the snowpack go to 0.
              this%pk_def(chru) = 0.0
              this%pk_temp(chru) = 0.0
              ! The portion of net rain that brings the snowpack to isothermal freezes.
              this%pk_ice(chru) = this%pk_ice(chru) + pndz

              ! The rest of the net rain becomes free water in the snowpack.
              ! Note that there cannot be previous freeh2o because the snowpack
              ! had a heat deficit (all water was ice) before this condition was reached.
              this%freeh2o(chru) = net_rain(chru) - pndz

              ! Calculate the excess heat per area added by the portion of rain
              ! that does not bring the snowpack to isothermal (using specific heat of water)
              calpr = train * (net_rain(chru) - pndz) * INCH2CM  ! [cal/cm^2]

              ! Add the new heat to the snow pack (the heat in this excess rain
              ! will melt some of the pack ice when the water cools to 0 degC).

                  ! if (chru == 5) then
                  !     print *, ' ', pkwater_equiv(chru), month, train, calpr, pndz, net_rain(chru), net_snow(chru)
                  ! endif

              call this%calin(model_climate, ctl_data, calpr, chru)

                  ! if (chru == 5) then
                  !     print *, '*', pkwater_equiv(chru)
                  ! endif
            endif

          ! (1.2) Rain on snowpack that is isothermal at 0 degC (no heat deficit)...
          else
            ! All net rain is added to free water in the snowpack.
            this%freeh2o(chru) = this%freeh2o(chru) + net_rain(chru)

            ! Calculate the heat per area added by the rain (using specific heat of water).
            calpr = train * net_rain(chru) * INCH2CM  ! [cal/cm^2]

            ! Add the new heat to the snow pack (the heat in rain will melt some
            ! of the pack ice when the water cools to 0 degC).
                ! if (chru == 5) then
                !     print *, '  ', pkwater_equiv(chru), month, train, calpr, pndz, net_rain(chru), net_snow(chru)
                ! endif

            call this%calin(model_climate, ctl_data, calpr, chru)
                ! if (chru == 5) then
                !     print *, '**', pkwater_equiv(chru)
                ! endif
          endif
        endif
      elseif (net_rain(chru) > 0.0) then
        ! (2) If there is net rain but no snowpack, set flag for a mix on no snowpack.

        ! Be careful with the code here.
        ! If this subroutine is called when there is an all-rain day on no
        ! existing snowpack (currently, it will not), then the flag here will be
        ! set inappropriately.
        this%pptmix_nopack(chru) = .true.  ! [flag]
      endif

      ! At this point, the subroutine has handled all conditions where there is
      ! net rain, so if there is net snow (doesn't matter if there is a pack or not)...
      if (net_snow(chru) > 0.0) then
        ! Add the new snow to the pack water equivalent, precip, and ice
        pkwater_equiv(chru) = pkwater_equiv(chru) + dble(net_snow(chru))
        this%pk_precip(chru) = this%pk_precip(chru) + net_snow(chru)
        this%pk_ice(chru) = this%pk_ice(chru) + net_snow(chru)

        ! The temperature of the new snow will determine its effect on snowpack heat deficit
        ! 2 options below (if-then, else)
        if (tsnow >= 0.0) then
          ! (1) if the new snow is at least 0 degC...

          ! Incoming snow does not change the overall heat content of the snowpack.
          ! However, the temperature will change, because the total heat content
          ! of the snowpack will be "spread out" among more snow.  Calculate the
          ! snow pack temperature from the heat deficit, specific heat of snow,
          ! and the new total snowpack water content.
          this%pk_temp(chru) = -this%pk_def(chru) / sngl(pkwater_equiv(chru) * 1.27_dp)  ! [degrees C]
        else
          ! (2) If the new snow is colder than 0 degC...

          ! Calculate the amount of heat the new snow will absorb if warming it
          ! to 0C (negative number). This is the negative of the heat deficit of
          ! the new snow.
          calps = tsnow * net_snow(chru) * 1.27  ! [cal/cm^2]

          ! The heat to warm the new snow can come from different sources
          ! depending on the state of the snowpack.
          ! 2 options below (if-then, else)
          if (this%freeh2o(chru) > 0.0) then
            ! (2.1) If there is free water in the pack (at least some of it is going to freeze)...
            call this%caloss(model_climate, calps, chru)
          else
            ! (2.2) If there is no free water (snow pack has a heat deficit greater
            !       than or equal to 0)...

            ! Heat deficit increases because snow is colder than pack (minus a
            ! negative number = plus) and calculate the new pack temperature.
            this%pk_def(chru) = this%pk_def(chru) - calps  ! [cal/cm^2]
            this%pk_temp(chru) = -this%pk_def(chru) / sngl(pkwater_equiv(chru) * 1.27_dp)  ! [degrees C]
          endif
        endif
      endif
    end associate
  end subroutine


  !***********************************************************************
  ! Interpolate along snow covered area depletion curve
  !***********************************************************************
  pure module function sca_deplcrv(snarea_curve, frac_swe) result(res)
    implicit none

    ! Arguments
    real(r32) :: res
      !! Snow covered area returned from function
    real(r32), intent(in) :: snarea_curve(11)
    real(r32), intent(in) :: frac_swe

    ! Local Variables
    integer(i32) :: idx
      !! Index of the depletion curve that brackets given frac_swe
    integer(i32) :: jdx
      !! Index of the depletion curve that brackets given frac_swe
    real(r32) :: dify
      !! Fraction of distance given frac_swe is between the next highest and lowest curve values [fraction]
    real(r32) :: difx
      !! Difference in snow covered area represented by next highest and lowest curve values

    !***********************************************************************
    if (frac_swe > 1.0) then
      res = snarea_curve(11)
    else
      ! Get the indices (as integers) of the depletion curve that bracket the
      ! given frac_swe (next highest and next lowest).
      idx = int(10.0 * (frac_swe + 0.2))  ! [index]
      jdx = idx - 1  ! [index]

      if (idx > 11) idx = 11

      ! Calculate the fraction of the distance (from the next lowest) the given
      ! frac_swe is between the next highest and lowest curve values.
      dify = (frac_swe * 10.0) - float(jdx - 1)  ! [fraction]

      ! Calculate the difference in snow covered area represented by next
      ! highest and lowest curve values.
      difx = snarea_curve(idx) - snarea_curve(jdx)

      ! Linearly interpolate a snow covered area between those represented by
      ! the next highest and lowest curve values.
      res = snarea_curve(jdx) + dify * difx
    endif
  end function


  !***********************************************************************
  !      Subroutine to compute snowpack albedo
  !***********************************************************************
  module subroutine snalbedo(this, intcp, model_precip, chru)
    implicit none

    ! Arguments
    class(Snowcomp), intent(inout) :: this
    type(Interception), intent(in) :: intcp
    class(Precipitation), intent(in) :: model_precip
    integer(i32), intent(in) :: chru

    ! Local Variables
    integer(i32) :: l
      !! Number of days (or effective days) since last snowfall [days]

    ! this
    ! albedo(RW), int_alb(RW), iso, lst(RW), slst(RW), snsv(RW),
    !***********************************************************************
    associate(prmx => model_precip%prmx(chru), &
              ! newsnow => model_precip%newsnow(chru), &
              ! pptmix => model_precip%pptmix(chru), &

              net_snow => intcp%net_snow(chru), &

              albedo => this%albedo(chru), &
              int_alb => this%int_alb(chru), &
              iso => this%iso(chru), &
              lst => this%lst(chru), &
              salb => this%salb(chru), &
              slst => this%slst(chru), &
              snsv => this%snsv(chru))

      ! The albedo is always reset to a new initial (high) value when there is
      ! new snow above a threshold (parameter). Albedo is then a function of the
      ! number of days since the last new snow. Intermediate conditions apply
      ! when there is new snow below the threshold to reset the albedo to its
      ! highest value.
      ! The curve for albedo change (decreasing) is different for the snow
      ! accumulation season and the snow melt season.
      ! The albedo first depends on if there is no new snow during the current
      ! time step, if there is new snow during accumulation season, or if there
      ! is new snow during melt season.

      ! 3 options below (if-then, elseif, else)
      if (this%newsnow(chru) == 0) then
        ! (1) There is no new snow

        ! If no new snow, check if there was previous new snow that
        ! was not sufficient to reset the albedo (lst=1)
        ! lst can only be greater than 0 during melt season (see below)
        ! if (lst > 0) then
        if (lst) then
          ! slst is the number of days (float) since the last new snowfall.
          ! Set the albedo curve back three days from the number of days since
          ! the previous snowfall (see salb assignment below).
          ! (note that "shallow new snow" indicates new snow that is insufficient
          ! to completely reset the albedo curve)
          ! In effect, a shallow new snow sets the albedo curve back a few days,
          ! rather than resetting it entirely.
          slst = salb - 3.0  ! [days]

          ! Make sure the number of days since last new snow isn't less than 1.
          if (slst < 1.0) slst = 1.0  ! [days]

          if (iso /= 2) then
            ! If not in melt season.

            ! NOTE: This code is unreachable in its current state. This code
            ! is only run during melt season due to the fact that lst can only
            ! be set to 1 in the melt season. Therefore, iso is always going to
            ! be equal to 2.
            ! Make sure the maximum point on the albedo curve is 5.
            ! In effect, if there is any new snow, the albedo can only get so
            ! low in accumulation season, even if the new snow is insufficient
            ! to reset albedo entirely.
            if (slst > 5.0) slst = 5.0  ! [days]
          endif

          ! Reset the shallow new snow flag and cumulative shallow snow variable (see below).
          ! lst = 0  ! [flag]
          lst = .false.  ! [flag]
          snsv = 0.0  ! [inches]
        endif
      elseif (iso == 2) then
        ! (2) New snow during the melt season
        ! RAPCOMMENT - CHANGED TO ISO FROM MSO

        ! If there is too much rain in a precipitation mix, albedo will not be reset.
        ! New snow changes albedo only if the fraction rain is less than the
        ! threshold above which albedo is not reset.
        if (prmx < this%albset_rnm) then
          ! If the fraction rain doesn't prevent the albedo from being reset,
          ! then how the albedo changes depends on whether the snow amount is
          ! above or below the threshold for resetting albedo.
          ! 2 options below (if-then, else)
          if (net_snow > this%albset_snm) then
            ! (2.1) If there is enough new snow to reset the albedo
            ! Reset number of days since last new snow to 0.
            slst = 0.0  ! [days]
            ! lst = 0  ! [flag]
            lst = .false.  ! [flag]

            ! Reset the saved new snow to 0.
            snsv = 0.0  ! [inches]
          else
            ! (2.2) If there is not enough new snow this time period to reset the
            !       albedo on its own.

            ! snsv tracks the amount of snow that has fallen as long as the
            ! total new snow is not enough to reset the albedo.
            snsv = snsv + net_snow ! [inches]

            ! Even if the new snow during this time period is insufficient to
            ! reset the albedo, it may still reset the albedo if it adds enough
            ! to previous shallow snow accumulation.  The change in albedo
            ! depends on if the total amount of accumulated shallow snow has
            ! become enough to reset the albedo or not.
            ! 2 options below (if-then, else)
            if (snsv > this%albset_snm) then
              ! (2.2.1) If accumulated shallow snow is enough to reset the albedo.
              ! Reset the albedo states.
              slst = 0.0  ! [days]
              ! lst = 0  ! [flag]
              lst = .false.  ! [flag]
              snsv = 0.0  ! [inches]
            else
              ! (2.2.2) If the accumulated shallow snow is not enough to
              !         reset the albedo curve.

              ! salb records the number of days since the last new snow
              ! that reset albedo.
              ! if (lst == 0) then
              if (.not. lst) then
                salb = slst  ! [days]
              endif

              ! Reset the number of days since new snow
              slst = 0.0  ! [days]

              ! Set the flag indicating that there is shallow new snow
              ! (i.e. not enough new snow to reset albedo).
              ! lst = 1  ! [flag]
              lst = .true.  ! [flag]
            endif
          endif
        endif
      else
        ! (3) New snow during the accumulation season.

        ! The change in albedo depends on if the precipitation is a mix, if the
        ! rain is above a threshold,  or if the snow is above a threshold.
        ! 4 options below (if-then, elseif, elseif, else)
        if (this%pptmix(chru) < 1) then
          ! (3.1) This is not a mixed event...
          ! During the accumulation season, the threshold for resetting the
          ! albedo does not apply if there is a snow-only event. Therefore, no
          ! matter how little snow there is, it will always reset the albedo
          ! curve the the maximum, if it occurs during the accumulation season.

          ! Reset the time since last snow to 0.
          slst = 0.0  ! [days]

          ! There is no new shallow snow
          ! lst = 0  ! [flag]
          lst = .false.  ! [flag]
        elseif (prmx >= this%albset_rna) then
          ! (3.2) This is a mixed event and the fraction rain is above
          !       the threshold above which albedo is not reset...

          ! There is no new shallow snow.
          ! lst = 0  ! [flag]
          lst = .false.  ! [flag]
          ! Albedo continues to decrease on the curve
        elseif (net_snow >= this%albset_sna) then
          ! (3.3) If it is a mixed event and there is enough new snow to reset albedo...

          ! Reset the albedo.
          slst = 0.0  ! [days]

          ! There is no new shallow snow.
          ! lst = 0  ! [flag]
          lst = .false.  ! [flag]
        else
          ! (3.4) This is a mixed event and the new snow was not enough to reset the albedo...

          ! Set the albedo curve back 3 days (increasing the albedo).
          slst = slst - 3.0  ! [days]

          ! Make sure the number of days since last new snow is not less than 0.
          if (slst < 0.0) then
            slst = 0.0  ! [days]
          endif

          ! Make sure the number of days since last new snow is not greater than 5.
          ! In effect, if there is any new snow, the albedo can only get so low
          ! in accumulation season, even if the new snow is insufficient to
          ! reset albedo entirely
          if (slst > 5.0) then
            slst = 5.0  ! [days]
          endif

          ! lst = 0  ! [flag]
          lst = .false.  ! [flag]
        endif

        snsv = 0.0  ! [inches]
      endif

      ! At this point, the subroutine knows where on the curve the albedo should
      ! be based on current conditions and the new snow (determined by value
      ! of slst variable).

      ! Get the integer value for days (or effective days) since last snowfall.
      l = INT(slst + 0.5)  ! [days]

      ! Increment the state variable for days since the last snowfall.
      slst = slst + 1.0  ! [days]

      !******Compute albedo
      ! Albedo will only be different from the max (default value) if it has
      ! been more than 0 days since the last new snow capable of resetting the
      ! albedo.  If albedo is at the maximum, the maximum is different for
      ! accumulation and melt season.

      ! 3 options below (if-then, elseif, else)
      if (l > 0) then
        ! (1) It has been more than 0 days since the last new snow.

        ! Albedo depends on whether it is currently on the accumulation season
        ! curve or on the melt season curve.
        ! 3 options below (if-then, elseif, else)
        if (int_alb == 2) then
          ! (1.1) Currently using the melt season curve (Old snow - Spring melt period)...

          ! Don't go past the last possible albedo value.
          if (l > MAXALB) l = MAXALB  ! [days]

          ! Get the albedo number from the melt season curve.
          albedo = this%amlt(l)  ! [fraction of radiation]
        elseif (l <= MAXALB) then
          ! (1.2) Currently using the accumulation season curve (Old snow - Winter accumulation period)...
          !       and not past the maximum curve index.

          ! Get the albedo number from the accumulation season curve.
          albedo = this%acum(l)  ! [fraction of radiation]
        else
          ! (1.3) Currently using the accumulation season curve and past the maximum curve index...

          ! Start using the the MELT season curve at 12 days previous to the
          ! current number of days since the last new snow.
          l = l - 12 ! [days]

          ! Keep using the melt season curve until its minimum value
          ! (maximum index) is reached or until there is new snow.
          if (l > MAXALB) l = MAXALB ! [days]

          ! Get the albedo value from the melt season curve.
          albedo = this%amlt(l) ! [fraction of radiation]
        endif
      elseif (iso == 2) then
        ! (2) New snow has reset the albedo and it is melt season.

        ! Set albedo to initial value during melt season.
        ! NOTE: RAPCOMMENT - CHANGED TO ISO FROM MSO
        ! albedo = 0.81  ! [fraction of radiation] original value
        albedo = 0.72  ! [fraction of radiation] value Rob suggested

        ! int_alb is a flag to indicate use of the melt season curve (2)
        ! or accumulation season curve (1).
        ! Set flag to indicate melt season curve.
        int_alb = 2  ! [flag]
      else
        ! (3) New snow has reset the albedo and it is accumulation season.

        ! Set albedo to initial value during accumulation season.
        albedo = 0.91  ! [fraction of radiation]

        ! Set flag to indicate accumulation season curve.
        int_alb = 1  ! [flag]
      endif
    end associate
  end subroutine


  !***********************************************************************
  ! Compute energy balance of snowpack
  !   1st call is for night period, 2nd call for day period
  !***********************************************************************
  module subroutine snowbal(this, cal, model_climate, ctl_data, intcp, model_precip, &
                            chru, month, niteda, cec, cst, esv, sw, temp, trd)
    implicit none

    ! Arguments
    class(Snowcomp), intent(inout) :: this
    real(r32), intent(out) :: cal
      !! Total energy potentially available from atmosphere: longwave, shortwave, and condensation/convection. [cal/cm^2] or [Langleys]
    type(Climateflow), intent(inout) :: model_climate
      ! NOTE: must be inout because some of the called procedures have intent inout
    type(Control), intent(in) :: ctl_data
    ! type(Parameters), intent(in) :: param_data
    type(Interception), intent(in) :: intcp
    class(Precipitation), intent(in) :: model_precip
    integer(i32), intent(in) :: chru
    integer(i32), intent(in) :: month
    integer(i32), intent(in) :: niteda
    real(r32), intent(in) :: cec
    real(r32), intent(in) :: cst
    real(r32), intent(in) :: esv
    real(r32), intent(in) :: sw
    real(r32), intent(in) :: temp
    real(r32), intent(in) :: trd
      !! Measured radiation

    ! Local Variables
    real(r32), PARAMETER :: ONETHIRD = 1.0 / 3.0

    ! integer(i32) :: idx1D
      !! 2D index to 1D array
    real(r32) :: air
      !! Potential long wave energy from air based on temperature [cal/cm^2] or [Langleys]
    real(r32) :: can
      !! Net incoming longwave radiation coming from the canopy [cal/cm^2] or [Langleys]
    real(r32) :: cecsub
      !! Energy from condensation and convection [cal/cm^2] or [Langleys]
    real(r32) :: emis
      !! Fraction of applied perfect black-body emission [fraction]
    real(r32) :: pk_defsub
      !! Heat deficit [cal/cm^2] or [Langleys]
    real(r32) :: pks
      !! Calories required to shift the pack to pkt deficit [cal/cm^2] or [Langleys]
    real(r32) :: pkt
      !! Pack deficit if the snowpack is all at the surface temperature [cal/cm^2] or [Langleys]
    real(r32) :: qcond
      !! Conductive heat flux [cal/cm^2] or [Langleys]
    real(r32) :: sky
      !! Net incoming longwave radiation coming from the sky [cal/cm^2] or [Langleys]
    real(r32) :: sno
      !! [cal/cm^2] or [Langleys]
    real(r32) :: ts
      !! [degree C]

    ! this
    ! pk_def(RW), pk_temp(RW)

    ! --------------------------------------------------------------------------
    ! 2D index to 1D
    ! idx1D = (month - 1) * ctl_data%nhru%value + chru

    associate(freeh2o_cap => this%freeh2o_cap(chru), &
              tstorm_mo => this%tstorm_mo(chru, month), &
              emis_noppt => this%emis_noppt(chru), &

              hru_ppt => model_precip%hru_ppt(chru), &

              pkwater_equiv => model_climate%pkwater_equiv(chru), &

              canopy_covden => intcp%canopy_covden(chru), &

              pk_def => this%pk_def(chru), &
              pk_temp => this%pk_temp(chru))

      !***********************************************************************
      ! Calculate the potential long wave energy from air based on temperature
      ! (assuming perfect black-body emission).
      ! Stefan Boltzmann/2 = (11.71E-8)/2 = 0.585E-7 because add for day and night
      air = 0.585E-7 * ((temp + 273.16)**4.0)  ! [cal/cm^2] or [Langleys]

      ! Set emissivity, which is the fraction of perfect black-body emission
      ! that is actually applied.
      emis = esv  ! [fraction of radiation]

      ! The snowpack surface temperature and long-wave radiation FROM the
      ! snowpack depend on the air temperature (effectively, snowpack temperature
      ! cannot be larger than 0 degC).
      ! 2 options below (if-then, else)
      if (temp < 0.0) then
        ! (1) If the temperature is below freezing, surface snow temperature and
        !     long wave energy are determined by temperature...
        ts = temp  ! [degrees C]
        sno = air  ! [cal/cm^2] or [Langleys]

      ! (2) If the temperature is at or above freezing, snow temperature and
      !     long wave energy are set to values corresponding to a temperature of 0 degC...
      else
        ts = 0.0  ! [degrees C]
        sno = 325.7  ! [cal/cm^2] or [Langleys]
      endif
      if (hru_ppt > 0.0) then
        ! If precipitation over the time period was due to convective thunderstorms,
        ! then the emissivity should be reset.
        if (tstorm_mo == 1) then
          ! The emissivity of air depends on if it is day or night and the
          ! fraction of measured short wave radiation to potential short wave
          ! radiation is used as a surrogate to the duration of the convective
          ! storms.
          ! 2 options below (if-then, else)
          if (niteda==1) then
            ! (1) Night

            ! Set the default emissivity
            emis = 0.85  ! [fraction of radiation]

            ! If measured radiation is greater than 1/3 potential radiation
            ! through the time period, then the emissivity is set to the
            ! "no precipitation" value.
            if (trd > ONETHIRD) emis = emis_noppt  ! [fraction of radiation]
          else
            ! (2) Day

            ! If measured radiation is greater than 1/3 potential radiation but
            ! less than 1/2, then the emissivity is interpolated between
            ! 1.0 and 0.85.
            if (trd > ONETHIRD) emis = 1.29 - (0.882 * trd)  ! [fraction of radiation]

            ! If measured radiation is greater than 1/2 potential radiation,
            ! then the emissivity is interpolated between 0.85 and 0.75.
            if (trd >= 0.5) emis = 0.95 - (0.2 * trd)  ! [fraction of radiation]
          endif
        endif
      endif

      ! Calculate the net incoming long wave radiation coming from the sky or
      ! canopy in the uncovered or covered portions of the snowpack, respectively.
      ! Note that the canopy is assumed to be a perfect blackbody (emissivity=1)
      ! and the air has emissivity as determined from previous calculations.
      sky = (1.0 - canopy_covden) * ((emis * air) - sno)  ! [cal/cm^2] or [Langleys]
      can = canopy_covden * (air - sno)  ! [cal/cm^2] or [Langleys]

      ! RAPCOMMENT - CHECK THE INTERECEPT MODULE FOR CHANGE. What if the land
      !              cover is grass? Is this automatically covered by
      !              canopy_covden being zero if the cover type is grass?

      ! If air temperature is above 0 degC then set the energy from condensation
      ! and convection, otherwise there is no energy from convection or condensation.
      cecsub = 0.0  ! [cal/cm^2] or [Langleys]

      if (temp > 0.0) then
        if (hru_ppt > 0.0) cecsub = cec * temp  ! [cal/cm^2] or [Langleys]
      endif

      ! Total energy potentially available from atmosphere: longwave,
      ! shortwave, and condensation/convection.
      cal = sky + can + cecsub + sw  ! [cal/cm^2] or [Langleys]

      ! If the surface temperature of the snow is 0 degC, and there is net
      ! incoming energy, then energy conduction has to be from the surface into
      ! the snowpack. Therefore, the energy from the atmosphere is applied to
      ! the snowpack and subroutine terminates.
      if (ts >= 0.0) then
        if (cal > 0.0) then
          call this%calin(model_climate, ctl_data, cal, chru)
          return
        endif
      endif

      ! If the program gets to this point, then either the surface temperature
      ! is less than 0 degC, or the total energy from the atmosphere is not
      ! providing energy to the snowpack.

      ! Because the temperature of the surface of the snowpack is assumed to be
      ! controlled by air temperature, there is a potential heat flux due to
      ! conduction between the deeper snowpack and its surface.

      ! Calculate conductive heat flux as a function of the temperature gradient
      ! then set new snowpack conditions depending on the direction of heat flow.
      qcond = cst * (ts - pk_temp)  ! [cal/cm^2] or [Langleys]

      ! RAPCOMMENT - The original equation in the paper implies that the this
      !              equation should be relative to the temperature gradient
      !              in degF, not degC (Anderson 1968).  Which is correct?

      ! The energy flow depends on the direction of conduction and the
      ! temperature of the surface of the snowpack. The total energy from the
      ! atmosphere can only penetrate into the snow pack if the temperature
      ! gradient allows conduction from the surface into the snowpack.
      ! 4 options below (if-then, elseif, elseif, else)
      if (qcond < 0.0) then
        ! (1) Heat is conducted from the snowpack to the surface
        !     (atmospheric energy is NOT applied to snowpack)...

        ! If the temperature of the snowpack is below 0 degC,
        ! add to the heat deficit.  Otherwise, remove heat
        ! from the 0 degC isothermal snow pack.
        if (pk_temp < 0.0) then
          ! Increase the heat deficit (minus a negative) and adjust temperature.
          pk_def = pk_def - qcond  ! [cal/cm^2] or [Langleys]
          pk_temp = -pk_def / sngl(pkwater_equiv * 1.27_dp)  ! [degrees C]
        else
          ! Remove heat from the snowpack.
          call this%caloss(model_climate, qcond, chru)
        endif

        ! NOTE: Even though cal is not applied to the snowpack under this condition,
        ! it maintains its value and the referencing code uses it to calculate
        ! the total energy balance of the snowpack. Right now cal isn't used for
        ! anything outside this subroutine, but care should be taken if it is.
      elseif (qcond < CLOSEZERO) then
        ! (2)  There is no heat conduction, qcond = 0.0

        ! If the pack temperature is isothermal at 0 degC, then apply any
        ! incoming radiation, condensation (latent heat), and convection heat
        ! to the snowpack.
        if (pk_temp >= 0.0) then
          ! It does not appear that the interior of the following if statement
          ! is reachable in its current form, because if these conditions are
          ! true, then the code for surface temperature=0 and cal=positive number
          ! would have run and the subroutine will have terminated.
          if (cal > 0.0) then
            call this%calin(model_climate, ctl_data, cal, chru)
          endif
        endif
      elseif (ts >= 0.0) then
        ! (3) conduction is from the surface to the snowpack and the
        !     surface temperature is 0 degrees C...

        ! Note that cal must be <= 0 for this condition to apply.
        ! Otherwise, the program wouldn't have gotten to this point.

        ! Determine if the conductive heat is enough to overcome the current
        ! heat deficit.
        pk_defsub = pk_def - qcond

        if (pk_defsub < 0.0) then
          ! Deficit is overcome and snowpack becomes isothermal at 0 degC.
          pk_def = 0.0  ! [cal/cm^2] or [Langleys]
          pk_temp = 0.0  ! [degrees C]
        else
          ! Deficit is decreased by conducted heat and temperature is recalculated.
          pk_def = pk_defsub  ! [cal/cm^2] or [Langleys]
          pk_temp = -pk_defsub / sngl(pkwater_equiv * 1.27_dp)  ! [degrees C]
        endif
      else
        ! (4) conduction is from the surface to the snowpack and the
        !     surface temperature is less than 0 degrees C...

        ! Calculate the pack deficit if the snowpack was all at the surface
        ! temperature, then calculate how many calories to shift the pack to
        ! that deficit (pks will be a positive number because the conduction
        ! direction is from the surface into the snowpack).
        pkt = -ts * sngl(pkwater_equiv * 1.27_dp)  ! [cal/cm^2] or [Langleys]
        pks = pk_def - pkt  ! [cal/cm^2] or [Langleys]

        ! Determine if the conducted heat is enough to shift the pack to the
        ! deficit relative to the surface temperature.
        pk_defsub = pks - qcond  ! [cal/cm^2] or [Langleys]

        ! The effect of incoming conducted heat depends on whether it is enough
        ! to bring the snowpack to the same temperature as the surface or not.
        ! 2 options below (if-then, else)
        if (pk_defsub < 0.0) then
          ! (4.1) There is enough conducted heat to bring the deep
          !       snowpack to the surface temperature...

          ! There is enough conduction to change to the new pack deficit.
          pk_def = pkt  ! [cal/cm^2] or [Langleys]
          pk_temp = ts  ! [degrees C]
        else
          ! (4.2) There is not enough conducted heat to bring the deep
          !       snowpack to the surface temperature...

          ! The pack deficit doesn't make it all the way to the surface deficit,
          ! but is decreased relative to the conducted heat. Note that the next
          ! statement is equivalent to pk_def = pk_def - qcond
          pk_def = pk_defsub + pkt  ! [cal/cm^2] or [Langleys]
          pk_temp = -pk_def / sngl(pkwater_equiv * 1.27_dp)  ! [degrees C]
        endif
      endif
    end associate
  end subroutine


  !***********************************************************************
  ! Subroutine to compute snow-covered area
  !***********************************************************************
  module subroutine snowcov(this, chru, ctl_data, model_basin, model_climate, intcp, model_precip)
    use UTILS_PRMS, only: get_array
    implicit none

    ! Arguments
    class(Snowcomp), intent(inout) :: this
    integer(i32), intent(in) :: chru
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Climateflow), intent(in) :: model_climate
    type(Interception), intent(in) :: intcp
    class(Precipitation), intent(in) :: model_precip

    ! Local Variables
    real(r32) :: snowcov_area_ante
      !! Antecedent snow-covered area [fraction]
    real(r64) :: difx
      !! Difference between the maximum snow-covered area and the snow-covered area before the last new snow [inches]
    real(r64) :: dify
      !! Difference between the water equivalent before the last new snow and the previous water equivalent [inches]
    real(r64) :: fracy
      !! Ratio of the unmelted amount of previous new snow in the snow pack to the value of 3/4 of previous new snow [fraction]
    ! real(r32), pointer :: snarea_curve_2d(:, :)
      !! Pointer to 2D version of 1D snarea_curve

    ! this
    ! ai(RW), frac_swe(RW), iasw(RW), pksv(RW), scrv(RW), snowcov_area(RW),
    ! snowcov_areasv(RW),

    ! --------------------------------------------------------------------------
    associate(nhru => model_basin%nhru, &

              net_snow => intcp%net_snow(chru), &

              ! newsnow => model_precip%newsnow(chru), &

              pkwater_equiv => model_climate%pkwater_equiv(chru), &

              ai => this%ai(chru), &
              frac_swe => this%frac_swe(chru), &
              iasw => this%iasw(chru), &
              pksv => this%pksv(chru), &
              pst => this%pst(chru), &
              scrv => this%scrv(chru), &
              snowcov_area => this%snowcov_area(chru), &
              snowcov_areasv => this%snowcov_areasv(chru))

      ! DANGER: TODO: Not the best way to do this; fix it.
      ! snarea_curve_2d => get_array(this%snarea_curve, (/11, nhru/))

      !***********************************************************************
      snowcov_area_ante = snowcov_area

      ! Reset snowcover area to the maximum
      ! snowcov_area = snarea_curve_2d(11, chru)  ! [fraction of area]
      snowcov_area = this%snarea_curve_2d(11, this%hru_deplcrv(chru))  ! [fraction of area]

      ! Track the maximum pack water equivalent for the current snow pack.
      if (pkwater_equiv > pst) pst = pkwater_equiv  ! [inches]

      ! Set ai to the maximum packwater equivalent, but no higher than the
      ! threshold for complete snow cover.
      ai = pst  ! [inches]
      ! DEBUG:
      ! write(*,*) '   ai, pst', ai, pst
      if (ai > this%snarea_thresh(chru)) ai = dble(this%snarea_thresh(chru))  ! [inches]

      ! Calculate the ratio of the current packwater equivalent to the maximum
      ! packwater equivalent for the given snowpack.
      ! DEBUG:
      ! write(*,*) chru, pkwater_equiv, ai, dble(this%snarea_thresh(chru))
      if (ai == 0.0) then
        frac_swe = 0.0
      else
        frac_swe = sngl(pkwater_equiv / ai)  ! [fraction]
      end if

      ! There are 3 potential conditions for the snow area curve:
      ! A. snow is accumulating and the pack is currently at its maximum level.
      ! B. snow is depleting and the area is determined by the snow area curve.
      ! C. new snow has occured on a depleting pack, temporarily resetting to 100% cover.
      ! For case (C), the snow covered area is linearly interpolated between 100%
      ! and the snow covered area before the new snow.
      ! In general, 1/4 of the new snow has to melt before the snow covered area
      ! goes below 100%, and then the remaining 3/4 has to melt to return to the
      ! previous snow covered area.

      ! First, the code decides whether snow is accumulating (A) or not (B/C).
      ! 2 options below (if-then, else)
      if (pkwater_equiv >= ai) then
        ! (1) The pack water equivalent is at the maximum

        ! Stay on the snow area curve (it will be at the maximum because the pack
        ! water equivalent is equal to ai and it can't be higher).
        ! iasw = 0
        iasw = .false.
      else
        ! (2) The pack water equivalent is less than the maximum

        ! If the snowpack isn't accumulating to a new maximum, it is either on the
        ! curve (condition B above) or being interpolated between the previous
        ! place on the curve and 100% (condition C above).
        ! 2 options below (if-then, elseif)
        if (this%newsnow(chru) /= 0) then
          ! (2.1) There was new snow...

          ! New snow will always reset the snow cover to 100%. However, different
          ! states change depending  on whether the previous snow area condition
          ! was on the curve or being interpolated between the curve and 100%.
          ! 2 options below (if-then, else)
          ! if (iasw > 0) then
          if (iasw) then
            ! (2.1.1) The snow area is being interpolated between 100%
            !         and a previous location on the curve...

            ! The location on the interpolated line is based on how much of the
            ! new snow has melted.  Because the first 1/4 of the new snow doesn't
            ! matter, it has to keep track of the current snow pack plus 3/4 of
            ! the new snow.
            scrv = scrv + (0.75_dp * dble(net_snow))  ! [inches]
            ! scrv = pkwater_equiv - (0.25D0*dble(net_snow))) ! [inches]
            !RAPCOMMENT - CHANGED TO INCREMENT THE SCRV VALUE if ALREADY
            !             INTERPOLATING BETWEEN CURVE AND 100%
          else
            ! (2.1.2) The current snow area is on the curve...

            ! If switching from the snow area curve to interpolation between the
            ! curve and 100%, the current state of the snow pack has to be saved
            ! so that the interpolation can continue until back to the original
            ! conditions.
            ! First, set the flag to indicate interpolation between 100% and the
            ! previous area should be done.
            ! iasw = 1  ! [flag]
            iasw = .true.  ! [flag]

            ! Save the current snow covered area (before the new net snow).
            snowcov_areasv = snowcov_area_ante  ! [inches] PAN: this is [fraction]

            ! Save the current pack water equivalent (before the new net snow).
            pksv = pkwater_equiv - dble(net_snow)  ! [inches]

            ! The location on the interpolated line is based on how much of the
            ! new snow has melted.  Because the first 1/4 of the new snow doesn't
            ! matter, it has to keep track of the current snow pack plus 3/4 of
            ! the new snow.
            scrv = pkwater_equiv - (0.25_dp * dble(net_snow))  ! [inches]
          endif

          ! The subroutine terminates here because the snow covered area always
          ! starts at 100% if there is any new snow (no need to reset it from the
          ! maximum value set at the beginning of the subroutine).
          RETURN
        ! elseif (iasw /= 0) then
        elseif (iasw) then
          ! (2.2) There was no new snow, but the snow covered area is currently
          !       being interpolated between 100% from a previous new snow and the
          !       snow covered area before that previous new snow...

          ! If the first 1/4 of the previous new snow has not melted yet, then the
          ! snow covered area is still 100% and the subroutine can terminate.
          if (pkwater_equiv > scrv) RETURN

          ! At this point, the program is almost sure it is interpolating between
          ! the previous snow covered area and 100%, but it is possible that
          ! enough snow has melted to return to the snow covered area curve instead.
          ! 2 options below (if-then, else)
          if (pkwater_equiv >= pksv) then
            ! (2.2.1) The snow pack still has a larger water equivalent than before
            !         the previous new snow.  I.e., new snow has not melted back to
            !         original area...

            ! Do the interpolation between 100% and the snow covered area before
            ! the previous new snow.

            ! Calculate the difference between the maximum snow covered area
            ! (remember that snowcov_area is always set to the maximum value at
            ! this point) and the snow covered area before the last new snow.
            difx = dble(snowcov_area - snowcov_areasv)

            ! Calculate the difference between the water equivalent before the
            ! last new snow and the previous water equivalent plus 3/4 of the last
            ! new snow. In effect, get the value of 3/4 of the previous new snow.
            dify = scrv - pksv  ! [inches]   !gl1098

            ! If 3/4 of the previous new snow is significantly different from
            ! zero, then calculate the ratio of the unmelted amount of previous
            ! new snow in the snow pack to the value of 3/4 of previous new snow.
            ! In effect, this is the fraction of the previous new snow that
            ! determines the current interpolation of snow covered area.
            fracy = 0.0_dp  ! [fraction]   !gl1098
            if (dify > 0.0_dp) fracy = (pkwater_equiv - pksv) / dify  ! [fraction]

            ! Linearly interpolate the new snow covered area.
            snowcov_area = snowcov_areasv + sngl(fracy * difx)  ! [fraction of area]

            ! Terminate the subroutine.
            RETURN
          else
            ! (2.2.2) The snow pack has returned to the snow water equivalent before
            !         the previous new snow. I.e. back to original area before new snow.

            ! Reset the flag to use the snow area curve
            ! iasw = 0  ! [flag]
            iasw = .false.  ! [flag]
          endif
        endif

        ! If this subroutine is still running at this point, then the program
        ! knows that the snow covered area needs to be adjusted according to the
        ! snow covered area curve.  So at this point it must interpolate between
        ! points on the snow covered area curve (not the same as interpolating
        ! between 100% and the previous spot on the snow area depletion curve).
        snowcov_area = this%sca_deplcrv(this%snarea_curve_2d(1:11, this%hru_deplcrv(chru)), frac_swe)

        ! call this%sca_deplcrv(snowcov_area, snarea_curve, frac_swe)
      endif
    end associate
  end subroutine


  !***********************************************************************
  !      Subroutine to compute evaporation from snowpack
  !***********************************************************************
  module subroutine snowevap(this, model_climate, chru, ctl_data, intcp, model_potet)
    implicit none

    ! Arguments
    class(Snowcomp), intent(inout) :: this
    type(Climateflow), intent(inout) :: model_climate
    integer(i32), intent(in) :: chru
    type(Control), intent(in) :: ctl_data
    ! type(Parameters), intent(in) :: param_data
    type(Interception), intent(in) :: intcp
    class(Potential_ET), intent(in) :: model_potet

    ! Local Variables
    real(r32) :: avail_et
      !! Available ET
    real(r32) :: cal
      !! Amount of heat deficit that is removed by sublimating ice [cal/cm^2]
    real(r32) :: ez
      !! Amount of evaporation affecting the snowpack [inches]

    ! this
    ! freeh2o(RW), pk_def(RW), pk_ice(RW), pk_temp(RW), snow_evap(RW),
    ! snowcov_area,

    ! climate
    ! pkwater_equiv(RW),

    ! --------------------------------------------------------------------------
    associate(print_debug => ctl_data%print_debug%value, &

              potet => model_potet%potet(chru), &
              potet_sublim => model_potet%potet_sublim(chru), &

              hru_intcpevap => intcp%hru_intcpevap(chru), &

              pkwater_equiv => model_climate%pkwater_equiv(chru), &

              freeh2o => this%freeh2o(chru), &
              snow_evap => this%snow_evap(chru), &
              snowcov_area => this%snowcov_area(chru), &
              pk_def => this%pk_def(chru), &
              pk_ice => this%pk_ice(chru), &
              pk_temp => this%pk_temp(chru))

      ! ***********************************************************************
      ! The amount of evaporation affecting the snowpack is the total
      ! evaporation potential minus the evaporation from the interception storage.
      ez = potet_sublim * potet * snowcov_area - hru_intcpevap  ! [inches]

      ! The effects of evaporation depend on whether there is any potential for
      ! evaporation, and if the potential evapotation is enough to completely
      ! deplete the snow pack or not.
      ! 3 options below (if-then, elseif, else)
      if (ez < CLOSEZERO) then
        ! (1) There is no potential for evaporation...

        snow_evap = 0.0  ! [inches]
      elseif (ez >= pkwater_equiv) then
        ! (2) Enough potential evaporation to entirely deplete the snowpack...

        ! Set the evaporation to the pack water equivalent and set all snowpack
        ! variables to no-snowpack values.
        snow_evap = sngl(pkwater_equiv)  ! [inches]
        pkwater_equiv = 0.0_dp  ! [inches]
        pk_ice = 0.0  ! [inches]
        pk_def = 0.0  ! [cal/cm^2]
        freeh2o = 0.0  ! [inches]
        pk_temp = 0.0  ! [degrees C]
      else
        ! (3) Potential evaporation only partially depletes snowpack...

        ! Evaporation depletes the amount of ice in the snowpack (sublimation).
        pk_ice = pk_ice - ez

        ! Change the pack conditions according to whether there is any ice left
        ! in the snowpack.
        if (pk_ice < 0.0) then
          ! RAPCOMMENT - CHANGED TO CHECK FOR NEGATIVE PACK ICE
          !              If all pack ice is removed, then there cannot be a
          !              heat deficit.
          pk_ice = 0.0
          pk_def = 0.0
          pk_temp = 0.0
        else
          ! Calculate the amount of heat deficit that is removed by the
          ! sublimating ice. Note that this only changes the heat deficit if the
          ! pack temperature is less than 0 degC.
          cal = pk_temp * ez * 1.27
          pk_def = pk_def + cal
        endif

        ! Remove the evaporated water from the pack water equivalent.
        pkwater_equiv = pkwater_equiv - ez
        snow_evap = ez
      endif

      if (snow_evap < 0.0) then
        pkwater_equiv = pkwater_equiv - dble(Snow_evap)

        if (pkwater_equiv < 0.0_dp) then
          if (print_debug > -1) then
            if (pkwater_equiv < -DNEARZERO) then
              print *, 'snowpack issue, negative pkwater_equiv in snowevap', pkwater_equiv
            endif

            pkwater_equiv = 0.0_dp
          endif
        endif

        snow_evap = 0.0
      endif

      avail_et = potet - hru_intcpevap - snow_evap

      if (avail_et < 0.0) then
        ! print *, 'snow evap', snow_evap, avail_et, pkwater_equiv
        snow_evap = snow_evap + avail_et
        pkwater_equiv = pkwater_equiv - dble(avail_et)

        if (snow_evap < 0.0) then
          pkwater_equiv = pkwater_equiv - dble(snow_evap)

          if (pkwater_equiv < 0.0_dp) then
            if (print_debug > -1) then
              if (pkwater_equiv < -DNEARZERO) then
                print *, 'snowpack issue 2, negative pkwater_equiv in snowevap', pkwater_equiv
              endif
            endif

            pkwater_equiv = 0.0_dp  ! To be sure negative snowpack is ignored
          endif

          snow_evap = 0.0
        endif
      endif
    end associate
  end subroutine

end submodule
