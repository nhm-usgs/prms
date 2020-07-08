submodule (PRMS_INTCP) sm_intcp
contains

  module subroutine init_Interception(this, ctl_data, model_basin, model_transp, model_summary)
    use prms_constants, only: dp
    use UTILS_PRMS, only: open_dyn_param_file, get_first_time
    implicit none

    class(Interception) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Transpiration), intent(in) :: model_transp
    type(Summary), intent(inout) :: model_summary

    integer(i32) :: jj
    integer(i32) :: ierr

    ! -------------------------------------------------------------------------
    associate(covden_sum_dynamic => ctl_data%covden_sum_dynamic%values(1), &
              covden_win_dynamic => ctl_data%covden_win_dynamic%values(1), &
              dyn_covden_flag => ctl_data%dyn_covden_flag%value, &
              dyn_intcp_flag => ctl_data%dyn_intcp_flag%value, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              outVarON_OFF => ctl_data%outVarON_OFF%value, &
              outVar_names => ctl_data%outVar_names, &
              param_hdl => ctl_data%param_file_hdl, &
              print_debug => ctl_data%print_debug%value, &
              save_vars_to_file => ctl_data%save_vars_to_file%value, &
              snow_intcp_dynamic => ctl_data%snow_intcp_dynamic%values(1), &
              srain_intcp_dynamic => ctl_data%srain_intcp_dynamic%values(1), &
              start_time => ctl_data%start_time%values, &
              wrain_intcp_dynamic => ctl_data%wrain_intcp_dynamic%values(1), &

              nhru => model_basin%nhru, &

              transp_on => model_transp%transp_on)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! Parameters
      allocate(this%covden_sum(nhru))
      call param_hdl%get_variable('covden_sum', this%covden_sum)

      allocate(this%covden_win(nhru))
      call param_hdl%get_variable('covden_win', this%covden_win)

      allocate(this%snow_intcp(nhru))
      call param_hdl%get_variable('snow_intcp', this%snow_intcp)

      allocate(this%srain_intcp(nhru))
      call param_hdl%get_variable('srain_intcp', this%srain_intcp)

      allocate(this%wrain_intcp(nhru))
      call param_hdl%get_variable('wrain_intcp', this%wrain_intcp)

      ! NEW VARIABLES and PARAMETERS for APPLICATION RATES
      this%use_transfer_intcp = .false.

      ! if (Water_use_flag==1) then
      !   ! irr_type may not exist if not doing water use so can't use associate
      !   this%use_transfer_intcp = .true.
      !
      !   allocate(this%gain_inches(nhru))
      !   allocate(this%net_apply(nhru))
      !
      !   this%gain_inches = 0.0
      !   this%net_apply = 0.0
      ! endif

      ! NOTE: PAN - intcp_on is not used for anything except as an output variable
      allocate(this%canopy_covden(nhru))
      allocate(this%hru_intcpevap(nhru))
      allocate(this%hru_intcpstor(nhru))
      allocate(this%intcp_changeover(nhru))
      allocate(this%intcp_evap(nhru))
      allocate(this%intcp_form(nhru))
      allocate(this%intcp_on(nhru))
      allocate(this%intcp_stor(nhru))
      allocate(this%intcp_transp_on(nhru))
      allocate(this%net_ppt(nhru))
      allocate(this%net_rain(nhru))
      allocate(this%net_snow(nhru))

      if (print_debug == 1) then
        allocate(this%intcp_stor_ante(nhru))
        this%intcp_stor_ante = 0.0
      end if

      this%canopy_covden = 0.0
      this%hru_intcpevap = 0.0
      this%intcp_changeover = 0.0
      this%intcp_evap = 0.0
      this%intcp_form = 0

      this%net_ppt = 0.0
      this%net_rain = -200.
      this%net_snow = 0.0

      ! if (this%use_transfer_intcp) then
      !   this%basin_hru_apply = 0.0_dp
      !   this%basin_net_apply = 0.0_dp
      ! end if

      ! The restart output variables have to be handled here because the netcdf
      ! read var routine reallocates the variables (which would mess up the
      ! output variable pointers)
      if (init_vars_from_file == 0) then
        this%hru_intcpstor = 0.0
        this%intcp_on = .false.
        this%intcp_stor = 0.0
        this%intcp_transp_on = transp_on
      else
        ! ~~~~~~~~~~~~~~~~~~~~~~~~
        ! Initialize from restart
        call ctl_data%read_restart_variable('hru_intcpstor', this%hru_intcpstor)
        call ctl_data%read_restart_variable('intcp_on', this%intcp_on)
        call ctl_data%read_restart_variable('intcp_stor', this%intcp_stor)
        call ctl_data%read_restart_variable('intcp_transp_on', this%intcp_transp_on)
      endif

      if (save_vars_to_file == 1) then
        ! Create restart variables
        call ctl_data%add_variable('hru_intcpstor', this%hru_intcpstor, 'nhru', 'inches')
        call ctl_data%add_variable('intcp_on', this%intcp_on, 'nhru', 'none')
        call ctl_data%add_variable('intcp_stor', this%intcp_stor, 'nhru', 'inches')
        call ctl_data%add_variable('intcp_transp_on', this%intcp_transp_on, 'nhru', 'none')
      end if

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Connect summary variables that need to be output
      if (outVarON_OFF == 1) then
        do jj = 1, outVar_names%size()
          select case(outVar_names%values(jj)%s)
            case('intcp_form')
              call model_summary%set_summary_var(jj, this%intcp_form)
            case('intcp_on')
              call model_summary%set_summary_var(jj, this%intcp_on)
            case('hru_intcpevap')
              call model_summary%set_summary_var(jj, this%hru_intcpevap)
            case('hru_intcpstor')
              call model_summary%set_summary_var(jj, this%hru_intcpstor)
            case('net_ppt')
              call model_summary%set_summary_var(jj, this%net_ppt)
            case('net_rain')
              call model_summary%set_summary_var(jj, this%net_rain)
            case('net_snow')
              call model_summary%set_summary_var(jj, this%net_snow)
            case default
              ! pass
          end select
        enddo
      endif

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! If requested, open dynamic parameter file(s)
      this%has_dynamic_params = dyn_intcp_flag > 0 .or. dyn_covden_flag > 0

      if (dyn_intcp_flag > 0) then
          ! Open the output unit for summary information
        open(NEWUNIT=this%dyn_output_unit, STATUS='REPLACE', FILE='dyn_' // MODNAME // '.out')
      end if

      if (any([1, 3, 5, 7]==dyn_intcp_flag)) then
        ! Open the wrain_intcp_dynamic file
        call open_dyn_param_file(this%wrain_intcp_unit, ierr, wrain_intcp_dynamic%s, 'wrain_intcp_dynamic')
        if (ierr /= 0) then
          write(output_unit, *) MODNAME, ' ERROR opening dynamic wrain_intcp parameter file.'
          stop
        end if

        this%next_dyn_wrain_intcp_date = get_first_time(this%wrain_intcp_unit, start_time(1:3))
        write(output_unit, *) ' Dynamic wrain_intcp next avail time: ', this%next_dyn_wrain_intcp_date

        allocate(this%wrain_intcp_chgs(nhru))
      end if

      if (any([2, 3, 6, 7]==dyn_intcp_flag)) then
        ! Open the srain_intcp_dynamic file
        call open_dyn_param_file(this%srain_intcp_unit, ierr, srain_intcp_dynamic%s, 'srain_intcp_dynamic')
        if (ierr /= 0) then
          write(output_unit, *) MODNAME, ' ERROR opening dynamic srain_intcp parameter file.'
          stop
        end if

        this%next_dyn_srain_intcp_date = get_first_time(this%srain_intcp_unit, start_time(1:3))
        write(output_unit, *) ' Dynamic srain_intcp next avail time: ', this%next_dyn_srain_intcp_date

        allocate(this%srain_intcp_chgs(nhru))
      end if

      if (any([4, 5, 6, 7]==dyn_intcp_flag)) then
        ! Open the snow_intcp_dynamic file
        call open_dyn_param_file(this%snow_intcp_unit, ierr, snow_intcp_dynamic%s, 'snow_intcp_dynamic')
        if (ierr /= 0) then
          write(output_unit, *) MODNAME, ' ERROR opening dynamic snow_intcp parameter file.'
          stop
        end if

        this%next_dyn_snow_intcp_date = get_first_time(this%snow_intcp_unit, start_time(1:3))
        write(output_unit, *) ' Dynamic snow_intcp next avail time: ', this%next_dyn_snow_intcp_date

        allocate(this%snow_intcp_chgs(nhru))
      end if

      if (any([1, 3]==dyn_covden_flag)) then
        ! Open the covden_sum_dynamic file
        call open_dyn_param_file(this%covden_sum_unit, ierr, covden_sum_dynamic%s, 'covden_sum_dynamic')
        if (ierr /= 0) then
          write(output_unit, *) MODNAME, ' ERROR opening dynamic covden_sum parameter file.'
          stop
        end if

        this%next_dyn_covden_sum_date = get_first_time(this%covden_sum_unit, start_time(1:3))
        write(output_unit, *) ' Dynamic covden_sum next avail time: ', this%next_dyn_covden_sum_date

        allocate(this%covden_sum_chgs(nhru))
      end if

      if (any([2, 3]==dyn_covden_flag)) then
        ! Open the covden_win_dynamic file
        call open_dyn_param_file(this%covden_win_unit, ierr, covden_win_dynamic%s, 'covden_win_dynamic')
        if (ierr /= 0) then
          write(output_unit, *) MODNAME, ' ERROR opening dynamic covden_win parameter file.'
          stop
        end if

        this%next_dyn_covden_win_date = get_first_time(this%covden_win_unit, start_time(1:3))
        write(output_unit, *) ' Dynamic covden_win next avail time: ', this%next_dyn_covden_win_date

        allocate(this%covden_win_chgs(nhru))
      end if
    end associate
  end subroutine


  module subroutine run_Interception(this, ctl_data, model_basin, model_potet, &
                                     model_precip, model_transp, model_climate, &
                                     model_time)
    use prms_constants, only: BARESOIL, GRASSES, SHRUBS, TREES, CONIFEROUS, LAND, &
                              LAKE, NEARZERO, DNEARZERO, IRR_SPRINKLER, IRR_FURROW_DRIP, IRR_IGNORE
    implicit none

    class(Interception) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    class(Potential_ET), intent(in) :: model_potet
    class(Precipitation), intent(in) :: model_precip
    class(Transpiration), intent(in) :: model_transp
    type(Climateflow), intent(in) :: model_climate
      !! Climate variables
    type(Time_t), intent(in) :: model_time

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: j

    real(r32) :: changeover
    real(r32) :: d
    real(r32) :: diff
    real(r32) :: evrn
    real(r32) :: evsn
    real(r32) :: extra_water
      !! used when dynamic cov_type transitions to bare soil and intcpstor > 0.0
    real(r32) :: intcpevap
    real(r32) :: intcpstor
    real(r32) :: last
    real(r32) :: netrain
    real(r32) :: netsnow
    real(r32) :: stor
    real(r32) :: z


    ! TODO: Add the following later
    ! nevap (in potet_pan)
    ! epan_coef (in potet_pan) - but always required in intcp
    ! hru_pansta (in potet_pan)

    ! --------------------------------------------------------------------------
    associate(et_module => ctl_data%et_module%values, &
              print_debug => ctl_data%print_debug%value, &

              Nowmonth => model_time%Nowmonth, &
              Nowyear => model_time%Nowyear, &
              Cfs_conv => model_time%Cfs_conv, &

              nhru => model_basin%nhru, &
              active_mask => model_basin%active_mask, &
              cov_type => model_basin%cov_type, &
              active_hrus => model_basin%active_hrus, &
              hru_area => model_basin%hru_area, &
              hru_route_order => model_basin%hru_route_order, &
              hru_type => model_basin%hru_type, &

              ! epan_coef => model_potet%epan_coef, &
              potet => model_potet%potet, &
              potet_sublim => model_potet%potet_sublim, &

              hru_ppt => model_precip%hru_ppt, &
              hru_rain => model_precip%hru_rain, &
              hru_snow => model_precip%hru_snow, &

              pkwater_equiv => model_climate%pkwater_equiv, &

              transp_on => model_transp%transp_on)

      ! Dynamic parameter read
      if (this%has_dynamic_params) then
        call this%read_dyn_params(ctl_data, model_basin, model_time)
      end if

      ! pkwater_equiv is from last time step
      if (print_debug == 1) then
        this%intcp_stor_ante = this%hru_intcpstor
      endif

      ! zero application rate variables for today
      if (this%use_transfer_intcp) then
        this%net_apply = 0.0
      endif

      this%intcp_form = 0
      where (hru_snow > 0.0)
        this%intcp_form = 1
      end where

      do j=1, active_hrus
        chru = hru_route_order(j)
        ! this%net_ppt(chru) = hru_ppt(chru)
        ! this%net_rain(chru) = hru_rain(chru)
        ! this%net_snow(chru) = hru_snow(chru)
        ! this%intcp_evap(chru) = 0.0
        ! this%hru_intcpevap(chru) = 0.0
        netrain = hru_rain(chru)
        netsnow = hru_snow(chru)

        changeover = 0.0
        extra_water = 0.0
        intcpevap = 0.0

        ! TODO: why is this intcp_stor instead of hru_intcpstor
        intcpstor = this%intcp_stor(chru)

        if (hru_type(chru) == LAKE .or. cov_type(chru) == BARESOIL) then
          ! Lake or bare ground HRUs
          if (cov_type(chru) == BARESOIL .and. intcpstor > 0.0) then
            ! This happens when dynamic cov_type changes from >0 to zero and
            ! intcp_stor has non-zero value from prior timestep.
            ! NOTE: prms5 has extra_water = Hru_intcpstor(i)
            ! extra_water = this%intcp_stor(chru)
            extra_water = this%hru_intcpstor(chru)

            if (print_debug > -1) then
              write(output_unit, *) 'WARNING: cov_type changed to 0 with canopy storage of: ', this%hru_intcpstor(chru)
              write(output_unit, *) '         this storage will be moved to intcp_changeover; HRU: ', chru
            end if

            intcpstor = 0.0
          end if
        end if

        ! ******Adjust interception amounts for changes in summer/winter cover density
        if (transp_on(chru)) then
          this%canopy_covden(chru) = this%covden_sum(chru)
        else
          this%canopy_covden(chru) = this%covden_win(chru)
        end if

        ! *****Determine the amount of interception from rain
        if (.not. transp_on(chru) .and. this%intcp_transp_on(chru)) then
          ! ***** go from summer to winter cover density
          this%intcp_transp_on(chru) = .false.

          if (intcpstor > 0.0) then
            ! assume canopy storage change falls as throughfall
            diff = this%covden_sum(chru) - this%canopy_covden(chru)
            changeover = intcpstor * diff

            if (this%canopy_covden(chru) > 0.0) then
              if (changeover < 0.0) then
                ! covden_win > covden_sum, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor * this%covden_sum(chru) / this%canopy_covden(chru)
                changeover = 0.0
              endif
            else
              if (print_debug > -1) then
                write(output_unit, *) 'covden_win=0 at winter change over with canopy storage, HRU:', chru, &
                                      'intcp_stor:', intcpstor, ' covden_sum:', this%covden_sum(chru)
              endif

              intcpstor = 0.0
            endif
          endif
        elseif (transp_on(chru) .and. .not. this%intcp_transp_on(chru)) then
          ! ****** go from winter to summer cover density, excess = throughfall
          this%intcp_transp_on(chru) = .true.

          if (intcpstor > 0.0) then
            diff = this%covden_win(chru) - this%canopy_covden(chru)
            changeover = intcpstor * diff

            if (this%canopy_covden(chru) > 0.0) then
              if (changeover < 0.0) then
                ! covden_sum > covden_win, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor * this%covden_win(chru) / this%canopy_covden(chru)
                changeover = 0.0
              endif
            else
              if (print_debug > -1) then
                write(output_unit, *) 'covden_sum=0 at summer change over with canopy storage, HRU:', chru, &
                                      'intcp_stor:', intcpstor, ' covden_win:', this%covden_win(chru)
              endif

              intcpstor = 0.0
            endif
          endif
        endif

        ! *****Determine the amount of interception from rain
        if (hru_type(chru) /= LAKE .and. cov_type(chru) /= BARESOIL) then
          if (transp_on(chru)) then
            stor = this%srain_intcp(chru)
          else
            stor = this%wrain_intcp(chru)
          endif

          if (hru_rain(chru) > 0.0) then
            if (this%canopy_covden(chru) > 0.0) then
              if (any([SHRUBS, TREES, CONIFEROUS]==cov_type(chru))) then
                call this%intercept(netrain, intcpstor, this%canopy_covden(chru), &
                                    hru_rain(chru), stor)
              elseif (cov_type(chru) == GRASSES) then
                !rsr, 03/24/2008 intercept rain on snow-free grass, when not a mixed event
                if (pkwater_equiv(chru) < DNEARZERO .and. netsnow < NEARZERO) then
                  call this%intercept(netrain, intcpstor, this%canopy_covden(chru), &
                                      hru_rain(chru), stor)
                  ! rsr 03/24/2008
                  ! It was decided to leave the water in intcpstor rather than put
                  ! the water in the snowpack, as doing so for a mixed event on
                  ! grass with snow-free surface produces a divide by zero in
                  ! snowcomp. Storage on grass will eventually evaporate.
                endif
              endif
            endif
          endif

          ! TODO: The following relies on water_use_read.f90 for this to work
          ! NEXT intercept application of irrigation water, but only if
          ! irrigation method (irr_type=hrumeth) is == IRR_SPRINKLER (0) for sprinkler method
          ! if (this%use_transfer_intcp) then
          !   this%gain_inches(chru) = 0.0
          !
          !   if (canopy_gain(chru) > 0.0) then
          !     if (this%canopy_covden(chru) > 0.0) then
          !       if (irr_type(chru) == IRR_IGNORE) then
          !         print *, 'WARNING, water-use transfer > 0, but irr_type = 2 (ignore), HRU:', chru, ', transfer:', canopy_gain(chru)
          !         canopy_gain(chru) = 0.0
          !       else
          !         this%gain_inches(chru) = canopy_gain(chru) / sngl(cfs_conv) / this%canopy_covden(chru) / hru_area(chru)
          !
          !         if (irr_type(chru) == IRR_SPRINKLER) then
          !           call this%intercept(this%net_apply(chru), intcpstor, &
          !                               this%canopy_covden(chru), this%gain_inches(chru), stor)
          !         else
          !           ! IRR_FURROW_DRIP
          !           this%net_apply(chru) = this%gain_inches(chru)
          !         endif
          !       endif
          !     else
          !       STOP 'ERROR, canopy transfer attempted to HRU with cov_den = 0.0'
          !     endif
          !   endif
          ! endif

          ! ******Determine amount of interception from snow
          if (hru_snow(chru) > 0.0) then
            if (this%canopy_covden(chru) > 0.0) then
              if (any([SHRUBS, TREES, CONIFEROUS]==cov_type(chru))) then
                stor = this%snow_intcp(chru)
                call this%intercept(netsnow, intcpstor, this%canopy_covden(chru), &
                                    hru_snow(chru), stor)

                if (netsnow < NEARZERO) then   !rsr, added 3/9/2006
                  netrain = netrain + netsnow
                  netsnow = 0.0
                endif
              endif
            endif
          endif
        end if

        ! ******compute evaporation or sublimation of interception
        if (intcpstor > 0.0) then
          ! If precipitation assume no evaporation or sublimation
          if (hru_ppt(chru) < NEARZERO) then
            ! NOTE: This differs from PRMS5, epan_coef is only used with
            !       the potet_pan module
            evrn = potet(chru)
            ! evrn = potet(chru) / epan_coef(chru, Nowmonth)
            evsn = potet_sublim(chru) * potet(chru)

            ! TODO: Uncomment when potet_pan module is added
            ! if (nevap > 0 .and. et_module%s == 'potet_pan') then
            !   evrn = pan_evap(hru_pansta(chru))
            !
            !   if (evrn < 0.0) evrn = 0.0
            ! endif

            ! ****** Compute snow interception loss
            if (this%intcp_form(chru) == 1) then
              z = intcpstor - evsn

              if (z > 0.0) then
                intcpevap = evsn
                intcpstor = z
              else
                intcpevap = intcpstor
                intcpstor = 0.0
              endif
            else
              d = intcpstor - evrn

              if (d > 0.0) then
                intcpevap = evrn
                intcpstor = d
              else
                intcpevap = intcpstor
                intcpstor = 0.0
              endif
            endif
          endif
        endif

        if (intcpevap * this%canopy_covden(chru) > potet(chru)) then
          last = intcpevap

          if (this%canopy_covden(chru) > 0.0) then
            intcpevap = potet(chru) / this%canopy_covden(chru)
          else
            intcpevap = 0.0
          endif
          intcpstor = intcpstor + last - intcpevap
        endif

        this%intcp_evap(chru) = intcpevap
        this%hru_intcpevap(chru) = intcpevap * this%canopy_covden(chru)
        this%intcp_stor(chru) = intcpstor

        if (intcpstor > 0.0) then
          this%intcp_on(chru) = .true.
        end if

        this%hru_intcpstor(chru) = intcpstor * this%canopy_covden(chru)
        this%intcp_changeover(chru) = changeover + extra_water

        this%net_rain(chru) = netrain
        this%net_snow(chru) = netsnow
        this%net_ppt(chru) = netrain + netsnow

        if (changeover > 0.0 .and. print_debug > -1) then
          write(output_unit, *) 'Change over storage:', changeover, '; HRU:', chru
        endif
      enddo
    end associate
  end subroutine


  module subroutine cleanup_Interception(this, ctl_data)
    class(Interception), intent(in) :: this
      !! Interception class
    type(Control), intent(in) :: ctl_data

    associate(save_vars_to_file => ctl_data%save_vars_to_file%value)
      if (this%has_dynamic_params) then
        close(this%dyn_output_unit)
      end if

      if (save_vars_to_file == 1) then
        ! Write out this module's restart variables
        call ctl_data%write_restart_variable('hru_intcpstor', this%hru_intcpstor)
        call ctl_data%write_restart_variable('intcp_on', this%intcp_on)
        call ctl_data%write_restart_variable('intcp_stor', this%intcp_stor)
        call ctl_data%write_restart_variable('intcp_transp_on', this%intcp_transp_on)
      end if
    end associate
  end subroutine


  !***********************************************************************
  ! Subroutine to compute interception of rain or snow
  !***********************************************************************
  pure elemental module subroutine intercept(net_precip, intcp_stor, cov, precip, stor_max)
    implicit none

    ! Arguments
    real(r32), intent(out) :: net_precip
    real(r32), intent(inout) :: intcp_stor
      !! Storage on the canopy (not HRU)
    real(r32), intent(in) :: cov
    real(r32), intent(in) :: precip
    real(r32), intent(in) :: stor_max

    !***********************************************************************
    ! NOTE: This isn't called when cov == 0
    net_precip = precip * (1.0 - cov)
    intcp_stor = intcp_stor + precip

    if (intcp_stor > stor_max) then
      net_precip = net_precip + (intcp_stor - stor_max) * cov
      intcp_stor = stor_max
    endif
  end subroutine


  module subroutine read_dyn_params(this, ctl_data, model_basin, model_time)
    use UTILS_PRMS, only: get_next_time, update_parameter, yr_mo_eq_dy_le
    implicit none

    class(Interception), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Basin), intent(in) :: model_basin
    type(Time_t), intent(in) :: model_time

    associate(dyn_covden_flag => ctl_data%dyn_covden_flag%value, &
              dyn_intcp_flag => ctl_data%dyn_intcp_flag%value, &

              nhru => model_basin%nhru, &
              active_hrus => model_basin%active_hrus, &
              hru_area => model_basin%hru_area, &
              hru_route_order => model_basin%hru_route_order, &
              hru_type => model_basin%hru_type, &

              curr_time => model_time%Nowtime)

    ! 0=no; 1=file wrain_intcp_dynamic; 2=file srain_intcp_dynamic;
    ! 4=file snow_intcp_dynamic; additive combinations
    if (any([1, 3, 5, 7]==dyn_intcp_flag)) then
      ! Updates of wrain_intcp
      if (yr_mo_eq_dy_le(this%next_dyn_wrain_intcp_date, curr_time(1:3))) then
      ! if (all(this%next_dyn_wrain_intcp_date == curr_time(1:3))) then
        read(this%wrain_intcp_unit, *) this%next_dyn_wrain_intcp_date, this%wrain_intcp_chgs
        write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: wrain_intcp was updated. ', this%next_dyn_wrain_intcp_date

        ! Update wrain_intcp with new values
        call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%wrain_intcp_chgs, 'wrain_intcp', this%wrain_intcp)
        this%next_dyn_wrain_intcp_date = get_next_time(this%wrain_intcp_unit)
      end if
    endif

    if (any([2, 3, 6, 7]==dyn_intcp_flag)) then
      ! Updates of srain_intcp
      if (yr_mo_eq_dy_le(this%next_dyn_srain_intcp_date, curr_time(1:3))) then
      ! if (all(this%next_dyn_srain_intcp_date == curr_time(1:3))) then
        read(this%srain_intcp_unit, *) this%next_dyn_srain_intcp_date, this%srain_intcp_chgs
        write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: srain_intcp was updated. ', this%next_dyn_srain_intcp_date

        ! Update srain_intcp with new values
        call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%srain_intcp_chgs, 'srain_intcp', this%srain_intcp)
        this%next_dyn_srain_intcp_date = get_next_time(this%srain_intcp_unit)
      end if
    endif

    if (any([4, 5, 6, 7]==dyn_intcp_flag)) then
      ! Updates of snow_intcp
      if (yr_mo_eq_dy_le(this%next_dyn_snow_intcp_date, curr_time(1:3))) then
      ! if (all(this%next_dyn_snow_intcp_date == curr_time(1:3))) then
        read(this%snow_intcp_unit, *) this%next_dyn_snow_intcp_date, this%snow_intcp_chgs
        write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: snow_intcp was updated. ', this%next_dyn_snow_intcp_date

        ! Update snow_intcp with new values
        call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%snow_intcp_chgs, 'snow_intcp', this%snow_intcp)
        this%next_dyn_snow_intcp_date = get_next_time(this%snow_intcp_unit)
      end if
    endif

    if (any([1, 3]==dyn_covden_flag)) then
      ! Updates of covden_sum
      if (yr_mo_eq_dy_le(this%next_dyn_covden_sum_date, curr_time(1:3))) then
      ! if (all(this%next_dyn_covden_sum_date == curr_time(1:3))) then
        read(this%covden_sum_unit, *) this%next_dyn_covden_sum_date, this%covden_sum_chgs
        write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: covden_sum was updated. ', this%next_dyn_covden_sum_date

        ! Update covden_sum with new values
        call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%covden_sum_chgs, 'covden_sum', this%covden_sum)
        this%next_dyn_covden_sum_date = get_next_time(this%covden_sum_unit)
      end if
    end if

    if (any([2, 3]==dyn_covden_flag)) then
      ! Updates of covden_win
      if (yr_mo_eq_dy_le(this%next_dyn_covden_win_date, curr_time(1:3))) then
      ! if (all(this%next_dyn_covden_win_date == curr_time(1:3))) then
        read(this%covden_win_unit, *) this%next_dyn_covden_win_date, this%covden_win_chgs
        write(output_unit, 9008) MODNAME, '%read_dyn_params() INFO: covden_win was updated. ', this%next_dyn_covden_win_date

        ! Update covden_win with new values
        call update_parameter(ctl_data, model_time, this%dyn_output_unit, this%covden_win_chgs, 'covden_win', this%covden_win)
        this%next_dyn_covden_win_date = get_next_time(this%covden_win_unit)
      end if
    endif


    9008 format(A, A, I4, 2('/', I2.2))

    ! ! leave any interception storage unchanged, it will be evaporated based on new values in intcp module
    ! IF (Wrainintcp_flag == 1) THEN
    !   IF (Wrain_intcp_next_mo /= 0) THEN
    !     IF (Wrain_intcp_next_yr == Nowyear .AND. Wrain_intcp_next_mo == Nowmonth .AND. Wrain_intcp_next_day == Nowday) THEN
    !       READ (Wrain_intcp_unit, *) Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day, Temp
    !       CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Wrain_intcp, 'wrain_intcp')
    !       CALL is_eof(Wrain_intcp_unit, Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day)
    !     ENDIF
    !   ENDIF
    ! ENDIF
    end associate
  end subroutine
end submodule
