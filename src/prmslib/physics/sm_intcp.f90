submodule (PRMS_INTCP) sm_intcp
contains
  module function constructor_Interception(ctl_data, model_transp, basin_summary) result(this)
    use prms_constants, only: dp

    type(Interception) :: this
      !! Interception class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    class(Transpiration), intent(in) :: model_transp
    type(Basin_summary_ptr), intent(inout) :: basin_summary
      !! Basin summary

    integer(i32) :: jj

    ! Control
    ! nhru, init_vars_from_file, print_debug

    ! Transpiration
    ! transp_on,

    ! -------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%value, &
              basinOutON_OFF => ctl_data%basinOutON_OFF%value, &
              basinOutVars => ctl_data%basinOutVars%value, &
              basinOutVar_names => ctl_data%basinOutVar_names%values, &
              init_vars_from_file => ctl_data%init_vars_from_file%value, &
              print_debug => ctl_data%print_debug%value, &
              transp_on => model_transp%transp_on)

      call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

      if (print_debug > -2) then
        ! Output module and version information
        call this%print_module_info()
      endif

      ! NEW VARIABLES and PARAMETERS for APPLICATION RATES
      ! this%use_transfer_intcp = 0
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
      if (print_debug == 1) allocate(this%intcp_stor_ante(nhru))

      this%canopy_covden = 0.0
      this%hru_intcpevap = 0.0
      this%hru_intcpstor = 0.0
      this%intcp_changeover = 0.0
      this%intcp_evap = 0.0
      this%intcp_form = 0
      this%intcp_on = .false.
      this%intcp_stor = 0.0
      this%intcp_transp_on = transp_on
      this%net_ppt = 0.0
      this%net_rain = 0.0
      this%net_snow = 0.0

      allocate(this%basin_changeover)
      allocate(this%basin_hru_apply)
      allocate(this%basin_intcp_evap)
      allocate(this%basin_intcp_stor)
      allocate(this%basin_net_apply)
      allocate(this%basin_net_ppt)
      allocate(this%basin_net_rain)
      allocate(this%basin_net_snow)

      this%basin_changeover = 0.0_dp
      this%basin_hru_apply = 0.0_dp
      this%basin_intcp_evap = 0.0_dp
      this%basin_intcp_stor = 0.0_dp
      this%basin_net_apply = 0.0_dp
      this%basin_net_ppt = 0.0_dp
      this%basin_net_rain = 0.0_dp
      this%basin_net_snow = 0.0_dp

      ! Connect any basin summary variables that need to be output
      if (basinOutON_OFF == 1) then
        do jj = 1, basinOutVars
          ! TODO: This is where the daily basin values are linked based on
          !       what was requested in basinOutVar_names.
          select case(basinOutVar_names(jj)%s)
            case('basin_changeover')
              call basin_summary%set_basin_var(jj, this%basin_changeover)
            case('basin_hru_apply')
              call basin_summary%set_basin_var(jj, this%basin_hru_apply)
            case('basin_intcp_evap')
              call basin_summary%set_basin_var(jj, this%basin_intcp_evap)
            case('basin_intcp_stor')
              call basin_summary%set_basin_var(jj, this%basin_intcp_stor)
            case('basin_net_apply')
              call basin_summary%set_basin_var(jj, this%basin_net_apply)
            case('basin_net_ppt')
              call basin_summary%set_basin_var(jj, this%basin_net_ppt)
            case('basin_net_rain')
              call basin_summary%set_basin_var(jj, this%basin_net_rain)
            case('basin_net_snow')
              call basin_summary%set_basin_var(jj, this%basin_net_snow)
            case default
              ! pass
          end select
        enddo
      endif

      if (init_vars_from_file == 1) then
        ! TODO: hook up reading restart file
      endif

    end associate
  end function

  module subroutine cleanup_Interception(this)
    class(Interception) :: this
      !! Interception class

    ! TODO: Add write restart file stuff
  end subroutine

  module subroutine run_Interception(this, ctl_data, param_data, model_basin, &
                                     model_potet, model_precip, model_transp, model_climate, model_time)
    use prms_constants, only: BARESOIL, GRASSES, SHRUBS, TREES, CONIFEROUS, LAND, &
                              LAKE, NEARZERO, DNEARZERO
    implicit none

    class(Interception) :: this
      !! Interception class
    type(Control), intent(in) :: ctl_data
      !! Control file parameters
    type(Parameters), intent(in) :: param_data
      !! Parameters
    type(Basin), intent(in) :: model_basin
      !! Basin variables
    class(Potential_ET), intent(in) :: model_potet
    class(Precipitation), intent(inout) :: model_precip
    class(Transpiration), intent(in) :: model_transp
    type(Climateflow), intent(in) :: model_climate
      !! Climate variables
    type(Time_t), intent(in) :: model_time

    ! NOTE: model_precip must be intent(inout) because newsnow and pptmix are
    !       modified in this subroutine.

    ! Local Variables
    integer(i32) :: chru
    integer(i32) :: j
    integer(i32) :: idx1D
      !! Used for conversion of 2D index to 1D index

    real(r32) :: changeover
    ! real(r32) :: cov
    real(r32) :: d
    real(r32) :: diff
    real(r32) :: evrn
    real(r32) :: evsn
    real(r32) :: intcpevap
    real(r32) :: intcpstor
    real(r32) :: last
    real(r32) :: netrain
    real(r32) :: netsnow
    real(r32) :: stor
    real(r32) :: z

    ! Control
    ! nevap, nhru, et_module, print_debug

    ! Basin
    ! basin_area_inv, active_hrus, hru_route_order

    ! Climate
    ! hru_ppt, hru_rain, hru_snow, newsnow (RW), pkwater_equiv, pptmix (RW),

    ! Parameters
    ! hru_area, hru_pansta, hru_type, covden_sum, covden_win, cov_type, epan_coef,
    ! potet_sublim, snow_intcp, srain_intcp, wrain_intcp,

    ! Potential_ET
    ! potet,

    ! Time_t
    ! Nowmonth, Cfs_conv

    ! Transpiration
    ! transp_on,

    ! --------------------------------------------------------------------------
    associate(nevap => ctl_data%nevap%value, &
              nhru => ctl_data%nhru%value, &
              et_module => ctl_data%et_module%values, &
              print_debug => ctl_data%print_debug%value, &

              Nowmonth => model_time%Nowmonth, &
              Nowyear => model_time%Nowyear, &
              Cfs_conv => model_time%Cfs_conv, &

              basin_area_inv => model_basin%basin_area_inv, &
              active_hrus => model_basin%active_hrus, &
              hru_route_order => model_basin%hru_route_order, &

              hru_area => param_data%hru_area%values, &
              hru_pansta => param_data%hru_pansta%values, &
              hru_type => param_data%hru_type%values, &
              covden_sum => param_data%covden_sum%values, &
              covden_win => param_data%covden_win%values, &
              cov_type => param_data%cov_type%values, &
              epan_coef => param_data%epan_coef%values, &
              potet_sublim => param_data%potet_sublim%values, &
              snow_intcp => param_data%snow_intcp%values, &
              srain_intcp => param_data%srain_intcp%values, &
              wrain_intcp => param_data%wrain_intcp%values, &

              potet => model_potet%potet, &

              hru_ppt => model_precip%hru_ppt, &
              hru_rain => model_precip%hru_rain, &
              hru_snow => model_precip%hru_snow, &
              newsnow => model_precip%newsnow, &
              pptmix => model_precip%pptmix, &

              pkwater_equiv => model_climate%pkwater_equiv, &

              transp_on => model_transp%transp_on)

      ! pkwater_equiv is from last time step
      if (print_debug == 1) then
        this%intcp_stor_ante = this%hru_intcpstor
        this%last_intcp_stor = this%basin_intcp_stor
      endif

      this%basin_changeover = 0.0_dp
      this%basin_intcp_evap = 0.0_dp
      this%basin_intcp_stor = 0.0_dp
      this%basin_net_ppt = 0.0_dp
      this%basin_net_rain = 0.0_dp
      this%basin_net_snow = 0.0_dp

      ! zero application rate variables for today
      ! if (this%use_transfer_intcp == 1) then
      if (this%use_transfer_intcp) then
        this%basin_hru_apply = 0.0_dp
        this%basin_net_apply = 0.0_dp
        this%net_apply = 0.0
      endif

      do j=1, active_hrus
        chru = hru_route_order(j)
        this%net_ppt(chru) = hru_ppt(chru)

        ! 2D index to 1D
        idx1D = (Nowmonth - 1) * nhru + chru

        if (hru_type(chru) == LAKE .or. cov_type(chru) == BARESOIL) then
          ! Lake or bare ground HRUs
          this%net_rain(chru) = hru_rain(chru)
          this%net_snow(chru) = hru_snow(chru)
          this%basin_net_ppt = this%basin_net_ppt + dble(this%net_ppt(chru) * hru_area(chru))
          this%basin_net_snow = this%basin_net_snow + dble(hru_snow(chru) * hru_area(chru))
          this%basin_net_rain = this%basin_net_rain + dble(hru_rain(chru) * hru_area(chru))
          CYCLE
        endif

        changeover = 0.0
        intcpevap = 0.0
        intcpstor = this%intcp_stor(chru)
        netrain = hru_rain(chru)
        netsnow = hru_snow(chru)
        this%intcp_form(chru) = 0

        ! ******Adjust interception amounts for changes in summer/winter cover density
        if (transp_on(chru)) then
          this%canopy_covden(chru) = covden_sum(chru)
        else
          this%canopy_covden(chru) = covden_win(chru)
        endif

        ! *****Determine the amount of interception from rain
        if (.not. transp_on(chru) .and. this%intcp_transp_on(chru)) then
          ! ***** go from summer to winter cover density
          this%intcp_transp_on(chru) = .false.

          if (intcpstor > 0.0) then
            ! assume canopy storage change falls as throughfall
            diff = covden_sum(chru) - this%canopy_covden(chru)
            changeover = intcpstor * diff

            if (this%canopy_covden(chru) > 0.0) then
              if (changeover < 0.0) then
                ! covden_win > covden_sum, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor * covden_sum(chru) / this%canopy_covden(chru)
                changeover = 0.0
              endif
            else
              if (print_debug > -1) then
                print *, 'covden_win=0 at winter change over with canopy storage, HRU:', chru, &
                         'intcp_stor:', intcpstor, ' covden_sum:', covden_sum(chru)
              endif

              intcpstor = 0.0
              this%intcp_on(chru) = .false.
            endif
          endif
        elseif (transp_on(chru) .and. .not. this%intcp_transp_on(chru)) then
          ! ****** go from winter to summer cover density, excess = throughfall
          this%intcp_transp_on(chru) = .true.

          if (intcpstor > 0.0) then
            diff = covden_win(chru) - this%canopy_covden(chru)
            changeover = intcpstor * diff

            if (this%canopy_covden(chru) > 0.0) then
              if (changeover < 0.0) then
                ! covden_sum > covden_win, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor * covden_win(chru) / this%canopy_covden(chru)
                changeover = 0.0
              endif
            else
              if (print_debug > -1) then
                print *, 'covden_sum=0 at summer change over with canopy storage, HRU:', chru, &
                         'intcp_stor:', intcpstor, ' covden_win:', covden_win(chru)
              endif

              intcpstor = 0.0
              this%intcp_on(chru) = .false.
            endif
          endif
        endif

        ! *****Determine the amount of interception from rain
        ! if (transp_on(chru) == 1) then
        if (transp_on(chru)) then
          stor = srain_intcp(chru)
        else
          stor = wrain_intcp(chru)
        endif

        if (hru_rain(chru) > 0.0) then
          if (this%canopy_covden(chru) > 0.0) then
            if (any([SHRUBS, TREES, CONIFEROUS]==cov_type(chru))) then
              call this%intercept(this%intcp_on(chru), netrain, intcpstor, this%canopy_covden(chru), &
                                  hru_rain(chru), stor)
            elseif (cov_type(chru) == GRASSES) then
              !rsr, 03/24/2008 intercept rain on snow-free grass, when not a mixed event
              if (pkwater_equiv(chru) < DNEARZERO .and. netsnow < NEARZERO) then
                call this%intercept(this%intcp_on(chru), netrain, intcpstor, &
                                    this%canopy_covden(chru), hru_rain(chru), stor)
                ! rsr 03/24/2008
                ! It was decided to leave the water in intcpstor rather than put
                ! the water in the snowpack, as doing so for a mixed event on
                ! grass with snow-free surface produces a divide by zero in
                ! snowcomp. Storage on grass will eventually evaporate.
              endif
            endif
          endif
        endif

        if (changeover > 0.0) then
          if (print_debug > -1) then
            print *, 'Change over storage added to rain throughfall:', changeover, '; HRU:', chru
          endif

          netrain = netrain + changeover
          this%basin_changeover = this%basin_changeover + dble(changeover * hru_area(chru))
        endif

        ! TODO: The following relies on water_use_read.f90 for this to work
        ! NEXT intercept application of irrigation water, but only if
        !  irrigation method (irr_type=hrumeth) is =0 for sprinkler method
        ! if (this%use_transfer_intcp) then
        !   this%gain_inches(chru) = 0.0
        !
        !   if (canopy_gain(chru) > 0.0) then
        !     if (this%canopy_covden(chru) > 0.0) then
        !       if (irr_type(chru) == 2) then
        !         print *, 'WARNING, water-use transfer > 0, but irr_type = 2 (ignore), HRU:', chru, ', transfer:', canopy_gain(chru)
        !         canopy_gain(chru) = 0.0
        !       else
        !         this%gain_inches(chru) = canopy_gain(chru) / sngl(cfs_conv) / this%canopy_covden(chru) / hru_area(chru)
        !
        !         if (irr_type(chru) == 0) then
        !           call this%intercept(this%intcp_on(chru), this%net_apply(chru), intcpstor, &
        !                               this%canopy_covden(chru), this%gain_inches(chru), stor)
        !         else ! Hrumeth=1
        !           this%net_apply(chru) = this%gain_inches(chru)
        !         endif
        !       endif
        !
        !       this%basin_hru_apply = this%basin_hru_apply + dble(this%gain_inches(chru) * hru_area(chru))
        !       this%basin_net_apply = this%basin_net_apply + dble(this%net_apply(chru) * hru_area(chru))
        !     else
        !       STOP 'ERROR, canopy transfer attempted to HRU with cov_den = 0.0'
        !     endif
        !   endif
        ! endif

        ! ******Determine amount of interception from snow
        if (hru_snow(chru) > 0.0) then
          if (this%canopy_covden(chru) > 0.0) then
            this%intcp_form(chru) = 1

            if (any([SHRUBS, TREES, CONIFEROUS]==cov_type(chru))) then
              stor = snow_intcp(chru)
              call this%intercept(this%intcp_on(chru), netsnow, intcpstor, this%canopy_covden(chru), &
                                  hru_snow(chru), stor)

              if (netsnow < NEARZERO) then   !rsr, added 3/9/2006
                netrain = netrain + netsnow
                netsnow = 0.0
                newsnow(chru) = 0
                pptmix(chru) = 0   ! reset to be sure it is zero
              endif
            endif
          endif
        endif

        this%net_ppt(chru) = netrain + netsnow

        ! ******compute evaporation or sublimation of interception
        if (this%intcp_on(chru)) then
          ! If precipitation assume no evaporation or sublimation
          if (hru_ppt(chru) < NEARZERO) then
            evrn = potet(chru) / epan_coef(idx1D)
            evsn = potet_sublim(chru) * potet(chru)

            ! TODO: Uncomment when potet_pan module is added
            ! if (nevap > 0 .and. et_module%s == 'potet_pan') then
            !   evrn = pan_evap(hru_pansta(chru))
            !
            !   if (evrn < 0.0) evrn = 0.0
            ! endif

            ! ******Compute snow interception loss
            if (this%intcp_form(chru) == 1) then
              z = intcpstor - evsn

              if (z > 0.0) then
                intcpevap = evsn
                intcpstor = z
                this%intcp_on(chru) = .true.
              else
                intcpevap = intcpstor
                intcpstor = 0.0
                this%intcp_on(chru) = .false.
              endif
            ! elseif ( Intcp_form(chru)==0 ) then
            else
              d = intcpstor - evrn

              if (d > 0.0) then
                intcpevap = evrn
                intcpstor = d
                this%intcp_on(chru) = .true.
              else
                intcpevap = intcpstor
                intcpstor = 0.0
                this%intcp_on(chru) = .false.
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
        this%hru_intcpstor(chru) = intcpstor * this%canopy_covden(chru)
        this%intcp_changeover(chru) = changeover

        this%net_rain(chru) = netrain
        this%net_snow(chru) = netsnow

        !rsr, question about depression storage for basin_net_ppt???
        !     My assumption is that cover density is for the whole HRU
        this%basin_net_ppt = this%basin_net_ppt + dble(this%net_ppt(chru) * hru_area(chru))
        this%basin_net_snow = this%basin_net_snow + dble(this%net_snow(chru) * hru_area(chru))
        this%basin_net_rain = this%basin_net_rain + dble(this%net_rain(chru) * hru_area(chru))
        this%basin_intcp_stor = this%basin_intcp_stor + dble(intcpstor * this%canopy_covden(chru) * hru_area(chru))
        this%basin_intcp_evap = this%basin_intcp_evap + dble(intcpevap * this%canopy_covden(chru) * hru_area(chru))

      enddo

      this%basin_net_ppt = this%basin_net_ppt * basin_area_inv
      this%basin_net_snow = this%basin_net_snow * basin_area_inv
      this%basin_net_rain = this%basin_net_rain * basin_area_inv
      this%basin_intcp_stor = this%basin_intcp_stor * basin_area_inv
      this%basin_intcp_evap = this%basin_intcp_evap * basin_area_inv
      this%basin_changeover = this%basin_changeover * basin_area_inv

      ! if (this%use_transfer_intcp == 1) then
      if (this%use_transfer_intcp) then
        this%basin_net_apply = this%basin_net_apply * basin_area_inv
        this%basin_hru_apply = this%basin_hru_apply * basin_area_inv
      endif
    end associate
  end subroutine


  !***********************************************************************
  ! Subroutine to compute interception of rain or snow
  !***********************************************************************
  module subroutine intercept(intcp_on, net_precip, intcp_stor, cov, precip, stor_max)
  ! module subroutine intercept(precip, stor_max, cov, intcp_on, intcp_stor, net_precip)
    implicit none

    ! Arguments
    ! integer(i32), intent(out) :: intcp_on
    logical, intent(out) :: intcp_on
    real(r32), intent(out) :: net_precip
    real(r32), intent(inout) :: intcp_stor
    real(r32), intent(in) :: cov
    real(r32), intent(in) :: precip
    real(r32), intent(in) :: stor_max

    !***********************************************************************
    intcp_on = .true.

    net_precip = precip * (1.0 - cov)
    intcp_stor = intcp_stor + precip

    if (intcp_stor > stor_max) then
      net_precip = net_precip + (intcp_stor - stor_max) * cov
      intcp_stor = stor_max
    endif
  end subroutine

end submodule
