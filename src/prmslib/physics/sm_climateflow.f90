submodule (PRMS_CLIMATEVARS) sm_climateflow

contains
  !***********************************************************************
  ! Climateflow constructor
  module function constructor_Climateflow(ctl_data, param_data) result(this)
    use conversions_mod, only: c_to_f, f_to_c
    use UTILS_PRMS, only: print_module_info, get_array
    implicit none

    type(Climateflow) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data

    ! Local variables
    integer(r32) :: i
    integer(r32) :: j
    ! integer(r32) :: idx1D
      !! 1D index from 2D

    real(r32), pointer :: tmax_allsnow_2d(:, :)
    real(r32), pointer :: tmax_allrain_offset_2d(:, :)

    ! ------------------------------------------------------------------------
    associate(nhru => ctl_data%nhru%values(1), &
              nmonths => ctl_data%nmonths%values(1), &
              print_debug => ctl_data%print_debug%value, &
              tmax_allsnow => param_data%tmax_allsnow%values, &
              tmax_allrain_offset => param_data%tmax_allrain_offset%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      ! TODO: how to handle when init_vars_from_file == 1 (during init)?
      ! TODO: how to handle when save_vars_to_file == 1 (during clean)?

      ! real(r32), allocatable :: tmax_allrain(:, :)

      ! Get 2d-indexed pointers to 1d arrays
      tmax_allsnow_2d => get_array(tmax_allsnow, (/nhru, nmonths/))
      tmax_allrain_offset_2d => get_array(tmax_allrain_offset, (/nhru, nmonths/))


      allocate(this%hru_ppt(nhru))
      allocate(this%hru_rain(nhru))
      allocate(this%hru_snow(nhru))

      allocate(this%potet(nhru))
      allocate(this%prmx(nhru))
      allocate(this%swrad(nhru))
      allocate(this%tavgf(nhru), this%tavgc(nhru))
      allocate(this%tmaxf(nhru), this%tmaxc(nhru))
      allocate(this%tminf(nhru), this%tminc(nhru))
      allocate(this%tmax_hru(nhru), this%tmin_hru(nhru))

      allocate(this%tmax_allrain_f(nhru, 12))
      allocate(this%tmax_allsnow_c(nhru, 12), this%tmax_allsnow_f(nhru, 12))
      allocate(this%tmax_aspect_adjust(nhru, 12), this%tmin_aspect_adjust(nhru, 12))

      allocate(this%newsnow(nhru))
      allocate(this%pptmix(nhru))
      allocate(this%transp_on(nhru))

      allocate(this%tdiff_arr(nhru))

      this%hru_ppt = 0.0
      this%hru_rain = 0.0
      this%hru_snow = 0.0
      this%potet = 0.0
      this%prmx = 0.0
      this%solrad_tmax = 0.0
      this%solrad_tmin = 0.0
      this%swrad = 0.0
      this%tavgc = 0.0
      this%tavgf = 0.0
      this%tmax_hru = 0.0
      this%tmaxc = 0.0
      this%tmaxf = 0.0
      this%tmin_hru = 0.0
      this%tminc = 0.0
      this%tminf = 0.0

      this%pptmix = 0
      this%newsnow = 0
      this%transp_on = 0

      ! TODO: Figure out how to check this correctly
      ! if (any(['ddsolrad', 'ccsolrad']==ctl_data%solrad_module%values(1)%s) .or. &
      !     ctl_data%model_mode == 'DOCUMENTATION') then
        allocate(this%orad_hru(nhru))
        this%orad_hru = 0.0
      ! endif

      ! ------------------------------------------------------------------------
      ! Set tmax_allrain in units of the input values
      ! tmax_allsnow must be in the units of the input values
      if (param_data%temp_units%values(1) == FAHRENHEIT) then
        this%tmax_allsnow_f = reshape(tmax_allsnow, shape(this%tmax_allsnow_f))

        this%tmax_allrain_f = tmax_allsnow_2d + tmax_allrain_offset_2d
        this%tmax_allsnow_c = f_to_c(tmax_allsnow_2d)

        this%tmax_allrain = this%tmax_allrain_f
      else
        ! Celsius
        this%tmax_allsnow_c = reshape(tmax_allsnow, shape(this%tmax_allsnow_c))

        this%tmax_allsnow_f = c_to_f(tmax_allsnow_2d)

        this%tmax_allrain = tmax_allsnow_2d + tmax_allrain_offset_2d
        this%tmax_allrain_f = c_to_f(this%tmax_allrain)
      endif
    end associate
  end function


  module function module_name() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODNAME
  end function


  module function version() result(res)
    implicit none

    character(:), allocatable :: res

    res = MODVERSION
  end function


  module subroutine cleanup(this, ctl_data)
    use Control_class, only: Control
    implicit none

    class(Climateflow), intent(in) :: this
    type(Control), intent(in) :: ctl_data

    ! ------------------------------------------------------------------------
    associate(rst_unit => ctl_data%restart_output_unit, &
              solrad_module => ctl_data%solrad_module%values(1))

      write(rst_unit) MODNAME
      write(rst_unit) this%basin_ppt, this%basin_rain, this%basin_snow, &
                      this%basin_obs_ppt, this%basin_temp, this%basin_orad, &
                      this%basin_tmax, this%basin_tmin, this%solrad_tmax, &
                      this%solrad_tmin, this%basin_transp_on, this%basin_potet, &
                      this%basin_horad, this%basin_swrad
      write(rst_unit) this%tmax_hru
      write(rst_unit) this%tmin_hru
      write(rst_unit) this%newsnow
      write(rst_unit) this%pptmix
      write(rst_unit) this%hru_ppt
      write(rst_unit) this%hru_rain
      write(rst_unit) this%hru_snow
      write(rst_unit) this%prmx
      write(rst_unit) this%tmaxf
      write(rst_unit) this%tminf
      write(rst_unit) this%tavgf
      write(rst_unit) this%tmaxc
      write(rst_unit) this%tminc
      write(rst_unit) this%tavgc
      write(rst_unit) this%transp_on
      write(rst_unit) this%potet
      write(rst_unit) this%swrad

      if (solrad_module%s == 'ddsolrad' .or. solrad_module%s == 'ccsolrad') then
        write(rst_unit) this%orad_hru
      endif
    end associate
  end subroutine



  !***********************************************************************
  !     Write or read restart file
  !***********************************************************************
  ! subroutine climateflow_restart(this, in_out)
  !   ! use PRMS_MODULE, only: Restart_outunit, Restart_inunit ! , Solrad_module
  !   ! use UTILS_PRMS, only: check_restart
  !   implicit none
  !
  !   ! Arguments
  !   class(Climateflow), intent(inout) :: this
  !   integer(i32), intent(in) :: in_out
  !
  !   ! Local Variables
  !   character(LEN=11) :: module_name
  !
  !   !***********************************************************************
  !   if (In_out == 0) then
  !     write(Restart_outunit) MODNAME
  !     write(Restart_outunit) this%basin_ppt, this%basin_rain, this%basin_snow, &
  !                            this%basin_obs_ppt, this%basin_temp, &
  !                            this%basin_orad, this%basin_tmax, this%basin_tmin, &
  !                            this%solrad_tmax, this%solrad_tmin, &
  !                            this%basin_transp_on, this%basin_potet, &
  !                            this%basin_horad, this%basin_swrad
  !     write(Restart_outunit) this%tmax_hru
  !     write(Restart_outunit) this%tmin_hru
  !     write(Restart_outunit) this%newsnow
  !     write(Restart_outunit) this%pptmix
  !     write(Restart_outunit) this%hru_ppt
  !     write(Restart_outunit) this%hru_rain
  !     write(Restart_outunit) this%hru_snow
  !     write(Restart_outunit) this%prmx
  !     write(Restart_outunit) this%tmaxf
  !     write(Restart_outunit) this%tminf
  !     write(Restart_outunit) this%tavgf
  !     write(Restart_outunit) this%tmaxc
  !     write(Restart_outunit) this%tminc
  !     write(Restart_outunit) this%tavgc
  !     write(Restart_outunit) this%transp_on
  !     write(Restart_outunit) this%potet
  !     write(Restart_outunit) this%swrad
  !
  !     ! if (ANY(['ddsolrad', 'ccsolrad']==Solrad_module)) write(Restart_outunit) this%orad_hru
  !   else
  !     read(Restart_inunit) module_name
  !     call check_restart(MODNAME, module_name)
  !     read(Restart_inunit) this%basin_ppt, this%basin_rain, this%basin_snow, &
  !                           this%basin_obs_ppt, this%basin_temp, &
  !                           this%basin_orad, this%basin_tmax, this%basin_tmin, &
  !                           this%solrad_tmax, this%solrad_tmin, &
  !                           this%basin_transp_on, this%basin_potet, &
  !                           this%basin_horad, this%basin_swrad
  !     read(Restart_inunit) this%tmax_hru
  !     read(Restart_inunit) this%tmin_hru
  !     read(Restart_inunit) this%newsnow
  !     read(Restart_inunit) this%pptmix
  !     read(Restart_inunit) this%hru_ppt
  !     read(Restart_inunit) this%hru_rain
  !     read(Restart_inunit) this%hru_snow
  !     read(Restart_inunit) this%prmx
  !     read(Restart_inunit) this%tmaxf
  !     read(Restart_inunit) this%tminf
  !     read(Restart_inunit) this%tavgf
  !     read(Restart_inunit) this%tmaxc
  !     read(Restart_inunit) this%tminc
  !     read(Restart_inunit) this%tavgc
  !     read(Restart_inunit) this%transp_on
  !     read(Restart_inunit) this%potet
  !     read(Restart_inunit) this%swrad
  !
  !     ! if (ANY(['ddsolrad', 'ccsolrad']==Solrad_module)) read(Restart_inunit) this%orad_hru
  !   endif
  ! end subroutine climateflow_restart

  !***********************************************************************
  !     Computes precipitation form (rain, snow or mix) and depth for each HRU
  !***********************************************************************
  ! module subroutine precip_form(this, ihru, month, hru_area, adjmix_rain, rain_adj, &
  !                               snow_adj, precip, sum_obs)
  !   use prms_constants, only: NEARZERO
  !   implicit none
  !
  !   ! Functions
  !   INTRINSIC ABS, DBLE
  !
  !   ! Arguments
  !   class(Climateflow), intent(inout) :: this
  !   integer(i32), intent(in) :: ihru
  !   integer(i32), intent(in) :: month
  !   real(r32), intent(in) :: hru_area
  !   real(r32), intent(in) :: adjmix_rain
  !   real(r32), intent(in) :: rain_adj
  !   real(r32), intent(in) :: snow_adj
  !   real(r32), intent(inout) :: precip
  !   real(r64), intent(inout) :: sum_obs
  !
  !   ! Local Variables
  !   real(r32) :: tdiff
  !
  !   !***********************************************************************
  !   ! basin precipitation before adjustments
  !   ! sum_obs = sum_obs + DBLE(precip * hru_area)
  !
  !   if (this%tmaxf(ihru) <= this%tmax_allsnow_f(ihru, month)) then
  !     !****** If maximum temperature is below or equal to the base temperature
  !     !****** for snow then precipitation is all snow
  !     this%hru_ppt(ihru) = precip * snow_adj
  !     this%hru_snow(ihru) = this%hru_ppt(ihru)
  !     this%newsnow(ihru) = 1
  !   elseif (this%tminf(ihru) > this%tmax_allsnow_f(ihru, month) .OR. this%tmaxf(ihru) >= this%tmax_allrain_f(ihru, month)) then
  !     !****** If minimum temperature is above base temperature for snow or
  !     !****** maximum temperature is above all_rain temperature then
  !     !****** precipitation is all rain
  !     this%hru_ppt(ihru) = precip * rain_adj
  !     this%hru_rain(ihru) = this%hru_ppt(ihru)
  !     this%prmx(ihru) = 1.0
  !   else
  !     !****** Otherwise precipitation is a mixture of rain and snow
  !     tdiff = this%tmaxf(ihru) - this%tminf(ihru)
  !
  !     if (tdiff < 0.0) then
  !       PRINT *, 'ERROR, tmax < tmin (degrees Fahrenheit), tmax:', this%tmaxf(ihru), ' tmin:', this%tminf(ihru)
  !       ! call print_date(1)
  !     endif
  !
  !     if (ABS(tdiff) < NEARZERO) tdiff = 0.0001
  !
  !     this%prmx(ihru) = ((this%tmaxf(ihru) - this%tmax_allsnow_f(ihru, month)) / tdiff) * adjmix_rain
  !     if (this%prmx(ihru) < 0.0) this%prmx(ihru) = 0.0
  !
  !     !****** Unless mixture adjustment raises the proportion of rain to
  !     !****** greater than or equal to 1.0 in which case it all rain
  !     !****** If not, it is a rain/snow mixture
  !     if (this%prmx(ihru) < 1.0) then
  !       this%pptmix(ihru) = 1
  !       this%hru_ppt(ihru) = precip * snow_adj
  !       this%hru_rain(ihru) = this%prmx(ihru) * this%hru_ppt(ihru)
  !       this%hru_snow(ihru) = this%hru_ppt(ihru) - this%hru_rain(ihru)
  !       this%newsnow(ihru) = 1
  !     else
  !       this%hru_ppt(ihru) = precip * rain_adj
  !       this%hru_rain(ihru) = this%hru_ppt(ihru)
  !       this%prmx(ihru) = 1.0
  !     endif
  !   endif
  !
  !   ! this%basin_ppt = this%basin_ppt + DBLE(this%hru_ppt(ihru) * hru_area)
  !   ! this%basin_rain = this%basin_rain + DBLE(this%hru_rain(ihru) * hru_area)
  !   ! this%basin_snow = this%basin_snow + DBLE(this%hru_snow(ihru) * hru_area)
  ! end subroutine precip_form

  module subroutine set_precipitation_form(this, ctl_data, param_data, model_basin, &
                                           month, rain_adj, snow_adj, rainmix_adj)
    use prms_constants, only: NEARZERO, INCHES, MM, MM2INCH
    implicit none

    class(Climateflow), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    integer(i32), intent(in) :: month
    real(r32), optional, intent(in) :: rain_adj(:)
      !! Array of rain adjustments
    real(r32), optional, intent(in) :: snow_adj(:)
      !! Array of snow adjustments
    real(r32), optional, intent(in) :: rainmix_adj(:)
      !! Array of rain mixture adjustments

    ! -------------------------------------------------------------------------
    this%tdiff_arr = this%tmaxf - this%tminf
    where (abs(this%tdiff_arr) < NEARZERO)
      this%tdiff_arr = 0.0001
    end where

    ! Convert precipitation to inches if required
    if (param_data%precip_units%values(1) == MM) then
      where (this%hru_ppt > 0.0) this%hru_ppt = this%hru_ppt * MM2INCH
    endif

    associate(basin_area_inv => model_basin%basin_area_inv, &
              hru_area => param_data%hru_area%values)

      ! Basin precipitation before any adjustments
      this%basin_obs_ppt = sum(dble(this%hru_ppt * hru_area)) * basin_area_inv

      !******Initialize HRU variables
      this%pptmix = 0
      this%newsnow = 0
      this%prmx = 0.0
      this%hru_rain = 0.0
      this%hru_snow = 0.0

      ! TODO: how to handle tmax_allsnow_f??
      where (this%tmaxf <= this%tmax_allsnow_f(:, month))
        this%hru_ppt = this%hru_ppt * snow_adj
        this%hru_snow = this%hru_ppt
        this%newsnow = 1
      elsewhere (this%tminf > this%tmax_allsnow_f(:, month) .or. &
                 this%tmaxf >= this%tmax_allrain_f(:, month))
        this%hru_ppt = this%hru_ppt * rain_adj
        this%hru_rain = this%hru_ppt
        this%prmx = 1.0
      elsewhere
        this%prmx = ((this%tmaxf - this%tmax_allsnow_f(:, month)) / this%tdiff_arr) * rainmix_adj

        where (this%prmx < 0.0)
          this%prmx = 0.0
        end where

        where (this%prmx < 1.0)
          this%pptmix = 1
          this%hru_ppt = this%hru_ppt * snow_adj
          this%hru_rain = this%prmx * this%hru_ppt
          this%hru_snow = this%hru_ppt - this%hru_rain
          this%newsnow = 1
        elsewhere
          this%hru_ppt = this%hru_ppt * rain_adj
          this%hru_rain = this%hru_ppt
          this%prmx = 1.0
        end where
      end where

      this%basin_ppt = sum(DBLE(this%hru_ppt * hru_area)) * basin_area_inv
      this%basin_rain = sum(DBLE(this%hru_rain * hru_area)) * basin_area_inv
      this%basin_snow = sum(DBLE(this%hru_snow * hru_area)) * basin_area_inv
    end associate
  end subroutine


  !***********************************************************************
  !     Sets temperatures in both system of units for each HRU
  !***********************************************************************
  module subroutine set_temperature(this, ctl_data, param_data, model_basin, tmin_adj, tmax_adj)
    use prms_constants, only: MINTEMP, MAXTEMP, FAHRENHEIT, CELSIUS
    use conversions_mod, only: c_to_f, f_to_c
    implicit none

    class(Climateflow), intent(inout) :: this
    type(Control), intent(in) :: ctl_data
    type(Parameters), intent(in) :: param_data
    type(Basin), intent(in) :: model_basin
    real(r32), optional, intent(in) :: tmin_adj(:)
      !! Array of minimum temperature adjustments
    real(r32), optional, intent(in) :: tmax_adj(:)
      !! Array of maximum temperature adjustments

    ! --------------------------------------------------------------------------
    ! NOTE: This is dangerous because it circumvents the intent for param_data
    associate(basin_area_inv => model_basin%basin_area_inv, &
              hru_area => param_data%hru_area%values)

      if (param_data%temp_units%values(1) == FAHRENHEIT) then
        if (present(tmax_adj)) then
          this%tmaxf = this%tmaxf + tmax_adj
        endif

        if (present(tmin_adj)) then
          this%tminf = this%tminf + tmin_adj
        endif

        this%tavgf = (this%tmaxf + this%tminf) * 0.5

        this%tmaxc = f_to_c(this%tmaxf)
        this%tminc = f_to_c(this%tminf)
        this%tavgc = f_to_c(this%tavgf)
        this%tmax_hru = this%tmaxf ! in units temp_units
        this%tmin_hru = this%tminf ! in units temp_units

        this%basin_temp = sum(dble(this%tavgf * hru_area)) * basin_area_inv
        this%basin_tmax = sum(dble(this%tmaxf * hru_area)) * basin_area_inv
        this%basin_tmin = sum(dble(this%tminf * hru_area)) * basin_area_inv
      else
        ! degrees Celsius
        if (present(tmax_adj)) then
          this%tmaxc = this%tmaxc + tmax_adj
        endif

        if (present(tmin_adj)) then
          this%tminc = this%tminc + tmin_adj
        endif

        this%tavgc = (this%tmaxc + this%tminc) * 0.5

        this%tmaxf = c_to_f(this%tmaxc)
        this%tminf = c_to_f(this%tminc)
        this%tavgf = c_to_f(this%tavgc)
        this%tmax_hru = this%tmaxc ! in units temp_units
        this%tmin_hru = this%tminc ! in units temp_units

        this%basin_temp = sum(dble(this%tavgc * hru_area)) * basin_area_inv
        this%basin_tmax = sum(dble(this%tmaxc * hru_area)) * basin_area_inv
        this%basin_tmin = sum(dble(this%tminc * hru_area)) * basin_area_inv
      endif

      this%solrad_tmax = real(this%basin_tmax, r32)
      this%solrad_tmin = real(this%basin_tmin, r32)

      ! if (this%tminf(ihru) < MINTEMP .OR. this%tmaxf(ihru) > MAXTEMP) then
      !   PRINT *, 'ERROR, invalid temperature value for HRU: ', ihru, this%tminf(ihru), this%tmaxf(ihru) !, ' Date:', Nowyear, Nowmonth, Nowday
      !   STOP
      ! endif
    end associate
  end subroutine

  ! module subroutine temp_set(this, param_data, ihru, hru_area, tmax, tmin)
  !   use prms_constants, only: MINTEMP, MAXTEMP
  !   use conversions_mod, only: c_to_f, f_to_c
  !   ! use Parameters_class, only: Parameters
  !   implicit none
  !
  !   class(Climateflow), intent(inout) :: this
  !   type(Parameters), intent(in) :: param_data
  !   integer(i32), intent(in) :: ihru
  !   real(r32), intent(in) :: hru_area
  !   real(r32), intent(in) :: tmax
  !     !! maximum temperature value from file
  !   real(r32), intent(in) :: tmin
  !     !! minimum temperature value from file
  !
  !   ! Functions
  !   INTRINSIC DBLE
  !
  !   !***********************************************************************
  !   if (param_data%temp_units%values(1) == 0) then
  !     ! degrees Fahrenheit
  !     ! this%tmaxf(ihru) = tmax
  !     ! this%tminf(ihru) = tmin
  !     ! this%tavgf(ihru) = (tmax + tmin) * 0.5
  !     ! this%tmaxc(ihru) = f_to_c(tmax)
  !     ! this%tminc(ihru) = f_to_c(tmin)
  !     ! this%tavgc(ihru) = f_to_c(this%tavgf(ihru))
  !     ! this%basin_temp = this%basin_temp + DBLE(this%tavgf(ihru) * hru_area)
  !   else
  !     ! degrees Celsius
  !     this%tmaxc(ihru) = tmax
  !     this%tminc(ihru) = tmin
  !     this%tavgc(ihru) = (tmax + tmin) * 0.5
  !     this%tmaxf(ihru) = c_to_f(tmax)
  !     this%tminf(ihru) = c_to_f(tmin)
  !     this%tavgf(ihru) = c_to_f(this%tavgc(ihru))
  !     this%basin_temp = this%basin_temp + DBLE(this%tavgc(ihru) * hru_area)
  !   endif
  !
  !   if (this%tminf(ihru) < MINTEMP .OR. this%tmaxf(ihru) > MAXTEMP) then
  !     PRINT *, 'ERROR, invalid temperature value for HRU: ', ihru, this%tminf(ihru), this%tmaxf(ihru) !, ' Date:', Nowyear, Nowmonth, Nowday
  !     STOP
  !   endif
  !
  !   ! this%tmax_hru(ihru) = tmax ! in units temp_units
  !   ! this%tmin_hru(ihru) = tmin ! in units temp_units
  !
  !   ! this%basin_tmax = this%basin_tmax + DBLE(this%tmax_hru(ihru) * hru_area)
  !   ! this%basin_tmin = this%basin_tmin + DBLE(this%tmin_hru(ihru) * hru_area)
  !   ! this%basin_tmax = this%basin_tmax + DBLE(tmax * hru_area)
  !   ! this%basin_tmin = this%basin_tmin + DBLE(tmin * hru_area)
  ! end subroutine temp_set
end submodule
