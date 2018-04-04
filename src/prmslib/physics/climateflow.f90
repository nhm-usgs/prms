!***********************************************************************
! Declares and initializes climate and flow parameters and variables
!***********************************************************************
module PRMS_CLIMATEVARS
  use variableKind
  implicit none

  character(len=*), parameter :: MODNAME = 'climateflow'
  character(len=*), parameter :: VERSION = 'climateflow.f90 2017-09-29 13:47:00Z'

  private
  public :: Climateflow

  type Climateflow
    real(r32) :: solrad_tmax(1)
    real(r32) :: solrad_tmin(1)

    real(r64) :: basin_horad(1)
    real(r64) :: basin_obs_ppt(1) = 0.0
    real(r64) :: basin_orad(1) = 0.0
      !! used when solrad_module = [ddsolrad, ccsolrad] or model==99
    real(r64) :: basin_potet(1) = 0.0
    real(r64) :: basin_ppt(1) = 0.0
    real(r64) :: basin_rain(1) = 0.0
    real(r64) :: basin_snow(1) = 0.0
    real(r64) :: basin_swrad(1) = 0.0
    real(r64) :: basin_temp(1) = 0.0
    real(r64) :: basin_tmax(1) = 0.0
    real(r64) :: basin_tmin(1) = 0.0

    integer(i32) :: basin_solsta = 0
    integer(i32) :: basin_transp_on = 0

    real(r32), allocatable :: hru_ppt(:)
    real(r32), allocatable :: hru_rain(:)
    real(r32), allocatable :: hru_snow(:)
    real(r32), allocatable :: orad_hru(:)
      !! used when solrad_module = [ddsolrad, ccsolrad] or model==99
    real(r32), allocatable :: potet(:)
    real(r32), allocatable :: prmx(:)
    real(r32), allocatable :: swrad(:)
    real(r32), allocatable :: tavgc(:)
    real(r32), allocatable :: tavgf(:)
    real(r32), allocatable :: tmax_hru(:)
    real(r32), allocatable :: tmaxc(:)
    real(r32), allocatable :: tmaxf(:)
    real(r32), allocatable :: tmin_hru(:)
    real(r32), allocatable :: tminc(:)
    real(r32), allocatable :: tminf(:)
    real(r32), allocatable :: tmax_allrain(:, :)
    real(r32), allocatable :: tmax_allrain_f(:, :)
    real(r32), allocatable :: tmax_allsnow_c(:, :)
    real(r32), allocatable :: tmax_allsnow_f(:, :)
    real(r32), allocatable :: tmax_aspect_adjust(:, :)
    real(r32), allocatable :: tmin_aspect_adjust(:, :)

    integer(i32), allocatable :: newsnow(:)
    integer(i32), allocatable :: pptmix(:)
    integer(i32), allocatable :: transp_on(:)

    contains
      procedure, public :: temp_set
      procedure, public :: precip_form

  end type

  interface Climateflow
    !! Climateflow constructor
    module function constructor_Climateflow(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters

      type(Climateflow) :: this
        !! Climateflow class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  contains
    !***********************************************************************
    ! Climateflow constructor
    module function constructor_Climateflow(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use conversions_mod, only: c_to_f, f_to_c
      implicit none

      type(Climateflow) :: this
      class(Control), intent(in) :: ctl_data
      class(Parameters), intent(in) :: param_data

      ! Local variables
      integer(r32) :: i
      integer(r32) :: j
      integer(r32) :: idx1D
        !! 1D index from 2D
      integer(r32) :: nhru
        !! local copy of ctl_data%nhru

      ! TODO: how to handle when init_vars_from_file == 1 (during init)?
      ! TODO: how to handle when save_vars_to_file == 1 (during clean)?

      ! Get a local copy of the number of HRUs in the model
      nhru = ctl_data%nhru%values(1)

      ! real(r32), allocatable :: tmax_allrain(:, :)

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
      if (param_data%temp_units%values(1) == 0) then
        this%tmax_allsnow_f = param_data%tmax_allsnow
        ! this%tmax_allsnow_f = reshape(param_data%tmax_allsnow%values, shape(this%tmax_allsnow_f))

        do j = 1, 12
          do i = 1, nhru
            idx1D = (j - 1) * nhru + i
            this%tmax_allrain_f(i, j) = param_data%tmax_allsnow%values(idx1D) + &
                                        param_data%tmax_allrain_offset%values(idx1D)
            this%tmax_allsnow_c(i, j) = f_to_c(param_data%tmax_allsnow%values(idx1D))
          enddo
        enddo

        this%tmax_allrain = this%tmax_allrain_f
      else
        this%tmax_allsnow_c = param_data%tmax_allsnow
        ! this%tmax_allsnow_c = reshape(param_data%tmax_allsnow%values, shape(this%tmax_allsnow_c))

        do j = 1, 12
          do i = 1, nhru
            idx1D = (j - 1) * nhru + i
            this%tmax_allsnow_f(i, j) = c_to_f(param_data%tmax_allsnow%values(idx1D))

            ! TODO: Is tmax_allrain properly allocated here?
            this%tmax_allrain(i, j) = param_data%tmax_allsnow%values(idx1D) + &
                                      param_data%tmax_allrain_offset%values(idx1D)
            this%tmax_allrain_f(i, j) = c_to_f(this%tmax_allrain(i, j))
          enddo
        enddo
      endif
    end function


    !***********************************************************************
    !     Write or read restart file
    !***********************************************************************
    subroutine climateflow_restart(this, in_out)
      use PRMS_MODULE, only: Restart_outunit, Restart_inunit ! , Solrad_module
      use UTILS_PRMS, only: check_restart
      implicit none

      ! Arguments
      class(Climateflow), intent(inout) :: this
      integer(i32), intent(in) :: in_out

      ! Local Variables
      character(LEN=11) :: module_name

      !***********************************************************************
      if (In_out == 0) then
        write(Restart_outunit) MODNAME
        write(Restart_outunit) this%basin_ppt, this%basin_rain, this%basin_snow, &
                               this%basin_obs_ppt, this%basin_temp, &
                               this%basin_orad, this%basin_tmax, this%basin_tmin, &
                               this%solrad_tmax, this%solrad_tmin, &
                               this%basin_transp_on, this%basin_potet, &
                               this%basin_horad, this%basin_swrad
        write(Restart_outunit) this%tmax_hru
        write(Restart_outunit) this%tmin_hru
        write(Restart_outunit) this%newsnow
        write(Restart_outunit) this%pptmix
        write(Restart_outunit) this%hru_ppt
        write(Restart_outunit) this%hru_rain
        write(Restart_outunit) this%hru_snow
        write(Restart_outunit) this%prmx
        write(Restart_outunit) this%tmaxf
        write(Restart_outunit) this%tminf
        write(Restart_outunit) this%tavgf
        write(Restart_outunit) this%tmaxc
        write(Restart_outunit) this%tminc
        write(Restart_outunit) this%tavgc
        write(Restart_outunit) this%transp_on
        write(Restart_outunit) this%potet
        write(Restart_outunit) this%swrad

        ! if (ANY(['ddsolrad', 'ccsolrad']==Solrad_module)) write(Restart_outunit) this%orad_hru
      else
        read(Restart_inunit) module_name
        call check_restart(MODNAME, module_name)
        read(Restart_inunit) this%basin_ppt, this%basin_rain, this%basin_snow, &
                              this%basin_obs_ppt, this%basin_temp, &
                              this%basin_orad, this%basin_tmax, this%basin_tmin, &
                              this%solrad_tmax, this%solrad_tmin, &
                              this%basin_transp_on, this%basin_potet, &
                              this%basin_horad, this%basin_swrad
        read(Restart_inunit) this%tmax_hru
        read(Restart_inunit) this%tmin_hru
        read(Restart_inunit) this%newsnow
        read(Restart_inunit) this%pptmix
        read(Restart_inunit) this%hru_ppt
        read(Restart_inunit) this%hru_rain
        read(Restart_inunit) this%hru_snow
        read(Restart_inunit) this%prmx
        read(Restart_inunit) this%tmaxf
        read(Restart_inunit) this%tminf
        read(Restart_inunit) this%tavgf
        read(Restart_inunit) this%tmaxc
        read(Restart_inunit) this%tminc
        read(Restart_inunit) this%tavgc
        read(Restart_inunit) this%transp_on
        read(Restart_inunit) this%potet
        read(Restart_inunit) this%swrad

        ! if (ANY(['ddsolrad', 'ccsolrad']==Solrad_module)) read(Restart_inunit) this%orad_hru
      endif
    end subroutine climateflow_restart

    !***********************************************************************
    !     Computes precipitation form (rain, snow or mix) and depth for each HRU
    !***********************************************************************
    subroutine precip_form(this, ihru, month, hru_area, adjmix_rain, rain_adj, snow_adj, &
                           precip, sum_obs)
      use prms_constants, only: NEARZERO
      implicit none

      ! Functions
      INTRINSIC ABS, DBLE

      ! Arguments
      class(Climateflow), intent(inout) :: this
      integer(i32), intent(in) :: ihru
      integer(i32), intent(in) :: month
      real(r32), intent(in) :: hru_area
      real(r32), intent(in) :: adjmix_rain
      real(r32), intent(in) :: rain_adj
      real(r32), intent(in) :: snow_adj
      real(r32), intent(inout) :: precip
      real(r64), intent(inout) :: sum_obs

      ! Local Variables
      real(r32) :: tdiff

      !***********************************************************************
      ! basin precipitation before adjustments
      sum_obs = sum_obs + DBLE(precip * hru_area)

      if (this%tmaxf(ihru) <= this%tmax_allsnow_f(ihru, month)) then
        !****** If maximum temperature is below or equal to the base temperature
        !****** for snow then precipitation is all snow
        this%hru_ppt(ihru) = precip * snow_adj
        this%hru_snow(ihru) = this%hru_ppt(ihru)
        this%newsnow(ihru) = 1
      elseif (this%tminf(ihru) > this%tmax_allsnow_f(ihru, month) .OR. this%tmaxf(ihru) >= this%tmax_allrain_f(ihru, month)) then
        !****** If minimum temperature is above base temperature for snow or
        !****** maximum temperature is above all_rain temperature then
        !****** precipitation is all rain
        this%hru_ppt(ihru) = precip * rain_adj
        this%hru_rain(ihru) = this%hru_ppt(ihru)
        this%prmx(ihru) = 1.0
      else
        !****** Otherwise precipitation is a mixture of rain and snow
        tdiff = this%tmaxf(ihru) - this%tminf(ihru)

        if (tdiff < 0.0) then
          PRINT *, 'ERROR, tmax < tmin (degrees Fahrenheit), tmax:', this%tmaxf(ihru), ' tmin:', this%tminf(ihru)
          ! call print_date(1)
        endif

        if (ABS(tdiff) < NEARZERO) tdiff = 0.0001

        this%prmx(ihru) = ((this%tmaxf(ihru) - this%tmax_allsnow_f(ihru, month)) / tdiff) * adjmix_rain
        if (this%prmx(ihru) < 0.0) this%prmx(ihru) = 0.0

        !****** Unless mixture adjustment raises the proportion of rain to
        !****** greater than or equal to 1.0 in which case it all rain
        !****** If not, it is a rain/snow mixture
        if (this%prmx(ihru) < 1.0) then
          this%pptmix(ihru) = 1
          this%hru_ppt(ihru) = precip * snow_adj
          this%hru_rain(ihru) = this%prmx(ihru) * this%hru_ppt(ihru)
          this%hru_snow(ihru) = this%hru_ppt(ihru) - this%hru_rain(ihru)
          this%newsnow(ihru) = 1
        else
          this%hru_ppt(ihru) = precip * rain_adj
          this%hru_rain(ihru) = this%hru_ppt(ihru)
          this%prmx(ihru) = 1.0
        endif
      endif

      this%basin_ppt = this%basin_ppt + DBLE(this%hru_ppt(ihru) * hru_area)
      this%basin_rain = this%basin_rain + DBLE(this%hru_rain(ihru) * hru_area)
      this%basin_snow = this%basin_snow + DBLE(this%hru_snow(ihru) * hru_area)
    end subroutine precip_form


    !***********************************************************************
    !     Sets temperatures in both system of units for each HRU
    !***********************************************************************
    subroutine temp_set(this, param_data, ihru, hru_area, tmax, tmin)
      use prms_constants, only: MINTEMP, MAXTEMP
      use conversions_mod, only: c_to_f, f_to_c
      use Parameters_class, only: Parameters
      implicit none

      class(Climateflow), intent(inout) :: this
      class(Parameters), intent(in) :: param_data
      integer(i32), intent(in) :: ihru
      real(r32), intent(in) :: hru_area
      real(r32), intent(in) :: tmax
      real(r32), intent(in) :: tmin

      ! Functions
      INTRINSIC DBLE

      !***********************************************************************
      if (param_data%temp_units%values(1) == 0) then
        ! degrees Fahrenheit
        this%tmaxf(ihru) = tmax
        this%tminf(ihru) = tmin
        this%tavgf(ihru) = (tmax + tmin) * 0.5
        this%tmaxc(ihru) = f_to_c(tmax)
        this%tminc(ihru) = f_to_c(tmin)
        this%tavgc(ihru) = f_to_c(this%tavgf(ihru))
        this%basin_temp = this%basin_temp + DBLE(this%tavgf(ihru) * hru_area)
      else
        ! degrees Celsius
        this%tmaxc(ihru) = tmax
        this%tminc(ihru) = tmin
        this%tavgc(ihru) = (tmax + tmin) * 0.5
        this%tmaxf(ihru) = c_to_f(tmax)
        this%tminf(ihru) = c_to_f(tmin)
        this%tavgf(ihru) = c_to_f(this%tavgc(ihru))
        this%basin_temp = this%basin_temp + DBLE(this%tavgc(ihru) * hru_area)
      endif

      if (this%tminf(ihru) < MINTEMP .OR. this%tmaxf(ihru) > MAXTEMP) then
        PRINT *, 'ERROR, invalid temperature value for HRU: ', ihru, this%tminf(ihru), this%tmaxf(ihru) !, ' Date:', Nowyear, Nowmonth, Nowday
        STOP
      endif

      this%tmax_hru(ihru) = tmax ! in units temp_units
      this%tmin_hru(ihru) = tmin ! in units temp_units

      this%basin_tmax = this%basin_tmax + DBLE(tmax * hru_area)
      this%basin_tmin = this%basin_tmin + DBLE(tmin * hru_area)
    end subroutine temp_set
end module
