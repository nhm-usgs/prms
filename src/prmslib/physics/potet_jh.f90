!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970)
!     Potet = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
!***********************************************************************
module PRMS_POTET_JH
  use variableKind
  implicit none

  private
  public :: Potet_jh

  character(len=*), parameter :: MODDESC = 'Potential Evapotranspiration'
  character(len=*), parameter :: MODNAME = 'potet_jh'
  character(len=*), parameter :: MODVERSION = '2016-05-10 15:48:00Z'

  type Potet_jh
    contains
      procedure, public :: run => run_Potet_jh
  end type

  interface Potet_jh
    !! Potet_jh constructor
    module function constructor_Potet_jh(ctl_data) result(this)
      use Control_class, only: Control

      type(Potet_jh) :: this
        !! Poteh_jh class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
    end function
  end interface

  contains
    !***********************************************************************
    ! Potet_jh constructor
    module function constructor_Potet_jh(ctl_data) result(this)
      use Control_class, only: Control
      use UTILS_PRMS, only: print_module_info
      implicit none

      type(Potet_jh) :: this
      class(Control), intent(in) :: ctl_data

      ! ------------------------------------------------------------------------
      associate(print_debug => ctl_data%print_debug%values(1))
        if (print_debug > -2) then
          ! Output module and version information
          call print_module_info(MODNAME, MODDESC, MODVERSION)
        endif
      end associate
    end function


    subroutine run_Potet_jh(this, ctl_data, param_data, model_basin, model_time, climate)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use PRMS_SET_TIME, only: Time
      use PRMS_BASIN, only: Basin
      use PRMS_CLIMATEVARS, only: Climateflow
      implicit none

      class(Potet_jh), intent(in) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Basin), intent(in) :: model_basin
      type(Time), intent(in) :: model_time
      type(Climateflow), intent(inout) :: climate

      ! Functions
      INTRINSIC DBLE

      ! Local Variables
      integer(i32) :: chru
        !! Current HRU
      integer(i32) :: j
        !! Loop variable
      integer(i32) :: idx1D
        !! 1D index from 2D
      real(r32) :: elh
        !! Latent heat of vaporization

      !***********************************************************************
      ! 597.3 cal/gm at 0 C is the energy required to change the state of
      ! water to vapor
      ! elh is the latent heat of vaporization (not including the *2.54)

      associate(curr_month => model_time%Nowmonth, &
                jh_coef => param_data%jh_coef%values, &
                jh_coef_hru => param_data%jh_coef_hru%values, &
                hru_area => param_data%hru_area%values)

        climate%basin_potet = 0.0

        do j = 1, model_basin%active_hrus
          chru = model_basin%hru_route_order(j)
          idx1D = (curr_month - 1) * ctl_data%nhru%values(1) + chru

          elh = (597.3 - (0.5653 * climate%tavgc(chru))) * 2.54
          climate%potet(chru) = jh_coef(idx1D) * (climate%tavgf(chru) - &
                                jh_coef_hru(chru)) * climate%swrad(chru) / elh

          if (climate%potet(chru) < 0.0) climate%potet(chru) = 0.0

          climate%basin_potet = climate%basin_potet + DBLE(climate%potet(chru) * hru_area(chru))
        enddo
      end associate

      climate%basin_potet = climate%basin_potet * model_basin%basin_area_inv
    end subroutine
end module
