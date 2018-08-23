module PRMS_WATER_BALANCE
  use variableKind
  use prms_constants, only: dp, sp
  implicit none

  private
  public :: WaterBalance

  character(len=*), parameter :: MODDESC = 'Water Balance'
  character(len=*), parameter :: MODNAME = 'waterbal'
  character(len=*), parameter :: MODVERSION = '2018-08-22 13:25:00Z'

  real(r32), parameter :: TOOSMALL = 3.1E-05
  real(r32), parameter :: SMALL = 1.0E-04
  real(r32), parameter :: BAD = 1.0E-03

  real(r64), parameter :: DSMALL = 1.0D-04
  real(r64), parameter :: DTOOSMALL = 1.0D-05

  type :: WaterBalance
    ! Local Variables
    integer(i32) :: balunt
    integer(i32) :: gwunit
    integer(i32) :: intcpunt
    integer(i32) :: snowunit
    integer(i32) :: srounit
    integer(i32) :: szunit

    real(r64) :: basin_dprst_wb
    real(r64) :: last_basin_gwstor

    real(r64), allocatable :: gwstor_ante(:)
    real(r64), allocatable :: hru_storage_ante(:)


    ! Declared variables
    real(r64) :: basin_capillary_wb
    real(r64) :: basin_gravity_wb
    real(r64) :: basin_soilzone_wb

  end type

  interface WaterBalance
    !! WaterBalance constructor
    module function constructor_Balance(ctl_data, param_data) result(this)
      type(WaterBalance) :: this
        !! WaterBalance class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  interface
    module subroutine run_WaterBalance(this, ctl_data, param_data, model_basin, &
                                       model_climate, model_potet, intcp, snow)
      class(WaterBalance), intent(inout) :: this
        !! WaterBalance class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Parameters), intent(in) :: param_data
        !! Parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      ! type(Flowvars), intent(in) :: model_flow
      class(Potential_ET), intent(in) :: model_potet
      type(Interception), intent(in) :: intcp
      type(Snowcomp), intent(in) :: snow
    end subroutine
  end interface
