module PRMS_WATER_BALANCE
  use variableKind
  use prms_constants, only: dp, sp
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use PRMS_BASIN, only: Basin
  use PRMS_CLIMATEVARS, only: Climateflow
  use PRMS_GWFLOW, only: Gwflow
  use PRMS_INTCP, only: Interception
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_SNOW, only: Snowcomp
  use PRMS_SOILZONE, only: Soilzone
  use PRMS_SRUNOFF, only: Srunoff
  use PRMS_SET_TIME, only: Time_t
  implicit none

  private
  public :: WaterBalance

  character(len=*), parameter :: MODDESC = 'Water Balance'
  character(len=*), parameter :: MODNAME = 'waterbal'
  character(len=*), parameter :: MODVERSION = '2018-08-30 15:38:00Z'

  ! real(r32), parameter :: TOOSMALL = 3.1E-05
  real(r32), parameter :: TOOSMALL = 3.1E-05
  real(r32), parameter :: SMALL = 1.0E-04
  real(r32), parameter :: BAD = 1.0E-03

  real(r64), parameter :: DSMALL = 1.0D-04
  real(r64), parameter :: DTOOSMALL = 1.0D-05

  type, extends(ModelBase) :: WaterBalance
    ! Local Variables
    integer(i32), private :: bal_unit
    integer(i32), private :: gw_unit
    integer(i32), private :: intcp_unit
    integer(i32), private :: snow_unit
    integer(i32), private :: sro_unit
    integer(i32), private :: sz_unit

    real(r64), private :: basin_dprst_wb
    real(r64), private :: last_basin_gwstor

    real(r64), private, allocatable :: gwstor_ante(:)
    ! real(r64), private, allocatable :: hru_storage_ante(:)

    ! Declared variables
    ! real(r64), private :: basin_capillary_wb
    ! real(r64), private :: basin_gravity_wb
    ! real(r64), private :: basin_soilzone_wb

    contains
      procedure, public :: run => run_WaterBalance
      ! procedure, private :: basin_wb_gwflow
      ! procedure, private :: basin_wb_intcp
      ! procedure, private :: basin_wb_snow
      ! procedure, private :: basin_wb_soilzone
      ! procedure, private :: basin_wb_srunoff
      procedure, public :: cleanup => cleanup_WaterBalance
  end type

  interface WaterBalance
    !! WaterBalance constructor
    module function constructor_WaterBalance(ctl_data, model_basin, model_gwflow) result(this)
      type(WaterBalance) :: this
        !! WaterBalance class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Gwflow), intent(in) :: model_gwflow
    end function
  end interface

  interface
    module subroutine run_WaterBalance(this, ctl_data, model_basin, &
                                       model_climate, model_gwflow, model_intcp, &
                                       model_precip, model_snow, model_soilzone, &
                                       model_srunoff, model_time)
      class(WaterBalance), intent(inout) :: this
        !! WaterBalance class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      type(Climateflow), intent(in) :: model_climate
        !! Climate variables
      type(Gwflow), intent(in) :: model_gwflow
        !! Groundwater
      type(Interception), intent(in) :: model_intcp
      class(Precipitation), intent(in) :: model_precip
      type(Snowcomp), intent(in) :: model_snow
      type(Soilzone), intent(in) :: model_soilzone
      type(Srunoff), intent(in) :: model_srunoff
      type(Time_t), intent(in) :: model_time
    end subroutine
  end interface

  ! interface
  !   module subroutine basin_wb_gwflow(this, model_gwflow, model_time)
  !     class(WaterBalance), intent(inout) :: this
  !     type(Gwflow), intent(in) :: model_gwflow
  !     type(Time_t), intent(in) :: model_time
  !   end subroutine
  ! end interface

  ! interface
  !   module subroutine basin_wb_intcp(this, model_intcp, model_srunoff, model_precip, model_time)
  !     class(WaterBalance), intent(inout) :: this
  !     type(Interception), intent(in) :: model_intcp
  !     type(Srunoff), intent(in) :: model_srunoff
  !     class(Precipitation), intent(in) :: model_precip
  !     type(Time_t), intent(in) :: model_time
  !   end subroutine
  ! end interface

  ! interface
  !   module subroutine basin_wb_snow(this, model_snow, model_time, basin_snowbal)
  !     class(WaterBalance), intent(inout) :: this
  !     type(Snowcomp), intent(in) :: model_snow
  !     type(Time_t), intent(in) :: model_time
  !     real(r64), intent(in) :: basin_snowbal
  !   end subroutine
  ! end interface

  ! interface
  !   module subroutine basin_wb_soilzone(this, model_soil, model_srunoff, model_time, basin_bal, soil_in)
  !     class(WaterBalance), intent(inout) :: this
  !     type(Soilzone), intent(in) :: model_soil
  !     type(Srunoff), intent(in) :: model_srunoff
  !     type(Time_t), intent(in) :: model_time
  !     real(r64), intent(in) :: basin_bal
  !     real(r64), intent(in) :: soil_in
  !   end subroutine
  ! end interface

  ! interface
  !   module subroutine basin_wb_srunoff(this, ctl_data, model_srunoff, model_time, basin_robal)
  !     class(WaterBalance), intent(inout) :: this
  !     type(Control), intent(in) :: ctl_data
  !     type(Srunoff), intent(in) :: model_srunoff
  !     type(Time_t), intent(in) :: model_time
  !     real(r64), intent(in) :: basin_robal
  !   end subroutine
  ! end interface

  interface
    module subroutine cleanup_WaterBalance(this)
      class(WaterBalance) :: this
        !! Srunoff class
    end subroutine
  end interface
end module
