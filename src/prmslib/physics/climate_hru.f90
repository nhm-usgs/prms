!***********************************************************************
! Read and makes available climate data (tmin, tmax, precip, potential
! solar radiation, potential evapotranspieration) and/or transpiration
! on, by HRU from files pre-processed Data Files available for other
! PRMS modules
!***********************************************************************
module PRMS_CLIMATE_HRU
    use variableKind
    use Control_class, only: Control
    use Parameters_class, only: Parameters
    use PRMS_BASIN, only: Basin
    use PRMS_CLIMATEVARS, only: Climateflow
    use PRMS_SET_TIME, only: Time_t
    implicit none

    private
    public :: Climate_HRU

    character(len=*), parameter :: MODDESC = 'Climate distribution by HRU'
    character(len=*), PARAMETER :: MODNAME = 'climate_hru'
    character(len=*), PARAMETER :: MODVERSION = '2017-09-29 13:49:00Z'

    type Climate_HRU
      integer(i32), private :: precip_funit
        !! Precipitation CBH file unit
      integer(i32), private :: tmax_funit
        !! Maximum temperature file unit
      integer(i32), private :: tmin_funit
        !! Minimum temperature file unit

      integer(i32), private :: nhru
        !! Internal copy of ctl_data%nhru

      contains
        procedure, public :: run => run_Climate_HRU
        procedure, nopass, public :: module_name
          !! Return the name of the module
        procedure, nopass, public :: version
          !! Return the version of the module
        procedure, private :: find_header_end
        procedure, private :: read_cbh_date
    end type

    interface Climate_HRU
      !! Climate_HRU constructor
      module function constructor_Climate_HRU(ctl_data, param_data) result(this)
        ! use Control_class, only: Control
        ! use Parameters_class, only: Parameters
        ! use UTILS_PRMS, only: print_module_info

        type(Climate_HRU) :: this
          !! Climate_HRU class
        type(Control), intent(in) :: ctl_data
          !! Control file parameters
        type(Parameters), intent(in) :: param_data
          !! Parameters
      end function
    end interface

    interface
      module subroutine run_Climate_HRU(this, ctl_data, param_data, model_time, model_basin, climate)
        ! use PRMS_BASIN, only: Basin
        ! use Control_class, only: Control
        ! use Parameters_class, only: Parameters
        ! use PRMS_CLIMATEVARS, only: Climateflow
        ! use PRMS_SET_TIME, only: Time_t

        class(Climate_HRU), intent(inout) :: this
        type(Control), intent(in) :: ctl_data
        type(Parameters), intent(in) :: param_data
        type(Time_t), intent(in) :: model_time
        type(Basin), intent(in) :: model_basin
        type(Climateflow), intent(inout) :: climate
      end subroutine
    end interface

    interface
      module subroutine find_header_end(this, Iunit, Iret, Fname, Paramname, Cbh_flag, &
                                 Cbh_binary_flag)
        class(Climate_HRU), intent(inout) :: this
        integer(i32), intent(out) :: Iunit
        integer(i32), intent(out) :: Iret
        character(len=*), intent(in) :: Fname
        character(len=*), intent(in) :: Paramname
        integer(i32), intent(in) :: Cbh_flag
        integer(i32), intent(in) :: Cbh_binary_flag
      end subroutine
    end interface

    interface
      module subroutine read_cbh_date(this, model_time, Year, Month, Day, Var, Ios, Iret)
        ! use PRMS_SET_TIME, only: Time_t

        class(Climate_HRU), intent(inout) :: this
        type(Time_t), intent(in) :: model_time
        integer(i32), intent(in) :: Year
        integer(i32), intent(in) :: Month
        integer(i32), intent(in) :: Day
        integer(i32), intent(in) :: Ios
        character(len=*), intent(in) :: Var
        integer(i32), intent(inout) :: Iret
      end subroutine
    end interface

    interface
      module function module_name() result(res)
        character(:), allocatable :: res
      end function
    end interface

    interface
      module function version() result(res)
        character(:), allocatable :: res
      end function
    end interface
end module
