module PRMS_STRMTEMP
  use VariableKind
  use ModelBase_class, only: ModelBase
  use Control_class, only: Control
  use Parameters_class, only: Parameters
  use PRMS_BASIN, only: Basin
  use PRMS_OBS, only: Obs
  use PRMS_POTET, only: Potential_ET
  use PRMS_PRECIPITATION, only: Precipitation
  use PRMS_SET_TIME, only: Time_t
  use PRMS_STREAMFLOW, only: Streamflow
  use PRMS_TEMPERATURE, only: Temperature
  use SOLAR_RADIATION, only: SolarRadiation
  use PRMS_SNOW, only: Snowcomp
  implicit none

  private
  public :: StreamTemp

  character(len=*), parameter :: MODDESC = 'Stream Temperature'
  character(len=*), parameter :: MODNAME = 'stream_temp'
  character(len=*), parameter :: MODVERSION = '2018-08-30 15:05:00Z'

  ! Conversions
  real(r32), PARAMETER :: HALF_PI = ACOS(0.0)
  real(r32), PARAMETER :: ZERO_C = 273.16
  real(r32), PARAMETER :: PI = ACOS(-1.0)
  real(r32), PARAMETER :: DEG_TO_RAD = PI / 180.0
  real(r32), PARAMETER :: DAYSYR = 365.242
  real(r64) :: MPS_CONVERT = 2.93981481D-07

  type, extends(ModelBase) :: StreamTemp
    ! Parameters
    integer(i32) :: maxiter_sntemp

    integer(i32), pointer :: gw_tau(:)
    integer(i32), pointer :: ss_tau(:)

    real(r32) :: albedo
    real(r32) :: melt_temp

    real(r32), pointer :: alte(:)
    real(r32), pointer :: altw(:)
    real(r32), pointer :: azrh(:)
    real(r32), pointer :: seg_elev(:)
    real(r32), pointer :: seg_lat(:)
    real(r32), pointer :: segshade_sum(:)
    real(r32), pointer :: segshade_win(:)    
    real(r32), pointer :: stream_tave_init(:)
    real(r32), pointer :: vce(:)
    real(r32), pointer :: vcw(:)
    real(r32), pointer :: vdemn(:)
    real(r32), pointer :: vdemx(:)
    real(r32), pointer :: vdwmn(:)
    real(r32), pointer :: vdwmx(:)
    real(r32), pointer :: vhe(:)
    real(r32), pointer :: vhw(:)
    real(r32), pointer :: voe(:)
    real(r32), pointer :: vow(:)
    real(r32), pointer :: width_alpha(:)
    real(r32), pointer :: width_m(:)

    real(r32), pointer :: lat_temp_adj(:, :)
    real(r32), pointer :: seg_humidity(:, :)

    ! Output variables
    real(r32), pointer :: seg_ccov(:)
    real(r32), pointer :: seg_daylight(:)
    real(r32), pointer :: seg_humid(:)
    real(r32), pointer :: seg_melt(:)
    real(r32), pointer :: seg_potet(:)
    real(r32), pointer :: seg_shade(:)
    real(r32), pointer :: seg_tave_air(:)
    real(r32), pointer :: seg_tave_gw(:)
    real(r32), pointer :: seg_tave_lat(:)
    real(r32), pointer :: seg_tave_ss(:)
    real(r32), pointer :: seg_tave_upstream(:)
    real(r32), pointer :: seg_tave_water(:)
    real(r32), pointer :: seg_width(:)

    ! Local Variables
    integer(i32) :: gw_index
    integer(i32) :: ss_index

    integer(i32), pointer :: seg_close(:)
    integer(i32), pointer :: seg_hru_count(:)

    real(r32), pointer :: flowsum(:)
    real(r32), pointer :: seg_carea_inv(:)
    real(r32), pointer :: seg_tave_sroff(:)

    ! Next variables only needed if strm_temp_shade_flag = 0
    real(r32), pointer :: cos_lat_decl(:, :)
    real(r32), pointer :: cos_seg_lat(:)
    real(r32), pointer :: gw_silo(:,:)
    real(r32), pointer :: gw_sum(:)
    real(r32), pointer :: horizontal_hour_angle(:, :)
    real(r32), pointer :: hru_area_sum(:)
    real(r32), pointer :: level_sunset_azimuth(:, :)
    real(r32), pointer :: local_sunrise_hour_angle(:, :)
    real(r32), pointer :: local_sunset_hour_angle(:, :)
    real(r32), pointer :: max_solar_altitude(:, :)
    real(r32), pointer :: press(:)
    real(r32), pointer :: shade_jday(:, :)
    real(r32), pointer :: sin_alrs(:, :)
    real(r32), pointer :: sin_declination(:, :)
    real(r32), pointer :: sin_lat_decl(:, :)
    real(r32), pointer :: sin_seg_lat(:)
    real(r32), pointer :: ss_silo(:,:)
    real(r32), pointer :: ss_sum(:)
    real(r32), pointer :: svi_jday(:, :)
    real(r32), pointer :: total_shade(:, :)

    ! Declared variables
    ! real(r32), pointer :: seg_rain(:)

    contains
      procedure, public :: init => init_StreamTemp
      procedure, public :: run => run_StreamTemp
      procedure, public :: cleanup => cleanup_StreamTemp

      procedure, private :: equilb
      procedure, private, nopass :: lat_inflow
      procedure, private :: rprnvg
      procedure, private :: shday
      procedure, private :: snr_sst
      procedure, private :: solalt
      procedure, private :: teak1
      procedure, private, nopass :: twavg

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Parameters

    ! segment parameters
    ! real(r32), allocatable :: seg_length(:)
    ! ! real(r32), allocatable :: mann_n(:)
    ! real(r32), allocatable :: seg_slope(:)
    ! real(r32), allocatable :: width_values(:, :)
    ! real(r32), allocatable :: width_alpha(:)
    ! real(r32), allocatable :: width_m(:)
    !
    ! integer(i32):: width_dim
    ! integer(i32):: maxiter_sntemp
    !
    ! real(r32), allocatable :: seg_humidity(:, :)
    ! real(r32), allocatable :: lat_temp_adj(:, :)
    !
    ! integer(i32), allocatable :: seg_humidity_sta(:)
    !
    ! ! Needed is stream_temp_shade_flag = 0
    ! real(r32), allocatable :: seg_lat(:)
    ! real(r32), allocatable :: seg_elev(:)
    !
    ! ! shade parameters needed if stream_temp_shade_flag = 0
    ! real(r32), allocatable :: azrh(:)
    ! real(r32), allocatable :: alte(:)
    ! real(r32), allocatable :: altw(:)
    ! real(r32), allocatable :: vce(:)
    ! real(r32), allocatable :: vdemx(:)
    ! real(r32), allocatable :: vhe(:)
    ! real(r32), allocatable :: voe(:)
    ! real(r32), allocatable :: vcw(:)
    ! real(r32), allocatable :: vdwmx(:)
    ! real(r32), allocatable :: vhw(:)
    ! real(r32), allocatable :: vow(:)
    ! real(r32), allocatable :: vdemn(:)
    ! real(r32), allocatable :: vdwmn(:)
    ! integer(i32) :: spring_jday
    ! integer(i32) :: summer_jday
    ! integer(i32) :: autumn_jday
    ! integer(i32) :: winter_jday
    !
    ! ! shade parameters needed if stream_temp_shade_flag = 2
    ! real(r32), allocatable :: segshade_sum(:)
    ! real(r32), allocatable :: segshade_win(:)
    ! real(r32):: albedo
    ! real(r32):: melt_temp
    !
    ! integer(i32), allocatable :: ss_tau(:)
    ! integer(i32), allocatable :: gw_tau(:)

    ! ! control parameters
    ! integer(i32) :: stream_temp_shade_flag
  end type

  interface StreamTemp
    !! StreamTemp constructor
    module subroutine init_StreamTemp(this, ctl_data, model_basin, model_streamflow) result(this)
      class(StreamTemp) :: this
        !! StreamTemp class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
        !! Basin variables
      class(Streamflow), intent(in) :: model_streamflow
    end subroutine
  end interface
  ! interface StreamTemp
  !   !! StreamTemp constructor
  !   module function constructor_StreamTemp(ctl_data, param_data, model_basin, model_streamflow) result(this)
  !     type(StreamTemp) :: this
  !       !! StreamTemp class
  !     type(Control), intent(in) :: ctl_data
  !       !! Control file parameters
  !     type(Parameters), intent(in) :: param_data
  !       !! Parameters
  !     type(Basin), intent(in) :: model_basin
  !       !! Basin variables
  !     class(Streamflow), intent(in) :: model_streamflow
  !   end function
  ! end interface

  interface
    module subroutine cleanup_StreamTemp(this, ctl_data)
      class(StreamTemp), intent(in) :: this
      type(Control), intent(in) :: ctl_data
    end subroutine
  end interface

  interface
    module subroutine run_StreamTemp(this, ctl_data, model_basin, model_precip, model_temp, model_potet, &
                                     model_obs, model_streamflow, snow, model_solrad, model_time)
      class(StreamTemp), intent(inout) :: this
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
    end subroutine
  end interface

  interface
    module subroutine equilb (this, param_data, model_streamflow, ted, ak1d, &
                              ak2d, sh, svi, seg_id, t_o)
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
    end subroutine
  end interface

  interface
    module subroutine lat_inflow(param_data, model_streamflow, qlat, tl_avg, id, tave_gw, tave_air, tave_ss, melt, rain)
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
    end subroutine
  end interface

  interface
    module function rprnvg (this, param_data, model_time, hrsr, hrrs, hrss, sino, coso, sin_d, cosod, sinod, seg_id) result(res)
      ! Compute the riparian vegetation shade segment between the
      ! two hour angles hrsr & hrss.
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
    end function
  end interface

  interface
    module subroutine shday(this, param_data, model_time, seg_id, shade, svi)
      class(StreamTemp), intent(inout) :: this
      type(Parameters), intent(in) :: param_data
      type(Time_t), intent(in) :: model_time
      integer(i32), intent(in) :: seg_id
      real(r32), intent(out):: shade
      real(r32), intent(out):: svi
        !! riparian vegetation shade
    end subroutine
  end interface

  interface
    module subroutine snr_sst (this, param_data, coso, sino, sin_d, alt, almn, almx, &
                               azmn, azmx, azs, als, hrs, seg_id)
      ! Determines the local solar sunrise/set azimuth, altitude, and hour angle
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
    end subroutine
  end interface

  interface
    module function solalt(this, param_data, coso, sino, sin_d, az, almn, almx) result(res)
      ! This subprogram is used to determine the solar altitude when the
      ! trigonometric parameters for latitude, declination, and azimuth
      ! are given.
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
    end function
  end interface

interface
  module subroutine teak1(this, param_data, A, B, C, D, Teq, Ak1c)
    class(StreamTemp), intent(in) :: this
    type(Parameters), intent(in) :: param_data
    real(r32), intent(in) :: A
    real(r32), intent(in) :: B
    real(r32), intent(in) :: C
    real(r32), intent(in) :: D
    real(r32), intent(inout) :: Teq
    real(r32), intent(out) :: Ak1c
  end subroutine
end interface


  interface
    module function twavg(qup, t0, qlat, tl_avg, te, ak1, ak2, width, length) result(res)
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
    end function
  end interface

end module
