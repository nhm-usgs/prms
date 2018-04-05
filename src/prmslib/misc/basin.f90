!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
module PRMS_BASIN
  use variableKind
  use iso_fortran_env, only: output_unit
  ! use fileio_mod, only: write_outfile
  implicit none

  private
  public :: Basin

  intrinsic :: EPSILON

  character(len=*), parameter :: MODDESC = 'Basin Definition'
  character(len=*), parameter :: MODNAME = 'basin'
  character(len=*), parameter :: MODVERSION = '2017-09-29 13:50:00Z'

  type :: Basin
    real(r64) :: basin_area_inv
    real(r64) :: basin_lat = 0.0
    real(r64) :: active_area = 0.0

    integer(i32) :: active_hrus
    integer(i32) :: hemisphere
    integer(i32), allocatable :: hru_route_order(:)

    real(r32), allocatable :: hru_frac_perv(:)
    real(r32), allocatable :: hru_imperv(:)
    real(r32), allocatable :: hru_perv(:)

    real(r64) :: total_area = 0.0
    real(r64) :: land_area = 0.0

    contains
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module
  end type

  interface Basin
    !! Basin constructor
    module function constructor_Basin(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters

      type(Basin) :: this
        !! Basin class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Parameters), intent(in) :: param_data
        !! Parameters
    end function
  end interface

  contains
    !***********************************************************************
    ! Basin constructor
    module function constructor_Basin(ctl_data, param_data) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use UTILS_PRMS, only: print_module_info
      implicit none

      type(Basin) :: this
      class(Control), intent(in) :: ctl_data
      class(Parameters), intent(in) :: param_data

      ! Local variables
      real(r64) :: basin_perv = 0.0
      real(r64) :: basin_imperv = 0.0
      real(r32) :: harea
      real(r64) :: harea_dble

      character(len=69) :: buffer
      integer(i32) :: chru
        !! Current HRU
      integer(i32) :: j
        !! General counter

      ! TODO: Verify required parameters are allocated
      ! Hru_area, Hru_elev, Hru_lat, Hru_type, Cov_type, Covden_sum,
      ! Covden_win, Elev_units, Hru_percent_imperv
      associate(nhru => ctl_data%nhru%values(1), &
                print_debug => ctl_data%print_debug%values(1), &
                model_output_unit => ctl_data%model_output_unit, &
                hru_area => param_data%hru_area%values, &
                hru_lat => param_data%hru_lat%values, &
                hru_percent_imperv => param_data%hru_percent_imperv%values, &
                hru_type => param_data%hru_type%values)

        if (print_debug > -2) then
          ! Output module and version information
          call print_module_info(MODNAME, MODDESC, MODVERSION)
        endif

        allocate(this%hru_route_order(nhru))
        allocate(this%hru_frac_perv(nhru))
        allocate(this%hru_imperv(nhru))
        allocate(this%hru_perv(nhru))
        this%hru_route_order = 0

        j = 0

        do chru = 1, nhru
          harea = hru_area(chru)
          harea_dble = DBLE(harea)
          this%total_area = this%total_area + harea_dble

          if (hru_type(chru) == 0) cycle ! inactive

          ! ????????? need to fix for lakes with multiple HRUs and PRMS lake routing ????????
          this%land_area = this%land_area + harea_dble ! swale or land

          this%basin_lat = this%basin_lat + DBLE(hru_lat(chru) * harea)
          j = j + 1
          this%hru_route_order(j) = chru

          this%hru_imperv(chru) = hru_percent_imperv(chru) * harea
          this%hru_perv(chru) = harea - this%hru_imperv(chru)

          this%hru_frac_perv(chru) = this%hru_perv(chru) / harea
          basin_perv = basin_perv + DBLE(this%hru_perv(chru))
          basin_imperv = basin_imperv + DBLE(this%hru_imperv(chru))
        enddo

        this%active_hrus = j
        this%active_area = this%land_area

        this%basin_area_inv = 1.0D0 / this%active_area
        this%basin_lat = this%basin_lat * this%basin_area_inv

        ! Used in solrad modules to winter/summer radiation adjustment
        if (this%basin_lat > 0.0D0) then
          this%hemisphere = 0 ! Northern
        else
          this%hemisphere = 1 ! Southern
        endif

        basin_perv = basin_perv * this%basin_area_inv
        basin_imperv = basin_imperv * this%basin_area_inv

        if (print_debug == 2) then
          write(output_unit, *) ' HRU     Area'
          write(output_unit, fmt='(i7, f14.5)') (j, hru_area(j), j=1, nhru)
          write(output_unit, 9001) 'Model domain area   =', this%total_area
          write(output_unit, 9001) 'Active basin area   =', this%active_area
          write(output_unit, 9001) 'Fraction impervious =', basin_imperv
          write(output_unit, 9001) 'Fraction pervious   =', basin_perv
          write(output_unit, *) ' '

          9001 format(a, 1x, f15.5)
        endif

        if (print_debug > -2) then
          write(model_output_unit, fmt='(1X)')
          write(model_output_unit, 9003) 'Model domain area:   ', this%total_area, '    Active basin area:', this%active_area

          write(model_output_unit, 9004) 'Fraction impervious:  ', basin_imperv, '    Fraction pervious: ', basin_perv
          write(model_output_unit, fmt='(/)')

          ! 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
          9003 FORMAT(2(a, f13.2))
          9004 FORMAT(2(a, f12.5))
          ! 9005 FORMAT (A, F13.2, A, F13.4)
        endif
      end associate
    end function

    function module_name()
      implicit none

      character(:), allocatable :: module_name
      module_name = MODNAME
    end function

    function version()
      implicit none

      character(:), allocatable :: version
      version = MODVERSION
    end function
end module
