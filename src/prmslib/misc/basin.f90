!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
module PRMS_BASIN
  use variableKind

  implicit none

  intrinsic :: EPSILON

  character(len=*), PARAMETER :: MODNAME = 'basin'
  character(len=*), PARAMETER :: VERSION = 'basin.f90 2017-09-29 13:50:00Z'

  private
  public :: Basin

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

    ! contains
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

      allocate(this%hru_route_order(ctl_data%nhru%values(1)))
      allocate(this%hru_frac_perv(ctl_data%nhru%values(1)))
      allocate(this%hru_imperv(ctl_data%nhru%values(1)))
      allocate(this%hru_perv(ctl_data%nhru%values(1)))
      this%hru_route_order = 0

      j = 0

      do chru = 1, ctl_data%nhru%values(1)
        harea = param_data%hru_area%values(chru)
        harea_dble = DBLE(harea)
        this%total_area = this%total_area + harea_dble

        ! TODO: This is not a good way to handle 'flexible' dimensions
        !       I lean toward forcing all parameters to adhere to their
        !       dimensions as defined in the TM chapter.
        if (param_data%hru_type%size() == 1) then
          ! defaults dimension is 'nhru' but it could be 'one'
          if (param_data%hru_type%values(1) == 0) cycle ! inactive
        else
          ! The size of the array is something bigger than one
          ! Assume it is 'nhru'
          if (param_data%hru_type%values(chru) == 0) cycle ! inactive
        endif

        ! ????????? need to fix for lakes with multiple HRUs and PRMS lake routing ????????
        this%land_area = this%land_area + harea_dble ! swale or land

        this%basin_lat = this%basin_lat + DBLE(param_data%hru_lat%values(chru) * harea)
        j = j + 1
        this%hru_route_order(j) = chru

        this%hru_imperv(chru) = param_data%hru_percent_imperv%values(chru) * harea
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

      ! TODO: How to handle the print_debug stuff?
      ! IF (Print_debug == 2) THEN
      !   PRINT *, ' HRU     Area'
      !   PRINT ('(I7, F14.5)'), (chru, param_data%hru_area(chru), chru = 1, Nhru)
      !   PRINT *, 'Model domain area     = ', this%total_area
      !   PRINT *, 'Active basin area     = ', this%active_area
      !   PRINT *, 'Fraction impervious   = ', basin_imperv
      !   PRINT *, 'Fraction pervious     = ', basin_perv
      !   PRINT *, ' '
      ! ENDIF

      ! Print out start and end times
      ! IF (Print_debug > -2) THEN
      !   !CALL write_outfile(' Surface Water and Energy Budgets Simulated by '//PRMS_VERSION)
      !   WRITE (Prms_output_unit, '(1X)')
      !
      !   WRITE (buffer, 9002) 'Start time: ', Starttime
      !   CALL write_outfile(buffer(:31))
      !
      !   WRITE (buffer, 9002) 'End time:   ', Endtime
      !   CALL write_outfile(buffer(:31))
      !
      !   WRITE (Prms_output_unit, '(1X)')
      !   WRITE (buffer, 9003) 'Model domain area:   ', this%total_area, '    Active basin area:', this%active_area
      !   CALL write_outfile(buffer)
      !
      !   WRITE (buffer, 9004) 'Fraction impervious:  ', basin_imperv, '    Fraction pervious: ', basin_perv
      !   CALL write_outfile(buffer)
      !   CALL write_outfile(' ')
      ! ENDIF
      !
      ! 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
      ! 9003 FORMAT (2(A, F13.2))
      ! 9004 FORMAT (2(A, F12.5))
      ! 9005 FORMAT (A, F13.2, A, F13.4)
    end function
end module
