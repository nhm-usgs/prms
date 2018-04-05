!***********************************************************************
! Determines whether current time period is one of active transpiration
! based on a temperature index method.
!***********************************************************************
MODULE PRMS_TRANSP_TINDEX
  use variableKind
  use prms_constants, only: YEAR, MONTH, DAY
  implicit none

  private
  public :: transp_tindex

  character(len=*), PARAMETER :: MODNAME = 'transp_tindex'
  character(len=*), PARAMETER :: MODVERSION = 'transp_tindex.f90 2015-01-06 00:09:15Z'

  type Transp_tindex
    ! Local Variables
    integer(i32), allocatable :: transp_check(:)
    integer(i32), allocatable :: transp_beg_restart(:)
    integer(i32), allocatable :: transp_end_restart(:)
    real(r32), allocatable :: tmax_sum(:)
    real(r32), allocatable :: transp_tmax_f(:)
    real(r32), allocatable :: transp_tmax_restart(:)

    contains
      procedure, public :: run => run_Transp_tindex
      procedure, nopass, public :: module_name
        !! Return the name of the module
      procedure, nopass, public :: version
        !! Return the version of the module

  end type

  interface Transp_tindex
    !! Transp_tindex constructor
    module function constructor_Transp_tindex(ctl_data, param_data, model_basin, climate) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use PRMS_BASIN, only: Basin
      use PRMS_CLIMATEVARS, only: Climateflow

      type(Transp_tindex) :: this
        !! Transp_tindex class
      class(Control), intent(in) :: ctl_data
        !! Control file parameters
      class(Parameters), intent(in) :: param_data
        !! Parameters
      class(Basin), intent(in) :: model_basin
        !! Model basin information
      class(Climateflow), intent(inout) :: climate
        !! Climate flow class
    end function
  end interface

  contains
    !***********************************************************************
    ! Transp_tindex constructor
    module function constructor_Transp_tindex(ctl_data, param_data, model_basin, climate) result(this)
      use Control_class, only: Control
      use Parameters_class, only: Parameters
      use PRMS_BASIN, only: Basin
      use PRMS_CLIMATEVARS, only: Climateflow
      use conversions_mod, only: c_to_f
      implicit none

      type(Transp_tindex) :: this
      class(Control), intent(in) :: ctl_data
      class(Parameters), intent(in) :: param_data
      class(Basin), intent(in) :: model_basin
      class(Climateflow), intent(inout) :: climate

      integer(i32) :: ii
        !! counter
      integer(i32) :: chru
        !! Current HRU

      ! ------------------------------------------------------------------------

      ! These are only allocated when initializing from restart file
      ! integer(i32), allocatable :: transp_beg_restart(:)
      ! integer(i32), allocatable :: transp_end_restart(:)
      ! real(r32), allocatable :: transp_tmax_restart(:)

      allocate(this%tmax_sum(ctl_data%nhru%values(1)))
      allocate(this%transp_check(ctl_data%nhru%values(1)))
      allocate(this%transp_tmax_f(ctl_data%nhru%values(1)))

      if (param_data%temp_units%values(1) == 0) then
        this%transp_tmax_f = param_data%transp_tmax%values(:)
      else
        do ii=1, ctl_data%nhru%values(1)
          this%transp_tmax_f(ii) = c_to_f(param_data%transp_tmax%values(ii))
        enddo
      endif

      ! TODO: Incorporate the load from restart file stuff
      this%tmax_sum = 0.0
      this%transp_check = 0
      climate%basin_transp_on = 0

      associate(st_month => ctl_data%start_time%values(MONTH), &
                st_day => ctl_data%start_time%values(DAY), &
                transp_beg => param_data%transp_beg%values, &
                transp_end => param_data%transp_end%values)

        do ii=1, model_basin%active_hrus
          chru = model_basin%hru_route_order(ii)

          if (st_month == transp_beg(chru)) then
            if (st_day > 10) then
              climate%transp_on(chru) = 1
            else
              this%transp_check(chru) = 1
            endif
          elseif (transp_end(chru) > transp_beg(chru)) then
            if (st_month > transp_beg(chru) .and. st_month < transp_end(chru)) then
              climate%transp_on(chru) = chru
            endif
          else
            if (st_month > transp_beg(chru) .or. (st_month + 12) < (transp_end(chru) + 12)) then
              ! TODO: shouldn't the 2nd line of the conditional just be:
              !       st_month < transp_end(chr)
              climate%transp_on(chru) = 1
            endif
          endif

          if (climate%basin_transp_on == 0) then
            if (climate%transp_on(chru) == 1) then
              climate%basin_transp_on = 1
            endif
          endif
        enddo

      end associate
    end function


    subroutine run_Transp_tindex(this, ctl_data, param_data, model_time, model_basin, climate)
      use conversions_mod, only: c_to_f

      use Control_class, only: Control
      use PRMS_SET_TIME, only: Time
      use PRMS_BASIN, only: Basin
      use Parameters_class, only: Parameters
      use PRMS_CLIMATEVARS, only: Climateflow

      implicit none

      class(Transp_tindex), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Parameters), intent(in) :: param_data
      type(Time), intent(in) :: model_time
      type(Basin), intent(in) :: model_basin
      type(Climateflow), intent(inout) :: climate

      ! Local Variables
      integer(i32) :: chru
        !! Current HRU
      integer(i32) :: j
        !! Counter

      !***********************************************************************

      !******Set switch for active transpiration period
      climate%basin_transp_on = 0

      associate(curr_month => model_time%Nowmonth, &
                curr_day => model_time%Nowday, &
                transp_beg => param_data%transp_beg%values, &
                transp_end => param_data%transp_end%values)

        do j = 1, model_basin%active_hrus
          chru = model_basin%hru_route_order(j)

          !******check for month to turn transp_check switch on or
          !******transpiration switch off (transp_on)
          if (curr_day == 1) then
            !******check for end of period
            if (curr_month == transp_end(chru)) then
              climate%transp_on(chru) = 0
              this%transp_check(chru) = 0
              this%tmax_sum(chru) = 0.0
            endif

            !******check for month to turn transpiration switch (transp_check) on or off
            if (curr_month == transp_beg(chru)) then
              this%transp_check(chru) = 1
              this%tmax_sum(chru) = 0.0
            endif
          endif

          !****** If in checking period, then for each day sum the maximum
          !****** temperature until it's greater than temperature index parameter,
          !****** at which time, turn transpiration switch on, check switch off
          ! freezing temperature assumed to be 32 degrees Fahrenheit
          if (this%transp_check(chru) == 1) then
            if (climate%tmaxf(chru) > 32.0) then
              this%tmax_sum(chru) = this%tmax_sum(chru) + climate%tmaxf(chru)
            endif

            if (this%tmax_sum(chru) > this%transp_tmax_f(chru)) then
              climate%transp_on(chru) = 1
              this%transp_check(chru) = 0
              this%tmax_sum(chru) = 0.0
            endif
          endif

          if (climate%basin_transp_on == 0) then
            if (climate%transp_on(chru) == 1) climate%basin_transp_on = 1
          endif
        enddo
      end associate
    end subroutine

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
    !***********************************************************************
    !     Write to or read from restart file
    !***********************************************************************
    ! subroutine transp_tindex_restart(In_out)
    !   use PRMS_MODULE, only: Restart_outunit, Restart_inunit
    !   use UTILS_PRMS, only: check_restart
    !   implicit none
    !
    !   ! Argument
    !   integer(i32), intent(in) :: In_out
    !
    !   ! Local Variable
    !   character(len=13) :: module_name
    !
    !   !***********************************************************************
    !   if (In_out == 0) then
    !     write (Restart_outunit) MODNAME
    !     write (Restart_outunit) Transp_check
    !     write (Restart_outunit) Tmax_sum
    !     write (Restart_outunit) Transp_beg
    !     write (Restart_outunit) Transp_end
    !     write (Restart_outunit) Transp_tmax
    !   else
    !     read (Restart_inunit) module_name
    !     call check_restart(MODNAME, module_name)
    !     read (Restart_inunit) Transp_check
    !     read (Restart_inunit) Tmax_sum
    !     read (Restart_inunit) Transp_beg_restart
    !     read (Restart_inunit) Transp_end_restart
    !     read (Restart_inunit) Transp_tmax_restart
    !   endif
    ! end subroutine transp_tindex_restart
end MODULE
