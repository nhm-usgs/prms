module UTILS_PRMS
  use variableKind
  use Control_class, only: Control
  use PRMS_SET_TIME, only: Time_t
  implicit none

  ! interface update_parameter
  !   module procedure update_parameter_i32, update_parameter_r32
  ! end interface
  interface update_parameter
    module subroutine update_parameter_i32(ctl_data, model_time, dyn_output_unit, dyn_values, param_name, param)
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      integer(i32), intent(in) :: dyn_output_unit
      integer(i32), intent(in) :: dyn_values(:)
      character(len=*), intent(in) :: param_name
      integer(i32), intent(inout) :: param(:)
    end subroutine

    module subroutine update_parameter_r32(ctl_data, model_time, dyn_output_unit, dyn_values, param_name, param)
      type(Control), intent(in) :: ctl_data
      type(Time_t), intent(in) :: model_time
      integer(i32), intent(in) :: dyn_output_unit
      real(r32), intent(in) :: dyn_values(:)
      character(len=*), intent(in) :: param_name
      real(r32), intent(inout) :: param(:)
    end subroutine
  end interface

  interface
    module function get_array(array, shape_) result(aptr)
      real(r32), pointer :: aptr(:,:)
      real(r32), target :: array(1)
      integer(i32) :: shape_(:)
    end function
  end interface

  interface
    module subroutine print_module_info(module_name, module_desc, module_version)
      character(len=*), intent(in) :: module_name
      character(len=*), intent(in) :: module_desc
      character(len=*), intent(in) :: module_version
    end subroutine
  end interface

  interface
    module subroutine PRMS_open_input_file(iunit, iret, Fname, Paramname, use_stream)
      integer(i32), intent(out) :: iunit
      integer(i32), intent(out) :: iret
      character(len=*), intent(in) :: Fname
      character(len=*), intent(in) :: Paramname
      logical, optional, intent(in) :: use_stream
    end subroutine
  end interface

  interface
    module subroutine PRMS_open_module_file(iunit, Fname)
      integer(i32), intent(out) :: iunit
      character(len=*), intent(in) :: Fname
    end subroutine
  end interface

  interface
    module function get_ftnunit(iunit) result(res)
      integer(i32) :: res
      integer, intent(in) :: iunit
    end function
  end interface

  interface
    pure module function yr_mo_eq_dy_le(lh_date, rh_date) result(res)
      !! Return true if left-hand year and month are equal to right-hand year and month
      !! and the left-hand day is less than or equal to the right-hand day
      logical :: res
      integer(i32), intent(in) :: lh_date(3)
      integer(i32), intent(in) :: rh_date(3)
    end function
  end interface

  ! interface
  !   module function get_first_time(iunit, datetime) result(res)
  !     integer(i32) :: res(3)
  !       !! Return a date array (YY, MM, DD)
  !     integer(i32), intent(in) :: iunit
  !     integer(i32), intent(in) :: datetime(3)
  !       !! Datetime to search for
  !   end function
  ! end interface

  interface
    module function get_first_time(iunit, datetime) result(res)
      integer(i32) :: res(3)
        !! Return a date array (YY, MM, DD)
      integer(i32), intent(in) :: iunit
      integer(i32), intent(in) :: datetime(3)
        !! Datetime to search for
    end function
  end interface

  interface
    module function get_next_time(iunit) result(res)
      integer(i32) :: res(3)
        !! Return a date array (YY, MM, DD)
      integer(i32), intent(in) :: iunit
    end function
  end interface

  interface
    module subroutine open_dyn_param_file(Iunit, Iret, Fname, Paramname)
      ! integer(i32), intent(in) :: nhru
        !! Expected number of HRUs in the CBH file
      integer(i32), intent(out) :: Iunit
      integer(i32), intent(out) :: Iret
      character(len=*), intent(in) :: Fname
      character(len=*), intent(in) :: Paramname
    end subroutine
  end interface

end module

