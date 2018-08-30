submodule (ModelBase_class) sm_ModelBase
contains
  module function module_description(this) result(res)
    implicit none

    character(:), allocatable :: res
    class(ModelBase), intent(in) :: this

    res = this%MODDESC
  end function

  module function module_name(this) result(res)
    implicit none

    character(:), allocatable :: res
    class(ModelBase), intent(in) :: this

    res = this%MODNAME
  end function

  module function version(this) result(res)
    implicit none

    character(:), allocatable :: res
    class(ModelBase), intent(in) :: this

    res = this%MODVERSION
  end function

  module subroutine print_module_info(this)
    use iso_fortran_env, only: output_unit
    implicit none

    class(ModelBase), intent(in) :: this
    
    ! ------------------------------------------------------------------------
    ! Output module and version information
    write(output_unit, 9008) this%MODDESC//repeat(' ', 30), &
                             this%MODNAME//repeat(' ', 20), &
                             this%MODVERSION//repeat(' ', 20)

    9008 format(a30, 1x, a20, 1x, a20)
  end subroutine

  module subroutine set_module_info(this, name, desc, version)
    implicit none

    class(ModelBase), intent(inout) :: this
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: desc
    character(len=*), intent(in) :: version

    ! --------------------------------------------------------------------------
    this%MODNAME = name
    this%MODDESC = desc
    this%MODVERSION = version
  end subroutine
end submodule
