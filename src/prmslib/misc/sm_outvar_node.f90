submodule (PRMS_OUTVAR_NODE ) sm_outvar_node

contains
  module function constructor_outvar_node(name, dimensions, datatype, description, units) result(this)
    implicit none

    type(outvar_node) :: this
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: dimensions
    integer(i32), intent(in) :: datatype
    ! character(len=*), intent(in) :: datatype
    character(len=*), intent(in) :: description
    character(len=*), intent(in) :: units

    ! -------------------------------------------------------------------------
    this%key = name
    this%dimensions = dimensions
    this%datatype = datatype
    this%description = description
    this%units = units
  end function

  pure elemental module subroutine destroy_node_data(this)
    class(outvar_node), intent(inout) :: this

    ! -------------------------------------------------------------------------
    if (allocated(this%key)) then
      deallocate(this%key)
    end if
  end subroutine

  module subroutine print(this)
    class(outvar_node), intent(in) :: this

    ! -------------------------------------------------------------------------
    write(output_unit, *) 'Output variable: ' // this%key
    write(output_unit, *) '  Dimensions: ' // this%dimensions
    write(output_unit, *) '  Datatype: ', this%datatype
    write(output_unit, *) '  Units: ' // this%units
  end subroutine

end submodule