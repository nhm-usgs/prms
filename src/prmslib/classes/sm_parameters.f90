submodule (Parameters_class) sm_parameters

contains
  !====================================================================!
  module function constructor_Parameters(Control_data) result(this)
    use UTILS_PRMS, only: print_module_info
    implicit none

    type(Parameters) :: this
    class(Control), intent(in) :: Control_data

    ! --------------------------------------------------------------------------
    associate(print_debug => Control_data%print_debug%value, &
              param_file => Control_data%param_file%values)

      if (print_debug > -2) then
        ! Output module and version information
        call print_module_info(MODNAME, MODDESC, MODVERSION)
      endif

      this%parameter_filenames = Control_data%param_file

      ! TODO: if print_debug > -1 output parameter file to stdout
      !       if print_debug > -2 output parameter file to model_output_file
    end associate

    call this%read()
  end function

  module subroutine read_parameters(this)
    use iso_fortran_env
    use variableKind
    use m_errors, only: eMsg
    use m_fileIO, only: openFile, closeFile
    use m_strings, only: compact, str
    use prms_constants, only: ENTRY_DELIMITER
    implicit none

    class(Parameters), intent(inout) :: this

    integer(i32) :: istat
      !! Contains the IOSTAT result from a read command
    integer(i32) :: iUnit
      !! Unit of the opened control file
    integer(i32) :: line
      !! Tracks the number of the last line read in the file
    character(len=cLen) :: buf
      !! Buffer for reading control file
    character(len=:), allocatable :: last
      !! Previous line read from file
    ! type(Abc), pointer :: ptr

    logical :: go

    integer(i32), parameter :: ENTRY_OFFSET = 2
    integer(i32) :: ii
      !! counter variable
    integer(i32) :: N
      !! used for getting declared number of dimension names or parameter values
    integer(i32) :: k
    integer(i32) :: numfiles

    !***********************************************************************

    ! NOTE: Dimensions no longer stored in the parameter file. They
    !       are now stored in the control file.

    ! Read all parameters and verify
    numfiles = size(this%parameter_filenames%values)

    do k = 1, numfiles
      ! Open parameter file
      call openFile(this%parameter_filenames%values(k)%s, iunit, stat='old', istat=istat)

      ! Read the Header line
      read(iUnit, 1) buf
      line = 1
      last = 'Header_1'

      read(iUnit, 1) buf
      line = line + 1
      last = 'Header_2'

      ! Read the next line - should be '####'
      read(iUnit, 1) buf
      call compact(buf)
      line = line + 1

      ! write(*, *) '-- reading' // this%parameter_filenames%values(k)%s

      go = .true.
      do while (go)
        ! read (iUnit, '(A)', IOSTAT=istat) buf
        ! line = line + 1
        ! if (istat == IOSTAT_END) EXIT ! found end of a Parameter File

        ! if (buf(:4) == '    ') CYCLE ! skip blank lines
        ! if (buf(:2) == '//') CYCLE ! skip comment lines

        if (buf(:4) == ENTRY_DELIMITER) then
          ! ~~~~~~~~~~~~~~~~~~
          ! Parameter name
          read(iUnit, '(A)', IOSTAT=istat) buf
          line = line + 1
          last = trim(buf)

          ! ##########################################
          ! select-case statements from external file
          include 'sm_parameter_case_block.inc'
          ! ##########################################

        else
          call eMsg(" Could not read from file " // this%parameter_filenames%values(k)%s // &
                    " for entry " // last // " at line " // str(line))
        endif

        read(iUnit, 1, IOSTAT=istat) buf
        if (istat == IOSTAT_END) exit
        call compact(buf)
        line = line + 1
      enddo

      call closeFile(this%parameter_filenames%values(k)%s, iUnit, '', istat)
      1   format(a)
    enddo
  end subroutine
end submodule
