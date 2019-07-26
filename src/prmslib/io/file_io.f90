
module fileio_mod
    ! use kinds_mod, only: r4, r8, i4, i8
    implicit none

    contains
    !***********************************************************************
    ! write_outfile - print to model output file
    !***********************************************************************
    ! SUBROUTINE write_outfile(out_string)
    !     USE PRMS_MODULE, ONLY: PRMS_output_unit !, Print_debug
    !     IMPLICIT NONE
    !
    !     ! Functions
    !     INTRINSIC LEN_TRIM
    !
    !     ! Arguments
    !     CHARACTER(LEN=*), INTENT(IN) :: out_string
    !
    !     !***********************************************************************
    !     ! IF (Print_debug == -2) RETURN
    !
    !     IF (len_trim(out_string) > 0) THEN
    !         WRITE(PRMS_output_unit, '(A)') out_string
    !     ELSE
    !         WRITE(PRMS_output_unit, '(/)')
    !     ENDIF
    ! END SUBROUTINE write_outfile

end module fileio_mod
