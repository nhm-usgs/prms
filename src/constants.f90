module prms_constants
    use kinds_mod, only: r4, r8, i4, i8
    implicit none
    ! use data_mod, only: str_arr_type
    ! use arr_mod, only: array_1D_t
    ! implicit none
    !
    ! type(array_1D_t) :: valid_model_modes
    ! type(str_arr_type), allocatable :: tmp_arr_1D(:) ! use as 1D temporary array
    !
    ! allocate(tmp_arr_1D(4))
    ! tmp_arr_1D(1)%str = 'PRMS'
    ! tmp_arr_1D(2)%str = 'DAILY'
    ! tmp_arr_1D(3)%str = 'WRITE_CLIMATE'
    ! tmp_arr_1D(4)%str = 'DOCUMENTATION'
    !
    ! ! define the valid model modes
    ! valid_model_modes = tmp_arr_1D
    ! deallocate(tmp_arr_1D)


    ! from prms6.f90
    INTEGER(i4), PARAMETER :: MAXFILE_LENGTH = 256
    integer(i4), parameter :: MAXCONTROL_LENGTH = 32
    INTEGER(i4), PARAMETER :: MAXDIM = 500
    CHARACTER(LEN=*), PARAMETER :: EQULS = '===================================================================='
    character(len=*), parameter :: DIM_HEADER = '** Dimensions **'
    character(len=*), parameter :: PARAM_HEADER = '** Parameters **'
    character(len=*), parameter :: ENTRY_DELIMITER = '####'

    ! from mmf_utils.f90
    ! DANGER, DANGER, hard coded maximum number of paraemters and dimensions, DANGER, DANGER
    ! INTEGER, PARAMETER :: MAXDIMENSIONS = 50
    integer(i4), parameter :: MAXPARAMETERS = 200
    integer(i4), parameter :: MAXVARIABLES = 100

    ! from read_control_file.f90
    INTEGER(i4), PARAMETER :: Max_num_control_parameters = 150 ! WARNING, hard coded, DANGER, DANGER

    real(r8), PARAMETER :: SECS_PER_DAY = 86400D0
    real(r8), PARAMETER :: SECS_PER_HOUR = 3600D0
    real(r4), PARAMETER :: MIN_PER_HOUR = 60
    real(r4), PARAMETER :: HOUR_PER_DAY = 24

    REAL(r4), PARAMETER :: NEARZERO = EPSILON(0.0)
    real(r8), PARAMETER :: FT2_PER_ACRE = 43560.0D0
    real(r8), parameter :: DNEARZERO = EPSILON(0.0D0)
    real(r8), PARAMETER :: CFS2CMS_CONV = 0.028316847D0
    REAL(r4), PARAMETER :: INCH2MM = 25.4
    real(r4), parameter :: INCH2M = 0.0254
    real(r4), parameter :: MAXTEMP = 200.0
    real(r4), parameter :: MINTEMP = -150.0
    REAL(r4), PARAMETER :: MM2INCH = 1.0 / INCH2MM
end module prms_constants
