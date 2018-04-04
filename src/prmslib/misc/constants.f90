module prms_constants
    use variableKind

    implicit none

    ! from prms6.f90
    INTEGER(i32), PARAMETER :: MAXFILE_LENGTH = 256
    integer(i32), parameter :: MAXCONTROL_LENGTH = 32
    INTEGER(i32), PARAMETER :: MAXDIM = 500
    CHARACTER(LEN=*), PARAMETER :: EQULS = '===================================================================='
    character(len=*), parameter :: DIM_HEADER = '** Dimensions **'
    character(len=*), parameter :: PARAM_HEADER = '** Parameters **'
    character(len=*), parameter :: ENTRY_DELIMITER = '####'

    ! from mmf_utils.f90
    ! DANGER, DANGER, hard coded maximum number of paraemters and dimensions, DANGER, DANGER
    ! INTEGER, PARAMETER :: MAXDIMENSIONS = 50
    ! integer(i32), parameter :: MAXPARAMETERS = 200
    ! integer(i32), parameter :: MAXVARIABLES = 100

    ! from read_control_file.f90
    ! INTEGER(i32), PARAMETER :: Max_num_control_parameters = 150 ! WARNING, hard coded, DANGER, DANGER

    real(r64), PARAMETER :: SECS_PER_DAY = 86400D0
    real(r64), PARAMETER :: SECS_PER_HOUR = 3600D0
    real(r32), PARAMETER :: MIN_PER_HOUR = 60
    real(r32), PARAMETER :: HOUR_PER_DAY = 24

    REAL(r32), PARAMETER :: NEARZERO = EPSILON(0.0)
    real(r64), PARAMETER :: FT2_PER_ACRE = 43560.0D0
    real(r64), parameter :: DNEARZERO = EPSILON(0.0D0)
    real(r64), PARAMETER :: CFS2CMS_CONV = 0.028316847D0
    REAL(r32), PARAMETER :: INCH2MM = 25.4
    real(r32), parameter :: INCH2M = 0.0254

    ! TODO: what units are MAXTEMP and MINTEMP?
    real(r32), parameter :: MAXTEMP = 200.0
    real(r32), parameter :: MINTEMP = -150.0

    REAL(r32), PARAMETER :: MM2INCH = 1.0 / INCH2MM

    ! Frequency values
    ! Used for basinOut_freq, nhruOut_freq, and nsubOut_freq
    integer(i32), parameter :: DAILY = 1
    integer(i32), parameter :: MONTHLY = 2
    integer(i32), parameter :: DAILY_MONTHLY = 3
    integer(i32), parameter :: MEAN_MONTHLY = 4
    integer(i32), parameter :: MEAN_YEARLY = 5
    integer(i32), parameter :: YEARLY = 6

    ! Model modes
    integer(i32), parameter :: PRMS = 1
    integer(i32), parameter :: WRITE_CLIMATE = 4
    integer(i32), parameter :: CLIMATE = 6
    integer(i32), parameter :: POTET = 7
    integer(i32), parameter :: TRANSPIRE = 8
    integer(i32), parameter :: FROST = 9
    integer(i32), parameter :: CONVERT = 10
    integer(i32), parameter :: DOCUMENTATION = 99

    ! Hemisphere constants
    integer(i32), parameter :: NORTHERN = 0
    integer(i32), parameter :: SOUTHERN = 0

    ! Date time index constants
    integer(i32), parameter :: YEAR = 1
    integer(i32), parameter :: MONTH = 2
    integer(i32), parameter :: DAY = 3
    integer(i32), parameter :: HOUR = 4
    integer(i32), parameter :: MINUTE = 5
    integer(i32), parameter :: SECOND = 6

    ! Temperature units
    integer(i32), parameter :: FAHRENHEIT = 0
    integer(i32), parameter :: CELSIUS = 1

    ! Precipitation units
    integer(i32), parameter :: INCHES = 0
    integer(i32), parameter :: MM = 1
end module prms_constants
