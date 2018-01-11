!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
MODULE PRMS_BASIN
    use kinds_mod, only: r4, r8, i4, i8
    use dimensions_mod, only: dimension_list

    IMPLICIT NONE

    INTRINSIC :: EPSILON

    !   Local Variables
    CHARACTER(LEN=5), SAVE :: MODNAME
    INTEGER(i4), SAVE :: Active_hrus
    integer(i4), save :: Hemisphere
    real(r8), SAVE :: Basin_area_inv
    real(r8), save :: Basin_lat
    real(r8), save :: Totarea
    real(r8), save :: Active_area
    real(r8), save :: Land_area
    INTEGER(i4), SAVE, ALLOCATABLE :: Hru_route_order(:)
    CHARACTER(LEN=:), allocatable, SAVE :: Version_basin

    !   Declared Variables
    real(r4), allocatable, save :: Hru_frac_perv(:)
    real(r4), allocatable, save :: Hru_perv(:)
    real(r4), allocatable, save :: Hru_imperv(:)

    !   Declared Parameters
    INTEGER(i4), SAVE :: Elev_units
    INTEGER(i4), ALLOCATABLE, save :: Hru_type(:)
    INTEGER(i4), ALLOCATABLE, save :: Cov_type(:)
    REAL(r4), ALLOCATABLE, save :: Hru_area(:)
    REAL(r4), ALLOCATABLE, save :: Hru_percent_imperv(:)
    REAL(r4), ALLOCATABLE, save :: Hru_elev(:)
    REAL(r4), ALLOCATABLE, save :: Hru_lat(:)
    REAL(r4), ALLOCATABLE, save :: Covden_sum(:)
    REAL(r4), ALLOCATABLE, save :: Covden_win(:)

    private :: basdecl, basinit
    public :: basin

    contains
        !***********************************************************************
        !     Main basin routine
        !***********************************************************************
        INTEGER FUNCTION basin(dim_data, param_data)
            USE PRMS_MODULE, ONLY: Process
            use parameter_arr_mod, only: parameter_arr_t
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data
            type(parameter_arr_t), intent(inout) :: param_data

            !***********************************************************************
            basin = 0

            IF (Process == 'declare') THEN
                basin = basdecl(dim_data, param_data)
            ELSEIF (Process == 'init') THEN
                basin = basinit(param_data)
            ENDIF
        END FUNCTION basin

        !***********************************************************************
        !     basdecl - set up parameters
        !   Declared Parameters
        !     print_debug, hru_area, hru_percent_imperv, hru_type, hru_elev,
        !     cov_type, hru_lat, dprst_frac_open, dprst_frac, basin_area
        !     lake_hru_id
        !***********************************************************************
        INTEGER FUNCTION basdecl(dim_data, param_data)
            USE PRMS_MODULE, ONLY: Nhru, print_module
            use UTILS_PRMS, only: read_error
            use variables_mod, only: declvar_real
            use parameter_arr_mod, only: parameter_arr_t
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data
            type(parameter_arr_t), intent(inout) :: param_data

            !***********************************************************************
            basdecl = 0

            Version_basin = 'basin.f90 2017-09-29 13:50:00Z'
            CALL print_module(Version_basin, 'Basin Definition            ', 90)
            MODNAME = 'basin'

            ! Declared Variables
            ALLOCATE (Hru_imperv(Nhru))
            CALL declvar_real(MODNAME, 'hru_imperv', 'nhru', Nhru, 'real', 'Area of HRU that is impervious', 'acres', Hru_imperv)

            ALLOCATE (Hru_perv(Nhru))
            CALL declvar_real(MODNAME, 'hru_perv', 'nhru', Nhru, 'real', 'Area of HRU that is pervious', 'acres', Hru_perv)

            ALLOCATE (Hru_frac_perv(Nhru))
            CALL declvar_real(MODNAME, 'hru_frac_perv', 'nhru', Nhru, 'real', 'Fraction of HRU that is pervious', 'decimal fraction', Hru_frac_perv)

            ! local arrays
            ALLOCATE (Hru_route_order(Nhru))

            ! Declared Parameters
            ALLOCATE (Hru_area(Nhru))
            IF (param_data%declparam(MODNAME, 'hru_area', 'nhru', 'real', '1.0', '0.0001', '1.0E9', &
                    &     'HRU area', 'Area of each HRU', 'acres', dim_data) /= 0) CALL read_error(1, 'hru_area')

            IF (param_data%declparam(MODNAME, 'elev_units', 'one', 'integer', '0', '0', '1', &
                    &     'Elevation units flag', 'Flag to indicate the units of the elevation values (0=feet; 1=meters)', &
                    &     'none', dim_data) /= 0) CALL read_error(1, 'elev_units')

            ALLOCATE (Hru_elev(Nhru))
            IF (param_data%declparam(MODNAME, 'hru_elev', 'nhru', 'real', '0.0', '-1000.0', '30000.0', &
                    &     'HRU mean elevation', 'Mean elevation for each HRU', &
                    &     'elev_units', dim_data) /= 0) CALL read_error(1, 'hru_elev')

            ALLOCATE (Hru_lat(Nhru))
            IF (param_data%declparam(MODNAME, 'hru_lat', 'nhru', 'real', '40.0', '-90.0', '90.0', &
                    &     'HRU latitude', 'Latitude of each HRU', &
                    &     'degrees North', dim_data) /= 0) CALL read_error(1, 'hru_lat')

            ALLOCATE (Hru_percent_imperv(Nhru))
            IF (param_data%declparam(MODNAME, 'hru_percent_imperv', 'nhru', 'real', '0.0', '0.0', '0.999', &
                    &     'HRU percent impervious', 'Fraction of each HRU area that is impervious', &
                    &     'decimal fraction', dim_data) /= 0) CALL read_error(1, 'hru_percent_imperv')

            ALLOCATE (Hru_type(Nhru))
            IF (param_data%declparam(MODNAME, 'hru_type', 'nhru', 'integer', '1', '0', '3', &
                    &     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake; 3=swale)', &
                    &     'none', dim_data) /= 0) CALL read_error(1, 'hru_type')

            ALLOCATE (Cov_type(Nhru))
            IF (param_data%declparam(MODNAME, 'cov_type', 'nhru', 'integer', '3', '0', '4', &
                    &     'Cover type designation for HRU', &
                          'Vegetation cover type for each HRU (0=bare soil; 1=grasses; 2=shrubs; 3=trees; 4=coniferous)', &
                    &     'none', dim_data) /= 0) CALL read_error(1, 'cov_type')

            ALLOCATE (Covden_sum(Nhru))
            IF (param_data%declparam(MODNAME, 'covden_sum', 'nhru', 'real', '0.5', '0.0', '1.0', &
                    &     'Summer vegetation cover density for major vegetation type', &
                    &     'Summer vegetation cover density for the major vegetation type in each HRU', &
                    &     'decimal fraction', dim_data) /= 0) CALL read_error(1, 'covden_sum')

            ALLOCATE (Covden_win(Nhru))
            IF (param_data%declparam(MODNAME, 'covden_win', 'nhru', 'real', '0.5', '0.0', '1.0', &
                    &     'Winter vegetation cover density for major vegetation type', &
                    &     'Winter vegetation cover density for the major vegetation type in each HRU', &
                    &     'decimal fraction', dim_data) /= 0) CALL read_error(1, 'covden_win')
        END FUNCTION basdecl

        !**********************************************************************
        !     basinit - check for validity of basin parameters
        !               and compute reservoir areas
        !**********************************************************************
        INTEGER FUNCTION basinit(param_data)
            use fileio_mod, only: write_outfile
            USE PRMS_MODULE, ONLY: Nhru, Print_debug, Starttime, Endtime, Prms_output_unit
            use UTILS_PRMS, only: read_error
            use parameter_arr_mod, only: parameter_arr_t
            IMPLICIT NONE

            type(parameter_arr_t), intent(in) :: param_data

            ! Functions
            INTRINSIC ABS, DBLE, SNGL

            ! Local Variables
            CHARACTER(LEN=69) :: buffer
            INTEGER(i4) :: i, j
            REAL(r4) :: harea
            real(r8) :: basin_imperv
            real(r8) :: basin_perv
            real(r8) :: harea_dble

            !**********************************************************************
            basinit = 0

            IF (param_data%getparam(MODNAME, 'hru_area', Nhru, 'real', Hru_area) /= 0) CALL read_error(2, 'hru_area')
            IF (param_data%getparam(MODNAME, 'hru_elev', Nhru, 'real', Hru_elev) /= 0) CALL read_error(2, 'hru_elev')
            IF (param_data%getparam(MODNAME, 'hru_lat', Nhru, 'real', Hru_lat) /= 0) CALL read_error(2, 'hru_lat')
            IF (param_data%getparam(MODNAME, 'hru_type', Nhru, 'integer', Hru_type) /= 0) CALL read_error(2, 'hru_type')
            IF (param_data%getparam(MODNAME, 'cov_type', Nhru, 'integer', Cov_type) /= 0) CALL read_error(2, 'cov_type')
            IF (param_data%getparam(MODNAME, 'covden_sum', Nhru, 'real', Covden_sum) /= 0) CALL read_error(2, 'covden_sum')
            IF (param_data%getparam(MODNAME, 'covden_win', Nhru, 'real', Covden_win) /= 0) CALL read_error(2, 'covden_win')
            IF (param_data%getparam(MODNAME, 'elev_units', 1, 'integer', Elev_units) /= 0) CALL read_error(2, 'elev_units')
            IF (param_data%getparam(MODNAME, 'hru_percent_imperv', Nhru, 'real', Hru_percent_imperv) /= 0) CALL read_error(2, 'hru_percent_imperv')

            basin_perv = 0.0D0
            basin_imperv = 0.0D0
            Totarea = 0.0D0
            Land_area = 0.0D0
            Active_area = 0.0D0
            Basin_lat = 0.0D0
            Hru_route_order = 0
            j = 0

            DO i = 1, Nhru
                harea = Hru_area(i)
                harea_dble = DBLE(harea)
                Totarea = Totarea + harea_dble

                IF (Hru_type(i) == 0) CYCLE ! inactive
                ! ????????? need to fix for lakes with multiple HRUs and PRMS lake routing ????????
                Land_area = Land_area + harea_dble ! swale or land

                Basin_lat = Basin_lat + DBLE(Hru_lat(i) * harea)
                j = j + 1
                Hru_route_order(j) = i

                Hru_imperv(i) = Hru_percent_imperv(i) * harea
                Hru_perv(i) = harea - Hru_imperv(i)

                Hru_frac_perv(i) = Hru_perv(i) / harea
                basin_perv = basin_perv + DBLE(Hru_perv(i))
                basin_imperv = basin_imperv + DBLE(Hru_imperv(i))
            ENDDO

            Active_hrus = j
            Active_area = Land_area

            Basin_area_inv = 1.0D0 / Active_area
            Basin_lat = Basin_lat * Basin_area_inv

            ! used in solrad modules to winter/summer radiation adjustment
            IF (Basin_lat > 0.0D0) THEN
                Hemisphere = 0 ! Northern
            ELSE
                Hemisphere = 1 ! Southern
            ENDIF

            basin_perv = basin_perv * Basin_area_inv
            basin_imperv = basin_imperv * Basin_area_inv

            IF (Print_debug == 2) THEN
                PRINT *, ' HRU     Area'
                PRINT ('(I7, F14.5)'), (i, Hru_area(i), i = 1, Nhru)
                PRINT *, 'Model domain area     = ', Totarea
                PRINT *, 'Active basin area     = ', Active_area
                PRINT *, 'Fraction impervious   = ', basin_imperv
                PRINT *, 'Fraction pervious     = ', basin_perv
                PRINT *, ' '
            ENDIF

            !     print out start and end times
            IF (Print_debug > -2) THEN
                !CALL write_outfile(' Surface Water and Energy Budgets Simulated by '//PRMS_VERSION)
                WRITE (Prms_output_unit, '(1X)')

                WRITE (buffer, 9002) 'Start time: ', Starttime
                CALL write_outfile(buffer(:31))

                WRITE (buffer, 9002) 'End time:   ', Endtime
                CALL write_outfile(buffer(:31))

                WRITE (Prms_output_unit, '(1X)')
                WRITE (buffer, 9003) 'Model domain area:   ', Totarea, '    Active basin area:', Active_area
                CALL write_outfile(buffer)

                WRITE (buffer, 9004) 'Fraction impervious:  ', basin_imperv, '    Fraction pervious: ', basin_perv
                CALL write_outfile(buffer)
                CALL write_outfile(' ')
            ENDIF

            9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
            9003 FORMAT (2(A, F13.2))
            9004 FORMAT (2(A, F12.5))
            ! 9005 FORMAT (A, F13.2, A, F13.4)

        END FUNCTION basinit

END MODULE PRMS_BASIN
