!***********************************************************************
! Computes the potential evapotranspiration using the Jensen-Haise
! formulation (Jensen and others, 1970)
!     Potet = Coef_t_mean*(Tavgf-Temp_x_mean)*Swrad/elh
!***********************************************************************
MODULE PRMS_POTET_JH
    use kinds_mod, only: r4, r8, i4, i8
    IMPLICIT NONE

    ! Local Variable
    CHARACTER(LEN=8), SAVE :: MODNAME

    ! Declared Parameters
    REAL(r4), SAVE, ALLOCATABLE :: Jh_coef(:, :), Jh_coef_hru(:)

    private
    public :: potet_jh

    contains
        INTEGER FUNCTION potet_jh(dim_data)
            USE PRMS_MODULE, ONLY: Process, Nhru, print_module
            USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_area, Hru_route_order
            USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Tavgf, Swrad
            USE PRMS_SET_TIME, ONLY: Nowmonth
            use UTILS_PRMS, only: read_error
            use parameter_mod, only: declparam, getparam
            ! use PRMS_MMFAPI, only: declparam, getparam
            use dimensions_mod, only: dimension_list
            IMPLICIT NONE

            type(dimension_list), intent(in) :: dim_data

            ! Functions
            INTRINSIC DBLE

            ! Local Variables
            INTEGER(i4) :: i, j
            REAL(r4) :: elh
            CHARACTER(LEN=80), SAVE :: Version_potet_jh

            !***********************************************************************
            potet_jh = 0

            IF (Process == 'run') THEN
                !***********************************************************************
                ! 597.3 cal/gm at 0 C is the energy required to change the state of
                ! water to vapor
                ! elh is the latent heat of vaporization (not including the *2.54)
                Basin_potet = 0.0D0
                DO j = 1, Active_hrus
                    i = Hru_route_order(j)
                    elh = (597.3 - (0.5653 * Tavgc(i))) * 2.54
                    Potet(i) = Jh_coef(i, Nowmonth) * (Tavgf(i) - Jh_coef_hru(i)) * Swrad(i) / elh
                    IF (Potet(i) < 0.0) Potet(i) = 0.0
                    Basin_potet = Basin_potet + DBLE(Potet(i) * Hru_area(i))
                ENDDO
                Basin_potet = Basin_potet * Basin_area_inv

                !******Declare parameters
            ELSEIF (Process == 'declare') THEN
                Version_potet_jh = 'potet_jh.f90 2016-05-10 15:48:00Z'
                CALL print_module(Version_potet_jh, 'Potential Evapotranspiration', 90)
                MODNAME = 'potet_jh'

                ALLOCATE (Jh_coef(Nhru, 12))
                IF (declparam(MODNAME, 'jh_coef', 'nhru,nmonths', 'real', '0.014', '0.0', '0.1', &
                        'Monthly air temperature coefficient for each HRU - Jensen-Haise', &
                        'Monthly (January to December) air temperature coefficient used in Jensen-Haise ' // &
                        'potential ET computations for each HRU', &
                        'per degrees Fahrenheit', dim_data) /= 0) CALL read_error(1, 'jh_coef')

                ALLOCATE (Jh_coef_hru(Nhru))
                IF (declparam(MODNAME, 'jh_coef_hru', 'nhru', 'real', '13.0', '-99.0', '150.0', &
                        &       'HRU air temperature coefficient - Jensen-Haise', &
                        &       'Air temperature coefficient used in Jensen-Haise potential ET computations for each HRU', &
                        &       'degrees Fahrenheit', dim_data) /= 0) CALL read_error(1, 'jh_coef_hru')

                !******Get parameters
            ELSEIF (Process == 'init') THEN
                IF (getparam(MODNAME, 'jh_coef', Nhru * 12, 'real', Jh_coef) /= 0) CALL read_error(2, 'jh_coef')
                IF (getparam(MODNAME, 'jh_coef_hru', Nhru, 'real', Jh_coef_hru) /= 0) CALL read_error(2, 'jh_coef_hru')

            ENDIF
        END FUNCTION potet_jh

END MODULE PRMS_POTET_JH
