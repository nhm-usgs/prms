!***********************************************************************
! Defines globals
!***********************************************************************
module PRMS_MODULE
    use kinds_mod, only: r4, r8, i4, i8
    use prms_constants, only: MAXFILE_LENGTH, MAXCONTROL_LENGTH
    use data_mod, only: str_arr_type
    implicit none

    character(len=*), PARAMETER :: MODNAME = 'prms6'
    character(len=*), PARAMETER :: PRMS_VERSION = 'Version 6.0.0 09/29/2017'

    character(len=:), allocatable, save :: Process   ! one of: setdims, declare, init, run, or clean
    character(len=:), allocatable, save :: PRMS_versn
    integer(i4), save :: Model
    integer(i4), save :: Number_timesteps
    integer(i4), save :: Nhru
    integer(i4), save :: Ntemp
    integer(i4), save :: Nrain
    integer(i4), save :: Nobs
    integer(i4), allocatable, save :: Starttime(:)  !> Simulation start date and time (YY MM DD hh mm ss)
    integer(i4), allocatable, save :: Endtime(:)    !> Simulation end date and time (YY MM DD hh mm ss)
    integer(i4), save :: Start_year
    integer(i4), save :: Start_month
    integer(i4), save :: Start_day
    integer(i4), save :: End_year
    integer(i4), save :: End_month
    integer(i4), save :: End_day

    ! integer(i4), save :: Inputerror_flag
    integer(i4), save :: Timestep
    integer(i4), save :: Prms_warmup
    integer(i4), save :: PRMS_output_unit
    integer(i4), save :: Restart_inunit
    integer(i4), save :: Restart_outunit
    integer(i4), save :: Elapsed_time_start(8)
    integer(i4), save :: Elapsed_time_end(8)
    integer(i4), save :: Elapsed_time_minutes
    character(len=:), allocatable, save :: Version_read_control_file
    character(len=:), allocatable, save :: Version_read_parameter_file
    real(r4), save :: Execution_time_start, Execution_time_end, Elapsed_time

    ! Control parameters
    integer(i4), save :: Print_debug            !> Flag to indicate type of debug output
                                                !! (-2=minimal output to screen and no model_output_file;
                                                !!  -1=minimize screen output;
                                                !!   0=none;
                                                !!   1=water balances;
                                                !!   2=basin module;
                                                !!   4=basin_sum module;
                                                !!   5=soltab module;
                                                !!   7=soilzone module;
                                                !!   9=snowcomp module;
                                                !!  13=cascade module;
                                                !!  14=subbasin module)
    integer(i4), save :: Parameter_check_flag   !> Flag to indicate if selected parameter values validations checks
                                                !! are treated as warning or errors (0=no; 1=yes; 2=check parameters then stop)

    integer(i4), save :: Init_vars_from_file    !> Flag to specify whether the Initial Conditons File is
                                                !! specified as an input file (0=no; 1=yes;
                                                !! 2=yes and us parameter values in Parameter File instead of values in the Initial Conditions File)
    integer(i4), save :: Save_vars_to_file      !> Flag to determine if an Initial Conditions File will be generated at
                                                !! the end of the simulation (0=no; 1=yes)
    integer(i4), save :: NhruOutON_OFF          !> Switch to specify whether nhru summary output files are generated (0=no; 1=yes)
    integer(i4), save :: BasinOutON_OFF         !> Switch to specify whether basin summary output files are generated (0=no; 1=yes)
    character(len=:), allocatable, save :: Model_output_file    !> Full path for Water-Budget File for results from module basin_sum
    character(len=:), allocatable, save :: Var_init_file    !> Pathname for Initial Conditions input file
    character(len=:), allocatable, save :: Var_save_file    !> Pathname for the Initial Conditions File to be generated at end of simulation
    character(len=:), allocatable, save :: Param_file       !> Name of parameter file
    character(len=:), allocatable, save :: Model_control_file
    character(len=:), allocatable, save :: Model_mode   !> Flag to indicate model mode
                                                        !! (PRMS, FROST, WRITE_CLIMATE, POTET, TRANSPIRE, DOCUMENTATION)

    character(len=:), allocatable, save :: Temp_module  !> Module name for temperature distribution method
                                                        !! (climate_hru, temp_1sta, temp_dist2, temp_laps, ide_dist, xyz_dist)
    character(len=:), allocatable, save :: Et_module    !> Module name for potential evapotranspiration method
                                                        !! (climate_hru, potet_jh, potet_hamon, potet_hs, potet_pt, potet_pm_sta, potet_pan)
    character(len=:), allocatable, save :: Transp_module    !> Module name for transpiration simulation method
                                                            !! (climate_hru, transp_frost, transp_tindex)
    character(len=:), allocatable, save :: Precip_module    !> Module name for precipitation-distribution method
                                                            !! (climate_hru, ide_dist, precip_1sta, precip_dist2, precip_laps, xyz_dist)
    character(len=:), allocatable, save :: Solrad_module    !> Module name for solar-radiation-distribution method
                                                            !! (ccsolrad, ddsolrad)

    ! Control parameters for basin_summary.f90
    integer(i4), save :: BasinOutVars
    integer(i4), save :: BasinOut_freq
    ! character(len=:), save, ALLOCATABLE :: BasinOutVar_names(:)
    type(str_arr_type), allocatable, save :: BasinOutVar_names(:)
    character(len=:), allocatable, save :: BasinOutBaseFileName

    ! Control parameters for nhru_summary.f90
    integer(i4), save :: NhruOutVars
    integer(i4), save :: NhruOut_freq
    ! character(len=:), allocatable, save :: NhruOutVar_names(:)
    type(str_arr_type), allocatable, save :: NhruOutVar_names(:)
    character(len=:), allocatable, save :: NhruOutBaseFileName

    ! Control Parameters for climate_hru.f90
    character(len=:), allocatable, save :: Tmin_day
    character(len=:), allocatable, save :: Tmax_day
    character(len=:), allocatable, save :: Precip_day

    ! used by read_control_file.f90
    character(len=:), allocatable, save :: Data_file
    character(len=:), allocatable, save :: Stat_var_file

    contains
        !***********************************************************************
        ! print_module
        ! print module version information to the screen
        !***********************************************************************
        subroutine print_module(Versn, Description, Ftntype)
            implicit none

            ! Arguments
            character(len=*), intent(in) :: Description
            character(len=*), intent(in) :: Versn
            integer(i4), intent(in) :: Ftntype

            ! Functions
            INTRINSIC INDEX, TRIM

            ! Local Variables
            integer(i4) :: nc
            integer(i4) :: n
            integer(i4) :: nb
            integer(i4) :: is
            character(len=28) :: blanks
            character(len=80) :: string

            !*******************************************************************
            if (Print_debug == -2) return

            nc = INDEX(Versn, 'Z') - 10
            n = INDEX(Versn, '.f') - 1
            if (n < 1) n = 1

            if (Ftntype == 90) then
                is = 5
            else
                is = 3
            endif

            blanks = ' '
            nb = 29 - (n + 3)
            string = Description // '   ' // Versn(:n) // blanks(:nb) // Versn(n + is:nc)
            print '(A)', TRIM(string)
            !      WRITE ( Logunt, '(A)' ) TRIM( string )
            if (Model /= 2) WRITE (PRMS_output_unit, '(A)') TRIM(string)
        end subroutine print_module
end module PRMS_MODULE
