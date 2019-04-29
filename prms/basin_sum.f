!***********************************************************************
! Computes daily, monthly, yearly, and total flow summaries of volumes
! and flows for all HRUs
!***********************************************************************
      MODULE PRMS_BASINSUM
!   Local Variables
      INTEGER, PARAMETER :: BALUNT = 199
      INTEGER, SAVE :: Header_prt, Endjday, Endyr
      CHARACTER(LEN=32) :: Buffer32
      CHARACTER(LEN=40) :: Buffer40
      CHARACTER(LEN=48) :: Buffer48
      CHARACTER(LEN=80) :: Buffer80
      CHARACTER(LEN=120) :: Buffer120
      CHARACTER(LEN=160) :: Buffer160
      CHARACTER(LEN=146), PARAMETER :: DASHS =
     +' -----------------------------------------------------'//
     +'--------------------------------------------------------------'//
     +'------------------------------'
      CHARACTER(LEN=140), PARAMETER :: STARS =
     +' *****************************************************'//
     +'**************************************************************'//
     +'************************'
      CHARACTER(LEN=140), PARAMETER :: EQULS =
     +' ====================================================='//
     +'=============================================================='//
     +'========================'
!   Declared Private Variables
      INTEGER, SAVE :: Totdays
      LOGICAL, SAVE :: Dprt, Mprt, Yprt, Tprt
      REAL, SAVE :: Obs_runoff_mo, Obs_runoff_yr, Obs_runoff_tot
      REAL, SAVE :: Basin_cfs_mo, Basin_cfs_yr, Basin_cfs_tot
      REAL, SAVE :: Basin_net_ppt_yr, Basin_net_ppt_tot, Watbal_sum
      REAL, SAVE :: Basin_max_temp_yr, Basin_max_temp_tot
      REAL, SAVE :: Basin_min_temp_yr, Basin_min_temp_tot
      REAL, SAVE :: Basin_potet_yr, Basin_potet_tot
      REAL, SAVE :: Basin_actet_yr, Basin_actet_tot
      REAL, SAVE :: Basin_et_yr, Basin_et_tot
      REAL, SAVE :: Basin_snowmelt_yr, Basin_snowmelt_tot
      REAL, SAVE :: Basin_gwflow_yr, Basin_gwflow_tot
      REAL, SAVE :: Basin_ssflow_yr, Basin_ssflow_tot
      REAL, SAVE :: Basin_sroff_yr, Basin_sroff_tot
      REAL, SAVE :: Basin_stflow_yr, Basin_stflow_tot
      REAL, SAVE :: Basin_ppt_yr, Basin_ppt_tot, Last_basin_stor
      REAL, SAVE :: Basin_intcp_evap_yr, Basin_intcp_evap_tot
      REAL, SAVE :: Obsq_inches_yr, Obsq_inches_tot
!   Declared Variables
      REAL, SAVE :: Basin_net_ppt_mo
      REAL, SAVE :: Basin_max_temp_mo, Basin_min_temp_mo, Basin_potet_mo
      REAL, SAVE :: Basin_actet_mo, Basin_et_mo, Basin_ppt_mo
      REAL, SAVE :: Basin_snowmelt_mo, Basin_gwflow_mo, Basin_ssflow_mo
      REAL, SAVE :: Basin_sroff_mo, Basin_stflow_mo, Obsq_inches_mo
      REAL, SAVE :: Basin_intcp_evap_mo, Basin_storage, Basin_et
      REAL, SAVE, ALLOCATABLE :: Hru_et_yr(:)
      REAL, SAVE :: Basin_storvol, Obsq_inches
!   Declared Variables from other modules - srunoff
      DOUBLE PRECISION :: Basin_dprst_evap, Basin_dprst_volcl
      DOUBLE PRECISION :: Basin_dprst_volop
!   Declared Variables from other modules - strmflow
      DOUBLE PRECISION :: Basin_sfres_stor, Basin_2ndstflow
!   Declared Parameters
      INTEGER, SAVE :: Print_type, Print_freq, Outlet_sta
      
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='basin_sum')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Summary')
      
      END MODULE PRMS_BASINSUM

!***********************************************************************
!     Main basin_sum routine
!***********************************************************************
      INTEGER FUNCTION basin_sum()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: sumbdecl, sumbinit, sumbrun
!***********************************************************************
      basin_sum = 0

      IF ( Process(:3)=='run' ) THEN
        basin_sum = sumbrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        basin_sum = sumbdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        basin_sum = sumbinit()
      ENDIF

      END FUNCTION basin_sum

!***********************************************************************
!     sumbdecl - set up basin summary parameters
!   Declared Parameters
!     print_type, print_freq, outlet_sta
!***********************************************************************
      INTEGER FUNCTION sumbdecl()
      USE PRMS_BASINSUM
      USE PRMS_MODULE, ONLY: Model, Nhru, Print_debug,
     +    Version_basin_sum, Basin_sum_nc
      USE PRMS_OBS, ONLY: Nobs
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declpri, declparam, declvar
      INTEGER, EXTERNAL :: getdim
      EXTERNAL read_error
!***********************************************************************
      sumbdecl = 1

      Version_basin_sum =
     +'$Id: basin_sum.f 4077 2012-01-05 23:46:06Z rsregan $'
      IF ( Print_debug>-1 ) THEN
        Basin_sum_nc = INDEX( Version_basin_sum, ' $' ) + 1
        IF ( declmodule(MODNAME, PROCNAME,
     +   Version_basin_sum(:Basin_sum_nc))/=0 ) STOP
      ENDIF

      IF ( declpri('sumb_last_basin_stor', 1, 'real', Last_basin_stor)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_watbal_sum', 1, 'real', Watbal_sum)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obs_runoff_mo', 1, 'real', Obs_runoff_mo)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_cfs_mo', 1, 'real', Basin_cfs_mo)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obs_runoff_yr', 1, 'real', Obs_runoff_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_cfs_yr', 1, 'real', Basin_cfs_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_net_ppt_yr', 1, 'real', Basin_net_ppt_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_max_temp_yr', 1, 'real',
     +     Basin_max_temp_yr).NE.0 ) RETURN
      IF ( declpri('sumb_basin_min_temp_yr', 1, 'real',
     +     Basin_min_temp_yr).NE.0 ) RETURN
      IF ( declpri('sumb_basin_potet_yr', 1, 'real', Basin_potet_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_actet_yr', 1, 'real', Basin_actet_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_snowmelt_yr', 1, 'real',
     +     Basin_snowmelt_yr).NE.0 ) RETURN
      IF ( declpri('sumb_basin_gwflow_yr', 1, 'real', Basin_gwflow_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ssflow_yr', 1, 'real', Basin_ssflow_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_sroff_yr', 1, 'real', Basin_sroff_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ppt_yr', 1, 'real', Basin_ppt_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_et_yr', 1, 'real', Basin_et_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_stflow_yr', 1, 'real', Basin_stflow_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obsq_inches_yr', 1, 'real', Obsq_inches_yr)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_intcp_evap_yr', 1, 'real',
     +     Basin_intcp_evap_yr).NE.0 ) RETURN
      IF ( declpri('sumb_obs_runoff_tot', 1, 'real', Obs_runoff_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_cfs_tot', 1, 'real', Basin_cfs_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ppt_tot', 1, 'real', Basin_ppt_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_net_ppt_tot', 1, 'real',
     +     Basin_net_ppt_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_max_temp_tot', 1, 'real',
     +     Basin_max_temp_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_min_temp_tot', 1, 'real',
     +     Basin_min_temp_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_potet_tot', 1, 'real', Basin_potet_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_actet_tot', 1, 'real', Basin_actet_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_et_tot', 1, 'real', Basin_et_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_snowmelt_tot', 1, 'real',
     +     Basin_snowmelt_tot).NE.0 ) RETURN
      IF ( declpri('sumb_basin_gwflow_tot', 1, 'real', Basin_gwflow_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_ssflow_tot', 1, 'real', Basin_ssflow_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_sroff_tot', 1, 'real', Basin_sroff_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_stflow_tot', 1, 'real', Basin_stflow_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_obsq_inches_tot', 1, 'real', Obsq_inches_tot)
     +     .NE.0 ) RETURN
      IF ( declpri('sumb_basin_intcp_evap_tot', 1, 'real',
     +     Basin_intcp_evap_tot).NE.0 ) RETURN
      IF ( declpri('sumb_totdays', 1, 'integer', Totdays).NE.0 ) RETURN

! declare parameters
      IF ( Nobs>0 .OR. Model==99 ) THEN
        IF ( declparam(MODNAME, 'outlet_sta', 'one', 'integer',
     +       '1', 'bounded', 'nobs',
     +       'Index of measurement station to use for basin outlet',
     +       'Index of measured streamflow station corresponding to'//
     +       ' the basin outlet',
     +       'none').NE.0 ) CALL read_error(1, 'outlet_sta')
      ENDIF

      IF ( declparam(MODNAME, 'print_type', 'one', 'integer',
     +     '1', '0', '2',
     +     'Type of output written to output file',
     +     'Flag to select the type of results written to the output'//
     +     ' file (0=measured and simulated flow only;'//
     +     ' 1=water balance table; 2=detailed output)',
     +     'none').NE.0 ) CALL read_error(1, 'print_type')

      IF ( declparam(MODNAME, 'print_freq', 'one', 'integer',
     +     '1', '0', '15',
     +     'Frequency for the output frequency',
     +     'Flag to select the output frequency; for combinations,'//
     +     ' add index numbers, e.g., daily plus yearly = 10;'//
     +     ' yearly plus total = 3 (0=none; 1=run totals; 2=yearly;'//
     +     ' 4=monthly; 8=daily; or additive combinations)',
     +     'none').NE.0 ) CALL read_error(1, 'print_freq')

      IF ( declvar(MODNAME, 'basin_intcp_evap_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average interception evaporation',
     +     'inches',
     +     Basin_intcp_evap_mo).NE.0 )
     +     CALL read_error(3, 'basin_intcp_evap_mo')

      IF ( declvar(MODNAME, 'basin_storage', 'one', 1, 'real',
     +     'Basin area-weighted average storage in all water'//
     +     ' storage reservoirs',
     +     'inches',
     +     Basin_storage).NE.0 ) CALL read_error(3, 'basin_storage')

!******************basin_storage volume:
      IF ( declvar(MODNAME, 'basin_storvol', 'one', 1, 'real',
     +     'Basin area-weighted average storage volume in all'//
     +     ' water storage reservoirs',
     +     'acre-inches',
     +     Basin_storvol).NE.0 ) CALL read_error(3, 'basin_storvol')

      ! kept for downward compatibility, set equal to basin_actet
      IF ( declvar(MODNAME, 'basin_et', 'one', 1, 'real',
     +     'Basin area-weighted average actual ET',
     +     'inches',
     +     Basin_et)/=0 ) CALL read_error(3, 'basin_et')

      IF ( declvar(MODNAME, 'obsq_inches', 'one', 1, 'real',
     +     'Measured streamflow at specified outlet station',
     +     'inches',
     +     Obsq_inches).NE.0 ) CALL read_error(3, 'obsq_inches')

      IF ( declvar(MODNAME, 'basin_ppt_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average precipitation',
     +     'inches',
     +     Basin_ppt_mo).NE.0 ) CALL read_error(3, 'basin_ppt_mo')

      IF ( declvar(MODNAME, 'basin_net_ppt_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average net precipitation',
     +     'inches',
     +     Basin_net_ppt_mo).NE.0 )
     +     CALL read_error(3, 'basin_net_ppt_mo')

      IF ( declvar(MODNAME, 'basin_max_temp_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average maximum air temperature',
     +     'degrees',
     +     Basin_max_temp_mo).NE.0 )
     +     CALL read_error(3, 'basin_max_temp_mo')

      IF ( declvar(MODNAME, 'basin_min_temp_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average minimum air temperature',
     +     'degrees',
     +     Basin_min_temp_mo).NE.0 )
     +     CALL read_error(3, 'basin_min_temp_mo')

      IF ( declvar(MODNAME, 'basin_potet_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average potential ET',
     +     'inches',
     +     Basin_potet_mo).NE.0 ) CALL read_error(3, 'basin_potet_mo')

      IF ( declvar(MODNAME, 'basin_actet_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average actual ET',
     +     'inches',
     +     Basin_actet_mo).NE.0 ) CALL read_error(3, 'basin_actet_mo')

      IF ( declvar(MODNAME, 'basin_et_mo', 'one', 1, 'real',
     +     'Total monthly basin_et',
     +     'inches',
     +     Basin_et_mo).NE.0 ) RETURN

      IF ( declvar(MODNAME, 'basin_snowmelt_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average snowmelt',
     +     'inches',
     +     Basin_snowmelt_mo).NE.0 )
     +     CALL read_error(3, 'basin_snowmelt_mo')

      IF ( declvar(MODNAME, 'basin_gwflow_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average groundwater discharge',
     +     'inches',
     +     Basin_gwflow_mo).NE.0 ) CALL read_error(3, 'basin_gwflow_mo')

      IF ( declvar(MODNAME, 'basin_ssflow_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average interflow',
     +     'inches',
     +     Basin_ssflow_mo).NE.0 ) CALL read_error(3, 'basin_ssflow_mo')

      IF ( declvar(MODNAME, 'basin_sroff_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average surface runoff',
     +     'inches',
     +     Basin_sroff_mo).NE.0 ) CALL read_error(3, 'basin_sroff_mo')

      IF ( declvar(MODNAME, 'basin_stflow_mo', 'one', 1, 'real',
     +     'Monthly area-weighted average simulated streamflow',
     +     'inches',
     +     Basin_stflow_mo).NE.0 ) CALL read_error(3, 'basin_stflow_mo')

      IF ( declvar(MODNAME, 'obsq_inches_mo', 'one', 1, 'real',
     +     'Measured streamflow at specified outlet station',
     +     'inches',
     +     Obsq_inches_mo).NE.0 ) CALL read_error(3, 'obsq_inches_mo')

      ALLOCATE (Hru_et_yr(Nhru))
      IF ( declvar(MODNAME, 'hru_et_yr', 'nhru', Nhru, 'real',
     +     'Yearly area-weighted average actual ET for each HRU',
     +     'inches',
     +     Hru_et_yr).NE.0 ) CALL read_error(3, 'hru_et_yr')

      sumbdecl = 0
      END FUNCTION sumbdecl

!***********************************************************************
!     sumbinit - Initialize basinsum module - get parameter values
!                set to zero
!***********************************************************************
      INTEGER FUNCTION sumbinit()
      USE PRMS_BASINSUM
      USE PRMS_MODULE, ONLY: Strmflow_flag, Dprst_flag, Print_debug
      USE PRMS_BASIN, ONLY: Timestep, Endtime
      USE PRMS_FLOWVARS, ONLY: Basin_imperv_stor, Basin_soil_moist,
     +    Basin_ssstor
      USE PRMS_OBS, ONLY: Nobs
      USE PRMS_INTCP, ONLY: Basin_intcp_stor
      USE PRMS_SNOW, ONLY: Basin_pweqv
      USE PRMS_GWFLOW, ONLY: Basin_gwstor
      IMPLICIT NONE
      INTRINSIC MAX
      INTEGER, EXTERNAL :: getparam, getvar, julian
      EXTERNAL :: header_print, read_error, opstr
! Local Variables
      INTEGER :: pftemp
!***********************************************************************
      sumbinit = 1

      IF ( Nobs>0 ) THEN
        IF ( getparam(MODNAME, 'outlet_sta', 1, 'integer', Outlet_sta)
     +       .NE.0 ) CALL read_error(2, 'outlet_sta')
        IF ( Outlet_sta<1 .OR. Outlet_sta>Nobs ) THEN
          PRINT *, 'Warning, invalid value specified for outlet_sta:',
     +             Outlet_sta, ' set to nobs:', Nobs
          Outlet_sta = Nobs
        ENDIF
      ENDIF

      IF ( getparam(MODNAME, 'print_type', 1, 'integer', Print_type)
     +     .NE.0 ) CALL read_error(2, 'print_type')

      IF ( getparam(MODNAME, 'print_freq', 1, 'integer', Print_freq)
     +     .NE.0 ) CALL read_error(2, 'print_freq')

      IF ( Strmflow_flag==2 ) THEN
        IF ( getvar(MODNAME, 'basin_sfres_stor', 1, 'double',
     +       Basin_sfres_stor).NE.0 )
     +       CALL read_error(4, 'basin_sfres_stor')
      ELSE
        Basin_sfres_stor = 0.0D0
      ENDIF

!***********************************************************************
!    Next two variables for depression storage
      IF ( Dprst_flag>0 ) THEN
        IF ( getvar(MODNAME, 'basin_dprst_volcl', 1, 'double', 
     +       Basin_dprst_volcl).NE.0 )
     +       CALL read_error(4, 'basin_dprst_volcl')

        IF ( getvar(MODNAME, 'basin_dprst_volop', 1, 'double',
     +       Basin_dprst_volop).NE.0 )
     +       CALL read_error(4, 'basin_dprst_volop')

        IF ( getvar(MODNAME, 'basin_dprst_evap', 1, 'double', 
     +       Basin_dprst_evap).NE.0 )
     +       CALL read_error(4, 'basin_dprst_evap')
      ELSE
        Basin_dprst_volcl = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_evap = 0.0D0
      ENDIF

      Last_basin_stor = Basin_soil_moist + Basin_intcp_stor +
     +                  Basin_gwstor + Basin_ssstor + Basin_pweqv +
     +                  Basin_imperv_stor + Basin_sfres_stor +
     +                  Basin_dprst_volop + Basin_dprst_volcl

!******Set daily print switch
      pftemp = Print_freq

      IF ( pftemp.GE.8 ) THEN
        Dprt = .TRUE.
        pftemp = pftemp - 8
      ELSE
        Dprt = .FALSE.
      ENDIF

!******Set monthly print switch
      IF ( pftemp.GE.4 ) THEN
        Mprt = .TRUE.
        pftemp = pftemp - 4
      ELSE
        Mprt = .FALSE.
      ENDIF

!******Set yearly print switch
      IF ( pftemp.GE.2 ) THEN
        Yprt = .TRUE.
        pftemp = pftemp - 2
      ELSE
        Yprt = .FALSE.
      ENDIF

!******Set total print switch
      IF ( pftemp.EQ.1 ) THEN
        Tprt = .TRUE.
      ELSE
        Tprt = .FALSE.
      ENDIF

!  Zero stuff out when Timestep = 0
      IF ( Timestep==0 ) THEN
        Watbal_sum = 0.

!******Zero all mo yr to that aren't variables
        Obs_runoff_mo = 0.
        Basin_cfs_mo = 0.
        Basin_ppt_mo = 0.
        Basin_net_ppt_mo = 0.
        Basin_max_temp_mo = 0.
        Basin_min_temp_mo = 0.
        Basin_intcp_evap_mo = 0.
        Basin_potet_mo = 0.
        Basin_actet_mo = 0.
        Basin_et_mo = 0.
        Basin_snowmelt_mo = 0.
        Basin_gwflow_mo = 0.
        Basin_ssflow_mo = 0.
        Basin_sroff_mo = 0.
        Basin_stflow_mo = 0.
        Obsq_inches_mo = 0.

        Obs_runoff_yr = 0.
        Basin_cfs_yr = 0.
        Basin_ppt_yr = 0.
        Basin_net_ppt_yr = 0.
        Basin_max_temp_yr = 0.
        Basin_min_temp_yr = 0.
        Basin_intcp_evap_yr = 0.
        Basin_potet_yr = 0.
        Basin_actet_yr = 0.
        Basin_et_yr = 0.
        Basin_snowmelt_yr = 0.
        Basin_gwflow_yr = 0.
        Basin_ssflow_yr = 0.
        Basin_sroff_yr = 0.
        Basin_stflow_yr = 0.
        Obsq_inches_yr = 0.

        Obs_runoff_tot = 0.
        Basin_cfs_tot = 0.
        Basin_ppt_tot = 0.
        Basin_net_ppt_tot = 0.
        Basin_max_temp_tot = 0.
        Basin_min_temp_tot = 0.
        Basin_intcp_evap_tot = 0.
        Basin_potet_tot = 0.
        Basin_actet_tot = 0.
        Basin_et_tot = 0.
        Basin_snowmelt_tot = 0.
        Basin_gwflow_tot = 0.
        Basin_ssflow_tot = 0.
        Basin_sroff_tot = 0.
        Basin_stflow_tot = 0.
        Obsq_inches_tot = 0.

        Hru_et_yr = 0.

        Totdays = 0
        Obsq_inches = 0.

        Basin_storage = 0.
        Basin_storvol = 0.
        Basin_et = 0.
        Basin_2ndstflow = 0.
      ENDIF

!******Set header print switch (1 prints a new header after every month
!****** summary, 2 prints a new header after every year summary
      Header_prt = 0
      IF ( Print_freq.EQ.6 .OR. Print_freq.EQ.7 .OR. Print_freq.EQ.10
     +     .OR. Print_freq.EQ.11 ) Header_prt = 1
      IF ( Print_freq.GE.12 ) Header_prt = 2
!$$$ Commented out since this prevented detailed report at yearly
!$$$ frequency. Intent unclear. RMTW
!$$$      
      IF ( Print_freq.EQ.2 .OR. Print_freq.EQ.3 ) Print_type = 3
      IF ( Print_freq==0 .AND. Print_type==0 ) Print_type = 4

!  Put a header on the output file (regardless of Timestep)
!  when the model starts.
      CALL header_print(Print_type)

! rmtw - Print span dashes and initial storage
!
!
      IF ( Print_type.EQ.1 ) THEN
        WRITE (Buffer48, "(35X,F9.4)") Last_basin_stor
        CALL opstr(Buffer48(:44))

      ELSEIF ( Print_type.EQ.2 ) THEN
        WRITE (Buffer120, 9001) Basin_intcp_stor, 
     +                          Basin_soil_moist, Basin_pweqv,
     +                          Basin_gwstor, Basin_ssstor
        CALL opstr(Buffer120(:94))
      ENDIF

      Endjday = julian('end', 'calendar')
      Endyr = Endtime(1)

      IF ( Print_debug.EQ.4 ) OPEN (BALUNT, FILE='basin_sum.dbg')

 9001 FORMAT (39X, F6.2, 18X, 2F6.2, F13.2, F6.2)

      sumbinit = 0
      END FUNCTION sumbinit

!***********************************************************************
!     sumbrun - Computes summary values
!***********************************************************************
      INTEGER FUNCTION sumbrun()
      USE PRMS_BASINSUM
      USE PRMS_MODULE, ONLY: Strmflow_flag, Dprst_flag, Cascade_flag,
     +    Print_debug, Nhru
      USE PRMS_BASIN, ONLY: Cfs2inches, Basin_area_inv,
     +    Active_hrus, Hru_route_order, Basin_cfs, Basin_stflow
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_lakeevap,
     +    Basin_actet, Basin_perv_et, Basin_swale_et, Hru_actet,
     +    Basin_sroff, Strm_farfield, Basin_imperv_evap, Basin_ssstor,
     +    Basin_imperv_stor, Basin_soil_moist
      USE PRMS_CLIMATEVARS, ONLY: Orad, Basin_ppt, Basin_potet,
     +    Basin_tmax, Basin_tmin
      USE PRMS_OBS, ONLY: Jday, Nobs, Modays, Yrdays, Streamflow_cfs,
     +    Nratetbl, Julwater, Nowyear, Nowmonth, Nowday
      USE PRMS_GWFLOW, ONLY: Basin_gwflow, Basin_gwstor, Basin_gwsink
      USE PRMS_INTCP, ONLY: Basin_intcp_evap, Basin_intcp_stor,
     +    Basin_net_ppt
      USE PRMS_SNOW, ONLY: Basin_snowmelt, Basin_pweqv, Basin_snowevap
      IMPLICIT NONE
      INTRINSIC SNGL, ABS, ALOG
      INTEGER, EXTERNAL :: getvar
      EXTERNAL :: header_print, read_error, opstr
! Local variables
      INTEGER :: i, j, wyday, endrun
      REAL :: wat_bal, obsrunoff, strmfarflow_inches
!***********************************************************************
      sumbrun = 1

      wyday = Julwater

      IF ( Nowyear==Endyr .AND. Jday==Endjday ) THEN
        endrun = 1
      ELSE
        endrun = 0
      ENDIF

      IF ( Cascade_flag==1 ) THEN
        Basin_cfs = Basin_cfs - Strm_farfield
        strmfarflow_inches = Strm_farfield*Cfs2inches
      ELSE
        strmfarflow_inches = 0.0
      ENDIF

      IF ( Dprst_flag>0 ) THEN
!***** Two variable added for depression storage
        IF ( getvar(MODNAME, 'basin_dprst_volop', 1, 'double',
     +       Basin_dprst_volop).NE.0 )
     +       CALL read_error(4, 'basin_dprst_volop')

        IF ( getvar(MODNAME, 'basin_dprst_volcl', 1, 'double',
     +       Basin_dprst_volcl).NE.0 )
     +       CALL read_error(4, 'basin_dprst_volcl')

        IF ( getvar(MODNAME, 'basin_dprst_evap', 1, 'double',
     +       Basin_dprst_evap).NE.0 )
     +       CALL read_error(4, 'basin_dprst_evap')
      ENDIF

      IF ( Strmflow_flag==2 ) THEN
        IF ( getvar(MODNAME, 'basin_sfres_stor', 1, 'double',
     +       Basin_sfres_stor).NE.0 )
     +       CALL read_error(4, 'basin_sfres_stor')
        IF ( Nratetbl>0 ) THEN
          IF ( getvar(MODNAME, 'basin_2ndstflow', 1, 'double',
     +         Basin_2ndstflow).NE.0 )
     +         CALL read_error(4, 'basin_2ndstflow')
        ENDIF
      ENDIF

!*****Compute aggregated values

      Last_basin_stor = Last_basin_stor
      Basin_storage = Basin_soil_moist + Basin_intcp_stor +
     +                Basin_gwstor + Basin_ssstor + Basin_pweqv +
     +                Basin_imperv_stor
     +                + Basin_sfres_stor + Basin_dprst_volop
     +                + Basin_dprst_volcl

! volume calculation for storage
      Basin_storvol = Basin_storage/Basin_area_inv

      Basin_et = Basin_actet
      obsrunoff = 0.0
      IF ( Nobs>0 ) obsrunoff = Streamflow_cfs(Outlet_sta)
      Obsq_inches = obsrunoff*Cfs2inches

      wat_bal = Last_basin_stor - Basin_storage + Basin_ppt
     +          - Basin_actet - Basin_stflow - Basin_gwsink
     +          - Basin_2ndstflow - strmfarflow_inches

      IF ( Print_debug.EQ.4 ) THEN
        WRITE (BALUNT,"(A,2I4,7F8.4)") ' bsto-sm-in-gw-ss-sn-iv ',
     +         Nowmonth, Nowday, Basin_storage, Basin_soil_moist,
     +         Basin_intcp_stor, Basin_gwstor, Basin_ssstor,
     +         Basin_pweqv, Basin_imperv_stor
 
        WRITE (BALUNT,"(A,I6,8F8.4)")' bet-pv-iv-in-sn-lk-sw-dp',Nowday,
     +         Basin_actet, Basin_perv_et, Basin_imperv_evap,
     +         Basin_intcp_evap, Basin_snowevap + Basin_lakeevap +
     +         Basin_swale_et + Basin_dprst_evap

        WRITE (BALUNT,"(A,I6,7F8.4,/)") ' bal-pp-et-st-ls-bs-gs ',
     +         Nowday, wat_bal, Basin_ppt, Basin_actet, Basin_stflow,
     +         Last_basin_stor, Basin_storage, Basin_gwsink
      ENDIF

      Watbal_sum = Watbal_sum + wat_bal

      Last_basin_stor = Basin_storage

!******Check for daily print

      IF ( Dprt ) THEN
        IF ( Print_type.EQ.0 ) THEN
          WRITE (Buffer40, "(I7,2I5,F11.2,F12.2)") Nowyear,
     +           Nowmonth, Nowday, obsrunoff, Basin_cfs
          CALL opstr(Buffer40)

        ELSEIF ( Print_type.EQ.1 ) THEN
          WRITE (Buffer120,"(I7,2I5,8F9.3)") Nowyear,
     +           Nowmonth, Nowday, Basin_ppt, Basin_actet,Basin_storage,
     +           Basin_stflow-strmfarflow_inches, Obsq_inches, wat_bal,
     +           Watbal_sum, strmfarflow_inches
          CALL opstr(Buffer120(:89))

        ELSEIF ( Print_type.EQ.2 ) THEN
          WRITE ( Buffer160, 9001 ) Nowyear, Nowmonth, Nowday, Orad,
     +            Basin_tmax, Basin_tmin, Basin_ppt, Basin_net_ppt,
     +            Basin_intcp_stor, Basin_intcp_evap, Basin_potet,
     +            Basin_actet, Basin_soil_moist, Basin_pweqv,
     +            Basin_snowmelt, Basin_gwstor, Basin_ssstor,
     +            Basin_gwflow, Basin_ssflow, Basin_sroff,
     +            Basin_stflow-strmfarflow_inches, Basin_cfs,
     +            obsrunoff, Basin_lakeevap
          CALL opstr(Buffer160(:154))

        ENDIF
      ENDIF
      IF ( Print_debug.EQ.4 ) WRITE (BALUNT, *) 'wat_bal =', wat_bal,
     +                               ' watbal_sum=', Watbal_sum

!******Compute monthly values
      IF ( Nowday==1 ) THEN
        Obs_runoff_mo = 0.
        Basin_cfs_mo = 0.
        Basin_ppt_mo = 0.
        Basin_net_ppt_mo = 0.
        Basin_max_temp_mo = 0.
        Basin_min_temp_mo = 0.
        Basin_intcp_evap_mo = 0.
        Basin_potet_mo = 0.
        Basin_actet_mo = 0.
        Basin_et_mo = 0.
        Basin_snowmelt_mo = 0.
        Basin_gwflow_mo = 0.
        Basin_ssflow_mo = 0.
        Basin_sroff_mo = 0.
        Basin_stflow_mo = 0.
        Obsq_inches_mo = 0.
      ENDIF

      Obs_runoff_mo = Obs_runoff_mo + obsrunoff
      Obsq_inches_mo = Obsq_inches_mo + obsrunoff*Cfs2inches
      Basin_cfs_mo = Basin_cfs_mo + Basin_cfs
      Basin_ppt_mo = Basin_ppt_mo + Basin_ppt
      Basin_net_ppt_mo = Basin_net_ppt_mo + Basin_net_ppt
      Basin_max_temp_mo = Basin_max_temp_mo + Basin_tmax
      Basin_min_temp_mo = Basin_min_temp_mo + Basin_tmin
      Basin_intcp_evap_mo = Basin_intcp_evap_mo + Basin_intcp_evap
      Basin_potet_mo = Basin_potet_mo + Basin_potet
      Basin_actet_mo = Basin_actet_mo + Basin_actet
      Basin_et_mo = Basin_et_mo + Basin_et
      Basin_snowmelt_mo = Basin_snowmelt_mo + Basin_snowmelt
      Basin_gwflow_mo = Basin_gwflow_mo + Basin_gwflow
      Basin_ssflow_mo = Basin_ssflow_mo + Basin_ssflow
      Basin_sroff_mo = Basin_sroff_mo + Basin_sroff
      Basin_stflow_mo = Basin_stflow_mo + Basin_stflow

      IF ( Nowday==Modays(Nowmonth) ) THEN
        Basin_max_temp_mo = Basin_max_temp_mo/Modays(Nowmonth)
        Basin_min_temp_mo = Basin_min_temp_mo/Modays(Nowmonth)
        Obs_runoff_mo = Obs_runoff_mo/Modays(Nowmonth)
        Basin_cfs_mo = Basin_cfs_mo/Modays(Nowmonth)

        IF ( Mprt ) THEN
          IF ( Print_type.EQ.0 ) THEN
            IF ( Dprt ) CALL opstr(DASHS(:40))
            WRITE (Buffer40, "(I7,I5,F16.2,F12.2)") Nowyear,
     +             Nowmonth, Obs_runoff_mo, Basin_cfs_mo
            CALL opstr(Buffer40)

          ELSEIF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            IF ( Dprt ) CALL opstr(DASHS(:62))
            WRITE (Buffer80, "(I7,I5,5X,5F9.3)") Nowyear,
     +             Nowmonth, Basin_ppt_mo, Basin_actet_mo,Basin_storage,
     +             Basin_stflow_mo, Obsq_inches_mo
            CALL opstr(Buffer80(:62))

          ELSEIF ( Print_type.EQ.2 ) THEN
            IF ( Dprt ) CALL opstr(DASHS)

            WRITE (Buffer160, 9006) Nowyear, Nowmonth,Basin_max_temp_mo,
     +            Basin_min_temp_mo, Basin_ppt_mo, Basin_net_ppt_mo,
     +            Basin_intcp_evap_mo, Basin_potet_mo, Basin_actet_mo,
     +            Basin_soil_moist, Basin_pweqv, Basin_snowmelt_mo,
     +            Basin_gwstor, Basin_ssstor, Basin_gwflow_mo,
     +            Basin_ssflow_mo, Basin_sroff_mo, Basin_stflow_mo,
     +            Basin_cfs_mo, Obs_runoff_mo
            CALL opstr(Buffer160(:131))
          ENDIF

        ENDIF
      ENDIF

!******Check for year print

      IF ( Yprt ) THEN
        Obs_runoff_yr = Obs_runoff_yr + obsrunoff
        Obsq_inches_yr = Obsq_inches_yr + obsrunoff*Cfs2inches
        Basin_cfs_yr = Basin_cfs_yr + Basin_cfs
        Basin_ppt_yr = Basin_ppt_yr + Basin_ppt
        Basin_net_ppt_yr = Basin_net_ppt_yr + Basin_net_ppt
        Basin_max_temp_yr = Basin_max_temp_yr + Basin_tmax
        Basin_min_temp_yr = Basin_min_temp_yr + Basin_tmin
        Basin_intcp_evap_yr = Basin_intcp_evap_yr + Basin_intcp_evap
        Basin_potet_yr = Basin_potet_yr + Basin_potet
        Basin_actet_yr = Basin_actet_yr + Basin_actet
        Basin_et_yr = Basin_et_yr + Basin_et
        Basin_snowmelt_yr = Basin_snowmelt_yr + Basin_snowmelt
        Basin_gwflow_yr = Basin_gwflow_yr + Basin_gwflow
        Basin_ssflow_yr = Basin_ssflow_yr + Basin_ssflow
        Basin_sroff_yr = Basin_sroff_yr + Basin_sroff
        Basin_stflow_yr = Basin_stflow_yr + Basin_stflow
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Hru_et_yr(i) = Hru_et_yr(i) + Hru_actet(i)
        ENDDO

        IF ( wyday.EQ.Yrdays ) THEN
          IF ( Print_type.EQ.0 ) THEN

            Obs_runoff_yr = Obs_runoff_yr/Yrdays
            Basin_cfs_yr = Basin_cfs_yr/Yrdays
            IF ( Mprt .OR. Dprt ) CALL opstr(EQULS(:40))
            WRITE (Buffer40, "(I7,F21.2,F12.2)") Nowyear, Obs_runoff_yr,
     +             Basin_cfs_yr
            CALL opstr(Buffer40)

! ****annual summary here
          ELSEIF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            IF ( Mprt .OR. Dprt ) CALL opstr(EQULS(:62))
            WRITE (Buffer80, "(I7,10X,5F9.3)") Nowyear, Basin_ppt_yr,
     +             Basin_actet_yr, Basin_storage, Basin_stflow_yr,
     +             Obsq_inches_yr
            CALL opstr(Buffer80(:62))

          ELSEIF ( Print_type.EQ.2 ) THEN
            Basin_max_temp_yr = Basin_max_temp_yr/Yrdays
            Basin_min_temp_yr = Basin_min_temp_yr/Yrdays
            Obs_runoff_yr = Obs_runoff_yr/Yrdays
            Basin_cfs_yr = Basin_cfs_yr/Yrdays
            IF ( Mprt .OR. Dprt ) CALL opstr(EQULS)
            WRITE (Buffer160, 9007) Nowyear, Basin_max_temp_yr,
     +            Basin_min_temp_yr, Basin_ppt_yr, Basin_net_ppt_yr,
     +            Basin_intcp_evap_yr, Basin_potet_yr, Basin_actet_yr,
     +            Basin_soil_moist, Basin_pweqv, Basin_snowmelt_yr,
     +            Basin_gwstor, Basin_ssstor, Basin_gwflow_yr,
     +            Basin_ssflow_yr, Basin_sroff_yr, Basin_stflow_yr,
     +            Basin_cfs_yr, Obs_runoff_yr
            CALL opstr(Buffer160(:131))
          ENDIF

          Obs_runoff_yr = 0.
          Basin_cfs_yr = 0.
          Basin_ppt_yr = 0.
          Basin_net_ppt_yr = 0.
          Basin_max_temp_yr = 0.
          Basin_min_temp_yr = 0.
          Basin_intcp_evap_yr = 0.
          Basin_potet_yr = 0.
          Basin_actet_yr = 0.
          Basin_et_yr = 0.
          Basin_snowmelt_yr = 0.
          Basin_gwflow_yr = 0.
          Basin_ssflow_yr = 0.
          Basin_sroff_yr = 0.
          Basin_stflow_yr = 0.
          Obsq_inches_yr = 0.
          Hru_et_yr = 0.

        ENDIF
      ENDIF

!******Print heading if needed
      IF ( endrun.EQ.0 ) THEN
        IF ( (Header_prt==2 .AND. Nowday==Modays(Nowmonth)) .OR.
     +       (Header_prt==1 .AND. wyday==Yrdays) ) THEN
          Buffer32 = ' '
          CALL opstr(Buffer32(:1))
          CALL header_print(Print_type)
        ENDIF
      ENDIF

!******Check for total print

      IF ( Tprt ) THEN
        Totdays = Totdays + 1
        Obs_runoff_tot = Obs_runoff_tot + obsrunoff
        Obsq_inches_tot = Obsq_inches_tot + obsrunoff*Cfs2inches
        Basin_cfs_tot = Basin_cfs_tot + Basin_cfs
        Basin_ppt_tot = Basin_ppt_tot + Basin_ppt
        Basin_net_ppt_tot = Basin_net_ppt_tot + Basin_net_ppt
        Basin_max_temp_tot = Basin_max_temp_tot + Basin_tmax
        Basin_min_temp_tot = Basin_min_temp_tot + Basin_tmin
        Basin_intcp_evap_tot = Basin_intcp_evap_tot + Basin_intcp_evap
        Basin_potet_tot = Basin_potet_tot + Basin_potet
        Basin_actet_tot = Basin_actet_tot + Basin_actet
        Basin_et_tot = Basin_et_tot + Basin_et
        Basin_snowmelt_tot = Basin_snowmelt_tot + Basin_snowmelt
        Basin_gwflow_tot = Basin_gwflow_tot + Basin_gwflow
        Basin_ssflow_tot = Basin_ssflow_tot + Basin_ssflow
        Basin_sroff_tot = Basin_sroff_tot + Basin_sroff
        Basin_stflow_tot = Basin_stflow_tot + Basin_stflow

        IF ( endrun.EQ.1 ) THEN

          IF ( Print_type.EQ.0 ) THEN
            Obs_runoff_tot = Obs_runoff_tot/Totdays
            Basin_cfs_tot = Basin_cfs_tot/Totdays
            CALL opstr(STARS(:40))
            WRITE (Buffer48, "(A,3X,2F12.2)") ' Total for run',
     +             Obs_runoff_tot, Basin_cfs_tot
            CALL opstr(Buffer48(:41))
          ENDIF

          IF ( Print_type.EQ.1 .OR. Print_type.EQ.3 ) THEN
            CALL opstr(STARS(:62))
            WRITE (Buffer80, 9005) ' Total for run', Basin_ppt_tot,
     +                            Basin_actet_tot, Basin_storage,
     +                            Basin_stflow_tot, Obsq_inches_tot
            CALL opstr(Buffer80(:62))
          ENDIF

          IF ( Print_type.EQ.2 ) THEN
            Obs_runoff_tot = Obs_runoff_tot/Totdays
            Basin_cfs_tot = Basin_cfs_tot/Totdays
            CALL opstr(STARS)
            WRITE (Buffer160, 9004) ' Total for run', Basin_ppt_tot,
     +            Basin_net_ppt_tot, Basin_intcp_evap_tot,
     +            Basin_potet_tot, Basin_actet_tot, Basin_soil_moist,
     +            Basin_pweqv, Basin_snowmelt_tot, Basin_gwstor,
     +            Basin_ssstor, Basin_gwflow_tot, Basin_ssflow_tot,
     +            Basin_sroff_tot, Basin_stflow_tot, Basin_cfs_tot,
     +            Obs_runoff_tot
            CALL opstr(Buffer160(:140))
          ENDIF
        ENDIF
      ENDIF

 9001 FORMAT (I6, 2I3, 3F5.0, 8F6.2, F7.2, 2F6.2, 4F7.4, 2F9.2,
     +        2F7.4)
 9004 FORMAT (A, 13X, 2F6.2, F11.1, F7.2, 3F6.2, F7.2, 2F6.2, 4F7.2,
     +        2F9.2)
 9005 FORMAT (A, 3X, 6F9.3)
 9006 FORMAT (I6, I3, 8X, 2F5.0, 2F6.2, F11.1, F7.2, 3F6.2,
     +        F7.2, 2F6.2, 4F7.2, 2F9.2)
 9007 FORMAT (I6, 11X, 2F5.0, 2F6.2, F11.1, F7.2, 3F6.2, F7.2,
     +        2F6.2, 4F7.2, 2F9.2)

      sumbrun = 0
      END FUNCTION sumbrun

!***********************************************************************
! Print headers for tables
! This writes the measured and simulated table header.
!***********************************************************************
      SUBROUTINE header_print(Print_type)
      USE PRMS_BASINSUM, ONLY: DASHS, Buffer80, Buffer120
      IMPLICIT NONE
      EXTERNAL opstr
! Arguments
      INTEGER, INTENT(IN) :: Print_type
!***********************************************************************
      IF ( Print_type.EQ.0 ) THEN
        CALL opstr('1  Year Month Day   Measured   Simulated')
        CALL opstr('                      (cfs)      (cfs)')
        CALL opstr(DASHS(:40))

!  This writes the water balance table header.
      ELSEIF ( Print_type.EQ.1 ) THEN
        CALL opstr(
     +'1  Year Month Day   Precip     ET    Storage S-Runoff M-Runoff'//
     +'   Watbal  WBalSum  Strmfar')
        WRITE (Buffer120, 9001)
        CALL opstr(Buffer120)
        CALL opstr(DASHS(:89))

!  This writes the detailed table header.
      ELSEIF ( Print_type.EQ.2 ) THEN
        CALL opstr(
     +    '1Year mo day srad  tmx  tmn  ppt  n-ppt  ints  intl potet'//
     +    ' actet  smav pweqv   melt gwsto sssto gwflow ssflow  sroff'//
     +    ' tot-fl      sim    meas lkevap')
        CALL opstr(
     +    '            (ly) (F/C)(F/C) (in.) (in.) (in.) (in.) (in.)'//
     +    ' (in.) (in.) (in.)  (in.) (in.) (in.)  (in.)  (in.)  (in.)'//
     +    '  (in.)    (cfs)    (cfs) (in.)')
        CALL opstr(DASHS)

!  This writes the water balance table header.
      ELSEIF ( Print_type.EQ.3 ) THEN
        CALL opstr(
     + '1  Year Month Day   Precip     ET    Storage S-Runoff M-Runoff')
        WRITE (Buffer80, 9002)
        CALL opstr(Buffer80(:62))
        CALL opstr(DASHS(:62))

      ENDIF

 9001 FORMAT (17X, 8(' (inches)'))
 9002 FORMAT (17X, 5(' (inches)'))

      END SUBROUTINE header_print

