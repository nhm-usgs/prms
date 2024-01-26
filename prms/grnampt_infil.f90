!***********************************************************************
!  Modified Green-Ampt infiltration
!  CAUTION does not handle depression storage
!***********************************************************************
      MODULE PRMS_GRNAMPT
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Green & Amp Infiltration'
      character(len=*), parameter :: MODNAME = 'grnampt_infil'
      character(len=*), parameter :: Version_grnampt_infil = '2024-01-05'
      DOUBLE PRECISION, parameter :: ZERO = 0.0D0
      INTEGER, SAVE :: Ncdels, Nchxs, Nofxs, Ncmoc, Ntstep, DBGUNIT, BALUNT
!     Bms    = HRU RECHARGE ZONE Sat_moist_stor
!     Bmsm   = HRU MAX RECHARGE ZONE Sat_moist_stor
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Bms(:), Bmsm(:), Coef(:), Hru_sum(:), Kpar24(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_pptexc, Storm_pptexc, Basin_storm_precip
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sat_moist_stor(:), Hru_pptexc(:), Storm_obspk(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ppt_exc(:, :), Ppt_exc_imp(:, :), Hru_rain_int(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Storm_precip(:), Storm_obsvol(:)
!  Declared Parameters
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Kpar(:), Psp(:), Rgf(:), Drnpar(:), Infil_dt(:)
      END MODULE PRMS_GRNAMPT
 
!***********************************************************************
!     Main grnampt_infil routine
!***********************************************************************
      INTEGER FUNCTION grnampt_infil()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, SETDIMENS
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: mgadecl, mgainit, mgarun, grnamptsetdims
!***********************************************************************
      grnampt_infil = 0

      IF ( Process_flag==RUN ) THEN
        grnampt_infil = mgarun()
      ELSEIF ( Process_flag==SETDIMENS ) THEN
        grnampt_infil = grnamptsetdims()
      ELSEIF ( Process_flag== DECL ) THEN
        grnampt_infil = mgadecl()
      ELSEIF ( Process_flag==INIT ) THEN
        grnampt_infil = mgainit()
      ENDIF

      END FUNCTION grnampt_infil

!***********************************************************************
!     grnamptsetdims - declares grnampt_infil module specific dimensions
!***********************************************************************
      INTEGER FUNCTION grnamptsetdims()
      USE PRMS_CONSTANTS, ONLY: MAXDIM
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL :: read_error
!***********************************************************************
      grnamptsetdims = 0

      IF ( decldim('ncdels', 0, MAXDIM, 'Number of internal timesteps for Green-Ampt infiltration')/=0 ) &
     &     CALL read_error(7, 'ncdels')
      IF ( decldim('nchxs', 0, MAXDIM, 'Number of cross sections + 1 a channel can be divided into for'// &
     &             ' kinematic routing computations')/=0 ) CALL read_error(7, 'nchxs')
      IF ( decldim('nofxs', 0, MAXDIM, 'Number of cross sections + 1 a flow plane can be divided'// &
     &             ' into for kinematic routing computations')/=0 ) CALL read_error(7, 'nofxs')
      IF ( decldim('ncmoc', 0, MAXDIM, 'Number of characteristics for MOC channel routing')/=0 ) CALL read_error(7, 'ncmoc')
      IF ( decldim('ntstep', 1, MAXDIM, 'Number of timesteps in one hour for channel routing')/=0 ) CALL read_error(7, 'ncdels')

      END FUNCTION grnamptsetdims

!***********************************************************************
!     mgadecl - set up parameters for kinematic routing of overland flow planes
!  Declared Parameters
!     kpar, psp, rgf, drnpar, imperv_stor_max, soil_rechr_max, infil_dt
!     hru_area, covden_sum, covden_win
!***********************************************************************
      INTEGER FUNCTION mgadecl()
      USE PRMS_CONSTANTS, ONLY: DEBUG_WB, DOCUMENTATION, ERROR_dim
      USE PRMS_GRNAMPT
      USE PRMS_MODULE, ONLY: Model, Nhru, Print_debug, Nobs
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar, getdim
      EXTERNAL :: read_error, print_module, PRMS_open_module_file, error_stop
! Local Variables
      INTEGER :: ierr
!***********************************************************************
      mgadecl = 0

      CALL print_module(MODDESC, MODNAME, Version_grnampt_infil)

      IF ( Print_debug==10 ) CALL PRMS_open_module_file(DBGUNIT, 'grnampt.dbg')
      IF ( Print_debug==DEBUG_WB ) THEN
        CALL PRMS_open_module_file(BALUNT, 'grnampt.wbal')
        WRITE ( BALUNT, 9001 )
      ENDIF

      Ncdels = getdim('ncdels')
      IF ( Ncdels==-1 ) CALL read_error(6, 'ncdels')
      Nchxs = getdim('nchxs')
      IF ( Nchxs==-1 ) CALL read_error(6, 'nchxs')
      Nofxs = getdim('nofxs')
      IF ( Nofxs==-1 ) CALL read_error(6, 'nofxs')
      Ncmoc = getdim('ncmoc')
      IF ( Ncmoc==-1 ) CALL read_error(6, 'ncmoc')
      Ntstep = getdim('ntstep')
      IF ( Ntstep==-1 ) CALL read_error(6, 'ntstep')

      IF ( Model==DOCUMENTATION ) THEN
        IF ( Ncdels==0 ) Ncdels = 1
        IF ( Nchxs==0 ) Nchxs = 1
        IF ( Nofxs==0 ) Nofxs = 1
        IF ( Ncmoc==0 ) Ncmoc = 1
        IF ( Ntstep==0 ) Ntstep = 1
      ELSE
        ierr = 0
        IF ( Ncdels==0 ) THEN
          PRINT *, 'ncdels = 0'
          ierr = 1
        ENDIF
        IF ( Nchxs==0 ) THEN
          PRINT *, 'nchxs = 0'
          ierr = 1
        ENDIF
        IF ( Nofxs==0 ) THEN
          PRINT *, 'nofxs = 0'
          ierr = 1
        ENDIF
        IF ( Ntstep==0 ) THEN
          PRINT *, 'ntstep = 0'
          ierr = 1
        ENDIF
        !IF ( Ncmoc==0 ) THEN
        !  PRINT *, 'ncmoc = 0'
        !  ierr = 1
        !ENDIF
        IF ( ierr==1 ) CALL error_stop('invalid dimension for subdaily mode', ERROR_dim)
      ENDIF

      ALLOCATE ( Ppt_exc(Ncdels, Nhru) )
      IF ( declvar(MODNAME, 'ppt_exc', 'ncdels,nhru', Ncdels*Nhru, 'double', &
     &     'Precipitation excess (qrp) for each HRU for time increments within PRMS timestep, pervious area', &
     &     'inches', Ppt_exc)/=0 ) CALL read_error(3, 'ppt_exc')

      ALLOCATE ( Hru_rain_int(Ncdels, Nhru) )
      IF ( declvar(MODNAME, 'hru_rain_int', 'ncdels,nhru', Ncdels*Nhru, 'double', &
     &     'Rain intensity for each HRU for time increments within PRMS timestep, pervious area', &
     &     'inches', Hru_rain_int)/=0 ) CALL read_error(3, 'hru_rain_int')

      ALLOCATE ( Ppt_exc_imp(Ncdels, Nhru) )
      IF ( declvar(MODNAME, 'ppt_exc_imp', 'ncdels,nhru', Ncdels*Nhru, 'double', &
     &     'Precipitation excess (qrp) for each HRU for time increments within PRMS timestep, impervious area', &
     &     'inches', Ppt_exc_imp)/=0 ) CALL read_error(3, 'ppt_exc_imp')

      ALLOCATE ( Sat_moist_stor(Nhru) )
      IF ( declvar(MODNAME, 'sat_moist_stor', 'nhru', Nhru, 'double', &
     &     'Saturated moisture storage for modified Green-Ampt infiltration computations - SMS in DR3M', &
     &     'inches', Sat_moist_stor)/=0 ) CALL read_error(3, 'sat_moist_stor')

      IF ( Nobs>0 .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Storm_obsvol(Nobs) )
        IF ( declvar(MODNAME, 'storm_obsvol', 'nobs', Nobs, 'double', &
     &       'Measured flow volume at each stream gage for current storm, cumulative total', &
     &       'ft3', Storm_obsvol)/=0 ) CALL read_error(3, 'storm_obsvol')
        ALLOCATE ( Storm_obspk(Nobs) )
        IF ( declvar(MODNAME, 'storm_obspk', 'nobs', Nobs, 'double', &
     &       'Measured peak flow at each stream gage for storm ', &
     &       'cfs', Storm_obspk)/=0 ) CALL read_error(3, 'storm_obspk')
      ENDIF

      ALLOCATE ( Storm_precip(Nhru) )
      IF ( declvar(MODNAME, 'storm_precip', 'nhru', Nhru, 'double', &
     &     'Sum of precipitation of the current storm for each HRU', &
     &     'inches', Storm_precip)/=0 ) CALL read_error(3, 'storm_precip')

      ALLOCATE ( Hru_pptexc(Nhru) )
      IF ( declvar(MODNAME, 'hru_pptexc', 'nhru', Nhru, 'double', &
     &     'Precipitation excess for each HRU for the timestep, weighted pervious + impervious', &
     &     'inches', Hru_pptexc)/=0 ) CALL read_error(3, 'hru_pptexc')

      IF ( declvar(MODNAME, 'basin_pptexc', 'one', 1, 'double', &
     &     'Basin area-weighted average precipitation excess from each HRU, for the timestep.', &
     &     'inches', Basin_pptexc)/=0 ) CALL read_error(3, 'basin_pptexc')
 
      IF ( declvar(MODNAME, 'storm_pptexc', 'one', 1, 'double', &
     &     'Sum of basin_pptexc for the storm', &
     &     'inches', Storm_pptexc)/=0 ) CALL read_error(3, 'storm_pptexc')

      IF ( declvar(MODNAME, 'basin_storm_precip', 'one', 1, 'double', &
     &     'Basin area-weighted average precipitation for the current storm', &
     &     'inches', Basin_storm_precip)/=0 ) CALL read_error(3, 'basin_storm_precip')

! Allocate arrays for parameters, local and variables from other modules
      ALLOCATE ( Kpar(Nhru), Psp(Nhru), Rgf(Nhru), Drnpar(Nhru), Infil_dt(Nhru) )
! Allocate arrays for local states + states retrieved from other modules
      ALLOCATE ( Bms(Nhru), Bmsm(Nhru), Coef(Nhru) )
      IF ( Print_debug==10 ) ALLOCATE ( Hru_sum(Nhru) )

!*** Declare parameters
      IF ( declparam(MODNAME, 'kpar', 'nhru', 'double', &
     &     '2.0', '0.0', '20.0', &
     &     'Hydraulic conductivity of the transmission zone', &
     &     'Hydraulic conductivity of the transmission zone', &
     &     'inches/hour')/=0 ) CALL read_error(1, 'kpar')

      IF ( declparam(MODNAME, 'psp', 'nhru', 'double', &
     &     '0.5', '0.0', '100.0', &
     &     'Moisture deficit * capillary drive', &
     &     'The product of moisture deficit and capillary drive for soil_rechr equal to soil_rechr_max', &
     &     'inches')/=0 ) CALL read_error(1, 'psp')

      IF ( declparam(MODNAME, 'rgf', 'nhru', 'double', &
     &     '9.5', '0.0', '100.0', &
     &     'Ratio of psp at field capacity to psp at wilting point', &
     &     'Ratio of psp at field capacity to psp at wilting point', &
     &     'none')/=0 ) CALL read_error(1, 'rgf')

      IF ( declparam(MODNAME, 'drnpar', 'nhru', 'double', &
     &     '1.0', '0.0', '2.0', &
     &     'Drainage factor to redistribute saturated moisture storage', &
     &     'Drainage factor for redistribution of saturated moisture'// &
     &     ' storage (Sat_moist_stor) to soil_rechr as a function of hydraulic conductivity (ksat)', &
     &     'inches/hr')/=0 ) CALL read_error(1, 'drnpar')

      IF ( declparam(MODNAME, 'infil_dt', 'nhru', 'double', &
     &     '5.0', '1.0', '15.0', &
     &     'Timestep for infiltration computation', 'Timestep for infiltration computation', &
     &     'minutes')/=0 ) CALL read_error(1, 'epan_coef')

 9001 FORMAT ('    Date        Water Bal         Precip         Netppt      Intcpevap      Intcpstor      last_stor')

      END FUNCTION mgadecl
 
!***********************************************************************
!     mgainit - Initialize modified Green-Ampt module -
!               get parameter values, initialize storm variables
!***********************************************************************
      INTEGER FUNCTION mgainit()
      USE PRMS_CONSTANTS, ONLY: ERROR_param
      USE PRMS_GRNAMPT
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Nobs
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: read_error, error_stop
      INTEGER :: i, j, ierr
!***********************************************************************
      mgainit = 0

      IF ( getparam(MODNAME, 'kpar', Nhru, 'double', Kpar)/=0 ) CALL read_error(2, 'kpar')
      IF ( getparam(MODNAME, 'psp', Nhru, 'double', Psp)/=0 ) CALL read_error(2, 'psp')
      IF ( getparam(MODNAME, 'rgf', Nhru, 'double', Rgf)/=0 ) CALL read_error(2, 'rgf')
      IF ( getparam(MODNAME, 'drnpar', Nhru, 'double', Drnpar)/=0 ) CALL read_error(2, 'drnpar')
      IF ( getparam(MODNAME, 'infil_dt', Nhru, 'double', Infil_dt)/=0 ) CALL read_error(2, 'infil_dt') 

      ierr = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Infil_dt(i)==ZERO ) THEN
          PRINT *, 'ERROR, infil_dt cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) CALL error_stop('in grnampt_infil', ERROR_param)
      
      IF ( Nobs>0 ) THEN
        Storm_obsvol = ZERO
        Storm_obspk = ZERO
      ENDIF
      Storm_pptexc = ZERO
      Basin_storm_precip = ZERO
      Basin_pptexc = ZERO
      ! dimension nhru
      Hru_pptexc = ZERO
      Sat_moist_stor = ZERO
      Storm_precip = ZERO
      Bms = ZERO
      Bmsm = ZERO
      Coef = ZERO
      Kpar24 = Kpar(i)*24.0
      IF ( Print_debug==10 ) Hru_sum = ZERO
      
      !dimension (ncdels, nhru)
      Hru_rain_int = ZERO
      Ppt_exc = ZERO
      Ppt_exc_imp = ZERO

      END FUNCTION mgainit

!***********************************************************************
!     mgarun -
!     Caution:  routine modifies another modules (srunoff) variables
!               (infil, imperv_stor, imperv_evap)
!***********************************************************************
      INTEGER FUNCTION mgarun()
      USE PRMS_CONSTANTS, ONLY: DNEARZERO, NEARZERO, ACTIVE, OFF, LAND, LAKE
      USE PRMS_GRNAMPT
      USE PRMS_MODULE, ONLY: Print_debug, Nhru, Nobs, Nowyear, Nowmonth, Nowday
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Hru_area_dble, Basin_area_inv, &
                            Hru_perv, Hru_imperv, Hru_percent_imperv, Hru_frac_perv
      USE PRMS_SET_TIME, ONLY: Timestep_hours, Timestep_seconds, Timestep_days, Timestep_minutes, &
                               Storm_status, Nowhour, Nowminute, Nowtime
      USE PRMS_OBS, ONLY: Runoff
      USE PRMS_CLIMATEVARS, ONLY: Potet, Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Soil_rechr, Soil_rechr_max, Imperv_stor_max, Imperv_stor, Infil, Pkwater_equiv
      USE PRMS_INTCP, ONLY: Net_ppt, Hru_intcpevap, Net_rain, Net_snow, Intcp_changeover
      USE PRMS_SNOW, ONLY: Snowmelt, Snow_evap, Snowcov_area, Pptmix_nopack, Pkwater_ante
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap, Imperv_evap
      IMPLICIT NONE
! Functions
      INTRINSIC NINT, DBLE
      EXTERNAL :: imperv_et
! Local Variables
      INTEGER :: i, j, icdels, jj, kk, imperv_flag
      DOUBLE PRECISION :: dth, delp, srn, fr, qrp, fin, cdels, ps, ksat, drn, runoff_dble, availh2o_total
      DOUBLE PRECISION :: availh2o, wbal, last_stor, harea, himperv, hperv, avail_et_dble
      REAL :: avail_et, Imperv_frac
!***********************************************************************
      mgarun = 0

      IF ( Storm_status==0 .OR. Storm_status==3 ) RETURN

      IF ( Storm_status==1 ) THEN
!       initialize storm stuff if new storm or end of storm
        DO jj = 1, Active_hrus
          i = Hru_route_order(jj)
          IF ( Hru_type(i)==LAKE ) CYCLE
          Bms(i) = Soil_rechr(i)
          Bmsm(i) = Soil_rechr_max(i)
          Coef(i) = (Rgf(i)-1.0D0)/Bmsm(i)
          Sat_moist_stor(i) = ZERO
          Storm_precip(i) = ZERO
        ENDDO
        IF ( Nobs>0 ) THEN
          Storm_obsvol = ZERO
          Storm_obspk = ZERO
        ENDIF
        Storm_pptexc = ZERO
        Basin_storm_precip = ZERO
        IF ( Print_debug==10 ) Hru_sum = ZERO
      ENDIF

! get obs runoff data
      IF ( Nobs>0 ) THEN
        DO i = 1, Nobs
          runoff_dble = DBLE( Runoff(i) )
          Storm_obsvol(i) = Storm_obsvol(i) + runoff_dble*Timestep_seconds
          IF ( runoff_dble>Storm_obspk(i) ) Storm_obspk(i) = runoff_dble
        ENDDO
      ENDIF

      Infil = 0.0
      Hru_pptexc = ZERO
      Imperv_evap = 0.0
      last_stor = ZERO
      Basin_pptexc = ZERO 
      DO jj = 1, Active_hrus
        i = Hru_route_order(jj)
        Storm_precip(i) = Storm_precip(i) + Hru_ppt(i)
        Basin_storm_precip = Basin_storm_precip + Hru_ppt(i)*Hru_area_dble(i)
        IF ( Hru_type(i)==LAKE ) CYCLE ! rsr, does not allow for cascading flow
        DO kk = 1, Ncdels
          Hru_rain_int(kk,i) = ZERO
          Ppt_exc(kk, i) = ZERO
          Ppt_exc_imp(kk, i) = ZERO
        ENDDO
        harea = Hru_area_dble(i)
        hperv = DBLE( Hru_perv(i) )
        himperv = DBLE( Hru_imperv(i) )
        imperv_flag = OFF
        IF ( himperv>ZERO ) imperv_flag = ACTIVE
        Imperv_frac = DBLE( Hru_percent_imperv(i) )

!     delp = fraction representing pt portion of dt interval
        cdels = Timestep_minutes/Infil_dt(i) ! number of infiltration steps in current timestep
        IF ( cdels<1.0D0 ) THEN
          PRINT *, 'ERROR, simulation timestep < Green-Ampt infiltration time step for HRU:', i
          PRINT *, 'Date:', Nowtime, ', Timestep in minutes:', Timestep_minutes, ', Infiltration dt:', Infil_dt(i)
          STOP
        ENDIF
        dth = Infil_dt(i)/60.0D0
        delp = 1.0D0/cdels
        icdels = NINT(cdels)
        IF ( icdels>Ncdels ) THEN
          PRINT *, 'ERROR, Green-Ampt infiltration time step for HRU:', i, ' > dimension ncdels'
          PRINT *, 'Date:', Nowtime, 'icdels:', icdels, ' ncdels:', Ncdels
          STOP
        ENDIF

        ksat = Kpar(i)*Timestep_hours
        availh2o = DBLE( Intcp_changeover(i) ) + Net_rain(i) ! no cascading Hortonian or glacier
        availh2o_total = DBLE( Intcp_changeover(i) )
        IF ( Pptmix_nopack(i)==ACTIVE ) availh2o_total = availh2o_total + Net_rain(i)
!*******If precipitation on snowpack all water available to the surface is considered to be snowmelt
!*******If there is no snowpack and no precip,then check for melt from last of snowpack.
!*******If rain/snow mix with no antecedent snowpack, compute snowmelt portion of runoff.
        IF ( Snowmelt(i)>0.0 ) THEN
          availh2o_total = availh2o_total + DBLE(Snowmelt(i))
!*******There was no snowmelt but a snowpack may exist.  If there is
!*******no snowpack then check for rain on a snowfree HRU.
        ELSEIF ( .not.(Pkwater_equiv(i)<ZERO) .and. .not.(Pkwater_ante(i)>ZERO) ) THEN
!         If no snowmelt and no snowpack but there was net snow then
!         snowpack was small and was lost to sublimation.
          IF ( Net_snow(i)<DNEARZERO .AND. Net_rain(i)>0.0D0 ) availh2o_total = availh2o_total + Net_rain(i)
        ENDIF

        IF ( availh2o_total>ZERO ) THEN
          srn = availh2o_total/cdels
          !IF ( Sat_moist_stor(i)<DNEARZERO ) Sat_moist_stor(i) = ZERO
          ps = Psp(i)*(Rgf(i)-Coef(i)*Bms(i))
          IF ( Sat_moist_stor(i)>ZERO ) THEN
            fr = ksat*(1.0D0+ps/Sat_moist_stor(i))
          ELSE
            fr = ZERO
            IF ( ksat>ZERO ) fr = ksat*(1.0D0+ps/srn)
          ENDIF

          last_stor = DBLE( Imperv_stor(i) )

          DO j = 1, icdels
            Hru_rain_int(j, i) = availh2o_total/cdels
            qrp = srn - 0.5D0*fr
            IF ( srn<fr .AND. fr/=ZERO ) qrp = 0.5D0*srn*srn/fr
            fin = srn - qrp
            Infil(i) = Infil(i) + fin
            Sat_moist_stor(i) = Sat_moist_stor(i) + fin

            IF ( Print_debug==10 ) WRITE ( DBGUNIT, 9002 ) i, j, srn, fr, qrp

            fr = ZERO
            IF ( ksat>ZERO .AND. Sat_moist_stor(i)>ZERO ) fr = ksat*(1.0D0+ps/Sat_moist_stor(i))

            IF ( Hru_type(i)==LAND ) THEN
              Ppt_exc(j, i) = qrp
              Hru_pptexc(i) = qrp*hperv
            ENDIF
            IF ( imperv_flag==1 ) THEN
              Imperv_stor(i) = Imperv_stor(i) + srn
              IF ( Hru_type(i)==LAND ) THEN
                IF ( Imperv_stor(i)>Imperv_stor_max(i) ) THEN
                  Ppt_exc_imp(j, i) = Imperv_stor(i) - Imperv_stor_max(i)
                  Imperv_stor(i) = Imperv_stor_max(i)
                  Hru_pptexc(i) = Hru_pptexc(i) + Ppt_exc_imp(j, i)*himperv
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          Basin_pptexc = Basin_pptexc + Hru_pptexc(i)
          Hru_pptexc(i) = Hru_pptexc(i)/Hru_area_dble(i)
        ENDIF

        ! no ET if precipitation
        IF ( Hru_ppt(i)<DNEARZERO ) THEN
          avail_et = sngl(Potet(i)) - Snow_evap(i) - Hru_intcpevap(i)
          IF ( imperv_flag==1 ) THEN
            CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i), Snowcov_area(i), avail_et)
            Hru_impervevap(i) = Imperv_evap(i)*Imperv_frac
            avail_et = avail_et - Hru_impervevap(i)
          ENDIF

          avail_et_dble = avail_et
          IF ( Sat_moist_stor(i)<avail_et_dble ) THEN
            Bms(i) = Bms(i) + Sat_moist_stor(i) - avail_et_dble
            IF ( Bms(i)<ZERO ) Bms(i) = ZERO
            Sat_moist_stor(i) = ZERO
          ELSE
            Sat_moist_stor(i) = Sat_moist_stor(i) - avail_et_dble
          ENDIF

          drn = Kpar24(i)*Timestep_days*Drnpar(i)
          IF ( Sat_moist_stor(i)>drn ) THEN
            Sat_moist_stor(i) = Sat_moist_stor(i) - drn
            Bms(i) = Bms(i) + drn
          ELSE
            Bms(i) = Bms(i) + Sat_moist_stor(i)
            Sat_moist_stor(i) = ZERO
          ENDIF
          IF ( Bms(i)>Bmsm(i) ) Bms(i) = Bmsm(i)
        ENDIF

        IF ( Print_debug>0 ) THEN
          wbal = DBLE(Infil(i)*Hru_frac_perv(i)) + Hru_pptexc(i) - availh2o_total
          IF ( Imperv_flag==1 ) wbal = wbal + (DBLE(Imperv_evap(i))+Imperv_stor(i)-last_stor)*DBLE(Hru_percent_imperv(i))
          IF ( Print_debug==1 ) THEN
            IF ( ABS(wbal)>1.0D-05 ) THEN
              WRITE ( BALUNT, * ) 'Green-Ampt HRU water-balance issue'
              WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, Nowhour, Nowminute, i, wbal, Infil(i), &
     &                Hru_pptexc(i), availh2o_total, last_stor, Imperv_stor(i), Imperv_evap(i), Net_ppt(i)
            ENDIF
          ELSEIF ( Print_debug==10 ) THEN
            WRITE ( DBGUNIT, 9003 ) Nowday, Nowhour, Nowminute, i, Infil(i), Hru_pptexc(i), availh2o_total, &
     &              wbal, Net_ppt(i), Hru_pptexc(i)
          ENDIF
        ENDIF
 
      ENDDO
 
      Basin_pptexc = Basin_pptexc*Basin_area_inv
      Storm_pptexc = Storm_pptexc + Basin_pptexc
      Basin_storm_precip = Basin_storm_precip*Basin_area_inv
 
      IF ( Print_debug==10 ) THEN
        WRITE ( DBGUNIT, '(3I3,2D12.5)' ) Nowday, Nowhour, Nowminute, Basin_pptexc, Storm_pptexc
        DO j = 1, Ncdels
          WRITE ( DBGUNIT, '(10D10.3)' ) (Ppt_exc(j, i), i=1, Nhru)
          DO i = 1, Nhru
            Hru_sum(i) = Hru_sum(i) + Ppt_exc(j, i)
          ENDDO
        ENDDO
        IF ( Nowhour==24 ) THEN
          WRITE ( DBGUNIT, 9005 ) (Hru_sum(i), i=1, Nhru)
          Hru_sum = ZERO
        ENDIF
      ELSEIF ( Print_debug==1 ) THEN
        WRITE ( BALUNT, 9006 ) Nowyear, Nowmonth, Nowday, Nowhour, Nowminute, Basin_pptexc, Storm_pptexc, Basin_storm_precip
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), I2.2, ':', I2.2, ' HRU:', I7, D12.5,F12.5,D12.5,3F12.5)
 9002 FORMAT (' i-j-srn-fr-qrp', 2I4, 3F10.7)
 9003 FORMAT (I2.2, I3.2, ':', I2.2, ' HRU:', I7, F11.5,3D12.5,F12.5,D12.5)
 9005 FORMAT ('DAILY TOTAL: ', 7D11.4)
 9006 FORMAT (I5, 2('/', I2.2), I3.2, ':', I2.2, 3D14.5)

      END FUNCTION mgarun

