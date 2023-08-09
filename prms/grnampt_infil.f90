!***********************************************************************
!  Modified Green-Ampt infiltration
!  CAUTION does not handle depression storage
!***********************************************************************
      MODULE PRMS_GRNAMPT
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Ncdels, Nchxs, Nofxs, Ncmoc, Ntstep, DBGUNIT, BALUNT
      REAL, SAVE :: Imperv_frac
!     Bms    = HRU RECHARGE ZONE Sat_moist_stor
!     Bmsm   = HRU MAX RECHARGE ZONE Sat_moist_stor
      REAL, SAVE, ALLOCATABLE :: Bms(:), Bmsm(:), Coef(:), Hru_sum(:), Kpar24(:)
      CHARACTER(LEN=13), SAVE :: MODNAME
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_pptexc, Storm_pptexc, Basin_storm_precip
      REAL, SAVE, ALLOCATABLE :: Sat_moist_stor(:), Hru_pptexc(:), Storm_obspk(:)
      REAL, SAVE, ALLOCATABLE :: Ppt_exc(:, :), Ppt_exc_imp(:, :), Hru_rain_int(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Storm_precip(:), Storm_obsvol(:)
!  Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Kpar(:), Psp(:), Rgf(:), Drnpar(:), Infil_dt(:)
      END MODULE PRMS_GRNAMPT
 
!***********************************************************************
!     Main grnampt_infil routine
!***********************************************************************
      INTEGER FUNCTION grnampt_infil()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: mgadecl, mgainit, mgarun, grnamptsetdims
      EXTERNAL :: grnampt_infil_restart
!***********************************************************************
      grnampt_infil = 0

      IF ( Process(:3)=='run' ) THEN
        grnampt_infil = mgarun()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        grnampt_infil = grnamptsetdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        grnampt_infil = mgadecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) THEN
          CALL grnampt_infil_restart(1)
        ELSE
          grnampt_infil = mgainit()
        ENDIF
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL grnampt_infil_restart(0)
      ENDIF

      END FUNCTION grnampt_infil

!***********************************************************************
!     grnamptsetdims - declares grnampt_infil module specific dimensions
!***********************************************************************
      INTEGER FUNCTION grnamptsetdims()
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL :: read_error
! Local Variables
      ! Maximum values are no longer limits
      INTEGER, PARAMETER :: MAXDIM = 500
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
      USE PRMS_GRNAMPT
      USE PRMS_MODULE, ONLY: Model, Nhru, Print_debug, Nobs
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar, getdim
      EXTERNAL :: read_error, print_module, PRMS_open_module_file
! Local Variables
      INTEGER :: ierr
      CHARACTER(LEN=80), SAVE :: Version_grnampt_infil
!***********************************************************************
      mgadecl = 0

      Version_grnampt_infil = '$Id: grnampt_infil.f90 7228 2015-03-07 00:36:47Z rsregan $'
      CALL print_module(Version_grnampt_infil, 'Green-Ampt infiltration   ', 90)
      MODNAME = 'grnampt_infil'


      IF ( Print_debug==10 ) CALL PRMS_open_module_file(DBGUNIT, 'grnampt.dbg')
      IF ( Print_debug==1 ) THEN
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

      IF ( Model==99 ) THEN
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
        IF ( ierr==1 ) STOP 'ERROR, invalid dimension for subdaily mode'
      ENDIF

      ALLOCATE ( Ppt_exc(Ncdels, Nhru) )
      IF ( declvar(MODNAME, 'ppt_exc', 'ncdels,nhru', Ncdels*Nhru, 'real', &
     &     'Precipitation excess (qrp) for each HRU for time increments within PRMS timestep, pervious area', &
     &     'inches', Ppt_exc)/=0 ) CALL read_error(3, 'ppt_exc')

      ALLOCATE ( Hru_rain_int(Ncdels, Nhru) )
      IF ( declvar(MODNAME, 'hru_rain_int', 'ncdels,nhru', Ncdels*Nhru, 'real', &
     &     'Rain intensity for each HRU for time increments within PRMS timestep, pervious area', &
     &     'inches', Hru_rain_int)/=0 ) CALL read_error(3, 'hru_rain_int')

      ALLOCATE ( Ppt_exc_imp(Ncdels, Nhru) )
      IF ( declvar(MODNAME, 'ppt_exc_imp', 'ncdels,nhru', Ncdels*Nhru, 'real', &
     &     'Precipitation excess (qrp) for each HRU for time increments within PRMS timestep, impervious area', &
     &     'inches', Ppt_exc_imp)/=0 ) CALL read_error(3, 'ppt_exc_imp')

      ALLOCATE ( Sat_moist_stor(Nhru) )
      IF ( declvar(MODNAME, 'sat_moist_stor', 'nhru', Nhru, 'real', &
     &     'Saturated moisture storage for modified Green-Ampt infiltration computations - SMS in DR3M', &
     &     'inches', Sat_moist_stor)/=0 ) CALL read_error(3, 'sat_moist_stor')

      IF ( Nobs>0 .OR. Model==99 ) THEN
        ALLOCATE ( Storm_obsvol(Nobs) )
        IF ( declvar(MODNAME, 'storm_obsvol', 'nobs', Nobs, 'double', &
     &       'Measured flow volume at each stream gage for current storm, cumulative total', &
     &       'ft3', Storm_obsvol)/=0 ) CALL read_error(3, 'storm_obsvol')
        ALLOCATE ( Storm_obspk(Nobs) )
        IF ( declvar(MODNAME, 'storm_obspk', 'nobs', Nobs, 'real', &
     &       'Measured peak flow at each stream gage for storm ', &
     &       'cfs', Storm_obspk)/=0 ) CALL read_error(3, 'storm_obspk')
      ENDIF

      ALLOCATE ( Storm_precip(Nhru) )
      IF ( declvar(MODNAME, 'storm_precip', 'nhru', Nhru, 'double', &
     &     'Sum of precipitation of the current storm for each HRU', &
     &     'inches', Storm_precip)/=0 ) CALL read_error(3, 'storm_precip')

      ALLOCATE ( Hru_pptexc(Nhru) )
      IF ( declvar(MODNAME, 'hru_pptexc', 'nhru', Nhru, 'real', &
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
      IF ( declparam(MODNAME, 'kpar', 'nhru', 'real', &
     &     '2.0', '0.0', '20.0', &
     &     'Hydraulic conductivity of the transmission zone', &
     &     'Hydraulic conductivity of the transmission zone', &
     &     'inches/hour')/=0 ) CALL read_error(1, 'kpar')

      IF ( declparam(MODNAME, 'psp', 'nhru', 'real', &
     &     '0.5', '0.0', '100.0', &
     &     'Moisture deficit * capillary drive', &
     &     'The product of moisture deficit and capillary drive for soil_rechr equal to soil_rechr_max', &
     &     'inches')/=0 ) CALL read_error(1, 'psp')

      IF ( declparam(MODNAME, 'rgf', 'nhru', 'real', &
     &     '9.5', '0.0', '100.0', &
     &     'Ratio of psp at field capacity to psp at wilting point', &
     &     'Ratio of psp at field capacity to psp at wilting point', &
     &     'none')/=0 ) CALL read_error(1, 'rgf')

      IF ( declparam(MODNAME, 'drnpar', 'nhru', 'real', &
     &     '1.0', '0.0', '2.0', &
     &     'Drainage factor to redistribute saturated moisture storage', &
     &     'Drainage factor for redistribution of saturated moisture'// &
     &     ' storage (Sat_moist_stor) to soil_rechr as a function of hydraulic conductivity (ksat)', &
     &     'inches/hr')/=0 ) CALL read_error(1, 'drnpar')

      IF ( declparam(MODNAME, 'infil_dt', 'nhru', 'real', &
     &     '5.0', '1.0', '15.0', &
     &     'Timestep for infiltration computation', 'Timestep for infiltration computation', &
     &     'minutes')/=0 ) CALL read_error(1, 'epan_coef')

 9001 FORMAT ('    Date     Water Bal     Precip     Netppt  Intcpevap  Intcpstor  last_stor')

      END FUNCTION mgadecl
 
!***********************************************************************
!     mgainit - Initialize modified Green-Ampt module -
!               get parameter values, initialize storm variables
!***********************************************************************
      INTEGER FUNCTION mgainit()
      USE PRMS_GRNAMPT
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Nobs, Inputerror_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_FLOWVARS, ONLY: Infil
      USE PRMS_SRUNOFF, ONLY: Imperv_evap
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: read_error
      INTEGER :: i, j, ierr
!***********************************************************************
      mgainit = 0

      IF ( getparam(MODNAME, 'kpar', Nhru, 'real', Kpar)/=0 ) CALL read_error(2, 'kpar')
      IF ( getparam(MODNAME, 'psp', Nhru, 'real', Psp)/=0 ) CALL read_error(2, 'psp')
      IF ( getparam(MODNAME, 'rgf', Nhru, 'real', Rgf)/=0 ) CALL read_error(2, 'rgf')
      IF ( getparam(MODNAME, 'drnpar', Nhru, 'real', Drnpar)/=0 ) CALL read_error(2, 'drnpar')
      IF ( getparam(MODNAME, 'infil_dt', Nhru, 'real', Infil_dt)/=0 ) CALL read_error(2, 'infil_dt') 

      ierr = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Infil_dt(i)==0.0 ) THEN
          PRINT *, 'ERROR, infil_dt cannot be 0.0, HRU:', i
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF
      
      IF ( Nobs>0 ) THEN
        Storm_obsvol = 0.0
        Storm_obspk = 0.0
      ENDIF
      Storm_pptexc = 0.0D0
      Basin_storm_precip = 0.0D0
      Basin_pptexc = 0.0D0
      ! dimension nhru
      DO i = 1, Nhru
        Hru_pptexc(i) = 0.0
        Sat_moist_stor(i) = 0.0
        Infil(i) = 0.0
        Storm_precip(i) = 0.0D0
        Imperv_evap(i) = 0.0
        Bms(i) = 0.0
        Bmsm(i) = 0.0
        Coef(i) = 0.0
        Kpar24(i) = Kpar(i)*24.0
      ENDDO
      IF ( Print_debug==10 ) Hru_sum = 0.0
      
      !dimension (ncdels, nhru)
      Hru_rain_int = 0.0
      Ppt_exc = 0.0
      Ppt_exc_imp = 0.0

      END FUNCTION mgainit

!***********************************************************************
!     mgarun -
!     Caution:  routine modifies another modules (srunoff) variables
!               (infil, imperv_stor, imperv_evap)
!***********************************************************************
      INTEGER FUNCTION mgarun()
      USE PRMS_GRNAMPT
      USE PRMS_MODULE, ONLY: Print_debug, Nhru, Nobs
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Hru_area, NEARZERO, Basin_area_inv, &
     &    Hru_perv, Hru_imperv, DNEARZERO, Hru_frac_imperv, Hru_frac_perv
      USE PRMS_SET_TIME, ONLY: Timestep_hours, Timestep_seconds, Timestep_days, Timestep_minutes, &
     &    Subdaily_status, Nowyear, Nowmonth, Nowday, Nowhour, Nowminute, Nowtime
      USE PRMS_OBS, ONLY: Runoff
      USE PRMS_CLIMATEVARS, ONLY: Potet, Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Soil_rechr, Soil_rechr_max, Imperv_stor_max, Imperv_stor, Infil, Pkwater_equiv
      USE PRMS_INTCP, ONLY: Net_ppt, Hru_intcpevap, Net_rain, Net_snow
      USE PRMS_SNOW, ONLY: Snowmelt, Snow_evap, Snowcov_area, Pptmix_nopack
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap, Imperv_evap
      IMPLICIT NONE
! Functions
      INTRINSIC SNGL, NINT
      EXTERNAL :: imperv_et
! Local Variables
      INTEGER :: i, j, icdels, jj, kk, imperv_flag
      REAL :: dth, delp, srn, fr, qrp, fin, cdels, ps, ksat, drn
      REAL :: availh2o, avail_et, wbal, last_stor, harea, himperv, hperv
!***********************************************************************
      mgarun = 0

      IF ( Subdaily_status==0 .OR. Subdaily_status==3 ) RETURN

      IF ( Subdaily_status==1 ) THEN
!       initialize storm stuff if new storm or end of storm
        DO jj = 1, Active_hrus
          i = Hru_route_order(jj)
          IF ( Hru_type(i)==2 ) CYCLE
          Bms(i) = Soil_rechr(i)
          Bmsm(i) = Soil_rechr_max(i)
          Coef(i) = (Rgf(i)-1.0)/Bmsm(i)
          Sat_moist_stor(i) = 0.0
          Storm_precip(i) = 0.0D0
        ENDDO
        IF ( Nobs>0 ) THEN
          Storm_obsvol = 0.0
          Storm_obspk = 0.0
        ENDIF
        Storm_pptexc = 0.0D0
        Basin_storm_precip = 0.0D0
        IF ( Print_debug==10 ) Hru_sum = 0.0
      ENDIF

! get obs runoff data
      IF ( Nobs>0 ) THEN
        DO i = 1, Nobs
          Storm_obsvol(i) = Storm_obsvol(i) + Runoff(i)*Timestep_seconds
          IF ( Runoff(i)>Storm_obspk(i) ) Storm_obspk(i) = Runoff(i)
        ENDDO
      ENDIF

      last_stor = 0.0
      Basin_pptexc = 0.0D0 
      DO jj = 1, Active_hrus
        i = Hru_route_order(jj)
        Storm_precip(i) = Storm_precip(i) + Hru_ppt(i)
        Basin_storm_precip = Basin_storm_precip + Hru_ppt(i)*Hru_area(i)
        IF ( Hru_type(i)==2 ) CYCLE ! rsr, does not allow for cascading flow
        DO kk = 1, Ncdels
          Hru_rain_int(kk,i) = 0.0
          Ppt_exc(kk, i) = 0.0
          Ppt_exc_imp(kk, i) = 0.0
        ENDDO
        Infil(i) = 0.0
        Hru_pptexc(i) = 0.0
        Imperv_evap(i) = 0.0
        harea = Hru_area(i)
        hperv = Hru_perv(i)
        himperv = Hru_imperv(i)
        imperv_flag = 0
        IF ( himperv>0.0 ) imperv_flag = 1
        Imperv_frac = Hru_frac_imperv(i)

!     delp = fraction representing pt portion of dt interval
        cdels = Timestep_minutes/Infil_dt(i) ! number of infiltration steps in current timestep
        IF ( cdels<1.0 ) THEN
          PRINT *, 'ERROR, simulation timestep < Green-Ampt infiltration time step for HRU:', i
          PRINT *, 'Date:', Nowtime, ', Timestep in minutes:', Timestep_minutes, ', Infiltration dt:', Infil_dt(i)
          STOP
        ENDIF
        dth = Infil_dt(i)/60.0
        delp = 1.0/cdels
        icdels = NINT(cdels)
        IF ( icdels>Ncdels ) THEN
          PRINT *, 'ERROR, Green-Ampt infiltration time step for HRU:', i, ' > dimension ncdels'
          PRINT *, 'Date:', Nowtime, 'icdels:', icdels, ' ncdels:', Ncdels
          STOP
        ENDIF
 
        ksat = Kpar(i)*Timestep_hours
 
        availh2o = 0.0 ! no cascading Hortonian
        IF ( Pptmix_nopack(i)==1 ) availh2o = Net_rain(i)
        IF ( Snowmelt(i)>0.0 ) THEN
          availh2o = availh2o + Snowmelt(i)
        ELSEIF ( Pkwater_equiv(i)<DNEARZERO ) THEN
          IF ( Net_snow(i)<=0.0 .AND. Net_rain(i)>0.0 ) availh2o = availh2o + Net_rain(i)
        ENDIF ! snowinfil_max not used

        IF ( availh2o>0.0 ) THEN
          srn = availh2o/cdels
          !IF ( Sat_moist_stor(i)<NEARZERO ) Sat_moist_stor(i) = 0.0
          ps = Psp(i)*(Rgf(i)-Coef(i)*Bms(i))
          IF ( Sat_moist_stor(i)>0.0 ) THEN
            fr = ksat*(1.0+ps/Sat_moist_stor(i))
          ELSE
            fr = 0.0
            IF ( ksat>0.0 ) fr = ksat*(1.0+ps/srn)
          ENDIF

          last_stor = Imperv_stor(i)

          DO j = 1, icdels
            Hru_rain_int(j, i) = availh2o/cdels
            qrp = srn - 0.5*fr
            IF ( srn<fr .AND. fr/=0.0 ) qrp = 0.5*srn*srn/fr
            fin = srn - qrp
            Infil(i) = Infil(i) + fin
            Sat_moist_stor(i) = Sat_moist_stor(i) + fin

            IF ( Print_debug==10 ) WRITE ( DBGUNIT, 9002 ) i, j, srn, fr, qrp

            fr = 0.0
            IF ( ksat>0.0 .AND. Sat_moist_stor(i)>0.0 ) fr = ksat*(1.0+ps/Sat_moist_stor(i))

            IF ( Hru_type(i)==1 ) THEN
              Ppt_exc(j, i) = qrp
              Hru_pptexc(i) = qrp*hperv
            ENDIF
            IF ( imperv_flag==1 ) THEN
              Imperv_stor(i) = Imperv_stor(i) + srn
              IF ( Hru_type(i)==1 ) THEN
                IF ( Imperv_stor(i)>Imperv_stor_max(i) ) THEN
                  Ppt_exc_imp(j, i) = Imperv_stor(i) - Imperv_stor_max(i)
                  Imperv_stor(i) = Imperv_stor_max(i)
                  Hru_pptexc(i) = Hru_pptexc(i) + Ppt_exc_imp(j, i)*himperv
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          Basin_pptexc = Basin_pptexc + Hru_pptexc(i)
          Hru_pptexc(i) = Hru_pptexc(i)/Hru_area(i)
        ENDIF

        ! no ET if precipitation
        IF ( Hru_ppt(i)<NEARZERO ) THEN
          avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)
          IF ( imperv_flag==1 ) THEN
            CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i), Snowcov_area(i), avail_et)
            Hru_impervevap(i) = Imperv_evap(i)*Imperv_frac
            avail_et = avail_et - Hru_impervevap(i)
          ENDIF

          IF ( Sat_moist_stor(i)<avail_et ) THEN
            Bms(i) = Bms(i) + Sat_moist_stor(i) - avail_et
            IF ( Bms(i)<0.0 ) Bms(i) = 0.0
            Sat_moist_stor(i) = 0.0
          ELSE
            Sat_moist_stor(i) = Sat_moist_stor(i) - avail_et
          ENDIF

          drn = Kpar24(i)*Timestep_days*Drnpar(i)
          IF ( Sat_moist_stor(i)>drn ) THEN
            Sat_moist_stor(i) = Sat_moist_stor(i) - drn
            Bms(i) = Bms(i) + drn
          ELSE
            Bms(i) = Bms(i) + Sat_moist_stor(i)
            Sat_moist_stor(i) = 0.0
          ENDIF
          IF ( Bms(i)>Bmsm(i) ) Bms(i) = Bmsm(i)
        ENDIF

        IF ( Print_debug>0 ) THEN
          wbal = Infil(i)*Hru_frac_perv(i) + Hru_pptexc(i) - availh2o
          IF ( Imperv_flag==1 ) wbal = wbal + (Imperv_evap(i)+Imperv_stor(i)-last_stor)*Hru_frac_imperv(i)
          IF ( Print_debug==1 ) THEN
            IF ( ABS(wbal)>1.E-05 ) THEN
              WRITE ( BALUNT, * ) 'Green-Ampt HRU water-balance issue'
              WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, Nowhour, Nowminute, i, wbal, Infil(i), &
     &                Hru_pptexc(i), availh2o, last_stor, Imperv_stor(i), Imperv_evap(i), Net_ppt(i)
            ENDIF
          ELSEIF ( Print_debug==10 ) THEN
            WRITE ( DBGUNIT, 9003 ) Nowday, Nowhour, Nowminute, i, Infil(i), Hru_pptexc(i), availh2o, &
     &              wbal, Net_ppt(i), Hru_pptexc(i)
          ENDIF
        ENDIF
 
      ENDDO
 
      Basin_pptexc = Basin_pptexc*Basin_area_inv
      Storm_pptexc = Storm_pptexc + Basin_pptexc
      Basin_storm_precip = Basin_storm_precip*Basin_area_inv
 
      IF ( Print_debug==10 ) THEN
        WRITE ( DBGUNIT, '(3I3,2F10.5)' ) Nowday, Nowhour, Nowminute, Basin_pptexc, Storm_pptexc
        DO j = 1, Ncdels
          WRITE ( DBGUNIT, '(10F8.3)' ) (Ppt_exc(j, i), i=1, Nhru)
          DO i = 1, Nhru
            Hru_sum(i) = Hru_sum(i) + Ppt_exc(j, i)
          ENDDO
        ENDDO
        IF ( Nowhour==24 ) THEN
          WRITE ( DBGUNIT, 9005 ) (Hru_sum(i), i=1, Nhru)
          Hru_sum = 0.0
        ENDIF
      ELSEIF ( Print_debug==1 ) THEN
        WRITE ( BALUNT, 9006 ) Nowyear, Nowmonth, Nowday, Nowhour, Nowminute, Basin_pptexc, Storm_pptexc, Basin_storm_precip
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), I2.2, ':', I2.2, ' HRU:', I7, 8F11.5)
 9002 FORMAT (' i-j-srn-fr-qrp', 2I4, 3F10.7)
 9003 FORMAT (4I4, 6F8.5)
 9004 FORMAT (3I3, 7F8.4)
 9005 FORMAT ('DAILY TOTAL: ', 7F8.4)
 9006 FORMAT (I5, 2('/', I2.2), I3.2, ':', I2.2, 3F14.5)

      END FUNCTION mgarun

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE grnampt_infil_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Nobs
      USE PRMS_GRNAMPT
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Ncdels, DBGUNIT, BALUNT, Basin_pptexc, Storm_pptexc, Basin_storm_precip
        WRITE ( Restart_outunit ) Bms
        WRITE ( Restart_outunit ) Bmsm
        WRITE ( Restart_outunit ) Coef
        WRITE ( Restart_outunit ) Sat_moist_stor
        WRITE ( Restart_outunit ) Hru_pptexc
        WRITE ( Restart_outunit ) Storm_precip
        IF ( Nobs>0 ) THEN
          WRITE ( Restart_outunit ) Storm_obsvol
          WRITE ( Restart_outunit ) Storm_obspk
        ENDIF
        WRITE ( Restart_outunit ) Ppt_exc
        WRITE ( Restart_outunit ) Ppt_exc_imp
        WRITE ( Restart_outunit ) Hru_rain_int
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Ncdels, DBGUNIT, BALUNT, Basin_pptexc, Storm_pptexc, Basin_storm_precip
        READ ( Restart_inunit ) Bms
        READ ( Restart_inunit ) Bmsm
        READ ( Restart_inunit ) Coef
        READ ( Restart_inunit ) Sat_moist_stor
        READ ( Restart_inunit ) Hru_pptexc
        READ ( Restart_inunit ) Storm_precip
        IF ( Nobs>0 ) THEN
          READ ( Restart_inunit ) Storm_obsvol
          READ ( Restart_inunit ) Storm_obspk
        ENDIF
        READ ( Restart_inunit ) Ppt_exc
        READ ( Restart_inunit ) Ppt_exc_imp
        READ ( Restart_inunit ) Hru_rain_int
      ENDIF
      END SUBROUTINE grnampt_infil_restart
