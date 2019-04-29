!***********************************************************************
! Computes volume of intercepted precipitation, evaporation from
! intercepted precipitation, and throughfall that reaches the soil or
! snowpack
!***********************************************************************
      MODULE PRMS_INTCP
      IMPLICIT NONE
      INTEGER, PARAMETER :: BALUNT = 198
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: Intcp_transp_on(:)
      DOUBLE PRECISION, SAVE :: Basin_changeover
!   Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Intcp_on(:), Intcp_form(:)
      DOUBLE PRECISION, SAVE :: Basin_net_ppt, Basin_intcp_stor
      DOUBLE PRECISION, SAVE :: Last_intcp_stor, Basin_intcp_evap
      REAL, SAVE, ALLOCATABLE :: Net_rain(:), Net_snow(:), Net_ppt(:)
      REAL, SAVE, ALLOCATABLE :: Intcp_stor(:), Intcp_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_intcpevap(:), Hru_intcpstor(:)
!   Declared Variables from other modules - snow
      DOUBLE PRECISION, ALLOCATABLE :: Pkwater_equiv(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Cov_type(:), Hru_pansta(:)
      REAL, SAVE :: Potet_sublim
      REAL, SAVE, ALLOCATABLE :: Covden_sum(:), Covden_win(:)
      REAL, SAVE, ALLOCATABLE :: Snow_intcp(:), Srain_intcp(:)
      REAL, SAVE, ALLOCATABLE :: Epan_coef(:), Wrain_intcp(:)
      
      CHARACTER*(*) MODNAME
      PARAMETER(MODNAME='intcp')
      CHARACTER*(*) PROCNAME
      PARAMETER(PROCNAME='Canopy Interception')
      
      END MODULE PRMS_INTCP

!***********************************************************************
!     Main intcp routine
!***********************************************************************
      INTEGER FUNCTION intcp()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: intdecl, intinit, intrun
!***********************************************************************
      intcp = 0

      IF ( Process(:3)=='run' ) THEN
        intcp = intrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        intcp = intdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        intcp = intinit()
      ENDIF

      END FUNCTION intcp

!***********************************************************************
!     intdecl - set up parameters for interception computations
!   Declared Parameters
!     snow_intcp, srain_intcp, wrain_intcp, potet_sublim, cov_type
!     covden_win, covden_sum, epan_coef, hru_area, hru_pansta
!***********************************************************************
      INTEGER FUNCTION intdecl()
      USE PRMS_INTCP
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Version_intcp, Intcp_nc
      USE PRMS_OBS, ONLY: Nevap
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
!***********************************************************************
      intdecl = 1

      Version_intcp = '$Id: intcp.f90 4077 2012-01-05 23:46:06Z rsregan $'
      Intcp_nc = INDEX( Version_intcp, ' $' ) + 1
      IF ( Print_debug>-1 ) THEN
        IF ( declmodule(MODNAME, PROCNAME, Version_intcp(:Intcp_nc))/=0 ) STOP
      ENDIF

      ALLOCATE ( Net_rain(Nhru) )
      IF ( declvar(MODNAME, 'net_rain', 'nhru', Nhru, 'real', &
           'Rain that falls through canopy for each HRU', &
           'inches', Net_rain)/=0 ) CALL read_error(3, 'net_rain')

      ALLOCATE ( Net_snow(Nhru) )
      IF ( declvar(MODNAME, 'net_snow', 'nhru', Nhru, 'real', &
           'Snow that falls through canopy for each HRU', &
           'inches', Net_snow)/=0 ) CALL read_error(3, 'net_snow')

      ALLOCATE ( Net_ppt(Nhru) )
      IF ( declvar(MODNAME, 'net_ppt', 'nhru', Nhru, 'real', &
           'Precipitation (rain and/or snow) that falls through the canopy for each HRU', &
           'inches', Net_ppt)/=0 ) CALL read_error(3, 'net_ppt')

      IF ( declvar(MODNAME, 'basin_net_ppt', 'one', 1, 'double', &
           'Basin area-weighted average throughfall', &
           'inches', Basin_net_ppt)/=0 ) CALL read_error(3, 'basin_net_ppt')

      ALLOCATE ( Intcp_stor(Nhru) )
      IF ( declvar(MODNAME, 'intcp_stor', 'nhru', Nhru, 'real', &
           'Interception storage in canopy for cover density for each HRU', &
           'inches', Intcp_stor)/=0 ) CALL read_error(3, 'intcp_stor')

      IF ( declvar(MODNAME, 'last_intcp_stor', 'one', 1, 'double', &
           'Basin area-weighted average changeover interception storage', &
           'inches', Last_intcp_stor)/=0 ) CALL read_error(3, 'last_intcp_stor')

      IF ( declvar(MODNAME, 'basin_intcp_stor', 'one', 1, 'double', &
           'Basin area-weighted average interception storage', &
           'inches', Basin_intcp_stor)/=0 ) CALL read_error(3, 'basin_intcp_stor')

      ALLOCATE ( Intcp_evap(Nhru) )
      IF ( declvar(MODNAME, 'intcp_evap', 'nhru', Nhru, 'real', &
           'Evaporation from the canopy for each HRU', &
           'inches', Intcp_evap)/=0 ) CALL read_error(3, 'intcp_evap')

      IF ( declvar(MODNAME, 'basin_intcp_evap', 'one', 1, 'double', &
           'Basin area-weighted evaporation from the canopy', &
           'inches', Basin_intcp_evap)/=0 ) CALL read_error(3, 'basin_intcp_evap')

      ALLOCATE ( Hru_intcpevap(Nhru) )
      IF ( declvar(MODNAME, 'hru_intcpevap', 'nhru', Nhru, 'real', &
           'Evaporation from the canopy for each HRU', &
           'inches', Hru_intcpevap)/=0 ) CALL read_error(3, 'hru_intcpevap')

      ALLOCATE ( Hru_intcpstor(Nhru) )
      IF ( declvar(MODNAME, 'hru_intcpstor', 'nhru', Nhru, 'real', &
           'Interception storage in the canopy for each HRU', &
           'inches', Hru_intcpstor)/=0 ) CALL read_error(3, 'hru_intcpstor')

      ALLOCATE ( Intcp_form(Nhru) )
      IF ( declvar(MODNAME, 'intcp_form', 'nhru', Nhru, 'integer', &
           'Form (rain or snow) of interception for each HRU', &
           'none', Intcp_form)/=0 ) CALL read_error(3, 'intcp_form')

      ALLOCATE ( Intcp_on(Nhru) )
      IF ( declvar(MODNAME, 'intcp_on', 'nhru', Nhru, 'integer', &
           'Flag indicating interception storage for each HRU (0=no; 1=yes)', &
           'none', Intcp_on)/=0 ) CALL read_error(3, 'intcp_on')

! declare parameters
      ALLOCATE ( Epan_coef(12) )
      IF ( declparam(MODNAME, 'epan_coef', 'nmonths', 'real', &
           '1.0', '0.2', '3.0', &
           'Evaporation pan coefficient', &
           'Monthly (January to December ) evaporation pan coefficient', &
           'none')/=0 ) CALL read_error(1, 'epan_coef')

      ALLOCATE ( Snow_intcp(Nhru) )
      IF ( declparam(MODNAME, 'snow_intcp', 'nhru', 'real', &
           '0.1', '0.0', '5.0', &
           'Snow interception storage capacity', &
           'Snow interception storage capacity for the major vegetation type in each HRU', &
           'inches')/=0 ) CALL read_error(1, 'snow_intcp')

      ALLOCATE ( Srain_intcp(Nhru) )
      IF ( declparam(MODNAME, 'srain_intcp', 'nhru', 'real', &
           '0.1', '0.0', '5.0', &
           'Summer rain interception storage capacity', &
           'Summer rain interception storage capacity for the major vegetation type in each HRU', &
           'inches')/=0 ) CALL read_error(1, 'srain_intcp')

      ALLOCATE ( Wrain_intcp(Nhru) )
      IF ( declparam(MODNAME, 'wrain_intcp', 'nhru', 'real', &
           '0.1', '0.0', '5.0', &
           'Winter rain interception storage capacity', &
           'Winter rain interception storage capacity for the major vegetation type in each HRU', &
           'inches')/=0 ) CALL read_error(1, 'wrain_intcp')

      ALLOCATE ( Cov_type(Nhru) )
      IF ( declparam(MODNAME, 'cov_type', 'nhru', 'integer', &
           '3', '0', '3', &
           'Cover type designation for HRU', &
           'Vegetation cover type for each HRU (0=bare soil; 1=grasses; 2=shrubs; 3=trees)', &
           'none')/=0 ) CALL read_error(1, 'cov_type')

      ALLOCATE ( Covden_sum(Nhru) )
      IF ( declparam(MODNAME, 'covden_sum', 'nhru', 'real', &
           '0.5', '0.0', '1.0', &
           'Summer vegetation cover density for major vegetation type', &
           'Summer vegetation cover density for the major vegetation type in each HRU', &
           'decimal fraction')/=0 ) CALL read_error(1, 'covden_sum')

      ALLOCATE ( Covden_win(Nhru) )
      IF ( declparam(MODNAME, 'covden_win', 'nhru', 'real', &
           '0.5', '0.0', '1.0', &
           'Winter vegetation cover density for major vegetation type', &
           'Winter vegetation cover density for the major vegetation type in each HRU', &
           'decimal fraction')/=0 ) CALL read_error(1, 'covden_win')

      IF ( declparam(MODNAME, 'potet_sublim', 'one', 'real', &
           '0.5', '0.1', '0.75', &
           'Fraction of potential ET that is sublimated from snow surface', &
           'Fraction of potential ET that is sublimated from the snow surface', &
           'decimal fraction')/=0 ) CALL read_error(1, 'potet_sublim')

      IF ( Nevap>0 ) THEN
        ALLOCATE ( Hru_pansta(Nhru) )
        IF ( declparam(MODNAME, 'hru_pansta', 'nhru', 'integer', &
             '0', 'bounded', 'nevap', &
             'Index of pan evaporation station for each HRU', &
             'Index of pan evaporation station used to compute HRU potential ET', &
             'none')/=0 ) CALL read_error(1, 'hru_pansta')
      ENDIF

! Allocate arrays for variables from other modules
      ALLOCATE ( Pkwater_equiv(Nhru) )

      intdecl = 0
      END FUNCTION intdecl

!***********************************************************************
!     intinit - Initialize intcp module - get parameter values,
!               set initial values.
!***********************************************************************
      INTEGER FUNCTION intinit()
      USE PRMS_INTCP
      USE PRMS_MODULE, ONLY: Nhru, Print_debug
      USE PRMS_BASIN, ONLY: Hru_type, Timestep, NEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Transp_on
      USE PRMS_OBS, ONLY: Nevap
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: i
!***********************************************************************
      intinit = 1

      IF ( getparam(MODNAME, 'snow_intcp', Nhru, 'real', Snow_intcp)/=0 ) CALL read_error(2, 'snow_intcp')
      IF ( getparam(MODNAME, 'wrain_intcp', Nhru, 'real', Wrain_intcp)/=0 ) CALL read_error(2, 'wrain_intcp')
      IF ( getparam(MODNAME, 'srain_intcp', Nhru, 'real', Srain_intcp)/=0 ) CALL read_error(2, 'srain_intcp')
      IF ( getparam(MODNAME, 'cov_type', Nhru, 'integer', Cov_type)/=0 ) CALL read_error(2, 'cov_type')
      IF ( getparam(MODNAME, 'covden_sum', Nhru, 'real', Covden_sum)/=0 ) CALL read_error(2, 'covden_sum')
      IF ( getparam(MODNAME, 'covden_win', Nhru, 'real', Covden_win)/=0 ) CALL read_error(2, 'covden_win')
      IF ( getparam(MODNAME, 'epan_coef', 12, 'real', Epan_coef)/=0 ) CALL read_error(2, 'epan_coef')
      DO i = 1, 12
        IF ( Epan_coef(i)<NEARZERO ) THEN
          PRINT *, 'Warning, epan_coef specified as 0 for month:', i, ' value changed to 1.0'
          Epan_coef(i) = 1.0
        ENDIF
      ENDDO
      IF ( Nevap>0 ) THEN
        IF ( getparam(MODNAME, 'hru_pansta', Nhru, 'integer', Hru_pansta)/=0 ) CALL read_error(2, 'hru_pansta')
        DO i = 1, Nhru
          IF ( Hru_pansta(i)<1 .OR. Hru_pansta(i)>Nevap ) THEN
            IF ( Print_debug>-1 ) PRINT *, 'Warning, hru_pansta=0 or hru_pansta>nevap, set to 1 for HRU:', i
            Hru_pansta(i) = 1
          ENDIF
        ENDDO
      ENDIF

      IF ( getparam(MODNAME, 'potet_sublim', 1, 'real', Potet_sublim)/=0 ) CALL read_error(2, 'potet_sublim')

      IF ( Print_debug==1 ) THEN
        OPEN (BALUNT, FILE='intcp.wbal')
        WRITE (BALUNT, 9001)
      ENDIF

      ALLOCATE ( Intcp_transp_on(Nhru) )
      DO i = 1, Nhru
        Intcp_transp_on(i) = Transp_on(i)
        IF ( Covden_win(i)<NEARZERO ) Covden_win(i) = 0.0
        IF ( Covden_sum(i)<NEARZERO ) Covden_sum(i) = 0.0
        IF ( Covden_win(i)>Covden_sum(i) ) THEN
          IF ( Print_debug>-1 ) THEN
            PRINT *, 'Warning, covden_win>covden_sum, HRU:', i, Covden_win(i), Covden_sum(i)
            PRINT *, ' Set covden_win to covden_sum'
          ENDIF
          Covden_win(i) = Covden_sum(i)
        ENDIF
        IF ( Cov_type(i)==0 .AND. (Covden_win(i)>0.0 .OR. Covden_sum(i)>0.0) ) THEN
          Covden_sum(i) = 0.0
          Covden_win(i) = 0.0
        ENDIF
        IF ( Cov_type(i)/=0 .AND. Hru_type(i)==2 ) THEN
          IF ( Print_debug>-1 ) PRINT *,  'Warning, cov_type must be 0 for lakes,', &
               ' reset from:', Cov_type(i), ' to 0 for HRU:', i
          Cov_type(i) = 0
          Covden_sum(i) = 0.0
          Covden_win(i) = 0.0
        ENDIF
      ENDDO

      IF ( Timestep==0 ) THEN
        Intcp_stor = 0.0
        Intcp_on = 0
        Intcp_form = 0
        Intcp_evap = 0.0
        Hru_intcpevap = 0.0
        Hru_intcpstor = 0.0
        Net_rain = 0.0
        Net_snow = 0.0
        Net_ppt = 0.0
        Basin_net_ppt = 0.0D0
        Basin_intcp_evap = 0.0D0
        Basin_intcp_stor = 0.0D0
        Last_intcp_stor = 0.0D0
      ENDIF

 9001 FORMAT ('    Date     Water Bal     Precip     Netppt  Intcpevap  Intcpstor  last_stor')

      intinit = 0
      END FUNCTION intinit

!***********************************************************************
!     intrun - Computes and keeps track of intercepted precipitation
!              and evaporation for each HRU
!***********************************************************************
      INTEGER FUNCTION intrun()
      USE PRMS_INTCP
      USE PRMS_MODULE, ONLY: Nhru, Print_debug
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_type, &
          Hru_route_order, Hru_area, NEARZERO, DNEARZERO
! Newsnow and Pptmix can be modfied, WARNING!!!
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Hru_rain, Hru_ppt, &
          Hru_snow, Basin_ppt, Transp_on, Potet
      USE PRMS_OBS, ONLY: Pan_evap, Nowtime, Nevap, Nowmonth, Nowday, Nowyear
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getvar
      EXTERNAL intercept, read_error
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, j
      REAL :: last, evrn, evsn, cov, evap, intcpstor, stor_last
      REAL :: diff, changeover, stor, intcpevap, z, d, delstor, harea
      DOUBLE PRECISION :: hrubal, delta_stor, pptbal, basin_last_stor
!***********************************************************************
      intrun = 1

      IF ( getvar(MODNAME, 'pkwater_equiv', Nhru, 'double', Pkwater_equiv)/=0 ) CALL read_error(4, 'pkwater_equiv')

      Basin_changeover = 0.0D0
      basin_last_stor = Basin_intcp_stor
      Basin_net_ppt = 0.0D0
      Basin_intcp_evap = 0.0D0
      Basin_intcp_stor = 0.0D0

      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        harea = Hru_area(i)
        Net_rain(i) = Hru_rain(i)
        Net_snow(i) = Hru_snow(i)
        Net_ppt(i) = Hru_ppt(i)

        ! Lake or bare ground HRUs
        IF ( Hru_type(i)==2 .OR. Cov_type(i)==0 ) THEN
          Basin_net_ppt = Basin_net_ppt + Net_ppt(i)*harea
          CYCLE
        ENDIF

        stor_last = Hru_intcpstor(i)

!******Adjust interception amounts for changes in summer/winter cover
!******density

        IF ( Transp_on(i)==1 ) THEN
          cov = Covden_sum(i)
        ELSE
          cov = Covden_win(i)
        ENDIF
        Intcp_form(i) = 0

        intcpstor = Intcp_stor(i)
        intcpevap = 0.0
        changeover = 0.0

!*****Determine the amount of interception from rain

!***** go from summer to winter cover density
        !rsr, changeovers for whole HRU
        IF ( Transp_on(i)==0 .AND. Intcp_transp_on(i)==1 ) THEN
          Intcp_transp_on(i) = 0
          IF ( intcpstor>0.0 ) THEN
            ! assume canopy storage change falls as throughfall
            diff = Covden_sum(i) - cov
            changeover = intcpstor*diff
            IF ( cov>0.0 ) THEN
              stor_last = stor_last - changeover
            ELSE
              IF ( Print_debug>-1 ) PRINT *, 'covden_win=0 at winter changeover and has', &
                   ' canopy storage', intcpstor, changeover, i
              stor_last = 0.0
              intcpstor = 0.0
              Intcp_on(i) = 0
            ENDIF
            basin_last_stor = basin_last_stor - changeover*harea*Basin_area_inv
            Basin_changeover = Basin_changeover + changeover*harea
          ENDIF

!****** go from winter to summer cover density
        ELSEIF ( Transp_on(i)==1 .AND. Intcp_transp_on(i)==0 ) THEN
          Intcp_transp_on(i) = 1
          IF ( intcpstor>0.0 ) THEN
            diff = Covden_win(i) - cov
            IF ( -intcpstor*diff>NEARZERO ) THEN
              IF ( cov>0.0 ) THEN
                intcpstor = intcpstor*Covden_win(i)/cov
              ELSE
                PRINT *, 'intcp problem, covden_sum=0.0 when covden_win>0.0, HRU:', i, &
                         ' intcp_stor:', intcpstor, ' covden_sum:', Covden_sum(i), ' covden_win:', Covden_win(i)
              ENDIF
            ENDIF
          ENDIF
        ENDIF

!*****Determine the amount of interception from rain

        IF ( Transp_on(i)==1 ) THEN
          stor = Srain_intcp(i)
        ELSE
          stor = Wrain_intcp(i)
        ENDIF
        IF ( Hru_rain(i)>0.0 .AND. cov>0.0 ) THEN

          IF ( Cov_type(i)>1 ) THEN
            CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i), intcpstor, Net_rain(i))
          ELSEIF ( Cov_type(i)==1 ) THEN
            !rsr, 03/24/2008 intercept rain on snow-free grass,
            !rsr             when not a mixed event
            IF ( Pkwater_equiv(i)<DNEARZERO .AND. Hru_snow(i)<NEARZERO ) THEN
              CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i), intcpstor, Net_rain(i))
              !rsr 03/24/2008
              !it was decided to leave the water in intcpstor rather
              !than put the water in the snowpack, as doing so for a
              !mixed event on grass with snow-free surface produces a
              !divide by zero in snowcomp. Storage on grass will
              !eventually evaporate
            ENDIF
          ENDIF
        ENDIF
        Net_rain(i) = Net_rain(i) + changeover

!******Determine amount of interception from snow

        IF ( Hru_snow(i)>0.0 .AND. cov>0.0 ) THEN
          Intcp_form(i) = 1
          IF ( Cov_type(i)>1 ) THEN
            stor = Snow_intcp(i)
            CALL intercept(Hru_snow(i), stor, cov, Intcp_on(i), intcpstor, Net_snow(i))
            IF ( Net_snow(i)<NEARZERO ) THEN   !rsr, added 3/9/2006
              Net_rain(i) = Net_rain(i) + Net_snow(i)
              Net_snow(i) = 0.0
              Newsnow(i) = 0
              Pptmix(i) = 0   ! reset to be sure it is zero
            ENDIF
          ENDIF
        ENDIF

        Net_ppt(i) = Net_rain(i) + Net_snow(i)

!******compute evaporation or sublimation of interception

        ! if precipitation assume no evaporation or sublimation
        IF ( Hru_ppt(i)<NEARZERO ) THEN
          IF ( Intcp_on(i)==1 ) THEN

            evrn = Potet(i)/Epan_coef(Nowmonth)
            evsn = Potet_sublim*Potet(i)

            IF ( Nevap>0 ) THEN
              IF ( Pan_evap(Hru_pansta(i))>-NEARZERO ) evrn = Pan_evap(Hru_pansta(i))
              IF ( evrn<NEARZERO ) evrn = 0.0
            ENDIF

!******Compute snow interception loss

            IF ( Intcp_form(i)==1 ) THEN
              IF ( Basin_ppt<DNEARZERO ) THEN
                z = intcpstor - evsn
                IF ( z>0.0 ) THEN
                  Intcp_on(i) = 1
                  intcpstor = z
                  intcpevap = evsn
                ELSE
                  intcpevap = intcpstor
                  intcpstor = 0.0
                  Intcp_on(i) = 0
                ENDIF
              ENDIF
!           ELSEIF ( Intcp_form(i)==0 ) THEN
            ELSE
              d = intcpstor - evrn
              IF ( d>0.0 ) THEN
                intcpstor = d
                intcpevap = evrn
                Intcp_on(i) = 1
              ELSE
                intcpevap = intcpstor
                intcpstor = 0.0
                Intcp_on(i) = 0
              ENDIF
            ENDIF
          ENDIF

        ENDIF

        evap = intcpevap*cov
        IF ( evap>Potet(i) ) THEN
          evap = Potet(i)
          last = intcpevap
          IF ( cov>0.0 ) THEN
            intcpevap = Potet(i)/cov
          ELSE
            intcpevap = 0.0
          ENDIF
          intcpstor = intcpstor + last - intcpevap
        ENDIF
        Intcp_evap(i) = intcpevap
        Hru_intcpevap(i) = intcpevap*cov
        Intcp_stor(i) = intcpstor
        Hru_intcpstor(i) = intcpstor*cov

        !rsr, question about depression storage for basin_net_ppt???
        !     my assumption is that cover density is for the whole HRU
        Basin_net_ppt = Basin_net_ppt + Net_ppt(i)*harea
        Basin_intcp_stor = Basin_intcp_stor + intcpstor*cov*harea
        Basin_intcp_evap = Basin_intcp_evap + intcpevap*cov*harea

        IF ( Print_debug==1 ) THEN
          delstor = Hru_intcpstor(i) - stor_last
          hrubal = Hru_rain(i) + Hru_snow(i) - Net_rain(i) - Net_snow(i) &
                   - delstor - Hru_intcpevap(i) + changeover
          IF ( ABS(hrubal)>1.0D-6 ) THEN
            IF ( ABS(hrubal)>1.0D-4 ) THEN
              WRITE (BALUNT, *) 'Possible HRU water balance error'
            ELSE
              WRITE (BALUNT, *) 'Interception HRU rounding issue'
            ENDIF
            WRITE ( BALUNT,'(7I5,15F10.5)' ) i, Nowtime, hrubal, &
                    Net_rain(i), Net_snow(i), Hru_rain(i), Hru_snow(i), &
                    intcpstor, stor_last, intcpevap, Srain_intcp(i), &
                    Wrain_intcp(i), Snow_intcp(i), cov, delstor, &
                    Hru_intcpstor(i), changeover
          ENDIF
        ENDIF

      ENDDO

      Basin_net_ppt = Basin_net_ppt*Basin_area_inv
      Basin_intcp_stor = Basin_intcp_stor*Basin_area_inv
      Basin_intcp_evap = Basin_intcp_evap*Basin_area_inv
      Basin_changeover = Basin_changeover*Basin_area_inv
      Last_intcp_stor = Basin_changeover

      IF ( Print_debug==1 ) THEN
        delta_stor = Basin_intcp_stor - basin_last_stor
        pptbal = Basin_ppt - Basin_net_ppt - delta_stor - Basin_intcp_evap + Basin_changeover
        IF ( ABS(pptbal)>1.0D-4 ) THEN
          WRITE ( BALUNT, * ) 'Possible basin water balance error', pptbal
        ELSEIF ( ABS( pptbal )>1.0D-5 ) THEN
          WRITE ( BALUNT, * ) 'Interception basin rounding issue', pptbal
        ENDIF
        WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, pptbal, &
                Basin_ppt, Basin_net_ppt, Basin_intcp_evap, &
                Basin_intcp_stor, basin_last_stor, Basin_changeover
      ENDIF

 9001 FORMAT (I5, 2('/', I2.2), 7F11.5)

      intrun = 0
      END FUNCTION intrun

!***********************************************************************
!      Subroutine to compute interception of rain or snow
!***********************************************************************
      SUBROUTINE intercept(Precip, Stor_max, Cov, Intcp_on, Intcp_stor, Net_precip)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(OUT) :: Intcp_on
      REAL, INTENT(IN) :: Precip, Cov, Stor_max
      REAL, INTENT(INOUT) :: Intcp_stor
      REAL, INTENT(OUT) :: Net_precip
!***********************************************************************
      Intcp_on = 1

      Net_precip = Precip*(1.0-Cov)

      Intcp_stor = Intcp_stor + Precip

      IF ( Intcp_stor>Stor_max ) THEN
        Net_precip = Net_precip + (Intcp_stor-Stor_max)*Cov
        Intcp_stor = Stor_max
      ENDIF

!*** allow intcp_stor to exceed stor_max with small amounts of precip
      IF ( Net_precip<0.000001 ) THEN
        IF ( Cov>0.0 ) THEN
          Intcp_stor = Intcp_stor + Net_precip/Cov
          Net_precip = 0.0
        ENDIF
      ENDIF

      END SUBROUTINE intercept

