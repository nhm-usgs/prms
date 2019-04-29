!***********************************************************************
! Routes water between segments in the system using Muskingum routing
!
!                   created by Tom Ryan USBR (801) 524-5573
!
!                               April 24, 1992
!   REVISED: 11/25/98 by Mark Mastin to include Muskingum routing in Tom
!         Ryan's fixroute module. Changed name from fixroute to musroute.

!   The Muskingum equation is described in 'Hydrology for Engineers', 3rd ed.
!   by Linsley, R.K, Kohler, M.A., and Paulhus, J.L.H., 1982 p. 275 and in 
!   'Water in Environmental Planning' by Dunne, T., and Leopold, L.B. 1978
!   p. 357.
!   
!   Note that the Muskingum equation assumes a linear relation of storage
!   to the inflow/outflow relation and therefore the relation is the same
!   throughout the range of the hydrograph.  The route_time parameter in
!   the fixroute module is replaced by two new parameters, K_coef and 
!   x_coef, which are described below:
!
!   The Muskingum method is based on the equation: S = K[xI + (1 - x)O]
!     where S is storage, K is the storage coefficient, x is a coefficient
!     between 0 and .5, I is inflow, and O is outflow.
!
!   Solving for the Outflow at day 2,O2; and knowing the inflow at day 1,
!   I1; the inflow at day 2,I2; and the outflow at day 1, O1; the storage
!   equation can be written as follows:
!
!        O2 = czero*I2 + cone*I1 + ctwo*O1
!
!     where czero = -((Kx - 12)    / (K - Kx + 12))
!           cone  =  (Kx + 12)     / (K - Kx + 12)
!           ctwo  =  (K - Kx - 12) / (K - Kx + 12)
!
!     assuming a time step of one day and K is in units of hours
!
!
!   In the musroute module, czero, cone, and ctwo are solved in
!   the intialization function
!
! rsr, changed nlake to nsegment, forcing nhru=ngw=nssr, thus don't need
!      ssr_segment and gw_segment
!      final segment will be the last segment in the order array
!***********************************************************************
      MODULE PRMS_MUSROUTE
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Replace_flag, Table_flag
      INTEGER, SAVE, ALLOCATABLE :: Nratetable(:), Order(:)
      DOUBLE PRECISION, SAVE :: Flow_out
      REAL, SAVE, ALLOCATABLE :: Czero(:), Cone(:), Ctwo(:)
      REAL, SAVE, ALLOCATABLE :: Pastinflow(:), Pastoutflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_hruarea(:)
      CHARACTER(LEN=8), PARAMETER :: MODNAME = 'musroute'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Streamflow Routing'
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Segment_cfs(:), Tosegment_cfs(:)
      REAL, SAVE, ALLOCATABLE :: Seginc_ssflow(:), Seginc_sroff(:)
      REAL, SAVE, ALLOCATABLE :: Seginc_gwflow(:), Seginc_swrad(:)
!   Declared Parameters
!      INTEGER, SAVE :: Final_segment
      INTEGER, SAVE, ALLOCATABLE :: Tosegment(:), Hru_segment(:)
      INTEGER, SAVE, ALLOCATABLE :: Obsin_segment(:), Segment_type(:)
      REAL, SAVE, ALLOCATABLE :: K_coef(:), X_coef(:)
      END MODULE PRMS_MUSROUTE

!***********************************************************************
!     Main musroute routine
!***********************************************************************
      INTEGER FUNCTION musroute()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: musroute_decl, musroute_init, musroute_run
!***********************************************************************
      musroute = 0

      IF ( Process(:3)=='run' ) THEN
        musroute  = musroute_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        musroute  = musroute_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        musroute  = musroute_init()
      ENDIF

      END FUNCTION musroute

!***********************************************************************
!     musroute_decl - set up fixed routing parameters
!   Declared Parameters
!     tosegment, hru_segment, final_segment, segment_type, k_coef, x_coef
!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION musroute_decl()
      USE PRMS_MUSROUTE
      USE PRMS_MODULE, ONLY: Model, Nhru, Nsegment, Version_musroute, Musroute_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: i
!***********************************************************************
      musroute_decl = 1

      Version_musroute = '$Id: musroute.f90 4881 2012-10-04 20:37:56Z rsregan $'
      Musroute_nc = INDEX( Version_musroute, 'Z' )
      i = INDEX ( Version_musroute, '.f90' ) + 3
      IF ( declmodule(Version_musroute(6:i), PROCNAME, Version_musroute(i+2:Musroute_nc))/=0 ) STOP

      ALLOCATE ( Obsin_segment(Nsegment) )
      IF ( declparam(MODNAME, 'obsin_segment', 'nsegment', 'integer', &
           '0', 'bounded', 'nobs', &
           'Index of measured streamflow station that replaces inflow to a segment', &
           'Index of measured streamflow station that replaces inflow to a segment', &
           'none')/=0 ) CALL read_error(1, 'obsin_segment')

      ALLOCATE ( Segment_type(Nsegment) )
      IF ( declparam(MODNAME, 'segment_type', 'nsegment','integer', &
           '0', '0', '3', &
           'Segment type', &
           'Segment type (0=link; 1=reservoir; 2=diversion; 3=replace inflow)', &
           'none')/=0 ) CALL read_error(1, 'segment_type')

!     Declare all parameters that are part of this module in 
!     the declare function

      ALLOCATE ( Tosegment(Nsegment) )
      IF ( declparam(MODNAME, 'tosegment', 'nsegment', 'integer', &
           '0', 'bounded', 'nsegment', &
           'The index of the downstream segment', &
           'Index of downstream segment to which the segment'// &
           ' streamflow flows, for segments that do not flow to'// &
           ' another segment enter 0', &
           'none')/=0 ) CALL read_error(1, 'tosegment')

      ALLOCATE ( Hru_segment(Nhru) )
      IF ( declparam(MODNAME, 'hru_segment', 'nhru', 'integer', &
           '0', 'bounded', 'nsegment', &
           'Segment index for HRU lateral inflows', &
           'Segment index to which an HRU contributes lateral flows'// &
           ' (surface runoff, interflow, and groundwater discharge)', &
           'none')/=0 ) CALL read_error(1, 'hru_segment')

!      IF ( decl param(MODNAME, 'final_segment', 'one', 'integer', &
!           '0', 'bounded', 'nsegment', &
!           'Index of the final segment in the system', &
!           'Every routing network will have a final segment. Enter this final segment index', &
!           'none')/=0 ) CALL read_error(1, 'final_segment')

      ALLOCATE ( K_coef(Nsegment) )
      IF ( declparam(MODNAME, 'K_coef', 'nsegment', 'real', &
           '0.0',  '0.0', '240.0', &
           'Muskingum storage coefficient', &
           'Travel time of flood wave from one segment to the next'// &
           ' downstream segment, called the Muskingum storage'// &
           ' coefficient; enter 0.0 for reservoirs, diversions, and'// &
           ' segment(s) flowing out of the basin', &
           'hours')/=0 ) CALL read_error(1, 'K_coef')

      ALLOCATE ( X_coef(Nsegment) )
      IF ( declparam(MODNAME, 'x_coef', 'nsegment', 'real', &
           '0.2', '0.0', '0.5', &
           'Routing weighting factor', &
           'The amount of attenuation of the flow wave, called the'// &
           ' Muskingum routing weighting factor; enter 0.0 for'// &
           ' reservoirs, diversions, and segment(s) flowing out of the basin', &
           'hours')/=0 ) CALL read_error(1, 'x_coef')

!     Declare all variable to be used in the module, that have not 
!     yet been declared by other modules.

      ALLOCATE ( Seginc_swrad(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_swrad', 'nsegment', Nsegment, 'real', &
           'Area-weighted average solar radiation for each segment'// &
           ' from HRUs contributing flow to the segment', &
           'Langleys', Seginc_swrad)/=0 ) CALL read_error(3, 'seginc_swrad')

      ALLOCATE ( Seginc_ssflow(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_ssflow', 'nsegment', Nsegment, 'real', &
           'Area-weighted average interflow for each segment from'// &
           ' HRUs contributing flow to the segment', &
           'cfs', Seginc_ssflow)/=0 ) CALL read_error(3, 'seginc_ssflow')

      ALLOCATE ( Seginc_gwflow(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_gwflow', 'nsegment', Nsegment, 'real', &
           'Area-weighted average groundwater discharge for each'// &
           ' segment from HRUs contributing flow to the segment', &
           'cfs', Seginc_gwflow)/=0 ) CALL read_error(3, 'seginc_gwflow')

      ALLOCATE ( Seginc_sroff(Nsegment) )
      IF ( declvar(MODNAME, 'seginc_sroff', 'nsegment', Nsegment, 'real', &
           'Area-weighted average surface runoff for each'// &
           ' segment from HRUs contributing flow to the segment', &
           'cfs', Seginc_sroff)/=0 ) CALL read_error(3, 'seginc_sroff')

      ALLOCATE ( Segment_cfs(Nsegment) )
      IF ( declvar(MODNAME, 'segment_cfs', 'nsegment', Nsegment, 'real', &
           'Total inflow to a segment (sroff, ssres_flow, gwres_flow, upstream streamflow (tosgement_cfs)', &
           'cfs', Segment_cfs)/=0 ) CALL read_error(3, 'segment_cfs')

      ALLOCATE ( Tosegment_cfs(Nsegment) )
      IF ( declvar(MODNAME, 'tosegment_cfs', 'nsegment', Nsegment, 'real', &
           'Routed streamflow leaving a segment to a downstream segment or basin outlet', &
           'cfs', Tosegment_cfs)/=0 ) CALL read_error(3, 'tosegment_cfs')

!     If we make it through all the declares then set the function
!     to 0 (non error)

      ALLOCATE ( Cone(Nsegment), Ctwo(Nsegment), Czero(Nsegment) )
      ALLOCATE ( Pastinflow(Nsegment), Pastoutflow(Nsegment) )
      ALLOCATE ( Segment_hruarea(Nsegment), Order(Nsegment) )

      musroute_decl = 0
      END FUNCTION musroute_decl

!***********************************************************************
!
!    musroute_init - Initialize musroute  module - get parameter values
!                determine order of computing segments - set to zero if
!                no errors are encountered
!
!***********************************************************************
      INTEGER FUNCTION musroute_init()
      USE PRMS_MUSROUTE
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Nsegmentp1
!      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_BASIN, ONLY: Hru_area, NEARZERO, Timestep
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
! Local Variables
      INTEGER :: test, lval, i, j, badparameter
      INTEGER, ALLOCATABLE :: x_off(:)
      REAL :: kc, kx, ts
!***********************************************************************
      musroute_init = 1

      ALLOCATE ( x_off(Nsegment) )
      x_off = 0

!     Get all paramaters that will be used in this module in the
!     initialize function including but not limited to those declared
!     in the declare module

      IF ( getparam(MODNAME, 'tosegment', Nsegment, 'integer', Tosegment)/=0 ) CALL read_error(2, 'tosegment')
      IF ( getparam(MODNAME, 'hru_segment', Nhru, 'integer', Hru_segment)/=0 ) CALL read_error(2, 'hru_segment')
      IF ( getparam(MODNAME, 'segment_type', Nsegment, 'integer', Segment_type)/=0 ) CALL read_error(2, 'segment_type')

      Replace_flag = 0
      DO i = 1, Nsegment
        IF ( Segment_type(i)==3 ) Replace_flag = 1
      ENDDO
      IF ( Replace_flag==1 ) THEN
        IF ( getparam(MODNAME, 'obsin_segment', Nsegment, 'integer', Obsin_segment)/=0 ) CALL read_error(2, 'obsin_segment')
      ELSE
        Obsin_segment = 0
      ENDIF

!      IF ( get param(MODNAME, 'final_segment', 1, 'integer', Final_segment)/=0 ) CALL read_error(1, 'final_segment')
!      IF ( Final_segment==0 ) Final_segment = Nsegment

      IF ( getparam(MODNAME, 'K_coef',  Nsegment, 'real', K_coef)/=0 ) CALL read_error(2, 'K_coef')
      IF ( getparam(MODNAME, 'x_coef',  Nsegment, 'real', X_coef)/=0 ) CALL read_error(2, 'x_coef')

      Segment_hruarea = 0.0D0
      DO i = 1, Nsegment
        DO j = 1, Nhru
          IF ( Hru_segment(j)==i ) Segment_hruarea(i) = Segment_hruarea(i) + Hru_area(j)
        ENDDO
      ENDDO

!     Begin the loops for ordering

      badparameter = 0
      DO j = 1, Nsegment
        IF ( Tosegment(j)>Nsegment ) THEN
          badparameter = 1
          PRINT *, 'ERROR, tosegment value > nsegment', j
        ENDIF
        IF ( Tosegment(j)==0 ) Tosegment(j) = Nsegmentp1
      ENDDO
      IF ( badparameter==1 ) STOP

      Order = 0
      lval = 0
      DO WHILE ( lval < Nsegment )

        DO i = 1, Nsegment
!
!     If segment "i" has not been crossed out consider it, else continue
!
          IF ( x_off(i)==1 ) CYCLE
!
!     Test to see if segment "i" is the to segment from other segments
!
          test = 1
          DO j = 1, Nsegment
            IF ( Tosegment(j)==i ) THEN
!
!     If segment "i" is a to segment, test to see if the originaing
!     segment has been crossed off the list.  if all have been, then
!     put the segment in as an ordered segment
!
              IF ( x_off(j)==0 ) THEN
                test = 0
                EXIT
              ENDIF
            ENDIF
          ENDDO
          IF ( test==1 ) THEN
            lval = lval + 1
            Order(lval) = i
!            print *, i, ' put in order'
            x_off(i) = 1
          ENDIF
        ENDDO
      ENDDO
!      Order(Nsegment) = Final_segment !rsr, don't need, as found above
!      IF ( Print_debug>-1 ) THEN
!        PRINT *, 'Stream Network Routing Order:'
!        WRITE (*, '(10I5)') Order
!        PRINT *, 'tosegment:'
!        WRITE (*, '(10I5)') Tosegment
!      ENDIF
      DEALLOCATE ( x_off )

      IF ( Timestep==0 ) THEN
        Segment_cfs = 0.0
        Seginc_gwflow = 0.0
        Seginc_ssflow = 0.0
        Seginc_sroff = 0.0
        Seginc_swrad = 0.0
        Tosegment_cfs = 0.0
      ENDIF

!
!     Compute the three constants in the Muskingum routing equation based
!     on the values of K_coef, x_coef, and the routing period assumed to
!     24 hours, 

!  if c2 is le 0.0 then  short travel time though reach (less daily
!  flows), thus outflow is mainly = inflow w/ small influence of previous
!  inflow. Therefore, keep czero as is, and lower cone by ctwo, set ctwo=0

!  if c0 is le 0.0 then long travel time through reach (greater than daily
!  flows), thus mainly dependent on yesterdays flows.  Therefore, keep
!  ctwo as is, reduce cone by czero and set czero=0
!
      Czero = 0.0
      Cone = 0.0
      Ctwo = 0.0

      ts = 12.0
      DO i = 1, Nsegment
        kc = K_coef(i)
        kx = kc*X_coef(i)
        kc = kc - kx

        Czero(i) = -1.0*((kx - ts)/(kc + ts))
        Cone(i) = (kx + ts)/(kc + ts)
        Ctwo(i) = (kc - ts)/(kc + ts)

! SHORT travel time
        IF ( Ctwo(i)<NEARZERO ) THEN
          Cone(i) = Cone(i) + Ctwo(i)
          Ctwo(i) = 0.0
        ENDIF

! LONG travel time
        IF ( Czero(i)<NEARZERO ) THEN
          Cone(i) = Cone(i) + Czero(i)
          Czero(i) = 0.0
        ENDIF

      ENDDO
      DEALLOCATE ( K_coef, X_coef )

      musroute_init = 0
      END FUNCTION musroute_init

!***********************************************************************
!     musroute_run - Computes summary values
!***********************************************************************
      INTEGER FUNCTION musroute_run()
      USE PRMS_MUSROUTE
      USE PRMS_MODULE, ONLY: Nsegment, Nsegmentp1
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV, Basin_stflow_in, Basin_sroff_cfs, &
          Hru_area, Basin_area_inv, Basin_cfs, Hru_route_order, &
          Basin_cms, Basin_gwflow_cfs, Basin_ssflow_cfs, Active_hrus, DNEARZERO, &
          Basin_stflow_out
      USE PRMS_CLIMATEVARS, ONLY: Swrad
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Ssres_flow, Sroff, Basin_sroff
      USE PRMS_OBS, ONLY: Nowyear, Nowmonth, Nowday, Cfs_conv, Nowtime, Streamflow_cfs
      USE PRMS_GWFLOW, ONLY: Gwres_flow, Basin_gwflow
      IMPLICIT NONE
      INTRINSIC DBLE
! Local Variables
      INTEGER :: i, j, jj, iorder, toseg
      REAL pastin, pastout, todayinflow
      DOUBLE PRECISION :: area_fac, temp, tocfs
!***********************************************************************
!     Get all variable declared and computed by other modules
!     that will be used in this module

!     SET yesterdays inflows and outflows into temp (past arrays)
!     values may be 0.0 as intial, > 0.0 for runtime and dynamic 
!     initial condtions. Then set outlfow and inflow for this time
!     step to 0.0

      DO i = 1, Nsegment
        Pastinflow(i) = Segment_cfs(i)
        Pastoutflow(i) = Tosegment_cfs(i)
        Tosegment_cfs(i) = 0.0
        Segment_cfs(i) = 0.0
        Seginc_gwflow(i) = 0.0
        Seginc_ssflow(i) = 0.0
        Seginc_sroff(i) = 0.0
        Seginc_swrad(i) = 0.0
      ENDDO

!     Compute surface runoff, ssflow, and gwflow going to each segment
!     This is todays "segment_cfs" before additional water is routed to
!     a new (if any is routed)
!
!     For each hru if the surface sunoff for this hru goes to the 
!     segment being evaluated (segment i) then add it on
!     (0.042014 converts runoff in inches times area in acres for one day
!      to cfs, MCM)
      DO i = 1, Nsegment
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          IF ( Hru_segment(j)==i ) THEN
            tocfs = Hru_area(j)*Cfs_conv
            temp = (Sroff(j)+Ssres_flow(j)+Gwres_flow(j))*tocfs
            Segment_cfs(i) = Segment_cfs(i) + temp
            Seginc_sroff(i) = Seginc_sroff(i) + Sroff(j)*tocfs
            Seginc_ssflow(i) = Seginc_ssflow(i) + Ssres_flow(j)*tocfs
            Seginc_gwflow(i) = Seginc_gwflow(i) + Gwres_flow(j)*tocfs
            Seginc_swrad(i) = Seginc_swrad(i) + Swrad(j)*Hru_area(j)
          ENDIF
        ENDDO
      ENDDO

      DO i = 1, Nsegment
        IF ( Segment_hruarea(i)>DNEARZERO ) Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
      ENDDO
      DO i = 1, Nsegment
        IF ( Segment_hruarea(i)<DNEARZERO ) THEN
          IF ( Tosegment(i)>0 .AND. Tosegment(i)<Nsegmentp1 ) THEN
            Seginc_swrad(i) = Seginc_swrad(Tosegment(i))
          ELSEIF ( i>1 ) THEN
            Seginc_swrad(i) = Seginc_swrad(i-1)
          ENDIF
        ENDIF
      ENDDO
!
!       Out2   =      In2*c0    +        In1*c1    +          Out1*c2
!   tosegment_cfs = segment_cfs*czero +  pastinflow*cone +     pastoutflow*ctwo
!
!         c0, c1, and c2:
!        initialized in the "_init" part of this module
!
      Flow_out = 0.0D0
      DO i = 1, Nsegment
        iorder = Order(i)
        toseg = Tosegment(iorder)

        IF ( toseg==Nsegmentp1 ) THEN
          Flow_out = Flow_out + DBLE( Segment_cfs(iorder) )
          CYCLE
        ENDIF

        IF ( Replace_flag==1 .AND. Obsin_segment(iorder)>0 ) THEN
          Tosegment_cfs(iorder) = Streamflow_cfs(Obsin_segment(iorder))
        ELSE
          todayinflow = Segment_cfs(iorder)
          pastin = Pastinflow(iorder)
          pastout = Pastoutflow(iorder)

          Tosegment_cfs(iorder)= todayinflow*Czero(iorder) + &
                                 pastin*Cone(iorder) + pastout*Ctwo(iorder)
        ENDIF

        Segment_cfs(toseg) = Segment_cfs(toseg) + Tosegment_cfs(iorder)

      ENDDO

      Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow
!      Basin_cfs = Segment_cfs(Final_segment)
      Basin_cfs = Flow_out
      area_fac = Cfs_conv/Basin_area_inv
      Basin_stflow_out = Basin_cfs/area_fac
      Basin_cms = Basin_cfs*CFS2CMS_CONV

      area_fac = Cfs_conv/Basin_area_inv
      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac

      musroute_run = 0
      END FUNCTION musroute_run

