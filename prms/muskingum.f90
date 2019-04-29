!***********************************************************************
! Routes water between segments in the system using Muskingum routing
!
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
!   Solving for the outflow at day 2,O2; and knowing the inflow at day 1,
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
!   This module is based on the "musroute.f" module. It differs in three
!   basic ways:
!
!   1. This module uses an internal routing time step of one hour.
!      The old muskingum module ran on the same daily time step as
!      the rest of PRMS. The problem with this is that there is no
!      ability to distinguish where the flood wave (front of the flow
!      change) within the segment. For example, if there is a series
!      of 4 1-day long segments, a flood wave will make it to the bottom
!      of these in 1 day. If the same system is modeled as 1 4-day long
!      segment, it will take 4 days.
!
!   2. The X parameter has been removed as a specified input and is now computed. To
!      my knowledge, no modeler had ever set this to anything other than the default
!      value (0.2) anyway. Always using the default value can lead to problems
!      with the C coffecients which can result in mass balance problems or negative
!      flow values.
!
!      To solve this problem, I assume that the C coefficients must
!      always be between 0 and 1. By setting the C coefficients equal to 0 and 1,
!      various limits on the time step (ts), X, and K can be determined. There are
!      two of these limits which are of interest:
!
!      When C0 = 0:
!             ts
!        K = -----
!             2X
!
!      When C2 = 0:
!            ts
!       K = -----
!           2(1-X)
!
!      Determining a value of K half way between these two limits (by averaging)
!      and solving for X using the quadratic formula results in:
!
!            1-sqrt(1-(ts/K))
!       X = ------------------
!                  2
!
!       So when ts is fixed at one hour and K is fixed as the average (or expected)
!       travel time corresponding to the segment (for each segment in the stream
!       network), a value of X can be computed (for each segment in the stream
!       network) which will result in both conservation of mass and non-negative
!       flows. Another benefit is that only one input parameter (K) needs to be
!       input to the module. 
!
!   3. If the travel time of a segment is less than or equal to the routing
!      time step (one hour), then the outflow of the segment is set to the
!      value of the inflow.
!
!***********************************************************************
      MODULE PRMS_MUSKINGUM
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE :: Replace_flag, Table_flag, Nbankval
      INTEGER, SAVE, ALLOCATABLE :: Ts_i(:)
      INTEGER, SAVE, ALLOCATABLE :: Nratetable(:), Order(:)
      DOUBLE PRECISION, SAVE :: Flow_out
      REAL, SAVE, ALLOCATABLE :: Ts(:), C0(:), C1(:), C2(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Currinsum(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Upstream_inflow_ts(:), Outflow_ts(:), Inflow_ts(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_hruarea(:)
      CHARACTER(LEN=9), PARAMETER :: MODNAME = 'muskingum'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Streamflow Routing'
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Seg_upstream_inflow(:), Seg_lateral_inflow(:)
      REAL, SAVE, ALLOCATABLE :: Seginc_ssflow(:), Seginc_sroff(:)
      REAL, SAVE, ALLOCATABLE :: Seginc_gwflow(:), Seginc_swrad(:)
      REAL, SAVE, ALLOCATABLE :: Seg_outflow(:), Seg_inflow(:), Hru_outflow(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Tosegment(:), Hru_segment(:), Obsin_segment(:)
      REAL, SAVE, ALLOCATABLE :: K_coef(:), X_coef(:)
!      INTEGER, SAVE, ALLOCATABLE :: Segment_type(:)
!      INTEGER, SAVE :: Final_segment
      END MODULE PRMS_MUSKINGUM

!***********************************************************************
!     Main muskingum routine
!***********************************************************************
      INTEGER FUNCTION muskingum()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: muskingum_decl, muskingum_init, muskingum_run
!***********************************************************************
      muskingum = 0

      IF ( Process(:3)=='run' ) THEN
        muskingum  = muskingum_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        muskingum  = muskingum_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        muskingum  = muskingum_init()
      ENDIF

      END FUNCTION muskingum

!***********************************************************************
!     muskingum_decl - set up fixed routing parameters
!   Declared Parameters
!     tosegment, hru_segment, obsin_segment, k_coef, x_coef
!***********************************************************************
      INTEGER FUNCTION muskingum_decl()
      USE PRMS_MUSKINGUM
      USE PRMS_MODULE, ONLY: Model, Nhru, Nsegment, Version_muskingum, Muskingum_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam, declvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: i
!***********************************************************************
      muskingum_decl = 1

      Version_muskingum = '$Id: muskingum.f90 4751 2012-08-17 19:49:30Z rsregan $'
      Muskingum_nc = INDEX( Version_muskingum, 'Z' )
      i = INDEX ( Version_muskingum, '.f90' ) + 3
      IF ( declmodule(Version_muskingum(6:i), PROCNAME, Version_muskingum(i+2:Muskingum_nc))/=0 ) STOP

      ALLOCATE ( Obsin_segment(Nsegment) )
      IF ( declparam(MODNAME, 'obsin_segment', 'nsegment', 'integer', &
           '0', 'bounded', 'nobs', &
           'Index of measured streamflow station that replaces inflow to a segment', &
           'Index of measured streamflow station that replaces inflow to a segment', &
           'none')/=0 ) CALL read_error(1, 'obsin_segment')

!      ALLOCATE ( Segment_type(Nsegment) )
!      IF ( decl param(MODNAME, 'segment_type', 'nsegment','integer', &
!           '0', '0', '3', &
!           'Segment type', &
!           'Segment type (0=link; 1=lake; 2=diversion; 3=replace inflow)', &
!           'none')/=0 ) CALL read_error(1, 'segment_type')

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

      ALLOCATE ( Seg_outflow(Nsegment) )
      IF ( declvar(MODNAME, 'seg_outflow', 'nsegment', Nsegment, 'real', &
           'Streamflow leaving a segment', &
           'cfs', Seg_outflow)/=0 ) CALL read_error(3, 'seg_outflow')

      ALLOCATE ( Seg_inflow(Nsegment) )
      IF ( declvar(MODNAME, 'seg_inflow', 'nsegment', Nsegment, 'real', &
           'Total flow entering a segment', &
           'cfs', Seg_inflow)/=0 ) CALL read_error(3, 'seg_inflow')

      ALLOCATE ( Seg_lateral_inflow(Nsegment) )
      IF ( declvar(MODNAME, 'seg_lateral_inflow', 'nsegment', Nsegment, 'real', &
           'Lateral inflow entering a segment', &
           'cfs', Seg_lateral_inflow)/=0 ) CALL read_error(3, 'seg_lateral_inflow')

      ALLOCATE ( Seg_upstream_inflow(Nsegment) )
      IF ( declvar(MODNAME, 'seg_upstream_inflow', 'nsegment', Nsegment, 'real', &
           'Sum of inflow from upstream segments', &
           'cfs', Seg_upstream_inflow)/=0 ) CALL read_error(3, 'seg_upstream_inflow')

      ALLOCATE ( Hru_outflow(Nhru) )
      IF ( declvar(MODNAME, 'hru_outflow', 'nhru', Nhru, 'real', &
           'Total flow leaving each HRU', &
           'cfs', Hru_outflow)/=0 ) CALL read_error(3, 'hru_outflow')

      ALLOCATE ( C1(Nsegment), C2(Nsegment), C0(Nsegment), Ts(Nsegment) )
      ALLOCATE ( Currinsum(Nsegment), Ts_i(Nsegment) )
      ALLOCATE ( Segment_hruarea(Nsegment), Order(Nsegment) )
      ALLOCATE ( Upstream_inflow_ts(Nsegment), Outflow_ts(Nsegment), Inflow_ts(Nsegment) )

!     If we make it through all the declares then set the function to 0 (non error)
      muskingum_decl = 0
      END FUNCTION muskingum_decl

!***********************************************************************
!    muskingum_init - Initialize muskingum  module - get parameter values
!                determine order of computing segments - set to zero if
!                no errors are encountered
!***********************************************************************
      INTEGER FUNCTION muskingum_init()
      USE PRMS_MUSKINGUM
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Nsegmentp1, Print_debug
      USE PRMS_BASIN, ONLY: Hru_area, NEARZERO, Timestep
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
      INTRINSIC SQRT
! Local Variables
      INTEGER :: test, lval, i, j, badparameter, write_x_flag
      INTEGER, ALLOCATABLE :: x_off(:)
      REAL :: k, x, d, x_max

!***********************************************************************
      muskingum_init = 1

      ALLOCATE ( x_off(Nsegment) )
      x_off = 0

!     Get all paramaters that will be used in this module in the
!     initialize function including but not limited to those declared
!     in the declare module

      IF ( getparam(MODNAME, 'tosegment', Nsegment, 'integer', Tosegment)/=0 ) CALL read_error(2, 'tosegment')
      IF ( getparam(MODNAME, 'hru_segment', Nhru, 'integer', Hru_segment)/=0 ) CALL read_error(2, 'hru_segment')
      IF ( getparam(MODNAME, 'obsin_segment', Nsegment, 'integer', Obsin_segment)/=0 ) CALL read_error(2, 'obsin_segment')
      IF ( getparam(MODNAME, 'K_coef',  Nsegment, 'real', K_coef)/=0 ) CALL read_error(2, 'K_coef')
      IF ( getparam(MODNAME, 'x_coef',  Nsegment, 'real', X_coef)/=0 ) CALL read_error(2, 'X_coef')
!      IF ( get param(MODNAME, 'segment_type', Nsegment, 'integer', Segment_type)/=0 ) CALL read_error(2, 'segment_type')
!      IF ( get param(MODNAME, 'final_segment', 1, 'integer', Final_segment)/=0 ) CALL read_error(1, 'final_segment')
!      IF ( Final_segment==0 ) Final_segment = Nsegment

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
        Seg_inflow = 0.0
        Seg_outflow = 0.0
        Seg_lateral_inflow = 0.0
        Seg_upstream_inflow = 0.0

        Inflow_ts = 0.0D0
        Outflow_ts = 0.0D0
        Upstream_inflow_ts = 0.0D0

        Seginc_gwflow = 0.0
        Seginc_ssflow = 0.0
        Seginc_sroff = 0.0
        Seginc_swrad = 0.0
        Hru_outflow = 0.0
      ENDIF

      write_x_flag = 0
!
!     Compute the three constants in the Muskingum routing equation based
!     on the values of K_coef and a routing period of 1 hour. See the notes
!     at the top of this file.
!
      C0 = 0.0
      C1 = 0.0
      C2 = 0.0
!make sure K>0
!      Ts = 1.0
      DO i = 1, Nsegment
        k = K_coef(i)
        x = x_coef(i)
         
! check the values of k and x to make sure that Muskingum routing is stable
        IF ( k<=0.0 .AND. write_x_flag==0 ) THEN
          PRINT *, 'ERROR in muskingum: segment ', i, ' must have K_coef value greater than 0.0'
          PRINT *, '***FIX THIS in your Parameter File***'
        ENDIF
         
        IF ( x>0.5 .AND. write_x_flag==0 ) THEN
          PRINT *, 'ERROR in muskingum: segment ', i, ' must have x_coef value less than 0.5'
          PRINT *, '***FIX THIS in your Parameter File***'
        ENDIF
         
        IF ( k<1.0 ) THEN
          Ts(i) = 0.0
          Ts_i(i) = -1
          IF ( Print_debug>-1 ) THEN
            IF ( write_x_flag==0 ) THEN
              PRINT ('(A, I4, A)'), 'muskingum: segment ', i, ' has K_coef (travel time) value less'// &
                                    ' than the minimum internal routing time (one hour)'
              PRINT *, 'Outflow for this segment is set to the inflow and there will be no lag or attenuation'
            ENDIF
          ENDIF
            
        ELSEIF ( k<2.0 ) THEN
          Ts(i) = 1.0
          Ts_i(i) = 1
            
        ELSEIF ( k<3.0 ) THEN
          Ts(i) = 2.0
          Ts_i(i) = 2
            
        ELSEIF ( k<4.0 ) THEN
          Ts(i) = 3.0
          Ts_i(i) = 3
            
        ELSEIF ( k<6.0 ) THEN
          Ts(i) = 4.0
          Ts_i(i) = 4
            
        ELSEIF ( k<8.0 ) THEN
          Ts(i) = 6.0
          Ts_i(i) = 6
            
        ELSEIF ( k<12.0 ) THEN
          Ts(i) = 8.0
          Ts_i(i) = 8
            
        ELSEIF ( k<24.0 ) THEN
          Ts(i) = 12.0
          Ts_i(i) = 12
            
        ELSE
          Ts(i) = 24.0
          Ts_i(i) = 24
            
        ENDIF
         
!  x must be <= t/(2K) the C coefficents will be negative. Check for this for all segments
!  with Ts >= minimum Ts (1 hour).
        IF ( Ts(i)>0.1 ) THEN
          x_max = Ts(i) / (2.0 * k)
            
          IF ( x>x_max ) THEN
            IF ( write_x_flag==0 ) THEN
              PRINT *, 'ERROR: segment ', i, ' x_coef value is TOO LARGE; a maximum value of', x_max, ' is suggested'
              PRINT *, '***FIX THIS in your Parameter File***'
            ELSE
              PRINT ('(f6.4)'), x_max
            ENDIF
          ELSE
            IF ( write_x_flag == 1 ) PRINT ('(f6.4)'), x
          ENDIF
        ELSE
          IF ( write_x_flag==1 ) PRINT ('(f6.4)'), x
        ENDIF

         d = k - (k * x) + (0.5 * Ts(i))
         C0(i) = (-(k * x) + (0.5 * Ts(i))) / d
         C1(i) = ((k * x) + (0.5 * Ts(i))) / d 
         C2(i) = (k - (k * x) - (0.5 * Ts(i))) / d

      ENDDO
!      PRINT *, 'done with muskingum'

      muskingum_init = 0
      END FUNCTION muskingum_init

!***********************************************************************
!     muskingum_run - Computes summary values
!***********************************************************************
      INTEGER FUNCTION muskingum_run()
      USE PRMS_MUSKINGUM
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
      INTRINSIC DBLE, MOD
! Local Variables
      INTEGER :: i, j, jj, iorder, toseg, imod, tspd, ts_foo
      DOUBLE PRECISION :: area_fac, tocfs, pastin, pastout, currin, currout
!***********************************************************************
!     Get all variable declared and computed by other modules
!     that will be used in this module
!
!     SET yesterdays inflows and outflows into temp (past arrays)
!     values may be 0.0 as intial, > 0.0 for runtime and dynamic 
!     initial condtions. Then set outlfow and inflow for this time
!     step to 0.0
!
!     upstream_inflow and outflow will vary by hour
!     lateral_inflow and everything else will vary by day
!
!     Compute surface runoff, ssflow, and gwflow going to each segment
!     This is todays "Seg_inflow" before additional water is routed to
!     a new (if any is routed)
!
!     For each HRU if the surface sunoff for this HRU goes to the 
!     segment being evaluated (segment i) THEN add it on
!     (0.042014 converts runoff in inches times area in acres for one day
!     to cfs, MCM)
!
!     Do these calculations once for the current day, before the hourly
!     routing starts.
!
      Seginc_gwflow = 0.0
      Seginc_ssflow = 0.0
      Seginc_sroff = 0.0
      Seginc_swrad = 0.0
      Seg_lateral_inflow = 0.0
      DO i = 1, Nsegment
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)
          tocfs = Hru_area(j)*Cfs_conv
          Hru_outflow(j) = (Sroff(j) + Ssres_flow(j) + Gwres_flow(j))*tocfs
          IF ( Hru_segment(j)==i ) THEN
            Seg_lateral_inflow(i) = Seg_lateral_inflow(i) + Hru_outflow(j)
            Seginc_sroff(i) = Seginc_sroff(i) + Sroff(j)*tocfs
            Seginc_ssflow(i) = Seginc_ssflow(i) + Ssres_flow(j)*tocfs
            Seginc_gwflow(i) = Seginc_gwflow(i) + Gwres_flow(j)*tocfs
            Seginc_swrad(i) = Seginc_swrad(i) + Swrad(j)*Hru_area(j)
          ENDIF
        ENDDO
      ENDDO

! Divide solar radiation by sum of HRU area to get avarage
      DO i = 1, Nsegment
        IF ( Segment_hruarea(i)>DNEARZERO ) Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
      ENDDO

! If there are no HRUs associated with a segment, then figure out some
! other way to get the solar radiation, the following is not great
      DO i = 1, Nsegment
        iorder = Order(i)
        IF ( Segment_hruarea(iorder)<DNEARZERO ) THEN
          IF ( Tosegment(iorder)>0 .AND. Tosegment(iorder)<Nsegmentp1 ) THEN
            Seginc_swrad(iorder) = Seginc_swrad(Tosegment(iorder))
          ELSEIF ( iorder>1 ) THEN
            Seginc_swrad(iorder) = Seginc_swrad(iorder-1)
          ELSE
            Seginc_swrad(iorder) = Seginc_swrad(iorder+1)
          ENDIF
        ENDIF
      ENDDO
!
!       Out2   =      In2*C0    +        In1*C1    +          Out1*C2
!     Seg_outflow = Seg_inflow*Czero + Pastinflow*Cone + Pastoutflow*Ctwo
!       C0, C1, and C2: initialized in the "init" part of this module
!
      Hru_outflow = 0.0
      Seg_inflow = 0.0
      Seg_outflow = 0.0
      Seg_upstream_inflow = 0.0
!      Upstream_inflow_ts = 0.0D0

! 24 hourly timesteps per day
      Currinsum = 0.0D0
      DO j = 1, 24

        DO i = 1, Nsegment
          iorder = Order(i)

! Add current timestep's flow rate to sum the upstream flow rates.
! This can be thought of as a volume because it is a volumetric rate
! (cubic feet per second) over a time step of an hour. Down below when
! this value is used, it will be divided by the number of hours in the
! segment's simulation time step, giving the mean flow rate over that
! period of time.
!
          toseg = Tosegment(iorder)
          IF ( toseg<=Nsegment ) Currinsum(toseg) = Currinsum(toseg) + Outflow_ts(iorder)
        ENDDO
 
        DO i = 1, Nsegment
          iorder = Order(i)

!         Check to see if this segment is to be routed on this time step
          ts_foo = Ts_i(iorder)
          tspd = Ts_i(iorder)
          imod = MOD( j, tspd )
          IF ( imod==0 ) THEN

! pastin is equal to the Inflow_ts on the previous routed timestep
            pastin = Inflow_ts(iorder)
! pastout is equal to the Inflow_ts on the previous routed timestep
            pastout = Outflow_ts(iorder)

! current inflow to the segment is the time weighted average of the outflow
! of the upstream segments plus the lateral HRU inflow
! not sure that this replacement flow belongs here
            IF ( Obsin_segment(iorder)>0 ) Currinsum(iorder) = Streamflow_cfs(Obsin_segment(i))
            Inflow_ts(iorder) = (Currinsum(iorder) / Ts(iorder)) + Seg_lateral_inflow(iorder)

            currin = Inflow_ts(iorder)

! Compute routed streamflow
            IF ( Ts_i(i)>0 ) THEN
! Muskingum routing equation
              Outflow_ts(iorder) = currin*C0(iorder) + pastin*C1(iorder) + pastout*C2(iorder)
            ELSE
! If travel time (K_coef paremter) is less than or equal to
! time step (one hour), then the outflow is equal to the inflow
! Outflow_ts is the value from last hour
              Outflow_ts(iorder) = currin
            ENDIF
            currout = Outflow_ts(iorder)

! Seg_outflow (the mean daily flow rate for each segment) will be the
! average of the hourly values.
            Seg_outflow(iorder) = Seg_outflow(iorder) + (currout * Ts(iorder))
            Seg_inflow(iorder) = Seg_inflow(iorder) + (currin * Ts(iorder))
            Seg_upstream_inflow(iorder) = Seg_upstream_inflow(iorder) + Currinsum(iorder)
               
! because the upstream inflow from streams is used, reset it to zero so new average
! can be computed next routing timestep.
            Currinsum(iorder) = 0.0D0
          ENDIF
        ENDDO ! segment
      ENDDO  ! timestep

      Flow_out = 0.0D0
      DO i = 1, Nsegment
        Seg_outflow(i) = Seg_outflow(i) / 24.0
        Seg_inflow(i) = Seg_inflow(i) / 24.0
        Seg_upstream_inflow(i) = Seg_upstream_inflow(i) / 24.0
      
! Flow_out is the total flow out of the basin, which allows for multiple outlets
        IF ( Tosegment(i)==Nsegmentp1 ) Flow_out = Flow_out + DBLE( Seg_outflow(i) )
      ENDDO

      area_fac = Cfs_conv/Basin_area_inv
      Basin_stflow_in = Basin_sroff + Basin_gwflow + Basin_ssflow
!      Basin_cfs = Seg_outflow(Final_segment)
      Basin_cfs = Flow_out
      Basin_stflow_out = Basin_cfs / area_fac
      Basin_cms = Basin_cfs*CFS2CMS_CONV

      Basin_sroff_cfs = Basin_sroff*area_fac
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_gwflow_cfs = Basin_gwflow*area_fac

      muskingum_run = 0
      END FUNCTION muskingum_run
