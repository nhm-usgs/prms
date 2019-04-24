!***********************************************************************
! Determines computational order of the HRUs and ground-water
! reservoirs for routing flow downslope
!***********************************************************************
      MODULE PRMS_CASCADE
      IMPLICIT NONE
!   Local Variables
      INTEGER, PARAMETER :: MSGUNT = 189
      INTEGER, SAVE :: Iorder, Igworder, Ndown
      INTEGER, SAVE :: Iseg_out, Nwtrbdy, Iwtrbdyorder
      INTEGER, SAVE :: Nhrup1, Nsegments, Gwcasc_flg
      ! Flag to indicate if an HRU is connected to a far-field stream
      ! (0=no; 1=yes).
      ! Flag to indicate if a GWR is connected to a far-field stream
      ! (0=no; 1=yes).
      INTEGER, SAVE :: Outflow_flg, Outflow_gwrflg
!   Computed Variables
      INTEGER, SAVE, ALLOCATABLE :: Hru_down(:, :), Gwr_down(:, :)
      INTEGER, SAVE, ALLOCATABLE :: Wtrbdy_down(:)
      INTEGER, SAVE, ALLOCATABLE :: Ncascade_hru(:), Ncascade_gwr(:)
      INTEGER, SAVE, ALLOCATABLE :: Wtrbdy_route_order(:)
      REAL, SAVE, ALLOCATABLE :: Cascade_area(:, :)
      REAL, SAVE, ALLOCATABLE :: Hru_down_fracwt(:, :)
      REAL, SAVE, ALLOCATABLE :: Cascade_gwr_area(:, :)
      REAL, SAVE, ALLOCATABLE :: Gwr_down_frac(:, :)
      REAL, SAVE, ALLOCATABLE :: Hru_down_frac(:, :) 
      INTEGER, SAVE, ALLOCATABLE :: Ncascade_wtrbdy(:)
!     INTEGER, SAVE, ALLOCATABLE :: Strmseg_hru_up_id(:)
      INTEGER, SAVE, ALLOCATABLE :: Strm_lake_down(:)
!     REAL, SAVE, ALLOCATABLE :: Gwr_down_fracwt(:, :)
! hru_down_frac: Fraction of HRU area used to compute flow routed
!               to a down slope HRU or stream segment.
! hru_down_fracwt: HRU area fraction, area weighted by down slope HRU
!                 area, used to compute flow routed to a down slope
!                 HRU or stream segment.
! gwr_down_fracwt: GWR area fraction, area weighted by down slope GWR
!                 area, used to compute flow routed to a down slope
!                 GWR or stream segment.
! gwr_down_frac: Fraction of GWR area used to compute flow routed
!               to a down slope cascade area or stream segment from
!               each cascade area of an GWR.
! cascade_area: Cascade area within an HRU.
! cascade_gwr_area: Cascade area within an GWR.
! hru_down: Indices of the down slope HRUs or stream segments to
!           which the cascade area routes flow.
! gwr_down: Indices of the down slope GWRs to which the cascade
!           area routes flow.
! wtrbdy_netwrk_down: Indices of the down segments and lakes which
!                     cascade flow in a routing order.
!   Declared Parameters
      INTEGER, SAVE :: Cascade_flg, Circle_switch
      REAL, SAVE :: Cascade_tol
      INTEGER, SAVE, ALLOCATABLE :: Hru_up_id(:), Hru_strmseg_down_id(:)
      INTEGER, SAVE, ALLOCATABLE :: Hru_down_id(:)
      INTEGER, SAVE, ALLOCATABLE :: Gw_up_id(:), Gw_strmseg_down_id(:)
      INTEGER, SAVE, ALLOCATABLE :: Gw_down_id(:)
      INTEGER, SAVE, ALLOCATABLE :: Strmseg_down_id(:), Seg_res_id(:)
      INTEGER, SAVE, ALLOCATABLE :: Lake_hru_down_id(:)
      REAL, SAVE, ALLOCATABLE:: Hru_pct_up(:), Gw_pct_up(:)
      END MODULE PRMS_CASCADE

!***********************************************************************
!     Main cascade routine
!***********************************************************************
      INTEGER FUNCTION cascade()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: cascdecl, cascinit, cascclean
!***********************************************************************
      cascade = 0

      IF ( Process(:4)=='decl' ) THEN
        cascade = cascdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        cascade = cascinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        cascade = cascclean()
      ENDIF

      END FUNCTION cascade

!***********************************************************************
!     cascdecl - set up parameters for cascading flow
!   Declared Parameters
!     hru_up_id, hru_down_id, hru_pct_up, hru_strmseg_down_id
!     gw_up_id, gw_down_id, gw_pct_up, gw_strmseg_down_id
!     hru_area, cascade_tol, cascade_flg, circle_switch
!     strmseg_down_id, sfres_hru, Lake_hru_down_id, seg_res_id
!***********************************************************************
      INTEGER FUNCTION cascdecl()
      USE PRMS_CASCADE
      USE PRMS_MODULE, ONLY: Model, Lake_flg, Ncascade, Ncascdgw, Nhru,
     +    Ngw, Nsfres, Nsegment, Print_debug, Cascadegw_flag,
     +    Cascade_flag, Version_cascade, Cascade_nc
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declmodule, declparam
      EXTERNAL read_error
!***********************************************************************
      cascdecl = 1

      Version_cascade =
     +'$Id: cascade.f 3793 2011-10-25 16:34:51Z rsregan $'
      IF ( Print_debug>-1 ) THEN
        Cascade_nc = INDEX( Version_cascade, ' $' ) + 1
        IF ( declmodule(Version_cascade(:Cascade_nc))/=0 ) STOP
      ENDIF

      Nhrup1 = Nhru + 1

      Gwcasc_flg = 0
      IF ( Cascadegw_flag==1 .AND. Model==1 ) Gwcasc_flg = 1

      IF ( Cascade_flag==1 .OR. Model==99 )
     +     ALLOCATE ( Ncascade_hru(Nhru) )

      IF ( Gwcasc_flg==1 .OR. Model==99 ) ALLOCATE ( Ncascade_gwr(Ngw) )

! declare lake cascade parameters and variables
      IF ( (Cascade_flag==1.AND.Lake_flg==1) .OR. Model==99 ) THEN
        Nwtrbdy = Nsegment + Nsfres

        ALLOCATE (Strmseg_down_id(Nsegment))
        IF ( declparam('cascade', 'strmseg_down_id', 'nsegment',
     +       'integer', '0', 'bounded', 'nsegment',
     +       'Index of down stream segment',
     +       'Index number of the down stream segment to which this'//
     +       ' stream segment flows',
     +       'none')/=0 ) CALL read_error(1, 'strmseg_down_id')

        ALLOCATE (Lake_hru_down_id(Nsegment))
        IF ( declparam('cascade', 'lake_hru_down_id',
     +       'nsegment', 'integer',
     +       '0', 'bounded', 'nhru',
     +       'Index of down stream lake HRU in the water body network',
     +       'Index number of the down stream lake HRU to which this'//
     +       ' stream segment flows',
     +       'none')/=0 ) CALL read_error(1, 'lake_hru_down_id')

! Index number of the upstream HRU to which this stream segment flows
!       ALLOCATE (Strmseg_hru_up_id(Nwtrbdy))
!       Strmseg_hru_up_id = 0

        ! Routing order for Water body network
        ALLOCATE (Wtrbdy_route_order(Nwtrbdy))

        ! Number of cascades from any water body
        ALLOCATE (Ncascade_wtrbdy(Nwtrbdy))
        Ncascade_wtrbdy = 0

        ! Stream segments flowing from any lake
        ALLOCATE (Wtrbdy_down(Nwtrbdy))
        Wtrbdy_down = 0

        ALLOCATE (Seg_res_id(Nsegment))
        IF ( declparam('cascade', 'seg_res_id', 'nsegment', 'integer',
     +       '0', 'bounded', 'nsfres',
     +       'Lake index number for a stream segment within a lake',
     +       'Lake index number for a stream segment within a lake'//
     +       ' (0=segment not in lake; >0=lake index number)',
     +       'none')/=0 ) CALL read_error(1, 'seg_res_id')
        ENDIF

! declare HRU cascade parameters
      IF ( Cascade_flag==1 .OR. Model==99 ) THEN
        ALLOCATE (Hru_up_id(Ncascade))
        IF ( declparam('cascade', 'hru_up_id', 'ncascade', 'integer',
     +       '1', 'bounded', 'nhru',
     +       'Index of HRU containing cascade area',
     +       'Index of HRU containing cascade area',
     +       'none')/=0 ) CALL read_error(1, 'hru_up_id')

        ALLOCATE (Hru_strmseg_down_id(Ncascade))
        IF ( declparam('cascade', 'hru_strmseg_down_id', 'ncascade',
     +       'integer', '0', 'bounded', 'nsegment',
     +       'Stream segment index that cascade area contributes flow',
     +       'Index number of the stream segment that cascade area'//
     +       ' contributes flow',
     +       'none')/=0 ) CALL read_error(1, 'hru_strmseg_down_id')

        ALLOCATE (Hru_down_id(Ncascade))
        IF ( declparam('cascade', 'hru_down_id', 'ncascade', 'integer',
     +       '0', 'bounded', 'nhru',
     +       'HRU index of down slope HRU',
     +       'Index number of the down slope HRU to which the upslope'//
     +       ' HRU contributes flow',
     +       'none')/=0 ) CALL read_error(1, 'hru_down_id')

        ALLOCATE (Hru_pct_up(Ncascade))
        IF ( declparam('cascade', 'hru_pct_up', 'ncascade', 'real',
     +       '1.0', '0.0', '1.0',
     +       'Fraction of HRU area associated with cascade area.',
     +       'Fraction of HRU area used to compute flow contributed'//
     +       ' to a down slope HRU or stream segment for cascade area.',
     +       'decimal fraction')/=0 ) CALL read_error(1, 'hru_pct_up')
      ENDIF

      IF ( declparam('cascade', 'cascade_tol', 'one', 'real',
     +     '5.0', '0.0', '99.0',
     +     'Cascade area below which a cascade link is ignored.',
     +     'Cascade area below which a cascade link is ignored.',
     +     'acres')/=0 ) CALL read_error(1, 'cascade_tol')

      IF ( declparam('cascade', 'cascade_flg', 'one', 'integer',
     +     '0', '0', '1',
     +     'Flag to indicate cascade type (0=allow many to many;'//
     +     ' 1=force one to one)',
     +     'Flag to indicate cascade type (0=allow many to many;'//
     +     ' 1=force one to one)',
     +     'none')/=0 ) CALL read_error(1, 'cascade_flg')

      IF ( declparam('cascade', 'circle_switch', 'one', 'integer',
     +     '1', '0', '1',
     +     'Switch to check for circles',
     +     'Switch to check for circles (0=no check; 1=check)',
     +     'none')/=0 ) CALL read_error(1, 'circle_switch')

      IF ( Gwcasc_flg==1 .OR. Model==99 ) THEN
! declare GWR cascade parameters
        ALLOCATE (Gw_up_id(Ncascdgw))
        IF ( declparam('cascade', 'gw_up_id', 'ncascdgw', 'integer',
     +       '1', 'bounded', 'ngw',
     +       'Index of GWR containing cascade area',
     +       'Index of GWR containing cascade area',
     +       'none')/=0 ) CALL read_error(1, 'gw_up_id')

        ALLOCATE (Gw_strmseg_down_id(Ncascdgw))
        IF ( declparam('cascade', 'gw_strmseg_down_id', 'ncascdgw',
     +       'integer', '0', 'bounded', 'nsegment',
     +       'Stream segment index that cascade area contributes flow',
     +       'StrIndex number of the stream segment that cascade area'//
     +       ' contributes flow',
     +       'none')/=0 ) CALL read_error(1, 'gw_strmseg_down_id')

        ALLOCATE (Gw_down_id(Ncascdgw))
        IF ( declparam('cascade', 'gw_down_id', 'ncascdgw', 'integer',
     +       '0', 'bounded', 'ngw',
     +       'GWR index of down slope GWR',
     +       'Index number of the down slope GWR to which the upslope'//
     +       ' GWR contributes flow',
     +       'none')/=0 ) CALL read_error(1, 'gw_down_id')

        ALLOCATE (Gw_pct_up(Ncascdgw))
        IF ( declparam('cascade', 'gw_pct_up', 'ncascdgw', 'real',
     +       '1.0', '0.0', '1.0',
     +       'Fraction of GWR area associated with cascade area',
     +       'Fraction of GWR area used to compute flow contributed'//
     +       ' to a down slope GWR or stream segment for cascade area',
     +       'decimal fraction')/=0 ) CALL read_error(1, 'gw_pct_up')

      ENDIF

      cascdecl = 0
      END FUNCTION cascdecl

!***********************************************************************
!     cascinit - Initialize cascade module - get parameter values,
!***********************************************************************
      INTEGER FUNCTION cascinit()
      USE PRMS_CASCADE
      USE PRMS_MODULE, ONLY: Ngw, Nsegment, Lake_flg, Cascade_flag,
     +    Print_debug
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order,
     +    Gwr_route_order, Gwr_type, Active_gwrs
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
! Functions
      EXTERNAL init_cascade, initgw_cascade, initwtrbdy_cascade
! Local Variables
      INTEGER :: i, j, k, ii
!***********************************************************************
      cascinit = 1

      IF ( getparam('cascade', 'cascade_tol', 1, 'real', Cascade_tol)
     +     /=0 ) CALL read_error(2, 'cascade_tol')

      IF ( getparam('cascade', 'cascade_flg', 1, 'integer', Cascade_flg)
     +     /=0 ) CALL read_error(2, 'cascade_flg')

      IF ( getparam('cascade', 'circle_switch', 1, 'integer',
     +     Circle_switch)/=0 ) CALL read_error(2, 'circle_switch')

      IF ( Print_debug==13 ) OPEN ( MSGUNT, FILE='cascade.msgs' )

      cascinit = 0
      IF ( Cascade_flag==1 ) THEN
        CALL init_cascade(cascinit)
        IF ( cascinit/=0 ) STOP
      ENDIF

      Gwr_type = 1
      IF ( Gwcasc_flg==1 ) THEN
        CALL initgw_cascade(cascinit)
        IF ( cascinit/=0 ) STOP
      ENDIF

      IF ( Cascade_flag==1.AND.Lake_flg==1 ) THEN
        CALL initwtrbdy_cascade(cascinit)
        IF ( cascinit/=0 ) STOP
      ENDIF

      IF ( Print_debug==13 ) THEN
        WRITE ( MSGUNT, 9001 ) 
        k = 0
        DO ii = 1, Active_hrus
          i = Hru_route_order(ii)
          DO j = 1, Ncascade_hru(i)
            k = k + 1
            WRITE ( MSGUNT, * ) k, i, Hru_down(j, i),
     +                          Hru_down_frac(j, i)*100.0
          ENDDO
        ENDDO
        IF ( Gwcasc_flg==1 ) THEN
          WRITE ( MSGUNT, 9002 ) 
          k = 0
          DO ii = 1, Active_gwrs
            i = Gwr_route_order(ii)
            DO j = 1, Ncascade_gwr(i)
              k = k + 1
              WRITE ( MSGUNT, * ) k, i, Gwr_down(j, i),
     +                            Gwr_down_frac(j, i)*100.0
            ENDDO
          ENDDO
        ENDIF
        CLOSE ( MSGUNT )
      ENDIF

 9001 FORMAT (//, 18X, 'UP HRU', 4X, 'DOWN HRU    FRACTION')
 9002 FORMAT (//, 18X, 'UP GWR', 4X, 'DOWN GWR    FRACTION')

      cascinit = 0
      END FUNCTION cascinit

!***********************************************************************
!     cascclean - deallocation arrays
!***********************************************************************
      INTEGER FUNCTION cascclean()
      USE PRMS_CASCADE
      USE PRMS_MODULE, ONLY: Cascade_flag
      IMPLICIT NONE
!***********************************************************************
      IF ( Cascade_flag>0 ) THEN
        DEALLOCATE ( Hru_down, Hru_down_frac, Hru_down_fracwt )
        DEALLOCATE ( Cascade_area)
      ENDIF
      IF ( Gwcasc_flg==1 ) THEN
        DEALLOCATE ( Gwr_down, Gwr_down_frac, Cascade_gwr_area )
!       DEALLOCATE ( Gwr_down_fracwt )
      ENDIF

      cascclean = 0
      END FUNCTION cascclean

!***********************************************************************
! Initialize cascading flow variables
!***********************************************************************
      SUBROUTINE init_cascade(Iret)
      USE PRMS_CASCADE
      USE PRMS_MODULE, ONLY: Ncascade, Ncascdgw, Lake_flg, Nhru, Ngw,
     +    Nsegment, Nsegmentp1, Nsfres, Print_debug, Cascade_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type,
     +    Hru_area, Sfres_hru
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL order_hrus, read_error
      INTRINSIC ABS
! Arguments
      INTEGER, INTENT(OUT) :: Iret
! Local Variables
      INTEGER :: i, j, k, ii, kk, dnhru, kup, jdn, istrm, ierr
      REAL, ALLOCATABLE :: hru_frac(:)
      REAL :: fracchk, carea, frac
!***********************************************************************
      Iret = 1

! Cascade parameters
      IF ( getparam('cascade', 'hru_up_id', Ncascade, 'integer',
     +     Hru_up_id)/=0 ) CALL read_error(2, 'hru_up_id')

      IF ( getparam('cascade', 'hru_strmseg_down_id', Ncascade,
     +     'integer', Hru_strmseg_down_id)/=0 )
     +     CALL read_error(2, 'hru_strmseg_down_id')

      IF ( getparam('cascade', 'hru_down_id', Ncascade, 'integer',
     +     Hru_down_id)/=0 ) CALL read_error(2, 'hru_down_id')

      IF ( getparam('cascade', 'hru_pct_up', Ncascade, 'real',
     +     Hru_pct_up)/=0 ) CALL read_error(2, 'hru_pct_up')

      Iret = 0

      Ncascade_hru = 0
      Ndown = 1
      ierr = 0
      DO i = 1, Ncascade
        k = Hru_up_id(i)
        IF ( k>0 ) THEN
          IF ( k>Nhru ) THEN
            PRINT *, '***ERROR***'
            PRINT *, 'hru_up_id > nhru for cascade:', i, ' up_id:', k
            ierr = 1
            CYCLE
          ENDIF
          jdn = Hru_down_id(i)
          IF ( jdn>Nhru ) THEN
            PRINT *, '***ERROR***'
            PRINT *, 'hru_down_id > nhru for cascade:', i, ' down_id:',
     +               jdn
            ierr = 1
            CYCLE
          ENDIF
          Ncascade_hru(k) = Ncascade_hru(k) + 1
          IF ( Ncascade_hru(k)>Ndown ) Ndown = Ncascade_hru(k)
        ENDIF
      ENDDO
 
      IF ( Gwcasc_flg==1 ) THEN
        IF ( getparam('cascade', 'gw_up_id', Ncascdgw, 'integer',
     +       Gw_up_id)/=0 ) CALL read_error(2, 'gw_up_id')
        IF ( getparam('cascade', 'gw_down_id', Ncascdgw, 'integer',
     +       Gw_down_id)/=0 ) CALL read_error(2, 'gw_down_id')
        IF ( getparam('cascade', 'gw_pct_up', Ncascdgw, 'real',
     +       Gw_pct_up)/=0 ) CALL read_error(2, 'gw_pct_up')
        Ncascade_gwr = 0
        DO i = 1, Ncascdgw
          k = Gw_up_id(i)
          IF ( k>0 ) THEN
            IF ( k>Ngw ) THEN
              PRINT *, '***ERROR***'
              PRINT *, 'gw_up_id > ngw for cascade:', i, ' up_id:', k
              ierr = 1
              CYCLE
            ENDIF
            jdn = Gw_down_id(i)
            IF ( jdn>Ngw ) THEN
              PRINT *, '***ERROR***'
              PRINT *, 'gw_down_id > ngw for cascade:', i, ' down_id:',
     +                 jdn
              ierr = 1
              CYCLE
            ENDIF
            Ncascade_gwr(k) = Ncascade_gwr(k) + 1
            IF ( Ncascade_gwr(k)>Ndown ) Ndown = Ncascade_gwr(k)
          ENDIF
        ENDDO
      ENDIF
      IF ( ierr==1 ) STOP

      IF ( Ndown>15 .AND. Print_debug==13 )
     +     WRITE ( MSGUNT, * ) 'possible ndown issue', Ndown

! allocate HRU variables
      ALLOCATE ( Hru_down(Ndown, Nhru), Cascade_area(Ndown, Nhru) )
      ALLOCATE ( Hru_down_frac(Ndown, Nhru) )
      ALLOCATE ( Hru_down_fracwt(Ndown, Nhru) )
      ALLOCATE ( hru_frac(Nhru) )
      Hru_down = 0
      Hru_down_frac = 0.0
      Hru_down_fracwt = 0.0
      Cascade_area = 0.0
      Ncascade_hru = 0
      hru_frac = 0.0

      ! determine number of water bodies (lakes + stream segments)
      ! but ignoring stream segments within lakes
      IF ( Cascade_flag==1.AND.Lake_flg==1 ) THEN
        IF ( getparam('cascade', 'seg_res_id', Nsegment, 'integer',
     +       Seg_res_id)/=0 ) CALL read_error(2, 'seg_res_id')
        IF ( getparam('cascade', 'strmseg_down_id', Nsegment, 'integer',
     +       Strmseg_down_id)/=0 )
     +       CALL read_error(2, 'strmseg_down_id')
        IF ( getparam('cascade', 'lake_hru_down_id', Nsegment,
     +       'integer', Lake_hru_down_id)/=0 )
     +       CALL read_error(2, 'lake_hru_down_id')

        ALLOCATE (Strm_lake_down(Nwtrbdy))
        Strm_lake_down = 0
        Nsegments = Nsegment
        DO i = 1, Nsegment
          ! ignore stream segments in lakes
          IF ( Seg_res_id(i)/=0 ) THEN
            Strm_lake_down(i) = Nhrup1
            Wtrbdy_down(i) = -Nhrup1
            Nsegments = Nsegments - 1
          ELSE
            IF ( Lake_hru_down_id(i)>Nhru ) THEN
              PRINT *, 'ERROR, lake_hru_down_id > nhru, segment:', i
              STOP
            ENDIF
            Strm_lake_down(i) = Lake_hru_down_id(i)
            Wtrbdy_down(i) = Strmseg_down_id(i)
          ENDIF
        ENDDO

      ENDIF

      Outflow_flg = 0
      Outflow_gwrflg = 0

      DO i = 1, Ncascade
        kup = Hru_up_id(i)
        IF ( kup<1 ) THEN
          PRINT *, 'Cascade ignored as hru_up_id<1, cascade:', i, kup
          CYCLE
        ENDIF
        jdn = Hru_down_id(i)
        frac = Hru_pct_up(i)
        IF ( frac>0.9998 ) frac = 1.0
        istrm = Hru_strmseg_down_id(i)

        IF ( Cascade_flag==1.AND.Lake_flg==1 ) THEN
          IF ( Hru_type(kup)==2 ) THEN
            DO k = 1, Nsfres
              IF ( kup==Sfres_hru(k) ) THEN
                kk = Nsegment + k
                ! what if closed
                Ncascade_wtrbdy(kk) = Ncascade_wtrbdy(kk) + 1
                IF ( Ncascade_wtrbdy(kk)>1 ) THEN
                  PRINT *, 'ERROR, water body outlets > 1'
                  RETURN
                ENDIF
                Wtrbdy_down(kk) = istrm
!               Strmseg_hru_up_id(istrm) = kup
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDIF

        IF ( frac<0.00001 ) THEN
          IF ( Print_debug==13 )
     +         WRITE (MSGUNT, 9004) 'Cascade ignored as hru_pct_up=0.0',
     +                              i, kup, jdn, frac, istrm
        ELSEIF ( istrm>Nsegment ) THEN
          IF ( Print_debug==13 )
     +       WRITE (MSGUNT, 9004) 'Cascade ignored as segment>nsegment',
     +                            i, kup, jdn, frac, istrm
        ELSEIF ( kup<1 .AND. jdn==0 ) THEN
          IF ( Print_debug==13 )
     +    WRITE (MSGUNT, 9004) 'Cascade ignored as up & down HRU = 0',
     +                         i, kup, jdn, frac, istrm
        ELSEIF ( istrm==0 .AND. jdn==0 ) THEN
          IF ( Print_debug==13 )
     +         WRITE ( MSGUNT, 9004 )
     +                 'Cascade ignored as down HRU & segment = 0',
     +                 i, kup, jdn, frac, istrm
        ELSEIF ( Hru_type(kup).EQ.0 ) THEN
          IF ( Print_debug==13 )
     +    WRITE (MSGUNT, 9004) 'Cascade ignored as up HRU is inactive',
     +                         i, kup, jdn, frac, istrm
        ELSEIF ( Hru_type(kup).EQ.3 ) THEN
          IF ( Print_debug==13 )
     +    WRITE (MSGUNT, 9004) 'Cascade ignored as up HRU is a swale',
     +                         i, kup, jdn, frac, istrm
        ELSEIF ( Hru_type(kup).EQ.2 .AND. istrm<1 ) THEN
          IF ( Print_debug==13 )
     +    WRITE ( MSGUNT, 9004 )
     +          'Cascade ignored as lake HRU cannot cascade to an HRU',
     +          i, kup, jdn, frac, istrm
        ELSE
          ! if cascade is negative, then farfield, so ignore istrm
!         IF ( jdn<0 .AND. istrm>0 ) THEN
          IF ( jdn<0 ) THEN
            IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
     +           'down HRU<0 thus cascade is to strm_farfield', i, kup,
     +           jdn, frac, istrm
            istrm = 0
          ENDIF

          IF ( jdn>0 .AND. istrm<1 ) THEN
            IF ( Hru_type(jdn).EQ.0 ) THEN
              IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
     +             'Cascade ignored as down HRU is inactive', i, kup,
     +             jdn, frac, istrm
              CYCLE
            ENDIF
          ENDIF

          ! want to ignore streams within lakes, not sure how to do yet
!         IF ( jdn>0 ) THEN
!           IF ( Hru_type(jdn)==2 .AND. istrm<1 ) THEN
!             istrm = 0
!             IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
!    +             'Up HRU of a lake HRU cannot cascade to stream'//
!    +             ' segment, hru_strmseg_down_id set to 0',
!    +             i, kup, jdn, frac, istrm
!           ENDIF
!         ENDIF

          carea = frac*Hru_area(kup)
          ! get rid of small cascades, redistribute fractions
          IF ( carea<Cascade_tol .AND. frac<0.075 ) THEN
            IF ( Print_debug==13 )
     +           WRITE ( MSGUNT, 9005 ) i, kup, jdn, frac*100.0, carea
          ELSEIF ( Cascade_flg.EQ.1 ) THEN
            IF ( frac>hru_frac(kup) ) THEN
              hru_frac(kup) = frac
              Ncascade_hru(kup) = 1
              Hru_down_frac(1, kup) = frac
              IF ( istrm>0 ) THEN
                Hru_down(1, kup) = -istrm
              ELSE
                ! if jdn is negative then farfield
                IF ( jdn<0 ) THEN
                  IF ( Print_debug==13 ) WRITE ( MSGUNT, 9006 ) i, frac,
     +                 Nsegmentp1
                  Hru_down(1, kup) = -Nsegmentp1
                  Outflow_flg = 1
                ELSE
                  Hru_down(1, kup) = jdn
                ENDIF
              ENDIF
            ENDIF
          ELSE
            hru_frac(kup) = hru_frac(kup) + frac
            IF ( hru_frac(kup)>1.0 ) THEN
              IF ( hru_frac(kup)>1.00001 ) THEN
                IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
     +              'Addition of cascade link makes contributing area'//
     +              ' add up to > 1.0, thus fraction reduced',
     +              i, kup, jdn, frac, istrm
              ENDIF
              frac = frac + 1.0 - hru_frac(kup)
              hru_frac(kup) = 1.0
            ENDIF
            Ncascade_hru(kup) = Ncascade_hru(kup) + 1
            kk = Ncascade_hru(kup)
            Hru_down_frac(kk, kup) = frac
            IF ( istrm>0 ) THEN
              Hru_down(kk, kup) = -istrm
            ELSE
              ! if jdn is negative then farfield
              IF ( jdn<0 ) THEN
                IF ( Print_debug==13 ) WRITE ( MSGUNT, 9006 ) i, frac,
     +               Nsegmentp1
                Hru_down(kk, kup) = -Nsegmentp1
                Outflow_flg = 1
              ELSE
                Hru_down(kk, kup) = jdn
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      ! how do we route headwater HRUs to stream segment rather than
      ! across valleys**********************RSR???

      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Ncascade_hru(i)>0 ) THEN
          fracchk = 0.0
          j = 1
          k = 1
          DO WHILE ( j<=Ncascade_hru(i) )
            frac = Hru_down_frac(k, i)
            Hru_down_frac(j, i) = frac + 
     +                            frac*(1.0-hru_frac(i))/hru_frac(i)
            fracchk = fracchk + Hru_down_frac(j, i)
            dnhru = Hru_down(k, i)
            DO kk = 1, j - 1
              IF ( dnhru.EQ.Hru_down(kk, i) ) THEN
                Hru_down_frac(kk, i) = Hru_down_frac(kk, i) +
     +                                Hru_down_frac(k, i)
                IF ( Hru_down_frac(kk, i)>1.0 ) THEN
                  IF ( Print_debug==13 ) THEN
                    WRITE ( MSGUNT, * )
     +               'Combining cascade links makes contributing area'//
     +               ' add up to > 1.0, thus fraction reduced.'
                    WRITE ( MSGUNT,* ) 'Up HRU:', i, ' Down HRU:', dnhru
                  ENDIF
                  fracchk = fracchk + 1.0 - Hru_down_frac(kk, i)
                  Hru_down_frac(kk, i) = 1.0
                ENDIF
                j = j - 1
                Ncascade_hru(i) = Ncascade_hru(i) - 1
                IF ( dnhru<0 ) THEN
! two cascades to same stream segment, combine
!                  IF ( Print_debug>-1 ) PRINT 9002, i, 'stream segment',
!     +                                              ABS(dnhru)
                  IF ( Print_debug==13 ) WRITE ( MSGUNT, 9002 ) i,
     +                 'stream segment', ABS( dnhru )
                ELSE
! two cascades to same HRU, combine
                  IF ( Print_debug==13 ) WRITE ( MSGUNT, 9002 ) i,
     +                 'downslope HRU', dnhru
                ENDIF
              ENDIF
            ENDDO
            Cascade_area(j, i) = Hru_down_frac(j, i)*Hru_area(i)
            IF ( dnhru>0 ) Hru_down_fracwt(j, i) =
     +                     Cascade_area(j, i)/Hru_area(dnhru)
            IF ( j<k ) Hru_down(j, i) = dnhru
            j = j + 1
            k = k + 1
          ENDDO
          ! allow for close enough
          IF ( fracchk<0.9998 ) THEN
            IF ( Print_debug==13 )
     +        WRITE (MSGUNT, *) 'Warning, contributing area of cascade',
     +            ' links for HRU:', i, ' sums to < HRU area:', fracchk
!           Iret = 1
          ENDIF
        ENDIF
      ENDDO
      DEALLOCATE ( hru_frac )

      CALL order_hrus(Iret)

      IF ( Print_debug==13 ) THEN
        WRITE ( MSGUNT, 9001 )
        WRITE ( MSGUNT, 9003 ) (Hru_route_order(i), i=1, Iorder)
      ENDIF

      DEALLOCATE ( Hru_up_id, Hru_pct_up )
      DEALLOCATE ( Hru_down_id, Hru_strmseg_down_id )

 9001 FORMAT (/, 'HRU routing order:')
 9002 FORMAT ('*** WARNING, combined multiple cascade paths from HRU:',
     +        I6, ' to ', A, ':', I6)
 9003 FORMAT (12I6)
 9004 FORMAT ('*** WARNING, ', A, /, '    Cascade:', I6, '; up HRU:',
     +        I6, '; down HRU:', I6, '; up fraction:', F8.4,
     +        '; stream segment:', I5)
 9005 FORMAT ('*** WARNING, ignoring small cascade, carea<cascade_tol',
     +        /, '    Cascade:', I6, '; HRU up:', I6, '; HRU down:', I6,
     +        '; fraction up:', F8.2, '; cascade area:', F8.2)
 9006 FORMAT ('*** INFO, HRU:', I6, ', fraction:', F8.2,
     +        ' is producing far-field flow to segment:', I6)

      END SUBROUTINE init_cascade

!***********************************************************************
! order hrus allowing many to 1
!***********************************************************************
      SUBROUTINE order_hrus(Iret)
      USE PRMS_CASCADE, ONLY: Hru_down, Iorder, MSGUNT, Circle_switch,
     +    Ncascade_hru
      USE PRMS_MODULE, ONLY: Nhru, Print_debug
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type
      IMPLICIT NONE
      EXTERNAL up_tree
!     Arguments
      INTEGER, INTENT(OUT) :: Iret
!     Local Variables
      INTEGER, ALLOCATABLE :: roots(:), path(:), hrus_up_list(:, :)
      INTEGER, ALLOCATABLE :: is_hru_on_list(:), dn_id_count(:)
      INTEGER, ALLOCATABLE :: up_id_count(:), up_id_cnt(:)
      INTEGER :: i, j, k, ii, nroots, circle_flg, ihru, npath
      INTEGER :: goes_on_list, up_hru_id, added, dnhru, max_up_id_count
!-----------------------------------------------------------------------
!     up_id_count equals number of upslope HRUs an HRU has
!     dn_id_count equals number of downslope HRUs an HRU has
!     ncascade_hru equals number of downslope HRUs and stream segments
!                  an HRU has
      max_up_id_count = 0
      ALLOCATE (up_id_count(Nhru), dn_id_count(Nhru), roots(Nhru))
      ALLOCATE (path(Nhru), is_hru_on_list(Nhru))
      DO i = 1, Nhru
        up_id_count(i) = 0
        dn_id_count(i) = 0
        roots(i) = 0
        path(i) = 0
        is_hru_on_list(i) = 0
      ENDDO
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        DO k = 1, Ncascade_hru(i)
          dnhru = Hru_down(k, i)
          IF ( dnhru>0 ) THEN
            dn_id_count(i) = dn_id_count(i) + 1
            up_id_count(dnhru) = up_id_count(dnhru) + 1
!           determine the maximum up_id_count
            IF ( up_id_count(dnhru)>max_up_id_count )
     +           max_up_id_count = up_id_count(dnhru)
          ENDIF
        ENDDO
      ENDDO
      ALLOCATE (hrus_up_list(max_up_id_count, Nhru))
      hrus_up_list = 0

! get the list of HRUs upslope of each HRU and root HRUs
      ALLOCATE (up_id_cnt(Nhru))
      nroots = 0
      up_id_cnt = up_id_count
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( dn_id_count(i)==0 ) THEN
          nroots = nroots + 1
          roots(nroots) = i
        ENDIF
        IF ( up_id_count(i)==0 ) THEN
          !HRU does not receive or cascade flow - swale
          IF ( Hru_type(i)==1 .AND. Ncascade_hru(i)==0 ) THEN
            IF ( Print_debug==13 ) WRITE ( MSGUNT, 9008 ) i
            PRINT 9008, i
            Hru_type(i) = 3
            CYCLE
          ENDIF
        ENDIF
        IF ( Hru_type(i)==1 .AND. Ncascade_hru(i)==0 ) THEN
          !HRU does not cascade flow - swale
          IF ( Print_debug==13 ) WRITE ( MSGUNT, 9009 ) i
          PRINT 9009, i
          Hru_type(i) = 3
          CYCLE
        ELSE
          DO k = 1, Ncascade_hru(i)
            dnhru = Hru_down(k, i)
            IF ( dnhru>0 ) THEN
              hrus_up_list(up_id_cnt(dnhru), dnhru) = i
              up_id_cnt(dnhru) = up_id_cnt(dnhru) - 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      DEALLOCATE (up_id_cnt)

      Iret = 0
! check for circles when circle_switch = 1
      IF ( Circle_switch==1 ) THEN
        circle_flg = 0
        DO i = 1, nroots
          ihru = roots(i)
          path(1) = ihru
          npath = 1
          circle_flg = 0
          CALL up_tree(Nhru, ihru, up_id_count, hrus_up_list, npath,
     +                 path, circle_flg, max_up_id_count)
          IF ( circle_flg==1 ) Iret = 1
        ENDDO
        IF ( circle_flg==1 ) THEN
          PRINT 9005
          IF ( Print_debug==13 ) WRITE ( MSGUNT, 9005 )
          Iret = 1
          RETURN
        ENDIF
      ENDIF
      DEALLOCATE (path)

! Determine HRU routing order
      Hru_route_order = 0
      Iorder = 0  !number of HRUs added to Hru_rout_order
      DO WHILE ( Iorder < Active_hrus )
        added = 0
        DO i = 1, Nhru
          IF ( Hru_type(i)==0 ) CYCLE !ignore inactive HRUs
            IF ( is_hru_on_list(i)==0 ) THEN
            goes_on_list = 1
            DO j = 1, up_id_count(i)
              up_hru_id = hrus_up_list(j, i)
              ! if upslope HRU not on list, can't add HRU i
              IF ( is_hru_on_list(up_hru_id)==0 ) THEN
                goes_on_list = 0
                EXIT
              ENDIF
            ENDDO
            !add HRU to list
            IF ( goes_on_list==1 ) THEN
              is_hru_on_list(i) = 1
              Iorder = Iorder + 1
              Hru_route_order(Iorder) = i
              added = 1
            ENDIF
          ENDIF
        ENDDO
        IF ( added==0 ) THEN
          PRINT *, 'No HRUs added to routing order on last pass',
     +             ' through cascades, possible circles in HRUs'
          DO i = 1, Nhru
            IF ( is_hru_on_list(i)==0 ) THEN
              PRINT *,'HRU not in order:', i
              IF ( Print_debug==13 )
     +             WRITE ( MSGUNT, * ) 'HRU not in order:', i
            ENDIF
          ENDDO
          PRINT 9002, (Hru_route_order(i), i=1, Iorder)
          IF ( Print_debug==13 ) THEN
            WRITE (MSGUNT, *) 'Major screw up no HRUs added to routing',
     +                        ' order on last pass through'
            WRITE ( MSGUNT, 9001 ) Iorder
            WRITE ( MSGUNT, 9002 ) (Hru_route_order(i), i=1, Iorder)
          ENDIF
          Iret = 1
          RETURN
        ENDIF
      ENDDO
      DEALLOCATE ( hrus_up_list, up_id_count, dn_id_count )

      IF ( Print_debug==13 ) THEN
        WRITE ( MSGUNT, 9003 ) nroots
        WRITE ( MSGUNT, 9002 ) (roots(i), i=1, nroots)
      ENDIF
      DEALLOCATE ( roots )

      IF ( Iorder/=Active_hrus ) THEN
        PRINT 9004, Iorder, Nhru, Active_hrus
        IF ( Print_debug==13 )
     +       WRITE ( MSGUNT, 9004 ) Iorder, Nhru, Active_hrus
        DO i = 1, Nhru
          IF ( is_hru_on_list(i).EQ.0 ) THEN
            IF ( Hru_type(i)/=0 ) THEN
              PRINT 9006, i
              IF ( Print_debug==13 ) WRITE ( MSGUNT, 9006 ) i
              Iret = 1
            ELSE
              IF ( Print_debug==13 ) WRITE ( MSGUNT, 9007 ) i
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE ( is_hru_on_list )

 9001 FORMAT (/, I6, ' HRUs in routing order)')
 9002 FORMAT (12I6)
 9003 FORMAT (/, I6, ' HRUs that do not cascade to another HRU (roots)')
 9004 FORMAT (/, 'Warning, not all HRUs are included in the cascading',
     +        ' pattern, likely circle or inactive HRUs', //,
     +        'Number of HRUs in pattern:' I6, ', number of HRUs:', I6,
     +        ', Active HRUs:', I6, //, 'HRUs not in routing order:')
 9005 FORMAT (/, 'Error, circular HRU path found', /)
 9006 FORMAT (I6, ' missing')
 9007 FORMAT (I6, ' inactive')
 9008 FORMAT ('Warning, HRU', I7, ' does not cascade or receive flow', /
     +        , 9X, 'and was specified as hru_type 1,', /, 9X,
     +        'hru_type was changed to 3 (swale)', /)
 9009 FORMAT ('Warning, HRU', I7, ' receives flow but does not cascade',
     +        /, 9X, 'and was specified as hru_type 1,', /, 9X,
     +        'hru_type was changed to 3 (swale)', /)

      END SUBROUTINE order_hrus

!***********************************************************************
! Initialize cascading flow variables
!***********************************************************************
      SUBROUTINE initgw_cascade(Iret)
      USE PRMS_CASCADE
      USE PRMS_MODULE, ONLY: Ncascdgw, Ngw, Nsegment, Nsegmentp1,
     +    Print_debug
      USE PRMS_BASIN, ONLY: Active_gwrs, Gwr_route_order, Gwr_type,
     +    Hru_area
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL order_gwrs, read_error
      INTRINSIC ABS
! Arguments
      INTEGER, INTENT(OUT) :: Iret
! Local Variables
      INTEGER :: i, j, k, ii, kk, dngwr, kup, jdn, istrm
      REAL :: fracchk, carea, frac
      REAL, ALLOCATABLE :: gwr_frac(:)
!***********************************************************************
      Iret = 1

! Cascade parameters
      IF ( getparam('cascade', 'gw_strmseg_down_id', Ncascdgw,
     +     'integer', Gw_strmseg_down_id)/=0 )
     +     CALL read_error(2, 'gw_strmseg_down_id')

      Iret = 0

! allocate GWR variables
      ALLOCATE (Gwr_down(Ndown, Ngw))
      ALLOCATE (Gwr_down_frac(Ndown, Ngw))
      ALLOCATE (Cascade_gwr_area(Ndown, Ngw))
!     ALLOCATE (Gwr_down_fracwt(Ndown, Ngw))
      ALLOCATE (gwr_frac(Ngw))
      DO i = 1, Ngw
        DO k = 1, Ndown
          Gwr_down(k, i) = 0
          Gwr_down_frac(k, i) = 0.0
!         Gwr_down_fracwt(k, i) = 0.0
          Cascade_gwr_area(k, i) = 0.0
        ENDDO
        gwr_frac(i) = 0.0
        Ncascade_gwr(i) = 0
      ENDDO

      IF ( Print_debug==13 ) WRITE ( MSGUNT, * )

      DO i = 1, Ncascdgw
        kup = Gw_up_id(i)
        IF ( kup<1 ) THEN
          PRINT *, 'Cascade ignored as gw_up_id<1, cascade:', i, kup
          CYCLE
        ENDIF
        jdn = Gw_down_id(i)
        frac = Gw_pct_up(i)
        IF ( frac>1.0 ) frac = 1.0
        istrm = Gw_strmseg_down_id(i)

        IF ( frac<0.00001 ) THEN
          IF ( Print_debug==13 ) 
     +         WRITE ( MSGUNT,9004 ) 'Cascade ignored as gw_pct_up=0.0',
     +                               i, kup, jdn, frac, istrm
        ELSEIF ( istrm>Nsegment ) THEN
          IF ( Print_debug==13 ) 
     +       WRITE (MSGUNT, 9004) 'Cascade ignored as segment>nsegment',
     +                            i, kup, jdn, frac, istrm
        ELSEIF ( kup<1 .AND. jdn==0 ) THEN
          IF ( Print_debug==13 )
     +    WRITE (MSGUNT, 9004) 'Cascade ignored as up and down GWR = 0',
     +                         i, kup, jdn, frac, istrm
        ELSEIF ( Gwr_type(kup).EQ.0 ) THEN
          IF ( Print_debug==13 )
     +     WRITE (MSGUNT, 9004) 'Cascade ignored as up GWR is inactive',
     +                         i, kup, jdn, frac, istrm
        ELSEIF ( istrm==0 .AND. jdn==0 ) THEN
          IF ( Print_debug==13 ) THEN
            WRITE ( MSGUNT, 9004 )
     +              'Cascade ignored as down GWR & segment ids = 0',
     +              i, kup, jdn, frac, istrm
            PRINT *,
     +            'Cascade ignored as down GWR & segment ids = 0',
     +            i, kup, jdn, frac, istrm
          ENDIF
        ELSEIF ( Gwr_type(kup)==3 ) THEN
          Gwr_type(kup) = 1
        ELSEIF ( Gwr_type(kup)==2 ) THEN
          Gwr_type(kup) = 1
        ELSE
          ! if cascade is negative, then farfield, so ignore istrm
!         IF ( jdn<0 .AND. istrm>0 ) THEN
          IF ( jdn<0 ) THEN
            IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
     +           'down GWR<0 thus cascade is to strm_farfield',
     +           i, kup, jdn, frac, istrm 
            istrm = 0
          ENDIF

          IF ( jdn>0 .AND. istrm<1 ) THEN
            IF ( Gwr_type(jdn).EQ.0 ) THEN
              IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
     +             'Cascade ignored as down GWR is inactive',
     +             i, kup, jdn, frac, istrm
              CYCLE
            ENDIF
          ENDIF

          ! want to ignore streams within lakes, not sure how to do yet
!         IF ( jdn>0 ) THEN
!           IF ( Gwr_type(jdn)==2 .AND. istrm>1 ) THEN
!             istrm = 0
!             IF ( Print_debug==13 ) WRITE (MSGUNT, 9004)
!    +                 'Up GWR of a lake GWR cannot cascade to stream'//
!    +                 ' segment, gw_strmseg_down_id set to 0',
!    +                 i, kup, jdn, frac, istrm
!           ENDIF
!         ENDIF

          carea = frac*Hru_area(kup)
          ! get rid of small cascades, redistribute fractions
          IF ( carea<Cascade_tol .AND. frac<0.075 ) THEN
            IF ( Print_debug==13 )
     +           WRITE ( MSGUNT, 9005 ) i, kup, jdn, frac*100.0, carea
          ELSEIF ( Cascade_flg.EQ.1 ) THEN
            IF ( frac>gwr_frac(kup) ) THEN
              gwr_frac(kup) = frac
              Ncascade_gwr(kup) = 1
              Gwr_down_frac(1, kup) = frac
              IF ( istrm>0 ) THEN
                Gwr_down(1, kup) = -istrm
              ELSE
                ! if jdn is negative then farfield
                IF ( jdn<0 ) THEN
                  IF ( Print_debug==13 )
     +                 WRITE ( MSGUNT, 9006 ) i, frac, Nsegmentp1
                  Gwr_down(1, kup) = -Nsegmentp1
                  Outflow_gwrflg = 1
                ELSE
                  Gwr_down(1, kup) = jdn
                ENDIF
              ENDIF
            ENDIF
          ELSE
            gwr_frac(kup) = gwr_frac(kup) + frac
            IF ( gwr_frac(kup)>1.0 ) THEN
              IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
     +          'Addition of GWR cascade link makes contributing area'//
     +          ' add up to > 1.0, thus fraction reduced',
     +          i, kup, jdn, frac, istrm
              frac = frac + 1.0 - gwr_frac(kup)
              gwr_frac(kup) = 1.0
            ENDIF
            Ncascade_gwr(kup) = Ncascade_gwr(kup) + 1
            kk = Ncascade_gwr(kup)
            Gwr_down_frac(kk, kup) = frac
            IF ( istrm>0 ) THEN
              Gwr_down(kk, kup) = -istrm
            ELSE
              ! if jdn is negative then farfield
              IF ( jdn<0 ) THEN
                IF ( Print_debug==13 ) WRITE ( MSGUNT, 9006 ) i, frac,
     +               Nsegmentp1
                Gwr_down(kk, kup) = -Nsegmentp1
                Outflow_gwrflg = 1
              ELSE
                Gwr_down(kk, kup) = jdn
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      DO ii = 1, Active_gwrs
        i = Gwr_route_order(ii)
        IF ( Ncascade_gwr(i)>0 ) THEN
          fracchk = 0.0
          j = 1
          k = 1
          DO WHILE ( j<=Ncascade_gwr(i) )
            frac = Gwr_down_frac(k, i)
            Gwr_down_frac(j, i) = frac +
     +                            frac*(1.0-gwr_frac(i))/gwr_frac(i)
            fracchk = fracchk + Gwr_down_frac(j, i)
            dngwr = Gwr_down(k, i)
            DO kk = 1, j - 1
              IF ( dngwr.EQ.Gwr_down(kk, i) ) THEN
                Gwr_down_frac(kk, i) = Gwr_down_frac(kk, i) +
     +                                 Gwr_down_frac(k, i)
                IF ( Gwr_down_frac(kk, i)>1.0 ) THEN
                  IF ( Print_debug==13 ) WRITE ( MSGUNT, 9004 )
     +           'Combining GWR cascade links makes contributing area'//
     +            ' add up to > 1.0, thus fraction reduced',
     +            i, dngwr, Gwr_down_frac(kk, i)
                  fracchk = fracchk + 1.0 - Gwr_down_frac(kk, i)
                  Gwr_down_frac(kk, i) = 1.0
                ENDIF
                j = j - 1
                Ncascade_gwr(i) = Ncascade_gwr(i) - 1
                IF ( dngwr<0 ) THEN
! two cascades to same stream segment, combine
                  IF ( Print_debug==13 )
     +              WRITE (MSGUNT, 9002) i, 'stream segment', ABS(dngwr)
                ELSE
                  IF ( Print_debug==13 )
     +                 WRITE ( MSGUNT, 9002 ) i, 'downslope GWR', dngwr
                ENDIF
              ENDIF
            ENDDO
            Cascade_gwr_area(j, i) = Gwr_down_frac(j, i)*Hru_area(i)
!           IF ( dngwr>0 ) Gwr_down_fracwt(j, i) =
!    +                         Cascade_gwr_area(j, i)/Gwres_area(dngwr)
            IF ( j<k ) Gwr_down(j, i) = dngwr
            j = j + 1
            k = k + 1
          ENDDO
          IF ( fracchk<0.9998 ) THEN
            IF ( Print_debug==13 )
     +        WRITE (MSGUNT, *) 'Warning, contributing area of cascade',
     +            ' links for GWR:', i, ' sums to < GWR area:', fracchk
!           Iret = 1
          ENDIF
        ENDIF
      ENDDO
      DEALLOCATE (gwr_frac)

      CALL order_gwrs(Iret)

      IF ( Print_debug==13 ) THEN
        WRITE ( MSGUNT, 9001 )
        WRITE ( MSGUNT, 9003 ) (Gwr_route_order(i), i=1, Igworder)
      ENDIF

      DEALLOCATE ( Gw_strmseg_down_id, Gw_up_id )
      DEALLOCATE ( Gw_down_id, Gw_pct_up )

 9001 FORMAT (/, 'GWR routing order:')
 9002 FORMAT ('Warning, combined multiple cascade paths from GWR:', I6,
     +        ' to ', A, ':', I6)
 9003 FORMAT (12I6)
 9004 FORMAT ('*** WARNING, ', A, /, '    Cascade:', I6, '; up GWR:',
     +        I6, '; down GWR:', I6, '; up fraction:', F8.4,
     +        '; stream segment:', I5)
 9005 FORMAT ('*** WARNING, ignoring small cascade, carea<cascade_tol',
     +        /, '    Cascade:', I6, '; GWR up:', I6, '; GWR down:', I6,
     +        '; fraction up:', F8.2, '; cascade area:', F8.2)
 9006 FORMAT ('*** INFO, GWR:', I6, ', fraction:', F8.2,
     +        ' is producing far-field flow to segment:', I6)

      END SUBROUTINE initgw_cascade

!***********************************************************************
! order GWRs allowing many to 1
!***********************************************************************
      SUBROUTINE order_gwrs(Iret)
      USE PRMS_CASCADE, ONLY: Gwr_down, Igworder, MSGUNT, Circle_switch,
     +    Ncascade_gwr
      USE PRMS_MODULE, ONLY: Ngw, Print_debug
      USE PRMS_BASIN, ONLY: Active_gwrs, Gwr_route_order, Gwr_type
      IMPLICIT NONE
      EXTERNAL up_tree
!     Arguments
      INTEGER, INTENT(OUT) :: Iret
!     Local Variables
      INTEGER, ALLOCATABLE :: roots(:), path(:), gwrs_up_list(:, :)
      INTEGER, ALLOCATABLE :: is_gwr_on_list(:), dn_id_count(:)
      INTEGER, ALLOCATABLE :: up_id_count(:), up_id_cnt(:)
      INTEGER :: i, j, k, ii, nroots, circle_flg, igwr, npath, swalegwr
      INTEGER :: goes_on_list, up_gwr_id, added, dngwr, max_up_id_count
!-----------------------------------------------------------------------
!     up_id_count equals number of upslope GWRs a GWR has
!     dn_id_count equals number of downslope GWRs a GWR has
!     ncascade_gwr equals number of downslope GWRs and stream segments
!                  a GWR has
      max_up_id_count = 0
      ALLOCATE (up_id_count(Ngw), dn_id_count(Ngw), roots(Ngw))
      ALLOCATE (path(Ngw), is_gwr_on_list(Ngw))
      DO i = 1, Ngw
        up_id_count = 0
        dn_id_count = 0
        roots(i) = 0
        path(i) = 0
        is_gwr_on_list(i) = 0
      ENDDO
      DO ii = 1, Active_gwrs
        i = Gwr_route_order(ii)
        DO k = 1, Ncascade_gwr(i)
          dngwr = Gwr_down(k, i)
          IF ( dngwr>0 ) THEN
            dn_id_count(i) = dn_id_count(i) + 1
            up_id_count(dngwr) = up_id_count(dngwr) + 1
!           determine the maximum up_id_count
            IF ( up_id_count(dngwr)>max_up_id_count )
     +           max_up_id_count = up_id_count(dngwr)
          ENDIF
        ENDDO
      ENDDO
      ALLOCATE (gwrs_up_list(max_up_id_count, Ngw))
      gwrs_up_list = 0

! get the list of GWRs upslope of each GWR and root GWRs
      ALLOCATE (up_id_cnt(Ngw))
      nroots = 0
      up_id_cnt = up_id_count
      swalegwr = 0
      DO ii = 1, Active_gwrs
        i = Gwr_route_order(ii)
        IF ( dn_id_count(i)==0 ) THEN
          nroots = nroots + 1
          roots(nroots) = i
        ENDIF
        IF ( Ncascade_gwr(i)==0 ) THEN
          IF ( up_id_count(i)==0 ) THEN
            !GWR does not receive or cascade flow
            PRINT 9008, i
            IF ( Print_debug==13 ) WRITE ( MSGUNT, 9008 ) i
          ENDIF
          !GWR does not cascade flow
          PRINT 9009, i
          IF ( Print_debug==13 ) WRITE ( MSGUNT, 9009 ) i
          swalegwr = 1
        ELSE
          DO k = 1, Ncascade_gwr(i)
            dngwr = Gwr_down(k, i)
            IF ( dngwr>0 ) THEN
              gwrs_up_list(up_id_cnt(dngwr), dngwr) = i
              up_id_cnt(dngwr) = up_id_cnt(dngwr) - 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      IF ( swalegwr==1 ) THEN
        Iret = 1
        RETURN
      ENDIF
      DEALLOCATE (up_id_cnt)

      Iret = 0
! check for circles when circle_switch = 1
      IF ( Circle_switch==1 ) THEN
        circle_flg = 0
        DO i = 1, nroots
          igwr = roots(i)
          path(1) = igwr
          npath = 1
          circle_flg = 0
          CALL up_tree(Ngw, igwr, up_id_count, gwrs_up_list, npath,
     +                 path, circle_flg, max_up_id_count)
          IF ( circle_flg==1 ) Iret = 1
        ENDDO
        IF ( circle_flg==1 ) THEN
          PRINT 9005
          IF ( Print_debug==13 ) WRITE ( MSGUNT, 9005 )
          Iret = 1
          RETURN
        ENDIF
      ENDIF
      DEALLOCATE (path)

! Determine GWR routing order
      Gwr_route_order = 0
      Igworder = 0  !number of GWRs added to Gwr_route_order
      DO WHILE ( Igworder < Active_gwrs )
        added = 0
        DO i = 1, Ngw
          IF ( Gwr_type(i).EQ.0 ) CYCLE !ignore inactive GWRs
          IF ( is_gwr_on_list(i)==0 ) THEN
            goes_on_list = 1
            DO j = 1, up_id_count(i)
              up_gwr_id = gwrs_up_list(j, i)
              ! if upslope GWR not on list, can't add GWR i
              IF ( is_gwr_on_list(up_gwr_id)==0 ) THEN
                goes_on_list = 0
                EXIT
              ENDIF
            ENDDO
            !add GWR to list
            IF ( goes_on_list==1 ) THEN
              is_gwr_on_list(i) = 1
              Igworder = Igworder + 1
              Gwr_route_order(Igworder) = i
              added = 1
            ENDIF
          ENDIF
        ENDDO
        IF ( added==0 ) THEN
          PRINT *, 'Major screw up, no GWRs added in routing order',
     +             ' on last pass through'
          PRINT 9002, (Gwr_route_order(i), i=1, Igworder)
          IF ( Print_debug==13 ) THEN
            WRITE (MSGUNT, *) 'Major screw up no GWRs added in routing',
     +                        ' order on last pass through'
            WRITE ( MSGUNT, 9001 ) Igworder
            WRITE ( MSGUNT, 9002 ) (Gwr_route_order(i), i=1, Igworder)
          ENDIF
          Iret = 1
          RETURN
        ENDIF
      ENDDO
      DEALLOCATE (gwrs_up_list, up_id_count, dn_id_count)

      IF ( Print_debug==13 ) THEN
        WRITE ( MSGUNT, 9003 ) nroots
        WRITE ( MSGUNT, 9002 ) (roots(i), i=1, nroots)
      ENDIF
      DEALLOCATE ( roots )

      IF ( Igworder/=Active_gwrs ) THEN
        PRINT 9004, Igworder, Ngw, Active_gwrs
        IF ( Print_debug==13 )
     +       WRITE ( MSGUNT, 9004 ) Igworder, Ngw, Active_gwrs
        DO i = 1, Ngw
          IF ( is_gwr_on_list(i).EQ.0 ) THEN
            IF ( Gwr_type(i)/=0 ) THEN
              PRINT 9006, i
              IF ( Print_debug==13 ) WRITE ( MSGUNT, 9006 ) i
              Iret = 1
            ELSE
              IF ( Print_debug==13 ) WRITE ( MSGUNT, 9007 ) i
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE (is_gwr_on_list)

 9001 FORMAT (/, I6, ' GWRs that do not receive upslope flow (heads)')
 9002 FORMAT (12I6)
 9003 FORMAT (/, I6, ' GWRs that do not cascade to another GWR (roots)')
 9004 FORMAT (/, 'Warning, not all GWRs are included in the cascading',
     +        ' pattern, likely circle or inactive GWRs', //,
     +        'Number of GWRs in pattern:' I6, ', number of GWRs:', I6,
     +        ', Active GWRs:', I6, //, 'GWRs not in routing order:')
 9005 FORMAT (/, 'ERROR, circular GWR path found', /)
 9006 FORMAT (I6, ' missing')
 9007 FORMAT (I6, ' inactive')
 9008 FORMAT ('ERROR, GWR', I7, ' does not cascade or receive flow')
 9009 FORMAT ('ERROR, GWR', I7, ' does not cascade flow')

      END SUBROUTINE order_gwrs

!***********************************************************************
! Initialize cascading water bodey flow variables
!***********************************************************************
      SUBROUTINE initwtrbdy_cascade(Iret)
      USE PRMS_CASCADE
      USE PRMS_MODULE, ONLY: Nsegment, Print_debug
      USE PRMS_BASIN, ONLY: Hru_type
      IMPLICIT NONE
      EXTERNAL order_wtrbdy
      INTRINSIC ABS
! Arguments
      INTEGER, INTENT(OUT) :: Iret
! Local Variables
      INTEGER :: i, k, ii, ierr
!***********************************************************************
      Iret = 1

      IF ( Print_debug==13 ) WRITE (MSGUNT, *)

      DO i = 1, Nwtrbdy
        Wtrbdy_route_order(i) = 0
        Ncascade_wtrbdy(i) = 0
      ENDDO

      Iseg_out = 0
! determine maximum number of down water bodies
      ! Already added lake outlets to Wtrbdy_down
      ierr = 0
      DO i = 1, Nwtrbdy
        ii = Strm_lake_down(i)
        IF ( ii==Nhrup1 ) CYCLE
        k = Wtrbdy_down(i)
        IF ( ii>0 ) THEN
          IF ( Hru_type(ii)<0 ) THEN
            PRINT *, 'ERROR, lake HRU is designated as farfield,'//
     +               ' segment:', i
            ierr = 1
          ELSEIF ( Hru_type(ii)==0 ) THEN
            PRINT *, 'ERROR, lake HRU is inactive, segment:', i
            ierr = 1
          ELSEIF ( Hru_type(ii)/=2 ) THEN
            PRINT *, 'ERROR, HRU is not a lake, segment:', i
            ierr = 1
          ENDIF
        ELSEIF ( ii>Nhrup1 ) THEN
          PRINT *, 'ERROR, lake_hru_down_id > nhru, segment:', i
          ierr = 1
        ELSEIF ( k>Nsegment ) THEN
          PRINT *, 'ERROR, segment > nsegment, segment:', i
          ierr = 1
        ENDIF
        IF ( k==0 .AND. ii==0 ) THEN
          Iseg_out = i
        ELSEIF ( ii==0 .AND. k>0 ) THEN
          Ncascade_wtrbdy(i) = Ncascade_wtrbdy(i) + 1
          Wtrbdy_down(i) = k
        ENDIF
        IF ( ii>0 ) THEN
          Ncascade_wtrbdy(i) = Ncascade_wtrbdy(i) + 1
          Wtrbdy_down(i) = -ii
        ENDIF
        IF ( Ncascade_wtrbdy(i)>1 ) THEN
          PRINT *, 'ERROR, water body outlets > 1', Ncascade_wtrbdy(i),i
          RETURN
        ENDIF
      ENDDO
      IF ( ierr==1 ) RETURN

      IF ( Iseg_out==0 ) THEN
        Iseg_out = Nsegment
        PRINT *, ' no outlet found, setting outlet to Nsegment'
      ENDIF
 
      CALL order_wtrbdy(Iret)
      
      IF ( Print_debug==13 ) THEN
        WRITE (MSGUNT, 9001)
        WRITE (MSGUNT, 9002) (Wtrbdy_route_order(i), i=1, Nwtrbdy)
      ENDIF

      DEALLOCATE ( Strmseg_down_id, Lake_hru_down_id )
      DEALLOCATE ( Seg_res_id )

 9001 FORMAT (/, ' Water-body routing order:')
 9002 FORMAT (12I6)

      END SUBROUTINE initwtrbdy_cascade

!***********************************************************************
! order Water-body network (1 to 1)
!***********************************************************************
      SUBROUTINE order_wtrbdy(Iret)
      USE PRMS_CASCADE
      USE PRMS_MODULE, ONLY: Nsfres, Nsegment, Print_debug
      USE PRMS_BASIN, ONLY: Sfres_hru
      IMPLICIT NONE
      EXTERNAL up_tree
!     Arguments
      INTEGER, INTENT(OUT) :: Iret
!     Local Variables
      INTEGER, ALLOCATABLE :: roots(:), path(:), wtrbdy_up_list(:, :)
      INTEGER, ALLOCATABLE :: is_wtrbdy_on_list(:), dn_id_count(:)
      INTEGER, ALLOCATABLE :: up_id_count(:), up_id_cnt(:)
      INTEGER, ALLOCATABLE :: dn_wtrbdy_lake(:)
      INTEGER :: i, j, nroots, circle_flg, iwtrbdy, npath
      INTEGER :: goes_on_list, up_wtrbdy_id, added, dnwtrbdy
      INTEGER :: max_up_id_count
!-----------------------------------------------------------------------
!     up_id_count equals number of upslope water bodies
!     dn_id_count equals number of downslope water bodies has = 1
!     ncascade_wtrbdy equals number of downslope water bodies
!                  a water body has
      max_up_id_count = 0
      ALLOCATE (up_id_count(Nwtrbdy), dn_id_count(Nwtrbdy))
      ALLOCATE (dn_wtrbdy_lake(Nsfres))
      DO i = 1, Nwtrbdy
        up_id_count(i) = 0
        dn_id_count(i) = 0
      ENDDO
      DO i = 1, Nwtrbdy
        dnwtrbdy = Wtrbdy_down(i)
        IF ( dnwtrbdy>0 ) THEN
          iwtrbdy = dnwtrbdy
          up_id_count(iwtrbdy) = up_id_count(iwtrbdy) + 1
        ELSEIF ( dnwtrbdy<0 ) THEN
          iwtrbdy = ABS(dnwtrbdy)
          ! wtrbdy_down for stream segments in lakes are set to -Nhrup1
          IF ( iwtrbdy/=Nhrup1 ) THEN
            ! could have disconnected lakes
            DO j = 1, Nsfres
              IF ( iwtrbdy==Sfres_hru(j) ) THEN
                iwtrbdy = Nsegment + j
                up_id_count(iwtrbdy) = up_id_count(iwtrbdy) + 1
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ELSE
          CYCLE
        ENDIF
        dn_id_count(i) = dn_id_count(i) + 1
!       determine the maximum up_id_count
        IF ( iwtrbdy/=Nhrup1 ) THEN
          IF ( up_id_count(iwtrbdy)>max_up_id_count )
     +         max_up_id_count = up_id_count(iwtrbdy)
        ENDIF
      ENDDO
      ALLOCATE (wtrbdy_up_list(max_up_id_count, Nwtrbdy))
      wtrbdy_up_list = 0

! get the list of water bodies upslope of each and roots
      ALLOCATE (up_id_cnt(Nwtrbdy), roots(Nwtrbdy))
      nroots = 0
      roots = 0
      up_id_cnt = up_id_count
      DO i = 1, Nwtrbdy
        IF ( dn_id_count(i)==0 ) THEN
          nroots = nroots + 1
          roots(nroots) = i
          IF ( up_id_count(i)==0 ) THEN
!           print *, 'isolated stream'
            !water body does not receive or cascade flow, closed water
            !      body, probably a lake
            Ncascade_wtrbdy(i) = 0
            CYCLE
          ENDIF
        ENDIF
        dnwtrbdy = ABS(Wtrbdy_down(i))
        IF ( dnwtrbdy>0 .AND. dnwtrbdy/=Nhrup1 ) THEN
          IF ( Wtrbdy_down(i)<0 ) THEN
            DO j = 1, Nsfres
              IF ( dnwtrbdy==Sfres_hru(j) ) THEN
                dnwtrbdy = Nsegment + j
                EXIT
              ENDIF
            ENDDO
          ENDIF
          wtrbdy_up_list(up_id_cnt(dnwtrbdy), dnwtrbdy) = i
          up_id_cnt(dnwtrbdy) = up_id_cnt(dnwtrbdy) - 1
        ENDIF
      ENDDO
      DEALLOCATE (up_id_cnt)

      Iret = 0
! check for circles when circle_switch = 1
      IF ( Circle_switch==1 ) THEN
        ALLOCATE (path(Nwtrbdy))
        path = 0
        circle_flg = 0
        DO i = 1, nroots
          iwtrbdy = roots(i)
          path(1) = iwtrbdy
          npath = 1
          circle_flg = 0
          CALL up_tree(Nwtrbdy, iwtrbdy, up_id_count, wtrbdy_up_list,
     +                 npath, path, circle_flg, max_up_id_count)
          IF ( circle_flg==1 ) Iret = 1
        ENDDO
        DEALLOCATE (path)
        IF ( circle_flg==1 ) THEN
          PRINT 9005
          IF ( Print_debug==13 ) WRITE (MSGUNT, 9005)
          Iret = 1
          RETURN
        ENDIF
      ENDIF

! Determine stream routing order
      Wtrbdy_route_order = 0
      ! number of water bodies added to Wtrbdy_route_order
      Iwtrbdyorder = 0
      ALLOCATE (is_wtrbdy_on_list(Nwtrbdy))
      is_wtrbdy_on_list = 0
      DO WHILE ( Iwtrbdyorder<Nwtrbdy )
        added = 0
        DO i = 1, Nwtrbdy
          IF ( is_wtrbdy_on_list(i)==0 ) THEN
            goes_on_list = 1
            DO j = 1, up_id_count(i)
              up_wtrbdy_id = wtrbdy_up_list(j, i)
              ! if upslope water body not on list, can't add
              IF ( is_wtrbdy_on_list(up_wtrbdy_id)==0 ) THEN
                goes_on_list = 0
                EXIT
              ENDIF
            ENDDO
            !add water body to list
            IF ( goes_on_list==1 ) THEN
              is_wtrbdy_on_list(i) = 1
              Iwtrbdyorder = Iwtrbdyorder + 1
              Wtrbdy_route_order(Iwtrbdyorder) = i
              added = 1
            ENDIF
          ENDIF
        ENDDO
        IF ( added==0 ) THEN
          PRINT *, 'Major screw up, no water bodies added in routing',
     +             ' order on last pass through'
          IF ( Print_debug==13 ) THEN
            WRITE (MSGUNT, *)
     +          'Major screw up, no water bodies added in routing',
     +          ' order on last pass through'
            WRITE (MSGUNT, 9001) Iwtrbdyorder
            WRITE (MSGUNT, 9002)
     +            (Wtrbdy_route_order(i), i=1, Iwtrbdyorder)
          ENDIF
          Iret = 1
          RETURN
        ENDIF
      ENDDO
      DEALLOCATE (wtrbdy_up_list, up_id_count, dn_id_count)

      IF ( Print_debug==13 ) THEN
        WRITE (MSGUNT, 9003) nroots
        WRITE (MSGUNT, 9002) (roots(i), i=1, nroots)
      ENDIF
      DEALLOCATE (roots)

      IF ( Iwtrbdyorder/=Nwtrbdy ) THEN
        PRINT 9004, Iwtrbdyorder, Nwtrbdy
        IF ( Print_debug==13 )
     +       WRITE (MSGUNT, 9004) Iwtrbdyorder, Nwtrbdy
        DO i = 1, Nwtrbdy
          IF ( is_wtrbdy_on_list(i).EQ.0 ) THEN
            PRINT 9006, i
            IF ( Print_debug==13 ) WRITE (MSGUNT, 9006) i
            Iret = 1
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE (is_wtrbdy_on_list)

 9001 FORMAT (/, I6, ' Water bodies that do not receive upslope flow',
     +        ' (heads)')
 9002 FORMAT (12I6)
 9003 FORMAT (/, I6, ' Water bodies that do not cascade to another',
     +        ' water body (roots)')
 9004 FORMAT (/, 'Warning, not all water bodies are included in the',
     +        ' network pattern, likely circle', //,
     +        'Number of water bodies in pattern:' I6,
     +        ', number of water bodies:', I6, //,
     +        'Water bodies not in routing order:')
 9005 FORMAT (/, 'Error, circular water body path found', /)
 9006 FORMAT (I6, ' missing')

      END SUBROUTINE order_wtrbdy

!***********************************************************************
! Recursively walk up a tree of cascading spatial units
!***********************************************************************
      RECURSIVE SUBROUTINE up_tree(Num, N, Down_id, Up_list, Npath,
     +                             Path, Circle_flg, Imx)
      IMPLICIT NONE
      EXTERNAL check_path
! Arguments
      INTEGER, INTENT(IN) :: Num, N, Imx
      INTEGER, INTENT(IN) :: Down_id(Num), Up_list(Imx, Num)
      INTEGER, INTENT(INOUT) :: Npath, Path(Num), Circle_flg
! Local Variables
      INTEGER :: nup, i, parent
!-----------------------------------------------------------------------
      IF ( Circle_flg.EQ.1 ) RETURN
      nup = Down_id(N)
      DO i = 1, nup
        Npath = Npath + 1
        parent = Up_list(i, N)
        Path(Npath) = parent
        CALL check_path(Npath, Path, Circle_flg, nup)
        CALL up_tree(Num, parent, Down_id, Up_list, Npath, Path,
     +               Circle_flg, Imx)
      ENDDO

      IF ( nup.EQ.0 ) CALL check_path(Npath, Path, Circle_flg, nup)
      Npath = Npath - 1

      END SUBROUTINE up_tree

!***********************************************************************
! check for circular path
!***********************************************************************
      SUBROUTINE check_path(Npath, Path, Circle_flg, Nup)
      USE PRMS_CASCADE, ONLY: MSGUNT
      USE PRMS_MODULE, ONLY: Print_debug
      IMPLICIT NONE
      INTRINSIC MIN
!     Arguments
      INTEGER, INTENT(IN) :: Npath, Path(Npath), Nup
      INTEGER, INTENT(OUT) :: Circle_flg
!     Local Variables
      INTEGER :: j, i, n
!-----------------------------------------------------------------------
      Circle_flg = 0
      DO j = 1, Npath - 1
        DO i = j + 1, Npath
          IF ( Path(j).EQ.Path(i) ) THEN
            PRINT *, 'Error, circular cascading path specified'
            PRINT *, Path
            Circle_flg = 1
          ENDIF
        ENDDO
      ENDDO

      IF ( Circle_flg.EQ.1 .OR. Nup.EQ.0 ) THEN
        IF ( Print_debug==13 ) THEN
          WRITE ( MSGUNT, * ) 'Cascade Path with', Npath, ' links'
          DO i = 1, Npath, 10
            n = MIN( Npath, i+9 )
            WRITE ( MSGUNT, 9001 ) (Path(j), j = i, n)
          ENDDO
        ENDIF
      ENDIF
 9001 FORMAT (10I8)

      END SUBROUTINE check_path

