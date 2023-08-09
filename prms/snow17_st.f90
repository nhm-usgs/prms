      MODULE SNOW_17
      
        TYPE Snow_17_states
        REAL :: WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,AEADJ,EXLAG(7),SNDPT,SNTMP
        END TYPE Snow_17_states
        
        CONTAINS
        
        SUBROUTINE dumpheaders(filenum)
            INTEGER filenum
          WRITE (filenum,'(13A15)') 'WE','NEGHS', 'LIQW', 'TINDEX','ACCMAX','SB','SBAESC', &
                                    'SBWS','STORGE','AEADJ','EXLAG(1)', 'SNDPT','SNTMP'
        END SUBROUTINE dumpheaders
        
        SUBROUTINE dumpstates(states,filenum)
          TYPE(Snow_17_states) :: states
          INTEGER filenum
          WRITE (filenum,'(13E15.6)',advance='no') states%WE,states%NEGHS, states%LIQW, &
              states%TINDEX,states%ACCMAX,states%SB,states%SBAESC, &
              states%SBWS,states%STORGE,states%AEADJ,states%EXLAG(1), states%SNDPT,states%SNTMP
        END SUBROUTINE dumpstates
        
      END MODULE SNOW_17

      MODULE SNOW_17_common

        ! /FSNWUP/
        INTEGER, SAVE :: IUPWE, IUPSC
        ! /SNCO19/
        INTEGER, SAVE :: NEXLAG
        REAL, SAVE :: WE, NEGHS, LIQW, TINDEX, ACCMAX, SB, SBAESC, SBWS, STORGE, AEADJ, EXLAG(7), SNDPT, SNTMP
        ! /SUMS19/
        INTEGER, SAVE :: NDRSP
        REAL, SAVE :: SPX, SSFALL, SRM, SMELT, SMELTR, SROBG, DSFALL, DRAIN, DQNET, DRSL
        ! /SNUP19/
        REAL, SAVE :: MFC, SFALLX, WINDC, SCTOL, WETOL, SNOF, UADJC

      END MODULE SNOW_17_common

      SUBROUTINE snow17_st(NDT, TA, PX, PCTS, RSL, OWE, OSC, PGM, RM, TWE, &
!                             LAST UPDATE: 06/22/95.14:05:09 BY $WC30EA
!
                  COVER, CWE, CAESC, IFUT, IDT, IDN, IMN, IYR, &
!     PREVIOUS TEMPERATURE
                  TPREV, &
!     SNOWFALL (SXFALL),SNOW DENSITY (DS)
                  SXFALL, DS, &
!     SNOW17 PARAMETERS
                  ALAT, SCF, MFMAX, MFMIN, UADJ, SI, NMF, TIPM1, MBASE, &
                  PXTEMP, PLWHC, DAYGM, ADC, &
!     SNOW17 PARAMETERS (DERIVED)
                  PA, LMFV, SMFV, LAEC, NPTAE, AE, SNEW, RMIN, STATES)
     
      USE SNOW_17
      USE PRMS_SNOW, ONLY: Storm_states
      IMPLICIT NONE

! Arguments
      TYPE(Snow_17_states) :: STATES
      INTEGER, INTENT(IN) :: ihru, NDT, IFUT, IDT, IDN, IMN, IYR, LMFV, LAEC, NPTAE
      REAL :: TA, RSL, OWE, OSC, PGM, TWE, COVER, CWE, CAESC, TPREV
      REAL :: SXFALL, DS, ALAT, SCF, UADJ, SI, TIPM1, PXTEMP, PLWHC, DAYGM
      REAL :: PA, SNEW, RMIN
      REAL :: MFMAX,MFMIN,NMF,MBASE
      REAL :: ADC(12), SMFV(12), AE(2,14)
      REAL :: PX(NDT), PCTS(NDT), RM(NDT)
! Local Variables
      INTEGER :: I
      REAL :: TIPM, TEX

      !***********************************************************

      !***********************************************************
      ! Common block assignments
      
      ! No melt correction
      MFC = 1.0
      ! No snow correction
      SFALLX = 1.0
      ! No wind correction 
      WINDC = 1.0 
      !? SCTOL
      !? WETOL
      SCTOL = 0.00001
      WETOL = 0.00001
      ! No snow offset
      SNOF = 4.8 * FLOAT(IDT)/24.0
      ! No wind adjust correction
      UADJC = 1.0
      ! Number of lag values based on number of hours in interval
      NEXLAG = 5/IDT + 2
      ! Set temperature index factor
      TIPM = 1.0-((1.0-TIPM1)**(IDT/6.0))
      ! No observed data used
      IUPWE = 0
      IUPSC = 0
      
      ! Reset states from values after last run
      WE=STATES%WE
      NEGHS=STATES%NEGHS
      LIQW=STATES%LIQW
      TINDEX=STATES%TINDEX
      ACCMAX=STATES%ACCMAX
      SB=STATES%SB
      SBAESC=STATES%SBAESC
      SBWS=STATES%SBWS
      STORGE=STATES%STORGE
      AEADJ=STATES%AEADJ
      TEX=0.0
      DO I=1,NEXLAG
         EXLAG(I)=STATES%EXLAG(I)
         TEX=TEX+EXLAG(I)
      END DO
      SNDPT=STATES%SNDPT
      SNTMP=STATES%SNTMP

      CALL SNOW17(NDT,TA,PX,PCTS,RSL,OWE,OSC,PGM,RM,TWE, &
     +    COVER,CWE,CAESC,IFUT,IDT,IDN,IMN,IYR,TPREV,SXFALL,DS, &
     +    ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE, &
     +    PXTEMP,PLWHC,DAYGM,ADC, PA,LMFV,SMFV,LAEC,NPTAE,AE, &
     +    SNEW,RMIN)

      ! Set states to new values after current run     
      STATES%WE=WE
      STATES%NEGHS=NEGHS
      STATES%LIQW=LIQW
      STATES%TINDEX=TINDEX
      STATES%ACCMAX=ACCMAX
      STATES%SB=SB
      STATES%SBAESC=SBAESC
      STATES%SBWS=SBWS
      STATES%STORGE=STORGE
      STATES%AEADJ=AEADJ
      DO I=1,NEXLAG
         STATES%EXLAG(I)=EXLAG(I)
      END DO
      STATES%SNDPT=SNDPT
      STATES%SNTMP=SNTMP

      END SUBROUTINE snow17_st

!.......................................
!     THIS SUBROUTINE EXECUTES THE 'SNOW-17 ' OPERATIONAL FOR ONE
!        COMPUTATIONAL PERIOD.\
!
!     SNOW17 CODE: VERBATIM
!.......................................
!     SUBROUTINE INITIALLY WRITTEN BY...
!        ERIC ANDERSON - HRL   MAY 1980

!        UPDATED 4/15/00 BY V. KOREN TO ADD SNOW DEPTH CALCULATIONS
!.......................................

      SUBROUTINE SNOW17(NDT,TA,PX,PCTS,RSL,OWE,OSC,PGM,RM,TWE, &
!                             LAST UPDATE: 06/22/95.14:05:09 BY $WC30EA
!
                        COVER,CWE,CAESC,IFUT,IDT,IDN,IMN,IYR, &
!     PREVIOUS TEMPERATURE
                        TPREV, &
!     SNOWFALL (SXFALL),SNOW DENSITY (DS)
                        SXFALL,DS, &
!     SNOW17 PARAMETERS
                        ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE, &
                        PXTEMP,PLWHC,DAYGM,ADC, &
!     SNOW17 PARAMETERS (DERIVED)
                        PA,LMFV,SMFV,LAEC,NPTAE,AE,SNEW,RMIN)

      USE SNOW_17_common
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: NDT, IFUT, IDT, IDN, IMN, IYR, LMFV, LAEC, NPTAE
      REAL :: TA, RSL, OWE, OSC, PGM, TWE, COVER, CWE, CAESC, TPREV
      REAL :: SXFALL, DS, ALAT, SCF, UADJ, SI, TIPM, PXTEMP, PLWHC, DAYGM
      REAL :: PA, SNEW, RMIN
      REAL :: MFMAX, MFMIN, NMF, MBASE
      REAL :: ADC(12), SMFV(12), AE(2,14)
      REAL :: PX(NDT), PCTS(NDT), RM(NDT)

      INTRINSIC :: FLOAT
! Local Variables
      INTEGER :: I, J, N, ITPX, MC, NR
      REAL :: DTA, SBC, FITPX, GM, FNDT, SFNEW, RFMIN, SBCI
      REAL :: PSFALL, PRAIN, PQNET, PSNWRO, PROBG, SXMELT, SXGSLOS, SXRFRZ
      REAL :: PXI, SFALL, CNHSPX, RAIN, RAINM, AESC, PCT, TPX, FRACS, FRACR
      REAL :: TS, TR, GMRO, MELT, ROBG, GMWLOS, GMSLOS, PCNHS, PMELT, CNHS
      REAL :: EA, TAK, TAK4, QN, QE, QH, QNET, WATER, HEAT, LIQWMX, EXCESS, PACKRO, TEX, SLIQ
!***********************************************************************

! VK   CALCULATE AIR TEMPERATURE CHANGE, DTA
      DTA = TA - TPREV
      IF (SNDPT .LE. 0.0) THEN
         DS = 0.1
      ELSE
         DS = 0.1*WE/SNDPT
      ENDIF 
! VK-----------------------------------------------------

!     CONSTANTS
!     IF SNOWFALL EXCEEDS SNEW/HR--TINDEX=TPX
      ! Now getting value from a passed variable
!      SNEW=1.5
!     IF RAIN EXCEEDS RMIN/HR--USE RAIN-ON-SNOW MELT EQUATION
      ! Now getting value from a passed variable
!      RMIN=0.25
!     SBC=STEFAN/BOLTZMAN CONSTANT--MM/(((DEGK/100)**4)*HR)
      SBC=.0612
!.......................................
!.......................................
!     INITIAL VALUES
      ITPX=IDT/NDT
      FITPX=FLOAT( ITPX )
      FNDT = FLOAT( NDT )
      GM=PGM/FNDT
      SFNEW=SNEW*FITPX
      RFMIN=RMIN*FITPX
      SBCI=SBC*FITPX
      MC=0
      PSFALL=0.0
      PRAIN=0.0
      PQNET=0.0
      PSNWRO=0.0
      PROBG=0.0

! VK ----     V.I.K.  04/10/00  --------- 
      SXFALL=0.0
      SXMELT=0.0
      SXGSLOS=0.0
      SXRFRZ=0.0
! --------------------------------------     
!.......................................
!.......................................
!     CYCLE THROUGH THE COMPUTATIONAL PERIOD FOR EACH PRECIPITATION
!        TIME INTERVAL
      DO I=1,NDT
      PXI=PX(I)
      IF((PXI.EQ.0.0).AND.(WE.EQ.0.0)) GO TO 160
      SFALL=0.0
      CNHSPX=0.0
      RAIN=0.0
      RAINM=0.0
      AESC=0.0
      IF(PXI.EQ.0.0) GO TO 110
!.......................................
!     DETERMINE FORM OF PRECIP. AND ACCUMULATE SNOW COVER IF SNOW.
      PCT=PCTS(I)
      IF(PCT.GT.1.0) PCT=1.0
      TPX=TA
      IF(.NOT.(PCT.LT.0.0)) THEN
!
!     FORM OF PRECIP. INPUT.
        FRACS=PCT
        FRACR=1.0-PCT
        GO TO 105
      ENDIF
      IF (LAEC.NE.0) THEN
        DRSL=DRSL+RSL
        NDRSP=NDRSP+1
!
!     FORM OF PRECIP. BASED ON RAIN-SNOW ELEVATION
        IF (.NOT.(RSL.GT.AE(1,1))) THEN
          FRACR=0.0
        ELSE
          FRACR=1.0
          DO J=2,NPTAE
            IF (RSL.GT.AE(1,J)) CYCLE
            FRACR=AE(2,J-1)+(AE(2,J)-AE(2,J-1)) * ((RSL-AE(1,J-1))/(AE(1,J)-AE(1,J-1)))
            EXIT
          END DO
        ENDIF
        FRACS=1.0-FRACR
        GO TO 105
      ENDIF
!
!     FORM OF PRECIP. BASED ON TEMPERATURE.
      IF(TPX.GT.PXTEMP) GO TO 103
!
!     SNOW
      FRACS=1.0
      FRACR=0.0
      GO TO 105
!
!     RAIN
  103 FRACS=0.0
      FRACR=1.0
  105 IF(FRACS.EQ.0.0) GO TO 109
!
!     ACCUMULATE SNOWFALL
      TS=TPX
      IF(TS.GT.0.0) TS=0.0
      SFALL=PXI*FRACS*SCF
      IF(IFUT.EQ.0) SFALL=SFALL*SFALLX
      SPX=SPX+SFALL
      SSFALL=SSFALL+SFALL
      DSFALL=DSFALL+SFALL
      PSFALL=PSFALL+SFALL
      IF (.NOT.((WE+LIQW).LT.SBWS)) THEN
        SBWS=SBWS+0.75*SFALL
        IF((SFALL.GE.SNOF).AND.(SB.GT.WE+LIQW)) SB=WE+LIQW
      ELSE
        IF(SFALL.GE.SNOF) SBWS=WE+LIQW+0.75*SFALL
      ENDIF
      WE=WE+SFALL  
!     IF WE+LIQW.GE.3*SB, ASSUME NEW ACCUMULATION PERIOD
      IF(.NOT.(WE+LIQW.LT.3.0*SB)) THEN
        ACCMAX=WE+LIQW
        AEADJ=0.0
      ENDIF
      CNHSPX=-TS*SFALL/160.0
      IF(SFALL.GT.SFNEW) TINDEX=TS
!
!     RAINFALL AND RAIN MELT.
  109 RAIN=PXI*FRACR
      SPX=SPX+RAIN
      PRAIN=PRAIN+RAIN
      IF(WE.EQ.0.0) GO TO 160
      DRAIN=DRAIN+RAIN
      TR=TPX
      IF(TR.LT.0.0) TR=0.0
      RAINM=0.0125*RAIN*TR
!.......................................
!     MELT AT GROUND-SNOW INTERFACE
  110 IF(WE.GT.GM) GO TO 111
      GMRO=WE+LIQW
      MELT=0.0
      ROBG=RAIN
      RAIN=0.0
      SROBG=SROBG+ROBG
      GO TO 150
  111 GMWLOS=(GM/WE)*LIQW
      GMSLOS=GM
!.......................................
!     COMPUTE SURFACE ENERGY EXCHANGE FOR THE COMPUTATIONAL PERIOD BASED
!        ON 100 PERCENT COVER AND NON-RAIN CONDITIONS -
      PCNHS = 0.0
      PMELT = 0.0
      IF(MC.EQ.1) GO TO 115
      CALL MELT19(IDN,IMN,ALAT,TA,PMELT,MFMAX,MFMIN,MBASE,TINDEX,TIPM, PCNHS,NMF,LMFV,SMFV)
      MC=1
!.......................................
!     DETERMINE MELT FOR THE TIME INTERVAL - SURFACE ENERGY EXCHANGE
!        IS UNIFORM DURING THE COMPUTATIONAL PERIOD.
  115 CNHS=PCNHS/FNDT
      NR=1
      IF(RAIN.GT.RFMIN) GO TO 120
!
!     NON-RAIN OR LIGHT DIZZLE INTERVAL
      MELT=PMELT/FNDT
      MELT=MELT*MFC
      MELT=MELT+RAINM
      GO TO 130
!
!     RAIN INTERVAL.
  120 EA=2.7489E8*EXP(-4278.63/(TA+242.792))
!     ASSUME 90 PERCENT RELATIVE HUMIDITY DURING RAIN-ON-SNOW
      EA=0.90*EA
      TAK=(TA+273)*0.01
      TAK4=TAK*TAK*TAK*TAK
      QN=SBCI*(TAK4-55.55)
!
!     UADJC IS UADJ MOD MULTIPLIER added by mike smith 2/12/97
      
      QE=8.5*(EA-6.11)*UADJ*UADJC
        IF(IFUT.EQ.0) QE=QE*WINDC
      QH=7.5*0.000646*PA*UADJ*TA*UADJC
        IF(IFUT.EQ.0) QH=QH*WINDC
      MELT=QN+QE+QH+RAINM
      IF(MELT.LT.0.0) MELT=0.0
      NR=0
  
!.......................................
!     COMPUTE AREAL EXTENT OF SNOW COVER BASED ON CONDITIONS AT THE
!        BEGINNING OF THE TIME INTERVAL ADJUSTED FOR NEW SNOWFALL.
!
  130 CALL AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
!
!     ADJUST VALUES FOR AESC.
      IF(.NOT.(AESC.EQ.1.0)) THEN
        MELT=MELT*AESC
        CNHS=CNHS*AESC
        GMWLOS=GMWLOS*AESC
        GMSLOS=GMSLOS*AESC
!.......................................
!     COMPUTE RAIN FALLING ON BARE GROUND
        ROBG=(1.0-AESC)*RAIN
        RAIN=RAIN-ROBG
      ELSE
        ROBG=0.0
      ENDIF
!.......................................
!     COMPUTE SUM AND CHECK CNHS.
      SROBG=SROBG+ROBG
      IF((CNHS+NEGHS).LT.0.0) CNHS=-1.0*NEGHS

! VK  CUMULATE FOR TIME PERIOD
      SXFALL=SXFALL+SFALL
      SXMELT=SXMELT+MELT
      SXGSLOS=SXGSLOS+GMSLOS
! VK -------------------------

!.......................................
!     ADJUST WE FOR SURFACE AND GROUND MELT
!     GROUND MELT
      WE=WE-GMSLOS
      LIQW=LIQW-GMWLOS
      GMRO=GMSLOS+GMWLOS
!
!     SURFACE MELT
      IF(MELT.LE.0.0) GO TO 137
      IF(MELT.LT.WE) GO TO 136
      MELT=WE+LIQW
      QNET=MELT
      DQNET=DQNET+QNET
      IF(NR.EQ.1) SMELT=SMELT+MELT
      IF(NR.EQ.0) SMELTR=SMELTR+MELT
      GO TO 150
  136 WE=WE-MELT
!     QNET=NET SURFACE ENERGY EXCHANGE IN MILLIMETERS WE.
  137 QNET=MELT-CNHS-CNHSPX
      DQNET=DQNET+QNET
      PQNET=PQNET+QNET
      IF(NR.EQ.1) SMELT=SMELT+MELT
      IF(NR.EQ.0) SMELTR=SMELTR+MELT
!.......................................
!     PERFORM HEAT AND WATER BALANCE FOR THE SNOW COVER.
      WATER=MELT+RAIN
      HEAT=CNHS+CNHSPX
      LIQWMX=PLWHC*WE
      NEGHS=NEGHS+HEAT
!     TEMPERATURE OF SNOW CAN NOT BE BELOW-52.8 DEGC
      IF(NEGHS.LT.0.0) NEGHS=0.0
      IF(NEGHS.GT.0.33*WE) NEGHS=0.33*WE
      IF((WATER+LIQW).LT.(LIQWMX+NEGHS+PLWHC*NEGHS)) GO TO 140
!
!     EXCESS WATER EXISTS.
      EXCESS=WATER+LIQW-LIQWMX-NEGHS-PLWHC*NEGHS
      LIQW=LIQWMX+PLWHC*NEGHS
      WE=WE+NEGHS
      NEGHS=0.0
      GO TO 145
  140 IF(WATER.LT.NEGHS) GO TO 141
!
!     WATER EXCEEDS NEGHS - LIQUID WATER CONTENT IS INCREASED.
      LIQW=LIQW+WATER-NEGHS
      WE=WE+NEGHS

! VK  CUMULATE REFROZEN WATER
      SXRFRZ=SXRFRZ+NEGHS
      
      NEGHS=0.0
      EXCESS=0.0
      GO TO 145
!
!     ALL WATER IS REFROZEN IN THE SNOW COVER.
  141 WE=WE+WATER
      NEGHS=NEGHS-WATER
      EXCESS=0.0

! VK  CUMULATE REFROZEN WATER
      SXRFRZ=SXRFRZ+WATER
      
!     IF NO NEGATIVE HEAT - TINDEX MUST BE 0.0.
  145 IF(NEGHS.EQ.0.0) TINDEX=0.0
!.......................................
!     ROUTE EXCESS WATER THROUGH THE SNOW COVER.
      CALL ROUT19(ITPX,EXCESS,WE,AESC,STORGE,NEXLAG,EXLAG,PACKRO)
!.......................................
!     ADD GROUNDMELT RUNOFF TO SNOW COVER OUTFLOW.
      PACKRO=PACKRO+GMRO
      GO TO 190
!.......................................
!     SNOW GONE - SET ALL CARRYOVER TO NO SNOW CONDITIONS.
  150 TEX=0.0
      DO N=1,NEXLAG
        TEX=TEX+EXLAG(N)
      END DO
      PACKRO=GMRO+MELT+TEX+STORGE+RAIN
      CALL ZERO19
      AESC=0.0
      GO TO 190
!.......................................
!     NO SNOW COVER - NO NEW SNOWFALL.
  160 ROBG=PXI
      PACKRO=0.0
      AESC=0.0
      SROBG=SROBG+ROBG
!.......................................
!     COMPUTE RAIN+MELT
  190 RM(I)=PACKRO+ROBG
      SRM=SRM+RM(I)
      PSNWRO=PSNWRO+PACKRO
      PROBG=PROBG+ROBG
      END DO
!     END OF COMPUTATIONAL PERIOD
!.......................................
!.......................................
!     SET SIMULATED AESC AND TOTAL WATER-EQUIVALENT.
      TEX=0.0
      DO N=1,NEXLAG
        TEX=TEX+EXLAG(N)
      END DO

! VK --  V.KOREN  04/05/00   ---------------------------------
! VK   CALL SNDEPTH SUBROUTINE TO CALCULATE SNOW DEPTH
      IF(WE .GT. 0.) THEN
       SLIQ=LIQW+TEX+STORGE
       SXFALL=SXFALL-SXMELT
       IF(SXFALL .LT. 0.) SXFALL=0.0
       CALL SNDEPTH(WE, SLIQ, SXFALL, SXGSLOS, SXRFRZ, TA, DTA, IDT, SNDPT, DS, SNTMP)
      ELSE
       SNDPT=0.
       SNTMP=0.
       DS=0.1
      ENDIF
! VK ---------------------------------------------------------      

      TWE=WE+LIQW+TEX+STORGE
      IF ( .NOT.(TWE.EQ.0.0) ) THEN
!
!     COMPUTE AREAL EXTENT BASED ON CONDITIONS AT THE END OF THE PERIOD.
      CALL AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
      COVER=AESC
      END IF
!
!     STORE VALUES SO COMPUTED VALUES WILL BE AVAILABLE FOR PRINTOUT
!        EVEN IF UPDATING OCCURS.
      CWE=TWE
      CAESC=COVER
!.......................................
!     UPDATING SECTION
      IF((OWE.LT.0.0).AND.(OSC.LT.0.0)) GO TO 280
      CALL UPDT19(OWE,OSC,TWE,COVER,IUPWE,IUPSC,WETOL,SCTOL,SI,ADC)
  280 CONTINUE
!.......................................
      END SUBROUTINE SNOW17
      

!.......................................
!     SUBROUTINES COMPUTES SURFACE MELT BASED ON 100 PERCENT
!        SNOW COVER AND NON-RAIN CONDITIONS.
!.......................................
!     INITIALLY WRITTEN BY...
!        ERIC ANDERSON - HRL   MAY 1980
!.......................................

      SUBROUTINE MELT19(IDN,IMN,ALAT,TA,MELT,MFMAX,MFMIN,MBASE,TINDEX,TIPM,CNHS,NMF,LMFV,SMFV)
! Arguments
      REAL MELT,MFMAX,MFMIN,MBASE,NMF,NMRATE,ALAT,TA,TINDEX,TIPM
      INTEGER :: MMD(12), LMFV, IDN, IMN
      REAL :: SMFV(12)
      DATA MMD/301,332,361,26,56,87,117,148,179,209,240,270/
      INTRINSIC :: FLOAT
! Local Variables
      INTEGER :: DAYN, MB, MA, MD, ND
      REAL :: CNHS, DIFF, FMD, FND, ADJMF, MF
      REAL :: X, XX, RATIO, TMX, TSUR, TNMX
!.......................................
!     INITIAL VALUES
      CNHS=0.0
      MELT=0.0
      DIFF=MFMAX-MFMIN
      DAYN=IDN
      IF(.NOT.(LMFV.EQ.0)) THEN
!.......................................
!     USER SPECIFIED MELT FACTOR VARIATION
        MB=IMN
      IF (IDN.GT.11.AND.IDN.LT.MMD(IMN)) MB=MB-1
      IF (MB.EQ.0) MB=12
      MA=MB+1
      IF (MA.EQ.13) MA=1
      MD=MMD(MA)-MMD(MB)
      IF (MD.LT.0) MD=MD+366
      ND=IDN-MMD(MB)
      IF (ND.LT.0) ND=ND+366
      FMD=MD
      FND=ND
      ADJMF= SMFV(MB)+(FND/FMD)*(SMFV(MA)-SMFV(MB))
      MF= MFMIN+ADJMF*DIFF
      GOTO 125
      END IF
      IF(ALAT.LT.54.0) GO TO 120
!.......................................
!     MELT FACTOR VARIATION FOR ALASKA.
      IF(IDN.GE.275) GO TO 102
      IF(IDN.GE.92) GO TO 101
      X=(91.0+DAYN)/183.0
      GO TO 105
  101 X=(275.0-DAYN)/(275.0-92.0)
      GO TO 105
  102 X=(DAYN-275.0)/(458.0-275.0)
  105 XX=(SIN(DAYN*2.0*3.1416/366.0)*0.5)+0.5
      IF(X.LE.0.48) GO TO 111
      IF(X.GE.0.70) GO TO 112
      ADJMF=(X-0.48)/(0.70-0.48)
      GO TO 110
  111 ADJMF=0.0
      GO TO 110
  112 ADJMF=1.0
  110 MF=(XX*ADJMF)*DIFF+MFMIN
      GO TO 125
!.......................................
!     MELT FACTOR VARIATION FOR THE LOWER 48.
  120 MF=(SIN(DAYN*2.0*3.1416/366.0)*DIFF*0.5)+(MFMAX+MFMIN)*0.5
  125 RATIO=MF/MFMAX
!.......................................
!     COMPUTE MELT AND NEGATIVE HEAT EXCHANGE INDEX TEMPERATURES.
      TMX=TA-MBASE
      IF(TMX.LT.0.0) TMX=0.0
      TSUR=TA
      IF (TSUR.GT.0.0) TSUR=0.0
      TNMX=TINDEX-TSUR
!.......................................
!     NEGATIVE HEAT EXCHANGE
      NMRATE=RATIO*NMF
      CNHS=NMRATE*TNMX
!
!     UPDATE TINDEX
      TINDEX=TINDEX+TIPM*(TA-TINDEX)
      IF(TINDEX.GT.0.0) TINDEX=0.0
      IF(TMX>0.0) THEN
!.......................................
!     SURFACE MELT.
      MELT=MF*TMX
      END IF
!.......................................
      END SUBROUTINE MELT19
      
!.......................................
!     THIS SUBROUTINE COMPUTES THE AREAL EXTENT OF SNOW COVER USING THE
!        AREAL DEPLETION CURVE FOR THE 'SNOW-17 ' OPERATION.
!.......................................
!     SUBROUTINE INITIALLY WRITTEN BY...
!        ERIC ANDERSON - HRL   MAY 1980
!.......................................

      SUBROUTINE AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
      USE SNOW_17_common, ONLY: SNOF
      IMPLICIT NONE
! Arguments
      REAL WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,AEADJ,AESC
      REAL :: ADC(11)
! Local Variables
      INTEGER :: N
      REAL :: TWE, AI, R, FN
      INTRINSIC :: INT
!.......................................
      TWE=WE+LIQW
      IF(TWE.GT.ACCMAX) ACCMAX=TWE
      IF (TWE.GE.AEADJ) AEADJ=0.0
      AI=ACCMAX
      IF(ACCMAX.GT.SI)AI=SI
      IF (AEADJ.GT.0.0) AI=AEADJ
      IF(TWE.GE.AI) GO TO 105
      IF(TWE.LE.SB) GO TO 110
      IF(TWE.GE.SBWS) GO TO 115
      AESC=SBAESC+((1.0-SBAESC)*((TWE-SB)/(SBWS-SB)))
      GO TO 120
  110 R=(TWE/AI)*10.0+1.0
      N=INT( R )
      FN=N
      R=R-FN
      AESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(AESC.GT.1.0) AESC=1.0
      SB=TWE+SNOF
      SBWS=TWE
      SBAESC=AESC
      GO TO 120
  105 SB=TWE
      SBWS=TWE
  115 AESC=1.0
  120 IF(AESC.LT.0.05) AESC=0.05
      IF(AESC.GT.1.0) AESC=1.0
!.......................................
      END SUBROUTINE AESC19
      
      SUBROUTINE ROUT19(IT,EXCESS,WE,AESC,STORGE,NEXLAG,EXLAG,PACKRO)
!.......................................
!     THIS SUBROUTINE ROUTES EXCESS WATER THROUGH THE SNOW COVER FOR
!        THE 'SNOW-17 ' OPERATION.
!.......................................
!     SUBROUTINE INITIALLY WRITTEN BY...
!        ERIC ANDERSON - HRL   MAY 1980
!.......................................
      IMPLICIT NONE
! Arguments
      INTEGER :: IT, I,NEXLAG
      REAL :: EXCESS,WE,AESC,STORGE,PACKRO
      REAL :: EXLAG(7)
! Local Varaibles
      INTEGER :: N, L1, L2
      REAL :: FIT, CL, FI, TERM, FLAG, POR1, POR2, FN, EL, ELS, WES, ENDL1, R1, OS
      INTRINSIC :: FLOAT, INT
!.......................................
!     INITIAL VALUES
      FIT=FLOAT( IT )
      PACKRO=0.0
      CL=0.03*FIT/6.0
!.......................................
!     LAG EXCESS WATER FIRST - FUNCTION OF EXCESS AND WE.
      IF(EXCESS.EQ.0.0) GO TO 150
      IF(EXCESS.LT.0.1) GO TO 120
      IF(WE.LT.1.0) GO TO 120
!
!     COMPUTE LAG IN HOURS AND PRORATE EXCESS.
      N=INT( ((EXCESS*4.0)**0.3)+0.5 )
      IF(N.EQ.0) N=1
      FN=FLOAT( N )
      DO I=1,N
      FI=I
      TERM=CL*WE*FN/(EXCESS*(FI-0.5))
      IF(TERM.GT.150.0) TERM=150.0
      FLAG=5.33*(1.0-EXP(-TERM))
      L2=INT( (FLAG+FIT)/FIT+1.0 )
      L1=L2-1
      ENDL1=FLOAT( L1*IT )
      POR2=(FLAG+FIT-ENDL1)/FIT
      POR1=1.0-POR2
      EXLAG(L2)=EXLAG(L2)+POR2*EXCESS/FN
      EXLAG(L1)=EXLAG(L1)+POR1*EXCESS/FN
      END DO
      GO TO 150
!
!     EXCESS OR WE SMALL, THUS NO LAG.
  120 EXLAG(1)=EXLAG(1)+EXCESS
!.......................................
!     ATTENUATE LAGGED EXCESS WATER - FUNCTION OF STORGE AND WE.
  150 IF((STORGE+EXLAG(1)).EQ.0.0) GO TO 190
      IF((STORGE+EXLAG(1)).GE.0.1) GO TO 160
!
!     NO ATTENUATION
      PACKRO=STORGE+EXLAG(1)
      STORGE=0.0
      GO TO 190
!
!     EFFECT OF ATTENUATION COMPUTED USING A ONE-HOUR TEME STEP.
  160 EL=EXLAG(1)/FIT
      ELS=EL/(25.4*AESC)
      WES=WE/(25.4*AESC)
      TERM=500.0*ELS/(WES**1.3)
      IF(TERM.GT.150.0) TERM=150.0
      R1=1.0/(5.0*EXP(-TERM)+1.0)
      DO 170 I=1,IT
      OS=(STORGE+EL)*R1
      PACKRO=PACKRO+OS
      STORGE=STORGE+EL-OS
  170 CONTINUE
      IF(STORGE.GT.0.001) GO TO 190
      PACKRO=PACKRO+STORGE
      STORGE=0.0
!
!     DOWNSHIFT WATER IN EXLAG().
  190 DO 195 I=2,NEXLAG
      EXLAG(I-1)=EXLAG(I)
  195 CONTINUE
      EXLAG(NEXLAG)=0.0
!.......................................
      END SUBROUTINE ROUT19      

!.......................................
!     THIS SUBROUTINE SETS ALL CARRYOVER VALUES TO NO SNOW CONDITIONS
!        FOR THE 'SNOW-17 ' OPERATION.
!.......................................
!     SUBROUTINE INITIALLY WRITTEN BY...
!        ERIC ANDERSON - HRL   MAY 1980

!    MODIFIED 4/20/00 BY V. KOREN TO ADD TWO MORE STATES: SNDPT & SNTMP
!.......................................

      SUBROUTINE ZERO19
      USE SNOW_17_common, ONLY: WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE, &
                                AEADJ,NEXLAG,EXLAG,SNDPT,SNTMP
      IMPLICIT NONE
! Local Variables
      INTEGER :: N
!
!.......................................
      WE=0.0
      NEGHS=0.0
      LIQW=0.0
      TINDEX=0.0
      ACCMAX=0.0
      SB=0.0
      SBAESC=0.0
      SBWS=0.0
      STORGE=0.0
      AEADJ=0.0

! VK  ADDED TWO MORE STATES
      SNDPT=0.0
      SNTMP=0.0
            
      DO N=1,NEXLAG
        EXLAG(N)=0.0
      END DO
!.......................................
      END SUBROUTINE ZERO19
      
! **  SUBROUTINE CALCULATES SNOW DEPTH
!
      SUBROUTINE SNDEPTH(WE,SLIQ,DFALL,SGSLOS,SRFRZ, TA,DTA,IDT,SH,DS,TSNOW)
      IMPLICIT NONE
! Arguments
      INTEGER :: IDT
      REAL ::WE, SLIQ, DFALL, SGSLOS, SRFRZ, TA, DTA, SH, DS, TSNOW
! Local Variables
      REAL :: DT, DHC, SDN, TSNEW, SHN, DSN
      INTRINSIC FLOAT

! **  WE     - WATER EQUIVALENT, MM
! **  SLIQ   - LIQUID WATER CONTENT, MM
! **  DFALL  - NEW SNOWFALL, MM
! **  SGSLOS - GROUND SNOW MELT, MM
! **  SRFRZ  - REFROZEN MELT/RAIN WATER, MM
! **  TA     - AIR TEMPERATURE, C
! **  DTA    - AIR TEMPERATURE CHANGE FOR THE 6HR TIME INTERVAL  
! **  IDT    - TIME STEP, HR
! **  SH     - SNOW DEPTH, CM
! **  DS     - SNOW DENSITY, G/CM3
! **  TSNOW  - AVERAGE SNOW TEMPERATURE, CELSIUS

      DT=FLOAT( IDT )
!  ADJUST SNOW DENSITY DUE TO SNOWFALL
      DHC=0.
      SDN=0.0
      TSNEW=TA
      IF(DFALL .GT. 0.) THEN
!  CALCILATE NEW SNOW FALL DEPTH/DENSITY
       CALL SNEW(TA,DFALL,DHC,SDN)
       CALL SNOWT(DHC,SDN,DFALL,0.,TA,DTA,TSNEW,0.)
       CALL SNOWPACK(DFALL,DT,DHC,SDN,0.,0.,0.,TSNEW)
      ENDIF
       
      IF(SH .GT. 0.0001) THEN
!  CALCULATE OLD SNOW COMPACTION/METAMORPHISM
       SHN=DHC+SH
       DSN=(SDN*DHC+DS*SH)/SHN
       CALL SNOWT(SHN,DSN,WE,SLIQ,TA,DTA,TSNOW,DHC)
       CALL SNOWPACK(WE,DT,SH,DS,SLIQ,DFALL,SRFRZ,TSNOW)
!  ACCOUNT FOR GROUND SNOW MELT
       IF(SGSLOS .GT. 0.) SH=SH-0.1*SGSLOS/DS
       IF(SH .LT. 0.) SH=0.0
!  COMBINE NEW SNOW FALL AND OLD SNOWPACK
       TSNOW=(TSNOW*SH+TSNEW*DHC)/(SH+DHC)
       SH=SH+DHC
       DS=0.1*WE/SH
      ELSE
!  THERE WAS NO SNOWPACK BEFORE THIS SNOW FALL
       SH=DHC
       DS=SDN
       IF(SGSLOS .GT. 0.) SH=SH-0.1*SGSLOS/DS
       IF(SH .LT. 0.) SH=0.0
       TSNOW=TSNEW
      ENDIF 
!      print*,'SNDPT,DS,TSNOW,WE,SLIQ,DFALL,SRFRZ,TA,DTA=',SH,DS,TSNOW, &
!     &        WE,SLIQ,DFALL,SRFRZ,TA,DTA

      END SUBROUTINE SNDEPTH

!.......................................
!     THIS SUBROUTINE UPDATES WATER-EQUIVALENT AND SNOW COVER
!       BASED ON OBSERVATIONS.  AFFECTED CARRYOVER VALUES ARE
!       ADJUSTED.
!.......................................
!     SUBROUTINE WRITTEN BY....
!           ERIC ANDERSON - HRL - DECEMBER 1982

! VK        MODIFIED 4/00 BY V. KOREN: TWO NEW STATES ADDED
!.......................................

      SUBROUTINE UPDT19(OWE,OSC,TWE,COVER,IUPWE,IUPSC,WETOL,SCTOL,SI,ADC)
      USE SNOW_17_common, ONLY: WE, LIQW, ACCMAX, SB, SBAESC, SBWS, AEADJ
      IMPLICIT NONE
! Arguments
      INTEGER :: IUPWE, IUPSC
      REAL :: OWE, OSC, TWE, COVER, WETOL, SCTOL, SI
      REAL :: ADC(11)
! Local Variables
      INTRINSIC :: ABS
!.......................................
!
!     UPDATE WATER-EQUIVALENT FIRST IF OBSERVED WE AVAILABLE.
      IF(OWE.LT.0.0) GO TO 250
      IF(IUPWE.EQ.0) GO TO 250
      IF(OWE.LT.1.0) OWE=0.0
!
!     UPDATE WE IF DIFFERENCE EXCEEDS TOLERANCE.
      IF(ABS(OWE-TWE).LE.(WETOL*TWE)) GO TO 250
      TWE=OWE
!
!     UPDATE CARRYOVER VALUES TO BE CONSISTENT WITH THIS CHANGE.
      IF(TWE.GT.0.0) GO TO 235
!
!     NO SNOW REMAINS AFTER UPDATE - CANNOT UPDATE AREAL SNOW
!        COVER EVEN IF OBSERVED VALUE EXISTS.
      CALL ZERO19
      COVER=0.0
      GO TO 290
!
!     ADJUST CARRYOVER VALUES BASED ON NEW WATER-EQUIVALENT.
  235 CALL ADJC19(TWE,SI,ADC)
!
!     COMPUTE AREAL EXTENT OF SNOW BASED ON NEW WE.
      CALL AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,COVER)
!
!     UPDATE AREAL EXTENT OF SNOW-COVER IF OBSERVED AESC AVAILABLE
!         AND A SNOW COVER EXISTS.
  250 IF(OSC.LT.0.0) GO TO 290
      IF(IUPSC.EQ.0) GO TO 290
      IF (TWE.LT.0.1) GO TO 290
!
!     UPDATE AESC IF DIFFERENCE EXCEEDS TOLERANCE - ALSO CHANGE AEADJ.
      IF(ABS(OSC-COVER).LE.SCTOL) GO TO 290
      IF(OSC.GT.0.05) GO TO 260
      IF(OSC.EQ.0.0) GO TO 255
!      WRITE(IPR,907)
!  907 FORMAT('0**WARNING** AREAL EXTENT OF SNOW IS NOT UPDATED FOR'// &
!             ' OBSERVED AREAL EXTENT VALUES IN THE RANGE GT 0.0 TO LE 0.05.)
!      CALL WARN
      GO TO 290
!
!     NO SNOW REMAINS
  255 CALL ZERO19
      COVER=0.0
      TWE=0.0
      GO TO 290
!
!     ADJUST CARRYOVER FOR OBS. AREAL EXTENT.
  260 IF(OSC.GT.1.0) OSC=1.0
      COVER=OSC
      CALL AECO19 (COVER,TWE,SI,ADC)
!     ALL UPDATING COMPLETED.
!.......................................
 290  RETURN
      END SUBROUTINE UPDT19
      
!  CALCULATING SNOW DEPTH AND DENSITITY OF THE NEW SNOWFALL
!      T   - AIR TEMPERATURE, C
!      P   - NEW SNOWFALL, MM 
!      HCN - SNOW DEPTH, CM
!      DHN - SNOW DENSITY
!   NEW VALUES OF SNOW DEPTH & DENSITY WILL BE RETURNED   

      SUBROUTINE SNEW ( T,P,HCN,DHN )
     
! ----------------------------------------------------------------------
! ***  CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE      **
! ***  EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED
! ***  AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE, 
! ***  VEMADOLEN, SWEDEN, 1980, 172-177pp.
!-----------------------------------------------------------------------
      IMPLICIT NONE
! Arguments
      REAL :: T, P, HCN, DHN
! Local Variables
      REAL :: PX
!-----------------------------------------------------------------------
!  CONVERT IN CALCULATION UNITS
      PX=0.1*P
      
      IF(T.LE.-15.) THEN
        DHN=0.05 
      ELSE                                                      
        DHN=0.05+0.0017*(T+15.)**1.5
      ENDIF

! **   CALCULATE CHANGE OF SNOW DEPTH       

      HCN=PX/DHN
      
      END SUBROUTINE SNEW    
      
!  SUBROUTINE CALCULATES AVERAGE SNOW TEMPERATURE
!
      SUBROUTINE SNOWT(SH,DS,WE,SLIQ,TA,DTA,TSNOW,DHC)

      IMPLICIT NONE
! Arguments
      REAL :: SH, DS, WE, SLIQ, TA, DTA, TSNOW, DHC
! Local Variables
      REAL :: SHX, DHCX, STOT, DST, SL, FL, SC, ALP
!  SPECIFIC HEAT CAPACITIES OF ICE, WATER, AND AIR
      REAL, PARAMETER :: CICE=2.1E06, CH2O=4.2E06, CAIR=1E03
!  HEAT WAVE LENGTH, IN SECONDS
      REAL, PARAMETER :: WAVEL=43200.0

       SHX=0.01*SH
       DHCX=0.01*DHC
       STOT=WE+SLIQ
       DST=0.1*STOT/SH 
       SL=0.0442*EXP(5.181*DST)
       FL=SLIQ/STOT
       SC=CICE*DS+CAIR*(1.-DS-FL)+CH2O*FL
       ALP=SQRT(3.14*SC/(WAVEL*SL))
       IF(DHC .GT. 0.) THEN
        TSNOW=TSNOW+DTA*((EXP(-ALP*DHCX)-EXP(-ALP*SHX)) / (ALP*(SHX-DHCX)))
       ELSE
        TSNOW=TSNOW+DTA*((1.-EXP(-ALP*SHX))/(ALP*SHX))
       ENDIF 
      IF(TSNOW .GT. 0.) TSNOW=0.
      
      END SUBROUTINE SNOWT

! ****************************************************************
! **  SUBROUTINE TO CALCULATE SNOW COMPACTION AND METAMORPHISM ***
! **  EQUATIONS OF INCREASING OF SNOW DENSITY WERE OBTAINED AS ***
! **  AN APPROXIMATE SOLUTIONS OF E. ANDERSON DIFFERENTIAL     ***
! **  EQUATIONS (3.29) AND (3.30), (3.31), NOAA TECHNICAL      *** 
! **  REPORT NWS 19, by   VICTOR KOREN   03/25/95              ***
! ****************************************************************

      SUBROUTINE SNOWPACK ( W,DTS,HC,DS,SLIQ,DFALL,SRFRZ,TSNOW)

! **************************************************************
!  W      IS A WATER EQUIVALENT OF SNOW, IN MM               ***
!  DTS    IS A TIME STEP, IN HOURS                           ***
!  HC     IS A SNOW DEPTH, IN CM                              ***
!  DS     IS A SNOW DENSITY, IN G/CM3                        ***
!  SLIQ   IS A LIQUID WATER CONTENTS, MM                     ***
!  TSNOW  IS AN AVERAGE SNOW TEMPERATURE, CELSIUS            ***
!      SUBROUTINE WILL RETURN NEW VALUES OF H AND DS         ***
! **************************************************************

      IMPLICIT NONE
! Arguments
      REAL :: W, DTS, HC, DS, SLIQ, DFALL, SRFRZ, TSNOW
! Local Variables
      REAL :: DSC, A, C, DSM, WXS, DSX, DWX
      DOUBLE PRECISION :: B, WX
      INTRINSIC :: DBLE, DEXP, EXP, SNGL

!  CONSTANTS OF SNOW COMPACTION/METAMORPHISM MODELS. FROM ANDERSON
!  TECHNICAL REPORT 19:
!    C1 IN 1/(CM HR), C2 IN CM3/G, C3 IN 1/HR, C4 IN 1/DEGREE C,
!    C5 DIMENSIONLESS, RDS IN G/CM3, CX IN CM3/G
!    ONLY PARAMETER C5 WAS CHANGED FROM 0.15 TO 0.20 BASED ON
!    RESULTS OF SIMULATIONS USING NOAA-ARS COOPERATIVE SNOW 
!    RESEARCH PROJECT DATA 
      REAL, PARAMETER :: C1=0.01, C2=21.0
      REAL, PARAMETER :: C3=0.01, C4=0.04, RDS=0.20, C5=2., CX=46.

! **  CONVERSION INTO SIMULATION UNITS   ************************* 
      WX=DBLE( W*0.1 )

! **  CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
! **  C1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR)) 
! **  C2 IS A CONSTANT (CM3/G) Kojima estimated as 21 cms/g
!   
      DSC=1.
      IF(WX.GT.1.0D-2) THEN
        B=DBLE( DTS*C1*EXP(0.08*TSNOW-C2*DS) )
        DSC=SNGL( (DEXP(B*WX)-1.0D0)/(B*WX) )
      ENDIF

! **  CALCULATE THE DENSITY CHANGE AS A RESULT OF SNOW METAMORPHISM
!     C3 IS THE FRACTIONAL SETTLING RATE AT 0 DEGREE FOR DENSITIES 
!        LESS THAN THRESHOLD DENSITY, RDS
!     C4 IS A CONSTANT 
!     C5 IS AN EMPIRICAL ADJUSTMENT FACTOR THAT ACCOUNTS FOR MELT
!        METAMORPHISM
! 
      A=C3
      IF(SLIQ .GT. 0.) A=A*C5

      C=C4*TSNOW
      IF(DS .GT. RDS) C=C-CX*(DS-RDS)
      DSM=EXP(A*DTS*EXP(C))

! **  NEW SNOW DENSITY AS A RESULT OF COMPACTION-METAMORPHISM 
      DSX=DS*DSC*DSM
      IF(DSX .GT. 0.45) DSX=0.45
      IF(DSX .LT. 0.05) DSX=DS
      DS=DSX
      WXS = SNGL ( WX )
      DWX=WXS-0.1*(DFALL+SRFRZ)
      IF(DWX .GT. 0.) THEN
       HC=DWX/DS
      ELSE 
       IF(WXS .GT. 0.) THEN
        HC=WXS/DSX
       ELSE
        HC=0.0
       ENDIF  
      ENDIF

      END SUBROUTINE SNOWPACK
      
!.......................................
!     THIS SUBROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A CHANGE
!        IN THE TOTAL WATER-EQUIVALENT.  USED IN THE 'SNOW-17 '
!        OPERATION.
!.......................................
!     SUBROUTINE INITIALLY WRITTEN BY...
!        ERIC ANDERSON - HRL   MAY 1980

! VK     MODIFIED 4/00 BY V. KOREN: TWO NEW STATES ADDED
!.......................................

      SUBROUTINE ADJC19(TWE,SI,ADC)
      USE SNOW_17_common, ONLY: WE, LIQW, ACCMAX, SB, SBAESC, SBWS, STORGE, &
          AEADJ ,NEXLAG, EXLAG, SNDPT, SNTMP, SNOF
      IMPLICIT NONE
! Arguments
      REAL :: TWE, SI, ADC(11)
! Local Variables
      INTEGER :: N
      REAL :: OLDWE, AI, OLDAI, TEX, FREEW, QUAL, SWE, R, FN
!.......................................
      OLDWE=WE+LIQW
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      OLDAI=AI
      TEX=0.0
      DO N=1,NEXLAG
        TEX=TEX+EXLAG(N)
      ENDDO
      FREEW=STORGE+TEX
      QUAL=1.0
      IF(WE.GT.0.0)QUAL=1.0+(LIQW/WE)
      WE=(TWE-FREEW)/QUAL
      
! VK  SNOW DEPTH IS ADJUSTED BY RATIO OF THE WE CHANGE
      IF(WE .GT. 0.0) THEN
       SNDPT=SNDPT*WE/(OLDWE-LIQW)
      ELSE
       SNDPT=0.0
       SNTMP=0.0
      ENDIF
! VK--------------------------------------------------
                    
      LIQW=(QUAL-1.0)*WE
      SWE=WE+LIQW
      IF(.NOT.(OLDWE.GT.0.8*ACCMAX)) THEN
        IF(SWE.GT.ACCMAX) ACCMAX=SWE
      ELSE
        ACCMAX=SWE*(ACCMAX/OLDWE)
      ENDIF
      IF (SWE.GE.AEADJ) AEADJ=0.0
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF (AEADJ.GT.0.0) AI=AEADJ
      IF(.NOT.(SWE.LT.AI)) THEN
        SB=SWE
        SBWS=SWE
      ELSE
        IF(.NOT.((OLDWE < OLDAI).AND.(OLDWE > SB))) THEN
          SB=SWE+SNOF
          SBWS=SWE
          R=(SWE/AI)*10.0+1.0
        ELSE
          R=SWE/OLDWE
          SB=SB*R
          SBWS=SBWS*R
          IF(SBWS < SB+0.75*SNOF)SBWS=SB+0.75*SNOF
          R=(SB/AI)*10.0+1.0
        ENDIF
        N=INT( R )
        FN=FLOAT( N )
        R=R-FN
        SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
        IF(SBAESC.GT.1.0) SBAESC=1.0
      ENDIF
!.......................................
      END SUBROUTINE ADJC19
      
!.......................................
!     THIS SUBROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A
!        CHANGE IN THE AREAL EXTENT OF THE SNOW COVER.
!        USED IN THE 'SNOW-17' OPERATION.
!.......................................
!     SUBROUTINE INITIALLY WRITTEN BY...
!        ERIC ANDERSON-HRL APRIL 1981

! VK      MODIFIED 4/00 BY V. KOREN: NEW STATES, SNDPT & SNTMP ADDED
!.......................................

      SUBROUTINE AECO19 (COVER,TWE,SI,ADC)
      USE SNOW_17
      USE SNOW_17_common, ONLY: NEXLAG, ACCMAX, EXLAG, SB, SBAESC, SBWS, STORGE, AEADJ, &
                                SB, SBAESC, SBWS, STORGE, SNOF
      IMPLICIT NONE
! Arguments
      REAL :: COVER, TWE, SI, ADC(11)
! Local Variables
      INTEGER :: I, J, N
      REAL :: TEX, FREEW, SWE, AI, SB1, R, FN, AESC, FJ, WEAI
!.......................................
!     DETERMINE IF CURRENTLY ON DEPLETION CURVE OR NEW SNOW LINE.
      TEX=0.0
      DO N = 1, NEXLAG
         TEX = TEX + EXLAG(N)
      END DO
      FREEW=STORGE+TEX
      SWE=TWE-FREEW
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      IF(SWE.GE.AI) GO TO 100
      IF(SWE.LE.SB) GO TO 100
      IF ( SWE <= SB1 ) GO TO 100
!.......................................
!     CURRENTLY ON NEW SNOW LINE.
      IF (COVER <= SBAESC) GO TO 100
      R=(SWE/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      AESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(COVER.LE.AESC) GO TO 100
!     ADJUST SBWS , LEAVE AEADJ AS IS.
      SBWS=((1.0-SBAESC)/(COVER-SBAESC))*(SWE-SB)+SB
      IF (SBWS.GT.SB+0.75*SNOF) RETURN
      SB=SBWS-0.75*SNOF
      R=(SB/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      RETURN
!.......................................
!     CURRENTLY OR SHOULD BE ON THE DEPLETION CURVE.
  100 WEAI=1.0
      DO I=2,11
        IF (COVER.GE.ADC(I)) CYCLE
        J=I-1
        FJ=J-1
        WEAI=0.1*(FJ+ (COVER-ADC(J))/(ADC(I)-ADC(J)))
        EXIT
      END DO

      AEADJ=SWE/WEAI
      SBAESC=COVER
!.......................................
      END SUBROUTINE AECO19
