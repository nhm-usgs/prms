!**********************************************************************
!     WRITES NHM CSV SUMMARY FILE
!***********************************************************************
!   Declared Parameters
!     outlet_sta
!***********************************************************************
      INTEGER FUNCTION prms_summary()
      USE PRMS_MODULE, ONLY: Prms_summary_nc, Version_prms_summary, Strmflow_flag, Model, Process_flag, Npoigages, Nsegment
      USE PRMS_BASIN, ONLY: Timestep, Basin_gwflow_cfs, Basin_sroff_cfs, Basin_ssflow_cfs, Basin_cfs, Basin_stflow_in, &
     &    Basin_stflow_out
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Basin_tmax, Basin_tmin, Basin_potsw, Basin_ppt
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Basin_ssstor, Basin_imperv_stor, Basin_soil_to_gw, &
     &    Basin_dprst_volop, Basin_dprst_volcl, Basin_dprst_evap, Basin_imperv_evap, Basin_lakeevap, &
     &    Basin_perv_et, Basin_dprst_seep, Basin_actet, Basin_hortonian, Basin_dprst_wb
      USE PRMS_OBS, ONLY : Nowyear, Nowmonth, Nowday, Streamflow_cfs, Nobs
      USE PRMS_INTCP, ONLY: Basin_intcp_evap, Basin_intcp_stor
      USE PRMS_SNOW, ONLY: Basin_pweqv, Basin_snowevap, Basin_snowmelt, Basin_snowcov, Basin_pk_precip
      USE PRMS_SOILZONE, ONLY: Basin_capwaterin, Basin_pref_flow_in, Basin_prefflow, Basin_recharge, Basin_slowflow, &
     &    Basin_pref_stor, Basin_slstor, Basin_soil_rechr, Basin_sz2gw, Basin_dunnian, Basin_capillary_wb, Basin_gravity_wb, &
     &    Basin_soilzone_wb
      USE PRMS_GWFLOW, ONLY: Basin_gwstor, Basin_gwin, Basin_gwsink, Basin_gwflow, &
     &    Basin_gwstor_minarea_wb, Basin_dnflow
      USE PRMS_STRMFLOW_LAKE, ONLY: Basin_lake_stor
      USE PRMS_MUSKINGUM, ONLY: Seg_outflow
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX, CHAR
      INTEGER, EXTERNAL :: declmodule, declparam, declvar, getparam, get_ftnunit, getdim
      EXTERNAL :: read_error
      INTEGER, EXTERNAL :: getparamstring, control_string
! Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_total_storage, Basin_surface_storage
! Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Parent_poigages(:), Poi_gage_segment(:)
      CHARACTER(LEN=16), SAVE, ALLOCATABLE :: Poi_gage_id(:)
! Local Variables
      INTEGER :: i, n, ios, ierr, foo
      INTEGER, SAVE :: iunit
      INTEGER, PARAMETER :: NVARS = 54
      INTEGER, SAVE, ALLOCATABLE :: gageid_len(:)
      REAL, SAVE, ALLOCATABLE :: segmentout(:), gageout(:)
      CHARACTER(LEN=12), PARAMETER :: MODNAME = 'prms_summary'
      CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'PRMS Summary'
      CHARACTER(LEN=48), ALLOCATABLE :: streamflow_pairs(:)
      CHARACTER(LEN=8), ALLOCATABLE :: cfs_strings(:)
      CHARACTER(LEN=24) :: fmt
      CHARACTER(LEN=40), SAVE :: fmt2
      CHARACTER(LEN=10) :: chardate
      CHARACTER(LEN=128) :: csv_output_file
!***********************************************************************
      prms_summary = 1

      IF ( Process_flag==0 ) THEN
        DO i = 1, Npoigages
          segmentout(i) = Seg_outflow(Poi_gage_segment(i))
          gageout(i) = Streamflow_cfs(Parent_poigages(i))
        ENDDO

        IF ( Strmflow_flag/=2 ) Basin_lake_stor = 0.0D0
        Basin_total_storage = Basin_soil_moist + Basin_intcp_stor + Basin_gwstor + Basin_ssstor + Basin_pweqv + &
     &                        Basin_imperv_stor + Basin_lake_stor + Basin_dprst_volop + Basin_dprst_volcl
        Basin_surface_storage = Basin_intcp_stor + Basin_pweqv + Basin_imperv_stor + Basin_lake_stor + &
     &                          Basin_dprst_volop + Basin_dprst_volcl
        WRITE (chardate, '(I4.4,2("-",I2.2))') Nowyear, Nowmonth, Nowday
        WRITE ( iunit, fmt2 ) chardate, &
     &          Basin_potet, Basin_actet, Basin_dprst_evap, Basin_imperv_evap, Basin_intcp_evap, Basin_lakeevap, &
     &          Basin_perv_et, Basin_snowevap, Basin_potsw, Basin_ppt, Basin_pk_precip, &
     &          Basin_tmax, Basin_tmin, Basin_snowcov, &
     &          Basin_total_storage, Basin_surface_storage, &
     &          Basin_dprst_volcl, Basin_dprst_volop, Basin_gwstor, Basin_imperv_stor, Basin_intcp_stor, Basin_lake_stor, &
     &          Basin_pweqv, Basin_soil_moist, Basin_ssstor, &
     &          Basin_pref_stor, Basin_slstor, Basin_soil_rechr, &
     &          Basin_capwaterin, Basin_dprst_seep, Basin_gwin, Basin_pref_flow_in, Basin_recharge, Basin_snowmelt, &
     &          Basin_soil_to_gw, Basin_sz2gw, &
     &          Basin_gwsink, Basin_prefflow, Basin_slowflow, Basin_hortonian, Basin_dunnian, &
     &          Basin_stflow_in, Basin_stflow_out, Basin_gwflow, Basin_dnflow, &
     &          Basin_gwstor_minarea_wb, Basin_dprst_wb, Basin_capillary_wb, Basin_gravity_wb, Basin_soilzone_wb, &
     &          Basin_cfs, Basin_gwflow_cfs, Basin_sroff_cfs, Basin_ssflow_cfs, &
     &          (segmentout(i), gageout(i), i = 1, Npoigages)

! Declare procedure
      ELSEIF ( Process_flag==1 ) THEN
        Version_prms_summary = '$Id: prms_summary.f90 4822 2012-09-14 17:54:15Z rsregan $'
        Prms_summary_nc = INDEX( Version_prms_summary, 'Z' )
        n = INDEX( Version_prms_summary, '.f90' ) + 3
        IF ( declmodule(Version_prms_summary(6:n), PROCNAME, Version_prms_summary(n+2:Prms_summary_nc))/=0 ) STOP

!       Open summary file
        IF ( control_string(csv_output_file, 'csv_output_file')/=0 ) CALL read_error(5, 'csv_output_file')
        iunit = get_ftnunit(318)
        OPEN ( iunit, FILE=csv_output_file, IOSTAT=ios )
        IF ( ios/=0 ) THEN
          PRINT *, 'ERROR opening file: ', csv_output_file
          STOP
        ELSE
          OPEN ( UNIT=iunit, FILE=csv_output_file )
        ENDIF

        Npoigages = getdim('npoigages')
        IF ( Npoigages==-1 ) CALL read_error(6, 'npoigages')
        IF ( Npoigages>0 ) THEN
          ALLOCATE ( Parent_poigages(Npoigages) )
          IF ( declparam(MODNAME, 'parent_poigages', 'npoigages', 'integer', &
     &       '1', 'bounded', 'nobs', &
     &       'Index of streamflow measurement station in parent model', &
     &       'Index of measured streamflow station corresponding to each point of interest', &
     &       'none')/=0 ) CALL read_error(1, 'parent_poigages')
          ALLOCATE ( Poi_gage_segment(Npoigages) )
          IF ( declparam(MODNAME, 'poi_gage_segment', 'npoigages', 'integer', &
     &       '1', 'bounded', 'nsegment', &
     &       'Index of stream segment in child model', &
     &       'Index of stream segment corresponding to each point of interest', &
     &       'none')/=0 ) CALL read_error(1, 'poi_gage_segment')
          ALLOCATE ( Poi_gage_id(Npoigages) )
          IF ( declparam(MODNAME, 'poi_gage_id', 'npoigages', 'string', &
     &       '0', '0', '9999999', &
     &       'Identification number of streamflow measurement station', &
     &       'Identification number of streamflow measurement station corresponding to each point of interest', &
     &       'none')/=0 ) CALL read_error(1, 'poi_gage_id')
        ENDIF
        
        IF ( declvar(MODNAME, 'basin_total_storage', 'one', 1, 'double', &
     &       'Basin area-weighted average storage in all water storage reservoirs', &
     &       'inches', Basin_total_storage)/=0 ) CALL read_error(3, 'basin_total_storage')

        IF ( declvar(MODNAME, 'basin_surface_storage', 'one', 1, 'double', &
     &       'Basin area-weighted average storage in all water storage reservoirs', &
     &       'inches', Basin_surface_storage)/=0 ) CALL read_error(3, 'basin_surface_storage')

! Initialize Procedure
      ELSEIF ( Process_flag==2 ) THEN
        IF ( Timestep==0 ) THEN
          Basin_total_storage = 0.0D0
          Basin_surface_storage = 0.0D0
        ENDIF

        IF ( Npoigages>0 ) THEN
          ALLOCATE ( streamflow_pairs(Npoigages), cfs_strings(Npoigages) )
          ALLOCATE ( segmentout(Npoigages), gageout(Npoigages) )
          ALLOCATE ( gageid_len(Npoigages) )
          IF ( getparam(MODNAME, 'parent_poigages', Npoigages, 'integer', Parent_poigages)/=0 ) &
     &         CALL read_error(2, 'parent_poigages')
          IF ( getparam(MODNAME, 'poi_gage_segment', Npoigages, 'integer', Poi_gage_segment)/=0 ) &
     &         CALL read_error(2, 'poi_gage_segment')
          DO i = 1, Npoigages
            Poi_gage_id(i) = '                '
          ENDDO
          IF ( getparam(MODNAME, 'poi_gage_id', Npoigages, 'string', Poi_gage_id)/=0 ) &
     &         CALL read_error(2, 'poi_gage_id')
          !print *, poi_gage_id

          DO i = 1, Npoigages
            foo = getparamstring(MODNAME, 'poi_gage_id', Npoigages, 'string', &
     &            i-1, Poi_gage_id(i))
          ENDDO
          !print *, "second", poi_gage_id
          ierr = 0
          DO i = 1, Npoigages
            IF ( Parent_poigages(i)<1 .OR. Parent_poigages(i)>Nobs ) THEN
              ierr = 1
              PRINT *, 'ERROR, invalid parent_poigage for POI:', i, '; parent gage:', &
     &                 Parent_poigages(i), '; nobs:', Nobs
              CYCLE
            ENDIF
            IF ( Poi_gage_segment(i)<1 .OR. Poi_gage_segment(i)>Nsegment ) THEN
              ierr = 1
              PRINT *, 'ERROR, invalid poi_gage_segment for POI:', i, '; child segment:', &
     &                 Poi_gage_segment(i), '; nsegment:', Nsegment
              CYCLE
            ENDIF
            gageid_len(i) = INDEX( Poi_gage_id(i), ' ' ) - 1
            IF ( gageid_len(i)<0 ) gageid_len(i) = INDEX( Poi_gage_id(i), CHAR(0) ) - 1
            !PRINT *, 'gageid_len ', gageid_len(i), ' :', Poi_gage_id(i), ':'
            IF (gageid_len(i)<1 ) gageid_len(i) = 0
            IF ( gageid_len(i)>0 ) THEN
              IF ( gageid_len(i)>23 ) gageid_len(i) = 23
              WRITE (streamflow_pairs(i), '(A,I4.4,2A)' ) ',seg_outflow_', Poi_gage_segment(i), ',runoff_', &
     &                                                    Poi_gage_id(i)(:gageid_len(i))
            ELSE
              gageid_len(i) = 4
              WRITE (streamflow_pairs(i), '(2(A,I4.4))' ) ',seg_outflow_', Poi_gage_segment(i), ',runoff_', &
     &                                                    Parent_poigages(i)
            ENDIF
          ENDDO
          !print *, 'pairs', streamflow_pairs
          IF ( ierr==1 ) STOP
        ELSE
          ALLOCATE ( streamflow_pairs(1), cfs_strings(1) )
          ALLOCATE ( segmentout(1), gageout(1) )
          streamflow_pairs = ' '
        ENDIF
        cfs_strings = ',cfs,cfs'

        WRITE ( fmt, '(A,I4,A)' ) '( ', 2*Npoigages+14, 'A )'
        WRITE ( iunit, fmt ) 'Date,', &
     &          'basin_potet,basin_actet,basin_dprst_evap,basin_imperv_evap,basin_intcp_evap,basin_lakeevap,', &
     &          'basin_perv_et,basin_snowevap,basin_potsw,basin_ppt,basin_pk_precip,', &
     &          'basin_tmax,basin_tmin,basin_snowcov,', &
     &          'basin_total_storage,basin_surface_storage,', &
     &          'basin_dprst_volcl,basin_dprst_volop,basin_gwstor,basin_imperv_stor,basin_intcp_stor,basin_lake_stor,', &
     &          'basin_pweqv,basin_soil_moist,basin_ssstor,', &
     &          'basin_pref_stor,basin_slstor,basin_soil_rechr,', &
     &          'basin_capwaterin,basin_dprst_seep,basin_gwin,basin_pref_flow_in,basin_recharge,basin_snowmelt,', &
     &          'basin_soil_to_gw,basin_sz2gw,', &
     &          'basin_gwsink,basin_prefflow,basin_slowflow,basin_hortonian,basin_dunnian,', &
     &          'basin_stflow_in,basin_stflow_out,basin_gwflow,basin_dnflow,', &
     &          'basin_gwstor_minarea_wb,basin_dprst_wb,basin_capillary_wb,basin_gravity_wb,basin_soilzone_wb,', &
     &          'basin_cfs,basin_gwflow_cfs,basin_sroff_cfs,basin_ssflow_cfs', &
     &          (streamflow_pairs(i)(:gageid_len(i)+25), i = 1, Npoigages)

        WRITE ( iunit, fmt ) 'year-month-day,', &
     &          'inches/day,inches/day,inches/day,inches/day,inches/day,inches/day,', &
     &          'inches/day,inches/day,Langleys,inches/day,inches/day,', &
     &          'degrees,degrees,fraction,', &
     &          'inches,inches,', &
     &          'inches,inches,inches,inches,inches,inches,', &
     &          'inches,inches,inches,', &
     &          'inches,inches,inches,', &
     &          'inches/day,inches/day,inches/day,inches/day,inches/day,inches/day,', &
     &          'inches/day,inches/day,', &
     &          'inches/day,inches/day,inches/day,inches/day,inches/day,', &
     &          'inches/day,inches/day,inches/day,inches/day,', &
     &          'fraction,fraction,fraction,fraction,fraction,', &
     &          'cfs,cfs,cfs,cfs', &
     &          (cfs_strings(i), i = 1, Npoigages)

        WRITE ( fmt2, '(A,I4,A)' )  '( A,', 2*Npoigages+NVARS, '(",",SPES10.3) )'
         !print *, 'fmt: ', fmt
         !print *, 'fmt2: ', fmt2
      ELSEIF ( Process_flag==3 ) THEN
        CLOSE ( iunit )
      ENDIF

      prms_summary = 0
      END FUNCTION prms_summary

