!***********************************************************************
! streamflow characteristics module
!***********************************************************************
      MODULE PRMS_STRMFLOW_CHARACTER
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Streamflow Characteristics'
      character(len=18), parameter :: MODNAME = 'strmflow_character'
      character(len=*), parameter :: Version_strmflow_character = '2024-01-22'
!   Declared Variables
      double precision, save, allocatable :: Seg_width(:), Seg_depth(:), Seg_area(:)
      double precision, save, allocatable :: Seg_velocity(:), seg_res_time(:)
!   Segment Parameters
      double precision, save, allocatable :: width_alpha(:), width_m(:)
      double precision, save, allocatable :: depth_alpha(:), depth_m(:)
      REAL, SAVE, ALLOCATABLE :: width_alpha_sngl(:), width_m_sngl(:)
      REAL, SAVE, ALLOCATABLE :: depth_alpha_sngl(:), depth_m_sngl(:)
      END MODULE PRMS_STRMFLOW_CHARACTER

!***********************************************************************
!     Main stream temperature routine
!***********************************************************************
      INTEGER FUNCTION strmflow_character()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: strmflow_character_run, strmflow_character_decl
      INTEGER, EXTERNAL :: strmflow_character_init
!***********************************************************************
      strmflow_character = 0

      IF ( Process_flag==RUN ) THEN
         strmflow_character = strmflow_character_run()
      ELSEIF ( Process_flag==DECL ) THEN
         strmflow_character = strmflow_character_decl()
      ELSEIF ( Process_flag==INIT ) THEN
         strmflow_character = strmflow_character_init()
      ENDIF

      END FUNCTION strmflow_character

!***********************************************************************
!     strmflow_character_decl - set up parameters and storage
!   Declared Parameters
!***********************************************************************
      INTEGER FUNCTION strmflow_character_decl()
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_STRMFLOW_CHARACTER
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declvar, declparam
      EXTERNAL :: read_error, print_module
!***********************************************************************
      strmflow_character_decl = 0

      CALL print_module(MODDESC, MODNAME, Version_strmflow_character)

! Declared Variables
      ALLOCATE ( Seg_width(Nsegment) )
      IF ( declvar( MODNAME, 'seg_width', 'nsegment', Nsegment, 'double', &
     &     'Width of flow in each segment', &
     &     'meters', Seg_width )/=0 ) CALL read_error(3, 'seg_width')

      ALLOCATE ( Seg_depth(Nsegment) )
      IF ( declvar( MODNAME, 'seg_depth', 'nsegment', Nsegment, 'double', &
     &     'Depth of flow in each segment', &
     &     'meters', Seg_depth )/=0 ) CALL read_error(3, 'seg_depth')

      ALLOCATE ( Seg_area(Nsegment) )
      IF ( declvar( MODNAME, 'seg_area', 'nsegment', Nsegment, 'double', &
     &     'Cross sectional area of flow in each segment', &
     &     'square meters', Seg_area )/=0 ) CALL read_error(3, 'seg_area')

      ALLOCATE ( Seg_velocity(Nsegment) )
      IF ( declvar( MODNAME, 'seg_velocity', 'nsegment', Nsegment, 'double', &
     &     'Mean velocity of flow in each segment', &
     &     'meters per second', Seg_velocity )/=0 ) CALL read_error(3, 'seg_velocity')

      ALLOCATE ( seg_res_time(Nsegment) )
      IF ( declvar( MODNAME, 'seg_res_time', 'nsegment', Nsegment, 'double', &
     &     'Mean residence time of water in each segment', &
     &     'seconds', seg_res_time )/=0 ) CALL read_error(3, 'seg_res_time')

      ALLOCATE ( width_alpha(Nsegment), width_alpha_sngl(Nsegment) )
      IF ( declparam( MODNAME, 'width_alpha', 'nsegment', 'real', &
     &     '7.2', '2.6', '20.2', &
     &     'Alpha coefficient in power function for width calculation', &
     &     'Alpha coefficient in power function for width calculation (for units M and CMS)', &
     &     'meters')/=0 ) CALL read_error(1, 'width_alpha')

      ALLOCATE ( width_m(Nsegment), width_m_sngl(Nsegment) )
      IF ( declparam( MODNAME, 'width_m', 'nsegment', 'real', &
     &     '0.5', '0.48', '0.52', &
     &     'M value in power function for width calculation', &
     &     'M value in power function for width calculation (for units M and CMS)', &
     &     'none')/=0 ) CALL read_error(1, 'width_m')

      ALLOCATE ( depth_alpha(Nsegment), depth_alpha_sngl(Nsegment) )
      IF ( declparam( MODNAME, 'depth_alpha', 'nsegment', 'real', &
    &      '0.27', '0.12', '0.63', &
     &     'Alpha coefficient in power function for depth calculation', &
     &     'Alpha coefficient in power function for depth calculation (for units M and CMS)', &
     &     'meters')/=0 ) CALL read_error(1, 'depth_alpha')

      ALLOCATE ( depth_m(Nsegment), depth_m_sngl(Nsegment) )
      IF ( declparam( MODNAME, 'depth_m', 'nsegment', 'real', &
     &     '0.39', '0.38', '0.40', &
     &     'M value in power function for depth calculation', &
     &     'M value in power function for depth calculation (for units M and CMS)', &
     &     'meters')/=0 ) CALL read_error(1, 'depth_m')

      END FUNCTION strmflow_character_decl

!***********************************************************************
!    strmflow_character_init - Initialize module - get parameter values
!***********************************************************************
      INTEGER FUNCTION strmflow_character_init()
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_STRMFLOW_CHARACTER
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: DBLE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: read_error
!***********************************************************************
      strmflow_character_init = 0

      IF ( getparam( MODNAME, 'width_alpha', Nsegment, 'real', width_alpha_sngl)/=0 ) CALL read_error(2, 'width_alpha')
      IF ( getparam( MODNAME, 'width_m', Nsegment, 'real', width_m_sngl)/=0 ) CALL read_error(2, 'width_m')
      IF ( getparam( MODNAME, 'depth_alpha', Nsegment, 'real', depth_alpha_sngl)/=0 ) CALL read_error(2, 'depth_alpha')
      IF ( getparam( MODNAME, 'depth_m', Nsegment, 'real', depth_m_sngl)/=0 ) CALL read_error(2, 'depth_m')
      width_alpha = DBLE( width_alpha_sngl )
      width_m = DBLE( width_m_sngl )
      depth_alpha = DBLE( depth_alpha_sngl )
      depth_m = DBLE( depth_m_sngl )
      DEALLOCATE ( width_alpha_sngl, width_m_sngl, depth_alpha_sngl, depth_m_sngl )

! Initialize declared variables
      Seg_width = 0.0D0
      Seg_depth = 0.0D0
      Seg_area = 0.0D0
      Seg_velocity = 0.0D0

      END FUNCTION strmflow_character_init

!***********************************************************************
!     strmflow_character_run - Computes streamflow characteristics
!***********************************************************************
      INTEGER FUNCTION strmflow_character_run()
      USE PRMS_CONSTANTS, ONLY: CFS2CMS_CONV
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_STRMFLOW_CHARACTER
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      USE PRMS_ROUTING, ONLY: Seg_length
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE
! Local Variables
      INTEGER :: i
      double precision :: segflow
!***********************************************************************
      strmflow_character_run = 0

      DO i = 1, Nsegment
         if ( .not.(Seg_outflow(i) < 0.0D0) ) then
            segflow = Seg_outflow(i) * CFS2CMS_CONV
            Seg_width(i) = width_alpha(i) * (segflow ** width_m(i))
            Seg_depth(i) = depth_alpha(i) * (segflow ** depth_m(i))
            Seg_area(i) = Seg_width(i) * Seg_depth(i)
            if ( .not.(Seg_area(i)) < 0.0D0 ) then
               Seg_velocity(i) = segflow / Seg_area(i)
            else
               Seg_velocity(i) = 0.0D0
            endif
            seg_res_time(i) = (Seg_area(i) * DBLE(Seg_length(i))) / segflow
         else
            Seg_width(i) = 0.0D0
            Seg_depth(i) = 0.0D0
            Seg_area(i) = 0.0D0
            Seg_velocity(i) = 0.0D0
            seg_res_time(i) = 0.0D0
         endif
      ENDDO

      END FUNCTION strmflow_character_run
