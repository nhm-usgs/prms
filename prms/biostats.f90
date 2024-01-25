!######################################################################
! PRMS_BIOSTATS module - Originally authored by Rob Payn
! Module for summarizing sliding-window averages and standard
! deviations of certain PRMS variables.
! Size of the sliding windows is determined by a parameter:
! biostats_window
! Variables implemented:
! basin_cfs, subbasin_cfs, runoff
!
!######################################################################

      MODULE PRMS_BIOSTATS
      character(len=*), parameter :: MODDESC = 'Bio Stats'
      character(len=*), parameter :: MODNAME = 'biostats'
      character(len=*), parameter :: Version_biostats = '2024-01-25'
      ! Parameters
      integer :: Biostats_window

      ! Local variables
      integer :: Biostats_counter !, Season_counter
      double precision, save, allocatable :: Basin_history(:), Runoff_history(:, :)
      double precision, save, allocatable :: Subbasin_history(:, :)
!      REAL :: Sample_basin_histavg(150), Sample_basin_cfs(150)

      ! Declared variables
      double precision, save :: Basin_histavg, Basin_histsigma
      double precision, save, allocatable :: Runoff_histavg(:), Subbasin_histavg(:)
      double precision, save, allocatable :: Runoff_histsigma(:), Subbasin_histsigma(:)

      end MODULE PRMS_BIOSTATS

!#######################################################################

      subroutine biostats( )
      use PRMS_CONSTANTS,  only: RUN, INIT, DECL, DOCUMENTATION
      use PRMS_MODULE,  only: Nobs, Nsub, Process_flag, Model !, Nowyear, Nowday
      use PRMS_BIOSTATS
      use PRMS_OBS,  only: Runoff
      use PRMS_FLOWVARS,  only: Basin_cfs
      use PRMS_SUBBASIN,  only: Sub_cfs
      implicit none
! functions
      integer, external :: declparam, getparam, declvar
      double precision, external :: biostats_avg, biostats_sigma
      external :: print_module, read_error
!      external :: biostats_clear_samples
! Local Variables
      integer :: j

      if ( Process_flag == RUN ) then
        ! Record the current basin streamflow in the sliding window
        ! (overwriting the previous oldest value)
        ! and calculate the average for the current window
        Basin_history(Biostats_counter) = Basin_cfs
        Basin_histavg = biostats_avg( Basin_history, Biostats_window )
        Basin_histsigma = biostats_sigma( Basin_history, Biostats_window, Basin_histavg )

        ! Repeat the sliding window caculation described in the block
        ! above for all measured flow (from data file) and all subbasin flows
        if ( Nobs > 0 ) then
          do j = 1, Nobs
            Runoff_history(Biostats_counter, j) = Runoff(j)
            Runoff_histavg(j) = biostats_avg( Runoff_history(1, j ), Biostats_window )
            Runoff_histsigma(j) = biostats_sigma(Runoff_history(1, j), Biostats_window, Runoff_histavg(j) )
          enddo
        endif
        if ( Nsub > 0 ) then
          do j = 1, Nsub
            Subbasin_history(Biostats_counter, j) = Sub_cfs(j)
            Subbasin_histavg(j) = biostats_avg( Subbasin_history(1, j), Biostats_window )
            Subbasin_histsigma(j) = biostats_sigma( Subbasin_history(1, j), Biostats_window, Subbasin_histavg(j) )
          enddo
        endif

        ! Calculate the location of the next oldest value in the sliding window
        if ( Biostats_counter == Biostats_window ) then
          Biostats_counter = 1
        else
          Biostats_counter = Biostats_counter + 1
        endif

        ! Started writing code to calculated biostats form sliding windows
        ! THIS DEVELOPMENT WAS SUSPENDED BECAUSE A POSTPROCESSOR WAS
        ! DEVELOPED INSTEAD

        ! Determine whether it is spring (March - June) or summer (July - September)
!        if (Nowyear > 2 .AND. Nowyear < 10) then
!          ! Spring or summer
!          if (Nowyear < 7) then
!            ! Spring
!            Season_counter = Season_counter + 1
!            ! Spring code here
!            Sample_basin_histavg(Season_counter) = Basin_histavg
!            Sample_basin_cfs(Season_counter) = Basin_cfs
!            if (Nowyear == 6 .AND. Nowday == 30) then
!              ! Last day of June
!              ! Spring final calc code here
!              call biostats_clear_samples
!            endif
!          else
!            ! Summer
!            Season_counter = Season_counter + 1
!            ! Summer code here
!            Sample_basin_histavg(Season_counter) = Basin_histavg
!            Sample_basin_cfs(Season_counter) = Basin_cfs
!            if (Nowyear == 9 .AND. Nowday == 30) then
!              ! Last day of September
!              ! Summer final calc code here
!              call biostats_clear_samples
!            endif
!          endif
!        endif

      elseif ( Process_flag == DECL ) then
        call print_module( MODDESC, MODNAME, Version_biostats )

        ! Declared parameters
        if ( declparam(MODNAME, 'biostats_window', 'one', 'integer', &
             '10', '1', '366', &
             'Number of time increments for average', &
             'Number of time increments (days in daily mode) for sliding average of flow variables', &
             'none') /= 0 ) call read_error( 1, 'biostats_window' )

        ! Declared variables
        if ( declvar(MODNAME, 'basin_histavg', 'one', 1, 'double', &
             'Sliding average of basin stream flow.'// &
             ' Number of increments averaged determined by biostats_window', &
             'cfs', Basin_histavg) /= 0 ) call read_error( 3, 'basin_histavg' )
        if ( declvar(MODNAME, 'basin_histsigma', 'one', 1, 'double', &
             'Sliding standard deviation of basin stream flow.'// &
             ' Number of increments used for calculation of standard deviation is determined by biostats_window', &
             'cfs', Basin_histsigma) /= 0 ) call read_error( 3, 'basin_histsigma' )

        if ( Nobs > 0 .OR. Model == DOCUMENTATION ) then
          allocate ( Runoff_histavg(Nobs) )
          if ( declvar(MODNAME, 'runoff_histavg', 'nobs', Nobs, 'double', &
               'Sliding average of measured runoff stream flow.'// &
               ' Number of increments averaged determined by biostats_window', &
               'cfs', Runoff_histavg) /= 0 ) call read_error( 3, 'runoff_histavg' )
          allocate ( Runoff_histsigma(Nobs) )
          if ( declvar(MODNAME, 'runoff_histsigma', 'nobs', Nobs, 'double', &
               'Sliding standard deviation of measured stream flow.'// &
               ' Number of increments used for calculation of standard deviation is determined by biostats_window', &
               'cfs', Runoff_histsigma) /= 0 ) call read_error( 3, 'runoff_histsigma' )
        endif

        if ( Nsub > 0 .OR. Model == DOCUMENTATION ) then
          allocate ( Subbasin_histavg(Nsub) )
          if ( declvar(MODNAME, 'subbasin_histavg', 'nsub', Nsub, 'double', &
               'Sliding average of subbasin runoff stream flow.'// &
               ' Number of increments averaged determined by biostats_window', &
               'cfs', Subbasin_histavg) /= 0 ) call read_error( 3, 'runoff_histsigma' )
          allocate (Subbasin_histsigma(Nsub))
          if (declvar(MODNAME, 'subbasin_histsigma', 'nsub', Nsub, 'double', &
          'Sliding standard deviation of subbasin stream flow.'// &
          ' Number of increments used for calculation of standard deviation is determined by biostats_window', &
          'cfs', Subbasin_histsigma) /= 0 ) call read_error( 3, 'subbasin_histsigma' )
        endif

      elseif ( Process_flag == INIT ) then
        if ( getparam(MODNAME, 'biostats_window', 1, 'integer', Biostats_window) /= 0 ) call read_error( 2, 'biostats_window' )

        ! allocate local variables and variables declared elsewhere
        allocate ( Basin_history(Biostats_window) )
        if ( Nobs > 0 ) then
          allocate ( Runoff_history(Biostats_window, Nobs) )
          Runoff_history = 0.0D0
        endif
        if ( Nsub > 0 ) then
          allocate ( Subbasin_history(Biostats_window, Nsub) )
          Subbasin_history = 0.0D0
        endif

        Biostats_counter = 1
        Basin_history = 0.0D0
        Basin_histavg = 0.0D0
        Basin_histsigma = 0.0D0
!       Season_counter = 0
!       Sample_basin_histavg = 0.0D0
!       Sample_basin_cfs = 0.0D0

        ! call biostats_clear_samples
      endif

      end subroutine biostats

!#######################################################################

      double precision function biostats_avg(inarray, nelements)
      implicit none
      ! Array dimension from passed variable
      integer :: nelements
      ! Passed array
      double precision :: inarray(1:nelements)
      intrinsic :: dble
      ! Local variables
      double precision :: sum1
      integer :: num
      integer :: j
!***********************************************
      sum1 = 0.0D0
      num = 0
      do j=1,nelements
        if (inarray(j) > -90.0D0) then
          sum1 = sum1 + inarray(j)
          num = num + 1
        endif
      enddo
      if ( num>0 ) then
        biostats_avg = sum1 / dble(num)
      else
        biostats_avg = -999.0D0
      endif

      end function biostats_avg

!#######################################################################

      double precision function biostats_sigma(inarray, nelements, mean)
      implicit none
      ! Array dimension from passed variable
      integer :: nelements
      ! Passed array
      double precision :: inarray(1:nelements), mean
      intrinsic :: dble
      ! Local variables
      double precision :: sum1
      integer :: num, j
!***********************************************
      sum1 = 0.0D0
      num = 0
      do j = 1, nelements
        if (inarray(j) > -90.0D0) then
          sum1 = sum1 + (inarray(j) - mean)**2
          num = num + 1
        endif
      enddo
      if ( num>0 ) then
        biostats_sigma = (sum1 / dble(num - 1))**0.5D0
      else
        biostats_sigma = -999.0D0
      endif

      end function biostats_sigma

!#######################################################################

!      subroutine biostats_clear_samples()
!
!      use PRMS_BIOSTATS
!      implicit none
!
!      integer :: j
!
!      Season_counter = 0
!      Sample_basin_histavg = 0.0D0
!      Sample_basin_cfs = 0.0D0
!      enddo
!
!      end subroutine biostats_clear_samples

