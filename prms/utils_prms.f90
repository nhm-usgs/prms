!***********************************************************************
!     Read CBH File to current time
!***********************************************************************
      SUBROUTINE find_current_time(Iunit, Year, Month, Day)
! Argument
      INTEGER, INTENT(IN) :: Iunit, Year, Month, Day
! Local Variables
      INTEGER :: i, yr, mo, dy, ios
!***********************************************************************
      i = 0
      DO WHILE ( i==0 )
        READ ( Iunit, *, IOSTAT=ios ) yr, mo, dy
        IF ( ios/=0 ) THEN
          yr = 0
          mo = 0
          dy = 0
          RETURN
        ENDIF
        IF ( yr==Year .AND. mo==Month .AND. dy==Day ) i = 1
      ENDDO
      BACKSPACE Iunit
      END SUBROUTINE find_current_time

!***********************************************************************
!     Read File dynamic paramter file to current time
!***********************************************************************
      SUBROUTINE find_current_file_time(Iunit, Year, Month, Day, Year_file, Month_file, Day_file)
! Argument
      INTEGER, INTENT(IN) :: Iunit, Year, Month, Day
      INTEGER, INTENT(OUT) :: Year_file, Month_file, Day_file
! Local Variables
      INTEGER :: i, ios
!***********************************************************************
! find first value for simulation time period
      READ ( Iunit, *, IOSTAT=ios ) Year_file, Month_file, Day_file
      IF ( ios/=0 ) THEN
        Year_file = 0
        Month_file = 0
        Day_file = 0
        RETURN
      ENDIF
      IF ( Year_file<Year ) THEN
        i = 0
        DO WHILE ( i==0 )
          READ ( Iunit, *, IOSTAT=ios ) Year_file, Month_file, Day_file
          IF ( ios/=0 ) THEN
            Year_file = 0
            Month_file = 0
            Day_file = 0
            RETURN
          ENDIF
          IF ( Year_file>=Year ) i = 1
        ENDDO
      ENDIF
      IF ( Year_file==Year ) THEN
        IF ( Month_file<Month ) THEN
          i = 0
          DO WHILE ( i==0 )
            READ ( Iunit, *, IOSTAT=ios ) Year_file, Month_file, Day_file
            IF ( ios/=0 ) THEN
              Year_file = 0
              Month_file = 0
              Day_file = 0
              RETURN
            ENDIF
            IF ( Month_file>=Month ) i = 1
          ENDDO
        ENDIF
        IF ( Month_file==Month ) THEN
          IF ( Day_file<Day ) THEN
            i = 0
            DO WHILE ( i==0 )
              READ ( Iunit, *, IOSTAT=ios ) Year_file, Month_file, Day_file
              IF ( ios/=0 ) THEN
                Year_file = 0
                Month_file = 0
                Day_file = 0
                RETURN
              ENDIF
              IF ( Day_file>=Day ) i = 1
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      BACKSPACE Iunit
      END SUBROUTINE find_current_file_time

!***********************************************************************
!     Read File to line before data starts in file
!***********************************************************************
      SUBROUTINE find_header_end(Iunit, Fname, Iret)
      USE PRMS_MODULE, ONLY: Nhru
! Argument
      INTEGER, INTENT(OUT) :: Iunit, Iret
      CHARACTER(LEN=*), INTENT(IN) :: Fname
! Functions
      INTEGER, EXTERNAL :: get_ftnunit
! Local Variables
      INTEGER :: i, ios, dim, orad_flag
      CHARACTER(LEN=4) :: dum
!***********************************************************************
      orad_flag = Iret
      Iret = 0
      Iunit = get_ftnunit(720)
! read to line before data starts in each file
      OPEN ( Iunit, FILE=Fname, STATUS='OLD', IOSTAT=ios )
      IF ( ios/=0 ) THEN
        WRITE ( *, '(/,A,/,A,/)' ) 'ERROR opening file:', Fname, 'check to be sure the input file exists'
        Iret = 1
      ELSE
! read to line before data starts in each file
        i = 0
        DO WHILE ( i==0 )
          READ ( Iunit, FMT='(A4)', IOSTAT=ios ) dum
          IF ( ios/=0 ) THEN
            WRITE ( *, '(/,A,/,A,/)' ) 'ERROR reading file:', Fname, 'check to be sure the input file is in correct format'
            Iret = 1
            EXIT
          ELSEIF ( dum=='####' ) THEN
            BACKSPACE Iunit
            BACKSPACE Iunit
            IF ( Orad_flag==2 ) BACKSPACE Iunit ! backspace again as swrad CBH file contains orad as last column
            READ ( Iunit, * ) dum, dim
            !print *, 'utils: ', dum, dim
            IF ( dim/=Nhru ) THEN
              PRINT '(/,2(A,I7))', '***CBH file dimension incorrect*** nhru=', Nhru, ' CBH dimension=', dim, ' File:', Fname
              STOP 'ERROR: update Control File with correct CBH files'
            ENDIF
            READ ( Iunit, FMT='(A4)' ) dum
            IF ( Orad_flag==2 ) READ ( Iunit, FMT='(A4)' ) dum ! read again as swrad CBH file contains orad as last column
            i = 1
          ENDIF
        ENDDO
      ENDIF
      END SUBROUTINE find_header_end

!**********************
! Check for end of file
!**********************
      SUBROUTINE is_eof(Iunit, Next_yr, Next_mo, Next_day)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit
      INTEGER, INTENT (OUT) :: Next_yr, Next_mo, Next_day
! Local Variables
      INTEGER :: ios, i
      CHARACTER(LEN=80) :: dum
!*******************************************************************************
      Next_yr = 0
      Next_mo = 0
      Next_day = 0
      i = 0
      DO WHILE ( i==0 )
        READ ( Iunit, '(A)', iostat=ios ) dum
        IF ( ios/=0 ) RETURN
        IF ( dum(:2)/='//' ) i = 1
      ENDDO
      READ ( dum, *, iostat=ios ) Next_yr, Next_mo, Next_day
      IF ( ios/=0 ) THEN
        Next_yr = 0
        Next_mo = 0
        Next_day = 0
      ELSE
        BACKSPACE Iunit
      ENDIF
      END SUBROUTINE is_eof

!***********************************************************************
!     Determine an unopened FORTRAN File Unit
!***********************************************************************
      INTEGER FUNCTION get_ftnunit(Iunit)
! Argument
      INTEGER, INTENT(IN) :: Iunit
! Local Variables
      INTEGER :: good_unit
      LOGICAL :: opend
!***********************************************************************
      good_unit = Iunit
      opend = .TRUE.
      DO WHILE ( opend )
        good_unit = good_unit + 1
        INQUIRE (UNIT=good_unit, OPENED=opend)
      ENDDO
      get_ftnunit = good_unit
      END FUNCTION get_ftnunit

!***********************************************************************
! Convert Fahrenheit to Celsius
!***********************************************************************
      REAL FUNCTION f_to_c(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
!***********************************************************************
      f_to_c = (Temp-32.0)/1.8
      END FUNCTION f_to_c

!***********************************************************************
! Convert Celsius to Fahrenheit
!***********************************************************************
      REAL FUNCTION c_to_f(Temp)
! Arguments
      REAL, INTENT(IN) :: Temp
!***********************************************************************
      c_to_f = Temp*1.8 + 32.0
      END FUNCTION c_to_f

!***********************************************************************
      SUBROUTINE write_integer_param(Iunit, Parm_name, Dimen_name, Dimen, Values)
!***********************************************************************
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit, Dimen
      INTEGER, INTENT(IN) :: Values(Dimen)
      CHARACTER(LEN=*), INTENT(IN) :: Parm_name, Dimen_name
! Local Variables
      INTEGER i
      CHARACTER(LEN=40), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, "1")'
!***********************************************************************
      WRITE ( Iunit, fmt1 ) Parm_name, Dimen_name, Dimen
      DO i = 1, Dimen
        WRITE ( Iunit, * ) Values(i)
      ENDDO
      END SUBROUTINE write_integer_param

!***********************************************************************
      SUBROUTINE write_real_param(Iunit, Parm_name, Dimen_name, Dimen, Values)
!***********************************************************************
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit, Dimen
      REAL, INTENT(IN) :: Values(Dimen)
      CHARACTER(LEN=*), INTENT(IN) :: Parm_name, Dimen_name
! Local Variables
      INTEGER i
      CHARACTER(LEN=40), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, "2")'
!***********************************************************************
      WRITE ( Iunit, fmt1) Parm_name, Dimen_name, Dimen
      DO i = 1, Dimen
        WRITE ( Iunit, * ) Values(i)
      ENDDO
      END SUBROUTINE write_real_param

!***********************************************************************
      SUBROUTINE write_double_param(Iunit, Parm_name, Dimen_name, Dimen, Values)
!***********************************************************************
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit, Dimen
      DOUBLE PRECISION, INTENT(IN) :: Values(Dimen)
      CHARACTER(LEN=*), INTENT(IN) :: Parm_name, Dimen_name
! Local Variables
      INTEGER i
      CHARACTER(LEN=40), PARAMETER :: fmt1 = '("####", /, A, /, "1", /, A, /, I6, "3")'
!***********************************************************************
      WRITE ( Iunit, fmt1 ) Parm_name, Dimen_name, Dimen
      DO i = 1, Dimen
        WRITE ( Iunit, * ) Values(i)
      ENDDO
      END SUBROUTINE write_double_param

!***********************************************************************
      SUBROUTINE write_2D_double_param(Iunit, Parm_name, Dimen_name1, Dimen1, &
                                       Dimen_name2, Dimen2, Values)
!***********************************************************************
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Dimen1, Dimen2, Iunit
      DOUBLE PRECISION, INTENT(IN) :: Values(Dimen1, Dimen2)
      CHARACTER(LEN=*), INTENT(IN) :: Parm_name
      CHARACTER(LEN=*), INTENT(IN) :: Dimen_name1, Dimen_name2
! Local Variables
      INTEGER i, j
      CHARACTER(LEN=46), PARAMETER :: fmt1 = '("####", /, A, /, "2", /, A, /, A, /, I8, "3")'
!***********************************************************************
      WRITE ( Iunit, fmt1 ) Parm_name, Dimen_name1, Dimen_name2, Dimen1*Dimen2
      DO i = 1, Dimen2
        DO j = 1, Dimen1
          WRITE ( Iunit, * ) Values(j, i)
        ENDDO
      ENDDO
      END SUBROUTINE write_2D_double_param

!***********************************************************************
      SUBROUTINE write_2D_double_array_grid(Iunit, Parm_name, Dimen_name1, &
                                    Dimen1, Dimen_name2, Dimen2, Values)
!***********************************************************************
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit, Dimen1, Dimen2
      DOUBLE PRECISION, INTENT(IN) :: Values(Dimen1, Dimen2)
      CHARACTER(LEN=*), INTENT(IN) :: Parm_name
      CHARACTER(LEN=*), INTENT(IN) :: Dimen_name1, Dimen_name2
! Local Variables
      INTEGER i, j
      CHARACTER(LEN=12) :: fmt
!***********************************************************************
      WRITE ( Iunit, 9001) Parm_name, Dimen_name1, Dimen_name2, Dimen1*Dimen2
      WRITE ( fmt, 9002 ) Dimen2
      DO i = 1, Dimen2
        WRITE ( Iunit, fmt ) (Values(j, i), j=1,Dimen1)
      ENDDO

 9001 FORMAT ( '####', /, A, /, '2', /, A, /, A, /, I8, /, '3' )
 9002 FORMAT ( '(', I5, 'F10.5)' )
      END SUBROUTINE write_2D_double_array_grid

!**********************************************************************
!     Version Check
!**********************************************************************
      SUBROUTINE version_check(Module_version, Length, Param_version)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Module_version, Param_version
      INTEGER, INTENT(IN) :: Length
!**********************************************************************
      IF ( Module_version(13:Length+12)/=Param_version(:Length) ) THEN
        PRINT 9001, Module_version(13:Length+12), Param_version(:Length)
        PRINT *, 'Enter return to continue'
        READ (*, *)
      ENDIF
 9001 FORMAT ('Warning, module versions are not identical', /, &
              '      Executable version: ', A, /, &
              '  Parameter File version: ', A, /)
      END SUBROUTINE version_check

!**********************************************************************
!     Parameter or Variable delcare or read error
!**********************************************************************
      SUBROUTINE read_error(Type, Name)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Type
      CHARACTER(LEN=*), INTENT(IN) :: Name
!**********************************************************************
      PRINT '(/,A,/)', 'Due to error condition simulation halted'
      IF ( Type==1 ) THEN
        PRINT *, 'Declare error for parameter: ', Name
      ELSEIF ( Type==2 ) THEN
        PRINT *, 'Get error for parameter: ', Name
      ELSEIF ( Type==3 ) THEN
        PRINT *, 'Declare error for variable: ', Name
      ELSEIF ( Type==4 ) THEN
        PRINT *, 'Get error for variable: ', Name
      ELSEIF ( Type==5 ) THEN
        PRINT *, 'Read error for control parameter: ', Name
      ELSEIF ( Type==6 ) THEN
        PRINT *, 'Read error for dimension parameter: ', Name
      ELSEIF ( Type==7 ) THEN
        PRINT *, 'Declare error for dimension parameter: ', Name
      ELSEIF ( Type==8 ) THEN
        PRINT *, 'Declare error for Data File variable: ', Name
      ELSEIF ( Type==9 ) THEN
        PRINT *, 'Read error for Data File variable: ', Name
      ENDIF
      STOP
      END SUBROUTINE read_error

!**********************************************************************
!     Module error
!**********************************************************************
      SUBROUTINE module_error(Modname, Arg, Retcode)
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Arg
      INTEGER, INTENT(IN) :: Retcode
!**********************************************************************
      PRINT 9001, Modname, Arg, Retcode
      STOP
 9001 FORMAT ('ERROR in ', A, ' module, arg = ', A, /, 'Return val =', I4)
      END SUBROUTINE module_error

!***********************************************************************
! Compute saturation vapor pressure over water
! 6th order Polynominal method (Flatau et. all., 1992) valid: -50 to 50C
!***********************************************************************
      REAL FUNCTION sat_vapor_press_poly(Tempc)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Tempc
!***********************************************************************
      sat_vapor_press_poly = 6.11176750 + 0.443986062*Tempc &
                             + 0.0143053301*Tempc**2 &
                             + 0.265027242E-03*Tempc**3 &
                             + 0.302246994E-05*Tempc**4 &
                             + 0.203886313E-07*Tempc**5 &
                             + 0.638780966E-10*Tempc**6
! Mastin documentation for potet_dpm
!      sat_vapor_press_poly = 23.38*exp(18.1-5303.3/(Tempc+273.0))
! Mastin documentation for param_leaf-loss.aml
!      sat_vapor_press_poly = 6.1078*EXP(17.269*Tempc/(237.30D0+Tempc))
! Buck Research Manual (1996)
!      sat_vapor_press_poly = 6.1121D0*EXP((18.678D0-Tempc/234.5D0)*Tempc/(257.14+Tempc))
! WMO 2008, CIMO Guide
!      sat_vapor_press_poly = 6.112*EXP(17.62*Tempc/(243.12+Tempc))
      END FUNCTION sat_vapor_press_poly

!***********************************************************************
! Compute saturation vapor pressure over water
! Irmak and others (2012)
!***********************************************************************
      REAL FUNCTION sat_vapor_press(Tempc)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Tempc
!***********************************************************************
      sat_vapor_press = 6.1078*EXP(17.269*Tempc/(237.30+Tempc))
      END FUNCTION sat_vapor_press