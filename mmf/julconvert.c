/*
 * convert Gregorian days to Julian date
 *
 * Compile with 'cc greg2jul.c -o greg2jul'
 *
 * Modify as needed for your application.
 *
 * The Julian day starts at noon of the Gregorian day and extends
 * to noon the next Gregorian day.
 *
 * $Id: julconvert.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
		$Log: julconvert.c,v $
		Revision 1.6  2001/04/03 18:18:06  markstro
		Unknown

		Revision 1.5  2001/01/22 22:26:41  markstro
		unknown

		Revision 1.4  1999/08/24 16:34:09  markstro
		Version 1.1.1

		Revision 1.3  1996/02/19 20:00:14  markstro
		Now lints pretty clean

		Revision 1.2  1994/09/30 14:54:33  markstro
		Initial work on function prototypes.

 * Revision 1.1  1994/03/24  22:46:20  markstro
 * Initial version from TERRA
 *
 */

#define JULCONVERT_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "mms.h"


//static char *dayofweekstr[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

/*
** Takes a date, and returns a Julian day. A Julian day is the number of
** days since some base date  (in the very distant past).
** Handy for getting date of x number of days after a given Julian date
** (use jdate to get that from the Gregorian date).
** Author: Robert G. Tantzen, translator: Nat Howard
** Translated from the algol original in Collected Algorithms of CACM
** (This and jdate are algorithm 199).
*/
double getjulday(int mon, int day, int year, int h, int mi, double se) {
	DATETIME datetime;

	datetime.year = year;
	datetime.month = mon;
	datetime.day = day;
	datetime.hour = h;
	datetime.min = mi;
	datetime.sec = se;
	julday(&datetime);

	return datetime.jt;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getjulday_
 | COMMENT      : getjulday binding for Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double getjulday_(int *mon, int *day, int *year, int *h, int *mi, double *se) {
   return getjulday (*mon, *day, *year, *h, *mi, *se);
}

int dayofweek(double j) {
    j += 0.5;
    return (int) (j + 1) % 7;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : isleap_
 | COMMENT      : isleap binding for Fortran
 | PARAMETERS   : see below
 | RETURN VALUE : see below
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long isleap_ (ftnint *year) {
   return ((long)isleap((int)(*year)));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : isleap
 | COMMENT      : Thanks to Rick Webb
 | PARAMETERS   : int year - the year to test
 | RETURN VALUE : Returns 1 if year is a leap year, 0 if not.
 | RESTRICTIONS : Called from C.
\*--------------------------------------------------------------------*/
int isleap (int year) {

   double nptr;

/*
**  Check if leapyear - Start by identifying all years not
**       divisible by 4
*/
   if (modf ((double)year/4.0, &nptr)!=0) {
      return(0);
/*
**  Identify leap years that are not century years
 */
   } else if (modf((double)year/4.0, &nptr)==0 && modf(year/100.0, &nptr)!=0 ) {
      return(1);

/*
**  century years are not leap years unless divisible by 400
*/
   } else if (modf((double)year/400.0, &nptr)!=0 ) {
      return(0);

/*
**  all that's left are century years divisible by 400 which
**         are also leap years
*/
   } else {
      return(1);
   }
}
