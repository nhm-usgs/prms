/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : get_times.c
 * AUTHOR   : Mike Dixon CADSWES
 * DATE     : March 1990
 * FUNCTION : get_times
 * COMMENT  : get start and end times from control data base
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: get_times.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: get_times.c,v $
        Revision 1.5  1996/02/19 20:00:02  markstro
        Now lints pretty clean

        Revision 1.4  1994/10/24 14:18:30  markstro
        (1)  Integration of CADSWES's work on GIS.
        (2)  Prototypes were added to the files referenced in "mms_proto.h".

 * Revision 1.3  1994/09/30  14:54:20  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:16:27  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define GET_TIMES_C
#include <stdio.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : get_times
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void get_times (void) {
  long *datetime;
  float *newvalue;

  datetime = (long *) control_var("start_time");
  Mstrttime->year = datetime[0];
  Mstrttime->month = datetime[1];
  Mstrttime->day = datetime[2];
  Mstrttime->hour = datetime[3];
  Mstrttime->min = datetime[4];
  Mstrttime->sec = datetime[5];

  datetime = (long *) control_var("end_time");
  Mendtime->year = datetime[0];
  Mendtime->month = datetime[1];
  Mendtime->day = datetime[2];
  Mendtime->hour = datetime[3];
  Mendtime->min = datetime[4];
  Mendtime->sec = datetime[5];

  /* compute julian day for start and end  - this fills in the julian date
     parts of the datetime data structure */

  julday(Mstrttime);
  julday(Mendtime);

  newvalue = (float *) control_var("initial_deltat");
  Mdeltat = (double)(*newvalue / 24.0);
  Mdeltanext = (double)(*newvalue / 24.0);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

