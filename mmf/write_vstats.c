/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : write_vstats.c
 * AUTHOR   : Pedro Restrepo CADSWES
 * DATE     : June 1990
 * FUNCTION : write_vstats
 * COMMENT  : saves values of stat variables into a temporary file.
 *            The temporary file was open in user_input
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: write_vstats.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: write_vstats.c,v $
        Revision 1.4  1996/02/19 20:01:24  markstro
        Now lints pretty clean

        Revision 1.3  1994/10/24 14:19:09  markstro
        (1)  Integration of CADSWES's work on GIS.
        (2)  Prototypes were added to the files referenced in "mms_proto.h".

 * Revision 1.2  1994/01/31  20:17:56  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define WRITE_VSTATS_C
#include <stdio.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : write_vstats
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void write_vstats (FILE *statvar_file) { 

  STAT_LIST_TYPE *stat_list;

  /*
   * writes first record number, date and time info
   */

  (void)fprintf(statvar_file, "%ld %ld %ld %ld %ld %ld %ld ",
	  Mnsteps, Mnowtime->year,
	  Mnowtime->month, Mnowtime->day, Mnowtime->hour,
	  Mnowtime->min, Mnowtime->sec);

  /*
   * Initializes linked list to first pointer
   */

  stat_list = Mfirst_stat_list;

  /*
   * The list is NULL-terminated
   */

  while (stat_list)  {
    
    /*
     * Gets variable value
     */

    switch (stat_list->type) {

    case M_FLOAT:

      (void)fprintf(statvar_file,"%f ", *(float *)stat_list->value);
      break;

    case M_DOUBLE:

      (void)fprintf(statvar_file,"%lf ", *(double *)stat_list->value);
      break;

    case M_LONG:

      (void)fprintf(statvar_file,"%ld ", *(long *)stat_list->value);
      break;

    }

    /*
     * Updates pointer
     */

    stat_list = stat_list->next;

  }

  (void)fprintf(statvar_file,"\n");

}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

