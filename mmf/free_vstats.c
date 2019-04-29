/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : free_vstats.c
 * AUTHOR   : CADSWES
 * DATE     : 
 * FUNCTION : free_vstats
 * COMMENT  : free linked list for stats variables
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: free_vstats.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: free_vstats.c,v $
        Revision 1.7  1996/06/28 19:32:23  markstro
        (1) Fixed 3d control window.
        (2) Fixed stats.

 * Revision 1.6  1996/02/19  20:00:00  markstro
 * Now lints pretty clean
 *
        Revision 1.5  1995/05/25 14:26:29  markstro
        (1) Added batch mode
        (2) Replaced "b" functions with "mem" versions

 * Revision 1.4  1994/10/24  14:18:25  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.3  1994/09/30  14:54:17  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:16:23  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define FREE_VSTATS_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : free_vstats
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void free_vstats (void) {
  long nstatVars;
  STAT_LIST_TYPE *curr_stat_list, *prev_stat_list;

  nstatVars = *control_lvar("nstatVars");

  if (nstatVars > 0) {

    curr_stat_list  = Mfirst_stat_list;

    while (curr_stat_list->next != NULL) {
      	prev_stat_list = curr_stat_list;
		curr_stat_list = prev_stat_list->next;
//    	ufree((char *)prev_stat_list);
    }
//      ufree((char *)curr_stat_list);
	Mfirst_stat_list = NULL;
  }
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

