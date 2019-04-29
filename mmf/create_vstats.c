/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : create_vstats.c
 * AUTHOR   : CADSWES
 * DATE     : Thu 20 Oct 1994
 * FUNCTION : create_vstats
 * COMMENT  : create linked list for stats variables
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: create_vstats.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: create_vstats.c,v $
        Revision 1.10  1996/06/28 19:32:22  markstro
        (1) Fixed 3d control window.
        (2) Fixed stats.

 * Revision 1.9  1996/02/19  19:59:41  markstro
 * Now lints pretty clean
 *
        Revision 1.8  1995/05/25 14:26:23  markstro
        (1) Added batch mode
        (2) Replaced "b" functions with "mem" versions

 * Revision 1.7  1994/11/22  17:19:17  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.6  1994/11/08  16:17:20  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.5  1994/10/24  14:18:13  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.4  1994/09/30  14:53:59  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.3  1994/06/21  20:20:23  markstro
 * More work on taking the module name out of the DB keyword.
 *
 * Revision 1.2  1994/01/31  20:16:04  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define CREATE_VSTATS_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : create_vstats
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void create_vstats (void) {

  long *nstatVars, i;
  char **statVar_names;
  char **statVar_element;
  char *ptr;
  STAT_LIST_TYPE *curr_stat_list;
  PUBVAR *var;

  /*
   * get number of statVars
   */

  nstatVars = (long *) control_var("nstatVars");

  /*
   * get address of statVar names array 
   */
  
  statVar_names = (char **) control_var("statVar_names");
  
  /*
   * get address of statVar element  array 
   */
  
  statVar_element = (char **) control_svar("statVar_element");
  
/*
*	Make_linked_list;
*/
	curr_stat_list  = NULL;
	Mfirst_stat_list = NULL;
	for (i = 0; i < *nstatVars; i++) {

		if (curr_stat_list == NULL) {
			curr_stat_list = (STAT_LIST_TYPE *)umalloc(sizeof(STAT_LIST_TYPE));
			Mfirst_stat_list = curr_stat_list;
		} else {
			curr_stat_list->next =
				(STAT_LIST_TYPE *)umalloc(sizeof(STAT_LIST_TYPE));
			curr_stat_list = curr_stat_list->next;
		}

		(void)strcpy (curr_stat_list->key, statVar_names[i]);
		ptr = strchr (curr_stat_list->key, '.');
		if (ptr) *ptr = '\0';

		curr_stat_list->element = statVar_element[i];
		curr_stat_list->value = (char *)GetElemAddress (curr_stat_list->key,
					statVar_element[i], M_VARIABLE);

		if ((var = var_addr (curr_stat_list->key)) == NULL ) {
			(void)fprintf(stderr, "ERROR - create_vstats.\n");
			(void)fprintf(stderr, "Getting var_addr for var '%s'.\n",
			statVar_names[i]);
			exit(1);
		}
		curr_stat_list->type = var->type;
		curr_stat_list->next = NULL;
	}
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/
