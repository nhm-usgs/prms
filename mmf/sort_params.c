/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : sort_params.c
 * AUTHOR   : 
 * DATE     : Thu 15 Sep 1994
 * FUNCTION : sort_params
 * COMMENT  : sorts the param array so that the key for each structure
 *             is in increasing alphabetical order
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: sort_params.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: sort_params.c,v $
        Revision 1.5  1996/02/19 20:01:12  markstro
        Now lints pretty clean

        Revision 1.4  1994/09/30 14:55:20  markstro
        Initial work on function prototypes.

 * Revision 1.3  1994/09/19  15:51:17  markstro
 * Fixed multiple dimension edit parameter window.
 *
 * Revision 1.2  1994/01/31  20:17:33  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SORT_PARAMS_C
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
 | FUNCTION     : sort_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void sort_params (void) {

	PARAM **params;
	PARAM *tmpparam;
	int i, j;

	params =  Mparambase;

/*
**	Make a array of the unsorted parameter order.
*/
	if (!unsort_params) {
		unsort_params = (PARAM **)malloc (Mnparams * sizeof (PARAM *));
		for (i = 0; i < Mnparams; i++)
			unsort_params[i] = params[i];
	}

/*
**	Sort the parameter data base
*/
	for (i = Mnparams-2; i >= 0; i--) {
		for (j =  0; j <= i; j++) {
			if(strcmp(params[j]->key,params[j+1]->key) > 0) {
				tmpparam = params[j];
				params[j] = params[j+1];
				params[j+1] = tmpparam;
			}
		}
	}
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/
