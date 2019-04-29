/*+
 * United States Geological Survey
 *
 * PROJECT  : BMAT
 * NAME     : build_lists.c
 * AUTHOR   : Steve Markstrom (markstro)
 * DATE     : Wed 04 Jan 1995
 * FUNCTION :
 * COMMENT  :
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: build_lists.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: build_lists.c,v $
        Revision 1.4  1996/04/09 21:04:02  markstro
        (1) Work on control files
        (2) Runtime graphs

 * Revision 1.3  1996/02/19  19:59:30  markstro
 * Now lints pretty clean
 *
        Revision 1.2  1996/01/23 18:44:08  markstro
        Fixes for HP compiler

 * Revision 1.1  1996/01/23  16:49:35  markstro
 * Initial version
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define BUILD_LISTS_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

#define INPUT  1
#define OUTPUT  2

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL data ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : ALLOC_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
LIST *ALLOC_list (char *name, int type, int size) {

	LIST	*list;

	list = (LIST *)malloc (sizeof (LIST));

	if (name)
		list->name = strdup (name);
	else
		list->name = NULL;

	list->size = size;
	list->count = 0;
	list->type = type;
	list->out = FALSE;
	list->user_data = NULL;
	if (size)
		list->itm = (void **)malloc (size * sizeof (void *));
	else 
		list->itm = NULL;

	return (list);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DELETE_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void DELETE_list (LIST *list) {

	int		i;

	if (list->name)
		free (list->name);

	for (i = 0; i < list->count; i++)
		if (list->itm[i])
			free (list->itm[i]);

	free (list->itm);
	free (list);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : RESIZE_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void RESIZE_list (LIST *list, int new_size) {
	list->size = new_size;
	list->itm = (void **)realloc (list->itm, new_size * sizeof (void *));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ADD_to_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void ADD_to_list (LIST *list, void *itm) {
	if (list->count >= list->size)
		RESIZE_list (list, list->size + 100);

	list->itm[list->count++] = itm;
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

