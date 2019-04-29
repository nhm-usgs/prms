/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : control_addr.c
 * AUTHOR   : CADSWES
 * DATE     : Mon 08 Apr 1996
 * FUNCTION :
 * COMMENT  :
 * returns a pointer to a CONTROL struct which contains the given key
 * returns NULL if key not found
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: control_addr.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: control_addr.c,v $
        Revision 1.5  1996/04/09 21:04:03  markstro
        (1) Work on control files
        (2) Runtime graphs

 * Revision 1.4  1996/02/19  19:59:34  markstro
 * Now lints pretty clean
 *
        Revision 1.3  1994/09/30 14:53:53  markstro
        Initial work on function prototypes.

 * Revision 1.2  1994/01/31  20:15:58  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define CONTROL_ADDR_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : control_addr
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
CONTROL *control_addr (char *key) { 
	CONTROL	*cp;
	int		i;

	for (i = 0; i < cont_db->count; i++) {
		cp = (CONTROL *)(cont_db->itm[i]);
		//printf ("control_addr: i = %d comparing %s to %s \n", i, key, cp->key);
		if (!strcmp (cp->key, key))
			return (cp);
	}

	return (NULL);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

