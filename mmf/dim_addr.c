/**************************************************************************
 * dim_addr.c: 
 *
 * returns a pointer to a DIMEN struct which contains the given name
 * returns NULL if name not found
 *
 * $Id: dim_addr.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: dim_addr.c,v $
        Revision 1.7  1996/04/29 16:23:00  markstro
        Unknown

 * Revision 1.6  1996/02/19  19:59:52  markstro
 * Now lints pretty clean
 *
        Revision 1.5  1994/11/22 17:19:28  markstro
        (1) Cleaned up dimensions and parameters.
        (2) Some changes due to use of malloc_dbg.

 * Revision 1.4  1994/09/30  14:54:11  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.3  1994/09/09  14:56:24  markstro
 * (1)  Fixed up main edit menu.
 * (2)  Added a "notes" field to dimension indicies
 * (3)  A little more Rosenbrock work.
 * (4)  Fixed the list selector -- changed button names & first item
 *      selected by default.
 * (5)  Modified spread sheet help to be able to display dimension notes
 * (6)  Ran some source through "cb"
 *
 * Revision 1.2  1994/01/31  20:16:13  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#define DIM_ADDR_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : dim_addr
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
DIMEN *dim_addr (char *name) { 
	long i;

	if (!dim_db->count)
		return (NULL);

	for (i = 0; i < dim_db->count; i++)
		if (!strcmp (((DIMEN *)(dim_db->itm[i]))->name, name))
			return ((DIMEN *)(dim_db->itm[i]));

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : dim_notes
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *dim_notes (char *ch_ptr) {
	int		i, j;
	DIMEN	*dim;

	for (i = 0; i < dim_db->count; i++) {
		dim = (DIMEN *)(dim_db->itm[i]);
		for (j = 0; j < dim->value; j++)
			if (dim->names && dim->names[j] && (!strcmp (dim->names[j],ch_ptr)))
				return (dim->notes[j]);
	}

	return (NULL);
}
