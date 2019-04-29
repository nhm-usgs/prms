/**************************************************************************
 * sort_dims.c: sorts the dimen array so that the key for each
 * structure is in increasing alphabetical order
 *
 * $Id: sort_dims.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: sort_dims.c,v $
        Revision 1.5  1996/04/29 16:23:25  markstro
        Unknown

 * Revision 1.4  1996/02/19  20:01:11  markstro
 * Now lints pretty clean
 *
        Revision 1.3  1994/09/30 14:55:19  markstro
        Initial work on function prototypes.

 * Revision 1.2  1994/01/31  20:17:32  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#define SORT_DIMS_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : sort_dims
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void sort_dims (void) {
	int		i, j;
	DIMEN	*tmpdim, **dims;

	dims = (DIMEN **)(dim_db->itm);

	for (i = dim_db->count - 2; i >= 0; i--) {
		for (j =  0; j <= i; j++) {
			if (strcmp (dims[j]->name, dims[j+1]->name) > 0) {
				tmpdim = dims[j];
				dims[j] = dims[j+1];
				dims[j+1] = tmpdim;
			}
		}
	}
}
