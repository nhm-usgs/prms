/**************************************************************************
 * getdim.c: gets the dimension associated with a name, and
 * returns it as a long int. Returns -1 if error.
 *
 * There are 2 functions: getdim() to be called from C
 *                        getdim_() to be called from Fortran
 *
 * $Id: getdim.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: getdim.c,v $
        Revision 1.5  1996/02/19 20:00:03  markstro
        Now lints pretty clean

        Revision 1.4  1994/11/22 17:19:38  markstro
        (1) Cleaned up dimensions and parameters.
        (2) Some changes due to use of malloc_dbg.

 * Revision 1.3  1994/09/30  14:54:21  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:16:28  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#define GETDIM_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**************************************************************************
 */

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdim_
 | COMMENT		: called from Fortran, sorts out args and calls getdim()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdim_ (char *dname, ftnlen namelen) {
  char *name;
  long retval;

  /*
   * copy name and terminate
   */

  name = (char *) umalloc(namelen + 1);
  strncpy(name, dname, namelen);
  name[namelen] = '\0';

  /*
   * call C version of getdim()
   */

  retval =  getdim(name);

  /*
   * free up array
   */

//ufree(name);

  return retval;

}

/**************************************************************************
 */
/*--------------------------------------------------------------------*\
 | FUNCTION     : getdim
 | COMMENT		: is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdim (char *name) {

  DIMEN *dim;

  /*
   * get pointer to dimension with name
   */

  dim = dim_addr(name);

  if (dim == NULL) {
    (void)fprintf(stderr, 
	    "ERROR - getdim - dimension not found.\n");
    (void)fprintf(stderr, "Name:   '%s'\n", name);
    return(-1L);
  }

  /*
   * return the dimension
   */

  dim->got = TRUE;
  return dim->value;

}

