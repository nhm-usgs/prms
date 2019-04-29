/**************************************************************************
 * param_addr.c: 
 *
 * returns a pointer to a PARAM struct which contains the given key
 * returns NULL if key not found
 *
   $Revision: 3058 $
        $Log: param_addr.c,v $
        Revision 1.4  1996/02/19 20:00:33  markstro
        Now lints pretty clean

        Revision 1.3  1994/09/30 14:54:49  markstro
        Initial work on function prototypes.

 * Revision 1.2  1994/01/31  20:17:02  markstro
 * Make sure that all source files have CVS log.
 *
 * $Id: param_addr.c 3058 2007-01-25 22:25:59Z rsregan $
 *
 **************************************************************************/
#define PARAM_ADDR_C
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : param_addr
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
PARAM * param_addr (char *key) { 
  PARAM **params;
  long i;

  if (Mnparams == 0) return NULL; /* no parameters to locate */

  /*
   * get params from Mparambase, the global pointer
   */

  params = Mparambase;

  /*
   * search between 0 and Mnparams-1
   */

  for (i = 0; i < Mnparams; i++) {
    if (!strcmp(params[i]->key, key))
      return params[i];
  }

  /* if no match found, return null */
  return NULL;
}

