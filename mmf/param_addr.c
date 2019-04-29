/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : param_addr
 *            returns a pointer to a PARAM struct which contains the given key
 *            returns NULL if key not found
 * COMMENT  :
 *
 * $Id: param_addr.c 6195 2014-02-07 21:49:14Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
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
