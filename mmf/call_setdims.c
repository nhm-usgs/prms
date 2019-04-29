/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : call_setdims
 * COMMENT  :
 *
 * $Id: call_setdims.c 6195 2014-02-07 21:49:14Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdio.h>
#include "mms.h"

extern long setdims_();

int call_setdims()

{

  long retval;

  retval = setdims_();
  if (retval) {
    fprintf(stderr,"ERROR in 'setdims' routine.\n");
    fprintf(stderr,"Return val = %ld\n", retval);
    return(1);
  }
  return(0);
}
