/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : call_modules
 * COMMENT  : used to call a Fortran version
 *
 * $Id: call_modules.c 6195 2014-02-07 21:49:14Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdlib.h>
#include <string.h>
#include "mms.h"

extern long call_modules_ (char *, ftnlen);

int call_modules(char *arg) {
	 long retval;
	 ftnlen len;

	 len = (ftnlen)strlen(arg);
	 retval = call_modules_ (arg, len);
	 return((int)retval);
}

