/*********************************************************
 * call_modules.c: to replace the one created by 'mbuild',
 * used to call a Fortran version, such as for GSFLOW
 * Creation time: Wed Jan 18 15:52:21 2007
 * Creation time: Thu May 26 10:54:21 2005
 *********************************************************/

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
