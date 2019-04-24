/*************************************************
 * call_setdims.c: created by 'mbuild'.
 * Creation time: Tue May 23 11:13:30 2006
 *************************************************/

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
