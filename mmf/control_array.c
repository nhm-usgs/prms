/*************************************************************************
 * control_array routines
 *
 * These return pointers to particular elements
 * in a control array.
 *
 * control_array - generic, returns (char *) as a generic pointer
 * control_larray - returns long *
 * control_farray - returns float *
 * control_darray - returns double *
 * control_sarray - returns char ** - string

 * $Id: control_array.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: control_array.c,v $
        Revision 1.7  1996/02/19 19:59:35  markstro
        Now lints pretty clean

        Revision 1.6  1995/02/01 17:47:16  markstro
        Addition of Rosenbrock optimization.  Start of sensitivity.  Many bug fixes.

 * Revision 1.5  1994/11/22  17:19:11  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.4  1994/11/08  16:17:17  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.3  1994/09/30  14:53:54  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:15:59  markstro
 * Make sure that all source files have CVS log.
 *
 *************************************************************************/
#define CONTROL_ARRAY_C
#include <stdlib.h>
#include "mms.h"

/**************************************************************************
 * control_array.c: 
 *
 * returns a pointer to a particular entry in a CONTROL struct
 *
 * index is 0-based, max size-1
 *
 **************************************************************************/

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_array
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *control_array (char *key, long ind) {
 
  CONTROL *control;

  if ((control = control_addr(key)) == NULL) {
    (void)fprintf(stderr, 
	    "ERROR - control_array - key '%s' not found.\n", key);
    exit(1);
  }

  if (ind >= control->size) {
    (void)fprintf(stderr, 
	    "ERROR - control_array - ind %ld too high for %s.\n", ind, key);
    (void)fprintf(stderr, 
	    "Max ind is %ld.\n", control->size-1);
    exit(1);
  }

	switch (control->type) {
		case M_DOUBLE:
			return (char *) ((double *)(control->start_ptr) + ind * sizeof(double));

		case M_FLOAT:
			return (char *) ((float *)(control->start_ptr) + ind * sizeof(float));

		case M_LONG:
			return (char *) ((long *)(control->start_ptr) + ind * sizeof(long));

		case M_STRING:
			printf ("control_array: key = %s ind = %ld val = %s\n", key, ind, *((char **)control->start_ptr + ind));
//			return (char *) (((char **)(control->start_ptr)) + (ind * sizeof(char *)));
			return *((char **)control->start_ptr + ind);
	}

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_larray
 | COMMENT		: returns a pointer to a long entry 
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long *control_larray (char *key, long ind) {
  return ((long *) control_array(key, ind));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_farray
 | COMMENT		: returns a pointer to a float entry in control array
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
float *control_farray (char *key, long ind) {
  return ((float *) control_array(key, ind));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_darray
 | COMMENT		: returns a pointer to a double entry in control array
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double *control_darray (char *key, long ind) {
  return ((double *) control_array(key, ind));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_sarray
 | COMMENT		: returns a pointer to a string entry in control array
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *control_sarray (char *key, long ind) {
  return control_array(key, ind);
}
