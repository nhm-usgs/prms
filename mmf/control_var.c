/*************************************************************************
 * control_var.c : returns pointers to various control array entries
 *
 * control_var - generic, returns (char *) as a generic pointer
 * control_lvar - returns long *
 * control_fvar - returns float *
 * control_dvar - returns double *
 * control_svar - returns char ** - string

 * $Id: control_var.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: control_var.c,v $
        Revision 1.6  1996/02/19 19:59:36  markstro
        Now lints pretty clean

        Revision 1.5  1994/11/22 17:19:13  markstro
        (1) Cleaned up dimensions and parameters.
        (2) Some changes due to use of malloc_dbg.

 * Revision 1.4  1994/11/08  16:17:18  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.3  1994/09/30  14:53:55  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:16:00  markstro
 * Make sure that all source files have CVS log.
 *
 *************************************************************************/

#define CONTROL_VAR_C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_var
 | COMMENT		: returns a pointer to the start of the variable
 |			( or first element in * the array) in a CONTROL struct
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *control_var (char *key) {
 
  CONTROL *control;

  if ((control = control_addr(key)) == NULL) {
    (void)fprintf(stderr, 
	    "ERROR - control_var - key '%s' not found.\n", key);
    exit(1);
  }

  return (char *) control->start_ptr;

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_lvar
 | COMMENT		: returns a pointer to a long variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long *control_lvar (char *key) {
  return ((long *) control_var(key));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_fvar
 | COMMENT		: returns a pointer to a float variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
float *control_fvar (char *key) {
  return ((float *) control_var(key));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_dvar
 | COMMENT		: returns a pointer to a double variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double *control_dvar (char *key) {
  return ((double *) control_var(key));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_svar
 | COMMENT		: returns a pointer to a string variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char **control_svar (char *key) {
  return ((char **) control_var(key));
}
/*--------------------------------------------------------------------*\
 | FUNCTION     : control_string_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_string_ (char *retval, char *tag, ftnlen len, ftnlen tlen) {
	char *foo;

	foo = (char *) umalloc(tlen + 1);
	strncpy(foo, tag, tlen);
	foo[tlen] = '\0';

	memset (retval, ' ', len);
	strncpy (retval, *control_svar(foo), len);
	return 0;
}



/*--------------------------------------------------------------------*\
 | FUNCTION     : control_string_array_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_string_array_ (char *retval, char *tag, int *index, ftnlen len, ftnlen tlen) {
	char *foo;
    char **strings;
    int i;

	foo = (char *) umalloc(tlen + 1);
	strncpy(foo, tag, tlen);
	foo[tlen] = '\0';

    strings = (char **) control_var(foo);
    i = *index - 1;
	strncpy (retval, *(strings+i), len);
	return 0;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_integer_
 | COMMENT		: returns a long variable value
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_integer_ (int *retval, char *key, ftnlen len) {
	char *foo;

	foo = (char *) umalloc(len + 1);
	strncpy(foo, key, len);
	foo[len] = '\0';

	*retval = *control_var(foo);
	return 0;
}
