/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : readvar.c
 * AUTHOR   : Steve Markstrom (markstro)
 * DATE     : Mon 08 Apr 1996
 * FUNCTION :
 * COMMENT  :
 * readvar.c: reads the values associated with a key from an input file,
 * and stores it in the data base
 *
 * There are 2 functions: readvar() to be called from C
 *                        readvar_() to be called from Fortran
 *
 * returns 0 if success, 1 if failure

 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: readvar.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: readvar.c,v $
        Revision 1.8  1996/04/09 21:04:14  markstro
        (1) Work on control files
        (2) Runtime graphs

 * Revision 1.7  1996/02/19  20:00:46  markstro
 * Now lints pretty clean
 *
        Revision 1.6  1994/11/22 17:20:15  markstro
        (1) Cleaned up dimensions and parameters.
        (2) Some changes due to use of malloc_dbg.

 * Revision 1.5  1994/09/30  14:55:00  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.4  1994/06/16  16:47:15  markstro
 * Worked over runcontrol.c
 *
 * Revision 1.3  1994/01/31  20:17:17  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READVAR_C
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/
#define MISSING_VAR -999

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : readvar_
 | COMMENT		: called from Fortran, sorts out args and calls readvar()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long readvar_ (char *mname, char *vname, ftnlen mnamelen, ftnlen vnamelen) {
	char module[80], name[80];
	long retval;

/*
* copy args to new strings, and terminate
*/
	strncpy (module, mname, mnamelen);
	*(module + mnamelen) = '\0';

	strncpy (name, vname, vnamelen);
	*(name + vnamelen) = '\0';

/*
* call C version of readvar()
*/
	retval = readvar (module, name);

	return (retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : readvar
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long readvar (char *module, char *name) {

	PUBVAR *var;
	long i, found;
	char vkey[80];
	long *long_var;
	float *float_var;
	double *double_var;

/*
* compute the key
*/

/*
  vkey = (char *) umalloc(strlen(module) + strlen(name) + 2);
  (void)strcpy(vkey, module);
  strcat(strcat(vkey, "."), name);
*/
	strcpy (vkey, name);

/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - readvar - variable not found.\n");
		(void)fprintf(stderr, "Key:   %s.\n", vkey);
		return(1);
	}

/*
* check that this is the correct variable, and that the size is
* set to the expected read count
*/
	found = -1;
	for (i = 0; i < Mnreads; i++) {
		if (var == Mcheckbase[i]->var) {
			found = i;
			break;
		}
	}

/*
	if (!(var->size))
		return (0);
*/

	if (found == -1) {
		(void)fprintf(stderr, "ERROR - readvar\n");
		(void)fprintf(stderr, "Attempting to read var '%s'\n", vkey);
		(void)fprintf(stderr, "Variable not found in data file\n");
		return (1);
	}

/*
* data is present in file
*/

  if(var->size != Mcheckbase[found]->count) {
    (void)fprintf(stderr, "ERROR - readvar\n");
    (void)fprintf(stderr, "Reading var '%s'\n", vkey);
    (void)fprintf(stderr, "Attempting to read %ld items\n", var->size);
    (void)fprintf(stderr, "Data file has %ld items for this variable.\n",
	    Mcheckbase[found]->count);
    return(1);

  }
    
  /*
   * copy the variable from the input line into the data base,
   * according to the type, if size > 0
   */

  if (var->size > 0) {
  
    switch (var->type) {
      
    case M_LONG:
      long_var = (long *) var->value;
      for (i = 0; i < var->size; i++) {
	long_var[i] = Mcheckbase[found]->Types.valuel[i];
      }
      break;
      
    case M_FLOAT:
      float_var = (float *) var->value;
      for (i = 0; i < var->size; i++) {
	float_var[i] = Mcheckbase[found]->Types.valuef[i];
      }
      break;
      
    case M_DOUBLE:
      double_var = (double *) var->value;
      for (i = 0; i < var->size; i++) {
	double_var[i] = Mcheckbase[found]->Types.valued[i];
      }
      break;
      
    }
    

  }  /* if(var->size > 0) */


  return(0);

}


/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/
