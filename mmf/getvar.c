/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : getvar.c
 * AUTHOR   : CADSWES
 * DATE     : Mon 08 Apr 1996
 * FUNCTION :
 * COMMENT  :
 * getvar.c: gets the value associated with a module and name, and copies
 * it into the variable provided by the calling routine.
 *
 * There are 2 functions: getvar() to be called from C
 *                        getvar_() to be called from Fortran
 *
 * Returns 0 if successful, 1 otherwise.
 *
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: getvar.c 7446 2012-08-30 21:41:41Z rsregan $
 *
   $Revision: 7446 $
        $Log: getvar.c,v $
        Revision 1.8  1996/04/09 21:04:06  markstro
        (1) Work on control files
        (2) Runtime graphs

 * Revision 1.7  1996/02/19  20:00:06  markstro
 * Now lints pretty clean
 *
        Revision 1.6  1995/05/25 14:26:31  markstro
        (1) Added batch mode
        (2) Replaced "b" functions with "mem" versions

 * Revision 1.5  1994/11/22  17:19:41  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.4  1994/09/30  14:54:25  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.3  1994/06/16  16:47:10  markstro
 * Worked over runcontrol.c
 *
 * Revision 1.2  1994/01/31  20:16:33  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION		: getvar_
 | COMMENT		: called from Fortran, sorts out args and calls getvar()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getvar_ (char *mname, char *vname, ftnint *vmaxsize, char *vtype, double *value, ftnlen mnamelen, ftnlen vnamelen, ftnlen vtypelen) {
	char module[80], name[80], type[80];
	long maxsize, retval;
  
/*
* copy size to local long int
* copy args to new strings, and terminate
*/
	maxsize = *vmaxsize;

	strncpy (module, mname, mnamelen);
	*(module + mnamelen) = '\0';

	strncpy (name, vname, vnamelen);
	*(name + vnamelen) = '\0';

	strncpy (type, vtype, vtypelen);
	*(type + vtypelen) = '\0';
  
/*
* call C version of getvar()
*/
	retval =  getvar (module, name, maxsize, type, value);
  
	return (retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getvar
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getvar (char *module, char *name, long maxsize, char *type, double *value) {

	int var_type;
	PUBVAR *var;
//	char *vkey;
	char vkey[128];
	long i;
	long n1, n2;
	char *ptr1;
	char *ptr2;

/*
* compute the key
*/
  
/*
  vkey = (char *) umalloc(strlen(module) + strlen(name) + 2);
  (void)strcpy(vkey, module);
  strcat(strcat(vkey, "."), name);
*/
//  vkey = strdup (name);
   strncpy (vkey, name, 128);
  
/*
* convert fortran types to C types
*/

	var_type = M_LONG;
	if (!strcmp(type, "real") || !strcmp(type, "float"))
	  var_type = M_FLOAT;
	else if (!strcmp(type, "double precision") || !strcmp(type, "double"))
	  var_type = M_DOUBLE;

/*
* check that type is possible
*/
	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE)){
		(void)fprintf(stderr,
				"ERROR - getvar - type %s is illegal.\n", type);
		(void)fprintf(stderr, "Key is '%s'.\n", vkey);
		(void)fprintf(stderr, "Type is '%s'.\n", type);
		return(1);
	}
  
/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - getvar - variable not found.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		return(1);
	}
  
/*
* check that there is enough space allocated in the calling routine
* to accommodate the data
*/
  
	if (var->size > maxsize) {
		(void)fprintf (stderr, 
	    			"ERROR - getvar - insufficient space for data transfer.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		(void)fprintf(stderr, "Actual size in data base: %ld\n", var->size);
		(void)fprintf(stderr, "Available space in calling routine: %ld\n",
						maxsize);
		return(1);
	}

/*
	if (strcmp(Mtypes[var->type], type)) {
		(void)fprintf(stderr, 
				"ERROR - getvar - incorrect data type requested.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		(void)fprintf(stderr, "Requested type: %s\n", type);
		(void)fprintf(stderr, "Actual declared type: %s\n",
							Mtypes[var->type]);
		return(1);
	}
*/
  
/*
* copy the variable across
*/
  
	if (var->ndimen == 1) {
		switch (var->type) {
			case M_LONG:
				memcpy ((char *)value, (char *)var->value,
							var->size * sizeof(long));
				break;
      
			case M_FLOAT:
				memcpy ((char *)value, (char *)var->value,
							var->size * sizeof(float));
				break;
      
			case M_DOUBLE:
				memcpy ((char *)value, (char *)var->value,
							var->size * sizeof(double));
				break;
		}
	} else if (var->ndimen ==2) {
		n1 = var->dimen[0]->value;
		n2 = var->dimen[1]->value;
		if (n1*n2!=maxsize) n1 = var->dimen[0]->max;
/*rsr added next block*/
		if (n1*n2 > maxsize ) {
			n1 = var->dimen[0]->value;
		}
/*rsr end block*/
		ptr1 = var->value;
		ptr2 = (char *)value;

		for (i = 0; i < n2; i++) {

			switch (var->type) {
				case M_LONG:
					memcpy ((char *)ptr2, (char *)ptr1, n1 * sizeof(long));
					ptr1 += n1 * sizeof(long);
					ptr2 += n1 * sizeof(long);
					break;

				case M_FLOAT:
					memcpy ((char *)ptr2, (char *)ptr1, n1 * sizeof(float));
					ptr1 += n1 * sizeof(float);
					ptr2 += n1 * sizeof(float);
					break;

				case M_DOUBLE:
					memcpy ((char *)ptr2, (char *)ptr1, n1 * sizeof(double));
					ptr1 += n1 * sizeof(double);
					ptr2 += n1 * sizeof(double);
					break;
			}
		}
	}

//      free (vkey);
	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION		: getvartype_
 | COMMENT		: called from Fortran, sorts out args and returns the type()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getvartype_ (char *vname, ftnlen vnamelen) {
	char vkey[128];
	PUBVAR *var;
  
    strncpy (vkey, vname, 128);
/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - getvartype - variable not found.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		return(-1);
	}

	return (var->type);
}

/*--------------------------------------------------------------------*\
 | FUNCTION		: getvarsize_
 | COMMENT		: called from Fortran, sorts out args and returns the variable size()
 | PARAMETERS   :
 | RETURN VALUE : size of the array for input variable
 | RESTRICTIONS : variable must be declared
\*--------------------------------------------------------------------*/
long getvarsize_ (char *vname, ftnlen vnamelen) {
	char vkey[128];
	PUBVAR *var;
  
    strncpy (vkey, vname, 128);
/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - getvartype - variable not found.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		return(-1);
	}

	return (var->size);
}
/**8************************** TEST DRIVER ****************************/

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

