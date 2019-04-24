/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : load_param.c
 * AUTHOR   :
 * DATE     :
 * FUNCTION : load_param
 * COMMENT  : Stores the parameter value, minima and maxima at the
 *  required address.  Uses str_to_vals to decode the strings and
 *  store the values. This routine mainly handles the error conditions.
 *  Examples of legal strings for this routine are given in str_to_vals.c
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: load_param.c 6064 2011-10-27 20:56:21Z markstro $
 *
   $Revision: 6064 $
        $Log: load_param.c,v $
        Revision 1.5  1996/02/19 20:00:15  markstro
        Now lints pretty clean

        Revision 1.4  1994/11/22 17:19:49  markstro
        (1) Cleaned up dimensions and parameters.
        (2) Some changes due to use of malloc_dbg.

 * Revision 1.3  1994/09/30  14:54:33  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:16:40  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define LOAD_PARAM_C
#include <stdio.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : load_param
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : Returns 0 if successful, 1 otherwise.
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long load_param (PARAM *param) {

	long i;
	double *dval, *dmin, *dmax, *ddef;
	float *fval, *fmin, *fmax, *fdef;
	long *lval, *lmin, *lmax, *ldef;
	char *sval, *sdef;

	if (param->type == M_DOUBLE) {
		param->value = (char *)umalloc (param->size * sizeof (double));
		param->def = (char *)umalloc (param->size * sizeof (double));
		param->min = (char *)umalloc (param->size * sizeof (double));
		param->max = (char *)umalloc (param->size * sizeof (double));
	} else if (param->type == M_FLOAT) {
		param->value = (char *)umalloc (param->size * sizeof (float));
		param->def = (char *)umalloc (param->size * sizeof (float));
		param->min = (char *)umalloc (param->size * sizeof (float));
		param->max = (char *)umalloc (param->size * sizeof (float));
	} else if (param->type == M_LONG) {
		param->value = (char *)umalloc (param->size * sizeof (long));
		param->def = (char *)umalloc (param->size * sizeof (long));
		param->min = (char *)umalloc (param->size * sizeof (long));
		param->max = (char *)umalloc (param->size * sizeof (long));
	} else if (param->type == M_STRING) {
		param->value = (char *)umalloc (param->size * sizeof (char *));
		param->def = (char *)umalloc (param->size * sizeof (char *));
		param->min = (char *)umalloc (param->size * sizeof (char *));
		param->max = (char *)umalloc (param->size * sizeof (char *));
	}

/*
* decode minima
*/
	if (param->bound_status == M_BOUNDED) {
		lmin = (long *)(param->min);	
		for (i = 0; i < param->size; i++)
			*lmin++ = 0;
	} else {
		if (str_to_vals (param->min_string, param->size,
									param->type, param->min)) {
			(void)fprintf (stderr, "Parameter is '%s'\n", param->key);
			(void)fprintf (stderr, "Decoding minimum values.\n");
			(void)fprintf (stderr, "Encoded string is:\n'%s'\n", param->min_string);
			return (1);
		}
	}

/*
* decode maxima
*/
	if (param->bound_status == M_BOUNDED) {
		lmax = (long *)(param->max);	
		for (i = 0; i < param->size; i++)
			*lmax++ = (long)(param->bound_dimen->value);
	} else {
		if (str_to_vals (param->max_string, param->size,
									param->type, param->max)) {
			(void)fprintf (stderr,"Parameter is '%s'\n", param->key);
			(void)fprintf (stderr,"Decoding maximum values.\n");
			(void)fprintf (stderr,"Encoded string is:\n'%s'\n",param->max_string);
			return (1);
		}
	}

/*
* decode default values
*/
	if (str_to_vals (param->value_string, param->size, param->type,
								param->def)) {
		(void)fprintf(stderr,"Parameter is '%s'\n", param->key);
		(void)fprintf(stderr,"Decoding default values.\n");
		(void)fprintf(stderr,"Encoded string is:\n'%s'\n",param->value_string);
		return(1);
	}

	switch (param->type) {
		case M_DOUBLE:
			dval = (double *)param->value;
			ddef = (double *)param->def;
			for (i = 0; i < param->size; i++)
				*dval++ = *ddef++;
			break;

		case M_FLOAT:
			fval = (float *)param->value;
			fdef = (float *)param->def;
			for (i = 0; i < param->size; i++)
				*fval++ = *fdef++;
			break;

		case M_LONG:
			lval = (long *)param->value;
			ldef = (long *)param->def;
			for (i = 0; i < param->size; i++)
				*lval++ = *ldef++;
			break;

		case M_STRING:
			sval = (char *)param->value;
			sdef = (char *)param->def;
			for (i = 0; i < param->size; i++)
				*sval++ = *sdef++;
			break;
	}

/*
* check that the defaults lie within the min and max range
*/
	switch (param->type) {

	case M_DOUBLE:

		dval = (double *) param->value;
		dmin = (double *) param->min;
		dmax = (double *) param->max;

		for (i = 0; i < param->size; i++) {

			if (dmin[i] > dmax[i]) {
				(void)fprintf(stderr,
				    "ERROR - load_param - min value exceeds max value.\n");
				(void)fprintf(stderr, "Key is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Min and max strings are:\nMin: '%s'\nMax: '%s'\n",
				    param->min_string, param->max_string);
				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Min = %lf, max = %lf\n", dmin[i], dmax[i]);
				return(1);
			}

			if (dval[i] < dmin[i] || dval[i] > dmax[i]) {
				(void)fprintf(stderr,
				    "ERROR - load_param - default value out of range.\n");
				(void)fprintf(stderr, "Key is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Val, min and max strings are:\nVal: '%s'\nMin: '%s'\nMax: '%s'\n",
				    param->value_string, param->min_string, param->max_string);
				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Val = %lf, min = %lf, max = %lf\n",
				    dval[i], dmin[i], dmax[i]);
				return(1);
			}

		}

		break;

	case M_FLOAT:

		fval = (float *) param->value;
		fmin = (float *) param->min;
		fmax = (float *) param->max;

		for (i = 0; i < param->size; i++) {

			if (fmin[i] > fmax[i]) {
				(void)fprintf(stderr,
				    "ERROR - load_param - min value exceeds max value.\n");
				(void)fprintf(stderr, "Key is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Min and max strings are:\nMin: '%s'\nMax: '%s'\n",
				    param->min_string, param->max_string);
				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Min = %f, max = %f\n", fmin[i], fmax[i]);
				return(1);
			}

			if (fval[i] < fmin[i] || fval[i] > fmax[i]) {
				(void)fprintf(stderr,
				    "ERROR - load_param - default value out of range.\n");
				(void)fprintf(stderr, "Key is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Val, min and max strings are:\nVal: '%s'\nMin: '%s'\nMax: '%s'\n",
				    param->value_string, param->min_string, param->max_string);
				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Val = %f, min = %f, max = %f\n",
				    fval[i], fmin[i], fmax[i]);
				return(1);
			}

		}

		break;

	case M_LONG:

		lval = (long *) param->value;
		lmin = (long *) param->min;
		lmax = (long *) param->max;

		for (i = 0; i < param->size; i++) {

			if (lmin[i] > lmax[i]) {
				(void)fprintf(stderr,
				    "ERROR - load_param - min value exceeds max value.\n");
				(void)fprintf(stderr, "Key is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Min and max strings are:\nMin: '%s'\nMax: '%s'\n",
				    param->min_string, param->max_string);
				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Min = %ld, max = %ld\n", lmin[i], lmax[i]);
				return(1);
			}

			if (lval[i] < lmin[i] || lval[i] > lmax[i]) {
				(void)fprintf(stderr,
				    "ERROR - load_param - default value out of range.\n");
				(void)fprintf(stderr, "Key is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Val, min and max strings are:\nVal: '%s'\nMin: '%s'\nMax: '%s'\n",
				    param->value_string, param->min_string, param->max_string);
				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Val = %ld, min = %ld, max = %ld\n",
				    lval[i], lmin[i], lmax[i]);
				return(1);
			}
		}
		break;

	case M_STRING:
// Nothing to check
		break;

	}
	return(0);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

