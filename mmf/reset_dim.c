/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : reset_dim.c
 * AUTHOR   : CADSWES
 * DATE     : 
 * FUNCTION :
 * COMMENT  :
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: reset_dim.c 5702 2010-07-28 20:42:46Z rsregan $
 *
   $Revision: 5702 $
        $Log: reset_dim.c,v $
        Revision 1.15  1999/10/22 17:14:37  markstro
        Added private variables

        Revision 1.14  1996/04/29 16:23:15  markstro
        Unknown

 * Revision 1.13  1996/02/19  20:00:49  markstro
 * Now lints pretty clean
 *
        Revision 1.12  1995/03/20 20:42:27  markstro
        Import fix

 * Revision 1.11  1994/11/23  20:12:50  markstro
 * More malloc_dbg changes
 *
 * Revision 1.10  1994/11/22  17:20:17  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.9  1994/11/08  16:17:41  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.8  1994/10/13  17:53:37  markstro
 * (1) Added annotation to parameter values through the spreadsheet
 * (2) Included <string.h> in a few more files that needed it.
 *
 * Revision 1.7  1994/09/30  14:55:02  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.6  1994/09/23  22:49:12  markstro
 * Unknown
 *
 * Revision 1.5  1994/09/15  22:13:53  markstro
 * Fixes for dimension index notes.
 *
 * Revision 1.4  1994/09/15  17:22:46  markstro
 * Added the call declfix to the system for declaring fixed dimensions.
 *
 * Revision 1.3  1994/05/23  14:27:26  markstro
 * Cleaned out a lot of includes in include files
 *
 * Revision 1.2  1994/01/31  20:17:19  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define RESET_DIM_C 0
#define VALUE_CASE 0
#define MIN_CASE 1
#define MAX_CASE 2
#define NCASES 3

#include <stdio.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static void resize_param (PARAM *, long, long, long, long);

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : reset_dim
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void reset_dim (DIMEN *dim, long nnew) {
	int dimen_used;
	long iparam, ivar, idimen, i, j;
	long size_new, nold;
	long dimen_num;

	long *lptr_max, *lptr_value;

	PARAM *param;
	PUBVAR *var;

	if (nnew == dim->value) return;

/*
**	reset the dimension
*/
	nold = dim->value;
	dim->value = nnew;

/*
**	if existing entry had any index names, free the excess ones, if new size is
**	smaller than previous size. Otherwise, fill the new ones with null strings.
*/

	if (nnew > nold) {
		if (dim->names) {
			dim->names = (char **)realloc ((char **)dim->names,
				nnew * (sizeof(char *)));
			for (i = nold ; i < nnew; i++)
				dim->names[i] = NULL;
		}

		if (dim->notes) {
			dim->notes = (char **)realloc ((char **)dim->notes,
				nnew * (sizeof(char *)));
			for (i = nold ; i < nnew; i++)
				dim->notes[i] = NULL;
		}	

	} else {
		for (i = nnew + 1; i < nold; i++) {
			if (dim->names && dim->names[i]) {
//				free (dim->names[i]);
				dim->names[i] = NULL;
			}

			if (dim->notes && dim->notes[i]) {
//				free (dim->notes[i]);
				dim->notes[i] = NULL;
			}
		}

		if (nnew) {
			if (dim->names)
				dim->names = (char **)realloc ((char **)dim->names,
					nnew * sizeof (char *));
			if (dim->notes)
				dim->notes = (char **)realloc ((char **)dim->notes,
					nnew * sizeof (char *));
		} else {
			dim->names = NULL;
			dim->notes = NULL;
		}
	}

/*
* search through params for parameters which use this dimension
* and resize
*/

	for (iparam = 0; iparam < Mnparams; iparam++) {
		param = Mparambase[iparam];
		dimen_used = FALSE;
		size_new = 1;
		dimen_num = 1;

		for (idimen = 0; idimen < param->ndimen; idimen++) {
			size_new *= param->dimen[idimen]->value;
			if (dim == param->dimen[idimen]) {
				dimen_num = idimen;
				dimen_used = TRUE;
			}
		}
/*
* if this dimension is used by this parameter, resize the parameter
* array
*/
		if (dimen_used) {

/*
* if size_new is zero, set size_new to 1 so that there is at least one
* entry in the parameters data base. This is necesary so that the
* default, maximum and minimum values will be retained for use when
* the size is set to a non-zero value
*/
			if (size_new == 0)
				size_new = 1;

			resize_param (param, dimen_num, nold, nnew, size_new);
			param->size = size_new;
		}
	}

/*
* if a param is bounded by this dimension,
* reset the maximum values accordingly, and the set the current
* values to the maximum if they exceed it
*/
	for (iparam = 0; iparam < Mnparams; iparam++) {
		param = Mparambase[iparam];
		if((param->bound_status == M_BOUNDED) &&
		 					(param->bound_dimen == dim)) {
/*
 (void)fprintf (stderr,"check bound max for %s\n", param->name);
 (void)fprintf (stderr,"   dim = %s;   bound dim = %s\n", dim->name, param->bound_dimen->name);
*/
			lptr_value = (long *) param->value;
			lptr_max = (long *) param->max;

			for (j = 0; j < param->size; j++) {
				lptr_max[j] = dim->value;
/*
 (void)fprintf (stderr,"   j = %d\n", j);
*/
				if (lptr_value[j] > lptr_max[j])
					lptr_value[j] = lptr_max[j];
			}
		}
	}

/*
* search through vars for variables which use this dimension
* and reset size
*/
   for (ivar = 0; ivar < Mnvars; ivar++) {
      var = Mvarbase[ivar];
      if (!(var->private)) {
         dimen_used = FALSE;
         size_new = 1;

         for (idimen = 0; idimen < var->ndimen; idimen++) {
            size_new *= var->dimen[idimen]->value;
            if (dim == var->dimen[idimen]) {
               dimen_num = idimen;
               dimen_used = TRUE;
            }
         }
/*
* if this dimension is used by this variable, resize the variable
*/
         if (dimen_used) {
            var->size = size_new;
         }
      }
   }
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : resize_param
 | COMMENT		: resizes and repacks param array to take account of
 |                  a change in the value of a dimension
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static void resize_param (PARAM *param, long dimen_num, long nold, long nnew, long size_new) {

	char *aptr_prev, *aptr_new;

	long i, j, iframe, inew, iold, icase;
	long nframes;
	long blocksize;
	long new_framesize, old_framesize;
	long new_index, old_index;
	long *lptr_prev, *lptr_new;

	float *fptr_prev, *fptr_new;

	double *dptr_prev, *dptr_new;

/*
* compute the number of frames
*/

	nframes = 1;

	for (i = dimen_num + 1; i < param->ndimen; i++)
		nframes *= param->dimen[i]->value;

/*
* compute the block size
*/

	blocksize = 1;

	for (i = 0; i < dimen_num; i++)
		blocksize *= param->dimen[i]->value;

/*
* compute the old and new frame sizes
*/

	old_framesize = blocksize * nold;
	new_framesize = blocksize * nnew;

/*
**	resize the value_desc
*/
	if (size_new)
		param->value_desc = (char **) realloc (param->value_desc,
			size_new * sizeof (char *));

	for (i = param->size; i < size_new; i++)
		param->value_desc[i] = NULL;

/*
* copy the data
*/
	for (icase = 0; icase < NCASES; icase++) {
		switch (icase) {
			case VALUE_CASE:
				aptr_prev = param->value;
				break;

			case MIN_CASE:
				aptr_prev = param->min;
				break;

			case MAX_CASE:
				aptr_prev = param->max;
				break;
		}

		switch (param->type) {
			case M_LONG:
				lptr_prev = (long *) aptr_prev;
				aptr_new = (char *) umalloc (size_new * sizeof(long));
				lptr_new = (long *) aptr_new;
				break;

			case M_FLOAT:
				fptr_prev = (float *) aptr_prev;
				aptr_new = (char *) umalloc (size_new * sizeof(float));
				fptr_new = (float *) aptr_new;
				break;

			case M_DOUBLE:
				dptr_prev = (double *) aptr_prev;
				aptr_new = (char *) umalloc (size_new * sizeof(double));
				dptr_new = (double *) aptr_new;
				break;

		} /* switch (param->type) */

		for (iframe = 0; iframe < nframes; iframe++) {
			for (inew = 0; inew < nnew; inew++) {
				if (inew < nold)
					iold = inew;
				else
					iold = nold - 1;

				for (j = 0; j < blocksize; j++) {
					new_index = j + inew * blocksize + iframe * new_framesize;
					old_index = j + iold * blocksize + iframe * old_framesize;

					switch (param->type) {
						case M_LONG:
							lptr_new[new_index] = lptr_prev[old_index];
							break;

						case M_FLOAT:
							fptr_new[new_index] = fptr_prev[old_index];
							break;

						case M_DOUBLE:
							dptr_new[new_index] = dptr_prev[old_index];
							break;

					} /* switch (param->type) */
				} /* j */
			} /* inew */
		} /* iframe */
//		ufree(aptr_prev);

		switch (icase) {
			case VALUE_CASE:
				param->value = aptr_new;
				break;

			case MIN_CASE:
				param->min = aptr_new;
				break;

			case MAX_CASE:
				param->max = aptr_new;
				break;

		} /* switch (icase) */
	} /* icase */
}
/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

