/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : save_vars.c
 * AUTHOR   :
 * DATE     :
 * FUNCTION :
 * COMMENT  : saves the var data base to a file
 *             File name is passed in as arg
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: save_vars.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: save_vars.c,v $
        Revision 1.14  2000/02/18 18:27:07  markstro
        Made previous Julian time a global.  It is set to -1.0 before the run
        so that read_line knows to recalculate it.

        Revision 1.13  1999/12/07 21:10:42  markstro
        More nstep and init_var stuff

        Revision 1.12  1999/11/30 22:06:19  markstro
        Added nsteps to the var save file.

        Revision 1.11  1999/10/22 17:14:38  markstro
        Added private variables

        Revision 1.10  1996/04/29 16:23:20  markstro
        Unknown

 * Revision 1.9  1996/02/19  20:00:59  markstro
 * Now lints pretty clean
 *
        Revision 1.8  1994/12/21 21:36:23  markstro
        (1) Fixed ESP to work with multiple data files.
        (2) Fixed Optimization to work with multiple data files.
        (3) Fixed Sensitivity to work with multiple data files.

 * Revision 1.7  1994/11/22  17:20:24  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.6  1994/11/10  23:26:46  markstro
 * (1)  Some memory fixes -- results of malloc_dbg.
 * (2)  More stuff removed from set menu.
 *
 * Revision 1.5  1994/09/30  14:55:09  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.4  1994/06/21  20:20:34  markstro
 * More work on taking the module name out of the DB keyword.
 *
 * Revision 1.3  1994/05/18  17:16:04  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.2  1994/01/31  20:17:25  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SAVE_VARS_C
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
 | FUNCTION     : save_vars
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int save_vars (char *var_file_name) {
	FILE *var_file;
	PUBVAR *var;
	DIMEN *dim;
	long i,j;
	double *dvalptr;
	float *fvalptr;
	long *lvalptr;
    char *buf, *ptr;

/*
* get var file path, open file
*/
	if ((var_file = fopen (var_file_name, "w")) == NULL) {
		(void)fprintf(stderr, "ERROR - save_vars - creating file '%s'\n", var_file_name);
		return(1);
	}

/*
* write the run info string
*/
    buf = strdup (Mparaminfo);
    ptr = strchr (buf, '\n');

    if (ptr) *ptr = '\0';
   
	(void)fprintf (var_file, "%s\n", buf);

/*
* write nstep
*/
	(void)fprintf (var_file, "last nstep %ld\n", Mnsteps);

/*
* write Mnowtime->jt
*/
	(void)fprintf (var_file, "last julian time %f\n", Mnowtime->jt);

/*
* write delta time
*/
	(void)fprintf (var_file, "last delta time %f\n", Mdeltat);

/*
* write out dimensions
*/
	(void)fprintf(var_file, "** Dimensions **\n");
	for (i = 0; i < dim_db->count; i++) {
		dim = (DIMEN *)(dim_db->itm[i]);
		(void)fprintf(var_file, "####\n");
		(void)fprintf(var_file, "%s\n", dim->name);
		(void)fprintf(var_file, "%ld\n", dim->value);

	}

/*
* write out variable values
*/
	(void)fprintf(var_file, "** Variables **\n");

   for (i = 0; i < Mnvars; i++) {
      var = Mvarbase[i];
      (void)fprintf (var_file, "####\n");
      (void)fprintf (var_file, "%s\n", var->key);
      if (!(var->private)) {
         (void)fprintf (var_file, "%ld \n", var->ndimen);

         for (j = 0; j < var->ndimen; j++)
            (void)fprintf (var_file, "%s\n", var->dimen[j]->name);
      } else {
         (void)fprintf (var_file, "1\n");
         (void)fprintf (var_file, "PRIVATE\n");
      }

      (void)fprintf(var_file, "%ld \n", var->size);
      (void)fprintf(var_file, "%ld \n", var->type);

      switch (var->type) {
         case M_DOUBLE:
            dvalptr = (double *) var->value;
            for (j = 0; j < var->size; j++) {
               (void)fprintf(var_file, "%.20le \n", *dvalptr);
               dvalptr++;
            }
			break;

         case M_FLOAT:
            fvalptr = (float *) var->value;
            for (j = 0; j < var->size; j++) {
               (void)fprintf(var_file, "%.12e \n", *fvalptr);
               fvalptr++;
            }
            break;

         case M_LONG:
            lvalptr = (long *) var->value;
            for (j = 0; j < var->size; j++) {
               (void)fprintf(var_file, "%ld \n", *lvalptr);
               lvalptr++;
            }
            break;
      }

   }

   fclose(var_file);
   return(0);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

