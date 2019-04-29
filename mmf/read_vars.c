/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : read_vars.c
 * AUTHOR   : Steve Markstrom (markstro)
 * DATE     : Tue 22 Nov 1994
 * FUNCTION : read_vars
 * COMMENT  : reads the vars data base from a file.
 *             File name is passed in as an argument
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: read_vars.c 5644 2010-05-25 13:32:09Z markstro $
 *
   $Revision: 5644 $
        $Log: read_vars.c,v $
        Revision 1.12  2000/02/18 18:27:06  markstro
        Made previous Julian time a global.  It is set to -1.0 before the run
        so that read_line knows to recalculate it.

        Revision 1.11  1999/11/30 22:06:18  markstro
        Added nsteps to the var save file.

        Revision 1.10  1999/10/22 17:14:37  markstro
        Added private variables

        Revision 1.9  1996/04/09 21:04:12  markstro
        (1) Work on control files
        (2) Runtime graphs

 * Revision 1.8  1996/02/19  20:00:45  markstro
 * Now lints pretty clean
 *
        Revision 1.7  1995/02/10 23:58:32  markstro
        Bug fixes for class

 * Revision 1.6  1994/11/22  17:20:13  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.5  1994/09/30  14:54:59  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.4  1994/09/09  14:56:28  markstro
 * (1)  Fixed up main edit menu.
 * (2)  Added a "notes" field to dimension indicies
 * (3)  A little more Rosenbrock work.
 * (4)  Fixed the list selector -- changed button names & first item
 *      selected by default.
 * (5)  Modified spread sheet help to be able to display dimension notes
 * (6)  Ran some source through "cb"
 *
 * Revision 1.3  1994/05/18  17:15:57  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.2  1994/01/31  20:17:15  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_VARS_C
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static int read_var_line (char *, char *, FILE *, char *);

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : read_vars
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int read_vars (char *var_file_name) {

	FILE *var_file;
	PUBVAR *var;
	DIMEN *dim;
	long dim_size, var_size, type, i;
	double *dvalptr;
	float *fvalptr;
	long *lvalptr;
	char line[MAXDATALNLEN], key[MAXDATALNLEN];
	char dimen[MAXDATALNLEN];
	char *pathname;
	char *endptr;

/*
* get var name, open file
*/
      pathname = strdup (var_file_name);

   if ((var_file = fopen (pathname, "r")) == NULL) {
      (void)fprintf(stderr, "WARNING - read_vars - cannot open file '%s'\n",
                    pathname);
//    ufree(pathname);
      return(0);
   }

/*
* read in run info string
*/
   if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }
//   if (Mparaminfo) free (Mparaminfo);
   Mparaminfo = strdup (line);

/*
* read in last nstep
*/
   if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }

   Mnsteps = strtol(&(line[11]), &endptr, 10);

/*
* read in last time step
*/
   if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }

/*
   (void)fprintf (stderr,"read_vars Mnowtime->jt stirng = %s\n", &(line[17]));
   Mnowtime->jt = strtod(&(line[17]), &endptr);
   (void)fprintf (stderr,"read_vars Mnowtime->jt = %d\n", Mnowtime->jt);
*/

/*
* read in last delta time
*/
   if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }

   Mdeltat = strtod(&(line[16]), &endptr);
   Mdeltanext = strtod(&(line[16]), &endptr);

/*
* read in dimensions
*/
   while (!feof(var_file)) {

/*
* space fwd to #### header
*/
   (void)strcpy(line, " ");
   while (strncmp(line, "####", 4)) {
      if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
         fclose(var_file);
         return(0);
      }

/*
* break if variable list starts
*/
      if(!strncmp(line, "** Variables **", strlen("** Variables **")))
         goto variables;
      }

/*
* get dimen name
*/

      if(fgets(key, MAXDATALNLEN, var_file) == NULL) {
         (void)fprintf(stderr, "ERROR - read_var, reading dimen name.\n");
         (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
         return(1);
      }
      key[strlen(key)-1] = '\0';

      if ((dim = dim_addr(key)) == NULL) {
         (void)fprintf(stderr, "WARNING - read_vars.\n");
         (void)fprintf(stderr, "Using var file '%s'\n", pathname);
         (void)fprintf(stderr, "Dimension '%s' not declared.\n", key);
         (void)fprintf(stderr, "Variables not read from file.\n");
         fclose(var_file);
         return(0);
      } else {

/*
* get dimen size
*/
         if(fgets(line, MAXDATALNLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading dimen size.\n");
            fprintf(stderr,"Early end-of-file, file '%s'\n",var_file_name);
            return(1);
         }

         errno = 0;
         dim_size = strtol(line, &endptr, 10);
         if(errno != 0) {
            (void)fprintf(stderr,
                        "ERROR - read_var, decoding size from '%s'.\n", line);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            perror(" ");
            return(1);
         }
/*
* check dimension size
*/
         if (dim->value != dim_size) {
            (void)fprintf(stderr, "WARNING - read_vars.\n");
            (void)fprintf(stderr, "Using var file '%s'\n", pathname);
            (void)fprintf(stderr, "Dimension '%s' has size %ld.\n", key, dim->value);
            (void)fprintf(stderr, "Size in var file is %ld.\n", dim_size);
            (void)fprintf(stderr, "Variables not read from file.\n");
            fclose(var_file);
            return(0);
         }
      }
   } /* while */

/*
* read in variables
*/

variables:
   while (!feof(var_file)) {

/*
* space fwd to #### header
*/
      (void)strcpy(line, " ");
      while (strncmp(line, "####", 4)) {
         if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
            fclose(var_file);
            return(0);
         }
      }

/*
* get key
*/
      if(fgets(key, MAXDATALNLEN, var_file) == NULL) {
         (void)fprintf(stderr, "ERROR - read_var, reading var key.\n");
         (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
         return(1);
      }
      key[strlen(key) - 1] = '\0';

      if ((var = var_addr(key)) != NULL) {
/*
* get number of dimensions
*/
         if(fgets(line, MAXDATALNLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading var ndimen.\n");
            fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
            return(1);
         }

         if((var->ndimen = atol(line)) == 0) {
            (void)fprintf(stderr,
                  "ERROR - read_var, decoding var ndimen from '%s'.\n", line);
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }

/*
* get dimens
*/

         for (i = 0; i < var->ndimen; i++) {
            if(fgets(dimen, MAXDATALNLEN, var_file) == NULL) {
               (void)fprintf(stderr, "ERROR - read_var, reading var dimen.\n");
               (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
               return(1);
            }
            dimen[strlen(dimen) - 1] = '\0';

            if (strcmp(dimen, "PRIVATE")) {
               if (strcmp(dimen, var->dimen[i]->name)) {
                  (void)fprintf(stderr, "ERROR - read_var, reading var dimen.\n");
                  (void)fprintf(stderr, "Expecting dimension '%s'\n", var->dimen[i]->name);
                  (void)fprintf(stderr, "Read dimension '%s'\n", dimen);
                  (void)fprintf(stderr, "Key is '%s'\n", key);
                  (void)fprintf(stderr, "File '%s'\n", var_file_name);
                  return(1);
               }
            }
         } /* i */

/*
* get var size
*/

         if(fgets(line, MAXDATALNLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading var size.\n");
            (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
            return(1);
         }

         errno = 0;
         var_size = strtol(line, &endptr, 10);
         if(errno != 0) {
            (void)fprintf(stderr,
                     "ERROR - read_var, decoding var size from '%s'.\n", line);
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }
         if(var_size != var->size) {
            (void)fprintf(stderr, "ERROR - read_var, size incorrect.\n");
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }

/*
* get type
*/
         if(fgets(line, MAXDATALNLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading var type.\n");
            (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
            return(1);
         }
         if((type = atol(line)) == 0) {
            (void)fprintf(stderr,
                  "ERROR - read_var, decoding var type from '%s'.\n", line);
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }
         if(type != var->type) {
            (void)fprintf(stderr, "ERROR - read_var, type incorrect.\n");
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }

/*
* read in and store the file data
*/

         switch (type) {
            case M_DOUBLE:
               dvalptr = (double *) var->value;
               for (i = 0; i < var_size; i++) {
                  if(read_var_line(key, line, var_file, var_file_name))
                     return(1);
                  dvalptr[i] = atof(line);
               }
               break;

            case M_FLOAT:
               fvalptr = (float *) var->value;
               for (i = 0; i < var_size; i++) {
                  if(read_var_line(key, line, var_file, var_file_name))
                     return(1);
                  fvalptr[i] = (float) atof(line);
               }
               break;

         case M_LONG:
            lvalptr = (long *) var->value;
            for (i = 0; i < var_size; i++) {
               if(read_var_line(key, line, var_file, var_file_name))
                  return(1);
               lvalptr[i] =  atol(line);
            }
            break;
         }
      } /* if (var ... */

   } /* while */

   fclose(var_file);
// ufree(pathname);
   return(0);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : read_var_line
 | COMMENT		: gets a line from the variable file
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static int read_var_line (char *key, char *line, FILE *var_file, char *var_file_name) {

	if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
		(void)fprintf(stderr,
		    "ERROR - read_var, reading data.\n");
		(void)fprintf(stderr,
		    "Early end-of-file, file '%s'\n", var_file_name);
		(void)fprintf(stderr, "Key is '%s'\n", key);
		return(1);
	}

	return(0);

}
/**8************************** TEST DRIVER ****************************/


