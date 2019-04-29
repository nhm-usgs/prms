/**************************************************************************
 * print_vars.c: prints the var data base to a file
 *
 * $Id: print_vars.c 5644 2010-05-25 13:32:09Z markstro $
 *
   $Revision: 5644 $
        $Log: print_vars.c,v $
        Revision 1.10  1999/10/22 17:14:37  markstro
        Added private variables

        Revision 1.9  1999/08/24 16:34:14  markstro
        Version 1.1.1

        Revision 1.8  1996/02/19 20:00:37  markstro
        Now lints pretty clean

        Revision 1.7  1995/02/01 17:47:34  markstro
        Addition of Rosenbrock optimization.  Start of sensitivity.  Many bug fixes.

 * Revision 1.6  1994/11/22  17:20:06  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.5  1994/09/30  14:54:51  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.4  1994/05/23  14:27:24  markstro
 * Cleaned out a lot of includes in include files
 *
 * Revision 1.3  1994/05/18  17:15:52  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.2  1994/01/31  20:17:07  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#define PRINT_VARS_C
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mms.h"

#define PRINTLEN 77

/**************************************************************************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : print_vars
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int print_vars (void) {

  char pathname[MAXDATALNLEN], *infostr;
  FILE *var_file;
  PUBVAR *var;
  long i, j;

  /*
   * get var file path name, open file
   */

  (void)sprintf (pathname, "%s.var_name", MAltContFile);


  if ((var_file = fopen (pathname, "w")) == NULL) {
    (void)fprintf(stderr,
	    "ERROR - print_vars - creating file '%s'\n", pathname);
    perror("");
//  ufree(pathname);
    return(1);
  }

  /*
   * write header
   */

  (void)fprintf(var_file, "Program MMS                     \n");
  (void)fprintf(var_file, "============\n\n");

  (void)fprintf(var_file, "Printout of variables after a run.\n\n");

  /*
   * write file names
   */

  (void)fprintf(var_file, "Parameter file: %s\n", *control_svar("param_file"));
  (void)fprintf(var_file, "Data file     : %s\n", *control_svar("data_file"));
  (void)fprintf(var_file, "\n");

  /*
   * write the run info string
   */

  infostr = (char *) umalloc (strlen(Mparaminfo) + 1);
  (void)strcpy(infostr, Mparaminfo);
  (void)fprintf(var_file, "%s\n\n", infostr);
//ufree(infostr);

  /*
   * write start and end times, and step number
   */

  (void)fprintf(var_file, "Start time        : %02ld:%02ld:%02ld  %02ld/%02ld/%04ld\n",
	  Mstrttime->hour, Mstrttime->min, Mstrttime->sec,
	  Mstrttime->month, Mstrttime->day, Mstrttime->year);
  (void)fprintf(var_file, "End time          : %02ld:%02ld:%02ld  %02ld/%02ld/%04ld\n",
	  Mendtime->hour, Mendtime->min, Mendtime->sec,
	  Mendtime->month, Mendtime->day, Mendtime->year);
  (void)fprintf(var_file, "Current time      : %02ld:%02ld:%02ld  %02ld/%02ld/%04ld\n",
	  Mnowtime->hour, Mnowtime->min, Mnowtime->sec,
	  Mnowtime->month, Mnowtime->day, Mnowtime->year);
  (void)fprintf(var_file, "Current time step : %8ld\n", Mnsteps);

  /*
   * write out variable values
   */

  for (i = 0; i < Mnvars; i++) {

    var = Mvarbase[i];

    if (!(var->private)) {

       (void)fprintf(var_file, "\n");
       (void)fprintf(var_file, "Name: %s\n", var->name);
       (void)fprintf(var_file, "Module: %s\n", var->module);
       (void)fprintf(var_file, "Ndimen: %ld\n", var->ndimen);
       (void)fprintf(var_file, "Dimensions: ");

       for (j = 0; j < var->ndimen; j++) {
         (void)fprintf(var_file, "%s - %ld",
	         var->dimen[j]->name, var->dimen[j]->value);
         if (j < var->ndimen - 1)
	   (void)fprintf(var_file, ", ");
       } /* j */

       (void)fprintf(var_file, "\n");
       (void)fprintf(var_file, "Size: %ld\n", var->size);
       (void)fprintf(var_file, "Type: %s\n", Mtypes[var->type]);
/* DANGER */
       (void)fprintf(var_file, "Desc: %s\n", var->help);
/* DANGER */
       (void)fprintf(var_file, "Units: %s\n", var->units);
       if (var->private)
          (void)fprintf(var_file, "Private \n");
/*
    (void)fprintf(var_file, "Value(s):\n");
    
    if (var->ndimen >= 3) {

      for (j = 0; j < var->dimen[2]->value; j++) {

	(void)fprintf(var_file, "[%ld]\n", j + 1);

	nk = var->dimen[1]->value;
	
	for (k = 0; k < nk; k++) {

	  (void)fprintf(var_file, "%5ld:", k + 1);

	  nl = var->dimen[0]->value;

	  for (l = 0; l < nl; l++) {

	    print_var(var_file, var, l, nl, k, nk, j);

	  }

	  (void)fprintf(var_file, "\n");

	}

      }

    } else if (var->ndimen == 2) {

      nk = var->dimen[1]->value;
	
      for (k = 0; k < nk; k++) {

	(void)fprintf(var_file, "%5ld:", k + 1);

	nl = var->dimen[0]->value;

	for (l = 0; l < nl; l++) {

	  print_var(var_file, var, l, nl, k,0,0);

	}

	(void)fprintf(var_file, "\n");

      }

    } else {

      nl = var->dimen[0]->value;

      for (l = 0; l < nl; l++) {
	print_var(var_file, var, l,0,0,0,0);

      }

      (void)fprintf(var_file, "\n");

    }
*/
     }
    
  } /* i */

  fclose(var_file);
  
  return(0);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : print_var
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void print_var (FILE *var_file, PUBVAR *var, long l, long nl, long k, long nk,
	long j) {
  
  long ind;

  switch (var->ndimen) {

  case 1:

    ind = l;
    break;

  case 2:

    ind = l + k * nl;
    break;

  default:

    ind = l + k * nl + j * nl * nk;
    break;

  } /* switch (var->ndimen) */

  switch (var->type) {

  case M_DOUBLE:
    (void)fprintf(var_file, " %10lg", *((double *) var->value + ind));
    break;
	
  case M_FLOAT:
    (void)fprintf(var_file, " %10g", *((float *) var->value + ind));
    break;
	
  case M_LONG:
    (void)fprintf(var_file, " %10ld", *((long *) var->value + ind));
    break;
	
  } /* switch (var->type) */

}
