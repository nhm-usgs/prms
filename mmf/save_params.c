/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : save_params.c
 * AUTHOR   : CADSWES; modified by Markstrom
 * DATE     :
 * FUNCTION : save_params
 * COMMENT  : saves the param data base to a file. File name is passed in.
 * REF      :
 * REVIEW   :
 * PR NRS   :
   $Revision: 6757 $
        $Log: save_params.c,v $
        Revision 1.17  1998/03/04 17:20:20  markstro
        Added seperate runcontrol functions for each run type.

        Revision 1.16  1996/06/28 19:32:29  markstro
        (1) Fixed 3d control window.
        (2) Fixed stats.

 * Revision 1.15  1996/04/29  16:23:19  markstro
 * Unknown
 *
 * Revision 1.14  1996/02/19  20:00:58  markstro
 * Now lints pretty clean
 *
        Revision 1.13  1994/12/21 21:36:22  markstro
        (1) Fixed ESP to work with multiple data files.
        (2) Fixed Optimization to work with multiple data files.
        (3) Fixed Sensitivity to work with multiple data files.

 * Revision 1.12  1994/11/25  18:13:43  markstro
 * unknown
 *
 * Revision 1.11  1994/11/22  17:20:23  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.10  1994/11/08  16:17:45  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.9  1994/10/13  17:53:38  markstro
 * (1) Added annotation to parameter values through the spreadsheet
 * (2) Included <string.h> in a few more files that needed it.
 *
 * Revision 1.8  1994/09/30  14:55:08  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.7  1994/09/13  15:59:20  markstro
 * (1)  Version of save_params is now written into parameter file.
 * (2)  Took out min and max values for parameters -- these were not necessary.
 *
 * Revision 1.6  1994/09/09  14:56:32  markstro
 * (1)  Fixed up main edit menu.
 * (2)  Added a "notes" field to dimension indicies
 * (3)  A little more Rosenbrock work.
 * (4)  Fixed the list selector -- changed button names & first item
 *      selected by default.
 * (5)  Modified spread sheet help to be able to display dimension notes
 * (6)  Ran some source through "cb"
 *
 * Revision 1.5  1994/05/18  17:16:03  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.4  1994/03/29  19:07:53  markstro
 * Save parameter file selector now comes up in exit sequence (if necessary).
 *
 * Revision 1.3  1994/03/11  21:16:41  markstro
 * Got rid of client_data data types.
 *
 * Revision 1.2  1994/01/31  20:17:24  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SAVE_PARAMS_C
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static void write_parameters (FILE *, int);
static void write_dimensions (FILE *);
static void write_header (FILE *, char *);

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : save_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int save_params (char *param_file_name) {
	FILE *param_file;
	//PARAM *param;
	//DIMEN *dim;
	//char *ptr;
	//long i,j;
	//double	*dvalptr;
	//float	*fvalptr;
	//long	*lvalptr;

	if ((param_file = fopen (param_file_name, "w")) == NULL) {
		(void)fprintf(stderr, "ERROR - save_params - creating file '%s'\n", param_file_name);
		return(1);
	}

	write_header (param_file, "Default Parameter File generated based on active modules and any specified Parameter File(s)\n");
	write_dimensions (param_file);
	write_parameters (param_file, TRUE);
	
	fclose(param_file);
	return(0);
}

int write_preprocess_params () {
	FILE *param_file;
	char param_file_name[512];
	char   **fname;
	/*char *extension, *ptr, *ptr1;*/
	char *ptr, *ptr1;

	fname =   control_svar ("param_file");
	strcpy (param_file_name, fname[0]);

// Isolate the file name from the path
	ptr1 = strrchr (param_file_name, '/');

// Find the last "." in the file name
	if (!ptr1) {
		ptr = NULL;
	} else {
		ptr = strrchr (ptr1, '.');
	}

	if (!ptr) {
		ptr = param_file_name + strlen(param_file_name);
	}
	strcpy (ptr, "_preprocess.params");


	printf ("NOTICE: preprocessed parameters are being written to file: %s\n", param_file_name);

	if ((param_file = fopen (param_file_name, "w")) == NULL) {
		(void)fprintf(stderr, "ERROR - save_params - creating file '%s'\n", param_file_name);
		return(1);
	}

	write_parameters (param_file, FALSE);
	return(0);
}

static void write_header (FILE *param_file, char *desc) {
    (void)fprintf (param_file, desc);
	(void)fprintf (param_file, "PRMS version 4\n");
}

static void write_dimensions (FILE *param_file) {
	DIMEN *dim;
	long i,j;
	(void)fprintf(param_file, "** Dimensions **\n");

	for (i = 0; i < dim_db->count; i++) {

		dim = (DIMEN *)(dim_db->itm[i]);

		(void)fprintf(param_file, "####\n");
		(void)fprintf(param_file, "%s\n", dim->name);
		(void)fprintf(param_file, "%ld\n", dim->value);
		for (j = 0; j < dim->value; j++) {
			if (dim->names && dim->names[j])
				(void)fprintf (param_file, "%s\n", dim->names[j]);
			if (dim->notes && dim->notes[j])
				(void)fprintf (param_file, "@%s\n", dim->notes[j]);
		}
	}
}


static void write_parameters (FILE *param_file, int writeAllParams) {
	PARAM *param;
	char *ptr;
	long i,j;
	double	*dvalptr;
	float	*fvalptr;
	long	*lvalptr;
/*
* Write out parameter values and description if any.
*/
	if (writeAllParams) {
		(void)fprintf(param_file, "** Parameters **\n");
	}

	for (i = 0; i < Mnparams; i++) {
		param = Mparambase[i];

		if (writeAllParams || param->preprocess ) {

			(void)fprintf(param_file, "####\n");
			(void)fprintf(param_file, "%s %ld", param->key, param->column_width);
			if (param->format)
				(void)fprintf(param_file, " %s\n", param->format);
			else
				(void)fprintf (param_file, "\n");
			(void)fprintf (param_file, "%ld\n", param->ndimen);
			for (j = 0; j < param->ndimen; j++)
				(void)fprintf(param_file, "%s\n", param->dimen[j]->name);

			(void)fprintf(param_file, "%ld\n", param->size);
			(void)fprintf(param_file, "%ld\n", param->type);

			switch (param->type) {
				case M_DOUBLE:
					if (writeAllParams) {
						dvalptr = (double *) param->value;
					} else {
						dvalptr = (double *) (param->references[0]);
					}

					for (j = 0; j < param->size; j++) {
						(void)fprintf(param_file, "%.20le\n", *dvalptr);
						dvalptr++;
						if (param->value_desc[j]) {
						  while ((ptr = strchr (param->value_desc[j], '\n'))) {
							*ptr = '\0';
							(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
							param->value_desc[j] = ptr + 1;
						  }
						  if (param->value_desc[j] && strlen (param->value_desc[j]))
							(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						}
					}
					break;

				case M_FLOAT:
					if (writeAllParams) {
						fvalptr = (float *) param->value;
					} else {
						fvalptr = (float *) (param->references[0]);
					}

					for (j = 0; j < param->size; j++) {
						(void)fprintf(param_file, "%.12e\n", *fvalptr);
						fvalptr++;
						if (param->value_desc[j]) {
						  while ((ptr = strchr (param->value_desc[j], '\n'))) {
							*ptr = '\0';
							(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
							param->value_desc[j] = ptr + 1;
						  }
						  if (param->value_desc[j] && strlen (param->value_desc[j]))
							(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						}
					}
					break;

				case M_LONG:
					if (writeAllParams) {
						lvalptr = (long *) param->value;
					} else {
						lvalptr = (long *) (param->references[0]);
					}

					for (j = 0; j < param->size; j++) {
						(void)fprintf(param_file, "%ld\n", *lvalptr);
						lvalptr++;
						if (param->value_desc[j]) {
						  while ((ptr = strchr (param->value_desc[j], '\n'))) {
							*ptr = '\0';
							(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
							param->value_desc[j] = ptr + 1;
						  }
						  if (param->value_desc[j] && strlen (param->value_desc[j]))
							(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						}
					}
					break;
			}
		}
	}
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

