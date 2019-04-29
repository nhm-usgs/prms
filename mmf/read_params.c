/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : read_params
 * COMMENT  : reads the params data base from a file
 *            File name is passed in as an argument
 *
 * $Id: read_params.c 6662 2014-08-06 15:13:01Z markstro $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_PARAMS_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include "mms.h"

static char *READ_param_head (PARAM **, FILE **, char *, char[], int);
static char *READ_param_values (long, long, char *, char *, FILE *, char[]);
static char *rp (char *, int, int);
static int checkForValidDimensions (PARAM *);
static int isDimensionIncompatable (char *, char *);
static void oneToAnySizedArray(PARAM *, char *);
static int getParamFileParamSize (PARAM *);
static char *getMapParamName(char *);
static void subbasinTo1DArray (PARAM *, PARAM *, char *);

static char* dimNames[] = {"nhru", "nsegment",
	"nrain", "ntemp", "nobs", "ngw",
	"nssr"
};

static char* mapParamNames[] = {"hru_subbasin", "segment_subbasin",
	"rain_subbasin", "temp_subbasin", "obs_subbasin", "gw_subbasin",
	"ssr_subbasin"
};

int nComments;
char **Comments;

/*--------------------------------------------------------------------*\
 | FUNCTION     : read_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_params (char *param_file_name, int index, int mapping_flag) {
  	static char *foo = NULL;
  	char old[256], *cptr;

	if (foo) {
		strncpy (old, foo, 256);
		free (foo);
		foo = strdup (param_file_name);
	} else {
		strncpy (old, param_file_name, 256);
		foo = strdup (param_file_name);
	}

	cptr = rp (param_file_name, index, mapping_flag);

	if (cptr) {
		rp (old, index, 0);

		free (foo);
		foo = strdup (old);
	}

	return (cptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : read_dims
 | COMMENT	:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_dims (char *param_file_name) {
  FILE *param_file;
  DIMEN *dim;
  int dim_size, i, j;

  char line[MAXDATALNLEN], key[MAXDATALNLEN];
  static char buf[256];
  char *endptr;
  char *nch;
  int		done;


/*
* get param name, open file
*/
	if ((param_file = fopen (param_file_name, "r")) == NULL) {
		if (param_file_name) {
			(void)sprintf (buf, "ERROR: cannot open Parameter File: %s", param_file_name);
		} else {
			(void)sprintf (buf, "ERROR: cannot open Parameter File");
		}
		return (buf);
	}

/*
* read in run info string
*/
	if (!fgets (line, MAXDATALNLEN, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}

		(void)sprintf (buf, "ERROR: problems reading info line in Parameter File");
		return (buf);
	}

	if (Mparaminfo) {
		free (Mparaminfo);
	}
	Mparaminfo = strdup (line);

/*
**	See if version number is set
*/
	if (!fgets (line, MAXDATALNLEN, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}

		(void)sprintf (buf, "ERROR: problems reading version number in Parameter File");
		return (buf);
	}

	if (!fgets (line, MAXDATALNLEN, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "ERROR: problems reading dimension label in Parameter File");
		return (buf);
	}

/*
 *  Read in comments -- everything between version line and
 *  "** Dimensions **" line is a comment
 */

	Comments = (char **)malloc (1000 * sizeof (char *));
	nComments = 0;

	while (strncmp (line, "** Dimensions **", 16)) {
		if (!fgets (line, MAXDATALNLEN, param_file)) {
		   if (param_file != NULL) {
		      fclose (param_file);
		      param_file = NULL;
		   }
			(void)sprintf (buf, "ERROR: problems skipping comments in Parameter File");
			return (buf);
		}

		if (strncmp (line, "** Dimensions **", 16)) {
			printf ("Comment line = %s\n", line);
			Comments[nComments++] = strdup (line);
		}
	}
	//}

/*
**	Check dimension label
*/
	if (strncmp (line, "** Dimensions **", 16)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "ERROR: ** Dimensions ** label not found in Parameter File %s.",
		param_file_name);
		return (buf);
	}
  
	if (!fgets (line, MAXDATALNLEN, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "ERROR: unexpected end of Parameter File");
		return (buf);
	}

/*
* read in dimensions
*/
	while (strncmp (line, "** Parameters **", 16)) {

		if (strncmp (line, "####", 4)) {
		   if (param_file != NULL) {
		      fclose (param_file);
		      param_file = NULL;
		   }
			(void)sprintf (buf, "ERROR: expecting '####' found %s in Parameter File %s", line, param_file_name);
			return (buf);
		}

/*
**	Read dimension name from parameter file.
*/
		if (fgets (key, MAXDATALNLEN, param_file) == NULL) {
		   if (param_file != NULL) {
		      fclose (param_file);
		      param_file = NULL;
	       }
			(void)sprintf (buf, "ERROR: trying to read dimension name %s in Parameter File %s.", key, param_file_name);
			return (buf);
		}

		key[strlen(key)-1] = '\0';

		dim = dim_addr (key);
		if (dim) {
/*
**	Read dimension size from parameter file.
*/
			if (fgets (line, MAXDATALNLEN, param_file) == NULL) {
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
	           }
				(void)sprintf (buf, "ERROR: can't read dimension size for %s in Parameter File %s.", key, param_file_name);
				return (buf);
			}

			errno = 0;
			dim_size = strtol(line, &endptr, 10);
			if (errno != 0) {
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
		       }
				(void)sprintf (buf, "ERROR: size problem with %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

/*
**	If necessary, reset dimension to value read from file.
*/
			if (dim->value != dim_size) {
				//reset_dim (dim, dim_size);
				dim->value = dim_size;
			}

/*
* check if there are index names below
*/
			if (fgets (line, MAXDATALNLEN, param_file)) {
				if (strncmp (line, "** Parameters **", 16)) {
					if (dim->names) {
				//        free (dim->names);
						dim->names = NULL;
					}

					if (dim->notes) {
					//        free (dim->notes);
						dim->notes = NULL;
					}

					if (strncmp (line, "####", 4)) {
						dim->names = (char **)calloc (dim_size, sizeof (char *));
						dim->notes = (char **)calloc (dim_size, sizeof (char *));

						done = FALSE;
						i = 0;
						while (!done) {
							if (!strncmp (line, "####", 4)) {
								for (j = i; j < dim_size; j++) {
									dim->names[j] = NULL;
									dim->notes[j] = NULL;
								}
								done = TRUE;

							} else if (line[0] == '@') {
								i--;
								nch = (char *)strchr (line, '\n');
								if (nch) {
									*nch = '\0';
								}
								dim->notes[i] = strdup (&(line[1]));
								fgets (line, MAXDATALNLEN, param_file);
								i++;

							} else {
								nch = (char *)strchr (line, '\n');
								if (nch) {
									*nch = '\0';
								}
								dim->names[i] = strdup (line);
								fgets (line, MAXDATALNLEN, param_file);
								i++;
							}

							if ((i > dim_size) || ((i == dim_size) && (line[0] != '@'))) {
								done = TRUE;
							}
						}
					} else {
						dim->names = NULL;
						dim->files = NULL;
						dim->notes = NULL;
					}
				}
			} else {
		       if (param_file != NULL) {
		          fclose (param_file);  // EOL was returned -- done reading dimensions from this file;
		          param_file = NULL;
		       }
				return (NULL);
			}
		} else {
			(void)fprintf (stderr,"\nWARNING: dimension '%s' is not required; set in parameter file:\n         %s\n", key, param_file_name);
			fgets (line, MAXDATALNLEN, param_file);
			fgets (line, MAXDATALNLEN, param_file);
		}
	}

	if (param_file != NULL) {
	   fclose (param_file);
	   param_file = NULL;
	}
	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : rp
 | COMMENT	:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static char *rp (char *param_file_name, int index, int map_flag) {

  FILE *param_file;
  PARAM *param;

  char line[MAXDATALNLEN];
  static char buf[256], *buf_ptr;
  char *pf_value, *mapParamName;
  int i, j, k;
  PARAM *mapping_param;
//  int *mapping;


/*
* get param name, open file
*/
	if ((param_file = fopen (param_file_name, "r")) == NULL) {
		if (param_file_name)
			(void)sprintf (buf, "ERROR: cannot open Parameter File: %s", param_file_name);
		else
			(void)sprintf (buf, "ERROR: cannot open Parameter File");

		return (buf);
	}

	fgets (line, MAXDATALNLEN, param_file);
	if (index == 0) {  // if index equals zero, than this parameter file has dimension stuff and we need to skip over it.
		while (strncmp (line, "** Parameters **", 16)) {
			if (!fgets (line, MAXDATALNLEN, param_file)) {  // return if hits eol
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
		       }
			   return (NULL);
			}
		}
		fgets (line, MAXDATALNLEN, param_file);
	}

/*
**	Read in parameters.
*/
	while (!feof (param_file)) {
		buf_ptr = READ_param_head (&param, &param_file, param_file_name, line, map_flag);
		if (buf_ptr) {
			if (buf_ptr == (char *)-1) {
		      if (param_file != NULL) {
		         fclose (param_file);
		         param_file = NULL;
		      }
			  return (NULL);
			} else {
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
		       }
			   return (buf_ptr);
			}
		}

		if (param != NULL) {

			// If param->size (array size as defined by module) is the
			// same as param->pf_size (array size as defined by parameter
			// file, then read the values for this parameter from the
			// parameter file directly into param->value because the 
			// dimensions match.  If the sizes do not match, read the
			// values into a temporary array that is used to remap the
			// values into the correct size and shape for param->value.
			if (param->pf_size == param->size) {
				buf_ptr = READ_param_values (param->size, param->type, param->name, param->value, param_file, line);

			} else {

//  Make sure that this resizing code is in sync with the ParamToolExpandor code in the oui4 code base.

				if (param->type == M_DOUBLE) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (double));
				} else if (param->type == M_FLOAT) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (float));
				} else if (param->type == M_LONG) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (int));
				} else if (param->type == M_STRING) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (char *));
				} else {
					pf_value = NULL;
				}

				buf_ptr = READ_param_values (param->pf_size, param->type, param->name, pf_value, param_file, line);

				// The values read from the parameter file need to be resized to fit into the size
				// of the module array for this parameter.

				// It's easy when the size is 1.  Tested this works for floats
				if (param->pf_size == 1) {
					oneToAnySizedArray(param, pf_value);

				} else if (param->pf_ndimen == 1 && !strncmp(param->pf_dimNames[0], "nsub", 4) &&
						(!strncmp(param->dimen[0]->name, "nhru", 4) || !strncmp(param->dimen[0]->name, "nsegment", 8) ||
						!strncmp(param->dimen[0]->name, "nrain", 5) || !strncmp(param->dimen[0]->name, "ntemp", 5) ||
						!strncmp(param->dimen[0]->name, "nobs", 4) || !strncmp(param->dimen[0]->name, "ngw", 3) ||
						!strncmp(param->dimen[0]->name, "nssr", 4))) {  // subbasin to one mappable dimension

					mapParamName = getMapParamName(param->dimen[0]->name);

					mapping_param = param_addr (mapParamName);

					if (!mapping_param || !(mapping_param->read_in)) {
						sprintf (buf, "\nERROR: mapping parameter %s must be set in parameter file before parameter %s\n",
							mapParamName, param->name);
						return (buf);
					}

					subbasinTo1DArray (param, mapping_param, pf_value);

				} else if (param->pf_ndimen == 1 && param->ndimen == 2) { // 1D in parameter file to 2D in module

   					// convert "nmonths" to "nhru,nmonths"
					if (!strncmp(param->pf_dimNames[0], "nmonths", 7)
                                && !strncmp(param->dimen[0]->name, "nhru", 4) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[j + (i*param->dimen[0]->value)] = ((double *)pf_value)[i];
								}
							}

						} else if (param->type == M_FLOAT) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[j + (i*param->dimen[0]->value)] = ((float *)pf_value)[i];
								}
							}

						} else if (param->type == M_LONG) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[j + (i*param->dimen[0]->value)] = ((int *)pf_value)[i];
								}
							}

						} else if (param->type == M_STRING) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (i*param->dimen[0]->value)) = strdup (pf_value + i);
								}
							}
						}

            // convert "nhru" to "nhru,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nhru", 4)
                                && !strncmp(param->dimen[0]->name, "nhru", 4) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[k++] = ((double *)pf_value)[j];
								}
							}

						} else if (param->type == M_FLOAT) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[k++] = ((float *)pf_value)[j];
								}
							}

						} else if (param->type == M_LONG) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[k++] = ((int *)pf_value)[j];
								}
							}

						} else if (param->type == M_STRING) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (k++)) = strdup (pf_value + j);
								}
							}
						}

            // convert "nrain" to "nrain,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nrain", 5)
                                && !strncmp(param->dimen[0]->name, "nrain", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[k++] = ((double *)pf_value)[j];
								}
							}

						} else if (param->type == M_FLOAT) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[k++] = ((float *)pf_value)[j];
								}
							}

						} else if (param->type == M_LONG) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[k++] = ((int *)pf_value)[j];
								}
							}

						} else if (param->type == M_STRING) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (k++)) = strdup (pf_value + j);
								}
							}
						}

            // convert "ntemp" to "ntemp,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "ntemp", 5)
                                && !strncmp(param->dimen[0]->name, "ntemp", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[k++] = ((double *)pf_value)[j];
								}
							}

						} else if (param->type == M_FLOAT) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[k++] = ((float *)pf_value)[j];
								}
							}

						} else if (param->type == M_LONG) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[k++] = ((int *)pf_value)[j];
								}
							}

						} else if (param->type == M_STRING) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (k++)) = strdup (pf_value + j);
								}
							}
						}

            // convert "nmonths" to "nrain,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nmonths", 7)
                                && !strncmp(param->dimen[0]->name, "nrain", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[j + (i*param->dimen[0]->value)] = ((double *)pf_value)[i];
								}
							}

						} else if (param->type == M_FLOAT) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[j + (i*param->dimen[0]->value)] = ((float *)pf_value)[i];
								}
							}

						} else if (param->type == M_LONG) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[j + (i*param->dimen[0]->value)] = ((int *)pf_value)[i];
								}
							}

						} else if (param->type == M_STRING) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (i*param->dimen[0]->value)) = strdup (pf_value + i);
								}
							}
						}

            // convert "nmonths" to "ntemp,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nmonths", 7)
                                && !strncmp(param->dimen[0]->name, "ntemp", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[j + (i*param->dimen[0]->value)] = ((double *)pf_value)[i];
								}
							}

						} else if (param->type == M_FLOAT) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[j + (i*param->dimen[0]->value)] = ((float *)pf_value)[i];
								}
							}

						} else if (param->type == M_LONG) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[j + (i*param->dimen[0]->value)] = ((int *)pf_value)[i];
								}
							}

						} else if (param->type == M_STRING) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (i*param->dimen[0]->value)) = strdup (pf_value + i);
								}
							}
						}







                    }  // end of 1D to 2D conversion code
				}
			}

			if (buf_ptr) {
		        if (param_file != NULL) {
		           fclose (param_file);
		           param_file = NULL;
		        }
				return (buf_ptr);
			}

			// This function copies the parameter values from the param structure
			// to the arrays in the modules.
			updateparam (param->name);
		}
	}

    if (param_file != NULL) {
	   fclose (param_file);
	   param_file = NULL;
    }

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : READ_param_head
 | COMMENT		: Read the preliminary stuff for the parameter.  This is
 |                 the stuff between the ####s and where the data actually
 |                 starts.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static char *READ_param_head (PARAM **param_ptr, FILE **param_file, char *param_file_name, char line[], int map_flag) {
  char key[MAXDATALNLEN];
  char dimen[MAXDATALNLEN];
  static char buf[256];
  char *temp, *npos, *tempfmt;
  int tempwidth, i, param_size, type;

  int badFlag;
  
/*
* space fwd to #### header
*/
  while (strncmp (line, "####", 4))
    if (!fgets (line, MAXDATALNLEN, *param_file)) {
		if (*param_file != NULL) {
		   fclose (*param_file);
		   *param_file = NULL;
		}
      return ((char *)-1);
    }
  
/*
* get key, column width and format
*/
  if (fgets (line, MAXDATALNLEN, *param_file) == NULL) {
	  (void)sprintf (buf, "\nERROR: Early end of Parameter File: %s", param_file_name);
    return (buf);
  }

/*
**	get the key
*/
  temp = (char *)strtok(line," ");


  npos = strchr(temp,'\n');
  if (npos) *npos = '\0';

  (void)strcpy(key,temp);
  key[strlen(temp)] = '\0';
	
/*
**	get the column width
*/
  temp = (char *)strtok(NULL," ");
  if (temp) {
    tempwidth = atoi(temp);
  } else {
    tempwidth = 0;
  }

/*
**	get the format
*/
  tempfmt = (char *)strtok(NULL," ");

/*
** markstro -- this check is added so that if there is just a space
**             after the width the parameter will not have a blank
**             format.
*/
  if (tempfmt && (strlen (tempfmt) < 2)) {
     tempfmt = NULL;
  }

/*
**  param is allocated by calls from the modules to declparam.
*/
	if (!map_flag) {
		*param_ptr = param_addr(key);

	} else {
		if (!strncmp (key, mapParamNames[0], strlen(key)) ||
			!strncmp (key, mapParamNames[1], strlen(key)) ||
			!strncmp (key, mapParamNames[2], strlen(key)) ||
			!strncmp (key, mapParamNames[3], strlen(key)) ||
			!strncmp (key, mapParamNames[4], strlen(key)) ||
			!strncmp (key, mapParamNames[5], strlen(key)) ||
			!strncmp (key, mapParamNames[6], strlen(key))) {
			*param_ptr = param_addr (key);

			if (*param_ptr == NULL) {  // Didn't find this mapping parameter in the parameter DB so declare one
				declparam ("read_params", key, NULL, "integer", NULL, NULL, NULL, NULL, NULL, NULL);
				*param_ptr = param_addr(key);
			}

		} else {
			*param_ptr = NULL;
		}
	}

	if (*param_ptr) {
	  /*
	  **  Set the read_in flag to true
	  */
		(*param_ptr)->read_in = 1;
/*
* save format and column width
*/
		(*param_ptr)->column_width = tempwidth;
		if (tempfmt) {
			tempfmt[strlen(tempfmt)-1] = '\0';
			if(!(*param_ptr)->format) {
				(*param_ptr)->format = (char *)(malloc(strlen(tempfmt)+1));
			} else {
				(*param_ptr)->format = (char *)(realloc((*param_ptr)->format, strlen(tempfmt) + 1));
			}   
			(void)strcpy((*param_ptr)->format, tempfmt);
		} else {
			(*param_ptr)->format = NULL;
		}
/*
* get number of dimensions
*/
		if(fgets(line, MAXDATALNLEN, *param_file) == NULL) {
			(void)sprintf (buf,"ERROR: reading param number of dimensions for %s in Parameter File %s", key, param_file_name);
			return buf;
		}

		if (isdigit(*line)) {
			//if ((*param_ptr)->ndimen != atol(line)) {
			//	sprintf (buf, "\nERROR: number of dimensions for parameter %s doesn't match parameter declaration.\nParameter File: %s\n", key, param_file_name);
			//	return buf;
			//}

			(*param_ptr)->pf_ndimen = atol(line);

			//if((*param_ptr)->ndimen == 0) {
			//	(void)sprintf (buf, "\nERROR: number of dimensions is 0 for %s in Parameter File %s", key, param_file_name);
			//	return (buf);
			//}

			if((*param_ptr)->pf_ndimen == 0) {
				(void)sprintf (buf, "\nERROR: number of dimensions is 0 for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}
/*
* get number of dimensions if file format supports 2D arrays. Otherwise
* get dimension name.
*/
/*			for (i = 0; i < (*param_ptr)->ndimen; i++) {
				if(fgets(dimen, MAXDATALNLEN, *param_file) == NULL) {
					(void)sprintf (buf, "\nERROR: number of dimensions is wrong for %s in Parameter File %s", key, param_file_name);
					return (buf);
				}

				dimen[strlen(dimen) - 1] = '\0';
				if (strcmp(dimen, (*param_ptr)->dimen[i]->name)) {
					(void)sprintf (buf, "\nERROR: dimension specification is wrong for %s in Parameter File %s", key, param_file_name);
					return (buf);
				}
			}*/ /* i */

			(*param_ptr)->pf_dimNames = (char **)malloc ((*param_ptr)->pf_ndimen * sizeof (char *));

			for (i = 0; i < (*param_ptr)->pf_ndimen; i++) {
				if(fgets(dimen, MAXDATALNLEN, *param_file) == NULL) {
					(void)sprintf (buf, "\nERROR: number of dimensions is wrong for %s in Parameter File %s", key, param_file_name);
					return (buf);
				}

				dimen[strlen(dimen) - 1] = '\0';
				(*param_ptr)->pf_dimNames[i] = strdup(dimen);
				//if (strcmp(dimen, (*param_ptr)->dimen[i]->name)) {
				//	(void)sprintf (buf, "\nERROR: dimension specification is wrong for %s in Parameter File %s", key, param_file_name);
				//	return (buf);
				//}
			}

			if (map_flag) { // Need to set some values in the param structure for mapping parameter
				(*param_ptr)->ndimen = 1;
				(*param_ptr)->dimen = (DIMEN **)umalloc ((*param_ptr)->ndimen * sizeof (DIMEN *));
				(*param_ptr)->dimen[0] = dim_addr((*param_ptr)->pf_dimNames[0]);
			}

			badFlag = checkForValidDimensions (*param_ptr);  // 0 = good;  1 = bad

			if (badFlag) {
				(void)sprintf (buf, "ERROR: dimensions for %s in Parameter File %s are incompatable with declaration in module", key, param_file_name);
				return (buf);
			}

			(*param_ptr)->pf_size = getParamFileParamSize(*param_ptr);

			if (map_flag) { // Need to set some values in the param structure for mapping parameter
				(*param_ptr)->size = (*param_ptr)->pf_size;
				(*param_ptr)->value = (char *)umalloc ((*param_ptr)->size * sizeof (int)); // Mapping parameters are always integers
			}
/*
* get param size
*/
			fgets(line, MAXDATALNLEN, *param_file);
			if(line == NULL) {
				(void)sprintf (buf, "ERROR: incorrect parameter size for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

			if((param_size = atol(line)) == 0) {
				(void)sprintf (buf, "\nERROR: incorrect parameter size for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

			//if(param_size != (*param_ptr)->size) {
			if(param_size != (*param_ptr)->pf_size) {
				(void)sprintf (buf, "\nERROR: incorrect parameter size for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

		} else {  //  number of dimensions not a digit
			(*param_ptr)->ndimen = 1;
			strncpy(dimen, line, strlen(line));
			dimen[strlen(line)-1] = '\0';

			if (strcmp(dimen, (*param_ptr)->dimen[0]->name)) {
				(void)sprintf (buf, "\nERROR: incorrect dimension specified for parameter %s in Parameter File %s",
				  key, param_file_name);
				return (buf);
			}
			(*param_ptr)->size = getdim(dimen);
			param_size = (*param_ptr)->size;
		}
/*
* get type
*/
		fgets(line, MAXDATALNLEN, *param_file);
		if(line == NULL) {
			(void)sprintf (buf, "\nERROR: incorrect data type specified for parameter %s in Parameter File %s", key, param_file_name);
			return (buf);
		}

		if((type = atol(line)) == 0) {
			sprintf (buf, "\nERROR: incorrect data type specified for parameter %s in Parameter File %s", key, param_file_name);
			return (buf);
		}

		if(type != (*param_ptr)->type) {
			sprintf (buf, "\nERROR: incorrect data type specified for parameter %s in Parameter File %s", key, param_file_name);
			return (buf);
		}
  
	} else {
		if (!map_flag) {
			(void)printf ("\nWARNING: parameter %s is ignored as it is not required.\n", key);
			(void)printf ("         Read from Parameter File: %s\n", param_file_name);
		}
	}

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : READ_param_values
 | COMMENT		: Read the values and comments for the parameter.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static char *READ_param_values (long size, long type, char *name, char *value,
								FILE *param_file, char line[]) {
	int i, j;
//	char *nch;
//	int l1, l2
    int  done;
	int	desc_count = 0;
	int repeat_count;
	char delims[] = ",";
	char *result = NULL;
	char *comp_ptr = NULL;
	static char crap[MAXDATALNLEN], crap2[MAXDATALNLEN];
	static char buf[256];
	float foo;
	double d;
	char *endp;
	long l;


/*
**  Space for the values and value_desc are allocated in declparam
*/
	done = FALSE;
	i = 0;
	while (!done) {
		fgets (line, MAXDATALNLEN, param_file);
		if (!line) {
			done = TRUE;

		} else if (!strncmp (line, "####", 4)) {
			done = TRUE;

		//} else if (!param) {
		//	;

		//} else if (line[0] == '@') {
		//	i--;

		//	nch = (char *)strchr (line, '\n');
		//	if (nch) *nch = '\0';

		//	if (desc_count) {
		//		if (param->value_desc[i]) {
		//			l1 = strlen (param->value_desc[i]);
		//			l2 = strlen (line);
		//			param->value_desc[i] = (char *)realloc
		//			    (param->value_desc[i],
		//			    (l1 + l2 + 2) * sizeof (char));
		//			strcat (param->value_desc[i], "\n");
		//			strcat (param->value_desc[i], &(line[1]));
		//		} else {
		//			param->value_desc[i] = strdup (&(line[1]));
		//		}
		//	} else {

		//		param->value_desc[i] = strdup (&(line[1]));
		//	}
		//	i++;

		} else {
			desc_count = 0;
			result = NULL;
			//printf ("READ_param_values: line is %s\n", line);
			strncpy (crap, line, MAXDATALNLEN);
			//printf ("crap is %s\n", crap);

			result = strtok (crap, delims);
			while (result != NULL && !done) {
				//printf ("   READ_param_values: result is |%s|\n", result);

				strncpy (crap2, result, MAXDATALNLEN);
				//printf ("crap2 is %s\n", crap2);
				comp_ptr = strchr (crap2, '*');
				//printf ("comp_ptr is %s\n", comp_ptr);
				if (comp_ptr == NULL){
					repeat_count = 1;
					comp_ptr = crap2;
					//printf ("comp_ptr is %s\n", comp_ptr);
				} else {
					*comp_ptr = '\0';
					repeat_count = atol(crap2);
					comp_ptr++;
					//printf ("comp_ptr is %s\n", comp_ptr);
					foo = (float) atof(comp_ptr);
				}

				for (j = 0; j < repeat_count && !done; j++) {
					if (i < size) {
						switch (type) {

							case M_STRING:
                                comp_ptr[strlen(comp_ptr)-1] = '\0';
                                //*((char **)param->value + i) = strdup (comp_ptr);
								*((char **)value + i) = strdup (comp_ptr);
                                i++;

								//if (comp_ptr != endp && *endp == '\n') {

								//} else {
								//	sprintf (buf, "There is a parameter format error. Parameter name: %s Index = %d\n   The data type should be a character string or there could be white spaces after the values on the line.", param->name, (i+1));
									//printf ("%s", buf);
								//	return (buf);
								//}

								//((double *)(param->value))[i++] = atof(comp_ptr);
								break;

							case M_DOUBLE:
								d = strtod(comp_ptr, &endp);
								if (comp_ptr != endp && *endp == '\n') {
									//((double *)(param->value))[i++] = d;
									((double *)value)[i++] = d;
								} else {
									sprintf (buf, "\nERROR: parameter format error. Parameter name: %s Index = %d\n   The data type should be a double precision float or there could be white spaces after the values on the line.", name, (i+1));
									return (buf);
								}
								break;

							case M_FLOAT:
								d = strtod(comp_ptr, &endp);
								if (comp_ptr != endp && *endp == '\n') {
									//((float *)(param->value))[i++] = (float)d;
									((float *)value)[i++] = (float)d;
								} else {
									sprintf (buf, "\nERROR: parameter format error. Parameter name: %s Index = %d\n   The data type should be a float or there could be white spaces after the values on the line.", name, (i+1));
									return (buf);
								}
								break;

							case M_LONG:
								l = strtol(comp_ptr, &endp, 0);
								if (comp_ptr != endp && *endp == '\n') {
									//((int *)(param->value))[i++] = (int)l;
									((int *)value)[i++] = (int)l;
								} else {
									sprintf (buf, "\nERROR: parameter format error. Parameter name: %s Index = %d\n   The data type should be an integer or there could be white spaces after the values on the line.", name, (i+1));
									return (buf);
								}
								break;
						} // switch
				 
					} else { // if (i < size)
						done = TRUE;
						i++;
					} // if (i < size)
				}
				result = strtok(NULL, delims);
			} // while
		}
	}

	if (i < size) {
		sprintf (buf, "\nERROR: too few values read for paramter %s in Parameter File", name);
		return (buf);
	} else if (i > size && !done) {
		sprintf (buf, "\nERROR: too many values read for paramter %s in Parameter File", name);
		return (buf);
	}
	return (NULL);
}

// returns
// 0 = good;  1 = bad
static int checkForValidDimensions (PARAM *param_ptr) {
	int i, badFlag;

//	printf ("checkForValidDimensions name = %s\n", param_ptr->name);
//	printf ("   pf_ndimen = %d; module_ndimen = %d\n", (int)(param_ptr->pf_ndimen), (int)(param_ptr->ndimen));

	if (param_ptr->pf_ndimen > param_ptr->ndimen ) { // more dimensions in the parameter file is always invalid
		return 1;

	} else if (param_ptr->pf_ndimen == param_ptr->ndimen ) {
		for (i = 0; i < param_ptr->pf_ndimen; i++) {  // check each dimension for compatiblilty
//printf ("   1 comparing %s to %s\n", param_ptr->pf_dimNames[i], param_ptr->dimen[i]->name);
			badFlag = isDimensionIncompatable (param_ptr->pf_dimNames[i], param_ptr->dimen[i]->name); // 0 = good;  1 = bad
		}
		if (badFlag == 1) {
			return 1;
		}

	} else { // less dimensions in the parameter file than declared in the module.
		badFlag = 1;
//printf ("   2 parameter file has %d dimensions\n", param_ptr->pf_ndimen);
		for (i = 0; i < param_ptr->ndimen; i++) {  // check each dimension for compatiblilty; only need to find one that is compatable
//printf ("   2 comparing %s to %s\n", param_ptr->pf_dimNames[0], param_ptr->dimen[i]->name);
			if (badFlag == 1) {
				badFlag = isDimensionIncompatable (param_ptr->pf_dimNames[0], param_ptr->dimen[i]->name); // 0 = good;  1 = bad
			}
		}
		if (badFlag == 1) {
			return 1;
		}
	}

	//param_ptr->ndimen
	return 0;
}

// returns
// 0 = good;  1 = bad
static int isDimensionIncompatable (char *pfDimName, char *modDimName) {
//	char *dimNames[] ={"one",
//		"ncascade",
//		"ncascdgw",
//		"nsegment",
//		"npoigages",
//		"nsub",
//		"nhrucell",
//		"ngw",
//		"nhru",
//		"nssr",
//		"nsfres",
//		"nlake",
//		"nrain",
//		"nsol",
//		"ntemp",
//		"nratetbl",
//		"nwateruse",
//		"ndepl",
//		"ndeplval",
//		"ndays",
//		"nmonths",
//		"nlapse",
//		"nobs",
//		"nsnow",
//		"nform",
//		"nevap",
//		"nsfelev",
//		"nlakeelev",
//		"nwind",
//		"nhumid",
//		"ngate",
//		"nstage",
//		"ngate2",
//		"nstage2",
//		"ngate3",
//		"nstage3",
//		"ngate4",
//		"nstage4",
//		"mxnsos",
//	};

	if (!strncmp (pfDimName, modDimName, 10)) {  // a dimension is compatable with itself
		return 0; 
	}

	if (!strncmp (pfDimName, "one", 3)) {  // "one" in the parameter file is compatable with everything
		return 0; 
	}

	// Subbasin (nsub) can be mapped to these dimensions with mapping parameter
	// "nhru" "hru_subbasin";
    // "nsegment" "segment_subbasin";
    // "nrain" "rain_subbasin";
    // "ntemp" "temp_subbasin";
    // "nobs"  "obs_subbasin";
    // "ngw" "gw_subbasin";
    // "nssr" "ssr_subbasin";
	if (!strncmp (pfDimName, "nsub", 4)) {
		if (!strncmp (modDimName, "nhru", 4)) {
			return 0;

		} else if (!strncmp (modDimName, "nsegment", 8)) {
			return 0;

		} else if (!strncmp (modDimName, "nrain", 5)) {
			return 0;

		} else if (!strncmp (modDimName, "ntemp", 5)) {
			return 0;

		} else if (!strncmp (modDimName, "nobs", 4)) {
			return 0;

		} else if (!strncmp (modDimName, "ngw", 3)) {
			return 0;

		} else if (!strncmp (modDimName, "nssr", 4)) {
			return 0;
		}
	}
	

	return 1;
}

static int getParamFileParamSize (PARAM *param) {
	int i, size;

	size = 1;
	for (i = 0; i < param->pf_ndimen; i++) {  // check each dimension for size
		size = size * getdim(param->pf_dimNames[i]);
	}
	return size;
}

static void oneToAnySizedArray(PARAM *param, char *pf_value) {
	int i;

	if (param->type == M_DOUBLE) {
		for (i = 0; i < param->size; i++) {
			((double *)(param->value))[i] = *((double *)pf_value);
		}
	} else if (param->type == M_FLOAT) {
		for (i = 0; i < param->size; i++) {
			((float *)(param->value))[i] = *((float *)pf_value);
		}
	} else if (param->type == M_LONG) {
		for (i = 0; i < param->size; i++) {
			((int *)(param->value))[i] = *((int *)pf_value);
		}
	} else if (param->type == M_STRING) {
		for (i = 0; i < param->size; i++) {
			*((char **)param->value + i) = strdup (pf_value);
		}
	}
}

static char *getMapParamName(char *name) {
	char *mapParamName;
	int i;

	mapParamName = NULL;
	for (i = 0; i < (sizeof (dimNames) / sizeof (dimNames[0])); i++) {
		if (!strncmp (name, dimNames[i], strlen(dimNames[i]))) {
			mapParamName = mapParamNames[i];
		} 
	}

    return mapParamName;
}

static void subbasinTo1DArray (PARAM *param, PARAM *mapping_param, char *pf_value) {
	int i, map;

	if (param->type == M_DOUBLE) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
			((double *)(param->value))[i] = ((double *)pf_value)[map - 1];
		}

	} else if (param->type == M_FLOAT) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
			((float *)(param->value))[i] = ((float *)pf_value)[map - 1];
		}

	} else if (param->type == M_LONG) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
			((int *)(param->value))[i] = ((int *)pf_value)[map - 1];
		}

	} else if (param->type == M_STRING) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
//			*((char **)param->value + i) = strdup (*pf_value + map - 1);
			*((char **)param->value + i) = strdup (pf_value + map - 1);
		}
	}
}

