/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : read_params.c
 * AUTHOR   : CADSWES, modified by Steve Markstrom (markstro)
 * DATE     : Wed 12 Oct 1994
 * FUNCTION :
 * COMMENT  :
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * read_params.c: reads the params data base from a file
 * File name is passed in as an argument
 *
 * $Id: read_params.c 6064 2011-10-27 20:56:21Z markstro $
 *
   $Revision: 6064 $
        $Log: read_params.c,v $
        Revision 1.30  1998/10/20 15:53:02  markstro
        Fixed "blank" format.

        Revision 1.29  1997/09/22 14:07:09  markstro
        Unknown

        Revision 1.28  1996/08/28 15:24:12  markstro
        Unknown

 * Revision 1.27  1996/06/28  19:32:24  markstro
 * (1) Fixed 3d control window.
 * (2) Fixed stats.
 *
 * Revision 1.26  1996/04/29  16:23:12  markstro
 * Unknown
 *
 * Revision 1.25  1996/02/19  20:00:44  markstro
 * Now lints pretty clean
 *
        Revision 1.24  1995/05/25 14:26:38  markstro
        (1) Added batch mode
        (2) Replaced "b" functions with "mem" versions

 * Revision 1.23  1995/05/17  19:20:22  markstro
 * Bug fixes
 *
 * Revision 1.22  1995/02/10  23:58:30  markstro
 * Bug fixes for class
 *
 * Revision 1.21  1995/02/01  17:47:36  markstro
 * Addition of Rosenbrock optimization.  Start of sensitivity.  Many bug fixes.
 *
 * Revision 1.20  1994/12/21  21:36:19  markstro
 * (1) Fixed ESP to work with multiple data files.
 * (2) Fixed Optimization to work with multiple data files.
 * (3) Fixed Sensitivity to work with multiple data files.
 *
 * Revision 1.19  1994/11/22  17:20:12  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.18  1994/11/09  22:10:46  markstro
 * GIS stuff out
 *
 * Revision 1.17  1994/11/08  16:17:39  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.16  1994/10/24  14:18:53  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.15  1994/10/13  17:53:36  markstro
 * (1) Added annotation to parameter values through the spreadsheet
 * (2) Included <string.h> in a few more files that needed it.
 *
 * Revision 1.14  1994/09/30  14:54:57  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.13  1994/09/13  15:59:18  markstro
 * (1)  Version of save_params is now written into parameter file.
 * (2)  Took out min and max values for parameters -- these were not necessary.
 *
 * Revision 1.12  1994/09/09  14:56:27  markstro
 * (1)  Fixed up main edit menu.
 * (2)  Added a "notes" field to dimension indicies
 * (3)  A little more Rosenbrock work.
 * (4)  Fixed the list selector -- changed button names & first item
 *      selected by default.
 * (5)  Modified spread sheet help to be able to display dimension notes
 * (6)  Ran some source through "cb"
 *
 * Revision 1.11  1994/08/02  17:46:38  markstro
 * Split data file capabilities
 *
 * Revision 1.10  1994/07/25  17:06:39  markstro
 * Fixed message for when param file DNE.
 *
 * Revision 1.9  1994/05/18  17:15:56  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.8  1994/04/19  19:11:17  markstro
 * Unknown
 *
 * Revision 1.7  1994/04/08  16:04:11  markstro
 * Changes from TERRA
 *
 * Revision 1.6  1994/04/01  22:03:53  markstro
 * (1)  Error strings go back through return values.
 *
 * Revision 1.5  1994/03/25  22:07:33  markstro
 * Unknown
 *
 * Revision 1.3  1994/01/31  20:17:14  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
#define READ_PARAMS_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include "mms.h"

static char *READ_param_head (PARAM **, FILE **, char *, char[]);
static char *READ_param_values (PARAM *, FILE *, char []);
static char *rp (char *, int);
//static int ver = 0, rev = 0;

int nComments;
char **Comments;

/*--------------------------------------------------------------------*\
 | FUNCTION     : read_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_params (char *param_file_name, int index) {
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

	cptr = rp (param_file_name, index);

	if (cptr) {
		rp (old, index);

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
			(void)sprintf (buf, "read_dims: cannot open file: %s", param_file_name);
		} else {
			(void)sprintf (buf, "read_dims: cannot open file:");
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

		(void)sprintf (buf, "read_dims: problems reading info line");
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

		(void)sprintf (buf, "read_dims: problems reading version number");
		return (buf);
	}

  //if (!strncmp (line, "Version:", 8)) {
  //  nch = (char *)strchr (line, ' ');
  //  if (nch) {
  //    *nch = '\0';
  //    nch++;
  //    ver = atoi (nch);
  //  }

  //  nch = (char *)strchr (nch, '.');
  //  if (nch) {
  //    *nch = '\0';
  //    nch++;
  //    rev = atoi (nch);
  //  }

	if (!fgets (line, MAXDATALNLEN, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "read_dims: problems reading dimension label");
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
			(void)sprintf (buf, "read_dims: problems skipping comments");
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
		(void)sprintf (buf, "read_dims: file: %s '** Dimensions **' label expected but not found.",
		param_file_name);
		return (buf);
	}
  
	if (!fgets (line, MAXDATALNLEN, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "read_dims: unexpected end of file");
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
			(void)sprintf (buf, "read_dims: file: %s.  Expecting '####' found %s.", param_file_name, line);
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
			(void)sprintf (buf, "read_dims: file: %s.  Trying to read dimension name.", param_file_name);
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
				(void)sprintf (buf, "read_dims: file: %s key: %s Can't read dimension size.", param_file_name, key);
				return (buf);
			}

			errno = 0;
			dim_size = strtol(line, &endptr, 10);
			if (errno != 0) {
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
		       }
				(void)sprintf (buf, "read_dims: Trouble decoding size from: %s file: %s", line, param_file_name);
				return (buf);
			}

/*
**	If necessary, reset dimension to value read from file.
*/
			if (dim->value != dim_size) {
				reset_dim (dim, dim_size);
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
			(void)fprintf (stderr,"\nMMS Warning -- from read_dims:\ndimension '%s' is set in parameter file:\n%s\nbut has never been declared.\n\n", key, param_file_name);
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
static char *rp (char *param_file_name, int index) {

  FILE *param_file;
  PARAM *param;

  char line[MAXDATALNLEN];
  static char buf[256], *buf_ptr;


/*
* get param name, open file
*/
	if ((param_file = fopen (param_file_name, "r")) == NULL) {
		if (param_file_name)
			(void)sprintf (buf, "read_params: cannot open file: %s", param_file_name);
		else
			(void)sprintf (buf, "read_params: cannot open file:");

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
		buf_ptr = READ_param_head (&param, &param_file, param_file_name, line);
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
			buf_ptr = READ_param_values (param, param_file, line);
			if (buf_ptr) {
		        if (param_file != NULL) {
		           fclose (param_file);
		           param_file = NULL;
		        }
				return (buf_ptr);
			}
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
static char *READ_param_head (PARAM **param_ptr, FILE **param_file, char *param_file_name, char line[]) {
  char key[MAXDATALNLEN];
  char dimen[MAXDATALNLEN];
  static char buf[256];
  char *temp, *npos, *tempfmt;
  int tempwidth, i, param_size, type;
  
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
    (void)sprintf (buf, "\nread_params: Early end-of-file: %s", param_file_name);
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
  if (temp)
    tempwidth = atoi(temp);
  else
    tempwidth = 0;

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
  *param_ptr = param_addr (key);
  if (*param_ptr) {
	  /*
	  **  Set the read_in flag to true
	  */
	  (*param_ptr)->read_in = 1;
/*
* save format and column width
*/
/*
		(void)fprintf (stderr,"READ_param_head: tempwidth = %d\n", tempwidth);
*/
    (*param_ptr)->column_width = tempwidth;
    if (tempfmt) {
      tempfmt[strlen(tempfmt)-1] = '\0';
      if(!(*param_ptr)->format)
	(*param_ptr)->format = (char *)(malloc(strlen(tempfmt)+1));
      else
	(*param_ptr)->format = (char *)(realloc((*param_ptr)->format, strlen(tempfmt) + 1));
      (void)strcpy((*param_ptr)->format, tempfmt);
    } else
      (*param_ptr)->format = NULL;
/*
* get number of dimensions
*/
    if(fgets(line, MAXDATALNLEN, *param_file) == NULL) {
      (void)printf ("ERROR, getparam: reading param ndimen, Early end-of-file: %s",
		     param_file_name);
      exit(1);
    }

   if (isdigit(*line)) {
      if ((*param_ptr)->ndimen != atol(line)) {
         (void)printf (
            "\nERROR, getparam: number of dimensions for parameter %s\ndoesn't match parameter declaration.\nParameter File: %s\n",
			key, param_file_name);
         exit(1);
      }

      if((*param_ptr)->ndimen == 0) {
         (void)sprintf (buf,
            "read_params: decoding param ndimen from: %s\nkey is: %s\nfile: %s",
            line, key, param_file_name);
         return (buf);
      }
/*
* get number of dimensions if file format supports 2D arrays. Otherwise
* get dimension name.
*/

      for (i = 0; i < (*param_ptr)->ndimen; i++) {
        if(fgets(dimen, MAXDATALNLEN, *param_file) == NULL) {
           (void)sprintf (buf,
                   "read_params: reading param dimen, Early end-of-file: %s",
			       param_file_name);
           return (buf);
        }
        dimen[strlen(dimen) - 1] = '\0';

        if (strcmp(dimen, (*param_ptr)->dimen[i]->name)) {
			(void)fprintf (stderr,
                 "ERROR, getparam: expecting dimension: %s\nread dimension: %s\nparameter: %s\nfile: %s\n",
                 (*param_ptr)->dimen[i]->name, dimen, key, param_file_name);
			exit(1);
        }
     } /* i */

/*
* get param size
*/

     if(fgets(line, MAXDATALNLEN, *param_file) == NULL) {
        (void)sprintf (buf, "read_params: reading param size, Early end-of-file: %s", param_file_name);
        return (buf);
     }

     if((param_size = atol(line)) == 0) {
        (void)sprintf (buf, "read_params: decoding size from:%s\nkey: %s\nfile: %s",
        line, key, param_file_name);
        return (buf);
     }

      if(param_size != (*param_ptr)->size) {
		  (void)printf ("ERROR, getparam: size incorrect, \nexpected: %ld, specified: %d\nkey: %s\nfile: %s",
		       (*param_ptr)->size, param_size, key, param_file_name);
		exit(1);
      }
    } else {
      (*param_ptr)->ndimen = 1;
      strncpy(dimen, line, strlen(line));
      dimen[strlen(line)-1] = '\0';

      if (strcmp(dimen, (*param_ptr)->dimen[0]->name)) {
	(void)sprintf (buf, "read_params: reading parameter dimension \nexpecting: %s\nread: %s\nkey is: %s\nfile: %s",
		       (*param_ptr)->dimen[1]->name, dimen, key, param_file_name);
	return (buf);
      }
      (*param_ptr)->size = getdim(dimen);
      param_size = (*param_ptr)->size;
    }
/*
* get type
*/

    if(fgets(line, MAXDATALNLEN, *param_file) == NULL) {
      (void)sprintf (buf, "read_params: reading param type, Early end-of-file: %s",
		     param_file_name);
      return (buf);
    }
    if((type = atol(line)) == 0) {
      (void)printf ("\nERROR, getparam: decoding type from:%s\nkey: %s\nfile: %s\n",
		     line, key, param_file_name);
      exit(1);
    }
    if(type != (*param_ptr)->type) {
		(void)fprintf (stderr, "ERROR, getparam: incorrect or missing type: \nparameter: %s\nfile: %s\n", key, param_file_name);
		exit(1);
    }
  

  } else {
	  (void)printf ("read_params: parameter %s in %s not declared.\n",
		     key, param_file_name);
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
static char *READ_param_values (PARAM *param, FILE *param_file, char line[]) {
	int i, j;
	char *nch;
	int l1, l2, done;
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

	//if (!strncmp (param->name, "imperv_stor_max", 15)) {
	//	printf ("imperv_stor_max\n");
	//}
/*
**  Space for the values and value_desc are allocated in declparam
*/
	done = FALSE;
	i = 0;
	while (!done) {
		if (!fgets (line, MAXDATALNLEN, param_file)) {
			done = TRUE;

		} else if (!strncmp (line, "####", 4)) {
			done = TRUE;

		} else if (!param) {
			;

		} else if (line[0] == '@') {
			i--;

			nch = (char *)strchr (line, '\n');
			if (nch) *nch = '\0';

			if (desc_count) {
				if (param->value_desc[i]) {
					l1 = strlen (param->value_desc[i]);
					l2 = strlen (line);
					param->value_desc[i] = (char *)realloc
					    (param->value_desc[i],
					    (l1 + l2 + 2) * sizeof (char));
					strcat (param->value_desc[i], "\n");
					strcat (param->value_desc[i], &(line[1]));
				} else {
					param->value_desc[i] = strdup (&(line[1]));
				}
			} else {
//				if (param->value_desc[i]) free (param->value_desc[i]);

				param->value_desc[i] = strdup (&(line[1]));
			}
			i++;

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
					if (i < param->size) {
						switch (param->type) {

							case M_STRING:
                                comp_ptr[strlen(comp_ptr)-1] = '\0';
                                *((char **)param->value + i) = strdup (comp_ptr);
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
									((double *)(param->value))[i++] = d;
								} else {
									sprintf (buf, "There is a parameter format error. Parameter name: %s Index = %d\n   The data type should be a double precision float or there could be white spaces after the values on the line.", param->name, (i+1));
									//printf ("%s", buf);
									return (buf);
								}

								//((double *)(param->value))[i++] = atof(comp_ptr);
								break;

							case M_FLOAT:
								d = strtod(comp_ptr, &endp);
								if (comp_ptr != endp && *endp == '\n') {
									((float *)(param->value))[i++] = (float)d;
								} else {
									sprintf (buf, "Parameter format error. Parameter name: %s Index = %d\n   The data type should be a float or there could be white spaces after the values on the line.", param->name, (i+1));
									//printf ("%s", buf);
									return (buf);
								}

								//((float *)(param->value))[i++] = (float) atof(comp_ptr);
								break;

							case M_LONG:
								l = strtol(comp_ptr, &endp, 0);
								if (comp_ptr != endp && *endp == '\n') {
									((int *)(param->value))[i++] = (int)l;
								} else {
									sprintf (buf, "Parameter format error. Parameter name: %s Index = %d\n   The data type should be an integer or there could be white spaces after the values on the line.", param->name, (i+1));
									//printf ("%s", buf);
									return (buf);
								}

								//((int *)(param->value))[i++] =  atol(comp_ptr);
								break;
						} // switch
				 
					} else { // if (i < param->size)
						done = TRUE;
						i++;
					} // if (i < param->size)
				}
				result = strtok(NULL, delims);
				//printf ("foo\n");
			} // while

/*
**	If the parameter file was written by version 1.6 (or eariler) of
**	save_params, read the line for minimum and maximum parameter values, but
**	disregard them.
*/
			//if ((ver < 1)  || ((ver == 1) && (rev <=6))) {
			//	if (!fgets (line, MAXLNLEN, param_file))
			//		done = TRUE;
			//	if (!fgets (line, MAXLNLEN, param_file))
			//		done = TRUE;
			//}
		}
	}

	if (i < param->size) {
		printf ("ERROR, getparam: too few values read. \nparameter %s\nread_count = %d size = %ld\n", param->name, i, param->size);
		exit(1);
	} else if (i > param->size) {
		sprintf (buf, "READ_param_values: too MANY values read. param = %s read_count = %d size = %ld\n", param->name, i, param->size);
		printf ("%s", buf);
		return (buf);
	}

	return (NULL);
}
/***************************** TEST DRIVER ****************************/
