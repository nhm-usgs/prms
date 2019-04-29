/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : xmms.c
 * AUTHOR   : CADSWES; reworked by Markstrom
 * DATE     : 
 * FUNCTION : xmms - main driver for xmms
 * COMMENT  : Main driver for xmms system.
 * REF      :
 * REVIEW   :
 * PR NRS   :
 * $Id: mmf.c 5803 2010-12-08 22:38:20Z markstro $
 *
 -*/

/**1************************ INCLUDE FILES ****************************/
#define MAIN
#define MMF_C

#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <string.h>
#include "mms.h"


/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
extern int call_modules(char *);
extern int call_setdims(void);

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
  | FUNCTION     : main
  | COMMENT		: Main source for xmms
  | PARAMETERS   :
  |     int argc -- Argument count of command line parameters.
  |     char *argv[] -- Command line parameters.
  | RETURN VALUE : None
  | RESTRICTIONS : None
  \*--------------------------------------------------------------------*/
int main (int argc, char *argv[]) {
   int set_count;
   int i, j;
   char **set_name, **set_value;
   CONTROL *cp;
   char *cptr;
   double *dptr;
   float *fptr;
   long *lptr;
   char **cpt;
   struct stat stbuf;
   char	buf[512];
   char	*err;
   static int      num_param_files = 0;
   char   **fname;
   char pathname[MAXDATALNLEN];

   
	/*
	**  List of modules that are used by the model. This is
	**  determined by calls to declmodule
	*/
	module_db = ALLOC_list ("Module Data Base", 0, 100);

   //declmodule("PRMS system library", "$Id: mmf.c 5803 2010-12-08 22:38:20Z markstro $");

  /*
  **	parse the command-line arguments
  */
   set_count = 0;
   set_name = (char **)malloc (100 * sizeof (char *));
   set_value = (char **)malloc (100 * sizeof (char *));
   parse_args (argc, argv, &set_count, set_name, set_value);

   if (MAltContFile == NULL) {
      (void)fprintf (stderr,"Usage: Set the full path to the control file using the '-C' option.\n\n");
      exit(0);
   }

   alloc_space ();

   setup_cont ();
   	err = read_control (MAltContFile);
	if (err) {
       (void)fprintf (stderr,"%s\n", err);
        exit (1);           
   }

	fname =   control_svar ("param_file");
    num_param_files = control_var_size ("param_file");

   for (i = 0; i < set_count; i++) {
      cp = control_addr (*(set_name + i));
      if (cp) {

         (void)fprintf (stderr,"\nControl variable %s set to %s.\n\n",
                 *(set_name + i), *(set_value + i));

         cptr = (char *)strtok (*(set_value + i), ",");

         j = 0;
         while (cptr) {
            if (cp->type == M_DOUBLE) {
               dptr = (double *) cp->start_ptr;
               dptr[j] = atof(cptr);
            } else if (cp->type == M_FLOAT) {
               fptr = (float *) cp->start_ptr;
               fptr[j] = (float) atof(cptr);
            } else if (cp->type == M_LONG) {
               lptr = (long *) cp->start_ptr;
               lptr[j] =  atol(cptr);
            } else if (cp->type == M_STRING) {
               cpt = (char **) cp->start_ptr;
               cpt[j] = strdup (cptr);
            }

            cptr = (char *)strtok (NULL, ",");
            j++;
         }

      } else {
         (void)fprintf (stderr,"\nControl variable %s not found -- ignored.\n\n",
                 *(set_name + i));
      }
   }

	fname =   control_svar ("param_file");
    num_param_files = control_var_size ("param_file");

/*
   append_env (MAltEnvFile, MAltContFile);
*/

    if (call_setdims()) {
      (void)fprintf(stderr, "ERROR - mmf\n");
      (void)fprintf(stderr, "Calling function 'call_setdims'\n");
    }
    

    /*
    **	read dimension info from parameter file
    */
    if (stat (*control_svar("param_file"), &stbuf) != -1) {
       if (stbuf.st_size) {
      } else {
	     (void)fprintf (stderr,buf, "Parameter file: %s is empty.",
		               *control_svar("param_file"));
      }
    }
    
    err = read_dims (*control_svar("param_file"));
    if (err) {
      (void)fprintf (stderr,"MMS - Warning: %s\n", err);
    }


	fname =   control_svar ("param_file");
    num_param_files = control_var_size ("param_file");

    if (call_modules("declare")) {
      (void)fprintf(stderr, "ERROR - mmf\n");
      (void)fprintf(stderr, "Calling function 'call_modules'\n");
    }
    
    /*
    **	read in parameter values from parameter file
    */
	fname =   control_svar ("param_file");
    num_param_files = control_var_size ("param_file");

	for (i = 0; i < num_param_files; i++) {
		if (stat (fname[i], &stbuf) != -1) {
		   if (stbuf.st_size) {
		  } else {
			 (void)fprintf (stderr,buf, "Parameter file: %s is empty.",
						   fname[i]);
		  }
		}
	    
		err = read_params (fname[i], i);
		if (err) {
		  (void)fprintf (stderr,"MMS - Warning: %s\n", err);
		}
	}
    
    /*
    **  get data info string into the global
    */
    err = READ_data_info ();
    if (err) (void)fprintf (stderr,"MMS - Warning: %s", err);

    /*
    **	get start and end time
    */
    get_times ();
    
    if (print_mode) {
      print_params();
      print_vars();
      print_model_info();
	  (void)sprintf (pathname, "%s.param", MAltContFile);
	  save_params (pathname);
/*
    } else if (esp_mode) {
      ESP_batch_run ();
    } else if (rosenbrock_mode) {
      ROSENBROCK_batch_run ();
*/
    } else {

//      exit(BATCH_run ());
      BATCH_run ();
      ;
    }

    exit (0);
}


/**8************************** TEST DRIVER ****************************/

