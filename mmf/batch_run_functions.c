/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : single_run.c
 * AUTHOR   : Mike Dixon
 *            edited April 1991 - Jim Brannon
 *            August 1991 -Pedro Restrepo
 * DATE     : June March 1990
 * FUNCTION :
 * COMMENT  :
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: batch_run_functions.c 5803 2010-12-08 22:38:20Z markstro $
 *
   $Revision: 5803 $
        $Log: single_run.c,v $
        Revision 1.47  2006/11/27 14:30:50  rsregan
	changed GIS file to animation (ani) file

        Revision 1.46  2000/08/01 14:30:50  markstro
        Time steps coming out of storms

        Revision 1.45  2000/04/19 15:59:17  markstro
        Fixed init var stuff

        Revision 1.44  2000/03/07 18:35:13  markstro
        Fixed Mnstep reset for subsequent runs.

        Revision 1.43  2000/02/22 17:10:22  markstro
        Fixed the GIS output file stuff.

        Revision 1.42  2000/02/18 18:27:08  markstro
        Made previous Julian time a global.  It is set to -1.0 before the run
        so that read_line knows to recalculate it.

        Revision 1.41  1999/12/07 21:10:43  markstro
        More nstep and init_var stuff

        Revision 1.40  1998/01/07 18:22:51  markstro
        Set precision of arc view GIS output files to 2 decimal places.

        Revision 1.39  1997/12/12 18:03:12  markstro
        Unknown

        Revision 1.38  1997/11/13 17:13:36  markstro
        unknown

        Revision 1.37  1996/10/10 13:26:47  markstro
        (1) Work on Rosenbrock
        (2) Bug in fix dimension size

        Revision 1.36  1996/06/28 19:32:32  markstro
        (1) Fixed 3d control window.
        (2) Fixed stats.

 * Revision 1.35  1996/05/24  17:59:58  markstro
 * plot_widget curve data structure malloc fix
 *
        Revision 1.34  1996/05/14 02:42:08  msbrewer
        Cleaned up cvs conflicts. Bug fixes in dump_to_db.

 *
 * Revision 1.32  1996/03/26  22:31:11  markstro
 * Work on GIS displayer.
 *
 * Revision 1.31  1996/03/14  21:11:21  markstro
 * Added runtime xmgr
 *
 * Revision 1.30  1996/02/19  20:01:10  markstro
 * Now lints pretty clean
 *
        Revision 1.29  1996/01/23 18:44:25  markstro
        Fixes for HP compiler

 * Revision 1.28  1995/11/24  14:35:50  markstro
 * Initial Purify work.
 * This is the version for Watershed Systems Modeling class 11/27 - 12/1, 1995
 *
 * Revision 1.27  1995/06/21  18:07:28  markstro
 * Scenario stuff
 *
 * Revision 1.26  1995/06/08  18:01:55  markstro
 * (1)  Fixed info window
 * (2)  Changed b functions to mem functions for solaris compatibility
 * (3)  Fixed default values in spreadsheet help
 *
 * Revision 1.25  1994/12/21  21:36:28  markstro
 * (1) Fixed ESP to work with multiple data files.
 * (2) Fixed Optimization to work with multiple data files.
 * (3) Fixed Sensitivity to work with multiple data files.
 *
 * Revision 1.24  1994/12/15  19:12:32  markstro
 * Changes for Christoph:  (1) More work on setting data file labels;
 * and (2) Fixed problems with empty display variable lists.
 *
 * Revision 1.23  1994/11/22  17:20:32  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.22  1994/11/10  23:26:49  markstro
 * (1)  Some memory fixes -- results of malloc_dbg.
 * (2)  More stuff removed from set menu.
 *
 * Revision 1.21  1994/11/09  22:10:51  markstro
 * GIS stuff out
 *
 * Revision 1.20  1994/11/08  16:17:47  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.19  1994/10/24  20:48:50  markstro
 * Hacked out the old text interface.
 *
 * Revision 1.18  1994/10/24  14:19:05  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.17  1994/09/30  14:55:18  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.16  1994/08/31  21:50:47  markstro
 * Unknown
 *
 * Revision 1.15  1994/08/02  17:46:43  markstro
 * Split data file capabilities
 *
 * Revision 1.14  1994/07/07  14:23:59  markstro
 * DG fixes
 *
 * Revision 1.13  1994/06/29  22:29:39  markstro
 * DG fixes
 *
 * Revision 1.12  1994/06/21  20:20:38  markstro
 * More work on taking the module name out of the DB keyword.
 *
 * Revision 1.11  1994/06/16  16:47:19  markstro
 * Worked over runcontrol.c
 *
 * Revision 1.10  1994/05/18  17:16:09  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.9  1994/05/11  14:29:41  markstro
 * Changes from TERRA
 *
 * Revision 1.8  1994/04/07  15:14:40  markstro
 * Work on autoconf system.
 * Cleaned up menu_bar variable.
 *
 * Revision 1.7  1994/03/25  22:06:50  markstro
 * TERRA chanages that use xmgr.
 *
 * Revision 1.6  1994/03/23  20:05:39  markstro
 * Changes from TERRA
 *
 * Revision 1.5  1994/03/11  21:16:44  markstro
 * Got rid of client_data data types.
 *
 * Revision 1.4  1994/01/31  20:17:30  markstro
 * Make sure that all source files have CVS log.
-*/
/**1************************ INCLUDE FILES ****************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/
  static FILE *statvar_file;
  static FILE **ani_out_files;
  static long nstatVars, naniVars;
  static char **statVar_names, **statVar_element;
  static char **aniVar_names;
  static char statvar_path[MAXDATALNLEN];
  static char ani_path[MAXDATALNLEN];
  static char output_path[MAXDATALNLEN];
  static char buf[256];
  static long i, j, init_flag, stats_flag, ani_out_flag;
  static char  *err_message, *c;
  static char   err[256];
  static int       started;
  static PUBVAR    **ani_out_vars, *var;
  static DIMEN **ani_out_dims, *dim;
  static FILE **ani_var_files;
  static int num_ani_dims, found, k;
  static char *pathname, *endptr;
  static FILE *var_file;
  static char line[MAXDATALNLEN];
  static DATETIME start_of_data, end_of_data;

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_pre_init
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_pre_init () {

  stats_flag = *control_lvar ("statsON_OFF");
  if (stats_flag == 1)
     nstatVars = *control_lvar("nstatVars");
  else
     nstatVars = 0;

  if (!nstatVars)
    stats_flag = 0;

  ani_out_flag = *control_lvar ("aniOutON_OFF");
  if (ani_out_flag == 1)
     naniVars = *control_lvar("naniOutVars");
  else
     naniVars = 0;

  if (!naniVars)
    ani_out_flag = 0;

/*
**  Make sure that all of the selected stat and display variables are OK
**  before a run is attempted.
*/
  if (stats_flag) {
    err_message = CHECK_stat_vars();
    if (err_message) return (err_message);
  }

  if (ani_out_flag) {
    err_message = CHECK_ani_vars ();
    if (err_message) return (err_message);
  }

/*
* create stats vars linked list
*/
  if (stats_flag)
    create_vstats();

/*
* open data file
* ensure datainfo is up to date
*/
  err_message = DATA_read_init ();
  if (err_message) {
     (void)printf ("%s\n", err_message);
     return (err_message);
  }

/*
 * **  Reset run period to period of record if -por flag set
 */
  if (run_period_of_record) {
    DATA_find_end (&start_of_data, &end_of_data);
    julday (&start_of_data);
    julday (&end_of_data);

    printf ("resetting model start time to period of record %ld %ld %ld %ld %ld %ld\n",
            start_of_data.year, start_of_data.month, start_of_data.day,
            start_of_data.hour, start_of_data.min, start_of_data.sec);

    printf ("resetting model end time to period of record %ld %ld %ld %ld %ld %ld\n",
            end_of_data.year, end_of_data.month, end_of_data.day,
            end_of_data.hour, end_of_data.min, end_of_data.sec);


    Mendtime = &end_of_data;
    Mstrttime = &start_of_data;
  }

  err_message = DATA_check_start ();
  if (err_message) {
     (void)printf ("%s\n", err_message);
     return (err_message);
  }

/*
* Open statvar file, and store number of variables and variable names
*/
  if (stats_flag) {
    (void)sprintf(statvar_path, "%s", *((char **) control_var("stat_var_file")));

    if ((statvar_file = fopen(statvar_path, "w")) == NULL) {
      (void)sprintf (err, "ERROR - single_run: Could not open statvar file '%s'\n",
		     statvar_path);
      return (err);
    }

    statVar_names = (char **) control_var("statVar_names");
    statVar_element = (char **) control_var("statVar_element");

/*
* write number of variables and statVars names to stats data file.
*/
    (void)fprintf(statvar_file,"%ld\n",nstatVars);

    for (i = 0; i < nstatVars; i++)
      (void)fprintf(statvar_file,"%s %s\n", statVar_names[i], statVar_element[i]);
  }

/*
* Open ani output files.
*/
  if (ani_out_flag) {
    aniVar_names = (char **) control_var("aniOutVar_names");
    (void)sprintf(ani_path, "%s", *((char **) control_var("ani_output_file")));

    ani_out_dims = (DIMEN **)malloc (naniVars * sizeof (DIMEN *));
    ani_var_files = (FILE **)malloc (naniVars * sizeof (FILE *));
    ani_out_vars = (PUBVAR **)malloc (naniVars * sizeof (PUBVAR *));

/*
**  Get the pubvars.
*/
    for (i = 0; i < naniVars; i++) {

       c = strchr (aniVar_names[i], '.');
         if (c)
           *c = '\0';

       sprintf (buf, "%s.%s", ani_path, aniVar_names[i]);
       c = strchr (buf, ' ');
         if (c)
           *c = '\0';

       ani_out_vars[i] = var_addr (aniVar_names[i]);
    }

/*
**  List of unique ANIMATION dimensions.
*/
    num_ani_dims = 0;
    for (i = 0; i < naniVars; i++) {
       found = FALSE;
       for (j = 0; j < num_ani_dims; j++)
          if (ani_out_vars[i]->dimen[0] == ani_out_dims[j])
             found = TRUE;

       if (!found) {
          ani_out_dims[j] = ani_out_vars[i]->dimen[0];
          num_ani_dims++;
       }
    }

/*
**  Open a file for each dimension.
*/
    ani_out_files = (FILE **)malloc (num_ani_dims * sizeof (FILE *));

    for (i = 0; i < num_ani_dims; i++) {
       sprintf (buf, "%s.%s", ani_path, ani_out_dims[i]->name);
       if ((ani_out_files[i] = fopen(buf, "w")) == NULL) {
          (void)sprintf (err, "ERROR - single_run: Could not open ani file '%s'\n", buf);
          return (err);
       }

       fprintf (ani_out_files[i], "#\n# Begin DBF\n");
       fprintf (ani_out_files[i], "# timestamp,#FIELD_ISODATETIME,19,0\n");
       fprintf (ani_out_files[i], "# %s,#FIELD_DECIMAL,10,2\n", ani_out_dims[i]->name);
    }

/*
**  Map each variable to a file.
*/
    for (i = 0; i < naniVars; i++) {
       for (j = 0; j < num_ani_dims; j++) {
          if (ani_out_vars[i]->dimen[0] == ani_out_dims[j]) {
             ani_var_files[i] = ani_out_files[j];
          }
       }
    }

/*
**  Finish writing the headers.
*/
    for (i = 0; i < naniVars; i++)
       fprintf (ani_var_files[i], "# %s,#FIELD_DECIMAL,10,2\n", ani_out_vars[i]->name);

    for (i = 0; i < num_ani_dims; i++) {
       fprintf (ani_out_files[i], "# End DBF\n#\n");
    }

/*
**  Write variable name line
*/
    for (i = 0; i < num_ani_dims; i++) {
       fprintf (ani_out_files[i], "timestamp	%s", ani_out_dims[i]->name);
    }

    for (i = 0; i < naniVars; i++) {
       fprintf (ani_var_files[i], "	%s", ani_out_vars[i]->name);
    }

    for (i = 0; i < num_ani_dims; i++)
       fprintf (ani_out_files[i], "\n");

/*
**  Write variable size line
*/
    for (i = 0; i < num_ani_dims; i++) {
       fprintf (ani_out_files[i], "19d	10n");
    }

    for (i = 0; i < naniVars; i++) {
       fprintf (ani_var_files[i], "	10n");
    }

    for (i = 0; i < num_ani_dims; i++)
       fprintf (ani_out_files[i], "\n");

  }

/*
* Open output file
*/
  (void)sprintf(output_path, "%s", *control_svar("model_output_file"));

  if ((Moutfile = fopen(output_path, "w")) == NULL) {
    (void)sprintf (err, "single_run: Could not open '%s'", output_path);
    return (err);
  }

/*
** Read the last nstep from the var init file if there is one
*/
  init_flag = *control_lvar("init_vars_from_file");

   if (init_flag) {
/*
* get var name, open file
*/
      pathname = strdup (*control_svar("var_init_file"));

      if ((var_file = fopen (pathname, "r")) == NULL) {
         (void)fprintf(stderr, "WARNING - read_vars - cannot open file '%s'\n",
                       pathname);
/*
         ufree(pathname);
*/
         return("WARNING - read_vars - cannot open file");
      }

/*
* read in run info string
*/
      if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
         fclose(var_file);
         return("WARNING - read_vars - no run info string");
      }

/*
* read in last nstep
*/
      if (fgets(line, MAXDATALNLEN, var_file) == NULL) {
         fclose(var_file);
         return("WARNING - read_vars - no last nstep");
      }

      Mnsteps = strtol(&(line[11]), &endptr, 10);
      fclose(var_file);
  } else {
/*
**  set initial values of nsteps global variable if they
**  don't come from the var init file
*/
    Mnsteps = 0;
  }
/*
* initialize modules
*/
  MuserFiles = 1;

  errno = 0;
  return(NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_post_init
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_post_init () {

/*  DANGER -- commented out until we can figure out why errno
 *  **            always indicates an error with the sun fortran
 *  **            compiler.

    if (errno) {
        return ("single_run - module initialization");
        perror (" ");
	return;
    }
*/


/* print initial debug information (start and end dates
*/
/*
  initialDebugInfo();
*/

/*
    if (errno) {
        return ("single_run - initializing output tools");
        perror (" ");
	return;
    }
*/

  initializeRuntimeGraphs();

/*
* if required, initialize vars from file
*/
  init_flag = *control_lvar("init_vars_from_file");

  if(init_flag) {
    read_vars(*control_svar("var_init_file"));
  } else {
/*
**  set initial values of nsteps global variable if they
**  don't come from the var init file
*/
    Mnsteps = 0;
  }

  //printf ("Mnsteps = %ld\n", Mnsteps);

  started = FALSE;

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_pre_run
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_pre_run () {
	/*
	static char nameToCheck[256], path[256];
	struct stat stbuf;
	char	*err;
	*/

	started = TRUE;


/*
*  print debug  information
*/
/*
      runDebugInfo();
*/
	/* DANGER markstro commenting out this hardwired dynamic parameter stuff
	 * Not sure how this got in here.
	 
	  strncpy (path, "C:\\markstro\\development\\prms\\dynamic_param_test\\input\\parameters\\imperv_area\\", 256);
	  sprintf (nameToCheck, "%s%4ld%02ld%02ld%s", path, Mnowtime->year, Mnowtime->month, Mnowtime->day, '\0');


	if (stat (nameToCheck, &stbuf) != -1) {
		if (stbuf.st_size) {
			printf ("single_run_pre_run: found %s\n", nameToCheck);

			err = read_params (nameToCheck, 1);
			if (err) {
				(void)fprintf (stderr,"single_run_pre_run:  %s\n", err);
				return (err);
			}

			updateparam ("hru_percent_imperv");

		} else {
			printf ("single_run_pre_run: found but empty %s\n", nameToCheck);

		}
	} else {
		//printf ("single_run_pre_run: did not find %s\n", nameToCheck);
	}
*/

	errno = 0;
	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_post_run
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_post_run () {
/*
  if (errno) {

                sprintf (err, "single_run: call to run modules\nLast time step run: %d %d %d %d %d %f\nerrno = %d\n", Mnowtime->year, Mnowtime->month, Mnowtime->day, Mnowtime->hour, Mnowtime->min, Mnowtime->sec, errno);
                perror (err);
                return (err);
            }
*/

      if (stats_flag)
         write_vstats (statvar_file);

/*
**  Write the ANIMATION output variables to their respective files.
*/
      if (ani_out_flag) {

/*
**  Each dimension has it's own file.
*/
         for (i = 0; i < num_ani_dims; i++) {
            dim = ani_out_dims[i];
            for (j = 0; j < dim->value; j++) {

/*
**  Write the current time stamp to the dimension file.
*/
            fprintf (ani_out_files[i], "%4ld-%02ld-%02ld:%02ld:%02ld:%02ld\t%10ld",
                     Mnowtime->year,
                     Mnowtime->month, Mnowtime->day, Mnowtime->hour,
                     Mnowtime->min, Mnowtime->sec, j + 1);

/*
**  Write the variable values to the dimension file.
*/
               for (k = 0; k < naniVars; k++) {
                  var = ani_out_vars[k];
                  if (var->dimen[0] == dim) {
                     switch (var->type) {
                        case M_DOUBLE:
                           fprintf (ani_var_files[k], "\t%10.3e",
                                    *((double *) var->value + j));
                           break;

                        case M_FLOAT:
                           fprintf (ani_var_files[k], "\t%14.6e",
                                    *((float *) var->value + j));
                           break;

                        case M_LONG:
                           fprintf (ani_var_files[i], "\t%10ld",
                                    *((long *) var->value + j));
                           break;
                     }
                  }
               }
               fprintf (ani_out_files[i], "\n");
            }
         }
      }

      plotRuntimeGraphValue();

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_pre_cleanup
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_pre_cleanup () {

/*
  if (GISProcessRunning)
    GISEndAnimation();

  if (ani_init) {
    RESET_animation_control ();
  }
*/

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_post_cleanup
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_post_cleanup () {

/*
* free up stats vars linked list
*/
  if (stats_flag)
    free_vstats();

/*
* close files and tidy up
*/
  DATA_close ();
  //closeUserFiles();

  if (stats_flag)
    fclose(statvar_file);

/*
**  Close the ANIMATION output files
*/
   if (ani_out_flag) {
      for (i = 0; i < num_ani_dims; i++)
         fclose (ani_out_files[i]);

/*
      free (ani_out_files);
*/
   }

  fclose(Moutfile);

  closeRuntimeGraphs();

  if (!started) {
    return ("Run period outside of data in file.");
  }

/*
* compute statistics
*/

/*
  if (stats_flag)
      if (stats())
          return ("Problem with statistics.");
*/

/*
* if required, save vars to file
*/
  if (*control_lvar("save_vars_to_file"))
    save_vars (*control_svar("var_save_file"));

/*
* if required, save vars to file
*/
  if (preprocess_on) {
    write_preprocess_params ();
  }

   return (NULL);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

