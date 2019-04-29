/*
 *  $Id: globals.h 5145 2012-12-19 17:39:07Z rsregan $
 */
/*
**
**		$Author: rsregan $
**
**		$Date: 1996/02/19 20:05:09 $
**
**		$Locker:  $
**
**		$Log: globals.h,v $
**		Revision 1.15  1996/02/19 20:05:09  markstro
**		After first lint session
**
**		Revision 1.14  1996/01/23 20:09:23  markstro
**		Fixed menu_bar
**
 * Revision 1.13  1996/01/23  18:43:58  markstro
 * Fixes for HP compiler
 *
 * Revision 1.12  1995/06/21  18:06:54  markstro
 * Scenario stuff
 *
 * Revision 1.11  1994/12/09  16:16:27  markstro
 * (1)  Work on global variables
 * (2)  Changed some prototypes
 *
 * Revision 1.10  1994/11/22  17:18:43  markstro
 * (1) CLeaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.9  1994/11/08  16:16:15  markstro
 * More proto type fine tuning
 *
 * Revision 1.8  1994/05/23  14:26:03  markstro
 * (1) Put in ifndef so included only once.
 * (2) Cleaned out a lot of includes in includes
 *
 * Revision 1.7  1994/05/18  17:15:06  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.6  1994/05/11  14:29:22  markstro
 * Changes from TERRA
 *
 * Revision 1.5  1994/03/23  20:05:18  markstro
 * Changes from TERRA
 *
 * Revision 1.4  1994/03/11  21:15:46  markstro
 * Got rid of client_data data types.
 *
 * Revision 1.3  1994/02/28  22:57:53  markstro
 * Cleaned up some of the "Set" dialogs.
 *
 * Revision 1.2  1994/01/04  19:37:25  markstro
 * Pedro's include files
 *
 * Revision 1.1  91/07/24  08:20:04  schmitz
 * Initial revision
 *
 * Revision 2.1  91/07/07  13:46:30  farah
 * *** empty log message ***
 *
 * Revision 2.0  91/04/16  13:49:37  brannon
 * new revision branch started by Jim Brannon
 *
 * Revision 1.2  91/04/12  14:01:11  brannon
 * left locked by steve
 *
 * Revision 1.1  90/07/31  12:25:05  markb
 * Initial revision
 *
**Revision 1.1  90/07/31  12:11:30  markb
**Initial revision
**
**
**		$Revision: 5145 $
**
**		$Source: /z/runoff/work4/mms_cvs/mms/src/include/globals.h,v $
**
**		$State: Exp $
*/

#ifndef MMS_GLOBAL_H
#define MMS_GLOBAL_H

#ifdef MAIN

/*
 * for MAIN only
 */

char *MAltContFile = NULL;          /* Alt. name of control file */
char *MAltEnvFile = NULL;           /* Alt. name of enivironment file */
long Mdebuglevel = 0;               /* the current debug level */
char *model_name = NULL;
char *executable_model = NULL;
int batch_run_mode = FALSE;         /* flag for running in batch mode  */
int esp_mode = FALSE;               /* flag for running esp in batch mode  */
int rosenbrock_mode = FALSE;        /* flag for running rosenbrock opt in
                                            batch mode  */
int run_period_of_record = FALSE;   /* flag for running entire period of
                                            record in batch mode  */
int print_mode = FALSE;
int runtime_graph_on = FALSE;
int preprocess_on = FALSE;         /* flag for running in preprocess mode */
LIST *cont_db;
LIST *dim_db;
LIST *module_db;
MODULE_DATA *current_module;
PUBVAR **Mvarbase = NULL;           /* pointer to public variables data base */
long Mnvars = 0;                    /* no of public variables in data base */
PARAM **Mparambase = NULL;          /* pointer to parameter data base */
long Mnparams = 0;                  /* no of params in data base */
READCHECK **Mcheckbase = NULL;      /* pointer to read check data base */
long Mnreads = 0;                   /* max no of calls to be made by readvar */
DATETIME *Mstrttime = NULL;         /* pointer to start time structure */
DATETIME *Mendtime = NULL;          /* pointer to end time structure */
DATETIME *Mnowtime = NULL;          /* pointer to current data time structure */
DATETIME *Mnexttime = NULL;         /* pointer to next data time structure */
char *Mparaminfo = NULL;            /* pointer to param information string */
char *Mdatainfo = NULL;             /* pointer to data information string */
PARAM **unsort_params = NULL;       /* pointer to unsorted parameters */
FILE_DATA   **fd = NULL;
long Mnsteps = 0;                   /* the number of steps so far */
double Mprevjt = -1.0;              /* the latest previous Julian time  */
double Mdeltat = 0.0;               /* the latest time step in hours */
char *Minpptr = NULL;               /* pointer to current posn in data input line*/
double Mdeltanext = 0.0;            /* the latest next time step in hours */
FILE *Moutfile = NULL;              /* pointer to model output file */
int MuserFiles = 0;                 /* to allow or disallow user-written files */
int  M_stop_run = 0;                /* Run switch 0 -or 1 */
STAT_LIST_TYPE *Mfirst_stat_list = NULL;     /* pointer to first entry
						in stats link list */
char scen[80];
char scen_info[80];
char *Mtypes[] = {"", "long", "float", "double", "string", "", "","", "", ""};
long ParamBaseIsDirty = FALSE;
int max_vars;
int max_params;
int max_read_vars;
int max_dims;
int max_controls;
int popen_pid;

#else

/*
 * for all functions except main
 */

extern char *MAltContFile;
extern char *MAltEnvFile;
extern long Mdebuglevel;
extern char *model_name;
extern char *executable_model;
extern int batch_run_mode;
extern int esp_mode;
extern int rosenbrock_mode;
extern int run_period_of_record;
extern int print_mode;
extern int runtime_graph_on;
extern int preprocess_on;
extern LIST *cont_db;
extern LIST *dim_db;
extern LIST *module_db;
extern MODULE_DATA *current_module;
extern PUBVAR **Mvarbase;
extern long Mnvars;
extern PARAM **Mparambase;
extern long Mnparams;
extern READCHECK **Mcheckbase;
extern long Mnreads;
extern DATETIME *Mstrttime;
extern DATETIME *Mendtime;
extern DATETIME *Mnowtime;
extern DATETIME *Mnexttime;
extern char *Mparaminfo;
extern char *Mdatainfo;
extern PARAM **unsort_params;
extern FILE_DATA   **fd;
extern long Mnsteps;
extern double Mprevjt;
extern double Mdeltat;
extern char *Minpptr;
extern double Mdeltanext;
extern FILE *Moutfile;
extern int MuserFiles;
extern int  M_stop_run;
extern STAT_LIST_TYPE *Mfirst_stat_list;
extern char scen[];
extern char scen_info[];
extern char *Mtypes[];
extern long ParamBaseIsDirty;
extern int max_vars;
extern int max_params;
extern int max_read_vars;
extern int max_dims;
extern int max_controls;
extern int popen_pid;

#endif /* MAIN */

#endif /* MMS_GLOBAL_H */
