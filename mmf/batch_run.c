/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : batch_run.c
 * AUTHOR   : Steve Markstrom (markstro)
 * DATE     : Thu 18 May 2005
 * FUNCTION : batch_run
 * COMMENT  : runs the MMS time loop
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: batch_run.c 6387 2012-02-10 20:44:24Z markstro $
   $Revision: 6387 $
        $Log: batch_run.c,v $
        Revision 1.10  2000/02/18 18:27:03  markstro
        Made previous Julian time a global.  It is set to -1.0 before the run
        so that read_line knows to recalculate it.

        Revision 1.9  1999/12/07 21:10:42  markstro
        More nstep and init_var stuff

        Revision 1.8  1999/10/22 17:14:35  markstro
        Added private variables

        Revision 1.7  1997/04/18 16:44:09  markstro
        (1)  Commented out errno problem with opening files from fortran.
        (2)  Put in checks for saving parameter file when loading new one.
        (3)  Changes to runcontrol.c and timing.c unknown

        Revision 1.6  1996/09/12 23:36:26  msbrewer
        Added printf line to print "writing year" to screen.

        Revision 1.5  1996/06/28 19:32:20  markstro
        (1) Fixed 3d control window.
        (2) Fixed stats.

 * Revision 1.4  1996/06/24  20:45:58  markstro
 * put rdb stuff into batch mode
 *
        Revision 1.3  1996/05/24 17:59:55  markstro
        plot_widget curve data structure malloc fix

        Revision 1.2  1996/02/19 19:59:28  markstro
        Now lints pretty clean

        Revision 1.1  1995/05/25 15:26:30  markstro
        Initial version

-*/

/**1************************ INCLUDE FILES ****************************/

#define BATCH_RUN_C

#include <string.h>
#include <errno.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
extern int call_modules (char *);
extern char *single_run_pre_init (void);
extern char *single_run_post_init (void);
extern char *single_run_pre_run (void);
extern char *single_run_post_run (void);
extern char *single_run_pre_cleanup (void);
extern char *single_run_post_cleanup (void);

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : BATCH_run
 | COMMENT      :
 | PARAMETERS   : None
 | RETURN VALUE : char * - error message if there is one.
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
int BATCH_run (void) {
   char *ret;
   long endofdata = 0;

   ret = single_run_pre_init ();
   if (ret) {
      fprintf (stderr, ret);
      return(1);
   }

   if (call_modules("initialize")) {
      //closeUserFiles();
      fprintf (stderr, "single_run:  Problem with initializing modules.");
      return(1);
   }

   ret = single_run_post_init ();
   if (ret) return(1);

/*
* perform the main loop
*/

   M_stop_run = 0;
   MuserFiles = 1;
   Mprevjt = -1.0;

   while(!endofdata) {
      if(!(endofdata = read_line ())) {
         ret = single_run_pre_run ();
         if (ret) return(1);

/*
         if ((Mnowtime->month == 1) && (Mnowtime->day == 1)) {
             printf ("  running year = %ld\n", Mnowtime->year);
         }
*/
         errno = 0;

         if(call_modules("run")) {
            //closeUserFiles ();
            fprintf (stderr, "Problem while running modules.");
            return(1);
         }

         ret = single_run_post_run ();
         if (ret) return(1);
      }
   }

   ret = single_run_pre_cleanup ();
   if (ret) return(1);

/*
* cleanup modules
*/

   if (call_modules("cleanup")) {
       fprintf (stderr, "Problem with module cleanup.");
       return(1);
   }

   ret = single_run_post_cleanup ();
   if (ret) return(1);

   return(0);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/
