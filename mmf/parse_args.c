/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : parse_args.c
 * AUTHOR   : Mike Dixon CADSWES
 * DATE     : March 1990
 * FUNCTION : parse_args
 * COMMENT  : parses the command line arguments
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: parse_args.c 5242 2009-06-16 16:43:56Z markstro $
 *
   $Revision: 5242 $
        $Log: parse_args.c,v $
        Revision 1.15  1999/10/22 17:14:36  markstro
        Added private variables

        Revision 1.14  1999/08/25 17:44:33  markstro
        Version for MMS 1.1.1

        Revision 1.13  1999/08/24 16:34:12  markstro
        Version 1.1.1

        Revision 1.12  1997/11/25 15:49:37  markstro
        Initial version

        Revision 1.11  1997/09/26 16:32:25  markstro
        Added ESP batch run mode.

        Revision 1.10  1996/02/19 20:00:34  markstro
        Now lints pretty clean

        Revision 1.9  1995/05/25 14:26:33  markstro
        (1) Added batch mode
        (2) Replaced "b" functions with "mem" versions

 * Revision 1.8  1994/11/22  17:20:03  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.7  1994/11/08  16:17:33  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.6  1994/10/24  14:18:47  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.5  1994/08/31  21:50:36  markstro
 * Unknown
 *
 * Revision 1.4  1994/03/23  20:05:36  markstro
 * Changes from TERRA
 *
 * Revision 1.3  1994/02/11  23:12:10  markstro
 * Fixed up the "Edit Dimension Index Names" stuff.
 *
 * Revision 1.2  1994/01/31  20:17:03  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define PARSE_ARGS_C
#include <math.h> 
#include <string.h> 
#include <stdlib.h> 
#include "mms.h" 

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : parse_args
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void parse_args (int argc, char **argv, int *set_count, char **set_name, char **set_value) {

   int i;
   char *ptr;

   Mdebuglevel = 0;
   MAltContFile = strdup ("control");

/*
**  Get the model name.
*/
   ptr = strrchr (argv[0], '/');
   if (!ptr) ptr = strrchr (argv[0], '\\');
   if (ptr) ++ptr;
   else ptr = argv[0];

   model_name = strdup (ptr);

   executable_model = strdup (argv[0]);
   ptr = strstr (executable_model, ".exe");
   if (ptr) *ptr = '\0';

   if (argc >= 2) {
      for (i = 1; i < argc ; i++) {
		 if (!strcmp(argv[i], "-debug")) {
			 Mdebuglevel = atoi(argv[i+1]);

		 } else if (!strncmp(argv[i],"-C",2)) {
            MAltContFile = (char *)((argv[i]));
            MAltContFile+=2;

         } else if (!strncmp(argv[i],"-E",2)){
            MAltEnvFile = (char *)((argv[i]));
            MAltEnvFile+=2;

         } else if (!strncmp(argv[i],"-batch", 6)){
            batch_run_mode = TRUE;

         } else if (!strncmp(argv[i],"-esp", 4)){
            esp_mode = TRUE;

         } else if (!strncmp(argv[i],"-rosenbrock", 11)){
            rosenbrock_mode = TRUE;

         } else if (!strncmp(argv[i],"-print", 6)){
            print_mode = TRUE;

         } else if (!strncmp(argv[i],"-por", 4)){
            run_period_of_record = TRUE;

         } else if (!strncmp(argv[i],"-rtg", 4)){
            runtime_graph_on = TRUE;

		 } else if (!strncmp(argv[i],"-preprocess", 11)){
            preprocess_on = TRUE;

         } else if (!strncmp(argv[i],"-set",4)){
            i++;
            *(set_name + *set_count) = strdup ((char *)((argv[i])));
            i++;
            *(set_value + *set_count) = strdup ((char *)((argv[i])));
            (*set_count)++;

		 } else { // Assume argument with no flag is control file name
			MAltContFile = (char *)((argv[i]));
		 }
      }
   }
}
/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

