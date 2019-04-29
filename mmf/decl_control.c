/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : decl_control.c
 * AUTHOR   :
 * DATE     :
 * FUNCTION : decl_control
 * COMMENT  : initializes a module variable entry in the memory database
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: decl_control.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: decl_control.c,v $
        Revision 1.12  1996/04/09 21:04:05  markstro
        (1) Work on control files
        (2) Runtime graphs

 * Revision 1.11  1996/02/19  19:59:46  markstro
 * Now lints pretty clean
 *
        Revision 1.10  1994/11/23 20:12:44  markstro
        More malloc_dbg changes

 * Revision 1.9  1994/11/22  17:19:23  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.8  1994/11/08  16:17:24  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.7  1994/10/24  14:18:16  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.6  1994/09/30  14:54:05  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.5  1994/08/31  21:50:27  markstro
 * Unknown
 *
 * Revision 1.4  1994/02/01  21:17:10  markstro
 * Unknown
 *
 * Revision 1.3  1994/02/01  18:35:03  markstro
 * Made the declaration of controls dynamic -- no more MAXCONTROLS
 *
 * Revision 1.2  1994/01/31  20:16:07  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define DECL_CONTROL_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**************************************************************************/

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : add_control
 | COMMENT		: This allocates a control structure and adds it to the
 |                control DB.  It also allocates the space for the variables.
 | PARAMETERS   :
 | RETURN VALUE : None
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
CONTROL *add_control (char *key, long type, long size) {
   CONTROL *cp;

/*
**	check that key does not already exist
*/

   if (control_addr (key)) {
      (void)fprintf (stderr,
         "ERROR - add_control - key '%s' already exists.\n", key);
      exit(1);
   }
// printf ("adding control parameter - key: %s type: %ld size: %ld\n", key, type, size);

/*
**  allocate space for a structure, and store pointer in controls
**  allocate space, and store control variable properties
*/
   cp = (CONTROL *) umalloc (sizeof(CONTROL));
   ADD_to_list (cont_db, (void *)cp);

   cp->key = strdup (key);
   cp->size = size;
   cp->type = type;
   cp->set_in_file = 0;

   if (type == M_STRING) {
      cp->start_ptr = (char *)umalloc (sizeof (char *) * size);
   
   } else if (type == M_LONG) {
      cp->start_ptr = (char *)umalloc (sizeof (long) * size);

   } else if (type == M_FLOAT) {
	   cp->start_ptr = (char *)umalloc (sizeof (float) * size);

   } else if (type == M_DOUBLE) {
	   cp->start_ptr = (char *)umalloc (sizeof (double) * size);

   } else {
      (void)fprintf (stderr,
         "ERROR - add_control - key '%s' don't know what type code %ld is.\n", key, type);
      exit(1);
   }

   return cp;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : decl_control
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : None
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void decl_control (char *key, long type, long size, void *valstr) {
   CONTROL *cp;

/*
**	check that key does not already exist
*/

   if (control_addr (key)) {
      (void)fprintf (stderr,
         "ERROR - decl_control - key '%s' already exists.\n", key);
      exit(1);
   }

/*
**  allocate space for a structure, and store pointer in controls
**  allocate space, and store control variable properties
*/
   cp = (CONTROL *) umalloc (sizeof(CONTROL));
   ADD_to_list (cont_db, (void *)cp);

   cp->key = key;
   cp->size = size;
   cp->type = type;
   cp->start_ptr = (char *)valstr;
   cp->set_in_file = 0;

}

void decl_control_string (char *key, char *valstr) {
   char **cp;
   cp = (char **)umalloc (sizeof (char *) * 1);
   *cp = strdup (valstr);
   decl_control (strdup (key), M_STRING, 1, cp);
}

void decl_control_string_array (char *key, long size, char *valstr) {
   char **cp;
   int i;

   cp = (char **)umalloc (sizeof (char *) * size);
   for (i = 0; i < size; i++) {
      cp[i] = strdup (valstr);
   }

   decl_control (strdup (key), M_STRING, size, cp);
}

void decl_control_int_array (char *key, long size, long *valstr) {
   long *lp;
   int i;

   lp = (long *)umalloc (sizeof (long) * size);
   for (i = 0; i < size; i++) {
      lp[i] = valstr[i];
   }

   decl_control (strdup (key), M_LONG, size, lp);
}

void decl_control_float_array (char *key, long size, float *valstr) {
   float *fp;
   int i;

   fp = (float *)umalloc (sizeof (float) * size);
   for (i = 0; i < size; i++) {
      fp[i] = (float)(valstr[i]);
   }

   decl_control (strdup (key), M_FLOAT, size, fp);
}

void decl_control_double_array (char *key, long size, double *valstr) {
   double *fp;
   int i;

   fp = (double *)umalloc (sizeof (double) * size);
   for (i = 0; i < size; i++) {
      fp[i] = (double)(valstr[i]);
   }

   decl_control (strdup (key), M_DOUBLE, size, fp);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : decl_control_
 | COMMENT		: decl_control_() is called from Fortran, sorts out args
 |                 and calls decl_control()
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void decl_control_ (char *ckey, ftnint *ctype, ftnint *csize, void *value, ftnlen klen) {
	char *key;
	long type, size;

  /*
   * copy ctype and csize to local long int
   */
	type = *ctype;
	size = *csize;

  /*
   * copy args to new strings, and terminate correctly
   */
	key = (char *) umalloc((unsigned int)(klen + 1));
	strncpy(key, ckey, (int)klen);
	key[klen] = '\0';

	decl_control(key, type, size, value);
	return;
}
/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/
