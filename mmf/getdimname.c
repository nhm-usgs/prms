/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : getdimname.c
 * AUTHOR   : Pedro J. Restrepo, Steve Markstrom (markstro)
 * DATE     : May 1992
 * FUNCTION :
 * COMMENT  : The following are two routines to obtain the "ith" index name 
 *            of a dimension variable from either Fortran or C modules.
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
   $Revision: 5088 $
        $Log: getdimname.c,v $
        Revision 1.10  1999/08/24 16:34:04  markstro
        Version 1.1.1

        Revision 1.9  1996/04/29 16:23:02  markstro
        Unknown

 * Revision 1.8  1996/04/25  13:27:07  msbrewer
 * Fixed up Markstorm's pathetic coding mistakes
 *
        Revision 1.7  1996/04/23 14:29:49  markstro
        Added getdimdesc system function.

 * Revision 1.6  1996/02/19  20:00:04  markstro
 * Now lints pretty clean
 *
        Revision 1.5  1995/11/24 14:35:24  markstro
        Initial Purify work.
        This is the version for Watershed Systems Modeling class 11/27 - 12/1, 1995

 * Revision 1.4  1994/11/22  17:19:39  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.3  1994/09/30  14:54:22  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:16:29  markstro
 *me - dimension name. Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define GETDIMNAME_C
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimname_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimname_ (char *name, ftnint *i, char *idxname, ftnlen namelen, ftnlen idxlen) {
  /*
   * local copies
   */

  char * lname;

  lname = (char *)malloc(namelen+1);
  strncpy(lname, name, namelen);
  lname[namelen] = '\0';

  /*
   * call c version
   */
 getdimname(lname, (*i) - 1, idxname);
  
  idxlen = strlen(idxname);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimdesc_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimdesc_ (char *name, ftnint *i, char *desc, ftnlen namelen, ftnlen desclen) {
  /*
   * local copies
   */

  char * lname;

  lname = (char *)malloc(namelen+1);
  strncpy(lname, name, namelen);
  lname[namelen] = '\0';

/*
**	call c version
*/
	getdimdesc (lname, (*i) - 1, desc);
	desclen = strlen (desc);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimnameint_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimnameint_ (char *name, ftnint *i, ftnint *idx, ftnlen namelen) {
  /*
   * local copies
   */

  char * lname;
  char idxname[80];

  lname = (char *)malloc(namelen+1);
  strncpy(lname, name, namelen);
  lname[namelen] = '\0';

  /*
   * call c version
   */
 getdimname(lname, (*i) - 1, idxname);
  
  *idx = atoi(idxname);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimname
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimname (char *name, long i, char *idxname) {
  DIMEN *dim;

  dim = dim_addr(name);
  if (!dim) {
      (void)fprintf(stderr, "ERROR - getdimname, Can't find dimension named %s\n",name);
      return;
	}
  
  if (!dim->names) {
      (void)fprintf(stderr, "ERROR - getdimname. Dimension %s has no named indices\n",name);
      return;
    }
  (void)strcpy(idxname, dim->names[i]);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimdesc
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimdesc (char *name, long i, char *descname) {
	DIMEN *dim;

	dim = dim_addr(name);
	if (!dim) {
		(void)fprintf (stderr,
			"ERROR - getdimname, Can't find dimension named %s\n", name);
		(void)strcpy (descname, "");
		return;
	}
  
	if (!dim->notes) {
		(void)strcpy (descname, "");
		return;
	}

	if (dim->notes[i])
		(void)strcpy (descname, dim->notes[i]);
	else
		(void)strcpy (descname, "");

}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

