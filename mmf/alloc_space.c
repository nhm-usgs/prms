/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : alloc_space.c
 * AUTHOR   : Mike Dixon CADSWES March 1990
 * DATE     : Thu 20 Oct 1994
 * FUNCTION : alloc_space.c
 * COMMENT  : allocates space for variables
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: alloc_space.c 3058 2007-01-25 22:25:59Z rsregan $
 *
 *  $Revision: 3058 $
 *       $Log: alloc_space.c,v $
 *       Revision 1.19  1996/04/29 16:22:56  markstro
 *       Unknown
 *
 * Revision 1.18  1996/04/09  21:04:00  markstro
 * (1) Work on control files
 * (2) Runtime graphs
 *
 * Revision 1.17  1996/02/26  14:50:56  markstro
 * Some sensitivity work.
 *
 * Revision 1.16  1996/02/19  19:59:26  markstro
 * Now lints pretty clean
 *
 *       Revision 1.15  1994/11/23 20:12:40  markstro
 *       More malloc_dbg changes
 *
 * Revision 1.14  1994/11/22  17:19:08  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.13  1994/11/08  16:17:15  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.12  1994/10/24  14:18:07  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.11  1994/09/30  14:53:50  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.10  1994/03/07  21:19:57  markstro
 * Changes from TERRA
 *
 * Unknown
 *
 * Revision 1.8  1994/02/01  18:49:38  markstro
 * Made the declaration of read vars dynamic -- no more MAXREADVARS
 *
 * Revision 1.7  1994/02/01  18:35:01  markstro
 * Made the declaration of controls dynamic -- no more MAXCONTROLS
 *
 * Revision 1.6  1994/02/01  18:11:04  markstro
 * Made the declaration of dimensions dynamic -- no more MAXDIMENS
 *
 * Revision 1.5  1994/02/01  17:41:24  markstro
 * Made the declaration of parameters dynamic -- no more MAXPARAMS
 *
 * Revision 1.4  1994/02/01  17:14:06  markstro
 * Made the declaration of variables dynamic -- no more MAXVARS
 *
 * Revision 1.3  1994/01/31  20:15:54  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define ALLOC_SPACE_C
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : alloc_space
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void alloc_space (void) {
	static DATETIME start, end, now, next;

	cont_db = ALLOC_list ("Control Data Base", 0, 100);

  /*
   * space for the dimension pointer  array
   */

/*
  max_dims = 50;
  Mdimbase = (DIMEN **) umalloc (max_dims * sizeof(DIMEN *));
  Mndims = 0;
*/
	dim_db = ALLOC_list ("Dimension Data Base", 0, 50);

  /*
   * default dimension "one"
   */

  decldim ("one", 1, 1, "Default dimension with value 1");

  /*
   * space for the public variable pointer array
   */

  max_vars = 500;
  Mvarbase = (PUBVAR **) umalloc (max_vars * sizeof(PUBVAR *));
  Mnvars = 0;

/*
	var_db = ALLOC_list ("Variable data base", 0, 100);
*/

  /*
   * space for the parameter pointer  array
   */

  max_params = 500;
  Mparambase = (PARAM **) umalloc (max_params * sizeof(PARAM *));
  Mnparams = 0;
/*
	param_db = ALLOC_list ("Paraameter data base", 0, 100);
*/

  /*
   * space for the read check data base
   */

  max_read_vars = 50;
  Mcheckbase = (READCHECK **) umalloc (max_read_vars * sizeof(READCHECK *));
  Mnreads = 0;

/*
	read_var_db = ALLOC_list ("Paraameter data base", 0, 100);
*/

/*
* space for time structures
*/
	Mstrttime = &start;
	Mendtime = &end;
	Mnowtime = &now;
	Mnexttime = &next;

/*
* space for run info string
*/
	Mparaminfo = strdup ("Default case");
	Mdatainfo = strdup ("Default case");
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

