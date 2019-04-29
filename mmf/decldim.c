/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : decldim.c
 * AUTHOR   : CADSWES; modified by Steve Markstrom (markstro)
 * DATE     : 
 * FUNCTION : decldim() to be called from C
 *            decldim_() to be called from Fortran
 *            declfix() to be called from C
 *            declfix_() to be called from Fortran
 * COMMENT  : initializes an entry in the dimension database
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: decldim.c 6868 2012-05-08 21:56:34Z markstro $
 *
   $Revision: 6868 $
        $Log: decldim.c,v $
        Revision 1.13  1996/09/10 16:25:21  markstro
        Unknown

 * Revision 1.12  1996/04/29  16:22:59  markstro
 * Unknown
 *
 * Revision 1.11  1996/02/19  19:59:48  markstro
 * Now lints pretty clean
 *
        Revision 1.10  1994/11/23 20:12:45  markstro
        More malloc_dbg changes

 * Revision 1.9  1994/11/22  17:19:24  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.8  1994/10/24  14:18:17  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.7  1994/09/30  14:54:07  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.6  1994/09/15  17:22:43  markstro
 * Added the call declfix to the system for declaring fixed dimensions.
 *
 * Revision 1.5  1994/09/09  14:56:23  markstro
 * (1)  Fixed up main edit menu.
 * (2)  Added a "notes" field to dimension indicies
 * (3)  A little more Rosenbrock work.
 * (4)  Fixed the list selector -- changed button names & first item
 *      selected by default.
 * (5)  Modified spread sheet help to be able to display dimension notes
 * (6)  Ran some source through "cb"
 *
 * Revision 1.4  1994/02/01  21:17:11  markstro
 * Unknown
 *
 * Revision 1.3  1994/02/01  18:11:05  markstro
 * Made the declaration of dimensions dynamic -- no more MAXDIMENS
 *
 * Revision 1.2  1994/01/31  20:16:08  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define DECLDIM_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : decldim_
 | COMMENT		: called from Fortran, sorts out args and calls decldim()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long decldim_ (char *dname, ftnint *dval, ftnint *dmax, char *ddescr, ftnlen namelen, ftnlen descrlen) {
	long value, max;
	char *name, *descr;
	long retval;

/*
* copy value & max into local long int
*/

	value = *dval;
	max = *dmax;

/*
* copy args to new strings, and terminate correctly
*/

	name = (char *) umalloc(namelen + 1);
	strncpy(name, dname, namelen);
	name[namelen] = '\0';

	descr = (char *) umalloc(descrlen + 1);
	strncpy(descr, ddescr, descrlen);
	descr[descrlen] = '\0';

/*
* call C version of decldim()
*/

	retval = decldim(name, value, max, descr);

/*
* free up strings 
*/

/*
	ufree(name);
	ufree(descr);
*/
	return(retval);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : decldim
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long decldim (char *name, long value, long max, char *descr) {
	DIMEN *dim;

/*
* check that name does not already exist
*/


	dim = dim_addr(name);
   if (dim != NULL) {
		// This dimension has already been declared. Set the size to the
		// value of the last call.
		dim->value = value;

      return(0);
   }

   if (Mdebuglevel >= M_FULLDEBUG) {
      (void)fprintf(stderr, "Declaring dimension '%s'\n", name);
   }

/*
* check that default value is within limits
*/

   if(value < 0) {
      (void)fprintf(stderr, 
		    "ERROR - decldim - default dimension value negative.\n");
      (void)fprintf(stderr, "Name   :   '%s'\n", name);
      (void)fprintf(stderr, "Default:   %ld\n", value);
      return(1);
   }

   if(value > max) {
      (void)fprintf(stderr, 
        "ERROR - decldim - default dimension value exceeds max. allowed\n");
      (void)fprintf(stderr, "Name   :   '%s'\n", name);
      (void)fprintf(stderr, "Default:   %ld\n", value);
      (void)fprintf(stderr, "Max    :   %ld\n", max);
      return(1);
   }

/*
* allocate space for a structure, and store pointer in dimbase
*/
   dim = (DIMEN *) umalloc (sizeof(DIMEN));
   ADD_to_list (dim_db, (void *)dim);

/*
* allocate space, and store dimension properties
*/
   if (descr) dim->descr = strdup (descr);
   else dim->descr = NULL;

   if (name) dim->name = strdup (name);
   else dim->name = NULL;

   dim->value = value;
   dim->max = max;
   dim->names = NULL;
   dim->notes = NULL;
   dim->files = NULL;
   dim->format = NULL;
   dim->column_width = 10;
   dim->fixed = FALSE;
   dim->got = FALSE;

   sort_dims ();
   return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declfix
 | COMMENT		: Called from C to declare a fixed dimension.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declfix (char *name, long value, long max, char *descr) {
   long ret;

   ret = decldim (name, value, max, descr);
   ((DIMEN *)(dim_db->itm[dim_db->count - 1]))->fixed = TRUE;

   return (ret);
}
/*--------------------------------------------------------------------*\
 | FUNCTION     : declfix_
 | COMMENT		: called from Fortran to declare a fixed dimension.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declfix_ (char *dname, ftnint *dval, ftnint *dmax, char *ddescr, ftnlen namelen, ftnlen descrlen) {
	long	ret;

	ret = decldim_ (dname, dval, dmax, ddescr, namelen, descrlen);
	((DIMEN *)(dim_db->itm[dim_db->count - 1]))->fixed = TRUE;

	return (ret);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declmodule
 | COMMENT		: Called from C to set the version id for the module.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declmodule (char *mod_name, char * modType, char *id) {
	char *foo, *cp;

	printf ("%s %s, version: %s\n", modType, mod_name, id);

	foo = strdup (id);
	foo = foo + 5;
	cp = strchr (foo, '.');
	if (cp != NULL) {
		*cp = '\0';
	}
	
	current_module = (MODULE_DATA *) umalloc (sizeof(MODULE_DATA));
	current_module->name = strdup (mod_name);
	current_module->version = strdup (id);
	current_module->params = ALLOC_list ("params", 0, 100);
	current_module->vars = ALLOC_list ("vars", 0, 100);

    ADD_to_list (module_db, current_module);

	return 0;
}
///*--------------------------------------------------------------------*\
// | FUNCTION     : getmodule
// | COMMENT		:
// | PARAMETERS   :
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//MODULE_DATA * getmodule (char *key) { 
//	MODULE_DATA *module;
//	long i;
//
//	for (i = 0; i < module_db->count; i++) {
//		module = (MODULE_DATA *)(module_db->itm[i]);
//	   printf ("comparing %s to %s\n", key, module->name);
//		if (!strcmp(module->name, key))
//		return module;
//	}
//
//	/* if no match found, return null */
//	return NULL;
//}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declmodule_
 | COMMENT		: called from Fortran to set the version id for the module.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declmodule_ (char *mn, char *mt, char *id, ftnlen mnlen , ftnlen mtlen , ftnlen idlen) {
	char *id_foo;
	char *mt_foo;
	char *mn_foo;

/*
* copy args to new strings, and terminate correctly
*/
	id_foo = (char *) umalloc(idlen + 1);
	strncpy(id_foo, id, idlen);
	id_foo[idlen] = '\0';

	mt_foo = (char *) umalloc(mtlen + 1);
	strncpy(mt_foo, mt, mtlen);
	mt_foo[mtlen] = '\0';

	mn_foo = (char *) umalloc(mnlen + 1);
	strncpy(mn_foo, mn, mnlen);
	mn_foo[mnlen] = '\0';

	declmodule (mn_foo, mt_foo, id_foo);
	return 0;
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

