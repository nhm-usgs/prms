/**************************************************************************
 * declvar.c: initializes a module variable entry in the memory database
 *
 * There are 2 functions: declvar() to be called from C
 *                        declvar_() to be called from Fortran
 *
 * Returns 0 if successful, 1 otherwise.

 * $Id: declvar.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: declvar.c,v $
        Revision 1.17  1999/10/22 17:14:35  markstro
        Added private variables

        Revision 1.16  1996/02/19 19:59:51  markstro
        Now lints pretty clean

        Revision 1.15  1995/11/25 02:42:12  markstro
        Reading unit vs. daily data files.

 * Revision 1.14  1994/11/23  20:12:46  markstro
 * More malloc_dbg changes
 *
 * Revision 1.13  1994/11/22  17:19:26  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.12  1994/11/08  16:17:26  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.11  1994/10/24  14:18:20  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.10  1994/09/30  14:54:09  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.9  1994/06/21  20:20:24  markstro
 * More work on taking the module name out of the DB keyword.
 *
 * Revision 1.8  1994/06/16  16:47:07  markstro
 * Worked over runcontrol.c
 *
 * Revision 1.7  1994/02/01  21:17:13  markstro
 * Unknown
 *
 * Revision 1.6  1994/02/01  17:41:26  markstro
 * Made the declaration of parameters dynamic -- no more MAXPARAMS
 *
 * Revision 1.5  1994/02/01  17:14:07  markstro
 * Made the declaration of variables dynamic -- no more MAXVARS
 *
 * Revision 1.4  1994/01/31  20:16:10  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#define DECLVAR_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

#define LONG 1
#define FLOAT 2
#define DOUBLE 3

/*--------------------------------------------------------------------*\
 | FUNCTION     : declvar_
 | COMMENT		: called from Fortran, sorts out args and calls declvar()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declvar_ (char *mname, char *vname, char *vdimen, ftnint *maxsizeptr,
	char *vtype, char *hstr, char *ustr, char *value, ftnlen mnamelen,
	ftnlen vnamelen, ftnlen vdimenlen, ftnlen vtypelen, ftnlen hlen, ftnlen ulen) {

  char *module, *name, *dimen, *type, *help, *units;
  long maxsize, retval;

  /*
   * copy maxsize to local long int
   */

  maxsize = *maxsizeptr;

  /*
   * copy args to new strings, and terminate correctly
   */

  module = (char *) umalloc((unsigned int)(mnamelen + 1));
  strncpy(module, mname, (int)mnamelen);
  module[mnamelen] = '\0';

  name = (char *) umalloc((unsigned int)(vnamelen + 1));
  strncpy(name, vname, (int)vnamelen);
  name[vnamelen] = '\0';

  dimen = (char *) umalloc((unsigned int)(vdimenlen + 1));
  strncpy(dimen, vdimen, (int)vdimenlen);
  dimen[vdimenlen] = '\0';

  type = (char *) umalloc((unsigned int)(vtypelen + 1));
  strncpy(type, vtype, (int)vtypelen);
  type[vtypelen] = '\0';

  help = (char *) umalloc((unsigned int)(hlen + 1));
  strncpy(help, hstr, (int)hlen);
  help[hlen] = '\0';

  units = (char *) umalloc((unsigned int)(ulen + 1));
  strncpy(units, ustr, (int)ulen);
  units[ulen] = '\0';

  /*
   * call C version of declvar()
   */

  retval = declvar(module, name, dimen, maxsize, type, help, units, value);

  /*
   * free up allocated strings
   */

//ufree(module);
//ufree(name);
//ufree(dimen);
//ufree(type);
//ufree(help);
//ufree(units);

  return(retval);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declvar()
 | COMMENT		: is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declvar (char *module, char *name, char *dimen, long maxsize, char *type,
	char *help, char *units, char *value) {
  int var_type;

  char *vkey;
  char *token;
  char *tmpdimen;
  long i, size;

  PUBVAR **vars, *var;
  //MODULE_DATA *mod_data;

  /*
   * realloc if too large
   */

  if(Mnvars >= max_vars -1) {
	max_vars += 100;
  	Mvarbase = (PUBVAR **)urealloc ((char *)Mvarbase,
		max_vars * sizeof(PUBVAR *));
  }

  /*
   * compute the key
   */

  vkey = strdup (name);
/*
  vkey = (char *)umalloc (strlen(name));
  (void)strcpy(vkey, module);
  strcat(strcat(vkey, "."), name);
*/

  if (var_addr(vkey) != NULL) {
	  if (print_mode) {
	      return(0);
	  } else {
              fprintf(stderr,
	      "ERROR - declvar - key '%s' already exists.\n", vkey);
              return(1); }
  }

// Not sure why this stuff is needed, so I commented it out.

	//mod_data = getmodule(module);
	//ADD_to_list (mod_data->vars, vkey);

  /*
   * convert fortran types to C equivalents
   */

  var_type = M_LONG;
  if (!strcmp(type, "real") || !strcmp(type, "float"))
    var_type = M_FLOAT;
  else if (!strcmp(type, "double precision") || !strcmp(type, "double"))
    var_type = M_DOUBLE;

  /*
   * check that type is possible
   */

	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE))
		{
    	(void)fprintf(stderr,
	    	"ERROR - declvar - type '%s' is illegal.\n", type);
    	(void)fprintf(stderr, "Key is '%s'.\n", vkey);
    	(void)fprintf(stderr, "Type is '%s'.\n", type);
    	return(1);
  		}

  /*  
   * get vars from Mvarbase, the global pointer
   */


  if (Mdebuglevel >= M_FULLDEBUG) {
    (void)fprintf(stderr, "Declaring variable '%s'\n", vkey);
  }

  /*
   * allocate space for a structure, and store pointer in vars
   */
  Mnvars += 1;

  vars = Mvarbase;

  var = (PUBVAR *) umalloc (sizeof(PUBVAR));
  vars[Mnvars-1] = var; /* copy address into vars array */

  /*
   * determine dimensions
   */

  tmpdimen = strdup (dimen);

  var->ndimen = 0;

  token = strtok (tmpdimen, ",");

  while (token != (char *) NULL) {
    var->ndimen++;
    token = strtok((char *) NULL, ",");
  }

  if (var->ndimen > MAX_NDIMEN) {

    (void)fprintf(stderr, "ERROR - declvar\n");
    (void)fprintf(stderr, "Attempt to use %ld dimensions - this is too many.\n",
	    var->ndimen);
    (void)fprintf(stderr, "Max number of dimensions allowed : %d.\n", MAX_NDIMEN);
    (void)fprintf(stderr, "Key is '%s'.\n", vkey);
    return(1);

  }

  var->dimen = (DIMEN **)umalloc (var->ndimen * sizeof(DIMEN *));

  (void)strcpy (tmpdimen, dimen);

  i = 0;
  token = strtok(tmpdimen, ",");

  while (token != (char *) NULL) {
    if (!(var->dimen[i] = dim_addr (token))) {
      (void)fprintf(stderr, "ERROR - declvar\n");
      (void)fprintf(stderr, "Variable '%s'\n", vkey);
      (void)fprintf(stderr, "Dimension '%s' is not declared.\n", token);
      return(1);
    }
    token = strtok ((char *) NULL, ",");
    i++;
  }

//ufree(tmpdimen);

  /*
   * get the size of the variable
   */
  
  size = 1;

  for (i = 0; i < var->ndimen; i++) {

    size *= var->dimen[i]->value;

  }

  var->size = size;

  if (size > maxsize) {

    (void)fprintf(stderr,
	    "ERROR - declvar - dimension exceeds space available.\n");
    (void)fprintf(stderr, "Key is '%s'.\n", vkey);
    (void)fprintf(stderr, "Size is %ld.\n", size);
    for (i = 0; i < var->ndimen; i++) {
      (void)fprintf (stderr, "Dimension '%s' is %ld.\n",
	      var->dimen[i]->name, var->dimen[i]->value);
    }
    (void)fprintf(stderr, "Space available is %ld.\n", maxsize);
    return(1);

  }

  /*
   * allocate space, and store variable properties
   */

  var->key = vkey;
  var->module = strdup (module);
  var->name = strdup (name);

  if(var_type == M_DOUBLE)
    var->type = M_DOUBLE;
	else 
		if (var_type == M_FLOAT)
    		var->type = M_FLOAT;
  			else 
				if (var_type == M_LONG) 
    				var->type = M_LONG;
  var->value = value;
  var->help = strdup (help);
  var->units = strdup (units);
  var->private = FALSE;

  sort_vars();

  return(0);
  
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declpri_
 | COMMENT		: called from Fortran, sorts out args and calls declpri()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declpri_ (char *vname, ftnint *maxsizeptr,
	char *vtype, char *value,
	ftnlen vnamelen, ftnlen vtypelen) {

  char *name, *type;
  long maxsize, retval;

  /*
   * copy maxsize to local long int
   */

  maxsize = *maxsizeptr;

  /*
   * copy args to new strings, and terminate correctly
   */

  name = (char *) umalloc((unsigned int)(vnamelen + 1));
  strncpy(name, vname, (int)vnamelen);
  name[vnamelen] = '\0';

  type = (char *) umalloc((unsigned int)(vtypelen + 1));
  strncpy(type, vtype, (int)vtypelen);
  type[vtypelen] = '\0';

  /*
   * call C version of declpri()
   */

  retval = declpri(name, maxsize, type, value);

  /*
   * free up allocated strings
   */

//ufree(name);
//ufree(type);

  return(retval);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declpri()
 | COMMENT		: is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declpri (char *name, long size, char *type, char *value) {
  int var_type;

  char *vkey;

  PUBVAR **vars, *var;

  /*
   * realloc if too large
   */

  if(Mnvars >= max_vars -1) {
	max_vars += 100;
  	Mvarbase = (PUBVAR **)urealloc ((char *)Mvarbase,
		max_vars * sizeof(PUBVAR *));
  }

  /*
   * compute the key
   */

  vkey = strdup (name);

  if (var_addr(vkey) != NULL) {
    (void)fprintf(stderr,
	    "ERROR - declvar - key '%s' already exists.\n", vkey);
    return(1);
  }

  /*
   * convert fortran types to C equivalents
   */

  var_type = M_LONG;
  if (!strcmp(type, "real") || !strcmp(type, "float"))
    var_type = M_FLOAT;
  else if (!strcmp(type, "double precision") || !strcmp(type, "double"))
    var_type = M_DOUBLE;

  /*
   * check that type is possible
   */

	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE))
		{
    	(void)fprintf(stderr,
	    	"ERROR - declvar - type '%s' is illegal.\n", type);
    	(void)fprintf(stderr, "Key is '%s'.\n", vkey);
    	(void)fprintf(stderr, "Type is '%s'.\n", type);
    	return(1);
  		}

  /*  
   * get vars from Mvarbase, the global pointer
   */


  if (Mdebuglevel >= M_FULLDEBUG) {
    (void)fprintf(stderr, "Declaring private variable '%s'\n", vkey);
  }

  /*
   * allocate space for a structure, and store pointer in vars
   */
  Mnvars += 1;

  vars = Mvarbase;

  var = (PUBVAR *) umalloc (sizeof(PUBVAR));
  vars[Mnvars-1] = var; /* copy address into vars array */

  /*
   * get the size of the variable
   */
  
  var->size = size;

  /*
   * allocate space, and store variable properties
   */

  var->key = vkey;
  var->module = NULL;
  var->name = strdup (name);

   if(var_type == M_DOUBLE) var->type = M_DOUBLE;
   else if (var_type == M_FLOAT) var->type = M_FLOAT;
   else if (var_type == M_LONG) var->type = M_LONG;

   var->value = value;
   var->help = NULL;
   var->units = NULL;
   var->private = TRUE;

   sort_vars();

   return(0);
}

