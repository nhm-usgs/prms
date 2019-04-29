/**************************************************************************
 * getparam.c: gets the parameter associated with a module and name, and
 * copies it into the space provided by the calling routine.
 *
 * There are 2 functions: getparam() to be called from C
 *                        getparam_() to be called from Fortran
 *
 * Returns 0 if successful, 1 otherwise.
 *
 * $Id: getparam.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: getparam.c,v $
        Revision 1.10  1997/03/26 17:04:14  markstro
        Added function getdataname

        Revision 1.9  1996/12/05 21:24:12  markstro
        (1)  Added getoutname()
        (2)  Sensitivity work
        (3)  Optimization work

        Revision 1.8  1996/10/10 13:26:32  markstro
        (1) Work on Rosenbrock
        (2) Bug in fix dimension size

        Revision 1.7  1996/02/19 20:00:05  markstro
        Now lints pretty clean

        Revision 1.6  1995/05/25 14:26:30  markstro
        (1) Added batch mode
        (2) Replaced "b" functions with "mem" versions

 * Revision 1.5  1994/11/22  17:19:40  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.4  1994/09/30  14:54:24  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.3  1994/06/16  16:47:09  markstro
 * Worked over runcontrol.c
 *
 * Revision 1.2  1994/01/31  20:16:32  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#define GETPARAM_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static long paramcopy (PARAM *, double *, int);

/*--------------------------------------------------------------------*\
 | FUNCTION     : updateparam
 | COMMENT		: This function updates the local parameter value arrays
 |                in the modules with the current values in the parameter
 |                data structure. The local arrays are registered with the
 |                param structures by declaring them with the "declparam_u"
 |                function. This essentually implements a "listener"
 |                pattern, making each module a listener for new parameter
 |                values.
 | PARAMETERS   : name is the name of the parameter to update.
 | RETURN VALUE : integer error code
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long updateparam (char *name) {
	PARAM *param;
	int i;

	param = param_addr(name);

	if (param == NULL) {
		(void)fprintf(stderr, "updateparam: %s not found.\n", name);

	} else {
		for (i = 0; i < param->num_references; i++) {
			paramcopy (param, (double *)(param->references[i]), -1);
		}
	}

	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparam_
 | COMMENT		: called from Fortran, sorts out args and calls getparam()
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparam_ (char *mname, char *pname, ftnint *pmaxsize, char *ptype, double *pval,
	       ftnlen mnamelen, ftnlen pnamelen, ftnlen ptypelen) {

	char *module, *name, *type;
	int maxsize;
	long retval;

// copy maxsize to local long int
	maxsize = *pmaxsize;

// copy args to new strings, and terminate
	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

// call C version of getparam()
	retval = getparam(module, name, maxsize, type, pval);

//ufree(module);
//ufree(name);
//ufree(type);

	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparam
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparam (char *module, char *name, int maxsize, char *type, double *pval) {
	int var_type;
	PARAM *param;
	char *pkey;

	pkey = strdup (name);

// convert fortran types to C types
	var_type = M_LONG;
	if (!strcmp(type, "real") || !strcmp(type, "float")) {
		var_type = M_FLOAT;
	} else if (!strcmp(type, "double precision") || !strcmp(type, "double")) {
		var_type = M_DOUBLE;
	} else if (!strcmp (type, "string")) {
		var_type = M_STRING;
	}

// check that type is possible
	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE) && (var_type != M_STRING)) {
		(void)fprintf(stderr, "\nERROR: data type for parameter %s in module %s has inconsistent uses.\n", pkey, module);
		return(1);
	}

// get pointer to parameter with key
	param = param_addr(pkey);

	if (param == NULL) {
		(void)fprintf(stderr, "\nERROR: getting parameter %s in module %s, but parameter is not found.\n", pkey, module);
		return(1);
	}

//  Check to see if the parameter values were set in the Parameter File
	if (param->read_in == 0) {
		(void)fprintf(stderr,"\nWARNING: parameter %s is used by module %s but values are not\n", pkey, module);
		(void)fprintf(stderr,"         set in the Parameter File. Module default values are being used.\n");
	}

// check that there is enough space allocated in the calling routine
	if (param->size > maxsize) {
		(void)fprintf(stderr, "\nERROR: parameter %s declared array size is not big enough in module %s.\n", pkey, module);
		return(1);
	}

	return paramcopy (param, pval, maxsize);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : paramcopy
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static long paramcopy (PARAM *param, double *pval, int maxsize) {
	DIMEN *dim1, *dim2;
	char * ptr;
	int max1, max2, val1, val2, nrow;
	int i;
  /*
   * copy the parameter across
   */

	if (param->ndimen == 1) {
		switch (param->type) {

			case M_LONG:
				memcpy ((char *) pval, (char *) param->value, param->size * sizeof(int));
				break;

			case M_FLOAT:
				memcpy ((char *)pval, (char *)param->value, param->size * sizeof(float));
				break;

			case M_DOUBLE:
				memcpy ((char *)pval, (char *)param->value, param->size * sizeof(double));
				break;

			case M_STRING:  // DANGER fortran string size hardwired to 16 characters
 	  			for (i = 0; i < param->size; i++) {
					memcpy ((char *)pval+i, *((char **)param->value+i), 16 * sizeof(char *));
                }
				break;
		}
	} else {
      ptr = (char *) pval;

      dim1 = param->dimen[0];
 	  dim2 = param->dimen[1];

      val1 = dim1->value;
      max1  = dim1->max;

      val2 = dim2->value;
 	  max2 = dim2->max;

 	  if ((max1*max2 == val1*val2) == maxsize ) {
 		  if (max1 == val1 && max2 == val2 ) {
 			  nrow = val1;
 		  } else {
 			  nrow = val1;
 			  (void)fprintf(stderr, "paramcopy: DANGER. Mismatch in array sizes.\n");
			  (void)fprintf(stderr, "Key:   '%s'\n", param->name);
 		  }
 	  } else if (val1*val2 == maxsize) {
 		  nrow = val1;
 	  } else if (max1*max2 == maxsize) {
 		  nrow = max1;
 	  } else {
 		  nrow = val1;
 		  (void)fprintf(stderr, "paramcopy: DANGER 2. Mismatch in array sizes.\n");
		  (void)fprintf(stderr, "Key:   '%s'\n", param->name);
 	  }


 	  for (i = 0; i	< val2;	i++) {
 		  switch (param->type) {

 			case M_LONG:
 				memcpy (ptr, (char *)(param->value + i * val1*sizeof(int)), val1*sizeof(int));
 				ptr	+=	nrow * sizeof(int);
 				break;

 			case M_FLOAT:
 				memcpy (ptr, (char *) (param->value + i * val1*sizeof(float)), val1*sizeof(float));
 				ptr +=  nrow * sizeof(float);
 				break;

 			case M_DOUBLE:
 				memcpy (ptr, (char *) (param->value + i * val1*sizeof(double)), val1*sizeof(double));
 				ptr +=  nrow * sizeof(double);
 				break;

 			case M_STRING:
 				memcpy (ptr, (char *) (param->value + i * val1*sizeof(char *)), val1*sizeof(char *));
 				ptr +=  nrow * sizeof(char *);
 				break;

 		  }
 	  }
	}
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdatainfo_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdatainfo_ (char *dinfo, ftnlen len) {
	long retval;
	retval = getdatainfo (dinfo, len);
	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdatainfo
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdatainfo (char *dinfo, ftnlen len) {
	strncpy (dinfo, Mdatainfo, len);
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutname_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getoutname_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
  char *foo;
  long ret;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

	ret = getoutname (dinfo, foo);

    if (strlen (dinfo) >= (size_t)len) {
		printf ("getoutname:  path name is too long for your buffer!\n");
        ret = 1;
	}
    return(ret);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutname
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getoutname (char *dinfo, char *ext) {
	sprintf(dinfo, "%s\\%s", *control_svar("model_output_file"), ext);
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdataname_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdataname_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
	char *foo;
	long retval;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

	retval = getdataname (dinfo, foo);
	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdataname
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdataname (char *dinfo, char *ext) {
	sprintf(dinfo, "%s%s", *control_svar("data_file"), ext);
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutdirfile_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getoutdirfile_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
	char *foo;
	long retval;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

   retval = getoutdirfile (dinfo, foo);
   return(retval);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutdirfile
 | COMMENT      : called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getoutdirfile (char *dinfo, char *foo) {
   sprintf(dinfo, "%s%s", *control_svar("mms_user_out_dir"), foo);
   return(0);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getuserdirfile_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getuserdirfile_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
	char *foo;
	long retval;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

   retval = getuserdirfile (dinfo, foo);
   return(retval);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getuserdirfile
 | COMMENT      : called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getuserdirfile (char *dinfo, char *foo) {
   sprintf(dinfo, "%s%s", *control_svar("mms_user_dir"), foo);
   return (0);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getparamfile
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparamfile (char *dinfo) {
	sprintf(dinfo, "%s", *control_svar("param_file"));
	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparamfile_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparamfile_ (char *dinfo, ftnlen len) {
	long retval;
	retval = getparamfile (dinfo);
	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparamstring_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/

long getparamstring_ (char *mname, char *pname, ftnint *pmaxsize, char *ptype, ftnint *pindex, char *pstring,
	       ftnlen mnamelen, ftnlen pnamelen, ftnlen ptypelen, ftnlen pslen) {

  char *module, *name, *type;
  int maxsize;
  PARAM *param;

  /*
   * copy maxsize to local long int
   */

  maxsize = *pmaxsize;

  /*
   * copy args to new strings, and terminate
   */

  module = (char *) umalloc(mnamelen + 1);
  strncpy(module, mname, mnamelen);
  module[mnamelen] = '\0';

  name = (char *) umalloc(pnamelen + 1);
  strncpy(name, pname, pnamelen);
  name[pnamelen] = '\0';

  type = (char *) umalloc(ptypelen + 1);
  strncpy(type, ptype, ptypelen);
  type[ptypelen] = '\0';




  param = param_addr(name);

  if (param == NULL) {
    (void)fprintf(stderr,
		"\nERROR: - parameter %s is not found.\n", name);
//    (void)fprintf(stderr, "Key:   '%s'\n", name);
    return(1);
  }

  /*
  **  Check to see if the parameter values were set in the Parameter File
  */
  if (param->read_in == 0) {
		(void)fprintf(stderr,"\nWARNING: parameter %s is used by module %s but values are not\n", name, module);
		(void)fprintf(stderr,"         set in the Parameter File. Module default values are being used.\n");
//	  (void)fprintf(stderr,
//	    "getparamstring - parameter %s is used but values are not set in the Parameter File.  Module default values are being used.\n", name);
  }

//   strncpy (pstring, (char *)(param->value)[*pindex], 80);
   strncpy (pstring, *((char **)param->value + *pindex), pslen);

   return(0);

}
