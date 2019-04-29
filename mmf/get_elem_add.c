/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : get_elem_add.c
 * AUTHOR   : Programmer: Pedro J. Restrepo
 *              University of Colorado, CADSWES, June, 1992
 * DATE     : Jun 1992
 * FUNCTION :
 * COMMENT  : This file contains utility routines for multiple index arrays.
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: get_elem_add.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: get_elem_add.c,v $
        Revision 1.12  1996/02/19 20:00:01  markstro
        Now lints pretty clean

        Revision 1.11  1994/11/22 17:19:36  markstro
        (1) Cleaned up dimensions and parameters.
        (2) Some changes due to use of malloc_dbg.

 * Revision 1.10  1994/11/08  16:17:28  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.9  1994/10/24  14:18:27  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.8  1994/09/30  14:54:18  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.7  1994/08/02  17:46:31  markstro
 * Split data file capabilities
 *
 * Revision 1.6  1994/07/07  14:23:55  markstro
 * DG fixes
 *
 * Revision 1.5  1994/06/21  20:20:26  markstro
 * More work on taking the module name out of the DB keyword.
 *
 * Revision 1.4  1994/05/13  15:37:38  markstro
 * Changes from TERRA
 *
 * Revision 1.3  1994/05/11  14:29:31  markstro
 * Changes from TERRA
 *
 * Revision 1.2  1994/01/31  20:16:24  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define GET_ELEM_ADD_C
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : CheckIndices
 | COMMENT		: Verifies that the number of indices
 |                 passed as an argument is compatible with the indices
 |                 declared for a parameter or a variable.
 | PARAMETERS   :
 |     
 |      key:        is the name of the parameter or variable
 |      elemString: is the string that contains the elements
 |                  separated by commas
 |      type:       = M_PARAMETER for parameters, 
 |                  = M_VARIABLE for variables.
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int CheckIndices (char *key, char *elemString, int type) {
	PARAM	*paddr;
	PUBVAR	*vaddr;
	DIMEN	*dim;
	int      nd;
	char	tsave[80], *temp, *t, *ptr;
	char	**strindx;
	DIMEN	**dimname;
	int		*intindx;
	int		nindex;
	int		i, k, nk;
	int		list_size, list_count;

/*
**	Get the address of the array.
*/
	if (type == M_PARAMETER) {
	    if (!(paddr = param_addr (key)))
   		   	return (1);

		nd = paddr->ndimen;
	    dimname = paddr->dimen;

  	} else {
		if (!(vaddr = var_addr (key)))
			return (1);

		nd = vaddr->ndimen;
		dimname = vaddr->dimen;
	}

/*
**	Parse the index string. First make a local copy
*/
	(void)strcpy (tsave, elemString);

/*
**	Check for '(' and ')', and delete them
*/
	t = strchr (tsave, '(');
	if (t) temp = t;
	else temp = tsave;
  
	t = strchr (temp, ')');
	if (t) *t = '\0';
/*
**	parse the string, build up a list of the multidimensional indices.
*/
	t = temp;
	nindex = 0;

	list_size = 100;
	list_count = 0;
	strindx = (char **)malloc (list_size * sizeof (char *));

	while (t) {
		if (list_count >= list_size) {
			list_size += 100;
			strindx = (char **)realloc (strindx, list_size * sizeof (char *));
		}
		ptr = strchr (t, ',');

		if (ptr)
			*ptr = '\0';

		strindx[list_count] = (char *)malloc (20 * sizeof (char));
		(void)strcpy(strindx[list_count], t);
		list_count++;

		if (ptr)
			t = ptr + 1;
		else
			t = NULL;
	} 

	nindex = list_count;
/*
**	compare number of indices
*/
	if (nd != nindex){
		return (2);
	}
                                /* check if indices are numeric values */
/*
**	ANSI-CHANGE
**  bad assignment type: long * = int *
**	changed declreation from long * to int *
*/
	intindx = (int *)calloc (nindex, sizeof (int));
  
	for (i = 0; i < nindex; i++) {
/*
**	check if all digits are numeric
*/
		t = strindx[i];
		k = 0;
		nk = strlen(strindx[i]);
    
		while (isdigit (t[k]) && (k < nk-1 )) k++;
    
		if (!isdigit (t[k]))
			return(3);
   
		intindx[i] = atoi (strindx[i]);

		if (intindx[i] < 1)
			return(4);

/*
**	get address of ith dimension
*/
		dim = dimname[i];
    
		if (intindx[i] > (int)dim->value)
			return(5);
	}

//      for (k = 0; k < nindex; k++) free((char *)strindx[k]);
//      free((char *)strindx);
//      free((char *)intindx);
	return(0);
}
  
/*--------------------------------------------------------------------*\
 | FUNCTION     : GetElemAddress
 | COMMENT		: This function returns a pointer to the memory location
 |    corresponding to elemString. The format of the string should each
 |    such that each component is separated by a comma. For example:
 |
 |    "3" for single-element arrays, or
 |    "2,4" for two-element arrays, or
 |    "4,1,3" for three-element arrays.
 |
 | PARAMETERS   :
 |      key:        is the name of the parameter or variable
 |      elemString: is the string that contains the elements
 |                  separated by commas
 |      type:       = M_PARAMETER for parameters, 
 |                  = M_VARIABLE for variables.
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *GetElemAddress (char *key, char *elemString, int type) {
	PARAM	*paddr;
	PUBVAR	*vaddr;
	DIMEN	*dim;
	char	*addr;
	char	*temp, *t, tkey[80], elmstr[80], *ptr;
	char	**strindx;
	DIMEN	**dimname;
	long     offset;
	long     prod;
	int      nindex;
	int      vtype;
	int      i;
	int		list_size, list_count;

/*
**	first, make a temporary copy of the key
*/
	(void)strcpy (tkey, key);

/*
**	strips leading blanks
*/
	ptr = tkey;
	while (*ptr == ' ')
		ptr++;
/*
**	strips trailing blanks
*/
	if ((temp = strchr (ptr, ' ')))
		*temp = '\0';
	if ((temp = strchr (ptr, '.')))
		*temp = '\0';

	if (type == M_PARAMETER) {
		paddr = param_addr (ptr);
/*
**	check number of dimensions
*/
		vtype = paddr->type;
		dimname = paddr->dimen;
		addr = paddr->value;
	} else {
		vaddr = var_addr(tkey);
/*
**	check number of dimensions
*/
		if (vaddr) {
			dimname = vaddr->dimen;
			vtype = vaddr->type;
			addr = vaddr->value;
		} else {
			(void)fprintf (stderr,"GetElemAddress: %s does not exist\n", ptr);
			return (NULL);
		}
	}

/*
**	parse the string. First make a local copy
*/
	(void)strcpy (elmstr, elemString);
	temp = elmstr; 

/*
**	check for '(' and ')', and delete them
*/
	t = strchr (temp, '(');
	if (t) temp = t;
  
	t = strchr (temp, ')');
	if (t) *t = '\0';
/*
**	parse the string
*/
	t = temp;
	nindex = 0;

	list_size = 100;
	list_count = 0;
	strindx = (char **)malloc (list_size * sizeof (char *));

	while (t) {
		if (list_count >= list_size) {
			list_size += 100;
			strindx = (char **)realloc (strindx, list_size * sizeof (char *));
		}
		ptr = strchr (t, ',');

		if (ptr)
			*ptr = '\0';

		strindx[list_count] = (char *)malloc (20 * sizeof (char));

		(void)strcpy (strindx[list_count], t);
		list_count++;

		if (ptr)
			t = ptr + 1;
		else
			t = NULL;
	} 

	nindex = list_count;
	offset = 0;
	prod   = 1;

	for (i = 0; i < nindex; i++) {
		dim = dimname[i];
		offset += (atol(strindx[i]) - 1) * prod;

		switch (type) {
			case M_PARAMETER:
				prod *= dim->value;
				break;

			case M_VARIABLE:
/*rsr changed next line */
/*				prod *= dim->max; */
				prod *= dim->value;
				break;
		}
	}

	switch (vtype) {
		case M_LONG:
			return (addr += offset * sizeof(long));
/*NOTREACHED*/
			break;

		case M_FLOAT:
			return(addr += offset * sizeof(float));
/*NOTREACHED*/
			break;

		case M_DOUBLE:
			return(addr += offset * sizeof(double));
/*NOTREACHED*/
			break;
	}
    return NULL;
}
/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

