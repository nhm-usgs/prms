/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modelling System (MMS)
 * NAME     : check_vars.c
 * AUTHOR   : Steve Markstrom (markstro)
 * DATE     : Thu 16 Dec 1993
 * FUNCTION :
 * COMMENT  :
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: check_vars.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        Revision 1.12  2006/11/27  rsregan
	change gis to ani

        $Log: check_vars.c,v $
        Revision 1.11  1997/11/13 17:13:28  markstro
        unknown

        Revision 1.10  1996/02/19 19:59:33  markstro
        Now lints pretty clean

        Revision 1.9  1994/12/15 19:12:30  markstro
        Changes for Christoph:  (1) More work on setting data file labels;
        and (2) Fixed problems with empty display variable lists.

 * Revision 1.8  1994/11/08  16:17:16  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.7  1994/10/24  14:18:09  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.6  1994/09/30  14:53:52  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.5  1994/07/07  14:23:53  markstro
 * DG fixes
 *
 * Revision 1.4  1994/06/21  20:20:20  markstro
 * More work on taking the module name out of the DB keyword.
 *
 * Revision 1.3  1994/05/11  14:29:30  markstro
 * Changes from TERRA
 *
 * Revision 1.2  1994/01/31  20:15:57  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define CHECK_VARS_C
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_stat_vars
 | COMMENT		: Makes sure that the selected statistic variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE : 0 - no bad ones are found
 |              : 1 - at least one bad one found
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *CHECK_stat_vars (void) {

	static char err_message[256];

	int		i, status = 0;
	char	**names, **elements, buf[80], *ptr;

	names = (char **) control_var("statVar_names");
	elements = (char **) control_var("statVar_element");

	for (i = 0; i < *((long *)control_var ("nstatVars")); i++) {
		(void)strcpy (buf, names[i]);
		ptr = strchr (buf, '.');
		if (ptr) *ptr = '\0';

		if (CheckIndices (buf, elements[i], M_VARIABLE)) {
			(void)fprintf (stderr, "ERROR - CHECK_stat_vars: %s[%s] is not a valid stat variable.\n", names[i], elements[i]);
			(void)sprintf (err_message, "Set stat variables: %s[%s] is not a valid stat variable.\n", names[i], elements[i]);
			status = 1;
		}
	}

	if (status)
		return (err_message);
	else
		return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_disp_vars
 | COMMENT		: Makes sure that the selected display variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE : error message
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char * CHECK_disp_vars (void) {
       static char	err_message[256];
	int		status = 0;
	int		i, j;
	char	buf[MAXDATALNLEN], buf0[MAXDATALNLEN], buf1[MAXDATALNLEN], buf2[MAXDATALNLEN];
	char	*dv_name, *dv_index, *ptr;

	for (i = 0; i < *(control_lvar ("ndispGraphs")); i++) {
		(void)sprintf (buf0, "ndispVars%d", i);
		(void)sprintf (buf1, "dispVar_names%d", i);
		(void)sprintf (buf2, "dispVar_element%d", i);

		for (j = 0; j < *(control_lvar (buf0)); j++) {
			dv_name = *((char **)(control_sarray (buf1, j)));
			(void)strcpy (buf, dv_name);
			if ( (ptr = strchr (buf, '.')) )
				*ptr = '\0';
			dv_index = *((char **)(control_sarray (buf2, j)));
			if ((CheckIndices (buf, dv_index, M_VARIABLE))) {
				(void)fprintf (stderr, "ERROR - CHECK_disp_vars: %s[%s] from graph %d is not a valid display variable.\n", buf, dv_index, i+1);
				(void)sprintf (err_message, "Set display variables: %s[%s] from graph %d is not a valid display variable.\n", buf, dv_index, i+1);
				status = 1;
			}
		}
	}

	if (status)
		return (err_message);
	else
		return (NULL);
}


/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_ani_vars
 | COMMENT      : Makes sure that the selected ani variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *CHECK_ani_vars (void) {
    int     i, status = 0;
    char    **names, buf[80], *ptr;
    PUBVAR  *vaddr;
    static char err_message[256];

    names = (char **) control_var ("aniOutVar_names");

    for (i = 0; i < *((long *)control_var ("naniOutVars")); i++) {
        (void)strcpy (buf, names[i]);
        ptr = strchr (buf, '.');
        if (ptr) *ptr = '\0';

        if (!(vaddr = var_addr (buf))) {
           fprintf (stderr, "ERROR - CHECK_ani_vars: %s is not a valid animation output variable.\n", names[i]);
           sprintf (err_message, "Set animation variables: %s is not a valid stat variable.\n", names[i]);
           status = 1;
        }
    }

    if (status)
        return (err_message);
    else
        return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_map_vars
 | COMMENT      : Makes sure that the selected map variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *CHECK_map_vars (void) {
    int     i, status = 0;
    char    **names, buf[80], *ptr;
    PUBVAR  *vaddr;
    static char err_message[256];

    names = (char **) control_var ("mapOutVar_names");

    for (i = 0; i < *((long *)control_var ("nmapOutVars")); i++) {
        (void)strcpy (buf, names[i]);
        ptr = strchr (buf, '.');
        if (ptr) *ptr = '\0';

        if (!(vaddr = var_addr (buf))) {
           fprintf (stderr, "ERROR - CHECK_map_vars: %s is not a valid map output variable.\n", names[i]);
           sprintf (err_message, "Set map variables: %s is not a valid stat variable.\n", names[i]);
           status = 1;
        }
    }

    if (status)
        return (err_message);
    else
        return (NULL);
}
/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

