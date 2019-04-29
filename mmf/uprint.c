/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : uprint.c
 * AUTHOR   : Adapted from oprint, written by Mike Dixon CADSWES CU
 *              Pedro J. Restrepo, CADSWES, CU, April, 1992
 * DATE     : August 1990
 * FUNCTION :
 * COMMENT  : The following is a series of utility routines for printing
 *              to the output file from either Fortran or C modules.
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: uprint.c 6387 2012-02-10 20:44:24Z markstro $
 *
   $Revision: 6387 $
        $Log: uprint.c,v $
        Revision 1.10  1996/04/29 16:23:26  markstro
        Unknown

 * Revision 1.9  1996/02/19  20:01:22  markstro
 * Now lints pretty clean
 *
        Revision 1.8  1995/11/24 14:35:54  markstro
        Initial Purify work.
        This is the version for Watershed Systems Modeling class 11/27 - 12/1, 1995

 * Revision 1.7  1995/05/12  15:18:47  markstro
 * Unknown
 *
 * Revision 1.6  1994/11/22  17:20:39  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.5  1994/10/24  14:19:07  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.4  1994/09/30  14:55:33  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.3  1994/05/18  17:16:12  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.2  1994/01/31  20:17:50  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
////#define UPRINT_C
////#include <string.h>
////#include <stdlib.h>
////#include <stdio.h>
////#include "mms.h"
////
/////**2************************* LOCAL MACROS ****************************/
////
/////**3************************ LOCAL TYPEDEFS ***************************/
////
/////**4***************** DECLARATION LOCAL FUNCTIONS *********************/
////
/////**5*********************** LOCAL VARIABLES ***************************/
////
/////**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : GetUserFile
//// | COMMENT		:
//// | PARAMETERS   :
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////FILE *GetUserFile (char *name, long dimNo) {
////	DIMEN *dim;
////	//char pathname[512];
////	int i;
////
////	/*
////   * Find if dimension "name" has names defined
////   */
////
////	dim = dim_addr(name);
////
////	if (!dim)
////	{
////		(void)fprintf(stderr, "ERROR - GetUserFile, Can't find dimension named %s\n",name);
////		return (NULL);
////	}
////
////	if (!dim->names)
////	{
////		(void)fprintf(stderr, "ERROR - GetUserFile. Dimension %s has no named indices\n",name);
////		return (NULL);
////	}
////
////	/*
////   * If so, find if the file indexed dimNo is opened. If it isn't, open it.
////   */
////
////	if (!dim->files)
////	{
////		dim->files = (FILE **)calloc(dim->value,sizeof(FILE *));
////
////		/*
////       * initalize all pointers to NULL
////       */
////
////		for (i = 0; i < dim->value; i++)
////			dim->files[i] = NULL;
////	}
////
////	//if (!dim->files[dimNo-1] && MuserFiles)
////	//{
////	//	/*
//// //      * get user output directory from environment
//// //      */
////
//// //     (void)sprintf (pathname, "%s%s", *control_svar("stats_output_file"), dim->names[dimNo-1]);
//// //     dim->files[dimNo-1] = fopen(pathname,"w");
////	//}
////
////	/* 
////   * return file pointer
////   */
////
////	return(dim->files[dimNo-1]);
////
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : closeUserFiles
//// | COMMENT		:
//// | PARAMETERS   :
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void closeUserFiles (void) {
////
////	int		i, j;
////	DIMEN	*dp;
////
////	for (i = 0; i < dim_db->count; i++) {
////		dp = (DIMEN *)(dim_db->itm[i]);
////		if (dp->files) {
////			for (j = 0; j < dp->value; j++) {
////				if (dp->files[j]) {
////					fclose (dp->files[j]);
////				}
////			}
//////			free (dp->files);
////			dp->files = NULL;
////		}
////	}
////	MuserFiles = 0;
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : upstr_
//// | COMMENT		: called from Fortran as 'call upstr(dimname, dimNo, string)'
//// | PARAMETERS   :
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void upstr_ (char *dimname, ftnint *dimNo, char *str, ftnlen dimlen, ftnlen stringlen) {
////
////	char *string;
////	char *name;
////	FILE *UserFile;
////
////	/*
////   * return if file pointer is NULL
////   */
////
////	name = (char *) umalloc(dimlen + 1);
////	strncpy(name, dimname, dimlen);
////	name[dimlen] = '\0';
////
////	UserFile = GetUserFile(name,*dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	/*
////   * copy string to new string
////   */
////
////	string = (char *) umalloc(stringlen + 1);
////	strncpy(string, str, stringlen);
////	string[stringlen] = '\0';
////
////	(void)fprintf(UserFile, "%s\n", string);
////
//////	ufree(string);
////}
////
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : upstr
//// | COMMENT		: upstr is called from C as 'upstr(dimname, dimNo, string)
//// | PARAMETERS   :
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void upstr (char *dimname, long dimNo, char *string) {
////
////	FILE *UserFile;
////	/*
////   * return if file pointer is NULL
////   */
////
////	UserFile = GetUserFile(dimname,dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	(void)fprintf(UserFile, "%s\n", string);
////
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : upint4_
//// | COMMENT		: print integer from Fortran
//// | PARAMETERS   : 'string' is a string
//// |                'array' is the INTEGER*4 of long array or scalar to be printed
//// |                'n' is the number of values in the array, 1 if a scalar
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void upint4_ (char *dimname, ftnint *dimNo, char *str, ftnint *array, ftnint *n,
////ftnlen dimlen, ftnlen stringlen) {
////
////	FILE *UserFile;
////
////	char *string;
////	char * name;
////	int i;
////
////	name = (char *) umalloc(dimlen + 1);
////	strncpy(name, dimname, dimlen);
////	name[dimlen] = '\0';
////
////	/*
////   * return if file pointer is NULL
////   */
////
////	UserFile = GetUserFile(name,*dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	/*
////   * copy string to new string
////   */
////
////	string = (char *) umalloc(stringlen + 1);
////	strncpy(string, str, stringlen);
////	string[stringlen] = '\0';
////
////	(void)fprintf(UserFile, "%s ",string);
////
////	for (i=0; i < *n; i++)
////		(void)fprintf(UserFile, " %d",array[i]);
//////		(void)fprintf(UserFile, " %ld",array[i]);
////
////	(void)fprintf(UserFile, "\n");
////
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : uplong
//// | COMMENT      : print long from C
//// | PARAMETERS   : 'string' is a string
//// |                'array' is the INTEGER*4 of long array or scalar to be printed
//// |                'n' is the number of values in the array, 1 if a scalar
//// | RETURN VALUE :
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void uplong (char *dimname, long dimNo, char *string, long *array, long n) {
////
////	int i;
////	FILE *UserFile;
////
////	/*
////   * return if file pointer is NULL
////   */
////
////	UserFile = GetUserFile(dimname,dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	(void)fprintf(UserFile, "%s ",string);
////
////	for (i=0; i < n; i++)
////		(void)fprintf(UserFile, " %ld", array[i]);
////
////	(void)fprintf(UserFile, "\n");
////
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : upreal_
//// | COMMENT		: print real array from Fortran
//// |                The fortran call is:
//// |                call upreal(string, array, n)
//// | PARAMETERS   : 'string' is a string
//// |                'array' is the REAL or float array or scalar to be printeD
//// |                'n' is the number of values in the array, 1 if a scalar.
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void upreal_ (char *dimname, ftnint *dimNo, char *str, float *array, ftnint *n,
////ftnlen dimlen, ftnlen stringlen) {
////
////	char *string;
////	char *name;
////	int i;
////	FILE *UserFile;
////
////	name = (char *) umalloc(dimlen + 1);
////	strncpy(name, dimname, dimlen);
////	name[dimlen] = '\0';
////
////	/*
////   * return if file pointer is NULL
////   */
////
////	UserFile = GetUserFile(name,*dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	/*
////   * copy string to new string
////   */
////
////	string = (char *) umalloc(stringlen + 1);
////	strncpy(string, str, stringlen);
////	string[stringlen] = '\0';
////
////	(void)fprintf(UserFile, "%s ",string);
////
////	for (i=0; i < *n; i++)
////		(void)fprintf(UserFile, " %10g", array[i]);
////
////	(void)fprintf(UserFile, "\n");
////
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : upfloat
//// | COMMENT		: print float array from C
//// |                The C call is:
//// |                upfloat(string, array, n)
//// | PARAMETERS   : 'string' is a string
//// |                'array' is the REAL or float array or scalar to be printeD
//// |                'n' is the number of values in the array, 1 if a scalar.
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void upfloat (char *dimname, long dimNo, char *string, float *array, long n) {
////
////	int i;
////	FILE *UserFile;
////
////	/*
////   * return if file pointer is NULL
////   */
////
////	UserFile = GetUserFile(dimname,dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	(void)fprintf(UserFile, "%s ",string);
////
////	for (i=0; i < n; i++)
////		(void)fprintf(UserFile, " %10g", array[i]);
////
////	(void)fprintf(UserFile, "\n");
////
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : updble_
//// | COMMENT		: print double precision array from Fortran
//// |                The fortran call is:
//// |                call updble(string, array, n)
//// | PARAMETERS   : 'string' is a string
//// |                'array' is the double precision array or scalar to be printed
//// |                'n' is the number of values in the array, 1 if a scalar
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void updble_ (char *dimname, ftnint *dimNo, char *str, double *array, ftnint *n, ftnlen dimlen, ftnlen stringlen) {
////
////	char *string;
////	char *name;
////	int i;
////	FILE *UserFile;
////
////	name = (char *) umalloc(dimlen + 1);
////	strncpy(name, dimname, dimlen);
////	name[dimlen] = '\0';
////
////	/*
////   * return if file pointer is NULL
////   */
////
////	UserFile = GetUserFile(name,*dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	/*
////   * copy string to new string
////   */
////
////	string = (char *) umalloc(stringlen + 1);
////	strncpy(string, str, stringlen);
////	string[stringlen] = '\0';
////
////	(void)fprintf(UserFile, "%s ",string);
////
////	for (i=0; i < *n; i++)
////		(void)fprintf(UserFile, " %10lg", array[i]);
////
////	(void)fprintf(UserFile, "\n");
////
////}
////
/////*--------------------------------------------------------------------*\
//// | FUNCTION     : updble
//// | COMMENT		: print double array from C
//// |                The C call is
//// |                call updble(string, array, n)
//// | PARAMETERS   : 'string' is a string
//// |                'array' is the double precision array or scalar to be printed
//// |                'n' is the number of values in the array, 1 if a scalar
//// | RETURN VALUE : 
//// | RESTRICTIONS :
////\*--------------------------------------------------------------------*/
////void updble (char *dimname, long dimNo, char *string, double *array, long n) {
////
////	int i;
////	FILE *UserFile;
////
////	/*
////   * return if file pointer is NULL
////   */
////
////	UserFile = GetUserFile(dimname,dimNo);
////
////	if (UserFile == NULL)
////		return;
////
////	(void)fprintf(UserFile, "%s ",string);
////
////	for (i=0; i < n; i++)
////		(void)fprintf(UserFile, " %10lg", array[i]);
////
////	(void)fprintf(UserFile, "\n");
////
////}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

