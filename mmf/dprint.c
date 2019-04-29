/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : dprint.c
 * AUTHOR   : Mike Dixon CADSWES CU August 1990
 * DATE     : Thu 20 Oct 1994
 * FUNCTION : dprint
 * COMMENT  : The following is a series of utility routines for printing
 *  to stderr from either Fortran or C modules. If the current debug level
 *  (Mdebuglevel) equals or exceeds that passed in the call, the
 *  print is performed.
 * 'dlevel' is the debug level passed by the print call
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: dprint.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: dprint.c,v $
        Revision 1.5  1996/02/19 19:59:54  markstro
        Now lints pretty clean

        Revision 1.4  1994/11/22 17:19:30  markstro
        (1) Cleaned up dimensions and parameters.
        (2) Some changes due to use of malloc_dbg.

 * Revision 1.3  1994/10/24  14:18:21  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.2  1994/01/31  20:16:16  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/

//#define DPRINT_C
//#include <string.h>
//#include "mms.h"
//
///**2************************* LOCAL MACROS ****************************/
//
///**3************************ LOCAL TYPEDEFS ***************************/
//
///**4***************** DECLARATION LOCAL FUNCTIONS *********************/
//
///**5*********************** LOCAL VARIABLES ***************************/
//
///**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
///*--------------------------------------------------------------------*\
// | FUNCTION     : dpstr
// | COMMENT		: print string
// | dpstr_ is called from Fortran as 'call dpstr(string, dlevel)'
// | dpstr is called from C as 'dpstr(string, dlevel)
// | PARAMETERS   :
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void dpstr_ (char *str, ftnint *dlevel, ftnlen stringlen) {
//
//  char *string;
//
//  /*
//   * act only if the current debug level equals or exceeds
//   * that specified in the print
//   */
//
//  if(*dlevel > Mdebuglevel)
//    return;
//
//  /*
//   * copy string to new string
//   */
//
//  string = (char *) umalloc(stringlen + 1);
//  strncpy(string, str, stringlen);
//  string[stringlen] = '\0';
//
//  (void)fprintf(stderr, "%s\n", string);
//  
//  ufree(string);
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : dpstr
// | COMMENT		:
// | PARAMETERS   :
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void dpstr (char *string, long dlevel) {
//
//  if(dlevel > Mdebuglevel)
//    return;
//
//  (void)fprintf(stderr, "%s\n", string);
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : dpint4_
// | COMMENT		: The fortran call is:
// |     call dpint4(string, array, n, dlevel)
// | PARAMETERS   :
// |     where 'string' is a string,
// |           'array' is the INTEGER*4 of long array or scalar to be printed
// |           'n' is the number of values in the array, 1 if a scalar.
// |           'dlevel' is the debug level
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void dpint4_ (char *str, ftnint *array, ftnint *n, ftnint *dlevel, ftnlen stringlen) {
//
//  char *string;
//  int i;
//
//  if(*dlevel > Mdebuglevel)
//    return;
//
//  /*
//   * copy string to new string
//   */
//
//  string = (char *) umalloc(stringlen + 1);
//  strncpy(string, str, stringlen);
//  string[stringlen] = '\0';
//
//  (void)fprintf(stderr, "%s ",string);
//
//  for (i=0; i < *n; i++)
///*
//    (void)fprintf(stderr, " %ld",array[i]);
//*/
//    (void)fprintf(stderr, " %d",array[i]);
//
//  (void)fprintf(stderr, "\n");
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : dplong
// | COMMENT		: print long from C The C call is
// |     dplong(string, array, n, dlevel)
// | PARAMETERS   :where 'string' is a string,
// |           'array' is the INTEGER*4 of long array or scalar to be printed
// |           'n' is the number of values in the array, 1 if a scalar.
// |           'dlevel' is the debug level
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void dplong (char *string, long *array, long n, long dlevel) {
//
//  int i;
//
//  if(dlevel > Mdebuglevel)
//    return;
//
//  (void)fprintf(stderr, "%s ",string);
//
//  for (i=0; i < n; i++)
//    (void)fprintf(stderr, " %ld", array[i]);
//
//  (void)fprintf(stderr, "\n");
//
//}

/*--------------------------------------------------------------------*\
 | FUNCTION     : dpreal_
 | COMMENT		: print real array from Fortran
 | The fortran call is:
 |     call dpreal(string, array, n, dlevel)
 | PARAMETERS   : 'string' is a string,
 *           'array' is the REAL or float array or scalar to be printed
 *           'n' is the number of values in the array, 1 if a scalar.
 *           'dlevel' is the debug level
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
//void dpreal_ (char *str, float *array, ftnint *n, ftnint *dlevel, ftnlen stringlen) {
//  char *string;
//  int i;
//
//  if(*dlevel > Mdebuglevel)
//    return;
//
//  /*
//   * copy string to new string
//   */
//
//  string = (char *) umalloc(stringlen + 1);
//  strncpy(string, str, stringlen);
//  string[stringlen] = '\0';
//
//  (void)fprintf(stderr, "%s ",string);
//
//  for (i=0; i < *n; i++)
//    (void)fprintf(stderr, " %10g", array[i]);
//
//  (void)fprintf(stderr, "\n");
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : dpfloat
// | COMMENT		: print float array from C 
// |   The C call is:
// |     dpfloat(string, array, n, dlevel)
// | PARAMETERS   : where 'string' is a string,
// |           'array' is the REAL or float array or scalar to be printed
// |           'n' is the number of values in the array, 1 if a scalar.
// |           'dlevel' is the debug level
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void dpfloat (char *string, float *array, long n, long dlevel) {
//  int i;
//
//  if(dlevel > Mdebuglevel)
//    return;
//
//  (void)fprintf(stderr, "%s ",string);
//
//  for (i=0; i < n; i++)
//    (void)fprintf(stderr, " %10g", array[i]);
//
//  (void)fprintf(stderr, "\n");
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : dpdble_
// | COMMENT		: print double precision array from Fortran
// |  The fortran call is:
// |     call dpdble (string, array, n, dlevel)
// | PARAMETERS   : 'string' is a string,
// |           'array' is the double precision array or scalar to be printed
// |           'n' is the number of values in the array, 1 if a scalar.
// |           'dlevel' is the debug level
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void dpdble_ (char *str, double *array, ftnint *n, ftnint *dlevel, ftnlen stringlen) {
//
//  char *string;
//  int i;
//
//  if(*dlevel > Mdebuglevel)
//    return;
//
//  /*
//   * copy string to new string
//   */
//
//  string = (char *) umalloc(stringlen + 1);
//  strncpy(string, str, stringlen);
//  string[stringlen] = '\0';
//
//  (void)fprintf(stderr, "%s ",string);
//
//  for (i=0; i < *n; i++)
//    (void)fprintf(stderr, " %10lg", array[i]);
//
//  (void)fprintf(stderr, "\n");
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : dpdble
// | COMMENT		: print double array from C
// | The fortran call is:
// |     call dpdble(string, array, n, dlevel)
// | PARAMETERS   : 'string' is a string,
// |           'array' is the double precision array or scalar to be printed
// |           'n' is the number of values in the array, 1 if a scalar.
// |           'dlevel' is the debug level
// | RETURN VALUE : 
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void dpdble (char *string, double *array, long n, long dlevel) {
//
//  int i;
//
//  if(dlevel > Mdebuglevel)
//    return;
//
//  (void)fprintf(stderr, "%s ",string);
//
//  for (i=0; i < n; i++)
//    (void)fprintf(stderr, " %10lg", array[i]);
//
//  (void)fprintf(stderr, "\n");
//
//}
//
///**7****************** LOCAL FUNCTION DEFINITIONS *********************/
//
///**8************************** TEST DRIVER ****************************/
//
