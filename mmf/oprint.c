/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : oprint.c
 * AUTHOR   : Mike Dixon CADSWES CU
 * DATE     : August 1990
 * FUNCTION :
 * COMMENT  : The following is a series of utility routines for printing
 *             to the output file from either Fortran or C modules.
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: oprint.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: oprint.c,v $
        Revision 1.4  1996/02/19 20:00:29  markstro
        Now lints pretty clean

        Revision 1.3  1994/10/24 14:18:44  markstro
        (1)  Integration of CADSWES's work on GIS.
        (2)  Prototypes were added to the files referenced in "mms_proto.h".

 * Revision 1.2  1994/01/31  20:16:59  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define OPRINT_C
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : opstr_
 | COMMENT		: opstr: print string
 |                 opstr_ is called from Fortran as 'call opstr(string)'
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void opstr_ (char *str, ftnlen stringlen) {

  char *string;

  /*
   * return if file pointer is NULL
   */

  if (Moutfile == NULL)
    return;

  /*
   * copy string to new string
   */

  string = (char *) umalloc(stringlen + 1);
  strncpy(string, str, stringlen);
  string[stringlen] = '\0';

  (void)fprintf(Moutfile, "%s\n", string);

//ufree(string);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : opstr
 | COMMENT		: opstr: print string
 |                 opstr is called from C as 'opstr(string)
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void opstr (char *string) {

  /*
   * return if file pointer is NULL
   */

  if (Moutfile == NULL)
    return;

  (void)fprintf(Moutfile, "%s\n", string);

}


///*--------------------------------------------------------------------*\
// | FUNCTION     : opint4_
// | COMMENT		: opint4_ : print integer from Fortran
// |                 The fortran call is:
// |                   call opint4(string, array, n)
// | PARAMETERS   : 'string' is a string,
// |                'array' is the INTEGER*4 of long array or scalar to
// |                    be printed
// |                'n' is the number of values in the array, 1 if a scalar.
// | RETURN VALUE : void
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void opint4_ (char *str, ftnint *array, ftnint *n, ftnlen stringlen) {
//
//  char *string;
//  int i;
//
//  /*
//   * return if file pointer is NULL
//   */
//
//  if (Moutfile == NULL)
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
//  (void)fprintf(Moutfile, "%s ",string);
//
//  for (i=0; i < *n; i++)
//    (void)fprintf(Moutfile, " %d",array[i]);
///*
//    (void)fprintf(Moutfile, " %ld",array[i]);
//*/
//
//  (void)fprintf(Moutfile, "\n");
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : oplong
// | COMMENT		: print long from C
// |                  The C call is
// |                      oplong(string, array, n)
// | PARAMETERS   : 'string' is a string
// |                'array' is the INTEGER*4 of long array or scalar
// |                  to be printed
// |                'n' is the number of values in the array, 1 if a scalar.
// | RETURN VALUE : void
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void oplong (char *string, long *array, long n) {
//
//  int i;
//
//  /*
//   * return if file pointer is NULL
//   */
//
//  if (Moutfile == NULL)
//    return;
//
//  (void)fprintf(Moutfile, "%s ",string);
//
//  for (i=0; i < n; i++)
//    (void)fprintf(Moutfile, " %ld", array[i]);
//
//  (void)fprintf(Moutfile, "\n");
//
//}
//
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : opreal_
// | COMMENT		: print real array from Fortran
// |                 The fortran call is:
// |                   call opreal(string, array, n)
// | PARAMETERS   : 'string' is a string
// |                'array' is the REAL or float array or scalar to be printed
// |                'n' is the number of values in the array, 1 if a scalar.
// | RETURN VALUE : void
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void opreal_ (char *str, float *array, ftnint *n, ftnlen stringlen) {
//
//  char *string;
//  int i;
//
//  /*
//   * return if file pointer is NULL
//   */
//
//  if (Moutfile == NULL)
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
//  (void)fprintf(Moutfile, "%s ",string);
//
//  for (i=0; i < *n; i++)
//    (void)fprintf(Moutfile, " %10g", array[i]);
//
//  (void)fprintf(Moutfile, "\n");
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : opfloat
// | COMMENT		: print float array from C
// |                The C call is:
// |                   opfloat(string, array, n)
// | PARAMETERS   : 'string' is a string
// |                'array' is the REAL or float array or scalar to be printed
// |                'n' is the number of values in the array, 1 if a scalar
// | RETURN VALUE : void
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void opfloat (char *string, float *array, long n) {
//
//  int i;
//
//  /*
//   * return if file pointer is NULL
//   */
//
//  if (Moutfile == NULL)
//    return;
//
//  (void)fprintf(Moutfile, "%s ",string);
//
//  for (i=0; i < n; i++)
//    (void)fprintf(Moutfile, " %10g", array[i]);
//
//  (void)fprintf(Moutfile, "\n");
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : opdble_
// | COMMENT		: print double precision array from Fortran
// |                 The fortran call is:
// |                    call opdble(string, array, n)
// | PARAMETERS   : 'string' is a string
// |                'array' is the double precision array or scalar to be printed
// |                'n' is the number of values in the array, 1 if a scalar
// | RETURN VALUE : void
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void opdble_ (char *str, double *array, ftnint *n, ftnlen stringlen) {
//
//  char *string;
//  int i;
//
//  /*
//   * return if file pointer is NULL
//   */
//
//  if (Moutfile == NULL)
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
//  (void)fprintf(Moutfile, "%s ",string);
//
//  for (i=0; i < *n; i++)
//    (void)fprintf(Moutfile, " %10lg", array[i]);
//
//  (void)fprintf(Moutfile, "\n");
//
//}
//
///*--------------------------------------------------------------------*\
// | FUNCTION     : opdble
// | COMMENT		: print double array from C
// |                  The C call is:
// |                    opdble(string, array, n)
// | PARAMETERS   : 'string' is a string
// |                'array' is the double precision array or scalar to be printed
// |                'n' is the number of values in the array, 1 if a scalar
// | RETURN VALUE : void
// | RESTRICTIONS :
//\*--------------------------------------------------------------------*/
//void opdble (char *string, double *array, long n) {
//
//  int i;
//
//  /*
//   * return if file pointer is NULL
//   */
//
//  if (Moutfile == NULL)
//    return;
//
//  (void)fprintf(Moutfile, "%s ",string);
//
//  for (i=0; i < n; i++)
//    (void)fprintf(Moutfile, " %10lg", array[i]);
//
//  (void)fprintf(Moutfile, "\n");
//
//}
//
///**7****************** LOCAL FUNCTION DEFINITIONS *********************/
//
///**8************************** TEST DRIVER ****************************/
//
