/**************************************************************************
 * str_to_vals.c: decodes a string into values, and loads memory addresses
 *
 * Examples of legal strings for this routine:
 *
 *           "1 2 3 4 5"
 *           "1.0, 2.2, 19e9"
 *           "1*23.5, 7*1 13 12*3"
 *
 * Blanks, commas, tabs and newlines may delimit the values.
 * The repeat count is optional, but must be greater than 0 if included.
 * If the total number of entries is less than required, the sequence
 * is repeated.
 *
 * $Id: str_to_vals.c 5702 2010-07-28 20:42:46Z rsregan $
 *
   $Revision: 5702 $
        $Log: str_to_vals.c,v $
        Revision 1.7  1996/02/19 20:01:17  markstro
        Now lints pretty clean

        Revision 1.6  1994/11/23 20:12:59  markstro
        More malloc_dbg changes

 * Revision 1.5  1994/11/22  17:20:37  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.4  1994/11/08  16:17:51  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.3  1994/09/30  14:55:25  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:17:39  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#define STR_TO_VALS_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <stdlib.h>
#include "mms.h"

#define S2V_ERROR 1l
#define S2V_SUCCESS 0l

/*--------------------------------------------------------------------*\
 | FUNCTION     : str_to_vals
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long str_to_vals (char *encoded_string, long size, long type, char *store_addr) {
  long i, isource;
  long ndecoded, repeat;
  char *scopy, *token, *valstr, *asterisk, *end_point;
  char tcopy[MAXDATALNLEN];
  double dvalue, *dval;
  float fvalue, *fval;
  long lvalue, *lval;

  /*
   * set up pointer for data type
   */

  dval = NULL;
  fval = NULL;
  lval = NULL;
  switch (type) {
  case M_DOUBLE:
    dval = (double *) store_addr;
    break;
  case M_FLOAT:
    fval = (float *) store_addr;
    break;
  case M_LONG:
    lval = (long *) store_addr;
    break;
  }

  /*
   * copy encoded_string before tokenizing
   */

  scopy = strdup (encoded_string);

  token = strtok (scopy, " ,\t\n");

  ndecoded = 0;

  while (token != NULL) {

    (void)strcpy(tcopy, token);
    asterisk = strrchr(tcopy, '*'); /* search for '*' */

    if (asterisk == NULL ) {        /* no repeat count */

      valstr = tcopy;
      repeat = 1;

    } else {

      valstr = asterisk + 1;
      *asterisk = '\0';             /* terminate repeat count str */
      repeat = strtol(tcopy, &end_point, 10l);

      if (repeat <= 0 || *end_point != '\0') {
	(void)fprintf(stderr,
		"ERROR - str_to_vals - decoding string into values.\n");
	(void)fprintf(stderr, "Illegal repeat count.\n");
	return S2V_ERROR;
      }

    }

    /*
     * set errno to 0 so that previous errors are cancelled
     */

    errno = 0;

    dvalue = 0.0;
    fvalue = 0.0;
    lvalue = 0;
    switch (type) {

    case M_DOUBLE:
      dvalue = strtod(valstr, &end_point);
      break;
    case M_FLOAT:
      fvalue = (float) strtod(valstr, &end_point);
      break;
    case M_LONG:
      lvalue = strtol(valstr, &end_point, 10);
      break;
    }

    if (errno == EDOM) {
      (void)fprintf(stderr,
	      "ERROR - str_to_vals - decoding string into values.\n");
      (void)fprintf(stderr, "Illegal value.\n");
      return S2V_ERROR;
    }

    if (errno == ERANGE) {
      (void)fprintf(stderr,
	      "ERROR - str_to_vals - decoding string into values.\n");
      (void)fprintf(stderr, "Value out of range.\n");
      return S2V_ERROR;
    }

    if (ndecoded + repeat > size) {
      repeat = size - ndecoded;
    }

    switch (type) {

    case M_DOUBLE:
      for (i = 0; i < repeat; i++) {
	dval[ndecoded] = dvalue;
	ndecoded++;
      }
      break;

    case M_FLOAT:
      for (i = 0; i < repeat; i++) {
	fval[ndecoded] = fvalue;
	ndecoded++;
      }
      break;

    case M_LONG:
      for (i = 0; i < repeat; i++) {
	lval[ndecoded] = lvalue;
	ndecoded++;
      }
      break;
    }

    token = strtok(NULL, " ,\n\t");

  }

  /*
   * If too few elements decoded, repeat the sequence
   */

  if (ndecoded < size) {

    isource = 0;

    switch (type) {

    case M_DOUBLE:
      for (i = ndecoded; i < size; i++) {
	dval[i] = dval[isource];
	isource++;
	if (isource == ndecoded)
	  isource = 0;
      }
      break;

    case M_FLOAT:
      for (i = ndecoded; i < size; i++) {
	fval[i] = fval[isource];
	isource++;
	if (isource == ndecoded)
	  isource = 0;
      }
      break;

    case M_LONG:
      for (i = ndecoded; i < size; i++) {
	lval[i] = lval[isource];
	isource++;
	if (isource == ndecoded)
	  isource = 0;
      }
      break;
    }

  }

//ufree(scopy);
  return S2V_SUCCESS;

}
