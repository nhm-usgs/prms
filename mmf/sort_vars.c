/**************************************************************************
 * sort_vars.c: sorts the pubvar array so that the key for each
 * structure is in increasing alphabetical order
 *
 * $Id: sort_vars.c 3058 2007-01-25 22:25:59Z rsregan $
 *
   $Revision: 3058 $
        $Log: sort_vars.c,v $
        Revision 1.5  1996/02/19 20:01:12  markstro
        Now lints pretty clean

        Revision 1.4  1994/11/10 23:26:51  markstro
        (1)  Some memory fixes -- results of malloc_dbg.
        (2)  More stuff removed from set menu.

 * Revision 1.3  1994/09/30  14:55:21  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.2  1994/01/31  20:17:34  markstro
 * Make sure that all source files have CVS log.
 *
 **************************************************************************/
#ifdef MALLOC_FUNC_CHECK
#include <malloc_dbg.h>
#endif

#define SORT_VARS_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : sort_vars
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void sort_vars (void) {

  PUBVAR **vars;
  PUBVAR *tmpvar;
  int i, j;


  /*
   * get vars from varbase, the global pointer
   */

  vars =  Mvarbase;

  for (i = Mnvars-2; i >= 0; i--) {

    for (j =  0; j <= i; j++) {

      if(strcmp(vars[j]->key,vars[j+1]->key) > 0) {

	tmpvar = vars[j];
	vars[j] = vars[j+1];
	vars[j+1] = tmpvar;

      }

    }

  }
/*
  printf("sort_vars\n");
  for (i = 0; i < Mnvars; i++) {
          printf("I: %ld %s\n",i,vars[i]->key);
      }
*/

}

