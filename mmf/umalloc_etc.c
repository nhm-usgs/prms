/**********************************************************************
 * umalloc_etc.c : memory allocation routines with error handling
 *
 * utility routine
 *
 * Mike Dixon CADSWES CU July 1990
 *
 * $Id: umalloc_etc.c 4627 2008-10-01 16:48:11Z markstro $
 *
 **********************************************************************/
#define UMALLOC_ETC_C
#include <stdlib.h>
#include <stdio.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : umalloc
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *umalloc (unsigned size) {
  char *ptr;

  if (!size)
    return (NULL);

  if ((ptr = (char *)malloc(size)) == NULL)
    if (size != 0) {
      (void)fprintf(stderr, "Cannot perform malloc, size = %d\n",size);
      exit(1);
    }
  return(ptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : urealloc
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *urealloc (char *ptr, unsigned size) {
  if (ptr == NULL) return(umalloc(size));
  if ((ptr = (char *)realloc(ptr, size)) == NULL)
    if (size != 0) {
      (void)fprintf(stderr, "Cannot perform realloc, size = %d\n",size);
      exit(1);
    }
  return(ptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ucalloc
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *ucalloc (unsigned num, unsigned size) {
  char *ptr;
  if ((ptr = (char *)calloc(num, size)) == NULL) 
    if ((size != 0) && (num != 0))
      (void)fprintf(stderr, "Cannot perform calloc, num, size = %d,%d\n",num,size);
      exit(1);
  return(ptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ufree
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void ufree (char *ptr) {
/*
   if (ptr != NULL) free(ptr);
*/
}

