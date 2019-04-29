/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modelling System (MMS)
 * NAME     : read_datainfo.c
 * AUTHOR   : CADSWES; modified by Steve Markstrom (markstro)
 * DATE     : Wed 09 Mar 1994
 * FUNCTION :
 * COMMENT  : read_datainfo.c: reads the data file and updates the
 *                datainfo string and the data variable names and
 *                sizes
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: read_datainfo.c 5737 2010-09-30 23:36:13Z rsregan $
 *
   $Revision: 5737 $
        $Log: read_datainfo.c,v $
        Revision 1.15  2000/03/07 20:35:18  markstro
        Added comments to data file header

        Revision 1.14  1996/02/19 20:00:41  markstro
        Now lints pretty clean

        Revision 1.13  1995/03/20 22:44:40  markstro
        DG changes

 * Revision 1.12  1994/11/22  17:20:10  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.11  1994/11/08  16:17:37  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.10  1994/10/24  14:18:50  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.9  1994/09/30  14:54:55  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.8  1994/08/02  17:46:36  markstro
 * Split data file capabilities
 *
 * Revision 1.7  1994/05/18  17:15:55  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.6  1994/03/11  21:16:38  markstro
 * Got rid of client_data data types.
 *
 * Revision 1.5  1994/02/01  21:17:16  markstro
 * Unknown
 *
 * Revision 1.4  1994/02/01  18:49:39  markstro
 * Made the declaration of read vars dynamic -- no more MAXREADVARS
 *
 * Revision 1.3  1994/01/31  20:17:12  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_DATAINFO_C
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : read_datainfo
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_datainfo (FILE_DATA *fd) {

   static char   err_buf[512];
   long   count, nline = 0;
   char   line[MAXDATALNLEN], linecopy[MAXDATALNLEN];
   PUBVAR   *var;
   char   *key, *countstr, *endptr;

   Mnreads = 0;

   if (!(fgets (fd->info, MAXDATALNLEN, fd->fp))) {
      (void)sprintf (err_buf, "Can't read data file info string\n%s", fd->name);
      return (err_buf);
   }

   fd->info[strlen (fd->info) - 1] = '\0';
   nline++;

/*
**  Read the header of the data file.  The end of the header occures
**  when a line starts with at least 4 "#"s.
*/
   (void)strcpy (line, "");
   while (strncmp (line, "####", 4)) {
      if ((fgets (line, MAXDATALNLEN, fd->fp)) == NULL) {
         (void)sprintf (err_buf,"#### delimiter not found in data file\n%s", fd->name);
         return (err_buf);
      }

      nline++;
      if (strncmp(line, "####", 4)) {

/*
**  A line that starts with "//" is a comment (as far as MMS is concerned).
*/
         if ((line[0] == '/') && (line[1] == '/')) {
/*
            printf ("Comment: %s\n", line);
*/
/*
**  Ignore blank lines
*/
         } else if (strlen (line) <= 1) {

/*
** Assume anything else is a data line
*/
         } else {

/* 
**   Increase size of array pointers
*/
            if (Mnreads >= max_read_vars) {
               max_read_vars += 50;
               Mcheckbase = (READCHECK **)realloc(Mcheckbase, max_read_vars * 
                  sizeof(READCHECK *));
            }
      
/*
**    get key, check the var has been declared
*/

            (void)strcpy(linecopy, line);
            key = strtok(linecopy, " \t");

            if (key == NULL) {
               (void)sprintf (err_buf,"Check format at line number %ld in\n%s\n%s", nline,
                  fd->name, line);
               return (err_buf);
            }

            if ((var = var_addr(key)) == NULL) {
               (void)sprintf (err_buf,
                  "Variable %s not declared at line number %ld in\n%s\n%s",
                  key, nline, fd->name, line);
               return (err_buf);
            }

/*
**   make space for data base entry, load pointer to var
*/

            Mcheckbase[Mnreads] = (READCHECK *) umalloc(sizeof(READCHECK));
            Mcheckbase[Mnreads]->var = var;

/*
**   get size of var in data file
*/

            countstr = strtok(NULL, " \t");

            if (countstr == NULL) {
               (void)sprintf (err_buf,"Check format at line number %ld in\n%s\n%s", nline,
                  fd->name, line);
               return (err_buf);
            }

            errno = 0;
            count = strtol (countstr, &endptr, 10);

            if (errno || (count < 0)) {
               (void)sprintf (err_buf,"Decoding %s at line number %ld in\n%s\n%s",
                  countstr, nline, fd->name, line);
               return (err_buf);
            }

            Mcheckbase[Mnreads]->count = count;

/* 
**   allocate enough room to read variables in, depending on variable type
*/

            if (Mcheckbase[Mnreads]->var) {
               switch (Mcheckbase[Mnreads]->var->type) {
                  case M_LONG :
                     Mcheckbase[Mnreads]->Types.valuel = (long *)umalloc(count * 
                        sizeof(long));
                     break;
     
                  case M_FLOAT :
                     Mcheckbase[Mnreads]->Types.valuef = (float *)umalloc(count *
                        sizeof(float));
                     break;
     
                  case M_DOUBLE :
                     Mcheckbase[Mnreads]->Types.valued=(double *)umalloc(count *
                        sizeof(double));
                     break;

               }
            }           
            Mnreads++;
         }
      }
   }

/*
**   Read first line of data
*/
   if (!(fgets (fd->line, MAXDATALNLEN, fd->fp))) {
      (void)sprintf (err_buf,
         "read_datainfo: Data for first timestep not found in file %s\n",
         fd->name);
      return (err_buf);
   }

   return (NULL);
}
/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/
