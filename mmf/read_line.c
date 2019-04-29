/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : read_line.c
 * AUTHOR   : Mike Dixon CADSWES March 1990
 *            Pedro Restrepo CADSWES April 1992
 *            Steve Markstrom July 1994
 * DATE     : Tue 19 Jul 1994
 * FUNCTION : read_line
 * COMMENT  :    Reads one line from data file into a string,
 *               decodes date and time, and returns if the time
 *               is within start and end limits.
 *               Otherwise reads lines until within limits or the
 *               end of file is encountered.
 *                  
 *               Returns 1l if end of data, 0l if more data to be read,
 *               and 2l if end of file
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: read_line.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: read_line.c,v $
        Revision 1.35  2001/01/22 22:26:41  markstro
        unknown

        Revision 1.34  2000/07/27 18:18:59  markstro
        Storm mode hacks

        Revision 1.33  2000/07/25 14:18:35  markstro
        Took out debugging print statements.

        Revision 1.32  2000/07/24 23:12:56  markstro
        Storm hack

        Revision 1.31  2000/02/18 18:27:05  markstro
        Made previous Julian time a global.  It is set to -1.0 before the run
        so that read_line knows to recalculate it.

        Revision 1.30  1999/09/13 15:44:47  markstro
        Fixed multiple close of data file

        Revision 1.29  1999/09/10 22:43:25  markstro
        Fixed multiple closing of data files.

        Revision 1.28  1999/08/24 16:34:15  markstro
        Version 1.1.1

        Revision 1.27  1998/03/04 17:20:17  markstro
        Added seperate runcontrol functions for each run type.

        Revision 1.26  1996/09/10 15:49:07  markstro
        Fixed the "day before the end" problem in DATA_find_end.

 * Revision 1.25  1996/02/19  20:00:42  markstro
 * Now lints pretty clean
 *
 * Revision 1.24  1995/11/25  23:05:07  markstro
 * Put in hack to get around delta time = 0 when coming out of storm mode.
 *
 * Revision 1.23  1995/11/25  02:42:14  markstro
 * Reading unit vs. daily data files.
 *
 * Revision 1.22  1995/11/24  14:35:36  markstro
 * Initial Purify work.
 * This is the version for Watershed Systems Modeling class 11/27 - 12/1, 1995
 *
 * Revision 1.21  1995/06/21  18:07:18  markstro
 * Scenario stuff
 *
 * Revision 1.20  1995/05/25  14:26:37  markstro
 * (1) Added batch mode
 * (2) Replaced "b" functions with "mem" versions
 *
 * Revision 1.19  1995/01/23  22:03:24  markstro
 * Fixed getting end date of data.
 *
 * Revision 1.18  1995/01/06  20:26:30  markstro
 * Argument list fixes
 *
 * Revision 1.17  1994/12/21  21:36:16  markstro
 * (1) Fixed ESP to work with multiple data files.
 * (2) Fixed Optimization to work with multiple data files.
 * (3) Fixed Sensitivity to work with multiple data files.
 *
 * Revision 1.16  1994/12/15  19:12:31  markstro
 * Changes for Christoph:  (1) More work on setting data file labels;
 * and (2) Fixed problems with empty display variable lists.
 *
 * Revision 1.15  1994/12/09  16:17:38  markstro
 * (1)  More work on selection of data files.
 * (2)  Work on display variable windows.
 *
 * Revision 1.14  1994/11/25  18:13:41  markstro
 * unknown
 *
 * Revision 1.13  1994/11/22  17:20:11  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.12  1994/11/09  22:10:45  markstro
 * GIS stuff out
 *
 * Revision 1.11  1994/11/08  16:17:38  markstro
 * (1) More proto type fine tuning
 * (2) fixed up data file reading
 *
 * Revision 1.10  1994/10/24  14:18:51  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.9  1994/10/03  19:38:19  markstro
 * prevjt now static -- big bug fix !!
 *
 * Revision 1.8  1994/09/30  14:54:56  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.7  1994/09/06  19:16:44  markstro
 * Fixed the day "off by one" problem.
 *
 * Revision 1.6  1994/08/31  21:50:39  markstro
 * Unknown
 *
 * Revision 1.5  1994/08/02  17:46:37  markstro
 * Split data file capabilities
 *
 * Revision 1.4  1994/01/31  20:17:13  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_LINE_C

#include <sys/stat.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static void INSERT_time (char *, DATETIME *);

/**5*********************** LOCAL VARIABLES ***************************/
/*
static double   prevjt = -1.0;
*/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : read_line
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long read_line (void) {

   /*static char err[80];*/

   char   *start_point, *end_point;
   float   initial_deltat;
   long   i,j;
   static int   start_of_data;
   DATETIME   prevtime;
   FILE_DATA   *cur_fd;
   char   *err_ptr;
   char line[MAXDATALNLEN];
  
/*
**   get initial delta-t from control data base
*/
   initial_deltat = *control_fvar("initial_deltat");

   if (Mnsteps == 0) {
      start_of_data = TRUE;
      Mprevjt = 1.0;
/*
      prevjt = -1.0;
*/
   }

   prevtime = *Mnowtime;

   while (TRUE) {

/*
**  Load cur_fd with the data for the next time step.
*/
      cur_fd = FILE_with_next_ts ();

/*
**  9999 in the year field is the code for EOF. 
*/
      if (cur_fd->time.year == 9999)
         return ENDOFFILE;

/*
**   DANGER -- This "if" is a hack to get delta time back after storm mode
**            This is the situation :

cur_fd->time = {year = 1956, month = 2, day = 19, hour = 0, min = 0, sec = 0, 
  jd = 2435523, jt = 2435523}

*Mnowtime = {year = 1956, month = 2, day = 18, hour = 24, min = 0, sec = 0, 
  jd = 2435522, jt = 2435523}

*/

      if ((Mprevjt < 0.0) && (cur_fd->time.jt - Mprevjt <= 0.000001)) {
         Mprevjt = (double)(Mnowtime->jd);
      }

/*
**  End of DANGER
*/

/*
**   Copy time from current file into global time structure
*/
      Mnowtime->year = cur_fd->time.year;
      Mnowtime->month = cur_fd->time.month;
      Mnowtime->day = cur_fd->time.day;
      Mnowtime->hour = cur_fd->time.hour;
      Mnowtime->min = cur_fd->time.min;
      Mnowtime->sec = cur_fd->time.sec;
      Mnowtime->jd = cur_fd->time.jd;
      Mnowtime->jt = cur_fd->time.jt;

/*
**   check if data time is within limits
*/
      if (Mnowtime->jt > Mendtime->jt) {
         *Mnowtime = prevtime;
         return ENDOFDATA;
      }

      if (Mnowtime->jt >= Mstrttime->jt) {
         if (start_of_data) {
            start_of_data = 0;
            Mprevjt = Mnowtime->jt - (double)(initial_deltat / 24.0);
         }

         (void)strcpy (line, cur_fd->start_of_data);
         Mnsteps++;
/*
**  DANGER -- Mprevjt must be hacked if starting from var init file.
**            It is computed based on current time and Mdeltat (which
**            is read from var init file.
*/

/*
         (void)fprintf (stderr,"\n\n read_line Mprevjt = %f Mnowtime->jt = %f\n", Mprevjt, Mnowtime->jt);
         (void)fprintf (stderr,"     read_line dt = %f\n", Mnowtime->jt - Mprevjt );
         (void)fprintf (stderr,"          year, mon, day = %d %d %d %d %d\n", Mnowtime->year, Mnowtime->month, Mnowtime->day, Mnowtime->hour, Mnowtime->min);
*/

		 if (Mnowtime->jt < Mprevjt) {
			(void)fprintf (stderr,"\n\n read_line Mprevjt = %f Mnowtime->jt = %f\n", Mprevjt, Mnowtime->jt);
			(void)fprintf (stderr,"Current time step is before previous time step.\n");
			(void)fprintf (stderr,"The data file(s) are running backwards.\n");
			return (ERROR_TIME);
		 }

         if ((Mnowtime->jt - Mprevjt) < 0.0000115) {
/*
** DANGER This hack is to come out of the storm
*/
            (void)fprintf (stderr,"read_line:  comming out of storm. dt = 1 day\n");
            Mdeltat = 1.0;
            Mprevjt = Mnowtime->jt - Mdeltat;

         } else {
            if (Mprevjt < 0.0) {
               Mprevjt = Mnowtime->jt - Mdeltat;
            } else {
               Mdeltat = Mnowtime->jt - Mprevjt;
            }
         }
/*
         (void)fprintf (stderr,"read_line Mdeltat = %f\n", Mdeltat );
*/

/*
**  End DANGER
*/

         Minpptr = end_point = line;

/* 
**   Read variables from the line into their respective buffers
*/
         for (i = 0; i < Mnreads; i++) {
            for (j = 0; j < Mcheckbase[i]->count; j++) {
               start_point = NULL;
               if (Mcheckbase[i]->var) {
                  start_point = end_point;
                  errno = 0;
                  switch (Mcheckbase[i]->var->type) {
                     case M_LONG:
                        Mcheckbase[i]->Types.valuel[j] =
                           strtol (start_point, &end_point, 10);
                        break;
      
                     case M_FLOAT:
                        Mcheckbase[i]->Types.valuef[j] =
                           (float)strtod (start_point, &end_point);
                        break;
      
                     case M_DOUBLE:
                        Mcheckbase[i]->Types.valued[j] =
                           strtod (start_point, &end_point);
                        break;
                     }

                  if (CHECK_data (errno, cur_fd)) return (ENDOFDATA);

               } else {
                  (void)strtod (start_point, &end_point);
               }
            }
         }

         Mprevjt = Mnowtime->jt;

         if (!(fgets (cur_fd->line, MAXDATALNLEN, cur_fd->fp))) {
            fclose (cur_fd->fp);
            cur_fd->fp = NULL;
            cur_fd->time.year = 9999;
         } else if (cur_fd->line[0] == '\n') {
            fclose (cur_fd->fp);
            cur_fd->fp = NULL;
            cur_fd->time.year = 9999;
         } else {
            err_ptr = EXTRACT_time (cur_fd);
            if (err_ptr) {
               (void)fprintf (stderr,"%s\n", err_ptr);
               return (ERROR_TIME);
            }
         }

/*
**   Copy time from current file into global next time structure
*/
         if (cur_fd && cur_fd->fp) {
            Mnexttime->year = cur_fd->time.year;
            Mnexttime->month = cur_fd->time.month;
            Mnexttime->day = cur_fd->time.day;
            Mnexttime->hour = cur_fd->time.hour;
            Mnexttime->min = cur_fd->time.min;
            Mnexttime->sec = cur_fd->time.sec;
            Mnexttime->jd = cur_fd->time.jd;
            Mnexttime->jt = cur_fd->time.jt;
            Mdeltanext = Mnexttime->jt - Mnowtime->jt;
         }
         return (0);
      } else {
/*
**   Read throgh the data before the start time.
*/
         Mprevjt = Mnowtime->jt;

         if (!(fgets (cur_fd->line, MAXDATALNLEN, cur_fd->fp))) {
            fclose (cur_fd->fp);
            cur_fd->fp = NULL;
            cur_fd->time.year = 9999;
         }

         err_ptr = EXTRACT_time (cur_fd);
         if (err_ptr) {
               (void)fprintf (stderr,"%s\n", err_ptr);
            return (ERROR_TIME);
         }
      }
   }
/*
   return (0);
*/
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_read_init
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *DATA_read_init (void) {

   static char err[80];

   int      i;
   static int      num_data_files = 0;
   char   **fname, line[MAXDATALNLEN], *err_ptr;
   static char      buf[256];

/*
**   Clean up the old files
*/
   if (fd) {
      for (i = 0; i < num_data_files; i++)
         if ((fd[i])->fp) {
               fclose ((fd[i])->fp);
            fd[i]->fp = NULL;
            }
/*
      free (fd);
*/
   }

   fname =   control_svar ("data_file");
   num_data_files = control_var_size ("data_file");

   fd = (FILE_DATA **)malloc (num_data_files * sizeof (FILE_DATA *));
    for (i = 0; i < num_data_files; i++) {
      fd[i] = (FILE_DATA *)malloc (sizeof (FILE_DATA));
    }

/*
**   Open the files.
*/
   for (i = 0; i < num_data_files; i++) {
      (fd[i])->name = strdup (fname[i]);
      if (!((fd[i])->fp = fopen (fname[i], "r"))) {
         (void)sprintf (err, "DATA_read_init: can't open data file %s\n",
            fname[i]);
         return (err);
      }

      fgets (line, MAXDATALNLEN, (fd[i])->fp);
      while (strncmp (line, "####", 4)) {
         if (!(fgets (line, MAXDATALNLEN, (fd[i])->fp))) {
               (void)sprintf (buf, "DATA_read_init - Spacing fwd to data - Check format of file %s.", fname[i]);
               return (buf);
            }
      }

    /*
     * initialize year
     * PJR 7/10/95
     */
       (fd[i])->time.year = 0;
      fgets ((fd[i])->line, MAXDATALNLEN, (fd[i])->fp);
/* DANGER
      if (err_ptr = EXTRACT_time (&(fd[i])))
*/
      err_ptr = EXTRACT_time (fd[i]);
      if (err_ptr) return (err_ptr);
    
   }
   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : READ_data_info
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *READ_data_info (void) {

   static char err[80];

   int      i, num_data_files;
   char   **fname, *err_ptr;
   FILE_DATA  lfd;
   struct stat stbuf;

   fname =   (char **) control_var ("data_file");
   num_data_files = control_var_size ("data_file");

/*
**   Check the files
*/
   for (i = 0; i < num_data_files; i++) {
      if (stat (fname[i], &stbuf) == -1) {
         (void)sprintf (err, "Reading Data Info: Can't open data file %s\n",
            fname[i]);
         return (err);
      } else if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
         (void)sprintf (err, "Reading Data Info: Can't open data file %s\n",
            fname[i]);
         return (err);
      }
   }
/*
**   Open the files.
*/
   for (i = 0; i < num_data_files; i++) {
      lfd.name = strdup (fname[i]);
      if (!(lfd.fp = fopen (fname[i], "r"))) {
         (void)sprintf (err, "DATA_read_init: can't open data file %s\n",
            fname[i]);
         return (err);
      }
      err_ptr = read_datainfo (&lfd);
      if (err_ptr) return (err_ptr);
/*
      free (lfd.name);
*/
      fclose (lfd.fp);
   }
   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_check_start
 | COMMENT      : Check if start time of model is more than a day before
 |                 the start time of the data.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *DATA_check_start (void) {

   int num_data_files, found, i;

   num_data_files = control_var_size ("data_file");

   found = FALSE;
   for (i = 0; (i < num_data_files) && !found; i++)
      if ((fd[i])->time.jd <= Mstrttime->jd)
         found = TRUE;

   if (!found)
      return ("Start time is before first data.");

   return (NULL);
}
/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_close
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : void 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void DATA_close (void) {
   int      i, num_data_files;

   num_data_files = control_var_size ("data_file");

   for (i = 0; i < num_data_files; i++) {
        if (((fd[i])->fp) != NULL) {
         fclose ((fd[i])->fp);
         (fd[i])->fp = NULL;
        }
    }

/*
   free (fd);
*/
   fd = NULL;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_var_size
 | COMMENT      : returns the size of the array
 | PARAMETERS   :
 | RETURN VALUE : int - returns the size of the array
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int control_var_size (char *key) {
   CONTROL *control;

   if (!(control = control_addr(key))) {
      (void)fprintf (stderr, 
         "control_var_size - key '%s' not found.\n", key);
      return (1);
   }
   return (control->size);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : FILE_with_next_ts
 | COMMENT      : Determine the file with the next time step.
 | PARAMETERS   : None
 | RETURN VALUE : Proiter to file data structure
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
FILE_DATA * FILE_with_next_ts (void) {

   FILE_DATA *cur_fd, *fd_ptr;
   int      num_data_files, i;
   char   *err_ptr;

   num_data_files = control_var_size ("data_file");
   cur_fd = fd[0];

   for (i = 1; i < num_data_files; i++) {
        fd_ptr = fd[i];  
      if (fd_ptr->time.year != 9999) {
/*
**   If two files have the same julian day, assume one is a storm file
**   and one is a daily file.  Throw out the daily value.
*/
         if (fd_ptr->time.jd == cur_fd->time.jd) {
            if (fd_ptr->time.jt == cur_fd->time.jt) {
               (void)fprintf (stderr,
                  "FILE_with_next_ts: The files %s and %s both seem to contain the same storm on %ld - %ld - %ld.\n",
                  fd_ptr->name, cur_fd->name, fd_ptr->time.year,
                  fd_ptr->time.month, fd_ptr->time.day);

            } else if (fd_ptr->time.jt < cur_fd->time.jt) {

               Mprevjt = fd_ptr->time.jt;

               if (!(fgets (fd_ptr->line, MAXDATALNLEN, fd_ptr->fp))) {
                  fclose (fd_ptr->fp);
                      fd_ptr->fp = NULL;
                  fd_ptr->time.year = 9999;
               } else if (fd_ptr->line[0] == '\n') {
                  fclose (fd_ptr->fp);
                      fd_ptr->fp = NULL;
                  fd_ptr->time.year = 9999;
               } else {
                  err_ptr = EXTRACT_time (fd_ptr);
                  if (err_ptr) (void)fprintf (stderr,"%s\n", err_ptr);
               }


            } else {

               Mprevjt = cur_fd->time.jt;

               if (!(fgets (cur_fd->line, MAXDATALNLEN, cur_fd->fp))) {
                  fclose (cur_fd->fp);
                      cur_fd->fp = NULL;
                  cur_fd->time.year = 9999;
               } else if (cur_fd->line[0] == '\n') {
                  fclose (cur_fd->fp);
                      cur_fd->fp = NULL;
                  cur_fd->time.year = 9999;
               } else {
                  err_ptr = EXTRACT_time (cur_fd);
                  if (err_ptr) (void)fprintf (stderr,"%s\n", err_ptr);
               }

               cur_fd = fd_ptr;
            }

         } else if (fd_ptr->time.jd < cur_fd->time.jd) {
            cur_fd = fd_ptr;
         }
      }
   }

   return (cur_fd);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : EXTRACT_time
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char * EXTRACT_time (FILE_DATA *data) {
   char   *start_point, *end_point;
   char   line[MAXDATALNLEN];
   static char   err_buf[MAXDATALNLEN];

   if (data->time.year == 9999) {
      return (NULL);
   }

   (void)strcpy (line, data->line);

   start_point = line;

   errno = 0;
   data->time.year = strtol (start_point, &end_point, 10);

   if (data->time.year < 1800 || data->time.year > 2100) {
      (void)sprintf (err_buf, "EXTRACT_time - year %ld out of range.\nline:%s",
         data->time.year, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf(err_buf, "EXTRACT_time - Decoding year line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.month = strtol (start_point, &end_point, 10);

   if (data->time.month < 1 || data->time.month > 12) {
      (void)sprintf (err_buf, "EXTRACT_time - month %ld out of range.\nline:%s",
         data->time.month, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf(err_buf, "EXTRACT_time - Decoding month line %s",start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.day = strtol (start_point, &end_point, 10);

   if (data->time.day < 1 || data->time.day > 31) {
      (void)sprintf (err_buf, "EXTRACT_time - day %ld out of range.\nline:%s",data->time.day, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf (err_buf, "Decoding day line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.hour = strtol(start_point, &end_point, 10);

   if (data->time.hour < 0 || data->time.hour > 24) {
      (void)sprintf(err_buf,"EXTRACT_time - hour %ld out of range.\nline:%s",data->time.hour, data->line);
      return  (err_buf);
   }

   if (errno == EDOM || errno == ERANGE)   {
      (void)sprintf (err_buf, "Decoding hour line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.min = strtol(start_point, &end_point, 10);


    if (data->time.min < 0 || data->time.min > 59) {
      (void)sprintf (err_buf, "EXTRACT_time - minute %ld out of range.\nline:%s",
         data->time.min, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf (err_buf, "Decoding minute line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.sec = (int)(strtod(start_point, &end_point));
/*
   data->time.sec = strtod(start_point, &end_point, 10);
*/

   if (data->time.sec < 0 || data->time.min > 59) {
      (void)sprintf (err_buf, "EXTRACT_time - second %ld out of range.\nline:%s",
         data->time.sec, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf (err_buf, "Decoding second line %s\n", start_point);
      return (err_buf);
   }

   data->start_of_data = data->line + (end_point - line);

   julday (&(data->time));

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_data
 | COMMENT      : See if there is an error with the data value.
 | PARAMETERS   :
 | RETURN VALUE : Error code
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
int CHECK_data (int en, FILE_DATA *cur_fd) {
   if (en == EDOM || en == ERANGE) {
      (void)fprintf (stderr,"read_line");
      perror (" ");
      (void)fprintf (stderr, "Reading line %s\n", cur_fd->line);
      return (ENDOFDATA);
   }
   return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_find_end
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void DATA_find_end (DATETIME *start_of_data, DATETIME *end_of_data) {
  FILE   *f_ptr;
  int      i, num_data_files;
  DATETIME check;
  char line[MAXDATALNLEN];

  num_data_files = control_var_size ("data_file");

  /*
  **  Get start and end of first file.
  */
  f_ptr = fopen ((fd[0])->name, "r");
  
  fgets (line, MAXDATALNLEN, f_ptr);
  while (strncmp (line, "####", 4))
    fgets (line, MAXDATALNLEN, f_ptr);

  fgets (line, MAXDATALNLEN, f_ptr);
  INSERT_time (line, start_of_data);

  while (fgets (line, MAXDATALNLEN, f_ptr));

  INSERT_time (line, end_of_data);

  fclose (f_ptr);

  /*
  **  Loop through the other ones.
  */
  for (i = 1; i < num_data_files; i++) {
    f_ptr = fopen ((fd[i])->name, "r");

    fgets (line, MAXDATALNLEN, f_ptr);
    while (strncmp (line, "####", 4))
      fgets (line, MAXDATALNLEN, f_ptr);

    fgets (line, MAXDATALNLEN, f_ptr);
    INSERT_time (line, &check);

    if (check.jt < start_of_data->jt) {
      start_of_data->year = check.year;
      start_of_data->month = check.month;
      start_of_data->day = check.day;
      start_of_data->hour = check.hour;
      start_of_data->min = check.min;
      start_of_data->sec = check.sec;
      start_of_data->jd = check.jd;
      start_of_data->jt = check.jt;
    }

    while (fgets (line, MAXDATALNLEN, f_ptr));
    INSERT_time (line, &check);

    if (check.jt > end_of_data->jt) {
      end_of_data->year = check.year;
      end_of_data->month = check.month;
      end_of_data->day = check.day;
      end_of_data->hour = check.hour;
      end_of_data->min = check.min;
      end_of_data->sec = check.sec;
      end_of_data->jd = check.jd;
      end_of_data->jt = check.jt;
    }
    fclose (f_ptr);
  }
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : INSERT_time
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static void INSERT_time (char *line, DATETIME *ptr) {
   char   *start_point, *end_point;

   start_point = line;
   ptr->year = strtol (start_point, &end_point, 10);

   start_point = end_point;
   ptr->month = strtol (start_point, &end_point, 10);

   start_point = end_point;
   ptr->day = strtol (start_point, &end_point, 10);

   start_point = end_point;
   ptr->hour = strtol(start_point, &end_point, 10);

   start_point = end_point;
   ptr->min = strtol(start_point, &end_point, 10);

   start_point = end_point;
   ptr->sec = strtol(start_point, &end_point, 10);

   julday (ptr);
}

/**8************************** TEST DRIVER ****************************/

