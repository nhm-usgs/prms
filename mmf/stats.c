/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : stats.c
 * AUTHOR   : Mike Dixon, Pedro Restrepo CADSWES, University of Colorado,
 *             Boulder
 * DATE     : May 1990   
 * FUNCTION : stats
 * COMMENT  : statistical analysis postprocessor
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: stats.c 5739 2010-10-01 18:02:03Z rsregan $
 *
   $Revision: 5739 $
        $Log: stats.c,v $
        Revision 1.13  1998/11/10 15:17:44  markstro
        unknown

        Revision 1.12  1996/03/25 21:58:12  markstro
        Unknown

 * Revision 1.11  1996/02/19  20:01:15  markstro
 * Now lints pretty clean
 *
        Revision 1.10  1995/02/01 17:47:50  markstro
        Addition of Rosenbrock optimization.  Start of sensitivity.  Many bug fixes.

 * Revision 1.9  1994/11/22  17:20:35  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.8  1994/10/24  14:19:06  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.7  1994/09/30  14:55:23  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.6  1994/05/18  17:16:10  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.5  1994/03/07  21:20:00  markstro
 * Changes from TERRA
 *
 * Revision 1.4  1994/02/01  21:17:18  markstro
 * Unknown
 *
 * Revision 1.3  1994/01/31  20:17:36  markstro
 * Make sure that all source files have CVS log.
-*/

/**1************************ INCLUDE FILES ****************************/
#define STATS_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "mms.h"

#define MAXCELLS 100


/**2************************* LOCAL MACROS ****************************/
#define SQR(A) (A) * (A)
#define CUBE(A) (A) * (A) * (A)

/**3************************ LOCAL TYPEDEFS ***************************/
  typedef struct {
    char varName[30];        /* variable name                  */
    char * elem_number;      /* element number                 */
    float Sx;                /* Sum of x                       */
    float Sx2;               /* Sum of x*x                     */
    float Sx3;               /* Sum of x*x*x                   */
    float mx;		     /* mean of x                      */
    float sdev;		     /* standard deviation             */
    float skew;		     /* skewness                       */
    float min;		     /* minimum                        */
    float max;		     /* maximum                        */
    float histmin;	     /* histogram minimum              */
    float histmax;	     /* histogram maximum              */
    int   ncells;	     /* number of cell in the histogram*/
    float width;             /* width of the histogram cell    */
    float histog[MAXCELLS];  /* histogram                      */
  }STATS;

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : stats
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : int
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int stats (void) {
     
  int     nvars;                 /* number of variables in the file*/
  STATS   st[MAXSTATVARS];            /* array of statistics structures */
  FILE    *statvar_file;
  //FILE    *stats_file;
  char    path[MAXDATALNLEN];
  char    line[MAXDATALNLEN];
  int     i,nvals;
  //int     j;
  int     recNo;
  int     year;
  int     month;
  int     day;
  int     hour;
  int     minute;
  int     second;
  char    elem_number[MAXDATALNLEN];
  double  x[MAXSTATVARS];
  float   squared;
  //float   cumul,comp;

  /*
   * Open statvar file, and store number of variables and variable names 
   */

  if (*((char **) control_var("stat_var_file")) == NULL) {
    (void)fprintf(stderr, "ERROR - stats");
    (void)fprintf(stderr, "Check Control File, stat_var_file missing.\n");
    return(1);
  }

  (void)sprintf(path, "%s", *((char **) control_var("stat_var_file")));

  if ((statvar_file = fopen(path, "r")) == NULL) {
      (void)fprintf(stderr, "ERROR - stats");
      (void)fprintf(stderr, "Could not open statvar file '%s'\n",
	      path);
      return(1);
    }
  
  fscanf(statvar_file,"%d",&nvars);

	if (!nvars) {
		//fclose(stats_file);
		return(0);
	}


  for (i=0;i<nvars;i++) {
      memset (&st[i], 0, sizeof(STATS));
      st[i].min = 1e30;
      st[i].max = -1e30;
      fscanf(statvar_file,"%s %s", st[i].varName,
	     elem_number);
      st[i].elem_number = (char *)malloc(strlen(elem_number) + 1);
      (void)strcpy(st[i].elem_number, elem_number);

    }

  nvals = 0;

  while (EOF !=
	 fscanf(statvar_file, "%d %d %d %d %d %d %d",
		&recNo,&year,&month,&day,&hour,&minute,&second))
    {
      nvals++;
      for (i=0;i<nvars;i++) 
	{
	  fscanf(statvar_file, "%lf", &x[i]); 
	  st[i].Sx += x[i];
	  st[i].Sx2 += SQR(x[i]);
	  st[i].Sx3 += CUBE(x[i]);
	  st[i].min = MIN(st[i].min,x[i]);
	  st[i].max = MAX(st[i].max,x[i]);
	}
    }

  for (i=0;i<nvars;i++)
    {
      if (nvals > 1) {
	st[i].mx   = st[i].Sx/nvals;
	
	squared = (st[i].Sx2 - nvals*SQR(st[i].mx)) / (nvals-1);
	
	if (squared < 0.0)
	  squared = 0.0;
	
	st[i].sdev =  (float)sqrt(squared);
	
	if (st[i].sdev > 0.0) {
	  st[i].skew = ((st[i].Sx3/nvals -
			 3.0*st[i].mx*st[i].Sx2/nvals+2.0*CUBE(st[i].mx))
			/(CUBE(st[i].sdev)));
	}
	
	if(nvals) 
/* DANGER
	  st[i].ncells = MIN(1 + 3.3 * log10((double)nvals),MAXCELLS);
*/
	  st[i].ncells = MAXCELLS;
	else
	  {
	    /*
	     ** NOTE: No values exist for histogram
	     */
	    st[i].ncells = 0;
	  }
	
	st[i].histmin = st[i].min;
	st[i].histmax = st[i].max;
	
	if (st[i].ncells)
	  st[i].width = ((st[i].histmax - st[i].histmin)/st[i].ncells);
	else
	  st[i].width = 0.0;
	
      }
    }
  /*
   * rewind the statvar file
   */

  fseek(statvar_file, 0L, 0);

  /*
   * space fwd to data
   */

  for (i = 0; i < nvars+1; i++) {
    if (fgets(line, MAXDATALNLEN, statvar_file) == NULL) {
      (void)fprintf(stderr, "ERROR - stats.\n");
      (void)fprintf(stderr, "Reading statvar file for histogram comps.\n");
      perror(path);
      return(1);
    }
  }

  /*
   * re-read the data to load the histograms
   */

  while (EOF !=
	 fscanf(statvar_file,"%d %d %d %d %d %d %d",
		&recNo,&year,&month,&day,&hour,&minute,&second))
    {
      for (i=0;i<nvars;i++) 
	{
	  fscanf(statvar_file,"%lf",&x[i]); 
	  (st[i].histog[(int)((x[i]-st[i].histmin)/st[i].width)])++;
	}
    }

  /*
   * close statvar file
   */

  fclose(statvar_file);

  /*
   * Open output file
   */

//  (void)sprintf(path, "%s", *control_svar("stats_output_file"));
//  
//  if ((stats_file = fopen(path, "w")) == NULL)
//    {
//      (void)fprintf(stderr, "ERROR - stats - ");
//      (void)fprintf(stderr, "Could not create statistics output file\n");
//      perror(path);
//      return(1);
//    }
//
//  for (i=0;i<nvars;i++) 
//    {
//      for (j = 0;j < st[i].ncells;j++)
//	st[i].histog[j] /= nvals;
//  
//      (void)fprintf(stats_file,"\n");
//      (void)fprintf(stats_file,"Variable:  %s\n",st[i].varName);
//      (void)fprintf(stats_file,"Elem #     %s\n",st[i].elem_number);
//      (void)fprintf(stats_file,"Mean       %f\n",st[i].mx);
//      (void)fprintf(stats_file,"Std Dev    %f\n",st[i].sdev);
//      (void)fprintf(stats_file,"Skewness   %f\n",st[i].skew);
//      (void)fprintf(stats_file,"Minimum    %f\n",st[i].min);
//      (void)fprintf(stats_file,"Maximum    %f\n",st[i].max);
//      (void)fprintf(stats_file,"#. Cells   %d\n",st[i].ncells);
//      (void)fprintf(stats_file,"Cell width %f\n",st[i].width);
//      (void)fprintf(stats_file,
//	      "\nHistogram\nCellNo. Lower Limit   Upper Limit   Frequency   Cumulative Complementary\n");
//  
//      cumul = 0.0;
//      comp = 1.0;
//      for (j = 0;j < st[i].ncells;j++){
//	cumul += st[i].histog[j];
//	comp = 1.0-cumul;
//	(void)fprintf(stats_file,"%4d %11f %13f %13f %13f %13f\n", j,
//		st[i].histmin+j*st[i].width,
//		st[i].histmin+(j+1)*st[i].width,
//		st[i].histog[j],cumul,comp);
//      }
////    free((char *)st[i].elem_number);
//    }
//
//  fclose(stats_file);

  return(0);

}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/

