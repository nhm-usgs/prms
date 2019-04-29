
/*
 *  $Id: defs.h 5145 2012-12-19 17:39:07Z rsregan $
 */

/*
**
**		$Author: rsregan $
**
**		$Date: 1997/04/21 22:28:58 $
**
**		$Locker:  $
**
**		$Log: defs.h,v $
**		Revision 1.11  1997/04/21 22:28:58  markstro
**		Xmbuild fixes
**
**		Revision 1.10  1996/09/10 16:37:59  markstro
**		(1) Bugs in XY plotter
**		(2) Reworked file text widgets in run control.
**
 * Revision 1.9  1996/02/19  20:05:08  markstro
 * After first lint session
 *
**		Revision 1.8  1995/11/24 14:39:45  markstro
**		Initial Purify work.
**		This is the version for the Watershed Systems Modeling class 11/27 - 12/1, 1995
**
 * Revision 1.7  1995/02/07  23:16:58  markstro
 * Stuff for rosenbrock and sensitivity
 *
 * Revision 1.6  1994/12/21  21:43:18  markstro
 * Unknown
 *
 * Revision 1.5  1994/11/23  19:46:06  markstro
 * Unknown
 *
 * Revision 1.4  1994/06/02  20:13:28  markstro
 * Clean include files
 *
 * Revision 1.3  1994/05/23  14:26:00  markstro
 * (1) Put in ifndef so included only once.
 * (2) Cleaned out a lot of includes in includes
 *
 * Revision 1.2  1994/01/04  19:37:22  markstro
 * Pedro's include files
 *
 * Revision 1.1  91/07/05  14:23:08  farah
 * Initial revision
 *
 * Revision 1.1  90/07/31  12:25:03  markb
 * Initial revision
 *
**Revision 1.1  90/07/31  12:11:14  markb
**Initial revision
**
**
**		$Revision: 5145 $
**
**		$Source: /z/runoff/work4/mms_cvs/mms/src/include/defs.h,v $
**
**		$State: Exp $
*/

#ifndef MMS_DEF_H
#define MMS_DEF_H

#define TRUE 1
#define FALSE 0

#define ftnint int
#define ftnlen int

#define M_LONG 1
#define M_FLOAT 2
#define M_DOUBLE 3
#define M_STRING 4

#define M_PARAMETER 0
#define M_VARIABLE  1

// Markstrom 05/24/2010 found some unused defined constants and removed them.
//#define MAX_OPT_ARRAY_SIZE 200

#define M_BOUNDED 1
#define M_UNBOUNDED 2

#define ERROR_TIME 100
//#define M_NODEBUG 0
//#define M_PARTDEBUG 1
#define M_FULLDEBUG 2

#ifndef MIN
#define MIN(a,b) (a < b) ? a : b
#endif

#ifndef MAX
#define MAX(a,b) (a > b) ? a : b
#endif

#define MAXDATALNLEN 10000 /* max no. of chars in input file line */

#define ENDOFFILE 2L
#define ENDOFDATA 1L
#define NOTENDOFDATA 0l

// Markstrom 05/24/2010 found some reads that were differnt sizes that the character arrays that
//                      were being read into. I fixed this by getting rid of all of these other
//                      string lengths and changing all string lengths to "MAXDATALNLEN" since this
//                      is the length that we need to read data lines in the data file.
//#define MAXENVLEN 256    /* max env file line length */
//#define MAXTOKLEN 128    /* max no of chars in token */
//#define MAXLNLEN 256     /* max file line length */
//#ifndef MAXPATHLEN
//#define MAXPATHLEN 256   /* max no of chars in a path */
//#endif

#define MAX_NDIMEN 3       /* max no. of dimensions for a var or param */
//#define MAXKEYLEN 50       /* max no. of chars in key string */
//#define MAXDIMLEN 50       /* max no. of chars in dimen string */
#define MAXSTATVARS 200     /* max no. of statistic variables */
//#define MAXDISPVARS 200    /* max no. of display variables */
//#define MAXINFOLEN 80      /* max no. of chars in run info string */

//#define MAX_SAVE_MAP 5

#endif /* MMS_DEF_H */
