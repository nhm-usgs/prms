/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : setup_cont.c
 * AUTHOR   : CADSWES
 * DATE     : Fri 14 Oct 1994
 * FUNCTION :
 * COMMENT  :
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: setup_cont.c 5145 2012-12-19 17:39:07Z rsregan $
 *
   $Revision: 5145 $
        $Log: setup_cont.c,v $
        Revision 1.31  1999/08/24 16:34:16  markstro
        Version 1.1.1

        Revision 1.30  1997/11/13 17:13:35  markstro
        unknown

        Revision 1.29  1996/12/05 21:24:25  markstro
        (1)  Added getoutname()
        (2)  Sensitivity work
        (3)  Optimization work

        Revision 1.28  1996/06/28 19:32:31  markstro
        (1) Fixed 3d control window.
        (2) Fixed stats.

 * Revision 1.27  1996/04/09  21:04:21  markstro
 * (1) Work on control files
 * (2) Runtime graphs
 *
 * Revision 1.26  1996/03/26  22:31:10  markstro
 * Work on GIS displayer.
 *
 * Revision 1.25  1996/02/19  20:01:08  markstro
 * Now lints pretty clean
 *
        Revision 1.24  1995/11/21 20:03:08  markstro
        Changes from Pedro -- added scenario stuff to control file.

 * Revision 1.23  1995/07/06  21:15:17  markstro
 * Fixed:
 * (1)  Index names are now initially set to number.
 * (2)  Variables dumped out to db come from selection list.
 *
 * Revision 1.22  1995/07/05  16:53:29  markstro
 * Pedro's sensitivity changes for time period.
 *
 * Revision 1.21  1995/06/08  18:01:54  markstro
 * (1)  Fixed info window
 * (2)  Changed b functions to mem functions for solaris compatibility
 * (3)  Fixed default values in spreadsheet help
 *
 * Revision 1.20  1995/02/13  15:11:50  markstro
 * unknown
 *
 * Revision 1.19  1995/02/12  23:57:32  markstro
 * Rosenbrock and Sensitivity changes just before class.
 *
 * Revision 1.18  1995/02/07  23:19:17  markstro
 * Stuff for rosenbrock and sensitivity
 *
 * Revision 1.17  1995/02/01  17:47:48  markstro
 * Addition of Rosenbrock optimization.  Start of sensitivity.  Many bug fixes.
 *
 * Revision 1.16  1994/12/21  21:36:26  markstro
 * (1) Fixed ESP to work with multiple data files.
 * (2) Fixed Optimization to work with multiple data files.
 * (3) Fixed Sensitivity to work with multiple data files.
 *
 * Revision 1.15  1994/11/22  17:20:31  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.14  1994/11/10  23:26:48  markstro
 * (1)  Some memory fixes -- results of malloc_dbg.
 * (2)  More stuff removed from set menu.
 *
 * Revision 1.13  1994/11/09  22:10:50  markstro
 * GIS stuff out
 *
 * Revision 1.12  1994/10/24  14:19:04  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.11  1994/09/30  14:55:17  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.10  1994/09/09  21:57:23  markstro
 * Added backup of param and control files.
 *
 * Revision 1.9  1994/08/31  21:50:45  markstro
 * Unknown
 *
 * Revision 1.8  1994/06/21  20:20:37  markstro
 * More work on taking the module name out of the DB keyword.
 *
 * Revision 1.7  1994/05/18  17:16:08  markstro
 * TERRA changed mhms to mms
 *
 * Revision 1.6  1994/05/11  14:29:39  markstro
 * Changes from TERRA
 *
 * Revision 1.5  1994/03/11  21:16:43  markstro
 * Got rid of client_data data types.
 *
 o Revisoon 1.4  1994/02/01  21:17:17  markstro
 * Unknown
 *
 * Revision 1.3  1994/01/31  20:17:29  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SETUP_CONT_C

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/**5*********************** LOCAL VARIABLES ***************************/
extern void decl_control_string (char *key, char *valstr);
extern void decl_control_int_array (char *key, long size, long *valstr);
extern void decl_control_float_array (char *key, long size, float *valstr);
extern void decl_control_string_array (char *key, long size, char *valstr);

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : setup_cont
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void setup_cont (void) {
        long *lval;
        float *fval;
//        char *cval;
//      char **cp;
//      int i;

        static long start_date[] = {2000,10,1,0,0,0};
        static long end_date[] = {2001,9,30,0,0,0};

/*
**	GSFLOW control variables
*/
        decl_control_string ("model_mode", "PRMS");
        decl_control_string ("modflow_name", "modflow.nam");
        decl_control_string ("precip_module", "precip_1sta");
        decl_control_string ("temp_module", "temp_1sta");
        decl_control_string ("et_module", "potet_jh");
        decl_control_string ("srunoff_module", "srunoff_smidx");
        decl_control_string ("solrad_module", "ddsolrad");
        decl_control_string ("soltab_module", "soltab");
        decl_control_string ("soilzone_module", "soilzone");
		decl_control_string ("strmflow_module", "strmflow");
        decl_control_string ("transp_module", "transp_tindex");
        decl_control_string ("gsflow_output_file", "gsflow.out");
        decl_control_string ("gsflow_csv_file", "gsflow.csv");
        decl_control_string ("capillary_module", "null");

/*
        cval = (char *)umalloc (sizeof (long));
        cval[0] = "recharge";
        decl_control_string_array ("mapOutVar_names", 20, cval);
*/

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 7;
        decl_control_int_array ("rpt_days", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
        decl_control_int_array ("gsf_rpt", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("print_debug", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("cascade_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("cascadegw_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("subbasin_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("frozen_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dprst_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("parameter_check_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_imperv_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_intcp_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_covden_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_covtype_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_transp_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_potet_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_soil_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_radtrncf_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_dprst_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("segment_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("gwr_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("external_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("lake_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dprst_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("seg2hru_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("glacier_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("mbInit_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("musroute_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("orad_flag", 1, lval);

/*
**	file names
*/
        decl_control_string ("executable_desc", "MOWS executable");
        decl_control_string ("executable_model", "prmsIV");
        decl_control_string ("data_file", "prms.data");
        decl_control_string ("param_file", "prms.params");
        decl_control_string ("var_save_file", "prms_ic.out");
        decl_control_string ("var_init_file", "prms_ic.in");
        //decl_control_string ("stats_output_file", "stats.out");
        decl_control_string ("stat_var_file", "statvar.out");
        decl_control_string ("ani_output_file", "animation.out");
        decl_control_string ("model_output_file", "prms.out");
        decl_control_string ("tmax_day", "tmax.day");
        decl_control_string ("tmin_day", "tmin.day");
        decl_control_string ("precip_day", "precip.day");
        decl_control_string ("swrad_day", "swrad.day");
        decl_control_string ("potet_day", "potet.day");
        decl_control_string ("transp_day", "transp.day");
        decl_control_string ("covden_dynamic", "dyncovden");
        decl_control_string ("dprst_area_dynamic", "dyndprst");
        decl_control_string ("dprst_depth_dynamic", "dyndprst");
		decl_control_string ("snow_intcp_dynamic", "dynsnowintcp");
		decl_control_string ("srain_intcp_dynamic", "dynsrainintcp");
		decl_control_string ("wrain_intcp_dynamic", "dynwrainintcp");
		decl_control_string ("imperv_frac_dynamic", "dynimperv");
		decl_control_string ("imperv_stor_dynamic", "dynimperv");
		decl_control_string ("covtype_dynamic", "dyncovtype");
		decl_control_string ("jhcoef_dynamic", "dynjhcoef");
		decl_control_string ("potet_coef_dynamic", "dynpotetcoef");
		decl_control_string ("transpbeg_dynamic", "dyntranspbeg");
		decl_control_string ("transpend_dynamic", "dyntranspend");
		decl_control_string ("soilrechr_dynamic", "dynsoilrechr");
		decl_control_string ("soilmoist_dynamic", "dynsoilmoist");
		decl_control_string ("radtrncf_dynamic", "dynradtrncf");
		decl_control_string ("csv_output_file", "prms_summary.csv");
/*
**	run start and end times
*/
        decl_control_int_array("start_time", 6, start_date);
        decl_control_int_array("end_time", 6, end_date);

/*
**	flag for initializing vars from file
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("init_vars_from_file", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("save_vars_to_file", 1, lval);

/*
**	initial delta-t - hours
*/
        fval = (float *)umalloc (sizeof (float));
		fval[0] = 24.0;
        decl_control_float_array ("initial_deltat", 1, fval);

/*
**	stats analysis
*/
//      cp = (char **)umalloc (sizeof (char *) * MAXSTATVARS);
//      for (i = 0; i < MAXSTATVARS; i++) *(cp+i) = strdup ("inactive");
//      decl_control ("statVar_names", M_STRING, MAXSTATVARS, cp);
//      cp = (char **)umalloc (sizeof (char *) * MAXSTATVARS);
//      for (i = 0; i < MAXSTATVARS; i++) *(cp+i) = strdup ("-1");
//      decl_control ("statVar_element", M_STRING, MAXSTATVARS, cp);
//
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("statsON_OFF", 1, lval);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("nstatVars", 1, lval);

/*
**	animation output
*/
//      for (i = 0; i < MAXSTATVARS; i++) cp[i] = strdup ("inactive");
//      decl_control ("aniOutVar_names", M_STRING, MAXSTATVARS, cp);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("aniOutON_OFF", 1, lval);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("naniOutVars", 1, lval);

/*
**	map output
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("mapOutON_OFF", 1, lval);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("nmapOutVars", 1, lval);

/*
**	graphics display
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("ndispGraphs", 1, lval);
//      decl_control_string ("dispVar_names", "inactive");
//      decl_control_string ("dispVar_element", "-1");

//      lval = -1;
//      decl_control_int_array ("dispVar_plot", 1, &lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 50;
        decl_control_int_array ("dispGraphsBuffSize", 1, lval);

// CSV output
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("csvON_OFF", 1, lval);
/*
**  Env file
*/
/*
	err = read_control (MAltContFile);
	if (err) {
           (void)fprintf (stderr,"%s\n", err);
           exit (1);
        }
*/

/*
        if (MAltEnvFile == NULL) MAltEnvFile = strdup (*control_svar ("env_file"));
*/
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/**8************************** TEST DRIVER ****************************/
