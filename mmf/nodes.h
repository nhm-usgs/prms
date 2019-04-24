typedef struct node_type {
  char  *name;
  int   x, y;
  int   num_connectnions;
} NODE;

NODE nodes[] = {
   {"/prms/basin_prms.f", 120, 87, 0},
   {"/prms/obs_prms.f", 350, 30, 0},
   {"/prms/soltab_prms.f", 113, 279, 0},
   {"/prms/xyz_dist.f", 351, 103, 5},
   {"/prms/ddsolrad_prms.f", 115, 192, 6},
   {"/prms/potet_jh_prms.f", 350, 173, 6},
   {"/prms/intcp_prms.f", 353, 243, 12},
   {"/prms/snowcomp_prms.f", 354, 318, 18},
   {"/prms/srunoff_smidx_prms.f", 355, 387, 15},
   {"/prms/smbal_prms.f", 359, 456, 10},
   {"/prms/ssflow_prms.f", 361, 524, 2},
   {"/prms/gwflow_prms.f", 365, 594, 4},
   {"/prms/strmflow_prms.f", 590, 388, 6},
   {"/prms/hru_sum_prms.f", 590, 255, 23},
   {"/prms/basin_sum_prms.f", 591, 148, 29},
   NULL
};

int  con_index[] = {
   1, 1, 1, 1, 1,
   2, 2, 2, 3, 1, 3,
   3, 3, 3, 3, 4, 4,
   3, 3, 3, 3, 5, 3, 3, 4, 0, 1, 5, 7,
   1, 4, 4, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 6,
   1, 6, 6, 3, 6, 3, 9, 5, 7, 7, 7, 7, 7, 0, 0,
   7, 5, 6, 7, 5, 8, 8, 0, 0, 3,
   0, 9,
   0, 10, 9, 10,
   8, 11, 10, 10, 11, 8,
   5, 4, 3, 3, 3, 6, 6, 6, 5, 9, 9, 8, 8, 7, 7, 7, 7, 7, 7, 6, 9, 7, 9,
   9, 11, 10, 1, 3, 9, 6, 7, 8, 9, 6, 11, 10, 8, 11, 10, 4, 6, 5, 7, 7, 3, 3, 3, 3, 9, 12, 12, 1   
};
