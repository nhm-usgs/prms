@ECHO OFF

rm ../output/*.csv
rm ../output/*.out

..\..\..\bin\prms .\prms.control -set init_vars_from_file 0 -set save_vars_to_file 0 > ..\output\run.log

..\..\..\bin\prms .\prms.control -set end_time 1983,9,1,0,0,0 -set csv_output_file ..\output\prms_summary1.out -set model_output_file ..\output\prms1.out -set basinOutBaseFileName ..\output\basinOut_1 -set init_vars_from_file 0 -set var_save_file ..\output\prms_ic_1 
..\..\..\bin\prms .\prms.control -set start_time 1983,09,02,0,0 -set csv_output_file ..\output\prms_summary2.out -set end_time 1983,09,08,0,0,0 -set model_output_file ..\output\prms2.out -set basinOutBaseFileName ..\output\basinOut_2 -set var_init_file ..\output\prms_ic_1 -set var_save_file ..\output\prms_ic_2
..\..\..\bin\prms .\prms.control -set start_time 1983,09,09,0,0 -set csv_output_file ..\output\prms_summary3.out -set end_time 1983,09,15,0,0,0 -set model_output_file ..\output\prms3.out -set basinOutBaseFileName ..\output\basinOut_3 -set var_init_file ..\output\prms_ic_2 -set var_save_file ..\output\prms_ic_3
..\..\..\bin\prms .\prms.control -set start_time 1983,09,16,0,0 -set csv_output_file ..\output\prms_summary4.out -set end_time 1983,09,22,0,0,0 -set model_output_file ..\output\prms4.out -set basinOutBaseFileName ..\output\basinOut_4 -set var_init_file ..\output\prms_ic_3 -set var_save_file ..\output\prms_ic_4
..\..\..\bin\prms .\prms.control -set start_time 1983,09,23,0,0 -set csv_output_file ..\output\prms_summary5.out -set end_time 1983,09,29,0,0,0 -set model_output_file ..\output\prms5.out -set basinOutBaseFileName ..\output\basinOut_5 -set var_init_file ..\output\prms_ic_4 -set var_save_file ..\output\prms_ic_5
..\..\..\bin\prms .\prms.control -set start_time 1983,09,30,0,0 -set csv_output_file ..\output\prms_summary6.out -set end_time 1983,10,06,0,0,0 -set model_output_file ..\output\prms6.out -set basinOutBaseFileName ..\output\basinOut_6 -set var_init_file ..\output\prms_ic_5 -set var_save_file ..\output\prms_ic_6
..\..\..\bin\prms .\prms.control -set start_time 1983,10,07,0,0 -set csv_output_file ..\output\prms_summary7.out -set end_time 1983,10,13,0,0,0 -set model_output_file ..\output\prms7.out -set basinOutBaseFileName ..\output\basinOut_7 -set var_init_file ..\output\prms_ic_6 -set var_save_file ..\output\prms_ic_7
..\..\..\bin\prms .\prms.control -set start_time 1983,10,14,0,0 -set csv_output_file ..\output\prms_summary8.out -set end_time 1983,10,20,0,0,0 -set model_output_file ..\output\prms8.out -set basinOutBaseFileName ..\output\basinOut_8 -set var_init_file ..\output\prms_ic_7 -set var_save_file ..\output\prms_ic_8
..\..\..\bin\prms .\prms.control -set start_time 1983,10,21,0,0 -set csv_output_file ..\output\prms_summary9.out -set end_time 1983,10,27,0,0,0 -set model_output_file ..\output\prms9.out -set basinOutBaseFileName ..\output\basinOut_9 -set var_init_file ..\output\prms_ic_8 -set var_save_file ..\output\prms_ic_9
..\..\..\bin\prms .\prms.control -set start_time 1983,10,28,0,0 -set csv_output_file ..\output\prms_summary10.out -set end_time 1983,11,03,0,0,0 -set model_output_file ..\output\prms10.out -set basinOutBaseFileName ..\output\basinOut_10 -set var_init_file ..\output\prms_ic_9 -set var_save_file ..\output\prms_ic_10
..\..\..\bin\prms .\prms.control -set start_time 1983,11,04,0,0 -set csv_output_file ..\output\prms_summary11.out -set end_time 1983,11,10,0,0,0 -set model_output_file ..\output\prms11.out -set basinOutBaseFileName ..\output\basinOut_11 -set var_init_file ..\output\prms_ic_10 -set var_save_file ..\output\prms_ic_11
..\..\..\bin\prms .\prms.control -set start_time 1983,11,11,0,0 -set csv_output_file ..\output\prms_summary12.out -set end_time 1983,11,17,0,0,0 -set model_output_file ..\output\prms12.out -set basinOutBaseFileName ..\output\basinOut_12 -set var_init_file ..\output\prms_ic_11 -set var_save_file ..\output\prms_ic_12
..\..\..\bin\prms .\prms.control -set start_time 1983,11,18,0,0 -set csv_output_file ..\output\prms_summary13.out -set end_time 1983,11,24,0,0,0 -set model_output_file ..\output\prms13.out -set basinOutBaseFileName ..\output\basinOut_13 -set var_init_file ..\output\prms_ic_12 -set var_save_file ..\output\prms_ic_13
..\..\..\bin\prms .\prms.control -set start_time 1983,11,25,0,0 -set csv_output_file ..\output\prms_summary14.out -set end_time 1983,12,01,0,0,0 -set model_output_file ..\output\prms14.out -set basinOutBaseFileName ..\output\basinOut_14 -set var_init_file ..\output\prms_ic_13 -set var_save_file ..\output\prms_ic_14
..\..\..\bin\prms .\prms.control -set start_time 1983,12,02,0,0 -set csv_output_file ..\output\prms_summary15.out -set model_output_file ..\output\prms15.out -set basinOutBaseFileName ..\output\basinOut_15 -set var_init_file ..\output\prms_ic_14 -set var_save_file ..\output\prms_ic_15



ECHO.
ECHO Run complete. Please press enter when you want to continue.
PAUSE>NUL