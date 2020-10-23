#/bin/sh

echo
echo '############'
echo 'executing GSFLOW Sagehen NWT integrated mode example'
echo '############'
echo

../../../bin/gsflow ./gsflow.control -set modflow_name ../input/modflow/sagehen_NWT.nam -set subbasin_flag 0 -set print_debug -2 -set gsflow_output_file ../output/gsflow_nwt.out

echo
echo '############'
echo 'Run complete'
echo '############'
echo

