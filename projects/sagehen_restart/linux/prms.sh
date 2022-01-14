#/bin/sh

echo
echo '############'
echo 'executing GSFLOW Sagehen PRMS-only example'
echo '############'
echo

../../../bin/gsflow ./prms.control -set init_vars_from_file 0 -set save_vars_to_file 0 -set model_output_file ../output/prms/prms_only.out

echo
echo '############'
echo 'Run complete'
echo '############'
echo

