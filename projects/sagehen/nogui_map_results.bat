@ECHO OFF
..\..\bin\prms .\control\map_results.control -set print_debug -1 > .\output\run.log
move recharge.monthly .\output
move recharge.yearly .\output
move recharge.total .\output
ECHO.
ECHO Run complete. Press any key to exit.
PAUSE>NUL
