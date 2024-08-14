@ECHO OFF
..\..\bin\prms .\control\control > .\output\run.log
move dynamic_soil_parameter.out .\output
ECHO.
ECHO Run complete. Press any key to exit.
PAUSE>NUL
