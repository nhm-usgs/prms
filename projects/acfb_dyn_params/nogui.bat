@ECHO OFF
..\..\bin\prms .\control\control > .\output\run.log
copy dynamic_soil_parameter.out .\output
del dynamic_soil_parameter.out
ECHO.
ECHO Run complete. Press any key to exit.
PAUSE>NUL
