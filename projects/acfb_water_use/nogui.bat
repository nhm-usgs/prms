@ECHO OFF
..\..\bin\prms .\control\control > .\output\run.log
move water_use.out .\output
ECHO.
ECHO Run complete. Press any key to exit.
PAUSE>NUL
