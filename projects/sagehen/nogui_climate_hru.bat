@ECHO OFF

..\..\bin\prms .\control\climate_hru.control -set print_debug -1 > .\output\run.log
ECHO.
ECHO Run complete. Please press enter when you want to continue.
PAUSE>NUL