@ECHO OFF

..\..\..\bin\prms .\control\gulkana.control -print -set print_debug -2

java -cp ..\..\..\dist\oui4.jar oui.mms.gui.Mms .\control\gulkana.control

ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
