@ECHO OFF
..\..\bin\prms -C.\control\control -print
java -cp ..\..\dist\oui4.jar oui.mms.gui.Mms .\control\control
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
