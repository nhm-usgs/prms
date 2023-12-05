@ECHO OFF
..\..\bin\prms -C.\control\mercdIDE.control -print
java -cp ..\..\dist\oui4.jar oui.mms.gui.Mms .\control\mercdIDE.control
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
