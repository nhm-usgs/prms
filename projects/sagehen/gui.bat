@ECHO OFF
..\..\bin\prms -C.\control\sagehen.control -print
java -cp ..\..\dist\oui4.jar oui.mms.gui.Mms .\control\sagehen.control
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
