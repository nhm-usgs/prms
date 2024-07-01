@ECHO OFF
ECHO.
ECHO When simulation finishes, enter control-C in this window.
ECHO and then enter Y when prompted.
ECHO.
ECHO Press any key to continue.
PAUSE>NUL

..\..\bin\prms .\control\sagehen.control -print
java -cp ..\..\dist\oui4.jar oui.mms.gui.Mms .\control\sagehen.control
ECHO.
ECHO Run complete. Press any key to exit.
PAUSE>NUL
