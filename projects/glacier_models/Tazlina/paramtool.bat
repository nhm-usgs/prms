@ECHO OFF

..\..\..\bin\prms .\control\Tazlina.control -print

java -cp ..\..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\Tazlina.par .\control\Tazlina.control.par_name
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
