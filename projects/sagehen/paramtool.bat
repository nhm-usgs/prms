@ECHO OFF
..\..\bin\prms -C.\control\sagehen.control -print
java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\sagehen.params .\control\sagehen.control.par_name
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
