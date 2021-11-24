@ECHO OFF

..\..\bin\prms .\control\Tazlina.control -print -set print_debug -2

java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\Tazlina.par .\control\Tazlina.control.par_name
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
