@ECHO OFF

..\..\bin\prms .\control\Tazlina.control -print
java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\Tazlina.par .\control\Tazlina.control.par_name
