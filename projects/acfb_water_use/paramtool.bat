@ECHO OFF

..\..\bin\prms .\control\control -print
java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\params .\control\control.par_name
