@ECHO OFF

..\..\bin\prms .\control\mercdXYZ.control -print
java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\mercdXYZ.param .\control\mercdXYZ.control.par_name
