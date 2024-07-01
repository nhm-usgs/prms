@ECHO OFF

..\..\bin\prms .\control\mercdIDE.control -print
java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\mercdIDE.param .\control\mercdIDE.control.par_name
