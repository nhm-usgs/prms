@ECHO OFF

..\..\bin\prms .\control\sagehen.control -print
java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\sagehen.params .\control\sagehen.control.par_name
