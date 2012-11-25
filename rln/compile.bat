@ echo off
del rln.exe
del rln.d
del rlng.d
c:\f77\bin\Fl -c fln.for  
c:\f77\bin\Fl -c rln4.for   
c:\f77\bin\Fl -c ln24.for   
c:\f77\bin\Fl -c ln34.for 
c:\f77\bin\Fl -c ln44.for  
set lib=c:\f77\lib
c:\f77\bin\LINK rln4+ln24+ln34+ln44+fln
rem del *.obj
del fln.obj
:end

