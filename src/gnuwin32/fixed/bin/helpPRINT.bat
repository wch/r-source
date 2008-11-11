@echo off
rem This file needs to be customised.
rem
rem arguments are the file name (in the current directory)
rem the topic name
rem the path to Rd.sty (using / as path separator)
rem
echo You must customize R_HOME/bin/helpPRINT.bat
pause
rem
rem For MiKTeX, something like
rem pdflatex --interaction=nonstopmode --include-directory=%3  %1
rem del %1.tex %1.log %1.aux
rem
rem Or using fptex,
rem set TEXINPUTS=.;%3;
rem latex \nonstopmode\input{%1}
rem dvips -Ppfb %1
rem gsview32.exe %1.ps
rem del %1.tex %1.log %1.aux %1.dvi %1.ps
