@echo off
rem This file needs to be customised.
rem
echo "You must customize RHOME/bin/helpPRINT.bat
pause
rem
rem BDR's setup uses:
rem latex \nonstopmode\input{%1}
rem dvips -Ppfb %1
rem gsview32.exe %1.ps
rem rm %1.tex %1.aux %1.dvi %1.ps