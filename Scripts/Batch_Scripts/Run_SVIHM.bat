@echo OFF
@setlocal
title Scott Valley Integrated Hydrologic Model
:: Set bindir to location of bin directory containing EXEs
:: relative to *inside* the SWBM/MODFLOW directories
set bindir=..\..\bin
set start=%time%
::copy Drains_initial_m3day.txt Drains_m3day.txt

REM Run SWBM
cd SWBM
call %bindir%\SWBM.exe

REM Copy over new SWBM-generated MODFLOW files
cd ..\
xcopy SWBM\SVIHM.* MODFLOW /Y /I

REM Run MODFLOW
cd MODFLOW
call %bindir%\MODFLOW-NWT.exe SVIHM.nam

REM Update Starting Heads              
REM Old command: Rscript Update_SVIHM_Starting_Heads.R
:: Command line arguments: heads_file_name SP_to_use
call %bindir%\Update_Starting_Heads.exe SVIHM.hds 2

REM Update drain flows going into big slough
REM Old command: Rscript Update_SVIHM_Drain_Inflows.R
:: Reads in file Update_Drain_Inflows.in
call %bindir%\Update_Drain_Inflows.exe

:: First Run done...
set firstend=%time%
set options="tokens=1-4 delims=:.,"
for /f %options% %%a in ("%start%") do set start_h=%%a&set /a start_m=100%%b %% 100&set /a start_s=100%%c %% 100&set /a start_ms=100%%d %% 100
for /f %options% %%a in ("%firstend%") do set firstend_h=%%a&set /a firstend_m=100%%b %% 100&set /a firstend_s=100%%c %% 100&set /a firstend_ms=100%%d %% 100

REM re-run SWBM to update SFR inflows
cd ..\SWBM
call %bindir%\SWBM.exe

REM Copy over new SWBM-generated MODFLOW files
cd ..\
xcopy SWBM\SVIHM.* MODFLOW /Y /I

REM Run MODFLOW
cd MODFLOW
call %bindir%\MODFLOW-NWT.exe SVIHM.nam

set end=%time%

set /a hours=%firstend_h%-%start_h%
set /a mins=%firstend_m%-%start_m%
set /a secs=%firstend_s%-%start_s%
set /a ms=%firstend_ms%-%start_ms%
if %ms% lss 0 set /a secs = %secs% - 1 & set /a ms = 100%ms%
if %secs% lss 0 set /a mins = %mins% - 1 & set /a secs = 60%secs%
if %mins% lss 0 set /a hours = %hours% - 1 & set /a mins = 60%mins%
if %hours% lss 0 set /a hours = 24%hours%
if 1%ms% lss 100 set ms=0%ms%

echo ----------------------------------------------------------------
set /a totalsecs = %hours%*3600 + %mins%*60 + %secs% 
echo First Run time %hours%:%mins%:%secs%.%ms% (%totalsecs%.%ms%s total)

for /f %options% %%a in ("%start%") do set start_h=%%a&set /a start_m=100%%b %% 100&set /a start_s=100%%c %% 100&set /a start_ms=100%%d %% 100
for /f %options% %%a in ("%end%") do set end_h=%%a&set /a end_m=100%%b %% 100&set /a end_s=100%%c %% 100&set /a end_ms=100%%d %% 100

set /a hours=%end_h%-%start_h%
set /a mins=%end_m%-%start_m%
set /a secs=%end_s%-%start_s%
set /a ms=%end_ms%-%start_ms%
if %ms% lss 0 set /a secs = %secs% - 1 & set /a ms = 100%ms%
if %secs% lss 0 set /a mins = %mins% - 1 & set /a secs = 60%secs%
if %mins% lss 0 set /a hours = %hours% - 1 & set /a mins = 60%mins%
if %hours% lss 0 set /a hours = 24%hours%
if 1%ms% lss 100 set ms=0%ms%

:: mission accomplished
echo ----------------------------------------------------------------
set /a totalsecs = %hours%*3600 + %mins%*60 + %secs% 
echo Total SVIHM Run time %hours%:%mins%:%secs%.%ms% (%totalsecs%.%ms%s total)