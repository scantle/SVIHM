@echo OFF
@setlocal
title Soil-Water Balance Model Test
:: Set bindir to location of bin directory containing EXEs
:: relative to *inside* the SWBM/MODFLOW directories
set bindir=..\..\bin

REM Run SWBM
cd SWBM
call %bindir%\SWBM.exe

echo Done!