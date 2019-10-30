@echo off
echo =========================================
echo Fortran 2008 v2 (gfortran 8.1)
echo =========================================
for /L %%A IN (200,200,4000) do (
  penna 100 100 %%A
)
pause