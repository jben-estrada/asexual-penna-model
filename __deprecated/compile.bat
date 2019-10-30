@echo off
gfortran -c init.f08 -o ./obj/a1.o -J./obj/
gfortran -c gene.f08 -o ./obj/a2.o -J./obj/
gfortran -c update_array.f08 -o ./obj/a3.o -J./obj/
gfortran -c rand_int.f08 -o ./obj/a4.o -J./obj/
gfortran -c population.f08 -o ./obj/a5.o -J./obj/
gfortran -c main.f08 -o ./obj/a6.o -J./obj/
gfortran ./obj/a1.o ./obj/a2.o ./obj/a3.o ./obj/a4.o ./obj/a5.o ./obj/a6.o -O3 -o penna.exe