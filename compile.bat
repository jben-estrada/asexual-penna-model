@echo off
gfortran -c main.f08 -o ./obj/a1.o 
gfortran -c init.f08 -o ./obj/a2.o 
gfortran -c gene.f08 -o ./obj/a3.o 
gfortran -c rand_int.f08 -o ./obj/a4.o 
gfortran -c population.f08 -o ./obj/a5.o 
gfortran -c stat.f08 -o ./obj/a6.o 
gfortran -c update_array.f08 -o ./obj/a7.o 
gfortran ./obj/a1.o ./obj/a2.o ./obj/a3.o ./obj/a4.o ./obj/a5.o ./obj/a6.o ./obj/a7.o  -o penna.exe