@echo off
gfortran -c ./src/main.f08 -o ./obj/a1.o -J ./obj
gfortran -c ./src/init.f08 -o ./obj/a2.o -J ./obj
gfortran -c ./src/gene.f08 -o ./obj/a3.o  -J ./obj
gfortran -c ./src/rand_int.f08 -o ./obj/a4.o -J ./obj
gfortran -c ./src/population.f08 -o ./obj/a5.o -J ./obj
gfortran -c ./src/stat.f08 -o ./obj/a6.o -J ./obj
gfortran -c ./src/update_array.f08 -o ./obj/a7.o -J ./obj
gfortran -c ./src/ticker.f08 -o ./obj/a8.o -J ./obj
gfortran -c ./src/save.f08 -o ./obj/a9.o -J ./obj
gfortran ./obj/a1.o ./obj/a2.o ./obj/a3.o ./obj/a4.o ./obj/a5.o ./obj/a6.o^
 ./obj/a7.o ./obj/a8.o ./obj/a9.o -J ./obj -o penna.exe