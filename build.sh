#!/bin/bash
GF=gfortran-8
out="penna.exe"

# Compile .f08 files
j=1
for i in src/*.f08
do
  $GF -c $i -J obj/ -o obj/a$j.o
  j=$(($j + 1))
done

# Assemble together object files
assemblestr=$GF
for i in obj/*.o
do
  assemblestr+=" $i"
done
assemblestr+=" -o $out"

eval $assemblestr
echo "Building done.."
