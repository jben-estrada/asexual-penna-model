#!/bin/bash
GF=gfortran-8
out="penna.exe"

# Compile .f08 files
j=1
for i in src/*.f08
do
  echo "Compiling '$i'..."
  $GF -c $i -J obj/ -o obj/a$j.o -O3
  j=$(($j + 1))
done

# Link together object files
assemblestr=$GF
for i in obj/*.o
do
  assemblestr+=" $i"
done
assemblestr+=" -o $out -O3"

echo "Linking together the object files..."
eval $assemblestr && echo "Building done.."