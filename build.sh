#!/bin/bash
GF=gfortran-8
OUT="penna.out"
ARGS="-g -Wall -Ofast -fcheck=all"

# Compile .f08 files
j=1
for i in src/*.f08
do
  echo "Compiling '$i'..."
  $GF -c $i -J obj/ $ARGS -o obj/a$j.o
  j=$(($j + 1))
done

# Link together object files
assemblestr=$GF
for i in obj/*.o
do
  assemblestr+=" $i"
done
assemblestr+=" $ARGS -o $OUT"

echo "Linking object files together..."
eval $assemblestr && echo "Building done."
