#!/bin/bash
#######################################################
# 'FoBiS.py' BUILD SCRIPT
#######################################################

# Fortran compiler
COMP="gnu"
FC="gfortran-9"

# Output file.
OUT="penna.out"

# Directories
SRC_DIR="./src"
MOD_DIR="./mod"
OBJ_DIR="./obj"
BIN_DIR="./bin"
# Directory flags.
DIRFLAGS="-s $SRC_DIR -dobj $OBJ_DIR -dmod $MOD_DIR"

# Compile flags.
FCFLAGS=" -c -std=f2008 -Wall -Wextra -fcheck=all -march=native -pedantic"

# FoBiS.py command
FOBIS="FoBiS.py"

function build() {
  # Choose build type.
  case "$1" in
    "default") BLD_SPEC_FLAGS="-O3 -g";;
    "debug")   BLD_SPEC_FLAGS="-O0 -g";;
    "static")  BLD_SPEC_FLAGS="-O3 -g -static";;
    *)         BLD_SPEC_FLAGS="-O3 -g";;
  esac

  $FOBIS build -fc $FC -o $BIN_DIR/$OUT $DIRFLAGS -compiler $COMP -cflags \
  "$FCFLAGS $BLD_SPEC_FLAGS"
}

function clean() {
  # Choose build type.
  case "$1" in
    "all")      CLEAN_SPEC_FLAGS="";;
    "obj_only") CLEAN_SPEC_FLAGS="-only_obj";;
    "obj_trgt") CLEAN_SPEC_FLAGS="-only_target";;
    *)          CLEAN_SPEC_FLAGS="-only_obj";;
  esac

  $FOBIS clean -dobj $OBJ_DIR -dmod $MOD_DIR "$CLEAN_SPEC_FLAGS"
}

function test() {
  $FOBIS --version
}

function help_txt() {
  echo "Usage:"
  echo " $0 {build [options...]|clean [options...]|test}"
  echo " $0 [-h|--h]"
  echo ""
  echo "Build the 'Asexual Penna model' program with FoBiS.py"
  echo ""
  echo "Options:"
  echo " build - Build the program. "
  echo "         Options for 'build' are 'default', 'debug' and 'static'. [default: 'default']"
  echo " clean - Clean the mod and object files."
  echo "         Options for 'clean' are: 'all', 'obj_only' and 'obj_target'. [default: 'obj_only']"
  echo " test  - Print the version of 'FoBiS.py'. This is just for checking if FoBiS.py is installed."
  echo " -h, --help - Show this help message."
}

# Check the user's input.
case "$1" in
  "build")
    build $2
    echo ""
    clean
    ;;

  "clean") clean $2;;

  "test") test;;

  "--help") help_txt;;

  "-h") help_txt;;

  *) 
    echo "Please provide an input: (build, clean, test, -h / --help)"
    echo "Enter \"$0 -h\" to see the functions of the different inputs."
    ;;
esac
