#######################################
# Makefile for building with FoBiS.py
######################################

# FoBiS.py command.
FOBIS:=FoBiS.py

# Executable output.
OUT:=penna.out

# Directories
SRC_DIR:=./src
MOD_DIR:=./mod
OBJ_DIR:=./obj
BIN_DIR:=./bin

# Compiler
FC:=gfortran-10
# Compiler type
CMPTYPE:=gnu

# Compile flags.
# Initial '"' character for FoBiS.py (ultimately python) to read the compile
# flags.
FCFLAGS:=" -c -std=f2008 -Wall -Wextra -fcheck=all -march=native -pedantic
BLD_SPEC_FLAG=

# FoBiS.py build options: directories.
DIRFLAGS:= -s ${SRC_DIR} -dobj $(OBJ_DIR) -dmod $(MOD_DIR)
# FoBiS.py build options.
BLDFLAG= build -fc $(FC) -compiler $(CMPTYPE) -o $(BIN_DIR)/$(OUT) \
	$(DIRFLAGS) -colors -j 2 -cflags $(FCFLAGS) $(BLD_SPEC_FLAG)

.PHONY: default help clean clean_mod rel_build debug_build static_build rbld \
	dbld sbld

default: help

help:
	@echo "Please run 'make' with one of the following rules:"
	@echo "  rel_build     - Build the program (release build). "
	@echo "  debug_build   - Build the program (debug build). "
	@echo "  static_build  - Build the program (release build with staticly linked libs). "
	@echo "  rbld          - Alias to 'rel_build'. "
	@echo "  dbld          - Alias to 'debug_build'. "
	@echo "  sbld          - Alias to 'static_build'. "
	@echo "  clean         - Remove *.o files."
	@echo "  clean_mod     - Remove *.mod and *.smod files."
	@echo ""
	@echo "  help          - Show this help message."

# Release build.
rel_build: BLD_SPEC_FLAG += -Ofast -g"
# Alias to 'rel_build'
rbld: rel_build

# Debug build.
debug_build: BLD_SPEC_FLAG += -O0 -g"
# Alias to 'rel_build'
dbld: debug_build

# Static build.
static_build: BLD_SPEC_FLAG += -Ofast -g -static"
# Alias to 'static_build'
sbld: static_build

rel_build debug_build static_build: build clean

build:
	@$(FOBIS) $(BLDFLAG)

clean:
	@echo "Removing $(OBJ_DIR)/*.o files"
	@$(RM) $(OBJ_DIR)/*.o

clean_mod:
	@echo "Removing $(MOD_DIR)/*.(s)mod files"
	@$(RM) $(MOD_DIR)/*.mod $(MOD_DIR)/*.smod
