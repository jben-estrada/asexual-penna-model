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
FC:=gfortran-9
# Compiler type
CMPTYPE:=gnu

# Compile flags.
# Initial '"' character for FoBiS.py (ultimately python) to read the compile
# flags.
FCFLAGS:=" -c -std=f2008 -Wall -Wextra -fcheck=all -march=native -pedantic
BLD_SPEC_FLAG=

# FoBiS.py build options.
DIRFLAGS:= -s ${SRC_DIR} -dobj $(OBJ_DIR) -dmod $(MOD_DIR)
BLDFLAG= build -fc $(FC) -compiler $(CMPTYPE) -o $(BIN_DIR)/$(OUT) \
	$(DIRFLAGS) -colors -cflags $(FCFLAGS) $(BLD_SPEC_FLAG)

.PHONY: default help clean clean_mod default_build debug_build static_build

default: help

help:
	@echo "Usage:"
	@echo " make [rules]"
	@echo " make [help]"
	@echo ""
	@echo "Build the Penna model program with FoBiS.py"
	@echo ""
	@echo "Please run 'make' with one of the following rules:"
	@echo "  default_build - Build the program. "
	@echo "  debug_build   - Build the program. "
	@echo "  static_build  - Build the program. "
	@echo "  clean         - Remove *.o files."
	@echo "  clean_mod     - Remove *.mod and *.smod files."
	@echo ""
	@echo "  help          - Show this help message."

# Default build.
default_build: BLD_SPEC_FLAG += -Ofast -g"
default_build: build

# Debug build.
debug_build: BLD_SPEC_FLAG += -O0 -g"
debug_build: build

# Static build.
static_build: BLD_SPEC_FLAG += -O3 -g -static"
static_build: build

build:
	@$(FOBIS) $(BLDFLAG)

clean:
	@echo "Removing $(OBJ_DIR)/*.o files"
	@$(RM) $(OBJ_DIR)/*.o

clean_mod:
	@echo "Removing $(MOD_DIR)/*.(s)mod files"
	@$(RM) $(MOD_DIR)/*.mod $(MOD_DIR)/*.smod
