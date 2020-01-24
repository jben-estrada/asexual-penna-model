# Fortran compiler
FC :=gfortran-9

# Compiler flags
FFLAGS = -std=f2008 -Wall -Wextra -fcheck=all -march=native -pedantic
FSYNTCHK := -fsyntax-only
FDEP := -cpp -MD

# Source directory
SRCDIR :=src
LIBDIR :=$(SRCDIR)/third-party

# Variable file name
VAR_FILE :=makefile.variables

# Default build (release build).
default_build: FFLAGS += -Ofast
default_build: build

# Debug build.
debug_build: FFLAGS += -g -O0
debug_build: build

# Build (default).
build: compile_var
	@echo Moving third-party library source code into $(SRCDIR).
	@$(MAKE) copy_lib -C $(LIBDIR) --no-print-directory
	@echo Compiling source code.
	@$(MAKE) -C $(SRCDIR) --no-print-directory
	@echo Removing third-party library source code in $(SRCDIR).
	@$(MAKE) remove_lib -C $(LIBDIR) --no-print-directory

# Create external variable file to share variables.
compile_var:
	@printf "FC :=$(FC)\nFFLAGS = $(FFLAGS)\nFSYNTCHK := $(FSYNTCHK)\nFDEP := $(FDEP)"\
		 > $(SRCDIR)/$(VAR_FILE)

# Clean dependency and object files.
.PHONY: clean
clean:
	@$(RM) -f $(SRCDIR)/*.d $(SRCDIR)/*.o $(SRCDIR)/$(VAR_FILE)

# Clean module and submodule files.
.PHONY: clean_mod
clean_mod:
	@$(RM) -f $(SRCDIR)/*.mod $(SRCDIR)/*.smod
