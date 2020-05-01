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

.PHONY: default_build static_build dep move_lib remove_lib clean clean_mod

# Default build.
default_build: FFLAGS += -g -Ofast
default_build: build

# Debug build.
debug_build: FFLAGS += -g -O0
debug_build: build

# Static build.
static_build: FFLAGS += -O3 -static
static_build: build

# Get dependencies.
dep: compile_var move_lib
	@echo Obtaining only the dependency files.
	@$(MAKE) dep -C $(SRCDIR) --no-print-directory
	@echo Removing .variable file.
	@$(RM) $(SRCDIR)/$(VAR_FILE)

# Move library source code to ./src/
move_lib:
	@echo Moving third-party library source code into $(SRCDIR).
	@$(MAKE) copy_lib -C $(LIBDIR) --no-print-directory

remove_lib:
	@$(MAKE) remove_lib -C $(LIBDIR) --no-print-directory

# Build (default).
build: compile_var move_lib
	@echo Compiling source code.
	@$(MAKE) -C $(SRCDIR) --no-print-directory
	@echo Removing third-party library source code in $(SRCDIR).
	@$(MAKE) remove_lib -C $(LIBDIR) --no-print-directory

# Create external variable file to share variables.
compile_var:
	@echo "FC :=$(FC)" >> $(SRCDIR)/$(VAR_FILE)
	@echo "FFLAGS = $(FFLAGS)" >> $(SRCDIR)/$(VAR_FILE)
	@echo "FSYNTCHK := $(FSYNTCHK)" >> $(SRCDIR)/$(VAR_FILE)
	@echo "FDEP := $(FDEP)" >> $(SRCDIR)/$(VAR_FILE)

# Clean dependency and object files.
clean:
	@$(RM) -f $(SRCDIR)/*.d $(SRCDIR)/*.o $(SRCDIR)/$(VAR_FILE)

# Clean module and submodule files.
clean_mod:
	@$(RM) -f $(SRCDIR)/*.mod $(SRCDIR)/*.smod
