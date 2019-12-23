# Fortran compiler
FC :=gfortran-9

# Compiler flags
FFLAGS = -std=f2008 -march=native
FSYNTCHK := -fsyntax-only
FDEP := -cpp -MD

# Source directory
SRCDIR :=src
LIBDIR :=$(SRCDIR)/third-party

# Variable file name
VAR_FILE :=compile.variables

default: compile_var
	@$(MAKE) -C $(LIBDIR)
	@$(MAKE) -C $(SRCDIR)

debug_build: compile_var
	@$(MAKE) debug_build -C $(LIBDIR)
	@$(MAKE) debug_build -C $(SRCDIR)

compile_var:
	@echo "FC :=$(FC)\nFFLAGS := $(FFLAGS)\nFSYNTCHK := $(FSYNTCHK)\nFDEP := $(FDEP)"\
		 > $(SRCDIR)/$(VAR_FILE)

# Clean dependency and object files.
clean:
	@$(RM) -f $(SRCDIR)/*.d $(SRCDIR)/*.o $(SRCDIR)/$(VAR_FILE)

# Clean module and submodule files.
clean_mod:
	@$(RM) -f $(SRCDIR)/*.mod $(SRCDIR)/*.smod

# Run test script
.PHONY: test
test:
	@test/test.sh
