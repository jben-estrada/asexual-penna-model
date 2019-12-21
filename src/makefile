# Fortran compiler
FC :=gfortran-9
TARGET :=penna.out

# Compiler flags
FFLAGS = -std=f2008 -march=native
FSYNTCHK := -fsyntax-only
FDEP := -cpp -MD

# Directories
BINDIR :=../bin

# File name lists.
SRCS :=$(wildcard *.f90)
OBJS :=$(SRCS:.f90=.o)
DEPS :=$(SRCS:.f90=.d)

all: default_build

# Default build (release build).
default_build: FFLAGS += -Ofast
default_build: $(BINDIR)/$(TARGET)
default_build: clean

# Debug build
debug_build: FFLAGS += -g -Wall -fcheck=all
debug_build: $(BINDIR)/$(TARGET)
debug_build: clean

# Link together object files
$(BINDIR)/$(TARGET): $(OBJS)
	@echo Link together object files.
	@$(FC) $(FFLAGS) $^ -o $(BINDIR)/$(TARGET)

-include $(DEPS)

%.o %.d: %.f90
	@echo Compile $<...
	@$(FC) $(FFLAGS) $(FDEP) -c $<


.PHONY: clean
clean:
	@$(RM) *.o *.d
