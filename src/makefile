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

debug:
	@$(foreach name,$(OBJS),echo $(name);)

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


# Module dependencies
penna.mod: pop.mod writertype.mod modelparam.mod demographics.mod \
	flag.mod progbartype.mod
pop.mod: gene.mod flag.mod modelparam.mod randind.mod rng.mod
cmdinterface.mod: modelparam.mod penna.mod rng.mod
writertype.mod: updatearray.mod saveformat.mod
demographics.mod: gene.mod modelparam.mod
randind.mod: rng.mod
rng.mod: mtmod.mod
modelparam.mod:	
flag.mod:
progbartype.mod:
gene.mod:
updatearray.mod:
saveformat.mod:

.PHONY: clean
clean:
	@$(RM) *.o *.d
