# Variable file
VAR_FILE :=compile.variables

# Directories
BINDIR :=../bin
LIBDIR :=./third-party

# File name lists.
SRCS :=$(wildcard *.f90)
OBJS :=$(SRCS:.f90=.o)
DEPS :=$(SRCS:.f90=.d)
TARGET :=penna.out

-include $(VAR_FILE)

default: default_build

compile_lib:
	@echo "FC :=$(FC)\nFFLAGS := $(FFLAGS)\nFSYNTCHK := $(FSYNTCHK)\nFDEP := $(FDEP)"\
		 > $(LIBDIR)/compile.variables

# Default build (release build).
default_build: FFLAGS += -Ofast
default_build: $(BINDIR)/$(TARGET)
default_build: clean

# Debug build.
debug_build: FFLAGS += -g -Wall -fcheck=all
debug_build: LIB_BLD_RULE += debug_build
debug_build: $(BINDIR)/$(TARGET)
debug_build: clean

# Link together object files.
$(BINDIR)/$(TARGET): $(OBJS)
	@echo Link together object files.
	@$(FC) $(FFLAGS) $^ -o $(BINDIR)/$(TARGET)

-include $(DEPS)

%.o %.d: %.f90
	@echo Compile $<...
	@$(FC) $(FFLAGS) $(FDEP) -c $<

clean:
	@$(RM) *.o *.d $(VAR_FILE)
