# Variable file
VAR_FILE :=makefile.variables

# Directories
BINDIR :=../bin
LIBDIR :=./third-party

include $(VAR_FILE)

# File name lists.
SRCS :=$(wildcard *.f90)
OBJS :=$(SRCS:.f90=.o)
DEPS :=$(SRCS:.f90=.d)
TARGET :=penna.out

# Create directory for executable.
$(shell mkdir -p $(BINDIR))

# Build (release build).
build: $(BINDIR)/$(TARGET) clean

# Link together object files.
$(BINDIR)/$(TARGET): $(OBJS)
	@echo Link together object files.
	@$(FC) $(SPEC_FFLAGS) $^ -o $(BINDIR)/$(TARGET)

-include $(DEPS)

%.o %.d: %.f90
	@echo Compile $<...
	@$(FC) $(FFLAGS) $(FDEP) -c $<

# Remove unneeded files for building.
.PHONY: clean
clean:
	@$(RM) *.o *.d $(VAR_FILE)
