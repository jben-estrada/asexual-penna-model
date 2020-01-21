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
	@echo "\tLink together object files."
	@$(FC) $(SPEC_FFLAGS) $^ -o $(BINDIR)/$(TARGET)

-include $(DEPS)

%.o: %.f90 %.d
	@echo "\tCompile $<..."
	@$(FC) $(FFLAGS) $(FDEP) -c $< -o $@

%.d: %.f90
	@-$(FC) $(FFLAGS) $(FDEP) -fsyntax-only -S $<

# Remove unneeded files for building.
.PHONY: clean
clean:
	@$(RM) *.o *.d $(VAR_FILE)
