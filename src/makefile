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

# Depedencies only
dep: $(DEPS)
	

# Link together object files.
$(BINDIR)/$(TARGET): $(OBJS)
	@printf "\n\tLinking together object files.\n"
	@$(FC) $(SPEC_FFLAGS) $^ -o $(BINDIR)/$(TARGET)

-include $(DEPS)

%.o: %.f90 %.d
	@printf "\tCompiling $<\n"
	@$(FC) $(FFLAGS) $(FDEP) -c $< -o $@

%.d: %.f90
	@printf "\tGetting the dependencies of $<\n"
	@$(FC) $(FFLAGS) $(FDEP) -fsyntax-only -S $<

# Remove unneeded files for building.
.PHONY: clean
clean:
	@$(RM) *.o *.d $(VAR_FILE)
