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
.PHONY: build
build: $(BINDIR)/$(TARGET) clean

# Depedencies only
.PHONY: dep
dep: $(DEPS)
	

# Link together object files.
$(BINDIR)/$(TARGET): $(OBJS)
	@echo "Assembling object to binary."
	@$(FC) $(SPEC_FFLAGS) $^ -o $(BINDIR)/$(TARGET)

-include $(DEPS)

%.o: %.f90 %.d
	@printf "\tCompiling $<\n"
	@$(FC) $(FFLAGS) $(FDEP) -c $< -o $@ -J$(MODDIR) -I$(MODDIR)

%.d: %.f90
	@$(FC) $(FFLAGS) $(FDEP) $(FSYNTCHK) -S $< -J$(MODDIR) -I$(MODDIR)

# Remove unneeded files for building.
.PHONY: clean
clean:
	@$(RM) *.o *.d $(VAR_FILE)
