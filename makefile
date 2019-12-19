# Fortran compiler.
FC := gfortran-9
TARGET_OUT :=penna.out

# Directories.
SRC_DIR := src
BIN_DIR := bin
TST_DIR := test

# Compiler flags for compiling source code.
FFLAGS = -std=f2008 -march=native -cpp -MD -I $(SRC_DIR)/ -J $(SRC_DIR)/

# File names.
SRCS :=$(shell find $(SRC_DIR)/ -name *.f90)
OBJS := $(SRCS:.f90=.o)
DEPS :=$(OBJS:.o=.d)


all: release_build

debug: debug_build $(BIN_DIR)/$(TARGET_OUT) clean

# Release build (default).
release_build: FFLAGS += -Ofast
release_build: $(BIN_DIR)/$(TARGET_OUT) clean

# Debug build.
debug_build: FFLAGS += -g -Wall -fcheck=all -O0
debug_build: $(BIN_DIR)/$(TARGET_OUT) clean

$(BIN_DIR)/$(TARGET_OUT): $(OBJS)
	@echo "> Link together object files."
	@$(FC) $(FFLAGS) $^ -o $@

# Include dependencies
-include $(DEPS)

$(SRC_DIR)/%.o: $(SRC_DIR)/%.f90
	@echo "> Compile $<"
	@$(FC) $(FFLAGS) -c -o $@ $<

# Generate .mod, .smod and .d files in src/ for VS Code Fortran linter.
mod:
	@$(foreach name, $(SRCS), \
			$(FC) $(FFLAGS) \
			 -fsyntax-only $(name) -o $(name:.f90=.o);)
	
# Run test script
.PHONY: test
test:
	@test/test.sh

.PHONY: clean
clean:
	@$(RM) $(OBJS) $(DEPS)
