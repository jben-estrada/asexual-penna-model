# Compiler choice.
FCOMPILER=gfortran-9

# Compiler options.
FLAGS= -std=f2008 -march=native

# Directories.
SRC_DIR=src
OBJ_DIR=obj
BIN_DIR=bin

# Files.
OUT:=$(BIN_DIR)/penna.out
OBJECT=$(wildcard $(OBJ_DIR)/*.o)

# Generic compile command.
COMPILE_COMM=$(FCOMPILER) $(FLAGS) -J $(OBJ_DIR)/ -I $(OBJ_DIR)/

# Source code with no external dependencies.
NAME_DEP0:=init tickertype rand_int update_array gene saveformat
COMPILE_DEP0=$(foreach name, $(NAME_DEP0), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).f08 -o $(OBJ_DIR)/$(name).o;)

# Source code with 1 ext dep from dep0.
NAME_DEP1:=writertype demographics
COMPILE_DEP1=$(foreach name, $(NAME_DEP1), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).f08 -o $(OBJ_DIR)/$(name).o;)

# Source code with at most 2 ext dep from dep0 and dep1.
NAME_DEP2:=population penna
COMPILE_DEP2=$(foreach name, $(NAME_DEP2), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).f08 -o $(OBJ_DIR)/$(name).o;)

# Source code with 3 or more ext dep from dep0, dep1 and dep2.
NAME_DEP3:=cmd_interface
COMPILE_DEP3=$(foreach name, $(NAME_DEP3), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).f08 -o $(OBJ_DIR)/$(name).o;)

# Final source code to compile depending on all other codes.
NAME_MAIN:=main
COMPILE_MAIN=$(COMPILE_COMM) -c $(SRC_DIR)/$(NAME_MAIN).f08 \
	-o $(OBJ_DIR)/$(NAME_MAIN).o


# Release build.
default: release_option

# Debug build.
debug: debug_option

debug_option: FLAGS += -g -Wall -fcheck=all -O0
debug_option: build

release_option: FLAGS += -Ofast
release_option: build

# Compile and link
build: compile
	@echo Linking together the object files.
	@$(COMPILE_COMM) $(OBJECT) -o $(OUT)

# Compile but do not link yet.
compile: dep3
	@echo Compiling '$(NAME_MAIN).f08'
	@$(COMPILE_MAIN)

dep3: dep2
	@echo Compiling fourth set.
	@$(COMPILE_DEP3)

dep2: dep1
	@echo Compiling third set.
	@$(COMPILE_DEP2)

dep1: dep0
	@echo Compiling second set.
	@$(COMPILE_DEP1)

dep0: clean
	@echo Compiling first set.
	@$(COMPILE_DEP0)

# Remove *.mod and *.o files in obj/.
clean:
	@rm -f $(OBJ_DIR)/*.mod $(OBJ_DIR)/*.smod $(OBJ_DIR)/*.o

.PHONY: test
test: 
	@test/test.sh
