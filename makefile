# Compiler choice.
FCOMPILER=gfortran-8

# Compiler options.
# 	For debugging.
# FLAGS=-g -Wall -Ofast -march=native -fcheck=all
#	Standard flags.
FLAGS=-Ofast -march=native

# File extension. *.f08 - Fortran 2008 specific
# This could be changed to .f95 or .f03 if need be.
FILEEXT=f08

# Directories.
SRC_DIR=src
OBJ_DIR=obj

# Files.
OUT:=penna.out
OBJECT=$(wildcard $(OBJ_DIR)/*.o)

# Generic compile command.
COMPILE_COMM=$(FCOMPILER) $(FLAGS) -J $(OBJ_DIR)/ -I $(OBJ_DIR)/

# Source code with no external dependencies.
NAME_DEP0:=init ticker
COMPILE_DEP0:=$(foreach name, $(NAME_DEP0), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).$(FILEEXT) -o $(OBJ_DIR)/$(name).o;)

# Source code with 1 ext dep.
NAME_DEP1:=gene update_array rand_int
COMPILE_DEP1:=$(foreach name, $(NAME_DEP1), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).$(FILEEXT) -o $(OBJ_DIR)/$(name).o;)

# Source code with 2 ext dep.
NAME_DEP2:=stat save
COMPILE_DEP2:=$(foreach name, $(NAME_DEP2), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).$(FILEEXT) -o $(OBJ_DIR)/$(name).o;)

# Source code with 3 or more ext dep.
NAME_DEP3:=population penna
COMPILE_DEP3:=$(foreach name, $(NAME_DEP3), $(COMPILE_COMM) \
	-c $(SRC_DIR)/$(name).$(FILEEXT) -o $(OBJ_DIR)/$(name).o;)

# Final source code to compile.
NAME_MAIN:=main
COMPILE_MAIN:=$(COMPILE_COMM) -c $(SRC_DIR)/$(NAME_MAIN).$(FILEEXT) \
	-o $(OBJ_DIR)/$(NAME_MAIN).o


# Build and then link.
default: build
	@echo Linking together the object files.
	@$(COMPILE_COMM) $(OBJECT) -o $(OUT)

# Build but do not link yet.
build: dep3
	@echo Compiling '$(NAME_MAIN).$(FILEEXT)'
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

dep0:
	@echo Compiling first set.
	@$(COMPILE_DEP0)

# Remove *.mod and *.o files in obj/.
clean:
	rm $(OBJ_DIR)/*.mod $(OBJ_DIR)/*.o
