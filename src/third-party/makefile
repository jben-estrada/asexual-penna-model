LIB_SRCS :=$(wildcard *.f90)
LIB_OBJS :=$(LIB_SRCS:.f90=.o)
LIB_DEPS :=$(LIB_SRCS:.f90=.d)
LIB_MODS  =$(wildcard *.mod)

VAR_FILE :=../compile.variables

-include $(VAR_FILE)

all: default_build

default_build: FFLAGS += -Ofast
default_build: $(LIB_OBJS)
# default_build: move_files

debug_build: FFLAGS += -g -Wall -fcheck=all
debug_build: $(LIB_OBJS)
# debug_build: move_files

%.d %.o: %.f90
	@echo Compile third-party library $<...
	@$(FC) $(FFLAGS) $(FDEP) -I ../ -J ../ -o ../$@ -c $<

move_files: $(LIB_OBJS) $(LIB_DEPS) $(LIB_MODS)
	@$(foreach file,$^, mv $(file) $(addprefix ../,$(file));)
