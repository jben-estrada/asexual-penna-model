# File names
SRC_LIST :=source_list
LIB_SRCS :=$(wildcard *.f90)

all:
	@echo usage:
	@echo "\tRun with 'copy_lib' rule to copy library to ./src"
	@echo "\tRun with 'remove_lib' rule to remove library in ./src\n"

# Copy third-party library into the main source directory
copy_lib: $(LIB_SRCS)
	@$(foreach file,$^, cp $(file) ../;)

# Remove third-party library into the main source directory
remove_lib: $(addprefix ../,$(LIB_SRCS))
	@$(foreach file,$^, rm $(file);)
