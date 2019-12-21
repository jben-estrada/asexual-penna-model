# Source directory
SRCDIR :=src

default:
	@$(MAKE) -C $(SRCDIR)

debug_build:
	@$(MAKE) debug -C $(SRCDIR)

# Clean dependency and object files.
clean:
	@$(RM) -f $(SRCDIR)/*.d $(SRCDIR)/*.o

# Clean module and submodule files.
clean_mod:
	@$(RM) -f $(SRCDIR)/*.mod $(SRCDIR)/*.smod

# Run test script
.PHONY: test
test:
	@test/test.sh
