## ${R_HOME}/share/make/winshlib.mk

## included after $(R_HOME)/etc${R_ARCH}/Makeconf
## for both standard and add-on packages.

all: $(SHLIB)

BASE = $(shell basename $(SHLIB) .dll)

ADDQU = 's/[^ ][^ ]*/"&"/g'

## do it with explicit rules as packages might add dependencies to this target
## (attempts to do this GNUishly failed for parallel makes,
## but we do want the link targets echoed)
$(SHLIB): $(OBJECTS)
	@if test "z$(OBJECTS)" != "z"; then \
	  if test -e "$(BASE)-win.def"; then \
	    echo $(SHLIB_LD) $(SHLIB_LDFLAGS) $(DLLFLAGS) -o $@ $(BASE)-win.def $(OBJECTS) $(ALL_LIBS); \
	    $(SHLIB_LD) $(SHLIB_LDFLAGS) $(DLLFLAGS) -o $@ $(BASE)-win.def $(OBJECTS) $(ALL_LIBS); \
	  else \
	    echo EXPORTS > tmp.def; \
	    $(NM) $^ | $(SED) -n $(SYMPAT) $(NM_FILTER) | $(SED) $(ADDQU)  >> tmp.def; \
	    echo $(SHLIB_LD) $(SHLIB_LDFLAGS) $(DLLFLAGS) -o $@ tmp.def $(OBJECTS) $(ALL_LIBS); \
	    $(SHLIB_LD) $(SHLIB_LDFLAGS) $(DLLFLAGS) -o $@ tmp.def $(OBJECTS) $(ALL_LIBS); \
	    $(RM) tmp.def; \
	  fi \
	fi

.PHONY: all shlib-clean compilers
shlib-clean:
	@rm -f $(OBJECTS) symbols.rds

## FIXME: why not Rscript?
symbols.rds: $(OBJECTS)
	@$(ECHO) "tools:::.shlib_objects_symbol_tables()" | \
	  $(R_HOME)/bin$(R_ARCH)/Rterm.exe --vanilla --no-echo --args $(OBJECTS)

compilers:
	@$(ECHO) "CC = $(CC)"
	@$(ECHO) "CXX = $(CXX)"
	@$(ECHO) "FC = $(FC)"
