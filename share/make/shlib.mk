## ${R_HOME}/share/make/shlib.mk

## This is included after $(R_HOME)/etc${R_ARCH}/Makeconf

SHLIB_LDFLAGS = $(SHLIB_LDFLAGS_R)

all: $(SHLIB)

$(SHLIB): $(OBJECTS)
	@if test  "z$(OBJECTS)" != "z"; then \
	  echo $(SHLIB_LINK) -o $@ $(OBJECTS) $(ALL_LIBS); \
	  $(SHLIB_LINK) -o $@ $(OBJECTS) $(ALL_LIBS); \
	fi

.PHONY: all shlib-clean compilers

shlib-clean:
	@rm -Rf .libs _libs
	@rm -f $(OBJECTS) symbols.rds


## FIXME: why not Rscript?
symbols.rds: $(OBJECTS)
	@$(ECHO) "tools:::.shlib_objects_symbol_tables()" | \
	  $(R_HOME)/bin/R --vanilla --no-echo --args $(OBJECTS)

compilers:
	@$(ECHO) "CC = $(CC)"
	@$(ECHO) "CXX = $(CXX)"
	@$(ECHO) "FC = $(FC)"
