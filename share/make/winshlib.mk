## ${R_HOME}/share/make/shlib.mk

include $(R_HOME)/etc${R_ARCH}/Makeconf

all: before $(SHLIB) after

BASE = $(shell basename $(SHLIB) .dll)

## do it this way as packages might add dependencies to this target
$(SHLIB): $(OBJECTS)
	@if test -e "$(BASE)-win.def"; then \
	  $(SHLIB_LD) -shared $(DLLFLAGS) -o $@ $(BASE)-win.def $(OBJECTS) $(ALL_LIBS); \
	else \
	  echo EXPORTS > tmp.def; \
	  $(NM) $^ | $(SED) -n 's/^.* [BCDRT] _/ /p' >> tmp.def; \
	  $(SHLIB_LD) -shared $(DLLFLAGS) -o $@ tmp.def $(OBJECTS) $(ALL_LIBS); \
	  $(RM) tmp.def; \
	fi

.PHONY: all before after shlib-clean
shlib-clean:
	@rm -f $(OBJECTS)
