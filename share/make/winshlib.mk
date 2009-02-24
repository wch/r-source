## ${R_HOME}/share/make/winshlib.mk

include $(R_HOME)/etc${R_ARCH}/Makeconf

all: before $(SHLIB) after

BASE = $(shell basename $(SHLIB) .dll)

## do it with explict rules as packages might add dependencies to this target
## and do it GNUishly to get commands echoed
ifeq ($(wildcard $(BASE)-win.def),$(BASE)-win.def)
$(SHLIB): $(OBJECTS)
	$(SHLIB_LD) -shared $(DLLFLAGS) -o $@ $(BASE)-win.def $(OBJECTS) $(ALL_LIBS)
else
$(SHLIB): $(OBJECTS)
	@echo EXPORTS > tmp.def
	@$(NM) $^ | $(SED) -n 's/^.* [BCDRT] _/ /p' >> tmp.def
	$(SHLIB_LD) -shared $(DLLFLAGS) -o $@ tmp.def $(OBJECTS) $(ALL_LIBS)
	@$(RM) tmp.def
endif

.PHONY: all before after shlib-clean
shlib-clean:
	@rm -f $(OBJECTS)
